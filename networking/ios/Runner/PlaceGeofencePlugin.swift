import CoreLocation
import Flutter
import UIKit

/// The iOS half of the place predicates (spec §System Predicates).
///
/// GLP supplies a name and a radius; the layer registers a circular region
/// around the device's current location and reports the crossings the platform
/// observes. No coordinate ever crosses back: the channel carries a
/// registration identifier and `entered`/`exited`, and nothing else.
///
/// Authorization is When In Use, by a standing decision of this project: the
/// layer asks for no background location. Region monitoring proper
/// (`startMonitoring(for:)`) is documented to need Always authorization, so
/// under When In Use it cannot be relied on to deliver anything. The regions
/// are still registered with it — where a user has granted Always for another
/// reason the system path works, and it costs nothing where they have not —
/// and the crossings the layer can guarantee come from foreground location
/// updates evaluated against the same regions.
///
/// Both paths feed one inside/outside state per region, so a crossing is
/// emitted once, on a change, whichever path saw it first. That state is also
/// what makes registration silent: a region centred on the device's current
/// location is entered by construction, and being already inside is not a
/// crossing.
class PlaceGeofencePlugin: NSObject {
  private let locationManager = CLLocationManager()

  private let crossingChannel: FlutterEventChannel
  private let observabilityChannel: FlutterEventChannel
  private let methodChannel: FlutterMethodChannel

  /// The regions standing, by registration identifier.
  private var regions: [String: CLCircularRegion] = [:]

  /// Whether the device was last seen inside each standing region. A region is
  /// seeded `true` at registration, since it is centred where the device is.
  private var isInside: [String: Bool] = [:]

  private var crossingSink: FlutterEventSink?
  private var observabilitySink: FlutterEventSink?

  /// Whether the application currently holds the foreground. Crossings are
  /// observable only while it does, which is what the layer above is told.
  private var isActive = true

  /// Registrations waiting on a location fix or on the authorization prompt.
  private var pending: [(registrationId: String, radius: CLLocationDistance,
                         result: FlutterResult)] = []
  private var awaitingFix = false

  /// iOS monitors at most 20 regions per application.
  private static let regionLimit = 20

  /// The authorization status, read compatibly: the instance property is
  /// iOS 14 and up, and this target still deploys to iOS 13.
  private var authorizationStatus: CLAuthorizationStatus {
    if #available(iOS 14.0, *) {
      return locationManager.authorizationStatus
    }
    return CLLocationManager.authorizationStatus()
  }

  init(messenger: FlutterBinaryMessenger) {
    methodChannel = FlutterMethodChannel(
      name: "grassroots/places", binaryMessenger: messenger)
    crossingChannel = FlutterEventChannel(
      name: "grassroots/places/crossings", binaryMessenger: messenger)
    observabilityChannel = FlutterEventChannel(
      name: "grassroots/places/observability", binaryMessenger: messenger)
    super.init()

    methodChannel.setMethodCallHandler { [weak self] call, result in
      self?.handle(call, result: result)
    }
    crossingChannel.setStreamHandler(
      PlaceStreamHandler(
        onListen: { [weak self] sink in self?.crossingSink = sink },
        onCancel: { [weak self] in self?.crossingSink = nil }))
    observabilityChannel.setStreamHandler(
      PlaceStreamHandler(
        onListen: { [weak self] sink in
          self?.observabilitySink = sink
          // The current state at once: a fresh stream must not be read as
          // "nothing has happened yet" when the truth is that nothing is
          // being watched.
          if let self = self { sink(self.isObservable) }
        },
        onCancel: { [weak self] in self?.observabilitySink = nil }))

    locationManager.delegate = self
    locationManager.desiredAccuracy = kCLLocationAccuracyHundredMeters

    // Monitored regions outlive the process: the system holds them, and a
    // process killed without applicationWillTerminate leaves them standing.
    // A fresh start holds no declarations, so anything still monitored is
    // residue and goes.
    for region in locationManager.monitoredRegions {
      locationManager.stopMonitoring(for: region)
    }

    let center = NotificationCenter.default
    center.addObserver(
      self, selector: #selector(applicationDidBecomeActive),
      name: UIApplication.didBecomeActiveNotification, object: nil)
    center.addObserver(
      self, selector: #selector(applicationWillResignActive),
      name: UIApplication.willResignActiveNotification, object: nil)
  }

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  // MARK: - Method channel

  private func handle(
    _ call: FlutterMethodCall, result: @escaping FlutterResult
  ) {
    switch call.method {
    case "register":
      guard let args = call.arguments as? [String: Any],
        let registrationId = args["registrationId"] as? String,
        let radius = args["radiusMetres"] as? Double
      else {
        result(FlutterError(
          code: "bad-arguments",
          message: "register needs registrationId and radiusMetres",
          details: nil))
        return
      }
      register(registrationId: registrationId, radius: radius, result: result)

    case "unregister":
      guard let args = call.arguments as? [String: Any],
        let registrationId = args["registrationId"] as? String
      else {
        result(FlutterError(
          code: "bad-arguments", message: "unregister needs registrationId",
          details: nil))
        return
      }
      unregister(registrationId)
      result(nil)

    case "dispose":
      releaseEverything()
      result(nil)

    default:
      result(FlutterMethodNotImplemented)
    }
  }

  private func register(
    registrationId: String, radius: CLLocationDistance,
    result: @escaping FlutterResult
  ) {
    guard radius.isFinite, radius > 0 else {
      result(false)
      return
    }
    guard regions.count < PlaceGeofencePlugin.regionLimit else {
      NSLog("[place] iOS monitors at most %d regions; refusing %@",
            PlaceGeofencePlugin.regionLimit, registrationId)
      result(false)
      return
    }

    switch authorizationStatus {
    case .notDetermined:
      pending.append((registrationId, radius, result))
      locationManager.requestWhenInUseAuthorization()
      return
    case .restricted, .denied:
      result(false)
      return
    case .authorizedWhenInUse, .authorizedAlways:
      break
    @unknown default:
      result(false)
      return
    }

    if let fix = locationManager.location {
      finishRegistration(
        registrationId: registrationId, radius: radius,
        centre: fix.coordinate, result: result)
      return
    }

    // No fix yet: ask for one and answer when it arrives. A registration is
    // centred on the device's current location, and there is no honest answer
    // without one.
    pending.append((registrationId, radius, result))
    requestFixIfNeeded()
  }

  private func requestFixIfNeeded() {
    guard !awaitingFix, !pending.isEmpty else { return }
    awaitingFix = true
    locationManager.requestLocation()
  }

  private func finishRegistration(
    registrationId: String, radius: CLLocationDistance,
    centre: CLLocationCoordinate2D, result: @escaping FlutterResult
  ) {
    let capped = min(radius, locationManager.maximumRegionMonitoringDistance)
    let region = CLCircularRegion(
      center: centre, radius: capped, identifier: registrationId)
    region.notifyOnEntry = true
    region.notifyOnExit = true

    regions[registrationId] = region
    // Centred where the device is, so it starts inside — and that is not a
    // crossing.
    isInside[registrationId] = true

    if CLLocationManager.isMonitoringAvailable(for: CLCircularRegion.self) {
      locationManager.startMonitoring(for: region)
    }
    if !regions.isEmpty {
      locationManager.startUpdatingLocation()
    }
    result(true)
  }

  private func unregister(_ registrationId: String) {
    guard let region = regions.removeValue(forKey: registrationId) else {
      return
    }
    isInside.removeValue(forKey: registrationId)
    locationManager.stopMonitoring(for: region)
    if regions.isEmpty {
      locationManager.stopUpdatingLocation()
    }
  }

  /// Release every registration. Platforms bound how many regions an
  /// application may monitor, so nothing is left registered once the layer is
  /// done with it.
  func releaseEverything() {
    for region in regions.values {
      locationManager.stopMonitoring(for: region)
    }
    // Anything this process monitors that we have lost track of goes too.
    for region in locationManager.monitoredRegions {
      locationManager.stopMonitoring(for: region)
    }
    regions.removeAll()
    isInside.removeAll()
    for entry in pending { entry.result(false) }
    pending.removeAll()
    locationManager.stopUpdatingLocation()
  }

  // MARK: - Observability

  /// Whether the platform is reporting crossings at all: it needs both the
  /// authorization and the foreground, since the layer asks for no background
  /// location.
  private var isObservable: Bool {
    switch authorizationStatus {
    case .authorizedWhenInUse, .authorizedAlways: return isActive
    default: return false
    }
  }

  private func emitObservability() {
    observabilitySink?(isObservable)
  }

  @objc private func applicationDidBecomeActive() {
    isActive = true
    if !regions.isEmpty {
      locationManager.startUpdatingLocation()
    }
    emitObservability()
  }

  @objc private func applicationWillResignActive() {
    isActive = false
    emitObservability()
  }

  // MARK: - Crossings

  /// Report a crossing of [registrationId], if it is a change.
  private func report(registrationId: String, inside: Bool) {
    guard regions[registrationId] != nil else { return }
    guard isObservable else { return }
    guard isInside[registrationId] != inside else { return }
    isInside[registrationId] = inside
    crossingSink?([
      "registrationId": registrationId,
      "crossing": inside ? "entered" : "exited",
    ])
  }
}

// MARK: - CLLocationManagerDelegate

extension PlaceGeofencePlugin: CLLocationManagerDelegate {
  @available(iOS 14.0, *)
  func locationManagerDidChangeAuthorization(_ manager: CLLocationManager) {
    authorizationChanged()
  }

  /// The iOS 13 callback. Both land in the same place.
  func locationManager(
    _ manager: CLLocationManager,
    didChangeAuthorization status: CLAuthorizationStatus
  ) {
    authorizationChanged()
  }

  private func authorizationChanged() {
    emitObservability()

    switch authorizationStatus {
    case .notDetermined:
      return
    case .authorizedWhenInUse, .authorizedAlways:
      requestFixIfNeeded()
    default:
      let waiting = pending
      pending.removeAll()
      for entry in waiting { entry.result(false) }
    }
  }

  func locationManager(
    _ manager: CLLocationManager, didUpdateLocations locations: [CLLocation]
  ) {
    guard let fix = locations.last else { return }
    awaitingFix = false

    let waiting = pending
    pending.removeAll()
    for entry in waiting {
      finishRegistration(
        registrationId: entry.registrationId, radius: entry.radius,
        centre: fix.coordinate, result: entry.result)
    }

    // The crossings the layer can guarantee under When In Use: the same
    // geometry the system would apply, applied here while the application has
    // the foreground.
    for (registrationId, region) in regions {
      report(
        registrationId: registrationId,
        inside: region.contains(fix.coordinate))
    }
  }

  func locationManager(
    _ manager: CLLocationManager, didFailWithError error: Error
  ) {
    NSLog("[place] Location update failed: %@", error.localizedDescription)
    awaitingFix = false
    let waiting = pending
    pending.removeAll()
    for entry in waiting { entry.result(false) }
  }

  func locationManager(
    _ manager: CLLocationManager, didEnterRegion region: CLRegion
  ) {
    report(registrationId: region.identifier, inside: true)
  }

  func locationManager(
    _ manager: CLLocationManager, didExitRegion region: CLRegion
  ) {
    report(registrationId: region.identifier, inside: false)
  }

  func locationManager(
    _ manager: CLLocationManager, monitoringDidFailFor region: CLRegion?,
    withError error: Error
  ) {
    // Expected under When In Use: system region monitoring wants Always. The
    // declaration stands — the foreground path watches the same region.
    NSLog("[place] System region monitoring unavailable for %@: %@",
          region?.identifier ?? "<none>", error.localizedDescription)
  }
}

// MARK: - Stream handler

/// One stream handler for both channels; what differs is what it does on
/// listen, which is passed in.
private class PlaceStreamHandler: NSObject, FlutterStreamHandler {
  private let onListenCallback: (@escaping FlutterEventSink) -> Void
  private let onCancelCallback: () -> Void

  init(
    onListen: @escaping (@escaping FlutterEventSink) -> Void,
    onCancel: @escaping () -> Void
  ) {
    onListenCallback = onListen
    onCancelCallback = onCancel
  }

  func onListen(
    withArguments arguments: Any?, eventSink events: @escaping FlutterEventSink
  ) -> FlutterError? {
    onListenCallback(events)
    return nil
  }

  func onCancel(withArguments arguments: Any?) -> FlutterError? {
    onCancelCallback()
    return nil
  }
}
