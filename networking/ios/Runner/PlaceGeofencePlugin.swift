import CoreLocation
import Flutter
import UIKit

/// The iOS half of the place predicates (spec §System Predicates).
///
/// GLP supplies a name and a radius; the layer registers a circular region
/// about the centre the runtime gives --- the one kept from the place's first
/// declaration, at a restart --- or about the device's current location where
/// it gives none, answers with the centre it registered, which the runtime
/// keeps (spec §Geofencing, GNA a1cb804), and reports the crossings the
/// platform observes. The crossing channel carries a registration identifier
/// and `entered`/`exited`, and nothing else; the centre reaches the runtime
/// and no program.
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
/// crossing. A region registered about a given centre is seeded from the
/// current fix where there is one, and from the first fix otherwise, silently
/// either way: a crossing that occurred while the application was not running
/// was unobservable, and is lost (spec §Geofencing).
class PlaceGeofencePlugin: NSObject {
  private let locationManager = CLLocationManager()

  private let crossingChannel: FlutterEventChannel
  private let observabilityChannel: FlutterEventChannel
  private let methodChannel: FlutterMethodChannel

  /// The regions standing, by registration identifier.
  private var regions: [String: CLCircularRegion] = [:]

  /// Whether the device was last seen inside each standing region. A region
  /// centred where the device is is seeded `true` at registration; one
  /// registered about a given centre with no fix at hand has no entry until
  /// the first fix seeds it.
  private var isInside: [String: Bool] = [:]

  private var crossingSink: FlutterEventSink?
  private var observabilitySink: FlutterEventSink?

  /// Whether the application currently holds the foreground. Crossings are
  /// observable only while it does, which is what the layer above is told.
  private var isActive = true

  /// Registrations waiting on a location fix or on the authorization prompt;
  /// `centre` is the one the runtime gave, nil for the device's location.
  private var pending: [(registrationId: String, radius: CLLocationDistance,
                         centre: CLLocationCoordinate2D?,
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
      var centre: CLLocationCoordinate2D? = nil
      if let latitude = args["latitude"] as? Double,
        let longitude = args["longitude"] as? Double
      {
        centre = CLLocationCoordinate2D(latitude: latitude, longitude: longitude)
      }
      register(
        registrationId: registrationId, radius: radius, centre: centre,
        result: result)

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

  /// Register a region of `radius` about `centre`, the one the runtime gave,
  /// or about the device's current location where it gave none; answer with
  /// the centre registered, and with nil where the platform refuses.
  private func register(
    registrationId: String, radius: CLLocationDistance,
    centre: CLLocationCoordinate2D?, result: @escaping FlutterResult
  ) {
    guard radius.isFinite, radius > 0 else {
      result(nil)
      return
    }
    guard regions.count < PlaceGeofencePlugin.regionLimit else {
      NSLog("[place] iOS monitors at most %d regions; refusing %@",
            PlaceGeofencePlugin.regionLimit, registrationId)
      result(nil)
      return
    }

    switch authorizationStatus {
    case .notDetermined:
      pending.append((registrationId, radius, centre, result))
      locationManager.requestWhenInUseAuthorization()
      return
    case .restricted, .denied:
      result(nil)
      return
    case .authorizedWhenInUse, .authorizedAlways:
      break
    @unknown default:
      result(nil)
      return
    }

    if let given = centre {
      // The centre is the runtime's; a fix is not needed to register, and the
      // inside/outside state is seeded from the fix at hand, if any.
      finishRegistration(
        registrationId: registrationId, radius: radius, centre: given,
        inside: locationManager.location.map { given.region(radius).contains($0.coordinate) },
        result: result)
      return
    }

    if let fix = locationManager.location {
      finishRegistration(
        registrationId: registrationId, radius: radius,
        centre: fix.coordinate, inside: true, result: result)
      return
    }

    // No fix yet: ask for one and answer when it arrives. A registration is
    // centred on the device's current location, and there is no honest answer
    // without one.
    pending.append((registrationId, radius, nil, result))
    requestFixIfNeeded()
  }

  private func requestFixIfNeeded() {
    guard !awaitingFix, !pending.isEmpty else { return }
    awaitingFix = true
    locationManager.requestLocation()
  }

  /// Register the region and answer with its centre. `inside` seeds whether
  /// the device is in it: true for a region centred where the device is, the
  /// fix's answer for a given centre, and nil where there is no fix yet, the
  /// first fix then seeding it silently.
  private func finishRegistration(
    registrationId: String, radius: CLLocationDistance,
    centre: CLLocationCoordinate2D, inside: Bool?,
    result: @escaping FlutterResult
  ) {
    let capped = min(radius, locationManager.maximumRegionMonitoringDistance)
    let region = CLCircularRegion(
      center: centre, radius: capped, identifier: registrationId)
    region.notifyOnEntry = true
    region.notifyOnExit = true

    regions[registrationId] = region
    // Being already inside at registration is not a crossing.
    isInside[registrationId] = inside

    if CLLocationManager.isMonitoringAvailable(for: CLCircularRegion.self) {
      locationManager.startMonitoring(for: region)
    }
    if !regions.isEmpty {
      locationManager.startUpdatingLocation()
    }
    result(["latitude": centre.latitude, "longitude": centre.longitude])
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
    for entry in pending { entry.result(nil) }
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

  /// Report a crossing of [registrationId], if it is a change. Where the
  /// region's state is not yet known --- registered about a given centre with
  /// no fix at hand --- a fix seeds it silently (`seeding`), while the system's
  /// own report of a crossing is one and is reported.
  private func report(registrationId: String, inside: Bool, seeding: Bool = false) {
    guard regions[registrationId] != nil else { return }
    guard isObservable else { return }
    if isInside[registrationId] == nil && seeding {
      isInside[registrationId] = inside
      return
    }
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
      for entry in waiting { entry.result(nil) }
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
      if let given = entry.centre {
        finishRegistration(
          registrationId: entry.registrationId, radius: entry.radius,
          centre: given,
          inside: given.region(entry.radius).contains(fix.coordinate),
          result: entry.result)
      } else {
        finishRegistration(
          registrationId: entry.registrationId, radius: entry.radius,
          centre: fix.coordinate, inside: true, result: entry.result)
      }
    }

    // The crossings the layer can guarantee under When In Use: the same
    // geometry the system would apply, applied here while the application has
    // the foreground.
    for (registrationId, region) in regions {
      report(
        registrationId: registrationId,
        inside: region.contains(fix.coordinate), seeding: true)
    }
  }

  func locationManager(
    _ manager: CLLocationManager, didFailWithError error: Error
  ) {
    NSLog("[place] Location update failed: %@", error.localizedDescription)
    awaitingFix = false
    let waiting = pending
    pending.removeAll()
    for entry in waiting {
      if let given = entry.centre {
        // The runtime's centre needs no fix: the region is registered, its
        // state seeded by the first fix.
        finishRegistration(
          registrationId: entry.registrationId, radius: entry.radius,
          centre: given, inside: nil, result: entry.result)
      } else {
        entry.result(nil)
      }
    }
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

// MARK: - Geometry

extension CLLocationCoordinate2D {
  /// The circular region of `radius` about this coordinate, for a containment
  /// test; not the one registered.
  fileprivate func region(_ radius: CLLocationDistance) -> CLCircularRegion {
    CLCircularRegion(center: self, radius: radius, identifier: "")
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
