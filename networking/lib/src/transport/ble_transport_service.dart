import 'dart:async';

import 'package:grassroots_bluetooth_layer/grassroots_bluetooth_layer.dart'
    as ble;
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:redux/redux.dart';

import '../models/identity.dart';
import '../models/packet.dart';
import '../store/store.dart';
import 'transport_service.dart';

// Re-export the iOS local-name marker from the BLE plugin — the single source
// of truth (the plugin owns this constant as of grassroots_bluetooth_layer
// 0.2.0). iOS devices advertise this fixed local name; the iOS-central →
// non-iOS second link is measurably broken (see CLAUDE.md, "Dual-Role BLE Is
// Mandatory"), so peers that see the marker yield the pair's first dial to the
// iOS device and open the reverse leg themselves.
export 'package:grassroots_bluetooth_layer/grassroots_bluetooth_layer.dart'
    show grassrootsIosLocalName;

/// Default display info for BLE transport
const _defaultBleDisplayInfo = TransportDisplayInfo(
  icon: Icons.bluetooth,
  name: 'Bluetooth',
  description: 'Bluetooth Low Energy direct P2P transport',
  color: Colors.blue,
);

/// Grassroots characteristic UUID, fixed across all peers. Registered under
/// the fixed [_grassrootsGattServiceUuid] data-plane service (the advertised
/// service UUID is a separate, per-slot rotating discovery beacon).
const String _grassrootsCharacteristicUuid =
    '0000ff01-0000-1000-8000-00805f9b34fb';

/// Fixed GATT data-plane service UUID, identical on every Grassroots device:
/// the Grassroots prefix plus a reserved constant suffix. The *advertised*
/// service UUID rotates per slot (a discovery/recognition hint only); the data
/// characteristic lives under this stable service, so re-advertising a new
/// beacon never rebuilds the GATT service or drops live peripheral links
/// (plugin `gattServiceUuid` decoupling, grassroots_bluetooth_layer ≥ 0.3.0).
const String _grassrootsGattServiceUuid =
    '84c40316-0871-e5ad-0000-0000000000da';

/// MTU we request from the peer on every central connect. ANNOUNCE alone is
/// ~200 bytes, far over the default ATT MTU of 23 (20-byte payload). 247 is
/// the largest most Android stacks negotiate; the actual value is whatever
/// the peer accepts and is reported back via the `BlePath.mtu` field.
const int _requestedAndroidMtu = 247;

/// BLE-based implementation of the transport service.
///
/// Wraps the `grassroots_bluetooth_layer` Flutter plugin which unifies central and peripheral
/// roles in one bondless layer. The plugin emits a single role-tagged path
/// stream; this service projects path lifecycle events into Redux actions and
/// forwards payloads to the message router.
///
/// ## Architecture
///
/// - **Peripheral mode**: advertises our derived Grassroots service UUID,
///   accepts incoming connections, exposes one notify+write characteristic.
/// - **Central mode**: scans for peers advertising the Grassroots service
///   prefix and connects to them.
/// - **Direct delivery only**. No relaying, no store-and-forward.
class BleTransportService extends TransportService {
  /// Local device name for advertising (informational; iOS ignores it,
  /// Android does not include it in the advertise packet either).
  final String? localName;

  /// Our identity
  final GrassrootsIdentity identity;

  /// Redux store
  final Store<AppState> store;

  /// Cold-start grace period a non-initiator waits for the deterministic
  /// initiator (the peer with the lower service UUID) to open the first leg
  /// before it dials anyway. Injectable so tests can exercise the fallback
  /// without a real delay.
  final Duration firstMoverFallback;

  /// Restart the continuous scan when no advertisement has reached us for
  /// this long while we are in a scanning role. The transport's discovery
  /// relies on a single long-running, OS-unfiltered scan (prefix matching is
  /// user-space), and Android can silently mute such a scan — observed on
  /// Pixel after a force-cancelled `connecting` wedge: the scan "runs" but
  /// delivers nothing, leaving the device discovery-blind (existing links
  /// keep working; reverse legs toward new peers never dial). With peers
  /// nearby advertising several times a second, 30s of total silence means a
  /// dead scanner, not an empty room — and if the room IS empty, a restart
  /// is harmless. Injectable for tests.
  final Duration scanSilenceRestart;

  /// The unified BLE plugin facade
  final ble.GrassrootsBluetooth _ble;

  // Subscriptions to plugin event streams
  StreamSubscription<ble.BleAdapterState>? _adapterSub;
  StreamSubscription<ble.BleAdvertisement>? _advertisementSub;
  StreamSubscription<ble.BlePath>? _pathSub;
  StreamSubscription<ble.BlePayload>? _payloadSub;
  StreamSubscription<String>? _logSub;

  /// Latest known plugin state per pathId (synchronous mirror of `paths()`).
  /// This is a strict cache of plugin facts, not consumer state.
  final Map<String, ble.BlePath> _paths = {};

  /// Advertised service UUIDs (lowercase) whose advertisements have carried
  /// the iOS platform marker ([grassrootsIosLocalName]) this session. Like
  /// [_paths] this is a cache of transport facts, not consumer state: a
  /// peer's platform never changes, but the marker is only present while the
  /// iOS app is foregrounded, so we remember every sighting. Used to scope
  /// the iOS second-link rules to the *measured* constraint (iOS-central →
  /// non-iOS second link is broken) without extrapolating it to iOS peers —
  /// dual-role is mandatory wherever hardware allows it (see CLAUDE.md,
  /// "Dual-Role BLE Is Mandatory").
  ///
  /// The advertised UUID rotates every slot, so a UUID-keyed sighting expires
  /// with its slot; once the beacon is resolvable to an identified peer the
  /// sighting is promoted to the rotation-stable [_iosMarkedPubkeyHexes].
  /// Values are the sighting slot: entries older than the previous slot can
  /// never match again (candidate matching spans ±1 slot) and are pruned on
  /// write, keeping the map bounded instead of accreting one dead UUID per
  /// marked peer per slot forever.
  final Map<String, int> _iosMarkedServiceUuids = {};

  /// Pubkeys (lowercase hex) of peers known to be iOS devices — the
  /// rotation-stable home of platform-marker sightings. Populated by
  /// promotion from [_iosMarkedServiceUuids] whenever a marked beacon is
  /// resolvable to an identified peer (at sighting time or lazily at query
  /// time in [_isIosPeerPubkey]).
  final Set<String> _iosMarkedPubkeyHexes = {};

  /// Central pathIds we are tearing down for a wrong-order mixed-pair reform
  /// (see `_onAdvertisement`). Advertisements arrive far faster than the
  /// plugin's disconnect round-trip, so without this an ad burst would issue
  /// duplicate disconnects. Entries clear when the path reaches a terminal
  /// state in [_onPathChanged].
  final Set<String> _reformingCentralPathIds = {};

  /// True while a `start()` call is in flight. Prevents re-entrant `start()`
  /// from `_onAdapterStateChanged` running concurrently with the original.
  bool _starting = false;

  /// Scan-liveness watchdog (see [scanSilenceRestart]). Armed whenever the
  /// continuous scan is started; cancelled on stop/dispose or when the role
  /// mode stops scanning.
  Timer? _scanWatchdog;
  DateTime _lastAdvertisementAt = DateTime.now();

  /// Slot-rotation timer: checks periodically whether the 15-minute BLE slot
  /// advanced and, if so, re-advertises the new current-slot beacon via the
  /// plugin's non-destructive refresh (the fixed [_grassrootsGattServiceUuid]
  /// keeps the GATT server and live peripheral links intact). A coarse
  /// periodic check rather than an exact boundary alarm: it self-heals after
  /// app sleep / clock adjustments, and adjacent-slot matching on the
  /// recognition side absorbs the up-to-one-check-interval lateness.
  Timer? _slotRotationTimer;

  /// The slot whose beacon we most recently advertised, or null when not
  /// advertising. Compared against [GrassrootsIdentity.currentBleSlot] by the
  /// rotation timer to detect boundaries.
  int? _advertisedSlot;
  static const Duration _scanWatchdogInterval = Duration(seconds: 10);
  static const Duration _slotRotationCheckInterval = Duration(seconds: 30);

  /// True after [stop] is called. Drops in-flight payloads and prevents
  /// adapter-on auto-restart.
  bool _stopped = false;

  /// Stream controllers for the public TransportService API.
  final _dataController = StreamController<TransportDataEvent>.broadcast();
  final _connectionController =
      StreamController<TransportConnectionEvent>.broadcast();

  // ===== Public callbacks =====

  /// Called when a BLE packet is deserialized and ready for routing.
  /// `rssi` is the per-packet signal strength reported by the BLE plugin
  /// for every received packet, regardless of role; nullable only because
  /// the typedef matches `MessageRouter.processPacket`'s shared signature.
  void Function(GrassrootsPacket packet,
      {String? bleDeviceId, int? rssi, BleRole? bleRole})? onBlePacketReceived;

  /// Called when a peer disconnects at the BLE level. The argument is the
  /// peer's current `PeerState` snapshot — this is BLE-transport-level only;
  /// `GrassrootsNetwork` decides whether overall reachability changed before
  /// firing its consolidated `onPeerDisconnected`.
  void Function(PeerState peer)? onPeerDisconnected;

  // ===== Convenience getters for Redux state =====

  PeersState get _peersState => store.state.peers;

  BleTransportService({
    required this.identity,
    required this.store,
    this.localName,
    this.firstMoverFallback = const Duration(seconds: 5),
    this.scanSilenceRestart = const Duration(seconds: 30),
    ble.GrassrootsBluetooth? grassrootsBluetooth,
  }) : _ble = grassrootsBluetooth ?? ble.GrassrootsBluetooth();

  // ===== TransportService implementation =====

  @override
  TransportType get type => TransportType.ble;

  @override
  TransportDisplayInfo get displayInfo => _defaultBleDisplayInfo;

  @override
  TransportState get state => store.state.transports.bleState;

  @override
  Stream<TransportDataEvent> get dataStream => _dataController.stream;

  @override
  Stream<TransportConnectionEvent> get connectionStream =>
      _connectionController.stream;

  @override
  int get connectedCount => _readyPaths.length;

  @override
  bool get isActive => store.state.transports.bleState == TransportState.active;

  /// Whether scanning is currently active. The plugin doesn't expose this
  /// directly, so we treat "transport is active" as a proxy.
  bool get isScanning => isActive;

  /// All known peers from Redux store
  List<PeerState> get knownPeers => _peersState.peersList;

  /// Connected peers from Redux store
  List<PeerState> get connectedKnownPeers => _peersState.connectedPeers;

  /// All currently sendable BLE pathIds (across both roles).
  Set<String> get connectedPeerIds => _readyPaths.map((p) => p.pathId).toSet();

  /// All discovered BLE peers (before ANNOUNCE).
  List<DiscoveredPeerState> get discoveredPeers =>
      _peersState.discoveredBlePeersList;

  bool isPeerReachable(Uint8List pubkey) => _peersState.isPeerReachable(pubkey);

  PeerState? getPeer(Uint8List pubkey) => _peersState.getPeerByPubkey(pubkey);

  /// Whether the given pathId is currently sendable.
  bool isDeviceConnected(String peerId) {
    final path = _paths[peerId];
    return path != null && _isReady(path);
  }

  // ===== Lifecycle =====

  @override
  Future<bool> initialize() async {
    if (state != TransportState.uninitialized) {
      return state.isUsable;
    }
    _setState(TransportState.initializing);

    try {
      _adapterSub = _ble.adapterStateChanges.listen((s) {
        _onAdapterStateChanged(s);
      });
      _advertisementSub = _ble.advertisements.listen(_onAdvertisement);
      _pathSub = _ble.pathChanges.listen(_onPathChanged);
      _payloadSub = _ble.payloads.listen(_onPayload);
      _logSub = _ble.logs.listen(
        (msg) => debugPrint('[grassroots_bluetooth_layer] $msg'),
      );

      // `restoreState: true` opts the iOS plugin into CoreBluetooth's
      // state-preservation. With this on, when iOS suspends and later
      // relaunches the app for a BLE event, the peripheral subscriptions,
      // GATT services and active scan are re-attached and the plugin's
      // `willRestoreState` handler rebuilds its in-process tables.
      await _ble.initialize(verboseLogging: true, restoreState: true);

      _setState(TransportState.ready);
      return true;
    } catch (e) {
      debugPrint('Failed to initialize BLE transport: $e');
      _setState(TransportState.error);
      return false;
    }
  }

  @override
  Future<void> start() async {
    if (state != TransportState.ready && state != TransportState.active) {
      debugPrint('Cannot start BLE transport in state: $state');
      return;
    }
    if (_starting) {
      // Re-entrant start (e.g. adapter-on event firing while a previous
      // start is still awaiting). Skip to avoid a redundant tear-down/rebuild
      // cycle that would drop currently-subscribed peripheral centrals.
      return;
    }
    _starting = true;
    _stopped = false;

    final mode = store.state.settings.bleRoleMode;
    final shouldAdvertise = mode != BleRoleMode.centralOnly;
    final shouldScan = mode != BleRoleMode.peripheralOnly;
    debugPrint('BLE start: roleMode=$mode '
        'advertise=$shouldAdvertise scan=$shouldScan');

    // Advertising and scanning are independent; failure in one must not
    // prevent the other. Track whether at least one succeeded — if both
    // fail (e.g. adapter still off), stay in `ready` so the next
    // adapter-on event re-invokes `start()`.
    var anyStarted = false;
    try {
      if (shouldAdvertise) {
        try {
          final slot = GrassrootsIdentity.currentBleSlot();
          await _ble.startAdvertising(
            // The rotating current-slot beacon — a discovery/recognition hint.
            serviceUuid:
                GrassrootsIdentity.deriveServiceUuidForSlot(
                    identity.publicKey, slot),
            characteristicUuid: _grassrootsCharacteristicUuid,
            // The stable data plane: beacon rotation never rebuilds the GATT
            // service or drops live peripheral links.
            gattServiceUuid: _grassrootsGattServiceUuid,
            localName: localName,
            bondless: true,
          );
          _advertisedSlot = slot;
          anyStarted = true;
          _armSlotRotation(localName);
        } catch (e) {
          debugPrint('Failed to start advertising: $e');
        }
      } else {
        // Make sure we aren't lingering as an advertiser from a previous
        // mode — explicitly tear down, including the slot-rotation timer,
        // which would otherwise resurrect advertising at the next 15-min
        // boundary (mirrors the scan-watchdog cancel in the scan branch).
        _slotRotationTimer?.cancel();
        _slotRotationTimer = null;
        _advertisedSlot = null;
        try {
          await _ble.stopAdvertising();
        } catch (_) {}
      }

      if (shouldScan) {
        try {
          await _ble.startScan(
            serviceUuidPrefix: GrassrootsIdentity.grassrootsUuidPrefix,
            timeout: Duration.zero, // continuous scan
            // iOS CoreBluetooth deduplicates per-peer advertisements by
            // default; with allowDuplicates=true it keeps delivering
            // didDiscover so we see liveness/RSSI updates and so a peer that
            // joined mid-scan still gets observed. It costs a bit more power,
            // but also gives us fresh chances to retry a missing reverse leg.
            allowDuplicates: true,
          );
          anyStarted = true;
          _lastAdvertisementAt = DateTime.now();
          _armScanWatchdog();
        } catch (e) {
          debugPrint('Failed to start scanning: $e');
        }
      } else {
        _scanWatchdog?.cancel();
        _scanWatchdog = null;
        try {
          await _ble.stopScan();
        } catch (_) {}
      }

      if (anyStarted) {
        _setState(TransportState.active);
      }
    } finally {
      _starting = false;
    }
  }

  void _armScanWatchdog() {
    _scanWatchdog?.cancel();
    _scanWatchdog = Timer.periodic(
      _scanWatchdogInterval,
      (_) => unawaited(checkScanLiveness()),
    );
  }

  /// Restart the continuous scan if the airwaves have been silent past
  /// [scanSilenceRestart] — the recovery for a silently muted scanner (see
  /// the field doc). One restart per silence window: the clock resets on the
  /// restart itself, so an empty room costs one cheap stop+start per window
  /// rather than one per watchdog tick.
  @visibleForTesting
  Future<void> checkScanLiveness({DateTime? now}) async {
    if (_stopped) return;
    if (store.state.settings.bleRoleMode == BleRoleMode.peripheralOnly) {
      return;
    }
    final t = now ?? DateTime.now();
    if (t.difference(_lastAdvertisementAt) < scanSilenceRestart) return;

    debugPrint(
      '[ble] scan-watchdog: no advertisements for '
      '${scanSilenceRestart.inSeconds}s — restarting the continuous scan '
      '(a long-running unfiltered Android scan can be silently muted).',
    );
    _lastAdvertisementAt = t;
    try {
      await _ble.startScan(
        serviceUuidPrefix: GrassrootsIdentity.grassrootsUuidPrefix,
        timeout: Duration.zero,
        allowDuplicates: true,
      );
    } catch (e) {
      debugPrint('[ble] scan-watchdog: scan restart failed: $e');
    }
  }

  /// Apply a runtime role-mode change. Stops the current scan/advertise and
  /// restarts under the new mode. Existing live paths are left untouched —
  /// the plugin's path stream will tear them down naturally if the OS
  /// disconnects them.
  Future<void> applyRoleModeChange() async {
    if (state != TransportState.active && state != TransportState.ready) {
      return;
    }
    debugPrint('BLE role mode changed → restarting transport');
    try {
      await _ble.stopScan();
    } catch (_) {}
    try {
      await _ble.stopAdvertising();
    } catch (_) {}
    if (state == TransportState.active) {
      _setState(TransportState.ready);
    }
    await start();
  }

  /// Arm the slot-rotation check: every [_slotRotationCheckInterval], if the
  /// 15-minute BLE slot advanced past [_advertisedSlot], re-advertise the new
  /// current-slot beacon. The GATT service UUID and characteristic stay fixed,
  /// so the plugin refreshes only the advertisement — live peripheral links
  /// survive the rotation (validated on Android + iOS hardware).
  void _armSlotRotation(String? localName) {
    _slotRotationTimer?.cancel();
    _slotRotationTimer =
        Timer.periodic(_slotRotationCheckInterval, (_) async {
      if (_stopped) return;
      // Never advertise in a role mode that doesn't (centralOnly): the timer
      // is cancelled on mode change, but guard here too so a stray tick can
      // never violate the "a central-only device never advertises" invariant.
      if (store.state.settings.bleRoleMode == BleRoleMode.centralOnly) return;
      final slot = GrassrootsIdentity.currentBleSlot();
      if (slot == _advertisedSlot) return;
      final beacon =
          GrassrootsIdentity.deriveServiceUuidForSlot(identity.publicKey, slot);
      try {
        await _ble.startAdvertising(
          serviceUuid: beacon,
          characteristicUuid: _grassrootsCharacteristicUuid,
          gattServiceUuid: _grassrootsGattServiceUuid,
          localName: localName,
          bondless: true,
        );
        _advertisedSlot = slot;
        debugPrint('[ble] slot rotated to $slot — advertising $beacon');
      } catch (e) {
        // Leave _advertisedSlot unchanged so the next tick retries.
        debugPrint('[ble] slot re-advertise failed (will retry): $e');
      }
    });
  }

  @override
  Future<void> stop() async {
    _stopped = true;
    _scanWatchdog?.cancel();
    _scanWatchdog = null;
    _slotRotationTimer?.cancel();
    _slotRotationTimer = null;
    _advertisedSlot = null;
    try {
      await _ble.stopScan();
    } catch (_) {}
    try {
      await _ble.stopAdvertising();
    } catch (_) {}

    // Disconnect every known path. Order doesn't matter — the plugin emits
    // disconnected events that we project into Redux.
    final pathIds = _paths.keys.toList(growable: false);
    for (final pathId in pathIds) {
      try {
        await _ble.disconnect(pathId, forget: true);
      } catch (_) {}
    }

    if (state == TransportState.active) {
      _setState(TransportState.ready);
    }
  }

  /// Trigger a finite scan window. The plugin handles the timeout natively.
  Future<void> scan({Duration? timeout}) async {
    if (store.state.settings.bleRoleMode == BleRoleMode.peripheralOnly) {
      return;
    }
    final t = timeout ?? const Duration(seconds: 10);
    try {
      await _ble.startScan(
        serviceUuidPrefix: GrassrootsIdentity.grassrootsUuidPrefix,
        timeout: t,
        // Match the continuous-scan path so already-discovered peers keep
        // surfacing for RSSI refreshes and reverse-leg retries.
        allowDuplicates: true,
      );
    } catch (e) {
      debugPrint('scan() failed: $e');
    }
  }

  @override
  Future<bool> sendToPeer(String peerId, Uint8List data) async {
    final path = _paths[peerId];
    if (path == null || !_isReady(path)) {
      return false;
    }
    try {
      await _ble.send(peerId, data);
      return true;
    } catch (e) {
      debugPrint('send() failed for $peerId: $e');
      return false;
    }
  }

  @override
  Future<void> broadcast(Uint8List data, {Set<String>? excludePeerIds}) async {
    // Sort by RSSI descending so the strongest signals get the data first.
    // Paths without a known RSSI (peripheral-role on iOS/Android, where the
    // OS doesn't expose remote signal strength) sort last via a very-weak
    // fallback so they still receive the broadcast.
    final ready = _readyPaths.toList()
      ..sort((a, b) => (b.rssi ?? -100).compareTo(a.rssi ?? -100));
    for (final path in ready) {
      if (excludePeerIds != null && excludePeerIds.contains(path.pathId)) {
        continue;
      }
      try {
        await _ble.send(path.pathId, data);
      } catch (e) {
        debugPrint('broadcast send() failed for ${path.pathId}: $e');
      }
    }
  }

  @override
  void associatePeerWithPubkey(String peerId, Uint8List pubkey) {
    final path = _paths[peerId];
    if (path == null || !_isReady(path)) return;

    final role = _roleFromPathId(peerId);
    if (role == null) return;
    store.dispatch(AssociateBleDeviceAction(
      publicKey: pubkey,
      deviceId: peerId,
      role: role,
    ));

    // Peripheral-side ANNOUNCE just identified the peer. This is the
    // moment we have enough information to act on the pair's reverse leg:
    // we know the peer's pubkey, so we can derive their service UUID and
    // correlate it with discovery state.
    if (role == BleRole.peripheral) {
      final peerIsIos = _isIosPeerPubkey(pubkey);
      if (defaultTargetPlatform == TargetPlatform.iOS && !peerIsIos) {
        // The reverse leg toward a non-iOS peer is the pair's SECOND link,
        // which an iOS central cannot open (hardware-measured; see
        // [_shouldOpenCentralLeg]) — so far from dialing it, abort any of our
        // central dials to this identity that are still in flight: they were
        // racing the inbound leg that just won, and would otherwise wedge in
        // `connecting` until the connect timeout. Toward iOS peers the
        // reverse leg is attempted like on any other platform (dual-role
        // mandate).
        _cancelDoomedCentralDialsForPubkey(pubkey);
      } else {
        _maybeDialReverseCentralForPubkey(pubkey);
      }
    }
  }

  /// iOS only: abort in-flight central dials to the peer identified by
  /// [pubkey]. Called the moment an inbound peripheral leg is authenticated —
  /// from that point any central dial of ours to the same identity is a
  /// doomed second link (it can never reach `didConnect`) and would hold a
  /// dial slot for the full connect timeout.
  void _cancelDoomedCentralDialsForPubkey(Uint8List pubkey) {
    final candidates = _candidateUuidsForPubkey(pubkey);
    for (final p in _paths.values.toList(growable: false)) {
      if (p.role != ble.BleRole.central) continue;
      if (p.state != ble.BlePathState.connecting) continue;
      final discovered = _peersState.getDiscoveredBlePeer(p.pathId);
      final discoveredUuid = discovered?.serviceUuid?.toLowerCase();
      if (discoveredUuid == null || !candidates.contains(discoveredUuid)) {
        continue;
      }
      debugPrint(
        '[ble] aborting central dial ${p.pathId}: peer just authenticated an '
        'inbound peripheral leg, and an iOS central cannot open a second '
        'link to the same pair.',
      );
      unawaited(disconnectDevice(p.pathId, forget: true));
    }
  }

  @override
  String? getPeerIdForPubkey(Uint8List pubkey) {
    final peer = _peersState.getPeerByPubkey(pubkey);
    if (peer == null) return null;

    // Prefer our central path because writes go directly to the peer's GATT
    // characteristic; fall back to the inbound peripheral path when that is
    // the only ready route.
    final centralId = peer.bleCentralDeviceId;
    if (centralId != null && isDeviceConnected(centralId)) {
      return centralId;
    }
    final peripheralId = peer.blePeripheralDeviceId;
    if (peripheralId != null && isDeviceConnected(peripheralId)) {
      return peripheralId;
    }
    return null;
  }

  @override
  Uint8List? getPubkeyForPeerId(String peerId) {
    for (final peer in _peersState.peersList) {
      if (peer.bleCentralDeviceId == peerId ||
          peer.blePeripheralDeviceId == peerId) {
        return peer.publicKey;
      }
    }
    return null;
  }

  /// Accepted-friend hint for a BLE path.
  ///
  /// For already authenticated paths this returns the mapped friend pubkey.
  /// For pre-ANNOUNCE central paths it may return a friend whose derived
  /// service UUID matches the advertisement. That second case is only a hint:
  /// callers must not send friend-only metadata until a signed ANNOUNCE maps
  /// the path to the same public key.
  Uint8List? getFriendPubkeyHintForPeerId(String peerId) {
    final mappedPubkey = getPubkeyForPeerId(peerId);
    if (mappedPubkey != null) {
      final peer = _peersState.getPeerByPubkey(mappedPubkey);
      if (peer?.isFriend == true) return mappedPubkey;
    }

    final discovered = _peersState.getDiscoveredBlePeer(peerId);
    final serviceUuid = discovered?.serviceUuid;
    if (serviceUuid == null) return null;
    return _friendPubkeyForDerivedServiceUuid(serviceUuid);
  }

  @override
  Future<void> dispose() async {
    await stop();
    await _adapterSub?.cancel();
    await _advertisementSub?.cancel();
    await _pathSub?.cancel();
    await _payloadSub?.cancel();
    await _logSub?.cancel();
    try {
      await _ble.dispose();
    } catch (_) {}

    _setState(TransportState.disposed);
    await _dataController.close();
    await _connectionController.close();
  }

  // ===== Manual connect/disconnect (still exposed for the UI) =====

  /// Connect to a discovered peer. The pathId is `central:<remote-id>`.
  ///
  /// The plugin's `connect()` is itself idempotent — calling it twice for
  /// the same pathId either reuses the in-flight connection (iOS) or returns
  /// the existing path (Android). We do not duplicate that guard here.
  /// Path-state updates flow through `_onPathChanged`, which is the only
  /// dispatcher of `BleDeviceConnectingAction` / `Connected` / `Failed`.
  Future<bool> connectToDevice(String pathId) async {
    if (!pathId.startsWith('central:')) {
      // Peripheral-side paths are inbound — we don't dial them.
      return false;
    }
    if (isDeviceConnected(pathId)) {
      return false;
    }

    final discovered = _peersState.getDiscoveredBlePeer(pathId);
    if (discovered?.isConnecting == true) {
      return false;
    }
    final serviceUuid = discovered?.serviceUuid;
    if (serviceUuid == null) {
      debugPrint('Cannot connect to $pathId: no advertised service UUID');
      return false;
    }
    // Hard invariant on iOS, enforced at the one choke point every central
    // dial passes (auto arbitration, reverse-leg helpers, manual UI taps):
    // an iOS central cannot open the second link toward a NON-iOS peer
    // (hardware-measured), so a dial to such an identity we already hold an
    // inbound peripheral leg from can never complete — it would wedge in
    // `connecting` for the full connect timeout. Scoped strictly to the
    // measured constraint: toward iOS peers the second link is attempted
    // (dual-role mandate; see CLAUDE.md).
    if (defaultTargetPlatform == TargetPlatform.iOS &&
        !_isIosPeerServiceUuid(serviceUuid) &&
        _hasLivePeripheralPathForServiceUuid(serviceUuid)) {
      debugPrint(
        'Skipping $pathId: already attached via inbound peripheral leg and '
        'an iOS central cannot open a second link toward a non-iOS peer.',
      );
      return false;
    }
    if (store.state.settings.coldCallTrustLevel == ColdCallTrustLevel.closed &&
        _friendPubkeyForDerivedServiceUuid(serviceUuid) == null) {
      debugPrint('Skipping $pathId: closed trust and unknown service UUID');
      return false;
    }

    final remoteId = pathId.substring('central:'.length);
    try {
      await _ble.connect(
        remoteId: remoteId,
        // Discovery keys off the rotating advertised beacon, but the GATT data
        // service to discover post-connect is the fixed UUID every Grassroots
        // device registers under.
        serviceUuid: _grassrootsGattServiceUuid,
        characteristicUuid: _grassrootsCharacteristicUuid,
        androidMtu: _requestedAndroidMtu,
        // Apple's docs say CoreBluetooth's connect can legitimately take
        // 10-15s, so we stay safely above that. The deterministic first-mover
        // gate should prevent the simultaneous-dial collision that used to wedge
        // a connect for the full window; 20s is a safety net for any collision
        // that still slips through (e.g. during the first-mover fallback) so it
        // recovers and retries sooner.
        timeout: const Duration(seconds: 20),
      );
      return true;
    } catch (e) {
      // The plugin throws synchronously only for invalid args / adapter off.
      // No path event will fire, so dispatch a failure action ourselves so
      // Redux doesn't show the peer stuck in `isConnecting` (which the
      // plugin would otherwise correct via a `failed` event).
      store.dispatch(BleDeviceConnectionFailedAction(pathId));
      return false;
    }
  }

  Future<void> disconnectDevice(String pathId, {bool forget = true}) async {
    try {
      await _ble.disconnect(pathId, forget: forget);
    } catch (e) {
      debugPrint('disconnect() failed for $pathId: $e');
    }
  }

  /// Process an incoming raw BLE packet. Deserializes and forwards to the
  /// MessageRouter via [onBlePacketReceived]. `rssi` is nullable because
  /// peripheral-role payloads carry no remote-RSSI measurement on either
  /// platform; the downstream router treats null as "unknown" and skips
  /// updating peer RSSI from that packet.
  void onPacketReceived(Uint8List data,
      {String? fromDeviceId, required int? rssi, BleRole? bleRole}) {
    if (_stopped) return;
    try {
      final packet = GrassrootsPacket.deserialize(data);
      onBlePacketReceived?.call(
        packet,
        bleDeviceId: fromDeviceId,
        rssi: rssi,
        bleRole: bleRole,
      );
    } catch (e) {
      debugPrint('Failed to deserialize packet: $e');
    }
  }

  // ===== Peer lifecycle helpers =====

  void onPeerBleConnected(String pathId, {int? rssi}) {
    debugPrint('BLE peer connected: $pathId');
  }

  void onPeerBleDisconnected(Uint8List pubkey, {BleRole? role}) {
    final peer = _peersState.getPeerByPubkey(pubkey);
    if (peer == null) return;
    store.dispatch(PeerBleDisconnectedAction(pubkey, role: role));
    onPeerDisconnected?.call(peer);
  }

  // ===== Plugin event handlers =====

  void _onAdapterStateChanged(ble.BleAdapterState adapterState) {
    if (adapterState != ble.BleAdapterState.poweredOn) {
      // The plugin already stops scan/advertising and tears down paths on
      // adapter-off; mirror that into Redux by dropping back to `ready` so
      // the next adapter-on triggers a fresh `start()`.
      if (state == TransportState.active) {
        _setState(TransportState.ready);
      }
      return;
    }

    // Adapter came back on. iOS resumes deferred scan/advertise itself, but
    // Android throws on a powered-off call so the prior start was dropped.
    // We re-issue both unconditionally when the transport is in `ready`;
    // duplicates are harmless because `start()` calls `stopScan`/`stopAdvertising`
    // first via the plugin's idempotent path.
    if (state == TransportState.ready && !_stopped) {
      unawaited(start());
    }
  }

  void _onAdvertisement(ble.BleAdvertisement adv) {
    // Any delivery proves the scanner is alive — feed the watchdog before
    // any gate below can return.
    _lastAdvertisementAt = DateTime.now();
    final pathId = 'central:${adv.remoteId}';
    final serviceUuid = _firstGrassrootsServiceUuid(adv.serviceUuids);
    if (serviceUuid == null) {
      // Plugin already filters by Grassroots prefix, but defensively skip
      // anything that lost its service UUID before reaching us.
      return;
    }

    // Resolve the advertised (rotating) beacon back to an already-identified
    // peer by matching current+adjacent-slot candidates. Null pre-ANNOUNCE.
    final resolvedPubkey = _pubkeyForAdvertisedUuid(serviceUuid);
    // Every UUID this logical peer may currently be advertising under. For an
    // unresolved (unknown) peer only the observed beacon itself is usable.
    final uuidAliases = resolvedPubkey != null
        ? _candidateUuidsForPubkey(resolvedPubkey)
        : {serviceUuid.toLowerCase()};

    // Record platform-marker sightings before any early return: the marker
    // identifies the peer as iOS for the rest of the session, including for
    // decisions made while this identity is connected (and thus not dialed).
    if (_advertisementCarriesIosMarker(adv)) {
      final markerSlot = GrassrootsIdentity.currentBleSlot();
      _iosMarkedServiceUuids[serviceUuid.toLowerCase()] = markerSlot;
      // Rotated-away beacons can never match again; drop them so the map
      // stays bounded by the number of currently-marked peers.
      _iosMarkedServiceUuids.removeWhere((_, slot) => slot < markerSlot - 1);
      // Promote to the rotation-stable pubkey key when the peer is known —
      // the UUID-keyed sighting expires with the slot, the pubkey never does.
      if (resolvedPubkey != null) {
        _iosMarkedPubkeyHexes.add(_pubkeyHexOf(resolvedPubkey));
      }

      // Wrong-order mixed-pair reform (dual-role mandate, CLAUDE.md). We are
      // non-iOS, hold ONLY a central leg to this iOS identity, and its
      // missing leg — an iOS-central second link toward us — is
      // hardware-broken: the pair can never upgrade in place. The marker in
      // THIS advertisement (not the sticky set) proves the iOS app is
      // foregrounded right now, i.e. it can redial within seconds. So drop
      // our wrong-order central leg and yield: the iPhone opens the first
      // leg, and our reverse dial completes the dual-role pair. Backgrounded
      // iPhones advertise no marker, so this self-damps — we never trade a
      // working link away unless the peer is provably there to rebuild it.
      if (defaultTargetPlatform != TargetPlatform.iOS &&
          store.state.settings.bleRoleMode == BleRoleMode.auto &&
          !_hasLivePeripheralPathForServiceUuid(serviceUuid)) {
        final centralId = _liveCentralPathIdForServiceUuid(serviceUuid);
        if (centralId != null && !_reformingCentralPathIds.contains(centralId)) {
          _reformingCentralPathIds.add(centralId);
          debugPrint(
            '[ble] reforming wrong-order pair: dropping our central leg '
            '$centralId so the (foregrounded) iOS peer can open the first '
            'leg; we reopen ours as the reverse leg.',
          );
          unawaited(disconnectDevice(centralId, forget: true));
          return;
        }
      }
    }

    // Drop advertisements from a rotated radio MAC when we already have a
    // live or in-flight path to the same logical peer on a different MAC.
    // The advertised UUID and the radio MAC both rotate ~every 15 minutes
    // (deliberately co-timed), so the same logical peer is matched via
    // [uuidAliases]: within a slot the UUID is a stable per-peer key, and for
    // an identified peer the candidate set also bridges slot boundaries.
    // Recording a rotated MAC as a new DiscoveredPeerState (and dialing it)
    // would create a duplicate central path racing the existing one — that's
    // exactly the failure mode behind the GATT-status-133 storm we see when
    // iOS rotates rapidly.
    final activeOnOtherMac = uuidAliases
        .expand(_peersState.getDiscoveredBlePeersByServiceUuid)
        .where((p) => p.transportId != pathId)
        .any((p) => p.isConnected || p.isConnecting);
    if (activeOnOtherMac) {
      return;
    }

    // Same suppression, keyed on the identified peer instead of the transient
    // discovery map. Once ANNOUNCE identifies the peer, the connected MAC's
    // DiscoveredPeerState stops being re-advertised (the peer rotates its RPA)
    // and is stale-pruned, which blinds the `activeOnOtherMac` check above. The
    // central attachment on the identified PeerState is rotation-stable, so
    // consult it directly: if we already hold a live central leg to this
    // identity, dialing a freshly-rotated MAC only duplicates it — the
    // GATT-133 storm. Peripheral attachments are deliberately ignored: when we
    // hold only the inbound peripheral leg, this dial is the reverse (central)
    // leg that completes the dual-role connection and must proceed.
    if (_hasLiveCentralPathForServiceUuid(serviceUuid)) {
      return;
    }

    final existing = _peersState.getDiscoveredBlePeer(pathId);
    if (existing == null) {
      store.dispatch(BleDeviceDiscoveredAction(
        deviceId: pathId,
        displayName: adv.advertisedName ?? adv.platformName,
        rssi: adv.rssi,
        serviceUuid: serviceUuid,
      ));
    } else {
      store.dispatch(BleDeviceRssiUpdatedAction(
        deviceId: pathId,
        rssi: adv.rssi,
      ));
    }

    // Update RSSI on identified peers too, so the UI ordering reflects fresh
    // signal strength.
    final pubkey = getPubkeyForPeerId(pathId);
    if (pubkey != null) {
      final existingPeer = _peersState.getPeerByPubkey(pubkey);
      if (existingPeer?.rssi == null || existingPeer?.rssi != adv.rssi) {
        store.dispatch(PeerRssiUpdatedAction(
          publicKey: pubkey,
          rssi: adv.rssi,
        ));
      }
    }

    if (existing != null && (existing.isConnected || existing.isConnecting)) {
      return;
    }
    if (!_shouldOpenCentralLeg(adv, serviceUuid, existing)) {
      return;
    }
    if (store.state.settings.coldCallTrustLevel == ColdCallTrustLevel.closed &&
        _friendPubkeyForDerivedServiceUuid(serviceUuid) == null) {
      return;
    }
    // BLE address rotation produces a fresh pathId every ~30s for the same
    // peer. Cap the number of in-flight central dials so a chatty rotator
    // can't exhaust the BLE stack's connection slots.
    if (_inFlightCentralDials() >= _maxInFlightCentralDials) {
      return;
    }

    unawaited(connectToDevice(pathId));
  }

  /// True when we already hold a live central (outbound) BLE path to the peer
  /// whose pubkey derives [serviceUuid]. Suppresses duplicate central dials to
  /// a peer that has merely rotated its advertising MAC: the rotation defeats
  /// the discovery-map guard in [_onAdvertisement] once the old MAC's
  /// DiscoveredPeerState is stale-pruned, but the identified peer's central
  /// attachment survives rotation. Mirrors the `bleCentralDeviceId != null`
  /// check in [_maybeDialReverseCentralForPubkey], and — like it — ignores
  /// peripheral attachments so the central leg of a dual-role connection is
  /// still dialed when only the inbound peripheral leg exists.
  bool _hasLiveCentralPathForServiceUuid(String serviceUuid) =>
      _liveCentralPathIdForServiceUuid(serviceUuid) != null;

  /// The live central pathId attached to the identified peer whose pubkey
  /// derives [serviceUuid] (in any current-window slot), or null when none is
  /// connected.
  String? _liveCentralPathIdForServiceUuid(String serviceUuid) {
    final normalized = serviceUuid.toLowerCase();
    for (final peer in _peersState.peersList) {
      if (!_candidateUuidsForPubkey(peer.publicKey).contains(normalized)) {
        continue;
      }
      final centralId = peer.bleCentralDeviceId;
      return centralId != null && isDeviceConnected(centralId)
          ? centralId
          : null;
    }
    return null;
  }

  /// Cap on simultaneous `connecting` central paths.
  /// Each `connectGatt` consumes a controller slot for ~5s on Android; too
  /// many parallel dials starve real connections.
  static const int _maxInFlightCentralDials = 2;

  int _inFlightCentralDials() {
    var count = 0;
    for (final p in _paths.values) {
      if (p.role != ble.BleRole.central) continue;
      if (p.state == ble.BlePathState.connecting ||
          p.state == ble.BlePathState.connected ||
          p.state == ble.BlePathState.subscribed) {
        count++;
      }
    }
    return count;
  }

  /// Central-dial arbitration for `auto` mode: decides whether this
  /// advertisement should trigger an outbound (central) dial right now.
  ///
  /// Dual-role (two legs per pair, each device central on one) is mandatory —
  /// see CLAUDE.md, "Dual-Role BLE Is Mandatory". Arbitration exists to pick
  /// the leg ORDER that makes it reachable, around one hardware-measured
  /// constraint (A2/iPhone field tests):
  ///
  ///  1. An **iOS central cannot open the second link toward a non-iOS
  ///     peer.** Once such a pair is linked, an iOS-initiated connect never
  ///     reaches `didConnect` and wedges in `connecting` until the connect
  ///     timeout. (Toward iOS peers this is unmeasured, so per the mandate we
  ///     attempt it — hardware, not extrapolation, gets to refuse.)
  ///  2. A **non-iOS central opens a second (reverse) link just fine** — to
  ///     iOS and Android peripherals alike.
  ///
  /// So for mixed pairs iOS must own the first link and the non-iOS side the
  /// reverse leg. iOS peers are recognized by the fixed `grs-ios` local name
  /// their advertisements carry (see [grassrootsIosLocalName]); among
  /// same-platform peers the deterministic smaller-pubkey tiebreaker (same
  /// rule as over IP; see [_isBleDialInitiator]) avoids the mutual-dial
  /// collision. Every waiting branch is backstopped by [firstMoverFallback]
  /// so a peer whose expected initiator never shows (backgrounded iOS,
  /// peripheral-only device, marker lost from the scan response) still gets a
  /// first link — and the pair keeps upgrading toward dual-role from there.
  ///
  /// Auto-only: a central-only device never advertises, so it can never be
  /// dialed and must always first-move.
  bool _shouldOpenCentralLeg(
    ble.BleAdvertisement adv,
    String serviceUuid,
    DiscoveredPeerState? existing,
  ) {
    if (store.state.settings.bleRoleMode != BleRoleMode.auto) return true;

    final peerIsIos = _isIosPeerServiceUuid(serviceUuid);
    if (defaultTargetPlatform == TargetPlatform.iOS) {
      if (!peerIsIos) {
        // Constraint 1: toward a non-iOS peer we already hold an inbound
        // peripheral leg from, our dial is the measured-broken second link.
        // The reverse leg of this pair is the peer's to open (fact 2).
        if (_hasLivePeripheralPathForServiceUuid(serviceUuid)) return false;
        // Mixed pair, no link yet: iOS must own the first link — dial on
        // sight.
        return true;
      }
      // iOS↔iOS: same dual-role protocol as any same-platform pair — the
      // tiebreaker picks who opens the first leg, and a live inbound
      // peripheral leg makes this dial the reverse leg. Unmeasured on
      // hardware; if iOS refuses the second link the dial times out and the
      // pair stays single-link with retries — never silently abandoned.
      return _isBleDialInitiator(serviceUuid) ||
          _hasLivePeripheralPathForServiceUuid(serviceUuid) ||
          _firstMoverFallbackElapsed(existing);
    }

    // Non-iOS facing an iOS peer: yield the first dial (constraint 1 — only
    // iOS can open it). Once its inbound leg lands, the dial below IS our
    // reverse leg (fact 2); the fallback covers an iOS peer that never
    // dials, where a single us-central first link still beats no link.
    if (peerIsIos) {
      return _hasLivePeripheralPathForServiceUuid(serviceUuid) ||
          _firstMoverFallbackElapsed(existing);
    }

    // Same-platform (non-iOS) pair: deterministic first-mover; the
    // non-initiator dials only as reverse leg or fallback.
    return _isBleDialInitiator(serviceUuid) ||
        _hasLivePeripheralPathForServiceUuid(serviceUuid) ||
        _firstMoverFallbackElapsed(existing);
  }

  /// Lowercase hex of [pubkey], the rotation-stable peer key.
  String _pubkeyHexOf(Uint8List pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Memoized per-slot candidate sets: pubkeyHex → (slot, lowercase UUIDs).
  /// One entry per identified peer, refreshed when the slot advances. Avoids
  /// recomputing 3 SHA-256 digests per peer on every advertisement
  /// ([_pubkeyForAdvertisedUuid] runs on the scan hot path).
  final Map<String, (int, Set<String>)> _candidateUuidCache = {};

  /// The service UUIDs (lowercase) under which [pubkey] may currently be
  /// advertising: previous, current, and next slot. Recognition always
  /// matches against this set — never a single derived UUID — so
  /// unsynchronized clocks and slot boundaries can't break it.
  Set<String> _candidateUuidsForPubkey(Uint8List pubkey) {
    final hex = _pubkeyHexOf(pubkey);
    final slot = GrassrootsIdentity.currentBleSlot();
    final cached = _candidateUuidCache[hex];
    if (cached != null && cached.$1 == slot) return cached.$2;
    final uuids = GrassrootsIdentity.candidateServiceUuids(pubkey)
        .map((u) => u.toLowerCase())
        .toSet();
    _candidateUuidCache[hex] = (slot, uuids);
    return uuids;
  }

  /// Resolve an advertised (rotating) service UUID to the pubkey of an
  /// already-identified peer via candidate matching, or null when the beacon
  /// doesn't correspond to any peer we know (pre-ANNOUNCE stranger).
  Uint8List? _pubkeyForAdvertisedUuid(String serviceUuid) {
    final normalized = serviceUuid.toLowerCase();
    for (final peer in _peersState.peersList) {
      if (_candidateUuidsForPubkey(peer.publicKey).contains(normalized)) {
        return peer.publicKey;
      }
    }
    return null;
  }

  /// Whether the identified peer [pubkey] is known to be an iOS device: a
  /// platform-marker sighting recorded under its pubkey, or under any of its
  /// current-window slot UUIDs (promoted to the pubkey key on hit, since a
  /// peer's platform never changes but its beacon rotates away).
  bool _isIosPeerPubkey(Uint8List pubkey) {
    final hex = _pubkeyHexOf(pubkey);
    if (_iosMarkedPubkeyHexes.contains(hex)) return true;
    if (_candidateUuidsForPubkey(pubkey)
        .any(_iosMarkedServiceUuids.containsKey)) {
      _iosMarkedPubkeyHexes.add(hex);
      return true;
    }
    return false;
  }

  /// Whether the peer behind [serviceUuid] has been seen advertising the iOS
  /// platform marker this session. Sightings are recorded in
  /// [_onAdvertisement]; membership is sticky because a peer's platform never
  /// changes, while the marker itself comes and goes with iOS foregrounding.
  /// Checks the raw UUID sighting first, then — for beacons resolvable to an
  /// identified peer — the rotation-stable pubkey record.
  bool _isIosPeerServiceUuid(String serviceUuid) {
    if (_iosMarkedServiceUuids.containsKey(serviceUuid.toLowerCase())) {
      return true;
    }
    final pubkey = _pubkeyForAdvertisedUuid(serviceUuid);
    return pubkey != null && _isIosPeerPubkey(pubkey);
  }

  /// Whether [adv] carries the fixed iOS platform marker
  /// ([grassrootsIosLocalName]) in its local name. iOS surfaces a scanned
  /// local name as `advertisedName`; Android surfaces the scan-response name
  /// there too, with the GAP-cached name in `platformName` — check both.
  /// Absence proves nothing (backgrounded iOS drops the name), which is why
  /// every marker-dependent branch in [_shouldOpenCentralLeg] has a fallback.
  bool _advertisementCarriesIosMarker(ble.BleAdvertisement adv) {
    return adv.advertisedName == ble.grassrootsIosLocalName ||
        adv.platformName == ble.grassrootsIosLocalName;
  }

  /// Cold-start tie-breaker between same-platform peers: the endpoint with
  /// the lexicographically smaller PUBLIC KEY is the initiator and opens the
  /// first (central) leg; the other waits for that inbound leg and then opens
  /// its reverse central leg via [_maybeDialReverseCentralForPubkey]. Same
  /// rule as over IP (spec `GLP_Networking_API` §BLE Discovery) — and it must
  /// be the pubkey, not the service UUID: the rotating suffix flips UUID
  /// order between slots, so a UUID comparison would no longer give both
  /// peers opposite verdicts.
  ///
  /// When the beacon isn't resolvable to a known pubkey (open-mode cold call,
  /// pre-ANNOUNCE), fall back to comparing our own current-slot UUID against
  /// the observed one. Both sides compare the same two strings, so verdicts
  /// stay complementary except transiently across a slot boundary — a
  /// collision or stall there is recovered by the connect timeout and
  /// [firstMoverFallback], exactly like any other cold-call race. The same
  /// backstops cover asymmetric knowledge (we know the peer's pubkey but they
  /// don't know ours, e.g. after their reinstall): the two sides then apply
  /// different predicates and may briefly both dial or both wait.
  bool _isBleDialInitiator(String peerServiceUuid) {
    final peerPubkey = _pubkeyForAdvertisedUuid(peerServiceUuid);
    if (peerPubkey != null) {
      return _pubkeyHexOf(identity.publicKey)
              .compareTo(_pubkeyHexOf(peerPubkey)) <
          0;
    }
    return identity.bleServiceUuid.toLowerCase().compareTo(
              peerServiceUuid.toLowerCase(),
            ) <
        0;
  }

  /// Whether we've been seeing [existing] long enough that the initiator has
  /// had its chance and we (the non-initiator) should fall back to dialing.
  /// A just-discovered (or absent) entry is never elapsed — the initiator gets
  /// the first move.
  bool _firstMoverFallbackElapsed(DiscoveredPeerState? existing) {
    if (existing == null) return false;
    return DateTime.now().difference(existing.discoveredAt) >=
        firstMoverFallback;
  }

  /// True when we already hold a live inbound peripheral path to the peer whose
  /// pubkey derives [serviceUuid]. Lets a non-initiator that has already been
  /// dialed open its reverse central leg from [_onAdvertisement] instead of
  /// being held back by the first-mover gate. Companion to
  /// [_hasLiveCentralPathForServiceUuid].
  bool _hasLivePeripheralPathForServiceUuid(String serviceUuid) {
    final normalized = serviceUuid.toLowerCase();
    for (final peer in _peersState.peersList) {
      if (!_candidateUuidsForPubkey(peer.publicKey).contains(normalized)) {
        continue;
      }
      final peripheralId = peer.blePeripheralDeviceId;
      return peripheralId != null && isDeviceConnected(peripheralId);
    }
    return false;
  }

  /// Bridge for the peripheral-ready event in `_onPathChanged`. Two ways
  /// to identify the dial target, tried in order:
  ///
  /// 1. **Identity-based** (correct for modern Android with BLE privacy):
  ///    if ANNOUNCE has already landed, look up the peer's pubkey via the
  ///    peripheral pathId and dial a discovered advertising MAC that
  ///    matches their derived service UUID.
  ///
  /// 2. **Same-address fallback** (older Android stacks, iOS, simulators,
  ///    and any platform where the advertising MAC equals the connection
  ///    MAC): dial `central:<peripheral's_remoteId>` if scan already saw
  ///    that exact remoteId advertising as Grassroots.
  ///
  /// Pre-ANNOUNCE-AND-no-same-address-discovery, there's nothing useful
  /// to do here. The dial is retried from [associatePeerWithPubkey] when
  /// identity arrives, or from `_onAdvertisement` when a fresh advertising
  /// MAC for this peer lands.
  void _maybeDialReverseCentralAfterPeripheralReady(ble.BlePath path) {
    if (store.state.settings.bleRoleMode != BleRoleMode.auto) return;
    if (path.role != ble.BleRole.peripheral || !_isReady(path)) return;

    final pubkey = getPubkeyForPeerId(path.pathId);
    if (pubkey != null) {
      _maybeDialReverseCentralForPubkey(pubkey);
      return;
    }

    // Same-address fallback. Strip `peripheral:` and look up the matching
    // `central:` discovery. Only proceed when the scanner actually saw the
    // remote advertising as a Grassroots peripheral (so the dial has a
    // known-good target).
    const peripheralPrefix = 'peripheral:';
    if (!path.pathId.startsWith(peripheralPrefix)) return;
    final remoteId = path.pathId.substring(peripheralPrefix.length);
    final centralPathId = 'central:$remoteId';

    final existingCentral = _paths[centralPathId];
    if (existingCentral != null &&
        (existingCentral.state == ble.BlePathState.connecting ||
            existingCentral.state == ble.BlePathState.connected ||
            existingCentral.state == ble.BlePathState.subscribed ||
            existingCentral.state == ble.BlePathState.ready)) {
      return;
    }
    if (_peersState.getDiscoveredBlePeer(centralPathId) == null) return;
    if (_inFlightCentralDials() >= _maxInFlightCentralDials) return;

    debugPrint(
      '[ble] reverse leg (same-address fallback): peripheral path '
      '${path.pathId} is ready; dialing $centralPathId.',
    );
    unawaited(connectToDevice(centralPathId));
  }

  /// Try to open the central direction for a peer we already hold a
  /// peripheral path to. Keyed by the peer's **identity** (derived service
  /// UUID), NOT by the radio MAC they connected to us from: modern Android
  /// stacks use BLE-privacy with distinct advertising and initiator
  /// addresses, so a peer's connection MAC almost never appears in scan
  /// results. Looking up the dial target by service UUID lets us pick an
  /// advertising MAC the scanner has actually seen (and therefore one with
  /// a live Grassroots GATT server) rather than blindly dialing the
  /// connection MAC, which would always fail.
  ///
  /// Idempotent and safe to call repeatedly. Triggered from:
  ///   - peripheral-path-ready (when ANNOUNCE has already landed),
  ///   - `associatePeerWithPubkey` (when ANNOUNCE just identified the peer),
  ///   - `_onAdvertisement` (when a fresh advertising MAC appears for a
  ///     peer we have peripheral-only).
  void _maybeDialReverseCentralForPubkey(Uint8List pubkey) {
    if (store.state.settings.bleRoleMode != BleRoleMode.auto) return;
    // On iOS, only toward iOS peers: the reverse leg toward a non-iOS peer
    // is the measured-broken second link, owned by the non-iOS side (see
    // [_shouldOpenCentralLeg]). Toward iOS peers we attempt it — dual-role
    // mandate.
    if (defaultTargetPlatform == TargetPlatform.iOS &&
        !_isIosPeerPubkey(pubkey)) {
      return;
    }

    final peer = _peersState.getPeerByPubkey(pubkey);
    if (peer == null) return;
    // Already have central? Nothing to do. Already have an in-flight dial
    // for the central side? Likewise — `_paths` tracks every state the
    // plugin has surfaced for the central pathIds we've requested.
    if (peer.bleCentralDeviceId != null) return;
    if (peer.blePeripheralDeviceId == null) return; // not actually peripheral-attached

    final candidateUuids = _candidateUuidsForPubkey(pubkey);

    // Skip if any central pathId for this identity's current-window UUIDs is
    // mid-handshake. (Across-MAC, since the discovery map can hold multiple
    // rotated MACs.)
    final alreadyDialing = _paths.values.any((p) {
      if (p.role != ble.BleRole.central) return false;
      if (p.state != ble.BlePathState.connecting &&
          p.state != ble.BlePathState.connected &&
          p.state != ble.BlePathState.subscribed &&
          p.state != ble.BlePathState.ready) {
        return false;
      }
      final discoveredUuid =
          _peersState.getDiscoveredBlePeer(p.pathId)?.serviceUuid?.toLowerCase();
      return discoveredUuid != null && candidateUuids.contains(discoveredUuid);
    });
    if (alreadyDialing) return;

    if (_inFlightCentralDials() >= _maxInFlightCentralDials) return;

    // Pick any discovered advertising MAC whose beacon matches this peer's
    // current-window slot UUIDs and isn't itself in a connecting/connected
    // state. If none exist yet we bail — the next advertisement that lands
    // for this peer will re-trigger us (`_onAdvertisement` calls back here).
    final candidate = candidateUuids
        .expand(_peersState.getDiscoveredBlePeersByServiceUuid)
        .firstWhere(
          (dp) => !dp.isConnected && !dp.isConnecting,
          orElse: () => _noCandidate,
        );
    if (identical(candidate, _noCandidate)) {
      // Loud on purpose: a peripheral-attached peer with no discovered
      // advertising MAC is the signature of a muted scanner (the pair then
      // silently stays single-link). The scan watchdog restarts a silent
      // scanner; this log is the breadcrumb tying the two together.
      debugPrint(
        '[ble] reverse leg: ${peer.displayName} is peripheral-attached but '
        'no advertising MAC for their identity has been discovered — cannot '
        'dial the reverse leg until an advertisement arrives.',
      );
      return;
    }

    debugPrint(
      '[ble] reverse leg: dialing ${candidate.transportId} for peer '
      '${peer.displayName} (peripheral up, opening central direction).',
    );
    unawaited(connectToDevice(candidate.transportId));
  }

  /// Sentinel returned by `firstWhere` when no candidate matches.
  /// Constructed once because `DiscoveredPeerState` requires positional args.
  static final DiscoveredPeerState _noCandidate = DiscoveredPeerState(
    transportId: '',
    rssi: 0,
    discoveredAt: DateTime.fromMillisecondsSinceEpoch(0),
    lastSeen: DateTime.fromMillisecondsSinceEpoch(0),
  );

  void _onPathChanged(ble.BlePath path) {
    final previous = _paths[path.pathId];
    _paths[path.pathId] = path;

    final role =
        path.role == ble.BleRole.central ? BleRole.central : BleRole.peripheral;

    switch (path.state) {
      case ble.BlePathState.discovered:
        // Already handled by _onAdvertisement for central; for peripheral
        // we only see paths once the central connects.
        break;
      case ble.BlePathState.connecting:
        if (path.role == ble.BleRole.central) {
          store.dispatch(BleDeviceConnectingAction(path.pathId));
        }
        break;
      case ble.BlePathState.connected:
      case ble.BlePathState.subscribed:
        // Not yet sendable — wait for `ready`.
        break;
      case ble.BlePathState.ready:
        if (previous?.state != ble.BlePathState.ready) {
          store.dispatch(BleDeviceConnectedAction(path.pathId));
          _addConnectionEvent(TransportConnectionEvent(
            peerId: path.pathId,
            transport: TransportType.ble,
            connected: true,
            reason: role.name,
            isIncoming: role == BleRole.peripheral,
          ));
          _maybeDialReverseCentralAfterPeripheralReady(path);
        }
        break;
      case ble.BlePathState.failed:
      case ble.BlePathState.disconnected:
      case ble.BlePathState.stale:
        // A reform teardown (wrong-order mixed pair) has completed its
        // disconnect round-trip; allow future reforms for this pathId.
        _reformingCentralPathIds.remove(path.pathId);
        if (path.state == ble.BlePathState.failed &&
            path.role == ble.BleRole.central) {
          store.dispatch(BleDeviceConnectionFailedAction(path.pathId));
        }
        // Mirror the connect emit at the `ready` case: surface a disconnect
        // to the upper layer only on a true transition out of `ready`.
        // Failed dials from `connecting` never produced a "connected" event,
        // and re-emits of an already-dead path (the iOS `.failed → .disconnected`
        // pair, and scan re-discoveries that keep firing path-changed with
        // the cached state) would otherwise spam disconnect logs.
        if (previous?.state == ble.BlePathState.ready) {
          _emitDisconnect(path, role);
        }
        _paths.remove(path.pathId);
        break;
    }
  }

  void _emitDisconnect(ble.BlePath path, BleRole role) {
    if (path.role == ble.BleRole.central) {
      store.dispatch(BleDeviceDisconnectedAction(path.pathId));
    }
    final pubkey = getPubkeyForPeerId(path.pathId);
    if (pubkey != null) {
      onPeerBleDisconnected(pubkey, role: role);
    }
    _addConnectionEvent(TransportConnectionEvent(
      peerId: path.pathId,
      transport: TransportType.ble,
      connected: false,
      reason: path.error ?? role.name,
    ));
  }

  void _addConnectionEvent(TransportConnectionEvent event) {
    if (_connectionController.isClosed) return;
    _connectionController.add(event);
  }

  void _onPayload(ble.BlePayload payload) {
    if (_stopped) return;
    // Drop payloads unless the plugin currently marks the path ready. This
    // prevents late ANNOUNCE packets, hot-restart leftovers, or connected-but-
    // not-sendable paths from populating PeerState BLE role fields.
    final path = _paths[payload.pathId];
    if (path == null || !_isReady(path)) return;

    final role = payload.role == ble.BleRole.central
        ? BleRole.central
        : BleRole.peripheral;
    onPacketReceived(
      payload.value,
      fromDeviceId: payload.pathId,
      rssi: payload.rssi,
      bleRole: role,
    );
    if (_dataController.isClosed) return;
    _dataController.add(TransportDataEvent(
      peerId: payload.pathId,
      transport: TransportType.ble,
      data: payload.value,
    ));
  }

  // ===== Helpers =====

  void _setState(TransportState newState) {
    if (store.state.transports.bleState != newState) {
      store.dispatch(BleTransportStateChangedAction(newState));
    }
  }

  bool _isReady(ble.BlePath path) =>
      path.state == ble.BlePathState.ready && path.canSend;

  Iterable<ble.BlePath> get _readyPaths => _paths.values.where(_isReady);

  /// Closed-mode recognition: resolve an advertised (rotating) beacon to an
  /// accepted friend's pubkey by matching the friend's current+adjacent-slot
  /// UUIDs, or null when no friend matches.
  Uint8List? _friendPubkeyForDerivedServiceUuid(String serviceUuid) {
    final normalized = serviceUuid.toLowerCase();
    for (final peer in _peersState.friends) {
      if (_candidateUuidsForPubkey(peer.publicKey).contains(normalized)) {
        return peer.publicKey;
      }
    }
    return null;
  }

  BleRole? _roleFromPathId(String pathId) {
    if (pathId.startsWith('central:')) return BleRole.central;
    if (pathId.startsWith('peripheral:')) return BleRole.peripheral;
    return null;
  }

  /// Find the first service UUID that matches the Grassroots prefix.
  String? _firstGrassrootsServiceUuid(List<String?> uuids) {
    for (final uuid in uuids) {
      if (uuid == null) continue;
      final hex = uuid.toLowerCase().replaceAll('-', '');
      if (hex.startsWith(GrassrootsIdentity.grassrootsUuidPrefix)) {
        return uuid;
      }
    }
    return null;
  }

}
