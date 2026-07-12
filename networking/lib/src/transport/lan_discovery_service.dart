import 'dart:async';

import 'package:bonsoir/bonsoir.dart';
import 'package:flutter/foundation.dart';

import '../models/identity.dart';

/// LAN discovery per spec `GLP_Networking_API` §LAN Discovery.
///
/// Two agents on the same local network find each other and communicate
/// directly — no BLE radio and no rendezvous server. Discovery uses DNS-SD
/// over multicast DNS: the agent advertises an instance of the service type
/// `_grassroots._udp` on the local domain and browses for instances of it.
///
/// The advertised instance name is a rotating token — the first 8 bytes,
/// hex-encoded, of SHA-256("glp lan suffix" | pk | T), with the same 15-minute
/// slot T as the BLE suffix — and the instance's SRV record carries the
/// agent's LAN address and bound UDP port. The token lets anyone who knows
/// the agent's public key recognize it before contact (current and adjacent
/// slots); to anyone else it is an identifier that changes every slot. The
/// advertisement carries no key and no other identifying payload.
///
/// This service owns advertising, browsing, and slot rotation. Recognition
/// (matching browsed tokens against known peers' keys) and contact (UDX +
/// ANNOUNCE + Noise with the recognized key as the expected static) live in
/// the coordinator, reached through [onInstanceResolved].
class LanDiscoveryService {
  LanDiscoveryService({required this.identity});

  static const String serviceType = '_grassroots._udp';

  final GrassrootsIdentity identity;

  BonsoirBroadcast? _broadcast;
  BonsoirDiscovery? _discovery;
  StreamSubscription<BonsoirDiscoveryEvent>? _discoverySub;
  Timer? _slotTimer;
  int? _advertisedSlot;
  int? _port;
  bool _running = false;

  /// Fired when a browsed instance resolves: the rotating [token] (lowercase
  /// instance name) and the `ip:port` it advertises. The coordinator decides
  /// whether the token matches a known peer (Closed) or is a cold-call
  /// opportunity (Open).
  void Function(String token, String address)? onInstanceResolved;

  bool get isRunning => _running;

  /// Start advertising our current-slot token on [port] and browsing for
  /// other instances. Re-advertises on each slot boundary so the token
  /// rotates with the BLE suffix.
  Future<void> start({required int port}) async {
    if (_running) return;
    _running = true;
    _port = port;
    await _advertiseForCurrentSlot();
    await _startBrowsing();
    _scheduleSlotRotation();
    debugPrint('[lan] discovery started (port $port, '
        'token ${identity.lanToken})');
  }

  Future<void> _advertiseForCurrentSlot() async {
    final slot = GrassrootsIdentity.currentBleSlot();
    if (_advertisedSlot == slot && _broadcast != null) return;
    await _broadcast?.stop();
    final token =
        GrassrootsIdentity.deriveLanTokenForSlot(identity.publicKey, slot);
    final service = BonsoirService(
      name: token,
      type: serviceType,
      port: _port!,
    );
    final broadcast = BonsoirBroadcast(service: service);
    await broadcast.initialize();
    await broadcast.start();
    _broadcast = broadcast;
    _advertisedSlot = slot;
    debugPrint('[lan] advertising $token.$serviceType on port $_port');
  }

  Future<void> _startBrowsing() async {
    final discovery = BonsoirDiscovery(type: serviceType);
    await discovery.initialize();
    _discoverySub = discovery.eventStream?.listen((event) {
      switch (event) {
        case BonsoirDiscoveryServiceFoundEvent(:final service):
          // Resolve to obtain the SRV address/port.
          unawaited(service.resolve(discovery.serviceResolver));
        case BonsoirDiscoveryServiceResolvedEvent(:final service):
          _handleResolved(service);
        default:
          break;
      }
    });
    await discovery.start();
    _discovery = discovery;
  }

  void _handleResolved(BonsoirService service) {
    final token = service.name.toLowerCase();
    // Our own advertisement echoes back — ignore every token we may
    // currently be advertising under.
    if (GrassrootsIdentity.candidateLanTokens(identity.publicKey)
        .contains(token)) {
      return;
    }
    // Prefer an IPv4 literal when the platform reports several addresses.
    final addresses = service.hostAddresses;
    if (addresses.isEmpty) return;
    var host = addresses.firstWhere(
      (a) => a.contains('.') && !a.contains(':'),
      orElse: () => addresses.first,
    );
    if (host.isEmpty) return;
    // Normalize a trailing-dot mDNS hostname; prefer literal addresses when
    // the platform hands one back.
    if (host.endsWith('.')) host = host.substring(0, host.length - 1);
    final address = host.contains(':') && !host.startsWith('[')
        ? '[$host]:${service.port}'
        : '$host:${service.port}';
    debugPrint('[lan] resolved instance $token at $address');
    onInstanceResolved?.call(token, address);
  }

  void _scheduleSlotRotation() {
    _slotTimer?.cancel();
    final slotMs = GrassrootsIdentity.bleSlotDuration.inMilliseconds;
    final nowMs = DateTime.now().millisecondsSinceEpoch;
    final untilNext = slotMs - (nowMs % slotMs) + 1000;
    _slotTimer = Timer(Duration(milliseconds: untilNext), () async {
      if (!_running) return;
      try {
        await _advertiseForCurrentSlot();
      } catch (e) {
        debugPrint('[lan] slot re-advertise failed: $e');
      }
      _scheduleSlotRotation();
    });
  }

  Future<void> stop() async {
    _running = false;
    _slotTimer?.cancel();
    _slotTimer = null;
    await _discoverySub?.cancel();
    _discoverySub = null;
    await _discovery?.stop();
    _discovery = null;
    await _broadcast?.stop();
    _broadcast = null;
    _advertisedSlot = null;
  }
}

/// Resolve a browsed LAN token against a set of known public keys: the key
/// whose current-or-adjacent-slot token matches, or null when no known peer
/// matches (spec §LAN Discovery — under Closed trust the token serves only
/// to recognize known peers, and unmatched instances are ignored).
Uint8List? matchLanToken(String token, Iterable<Uint8List> knownKeys,
    {DateTime? now}) {
  final normalized = token.toLowerCase();
  for (final key in knownKeys) {
    if (GrassrootsIdentity.candidateLanTokens(key, now: now)
        .contains(normalized)) {
      return key;
    }
  }
  return null;
}
