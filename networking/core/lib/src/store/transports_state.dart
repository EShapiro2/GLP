import 'package:flutter/foundation.dart';
import '../transport/transport_service.dart';
import '../transport/address_utils.dart';

enum NetworkConnectionType { wifi, cellular, ethernet, vpn, other, offline }

extension NetworkConnectionTypeX on NetworkConnectionType {
  String get displayName {
    switch (this) {
      case NetworkConnectionType.wifi:
        return 'Wi-Fi';
      case NetworkConnectionType.cellular:
        return 'Cellular';
      case NetworkConnectionType.ethernet:
        return 'Ethernet';
      case NetworkConnectionType.vpn:
        return 'VPN';
      case NetworkConnectionType.other:
        return 'Other';
      case NetworkConnectionType.offline:
        return 'Offline';
    }
  }
}

/// Per-transport lifecycle state for Redux store.
///
/// Tracks the lifecycle state of each transport independently,
/// replacing the old single global `TransportConnectionStatus`.
@immutable
class TransportsState {
  /// BLE transport lifecycle state
  final TransportState bleState;

  /// UDP transport lifecycle state
  final TransportState udpState;

  /// Error message for BLE transport (if in error state)
  final String? bleError;

  /// Error message for UDP transport (if in error state)
  final String? udpError;

  /// Whether BLE is currently scanning
  final bool bleScanning;

  /// Our discovered public UDP address (ip:port), null if not yet discovered
  final String? publicAddress;

  /// Our public IP address (no port), for display purposes.
  /// Set even when behind NAT. Updated by seeip.org discovery and friend
  /// reflection. IPv6-only.
  final String? publicIp;

  /// The current IP network type used for Internet connectivity.
  final NetworkConnectionType networkConnectionType;

  /// When we last observed unsolicited inbound traffic at [publicAddress]
  /// — i.e. a peer connected to us via UDP without us first coordinating
  /// a hole-punch.
  ///
  /// Bound to [publicAddress]: cleared whenever the address changes, since
  /// any prior observation was for a different network path.
  final DateTime? lastUnsolicitedInboundAt;

  /// True when the most recent public address discovery attempt finished
  /// without producing an address AND no friend/RV reflection has filled
  /// one in either. Cleared automatically as soon as a public address or
  /// reflected IP arrives, or when a new discovery attempt is in flight.
  final bool publicAddressDiscoveryFailed;

  const TransportsState({
    this.bleState = TransportState.uninitialized,
    this.udpState = TransportState.uninitialized,
    this.bleError,
    this.udpError,
    this.bleScanning = false,
    this.publicAddress,
    this.publicIp,
    this.networkConnectionType = NetworkConnectionType.offline,
    this.lastUnsolicitedInboundAt,
    this.publicAddressDiscoveryFailed = false,
  });

  static const TransportsState initial = TransportsState();

  /// Whether any transport is active
  bool get isAnyActive =>
      bleState == TransportState.active || udpState == TransportState.active;

  /// Whether the system is in a healthy state (any transport active)
  bool get isHealthy => isAnyActive;

  /// Whether this device has a publicly routable address.
  bool get hasPublicAddress =>
      publicAddress != null && isGloballyRoutableAddress(publicAddress!);

  /// Whether this device is well-connected: it has a globally routable UDP
  /// address and can advertise itself as a signaling facilitator.
  bool get isWellConnected => hasPublicAddress;

  /// Overall status display string derived from per-transport states
  String get statusDisplayString {
    if (isAnyActive) {
      if (bleScanning) return 'Scanning for peers...';
      return 'Online';
    }
    if (bleState == TransportState.initializing ||
        udpState == TransportState.initializing) {
      return 'Starting...';
    }
    if (bleState == TransportState.ready || udpState == TransportState.ready) {
      return 'Ready';
    }
    if (bleState == TransportState.error || udpState == TransportState.error) {
      return bleError ?? udpError ?? 'Error';
    }
    return 'Initializing...';
  }

  TransportsState copyWith({
    TransportState? bleState,
    TransportState? udpState,
    String? bleError,
    String? udpError,
    bool? bleScanning,
    String? publicAddress,
    String? publicIp,
    NetworkConnectionType? networkConnectionType,
    DateTime? lastUnsolicitedInboundAt,
    bool? publicAddressDiscoveryFailed,
  }) {
    return TransportsState(
      bleState: bleState ?? this.bleState,
      udpState: udpState ?? this.udpState,
      bleError: bleError ?? this.bleError,
      udpError: udpError ?? this.udpError,
      bleScanning: bleScanning ?? this.bleScanning,
      publicAddress: publicAddress ?? this.publicAddress,
      publicIp: publicIp ?? this.publicIp,
      networkConnectionType:
          networkConnectionType ?? this.networkConnectionType,
      lastUnsolicitedInboundAt:
          lastUnsolicitedInboundAt ?? this.lastUnsolicitedInboundAt,
      publicAddressDiscoveryFailed:
          publicAddressDiscoveryFailed ?? this.publicAddressDiscoveryFailed,
    );
  }

  /// Create a copy with publicAddress explicitly cleared (set to null).
  /// Keeps publicIp — the IP is still valid even if the full address isn't.
  /// Also clears lastUnsolicitedInboundAt because it was bound to the address.
  TransportsState clearPublicAddress() {
    return TransportsState(
      bleState: bleState,
      udpState: udpState,
      bleError: bleError,
      udpError: udpError,
      bleScanning: bleScanning,
      publicAddress: null,
      publicIp: publicIp,
      networkConnectionType: networkConnectionType,
      lastUnsolicitedInboundAt: null,
      publicAddressDiscoveryFailed: publicAddressDiscoveryFailed,
    );
  }

  /// Create a copy with both publicAddress and publicIp cleared.
  /// Also clears lastUnsolicitedInboundAt because it was bound to the address.
  TransportsState clearPublicConnectivity() {
    return TransportsState(
      bleState: bleState,
      udpState: udpState,
      bleError: bleError,
      udpError: udpError,
      bleScanning: bleScanning,
      publicAddress: null,
      publicIp: null,
      networkConnectionType: networkConnectionType,
      lastUnsolicitedInboundAt: null,
      publicAddressDiscoveryFailed: publicAddressDiscoveryFailed,
    );
  }

  /// Create a copy with publicAddress changed to a new value, clearing the
  /// reachability observation because it was bound to the previous address.
  /// Also clears the discovery-failed flag — we now have an address.
  TransportsState withNewPublicAddress(String address) {
    return TransportsState(
      bleState: bleState,
      udpState: udpState,
      bleError: bleError,
      udpError: udpError,
      bleScanning: bleScanning,
      publicAddress: address,
      publicIp: publicIp,
      networkConnectionType: networkConnectionType,
      lastUnsolicitedInboundAt: null,
      publicAddressDiscoveryFailed: false,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is TransportsState &&
          runtimeType == other.runtimeType &&
          bleState == other.bleState &&
          udpState == other.udpState &&
          bleError == other.bleError &&
          udpError == other.udpError &&
          bleScanning == other.bleScanning &&
          publicAddress == other.publicAddress &&
          publicIp == other.publicIp &&
          networkConnectionType == other.networkConnectionType &&
          lastUnsolicitedInboundAt == other.lastUnsolicitedInboundAt &&
          publicAddressDiscoveryFailed == other.publicAddressDiscoveryFailed;

  @override
  int get hashCode => Object.hash(
    bleState,
    udpState,
    bleError,
    udpError,
    bleScanning,
    publicAddress,
    publicIp,
    networkConnectionType,
    lastUnsolicitedInboundAt,
    publicAddressDiscoveryFailed,
  );

  @override
  String toString() =>
      'TransportsState(ble: $bleState, udp: $udpState, scanning: $bleScanning, publicAddr: $publicAddress, publicIp: $publicIp, network: ${networkConnectionType.displayName}, wellConnected: $isWellConnected, discoveryFailed: $publicAddressDiscoveryFailed)';
}
