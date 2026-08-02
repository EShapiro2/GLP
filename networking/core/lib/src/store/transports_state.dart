import '../platform/compat.dart';
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

  /// Our public UDP address (ip:port), null until a rendezvous server has
  /// observed and reflected one. The agent cannot observe it itself.
  final String? publicAddress;

  /// Our public IP address (no port), for display purposes.
  /// Set even when behind NAT, by rendezvous address reflection.
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
    );
  }

  /// Create a copy with publicAddress changed to a new value, clearing the
  /// reachability observation because it was bound to the previous address.
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
          lastUnsolicitedInboundAt == other.lastUnsolicitedInboundAt;

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
  );

  @override
  String toString() =>
      'TransportsState(ble: $bleState, udp: $udpState, scanning: $bleScanning, publicAddr: $publicAddress, publicIp: $publicIp, network: ${networkConnectionType.displayName}, wellConnected: $isWellConnected)';
}
