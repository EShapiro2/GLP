import '../transport/transport_service.dart';
import 'transports_state.dart';

/// Base class for transport state actions
abstract class TransportAction {}

/// Update BLE transport lifecycle state
class BleTransportStateChangedAction extends TransportAction {
  final TransportState state;
  final String? error;

  BleTransportStateChangedAction(this.state, {this.error});
}

/// Update UDP transport lifecycle state
class UdpTransportStateChangedAction extends TransportAction {
  final TransportState state;
  final String? error;

  UdpTransportStateChangedAction(this.state, {this.error});
}

/// BLE scanning state changed
class BleScanningChangedAction extends TransportAction {
  final bool scanning;

  BleScanningChangedAction(this.scanning);
}

/// Public address discovered or updated (null to clear)
class PublicAddressUpdatedAction extends TransportAction {
  final String? publicAddress;

  PublicAddressUpdatedAction(this.publicAddress);
}

/// Public IP (no port) discovered or reflected by a friend.
class PublicIpUpdatedAction extends TransportAction {
  final String publicIp;

  PublicIpUpdatedAction(this.publicIp);
}

/// Sets the publicAddressDiscoveryFailed flag in transports state.
///
/// Dispatched with `failed: true` when seeip-based discovery finishes without
/// producing an address. Dispatched with `failed: false` when a new attempt
/// starts (so the UI can hide the warning while retrying).
class PublicAddressDiscoveryFailedAction extends TransportAction {
  final bool failed;

  PublicAddressDiscoveryFailedAction(this.failed);
}

/// Clears the discovered public address and public IP.
class ClearPublicConnectivityAction extends TransportAction {}

/// Updates the current Internet connection type (Wi-Fi, cellular, etc.).
class NetworkConnectionTypeUpdatedAction extends TransportAction {
  final NetworkConnectionType connectionType;

  NetworkConnectionTypeUpdatedAction(this.connectionType);
}

/// Observed unsolicited inbound traffic at our [TransportsState.publicAddress]
/// — i.e. a peer successfully connected to us via UDP without a prior
/// hole-punch coordination.
class UnsolicitedInboundObservedAction extends TransportAction {
  /// When the inbound was observed (default: now).
  final DateTime observedAt;

  UnsolicitedInboundObservedAction({DateTime? observedAt})
    : observedAt = observedAt ?? DateTime.now();
}
