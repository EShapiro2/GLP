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

/// The agent's public address, as a rendezvous server observed it (null to
/// clear).
class PublicAddressUpdatedAction extends TransportAction {
  final String? publicAddress;

  PublicAddressUpdatedAction(this.publicAddress);
}

/// Public IP (no port), as reflected by a rendezvous server.
class PublicIpUpdatedAction extends TransportAction {
  final String publicIp;

  PublicIpUpdatedAction(this.publicIp);
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
