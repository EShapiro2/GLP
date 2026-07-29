import '../transport/local_network.dart';

/// Base class for local-network actions.
abstract class LocalNetworkAction {}

/// The attached local network was read and differs from the held one.
///
/// Dispatched only on a change: the reader compares before dispatching, so
/// every dispatch of this action is a network change the runtime is told about.
class LocalNetworkChangedAction extends LocalNetworkAction {
  final LocalNetwork network;

  LocalNetworkChangedAction(this.network);
}
