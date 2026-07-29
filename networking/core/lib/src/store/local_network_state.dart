import '../platform/compat.dart';
import '../transport/local_network.dart';

/// The attached local network, held apart from `TransportsState`.
///
/// Spec `docs/GLP_Networking_API/sections/ip.tex` §Local Network Identity. The
/// public address (in `TransportsState`) and the attached local network are
/// independent in both directions, so they are separate state, changed by
/// separate actions, and reported by separate callbacks.
@immutable
class LocalNetworkState {
  /// The current reading of the attached local network.
  final LocalNetwork network;

  const LocalNetworkState({this.network = LocalNetwork.none});

  static const LocalNetworkState initial = LocalNetworkState();

  /// The opaque fingerprint, or null when the agent is attached to no local
  /// network and no constituent is readable.
  String? get networkId => network.networkId;

  /// The agent's own local address prefixes.
  List<LocalPrefix> get prefixes => network.prefixes;

  LocalNetworkState copyWith({LocalNetwork? network}) =>
      LocalNetworkState(network: network ?? this.network);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is LocalNetworkState &&
          runtimeType == other.runtimeType &&
          network == other.network;

  @override
  int get hashCode => network.hashCode;

  @override
  String toString() => 'LocalNetworkState($network)';
}
