import 'dart:typed_data';

import '../platform/compat.dart';
import '../transport/address_utils.dart';

/// A rendezvous server the layer connects to over the WAN, as GLP declared
/// it, by its public key and its address (GNA §4.2: "The rendezvous servers
/// the layer connects to are those GLP has declared, each by its public key
/// and its address, and GLP adds and removes them while the layer runs").
///
/// Both profiles record each server `wan.putRendezvousServer(pk, address)`
/// declares as one of these, and drop it on `wan.removeRendezvousServer(pk)`.
@immutable
class RendezvousServer {
  /// The server's static, globally-routable `ip:port`, canonical (IPv4, or
  /// bracketed IPv6 as `[ip]:port`).
  final String address;

  /// The server's own Ed25519 public key, 32 bytes.
  final Uint8List publicKey;

  const RendezvousServer._(this.address, this.publicKey);

  /// A rendezvous server at [address] holding [publicKey].  Throws
  /// [ArgumentError] on an unparseable address or a key that is not 32
  /// bytes, as `putPeerAddress` refuses an unparseable address.
  factory RendezvousServer(String address, Uint8List publicKey) {
    final parsed = parseAddressString(address);
    if (parsed == null) {
      throw ArgumentError.value(address, 'address', 'not a valid ip:port');
    }
    if (publicKey.length != 32) {
      throw ArgumentError.value(
        publicKey.length,
        'publicKey',
        'a rendezvous server key is 32 bytes',
      );
    }
    return RendezvousServer._(
      parsed.toAddressString(),
      Uint8List.fromList(publicKey),
    );
  }

  /// Lowercase hex of [publicKey].
  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is RendezvousServer &&
          address == other.address &&
          pubkeyHex == other.pubkeyHex;

  @override
  int get hashCode => Object.hash(address, pubkeyHex);

  @override
  String toString() =>
      'RendezvousServer($address, ${pubkeyHex.substring(0, 8)}…)';
}
