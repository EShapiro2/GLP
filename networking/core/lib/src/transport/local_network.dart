/// The local network the agent is attached to: the address prefixes its
/// interfaces sit in, and an opaque fingerprint of them.
///
/// Spec `docs/GLP_Networking_API/sections/ip.tex` §Local Network Identity.
/// Two consumers share one enumeration: the fingerprint answers
/// `networkIdentity()`, and the prefixes decide whether an inbound source
/// address is LAN contact (§Cold Calls — "an unsolicited inbound contact whose
/// source address lies within one of the agent's own local address prefixes is
/// LAN contact").
///
/// This is not the agent's public address. The attached local network can
/// change while the public address does not, and the public address can change
/// while the attached local network does not.
library;

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/dart.dart' show DartSha256;

import '../platform/compat.dart';

/// Prefix length used for IPv4 interface addresses.
///
/// Dart's [NetworkInterface] reports interface addresses without their
/// netmask, and no platform channel supplies one without an added dependency,
/// so the prefix length is the conventional subnet size of the family. /24 is
/// the ordinary LAN subnet; a wider real subnet is therefore split into
/// several prefixes and a peer in another part of it is not recognised as LAN.
/// That is the safe direction for a trust decision: the failure is a refused
/// LAN peer, never an accepted stranger.
const int kIPv4LocalPrefixLength = 24;

/// Prefix length used for IPv6 interface addresses — the SLAAC subnet.
const int kIPv6LocalPrefixLength = 64;

/// Domain separator for the network fingerprint.
const String _networkIdLabel = 'glp network id';

/// One local address prefix: a network address and its length in bits.
@immutable
class LocalPrefix {
  /// The prefix's network address — the interface address with the host bits
  /// zeroed.
  final InternetAddress network;

  /// The prefix length in bits.
  final int prefixLength;

  const LocalPrefix(this.network, this.prefixLength);

  /// Whether [address] lies within this prefix.
  ///
  /// An address of the other family never does.
  bool contains(InternetAddress address) {
    if (address.type != network.type) return false;
    final bytes = address.rawAddress;
    final networkBytes = network.rawAddress;
    if (bytes.length != networkBytes.length) return false;

    var remaining = prefixLength;
    for (var i = 0; i < bytes.length && remaining > 0; i++) {
      final bits = remaining >= 8 ? 8 : remaining;
      final mask = bits == 8 ? 0xFF : (0xFF << (8 - bits)) & 0xFF;
      if ((bytes[i] & mask) != (networkBytes[i] & mask)) return false;
      remaining -= bits;
    }
    return true;
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is LocalPrefix &&
          runtimeType == other.runtimeType &&
          network.address == other.network.address &&
          prefixLength == other.prefixLength;

  @override
  int get hashCode => Object.hash(network.address, prefixLength);

  @override
  String toString() => '${network.address}/$prefixLength';
}

/// The prefix [address] sits in, at the conventional subnet size of its family.
LocalPrefix localPrefixOf(InternetAddress address) {
  final length = address.type == InternetAddressType.IPv6
      ? kIPv6LocalPrefixLength
      : kIPv4LocalPrefixLength;

  final bytes = List<int>.from(address.rawAddress);
  var remaining = length;
  for (var i = 0; i < bytes.length; i++) {
    if (remaining >= 8) {
      remaining -= 8;
      continue;
    }
    final mask = remaining == 0 ? 0x00 : (0xFF << (8 - remaining)) & 0xFF;
    bytes[i] = bytes[i] & mask;
    remaining = 0;
  }

  return LocalPrefix(
    InternetAddress.fromRawAddress(
      Uint8List.fromList(bytes),
      type: address.type,
    ),
    length,
  );
}

/// A reading of the attached local network.
@immutable
class LocalNetwork {
  /// The agent's own local address prefixes, sorted, without duplicates.
  final List<LocalPrefix> prefixes;

  /// The opaque fingerprint of the attached local network, or null when the
  /// agent is attached to none and no constituent is readable.
  ///
  /// Best-effort and collision-prone, as the spec states: two distinct local
  /// networks commonly yield the same fingerprint, and anyone who can read or
  /// guess the constituents can reproduce it. A match is evidence that the
  /// agent is attached to the same local network as before, not proof of it.
  final String? networkId;

  const LocalNetwork({
    this.prefixes = const [],
    this.networkId,
  });

  static const LocalNetwork none = LocalNetwork();

  /// Whether [address] lies within one of the agent's own local prefixes.
  bool containsAddress(InternetAddress address) =>
      prefixes.any((prefix) => prefix.contains(address));

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is LocalNetwork &&
          runtimeType == other.runtimeType &&
          networkId == other.networkId &&
          listEquals(prefixes, other.prefixes);

  @override
  int get hashCode => Object.hash(networkId, Object.hashAll(prefixes));

  @override
  String toString() =>
      'LocalNetwork(id: $networkId, prefixes: ${prefixes.join(', ')})';
}

/// Read the attached local network from the platform's interface list.
///
/// One enumeration serves both the fingerprint and the prefixes. Loopback is
/// excluded — it is not a network the agent is attached to. Link-local
/// addresses are included: a self-assigned network is still a local network,
/// and two agents on one carry LAN contact between them.
///
/// The spec names three constituents of the fingerprint: the default gateway
/// address, the local address prefix, and the wireless network name where the
/// platform makes it readable. Only the prefixes are readable here — the
/// gateway address and the wireless network name need a platform channel this
/// package does not depend on, and on iOS the wireless network name needs an
/// entitlement, which the spec's "without additional permission" excludes.
Future<LocalNetwork> readLocalNetwork() async {
  final List<NetworkInterface> interfaces;
  try {
    interfaces = await NetworkInterface.list(
      includeLoopback: false,
      includeLinkLocal: true,
    );
  } catch (e) {
    debugPrint('Local network read failed: $e');
    return LocalNetwork.none;
  }

  final prefixes = <LocalPrefix>[];
  for (final interface in interfaces) {
    for (final address in interface.addresses) {
      if (address.isLoopback) continue;
      prefixes.add(localPrefixOf(address));
    }
  }

  return localNetworkFromPrefixes(prefixes);
}

/// Reads the agent's own interface addresses. [readLocalHostAddresses] is the
/// platform reading; a hermetic test supplies its own.
typedef LocalHostAddressReader = Future<List<InternetAddress>> Function();

/// The agent's own interface addresses — its host candidates for dialing.
///
/// These are what tell the layer which address families it can dial in, and
/// they let a link-local peer on the same segment be paired with a link-local
/// local endpoint. They are read from the same interface list as
/// [readLocalNetwork] but are deliberately not part of [LocalNetwork]: the
/// fingerprint is derived from prefixes and answers §Local Network Identity,
/// and a fresh temporary address within a prefix already held must not read as
/// a change of local network.
///
/// Loopback is excluded, as it is for the prefixes; link-local addresses are
/// included.
Future<List<InternetAddress>> readLocalHostAddresses() async {
  final List<NetworkInterface> interfaces;
  try {
    interfaces = await NetworkInterface.list(
      includeLoopback: false,
      includeLinkLocal: true,
    );
  } catch (e) {
    debugPrint('Local host address read failed: $e');
    return const [];
  }

  final addresses = <InternetAddress>[];
  final seen = <String>{};
  for (final interface in interfaces) {
    for (final address in interface.addresses) {
      if (address.isLoopback) continue;
      if (seen.add(address.address)) addresses.add(address);
    }
  }
  return List.unmodifiable(addresses);
}

/// Build a [LocalNetwork] from a collection of prefixes, deriving the
/// fingerprint.
///
/// Duplicates collapse and the order is fixed, so neither the prefix list nor
/// the fingerprint depends on how many interface addresses landed in one
/// subnet or on the order the platform listed them.
///
/// Separate from [readLocalNetwork] so the derivation is testable without a
/// live interface list.
LocalNetwork localNetworkFromPrefixes(Iterable<LocalPrefix> prefixes) {
  final sorted = prefixes.toSet().toList()
    ..sort((a, b) => a.toString().compareTo(b.toString()));

  if (sorted.isEmpty) return LocalNetwork.none;

  final constituents = sorted.map((p) => p.toString()).join('|');
  final digest = const DartSha256()
      .hashSync(utf8.encode('$_networkIdLabel|$constituents'))
      .bytes;

  final fingerprint = digest
      .sublist(0, 16)
      .map((b) => b.toRadixString(16).padLeft(2, '0'))
      .join();

  return LocalNetwork(prefixes: List.unmodifiable(sorted), networkId: fingerprint);
}
