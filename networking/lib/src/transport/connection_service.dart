import 'dart:io';

import 'address_utils.dart';

/// A selected local/remote UDP address pair.
class AddressCandidatePair {
  final AddressInfo local;
  final AddressInfo remote;
  final AddressPairPriority priority;

  const AddressCandidatePair({
    required this.local,
    required this.remote,
    required this.priority,
  });

  @override
  String toString() => 'AddressCandidatePair(${local.toAddressString()} -> '
      '${remote.toAddressString()}, ${priority.name})';
}

/// Candidate pair priority, ordered from best to worst.
enum AddressPairPriority {
  linkLocalSameSubnet,
  ipv6,
  ipv4,
}

/// Selects the best UDP address pair from local and remote candidates.
///
/// Only same-family pairs are eligible. Selection priority:
/// 1. Link-local addresses on the same subnet
/// 2. IPv6
/// 3. IPv4
class UdpConnectionService {
  const UdpConnectionService();

  AddressCandidatePair? selectBestPairFromAddresses({
    required Iterable<String> localAddresses,
    required Iterable<String> remoteAddresses,
  }) {
    return selectBestPair(
      localCandidates: parseAddressCandidates(localAddresses),
      remoteCandidates: parseAddressCandidates(remoteAddresses),
    );
  }

  AddressCandidatePair? selectBestPair({
    required Iterable<AddressInfo> localCandidates,
    required Iterable<AddressInfo> remoteCandidates,
  }) {
    AddressCandidatePair? best;

    for (final local in localCandidates) {
      for (final remote in remoteCandidates) {
        final priority = _priorityFor(local.ip, remote.ip);
        if (priority == null) continue;

        final pair = AddressCandidatePair(
          local: local,
          remote: remote,
          priority: priority,
        );
        if (best == null || priority.index < best.priority.index) {
          best = pair;
        }
      }
    }

    return best;
  }

  AddressPairPriority? _priorityFor(
    InternetAddress local,
    InternetAddress remote,
  ) {
    if (local.type != remote.type) return null;
    if (_isUnspecified(local) || _isUnspecified(remote)) return null;

    if (_isSameSubnetLinkLocalPair(local, remote)) {
      return AddressPairPriority.linkLocalSameSubnet;
    }

    if (_isLinkLocal(local) || _isLinkLocal(remote)) {
      return null;
    }

    if (remote.type == InternetAddressType.IPv6) {
      return AddressPairPriority.ipv6;
    }
    if (remote.type == InternetAddressType.IPv4) {
      return AddressPairPriority.ipv4;
    }
    return null;
  }

  bool _isSameSubnetLinkLocalPair(
    InternetAddress local,
    InternetAddress remote,
  ) {
    if (!_isLinkLocal(local) || !_isLinkLocal(remote)) return false;
    if (local.type != remote.type) return false;

    final localBytes = local.rawAddress;
    final remoteBytes = remote.rawAddress;
    if (local.type == InternetAddressType.IPv4) {
      return localBytes.length == 4 &&
          remoteBytes.length == 4 &&
          localBytes[0] == remoteBytes[0] &&
          localBytes[1] == remoteBytes[1];
    }

    if (local.type == InternetAddressType.IPv6) {
      if (localBytes.length != 16 || remoteBytes.length != 16) return false;
      for (var i = 0; i < 8; i++) {
        if (localBytes[i] != remoteBytes[i]) return false;
      }
      return true;
    }

    return false;
  }

  bool _isUnspecified(InternetAddress address) {
    final bytes = address.rawAddress;
    if (address.type == InternetAddressType.IPv4) {
      return bytes.length == 4 && bytes.every((byte) => byte == 0);
    }
    if (address.type == InternetAddressType.IPv6) {
      return bytes.length == 16 && bytes.every((byte) => byte == 0);
    }
    return false;
  }

  bool _isLinkLocal(InternetAddress address) {
    if (address.isLinkLocal) return true;
    if (address.type != InternetAddressType.IPv4) return false;
    final bytes = address.rawAddress;
    return bytes.length == 4 && bytes[0] == 169 && bytes[1] == 254;
  }
}
