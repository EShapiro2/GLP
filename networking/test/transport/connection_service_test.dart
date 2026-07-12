import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/transport/address_utils.dart';
import 'package:grassroots_networking/src/transport/connection_service.dart';

void main() {
  group('UdpConnectionService', () {
    const service = UdpConnectionService();

    AddressInfo addr(String value) => parseAddressString(value)!;

    test('prefers same-subnet link-local over IPv6 and IPv4', () {
      final pair = service.selectBestPair(
        localCandidates: {
          addr('[2606:4700::1]:5000'),
          addr('198.51.100.10:5001'),
          addr('[fe80::1]:5002'),
        },
        remoteCandidates: {
          addr('198.51.100.20:6001'),
          addr('[2606:4700::2]:6000'),
          addr('[fe80::2]:6002'),
        },
      );

      expect(pair, isNotNull);
      expect(pair!.priority, AddressPairPriority.linkLocalSameSubnet);
      expect(pair.remote.toAddressString(), '[fe80::2]:6002');
    });

    test('prefers IPv6 over IPv4 when link-local is unavailable', () {
      final pair = service.selectBestPair(
        localCandidates: {
          addr('[2606:4700::1]:5000'),
          addr('198.51.100.10:5001'),
        },
        remoteCandidates: {
          addr('198.51.100.20:6001'),
          addr('[2606:4700::2]:6000'),
        },
      );

      expect(pair, isNotNull);
      expect(pair!.priority, AddressPairPriority.ipv6);
      expect(pair.remote.ip.type, InternetAddressType.IPv6);
    });

    test('selects IPv4 when both sides only have IPv4', () {
      final pair = service.selectBestPair(
        localCandidates: {addr('198.51.100.10:5001')},
        remoteCandidates: {addr('198.51.100.20:6001')},
      );

      expect(pair, isNotNull);
      expect(pair!.priority, AddressPairPriority.ipv4);
      expect(pair.remote.ip.type, InternetAddressType.IPv4);
    });

    test('returns null when families do not overlap', () {
      final pair = service.selectBestPair(
        localCandidates: {addr('[2606:4700::1]:5000')},
        remoteCandidates: {addr('198.51.100.20:6001')},
      );

      expect(pair, isNull);
    });

    test('checks all advertised candidates from both peers', () {
      final pair = service.selectBestPairFromAddresses(
        localAddresses: {
          '198.51.100.10:5001',
        },
        remoteAddresses: {
          '[2606:4700::2]:6000',
          '198.51.100.20:6001',
        },
      );

      expect(pair, isNotNull);
      expect(pair!.priority, AddressPairPriority.ipv4);
      expect(pair.local.ip.type, InternetAddressType.IPv4);
      expect(pair.remote.toAddressString(), '198.51.100.20:6001');
    });

    test('does not treat wildcard bind addresses as usable candidates', () {
      final pair = service.selectBestPairFromAddresses(
        localAddresses: {
          '[::]:5000',
          '0.0.0.0:5001',
        },
        remoteAddresses: {
          '[2606:4700::2]:6000',
          '198.51.100.20:6001',
        },
      );

      expect(pair, isNull);
    });
  });
}
