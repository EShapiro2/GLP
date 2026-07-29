import 'dart:io';

import 'package:grassroots_networking_core/src/store/app_state.dart';
import 'package:grassroots_networking_core/src/store/local_network_actions.dart';
import 'package:grassroots_networking_core/src/store/reducers.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';
import 'package:test/test.dart';

LocalPrefix prefixOf(String address) =>
    localPrefixOf(InternetAddress(address));

void main() {
  group('localPrefixOf', () {
    test('masks an IPv4 address to its /24 network', () {
      final prefix = prefixOf('192.168.1.37');
      expect(prefix.prefixLength, kIPv4LocalPrefixLength);
      expect(prefix.network.address, '192.168.1.0');
      expect(prefix.toString(), '192.168.1.0/24');
    });

    test('masks an IPv6 address to its /64 network', () {
      final prefix = prefixOf('2001:db8:1:2:aaaa:bbbb:cccc:dddd');
      expect(prefix.prefixLength, kIPv6LocalPrefixLength);
      expect(prefix.network.address, '2001:db8:1:2::');
    });

    test('two addresses on one subnet yield the same prefix', () {
      expect(prefixOf('10.0.5.1'), prefixOf('10.0.5.254'));
    });

    test('two addresses on different subnets yield different prefixes', () {
      expect(prefixOf('10.0.5.1'), isNot(prefixOf('10.0.6.1')));
    });
  });

  group('LocalPrefix.contains', () {
    test('holds for an address inside the prefix', () {
      expect(prefixOf('192.168.1.37').contains(InternetAddress('192.168.1.9')),
          isTrue);
    });

    test('fails for an address outside the prefix', () {
      expect(prefixOf('192.168.1.37').contains(InternetAddress('192.168.2.9')),
          isFalse);
    });

    test('fails across address families', () {
      expect(prefixOf('192.168.1.37').contains(InternetAddress('::1')), isFalse);
      expect(prefixOf('2001:db8::1').contains(InternetAddress('192.168.1.1')),
          isFalse);
    });

    test('holds for an IPv6 address in the same /64', () {
      final prefix = prefixOf('2001:db8:1:2:aaaa::1');
      expect(prefix.contains(InternetAddress('2001:db8:1:2:ffff::9')), isTrue);
      expect(prefix.contains(InternetAddress('2001:db8:1:3::9')), isFalse);
    });
  });

  group('localNetworkFromPrefixes', () {
    test('no prefixes yields no fingerprint', () {
      final network = localNetworkFromPrefixes(const []);
      expect(network.networkId, isNull);
      expect(network.prefixes, isEmpty);
      expect(network, LocalNetwork.none);
    });

    test('the fingerprint is stable for the same prefixes', () {
      final a = localNetworkFromPrefixes([prefixOf('192.168.1.5')]);
      final b = localNetworkFromPrefixes([prefixOf('192.168.1.200')]);
      expect(a.networkId, isNotNull);
      expect(a.networkId, b.networkId);
    });

    test('the fingerprint does not depend on enumeration order', () {
      final a = localNetworkFromPrefixes(
          [prefixOf('192.168.1.5'), prefixOf('10.0.0.5')]);
      final b = localNetworkFromPrefixes(
          [prefixOf('10.0.0.5'), prefixOf('192.168.1.5')]);
      expect(a.networkId, b.networkId);
    });

    test('a different local network yields a different fingerprint', () {
      final home = localNetworkFromPrefixes([prefixOf('192.168.1.5')]);
      final office = localNetworkFromPrefixes([prefixOf('10.11.12.5')]);
      expect(home.networkId, isNot(office.networkId));
    });

    test('duplicate prefixes collapse', () {
      final once = localNetworkFromPrefixes([prefixOf('192.168.1.5')]);
      final twice = localNetworkFromPrefixes(
          [prefixOf('192.168.1.5'), prefixOf('192.168.1.6')]);
      expect(twice.prefixes.length, 1);
      expect(twice.networkId, once.networkId);
    });

    test('containsAddress ranges over every prefix', () {
      final network = localNetworkFromPrefixes(
          [prefixOf('192.168.1.5'), prefixOf('10.0.0.5')]);
      expect(network.containsAddress(InternetAddress('192.168.1.99')), isTrue);
      expect(network.containsAddress(InternetAddress('10.0.0.99')), isTrue);
      expect(network.containsAddress(InternetAddress('172.16.0.1')), isFalse);
    });
  });

  group('readLocalNetwork', () {
    test('reads the machine\'s interfaces without throwing', () async {
      final network = await readLocalNetwork();
      // A machine with no non-loopback interface yields no fingerprint; one
      // with any interface yields a fingerprint and at least one prefix.
      if (network.prefixes.isEmpty) {
        expect(network.networkId, isNull);
      } else {
        expect(network.networkId, isNotNull);
        expect(network.networkId, hasLength(32));
      }
    });

    test('excludes loopback', () async {
      final network = await readLocalNetwork();
      expect(network.containsAddress(InternetAddress('127.0.0.1')), isFalse);
    });
  });

  group('local-network slice', () {
    test('starts empty', () {
      expect(AppState.initial.localNetwork.networkId, isNull);
      expect(AppState.initial.localNetwork.prefixes, isEmpty);
    });

    test('a change action records the new network', () {
      final network = localNetworkFromPrefixes([prefixOf('192.168.1.5')]);
      final next =
          appReducer(AppState.initial, LocalNetworkChangedAction(network));
      expect(next.localNetwork.network, network);
      expect(next.localNetwork.networkId, network.networkId);
    });

    test('the local network is independent of the public address', () {
      final network = localNetworkFromPrefixes([prefixOf('192.168.1.5')]);
      final next =
          appReducer(AppState.initial, LocalNetworkChangedAction(network));
      expect(next.transports, AppState.initial.transports);
      expect(next.transports.publicAddress, isNull);
    });
  });
}
