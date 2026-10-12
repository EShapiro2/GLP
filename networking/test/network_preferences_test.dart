import 'dart:convert';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/network_preferences.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:shared_preferences/shared_preferences.dart';

/// The demo app's preferences: the underlays and the rendezvous servers it
/// instantiates the layer with (GNA §Instantiation), which a change applies by
/// rebuilding the layer.  Persisted, and read with no tolerance.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  final server = RendezvousServer(
    '[2001:db8::10]:9516',
    Uint8List.fromList(List.filled(32, 0xaa)),
  );
  final custom = NetworkPreferences(
    underlays: const {Underlay.pan, Underlay.wan},
    rendezvousServers: [server],
  );

  group('NetworkPreferences', () {
    test('the defaults run every underlay and no rendezvous server', () {
      expect(NetworkPreferences.defaults.underlays, Underlay.values.toSet());
      expect(NetworkPreferences.defaults.rendezvousServers, isEmpty);
    });

    test('round-trip through JSON', () {
      expect(NetworkPreferences.fromJson(custom.toJson()), custom);
    });

    test('a blob with any other key is malformed', () {
      final blob = custom.toJson()..['bluetoothEnabled'] = true;
      expect(() => NetworkPreferences.fromJson(blob), throwsFormatException);
    });

    test('a blob naming no underlay, or none at all, is malformed', () {
      expect(
          () => NetworkPreferences.fromJson(
              custom.toJson()..['underlays'] = ['pan', 'ble']),
          throwsFormatException);
      expect(
          () => NetworkPreferences.fromJson(
              custom.toJson()..['underlays'] = <String>[]),
          throwsFormatException);
    });

    test('a malformed server is malformed', () {
      expect(
          () => NetworkPreferences.fromJson(custom.toJson()
            ..['rendezvousServers'] = [
              {'address': 'nowhere', 'publicKey': 'aa' * 32},
            ]),
          throwsFormatException);
      expect(
          () => NetworkPreferences.fromJson(custom.toJson()
            ..['rendezvousServers'] = [
              {'address': '198.51.100.20:9514', 'publicKey': 'aa'},
            ]),
          throwsFormatException);
    });
  });

  group('NetworkPreferencesStore', () {
    test('saves and loads', () async {
      SharedPreferences.setMockInitialValues({});
      final store = NetworkPreferencesStore();
      await store.save(custom);
      expect(await NetworkPreferencesStore().load(), custom);
    });

    test('nothing stored loads the defaults', () async {
      SharedPreferences.setMockInitialValues({});
      expect(await NetworkPreferencesStore().load(),
          NetworkPreferences.defaults);
    });

    test('a malformed blob is discarded whole', () async {
      SharedPreferences.setMockInitialValues({
        'grassroots_demo_network_v1':
            jsonEncode({'underlays': ['pan'], 'rendezvousServers': 'none'}),
      });
      expect(await NetworkPreferencesStore().load(),
          NetworkPreferences.defaults);
    });
  });
}
