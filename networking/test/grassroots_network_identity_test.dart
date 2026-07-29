import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Spec conformance for the local-network identity calls (GLP Networking API
/// §Local Network Identity): `networkIdentity()` returns an opaque fingerprint
/// of the attached local network, or null when it is attached to none, and
/// `onNetworkChanged(cb)` fires with `(oldId, newId)` when it changes. The
/// fingerprint is independent of the public address in both directions.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    final messenger =
        TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
    messenger.setMockMethodCallHandler(
      const MethodChannel('dev.fluttercommunity.plus/connectivity'),
      (call) async => ['wifi'],
    );
    messenger.setMockStreamHandler(
      const EventChannel('dev.fluttercommunity.plus/connectivity_status'),
      MockStreamHandler.inline(onListen: (arguments, events) {}),
    );
  });

  Future<GrassrootsIdentity> identity(String nickname) async {
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPair(),
      nickname: nickname,
    );
  }

  LocalNetwork networkOf(String address) =>
      localNetworkFromPrefixes([localPrefixOf(InternetAddress(address))]);

  test('networkIdentity reports the held fingerprint', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    final home = networkOf('192.168.1.5');
    store.dispatch(LocalNetworkChangedAction(home));

    expect(network.networkIdentity(), home.networkId);
    await network.dispose();
  });

  test('networkIdentity is null when attached to no local network', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    store.dispatch(LocalNetworkChangedAction(LocalNetwork.none));

    expect(network.networkIdentity(), isNull);
    await network.dispose();
  });

  test('the fingerprint is independent of the public address', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    final home = networkOf('192.168.1.5');
    store.dispatch(LocalNetworkChangedAction(home));
    final before = network.networkIdentity();

    // The public address changes; the attached local network does not.
    store.dispatch(PublicAddressUpdatedAction('[2001:db8::1]:9514'));
    expect(network.networkIdentity(), before);

    // The attached local network changes; the public address does not.
    store.dispatch(LocalNetworkChangedAction(networkOf('10.11.12.5')));
    expect(network.networkIdentity(), isNot(before));
    expect(store.state.transports.publicAddress, '[2001:db8::1]:9514');

    await network.dispose();
  });

  test('the layer reads the attached local network at construction', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    final expected = await readLocalNetwork();
    // The constructor's read is in flight; give it a moment to land.
    for (var i = 0; i < 50 && network.networkIdentity() == null; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 10));
    }

    expect(network.networkIdentity(), expected.networkId);
    await network.dispose();
  });

  test('a public-address change does not fire onNetworkChanged', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    // Let the construction-time read settle before watching for changes.
    for (var i = 0; i < 50 && network.networkIdentity() == null; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 10));
    }

    final networkChanges = <String?>[];
    network.onNetworkChanged = (oldId, newId) => networkChanges.add(newId);

    store.dispatch(PublicAddressUpdatedAction('[2001:db8::1]:9514'));
    await Future<void>.delayed(const Duration(milliseconds: 50));

    expect(networkChanges, isEmpty);
    await network.dispose();
  });

  test('onNetworkChanged fires with the old and new fingerprint', () async {
    final self = await identity('Self');
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    final changes = <(String?, String?)>[];
    network.onNetworkChanged = (oldId, newId) => changes.add((oldId, newId));

    // The construction-time read reports the first attachment: null -> id.
    for (var i = 0; i < 50 && changes.isEmpty; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 10));
    }

    if (changes.isNotEmpty) {
      expect(changes.single.$1, isNull);
      expect(changes.single.$2, network.networkIdentity());
    } else {
      // No non-loopback interface on this machine: attached to no local
      // network, so there is nothing to report.
      expect(network.networkIdentity(), isNull);
    }
    await network.dispose();
  });
}
