import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Spec conformance for the known-peer supply calls (GLP Networking API
/// §Cold-Call Trust Levels + §Connectivity and Address): the layer recognizes
/// only keys GLP supplies through its API — `putKnownPeer` adds a key,
/// `putPeerAddress` supplies an address AND the key (a dial-book entry even
/// for a peer never seen before), and `removeKnownPeer` withdraws both the
/// recognition and the recorded address.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  // GrassrootsNetwork's constructor subscribes to connectivity changes;
  // stub the plugin channels so construction works in a test isolate.
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

  String hexOf(List<int> pubkey) =>
      pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  test('putKnownPeer adds the key to the recognition set, idempotently',
      () async {
    final self = await identity('Self');
    final peer = await identity('Peer');
    final peerHex = hexOf(peer.publicKey);
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    network.putKnownPeer(peer.publicKey);

    expect(store.state.knownPeers.isKnown(peerHex), isTrue);
    expect(store.state.knownPeers.addressOf(peerHex), isNull);
    // The peer also gets a minimal identity record so BLE recognition and
    // reconnection sweeps have something to range over.
    expect(store.state.peers.getPeerByPubkeyHex(peerHex), isNotNull);

    // Idempotent: a second put changes nothing and never clears an address.
    network.putPeerAddress(peer.publicKey, '203.0.113.5:4001');
    network.putKnownPeer(peer.publicKey);

    expect(store.state.knownPeers.addressOf(peerHex), equals('203.0.113.5:4001'));
  });

  test('putPeerAddress supplies the key along with the address', () async {
    final self = await identity('Self');
    final peer = await identity('Peer');
    final peerHex = hexOf(peer.publicKey);
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    // A peer never seen before: putPeerAddress creates the dial-book entry
    // AND makes the key known (Closed-mode recognition covers it).
    network.putPeerAddress(peer.publicKey, '203.0.113.5:4001');

    expect(store.state.knownPeers.isKnown(peerHex), isTrue);
    expect(store.state.knownPeers.addressOf(peerHex), equals('203.0.113.5:4001'));
    expect(store.state.knownPeers.dialBook, equals({peerHex: '203.0.113.5:4001'}));
    final record = store.state.peers.getPeerByPubkeyHex(peerHex);
    expect(record, isNotNull);
    expect(record!.udpAddress, equals('203.0.113.5:4001'));
    // Supply-only, never a live session.
    expect(record.isReachable, isFalse);
  });

  test('putPeerAddress rejects an unparseable address and supplies nothing',
      () async {
    final self = await identity('Self');
    final peer = await identity('Peer');
    final peerHex = hexOf(peer.publicKey);
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    expect(
      () => network.putPeerAddress(peer.publicKey, 'not-an-address'),
      throwsArgumentError,
    );

    expect(store.state.knownPeers.isKnown(peerHex), isFalse);
    expect(store.state.peers.getPeerByPubkeyHex(peerHex), isNull);
  });

  test('removeKnownPeer drops recognition and the recorded address',
      () async {
    final self = await identity('Self');
    final peer = await identity('Peer');
    final peerHex = hexOf(peer.publicKey);
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final network =
        GrassrootsNetwork(identity: self, store: store, sodium: sodium);

    network.putPeerAddress(peer.publicKey, '203.0.113.5:4001');
    expect(store.state.knownPeers.isKnown(peerHex), isTrue);

    network.removeKnownPeer(peer.publicKey);

    expect(store.state.knownPeers.isKnown(peerHex), isFalse);
    expect(store.state.knownPeers.addressOf(peerHex), isNull);
    expect(store.state.knownPeers.dialBook, isEmpty);

    // Withdrawal is not amnesia about the wire: re-supplying works.
    network.putKnownPeer(peer.publicKey);
    expect(store.state.knownPeers.isKnown(peerHex), isTrue);
  });
}
