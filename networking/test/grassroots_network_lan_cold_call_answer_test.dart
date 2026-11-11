import 'dart:async';
import 'dart:io' show InternetAddress;
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

/// A LAN cold call is a call, not an answer (GNA §3.2, LAN: "the peers
/// exchange ANNOUNCE ... and run the XX handshake"; Implementation Notes: "a
/// clear ANNOUNCE is answered once per connection").  Alice opens her place
/// and calls Bob, whose place is still closed, so he ignores her ANNOUNCE.
/// When Bob opens and calls Alice, she answers with her ANNOUNCE, and the two
/// meet.  Until this test, Alice's ignored call counted as her answer: she
/// never answered Bob, and Bob refused her handshake for want of her
/// ANNOUNCE (phone run of 2026-11-11, an Android phone and an iOS simulator
/// on one Wi-Fi, in the order of p1-two-phones-lan, steps 2 and 3).
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    SharedPreferences.setMockInitialValues({});
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  String hex(Uint8List key) =>
      key.map((byte) => byte.toRadixString(16).padLeft(2, '0')).join();

  /// A node on loopback, its local network injected so that loopback is LAN
  /// contact (decision 8), and the address it listens on.
  Future<(GrassrootsNetwork, String)> startNode(GrassrootsIdentity id) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        // No heartbeat inside the test: its tick re-reads the local network.
        announceInterval: Duration(minutes: 30),
      ),
    );
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    addTearDown(network.dispose);
    await stateWhere(store, (s) => s.localNetwork.networkId != null);
    store.dispatch(LocalNetworkChangedAction(localNetworkFromPrefixes(
        [localPrefixOf(InternetAddress('127.0.0.1'))])));
    await hosts.read;
    final candidates = network.debugLocalCandidates();
    expect(candidates, isNotEmpty);
    return (network, candidates.first);
  }

  /// Let a contact that should not happen have the time to happen: no event
  /// shows that it did not.
  Future<void> settle() async {
    for (var i = 0; i < 10; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
  }

  /// Tokens no known key matches: each instance is a stranger's.
  const aliceToken = 'grs-00000000000000000000000000000001';
  const bobToken = 'grs-00000000000000000000000000000002';

  test('a call ignored under Closed trust leaves the callee\'s own call '
      'answered once it opens', timeout: Timeout.none, () async {
    final aliceId = await identityFromSeed(0x81);
    final bobId = await identityFromSeed(0x82);
    final (alice, aliceAddress) = await startNode(aliceId);
    final (bob, bobAddress) = await startNode(bobId);

    final bobMeetsAlice = Completer<Underlay>();
    bob.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(aliceId.publicKey) && !bobMeetsAlice.isCompleted) {
        bobMeetsAlice.complete(underlay);
      }
    };
    final aliceMeetsBob = Completer<Underlay>();
    alice.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(bobId.publicKey) && !aliceMeetsBob.isCompleted) {
        aliceMeetsBob.complete(underlay);
      }
    };

    // Alice opens her place and calls Bob, whose place is closed.
    await alice.lan!.setTrustLevel(ColdCallTrustLevel.open);
    await bob.lan!.setTrustLevel(ColdCallTrustLevel.closed);
    alice.debugResolveLanInstance(bobToken, bobAddress);
    await settle();
    expect(bob.isPeerReachable(aliceId.publicKey), isFalse,
        reason: 'Closed ignores the call of a stranger');

    // Bob opens his place and calls Alice: she answers, and they meet.
    await bob.lan!.setTrustLevel(ColdCallTrustLevel.open);
    bob.debugResolveLanInstance(aliceToken, aliceAddress);
    expect(await bobMeetsAlice.future, Underlay.lan);
    expect(await aliceMeetsBob.future, Underlay.lan);
  });

  test('an ANNOUNCE answered on a connection that has since closed is '
      'answered again on the next connection at the same address',
      timeout: Timeout.none, () async {
    final aliceId = await identityFromSeed(0x83);
    final bobId = await identityFromSeed(0x84);
    final (alice, aliceAddress) = await startNode(aliceId);
    final (bob, _) = await startNode(bobId);
    await alice.lan!.setTrustLevel(ColdCallTrustLevel.open);
    await bob.lan!.setTrustLevel(ColdCallTrustLevel.open);

    // Each meeting completes the next completer; a lost session sends the
    // test to the next meeting.
    var bobMeets = Completer<void>();
    var aliceMeets = Completer<void>();
    final bobLoses = Completer<void>();
    final aliceLoses = Completer<void>();
    bob.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(aliceId.publicKey) && !bobMeets.isCompleted) {
        bobMeets.complete();
      }
    };
    alice.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(bobId.publicKey) && !aliceMeets.isCompleted) {
        aliceMeets.complete();
      }
    };
    bob.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) == hex(aliceId.publicKey) && !bobLoses.isCompleted) {
        bobLoses.complete();
      }
    };
    alice.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) == hex(bobId.publicKey) && !aliceLoses.isCompleted) {
        aliceLoses.complete();
      }
    };

    // Bob calls Alice and she answers: the first connection.
    bob.debugResolveLanInstance(aliceToken, aliceAddress);
    await bobMeets.future;
    await aliceMeets.future;

    // The connection closes on both sides, as two silent intervals close it.
    bobMeets = Completer<void>();
    aliceMeets = Completer<void>();
    await alice.debugDropIpPath(Underlay.lan, bobId.publicKey);
    await bob.debugDropIpPath(Underlay.lan, aliceId.publicKey);
    await aliceLoses.future;
    await bobLoses.future;

    // Bob calls again from the same address: a new connection, and Alice's
    // answer on the first one does not stand in for an answer on this one.
    bob.debugResolveLanInstance(aliceToken, aliceAddress);
    await bobMeets.future;
    await aliceMeets.future;
  });
}
