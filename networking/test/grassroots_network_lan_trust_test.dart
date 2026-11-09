import 'dart:async';
import 'dart:convert';
import 'dart:io' show InternetAddress;

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

/// Unsolicited inbound IP contact from outside the agent's own local network
/// is governed by no trust level (spec §Trust levels): it is refused unless
/// the peer is in the known-peer set. LAN contact
/// arrives on the same path, a LAN session being an IP session, so the layer
/// tells the two apart by the source address alone — a source within one of
/// the agent's own local address prefixes is LAN contact, governed by the LAN
/// level; any other source is governed by no level.
///
/// Two full [GrassrootsNetwork] instances talk over loopback UDP. Loopback is
/// a local prefix (GNA 582ef60), so two layers on one machine are LAN contact
/// to each other, which the last test holds with the machine's own prefixes;
/// the others inject the receiver's prefixes to place the caller inside or
/// outside them, which is exactly the distinction under test.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    // The layer keeps its fair-delivery queue in the application's
    // SharedPreferences (GNA §2.10); a fresh store for every test.
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

  /// Each node's host-address reader, which tells when its candidates are
  /// read.
  final hostReaders = Expando<LoopbackHostReader>();

  Future<(GrassrootsNetwork, Store<AppState>)> startNode(
    GrassrootsIdentity id, {
    Set<Underlay> underlays = const {Underlay.lan, Underlay.wan},
  }) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: underlays,
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        // The heartbeat tick re-reads the attached local network, which would
        // replace the prefixes these tests inject. Long enough that no tick
        // fires inside the test window; each test asserts the injected
        // prefixes were still in force when the decision was taken.
        announceInterval: Duration(minutes: 30),
      ),
    );
    hostReaders[network] = hosts;
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    // The layer reads the attached local network once at construction, in
    // the background. A test that injects prefixes before that read lands has
    // them overwritten by the machine's real ones, so wait for it: a machine
    // with any non-loopback interface reports a fingerprint, and one with none
    // reads nothing that would overwrite the injection.
    await stateWhere(store, (s) => s.localNetwork.networkId != null);
    return (network, store);
  }

  /// Where a node listens, as its own host candidates report it.
  ///
  /// Not `getPublicAddress()`: that stays null until a rendezvous server
  /// reflects an address (spec §Connectivity and Address), and this test has
  /// none. The host candidates are read asynchronously at UDP init, so wait
  /// for them.
  Future<String?> listeningAddress(GrassrootsNetwork network) async {
    await hostReaders[network]!.read;
    final candidates = network.debugLocalCandidates();
    return candidates.isEmpty ? null : candidates.first;
  }

  /// The receiver's local prefixes, forced so that loopback — the only source
  /// address available in a test — falls inside or outside them.
  final loopbackIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('127.0.0.1'))]);
  final elsewhereIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('192.168.7.5'))]);

  /// An unknown caller dials the receiver and sends. Returns whether the
  /// receiver answered — completed a session with a key it does not know.
  /// Where the test expects an answer ([answers]), the wait is on the
  /// receiver holding the caller reachable; where it expects none, no event
  /// shows a refusal, and the wait is a fixed span.
  Future<bool> unknownCallerAnswered({
    required ColdCallTrustLevel lanLevel,
    required LocalNetwork receiverLocalNetwork,
    required bool answers,
  }) async {
    final callerId = await identityFromSeed(0x61);
    final receiverId = await identityFromSeed(0x62);

    final (caller, _) = await startNode(callerId);
    final (receiver, receiverStore) = await startNode(receiverId);
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });

    expect(await listeningAddress(caller), isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);

    receiverStore.dispatch(LocalNetworkChangedAction(receiverLocalNetwork));
    await receiver.lan!.setTrustLevel(lanLevel);

    // The caller learns where to dial; the receiver is told nothing about the
    // caller, so the caller's contact is unsolicited and its key unknown.
    caller.wan!.putPeerAddress(
      receiverId.publicKey,
      receiverAddress!,
    );
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('lan cold call')),
    );

    if (answers) {
      await stateWhere(receiverStore,
          (_) => receiver.isPeerReachable(callerId.publicKey));
    } else {
      await Future<void>.delayed(const Duration(seconds: 12));
    }
    final answered = receiver.isPeerReachable(callerId.publicKey);

    expect(
      receiverStore.state.localNetwork.network,
      receiverLocalNetwork,
      reason: 'the injected prefixes must still be the ones in force, or the '
          'test decided under the machine\'s real local network',
    );
    return answered;
  }

  // GNA 582ef60: "an unsolicited inbound contact whose source address lies
  // within one of the agent's own local address prefixes, a loopback address
  // among them, is LAN contact, governed by the LAN level" (Section 2.6), and
  // the appendix: "two agents on one machine are LAN contact to each other".
  // No prefix is injected: the receiver's are the machine's own, read by the
  // layer, and loopback is among them.  Its LAN Open answers an unknown
  // caller on loopback, which no level would admit as WAN contact, and the
  // session is reported LAN.
  test('two layers on one machine meet over the LAN: an unknown caller on '
      'loopback is LAN contact under the machine\'s own prefixes', () async {
    final callerId = await identityFromSeed(0x7a);
    final receiverId = await identityFromSeed(0x7b);
    final callerHex = hexOf(callerId.publicKey);
    final (caller, _) = await startNode(callerId);
    final (receiver, receiverStore) = await startNode(receiverId);
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });
    expect(
        receiverStore.state.localNetwork.network
            .containsAddress(InternetAddress.loopbackIPv4),
        isTrue,
        reason: 'loopback is among the prefixes the layer read');

    final connected = Completer<Underlay>();
    receiver.onPeerConnected = (pk, underlay, application) {
      if (hexOf(pk) == callerHex && !connected.isCompleted) {
        connected.complete(underlay);
      }
    };
    await receiver.lan!.setTrustLevel(ColdCallTrustLevel.open);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);
    caller.wan!.putPeerAddress(receiverId.publicKey, receiverAddress!);
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('one machine')),
    );

    expect(await connected.future, Underlay.lan);
    expect(receiver.peerUnderlays(callerId.publicKey), [Underlay.lan]);
  }, timeout: Timeout.none);

  test('LAN Open answers an unknown caller from within a local prefix',
      () async {
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.open,
        receiverLocalNetwork: loopbackIsLocal,
        answers: true,
      ),
      isTrue,
    );
  }, timeout: Timeout.none);

  test('LAN Open does not answer a caller from outside every local prefix',
      () async {
    // Same level, same unknown key — only the source address differs, and
    // that alone decides. Contact from the Internet at large is governed by
    // no level, so no level can admit it.
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.open,
        receiverLocalNetwork: elsewhereIsLocal,
        answers: false,
      ),
      isFalse,
    );
  }, timeout: Timeout.none);

  test('LAN Closed does not answer an unknown caller on the local network',
      () async {
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.closed,
        receiverLocalNetwork: loopbackIsLocal,
        answers: false,
      ),
      isFalse,
    );
  }, timeout: Timeout.none);

  test(
      'over the WAN an unknown key is refused at the contact gate, and a key '
      'given by putKnownPeer is answered (GNA §2.5, §2.7)', () async {
    // A layer running the WAN alone: no level governs its IP contact, so the
    // known-peer set alone decides.
    final strangerId = await identityFromSeed(0x65);
    final knownId = await identityFromSeed(0x66);
    final receiverId = await identityFromSeed(0x67);

    final (stranger, _) = await startNode(strangerId);
    final (known, _) = await startNode(knownId);
    final (receiver, receiverStore) =
        await startNode(receiverId, underlays: const {Underlay.wan});
    addTearDown(() async {
      await stranger.dispose();
      await known.dispose();
      await receiver.dispose();
    });

    expect(await listeningAddress(stranger), isNotNull);
    expect(await listeningAddress(known), isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);
    receiverStore.dispatch(LocalNetworkChangedAction(elsewhereIsLocal));
    receiver.putKnownPeer(knownId.publicKey);

    /// Whether the receiver answers [caller]: where the test expects it
    /// ([answers]), the wait is on the receiver holding the caller
    /// reachable; where it expects no answer, no event shows the refusal,
    /// and the wait is a fixed span.
    Future<bool> answered(GrassrootsNetwork caller, Uint8List callerKey,
        {required bool answers}) async {
      caller.wan!.putPeerAddress(receiverId.publicKey, receiverAddress!);
      await caller.send(
          receiverId.publicKey, Uint8List.fromList(utf8.encode('wan call')));
      if (answers) {
        await stateWhere(
            receiverStore, (_) => receiver.isPeerReachable(callerKey));
      } else {
        await Future<void>.delayed(const Duration(seconds: 6));
      }
      return receiver.isPeerReachable(callerKey);
    }

    expect(await answered(stranger, strangerId.publicKey, answers: false),
        isFalse,
        reason: 'an unknown key');
    expect(await answered(known, knownId.publicKey, answers: true), isTrue,
        reason: 'a key given by putKnownPeer');
    expect(receiver.peerUnderlays(knownId.publicKey), [Underlay.wan]);
    expect(receiver.isPeerReachable(strangerId.publicKey), isFalse);
  }, timeout: Timeout.none);

  test('the BLE level does not open the IP path', () async {
    // The two levels are independent, and the medium of discovery is the key:
    // opening BLE says nothing about contact arriving over IP.
    final callerId = await identityFromSeed(0x63);
    final receiverId = await identityFromSeed(0x64);

    final (caller, _) = await startNode(callerId);
    // The receiver runs the PAN too, so that its PAN level can be set on its
    // pan sub-object (GNA §2.2).
    final (receiver, receiverStore) = await startNode(receiverId,
        underlays: const {Underlay.pan, Underlay.lan, Underlay.wan});
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });

    expect(await listeningAddress(caller), isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);

    receiverStore.dispatch(LocalNetworkChangedAction(loopbackIsLocal));
    await receiver.pan!.setTrustLevel(ColdCallTrustLevel.open);

    expect(receiver.store.state.settings.trustLevelOf(ProximityUnderlay.pan),
        ColdCallTrustLevel.open);
    expect(receiver.store.state.settings.trustLevelOf(ProximityUnderlay.lan),
        ColdCallTrustLevel.closed);

    caller.wan!.putPeerAddress(
      receiverId.publicKey,
      receiverAddress!,
    );
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('ble-open should not help')),
    );

    await Future<void>.delayed(const Duration(seconds: 6));
    expect(receiver.isPeerReachable(callerId.publicKey), isFalse);
    expect(receiverStore.state.localNetwork.network, loopbackIsLocal,
        reason: 'the caller was inside a local prefix throughout, so only the '
            'LAN level being Closed can have refused it');
  }, timeout: Timeout.none);

  /// A known caller dials the receiver and sends. Returns what the receiver
  /// reported of the caller: the underlays of its connects, of its received
  /// messages, and of its discoveries, and its `peerUnderlays` once the
  /// message has arrived.
  Future<
      ({
        List<Underlay> connected,
        List<Underlay> received,
        List<ProximityUnderlay> discovered,
        List<Underlay> reachable,
        List<ProximityUnderlay> nearby,
        String? observed,
        String? recorded,
        String callerAddress,
      })> receiverReportOfKnownCaller({
    required int seed,
    required LocalNetwork receiverLocalNetwork,
    Set<Underlay> receiverUnderlays = const {Underlay.lan, Underlay.wan},
    bool expectDelivery = true,
  }) async {
    final callerId = await identityFromSeed(seed);
    final receiverId = await identityFromSeed(seed + 1);
    final callerHex = hexOf(callerId.publicKey);

    final (caller, _) = await startNode(callerId);
    final (receiver, receiverStore) =
        await startNode(receiverId, underlays: receiverUnderlays);
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });

    final callerAddress = await listeningAddress(caller);
    expect(callerAddress, isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);

    receiverStore.dispatch(LocalNetworkChangedAction(receiverLocalNetwork));
    // A known peer is answered under every level and from every source, so
    // only the source address can decide the underlay.
    receiver.putKnownPeer(callerId.publicKey);

    final connected = <Underlay>[];
    final received = <Underlay>[];
    final discovered = <ProximityUnderlay>[];
    final firstReceived = Completer<void>();
    receiver.onPeerConnected = (pk, underlay, application) {
      if (hexOf(pk) == callerHex) connected.add(underlay);
    };
    receiver.onMessageReceived = (sender, payload, messageId, underlay) {
      if (hexOf(sender) != callerHex) return;
      received.add(underlay);
      if (!firstReceived.isCompleted) firstReceived.complete();
    };
    for (final underlay in ProximityUnderlay.values) {
      final proximity =
          underlay == ProximityUnderlay.pan ? receiver.pan : receiver.lan;
      proximity?.onPeerDiscovered = (pk) {
        if (hexOf(pk) == callerHex) discovered.add(underlay);
      };
    }

    caller.wan!.putPeerAddress(receiverId.publicKey, receiverAddress!);
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('which underlay')),
    );

    // Where delivery is expected, the wait is on it; where it is not, no
    // event shows the refusal, and the wait is a fixed span.
    if (expectDelivery) {
      await firstReceived.future;
    } else {
      await Future<void>.delayed(const Duration(seconds: 6));
    }
    if (expectDelivery) {
      expect(received, isNotEmpty,
          reason: 'the known caller must get through');
    }
    expect(
      receiverStore.state.localNetwork.network,
      receiverLocalNetwork,
      reason: 'the injected prefixes must still be the ones in force',
    );
    return (
      connected: connected,
      received: received,
      discovered: discovered,
      reachable: receiver.peerUnderlays(callerId.publicKey),
      nearby: [
        for (final (underlay, proximity) in [
          (ProximityUnderlay.pan, receiver.pan),
          (ProximityUnderlay.lan, receiver.lan),
        ])
          for (final pk in proximity?.getPeers() ?? const <Uint8List>[])
            if (hexOf(pk) == callerHex) underlay,
      ],
      // A layer running no WAN has no wan sub-object, and observes no address.
      observed: receiver.wan?.observedPeerAddress(callerId.publicKey),
      // The recorded WAN address of the caller in the receiver's known set.
      recorded: receiverStore.state.knownPeers.known[callerHex],
      callerAddress: callerAddress!,
    );
  }

  test(
      'a UDP session with a peer within a local prefix is LAN: connect, '
      'receive and discovery report it', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x71,
      receiverLocalNetwork: loopbackIsLocal,
    );
    expect(report.connected, [Underlay.lan]);
    expect(report.received, everyElement(Underlay.lan));
    expect(report.discovered, [ProximityUnderlay.lan]);
    expect(report.reachable, [Underlay.lan]);
    // GNA, 2026-10-11, Task C: a LAN peer is nearby, and the layer observes
    // it at the source address of its session.
    expect(report.nearby, [ProximityUnderlay.lan]);
    expect(report.observed, report.callerAddress);
  }, timeout: Timeout.none);

  test(
      'a UDP session with a peer outside every local prefix is WAN, and WAN '
      'contact is no discovery', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x73,
      receiverLocalNetwork: elsewhereIsLocal,
    );
    expect(report.connected, [Underlay.wan]);
    expect(report.received, everyElement(Underlay.wan));
    expect(report.discovered, isEmpty);
    expect(report.reachable, [Underlay.wan]);
    // GNA, 2026-10-11, Task C: a WAN peer is not nearby, and is observed.
    expect(report.nearby, isEmpty);
    expect(report.observed, report.callerAddress);
  }, timeout: Timeout.none);

  test(
      'a known peer on a WAN session has the address it is observed at '
      'recorded as its WAN address', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x7b,
      receiverLocalNetwork: elsewhereIsLocal,
    );
    expect(report.connected, [Underlay.wan]);
    // Spec ip.tex §Connectivity and Address: the observed address replaces the
    // earlier recorded one, and is recorded the same way GLP's is.
    expect(report.recorded, report.callerAddress);
  }, timeout: Timeout.none);

  test('a known peer on a LAN session is never recorded as its WAN address',
      () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x7d,
      receiverLocalNetwork: loopbackIsLocal,
    );
    expect(report.connected, [Underlay.lan]);
    expect(report.recorded, isNull);
  }, timeout: Timeout.none);

  // GNA, 2026-10-11, Q1: "an IP session's underlay is lan when LAN runs and
  // the address is local, else wan when WAN runs, else no session".

  test(
      'a layer running the WAN alone takes a peer within a local prefix as '
      'WAN, and finds no LAN discovery', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x75,
      receiverLocalNetwork: loopbackIsLocal,
      receiverUnderlays: const {Underlay.wan},
    );
    expect(report.connected, [Underlay.wan]);
    expect(report.received, everyElement(Underlay.wan));
    expect(report.discovered, isEmpty);
    expect(report.reachable, [Underlay.wan]);
  }, timeout: Timeout.none);

  test(
      'a layer running the LAN alone holds no session with a peer outside '
      'every local prefix, known though it is', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x77,
      receiverLocalNetwork: elsewhereIsLocal,
      receiverUnderlays: const {Underlay.lan},
      expectDelivery: false,
    );
    expect(report.connected, isEmpty);
    expect(report.received, isEmpty);
    expect(report.discovered, isEmpty);
    expect(report.reachable, isEmpty);
    expect(report.nearby, isEmpty);
    expect(report.observed, isNull);
  }, timeout: Timeout.none);

  test(
      'a layer running the LAN alone takes a peer within a local prefix as '
      'LAN', () async {
    final report = await receiverReportOfKnownCaller(
      seed: 0x79,
      receiverLocalNetwork: loopbackIsLocal,
      receiverUnderlays: const {Underlay.lan},
    );
    expect(report.connected, [Underlay.lan]);
    expect(report.received, everyElement(Underlay.lan));
    expect(report.reachable, [Underlay.lan]);
  }, timeout: Timeout.none);
}

String hexOf(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
