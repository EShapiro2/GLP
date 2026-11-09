import 'dart:async';
import 'dart:io'
    show
        InternetAddress,
        InternetAddressType,
        RawDatagramSocket,
        RawSocketEvent;

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
import 'package:grassroots_networking_core/src/transport/address_utils.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

/// Host candidates on both loopback families, so a node can be dialed at an
/// IPv4 address and at an IPv6 one; [read] completes once the layer holds
/// them (see [LoopbackHostReader]).
class _DualLoopbackHostReader {
  final Completer<void> _read = Completer<void>();

  Future<List<InternetAddress>> call() async {
    Timer.run(() {
      if (!_read.isCompleted) _read.complete();
    });
    return [InternetAddress.loopbackIPv4, InternetAddress.loopbackIPv6];
  }

  Future<void> get read => _read.future;
}

/// Sessions are per underlay (GNA §Session Establishment: "one handshake per
/// underlay per pair, so a pair reachable over more than one underlay holds a
/// session on each"; decision 32): a pair on one local network that also
/// reaches each other across the Internet holds a LAN session and a WAN
/// session at once, on the same sockets, each with its own handshake, its own
/// attestation exchange, its own path and its own staleness clock, so that
/// losing one leaves the other.
///
/// Two full [GrassrootsNetwork] instances on one machine. Each node's local
/// prefixes are injected to hold 127.0.0.1 and not ::1, so IPv4 loopback is
/// LAN contact and IPv6 loopback is WAN contact — decision 8 tells the two
/// apart by the address alone.
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

  String hex(Uint8List key) =>
      key.map((byte) => byte.toRadixString(16).padLeft(2, '0')).join();

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  final ipv4LoopbackIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('127.0.0.1'))]);

  final hostReaders = Expando<_DualLoopbackHostReader>();

  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id,
      {LocalNetwork? localNetwork}) async {
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final hosts = _DualLoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        // Long enough that no tick re-reads the local network and replaces
        // the injected prefixes, or sweeps, inside the test window: the test
        // drives the sweep itself.
        announceInterval: Duration(minutes: 30),
      ),
    );
    hostReaders[network] = hosts;
    addTearDown(network.dispose);
    expect(await network.initialize(), isTrue);
    // Wait for the construction-time read of the local network, which would
    // otherwise overwrite the injection (see the LAN trust test).
    await stateWhere(store, (s) => s.localNetwork.networkId != null);
    store.dispatch(
        LocalNetworkChangedAction(localNetwork ?? ipv4LoopbackIsLocal));
    return network;
  }

  /// Where [network] listens on [family], as its own host candidates report
  /// it, or null when it binds no socket of that family.
  Future<String?> listening(
      GrassrootsNetwork network, InternetAddressType family) async {
    await hostReaders[network]!.read;
    for (final candidate in network.debugLocalCandidates()) {
      if (parseAddressString(candidate)?.ip.type == family) return candidate;
    }
    return null;
  }

  /// Two nodes, `dialer` the deterministic initiator (the smaller key), each
  /// holding the other reachable over the LAN and over the WAN, each over its
  /// own session; the connects and disconnects each reported, per underlay.
  Future<
      ({
        GrassrootsNetwork dialer,
        GrassrootsIdentity dialerId,
        GrassrootsNetwork callee,
        GrassrootsIdentity calleeId,
        List<Underlay> dialerConnects,
        List<Underlay> dialerDisconnects,
        String dialerLan,
        String dialerWan,
        String calleeWan,
      })?> linkedOverLanAndWan(int seedA, int seedB) async {
    final ids = [await identityFromSeed(seedA), await identityFromSeed(seedB)]
      ..sort((x, y) => hex(x.publicKey).compareTo(hex(y.publicKey)));
    final dialerId = ids.first;
    final calleeId = ids.last;
    final dialer = await startNode(dialerId);
    final callee = await startNode(calleeId);

    final dialerLan = await listening(dialer, InternetAddressType.IPv4);
    final dialerWan = await listening(dialer, InternetAddressType.IPv6);
    final calleeLan = await listening(callee, InternetAddressType.IPv4);
    final calleeWan = await listening(callee, InternetAddressType.IPv6);
    if (dialerLan == null ||
        dialerWan == null ||
        calleeLan == null ||
        calleeWan == null) {
      markTestSkipped('needs UDP sockets on both IPv4 and IPv6 loopback');
      return null;
    }

    final connects = <Underlay>[];
    final disconnects = <Underlay>[];
    dialer.onPeerConnected = (pk, underlay, _) {
      if (hex(pk) == hex(calleeId.publicKey)) connects.add(underlay);
    };
    dialer.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) == hex(calleeId.publicKey)) disconnects.add(underlay);
    };

    // Known to each other: the WAN answers only known keys (GNA §2.7), and a
    // LAN instance is recognized only for a known key.
    callee.putKnownPeer(dialerId.publicKey);
    // The WAN: the callee's IPv6 address is the dialer's recorded address for
    // it, and a send dials it on demand.
    dialer.wan!.putPeerAddress(calleeId.publicKey, calleeWan);
    await dialer.send(calleeId.publicKey, Uint8List.fromList([1]));
    await stateWhere(dialer.store,
        (_) => dialer.peerUnderlays(calleeId.publicKey).contains(Underlay.wan));
    // The LAN: the callee's instance resolves at its IPv4 address, and the
    // deterministic initiator dials it.
    dialer.debugResolveLanInstance(calleeId.lanToken, calleeLan);
    await stateWhere(dialer.store,
        (_) => dialer.peerUnderlays(calleeId.publicKey).contains(Underlay.lan));
    await stateWhere(callee.store,
        (_) => callee.peerUnderlays(dialerId.publicKey).length == 2);

    return (
      dialer: dialer,
      dialerId: dialerId,
      callee: callee,
      calleeId: calleeId,
      dialerConnects: connects,
      dialerDisconnects: disconnects,
      dialerLan: dialerLan,
      dialerWan: dialerWan,
      calleeWan: calleeWan,
    );
  }

  test(
      'a pair on the LAN and the WAN holds a session on each, peerUnderlays '
      'reporting both, and each inbound datagram is attributed to the session '
      'of its source address (decision 8)', () async {
    final linked = await linkedOverLanAndWan(0xc1, 0xc2);
    if (linked == null) return;
    final dialerHex = hex(linked.dialerId.publicKey);

    expect(linked.dialer.peerUnderlays(linked.calleeId.publicKey),
        [Underlay.lan, Underlay.wan]);
    expect(linked.callee.peerUnderlays(linked.dialerId.publicKey),
        [Underlay.lan, Underlay.wan]);
    expect(linked.dialerConnects, unorderedEquals([Underlay.lan, Underlay.wan]),
        reason: 'one connect per underlay, each session attested on its own');

    // The callee was dialed on both: each path it holds is the one its
    // inbound contact minted, keyed by the session its source address falls
    // under — the IPv4 (local) source to the LAN session, the IPv6 (not
    // local) source to the WAN session.
    expect(linked.callee.debugUdpPeerIdForAddress(linked.dialerLan),
        'lan/$dialerHex');
    expect(linked.callee.debugUdpPeerIdForAddress(linked.dialerWan),
        'wan/$dialerHex');

    // F02, per session: the WAN session's address is recorded as the peer's
    // WAN address, never the LAN session's.
    final recorded = linked.callee.store.state.peers
        .getPeerByPubkey(linked.dialerId.publicKey)
        ?.udpAddress;
    expect(recorded, linked.dialerWan);
    expect(linked.dialer.wan!.observedPeerAddress(linked.calleeId.publicKey),
        linked.calleeWan,
        reason: 'the observed address is the WAN session\'s');
  }, timeout: Timeout.none);

  test(
      'the WAN session torn down leaves the LAN session: the peer stays '
      'reachable over the LAN, onPeerDisconnected reports the WAN alone, a '
      'send rides the LAN, and the heartbeat sweep redials the WAN though the '
      'LAN is live', () async {
    final linked = await linkedOverLanAndWan(0xc3, 0xc4);
    if (linked == null) return;
    final dialer = linked.dialer;
    final calleePk = linked.calleeId.publicKey;

    // The WAN session's attempts run out (F06): that session is torn down,
    // as the liveness sweep tears down a silent one, and no other.
    dialer.debugExhaustIpAttempts(Underlay.wan, calleePk);
    await stateWhere(dialer.store,
        (_) => !dialer.peerUnderlays(calleePk).contains(Underlay.wan));

    expect(dialer.peerUnderlays(calleePk), [Underlay.lan]);
    expect(dialer.isPeerReachable(calleePk), isTrue);
    expect(linked.dialerDisconnects, [Underlay.wan],
        reason: 'onPeerDisconnected(pk, underlay) reports the underlay that '
            'went down (spec §Connection and Reachability), and the LAN did '
            'not');
    final peer = dialer.store.state.peers.getPeerByPubkey(calleePk)!;
    expect(peer.lanAuthenticated, isTrue);
    expect(peer.wanAuthenticated, isFalse);

    // A send now rides the session that stands.
    final arrived = Completer<Underlay>();
    linked.callee.onMessageReceived =
        (sender, payload, messageId, senderMessageId, underlay) {
      if (payload.length == 1 && payload[0] == 2 && !arrived.isCompleted) {
        arrived.complete(underlay);
      }
    };
    await dialer.send(calleePk, Uint8List.fromList([2]));
    expect(await arrived.future, Underlay.lan);

    // Reconnection is per underlay: the sweep dials the recorded WAN address,
    // the live LAN session notwithstanding.
    await dialer.debugSweepKnownPeers();
    await stateWhere(dialer.store,
        (_) => dialer.peerUnderlays(calleePk).contains(Underlay.wan));
    expect(dialer.peerUnderlays(calleePk), [Underlay.lan, Underlay.wan]);
    expect(linked.dialerDisconnects, [Underlay.wan]);
  }, timeout: Timeout.none);

  test(
      'the LAN path dropped leaves the WAN session, and the LAN redial dials '
      'the LAN though the WAN is live', () async {
    final linked = await linkedOverLanAndWan(0xc5, 0xc6);
    if (linked == null) return;
    final dialer = linked.dialer;
    final calleePk = linked.calleeId.publicKey;

    await dialer.debugDropIpPath(Underlay.lan, calleePk);
    await stateWhere(dialer.store,
        (_) => !dialer.peerUnderlays(calleePk).contains(Underlay.lan));
    expect(dialer.peerUnderlays(calleePk), [Underlay.wan]);
    expect(dialer.isPeerReachable(calleePk), isTrue);
    expect(linked.dialerDisconnects, [Underlay.lan]);

    // The sweep's LAN redial dials the cached instance: no LAN session, so
    // the live WAN session does not stand in for it.
    await dialer.debugSweepKnownPeers();
    await stateWhere(dialer.store,
        (_) => dialer.peerUnderlays(calleePk).contains(Underlay.lan));
    expect(dialer.peerUnderlays(calleePk), [Underlay.lan, Underlay.wan]);
  }, timeout: Timeout.none);

  test(
      'with the LAN and the WAN both live, a send rides the LAN, and the '
      'message settles on its sender: delivered, nothing queued, and neither '
      'IP lane holding it', () async {
    final linked = await linkedOverLanAndWan(0xc7, 0xc8);
    if (linked == null) return;
    final dialer = linked.dialer;
    final calleePk = linked.calleeId.publicKey;
    expect(dialer.peerUnderlays(calleePk), [Underlay.lan, Underlay.wan]);

    final arrived = Completer<Underlay>();
    linked.callee.onMessageReceived =
        (sender, payload, messageId, senderMessageId, underlay) {
      if (payload.length == 1 && payload[0] == 3 && !arrived.isCompleted) {
        arrived.complete(underlay);
      }
    };
    final messageId = await dialer.send(calleePk, Uint8List.fromList([3]));
    expect(await arrived.future, Underlay.lan,
        reason: 'send prefers the LAN over the WAN');

    // The recipient's acknowledgment settles the message: it is delivered,
    // no queue holds it, and the lane it rode lets it go.
    await stateWhere(
        dialer.store,
        (s) =>
            s.messages.outgoingMessages[messageId]?.status ==
                MessageStatus.delivered &&
            dialer.queuedMessageCountForPeer(calleePk) == 0);
    expect(dialer.debugIpLaneHolds(Underlay.lan, calleePk), isFalse,
        reason: 'the acknowledgment reached the LAN lane the message rode');
    expect(dialer.debugIpLaneHolds(Underlay.wan, calleePk), isFalse);
    expect(dialer.peerUnderlays(calleePk), [Underlay.lan, Underlay.wan]);
  }, timeout: Timeout.none);

  test(
      'a WAN path opened at an address the prefixes now in force place on '
      'the local network is never recorded as the WAN address (F09)', () async {
    final ids = [await identityFromSeed(0xc9), await identityFromSeed(0xca)];
    final dialerId = ids.first;
    final calleeId = ids.last;
    // The callee starts with prefixes that do not hold loopback, as before
    // its first read of the local network: the dialer's contact from
    // 127.0.0.1 opens a WAN path, and the WAN session runs on it.
    final callee = await startNode(calleeId,
        localNetwork: localNetworkFromPrefixes(
            [localPrefixOf(InternetAddress('192.168.7.5'))]));
    final dialer = await startNode(dialerId,
        localNetwork: localNetworkFromPrefixes(
            [localPrefixOf(InternetAddress('192.168.7.5'))]));
    final calleeV4 = await listening(callee, InternetAddressType.IPv4);
    final dialerV4 = await listening(dialer, InternetAddressType.IPv4);
    expect(calleeV4, isNotNull);
    expect(dialerV4, isNotNull);

    callee.putKnownPeer(dialerId.publicKey);
    dialer.wan!.putPeerAddress(calleeId.publicKey, calleeV4!);
    await dialer.send(calleeId.publicKey, Uint8List.fromList([4]));
    await stateWhere(callee.store,
        (_) => callee.peerUnderlays(dialerId.publicKey).contains(Underlay.wan));
    expect(callee.debugUdpPeerIdForAddress(dialerV4!),
        'wan/${hex(dialerId.publicKey)}');

    // GLP records the dialer's WAN address elsewhere; then the callee's
    // prefixes come to hold loopback, so the WAN path's address is now LAN
    // contact.
    const wanAddress = '203.0.113.9:4500';
    callee.wan!.putPeerAddress(dialerId.publicKey, wanAddress);
    callee.store.dispatch(LocalNetworkChangedAction(ipv4LoopbackIsLocal));

    // A packet on the WAN session is the occasion to record its observed
    // address; it arrives, and the recorded address stays.
    final arrived = Completer<void>();
    callee.onMessageReceived =
        (sender, payload, messageId, senderMessageId, underlay) {
      if (payload.length == 1 && payload[0] == 5 && !arrived.isCompleted) {
        arrived.complete();
      }
    };
    await dialer.send(calleeId.publicKey, Uint8List.fromList([5]));
    await arrived.future;
    expect(
        callee.store.state.peers
            .getPeerByPubkey(dialerId.publicKey)
            ?.udpAddress,
        wanAddress,
        reason: 'an address on the local network is never the WAN address');
    expect(callee.store.state.knownPeers.known[hex(dialerId.publicKey)],
        wanAddress);
  }, timeout: Timeout.none);

  test(
      'a dial to a peer at a non-public recorded address sends no hole punch '
      'before the handshake: punching is GLP\'s alone, through punchUdp',
      () async {
    final dialer = await startNode(await identityFromSeed(0xd1));
    final peerId = await identityFromSeed(0xd2);

    // A bare socket standing for the peer's recorded address. The first
    // datagram it receives is whatever the dial sends first: a punch, if the
    // layer punches on its own, or the handshake.
    final peerSocket = await RawDatagramSocket.bind(
        InternetAddress.loopbackIPv6, 0);
    addTearDown(peerSocket.close);
    final firstDatagram = Completer<Uint8List>();
    final sub = peerSocket.listen((event) {
      if (event != RawSocketEvent.read || firstDatagram.isCompleted) return;
      final datagram = peerSocket.receive();
      if (datagram != null) firstDatagram.complete(datagram.data);
    });
    addTearDown(sub.cancel);

    dialer.wan!.putPeerAddress(
        peerId.publicKey, '[::1]:${peerSocket.port}');
    // The dial never completes (nothing answers), so it is not awaited.
    unawaited(dialer.send(peerId.publicKey, Uint8List.fromList([7])));

    final first = await firstDatagram.future;
    expect(PunchPacket.isPunch(first), isFalse,
        reason: 'the layer must not punch before the handshake on its own');
  }, timeout: Timeout.none);

  test(
      'removeKnownPeer ends the live LAN and WAN sessions at once: the peer is '
      'unreachable on both underlays before the call returns, and '
      'onPeerDisconnected reports each underlay that was up', () async {
    final linked = await linkedOverLanAndWan(0xcb, 0xcc);
    if (linked == null) return;
    final dialer = linked.dialer;
    final calleePk = linked.calleeId.publicKey;
    expect(dialer.peerUnderlays(calleePk), [Underlay.lan, Underlay.wan]);

    final bothReported = Completer<void>();
    dialer.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) != hex(calleePk)) return;
      linked.dialerDisconnects.add(underlay);
      if (linked.dialerDisconnects.length == 2 && !bothReported.isCompleted) {
        bothReported.complete();
      }
    };

    dialer.removeKnownPeer(calleePk);

    expect(dialer.peerUnderlays(calleePk), isEmpty,
        reason: 'the sessions end in the call, not when the stores settle');
    expect(dialer.isPeerReachable(calleePk), isFalse);
    expect(dialer.store.state.knownPeers.isKnown(hex(calleePk)), isFalse);

    await bothReported.future;
    expect(linked.dialerDisconnects, unorderedEquals([Underlay.lan, Underlay.wan]),
        reason: 'one disconnect per underlay that was up');
  }, timeout: Timeout.none);
}
