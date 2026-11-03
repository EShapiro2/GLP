import 'dart:async';
import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/session/noise_session_manager.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

/// A datagram the relay carried: from the caller or from the callee, its
/// frame type, and for a handshake the kind of message its envelope names
/// (0x11 and 0x12 IK messages 1 and 2, 0x01 to 0x03 XX's).
typedef _Carried = ({bool fromCaller, int type, int? kind});

/// A loopback UDP relay in front of the callee that records what it carries
/// in each direction.
class _Relay {
  _Relay._(this._socket, this._calleePort) {
    _socket.listen((event) {
      if (event != RawSocketEvent.read) return;
      for (var d = _socket.receive(); d != null; d = _socket.receive()) {
        _relay(d);
      }
    });
  }

  static Future<_Relay> start(int calleePort) async => _Relay._(
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0),
        calleePort,
      );

  final RawDatagramSocket _socket;
  final int _calleePort;
  InternetAddress? _callerAddress;
  int? _callerPort;
  final List<_Carried> carried = [];

  /// Each datagram carried, as it is carried.
  final StreamController<void> _carriedEvents =
      StreamController<void>.broadcast(sync: true);

  /// Completes once [holds] is true of what the relay has carried.
  Future<void> carriedWhere(bool Function() holds) async {
    if (holds()) return;
    await _carriedEvents.stream.firstWhere((_) => holds());
  }

  int get port => _socket.port;
  String get address => '127.0.0.1:$port';

  void _relay(Datagram d) {
    final fromCallee = d.port == _calleePort &&
        d.address.address == InternetAddress.loopbackIPv4.address;
    final data = d.data;
    carried.add((
      fromCaller: !fromCallee,
      type: data.isEmpty ? -1 : data[0],
      kind: data.length > 7 && data[0] == PacketType.noiseHandshake.value
          ? data[7]
          : null,
    ));
    _carriedEvents.add(null);
    if (fromCallee) {
      final address = _callerAddress;
      final port = _callerPort;
      if (address != null && port != null) _socket.send(data, address, port);
      return;
    }
    _callerAddress = d.address;
    _callerPort = d.port;
    _socket.send(data, InternetAddress.loopbackIPv4, _calleePort);
  }

  Set<int> kinds() => {
        for (final c in carried)
          if (c.kind != null) c.kind!,
      };

  Iterable<int> types({required bool fromCaller}) =>
      carried.where((c) => c.fromCaller == fromCaller).map((c) => c.type);

  void close() => _socket.close();
}

/// The handshakes of GNA §2.6 over the Flutter profile's UDP carrier, on the
/// LAN: Noise IK between known peers, with no ANNOUNCE before it and ANNOUNCE
/// inside the session (GNA §2.5); Noise XX for a first contact under Open
/// trust, after ANNOUNCE (GNA §2.6, §3.2: "preceded by ANNOUNCE only for an
/// unrecognized instance under Open trust"); and the responder under Closed
/// trust answering no XX first message, and dropping the connection an
/// ANNOUNCE arrives on before a handshake (GNA §2.5, Trust gating). The
/// refusal of an unknown key's IK under Closed trust is
/// `grassroots_network_lan_trust_test.dart`'s.
///
/// Loopback is not a local network, so each node's prefixes are injected to
/// place loopback within them, which makes the contact LAN contact.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  final loopbackIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('127.0.0.1'))]);

  /// Each node's host-address reader, which tells when its candidates are
  /// read.
  final hostReaders = Expando<LoopbackHostReader>();

  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id) async {
    final store = Store<AppState>(appReducer, initialState: const AppState());
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        // Long enough that no tick re-reads the local network and replaces
        // the injected prefixes inside the test window.
        announceInterval: Duration(minutes: 30),
      ),
    );
    hostReaders[network] = hosts;
    addTearDown(network.dispose);
    expect(await network.initialize(), isTrue);
    // Wait for the construction-time read of the local network, which would
    // otherwise overwrite the injection (see the LAN trust test).
    await stateWhere(store, (s) => s.localNetwork.networkId != null);
    store.dispatch(LocalNetworkChangedAction(loopbackIsLocal));
    return network;
  }

  Future<String> listeningAddress(GrassrootsNetwork network) async {
    await hostReaders[network]!.read;
    return network.debugLocalCandidates().first;
  }

  Future<_Relay> relayTo(GrassrootsNetwork callee) async {
    final port = int.parse((await listeningAddress(callee)).split(':').last);
    final relay = await _Relay.start(port);
    addTearDown(relay.close);
    return relay;
  }

  /// Completes once [callee] and [caller] each hold the other reachable over
  /// the LAN.
  Future<void> linkedOverLan(GrassrootsNetwork caller, Uint8List callerKey,
      GrassrootsNetwork callee, Uint8List calleeKey) async {
    await stateWhere(caller.store,
        (_) => caller.peerUnderlays(calleeKey).contains(Underlay.lan));
    await stateWhere(callee.store,
        (_) => callee.peerUnderlays(callerKey).contains(Underlay.lan));
  }

  final announce = PacketType.announce.value;
  final secureAnnounce = PacketType.secureAnnounce.value;

  test(
      'two known peers on the LAN run IK: no ANNOUNCE before the handshake, '
      'ANNOUNCE inside the session', () async {
    final callerId = await identityFromSeed(0x81);
    final calleeId = await identityFromSeed(0x82);
    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    final relay = await relayTo(callee);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, relay.address);

    await caller.send(calleeId.publicKey, Uint8List.fromList([1]));
    await linkedOverLan(
        caller, callerId.publicKey, callee, calleeId.publicKey);
    expect(caller.peerUnderlays(calleeId.publicKey), contains(Underlay.lan));
    expect(callee.peerUnderlays(callerId.publicKey), contains(Underlay.lan));
    await relay.carriedWhere(() =>
        relay.types(fromCaller: true).contains(secureAnnounce) &&
        relay.types(fromCaller: false).contains(secureAnnounce));
    expect(relay.types(fromCaller: true), contains(secureAnnounce),
        reason: 'ANNOUNCE is sent inside the session');
    expect(relay.types(fromCaller: false), contains(secureAnnounce),
        reason: 'ANNOUNCE is sent inside the session');
    expect(relay.kinds(), {0x11, 0x12});
    expect(relay.types(fromCaller: true), isNot(contains(announce)));
    expect(relay.types(fromCaller: false), isNot(contains(announce)));
  }, timeout: Timeout.none);

  test(
      'an Open first contact on the LAN: ANNOUNCE first, then XX, each side '
      'checking the delivered key against the key ANNOUNCE claimed', () async {
    final callerId = await identityFromSeed(0x83);
    final calleeId = await identityFromSeed(0x84);
    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    final relay = await relayTo(callee);
    await caller.lan!.setTrustLevel(ColdCallTrustLevel.open);
    await callee.lan!.setTrustLevel(ColdCallTrustLevel.open);
    final discovered = <String>[];
    callee.lan!.onPeerDiscovered = (pk) => discovered.add(hexOf(pk));

    // The caller browses an instance it does not recognize: a cold call.
    caller.debugResolveLanInstance(
        'grs-00000000000000000000000000000000', relay.address);

    await linkedOverLan(
        caller, callerId.publicKey, callee, calleeId.publicKey);
    expect(caller.peerUnderlays(calleeId.publicKey), contains(Underlay.lan));
    expect(callee.peerUnderlays(callerId.publicKey), contains(Underlay.lan));
    final kinds = relay.kinds();
    expect(kinds, containsAll([0x01, 0x02, 0x03]));
    expect(kinds.intersection({0x11, 0x12}), isEmpty,
        reason: 'no key was expected, so no IK');
    expect(relay.carried.first.type, announce,
        reason: 'ANNOUNCE precedes the handshake');
    expect(relay.types(fromCaller: false).first, announce,
        reason: 'and the callee answers with its own');
    expect(discovered, [hexOf(callerId.publicKey)]);
    expect(caller.store.state.knownPeers.known, isEmpty,
        reason: 'the stranger stays a stranger to the layer');
  }, timeout: Timeout.none);

  /// Send [datagram] to [layer] from a socket of our own; answer whether
  /// anything came back, and the socket's address.
  Future<(bool, String)> answered(GrassrootsNetwork layer, Uint8List datagram,
      {Duration wait = const Duration(seconds: 3)}) async {
    final socket =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(socket.close);
    var replies = 0;
    socket.listen((event) {
      if (event == RawSocketEvent.read && socket.receive() != null) replies++;
    });
    final port = int.parse((await listeningAddress(layer)).split(':').last);
    socket.send(datagram, InternetAddress.loopbackIPv4, port);
    await Future<void>.delayed(wait);
    return (replies > 0, '127.0.0.1:${socket.port}');
  }

  test(
      'under Closed trust the LAN answers no XX first message, even from a '
      'known key, and drops the connection a clear ANNOUNCE arrives on',
      () async {
    final calleeId = await identityFromSeed(0x85);
    final knownId = await identityFromSeed(0x86);
    final callee = await startNode(calleeId);
    callee.putKnownPeer(knownId.publicKey);
    expect(callee.store.state.settings.trustLevelOf(ProximityUnderlay.lan),
        ColdCallTrustLevel.closed);

    final xx1 = (await NoiseSessionManager(identity: knownId, sodium: sodium)
        .startHandshake(PeerTransport.udp, calleeId.publicKey,
            pattern: NoisePattern.xx))!;
    final (xxAnswered, _) = await answered(
        callee,
        GrassrootsPacket(type: PacketType.noiseHandshake, payload: xx1)
            .serialize());
    expect(xxAnswered, isFalse);

    final record = GrassrootsPacket(
      type: PacketType.announce,
      payload: ProtocolHandler(identity: knownId, sodium: sodium)
          .createAnnouncePayload(),
    ).serialize();
    final (announceAnswered, from) = await answered(callee, record);
    expect(announceAnswered, isFalse);
    expect(callee.debugUdpPeerIdForAddress(from), isNull,
        reason: 'the connection is dropped');
    expect(callee.isPeerReachable(knownId.publicKey), isFalse);
  }, timeout: Timeout.none);
}

String hexOf(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
