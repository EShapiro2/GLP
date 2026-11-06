/// A [GlpNetwork] across isolates (port_network.dart): the agent's end passes
/// every function through to the layer's end and takes the layer's events,
/// and what the interface answers at once it answers from the key pair and
/// the mirror the layer's events carry (IGLP, Implementation Notes,
/// "Networking Seam", One interface and The contract).  Both ends run here in
/// one isolate, the ports being what they are; the agents in their isolates
/// are glp_multiagent/test/agent_entry_test.dart's.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/port_network.dart';
import 'package:test/test.dart';

/// A layer that records the calls made on it and answers as the test says.
class FakeLayer implements GlpNetwork {
  final calls = <String>[];
  final refused = Completer<void>();
  bool refuse = false;
  bool hasWan = true;
  final reachable = <PubKey, List<Underlay>>{};
  String? publicAddress;
  String? observed;

  @override
  PubKey getIdentity() => throw StateError('the agent\'s end answers it');

  @override
  void putKnownPeer(PubKey pk) => calls.add('putKnownPeer ${pk.hex}');
  @override
  void removeKnownPeer(PubKey pk) => calls.add('removeKnownPeer ${pk.hex}');
  @override
  void Function(PubKey, Underlay, Object?)? onPeerConnected;
  @override
  void Function(PubKey, Underlay)? onPeerDisconnected;
  @override
  bool isPeerReachable(PubKey pk) => reachable[pk]?.isNotEmpty ?? false;
  @override
  List<Underlay> peerUnderlays(PubKey pk) => reachable[pk] ?? const [];
  @override
  Future<void> send(PubKey pk, Uint8List payload) async {
    calls.add('send ${pk.hex} ${payload.length}');
    if (refuse) throw StateError('the queue was not saved');
  }

  @override
  void Function(PubKey, Uint8List, String, Underlay)? onMessageReceived;
  @override
  void acknowledge(String messageId) => calls.add('acknowledge $messageId');
  @override
  Uint8List sign(Uint8List message) =>
      throw StateError('the agent\'s end answers it');
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw StateError('the agent\'s end answers it');
  @override
  List<DiscoveredPeer> getPeers() => [
        for (final MapEntry(key: pk, value: us) in reachable.entries)
          if (us.contains(Underlay.lan))
            DiscoveredPeer(pk, ProximityUnderlay.lan),
      ];
  @override
  void Function(PubKey, ProximityUnderlay)? onPeerDiscovered;
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      calls.add('setTrustLevel ${underlay.name} ${level.name}');
  @override
  void Function(String?, String?)? onConnectivityStatusChanged;

  void _wan() {
    if (!hasWan) throw UnsupportedError('no WAN here');
  }

  @override
  String? getPublicAddress() {
    _wan();
    return publicAddress;
  }

  @override
  String? networkIdentity() {
    _wan();
    return 'net';
  }

  @override
  void Function(String?, String?)? onNetworkChanged;
  @override
  void putPeerAddress(PubKey pk, String address) =>
      calls.add('putPeerAddress ${pk.hex} $address');
  @override
  void putRendezvousServer(PubKey pk, String address) =>
      calls.add('putRendezvousServer ${pk.hex} $address');
  @override
  void removeRendezvousServer(PubKey pk) =>
      calls.add('removeRendezvousServer ${pk.hex}');
  @override
  String? observedPeerAddress(PubKey pk) {
    _wan();
    return observed;
  }

  @override
  void punchUdp(String address) => calls.add('punchUdp $address');
  @override
  Future<bool> declarePlace(String place, double radiusMetres) async {
    calls.add('declarePlace $place $radiusMetres');
    return !refuse;
  }

  @override
  Future<void> removePlace(String place) async =>
      calls.add('removePlace $place');
  @override
  void Function(String, PlaceEvent)? onPlaceEvent;
}

/// A layer given no location service, as the Flutter layer without a
/// geofence backend: it takes no place callback and refuses a declaration.
class NoPlacesLayer extends FakeLayer {
  @override
  set onPlaceEvent(void Function(String, PlaceEvent)? cb) =>
      throw StateError('no location service');
  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      throw StateError('no location service');
}

void main() {
  final me = PersonIdentity.fromSeed(Uint8List(32));
  final peer = PersonIdentity.fromSeed(Uint8List(32)..fillRange(0, 32, 7)).pub;
  late FakeLayer layer;
  late LayerPort port;
  late PortGlpNetwork net;

  setUp(() {
    layer = FakeLayer();
    port = LayerPort(layer);
    net = PortGlpNetwork(port.sendPort, me.keyPair);
  });
  tearDown(() {
    net.close();
    port.close();
  });

  /// Every message on the ports so far has been taken.
  Future<void> settled() => Future<void>.delayed(Duration.zero)
      .then((_) => Future<void>.delayed(Duration.zero))
      .then((_) => Future<void>.delayed(Duration.zero));

  test('the key pair answers the identity, sign and verify', () {
    expect(net.getIdentity(), me.pub);
    final m = Uint8List.fromList([1, 2, 3]);
    final s = net.sign(m);
    expect(s, me.sign(m));
    expect(net.verify(me.pub, m, s), isTrue);
    expect(net.verify(peer, m, s), isFalse);
  });

  test('each function passes through to the layer', () async {
    net
      ..putKnownPeer(peer)
      ..removeKnownPeer(peer)
      ..acknowledge('m1')
      ..setTrustLevel(ProximityUnderlay.lan, TrustLevel.open)
      ..removeRendezvousServer(peer);
    await settled();
    net
      ..putPeerAddress(peer, '10.0.0.1:7')
      ..putRendezvousServer(peer, '10.0.0.2:8')
      ..punchUdp('10.0.0.3:9');
    await net.send(peer, Uint8List(5));
    expect(await net.declarePlace('home', 10), isTrue);
    await net.removePlace('home');
    expect(layer.calls, [
      'putKnownPeer ${peer.hex}',
      'removeKnownPeer ${peer.hex}',
      'acknowledge m1',
      'setTrustLevel lan open',
      'removeRendezvousServer ${peer.hex}',
      'putPeerAddress ${peer.hex} 10.0.0.1:7',
      'putRendezvousServer ${peer.hex} 10.0.0.2:8',
      'punchUdp 10.0.0.3:9',
      'send ${peer.hex} 5',
      'declarePlace home 10.0',
      'removePlace home',
    ]);
  });

  test('a refusal of the layer reaches the agent', () async {
    layer.refuse = true;
    await expectLater(net.send(peer, Uint8List(1)), throwsStateError);
    expect(await net.declarePlace('home', 10), isFalse);
  });

  test('an event of the layer brings the mirror with it, before its callback',
      () async {
    final seen = <String>[];
    net.onPeerConnected = (pk, underlay, identity) => seen.add(
        '$underlay ${net.isPeerReachable(pk)} ${net.observedPeerAddress(pk)} '
        '${net.getPublicAddress()} $identity');
    net.onPeerDisconnected =
        (pk, underlay) => seen.add('$underlay ${net.isPeerReachable(pk)}');
    await settled();

    layer.reachable[peer] = [Underlay.lan];
    layer.observed = '10.0.0.9:1';
    layer.publicAddress = '203.0.113.7:2';
    layer.onPeerConnected!(peer, Underlay.lan, 'app');
    await settled();
    expect(seen.single,
        '${Underlay.lan} true 10.0.0.9:1 203.0.113.7:2 app');
    expect(net.peerUnderlays(peer), [Underlay.lan]);
    expect(net.getPeers().single.pk, peer);
    expect(net.networkIdentity(), 'net');

    layer.reachable.remove(peer);
    layer.onPeerDisconnected!(peer, Underlay.lan);
    await settled();
    expect(seen.last, '${Underlay.lan} false');
    expect(net.isPeerReachable(peer), isFalse);
  });

  test('what the layer delivers before the agent is bound waits for it',
      () async {
    final other = FakeLayer();
    final otherPort = LayerPort(other);
    addTearDown(otherPort.close);

    // Before the agent's end attaches.
    other.onMessageReceived!(peer, Uint8List(2), 'm1', Underlay.lan);
    final late = PortGlpNetwork(otherPort.sendPort, me.keyPair);
    addTearDown(late.close);
    // After, before its callback is set.
    await settled();
    other.onMessageReceived!(peer, Uint8List(3), 'm2', Underlay.lan);
    await settled();

    final got = <String>[];
    late.onMessageReceived = (sender, payload, id, underlay) =>
        got.add('$id ${payload.length} ${sender == peer}');
    expect(got, ['m1 2 true', 'm2 3 true']);
  });

  test('a place event reaches the agent once it watches places, and a layer '
      'with no location service only refuses the declaration', () async {
    final seen = <String>[];
    net.onPlaceEvent = (place, event) => seen.add('$place ${event.name}');
    await settled();
    layer.onPlaceEvent!('home', PlaceEvent.entered);
    await settled();
    expect(seen, ['home entered']);

    final other = NoPlacesLayer();
    final otherPort = LayerPort(other);
    addTearDown(otherPort.close);
    final noPlaces = PortGlpNetwork(otherPort.sendPort, me.keyPair);
    addTearDown(noPlaces.close);
    noPlaces.onPlaceEvent = (place, event) {};
    await expectLater(noPlaces.declarePlace('home', 10), throwsStateError);
  });

  test('a layer that runs no WAN: the functions throw, as the layer does',
      () async {
    final other = FakeLayer()..hasWan = false;
    final otherPort = LayerPort(other);
    addTearDown(otherPort.close);
    final noWan = PortGlpNetwork(otherPort.sendPort, me.keyPair);
    addTearDown(noWan.close);
    await settled();
    expect(() => noWan.getPublicAddress(), throwsUnsupportedError);
    expect(() => noWan.punchUdp('10.0.0.1:1'), throwsUnsupportedError);
    expect(() => noWan.putPeerAddress(peer, 'x'), throwsUnsupportedError);
    expect(() => noWan.observedPeerAddress(peer), throwsUnsupportedError);
  });
}
