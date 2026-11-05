// Undertakings presented (presenting_network.dart): two persons of GKK, each
// the agent of PersonHost over a network that carries messages in memory,
// wrapped by PresentingNetwork.  When both undertook the same contract, each
// holds the other's undertaking, kept on disk, and they become acquainted.
// When Bob's undertaking is for another package, Alice refuses it and holds
// his messages, and Bob refuses hers: no interaction is given to the agents.
// Needs the warranted starters' tree (GAS_TREE, ../phone/tool/sync_assets.sh);
// self-skips without it.
import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:gas_core/contracts.dart';
import 'package:gas_core/person_host.dart';
import 'package:gas_core/presenting_network.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:test/test.dart';

import 'install_test.dart' show copyTree;

Map<String, Object?> S(String f, List<Object?> a) => {'f': f, 'a': a};

/// Networks of one process: a message sent is received by the network of
/// the key it is sent to, later.
class Wire {
  final Map<String, MemoryNet> nets = {};
}

class MemoryNet implements GlpNetwork {
  final Wire wire;
  final PersonIdentity id;
  int _n = 0;
  final List<String> acked = [];
  MemoryNet(this.wire, this.id) {
    wire.nets[id.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => id.pub;
  @override
  Future<void> send(PubKey pk, Uint8List payload) async {
    final to = wire.nets[pk.hex];
    if (to == null) return;
    final mid = '${id.pub.hex.substring(0, 4)}:${_n++}';
    Timer(const Duration(milliseconds: 5), () => to.onMessageReceived?.call(id.pub, payload, mid, Underlay.lan));
  }

  @override
  void acknowledge(String messageId) => acked.add(messageId);
  @override
  Uint8List sign(Uint8List message) => id.sign(message);
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      PersonIdentity.verify(signer, message, signature);
  @override
  bool isPeerReachable(PubKey pk) => wire.nets.containsKey(pk.hex);
  @override
  List<Underlay> peerUnderlays(PubKey pk) => isPeerReachable(pk) ? const [Underlay.lan] : const [];
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String? getPublicAddress() => null;
  @override
  String? networkIdentity() => null;
  @override
  String? observedPeerAddress(PubKey pk) => null;
  @override
  Future<bool> declarePlace(String place, double radiusMetres) async => false;
  @override
  Future<void> removePlace(String place) async {}
  @override
  void putKnownPeer(PubKey pk) {}
  @override
  void removeKnownPeer(PubKey pk) {}
  @override
  void putPeerAddress(PubKey pk, String address) {}
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
  @override
  void punchUdp(String address) {}
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {}
  @override
  void Function(PubKey pk, Underlay underlay, Object? identity)? onPeerConnected;
  @override
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;
  @override
  void Function(PubKey sender, Uint8List payload, String messageId, Underlay underlay)? onMessageReceived;
  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;
  @override
  void Function(String? oldAddress, String? newAddress)? onConnectivityStatusChanged;
  @override
  void Function(String? oldId, String? newId)? onNetworkChanged;
  @override
  void Function(String place, PlaceEvent event)? onPlaceEvent;
}

void main() {
  test('an invitation names the contract, and its signer the inviter; its link carries it', () {
    final alice = PersonIdentity.fromSeed(Uint8List.fromList(List.filled(32, 0x41)));
    final bob = PersonIdentity.fromSeed(Uint8List.fromList(List.filled(32, 0x42)));
    final pkg = 'a' * 64, other = 'b' * 64, hSrc = 'c' * 64;
    final inv = invitationOf(alice, pkg, hSrc);
    expect(isInvitation(inv, alice.pub, pkg, hSrc), isTrue);
    expect(isInvitation(inv, bob.pub, pkg, hSrc), isFalse, reason: 'not Bob\'s');
    expect(isInvitation(inv, alice.pub, other, hSrc), isFalse, reason: 'to another package');
    expect(isInvitation(inv, alice.pub, pkg, 'd' * 64), isFalse, reason: 'for other code');
    expect(isInvitation(undertakingOf(alice, pkg, hSrc), alice.pub, pkg, hSrc), isFalse,
        reason: 'an undertaking is no invitation');
    expect(isInvitation(null, alice.pub, pkg, hSrc), isFalse);
    final l = GasLink.parse('Join me: https://talmonlab.bgu.ac.il/gas/add.html#${alice.pub.hex}&c=$pkg&s=$inv')!;
    expect([l.person, l.contract, l.invitation], [alice.pub.hex, pkg, inv]);
    final g = GasLink.parse('gas://add/${alice.pub.hex}?c=$pkg&s=$inv')!;
    expect(g.invitation, inv);
    expect(GasLink.parse('gas://add/${alice.pub.hex}?c=$pkg')!.invitation, isNull);
  });

  final tree = Directory(Platform.environment['GAS_TREE'] ?? '../phone/assets/tree');

  Future<T> until<T>(T? Function() f, String what, {int seconds = 90}) async {
    final deadline = DateTime.now().add(Duration(seconds: seconds));
    while (true) {
      final v = f();
      if (v != null) return v;
      if (DateTime.now().isAfter(deadline)) throw TimeoutException(what);
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
  }

  (int, List<int>) menu(PersonHost p) {
    for (final e in p.draws.entries) {
      final v = e.value;
      if (v is Map && v['f'] == 'menu_w') {
        final input = ((v['a'] as List)[0] as Map)['a'] as List;
        return (e.key, [for (final x in ((input[2] as Map)['a'] as List)[0] as List) x as int]);
      }
    }
    throw StateError('no menu');
  }

  Future<void> propose(PersonHost p, Map<String, Object?> v) async {
    final (id, path) = menu(p);
    await p.act(id, path, S('propose', [v]));
  }

  MapEntry<int, Object?>? asked(PersonHost p) {
    for (final e in p.draws.entries) {
      final v = e.value;
      if (v is Map && v['f'] == 'approval_w') return e;
    }
    return null;
  }

  List<Object?> atoms(PersonHost p) {
    final out = p.draws[0];
    if (out is! Map) return const [];
    final views = (out['a'] as List)[0] as List;
    return (((views.last as Map)['a'] as List)[0] as List);
  }

  bool knows(PersonHost p, String q) =>
      atoms(p).any((a) => a is Map && a['f'] == 'knows' && ((a['a'] as List)[0]) == q);

  Future<({PersonHost alice, PersonHost bob, PresentingNetwork an, PresentingNetwork bn, Directory base})> pair(
      {String? bobPackage}) async {
    final base = Directory.systemTemp.createTempSync('gas_presenting_');
    addTearDown(() => base.deleteSync(recursive: true));
    copyTree(tree.absolute, base);
    final store = ContractStore(base.path);
    await store.installStarters();
    final gkk = store.installed().firstWhere((c) => c.id == 'GKK');
    final hSrc = store.warrantyOf(gkk)!['hsrc'] as String;
    final wire = Wire();
    (PersonHost, PresentingNetwork) person(int fill, String package) {
      final id = PersonIdentity.fromSeed(Uint8List.fromList(List.filled(32, fill)));
      final net = PresentingNetwork(
          inner: MemoryNet(wire, id),
          person: id,
          package: package,
          hSrc: hSrc,
          dir: '${base.path}/undertakings/$fill',
          app: gkk.name);
      final host = PersonHost(
          program: store.program(gkk),
          rootSelfGlp: '${base.path}/programs/self.glp',
          keyPair: (pub: id.pub, priv: id.priv),
          network: net);
      return (host, net);
    }

    final (alice, an) = person(0x41, gkk.hash);
    final (bob, bn) = person(0x42, bobPackage ?? gkk.hash);
    await alice.start();
    await bob.start();
    expect(alice.status, 'ready');
    expect(bob.status, 'ready');
    return (alice: alice, bob: bob, an: an, bn: bn, base: base);
  }

  test('two persons who undertook GKK hold each other\'s undertaking, and meet', () async {
    if (!Directory('${tree.path}/packages').existsSync()) return markTestSkipped('no bundle tree');
    final p = await pair();
    final (alice, bob) = (p.alice, p.bob);
    await propose(alice, S('establish', [alice.me]));
    await propose(bob, S('establish', [bob.me]));
    await propose(alice, S('acquaint', [alice.me, bob.me]));
    final ask = await until(() => asked(bob), 'Bob was never asked');
    final approval = ((ask.value as Map)['a'] as List)[0] as Map;
    final input = (approval['a'] as List)[2] as Map;
    await bob.act(ask.key, [for (final x in (input['a'] as List)[0] as List) x as int], 'yes');
    await until(() => knows(alice, bob.me) ? true : null, 'Alice never knew Bob');
    await until(() => knows(bob, alice.me) ? true : null, 'Bob never knew Alice');
    expect(p.an.hasPresented(bob.keyPair.pub), isTrue);
    expect(p.bn.hasPresented(alice.keyPair.pub), isTrue);
    // Each keeps the other's undertaking, signed by them, for this package.
    final kept = jsonDecode(File('${p.base.path}/undertakings/${0x41}/${p.an.app}.json').readAsStringSync()) as Map;
    expect(isUndertaking(kept[bob.me] as String, bob.keyPair.pub, p.an.package, p.an.hSrc), isTrue);
  }, timeout: const Timeout(Duration(minutes: 5)));

  test('an undertaking for another package is refused, and so is the interaction', () async {
    if (!Directory('${tree.path}/packages').existsSync()) return markTestSkipped('no bundle tree');
    final p = await pair(bobPackage: 'f' * 64);
    final (alice, bob) = (p.alice, p.bob);
    final refusedAtBob = <String>[];
    p.bn.onRefused = (pk, why) => refusedAtBob.add(why);
    await propose(alice, S('establish', [alice.me]));
    await propose(bob, S('establish', [bob.me]));
    await propose(alice, S('acquaint', [alice.me, bob.me]));
    await until(() => refusedAtBob.isNotEmpty ? true : null, 'Bob never refused Alice\'s undertaking', seconds: 30);
    expect(refusedAtBob.first, contains('does not check'));
    await Future<void>.delayed(const Duration(seconds: 5));
    expect(asked(bob), isNull, reason: 'Alice\'s hello is held, not given to Bob\'s agent');
    expect(p.bn.hasPresented(alice.keyPair.pub), isFalse);
    expect(knows(bob, alice.me), isFalse);
  }, timeout: const Timeout(Duration(minutes: 5)));
}
