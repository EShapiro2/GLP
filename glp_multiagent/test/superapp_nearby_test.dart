/// The Grassroots App on the real entry, two and three phones in one process:
/// each phone a [SuperAppHost], the app's own host (lib/superapp_host.dart),
/// its agent over a network in this process, and its person acting on the
/// constructs home draws, as on a phone.  Not the boot form: no person is
/// scripted in GLP; each agent starts from every agent's initial goal, and
/// the application's first act, `superapp(Me, [])`, starts superapp/4 (GSA
/// paper, Appendix "The Operating System", "Boot"; IGLP, Implementation
/// Notes, "Boot").
///
/// The network carries payloads between keys, and reports discovery as GNA's
/// layer does: an agent on a proximity underlay contacts another it does not
/// know only while the trust of that underlay is open at both (GNA, "Trust
/// levels": "Open: The agent completes the ANNOUNCE exchange and the Noise XX
/// handshake with any unknown contact"; "Closed: The agent answers only an IK
/// handshake from a known key"), and then each is told the other,
/// `onPeerDiscovered(pk)` on that underlay (GNA, Section "Proximity").  So the
/// phones meet only once both persons open the place, GPE:1, which the
/// operating system turns into `declare(trust(pan, open))` and
/// `declare(trust(lan, open))` (GSA paper, Appendix, "Nearby").
///
/// What each person does and sees, as nearby_boot.glp and nearby3_boot.glp
/// have it under :boot: at start each operating system declares both trusts
/// closed (GSA paper, Appendix, "Nearby": "At start the operating system
/// declares the trust of pan and of lan closed"); both open the place; each is
/// nearby the other and the operating system with the smaller key puts the
/// encounter, GPE:2, each consenting while the place is open; each declares
/// the other to the layer a known peer ("The operating system declares to the
/// layer, as a known peer, every person its person knows"); home shows each
/// the other, `nearby([K])`, with a card for them and the button befriend;
/// alice befriends bob, GSG:1, by picking befriend on his card, no key typed
/// ("the person befriends one by picking them there"), bob says yes on his
/// card, and both are told `connected(K)`.  With three, carol opens her place
/// after alice and bob are friends, and meets both.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'programs_dir.dart';

typedef _KeyPair = ({PubKey pub, Uint8List priv});

/// Carries payloads between the [_PhoneNetwork]s joined to it, by key, each
/// delivery in a later turn of the event loop, as a layer delivers; and
/// tells two phones of each other once both have opened the trust of a
/// proximity underlay.
class _Air {
  final Map<String, _PhoneNetwork> _at = {};
  int _next = 0;

  /// The pairs told of each other, per underlay: `<underlay> <hex> <hex>`.
  final Set<String> _met = {};

  void carry(PubKey from, PubKey to, Uint8List payload) {
    final id = 'm${_next++}';
    Timer.run(() =>
        _at[to.hex]?.onMessageReceived?.call(from, payload, id, Underlay.pan));
  }

  /// [n]'s trust on [u] changed: every pair now open at both is told of
  /// each other, once.
  void trustChanged(_PhoneNetwork n, ProximityUnderlay u) {
    for (final o in _at.values) {
      if (identical(o, n)) continue;
      if (n.trust[u] != TrustLevel.open || o.trust[u] != TrustLevel.open) {
        continue;
      }
      final pair = ([n.keyPair.pub.hex, o.keyPair.pub.hex]..sort()).join(' ');
      if (!_met.add('${u.name} $pair')) continue;
      Timer.run(() {
        n.onPeerDiscovered?.call(o.keyPair.pub, u);
        o.onPeerDiscovered?.call(n.keyPair.pub, u);
      });
    }
  }
}

/// A [GlpNetwork] in the [_Air], instantiated with its key pair, both
/// proximity underlays Closed until the program declares them open.
class _PhoneNetwork extends GlpNetwork {
  final _KeyPair keyPair;
  final _Air air;
  final Map<ProximityUnderlay, TrustLevel> trust = {
    for (final u in ProximityUnderlay.values) u: TrustLevel.closed,
  };

  /// Each trust declaration, in order: `<underlay> <level>`.
  final List<String> declared = [];

  /// The known peers the program declared, `declare(peer(K))`, by key hex.
  final Set<String> known = {};

  _PhoneNetwork(this.keyPair, this.air) {
    air._at[keyPair.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => keyPair.pub;

  @override
  Future<void> send(PubKey pk, Uint8List payload) {
    air.carry(keyPair.pub, pk, payload);
    return Future.value();
  }

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {
    declared.add('${underlay.name} ${level.name}');
    trust[underlay] = level;
    air.trustChanged(this, underlay);
  }

  @override
  void acknowledge(String messageId) {}
  @override
  void putKnownPeer(PubKey pk) => known.add(pk.hex);
  @override
  void removeKnownPeer(PubKey pk) => known.remove(pk.hex);
  @override
  bool isPeerReachable(PubKey pk) => air._at.containsKey(pk.hex);
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [Underlay.pan];
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String? getPublicAddress() => null;
  @override
  String? networkIdentity() => null;
  @override
  void putPeerAddress(PubKey pk, String address) {}
  @override
  String? observedPeerAddress(PubKey pk) => null;
  @override
  void punchUdp(String address) {}
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      Future.value(false);
  @override
  Future<void> removePlace(String place) => Future.value();
}

/// One phone: the app's host, its network, and what its person sees.
class _Phone {
  final String name;
  final SuperAppHost host;
  final _PhoneNetwork net;
  final List<String> errors = [];
  final List<(bool Function(), Completer<void>)> _waits = [];

  /// The agent's transcript, and, where GLP_TRACE_DIR names a directory, the
  /// file it is written to as it comes.
  final List<String> lines = [];

  _Phone(this.name, this.host, this.net) {
    host.ui.onChange = _changed;
    final dir = Platform.environment['GLP_TRACE_DIR'];
    final sink = dir == null ? null : File('$dir/$name.log').openWrite();
    host.onOutput = (line) {
      lines.add(line);
      sink?.writeln(line);
      if (line.startsWith('[ERROR]')) {
        errors.add(line);
        for (final (_, c) in _waits) {
          c.completeError(StateError('$name: $line'));
        }
        _waits.clear();
      }
    };
  }

  UiRuntime get ui => host.ui;
  String get key => host.key;

  void _changed() => _waits.removeWhere((w) {
        if (!w.$1()) return false;
        w.$2.complete();
        return true;
      });

  /// Once [test] holds of what the person sees: on the event of the screen's
  /// change, with no clock.
  Future<void> until(bool Function() test) {
    if (test()) return Future.value();
    final c = Completer<void>();
    _waits.add((test, c));
    return c.future;
  }

  /// What the agent told the person, each notice's content, an atom by its
  /// name: a key, 64 hex characters, as it is.
  List<String> notices() {
    final v = ui.bridge.construct(0)?.view;
    return [
      if (v is GList)
        for (final m in v.items) _plain(ctorArgs(m).$2[2])
    ];
  }

  /// Home's request form: the stream of the person's requests.
  OpenConstruct? form() {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 && formatTerm(c.widget).startsWith('input_box(menu(')) {
        return c;
      }
    }
    return null;
  }

  /// One request on home's form, as the person sends it.
  void request(GTerm r) {
    final f = form()!;
    ui.bridge.grant(f.id, questionAt(f.view)!, r);
  }

  /// A friend's request to connect, the card home draws, where one is open:
  /// `form(from, ...)`, the friend and the buttons yes and no.
  OpenConstruct? card() {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 &&
          formatTerm(c.widget).startsWith('form(from,') &&
          _questionIn(c.view) != null) {
        return c;
      }
    }
    return null;
  }

  /// Yes on the card.
  void yes() {
    final c = card()!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('yes'));
  }

  /// The card home draws of a person nearby whom the person knows, the
  /// person of key [key], where one is open: `form(nearby, ...)`, the person
  /// and the button befriend (home.vglp, befriend_card/3).
  OpenConstruct? nearbyCard(String key) {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 &&
          formatTerm(c.widget).startsWith('form(nearby,') &&
          _plain(c.view).contains(key) &&
          _questionIn(c.view) != null) {
        return c;
      }
    }
    return null;
  }

  /// Befriend on the card of the person of key [key]: the person picks them
  /// in Nearby, and types no key.
  void befriend(String key) {
    final c = nearbyCard(key)!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('befriend'));
  }
}

/// [t] as text, an atom by its name, unquoted.
String _plain(GTerm t) => switch (t) {
      GAtom(:final name) => name,
      GList(:final items) => '[${items.map(_plain).join(', ')}]',
      GStruct(:final functor, :final args) =>
        '$functor(${args.map(_plain).join(', ')})',
      _ => formatTerm(t),
    };

/// The position of the question a view marks, anywhere inside it.
GList? _questionIn(GTerm v) {
  final p = questionAt(v);
  if (p != null) return p;
  if (v is GStruct) {
    for (final a in v.args) {
      final q = _questionIn(a);
      if (q != null) return q;
    }
  } else if (v is GList) {
    for (final a in v.items) {
      final q = _questionIn(a);
      if (q != null) return q;
    }
  }
  return null;
}

void main() {
  final programs = programsDir();
  final air = _Air();
  final phones = <_Phone>[];

  Future<_Phone> phone(String name) async {
    final kp = generateKeyPair();
    final net = _PhoneNetwork(kp, air);
    final host = SuperAppHost(
      agentId: name,
      program: '$programs/social/graph/core',
      rootSelfGlpPath: '$programs/self.glp',
      network: net,
      keyPair: kp,
      manifest: superappManifest,
    );
    final p = _Phone(name, host, net);
    phones.add(p);
    await host.start();
    await p.until(() => p.form() != null);
    return p;
  }

  tearDownAll(() {
    for (final p in phones) {
      p.host.dispose();
    }
  });

  late _Phone alice, bob;

  test('two phones: the first act superapp(Me, []) starts the Grassroots App, '
      'and home is drawn with Open place and Close place', () async {
    alice = await phone('alice');
    bob = await phone('bob');
    for (final p in [alice, bob]) {
      final w = formatTerm(p.form()!.widget);
      // No form takes a key typed: a person is known by an encounter, a link
      // or an introduction, and befriended by picking them in Nearby (GSA #2,
      // 2026-11-08 08:07 UTC, item 2).
      expect(w, isNot(contains('form(connect,')));
      expect(w, endsWith('button(open_place), button(close_place)]))'));
      await p.host.idle;
      expect(p.net.declared, ['pan closed', 'lan closed'],
          reason: 'both trusts are declared closed at start, and stay so '
              'until the place is opened');
    }
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('both open the place (GPE:1): each meets the other (GPE:2) and home '
      'shows each the other nearby', () async {
    alice.request(const GAtom('open_place'));
    bob.request(const GAtom('open_place'));
    await alice.until(() => alice.notices().contains('nearby([${bob.key}])'));
    await bob.until(() => bob.notices().contains('nearby([${alice.key}])'));
    for (final p in [alice, bob]) {
      expect(p.net.declared,
          ['pan closed', 'lan closed', 'pan open', 'lan open']);
      expect(p.errors, isEmpty);
    }
    // Each knows the other now, and declared them a known peer.
    expect(alice.net.known, contains(bob.key));
    expect(bob.net.known, contains(alice.key));
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('alice befriends bob (GSG:1) by picking him in Nearby, no key typed; '
      'bob says yes on his card, and both are told they are friends', () async {
    await alice.until(() => alice.nearbyCard(bob.key) != null);
    expect(formatTerm(alice.nearbyCard(bob.key)!.widget),
        "form(nearby, [field('Friend', shown), field('Befriend', button(befriend))])");
    alice.befriend(bob.key);
    await bob.until(() => bob.card() != null);
    bob.yes();
    await alice.until(() => alice.notices().contains('connected(${bob.key})'));
    await bob.until(() => bob.notices().contains('connected(${alice.key})'));
    expect(alice.errors, isEmpty);
    expect(bob.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('a third phone: carol opens her place, and she, alice and bob each see '
      'the other two nearby', () async {
    final carol = await phone('carol');
    carol.request(const GAtom('open_place'));
    final both = ([alice.key, bob.key]..sort()).join(', ');
    await carol.until(() => carol.notices().contains('nearby([$both])'));
    final ac = ([alice.key, carol.key]..sort()).join(', ');
    final bc = ([bob.key, carol.key]..sort()).join(', ');
    // Home shows the persons nearby in the order of their keys (place.glp,
    // nearby(Ks)).
    await alice.until(() => alice.notices().contains('nearby([$bc])'));
    await bob.until(() => bob.notices().contains('nearby([$ac])'));
    expect(carol.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('alice closes the place (GPE:1): the trust of both underlays is '
      'declared closed again', () async {
    alice.request(const GAtom('close_place'));
    await alice.host.idle;
    expect(alice.net.declared, [
      'pan closed', 'lan closed', 'pan open', 'lan open', 'pan closed',
      'lan closed'
    ]);
    expect(alice.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 3)));
}
