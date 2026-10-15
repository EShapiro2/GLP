/// The runtime's hosts over a network they are given (GLP #3 Cowork,
/// 2026-10-11 12:59 UTC, "01:05"): `AgentRuntime` and `IsolateManager` take a
/// `GlpNetwork`, and build the simulation's only when they are given none
/// (IGLP, Implementation Notes, "Networking Seam", One interface: "transport
/// integration is a backend swap").  Over a given network an agent is named by
/// its key (IGLP, Remark "Agent Names": "Over a real network, the name of an
/// agent p is its public key, held by the networking layer"), and no key is
/// made up for a name.
///
/// Each host runs two agents, each over its own network, and a cold call and
/// an assignment cross between them, as GNA's adapter test has them cross
/// between two runtimes over two headless layers (networking/core/test/
/// glp_adapter_test.dart): the caller cold-calls the callee, naming it by its
/// key, with a writer; the callee assigns the writer, and the assignment
/// crosses back.  The networks are fakes that carry payloads between keys:
/// `AgentRuntime`'s in one isolate, `IsolateManager`'s through a hub, each
/// opened in its agent's isolate.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// The agents' one program: GNA's adapter test's, the caller naming the
/// callee by the key its host passes it.
const String _play = r'''
procedure caller_init(_?, Key?, _?).
caller_init(_, Callee, _) :-
    send_to_net([msg(Callee?, ask(Answer))]),
    answered(Answer?).

procedure answered(_?).
answered(forty_two) :- send_to_user([answered(forty_two)]).

procedure callee_init(_?, _?).
callee_init(_, [msg(Me, ask(A?)) | _]) :-
    send_to_user([called(Me?)]),
    answer(A).

procedure answer(_).
answer(forty_two).
''';

/// An agent that cold-calls `bob`, a name that is no key.
const String _symbolic = r'''
procedure a_init(_?, _?).
a_init(_, _) :- send_to_net([msg(bob, ping)]).
''';

/// An agent that declares a place and tells its person when it is entered.
const String _placer = r'''
procedure placer_init(_?, _?).
placer_init(_, _) :- place_declare(home, 50, E), watch(E?).

procedure watch(_?).
watch([E|_]) :- ground(E?) | send_to_user([E?]).
''';

typedef _KeyPair = ({PubKey pub, Uint8List priv});

// =============================================================================
// AgentRuntime's network: a wire between keys, in one isolate
// =============================================================================

/// Carries payloads between the [_WireNetwork]s joined to it, by key, each
/// delivery in a later turn of the event loop, as a layer delivers.
class _Wire {
  final Map<String, _WireNetwork> _at = {};
  int _next = 0;

  /// Each payload carried, as `<from-hex> > <to-hex>`.
  final List<String> carried = [];

  void carry(PubKey from, PubKey to, Uint8List payload) {
    carried.add('${from.hex} > ${to.hex}');
    final id = 'w${_next++}';
    Timer.run(() =>
        _at[to.hex]?.onMessageReceived?.call(from, payload, id, Underlay.wan));
  }
}

/// A [GlpNetwork] on a [_Wire], instantiated with its key pair.
class _WireNetwork extends GlpNetwork {
  final _KeyPair keyPair;
  final _Wire wire;

  /// Places declared, in order.
  final List<String> declared = [];

  _WireNetwork(this.keyPair, this.wire) {
    wire._at[keyPair.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => keyPair.pub;

  @override
  void send(PubKey pk, Uint8List payload) => wire.carry(keyPair.pub, pk, payload);

  @override
  Future<bool> declarePlace(String place, double radiusMetres) {
    declared.add(place);
    return Future.value(true);
  }

  @override
  Future<void> removePlace(String place) => Future.value();

  // --- Not exercised here ---

  @override
  void putKnownPeer(PubKey pk) {}
  @override
  void removeKnownPeer(PubKey pk) {}
  @override
  bool isPeerReachable(PubKey pk) => wire._at.containsKey(pk.hex);
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [Underlay.wan];
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {}
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
}

// =============================================================================
// IsolateManager's networks: a hub in this isolate, a layer in each agent's
// =============================================================================

/// Carries payloads between the [_PortNetwork]s registered with it, by key:
/// each layer, opened in its agent's isolate, registers its inbox, and a
/// payload for a key not yet registered waits for it.
class _Hub {
  final ReceivePort _port = ReceivePort();
  final Map<String, SendPort> _inboxes = {};
  final Map<String, List<List<Object>>> _waiting = {};

  /// Each payload carried, as `<from-hex> > <to-hex>`.
  final List<String> carried = [];

  _Hub() {
    _port.listen((message) {
      final m = message as List;
      if (m[0] == 'register') {
        final key = m[1] as String;
        final inbox = m[2] as SendPort;
        _inboxes[key] = inbox;
        for (final d in _waiting.remove(key) ?? const <List<Object>>[]) {
          inbox.send(d);
        }
      } else if (m[0] == 'send') {
        final from = m[1] as String;
        final to = m[2] as String;
        carried.add('$from > $to');
        final delivery = <Object>[from, m[3] as Uint8List];
        final inbox = _inboxes[to];
        if (inbox == null) {
          _waiting.putIfAbsent(to, () => []).add(delivery);
        } else {
          inbox.send(delivery);
        }
      }
    });
  }

  SendPort get sendPort => _port.sendPort;

  void close() => _port.close();
}

/// A [GlpNetwork] in an agent's isolate, carrying payloads through a [_Hub].
class _PortNetwork extends GlpNetwork {
  final PubKey key;
  final SendPort hub;
  final ReceivePort _inbox = ReceivePort();
  int _next = 0;

  _PortNetwork._(this.key, this.hub) {
    _inbox.listen((message) {
      final m = message as List;
      onMessageReceived?.call(
          PubKey.fromHex(m[0] as String), m[1] as Uint8List, 'h${_next++}',
          Underlay.wan);
    });
    hub.send(['register', key.hex, _inbox.sendPort]);
  }

  @override
  PubKey getIdentity() => key;

  @override
  void send(PubKey pk, Uint8List payload) =>
      hub.send(['send', key.hex, pk.hex, payload]);

  // --- Not exercised here ---

  @override
  void putKnownPeer(PubKey pk) {}
  @override
  void removeKnownPeer(PubKey pk) {}
  @override
  bool isPeerReachable(PubKey pk) => true;
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [Underlay.wan];
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {}
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
  Future<bool> declarePlace(String place, double radiusMetres) =>
      Future.value(false);
  @override
  Future<void> removePlace(String place) => Future.value();
}

/// The layer of the agent of [keyPair], opened in its isolate over [hub].  A
/// top-level function, so that the closure it returns captures the two alone
/// and is sendable.
AgentLayer _layerOver(_KeyPair keyPair, SendPort hub) => AgentLayer(
      keyPair: keyPair,
      open: () async => _PortNetwork._(keyPair.pub, hub),
    );

/// The first line [lines] yields that [test] accepts, polling until
/// [timeout].
Future<String> _eventually(Iterable<String> Function() lines,
    bool Function(String) test,
    {Duration timeout = const Duration(seconds: 60)}) async {
  final deadline = DateTime.now().add(timeout);
  while (true) {
    for (final l in lines()) {
      if (test(l)) return l;
    }
    if (DateTime.now().isAfter(deadline)) {
      throw TimeoutException('no line came; the lines: ${lines().join(' | ')}');
    }
    await Future<void>.delayed(const Duration(milliseconds: 50));
  }
}

void main() {
  late Directory dir;
  late String root;
  late String play;
  late String symbolic;
  late String placer;

  setUpAll(() {
    // Written under the root, programs/ (TGLP modules.tex, "Scope
    // construction": "A program lies at or below the root"), as
    // agent_runtime_test.dart writes its play.
    dir = Directory('../programs/tests').createTempSync('glp_given_network_');
    play = '${dir.path}/play.glp';
    File(play).writeAsStringSync(_play);
    symbolic = '${dir.path}/symbolic.glp';
    File(symbolic).writeAsStringSync(_symbolic);
    placer = '${dir.path}/placer.glp';
    File(placer).writeAsStringSync(_placer);
    root = File('../programs/self.glp').absolute.path;
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  group('AgentRuntime over a given network', () {
    test('a cold call and an assignment cross between two agents, each named '
        'by its key', () async {
      final wire = _Wire();
      final callerKp = generateKeyPair();
      final calleeKp = generateKeyPair();
      final caller = AgentRuntime(
        agentId: 'caller',
        program: play,
        rootSelfGlpPath: root,
        goalLabel: 'caller_init/3',
        extraArgs: [calleeKp.pub.hex],
        keyPair: callerKp,
        network: _WireNetwork(callerKp, wire),
      );
      final callee = AgentRuntime(
        agentId: 'callee',
        program: play,
        rootSelfGlpPath: root,
        goalLabel: 'callee_init/2',
        keyPair: calleeKp,
        network: _WireNetwork(calleeKp, wire),
      );
      final out = <String, List<String>>{'caller': [], 'callee': []};
      caller.onOutput = out['caller']!.add;
      callee.onOutput = out['callee']!.add;
      // A given network delivers its own messages: the coordinator's channel
      // carries nothing.
      final viaCoordinator = <String>[];
      caller.onSendMadMessage = (to, _) async => viaCoordinator.add(to);
      callee.onSendMadMessage = (to, _) async => viaCoordinator.add(to);

      expect(caller.name, callerKp.pub.hex,
          reason: 'over a given network the agent is named by its key');
      expect(callee.name, calleeKp.pub.hex);

      await callee.initialize();
      await caller.initialize();

      // The cold call crosses: the callee's program receives it on its
      // network input, addressed to its own name, its key.
      final called = await _eventually(
          () => out['callee']!, (l) => l.startsWith('< called('));
      expect(called, contains(calleeKp.pub.hex));

      // The assignment crosses back: the caller's writer, assigned by the
      // callee, reaches the caller's program.
      final answered = await _eventually(
          () => out['caller']!, (l) => l.startsWith('< answered('));
      expect(answered, contains('forty_two'));

      expect(wire.carried, contains('${callerKp.pub.hex} > ${calleeKp.pub.hex}'),
          reason: 'the cold call left through the caller\'s network');
      expect(wire.carried, contains('${calleeKp.pub.hex} > ${callerKp.pub.hex}'),
          reason: 'the assignment left through the callee\'s network');
      expect(viaCoordinator, isEmpty);
    });

    test('no key is made up: a message to a name that is no key is not sent, '
        'and is reported', () async {
      final wire = _Wire();
      final kp = generateKeyPair();
      final agent = AgentRuntime(
        agentId: 'a',
        program: symbolic,
        rootSelfGlpPath: root,
        goalLabel: 'a_init/2',
        keyPair: kp,
        network: _WireNetwork(kp, wire),
      );
      final out = <String>[];
      agent.onOutput = out.add;
      await agent.initialize();
      expect(wire.carried, isEmpty,
          reason: 'bob names no agent over the network, and no key is made up '
              'for it');
      expect(out, contains(startsWith('[ERROR] a message to bob')));
    });

    test('a place event the layer reports is followed by a run', () async {
      final wire = _Wire();
      final kp = generateKeyPair();
      final network = _WireNetwork(kp, wire);
      final agent = AgentRuntime(
        agentId: 'placer',
        program: placer,
        rootSelfGlpPath: root,
        goalLabel: 'placer_init/2',
        keyPair: kp,
        network: network,
      );
      final out = <String>[];
      agent.onOutput = out.add;
      await agent.initialize();
      expect(network.declared, ['home']);

      // The layer reports the entry; the run that follows it reduces the
      // goal suspended on the place's stream, with no other event.
      network.onPlaceEvent!('home', PlaceEvent.entered);
      expect(await _eventually(() => out, (l) => l.startsWith('< ')),
          '< entered');
    });

    test('the agent\'s key pair is the layer\'s', () {
      final wire = _Wire();
      final kp = generateKeyPair();
      final other = generateKeyPair();
      expect(
          () => AgentRuntime(
                agentId: 'a',
                program: play,
                rootSelfGlpPath: root,
                network: _WireNetwork(kp, wire),
              ),
          throwsArgumentError,
          reason: 'the layer keeps its private key; the host is given the pair');
      expect(
          () => AgentRuntime(
                agentId: 'a',
                program: play,
                rootSelfGlpPath: root,
                keyPair: other,
                network: _WireNetwork(kp, wire),
              ),
          throwsArgumentError,
          reason: 'an agent has one key, its name');
    });

    test('a coordinator delivers nothing to an agent over a given network',
        () async {
      final kp = generateKeyPair();
      final agent = AgentRuntime(
        agentId: 'a',
        program: symbolic,
        rootSelfGlpPath: root,
        goalLabel: 'a_init/2',
        keyPair: kp,
        network: _WireNetwork(kp, _Wire()),
      );
      await agent.initialize();
      await expectLater(
          agent.onMadMessageReceived('b', Uint8List(0)), throwsStateError);
    });
  });

  group('AgentRuntime over the simulation', () {
    test('an unknown sender is a fault: its message is dropped and reported, '
        'not received from \'?\'', () async {
      final agent = AgentRuntime(
        agentId: 'a',
        program: symbolic,
        rootSelfGlpPath: root,
        goalLabel: 'a_init/2',
      );
      final out = <String>[];
      final logs = <String>[];
      agent.onOutput = out.add;
      agent.onLog = (_, m) => logs.add(m);
      await agent.initialize();
      expect(agent.name, 'a', reason: 'the simulation names agents symbolically');

      final stranger = generateKeyPair().pub;
      agent.ctx!.network!.onMessageReceived!(
          stranger, Uint8List.fromList([0, 1, 2]), 'm', Underlay.pan);
      expect(out, contains('[ERROR] a message from ${stranger.hex}, which '
          'names no agent, is dropped'));
      expect(logs.where((l) => l.startsWith('MAD_ERROR') && !l.contains('dropped')),
          isEmpty,
          reason: 'the message is not received, so nothing decodes it');
    });
  });

  group('IsolateManager over given layers', () {
    test('a cold call and an assignment cross between two agents in two '
        'isolates, each over its own layer and named by its key', () async {
      final hub = _Hub();
      addTearDown(hub.close);
      final callerKp = generateKeyPair();
      final calleeKp = generateKeyPair();
      final caller = callerKp.pub.hex;
      final callee = calleeKp.pub.hex;

      final manager = IsolateManager(layers: [
        _layerOver(callerKp, hub.sendPort),
        _layerOver(calleeKp, hub.sendPort),
      ]);
      addTearDown(manager.shutdown);

      // The boot program names each agent by its key (IGLP, "Boot": "each
      // body goal G@p directs the runtime to spawn an isolate for agent p").
      final config = BootLoader().load('''
procedure boot.
boot :- caller_init('$caller', '$callee', _)@'$caller',
        callee_init('$callee', _)@'$callee'.

$_play''');
      config.rootSelfGlpPath = root;
      expect(config.directives.map((d) => d.agentId), [caller, callee]);

      await manager.boot(config);
      manager.start();

      final called = await _eventually(
          () => manager.outputOf(callee), (l) => l.contains('called('));
      expect(called, contains(callee));
      final answered = await _eventually(
          () => manager.outputOf(caller), (l) => l.contains('answered('));
      expect(answered, contains('forty_two'));

      expect(hub.carried, contains('$caller > $callee'),
          reason: 'the cold call left through the caller\'s layer');
      expect(hub.carried, contains('$callee > $caller'),
          reason: 'the assignment left through the callee\'s layer');
      expect(manager.faults, isEmpty);
    }, timeout: const Timeout(Duration(minutes: 2)));

    test('an agent the manager holds no layer for is refused, and no key is '
        'made up for it', () async {
      final hub = _Hub();
      addTearDown(hub.close);
      final kp = generateKeyPair();
      final manager = IsolateManager(layers: [_layerOver(kp, hub.sendPort)]);
      addTearDown(manager.shutdown);
      final config = BootLoader().load('''
procedure boot.
boot :- a_init(alice, _)@alice.

$_symbolic''');
      config.rootSelfGlpPath = root;
      await expectLater(manager.boot(config), throwsStateError);
    });

    test('the simulation\'s harness controls are refused over given layers',
        () {
      final kp = generateKeyPair();
      final manager = IsolateManager(layers: [
        AgentLayer(
            keyPair: kp,
            open: () async => throw UnimplementedError('not opened here')),
      ]);
      expect(() => manager.cut('a', 'b'), throwsStateError);
      expect(() => manager.restore('a', 'b'), throwsStateError);
      expect(() => manager.holdDelivery('a', 'b'), throwsStateError);
      expect(() => manager.releaseDelivery('a', 'b'), throwsStateError);
      expect(() => manager.trustLevelOf('a', ProximityUnderlay.pan),
          throwsStateError);
    });
  });
}
