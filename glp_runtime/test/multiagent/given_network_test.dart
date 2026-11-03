/// The runtime's host over a network it is given (GLP #3 Cowork,
/// 2026-10-11 12:59 UTC, "01:05"): `AgentRuntime` takes a
/// `GlpNetwork`, and builds the simulation's only when it is given none
/// (IGLP, Implementation Notes, "Networking Seam", One interface: "transport
/// integration is a backend swap").  Over a given network an agent is named by
/// its key (IGLP, Remark "Agent Names": "Over a real network, the name of an
/// agent p is its public key, held by the networking layer"), and no key is
/// made up for a name.
///
/// The host runs two agents, each over its own network, and a cold call and
/// an assignment cross between them, as GNA's adapter test has them cross
/// between two runtimes over two headless layers (networking/core/test/
/// glp_adapter_test.dart): the caller cold-calls the callee, naming it by its
/// key, with a writer; the callee assigns the writer, and the assignment
/// crosses back.  The networks are fakes that carry payloads between keys, in
/// one isolate.  `IsolateManager` boots a simulation alone (IGLP,
/// Implementation Notes, "Boot"), and until 2026-11-02 it ran over given
/// layers too.
@Timeout.none
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart' show ConstTerm, StructTerm;

/// The agents' one program: GNA's adapter test's, each agent started from
/// agent(ch(_?,_),ch(_?,_)), every agent's initial goal (GLP-Spec maglp.tex,
/// Definition "Multiagent GLP"; IGLP, Implementation Notes, "Boot": "a
/// deployed agent is spawned by its application with the same initial
/// goal"), the caller's person naming the callee by its key, call(Key), and
/// the callee's telling its agent to answer.
const String _play = r'''
GnIn  ::= call(Key) ; answer.
GnOut ::= answered(Constant) ; called(Key).
GnMsg ::= ask(Constant?).

exported procedure agent(Channel(Stream(GnIn), Stream(GnOut))?, Channel(Stream(NetMsg(GnMsg)), Stream(NetMsg(GnMsg)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).

procedure start(Stream(GnIn)?, Stream(NetMsg(GnMsg))?).
start([call(Callee) | _], _) :- ground(Callee?) |
    send_to_net([msg(Callee?, ask(Answer))]),
    answered(Answer?).
start([answer | _], [msg(Me, ask(A?)) | _]) :-
    send_to_user([called(Me?)]),
    answer(A).
start([], _).

procedure answered(Constant?).
answered(forty_two) :- send_to_user([answered(forty_two)]).

procedure answer(Constant).
answer(forty_two).
''';

/// An agent that cold-calls the peer its person names: `bob` in the tests, a
/// readable name, which over a network stands for no key (IGLP 670d36a,
/// Remark "Agent Names").
const String _symbolic = r'''
exported procedure agent(Channel(Stream(Key), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch([P | _], []), ch(_, [])) :- send_to_net([msg(P?, ping)]).
agent(ch([], []), ch(_, [])).
''';

/// An agent that declares a place and tells its person when it is entered.
const String _placer = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])) :- declare(place(home, 50, E)), watch(E?).

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

  /// The messageIds the runtime acknowledged, in order, each an event a test
  /// waits on ([_Lines.lineWhere]).
  final _Lines acknowledged = _Lines();

  _WireNetwork(this.keyPair, this.wire) {
    wire._at[keyPair.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => keyPair.pub;

  @override
  void send(PubKey pk, Uint8List payload) => wire.carry(keyPair.pub, pk, payload);

  @override
  void acknowledge(String messageId) => acknowledged.add(messageId);

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
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
}

/// An agent's transcript, AgentRuntime.onOutput's lines, or the messageIds a
/// layer is told are acknowledged, which a test waits on one by one: on the
/// event, a line's arrival, and on no clock.  Until
/// 2026-11-02 a test polled the lines every 50 ms until a deadline of sixty
/// seconds, so its verdict depended on how loaded the machine was (IGLP-coop).
/// A line the agent reports as an error, `[ERROR] ...`, fails every wait.
class _Lines extends Iterable<String> {
  final List<String> _lines = [];
  final List<(bool Function(String), Completer<String>)> _waits = [];

  @override
  Iterator<String> get iterator => _lines.iterator;

  void add(String line) {
    _lines.add(line);
    if (line.startsWith('[ERROR]')) {
      for (final (_, c) in _waits) {
        c.completeError(StateError('the agent reported an error: $line'));
      }
      _waits.clear();
      return;
    }
    _waits.removeWhere((w) {
      final (test, c) = w;
      if (!test(line)) return false;
      c.complete(line);
      return true;
    });
  }

  /// The first line [test] accepts: one already written, or the next.
  Future<String> lineWhere(bool Function(String) test) {
    for (final l in _lines) {
      if (test(l)) return Future.value(l);
    }
    final c = Completer<String>();
    _waits.add((test, c));
    return c.future;
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
        keyPair: callerKp,
        network: _WireNetwork(callerKp, wire),
      );
      final callee = AgentRuntime(
        agentId: 'callee',
        program: play,
        rootSelfGlpPath: root,
        keyPair: calleeKp,
        network: _WireNetwork(calleeKp, wire),
      );
      final out = {'caller': _Lines(), 'callee': _Lines()};
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
      // Each person tells its agent what to do: the callee's to answer, the
      // caller's to call the callee, by its key.
      await callee.injectUserInput(ConstTerm('answer'));
      await caller.injectUserInput(
          StructTerm('call', [ConstTerm(calleeKp.pub.hex)]));

      // The cold call crosses: the callee's program receives it on its
      // network input, addressed to its own name, its key.
      final called =
          await out['callee']!.lineWhere((l) => l.startsWith('< called('));
      expect(called, contains(calleeKp.pub.hex));

      // The assignment crosses back: the caller's writer, assigned by the
      // callee, reaches the caller's program.
      final answered =
          await out['caller']!.lineWhere((l) => l.startsWith('< answered('));
      expect(answered, contains('forty_two'));

      expect(wire.carried, contains('${callerKp.pub.hex} > ${calleeKp.pub.hex}'),
          reason: 'the cold call left through the caller\'s network');
      expect(wire.carried, contains('${calleeKp.pub.hex} > ${callerKp.pub.hex}'),
          reason: 'the assignment left through the callee\'s network');
      expect(viaCoordinator, isEmpty);
    });

    test('a delivered message is acknowledged to the layer once the cycle '
        'that applied it is complete', () async {
      // GNA 776016c, Section 2.9: acknowledge(messageId) "Confirm[s] that the
      // message delivered under messageId has been applied and saved by the
      // runtime"; IGLP, Implementation Notes, "Persistence": "a delivered
      // message acknowledged to the layer, only from a state the store holds",
      // and with no store once its cycle is complete.
      final wire = _Wire();
      final callerKp = generateKeyPair();
      final calleeKp = generateKeyPair();
      final callerNet = _WireNetwork(callerKp, wire);
      final calleeNet = _WireNetwork(calleeKp, wire);
      final caller = AgentRuntime(
        agentId: 'caller',
        program: play,
        rootSelfGlpPath: root,
        keyPair: callerKp,
        network: callerNet,
      );
      final callee = AgentRuntime(
        agentId: 'callee',
        program: play,
        rootSelfGlpPath: root,
        keyPair: calleeKp,
        network: calleeNet,
      );
      final out = _Lines();
      caller.onOutput = out.add;
      // Over a given network the layer is told, and the coordinator is not.
      final toCoordinator = <String>[];
      caller.onDelivered = toCoordinator.add;
      callee.onDelivered = toCoordinator.add;
      await callee.initialize();
      await caller.initialize();
      // Each person tells its agent what to do: the callee's to answer, the
      // caller's to call the callee, by its key.
      await callee.injectUserInput(ConstTerm('answer'));
      await caller.injectUserInput(
          StructTerm('call', [ConstTerm(calleeKp.pub.hex)]));
      await out.lineWhere((l) => l.startsWith('< answered('));
      // The cold call was the wire's first message, w0, the assignment back
      // its second, w1; each is acknowledged by the agent that applied it.
      await calleeNet.acknowledged.lineWhere((id) => id == 'w0');
      await callerNet.acknowledged.lineWhere((id) => id == 'w1');
      expect(calleeNet.acknowledged, ['w0']);
      expect(callerNet.acknowledged, ['w1']);
      expect(toCoordinator, isEmpty);
    });

    test('no key is made up: a message to a name that is no key is not sent, '
        'and is reported', () async {
      final wire = _Wire();
      final kp = generateKeyPair();
      final agent = AgentRuntime(
        agentId: 'a',
        program: symbolic,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: _WireNetwork(kp, wire),
      );
      final out = <String>[];
      agent.onOutput = out.add;
      await agent.initialize();
      await agent.injectUserInput(ConstTerm('bob'));
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
        keyPair: kp,
        network: network,
      );
      final out = _Lines();
      agent.onOutput = out.add;
      await agent.initialize();
      expect(network.declared, ['home']);

      // The layer reports the entry; the run that follows it reduces the
      // goal suspended on the place's stream, with no other event.
      network.onPlaceEvent!('home', PlaceEvent.entered);
      expect(await out.lineWhere((l) => l.startsWith('< ')), '< entered');
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

  // IsolateManager boots a simulation alone (IGLP, Implementation Notes,
  // "Boot": "A boot is for a simulation; a deployed agent is booted by its
  // application"), each agent's key made at its spawn: until 2026-11-02 it also
  // ran agents over given layers, each named in the boot clause by a key made
  // before the boot, and three tests here held that.  A deployed agent over a
  // given layer is AgentRuntime's, above.
}
