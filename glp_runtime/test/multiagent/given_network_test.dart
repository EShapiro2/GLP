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
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/seam_declarations.dart';
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

/// An agent that declares a watch on the agents nearby over the PAN and
/// tells its person each presence (GLP-Spec 210cee1, "Networking seam":
/// nearby(U, S), Presence ::= seen(Key) ; gone(Key)).
const String _watcher = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])) :- declare(nearby(pan, S)), watch(S?).

procedure watch(Stream(Presence)?).
watch([P|S]) :- ground(P?) | send_to_user([P?]), watch(S?).
watch([]) :- send_to_user([closed]).
''';

/// An agent that makes a declaration of each kind a layer function takes ---
/// a peer, its address, a rendezvous server, both underlays' trust levels and
/// a place --- and tells its person each event of the place (GLP-Spec
/// appendix-guards, "Networking seam"; IGLP 0c77e47, Definition "Seam
/// Predicates").  PEER and SERVER are replaced by keys.
const String _declarer = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])) :-
    declare(peer("PEER")), declare(address("PEER", "192.0.2.1:4000")),
    declare(rendezvous("SERVER", "192.0.2.2:4000")),
    declare(trust(pan, open)), declare(trust(lan, closed)),
    declare(place(home, 50, E)), watch(E?).

procedure watch(Stream(PlaceEvent)?).
watch([E | S]) :- ground(E?) | send_to_user([E?]), watch(S?).
watch([]).
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

  /// How many sends still to refuse, as a layer refuses a message it cannot
  /// save (GNA, Section 2.9: send "fails if the message cannot be saved").
  int refuse = 0;

  /// The sends refused, by destination key.
  final List<String> refused = [];

  /// The messageIds the runtime acknowledged, in order, each an event a test
  /// waits on ([_Lines.lineWhere]).
  final _Lines acknowledged = _Lines();

  _WireNetwork(this.keyPair, this.wire) {
    wire._at[keyPair.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => keyPair.pub;

  @override
  Future<void> send(PubKey pk, Uint8List payload) {
    if (refuse > 0) {
      refuse--;
      refused.add(pk.hex);
      return Future.error(StateError('the message could not be saved'));
    }
    wire.carry(keyPair.pub, pk, payload);
    return Future.value();
  }

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

/// A [_WireNetwork] that writes each declaration it is given into [log], as
/// `layer: <function>(<arguments>)`, so a test can see what the layer was
/// told and in what order with what the agent did.
class _RecordingNetwork extends _WireNetwork {
  final List<String> log;

  _RecordingNetwork(super.keyPair, super.wire, this.log);

  @override
  void putKnownPeer(PubKey pk) => log.add('layer: putKnownPeer(${pk.hex})');
  @override
  void removeKnownPeer(PubKey pk) =>
      log.add('layer: removeKnownPeer(${pk.hex})');
  @override
  void putPeerAddress(PubKey pk, String address) =>
      log.add('layer: putPeerAddress(${pk.hex}, $address)');
  @override
  void putRendezvousServer(PubKey pk, String address) =>
      log.add('layer: putRendezvousServer(${pk.hex}, $address)');
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      log.add('layer: setTrustLevel(${underlay.name}, ${level.name})');
  @override
  Future<bool> declarePlace(String place, double radiusMetres) {
    log.add('layer: declarePlace($place, $radiusMetres)');
    return super.declarePlace(place, radiusMetres);
  }
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
  late String watcher;
  late String declarer;
  final declaredPeer = generateKeyPair().pub;
  final declaredServer = generateKeyPair().pub;

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
    watcher = '${dir.path}/watcher.glp';
    File(watcher).writeAsStringSync(_watcher);
    declarer = '${dir.path}/declarer.glp';
    File(declarer).writeAsStringSync(_declarer
        .replaceAll('PEER', declaredPeer.hex)
        .replaceAll('SERVER', declaredServer.hex));
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

    test('a send the layer refuses is kept and sent again, and arrives once',
        () async {
      // IGLP b014a9e, Implementation Notes, "Networking Seam", The contract:
      // "the layer refuses to send a message it cannot save, and the runtime
      // waits for its answer and, on a refusal, keeps the message and sends
      // it again later"; GNA, Section 2.9: send "fails if the message cannot
      // be saved".  The caller's layer refuses the cold call once; the cycle
      // of the caller's person's act call(Key) ends with it back in M_p,
      // undelivered; the layer then reports the callee connected, and the
      // caller hands it over again, once.
      final wire = _Wire();
      final callerKp = generateKeyPair();
      final calleeKp = generateKeyPair();
      final callerNet = _WireNetwork(callerKp, wire)..refuse = 1;
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
        network: _WireNetwork(calleeKp, wire),
      );
      final out = {'caller': _Lines(), 'callee': _Lines()};
      caller.onOutput = out['caller']!.add;
      callee.onOutput = out['callee']!.add;
      await callee.initialize();
      await caller.initialize();
      // Each person tells its agent what to do: the callee's to answer, the
      // caller's to call the callee, by its key.
      await callee.injectUserInput(ConstTerm('answer'));
      await caller.injectUserInput(
          StructTerm('call', [ConstTerm(calleeKp.pub.hex)]));

      // The act's cycle waited for the layer's answer: the cold call was
      // refused, nothing was carried, and the message is in M_p, undelivered.
      expect(callerNet.refused, [calleeKp.pub.hex]);
      expect(wire.carried, isEmpty);
      expect(caller.ctx!.mp.sendableLength, 1,
          reason: 'a refused message is kept');

      // The layer reports the callee connected: the message is sent again.
      callerNet.onPeerConnected!(calleeKp.pub, Underlay.wan, null);
      await out['caller']!.lineWhere((l) => l.startsWith('< answered('));
      expect(out['callee']!.where((l) => l.startsWith('< called(')),
          hasLength(1),
          reason: 'the cold call arrives once');
      expect(
          wire.carried
              .where((c) => c == '${callerKp.pub.hex} > ${calleeKp.pub.hex}'),
          hasLength(1),
          reason: 'the caller sent the callee one message, the cold call, '
              'once it was accepted');
      expect(caller.ctx!.mp.sendableLength, 0);
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

    test('a nearby declaration is fed from what the layer reports: seen on '
        'discovery, gone on the loss, seen on discovery again; a session is '
        'no presence; each followed by a run', () async {
      // GLP-Spec 210cee1, "Networking seam": "seen(K) when the layer
      // discovers on U an agent of key K that it recognises or, under open
      // trust, contacts, and gone(K) when that agent can no longer be reached
      // over U"; GNA cfbfeac, Section 3.1: onPeerDiscovered(pk) and
      // onPeerLost(pk) on the underlay's sub-object, "behind the stream of
      // declare(nearby(Underlay, Events))".  Over a given network K is the
      // key.
      final wire = _Wire();
      final kp = generateKeyPair();
      final network = _WireNetwork(kp, wire);
      final agent = AgentRuntime(
        agentId: 'watcher',
        program: watcher,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: network,
      );
      final out = _Lines();
      agent.onOutput = out.add;
      await agent.initialize();
      final peer = generateKeyPair().pub;
      final other = generateKeyPair().pub;
      Future<String> next(int n) async {
        var count = 0;
        return out.lineWhere((l) => l.startsWith('< ') && ++count == n);
      }

      network.onPeerDiscovered!(peer, ProximityUnderlay.pan);
      expect(await next(1), '< seen(${peer.hex})');
      network.onPeerConnected!(peer, Underlay.pan, null); // no presence
      network.onPeerConnected!(other, Underlay.wan, null); // no proximity
      network.onPeerDiscovered!(other, ProximityUnderlay.lan); // another stream
      network.onPeerDisconnected!(peer, Underlay.pan); // no presence
      network.onPeerLost!(peer, ProximityUnderlay.pan);
      expect(await next(2), '< gone(${peer.hex})');
      network.onPeerDiscovered!(peer, ProximityUnderlay.pan);
      expect(await next(3), '< seen(${peer.hex})');
      expect([for (final l in out) if (l.startsWith('< ')) l], [
        '< seen(${peer.hex})',
        '< gone(${peer.hex})',
        '< seen(${peer.hex})',
      ]);
    });

    test('a nearby declaration survives a restart from the saved state, and '
        'its stream is fed by the layer instantiated again', () async {
      // IGLP, Implementation Notes, "Persistence": the store keeps the
      // places and trust levels declared, and a restart resumes from it; a
      // nearby declaration's stream is fed and closed as a place's (IGLP
      // b20951d), and is kept with them.  What the layer reports is the
      // layer's, and is reported again by the layer the restart instantiates.
      final kp = generateKeyPair();
      final store = MemoryAgentStateStore();
      final first = _WireNetwork(kp, _Wire());
      final agent = AgentRuntime(
        agentId: 'watcher',
        program: watcher,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: first,
        stateStore: store,
      );
      final out = _Lines();
      agent.onOutput = out.add;
      await agent.initialize();
      final peer = generateKeyPair().pub;
      first.onPeerDiscovered!(peer, ProximityUnderlay.pan);
      expect(await out.lineWhere((l) => l.startsWith('< ')),
          '< seen(${peer.hex})');

      final second = _WireNetwork(kp, _Wire());
      final again = AgentRuntime(
        agentId: 'watcher',
        program: watcher,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: second,
        stateStore: store,
      );
      final out2 = _Lines();
      again.onOutput = out2.add;
      await again.initialize();
      expect(again.firstStart, isFalse);
      expect(again.runtime!.nearbyStreams.isDeclared('pan'), isTrue);
      final other = generateKeyPair().pub;
      second.onPeerDiscovered!(other, ProximityUnderlay.pan);
      expect(await out2.lineWhere((l) => l.startsWith('< ')),
          '< seen(${other.hex})');
    });

    test('at a restart from the saved state every standing declaration is '
        'made to the new layer, in the order made, before any goal of the '
        'agent is reduced; the program does not declare again', () async {
      // IGLP 0c77e47, Implementation Notes, "Persistence": the store keeps
      // "every declaration standing --- the peers, the addresses, the
      // rendezvous servers, the trust levels, the places, the nearby and
      // reachable declarations"; "The layer starts empty at every launch,
      // and at a restart the runtime makes every declaration the store holds
      // to it again before any goal of the agent is reduced".  Seen on two
      // phones until 2026-11-08: after a restart the layer, a new process,
      // knew no peer, trust level or nearby stream.
      final kp = generateKeyPair();
      final store = MemoryAgentStateStore();
      final firstLog = <String>[];
      final first = _RecordingNetwork(kp, _Wire(), firstLog);
      final agent = AgentRuntime(
        agentId: 'declarer',
        program: declarer,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: first,
        stateStore: store,
      );
      await agent.initialize();
      final made = [
        'layer: putKnownPeer(${declaredPeer.hex})',
        'layer: putPeerAddress(${declaredPeer.hex}, 192.0.2.1:4000)',
        'layer: putRendezvousServer(${declaredServer.hex}, 192.0.2.2:4000)',
        'layer: setTrustLevel(pan, open)',
        'layer: setTrustLevel(lan, closed)',
        'layer: declarePlace(home, 50.0)',
      ];
      expect(firstLog, unorderedEquals(made),
          reason: 'the first start makes each declaration once');

      final log = <String>[];
      final second = _RecordingNetwork(kp, _Wire(), log);
      final again = AgentRuntime(
        agentId: 'declarer',
        program: declarer,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: second,
        stateStore: store,
      );
      // Each goal reduced is traced, `GLP: ...`, once onLog is set.
      again.onLog = (_, m) {
        if (m.startsWith('GLP: ')) log.add('reduced');
      };
      final out = _Lines();
      again.onOutput = out.add;
      await again.initialize();
      expect(again.firstStart, isFalse);
      expect(log.where((l) => l.startsWith('layer: ')).toList(),
          [for (final l in firstLog) l],
          reason: 'every standing declaration is made to the new layer again, '
              'in the order the first start made them, and the program '
              'makes none');

      // The restored goal waits on the place's stream; the new layer's
      // report reaches it, and is the first reduction after the restart.
      second.onPlaceEvent!('home', PlaceEvent.entered);
      expect(await out.lineWhere((l) => l.startsWith('< ')), '< entered');
      final firstReduction = log.indexOf('reduced');
      expect(firstReduction, greaterThan(0));
      expect(log.sublist(firstReduction).where((l) => l.startsWith('layer: ')),
          isEmpty,
          reason: 'every declaration is made before any goal is reduced');
    });

    test('the standing record takes off what a removal removes, and a '
        'further declaration replaces the one standing and goes last',
        () async {
      // GLP-Spec appendix-guards, "Networking seam": remove(peer(P)) "takes
      // P, and any address recorded for it, off the layer's known peers";
      // remove(rendezvous(P)) takes the server off; a later trust
      // declaration of an underlay replaces the level then standing; a
      // further declaration of the same place supersedes the earlier one.
      final r = SeamDeclarations()
        ..peer('a')
        ..address('a', 'x')
        ..peer('b')
        ..address('b', 'y')
        ..rendezvous('s', 'z')
        ..trust('pan', 'open')
        ..place('home', 50)
        ..trust('pan', 'closed')
        ..removePeer('a')
        ..removeRendezvous('s')
        ..place('work', 10)
        ..removePlace('work')
        ..address('b', 'w');
      expect([for (final d in r.standing) '$d'], [
        'peer(b)',
        'place(home, 50)',
        'trust(pan, closed)',
        'address(b, w)',
      ]);
    });

    test("the layer's onPeerConnected and onPeerDisconnected are recorded for "
        'peer_app/2, and still reach the host', () async {
      // GLP-Spec appendix-guards, "Networking seam" (5fab8ac): peer_app(P, A)
      // assigns app(D, N, V) of the app attested in the layer's session with
      // P, unattested where that session is unattested, or none where the
      // layer has no session with P; the kernel reads what the layer
      // reported with onPeerConnected (GNA api.tex, Section 2.3).
      final wire = _Wire();
      final kp = generateKeyPair();
      final network = _WireNetwork(kp, wire);
      final agent = AgentRuntime(
        agentId: 'a',
        program: play,
        rootSelfGlpPath: root,
        keyPair: kp,
        network: network,
      );
      final seen = <String>[];
      agent.onPeerConnected = (pk, u, application) => seen.add('$u $application');
      agent.onPeerDisconnected = (pk, u) => seen.add('$u down');
      await agent.initialize();
      final attested = generateKeyPair().pub;
      final unattested = generateKeyPair().pub;
      const app =
          Application(developer: 'TEAM', name: 'app.grassroots', version: 7);

      expect(agent.ctx!.peerSession(attested), isNull);
      network.onPeerConnected!(attested, Underlay.wan, app);
      network.onPeerConnected!(unattested, Underlay.pan, null);
      expect(agent.ctx!.peerSession(attested)?.application, app);
      expect(agent.ctx!.peerSession(unattested), isNotNull);
      expect(agent.ctx!.peerSession(unattested)!.application, isNull,
          reason: 'an unattested session');

      network.onPeerDisconnected!(attested, Underlay.wan);
      expect(agent.ctx!.peerSession(attested), isNull,
          reason: 'no session over any underlay');
      expect(seen, [
        '${Underlay.wan} $app',
        '${Underlay.pan} null',
        '${Underlay.wan} down',
      ]);
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
