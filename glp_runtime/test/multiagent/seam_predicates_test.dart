/// Tests for the five networking seam kernels — '_peer_address'/2,
/// '_punch_udp'/1, '_place_declare'/3, '_place_remove'/1 and
/// '_trust_declare'/2 — and the declared place's event stream.
///
/// Covers IGLP Definition "Seam Predicates": peer_address assigns address(S), S
/// the address at which the layer observes a peer, or none where it observes
/// none (72b5efa; GLP-Spec 090e647, peer_address(Key?, PeerAddress)); punch_udp
/// opens a path to an address and returns nothing; place_declare declares a place and assigns a stream of that
/// agent's own entered, exited, unobservable and observable events, fed
/// serializer-fashion so one declaration yields one stream however many events
/// follow; place_remove ends the declaration; trust_declare sets a proximity
/// underlay's cold-call trust level, pan or lan (477c586; ble until
/// 2026-10-09). The stream is closed in exactly
/// two cases — place_remove and a superseding declaration — and an event for a
/// place removed or superseded is dropped. A declaration the layer refuses is
/// neither closing case: E receives unobservable, the declaration stands, and
/// observable follows if the layer later begins reporting.
///
/// The layer functions are GLP-Networking-API's. The simulation realization
/// provides none of the first four (their paper, §Not provided), and holds a
/// trust level per underlay (IGLP appendix-implementation-notes.tex,
/// Simulation realisation).
///
/// Where no layer is bound at all --- the REPL, a test run without the
/// simulation --- the runtime stands in for a layer that observes nothing and
/// does nothing: peer_address assigns none, place_declare assigns its stream
/// unobservable and nothing more, and the other seam predicates return having
/// done nothing (IGLP appendix-implementation-notes.tex, "One interface",
/// 6ba0329).

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/error.dart' show CompileError;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

/// A GlpNetwork that provides the seam predicates and nothing else, so the
/// kernels can be driven without a transport. Places are recorded rather than
/// registered with a platform; [fire] plays an event back through the callback
/// the runtime installed, as the layer does.
class _SeamNetwork extends GlpNetwork {
  /// Addresses the layer observes, by peer key hex.
  final Map<String, String> addresses = {};

  /// Places declared, in order, with their radii.
  final List<(String, double)> declared = [];

  /// Places removed, in order.
  final List<String> removed = [];

  /// Addresses punched, in order.
  final List<String> punched = [];

  /// Trust levels set, in order, with their media.
  final List<(ProximityUnderlay, TrustLevel)> trusted = [];

  /// Whether the platform accepts a declaration.
  bool accepts = true;

  /// Fired synchronously from [declarePlace], modelling a device already inside
  /// the region at the time of the call.
  PlaceEvent? onDeclare;

  @override
  String? observedPeerAddress(PubKey pk) => addresses[pk.hex];

  @override
  void punchUdp(String address) => punched.add(address);

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) =>
      trusted.add((underlay, level));

  @override
  Future<bool> declarePlace(String place, double radiusMetres) {
    declared.add((place, radiusMetres));
    final at = onDeclare;
    if (at != null) fire(place, at);
    return Future.value(accepts);
  }

  @override
  Future<void> removePlace(String place) {
    removed.add(place);
    return Future.value();
  }

  /// Play [event] for [place] back through the runtime's handler.
  void fire(String place, PlaceEvent event) => onPlaceEvent?.call(place, event);

  // --- Not exercised here ---

  @override
  void putIdentity(PubKey pub, Uint8List priv) {}
  @override
  PubKey getIdentity() => throw UnimplementedError();
  @override
  bool isPeerReachable(PubKey pk) => true;
  @override
  List<Transport> peerTransports(PubKey pk) => const [Transport.ip];
  @override
  void send(PubKey pk, Uint8List payload) {}
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String getPublicAddress() => throw UnimplementedError();
  @override
  String generatePeerLink() => throw UnimplementedError();
  @override
  void consumePeerLink(String uri) => throw UnimplementedError();
}

/// A MadContext over a bare runtime, with [network] bound.
({MadContext ctx, GlpRuntime rt, _SeamNetwork network}) _agentContext() {
  final rt = GlpRuntime();
  final ctx = MadContext(agentId: 'alice', runtime: rt);
  final network = _SeamNetwork();
  ctx.network = network;
  return (ctx: ctx, rt: rt, network: network);
}

/// The events on the stream growing from [addr], and whether it is closed —
/// as the program sees them.
({List<String> events, bool closed}) _stream(GlpRuntime rt, HeapCell addr) {
  final events = <String>[];
  Object? cell = rt.heap.derefAddr(addr);
  while (cell is StructTerm && cell.functor == '.') {
    final head = cell.args[0];
    if (head is ConstTerm) events.add('${head.value}');
    final tail = cell.args[1];
    if (tail is VarRef) {
      cell = rt.heap.derefAddr(tail.addr);
    } else {
      cell = tail;
    }
  }
  final closed = cell is ConstTerm && (cell.value == nil || cell.value == null);
  return (events: events, closed: closed);
}

/// A 64-character hex public key, distinct per [seed].
String _hex(int seed) =>
    List.generate(32, (i) => ((seed + i) & 0xff).toRadixString(16).padLeft(2, '0'))
        .join();

/// A madGLP engine with [network] bound as its GlpNetwork, capturing `_output`.
GlpEngine _engine(List<String> out, GlpNetwork network) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  engine.enableMadGLP(agentId: 'alice');
  engine.runtime.outputCallback = out.add;
  engine.madContext!.network = network;
  return engine;
}

/// An engine with no networking layer bound, capturing `_output`: the REPL's
/// own (bin/glp_repl.dart, before `:mad`), or, [mad], one in madGLP mode with
/// no GlpNetwork --- a test run without the simulation.
GlpEngine _unboundEngine(List<String> out, {required bool mad}) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  if (mad) engine.enableMadGLP(agentId: 'alice');
  engine.runtime.outputCallback = out.add;
  return engine;
}

void main() {
  group('place stream (Definition Seam Predicates)', () {
    test('one declaration yields one stream, fed serializer-fashion', () {
      final a = _agentContext();
      final (w, _) = a.rt.heap.allocateVariable();

      a.ctx.declarePlace('home', 100.0, w);
      expect(a.network.declared, [('home', 100.0)]);

      a.network.fire('home', PlaceEvent.entered);
      a.network.fire('home', PlaceEvent.exited);
      a.network.fire('home', PlaceEvent.entered);

      final s = _stream(a.rt, w);
      expect(s.events, ['entered', 'exited', 'entered']);
      expect(s.closed, isFalse,
          reason: 'the stream closes only on place_remove or a superseding '
              'declaration');
    });

    test('the stream carries all four events, not the crossings alone', () {
      final a = _agentContext();
      final (w, _) = a.rt.heap.allocateVariable();
      a.ctx.declarePlace('home', 50.0, w);

      for (final e in PlaceEvent.values) {
        a.network.fire('home', e);
      }

      expect(_stream(a.rt, w).events,
          ['entered', 'exited', 'unobservable', 'observable']);
    });

    test('place_remove closes the stream and reaches the layer', () {
      final a = _agentContext();
      final (w, _) = a.rt.heap.allocateVariable();
      a.ctx.declarePlace('home', 100.0, w);
      a.network.fire('home', PlaceEvent.entered);

      a.ctx.removePlace('home');

      final s = _stream(a.rt, w);
      expect(s.events, ['entered']);
      expect(s.closed, isTrue);
      expect(a.network.removed, ['home']);
      expect(a.ctx.hasDeclaredPlace('home'), isFalse);
    });

    test('removing a place that is not declared does nothing', () {
      final a = _agentContext();
      a.ctx.removePlace('nowhere');
      expect(a.network.removed, isEmpty);
    });

    test('a superseding declaration closes the earlier stream', () {
      final a = _agentContext();
      final (w1, _) = a.rt.heap.allocateVariable();
      final (w2, _) = a.rt.heap.allocateVariable();

      a.ctx.declarePlace('home', 100.0, w1);
      a.network.fire('home', PlaceEvent.entered);
      a.ctx.declarePlace('home', 200.0, w2);
      a.network.fire('home', PlaceEvent.exited);

      final first = _stream(a.rt, w1);
      expect(first.events, ['entered']);
      expect(first.closed, isTrue);

      final second = _stream(a.rt, w2);
      expect(second.events, ['exited']);
      expect(second.closed, isFalse);
      expect(a.network.declared, [('home', 100.0), ('home', 200.0)]);
    });

    test('an event for a place not declared here is dropped', () {
      final a = _agentContext();
      final (w, _) = a.rt.heap.allocateVariable();
      a.ctx.declarePlace('home', 100.0, w);

      a.network.fire('elsewhere', PlaceEvent.entered);
      a.ctx.removePlace('home');
      a.network.fire('home', PlaceEvent.entered);

      final s = _stream(a.rt, w);
      expect(s.events, isEmpty);
      expect(s.closed, isTrue);
    });

    test('a refused declaration carries unobservable and stands', () async {
      final a = _agentContext();
      a.network.accepts = false;
      final (w, _) = a.rt.heap.allocateVariable();

      a.ctx.declarePlace('home', 100.0, w);
      await Future<void>.delayed(Duration.zero);

      final s = _stream(a.rt, w);
      expect(s.events, ['unobservable']);
      expect(s.closed, isFalse,
          reason: 'Definition Seam Predicates closes a place stream in exactly '
              'two cases, and a refusal is neither');
      expect(a.ctx.hasDeclaredPlace('home'), isTrue);
    });

    test('observable follows a refusal when the layer begins reporting',
        () async {
      final a = _agentContext();
      a.network.accepts = false;
      final (w, _) = a.rt.heap.allocateVariable();

      a.ctx.declarePlace('home', 100.0, w);
      await Future<void>.delayed(Duration.zero);
      a.network.fire('home', PlaceEvent.observable);
      a.network.fire('home', PlaceEvent.entered);

      expect(_stream(a.rt, w).events, ['unobservable', 'observable', 'entered']);
    });

    test("a late refusal goes to no stream but the declaration it answers",
        () async {
      final a = _agentContext();
      a.network.accepts = false;
      final (w1, _) = a.rt.heap.allocateVariable();
      final (w2, _) = a.rt.heap.allocateVariable();

      a.ctx.declarePlace('home', 100.0, w1);
      a.network.accepts = true;
      a.ctx.declarePlace('home', 200.0, w2);
      await Future<void>.delayed(Duration.zero);

      final first = _stream(a.rt, w1);
      expect(first.events, isEmpty);
      expect(first.closed, isTrue,
          reason: 'the superseded declaration closed before the refusal came');

      final second = _stream(a.rt, w2);
      expect(second.events, isEmpty,
          reason: "the first declaration's refusal is not the second's");
      expect(second.closed, isFalse);
    });
  });

  // The sources below are application modules: they call the root self.glp's
  // seam predicates and, to show what came back, its send_to_user/1.  A
  // source with no file behind it is a module at the root, and -mode(system)
  // is admitted only for the root self.glp and programs/system/ (TGLP
  // appendix-root-self.tex, app:system-mode; GLP's round six, item 4), so
  // none declares it: until 2026-10-04 each did, Rule A skipping a source
  // with no file, and two called '_output' directly.
  group('seam kernels through their GLP wrappers', () {
    // What peer_address/2 assigns, taken apart by matching: address(S) or
    // none (GLP-Spec appendix-guards, "Networking seam").
    const emitAddress = '''
procedure emit(PeerAddress?).
emit(address(S)) :- ground(S?) | send_to_user([S?]).
emit(none) :- send_to_user([none]).
''';

    test("peer_address assigns address(S), S the layer's observed address",
        () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final peer = _hex(1);
      network.addresses[peer] = '203.0.113.7:41234';
      final engine = _engine(out, network);
      engine.loadSource('''
${emitAddress}procedure go.
go :- peer_address('$peer', A), emit(A?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['203.0.113.7:41234']);
    });

    test('peer_address assigns none where the layer observes no address, and '
        'does not abort', () async {
      final out = <String>[];
      final network = _SeamNetwork(); // observes no address for any peer
      final engine = _engine(out, network);
      engine.loadSource('''
${emitAddress}procedure go.
go :- peer_address('${_hex(1)}', A), emit(A?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue,
          reason: 'none is a value and not an abort');
      expect(out, ['none']);
    });

    test('peer_address/2 and punch_udp/1 are typed: Key?, PeerAddress; String?',
        () {
      final root = Parser(
              Lexer(File('../programs/self.glp').readAsStringSync()).tokenize())
          .parseModule();
      String decl(String name, int arity) => root.procDeclarations
          .singleWhere((d) => d.name == name && d.arity == arity)
          .toString();
      expect(decl('peer_address', 2), 'procedure peer_address(Key?, PeerAddress).');
      expect(decl('punch_udp', 1), 'procedure punch_udp(String?).');
      expect(decl('_peer_address', 2),
          'procedure _peer_address(Key?, PeerAddress).');
      expect(decl('_punch_udp', 1), 'procedure _punch_udp(String?).');
      final peerAddress =
          root.typeDefs.singleWhere((t) => t.name == 'PeerAddress');
      expect(peerAddress.alternatives.map((a) => '$a'),
          ['address(String)', 'none']);

      // The checker holds a program to them.
      final engine = _engine(<String>[], _SeamNetwork());
      Matcher refused(String why) => throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          allOf(contains('Type checking failed'), contains(why))));
      expect(() => engine.loadSource('''
procedure take(String?).
take(S) :- string(S?) | true.
procedure go.
go :- peer_address('${_hex(1)}', A), take(A?).
'''), refused('writer type PeerAddress is not a subtype of String'));
      expect(() => engine.loadSource('''
procedure go.
go :- punch_udp(41234).
'''), refused('(punch_udp) is not well-typed'),
          reason: 'an Integer is no String');
      expect(() => engine.loadSource('''
procedure go.
go :- peer_address(7, _).
'''), refused('(peer_address) is not well-typed'),
          reason: 'an Integer is no Key');
    });

    test('punch_udp hands the address to the layer and returns nothing',
        () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- punch_udp('203.0.113.7:41234').
''');
      await engine.runGoal('go');
      expect(network.punched, ['203.0.113.7:41234']);
    });

    test('place_declare declares at the layer and its stream reaches the program',
        () async {
      final out = <String>[];
      final network = _SeamNetwork()..onDeclare = PlaceEvent.entered;
      final engine = _engine(out, network);
      engine.loadSource('''
procedure watch(_?).
watch([E|_]) :- ground(E?) | send_to_user([E?]).
procedure go.
go :- place_declare(home, 100, E), watch(E?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(network.declared, [('home', 100.0)]);
      expect(out, ['entered']);
    });

    test('place_remove reaches the layer', () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- place_declare(home, 100, _), place_remove(home).
''');
      await engine.runGoal('go');
      expect(network.declared, [('home', 100.0)]);
      expect(network.removed, ['home']);
    });

    test("trust_declare sets each underlay's level at the layer", () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- trust_declare(pan, open), trust_declare(lan, closed).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(
          network.trusted,
          unorderedEquals([
            (ProximityUnderlay.pan, TrustLevel.open),
            (ProximityUnderlay.lan, TrustLevel.closed),
          ]));
    });

    test('trust_declare of an underlay the layer does not have aborts, ble '
        'among them', () async {
      for (final underlay in ['wifi', 'ble']) {
        final out = <String>[];
        final network = _SeamNetwork();
        final engine = _engine(out, network);
        engine.loadSource('''
procedure go.
go :- trust_declare($underlay, open).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isFalse,
            reason: "$underlay: the kernel aborts, and the goal that called it "
                "fails");
        expect(network.trusted, isEmpty);
      }
    });
  });

  // IGLP appendix-implementation-notes.tex, "One interface" (6ba0329): "Where
  // no layer is bound at all --- the REPL, a test run without the simulation
  // --- the runtime stands in for a layer that observes nothing and does
  // nothing: peer_address assigns none, place_declare assigns its stream
  // unobservable and nothing more, and the other seam predicates return having
  // done nothing".  Each test runs twice: under the REPL's engine, with no
  // MadContext, and in madGLP mode with no GlpNetwork bound.
  group('no layer bound: a layer that observes nothing and does nothing', () {
    const emitAddress = '''
procedure emit(PeerAddress?).
emit(address(S)) :- ground(S?) | send_to_user([S?]).
emit(none) :- send_to_user([none]).
''';

    // Reads E's first event, then waits on the rest: a further event or a
    // closing would show; a stream that carries nothing more leaves `more`
    // suspended at quiescence.
    const watchPlace = '''
procedure watch(_?).
watch([E|Es]) :- ground(E?) | send_to_user([E?]), more(Es?).
procedure more(_?).
more([E|_]) :- ground(E?) | send_to_user([E?]).
more([]) :- send_to_user([closed]).
''';

    for (final mad in [false, true]) {
      final where = mad ? 'madGLP, no GlpNetwork' : 'the REPL, no MadContext';

      test('$where: peer_address assigns none, for any peer, and does not abort',
          () async {
        for (final peer in [_hex(1), 'bob']) {
          final out = <String>[];
          final engine = _unboundEngine(out, mad: mad);
          engine.loadSource('''
${emitAddress}procedure go.
go :- peer_address('$peer', A), emit(A?).
''');
          final result = await engine.runGoal('go');
          expect(result.succeeded, isTrue, reason: '$peer: none is a value');
          expect(out, ['none'], reason: peer);
        }
      });

      test('$where: punch_udp returns having done nothing', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
procedure go.
go :- punch_udp('203.0.113.7:41234'), send_to_user([returned]).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isTrue);
        expect(out, ['returned']);
      });

      test('$where: place_declare assigns its stream unobservable and nothing '
          'more', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- place_declare(home, 100, E), watch(E?).
''');
        final result = await engine.runGoal('go');
        expect(out, ['unobservable']);
        expect(result.suspended, isTrue,
            reason: 'the stream stays open and nothing follows unobservable: '
                'no event, no observable, no closing');
        if (mad) {
          expect(engine.madContext!.hasDeclaredPlace('home'), isFalse,
              reason: 'nothing is declared');
        }
      });

      test('$where: place_remove returns having done nothing, the stream left '
          'as it was', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- place_declare(home, 100, E), watch(E?), place_remove(home),
    place_remove(nowhere).
''');
        final result = await engine.runGoal('go');
        expect(out, ['unobservable'],
            reason: 'place_remove does nothing: the stream is not closed');
        expect(result.suspended, isTrue);
      });

      test('$where: a second declaration of a place leaves the first stream as '
          'it was', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- place_declare(home, 100, E1), watch(E1?), place_declare(home, 200, E2),
    watch(E2?).
''');
        final result = await engine.runGoal('go');
        expect(out, ['unobservable', 'unobservable']);
        expect(result.suspended, isTrue,
            reason: 'nothing is declared, so nothing supersedes and nothing '
                'closes');
      });

      test('$where: trust_declare returns having done nothing', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
procedure go.
go :- trust_declare(pan, open), trust_declare(lan, closed),
    send_to_user([returned]).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isTrue);
        expect(out, ['returned']);
      });

      test('$where: an argument the predicate does not take still aborts, '
          'the want of a layer aside', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
procedure go.
go :- trust_declare(wifi, open).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isFalse,
            reason: 'wifi is no underlay: the kernel aborts and the goal '
                'fails, as with a layer bound');
      });
    }
  });

  group(
      'simulation realization: none of the first four (§Not provided), '
      'a trust level per underlay (Simulation realisation)', () {
    SimulationNetworkClient client() => SimulationNetworkClient(
          selfId: 'alice',
          directory: NetworkDirectory(),
          sendToRouter: (_, __) {},
        );

    test('observedPeerAddress and punchUdp throw', () {
      final c = client();
      expect(() => c.observedPeerAddress(PubKey.fromHex(_hex(2))),
          throwsUnsupportedError);
      expect(() => c.punchUdp('203.0.113.7:41234'), throwsUnsupportedError);
    });

    test('declarePlace and removePlace throw, and onPlaceEvent never fires', () {
      final c = client();
      var fired = 0;
      c.onPlaceEvent = (_, __) => fired++;
      expect(() => c.declarePlace('home', 100.0), throwsUnsupportedError);
      expect(() => c.removePlace('home'), throwsUnsupportedError);
      expect(fired, 0);
    });

    test("setTrustLevel holds each underlay's level, both closed until set",
        () {
      final c = client();
      expect(c.trustLevelOf(ProximityUnderlay.pan), TrustLevel.closed);
      expect(c.trustLevelOf(ProximityUnderlay.lan), TrustLevel.closed);
      c.setTrustLevel(ProximityUnderlay.pan, TrustLevel.open);
      expect(c.trustLevelOf(ProximityUnderlay.pan), TrustLevel.open);
      expect(c.trustLevelOf(ProximityUnderlay.lan), TrustLevel.closed,
          reason: 'the two underlays are declared independently');
    });
  });
}
