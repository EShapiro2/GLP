/// Tests for the four networking seam kernels — '_peer_address'/2,
/// '_punch_udp'/1, '_place_declare'/3 and '_place_remove'/1 — and the declared
/// place's event stream.
///
/// Covers IGLP Definition "Seam Predicates": peer_address assigns the address at
/// which the layer observes a peer; punch_udp opens a path to an address and
/// returns nothing; place_declare declares a place and assigns a stream of that
/// agent's own entered, exited, unobservable and observable events, fed
/// serializer-fashion so one declaration yields one stream however many events
/// follow; place_remove ends the declaration. The stream is closed in exactly
/// two cases — place_remove and a superseding declaration — and an event for a
/// place removed or superseded is dropped. A declaration the layer refuses is
/// neither closing case: E receives unobservable, the declaration stands, and
/// observable follows if the layer later begins reporting.
///
/// The layer functions are GLP-Networking-API's. The simulation realization
/// provides none of the four (their paper, §Not provided).

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';

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
  void setTrustLevel(TrustLevel level) {}
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
({List<String> events, bool closed}) _stream(GlpRuntime rt, int addr) {
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
  final closed = cell is ConstTerm && (cell.value == 'nil' || cell.value == null);
  return (events: events, closed: closed);
}

/// A 64-character hex public key, distinct per [seed].
String _hex(int seed) =>
    List.generate(32, (i) => ((seed + i) & 0xff).toRadixString(16).padLeft(2, '0'))
        .join();

/// A madGLP engine with [network] bound as its GlpNetwork, capturing `_output`.
GlpEngine _engine(List<String> out, GlpNetwork network) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path)
        ..strictTypes = false;
  engine.enableMadGLP(agentId: 'alice');
  engine.runtime.outputCallback = out.add;
  engine.madContext!.network = network;
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

  group('seam kernels through their GLP wrappers', () {
    test("peer_address binds the layer's observed address", () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final peer = _hex(1);
      network.addresses[peer] = '203.0.113.7:41234';
      final engine = _engine(out, network);
      engine.loadSource('''
-mode(system).
procedure emit(_?).
emit(A) :- ground(A?) | '_output'(A?).
procedure go.
go :- peer_address('$peer', A), emit(A?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['203.0.113.7:41234']);
    });

    test('punch_udp hands the address to the layer and returns nothing',
        () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
-mode(system).
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
-mode(system).
procedure watch(_?).
watch([E|_]) :- ground(E?) | '_output'(E?).
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
-mode(system).
procedure go.
go :- place_declare(home, 100, _), place_remove(home).
''');
      await engine.runGoal('go');
      expect(network.declared, [('home', 100.0)]);
      expect(network.removed, ['home']);
    });
  });

  group('simulation realization provides none of the four (§Not provided)', () {
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
  });
}
