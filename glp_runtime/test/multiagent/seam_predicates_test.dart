/// Tests for the five networking seam kernels — '_peer_address'/2,
/// '_peer_app'/2, '_punch_udp'/1, '_declare'/1 and '_remove'/1 — and the
/// declared place's event stream.  peer_app assigns app(D, N, V), the
/// developer, the name and the version number of the app attested in the
/// layer's session with a peer, unattested where that session is unattested,
/// or none where the layer has no session with it (GLP-Spec 5fab8ac, IGLP
/// 4a334c2).
///
/// Covers IGLP Definition "Seam Predicates" (3b736bb) and GLP-Spec
/// appendix-guards, "Networking seam" (1ce0263): peer_address assigns
/// address(S), S the address at which the layer observes a peer, or none where
/// it observes none; punch_udp opens a path to an address and returns nothing;
/// declare(D) makes the declaration D, Declaration ::= peer(Key) ; address(Key,
/// String) ; rendezvous(Key, String) ; trust(Underlay, TrustLevel) ;
/// place(Constant, Number, Stream(PlaceEvent)?), and remove(R) ends one,
/// Removal ::= peer(Key) ; rendezvous(Key) ; place(Constant).  A place
/// declaration assigns a stream of that agent's own entered, exited,
/// unobservable and observable events, fed serializer-fashion so one
/// declaration yields one stream however many events follow.  The stream is
/// closed in exactly two cases — remove(place(P)) and a superseding
/// declaration — and an event for a place removed or superseded is dropped. A
/// declaration the layer refuses is neither closing case: E receives
/// unobservable, the declaration stands, and observable follows if the layer
/// later begins reporting.  declare/1 and remove/1 replace place_declare/3,
/// place_remove/1, trust_declare/2, peer_declare/1 and address_declare/2,
/// gone on 2026-11-03.
///
/// The layer functions are GLP-Networking-API's. The simulation realization
/// provides neither peer_address, punch_udp, nor a place declaration and its
/// removal, holds a trust level per underlay, takes a declared peer into its
/// router's known-peer set, and takes an address or a rendezvous server to no
/// effect (IGLP appendix-implementation-notes.tex, Simulation realisation); it
/// names agents symbolically, resolving a name to its key at the seam.
///
/// Where no layer is bound at all --- the REPL, a test run without the
/// simulation --- the runtime stands in for a layer that observes nothing and
/// does nothing: peer_address assigns none, a place declaration assigns its
/// stream unobservable and nothing more, and the other seam predicates return
/// having done nothing (IGLP appendix-implementation-notes.tex, "One
/// interface").

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

  /// Keys given by putKnownPeer, in order, as hex.
  final List<String> known = [];

  /// Peer addresses given by putPeerAddress, in order: key hex and address.
  final List<(String, String)> peerAddresses = [];

  /// Keys taken off by removeKnownPeer, in order, as hex.
  final List<String> unknown = [];

  /// Rendezvous servers given by putRendezvousServer, in order: key hex and
  /// address.
  final List<(String, String)> servers = [];

  /// Rendezvous servers taken off by removeRendezvousServer, in order, as hex.
  final List<String> serversRemoved = [];

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
  void putKnownPeer(PubKey pk) => known.add(pk.hex);

  @override
  void putPeerAddress(PubKey pk, String address) =>
      peerAddresses.add((pk.hex, address));

  @override
  void removeKnownPeer(PubKey pk) => unknown.add(pk.hex);

  @override
  void putRendezvousServer(PubKey pk, String address) =>
      servers.add((pk.hex, address));

  @override
  void removeRendezvousServer(PubKey pk) => serversRemoved.add(pk.hex);

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
  PubKey getIdentity() => throw UnimplementedError();
  @override
  bool isPeerReachable(PubKey pk) => true;
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [Underlay.wan];
  @override
  Future<void> send(PubKey pk, Uint8List payload) => Future.value();
  @override
  void acknowledge(String messageId) {}
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String? getPublicAddress() => throw UnimplementedError();
  @override
  String? networkIdentity() => throw UnimplementedError();
}

/// What `_output` shows of app('TEAM', 'app.grassroots', 7).
const String _appTeam7 = 'app(TEAM, app.grassroots, 7)';

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
          reason: 'the stream closes only on remove(place(P)) or a superseding '
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

    test('remove(place(P)) closes the stream and reaches the layer', () {
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

    test('declare(place(P, R, E)) declares at the layer and its stream '
        'reaches the program',
        () async {
      final out = <String>[];
      final network = _SeamNetwork()..onDeclare = PlaceEvent.entered;
      final engine = _engine(out, network);
      engine.loadSource('''
procedure watch(_?).
watch([E|_]) :- ground(E?) | send_to_user([E?]).
procedure go.
go :- declare(place(home, 100, E)), watch(E?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(network.declared, [('home', 100.0)]);
      expect(out, ['entered']);
    });

    test('remove(place(P)) reaches the layer', () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(place(home, 100, _)), remove(place(home)).
''');
      await engine.runGoal('go');
      expect(network.declared, [('home', 100.0)]);
      expect(network.removed, ['home']);
    });

    test("a place's name reaches the layer as the constant's printed form",
        () async {
      // IGLP 0e1c9e0, Implementation Notes, "The contract": "The runtime
      // gives the layer a place's name, a constant, as its printed form";
      // GLP-Spec appendix-guards: place(Constant, Number,
      // Stream(PlaceEvent)?).  Until 2026-11-03 a number aborted at run time.
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(place(7, 100, _)), declare(place('7', 50, _)),
    remove(place(7)), remove(place('7')).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(network.declared, [('7', 100.0), ("'7'", 50.0)],
          reason: 'declare(place(7, 100, E)) reaches declarePlace with "7"; '
              "'7' is another constant, and another place");
      expect(network.removed, ['7', "'7'"]);
    });

    test("declare(trust(U, L)) sets each underlay's level at the layer", () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(trust(pan, open)), declare(trust(lan, closed)).
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

    test('declare(trust(U, L)) of an underlay or a level the language does '
        'not have is refused by the checker, ble among them', () {
      // GLP-Spec appendix-guards, "Networking seam": Underlay ::= pan ; lan,
      // TrustLevel ::= open ; closed.
      for (final trust in ['wifi, open', 'ble, open', 'pan, ajar']) {
        final network = _SeamNetwork();
        final engine = _engine(<String>[], network);
        expect(() => engine.loadSource('''
procedure go.
go :- declare(trust($trust)).
'''), throwsA(isA<CompileError>()), reason: 'trust($trust) is no Declaration');
        expect(network.trusted, isEmpty);
      }
    });

    test('declare(rendezvous(P, A)) gives the layer a rendezvous server, and '
        'remove(rendezvous(P)) takes it off', () async {
      // GLP-Spec appendix-guards, "Networking seam": "rendezvous(P, A) gives
      // the layer a rendezvous server, the agent of key P at address A, or
      // replaces its address, and remove(rendezvous(P)) takes the server off
      // and ends the layer's session with it"; GNA 776016c, Section 4.2,
      // putRendezvousServer(pk, address) "behind declare(rendezvous(Key,
      // Address))" and removeRendezvousServer(pk) "behind
      // remove(rendezvous(Key))".
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(rendezvous('${_hex(3)}', '198.51.100.9:7000')),
    remove(rendezvous('${_hex(3)}')), send_to_user([returned]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['returned'], reason: 'neither assigns anything');
      expect(network.servers, [(_hex(3), '198.51.100.9:7000')]);
      expect(network.serversRemoved, [_hex(3)]);
      expect(network.known, isEmpty,
          reason: 'a rendezvous server is not a declaration of a known peer');
    });

    test('remove(peer(P)) takes the peer off the layer\'s known peers',
        () async {
      // GLP-Spec appendix-guards, "Networking seam": "remove(peer(P)) takes
      // P, and any address recorded for it, off the layer's known peers";
      // GNA, "Known Peers", removeKnownPeer(pk).
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(peer('${_hex(1)}')), remove(peer('${_hex(1)}')),
    send_to_user([returned]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['returned']);
      expect(network.known, [_hex(1)]);
      expect(network.unknown, [_hex(1)]);
    });

    // IGLP, Definition "Seam Predicates": "declare(peer(P)) gives the layer
    // the public key P of a peer the program knows, ... and
    // declare(address(P, A)) gives the address A at which peer P is reached".
    test('declare(peer(P)) gives the layer the key of a peer the program knows',
        () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(peer('${_hex(1)}')), declare(peer('${_hex(2)}')),
    send_to_user([returned]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['returned'], reason: 'declare(peer(P)) assigns nothing');
      expect(network.known, unorderedEquals([_hex(1), _hex(2)]));
      expect(network.peerAddresses, isEmpty);
    });

    test("declare(address(P, A)) gives the layer the address at which a peer is "
        "reached", () async {
      final out = <String>[];
      final network = _SeamNetwork();
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(address('${_hex(1)}', '203.0.113.7:41234')),
    send_to_user([returned]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['returned'], reason: 'declare(address(P, A)) assigns nothing');
      expect(network.peerAddresses, [(_hex(1), '203.0.113.7:41234')]);
      expect(network.known, isEmpty,
          reason: 'an address is not a declaration of the peer');
    });

    // What peer_app/2 assigns, emitted whole: app(D, N, V), unattested or none
    // (GLP-Spec appendix-guards, "Networking seam", 5fab8ac; IGLP, Definition
    // "Seam Predicates", 4a334c2).  The kernel reads what the layer reported
    // with onPeerConnected (GNA api.tex, Section 2.3), which the host records
    // in the agent's MadContext; here the test records it as the host does.
    const emitApp = '''
procedure emit(App?).
emit(A) :- ground(A?) | send_to_user([A?]).
''';

    Future<List<String>> peerApp(
        void Function(MadContext ctx, PubKey pk) reports, String peer) async {
      final out = <String>[];
      final engine = _engine(out, _SeamNetwork());
      reports(engine.madContext!, PubKey.fromHex(peer));
      engine.loadSource('''
${emitApp}procedure go.
go :- peer_app('$peer', A), emit(A?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue,
          reason: 'app(D, N, V), unattested and none are values');
      return out;
    }

    test('peer_app assigns app(D, N, V), the app attested in the session',
        () async {
      expect(
          await peerApp(
              (ctx, pk) => ctx.peerConnected(
                  pk,
                  Underlay.wan,
                  const Application(
                      developer: 'TEAM', name: 'app.grassroots', version: 7)),
              _hex(1)),
          [_appTeam7]);
    });

    test("peer_app's version number is an Integer, matched as one", () async {
      final out = <String>[];
      final engine = _engine(out, _SeamNetwork());
      engine.madContext!.peerConnected(
          PubKey.fromHex(_hex(1)),
          Underlay.wan,
          const Application(
              developer: 'TEAM', name: 'app.grassroots', version: 7));
      engine.loadSource('''
procedure check(App?).
check(app(D, N, 7)) :- string(D?), string(N?) | send_to_user([seven]).
check(unattested) :- send_to_user([unattested]).
check(none) :- send_to_user([none]).
procedure go.
go :- peer_app('${_hex(1)}', A), check(A?).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['seven']);
    });

    test('peer_app assigns unattested where the session is unattested',
        () async {
      expect(
          await peerApp(
              (ctx, pk) => ctx.peerConnected(pk, Underlay.wan, null), _hex(1)),
          ['unattested']);
    });

    test('peer_app assigns none where the layer has no session with the peer',
        () async {
      // Never reported connected.
      expect(await peerApp((ctx, pk) {}, _hex(1)), ['none']);
      // Another peer's session is not this one's.
      expect(
          await peerApp(
              (ctx, pk) => ctx.peerConnected(
                  PubKey.fromHex(_hex(2)), Underlay.wan, null),
              _hex(1)),
          ['none']);
      // Reported disconnected over every underlay it was connected over.
      expect(
          await peerApp((ctx, pk) {
            ctx.peerConnected(pk, Underlay.pan, null);
            ctx.peerConnected(pk, Underlay.wan, null);
            ctx.peerDisconnected(pk, Underlay.pan);
            ctx.peerDisconnected(pk, Underlay.wan);
          }, _hex(1)),
          ['none']);
    });

    test('peer_app: a session stands while the peer is connected over any '
        'underlay, and answers the application reported last', () async {
      expect(
          await peerApp((ctx, pk) {
            ctx.peerConnected(pk, Underlay.pan, null);
            ctx.peerConnected(
                pk,
                Underlay.wan,
                const Application(
                    developer: 'TEAM', name: 'app.grassroots', version: 7));
            ctx.peerDisconnected(pk, Underlay.pan);
          }, _hex(1)),
          [_appTeam7]);
      expect(
          await peerApp((ctx, pk) {
            ctx.peerConnected(
                pk,
                Underlay.wan,
                const Application(
                    developer: 'TEAM', name: 'app.grassroots', version: 7));
            ctx.peerConnected(pk, Underlay.pan, null);
          }, _hex(1)),
          ['unattested']);
    });

    test('peer_app/2 is typed: Key?, App; App ::= app(String, String, '
        'Integer) ; unattested ; none', () {
      final root = Parser(
              Lexer(File('../programs/self.glp').readAsStringSync()).tokenize())
          .parseModule();
      String decl(String name, int arity) => root.procDeclarations
          .singleWhere((d) => d.name == name && d.arity == arity)
          .toString();
      expect(decl('peer_app', 2), 'procedure peer_app(Key?, App).');
      expect(decl('_peer_app', 2), 'procedure _peer_app(Key?, App).');
      final app = root.typeDefs.singleWhere((t) => t.name == 'App');
      expect(app.alternatives.map((a) => '$a'),
          ['app(String, String, Integer)', 'unattested', 'none']);

      final engine = _engine(<String>[], _SeamNetwork());
      Matcher refused(String why) => throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          allOf(contains('Type checking failed'), contains(why))));
      expect(() => engine.loadSource('''
procedure take(PeerAddress?).
take(none).
procedure go.
go :- peer_app('${_hex(1)}', A), take(A?).
'''), refused('writer type App is not a subtype of PeerAddress'));
      expect(() => engine.loadSource('''
procedure go.
go :- peer_app(7, _).
'''), refused('(peer_app) is not well-typed'),
          reason: 'an Integer is no Key');
    });

    test('a peer name that is no key aborts, and the layer is not reached',
        () async {
      for (final goal in [
        "declare(peer(bob))",
        "declare(address(bob, '203.0.113.7:41234'))",
      ]) {
        final out = <String>[];
        final network = _SeamNetwork();
        final engine = _engine(out, network);
        engine.loadSource('''
procedure go.
go :- $goal.
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isFalse,
            reason: '$goal: over a real network a peer is named by its key '
                '(IGLP, Remark "Agent Names"), and bob is none');
        expect(network.known, isEmpty);
        expect(network.peerAddresses, isEmpty);
      }
    });

    test('declare/1 and remove/1 are typed by the catalogue: Declaration?; '
        'Removal?', () {
      final root = Parser(
              Lexer(File('../programs/self.glp').readAsStringSync()).tokenize())
          .parseModule();
      String decl(String name, int arity) => root.procDeclarations
          .singleWhere((d) => d.name == name && d.arity == arity)
          .toString();
      List<String> alternatives(String type) => root.typeDefs
          .singleWhere((t) => t.name == type)
          .alternatives
          .map((a) => '$a')
          .toList();
      expect(decl('declare', 1), 'procedure declare(Declaration?).');
      expect(decl('remove', 1), 'procedure remove(Removal?).');
      expect(decl('_declare', 1), 'procedure _declare(Declaration?).');
      expect(decl('_remove', 1), 'procedure _remove(Removal?).');
      expect(alternatives('Declaration'), [
        'peer(Key)',
        'address(Key, String)',
        'rendezvous(Key, String)',
        'trust(Underlay, TrustLevel)',
        'place(Constant, Number, Stream(PlaceEvent)?)',
      ]);
      expect(alternatives('Removal'),
          ['peer(Key)', 'rendezvous(Key)', 'place(Constant)']);
      expect(alternatives('Underlay'), ['pan', 'lan']);
      expect(alternatives('TrustLevel'), ['open', 'closed']);
      expect(alternatives('PlaceEvent'),
          ['entered', 'exited', 'observable', 'unobservable']);
      for (final gone in [
        ('place_declare', 3),
        ('place_remove', 1),
        ('trust_declare', 2),
        ('peer_declare', 1),
        ('address_declare', 2),
      ]) {
        expect(
            root.procDeclarations
                .where((d) => d.name == gone.$1 && d.arity == gone.$2),
            isEmpty,
            reason: '${gone.$1}/${gone.$2} is replaced by declare/1 and '
                'remove/1');
      }

      // The checker accepts each alternative of Declaration and Removal.
      final k = _hex(1);
      for (final goal in [
        "declare(peer('$k'))",
        "declare(address('$k', '203.0.113.7:41234'))",
        "declare(rendezvous('$k', '198.51.100.9:7000'))",
        'declare(trust(pan, open))',
        'declare(trust(lan, closed))',
        'declare(place(home, 100, E)), watch(E?)',
        "remove(peer('$k'))",
        "remove(rendezvous('$k'))",
        'remove(place(home))',
      ]) {
        final engine = _engine(<String>[], _SeamNetwork());
        expect(() => engine.loadSource('''
procedure watch(Stream(PlaceEvent)?).
watch([E|_]) :- ground(E?) | send_to_user([E?]).
watch([]).
procedure go.
go :- $goal.
'''), returnsNormally, reason: goal);
      }

      // And refuses anything else.
      Matcher refused(String why) => throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          allOf(contains('Type checking failed'), contains(why))));
      for (final (goal, why) in [
        ('declare(peer(7))', 'an Integer is no Key'),
        ('declare(peer(key(bob)))', 'a structure is no Key'),
        ("declare(address(7, '203.0.113.7:41234'))", 'an Integer is no Key'),
        ("declare(address('$k', 41234))", 'an Integer is no String'),
        ("declare(address('$k', at(host, 41234)))", 'a structure is no String'),
        ("declare(rendezvous('$k', 7000))", 'an Integer is no String'),
        ('declare(trust(wifi, open))', 'wifi is no Underlay'),
        ('declare(trust(pan, ajar))', 'ajar is no TrustLevel'),
        ('declare(place(home, far, _))', 'far is no Number'),
        ("declare(wish('$k'))", 'wish is no Declaration'),
        ("declare(peer('$k', '$k'))", 'peer/2 is no Declaration'),
        ("remove(address('$k', '203.0.113.7:41234'))",
            'an address is no Removal'),
        ('remove(trust(pan, open))', 'a trust level is no Removal'),
        ('remove(place(home, 100))', 'place/2 is no Removal'),
        ("remove(rendezvous('$k', '198.51.100.9:7000'))",
            'rendezvous/2 is no Removal'),
      ]) {
        final engine = _engine(<String>[], _SeamNetwork());
        final name = goal.substring(0, goal.indexOf('('));
        expect(() => engine.loadSource('''
procedure go.
go :- $goal.
'''), refused('($name) is not well-typed'), reason: '$goal: $why');
      }
    });
  });

  // IGLP appendix-implementation-notes.tex, "One interface" (6ba0329): "Where
  // no layer is bound at all --- the REPL, a test run without the simulation
  // --- the runtime stands in for a layer that observes nothing and does
  // nothing: peer_address assigns none, a place declaration assigns its stream
  // unobservable and nothing more, and the other seam predicates return having
  // done nothing".  Each test runs twice: under the REPL's engine, with no
  // MadContext, and in madGLP mode with no GlpNetwork bound.  Closing a
  // place's stream is the runtime's bookkeeping and not the layer's, so with
  // no layer remove(place(P)) and a further declaration close it as ever, and
  // only the reporting is absent (IGLP madglp-spec.tex, "closed, its tail
  // assigned [], in exactly two cases: remove(place(P)), and a further
  // declaration of the same place"; GLP, 2026-10-11 12:59 UTC, "23:12" (a)).
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

      test('$where: peer_app assigns none, for any peer: a layer that observes '
          'nothing has no session with any peer', () async {
        for (final peer in [_hex(1), 'bob']) {
          final out = <String>[];
          final engine = _unboundEngine(out, mad: mad);
          engine.loadSource('''
procedure emit(App?).
emit(A) :- ground(A?) | send_to_user([A?]).
procedure go.
go :- peer_app('$peer', A), emit(A?).
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

      test('$where: declare(place(P, R, E)) assigns its stream unobservable and '
          'nothing '
          'more', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- declare(place(home, 100, E)), watch(E?).
''');
        final result = await engine.runGoal('go');
        expect(out, ['unobservable']);
        expect(result.suspended, isTrue,
            reason: 'the stream stays open and nothing follows unobservable: '
                'no event, no observable, and no closing until remove(place(P)) '
                'or a further declaration');
        expect(engine.runtime.placeStreams.isDeclared('home'), isTrue,
            reason: "the declaration stands in the runtime's bookkeeping, "
                'which is not the layer\'s');
        if (mad) {
          expect(engine.madContext!.hasDeclaredPlace('home'), isTrue);
        }
      });

      test('$where: remove(place(P)) closes the stream, its tail assigned [], '
          'and an undeclared place is no matter', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- declare(place(home, 100, E)), watch(E?), remove(place(home)),
    remove(place(nowhere)).
''');
        final result = await engine.runGoal('go');
        expect(out, ['unobservable', 'closed'],
            reason: 'remove(place(P)) closes the stream with no layer as with one');
        expect(result.succeeded, isTrue,
            reason: 'nothing waits on the stream once it is closed');
        expect(engine.runtime.placeStreams.isDeclared('home'), isFalse);
        if (mad) {
          expect(engine.madContext!.hasDeclaredPlace('home'), isFalse);
        }
      });

      test('$where: a further declaration of a place closes the first stream, '
          'and its own carries unobservable and stays open', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- declare(place(home, 100, E1)), watch(E1?), declare(place(home, 200, E2)),
    watch2(E2?).
procedure watch2(_?).
watch2([E|Es]) :- ground(E?) | send_to_user([second(E?)]), more2(Es?).
procedure more2(_?).
more2([E|_]) :- ground(E?) | send_to_user([second(E?)]).
more2([]) :- send_to_user([second(closed)]).
''');
        final result = await engine.runGoal('go');
        expect(out.toSet(), {'unobservable', 'closed', 'second(unobservable)'},
            reason: 'the first stream is unobservable then closed; the second '
                'carries unobservable');
        expect(out, hasLength(3));
        expect(out.indexOf('unobservable'), lessThan(out.indexOf('closed')));
        expect(result.suspended, isTrue,
            reason: 'the second stream stays open: it is the standing '
                'declaration');
        expect(engine.runtime.placeStreams.isDeclared('home'), isTrue);
      });

      test('$where: remove(place(P)) after a further declaration closes the '
          'standing stream', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
${watchPlace}procedure go.
go :- declare(place(home, 100, E1)), declare(place(home, 200, E2)),
    watch(E1?), watch2(E2?), remove(place(home)).
procedure watch2(_?).
watch2([E|Es]) :- ground(E?) | send_to_user([second(E?)]), more2(Es?).
procedure more2(_?).
more2([E|_]) :- ground(E?) | send_to_user([second(E?)]).
more2([]) :- send_to_user([second(closed)]).
''');
        final result = await engine.runGoal('go');
        expect(out.toSet(), {
          'unobservable',
          'closed',
          'second(unobservable)',
          'second(closed)'
        });
        expect(out, hasLength(4));
        expect(out.indexOf('unobservable'), lessThan(out.indexOf('closed')));
        expect(out.indexOf('second(unobservable)'),
            lessThan(out.indexOf('second(closed)')));
        expect(result.succeeded, isTrue,
            reason: 'both streams are closed: the first superseded, the '
                'second removed');
        expect(engine.runtime.placeStreams.isDeclared('home'), isFalse);
      });

      test('$where: declare(trust(U, L)) returns having done nothing', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
procedure go.
go :- declare(trust(pan, open)), declare(trust(lan, closed)),
    send_to_user([returned]).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isTrue);
        expect(out, ['returned']);
      });

      test('$where: declare(peer(P)) and declare(address(P, A)) return having '
          'done '
          'nothing, whatever peer they name', () async {
        for (final peer in [_hex(1), 'bob']) {
          final out = <String>[];
          final engine = _unboundEngine(out, mad: mad);
          engine.loadSource('''
procedure go.
go :- declare(peer('$peer')), declare(address('$peer', '203.0.113.7:41234')),
    send_to_user([returned]).
''');
          final result = await engine.runGoal('go');
          expect(result.succeeded, isTrue, reason: peer);
          expect(out, ['returned'], reason: peer);
        }
      });

      test('$where: an argument the predicate does not take is refused, the '
          'want of a layer aside', () {
        final engine = _unboundEngine(<String>[], mad: mad);
        expect(() => engine.loadSource('''
procedure go.
go :- declare(trust(wifi, open)).
'''), throwsA(isA<CompileError>()),
            reason: 'wifi is no Underlay: the checker refuses the program, as '
                'with a layer bound');
      });

      test('$where: a rendezvous server and the removal of a peer return '
          'having done nothing', () async {
        final out = <String>[];
        final engine = _unboundEngine(out, mad: mad);
        engine.loadSource('''
procedure go.
go :- declare(rendezvous('${_hex(3)}', '198.51.100.9:7000')),
    remove(rendezvous('${_hex(3)}')), remove(peer('${_hex(1)}')),
    send_to_user([returned]).
''');
        final result = await engine.runGoal('go');
        expect(result.succeeded, isTrue);
        expect(out, ['returned']);
      });
    }
  });

  group(
      'simulation realization: no peer address, hole punching or place '
      '(Not provided), a trust level per underlay (Simulation realisation)',
      () {
    SimulationNetworkClient client() => SimulationNetworkClient(
          selfId: 'alice',
          directory: NetworkDirectory(),
          keyPair: generateKeyPair(),
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

    // The simulation realisation names agents symbolically, "resolved to keys
    // at the seam" (IGLP, "Simulation realisation"), so the kernels resolve a
    // peer's name through its directory; "under closed, first contact from an
    // agent not declared a peer is not answered", and "the effect of an
    // address or a rendezvous declaration" is not provided.
    test('declare(peer(P)) resolves a peer by its name and reaches the router; '
        'an address and a rendezvous server are taken to no effect', () async {
      final directory = NetworkDirectory();
      final bobPk = generateKeyPair().pub;
      final carolPk = generateKeyPair().pub;
      directory.register('bob', bobPk);
      final toRouter = <(String, bool)>[];
      final network = SimulationNetworkClient(
        selfId: 'alice',
        directory: directory,
        keyPair: generateKeyPair(),
        sendToRouter: (toId, payload) {},
        knownPeerToRouter: (pk, known) => toRouter.add((pk.hex, known)),
      );
      final out = <String>[];
      final engine = _engine(out, network);
      engine.loadSource('''
procedure go.
go :- declare(peer(bob)), declare(peer('${carolPk.hex}')),
    declare(address(bob, '203.0.113.7:41234')),
    declare(rendezvous(bob, '198.51.100.9:7000')), remove(rendezvous(bob)),
    send_to_user([returned]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue);
      expect(out, ['returned']);
      expect(network.isKnownPeer(bobPk), isTrue,
          reason: 'bob is resolved to his key through the directory');
      expect(network.isKnownPeer(carolPk), isTrue,
          reason: 'a key in hex names its peer too');
      expect(toRouter, unorderedEquals([(bobPk.hex, true), (carolPk.hex, true)]));

      final engine2 = _engine(<String>[], network);
      engine2.loadSource('''
procedure go.
go :- declare(peer(zed)).
''');
      expect((await engine2.runGoal('go')).succeeded, isFalse,
          reason: 'zed is no agent the simulation names, and no key');
    });

    test('acknowledge hands the messageId to whoever delivered the message',
        () {
      // GNA 776016c, Section 2.9, acknowledge(messageId); the simulation's
      // router hands a message to the agent once and keeps none across a
      // restart, so the acknowledgement goes to whoever delivered it.
      final told = <String>[];
      final c = SimulationNetworkClient(
        selfId: 'alice',
        directory: NetworkDirectory(),
        keyPair: generateKeyPair(),
        sendToRouter: (_, __) {},
        acknowledgeToRouter: told.add,
      );
      c.acknowledge('7');
      c.acknowledge('8');
      expect(told, ['7', '8']);
      expect(() => client().acknowledge('9'), returnsNormally,
          reason: 'with no one to tell, it is taken');
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
