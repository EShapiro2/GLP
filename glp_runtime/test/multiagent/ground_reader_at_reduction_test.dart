/// Every occurrence of a ground-guarded reader receives the writer's value at the reduction, by Reduce applying the head writers' values to the body.
///
/// Normative source: the GLP paper, GLP-Spec 57e290d, glp.tex, Definition "cGLP Transition System", Reduce: "$G' = (G \setminus \{A\}) \cup B(\hat\sigma?)^\star$, and $\sigma' = \sigma \circ \tau$, where $\tau$ is the restriction of $(\hat\sigma?)^\star$ to the readers of the writers of $A$", and Section 2.3, Guards, Definitions "Ground Position, Ground-Guarded Writer" and "Ground Instantiation, SRSW of a Guarded Clause"; IGLP 405a2d8, concurrent-glp.tex, the same Definition: "The head writers' values are applied to the body, whose readers are theirs; the goal writers' values are communicated to their readers, which are elsewhere"; and maglp.tex, Definition "Multiagent GLP", Communicate, which removes the assignment from $\sigma_p$.
///
/// vGLP #13's case (GLP, 2026-10-15 11:00 UTC, item 3):
///
///   p(X, f(X?)) :- ground(X?) | q(X?).
///
/// with the goal p(a, Y), r(Y?).  The writer mgu is {X := a, Y := f(X?)}: the body receives X's value, q(a), and Y's value with X's applied, f(a), is communicated to r.  q receives a and r receives f(a), q and r at one agent, and at two.
///
/// At two, the value alice sends bob on the link of Y? is f(a), in one message, and no later delivery carries a: the value of a Send is $T^\uparrow$ (IGLP 405a2d8, madglp-spec.tex, Definition "madGLP Send Transaction", Reader case), and T here is f(a).  Bob applies it once, removes the entry and acknowledges; the acknowledgement releases the pending value at alice, closing the link; and a repeat of the value is dropped (the same Definition, Reader and Acknowledgement cases; Remark "Message Delivery").
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart' show wireMsgKindAcknowledgement;
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;

/// vGLP's p/2, and q/1 and r/1 that show their person what they receive:
/// programs/tests/communicate/ground_reader.glp, its comments dropped.
final String _procsPath =
    File('../programs/tests/communicate/ground_reader.glp').absolute.path;
final String _procs = File(_procsPath)
    .readAsLinesSync()
    .where((l) => !l.startsWith('%'))
    .join('\n');

/// q and r at one agent, alice.
final String _oneAgent = '''
procedure boot.
boot :- alice_init(alice, _)@alice.

procedure alice_init(_?, _?).
alice_init(_, _) :- p(a, Y), r(Y?).
$_procs''';

/// q at alice, r at bob: alice sends bob Y? before p is reduced, so bob's r
/// holds the reader of the variable p assigns f(X?) to.
final String _twoAgents = '''
procedure boot.
boot :- alice_init(alice, _)@alice, bob_init(bob, _)@bob.

procedure alice_init(_?, _?).
alice_init(_, _) :- send_to_net([msg(bob, Y?)]), p(a, Y).

procedure bob_init(_?, _?).
bob_init(_, [msg(_, Y) | _]) :- r(Y?).
$_procs''';

/// As [_twoAgents], with a itself communicated to alice: bob sends alice the
/// reader A? and then assigns A := a, so the ground-guarded reader alice's p
/// reads is assigned at bob, and its occurrences in q at alice and in f(X?),
/// sent back to bob, each receive it.
final String _fromBob = '''
procedure boot.
boot :- alice_init(alice, _)@alice, bob_init(bob, _)@bob.

procedure alice_init(_?, _?).
alice_init(_, [msg(_, A) | _]) :- send_to_net([msg(bob, Y?)]), p(A?, Y).

procedure bob_init(_?, _?).
bob_init(_, In) :- send_to_net([msg(alice, A?)]), give(A), take(In?).

procedure give(Constant).
give(a).

procedure take(_?).
take([msg(_, Y) | _]) :- r(Y?).
$_procs''';

/// alice's half of [_twoAgents], run in one engine over madGLP: she sends bob the reader Y?, and p assigns Y its value.
final String _aliceSends = '''
procedure go.
go :- send_to_net([msg(bob, Y?)]), p(a, Y).
$_procs''';

/// Whether [t] is the term f(a).
bool _isFOfA(Term t) =>
    t is StructTerm &&
    t.functor == 'f' &&
    t.args.length == 1 &&
    t.args.single is ConstTerm &&
    (t.args.single as ConstTerm).value == 'a';

void main() {
  test('alice sends bob f(a) on the link of Y?, in one message; bob applies it once, and the acknowledgement closes the link', () async {
    final alice = GlpEngine(rootSelfGlpPath: _rootSelf);
    alice.enableMadGLP(agentId: 'alice');
    final aliceCtx = alice.madContext!;
    final aliceLines = <String>[];
    alice.runtime.outputCallback = aliceLines.add;
    final toBob = <List<int>>[];
    aliceCtx.onMessageReady = (dest, msg) {
      expect(dest, 'bob');
      toBob.add(msg.payload);
    };
    expect(alice.loadSource(_aliceSends, filename: 'alice'), isTrue);
    final run = await alice.runGoal('go');
    expect(run.error, isNull);
    expect(aliceLines, ['q(a)']);

    // Two messages: the cold call carrying the reader Y?, and Y's value on its link, f(a).
    final sent = [for (final p in toBob) PayloadCodec.deserializeValuePayload(p)];
    expect(sent.map((m) => '${m.name} := ${m.value}'), hasLength(2));
    final coldCall = sent.singleWhere((m) => m.serial != null);
    expect(coldCall.name, GlobalName.writer('bob', 0));
    final link = extractGlobalNames(coldCall.value).singleWhere((n) => n.isReader);
    final value = sent.singleWhere((m) => m.serial == null);
    expect(value.name, link);
    expect(_isFOfA(value.value), isTrue, reason: '${value.value}');
    expect(aliceCtx.pendingReaderValueCount, 1);

    // bob receives both and holds f(a) on his network input, msg(bob, Z?) with Z := f(a).
    final bobRt = GlpRuntime();
    final bob = MadContext(agentId: 'bob', runtime: bobRt);
    final (bobNetIn, _) = bobRt.heap.allocateVariable();
    bob.wp.initializeSerializerEntry(bobNetIn);
    final toAlice = <List<int>>[];
    bob.onMessageReady = (dest, msg) {
      expect(dest, 'alice');
      toAlice.add(msg.payload);
    };
    for (final p in toBob) {
      bob.handleIncomingPayload(payload: p, fromAgent: 'alice');
    }
    final input = bobRt.heap.derefAddr(bobNetIn) as StructTerm;
    final msg = input.args[0] as StructTerm;
    expect(msg.functor, 'msg');
    final z = bobRt.heap.derefAddr((msg.args[1] as VarRef).addr);
    expect(z is Term && _isFOfA(z), isTrue, reason: '$z');
    expect(bob.wp.localizeEntryCount, 0);

    // bob acknowledges once; the acknowledgement releases the pending value at alice, closing the link.
    bob.flushMessages();
    expect(toAlice, hasLength(1));
    expect(toAlice.single[0], wireMsgKindAcknowledgement);
    expect(PayloadCodec.decodeRequestOrAckPayload(toAlice.single), link);
    aliceCtx.handleIncomingPayload(payload: toAlice.single, fromAgent: 'bob');
    expect(aliceCtx.pendingReaderValueCount, 0);
    expect(aliceCtx.globalSendRegistry.pendingCount, 0);

    // A repeat of the value is dropped: nothing applied, nothing acknowledged.
    final valuePayload = toBob[sent.indexOf(value)];
    bob.handleIncomingPayload(payload: valuePayload, fromAgent: 'alice');
    bob.flushMessages();
    expect(toAlice, hasLength(1));
    expect(bob.earlyAssignmentCount, 0);
  });

  test('one agent, one heap: q receives a and r receives f(a)', () async {
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    final lines = <String>[];
    engine.runtime.outputCallback = lines.add;
    expect(engine.loadFile(_procsPath), isTrue);
    final result = await engine.runGoal('p(a, Y), r(Y?)');
    expect(result.succeeded, isTrue, reason: '${result.error}');
    expect(lines.toSet(), {'q(a)', 'r(f(a))'});
  });

  group('agents in isolates', () {
    late IsolateManager manager;
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test('q and r at one agent: q receives a and r receives f(a)', () async {
      final config = BootLoader().load(_oneAgent);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice').toSet(), {'q(a)', 'r(f(a))'});
    }, timeout: const Timeout(Duration(seconds: 60)));

    test('q at alice and r at bob: q receives a and r receives f(a)',
        () async {
      final config = BootLoader().load(_twoAgents);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice'), ['q(a)']);
      expect(manager.outputOf('bob'), ['r(f(a))']);
    }, timeout: const Timeout(Duration(seconds: 60)));

    test('a assigned at bob, q at alice and r at bob: q receives a and r '
        'receives f(a)', () async {
      final config = BootLoader().load(_fromBob);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice'), ['q(a)']);
      expect(manager.outputOf('bob'), ['r(f(a))']);
    }, timeout: const Timeout(Duration(seconds: 60)));
  });
}
