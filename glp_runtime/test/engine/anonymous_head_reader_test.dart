/// The anonymous head reader `_?`.
///
/// TGLP typed-glp.tex, "Anonymous variables": in a clause head, at a produced
/// position, an anonymous variable is written `_?` and "denotes an output the
/// clause never produces" --- the output placeholder `Out?` of a variable
/// whose writer `Out` occurs nowhere in the clause.  So at a head position it
/// is a head reader, and the head is matched by GLP-Spec
/// appendix-term-matching.tex, Definition "Term Matching", column
/// "Reader X2?": a goal writer is assigned the head's reader (X1 := X2?), a
/// goal reader fails, and a goal term fails.  GLP's task of 2026-10-02
/// 08:40 UTC, B: "a goal constant meeting it fails as it would meet any head
/// reader".
///
/// Until 2026-10-02 codegen compiled a head `_?` as it compiles `_`: nothing
/// at an argument and `unify_void` in a structure, both of which pass over
/// whatever the goal holds there, so `p(1, 2)` succeeded against `p(_, _?)`;
/// and `unify_void` building a structure for a goal writer placed a fresh
/// writer where the clause's output placeholder stands, so `w(W)` gave
/// `pair(_, _)` where it gives `pair(_, _?)`.  It now compiles `_?` as a head
/// reader of a variable of its own: `get_variable` in reader mode at an
/// argument, `unify_variable` in reader mode in a structure.
///
/// p/2 has `_?` at an argument, c/1 inside a consumed structure, o/1 inside a
/// produced one, and w/1 has `_` and `_?` side by side; cons/2 is the shape a
/// `receive` guard unfolds to, `ch([Message|In], Out?)` in the head with `_`
/// and `_?` where the continuation is discarded (programs/tests/probe13_strict,
/// programs/tests/recv2x2_neg.glp).
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart' show CallEnv;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _typed = '''
Ch ::= ch(Integer, Integer?).
Out2 ::= out(Integer).
Pair ::= pair(Integer?, Integer).

procedure p(Integer?, Integer).
p(_, _?).

procedure c(Ch?).
c(ch(_, _?)).

procedure o(Out2).
o(out(_?)).

procedure w(Pair).
w(pair(_, _?)).

Item ::= it(Constant).
Items ::= [] ; [Item | Items].
Closed ::= closed.

procedure cons(Channel(Items, Closed)?, Constant).
cons(ch([it(_)|_], _?), matched).
cons(_, other) :- otherwise | true.
''';

/// A goal reader at `_?` is ill-moded (TGLP well-typing.tex: a reader is
/// consistent only at a consumed position), so the REPL's goal check refuses
/// any goal that would pass one there; this program is compiled untyped and
/// its goals posted to the scheduler directly, as suspended_map_test.dart
/// posts its own.
const _untyped = '''
p(_, _?).
c(ch(_, _?)).
''';

/// Post [proc] with the arguments [argsOf] makes, run it, and give its status.
ExecutionStatus _post(String proc, List<Term> Function(GlpRuntime rt) argsOf) {
  final rt = GlpRuntime();
  final image = codeImageFromProgram(GlpCompiler().compile(_untyped));
  final sched = Scheduler(rt: rt, runner: ByteRunner(image));
  final args = argsOf(rt);
  final id = rt.nextGoalId++;
  rt.setGoalEnv(
    id,
    CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}),
  );
  rt.gq.enqueue(GoalRef(id, image.entryOffsetOf(proc)!));
  return sched.drainWithStatus().status;
}

/// A goal term as the REPL passes one: the reader of a writer bound to it.
Term _bound(GlpRuntime rt, Term t) {
  final (w, r) = rt.heap.allocateVariable();
  rt.heap.bindWriter(w, t);
  return VarRef(r);
}

/// The reader, or the writer, of a fresh unbound variable.
Term _reader(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$2);
Term _writer(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$1);

GlpEngine _engine(String source) {
  final engine = GlpEngine(
    rootSelfGlpPath: File('../programs/self.glp').absolute.path,
  );
  expect(engine.loadSource(source), isTrue);
  return engine;
}

/// Expect goal variable [name] of [r] to be unbound: runGoal gives an unbound
/// variable the binding null.
void _expectUnbound(GlpEngine engine, ExecutionResult r, String name) {
  expect(r.bindings.containsKey(name), isTrue, reason: 'no variable $name');
  final t = r.bindings[name];
  expect(
    t == null || engine.runtime.heap.dereference(t) is VarRef,
    isTrue,
    reason: '$name = $t',
  );
}

void main() {
  group('`_?` compiles as a head reader', () {
    test('get_variable and unify_variable in reader mode, not unify_void', () {
      final prog = _engine(_typed).loadedPrograms['_source_']!;
      // A procedure's code runs from its entry label to its end label.
      List<Object?> code(String sig) =>
          prog.ops.sublist(prog.labels[sig]!, prog.labels['${sig}_end']!);
      final ops = [
        for (final sig in ['p/2', 'c/1', 'o/1', 'w/1', 'cons/2']) ...code(sig),
      ];
      final readerGets = ops
          .whereType<op.GetVariable>()
          .where((o) => o.isReader)
          .length;
      final readerUnifies = ops
          .whereType<op.UnifyVariable>()
          .where((o) => o.isReader)
          .length;
      final voids = ops.whereType<op.UnifyVoid>().length;
      // The source names no variable, so every reader instruction is a `_?`:
      // p/2's at an argument; c/1's, o/1's, w/1's and cons/2's in a structure.
      expect(readerGets, 1);
      expect(readerUnifies, 4);
      // c/1's, w/1's and cons/2's two `_`: unify_void is the writer's alone.
      expect(voids, 4);
      // No reduce/2 clauses are generated since 2026-10-02 (weeding round
      // three, item 10), so the expectation on them that stood here went with
      // the generation.
    });
  });

  group('a goal term at `_?` fails (column Reader X2?, row Term f1/n1)', () {
    test('p(1, 2): a constant at an argument', () async {
      final r = await _engine(_typed).runGoal('p(1, 2)');
      expect(r.status, ExecutionStatus.failed, reason: 'Error: ${r.error}');
    });

    test('c(ch(1, 2)): a constant in a consumed structure', () async {
      final r = await _engine(_typed).runGoal('c(ch(1, 2))');
      expect(r.status, ExecutionStatus.failed, reason: 'Error: ${r.error}');
    });

    test('o(out(3)): a constant in a produced structure', () async {
      final r = await _engine(_typed).runGoal('o(out(3))');
      expect(r.status, ExecutionStatus.failed, reason: 'Error: ${r.error}');
    });

    test('cons(ch([it(a)], closed), A): the unfolded receive with a closed '
        'Out falls to otherwise', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('cons(ch([it(a)], closed), A)');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      expect(
        engine.runtime.heap.dereference(r.bindings['A']!).toString(),
        contains('other'),
      );
    });

    test('cons(ch(S?, closed), A): the mismatch fails the clause though the '
        'head suspended on S? first, and otherwise answers at once', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('cons(ch(S?, closed), A)');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      expect(
        engine.runtime.heap.dereference(r.bindings['A']!).toString(),
        contains('other'),
      );
    });
  });

  group('a goal reader at `_?` fails (column Reader X2?, row Reader X1?)', () {
    test('p(1, R?) at an argument', () {
      expect(
        _post('p/2', (rt) => [_bound(rt, ConstTerm(1)), _reader(rt)]),
        ExecutionStatus.failed,
      );
    });

    test('c(ch(1, R?)) in a structure', () {
      expect(
        _post(
          'c/1',
          (rt) => [
            _bound(
              rt,
              StructTerm('ch', [_bound(rt, ConstTerm(1)), _reader(rt)]),
            ),
          ],
        ),
        ExecutionStatus.failed,
      );
    });

    test('and the same posts with a goal writer there succeed (control)', () {
      expect(
        _post('p/2', (rt) => [_bound(rt, ConstTerm(1)), _writer(rt)]),
        ExecutionStatus.succeeded,
      );
      expect(
        _post(
          'c/1',
          (rt) => [
            _bound(
              rt,
              StructTerm('ch', [_bound(rt, ConstTerm(1)), _writer(rt)]),
            ),
          ],
        ),
        ExecutionStatus.succeeded,
      );
    });

    test('and with a goal constant there fail, as through the REPL', () {
      expect(
        _post(
          'p/2',
          (rt) => [_bound(rt, ConstTerm(1)), _bound(rt, ConstTerm(2))],
        ),
        ExecutionStatus.failed,
      );
    });
  });

  group('a goal writer at `_?` is assigned it (row Writer X1: X1 := X2?)', () {
    test(
      'p(1, Y) succeeds and Y stays unbound: the output is never produced',
      () async {
        final engine = _engine(_typed);
        final r = await engine.runGoal('p(1, Y)');
        expect(
          r.status,
          ExecutionStatus.succeeded,
          reason: 'Error: ${r.error}',
        );
        _expectUnbound(engine, r, 'Y');
      },
    );

    test('c(ch(1, Y)) succeeds, Y unbound', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('c(ch(1, Y))');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      _expectUnbound(engine, r, 'Y');
    });

    test('o(out(Z)) succeeds, Z unbound', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('o(out(Z))');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      _expectUnbound(engine, r, 'Z');
    });

    test('cons(ch([it(a)], Out), A): the unfolded receive with an open Out '
        'matches', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('cons(ch([it(a)], Out), A)');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      expect(
        engine.runtime.heap.dereference(r.bindings['A']!).toString(),
        contains('matched'),
      );
      _expectUnbound(engine, r, 'Out');
    });

    test('w(W) gives W = pair(_, _?): a goal writer is assigned the head '
        'term, `_` a fresh writer in it and `_?` the reader of a variable '
        'nothing assigns', () async {
      final engine = _engine(_typed);
      final r = await engine.runGoal('w(W)');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      final heap = engine.runtime.heap;
      final pair = heap.dereference(r.bindings['W']!);
      expect(pair, isA<StructTerm>());
      final args = (pair as StructTerm).args;
      expect(pair.functor, 'pair');
      final first = args[0] as VarRef;
      final second = args[1] as VarRef;
      expect(heap.isWriter(first.addr), isTrue, reason: '`_` is a writer');
      expect(heap.isWriterBound(first.addr), isFalse);
      expect(heap.isReader(second.addr), isTrue, reason: '`_?` is a reader');
      expect(heap.isReaderBound(second.addr), isFalse);
    });
  });
}
