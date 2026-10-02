/// A goal writer against a head writer fails, in the runtime.
///
/// GLP-Spec appendix-term-matching.tex, Definition "Term Matching", row
/// "Writer X1", column "Writer X2": "fail".  The head writer is named or
/// anonymous --- `_` and `_X` are writers of their own (glp.tex, Remark
/// "Anonymous Variables", c3d3fc6: "each occurrence denotes a fresh writer
/// with no paired reader, the one exception to SRSW: an input a clause does
/// not read is dropped") --- at an argument or in a structure, first
/// occurrence or later, and the program is typed or not (GLP #3 Cowork,
/// 2026-10-02 13:00 UTC, answering Integration's 10:34 UTC question: "yes, the
/// runtime fails a goal writer against a head writer too, by the table, typed
/// or not"; landed by its 17:12 UTC, 1(a)).  Both partial evaluators fail a
/// call writer against a unit clause's head writer already
/// (pe_head_writer_test.dart).
///
/// Until 2026-10-02 the runtime took a goal writer at a head writer as the
/// head's variable: `s(_)`, `s1(f(_))`, `s2(X) :- t(X?)`, `s3(f(X)) :- ...`,
/// `s5([_|_])` and `s6(f(X), X?)` each succeeded with a goal writer there, and
/// only a later writer occurrence after a reader, `s4(X?, X)`, failed.  The
/// head instructions now fail it: `get_variable` and `get_value` in writer
/// mode, `unify_variable` and `head_variable` in writer mode in READ, and
/// `unify_void` in READ.  Two changes of codegen go with it: a head `_` at an
/// argument is `get_variable` in writer mode on a register of its own, where
/// it compiled to nothing; and a structure at an argument is matched by
/// `head_structure` at the argument, as a list is, where it was first taken
/// into a register by `get_variable` in writer mode, which is a head writer.
///
/// A goal reader or a goal term at a head writer is still assigned to it, and a
/// goal writer at a head term is still assigned the term (row "Writer X1",
/// column "Term f2/n2").
///
/// A goal writer at a head writer is ill-moded (TGLP well-typing.tex: a
/// writer is consistent only at a produced position), so the REPL's goal
/// check refuses any goal that would pass one there; these programs are
/// compiled and their goals posted to the scheduler directly, as
/// anonymous_head_reader_test.dart posts its own.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart'
    show BytecodeProgram, CallEnv;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _untyped = '''
s(_).
a(_X).
s1(f(_)).
s2(X) :- t(X?).
t(_).
s3(f(X)) :- t(X?).
s4(X?, X).
s5([_|_]).
s6(f(X), X?).
n(f(g(_))).
''';

/// The same head writers, typed: the runtime does not consult the types.
const _typed = '''
procedure ts(Integer?).
ts(_).

F ::= f(Integer).
procedure ts3(F?).
ts3(f(X)) :- tt(X?).

procedure tt(Integer?).
tt(_).
''';

/// Post [proc] of [source] with the arguments [argsOf] makes, run it, and give
/// its status and the arguments as they stand after the run.
(ExecutionStatus, List<Term>, GlpRuntime) _run(
  BytecodeProgram program,
  String proc,
  List<Term> Function(GlpRuntime rt) argsOf,
) {
  final rt = GlpRuntime();
  final image = codeImageFromProgram(program);
  final sched = Scheduler(rt: rt, runner: ByteRunner(image));
  final args = argsOf(rt);
  final id = rt.nextGoalId++;
  rt.setGoalEnv(
    id,
    CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}),
  );
  rt.gq.enqueue(GoalRef(id, image.entryOffsetOf(proc)!));
  return (sched.drainWithStatus().status, args, rt);
}

final _untypedProgram = GlpCompiler().compile(_untyped);

ExecutionStatus _post(String proc, List<Term> Function(GlpRuntime rt) argsOf) =>
    _run(_untypedProgram, proc, argsOf).$1;

/// A goal term as the REPL passes one: the reader of a writer bound to it.
Term _bound(GlpRuntime rt, Term t) {
  final (w, r) = rt.heap.allocateVariable();
  rt.heap.bindWriter(w, t);
  return VarRef(r);
}

/// The reader, or the writer, of a fresh unbound variable.
Term _reader(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$2);
Term _writer(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$1);

Term _f(GlpRuntime rt, Term arg) => _bound(rt, StructTerm('f', [arg]));
Term _one(GlpRuntime rt) => _bound(rt, ConstTerm(1));

/// A procedure's code, from its entry label to its end label.
List<op.Op> _code(BytecodeProgram prog, String sig) =>
    prog.ops.sublist(prog.labels[sig]!, prog.labels['${sig}_end']!);

void main() {
  group('a goal writer at a head writer fails (row Writer X1, column '
      'Writer X2)', () {
    test('s(_) with a goal writer: `_` at an argument', () {
      expect(_post('s/1', (rt) => [_writer(rt)]), ExecutionStatus.failed);
    });

    test('a(_X) with a goal writer: a named anonymous variable at an argument',
        () {
      expect(_post('a/1', (rt) => [_writer(rt)]), ExecutionStatus.failed);
    });

    test('s1(f(W)) against s1(f(_)): `_` in a structure', () {
      expect(
        _post('s1/1', (rt) => [_f(rt, _writer(rt))]),
        ExecutionStatus.failed,
      );
    });

    test('s2(W) against s2(X) :- t(X?): a named writer at an argument', () {
      expect(_post('s2/1', (rt) => [_writer(rt)]), ExecutionStatus.failed);
    });

    test('s3(f(W)) against s3(f(X)) :- t(X?): a named writer in a structure',
        () {
      expect(
        _post('s3/1', (rt) => [_f(rt, _writer(rt))]),
        ExecutionStatus.failed,
      );
    });

    test('s4(W1, W2) against s4(X?, X): a later writer occurrence', () {
      expect(
        _post('s4/2', (rt) => [_writer(rt), _writer(rt)]),
        ExecutionStatus.failed,
      );
    });

    test('s5([1|W]) and s5([W|R?]) against s5([_|_]): `_` in a list', () {
      expect(
        _post(
          's5/1',
          (rt) => [
            _bound(rt, StructTerm('.', [_one(rt), _writer(rt)])),
          ],
        ),
        ExecutionStatus.failed,
      );
      expect(
        _post(
          's5/1',
          (rt) => [
            _bound(rt, StructTerm('.', [_writer(rt), _reader(rt)])),
          ],
        ),
        ExecutionStatus.failed,
      );
    });

    test('s6(f(W1), W2) against s6(f(X), X?): the writer first, in a '
        'structure', () {
      expect(
        _post('s6/2', (rt) => [_f(rt, _writer(rt)), _writer(rt)]),
        ExecutionStatus.failed,
      );
    });

    test('typed or not: ts(W) and ts3(f(W)) of a typed program', () {
      final typed = GlpCompiler().compile(_typed);
      expect(
        _run(typed, 'ts/1', (rt) => [_writer(rt)]).$1,
        ExecutionStatus.failed,
      );
      expect(
        _run(typed, 'ts3/1', (rt) => [_f(rt, _writer(rt))]).$1,
        ExecutionStatus.failed,
      );
    });

    test('head_variable (0x14) in writer mode, in a decoded artefact: '
        'hv(f(W)) against hv(f(X))', () {
      // No compiler of this tree emits head_variable; an artefact may carry it.
      final hv = BytecodeProgram([
        op.Label('hv/1'),
        op.ClauseTry(),
        op.HeadStructure('f', 1, 0),
        op.HeadVariable(1, isReader: false),
        op.Commit(),
        op.Proceed(),
        op.Label('hv/1_end'),
        op.NoMoreClauses(),
      ]);
      expect(
        _run(hv, 'hv/1', (rt) => [_f(rt, _writer(rt))]).$1,
        ExecutionStatus.failed,
      );
      expect(
        _run(hv, 'hv/1', (rt) => [_f(rt, _one(rt))]).$1,
        ExecutionStatus.succeeded,
        reason: 'a goal term there is assigned to it',
      );
    });
  });

  group('a goal reader or term at a head writer is assigned to it (rows '
      'Reader X1? and Term f1/n1)', () {
    test('s(R?), s(1), a(R?)', () {
      expect(_post('s/1', (rt) => [_reader(rt)]), ExecutionStatus.succeeded);
      expect(_post('s/1', (rt) => [_one(rt)]), ExecutionStatus.succeeded);
      expect(_post('a/1', (rt) => [_reader(rt)]), ExecutionStatus.succeeded);
    });

    test('s1(f(1)), s1(f(R?))', () {
      expect(_post('s1/1', (rt) => [_f(rt, _one(rt))]),
          ExecutionStatus.succeeded);
      expect(_post('s1/1', (rt) => [_f(rt, _reader(rt))]),
          ExecutionStatus.succeeded);
    });

    test('s2(R?), s2(1), s3(f(1))', () {
      expect(_post('s2/1', (rt) => [_reader(rt)]), ExecutionStatus.succeeded);
      expect(_post('s2/1', (rt) => [_one(rt)]), ExecutionStatus.succeeded);
      expect(_post('s3/1', (rt) => [_f(rt, _one(rt))]),
          ExecutionStatus.succeeded);
    });

    test('s4(W1, R?): W1 is assigned the reader of X, and X the goal reader',
        () {
      final (status, args, rt) =
          _run(_untypedProgram, 's4/2', (rt) => [_writer(rt), _reader(rt)]);
      expect(status, ExecutionStatus.succeeded);
      expect(
        rt.heap.dereference(args[0]),
        rt.heap.dereference(args[1]),
        reason: 'W1 and R? end at the same variable',
      );
    });

    test('s5([1|R?])', () {
      expect(
        _post(
          's5/1',
          (rt) => [
            _bound(rt, StructTerm('.', [_one(rt), _reader(rt)])),
          ],
        ),
        ExecutionStatus.succeeded,
      );
    });
  });

  group('a goal writer at a head term is assigned the term (row Writer X1, '
      'column Term f2/n2)', () {
    test('s1(W) gives W = f(_), a fresh writer in it', () {
      final (status, args, rt) =
          _run(_untypedProgram, 's1/1', (rt) => [_writer(rt)]);
      expect(status, ExecutionStatus.succeeded);
      final t = rt.heap.dereference(args[0]);
      expect(t, isA<StructTerm>());
      expect((t as StructTerm).functor, 'f');
      final inner = t.args.single as VarRef;
      expect(rt.heap.isWriter(inner.addr), isTrue);
    });

    test('s3(W) and s5(W) succeed', () {
      expect(_post('s3/1', (rt) => [_writer(rt)]), ExecutionStatus.succeeded);
      expect(_post('s5/1', (rt) => [_writer(rt)]), ExecutionStatus.succeeded);
    });

    test('n(f(W)) against n(f(g(_))): W = g(_), the nested structure placed '
        'after pop meeting the writer it was assigned', () {
      final (status, args, rt) =
          _run(_untypedProgram, 'n/1', (rt) => [_f(rt, _writer(rt))]);
      expect(status, ExecutionStatus.succeeded);
      final f = rt.heap.dereference(args[0]) as StructTerm;
      expect(rt.heap.dereference(f.args.single).toString(), startsWith('g('));
    });

    test('through the REPL, a typed head structure binds the goal writer',
        () async {
      final engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path,
      );
      expect(
        engine.loadSource('''
Pair ::= pair(Integer, Integer).
procedure mk(Pair).
mk(pair(1, 2)).
'''),
        isTrue,
      );
      final r = await engine.runGoal('mk(P)');
      expect(r.status, ExecutionStatus.succeeded, reason: 'Error: ${r.error}');
      expect(
        engine.runtime.heap.dereference(r.bindings['P']!).toString(),
        contains('pair'),
      );
    });
  });

  group('codegen', () {
    test('a head `_` or `_X` at an argument is get_variable in writer mode on '
        'a register of its own', () {
      for (final sig in ['s/1', 'a/1']) {
        final gets = _code(_untypedProgram, sig).whereType<op.GetVariable>();
        expect(gets.length, 1, reason: sig);
        expect(gets.single.isReader, isFalse, reason: sig);
        expect(gets.single.argSlot, 0, reason: sig);
      }
    });

    test('a structure at an argument is head_structure at the argument, with '
        'no get_variable before it', () {
      for (final sig in ['s1/1', 's3/1', 'n/1']) {
        final code = _code(_untypedProgram, sig);
        expect(code.whereType<op.GetVariable>(), isEmpty, reason: sig);
        final hs = code.whereType<op.HeadStructure>().single;
        expect(hs.functor, 'f', reason: sig);
        expect(hs.argSlot, 0, reason: sig);
      }
    });
  });
}
