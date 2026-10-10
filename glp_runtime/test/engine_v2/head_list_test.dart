/// `head_list` (0x13) matches a list cell, the structure `'.'/2`, as
/// `head_structure` for `'.'/2` does.
///
/// IGLP code-format-fragment.tex, "Terms": "Lists are structures: a cell is
/// the structure '.'/2; the empty list is the constant nil".  GLP-Spec
/// appendix-term-matching.tex, Definition "Term Matching", row "Writer X1",
/// column "Term f2/n2": "X1 := T2", so a goal writer is assigned the cell; row
/// "Reader X1?" suspends; a goal term is matched (GLP #3 Cowork, 2026-10-02
/// 17:12 UTC, 2: "head_list assigns an unbound goal writer a two-slot cell as
/// head_structure does, and matches '.'/2").  Until 2026-10-02 it gave an
/// unbound goal writer a `'[|]'` cell with no slots, so placing the first
/// element threw, and matched a bound list only as `'[|]'/2`.
///
/// No compiler of this tree emits head_list, a list in a head compiling to
/// head_structure `'.'/2`; an artefact may carry it.  So each program here is
/// assembled by hand, encoded as an artefact and decoded from its bytes
/// ([codeImageFromProgram]), and run from the decoded code by the byte runner.
library;

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart'
    show BytecodeProgram, CallEnv;
import 'package:glp_runtime/engine_v2/code_image.dart' show CodeImage;
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/instruction_codec.dart' show decodeCode;
import 'package:test/test.dart';

/// `hl([1])`: head_list at argument 0, its head the constant 1 and its tail
/// nil, and `hn([[1]|_])`: a cell whose head is a cell, nested by push,
/// unify_structure and pop as codegen nests a list in a head.
final CodeImage _image = codeImageFromProgram(BytecodeProgram([
  op.Label('hl/1'),
  op.ClauseTry(),
  op.HeadList(0),
  op.UnifyConstant(1),
  op.UnifyConstant(nil),
  op.Commit(),
  op.Proceed(),
  op.Label('hl/1_end'),
  op.NoMoreClauses(),
  op.Label('hn/1'),
  op.ClauseTry(),
  op.HeadList(0),
  op.Push(10),
  op.UnifyStructure('.', 2),
  op.UnifyConstant(1),
  op.UnifyConstant(nil),
  op.Pop(10),
  op.UnifyVariable(10, isReader: false),
  op.UnifyVoid(count: 1),
  op.Commit(),
  op.Proceed(),
  op.Label('hn/1_end'),
  op.NoMoreClauses(),
]));

/// Post [proc] with [argsOf]'s arguments, run it from the decoded code, and
/// give its status, the arguments, and the runtime.
(ExecutionStatus, List<Term>, GlpRuntime) _run(
  String proc,
  List<Term> Function(GlpRuntime rt) argsOf,
) {
  final rt = GlpRuntime();
  final sched = Scheduler(rt: rt, runner: ByteRunner(_image));
  final args = argsOf(rt);
  final id = rt.nextGoalId++;
  rt.setGoalEnv(
    id,
    CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}),
  );
  rt.gq.enqueue(GoalRef(id, _image.entryOffsetOf(proc)!));
  return (sched.drainWithStatus().status, args, rt);
}

/// A goal term as the REPL passes one: the reader of a writer bound to it.
Term _bound(GlpRuntime rt, Term t) {
  final (w, r) = rt.heap.allocateVariable();
  rt.heap.bindWriter(w, t);
  return VarRef(r);
}

Term _writer(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$1);
Term _reader(GlpRuntime rt) => VarRef(rt.heap.allocateVariable().$2);

/// The cell [functor]`(h, t)` of the goal, its elements constants.
Term _cell(GlpRuntime rt, String functor, Object h, Object t) =>
    _bound(rt, StructTerm(functor, [ConstTerm(h), ConstTerm(t)]));

void main() {
  test('the decoded code holds head_list, as the artefact carries it', () {
    final ops = decodeCode(_image.code,
        procNameOf: (i) => _image.symbolAt(i).signature);
    expect(ops.whereType<op.HeadList>().map((o) => o.argSlot), [0, 0]);
  });

  group('a goal writer is assigned a cell of two slots (column "Term f2/n2")',
      () {
    test('hl(W) gives W = [1]', () {
      final (status, args, rt) = _run('hl/1', (rt) => [_writer(rt)]);
      expect(status, ExecutionStatus.succeeded);
      final w = rt.heap.dereference(args[0]);
      expect(w, isA<StructTerm>());
      final cell = w as StructTerm;
      expect(cell.functor, '.');
      expect(cell.args, hasLength(2));
      expect((rt.heap.dereference(cell.args[0]) as ConstTerm).value, 1);
      expect((rt.heap.dereference(cell.args[1]) as ConstTerm).value, nil);
    });

    test('hn(W) gives W = [[1]|_], the nested cell built in the first', () {
      final (status, args, rt) = _run('hn/1', (rt) => [_writer(rt)]);
      expect(status, ExecutionStatus.succeeded);
      final outer = rt.heap.dereference(args[0]) as StructTerm;
      expect(outer.functor, '.');
      final inner = rt.heap.dereference(outer.args[0]) as StructTerm;
      expect(inner.functor, '.');
      expect((rt.heap.dereference(inner.args[0]) as ConstTerm).value, 1);
      expect((rt.heap.dereference(inner.args[1]) as ConstTerm).value, nil);
      final tail = outer.args[1] as VarRef;
      expect(rt.heap.isWriter(tail.addr), isTrue,
          reason: "the tail is `_`'s fresh writer");
    });
  });

  group('a goal cell is matched as the structure \'.\'/2', () {
    test('hl([1]) succeeds', () {
      expect(_run('hl/1', (rt) => [_cell(rt, '.', 1, nil)]).$1,
          ExecutionStatus.succeeded);
    });

    test('hl([2]) fails: the element does not match', () {
      expect(_run('hl/1', (rt) => [_cell(rt, '.', 2, nil)]).$1,
          ExecutionStatus.failed);
    });

    test("hl('[|]'(1, nil)) fails: '[|]'/2 is no list cell", () {
      expect(_run('hl/1', (rt) => [_cell(rt, '[|]', 1, nil)]).$1,
          ExecutionStatus.failed);
    });

    test('hl(foo) and hl([]) fail', () {
      expect(_run('hl/1', (rt) => [_bound(rt, ConstTerm('foo'))]).$1,
          ExecutionStatus.failed);
      expect(_run('hl/1', (rt) => [_bound(rt, ConstTerm(nil))]).$1,
          ExecutionStatus.failed);
    });
  });

  test('a goal reader unbound suspends (row "Reader X1?")', () {
    expect(_run('hl/1', (rt) => [_reader(rt)]).$1, ExecutionStatus.suspended);
  });
}
