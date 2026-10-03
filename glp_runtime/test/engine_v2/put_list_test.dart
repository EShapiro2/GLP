/// `put_list` (0x33) builds a list cell, the structure `'.'/2`.
///
/// IGLP code-format-fragment.tex, "Terms": "Lists are structures: a cell is
/// the structure '.'/2; the empty list is the constant nil" (GLP #3 Cowork,
/// 2026-10-02 20:58 UTC, answering Integration's 20:10 UTC D: "yes, put_list
/// builds '.'").  Until 2026-10-02 it built a `'[|]'` cell, which no head
/// matches as a list, no printer shows as one and the encoding carries as a
/// structure of that name.
///
/// No compiler of this tree emits put_list, a list in a body compiling to
/// put_structure `'.'/2`; an artefact may carry it.  So the program here is
/// assembled by hand, encoded as an artefact and decoded from its bytes
/// ([codeImageFromProgram]), and run from the decoded code by the byte runner,
/// as head_list_test.dart runs head_list.
library;

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart'
    show BytecodeProgram, CallEnv;
import 'package:glp_runtime/engine_v2/code_image.dart' show CodeImage;
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/instruction_codec.dart' show decodeCode;
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

/// `pl(W)`: put_list at argument 0, its head the constant 1 and its tail nil,
/// so W = [1]; `pt(W, T)`: the same cell with the reader of a fresh variable
/// for its tail, the variable's writer given to T, so W = [1 | T?]; and
/// `hl([1])`, head_list's test of a cell, `'.'(1, nil)` and nothing else.
final CodeImage _image = codeImageFromProgram(BytecodeProgram([
  op.Label('pl/1'),
  op.ClauseTry(),
  op.Commit(),
  op.PutList(0),
  op.SetConstant(1),
  op.SetConstant('nil'),
  op.Proceed(),
  op.Label('pl/1_end'),
  op.NoMoreClauses(),
  op.Label('hl/1'),
  op.ClauseTry(),
  op.HeadList(0),
  op.UnifyConstant(1),
  op.UnifyConstant('nil'),
  op.Commit(),
  op.Proceed(),
  op.Label('hl/1_end'),
  op.NoMoreClauses(),
]));

/// Post each goal [goals] gives, in order, run them all from the decoded code,
/// and give the status, each goal's arguments, and the runtime.
(ExecutionStatus, List<List<Term>>, GlpRuntime) _run(
  List<(String, List<Term> Function(GlpRuntime rt))> goals,
) {
  final rt = GlpRuntime();
  final sched = Scheduler(rt: rt, runner: ByteRunner(_image));
  final posted = <List<Term>>[];
  for (final (proc, argsOf) in goals) {
    final args = argsOf(rt);
    posted.add(args);
    final id = rt.nextGoalId++;
    rt.setGoalEnv(
      id,
      CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}),
    );
    rt.gq.enqueue(GoalRef(id, _image.entryOffsetOf(proc)!));
  }
  return (sched.drainWithStatus().status, posted, rt);
}

void main() {
  test('the decoded code holds put_list, as the artefact carries it', () {
    final ops = decodeCode(_image.code,
        procNameOf: (i) => _image.symbolAt(i).signature);
    expect(ops.whereType<op.PutList>().map((o) => o.argSlot), [0]);
  });

  test("pl(W) gives W = [1], the cell '.'/2", () {
    final (status, args, rt) = _run([
      ('pl/1', (rt) => [VarRef(rt.heap.allocateVariable().$1)]),
    ]);
    expect(status, ExecutionStatus.succeeded);
    final w = rt.heap.dereference(args.single.single);
    expect(w, isA<StructTerm>());
    final cell = w as StructTerm;
    expect(cell.functor, '.');
    expect(cell.args, hasLength(2));
    expect((rt.heap.dereference(cell.args[0]) as ConstTerm).value, 1);
    expect((rt.heap.dereference(cell.args[1]) as ConstTerm).value, 'nil');
  });

  test('the cell put_list builds is the list head_list matches: pl(W), '
      'hl(W?) succeeds', () {
    late HeapCell reader;
    final (status, _, _) = _run([
      (
        'pl/1',
        (rt) {
          final (w, r) = rt.heap.allocateVariable();
          reader = r;
          return [VarRef(w)];
        }
      ),
      ('hl/1', (rt) => [VarRef(reader)]),
    ]);
    expect(status, ExecutionStatus.succeeded,
        reason: "hl([1]) matches '.'(1, nil); a '[|]' cell fails it");
  });

  test('the cell is encoded as the list it is', () {
    final (_, args, rt) = _run([
      ('pl/1', (rt) => [VarRef(rt.heap.allocateVariable().$1)]),
    ]);
    final cell = rt.heap.dereference(args.single.single) as StructTerm;
    final ground = StructTerm(cell.functor, [
      rt.heap.dereference(cell.args[0]),
      rt.heap.dereference(cell.args[1]),
    ]);
    expect(PayloadCodec.serializeAgentMessage(ground),
        PayloadCodec.serializeAgentMessage(
            StructTerm('.', [ConstTerm(1), ConstTerm('nil')])));
  });
}
