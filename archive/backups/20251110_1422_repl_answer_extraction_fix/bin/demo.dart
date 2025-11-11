import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/cells.dart';

BytecodeProgram compilePWriter(int w) => BytecodeProgram([
  Label('Cw'),
  ClauseTry(),
  HeadBindWriter(w),
  Commit(),
  BodySetConst(w, 'a'),
  Proceed(),
]);

BytecodeProgram compilePReader(int r) => BytecodeProgram([
  Label('Cr'),
  ClauseTry(),
  GuardNeedReader(r),
  UnionSiAndGoto('END'),
  Label('END'),
  SuspendEnd(),
]);

void printActivation(GoalRef a) {
  print('ACTIVATED: goal ${a.id}@${a.pc}');
}

void runGoal({required GlpRuntime rt, required int goalId, required int kappa, required BytecodeProgram prog}) {
  final runner = BytecodeRunner(prog);
  final res = runner.runWithStatus(RunnerContext(rt: rt, goalId: goalId, kappa: kappa));
  switch (res) {
    case RunResult.suspended:
      print('SUSPENDED: goal $goalId@$kappa');
      break;
    case RunResult.terminated:
      print('TERMINATED: goal $goalId');
      break;
    case RunResult.yielded:
      print('YIELDED: goal $goalId');
      break;
  }
}

void main(List<String> args) {
  final rt = GlpRuntime();

  if (args.isEmpty) {
    print('Usage: dart run bin/demo.dart [writer|reader|mix]');
    return;
  }

  if (args[0] == 'writer') {
    const W = 1, R = 1001;
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));
    runGoal(rt: rt, goalId: 1, kappa: 1, prog: compilePWriter(W));
    return;
  }

  if (args[0] == 'reader') {
    const W = 2, R = 2002;
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));
    runGoal(rt: rt, goalId: 2, kappa: 1, prog: compilePReader(R));
    return;
  }

  if (args[0] == 'mix') {
    const W = 3, R = 3003;
    rt.heap.addWriter(WriterCell(W, R));
    rt.heap.addReader(ReaderCell(R));

    // First: reader call suspends
    runGoal(rt: rt, goalId: 10, kappa: 1, prog: compilePReader(R));

    // Then: writer call binds to 'a' and enqueues activation(s) for the suspended goal
    runGoal(rt: rt, goalId: 20, kappa: 1, prog: compilePWriter(W));

    // Drain activations in FIFO
    while (rt.gq.length > 0) {
      final a = rt.gq.dequeue();
      if (a == null) break;
      printActivation(a);
      // Re-run the awakened goal from kappa using the same predicate for reader mode
      runGoal(rt: rt, goalId: a.id, kappa: a.pc, prog: compilePReader(R));
    }
    return;
  }

  print('Unknown mode: ${args[0]}');
}
