import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('first([a|Xs], Xs?)', () {
    final compiler = GlpCompiler();
    final source = 'first([a|Xs], Xs?).';
    print('Compiling: $source');
    final program = compiler.compile(source);
    
    print('Bytecode:');
    for (var i = 0; i < program.ops.length; i++) {
      print('  $i: ${program.ops[i]}');
    }

    final rt = GlpRuntime();
    
    // Create writer for X (arg1)
    const wX = 1, rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    
    // Create list [a,b] (arg0)
    const wList = 3, rList = 4;
    rt.heap.addWriter(WriterCell(wList, rList));
    rt.heap.addReader(ReaderCell(rList));
    rt.heap.bindWriterStruct(wList, '.', [
      ConstTerm('a'),
      StructTerm('.', [ConstTerm('b'), ConstTerm(null)])
    ]);
    
    print('\nHeap before:');
    print('  wX=$wX bound=${rt.heap.isWriterBound(wX)}');
    print('  wList=$wList bound=${rt.heap.isWriterBound(wList)} value=${rt.heap.valueOfWriter(wList)}');
    
    final runner = BytecodeRunner(program);
    final sched = Scheduler(rt: rt, runner: runner);
    
    const goalId = 100;
    rt.setGoalEnv(goalId, CallEnv(readers: {0: rList}, writers: {1: wX}));
    rt.gq.enqueue(GoalRef(goalId, program.labels['first/2']!));
    
    print('\nRunning goal: first([a,b]?, X)');
    final ran = sched.drain(maxCycles: 100);
    
    print('\nHeap after:');
    print('  wX=$wX bound=${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  wX value=${rt.heap.valueOfWriter(wX)}');
    }
    print('  Goals executed: $ran');
  });
}
