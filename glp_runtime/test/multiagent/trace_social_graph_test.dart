/// Debug: Trace bytecode execution for social_graph
import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:test/test.dart';

void main() {
  test('trace social_graph bytecode', () async {
    final source = File('/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    final runtime = GlpRuntime();
    
    // Print bytecode around social_graph/3
    final sgPC = program.labels['social_graph/3']!;
    print('=== Bytecode for social_graph/3 (starting at PC $sgPC) ===');
    for (int i = sgPC; i < sgPC + 80 && i < program.ops.length; i++) {
      print('  $i: ${program.ops[i]}');
    }
    
    final userChannel = createExternalChannel(runtime.heap, 'user');
    final bobChannel = createExternalChannel(runtime.heap, 'bob');
    final netChannel = createExternalChannel(runtime.heap, 'net');
    
    final friendPairs = rt.StructTerm('.', [
      rt.StructTerm(',', [rt.ConstTerm('bob'), rt.VarRef(bobChannel.outputVarId)]),  // Writer addr
      rt.ConstTerm('nil'),
    ]);
    
    final args = <int, rt.Term>{
      0: rt.ConstTerm('alice'),
      1: friendPairs,
      2: buildChannelTerm(userChannel),
      3: buildChannelTerm(netChannel),
    };
    
    final goalId = 1;
    runtime.setGoalEnv(goalId, CallEnv(args: args));
    runtime.setGoalProgram(goalId, 'main');
    
    final runner = BytecodeRunner(program);
    final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
    
    final entryPC = program.labels['agent/4'];
    runtime.gq.enqueue(GoalRef(goalId, entryPC!));
    
    // Run 3 cycles to get past agent, merge, build_friends
    print('\n=== Running first 3 cycles ===');
    await scheduler.drainAsyncWithStatus(maxCycles: 3, debug: false);
    
    // Now run social_graph with full debug
    print('\n=== Running social_graph with debug ===');
    await scheduler.drainAsyncWithStatus(maxCycles: 1, debug: true);
  });
}
