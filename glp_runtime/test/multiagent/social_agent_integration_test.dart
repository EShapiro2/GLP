/// Integration test - Run until social_graph failure
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';

void main() {
  test('run until social_graph failure', () async {
    // Load social_agent.glp
    final source = File('/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp').readAsStringSync();
    
    // Compile
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    
    // Create runtime
    final runtime = GlpRuntime();
    
    // Create channels
    final userChannel = createExternalChannel(runtime.heap, 'user');
    final bobChannel = createExternalChannel(runtime.heap, 'bob');
    final netChannel = createExternalChannel(runtime.heap, 'net');
    
    print('=== Initial channel var IDs ===');
    print('  userChannel: in=${userChannel.inputVarId}, out=${userChannel.outputVarId}');
    print('  bobChannel: in=${bobChannel.inputVarId}, out=${bobChannel.outputVarId}');
    print('  netChannel: in=${netChannel.inputVarId}, out=${netChannel.outputVarId}');
    
    // Build friend pairs: [(bob, BobOut)]
    final friendPairs = rt.StructTerm('.', [
      rt.StructTerm(',', [
        rt.ConstTerm('bob'),
        rt.VarRef(bobChannel.outputVarId),  // Writer addr
      ]),
      rt.ConstTerm('nil'),
    ]);
    
    // Build goal arguments
    final args = <int, rt.Term>{
      0: rt.ConstTerm('alice'),
      1: friendPairs,
      2: buildChannelTerm(userChannel),
      3: buildChannelTerm(netChannel),
    };
    
    // Set up goal
    final goalId = 1;
    runtime.setGoalEnv(goalId, CallEnv(args: args));
    runtime.setGoalProgram(goalId, 'main');
    
    // Create scheduler
    final runner = BytecodeRunner(program);
    final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
    
    // Enqueue goal
    final entryPC = program.labels['agent/4'];
    runtime.gq.enqueue(GoalRef(goalId, entryPC!));
    
    // Run one cycle at a time until failure or 20 cycles
    print('\n=== Running one cycle at a time ===');
    for (int cycle = 1; cycle <= 20; cycle++) {
      print('\n--- Cycle $cycle ---');
      print('GQ length before: ${runtime.gq.length}');
      
      if (runtime.gq.isEmpty) {
        print('GQ empty, stopping');
        break;
      }
      
      var result = await scheduler.drainAsyncWithStatus(maxCycles: 1, debug: true);
      print('Status: ${result.status}');
      
      if (result.status == ExecutionStatus.failed) {
        print('\n!!! FAILURE DETECTED !!!');
        // Heap debug info removed - allVarIds not available in pointer architecture
        break;
      }
    }
  });
}
