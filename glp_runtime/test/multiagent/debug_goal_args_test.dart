/// Debug: Dump goal arguments before social_graph executes
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
  test('debug goal args', () async {
    final source = File('/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    final runtime = GlpRuntime();
    
    final userChannel = createExternalChannel(runtime.heap, 'user');
    final bobChannel = createExternalChannel(runtime.heap, 'bob');
    final netChannel = createExternalChannel(runtime.heap, 'net');
    
    final friendPairs = rt.StructTerm('.', [
      rt.StructTerm(',', [rt.ConstTerm('bob'), rt.VarRef(bobChannel.outputWriterAddr)]),  // Writer addr
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
    
    // Run until we see social_graph goal appear
    for (int cycle = 1; cycle <= 10; cycle++) {
      print('\n=== Cycle $cycle ===');
      if (runtime.gq.isEmpty) break;
      
      // Peek at the goal queue
      final goals = <GoalRef>[];
      while (!runtime.gq.isEmpty) {
        goals.add(runtime.gq.dequeue()!);
      }
      
      for (final g in goals) {
        final label = program.labels.entries.where((e) => e.value == g.pc).map((e) => e.key).firstOrNull ?? 'PC${g.pc}';
        print('Goal ${g.id} at $label (PC ${g.pc})');
        
        // If this is social_graph, dump its environment
        if (label.startsWith('social_graph')) {
          print('  >>> SOCIAL_GRAPH GOAL FOUND <<<');
          final env = runtime.getGoalEnv(g.id);
          if (env != null) {
            for (final slot in env.argBySlot.keys.toList()..sort()) {
              final term = env.argBySlot[slot];
              print('  Arg slot $slot: $term');
              if (term is rt.VarRef) {
                final addr = term.addr;
                // Per irmaGLP-spec.md Section 3.2.1: use heap.isReader(addr)
                final isReaderVar = runtime.heap.isReader(addr);
                final isBound = runtime.heap.isBound(addr);
                final value = runtime.heap.getValue(addr);
                print('    -> addr=$addr, isReader=$isReaderVar, isBound=$isBound, value=$value');
                if (isReaderVar) {
                  // For local two-cell pairs, writer is at addr-1
                  final writerAddr = runtime.heap.tryWriterForReader(addr);
                  if (writerAddr != null) {
                    print('    -> writerAddr=$writerAddr');
                    final wBound = runtime.heap.isBound(writerAddr);
                    final wValue = runtime.heap.getValue(writerAddr);
                    print('    -> writer isBound=$wBound, value=$wValue');
                  }
                }
              }
            }
          }
        }
        
        runtime.gq.enqueue(g);
      }
      
      await scheduler.drainAsyncWithStatus(maxCycles: 1, debug: false);
    }
  });
}
