// Debug script to trace test3 execution and understand suspension
import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = File('/Users/udi/GLP/udi/glp/test_binding.glp').readAsStringSync();

  // Compile
  final compiler = GlpCompiler();
  final program = compiler.compile(source);

  // Print bytecode for test3
  print('=== BYTECODE FOR test3 ===');
  final test3Start = program.labels['test3/0'] ?? 0;
  for (var i = test3Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    print('$i: $op');
    if (op is NoMoreClauses || op is SuspendEnd) break;
  }
  print('');

  // Create runtime and execute with debugging
  final rt = GlpRuntime();
  final runner = BytecodeRunner(program);
  final scheduler = Scheduler(rt: rt, runners: {'test': runner});

  // Set all goals to use this program
  rt.setGoalProgram(1, 'test');
  rt.setGoalProgram(10000, 'test');
  rt.setGoalProgram(10001, 'test');

  // Execute test3
  final env = CallEnv(writers: {}, readers: {});
  final goalId = 1;
  rt.setGoalEnv(goalId, env);
  final entryPc = program.labels['test3/0']!;
  rt.gq.enqueue(GoalRef(goalId, entryPc));

  print('=== EXECUTION TRACE ===');
  scheduler.drain(maxCycles: 100, debug: true);

  print('');
  print('=== FINAL STATE ===');
  print('Goal queue empty: ${rt.gq.isEmpty}');
}
