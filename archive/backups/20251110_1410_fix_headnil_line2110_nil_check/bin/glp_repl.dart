/// GLP REPL (Read-Eval-Print Loop)
///
/// Interactive GLP interpreter
/// Run with: dart run bin/glp_repl.dart
library;

import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';

void main() {
  print('╔════════════════════════════════════════╗');
  print('║   GLP REPL - Interactive Interpreter   ║');
  print('╚════════════════════════════════════════╝');
  print('');
  print('Type GLP goals and press Enter.');
  print('Commands: :quit, :help');
  print('');

  final compiler = GlpCompiler();
  final rt = GlpRuntime();
  registerStandardPredicates(rt.systemPredicates);

  var goalId = 1;

  while (true) {
    stdout.write('GLP> ');
    final input = stdin.readLineSync();

    if (input == null) {
      break;
    }

    if (input.trim().isEmpty) {
      continue;
    }

    final trimmed = input.trim();

    // Handle commands
    if (trimmed == ':quit' || trimmed == ':q') {
      print('Goodbye!');
      break;
    }

    if (trimmed == ':help' || trimmed == ':h') {
      printHelp();
      continue;
    }

    // Compile and run the goal
    try {
      // Compile the input
      final program = compiler.compile(trimmed);

      // Create a scheduler with the program
      final runner = BytecodeRunner(program);
      final scheduler = Scheduler(rt: rt, runners: {'main': runner});

      // Set up initial goal
      final env = CallEnv();
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, 'main');

      // Enqueue the goal at PC 0
      rt.gq.enqueue(GoalRef(goalId, 0));
      goalId++;

      // Run scheduler
      final ran = scheduler.drain(maxCycles: 10000);

      // Report result
      print('→ Executed ${ran.length} goals');

    } catch (e) {
      print('Error: $e');
    }

    print('');
  }
}

void printHelp() {
  print('');
  print('GLP REPL Commands:');
  print('  :help, :h     Show this help');
  print('  :quit, :q     Exit REPL');
  print('');
  print('Examples:');
  print('  GLP> hello.');
  print('  GLP> execute(\'write\', [\'Hello World\']).');
  print('  GLP> merge([a,b], [c,d], X).');
  print('');
}
