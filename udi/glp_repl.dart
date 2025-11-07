/// GLP REPL (Read-Eval-Print Loop) for udi workspace
///
/// Interactive GLP interpreter with file loading support
/// Run from /Users/udi/GLP/udi with: dart run glp_repl.dart
library;

import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';

void main() {
  print('╔════════════════════════════════════════╗');
  print('║   GLP REPL - Interactive Interpreter   ║');
  print('╚════════════════════════════════════════╝');
  print('');
  print('Working directory: udi/');
  print('Source files: glp/*.glp');
  print('Compiled files: bin/*.glpc');
  print('');
  print('Input: filename.glp to load, or goal. to execute');
  print('Commands: :quit, :help');
  print('');

  final compiler = GlpCompiler();
  final rt = GlpRuntime();
  registerStandardPredicates(rt.systemPredicates);

  // Track loaded programs
  final loadedPrograms = <String, BytecodeProgram>{};

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

    // Check if input is a .glp file to load
    if (trimmed.endsWith('.glp') && !trimmed.contains(' ')) {
      final filename = trimmed;
      if (!loadProgram(filename, compiler, loadedPrograms)) {
        continue;
      }
      print('✓ Loaded: $filename');
      continue;
    }

    // Compile and run the goal
    try {
      // Parse the goal to extract functor and arguments
      // For now, handle simple cases like "hello." or "merge(..., Xs)."

      // Compile as a wrapper that spawns the goal
      // query_wrapper() :- merge([1,2,3], [a,b], Xs).
      final wrappedQuery = 'query__wrapper() :- $trimmed';

      final goalResult = compiler.compileWithMetadata(wrappedQuery);
      final goalProgram = goalResult.program;
      final variableMap = goalResult.variableMap;

      // Combine loaded programs with the goal
      final allOps = <Op>[];

      // Add all loaded programs first
      for (final loaded in loadedPrograms.values) {
        allOps.addAll(loaded.ops);
      }

      // Add the goal program
      allOps.addAll(goalProgram.ops);

      // Create combined program
      final combinedProgram = BytecodeProgram(allOps);

      // Create a scheduler with the combined program
      final runner = BytecodeRunner(combinedProgram);
      final scheduler = Scheduler(rt: rt, runners: {'main': runner});

      // Set up initial goal
      final env = CallEnv();
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, 'main');

      // Track heap size before execution to find new writers
      final heapSizeBefore = rt.heap.writers.length;

      // Find the entry point for query__wrapper/0
      final entryPC = combinedProgram.labels['query__wrapper/0'];
      if (entryPC == null) {
        print('Error: Could not find query wrapper entry point');
        continue;
      }

      // Enqueue the goal at the wrapper entry point
      rt.gq.enqueue(GoalRef(goalId, entryPC));
      final currentGoalId = goalId;
      goalId++;

      // Run scheduler
      final ran = scheduler.drain(maxCycles: 10000);

      // Report result
      print('→ Executed ${ran.length} goals: $ran');

      // After execution, find which writers were created and bound
      // These correspond to query variables
      final finalEnv = rt.getGoalEnv(currentGoalId);

      // Strategy: For each variable in the query, find recently created bound writers
      if (variableMap.isNotEmpty) {
        _displayBindingsFromHeap(rt, variableMap, heapSizeBefore);
      }

    } catch (e) {
      print('Error: $e');
    }

    print('');
  }
}

bool loadProgram(String filename, GlpCompiler compiler, Map<String, BytecodeProgram> loadedPrograms) {
  try {
    // Try to load source file from glp/
    final sourceFile = File('glp/$filename');

    if (!sourceFile.existsSync()) {
      print('Error: File not found: glp/$filename');
      return false;
    }

    final source = sourceFile.readAsStringSync();
    final program = compiler.compile(source);

    loadedPrograms[filename] = program;
    return true;
  } catch (e) {
    print('Error loading $filename: $e');
    return false;
  }
}


void printHelp() {
  print('');
  print('GLP REPL Usage:');
  print('  filename.glp           Load and compile glp/<filename>');
  print('  goal.                  Execute a goal (must end with .)');
  print('  :help, :h              Show this help');
  print('  :quit, :q              Exit REPL');
  print('');
  print('File Organization:');
  print('  glp/           GLP source files (.glp)');
  print('  bin/           Compiled bytecode files (.glpc)');
  print('');
  print('Examples:');
  print('  GLP> hello.glp                        # Load program');
  print('  GLP> hello.                           # Execute goal');
  print("  GLP> execute('write', ['Hello']).");
  print('  GLP> merge([1,2,3], [a,b], Xs).');
  print('');
}

void _displayBindingsFromHeap(GlpRuntime rt, Map<String, int> variableMap, int heapSizeBefore) {
  // Strategy: Find writers that were created during query execution and are bound

  print('');
  print('DEBUG: Total bound writers in heap: ${rt.heap.writerValue.length}');
  print('DEBUG: Heap size before: $heapSizeBefore, after: ${rt.heap.writers.length}');

  // Show all bound writers
  print('DEBUG: All bound writers:');
  for (final entry in rt.heap.writerValue.entries) {
    print('  W${entry.key} = ${_formatTerm(entry.value)}');
  }

  // For now, just show the most recently bound writer
  // This is a simple heuristic for queries with one output variable
  if (variableMap.length == 1) {
    final varName = variableMap.keys.first;

    // Find the most recently bound writer
    int? lastBoundWriter;
    for (final entry in rt.heap.writerValue.entries) {
      final writerId = entry.key;
      if (writerId >= 1000) {  // Fresh writers start at 1000
        lastBoundWriter = writerId;
      }
    }

    if (lastBoundWriter != null && rt.heap.writerValue.containsKey(lastBoundWriter)) {
      final value = rt.heap.writerValue[lastBoundWriter];
      print('  $varName = ${_formatTerm(value)}');
    } else {
      print('  $varName = <unbound>');
    }
  } else {
    // Multiple variables - show all bound writers created during execution
    for (final entry in variableMap.entries) {
      final varName = entry.key;
      print('  $varName = <multiple variables not yet supported>');
    }
  }
}

String _formatTerm(Object? term) {
  if (term == null) return '[]';

  // Use the term's built-in toString()
  // This will properly format ConstTerm, WriterTerm, ReaderTerm, StructTerm, etc.
  return term.toString();
}
