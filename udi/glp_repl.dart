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
  print('Type GLP goals and press Enter.');
  print('Commands: :load <file>, :compile <file>, :quit, :help');
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

    if (trimmed.startsWith(':load ')) {
      final filename = trimmed.substring(6).trim();
      if (!loadProgram(filename, compiler, loadedPrograms)) {
        continue;
      }
      print('✓ Loaded: $filename');
      continue;
    }

    if (trimmed.startsWith(':compile ')) {
      final filename = trimmed.substring(9).trim();
      if (!compileProgram(filename, compiler)) {
        continue;
      }
      print('✓ Compiled: glp/$filename → bin/${filename}c');
      continue;
    }

    if (trimmed.startsWith(':list')) {
      if (loadedPrograms.isEmpty) {
        print('No programs loaded.');
      } else {
        print('Loaded programs:');
        for (final name in loadedPrograms.keys) {
          print('  - $name');
        }
      }
      continue;
    }

    // Compile and run the goal
    try {
      // Compile the input as a goal
      final goalProgram = compiler.compile(trimmed);

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

bool compileProgram(String filename, GlpCompiler compiler) {
  try {
    // Load source from glp/
    final sourceFile = File('glp/$filename');

    if (!sourceFile.existsSync()) {
      print('Error: File not found: glp/$filename');
      return false;
    }

    final source = sourceFile.readAsStringSync();
    final _program = compiler.compile(source);

    // TODO: When bytecode serialization is implemented, write to bin/
    // For now, just validate it compiles
    print('Note: Bytecode serialization not yet implemented');
    print('      Program compiled successfully but not saved to bin/');

    return true;
  } catch (e) {
    print('Error compiling $filename: $e');
    return false;
  }
}

void printHelp() {
  print('');
  print('GLP REPL Commands:');
  print('  :help, :h              Show this help');
  print('  :quit, :q              Exit REPL');
  print('  :load <file>           Load and compile glp/<file>');
  print('  :compile <file>        Compile glp/<file> to bin/<file>c');
  print('  :list                  List loaded programs');
  print('');
  print('File Organization:');
  print('  glp/           GLP source files (.glp)');
  print('  bin/           Compiled bytecode files (.glpc)');
  print('');
  print('Examples:');
  print('  GLP> :load hello.glp');
  print('  GLP> :compile hello.glp');
  print("  GLP> execute('write', ['Hello World']).");
  print('  GLP> merge([a,b], [c,d], X).');
  print('');
}
