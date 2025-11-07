/// GLP REPL (Read-Eval-Print Loop) for udi workspace
///
/// Interactive GLP interpreter with file loading support
/// Run from /Users/udi/GLP/udi with: dart run glp_repl.dart
library;

import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

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
      // Parse the query goal to extract functor, arguments, and variables
      final lexer = Lexer(trimmed);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final ast = parser.parse();

      if (ast.procedures.isEmpty) {
        print('Error: No goal found');
        continue;
      }

      final proc = ast.procedures[0];
      if (proc.clauses.isEmpty) {
        print('Error: No clauses in goal');
        continue;
      }

      final goalClause = proc.clauses[0];
      final goalAtom = goalClause.head;
      final functor = goalAtom.functor;
      final arity = goalAtom.arity;
      final args = goalAtom.args;

      // Combine all loaded programs
      final allOps = <Op>[];
      for (final loaded in loadedPrograms.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      // Find the entry point for the predicate
      final procedureLabel = '$functor/$arity';
      final entryPC = combinedProgram.labels[procedureLabel];
      if (entryPC == null) {
        print('Error: Predicate $procedureLabel not found');
        print('Available predicates: ${combinedProgram.labels.keys.where((k) => !k.endsWith('_end') && !k.contains('_c')).join(', ')}');
        continue;
      }

      // Set up heap cells for each argument and track variable writers
      final queryVarWriters = <String, int>{};
      final readers = <int, int>{};
      final writers = <int, int>{};

      for (int i = 0; i < args.length; i++) {
        final arg = args[i];
        final (readerId, writerId) = _setupArgument(rt, arg, i, readers, writers, queryVarWriters);
      }

      // Set up goal environment
      final env = CallEnv(readers: readers, writers: writers);
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, 'main');

      // Create scheduler and run
      final runner = BytecodeRunner(combinedProgram);
      final scheduler = Scheduler(rt: rt, runners: {'main': runner});

      rt.gq.enqueue(GoalRef(goalId, entryPC));
      final currentGoalId = goalId;
      goalId++;

      final ran = scheduler.drain(maxCycles: 10000);

      // Report result
      print('→ Executed ${ran.length} goals');

      // Display variable bindings
      if (queryVarWriters.isNotEmpty) {
        print('');
        for (final entry in queryVarWriters.entries) {
          final varName = entry.key;
          final writerId = entry.value;
          if (rt.heap.isWriterBound(writerId)) {
            final value = rt.heap.valueOfWriter(writerId);
            print('  $varName = ${_formatTerm(value, rt)}');
          } else {
            print('  $varName = <unbound>');
          }
        }
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

// Set up heap cells for a query argument
// Returns (readerId, writerId) for the argument slot
(int, int) _setupArgument(
  GlpRuntime runtime,
  Term arg,
  int argSlot,
  Map<int, int> readers,
  Map<int, int> writers,
  Map<String, int> queryVarWriters,
) {
  if (arg is VarTerm) {
    // Variable: create fresh writer/reader pair
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.addWriter(WriterCell(writerId, readerId));
    runtime.heap.addReader(ReaderCell(readerId));

    // Track this variable for later display
    if (!arg.isReader) {
      queryVarWriters[arg.name] = writerId;
    }

    // Map to argument slot
    if (arg.isReader) {
      readers[argSlot] = readerId;
    } else {
      writers[argSlot] = writerId;
    }

    return (readerId, writerId);
  } else if (arg is ListTerm) {
    // List: create structure and bind it
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.addWriter(WriterCell(writerId, readerId));
    runtime.heap.addReader(ReaderCell(readerId));

    // Build list structure recursively
    final listValue = _buildListTerm(runtime, arg, queryVarWriters);
    runtime.heap.writerValue[writerId] = listValue;

    // Always use reader for pre-bound values
    readers[argSlot] = readerId;
    return (readerId, writerId);
  } else if (arg is ConstTerm) {
    // Constant: create bound writer/reader
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.addWriter(WriterCell(writerId, readerId));
    runtime.heap.addReader(ReaderCell(readerId));
    runtime.heap.writerValue[writerId] = rt.ConstTerm(arg.value);

    readers[argSlot] = readerId;
    return (readerId, writerId);
  } else {
    throw Exception('Unsupported argument type: ${arg.runtimeType}');
  }
}

// Build a list term recursively
rt.Term _buildListTerm(GlpRuntime runtime, ListTerm list, Map<String, int> queryVarWriters) {
  if (list.isNil) {
    return rt.ConstTerm(null);  // Empty list
  }

  final head = list.head;
  final tail = list.tail;

  // Convert head to runtime term
  rt.Term headTerm;
  if (head is ConstTerm) {
    headTerm = rt.ConstTerm(head.value);
  } else if (head is VarTerm) {
    // Variable in list - create writer/reader
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.addWriter(WriterCell(writerId, readerId));
    runtime.heap.addReader(ReaderCell(readerId));
    if (!head.isReader) {
      queryVarWriters[head.name] = writerId;
    }
    headTerm = head.isReader ? rt.ReaderTerm(readerId) : rt.WriterTerm(writerId);
  } else {
    throw Exception('Unsupported list head type: ${head.runtimeType}');
  }

  // Convert tail to runtime term
  rt.Term tailTerm;
  if (tail is ListTerm) {
    tailTerm = _buildListTerm(runtime, tail, queryVarWriters);
  } else {
    tailTerm = rt.ConstTerm(null);
  }

  return rt.StructTerm('.', [headTerm, tailTerm]);
}


String _formatTerm(rt.Term? term, [GlpRuntime? runtime, Set<int>? visited]) {
  if (term == null) return '[]';

  visited ??= <int>{};

  if (term is rt.ConstTerm) {
    if (term.value == null) return '[]';
    return term.value.toString();
  }

  if (term is rt.StructTerm && term.functor == '.' && term.args.length == 2) {
    // List cons cell - expand to readable format
    final elements = <String>[];
    rt.Term? current = term;

    while (true) {
      if (current is! rt.StructTerm || current.functor != '.') break;

      final head = current.args[0];
      final tail = current.args[1];

      // Format head element
      String headStr;
      if (head is rt.ReaderTerm && runtime != null) {
        // Dereference reader
        final readerId = head.readerId;
        if (visited.contains(readerId)) {
          headStr = '<circular>';
        } else {
          visited.add(readerId);
          final writer = _findPairedWriter(runtime, readerId);
          if (writer != null && runtime.heap.isWriterBound(writer)) {
            final value = runtime.heap.valueOfWriter(writer);
            headStr = _formatTerm(value, runtime, visited);
          } else {
            headStr = 'R$readerId';
          }
        }
      } else {
        headStr = _formatTerm(head, runtime, visited);
      }

      elements.add(headStr);

      // Move to tail
      if (tail is rt.ReaderTerm && runtime != null) {
        final readerId = tail.readerId;
        if (visited.contains(readerId)) break;
        visited.add(readerId);
        final writer = _findPairedWriter(runtime, readerId);
        if (writer != null && runtime.heap.isWriterBound(writer)) {
          current = runtime.heap.valueOfWriter(writer);
          if (current == null || current is! rt.StructTerm) break;
        } else {
          break;
        }
      } else if (tail is rt.ConstTerm && tail.value == null) {
        break;  // End of list
      } else {
        break;
      }
    }

    return '[${elements.join(', ')}]';
  }

  return term.toString();
}

int? _findPairedWriter(GlpRuntime runtime, int readerId) {
  // Find writer paired with this reader
  for (final entry in runtime.heap.writers.entries) {
    final writerId = entry.key;
    final writerCell = entry.value;
    if (writerCell.readerId == readerId) {
      return writerId;
    }
  }
  return null;
}
