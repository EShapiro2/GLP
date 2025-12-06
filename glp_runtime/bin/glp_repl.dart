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

void main() async {
  // Get git commit info
  final gitCommit = await _getGitCommit();
  // Build timestamp (updated at compile time)
  final buildTime = '2025-12-05 (Added conjunctive goals support)';

  print('╔════════════════════════════════════════╗');
  print('║   GLP REPL - Interactive Interpreter   ║');
  print('╚════════════════════════════════════════╝');
  print('');
  if (gitCommit != null) {
    print('Build: $gitCommit');
  }
  print('Compiled: $buildTime');
  print('Working directory: udi/');
  print('Source files: glp/*.glp');
  print('Compiled files: bin/*.glpc');
  print('');
  print('Input: filename.glp to load, or goal to execute');
  print('Commands: :quit, :help, :trace, :debug, :limit');
  print('');

  final compiler = GlpCompiler();
  final runtime = GlpRuntime();
  registerStandardPredicates(runtime.systemPredicates);

  // Track loaded programs
  final loadedPrograms = <String, BytecodeProgram>{};

  // Load stdlib files for system predicates
  final stdlibFiles = ['assign.glp', 'univ.glp', 'unify.glp'];
  for (final filename in stdlibFiles) {
    final stdlibPath = '../glp/stdlib/$filename';
    final stdlibFile = File(stdlibPath);
    if (stdlibFile.existsSync()) {
      try {
        final stdlibSource = stdlibFile.readAsStringSync();
        final stdlibCompiler = GlpCompiler();
        final stdlibProg = stdlibCompiler.compile(stdlibSource);
        loadedPrograms['__stdlib_${filename}__'] = stdlibProg;
        print('Loaded stdlib: $filename (${stdlibProg.ops.length} ops)');
      } catch (e) {
        print('Warning: Could not load stdlib $filename: $e');
      }
    } else {
      print('Warning: stdlib not found at $stdlibPath');
    }
  }

  var goalId = 1;
  var debugTrace = true; // Toggle with :trace command
  var debugOutput = false; // Toggle with :debug command
  var maxCycles = 10000; // Set with :limit command

  while (true) {
    stdout.write('GLP> ');
    final input = stdin.readLineSync();

    if (input == null) {
      break;
    }

    if (input.trim().isEmpty) {
      continue;
    }

    // Strip trailing . if present (allow goals without .)
    var trimmed = input.trim();
    if (trimmed.endsWith('.') && !trimmed.endsWith('.glp')) {
      trimmed = trimmed.substring(0, trimmed.length - 1).trim();
    }

    // Handle commands
    if (trimmed == ':quit' || trimmed == ':q') {
      print('Goodbye!');
      break;
    }

    if (trimmed == ':help' || trimmed == ':h') {
      printHelp();
      continue;
    }

    if (trimmed == ':trace' || trimmed == ':t') {
      debugTrace = !debugTrace;
      print('Trace ${debugTrace ? "enabled" : "disabled"}');
      continue;
    }

    if (trimmed == ':debug' || trimmed == ':d') {
      debugOutput = !debugOutput;
      print('Debug output ${debugOutput ? "enabled" : "disabled"}');
      continue;
    }

    if (trimmed.startsWith(':limit')) {
      final parts = trimmed.split(RegExp(r'\s+'));
      if (parts.length != 2) {
        print('Usage: :limit <number>');
        continue;
      }
      final limit = int.tryParse(parts[1]);
      if (limit == null || limit <= 0) {
        print('Error: limit must be a positive integer');
        continue;
      }
      maxCycles = limit;
      print('Goal reduction limit set to $maxCycles');
      continue;
    }

    if (trimmed.startsWith(':bytecode') || trimmed.startsWith(':bc')) {
      // Display bytecode for loaded programs
      if (loadedPrograms.isEmpty) {
        print('No programs loaded');
        continue;
      }
      for (final entry in loadedPrograms.entries) {
        print('\nBytecode for ${entry.key}:');
        print('=' * 60);
        final prog = entry.value;
        for (int i = 0; i < prog.ops.length; i++) {
          print('  ${i.toString().padLeft(4)}: ${prog.ops[i]}');
        }
      }
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
      // Check if this is a conjunction (contains comma outside of argument lists)
      // For conjunctions, parse as a term and execute each goal in sequence
      if (_isConjunction(trimmed)) {
        // Parse the conjunction by wrapping it as a clause body:
        // _conj_wrapper_ :- <conjunction>.
        final parseInput = '_conj_wrapper_ :- $trimmed.';
        final lexer = Lexer(parseInput);
        final tokens = lexer.tokenize();
        final parser = Parser(tokens);
        final ast = parser.parse();

        if (ast.procedures.isEmpty || ast.procedures[0].clauses.isEmpty) {
          print('Error: Could not parse conjunction');
          continue;
        }

        // Get the body goals from the parsed clause
        final clause = ast.procedures[0].clauses[0];
        if (clause.body == null || clause.body!.isEmpty) {
          print('Error: No goals in conjunction');
          continue;
        }

        // Convert Goals to Atoms for uniform handling
        final goals = clause.body!.map((g) => Atom(g.functor, g.args, g.line, g.column)).toList();

        if (debugOutput) {
          print('[DEBUG] Conjunction goals: ${goals.length}');
          for (final g in goals) {
            print('[DEBUG]   ${g.functor}/${g.args.length}');
          }
        }

        // Combine all loaded programs
        final allOps = <dynamic>[];
        for (final loaded in loadedPrograms.values) {
          allOps.addAll(loaded.ops);
        }
        final combinedProgram = BytecodeProgram(allOps);

        // Track variables across goals
        final queryVarWriters = <String, int>{};
        final varNameToId = <String, int>{}; // Map variable names to their IDs

        // Create runner and scheduler
        final runner = BytecodeRunner(combinedProgram);
        final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
        scheduler.resetDisplayNumbering();

        var allSucceeded = true;
        var anySuspended = false;

        // Execute each goal in sequence
        for (final goal in goals) {
          final functor = goal.functor;
          final arity = goal.args.length;
          final args = goal.args;

          // Find entry point for this goal
          final procedureLabel = '$functor/$arity';
          final entryPC = combinedProgram.labels[procedureLabel];
          if (entryPC == null) {
            print('Error: Predicate $procedureLabel not found');
            allSucceeded = false;
            break;
          }

          // Set up arguments, reusing existing variables where needed
          final argSlots = <int, rt.Term>{};
          for (int i = 0; i < args.length; i++) {
            final arg = args[i];
            _setupConjunctionArg(runtime, arg, i, argSlots, queryVarWriters, varNameToId, debugOutput: debugOutput);
          }

          // Set up goal environment
          final env = CallEnv(args: argSlots);
          runtime.setGoalEnv(goalId, env);
          runtime.setGoalProgram(goalId, 'main');

          // Set query variable names for display
          scheduler.setQueryVarNames(queryVarWriters);

          runtime.gq.enqueue(GoalRef(goalId, entryPC));
          goalId++;

          // Run until this goal completes
          final result = await scheduler.drainAsyncWithStatus(
            maxCycles: maxCycles,
            debug: debugTrace,
            showBindings: false,
            debugOutput: debugOutput,
          );

          if (result.status == ExecutionStatus.failed) {
            allSucceeded = false;
            break;
          } else if (result.status == ExecutionStatus.suspended) {
            anySuspended = true;
            // Continue with next goal - may unblock
          }
        }

        // Display variable bindings
        if (debugOutput) print('[DEBUG REPL] queryVarWriters = $queryVarWriters');
        if (queryVarWriters.isNotEmpty) {
          for (final entry in queryVarWriters.entries) {
            final varName = entry.key;
            final writerId = entry.value;
            if (debugOutput) print('[DEBUG] $varName (W$writerId), isBound=${runtime.heap.isBound(writerId)}');
            if (runtime.heap.isBound(writerId)) {
              final varRef = rt.VarRef(writerId, isReader: false);
              final derefValue = runtime.heap.dereference(varRef);
              print('$varName = ${_formatTerm(derefValue, runtime)}');
            } else {
              print('$varName = <unbound>');
            }
          }
        }

        // Print final status
        if (!allSucceeded) {
          _printStatus(ExecutionStatus.failed);
        } else if (anySuspended) {
          _printStatus(ExecutionStatus.suspended);
        } else {
          _printStatus(ExecutionStatus.succeeded);
        }
        continue;
      }

      // Single goal - parse directly
      // Add . back for parser (parser requires it)
      final parseInput = trimmed.endsWith('.') ? trimmed : '$trimmed.';
      final lexer = Lexer(parseInput);
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
      final allOps = <dynamic>[];
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
      final argSlots = <int, rt.Term>{};

      for (int i = 0; i < args.length; i++) {
        final arg = args[i];
        _setupArgument(runtime, arg, i, argSlots, queryVarWriters, debugOutput: debugOutput);
      }

      // Set up goal environment
      final env = CallEnv(args: argSlots);
      runtime.setGoalEnv(goalId, env);
      runtime.setGoalProgram(goalId, 'main');

      // Create scheduler and run
      final runner = BytecodeRunner(combinedProgram);
      final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

      // Reset variable numbering and set query variable names
      scheduler.resetDisplayNumbering();
      scheduler.setQueryVarNames(queryVarWriters);

      runtime.gq.enqueue(GoalRef(goalId, entryPC));
      final currentGoalId = goalId;
      goalId++;

      final result = await scheduler.drainAsyncWithStatus(
        maxCycles: maxCycles,
        debug: debugTrace,
        showBindings: false,  // Don't show internal bindings
        debugOutput: debugOutput,
      );

      // Display variable bindings using clean format
      if (debugOutput) print('[DEBUG REPL] queryVarWriters = $queryVarWriters');
      if (queryVarWriters.isNotEmpty) {
        for (final entry in queryVarWriters.entries) {
          final varName = entry.key;
          final writerId = entry.value;
          // Use single-ID heap methods (writerId == readerId in single-ID system)
          final rawValue = runtime.heap.getValue(writerId);
          if (debugOutput) print('[DEBUG] $varName (W$writerId), isBound=${runtime.heap.isBound(writerId)}, rawValue=$rawValue');
          if (runtime.heap.isBound(writerId)) {
            // Dereference the value to follow binding chains (e.g., X2 → X2? → [])
            final varRef = rt.VarRef(writerId, isReader: false);
            final derefValue = runtime.heap.dereference(varRef);
            print('$varName = ${_formatTerm(derefValue, runtime)}');
          } else {
            print('$varName = <unbound>');
          }
        }
      }

      // Print final status
      _printStatus(result.status);

    } catch (e) {
      print('Error: $e');
    }

    print('');
  }
}

/// Print execution status
void _printStatus(ExecutionStatus status) {
  switch (status) {
    case ExecutionStatus.succeeded:
      print('→ succeeds');
    case ExecutionStatus.failed:
      print('→ failed');
    case ExecutionStatus.suspended:
      print('→ suspended');
  }
}

bool loadProgram(String filename, GlpCompiler compiler, Map<String, BytecodeProgram> loadedPrograms) {
  try {
    // Support absolute paths or relative paths in glp/ subdirectory
    final File sourceFile;
    if (filename.startsWith('/')) {
      // Absolute path - use directly
      sourceFile = File(filename);
    } else {
      // Relative path - look in glp/ subdirectory
      sourceFile = File('glp/$filename');
    }

    if (!sourceFile.existsSync()) {
      print('Error: File not found: ${sourceFile.path}');
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
  print('');
  print('Commands:');
  print('  :help, :h              Show this help');
  print('  :quit, :q              Exit REPL');
  print('  :trace, :t             Toggle trace output (reductions)');
  print('  :debug, :d             Toggle DEBUG output');
  print('  :limit <n>             Set goal reduction limit to <n>');
  print('  :bytecode, :bc         Show loaded bytecode');
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
  print('  GLP> :limit 5000                      # Set reduction limit');
  print('');
}

// Check if query contains conjunction (comma outside of argument lists/structures)
bool _isConjunction(String query) {
  int depth = 0;  // Track parentheses/bracket depth
  for (int i = 0; i < query.length; i++) {
    final char = query[i];
    if (char == '(' || char == '[') {
      depth++;
    } else if (char == ')' || char == ']') {
      depth--;
    } else if (char == ',' && depth == 0) {
      // Comma at top level - this is a conjunction
      return true;
    }
  }
  return false;
}

// Flatten a conjunction term into a list of goals
// (A, (B, C)) -> [A, B, C]
List<Atom> _flattenConjunction(Atom term) {
  final goals = <Atom>[];

  void flatten(dynamic t) {
    if (t is StructTerm && t.functor == ',' && t.args.length == 2) {
      flatten(t.args[0]);
      flatten(t.args[1]);
    } else if (t is Atom) {
      goals.add(t);
    } else if (t is StructTerm) {
      // Convert StructTerm to Atom
      goals.add(Atom(t.functor, t.args, t.line, t.column));
    }
  }

  // Start with the atom itself - check if it's a conjunction
  if (term.functor == ',' && term.args.length == 2) {
    flatten(term.args[0]);
    flatten(term.args[1]);
  } else {
    goals.add(term);
  }

  return goals;
}

// Set up an argument for a conjunction goal, reusing existing variables
void _setupConjunctionArg(
  GlpRuntime runtime,
  Term arg,
  int argSlot,
  Map<int, rt.Term> argSlots,
  Map<String, int> queryVarWriters,
  Map<String, int> varNameToId, {
  bool debugOutput = false,
}) {
  if (arg is VarTerm) {
    final baseName = arg.name;
    final existingId = varNameToId[baseName];

    if (existingId != null) {
      // Reuse existing variable
      if (debugOutput) print('[DEBUG] Reusing var $baseName: ${arg.isReader ? "R" : "W"}$existingId');
      argSlots[argSlot] = rt.VarRef(existingId, isReader: arg.isReader);
    } else {
      // Create new variable
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      varNameToId[baseName] = writerId;  // In single-ID, writerId == readerId conceptually

      if (!arg.isReader) {
        queryVarWriters[baseName] = writerId;
      }

      if (debugOutput) print('[DEBUG] New var $baseName: ${arg.isReader ? "R" : "W"}$writerId');
      argSlots[argSlot] = rt.VarRef(arg.isReader ? readerId : writerId, isReader: arg.isReader);
    }
  } else if (arg is ListTerm) {
    // List - build recursively
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    final listValue = _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId);
    if (listValue is rt.ConstTerm) {
      runtime.heap.bindWriterConst(writerId, listValue.value);
    } else if (listValue is rt.StructTerm) {
      runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
    }
    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else if (arg is ConstTerm) {
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.bindWriterConst(writerId, arg.value);
    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else if (arg is StructTerm) {
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    final structValue = _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
    runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else {
    throw Exception('Unsupported argument type: ${arg.runtimeType}');
  }
}

// Build a structure term for conjunction, reusing existing variables
rt.Term _buildStructTermForConj(
  GlpRuntime runtime,
  StructTerm struct,
  Map<String, int> queryVarWriters,
  Map<String, int> varNameToId, {
  bool debugOutput = false,
}) {
  final argTerms = <rt.Term>[];

  for (final arg in struct.args) {
    if (arg is ConstTerm) {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      runtime.heap.bindWriterConst(writerId, arg.value);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else if (arg is VarTerm) {
      final baseName = arg.name;
      final existingId = varNameToId[baseName];

      if (existingId != null) {
        argTerms.add(rt.VarRef(existingId, isReader: arg.isReader));
      } else {
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        varNameToId[baseName] = writerId;
        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }
        argTerms.add(arg.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false));
      }
    } else if (arg is ListTerm) {
      if (arg.isNil) {
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        runtime.heap.bindWriterConst(writerId, 'nil');
        argTerms.add(rt.VarRef(readerId, isReader: true));
      } else {
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        final listValue = _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId) as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
        argTerms.add(rt.VarRef(readerId, isReader: true));
      }
    } else if (arg is StructTerm) {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      final structValue = _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else {
      throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
    }
  }

  return rt.StructTerm(struct.functor, argTerms);
}

// Build a list term for conjunction, reusing existing variables
rt.Term _buildListTermForConj(
  GlpRuntime runtime,
  ListTerm list,
  Map<String, int> queryVarWriters,
  Map<String, int> varNameToId,
) {
  if (list.isNil) {
    return rt.ConstTerm('nil');
  }

  final head = list.head;
  final tail = list.tail;

  rt.Term headTerm;
  if (head is ConstTerm) {
    headTerm = rt.ConstTerm(head.value);
  } else if (head is VarTerm) {
    final baseName = head.name;
    final existingId = varNameToId[baseName];
    if (existingId != null) {
      headTerm = rt.VarRef(existingId, isReader: head.isReader);
    } else {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      varNameToId[baseName] = writerId;
      if (!head.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      headTerm = head.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false);
    }
  } else if (head is ListTerm) {
    headTerm = _buildListTermForConj(runtime, head, queryVarWriters, varNameToId);
  } else {
    throw Exception('Unsupported list head type: ${head.runtimeType}');
  }

  rt.Term tailTerm;
  if (tail is ListTerm) {
    tailTerm = _buildListTermForConj(runtime, tail, queryVarWriters, varNameToId);
  } else if (tail is VarTerm) {
    final baseName = tail.name;
    final existingId = varNameToId[baseName];
    if (existingId != null) {
      tailTerm = rt.VarRef(existingId, isReader: tail.isReader);
    } else {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      varNameToId[baseName] = writerId;
      if (!tail.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      tailTerm = tail.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false);
    }
  } else {
    tailTerm = rt.ConstTerm(null);
  }

  return rt.StructTerm('.', [headTerm, tailTerm]);
}

// Set up heap cells for a query argument
// Returns (readerId, writerId) for the argument slot
void _setupArgument(
  GlpRuntime runtime,
  Term arg,
  int argSlot,
  Map<int, rt.Term> argSlots,
  Map<String, int> queryVarWriters, {
  bool debugOutput = false,
}) {
  if (arg is VarTerm) {
    // Variable: create fresh writer/reader pair (FCP: both cells created internally)
    final (writerId, readerId) = runtime.heap.allocateFreshPair();

    // Track this variable for later display
    if (!arg.isReader) {
      queryVarWriters[arg.name] = writerId;
    }

    // Map to argument slot
    if (arg.isReader) {
      argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
    } else {
      argSlots[argSlot] = rt.VarRef(writerId, isReader: false);
    }
  } else if (arg is ListTerm) {
    // List: create structure and bind it (FCP: both cells created internally)
    final (writerId, readerId) = runtime.heap.allocateFreshPair();

    // Build list structure recursively
    final listValue = _buildListTerm(runtime, arg, queryVarWriters);
    if (listValue is rt.ConstTerm) {
      runtime.heap.bindWriterConst(writerId, listValue.value);
    } else if (listValue is rt.StructTerm) {
      runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
    }

    // Always use reader for pre-bound values
    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else if (arg is ConstTerm) {
    // Constant: create bound writer/reader (FCP: both cells created internally)
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    runtime.heap.bindWriterConst(writerId, arg.value);

    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else if (arg is StructTerm) {
    // Structure: create and bind it (FCP: both cells created internally)
    final (writerId, readerId) = runtime.heap.allocateFreshPair();

    // Build structure term recursively
    final structValue = _buildStructTerm(runtime, arg, queryVarWriters, debugOutput: debugOutput) as rt.StructTerm;
    runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);

    argSlots[argSlot] = rt.VarRef(readerId, isReader: true);
  } else {
    throw Exception('Unsupported argument type: ${arg.runtimeType}');
  }
}

// Build a structure term recursively
rt.Term _buildStructTerm(GlpRuntime runtime, StructTerm struct, Map<String, int> queryVarWriters, {bool debugOutput = false}) {
  final argTerms = <rt.Term>[];

  for (final arg in struct.args) {
    if (arg is ConstTerm) {
      // Constant in structure - create bound writer/reader (FCP: both cells created internally)
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      runtime.heap.bindWriterConst(writerId, arg.value);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else if (arg is VarTerm) {
      // Variable in structure - check if already exists
      // Note: arg.name does NOT include the '?' suffix, so use it directly
      final baseName = arg.name;
      final existingWriterId = queryVarWriters[baseName];
      if (debugOutput) print('[DEBUG REPL] Structure arg: $baseName (isReader=${arg.isReader}), existing=$existingWriterId');

      if (arg.isReader && existingWriterId != null) {
        // Reader for existing writer - in single-ID, use same ID
        if (debugOutput) print('[DEBUG REPL]   Reusing as reader: R$existingWriterId');
        argTerms.add(rt.VarRef(existingWriterId, isReader: true));
      } else if (!arg.isReader && existingWriterId != null) {
        // Writer already exists - reuse it
        if (debugOutput) print('[DEBUG REPL]   Reusing as writer: W$existingWriterId');
        argTerms.add(rt.VarRef(existingWriterId, isReader: false));
      } else {
        // First occurrence - create fresh pair (FCP: both cells created internally)
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        if (debugOutput) print('[DEBUG REPL]   Creating fresh: W$writerId/R$readerId');
        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }
        argTerms.add(arg.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false));
      }
    } else if (arg is ListTerm) {
      if (arg.isNil) {
        // Empty list in structure - create bound writer/reader for 'nil' (FCP: both cells created internally)
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        runtime.heap.bindWriterConst(writerId, 'nil');
        argTerms.add(rt.VarRef(readerId, isReader: true));
      } else {
        // Non-empty list - recursively build and create writer/reader (FCP: both cells created internally)
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        final listValue = _buildListTerm(runtime, arg, queryVarWriters) as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
        argTerms.add(rt.VarRef(readerId, isReader: true));
      }
    } else if (arg is StructTerm) {
      // Nested structure - create bound writer/reader (FCP: both cells created internally)
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      final structValue = _buildStructTerm(runtime, arg, queryVarWriters, debugOutput: debugOutput) as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else {
      throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
    }
  }

  return rt.StructTerm(struct.functor, argTerms);
}

// Build a list term recursively
rt.Term _buildListTerm(GlpRuntime runtime, ListTerm list, Map<String, int> queryVarWriters) {
  if (list.isNil) {
    return rt.ConstTerm('nil');  // Empty list represented as 'nil'
  }

  final head = list.head;
  final tail = list.tail;

  // Convert head to runtime term
  rt.Term headTerm;
  if (head is ConstTerm) {
    headTerm = rt.ConstTerm(head.value);
  } else if (head is VarTerm) {
    // Variable in list - create writer/reader (FCP: both cells created internally)
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    if (!head.isReader) {
      queryVarWriters[head.name] = writerId;
    }
    headTerm = head.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false);
  } else if (head is ListTerm) {
    headTerm = _buildListTerm(runtime, head, queryVarWriters);
  } else {
    throw Exception('Unsupported list head type: ${head.runtimeType}');
  }

  // Convert tail to runtime term
  rt.Term tailTerm;
  if (tail is ListTerm) {
    tailTerm = _buildListTerm(runtime, tail, queryVarWriters);
  } else if (tail is VarTerm) {
    // Variable tail (e.g., [1|Z?]) - check if already exists
    final baseName = tail.name;
    final existingWriterId = queryVarWriters[baseName];

    if (tail.isReader && existingWriterId != null) {
      // Reader for existing writer - in single-ID, use same ID
      tailTerm = rt.VarRef(existingWriterId, isReader: true);
    } else if (!tail.isReader && existingWriterId != null) {
      // Writer already exists - reuse it
      tailTerm = rt.VarRef(existingWriterId, isReader: false);
    } else {
      // First occurrence - create fresh pair (FCP: both cells created internally)
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      if (!tail.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      tailTerm = tail.isReader ? rt.VarRef(readerId, isReader: true) : rt.VarRef(writerId, isReader: false);
    }
  } else {
    tailTerm = rt.ConstTerm(null);
  }

  return rt.StructTerm('.', [headTerm, tailTerm]);
}


String _formatTerm(rt.Term? term, [GlpRuntime? runtime, Set<int>? visited]) {
  if (term == null) return '[]';

  visited ??= <int>{};

  if (term is rt.ConstTerm) {
    if (term.value == null || term.value == 'nil') return '[]';
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
      if (head is rt.VarRef && runtime != null) {
        // Dereference VarRef (single-ID system) - fully recursive
        final varId = head.varId;
        if (visited.contains(varId)) {
          headStr = '<circular>';
        } else {
          visited.add(varId);
          final derefHead = runtime.heap.dereference(head);
          if (derefHead is rt.VarRef) {
            // Still unbound after dereferencing
            final displayId = derefHead.varId >= 1000 ? derefHead.varId - 1000 : derefHead.varId;
            headStr = derefHead.isReader ? 'X$displayId?' : 'X$displayId';
          } else {
            headStr = _formatTerm(derefHead, runtime, visited);
          }
        }
      } else if (head is rt.VarRef && runtime != null) {
        // OLD: Dereference reader (for backward compatibility)
        final readerId = head.varId;
        if (visited.contains(readerId)) {
          headStr = '<circular>';
        } else {
          visited.add(readerId);
          final writer = _findPairedWriter(runtime, readerId);
          if (writer != null && runtime.heap.isWriterBound(writer)) {
            final value = runtime.heap.valueOfWriter(writer);
            headStr = _formatTerm(value, runtime, visited);
          } else {
            final displayId = readerId >= 1000 ? readerId - 1000 : readerId;
            headStr = 'X$displayId';
          }
        }
      } else {
        headStr = _formatTerm(head, runtime, visited);
      }

      elements.add(headStr);

      // Move to tail
      if (tail is rt.VarRef && runtime != null) {
        // Handle VarRef (single-ID system) - dereference fully
        final varId = tail.varId;
        if (visited.contains(varId)) {
          // Circular reference in tail
          final displayId = varId >= 1000 ? varId - 1000 : varId;
          final label = tail.isReader ? 'X$displayId?' : 'X$displayId';
          return '[${elements.join(', ')} | <circular $label>]';
        }
        visited.add(varId);
        // Dereference recursively to get final value
        final derefTail = runtime.heap.dereference(tail);
        if (derefTail is rt.VarRef) {
          // Still unbound after dereferencing
          final displayId = derefTail.varId >= 1000 ? derefTail.varId - 1000 : derefTail.varId;
          final label = derefTail.isReader ? 'X$displayId?' : 'X$displayId';
          return '[${elements.join(', ')} | $label]';
        }
        current = derefTail;
        if (current == null || current is! rt.StructTerm) break;
      } else if (tail is rt.VarRef && runtime != null) {
        // OLD: Handle ReaderTerm (backward compatibility)
        final readerId = tail.varId;
        if (visited.contains(readerId)) {
          // Circular reference in tail
          final displayId = readerId >= 1000 ? readerId - 1000 : readerId;
          return '[${elements.join(', ')} | <circular X$displayId>]';
        }
        visited.add(readerId);
        final writer = _findPairedWriter(runtime, readerId);
        if (writer != null && runtime.heap.isWriterBound(writer)) {
          current = runtime.heap.valueOfWriter(writer);
          if (current == null || current is! rt.StructTerm) break;
        } else {
          // Unbound reader in tail - show it
          final displayId = readerId >= 1000 ? readerId - 1000 : readerId;
          return '[${elements.join(', ')} | X$displayId]';
        }
      } else if (tail is rt.ConstTerm && (tail.value == 'nil' || tail.value == null)) {
        break;  // End of list
      } else if (tail is rt.StructTerm && tail.functor == '.') {
        // Tail is already a properly formed cons cell - continue with it
        current = tail;
      } else {
        // Any other term in tail position - not a proper list
        break;
      }
    }

    return '[${elements.join(', ')}]';
  }

  if (term is rt.StructTerm) {
    // General structure - recursively format arguments with dereferencing
    final formattedArgs = term.args.map((arg) {
      if (arg is rt.VarRef && runtime != null) {
        final varId = arg.varId;
        if (visited?.contains(varId) ?? false) {
          return '<circular>';
        }
        visited?.add(varId);
        final deref = runtime.heap.dereference(arg);
        if (deref is rt.VarRef) {
          // Still unbound
          final displayId = deref.varId >= 1000 ? deref.varId - 1000 : deref.varId;
          return deref.isReader ? 'X$displayId?' : 'X$displayId';
        }
        return _formatTerm(deref, runtime, visited);
      }
      return _formatTerm(arg, runtime, visited);
    }).join(', ');
    return '${term.functor}($formattedArgs)';
  }

  return term.toString();
}

int? _findPairedWriter(GlpRuntime runtime, int readerId) {
  // In single-ID system: readerId == writerId
  // Just check if the variable exists
  return runtime.heap.allVarIds.contains(readerId) ? readerId : null;
}

// Get git commit info for build identification
Future<String?> _getGitCommit() async {
  try {
    final result = await Process.run('git', ['log', '-1', '--format=%h %s']);
    if (result.exitCode == 0) {
      return result.stdout.toString().trim();
    }
  } catch (e) {
    // Git not available or not a git repo
  }
  return null;
}
