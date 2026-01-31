/// GLP REPL with Type Checking
///
/// Development version of REPL that integrates the type checker.
/// Programs with procedure declarations are type-checked before compilation.
/// Programs without procedure declarations skip type checking.
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
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

/// Module info for REPL module tracking
class ModuleInfo {
  final String name;
  final BytecodeProgram program;
  final List<String> imports;

  ModuleInfo({required this.name, required this.program, required this.imports});
}

void main() async {
  // Get git commit info
  final gitCommit = await _getGitCommit();
  // Build timestamp
  final buildTime = '2026-01-12 (Type checker integration)';

  print('╔════════════════════════════════════════╗');
  print('║  GLP REPL - With Type Checking         ║');
  print('╚════════════════════════════════════════╝');
  print('');
  if (gitCommit != null) {
    print('Build: $gitCommit');
  }
  print('Compiled: $buildTime');
  print('Working directory: udi/');
  print('Source files: glp/*.glp');
  print('');
  print('Input: filename.glp to load, or goal to execute');
  print('Commands: :quit, :help, :trace, :debug, :limit');
  print('');

  final compiler = GlpCompiler();
  final runtime = GlpRuntime();
  registerStandardPredicates(runtime.systemPredicates);

  // Track loaded programs
  final loadedPrograms = <String, BytecodeProgram>{};

  // Track loaded modules (for module system support)
  final loadedModules = <String, ModuleInfo>{};

  // Load stdlib files for system predicates
  final stdlibFiles = ['assign.glp', 'univ.glp', 'unify.glp', 'mwm.glp', 'equator.glp', 'time.glp'];
  for (final filename in stdlibFiles) {
    final stdlibPath = '../programs/stdlib/$filename';
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
  var debugTrace = true;
  var debugOutput = false;
  var maxCycles = 10000;

  while (true) {
    stdout.write('GLP> ');
    final input = stdin.readLineSync();

    if (input == null) {
      break;
    }

    if (input.trim().isEmpty) {
      continue;
    }

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

    // Check if input is a .glp file to load (with or without 'load' command)
    if (trimmed.endsWith('.glp')) {
      String filename;
      if (trimmed.startsWith('load ')) {
        filename = trimmed.substring(5).trim();
      } else if (!trimmed.contains(' ')) {
        filename = trimmed;
      } else {
        // Has space but doesn't start with 'load' - not a file load
        filename = '';
      }
      if (filename.isNotEmpty) {
        if (!loadProgram(filename, compiler, loadedPrograms, loadedModules)) {
          continue;
        }
        print('✓ Loaded: $filename');
        continue;
      }
    }

    // Compile and run the goal
    try {
      // Check if this is a conjunction
      if (_isConjunction(trimmed)) {
        final parseInput = '_conj_wrapper_ :- $trimmed.';
        final lexer = Lexer(parseInput);
        final tokens = lexer.tokenize();
        final parser = Parser(tokens);
        final ast = parser.parse();

        if (ast.procedures.isEmpty || ast.procedures[0].clauses.isEmpty) {
          print('Error: Could not parse conjunction');
          continue;
        }

        final clause = ast.procedures[0].clauses[0];
        if (clause.body == null || clause.body!.isEmpty) {
          print('Error: No goals in conjunction');
          continue;
        }

        final goals = clause.body!.map((g) => Atom(g.functor, g.args, g.line, g.column)).toList();

        if (debugOutput) {
          print('[DEBUG] Conjunction goals: ${goals.length}');
          for (final g in goals) {
            print('[DEBUG]   ${g.functor}/${g.args.length}');
          }
        }

        final allOps = <dynamic>[];
        for (final loaded in loadedPrograms.values) {
          allOps.addAll(loaded.ops);
        }
        final combinedProgram = BytecodeProgram(allOps);

        final queryVarWriters = <String, int>{};
        final varNameToId = <String, int>{};

        final runner = BytecodeRunner(combinedProgram);
        final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
        scheduler.resetDisplayNumbering();

        var allSucceeded = true;
        var anySuspended = false;

        for (final goal in goals) {
          final functor = goal.functor;
          final arity = goal.args.length;
          final args = goal.args;

          // Handle remote goal (Module # Goal)
          if (functor == '#' && args.length == 2) {
            final moduleArg = args[0];
            final goalArg = args[1];

            String? moduleName;
            if (moduleArg is ConstTerm) {
              moduleName = moduleArg.value.toString();
            } else if (moduleArg is VarTerm) {
              final varName = moduleArg.name;
              final writerId = varNameToId[varName];
              if (writerId != null && runtime.heap.isBound(writerId)) {
                final value = runtime.heap.dereference(rt.VarRef(writerId));
                if (value is rt.ConstTerm) {
                  moduleName = value.value.toString();
                }
              }
              if (moduleName == null) {
                print('Error: Module variable $varName is unbound or not an atom');
                allSucceeded = false;
                break;
              }
            }

            if (moduleName == null) {
              print('Error: Invalid module in remote goal');
              allSucceeded = false;
              break;
            }

            String innerFunctor;
            List<Term> innerArgs;
            if (goalArg is StructTerm) {
              innerFunctor = goalArg.functor;
              innerArgs = goalArg.args;
            } else if (goalArg is ConstTerm) {
              innerFunctor = goalArg.value.toString();
              innerArgs = [];
            } else {
              print('Error: Invalid goal in remote goal');
              allSucceeded = false;
              break;
            }

            final targetModule = loadedModules[moduleName];
            if (targetModule == null) {
              print('Error: Module $moduleName not loaded');
              allSucceeded = false;
              break;
            }

            final procedureLabel = '$innerFunctor/${innerArgs.length}';
            final entryPC = combinedProgram.labels[procedureLabel];
            if (entryPC == null) {
              print('Error: Predicate $procedureLabel not found in module $moduleName');
              allSucceeded = false;
              break;
            }

            final argSlots = <int, rt.Term>{};
            for (int i = 0; i < innerArgs.length; i++) {
              final arg = innerArgs[i];
              _setupConjunctionArg(runtime, arg, i, argSlots, queryVarWriters, varNameToId, debugOutput: debugOutput);
            }

            final env = CallEnv(args: argSlots);
            runtime.setGoalEnv(goalId, env);
            runtime.setGoalProgram(goalId, 'main');

            final modCtx = _buildModuleContext(targetModule, loadedModules, combinedProgram: combinedProgram);
            if (modCtx != null) {
              runtime.setGoalModuleContext(goalId, modCtx);
            }

            scheduler.setQueryVarNames(queryVarWriters);
            runtime.gq.enqueue(GoalRef(goalId, entryPC));
            goalId++;

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
            }
            continue;
          }

          final procedureLabel = '$functor/$arity';
          final entryPC = combinedProgram.labels[procedureLabel];
          if (entryPC == null) {
            print('Error: Predicate $procedureLabel not found');
            allSucceeded = false;
            break;
          }

          final argSlots = <int, rt.Term>{};
          for (int i = 0; i < args.length; i++) {
            final arg = args[i];
            _setupConjunctionArg(runtime, arg, i, argSlots, queryVarWriters, varNameToId, debugOutput: debugOutput);
          }

          final env = CallEnv(args: argSlots);
          runtime.setGoalEnv(goalId, env);
          runtime.setGoalProgram(goalId, 'main');

          final module = _findModuleForProcedure(procedureLabel, loadedModules);
          if (module != null) {
            final modCtx = _buildModuleContext(module, loadedModules, combinedProgram: combinedProgram);
            if (modCtx != null) {
              runtime.setGoalModuleContext(goalId, modCtx);
            }
          }

          scheduler.setQueryVarNames(queryVarWriters);

          runtime.gq.enqueue(GoalRef(goalId, entryPC));
          goalId++;

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
          }
        }

        if (debugOutput) print('[DEBUG REPL] queryVarWriters = $queryVarWriters');
        if (queryVarWriters.isNotEmpty) {
          for (final entry in queryVarWriters.entries) {
            final varName = entry.key;
            final writerId = entry.value;
            if (debugOutput) print('[DEBUG] $varName (W$writerId), isBound=${runtime.heap.isBound(writerId)}');
            if (runtime.heap.isBound(writerId)) {
              final varRef = rt.VarRef(writerId);
              final derefValue = runtime.heap.dereference(varRef);
              print('$varName = ${_formatTerm(derefValue, runtime)}');
            } else {
              print('$varName = <unbound>');
            }
          }
        }

        if (!allSucceeded) {
          _printStatus(ExecutionStatus.failed);
        } else if (anySuspended) {
          _printStatus(ExecutionStatus.suspended);
        } else {
          _printStatus(ExecutionStatus.succeeded);
        }
        continue;
      }

      // Single goal
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

      final allOps = <dynamic>[];
      for (final loaded in loadedPrograms.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      final procedureLabel = '$functor/$arity';
      final entryPC = combinedProgram.labels[procedureLabel];
      if (entryPC == null) {
        print('Error: Predicate $procedureLabel not found');
        print('Available predicates: ${combinedProgram.labels.keys.where((k) => !k.endsWith('_end') && !k.contains('_c')).join(', ')}');
        continue;
      }

      final queryVarWriters = <String, int>{};
      final varNameToId = <String, int>{};
      final argSlots = <int, rt.Term>{};

      for (int i = 0; i < args.length; i++) {
        final arg = args[i];
        _setupArgument(runtime, arg, i, argSlots, queryVarWriters, varNameToId, debugOutput: debugOutput);
      }

      final env = CallEnv(args: argSlots);
      runtime.setGoalEnv(goalId, env);
      runtime.setGoalProgram(goalId, 'main');

      final module = _findModuleForProcedure(procedureLabel, loadedModules);
      if (module != null) {
        final modCtx = _buildModuleContext(module, loadedModules, combinedProgram: combinedProgram);
        if (modCtx != null) {
          runtime.setGoalModuleContext(goalId, modCtx);
        }
      }

      final runner = BytecodeRunner(combinedProgram);
      final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

      scheduler.resetDisplayNumbering();
      scheduler.setQueryVarNames(queryVarWriters);

      runtime.gq.enqueue(GoalRef(goalId, entryPC));
      goalId++;

      final result = await scheduler.drainAsyncWithStatus(
        maxCycles: maxCycles,
        debug: debugTrace,
        showBindings: false,
        debugOutput: debugOutput,
      );

      if (debugOutput) print('[DEBUG REPL] queryVarWriters = $queryVarWriters');
      if (queryVarWriters.isNotEmpty) {
        for (final entry in queryVarWriters.entries) {
          final varName = entry.key;
          final writerId = entry.value;
          final rawValue = runtime.heap.getValue(writerId);
          if (debugOutput) print('[DEBUG] $varName (W$writerId), isBound=${runtime.heap.isBound(writerId)}, rawValue=$rawValue');
          if (runtime.heap.isBound(writerId)) {
            final varRef = rt.VarRef(writerId);
            final derefValue = runtime.heap.dereference(varRef);
            print('$varName = ${_formatTerm(derefValue, runtime)}');
          } else {
            print('$varName = <unbound>');
          }
        }
      }

      _printStatus(result.status);

    } catch (e) {
      print('Error: $e');
    }

    print('');
  }
}

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

bool loadProgram(String filename, GlpCompiler compiler, Map<String, BytecodeProgram> loadedPrograms, Map<String, ModuleInfo> loadedModules) {
  try {
    final File sourceFile;
    if (filename.startsWith('/') || filename.startsWith('../') || filename.startsWith('./')) {
      sourceFile = File(filename);
    } else {
      sourceFile = File('glp/$filename');
    }

    if (!sourceFile.existsSync()) {
      print('Error: File not found: ${sourceFile.path}');
      return false;
    }

    final source = sourceFile.readAsStringSync();
    
    // Parse to get Module AST for type checking
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();
    
    // Type check if program has procedure declarations
    if (module.procDeclarations.isNotEmpty) {
      // Apply partial evaluation (defined guard expansion) BEFORE type checking
      // This transforms clauses to unfold unit clause guards, which affects coverage checking
      final ast = Program(module.procedures, module.line, module.column);
      final partialEvaluator = PartialEvaluator();
      final transformedAst = partialEvaluator.transformDefinedGuards(ast);
      
      // Type check with transformed procedures
      // Per compilation-pipeline.md: Type errors are ADVISORY, not fatal
      // Programs should compile and run regardless of type errors
      final typeResult = checkModule(module, transformedProcedures: transformedAst.procedures);
      if (!typeResult.isWellTyped) {
        print('Type errors in $filename:');
        for (final error in typeResult.errors) {
          print('  ✗ $error');
        }
        print('  [Continuing despite type errors]');
      } else {
        print('  ✓ Type check passed (${module.procDeclarations.length} procedures)');
      }
    }
    
    // Compile
    final program = compiler.compile(source);

    loadedPrograms[filename] = program;

    final moduleInfo = _extractModuleInfo(source, program, filename);
    loadedModules[moduleInfo.name] = moduleInfo;

    return true;
  } catch (e) {
    print('Error loading $filename: $e');
    return false;
  }
}

ModuleInfo _extractModuleInfo(String source, BytecodeProgram program, String filename) {
  String name;
  final moduleMatch = RegExp(r'-module\((\w+)\)\.').firstMatch(source);
  if (moduleMatch != null) {
    name = moduleMatch.group(1)!;
  } else {
    name = _moduleNameFromFilename(filename);
  }

  final imports = <String>[];
  final importMatch = RegExp(r'-import\(\[([^\]]*)\]\)\.').firstMatch(source);
  if (importMatch != null) {
    imports.addAll(importMatch.group(1)!
        .split(',')
        .map((e) => e.trim())
        .where((e) => e.isNotEmpty));
  }

  return ModuleInfo(name: name, program: program, imports: imports);
}

String _moduleNameFromFilename(String filename) {
  final baseName = filename.split('/').last;
  if (baseName.endsWith('.glp')) {
    return baseName.substring(0, baseName.length - 4);
  }
  return baseName;
}

ReplModuleContext? _buildModuleContext(ModuleInfo module, Map<String, ModuleInfo> loadedModules, {BytecodeProgram? combinedProgram}) {
  if (module.imports.isEmpty) {
    return null;
  }

  final imports = <int, ReplModuleTarget>{};
  for (int i = 0; i < module.imports.length; i++) {
    final importName = module.imports[i];
    final target = loadedModules[importName];
    if (target != null) {
      imports[i + 1] = ReplModuleTarget(target.name, target.program);
    }
  }

  return ReplModuleContext(
    moduleName: module.name,
    imports: imports,
    combinedProgram: combinedProgram,
    programKey: 'main',
  );
}

ModuleInfo? _findModuleForProcedure(String procedureLabel, Map<String, ModuleInfo> loadedModules) {
  for (final module in loadedModules.values) {
    if (module.program.labels.containsKey(procedureLabel)) {
      return module;
    }
  }
  return null;
}

void printHelp() {
  print('');
  print('GLP REPL (With Type Checking) Usage:');
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
  print('Type Checking:');
  print('  Programs with procedure declarations are type-checked');
  print('  Programs without procedure declarations skip type checking');
  print('');
  print('Examples:');
  print('  GLP> merge.glp                        # Load typed program');
  print('  GLP> merge([1,2],[a,b],X).            # Execute goal');
  print('');
}

bool _isConjunction(String query) {
  int depth = 0;
  for (int i = 0; i < query.length; i++) {
    final char = query[i];
    if (char == '(' || char == '[') {
      depth++;
    } else if (char == ')' || char == ']') {
      depth--;
    } else if (char == ',' && depth == 0) {
      return true;
    }
  }
  return false;
}

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
      if (debugOutput) print('[DEBUG] Reusing var $baseName: ${arg.isReader ? "R" : "W"}$existingId');
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      argSlots[argSlot] = rt.VarRef(arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;

      if (!arg.isReader) {
        queryVarWriters[baseName] = writerId;
      }

      if (debugOutput) print('[DEBUG] New var $baseName: ${arg.isReader ? "R" : "W"}$writerId');
      argSlots[argSlot] = rt.VarRef(arg.isReader ? readerId : writerId);
    }
  } else if (arg is ListTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();
    final listValue = _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId);
    if (listValue is rt.ConstTerm) {
      runtime.heap.bindWriterConst(writerId, listValue.value);
    } else if (listValue is rt.StructTerm) {
      runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
    }
    argSlots[argSlot] = rt.VarRef(readerId);
  } else if (arg is ConstTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();
    runtime.heap.bindWriterConst(writerId, arg.value);
    argSlots[argSlot] = rt.VarRef(readerId);
  } else if (arg is StructTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();
    final structValue = _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
    runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
    argSlots[argSlot] = rt.VarRef(readerId);
  } else {
    throw Exception('Unsupported argument type: ${arg.runtimeType}');
  }
}

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
      final (writerId, readerId) = runtime.heap.allocateVariable();
      runtime.heap.bindWriterConst(writerId, arg.value);
      argTerms.add(rt.VarRef(readerId));
    } else if (arg is VarTerm) {
      final baseName = arg.name;
      final existingId = varNameToId[baseName];

      if (existingId != null) {
        // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
        argTerms.add(rt.VarRef(arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId));
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;
        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }
        argTerms.add(arg.isReader ? rt.VarRef(readerId) : rt.VarRef(writerId));
      }
    } else if (arg is ListTerm) {
      if (arg.isNil) {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        runtime.heap.bindWriterConst(writerId, 'nil');
        argTerms.add(rt.VarRef(readerId));
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        final listValue = _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId) as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
        argTerms.add(rt.VarRef(readerId));
      }
    } else if (arg is StructTerm) {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      final structValue = _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argTerms.add(rt.VarRef(readerId));
    } else {
      throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
    }
  }

  return rt.StructTerm(struct.functor, argTerms);
}

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
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      headTerm = rt.VarRef(head.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;
      if (!head.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      headTerm = head.isReader ? rt.VarRef(readerId) : rt.VarRef(writerId);
    }
  } else if (head is ListTerm) {
    headTerm = _buildListTermForConj(runtime, head, queryVarWriters, varNameToId);
  } else if (head is StructTerm) {
    headTerm = _buildStructTermForConj(runtime, head, queryVarWriters, varNameToId);
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
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      tailTerm = rt.VarRef(tail.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;
      if (!tail.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      tailTerm = tail.isReader ? rt.VarRef(readerId) : rt.VarRef(writerId);
    }
  } else {
    tailTerm = rt.ConstTerm(null);
  }

  return rt.StructTerm('.', [headTerm, tailTerm]);
}

void _setupArgument(
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
      if (debugOutput) print('[DEBUG] Existing var $baseName: ${arg.isReader ? "R" : "W"}$existingId');
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      argSlots[argSlot] = rt.VarRef(arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;

      if (!arg.isReader) {
        queryVarWriters[baseName] = writerId;
      }

      if (debugOutput) print('[DEBUG] New var $baseName: ${arg.isReader ? "R" : "W"}$writerId');
      // Use readerId from allocation for reader addr (FCP pattern)
      argSlots[argSlot] = rt.VarRef(arg.isReader ? readerId : writerId);
    }
  } else if (arg is ListTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();

    final listValue = _buildListTerm(runtime, arg, queryVarWriters, varNameToId);
    if (listValue is rt.ConstTerm) {
      runtime.heap.bindWriterConst(writerId, listValue.value);
    } else if (listValue is rt.StructTerm) {
      runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
    }

    argSlots[argSlot] = rt.VarRef(readerId);
  } else if (arg is ConstTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();
    runtime.heap.bindWriterConst(writerId, arg.value);

    argSlots[argSlot] = rt.VarRef(readerId);
  } else if (arg is StructTerm) {
    final (writerId, readerId) = runtime.heap.allocateVariable();

    final structValue = _buildStructTerm(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
    runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);

    argSlots[argSlot] = rt.VarRef(readerId);
  } else {
    throw Exception('Unsupported argument type: ${arg.runtimeType}');
  }
}

rt.Term _buildStructTerm(GlpRuntime runtime, StructTerm struct, Map<String, int> queryVarWriters, Map<String, int> varNameToId, {bool debugOutput = false}) {
  final argTerms = <rt.Term>[];

  for (final arg in struct.args) {
    if (arg is ConstTerm) {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      runtime.heap.bindWriterConst(writerId, arg.value);
      argTerms.add(rt.VarRef(readerId));
    } else if (arg is VarTerm) {
      final baseName = arg.name;
      final existingId = varNameToId[baseName];
      if (debugOutput) print('[DEBUG REPL] Structure arg: $baseName (isReader=${arg.isReader}), existing=$existingId');

      if (existingId != null) {
        if (debugOutput) print('[DEBUG REPL]   Reusing: ${arg.isReader ? "R" : "W"}$existingId');
        // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
        argTerms.add(rt.VarRef(arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId));
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;
        if (debugOutput) print('[DEBUG REPL]   Creating fresh: W$writerId');
        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }
        // Use readerId from allocation for reader addr (FCP pattern)
        argTerms.add(rt.VarRef(arg.isReader ? readerId : writerId));
      }
    } else if (arg is ListTerm) {
      if (arg.isNil) {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        runtime.heap.bindWriterConst(writerId, 'nil');
        argTerms.add(rt.VarRef(readerId));
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        final listValue = _buildListTerm(runtime, arg, queryVarWriters, varNameToId) as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
        argTerms.add(rt.VarRef(readerId));
      }
    } else if (arg is StructTerm) {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      final structValue = _buildStructTerm(runtime, arg, queryVarWriters, varNameToId, debugOutput: debugOutput) as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argTerms.add(rt.VarRef(readerId));
    } else {
      throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
    }
  }

  return rt.StructTerm(struct.functor, argTerms);
}

rt.Term _buildListTerm(GlpRuntime runtime, ListTerm list, Map<String, int> queryVarWriters, Map<String, int> varNameToId) {
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
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      headTerm = rt.VarRef(head.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;
      if (!head.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      // Use readerId from allocation for reader addr (FCP pattern)
      headTerm = rt.VarRef(head.isReader ? readerId : writerId);
    }
  } else if (head is ListTerm) {
    headTerm = _buildListTerm(runtime, head, queryVarWriters, varNameToId);
  } else if (head is StructTerm) {
    headTerm = _buildStructTerm(runtime, head, queryVarWriters, varNameToId);
  } else {
    throw Exception('Unsupported list head type: ${head.runtimeType}');
  }

  rt.Term tailTerm;
  if (tail is ListTerm) {
    tailTerm = _buildListTerm(runtime, tail, queryVarWriters, varNameToId);
  } else if (tail is VarTerm) {
    final baseName = tail.name;
    final existingId = varNameToId[baseName];
    if (existingId != null) {
      // existingId is always writer addr; use readerForWriter() for reader addr (FCP pattern)
      tailTerm = rt.VarRef(tail.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
    } else {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      varNameToId[baseName] = writerId;
      if (!tail.isReader) {
        queryVarWriters[baseName] = writerId;
      }
      // Use readerId from allocation for reader addr (FCP pattern)
      tailTerm = rt.VarRef(tail.isReader ? readerId : writerId);
    }
  } else {
    tailTerm = rt.ConstTerm(null);
  }

  return rt.StructTerm('.', [headTerm, tailTerm]);
}


String _formatTerm(rt.Term? term, [GlpRuntime? runtime, Set<int>? path]) {
  if (term == null) return '[]';

  path ??= <int>{};

  if (term is rt.ConstTerm) {
    if (term.value == null || term.value == 'nil') return '[]';
    return term.value.toString();
  }

  if (term is rt.StructTerm && term.functor == '.' && term.args.length == 2) {
    final elements = <String>[];
    rt.Term? current = term;

    while (true) {
      if (current is! rt.StructTerm || current.functor != '.') break;

      final head = current.args[0];
      final tail = current.args[1];

      String headStr;
      if (head is rt.VarRef && runtime != null) {
        final addr = head.addr;
        if (path.contains(addr)) {
          headStr = '<circular>';
        } else {
          path.add(addr);
          final derefHead = runtime.heap.dereference(head);
          if (derefHead is rt.VarRef) {
            final displayId = derefHead.addr;
            headStr = runtime.heap.isReader(derefHead.addr) ? 'X$displayId?' : 'X$displayId';
          } else {
            headStr = _formatTerm(derefHead, runtime, path);
          }
          path.remove(addr);
        }
      } else {
        headStr = _formatTerm(head, runtime, path);
      }

      elements.add(headStr);

      if (tail is rt.VarRef && runtime != null) {
        final addr = tail.addr;
        if (path.contains(addr)) {
          final displayId = addr;
          final label = runtime.heap.isReader(tail.addr) ? 'X$displayId?' : 'X$displayId';
          return '[${elements.join(', ')} | <circular $label>]';
        }
        path.add(addr);
        final derefTail = runtime.heap.dereference(tail);
        if (derefTail is rt.VarRef) {
          path.remove(addr);
          final displayId = derefTail.addr;
          final label = runtime.heap.isReader(derefTail.addr) ? 'X$displayId?' : 'X$displayId';
          return '[${elements.join(', ')} | $label]';
        }
        current = derefTail;
        path.remove(addr);
        if (current is! rt.StructTerm) break;
      } else if (tail is rt.ConstTerm && (tail.value == 'nil' || tail.value == null)) {
        break;
      } else if (tail is rt.StructTerm && tail.functor == '.') {
        current = tail;
      } else {
        break;
      }
    }

    return '[${elements.join(', ')}]';
  }

  if (term is rt.StructTerm) {
    final currentPath = path;
    final formattedArgs = term.args.map((arg) {
      if (arg is rt.VarRef && runtime != null) {
        final addr = arg.addr;
        if (currentPath.contains(addr)) {
          return '<circular>';
        }
        currentPath.add(addr);
        final deref = runtime.heap.dereference(arg);
        String result;
        if (deref is rt.VarRef) {
          final displayId = deref.addr;
          result = runtime.heap.isReader(deref.addr) ? 'X$displayId?' : 'X$displayId';
        } else {
          result = _formatTerm(deref, runtime, currentPath);
        }
        currentPath.remove(addr);
        return result;
      }
      return _formatTerm(arg, runtime, currentPath);
    }).join(', ');
    return '${term.functor}($formattedArgs)';
  }

  return term.toString();
}

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
