/// GLP I/O Test Harness - Phase 0
///
/// Command-line program to test external I/O mechanism.
/// Tests that Dart can inject terms into GLP and observe output.
///
/// Based on docs/glp-io-spec.md
library;

import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';

void main(List<String> args) async {
  print('╔════════════════════════════════════════════════╗');
  print('║   GLP I/O Test Harness - Phase 0               ║');
  print('╚════════════════════════════════════════════════╝');
  print('');
  print('Commands:');
  print('  load <file.glp>     Load GLP program');
  print('  goal <goal>         Start goal with external channels');
  print('  inject <term>       Inject term into user input stream');
  print('  close               Close user input stream');
  print('  step [n]            Step n reductions (default 1)');
  print('  run                 Run until quiescent');
  print('  output              Show observed output');
  print('  status              Show runtime status');
  print('  help                Show this help');
  print('  quit                Exit');
  print('');

  final compiler = GlpCompiler();
  final runtime = GlpRuntime();
  registerStandardPredicates(runtime.systemPredicates);

  // Track loaded programs
  final loadedPrograms = <String, BytecodeProgram>{};

  // Load stdlib
  final stdlibFiles = ['assign.glp', 'univ.glp', 'unify.glp', 'mwm.glp'];
  for (final filename in stdlibFiles) {
    final stdlibPath = '../glp/stdlib/$filename';
    final stdlibFile = File(stdlibPath);
    if (stdlibFile.existsSync()) {
      try {
        final stdlibSource = stdlibFile.readAsStringSync();
        final stdlibCompiler = GlpCompiler();
        final stdlibProg = stdlibCompiler.compile(stdlibSource);
        loadedPrograms['__stdlib_${filename}__'] = stdlibProg;
      } catch (e) {
        print('Warning: Could not load stdlib $filename: $e');
      }
    }
  }

  // Create I/O context for agent 'test'
  final ioContext = AgentIOContext.create(runtime.heap, 'test');
  print('Created I/O context for agent: ${ioContext.agentId}');
  print('  User channel: ${ioContext.userChannel}');
  print('  Net channel: ${ioContext.netChannel}');
  print('');

  // Track scheduler and goal state
  Scheduler? scheduler;
  var goalId = 1;
  var goalActive = false;

  while (true) {
    stdout.write('IO> ');
    final input = stdin.readLineSync();

    if (input == null) break;
    if (input.trim().isEmpty) continue;

    final parts = input.trim().split(RegExp(r'\s+'));
    final cmd = parts[0].toLowerCase();

    try {
      switch (cmd) {
        case 'quit':
        case 'exit':
        case 'q':
          print('Goodbye!');
          ioContext.dispose();
          return;

        case 'help':
        case 'h':
        case '?':
          _printHelp();

        case 'load':
          if (parts.length < 2) {
            print('Usage: load <file.glp>');
            continue;
          }
          final filename = parts[1];
          if (_loadProgram(filename, compiler, loadedPrograms)) {
            print('Loaded: $filename');
          }

        case 'goal':
          if (parts.length < 2) {
            print('Usage: goal <goal>');
            print('Example: goal echo_test(Ch)');
            continue;
          }
          final goalStr = parts.sublist(1).join(' ');
          final result = _startGoal(
            goalStr,
            runtime,
            loadedPrograms,
            ioContext,
            goalId,
          );
          if (result != null) {
            scheduler = result.$1;
            goalId = result.$2;
            goalActive = true;
            print('Goal started. Use "step" or "run" to execute.');
          }

        case 'inject':
          if (parts.length < 2) {
            print('Usage: inject <term>');
            print('Examples:');
            print('  inject hello');
            print('  inject msg(alice, hi)');
            continue;
          }
          final termStr = parts.sublist(1).join(' ');
          _injectTerm(termStr, ioContext.userInput, runtime);

        case 'close':
          ioContext.userInput.close();
          print('User input stream closed.');

        case 'step':
          if (scheduler == null) {
            print('No goal active. Use "goal" to start one.');
            continue;
          }
          final n = parts.length > 1 ? int.tryParse(parts[1]) ?? 1 : 1;
          await _stepExecution(scheduler, n);

        case 'run':
          if (scheduler == null) {
            print('No goal active. Use "goal" to start one.');
            continue;
          }
          final result = await scheduler.drainAsyncWithStatus(
            maxCycles: 10000,
            debug: true,
            showBindings: false,
          );
          print('Execution ${result.status.name}');
          if (result.status != ExecutionStatus.suspended) {
            goalActive = false;
          }

        case 'output':
          _showOutput(ioContext);

        case 'status':
          _showStatus(runtime, ioContext, goalActive, scheduler);

        default:
          print('Unknown command: $cmd');
          print('Type "help" for available commands.');
      }
    } catch (e, st) {
      print('Error: $e');
      print(st);
    }
    print('');
  }
}

void _printHelp() {
  print('');
  print('GLP I/O Test Harness Commands:');
  print('');
  print('  load <file.glp>     Load GLP program from glp/ directory');
  print('  goal <goal>         Start goal with external channels');
  print('                      Ch is bound to user channel ch(In, Out)');
  print('                      Net is bound to network channel');
  print('  inject <term>       Inject term into user input stream');
  print('  close               Close user input stream (terminate)');
  print('  step [n]            Step n goal reductions (default 1)');
  print('  run                 Run until quiescent or limit reached');
  print('  output              Show observed output from GLP');
  print('  status              Show runtime status');
  print('  help                Show this help');
  print('  quit                Exit harness');
  print('');
  print('Example session:');
  print('  IO> load echo_io.glp');
  print('  IO> goal echo_test(Ch)');
  print('  IO> inject hello');
  print('  IO> run');
  print('  IO> output');
  print('');
}

bool _loadProgram(String filename, GlpCompiler compiler, Map<String, BytecodeProgram> programs) {
  try {
    final File sourceFile;
    if (filename.startsWith('/')) {
      sourceFile = File(filename);
    } else {
      sourceFile = File('glp/$filename');
    }

    if (!sourceFile.existsSync()) {
      print('Error: File not found: ${sourceFile.path}');
      return false;
    }

    final source = sourceFile.readAsStringSync();
    final program = compiler.compile(source);
    programs[filename] = program;
    return true;
  } catch (e) {
    print('Error loading $filename: $e');
    return false;
  }
}

/// Start a goal with external channels bound
/// Returns (scheduler, nextGoalId) or null on failure
(Scheduler, int)? _startGoal(
  String goalStr,
  GlpRuntime runtime,
  Map<String, BytecodeProgram> programs,
  AgentIOContext ioContext,
  int goalId,
) {
  try {
    // Parse goal (add . if missing)
    final parseInput = goalStr.endsWith('.') ? goalStr : '$goalStr.';
    final lexer = Lexer(parseInput);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final parsedAst = parser.parse();

    if (parsedAst.procedures.isEmpty || parsedAst.procedures[0].clauses.isEmpty) {
      print('Error: Could not parse goal');
      return null;
    }

    final goalClause = parsedAst.procedures[0].clauses[0];
    final goalAtom = goalClause.head;
    final functor = goalAtom.functor;
    final arity = goalAtom.arity;
    final args = goalAtom.args;

    // Combine loaded programs
    final allOps = <dynamic>[];
    for (final loaded in programs.values) {
      allOps.addAll(loaded.ops);
    }
    final combinedProgram = BytecodeProgram(allOps);

    // Find entry point
    final procedureLabel = '$functor/$arity';
    final entryPC = combinedProgram.labels[procedureLabel];
    if (entryPC == null) {
      print('Error: Predicate $procedureLabel not found');
      print('Available: ${combinedProgram.labels.keys.where((k) => !k.endsWith('_end') && !k.contains('_c')).join(', ')}');
      return null;
    }

    // Set up arguments
    final argSlots = <int, rt.Term>{};
    for (int i = 0; i < args.length; i++) {
      final arg = args[i];
      if (arg is ast.VarTerm) {
        // Bind special variable names to external channels
        final name = arg.name;
        if (name == 'Ch' || name == 'UserCh') {
          // User channel
          argSlots[i] = ioContext.userChannelTerm;
          print('Bound $name to user channel');
        } else if (name == 'Net' || name == 'NetCh') {
          // Network channel
          argSlots[i] = ioContext.netChannelTerm;
          print('Bound $name to network channel');
        } else {
          // Regular variable - allocate fresh
          final (writerId, readerId) = runtime.heap.allocateFreshPair();
          argSlots[i] = rt.VarRef(arg.isReader ? readerId : writerId, isReader: arg.isReader);
        }
      } else if (arg is ast.ConstTerm) {
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        runtime.heap.bindWriterConst(writerId, arg.value);
        argSlots[i] = rt.VarRef(readerId, isReader: true);
      } else if (arg is ast.StructTerm) {
        // For structs, need to build the term
        final (writerId, readerId) = runtime.heap.allocateFreshPair();
        final structValue = _buildStructTerm(runtime, arg);
        runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
        argSlots[i] = rt.VarRef(readerId, isReader: true);
      } else {
        print('Warning: Unsupported arg type ${arg.runtimeType}');
      }
    }

    // Set up goal environment
    final env = CallEnv(args: argSlots);
    runtime.setGoalEnv(goalId, env);
    runtime.setGoalProgram(goalId, 'main');

    // Create scheduler
    final runner = BytecodeRunner(combinedProgram);
    final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();

    // Enqueue goal
    runtime.gq.enqueue(GoalRef(goalId, entryPC));

    return (scheduler, goalId + 1);
  } catch (e) {
    print('Error starting goal: $e');
    return null;
  }
}

/// Build a runtime struct term from AST
rt.StructTerm _buildStructTerm(GlpRuntime runtime, ast.StructTerm struct) {
  final argTerms = <rt.Term>[];

  for (final arg in struct.args) {
    if (arg is ast.ConstTerm) {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      runtime.heap.bindWriterConst(writerId, arg.value);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else if (arg is ast.VarTerm) {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      argTerms.add(rt.VarRef(arg.isReader ? readerId : writerId, isReader: arg.isReader));
    } else if (arg is ast.StructTerm) {
      final (writerId, readerId) = runtime.heap.allocateFreshPair();
      final nested = _buildStructTerm(runtime, arg);
      runtime.heap.bindWriterStruct(writerId, nested.functor, nested.args);
      argTerms.add(rt.VarRef(readerId, isReader: true));
    } else {
      throw Exception('Unsupported struct arg: ${arg.runtimeType}');
    }
  }

  return rt.StructTerm(struct.functor, argTerms);
}

/// Inject a term into the input stream
void _injectTerm(String termStr, InputInjector injector, GlpRuntime runtime) {
  try {
    // Parse term (wrap as fact to use parser)
    final parseInput = '_temp_($termStr).';
    final lexer = Lexer(parseInput);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final parsedAst = parser.parse();

    if (parsedAst.procedures.isEmpty || parsedAst.procedures[0].clauses.isEmpty) {
      print('Error: Could not parse term');
      return;
    }

    final clause = parsedAst.procedures[0].clauses[0];
    if (clause.head.args.isEmpty) {
      print('Error: No term to inject');
      return;
    }

    final argAst = clause.head.args[0];
    final term = _astToRuntimeTerm(argAst, runtime);

    injector.inject(term);
    print('Injected: $termStr');
  } catch (e) {
    print('Error injecting term: $e');
  }
}

/// Convert AST term to runtime term
rt.Term _astToRuntimeTerm(ast.Term astTerm, GlpRuntime runtime) {
  if (astTerm is ast.ConstTerm) {
    return rt.ConstTerm(astTerm.value);
  } else if (astTerm is ast.VarTerm) {
    final (writerId, readerId) = runtime.heap.allocateFreshPair();
    return rt.VarRef(astTerm.isReader ? readerId : writerId, isReader: astTerm.isReader);
  } else if (astTerm is ast.StructTerm) {
    final args = astTerm.args.map((a) => _astToRuntimeTerm(a, runtime)).toList();
    return rt.StructTerm(astTerm.functor, args);
  } else if (astTerm is ast.ListTerm) {
    return _astListToRuntimeTerm(astTerm, runtime);
  }
  throw Exception('Unknown AST term type: ${astTerm.runtimeType}');
}

/// Convert AST list to runtime term
rt.Term _astListToRuntimeTerm(ast.ListTerm list, GlpRuntime runtime) {
  if (list.isNil) {
    return rt.ConstTerm('nil');
  }

  final head = _astToRuntimeTerm(list.head!, runtime);
  final tail = list.tail is ast.ListTerm
      ? _astListToRuntimeTerm(list.tail as ast.ListTerm, runtime)
      : list.tail != null
          ? _astToRuntimeTerm(list.tail!, runtime)
          : rt.ConstTerm('nil');

  return rt.StructTerm('.', [head, tail]);
}

/// Step execution by n reductions
Future<void> _stepExecution(Scheduler scheduler, int n) async {
  // Use drainAsyncWithStatus with limited cycles to step
  final result = await scheduler.drainAsyncWithStatus(
    maxCycles: n,
    debug: true,
    showBindings: false,
  );
  print('Stepped $n cycles: ${result.status.name}');
}

/// Show collected output
void _showOutput(AgentIOContext ctx) {
  print('User output (${ctx.userOutputTerms.length} terms):');
  for (var i = 0; i < ctx.userOutputTerms.length; i++) {
    print('  [$i] ${_formatTerm(ctx.userOutputTerms[i])}');
  }
  if (ctx.userOutputClosed) {
    print('  [stream closed]');
  }

  print('Network output (${ctx.netOutputTerms.length} terms):');
  for (var i = 0; i < ctx.netOutputTerms.length; i++) {
    print('  [$i] ${_formatTerm(ctx.netOutputTerms[i])}');
  }
  if (ctx.netOutputClosed) {
    print('  [stream closed]');
  }
}

/// Show runtime status
void _showStatus(GlpRuntime runtime, AgentIOContext ctx, bool goalActive, Scheduler? scheduler) {
  print('Runtime Status:');
  print('  Goal queue: ${runtime.gq.length} goals');
  print('  Heap vars: ${runtime.heap.allVarIds.length}');
  print('  Goal active: $goalActive');
  print('');
  print('I/O Status:');
  print('  User input writer: ${ctx.userInput.currentWriterId}');
  print('  User output reader: ${ctx.userOutput.currentReaderId}');
  print('  User output closed: ${ctx.userOutput.isClosed}');
  print('  Collected user output: ${ctx.userOutputTerms.length} terms');
  print('  Collected net output: ${ctx.netOutputTerms.length} terms');
}

/// Format a runtime term for display
String _formatTerm(rt.Term term) {
  if (term is rt.ConstTerm) {
    if (term.value == 'nil' || term.value == null) return '[]';
    return term.value.toString();
  }
  if (term is rt.VarRef) {
    return term.isReader ? 'X${term.varId}?' : 'X${term.varId}';
  }
  if (term is rt.StructTerm) {
    if (term.functor == '.' && term.args.length == 2) {
      // List - format specially
      final elements = <String>[];
      rt.Term current = term;
      while (current is rt.StructTerm && current.functor == '.' && current.args.length == 2) {
        elements.add(_formatTerm(current.args[0]));
        current = current.args[1];
      }
      if (current is rt.ConstTerm && (current.value == 'nil' || current.value == null)) {
        return '[${elements.join(', ')}]';
      }
      return '[${elements.join(', ')} | ${_formatTerm(current)}]';
    }
    final args = term.args.map(_formatTerm).join(', ');
    return '${term.functor}($args)';
  }
  return term.toString();
}
