/// GLP Engine - Embeddable GLP Execution Core
///
/// Extracted from glp_repl.dart to provide a single, reusable implementation
/// for running GLP programs. Used by:
/// - REPL (CLI wrapper)
/// - IsolateManager (madGLP agent isolates)
/// - Tests
///
/// This is the ONE way to run GLP programs.
library;

import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';

/// Result of running a goal
class ExecutionResult {
  final ExecutionStatus status;
  final Map<String, rt.Term?> bindings;
  final String? error;

  ExecutionResult({
    required this.status,
    this.bindings = const {},
    this.error,
  });

  bool get succeeded => status == ExecutionStatus.succeeded;
  bool get failed => status == ExecutionStatus.failed;
  bool get suspended => status == ExecutionStatus.suspended;
}

/// Module info for tracking loaded modules
class ModuleInfo {
  final String name;
  final BytecodeProgram program;
  final List<String> imports;

  ModuleInfo({required this.name, required this.program, required this.imports});
}

/// GLP Engine - the embeddable core for running GLP programs
class GlpEngine {
  final GlpCompiler _compiler = GlpCompiler();
  final GlpRuntime _runtime = GlpRuntime();
  final Map<String, BytecodeProgram> _loadedPrograms = {};
  final Map<String, ModuleInfo> _loadedModules = {};

  int _goalId = 1;

  /// Max execution cycles (default 10000)
  int maxCycles = 10000;

  /// Enable trace output (reductions)
  bool debugTrace = false;

  /// Enable debug output
  bool debugOutput = false;

  /// For madGLP: the MadContext for this engine
  MadContext? madContext;

  /// Access to the runtime (for madGLP integration)
  GlpRuntime get runtime => _runtime;

  /// Access to loaded programs
  Map<String, BytecodeProgram> get loadedPrograms =>
      Map.unmodifiable(_loadedPrograms);

  /// Constructor - registers standard predicates
  GlpEngine() {
    registerStandardPredicates(_runtime.systemPredicates);
  }

  /// Load stdlib files from a directory
  void loadStdlib(String stdlibDir) {
    final stdlibFiles = [
      'assign.glp',
      'univ.glp',
      'unify.glp',
      'mwm.glp',
      'equator.glp',
      'time.glp'
    ];

    for (final filename in stdlibFiles) {
      final path = '$stdlibDir/$filename';
      final file = File(path);
      if (file.existsSync()) {
        try {
          final source = file.readAsStringSync();
          final stdlibCompiler = GlpCompiler();
          final prog = stdlibCompiler.compile(source);
          _loadedPrograms['__stdlib_${filename}__'] = prog;
        } catch (e) {
          // Silently skip failed stdlib loads
        }
      }
    }
  }

  /// Load a GLP file from path
  ///
  /// Returns true if successful, false otherwise.
  /// Throws on parse/compile errors.
  bool loadFile(String path) {
    final file = File(path);
    if (!file.existsSync()) {
      throw FileSystemException('File not found', path);
    }

    final source = file.readAsStringSync();
    return loadSource(source, filename: path);
  }

  /// Load GLP source code
  ///
  /// Returns true if successful.
  /// Throws on parse/compile errors.
  bool loadSource(String source, {String? filename}) {
    final name = filename ?? '_source_';

    // Parse to get Module AST for type checking
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();

    // Type check if program has procedure declarations
    if (module.procDeclarations.isNotEmpty) {
      final ast = Program(module.procedures, module.line, module.column);
      final partialEvaluator = PartialEvaluator();
      final transformedAst = partialEvaluator.transformDefinedGuards(ast);

      // Type errors are advisory, not fatal
      final typeResult =
          checkModule(module, transformedProcedures: transformedAst.procedures);
      if (!typeResult.isWellTyped) {
        // Could expose errors via callback if needed
      }
    }

    // Compile
    final program = _compiler.compile(source);
    _loadedPrograms[name] = program;

    final moduleInfo = _extractModuleInfo(source, program, name);
    _loadedModules[moduleInfo.name] = moduleInfo;

    return true;
  }

  /// Run a goal and return the result
  ///
  /// [goalText] is the goal to run, e.g., "merge([1,2],[a,b],X)"
  Future<ExecutionResult> runGoal(String goalText) async {
    try {
      // Parse the goal
      var trimmed = goalText.trim();
      if (trimmed.endsWith('.')) {
        trimmed = trimmed.substring(0, trimmed.length - 1).trim();
      }

      // Check if this is a conjunction
      if (_isConjunction(trimmed)) {
        return await _runConjunction(trimmed);
      }

      return await _runSingleGoal(trimmed);
    } catch (e) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: e.toString(),
      );
    }
  }

  /// Enable madGLP mode for this engine
  void enableMadGLP({required String agentId}) {
    madContext = MadContext(agentId: agentId, runtime: _runtime);
  }

  /// Get the combined bytecode program from all loaded sources
  BytecodeProgram get combinedProgram {
    final allOps = <dynamic>[];
    for (final loaded in _loadedPrograms.values) {
      allOps.addAll(loaded.ops);
    }
    return BytecodeProgram(allOps);
  }

  // ============ Private Methods ============

  Future<ExecutionResult> _runSingleGoal(String trimmed) async {
    final parseInput = '$trimmed.';
    final lexer = Lexer(parseInput);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final ast = parser.parse();

    if (ast.procedures.isEmpty) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: 'No goal found',
      );
    }

    final proc = ast.procedures[0];
    if (proc.clauses.isEmpty) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: 'No clauses in goal',
      );
    }

    final goalClause = proc.clauses[0];
    final goalAtom = goalClause.head;
    final functor = goalAtom.functor;
    final arity = goalAtom.arity;
    final args = goalAtom.args;

    final program = combinedProgram;
    final procedureLabel = '$functor/$arity';
    final entryPC = program.labels[procedureLabel];

    if (entryPC == null) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: 'Predicate $procedureLabel not found',
      );
    }

    final queryVarWriters = <String, int>{};
    final varNameToId = <String, int>{};
    final argSlots = <int, rt.Term>{};

    for (int i = 0; i < args.length; i++) {
      _setupArgument(
          _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
    }

    final env = CallEnv(args: argSlots);
    _runtime.setGoalEnv(_goalId, env);
    _runtime.setGoalProgram(_goalId, 'main');

    final module = _findModuleForProcedure(procedureLabel);
    if (module != null) {
      final modCtx = _buildModuleContext(module, program);
      if (modCtx != null) {
        _runtime.setGoalModuleContext(_goalId, modCtx);
      }
    }

    final runner = BytecodeRunner(program);
    final scheduler = Scheduler(rt: _runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();
    scheduler.setQueryVarNames(queryVarWriters);

    _runtime.gq.enqueue(GoalRef(_goalId, entryPC));
    _goalId++;

    final result = await scheduler.drainAsyncWithStatus(
      maxCycles: maxCycles,
      debug: debugTrace,
      showBindings: false,
      debugOutput: debugOutput,
    );

    // Collect bindings
    final bindings = <String, rt.Term?>{};
    for (final entry in queryVarWriters.entries) {
      final varName = entry.key;
      final writerId = entry.value;
      if (_runtime.heap.isBound(writerId)) {
        final varRef = rt.VarRef(writerId);
        bindings[varName] = _runtime.heap.dereference(varRef);
      } else {
        bindings[varName] = null;
      }
    }

    return ExecutionResult(
      status: result.status,
      bindings: bindings,
    );
  }

  Future<ExecutionResult> _runConjunction(String trimmed) async {
    final parseInput = '_conj_wrapper_ :- $trimmed.';
    final lexer = Lexer(parseInput);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final ast = parser.parse();

    if (ast.procedures.isEmpty || ast.procedures[0].clauses.isEmpty) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: 'Could not parse conjunction',
      );
    }

    final clause = ast.procedures[0].clauses[0];
    if (clause.body == null || clause.body!.isEmpty) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: 'No goals in conjunction',
      );
    }

    final goals =
        clause.body!.map((g) => Atom(g.functor, g.args, g.line, g.column)).toList();
    final program = combinedProgram;
    final queryVarWriters = <String, int>{};
    final varNameToId = <String, int>{};

    final runner = BytecodeRunner(program);
    final scheduler = Scheduler(rt: _runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();

    var allSucceeded = true;
    var anySuspended = false;

    for (final goal in goals) {
      final functor = goal.functor;
      final arity = goal.args.length;
      final args = goal.args;

      final procedureLabel = '$functor/$arity';
      final entryPC = program.labels[procedureLabel];
      if (entryPC == null) {
        return ExecutionResult(
          status: ExecutionStatus.failed,
          error: 'Predicate $procedureLabel not found',
        );
      }

      final argSlots = <int, rt.Term>{};
      for (int i = 0; i < args.length; i++) {
        _setupConjunctionArg(
            _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
      }

      final env = CallEnv(args: argSlots);
      _runtime.setGoalEnv(_goalId, env);
      _runtime.setGoalProgram(_goalId, 'main');

      final module = _findModuleForProcedure(procedureLabel);
      if (module != null) {
        final modCtx = _buildModuleContext(module, program);
        if (modCtx != null) {
          _runtime.setGoalModuleContext(_goalId, modCtx);
        }
      }

      scheduler.setQueryVarNames(queryVarWriters);
      _runtime.gq.enqueue(GoalRef(_goalId, entryPC));
      _goalId++;

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

    // Collect bindings
    final bindings = <String, rt.Term?>{};
    for (final entry in queryVarWriters.entries) {
      final varName = entry.key;
      final writerId = entry.value;
      if (_runtime.heap.isBound(writerId)) {
        final varRef = rt.VarRef(writerId);
        bindings[varName] = _runtime.heap.dereference(varRef);
      } else {
        bindings[varName] = null;
      }
    }

    final status = !allSucceeded
        ? ExecutionStatus.failed
        : (anySuspended ? ExecutionStatus.suspended : ExecutionStatus.succeeded);

    return ExecutionResult(
      status: status,
      bindings: bindings,
    );
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

  ModuleInfo _extractModuleInfo(
      String source, BytecodeProgram program, String filename) {
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
      imports.addAll(importMatch
          .group(1)!
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

  ModuleInfo? _findModuleForProcedure(String procedureLabel) {
    for (final module in _loadedModules.values) {
      if (module.program.labels.containsKey(procedureLabel)) {
        return module;
      }
    }
    return null;
  }

  ReplModuleContext? _buildModuleContext(
      ModuleInfo module, BytecodeProgram combinedProg) {
    if (module.imports.isEmpty) {
      return null;
    }

    final imports = <int, ReplModuleTarget>{};
    for (int i = 0; i < module.imports.length; i++) {
      final importName = module.imports[i];
      final target = _loadedModules[importName];
      if (target != null) {
        imports[i + 1] = ReplModuleTarget(target.name, target.program);
      }
    }

    return ReplModuleContext(
      moduleName: module.name,
      imports: imports,
      combinedProgram: combinedProg,
      programKey: 'main',
    );
  }

  void _setupArgument(
    GlpRuntime runtime,
    Term arg,
    int argSlot,
    Map<int, rt.Term> argSlots,
    Map<String, int> queryVarWriters,
    Map<String, int> varNameToId,
  ) {
    if (arg is VarTerm) {
      final baseName = arg.name;
      final existingId = varNameToId[baseName];

      if (existingId != null) {
        argSlots[argSlot] = rt.VarRef(
            arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;

        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }

        argSlots[argSlot] = rt.VarRef(arg.isReader ? readerId : writerId);
      }
    } else if (arg is ListTerm) {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      final listValue =
          _buildListTerm(runtime, arg, queryVarWriters, varNameToId);
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
      final structValue =
          _buildStructTerm(runtime, arg, queryVarWriters, varNameToId)
              as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argSlots[argSlot] = rt.VarRef(readerId);
    } else {
      throw Exception('Unsupported argument type: ${arg.runtimeType}');
    }
  }

  void _setupConjunctionArg(
    GlpRuntime runtime,
    Term arg,
    int argSlot,
    Map<int, rt.Term> argSlots,
    Map<String, int> queryVarWriters,
    Map<String, int> varNameToId,
  ) {
    if (arg is VarTerm) {
      final baseName = arg.name;
      final existingId = varNameToId[baseName];

      if (existingId != null) {
        argSlots[argSlot] = rt.VarRef(
            arg.isReader ? runtime.heap.pairedReaderAddr(existingId) : existingId);
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;

        if (!arg.isReader) {
          queryVarWriters[baseName] = writerId;
        }

        argSlots[argSlot] = rt.VarRef(arg.isReader ? readerId : writerId);
      }
    } else if (arg is ListTerm) {
      final (writerId, readerId) = runtime.heap.allocateVariable();
      final listValue =
          _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId);
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
      final structValue =
          _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId)
              as rt.StructTerm;
      runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
      argSlots[argSlot] = rt.VarRef(readerId);
    } else {
      throw Exception('Unsupported argument type: ${arg.runtimeType}');
    }
  }

  rt.Term _buildStructTerm(
    GlpRuntime runtime,
    StructTerm struct,
    Map<String, int> queryVarWriters,
    Map<String, int> varNameToId,
  ) {
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
          argTerms.add(rt.VarRef(arg.isReader
              ? runtime.heap.pairedReaderAddr(existingId)
              : existingId));
        } else {
          final (writerId, readerId) = runtime.heap.allocateVariable();
          varNameToId[baseName] = writerId;
          if (!arg.isReader) {
            queryVarWriters[baseName] = writerId;
          }
          argTerms.add(rt.VarRef(arg.isReader ? readerId : writerId));
        }
      } else if (arg is ListTerm) {
        if (arg.isNil) {
          final (writerId, readerId) = runtime.heap.allocateVariable();
          runtime.heap.bindWriterConst(writerId, 'nil');
          argTerms.add(rt.VarRef(readerId));
        } else {
          final (writerId, readerId) = runtime.heap.allocateVariable();
          final listValue =
              _buildListTerm(runtime, arg, queryVarWriters, varNameToId)
                  as rt.StructTerm;
          runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
          argTerms.add(rt.VarRef(readerId));
        }
      } else if (arg is StructTerm) {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        final structValue =
            _buildStructTerm(runtime, arg, queryVarWriters, varNameToId)
                as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
        argTerms.add(rt.VarRef(readerId));
      } else {
        throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
      }
    }

    return rt.StructTerm(struct.functor, argTerms);
  }

  rt.Term _buildStructTermForConj(
    GlpRuntime runtime,
    StructTerm struct,
    Map<String, int> queryVarWriters,
    Map<String, int> varNameToId,
  ) {
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
          argTerms.add(rt.VarRef(arg.isReader
              ? runtime.heap.pairedReaderAddr(existingId)
              : existingId));
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
          final listValue =
              _buildListTermForConj(runtime, arg, queryVarWriters, varNameToId)
                  as rt.StructTerm;
          runtime.heap.bindWriterStruct(writerId, listValue.functor, listValue.args);
          argTerms.add(rt.VarRef(readerId));
        }
      } else if (arg is StructTerm) {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        final structValue =
            _buildStructTermForConj(runtime, arg, queryVarWriters, varNameToId)
                as rt.StructTerm;
        runtime.heap.bindWriterStruct(writerId, structValue.functor, structValue.args);
        argTerms.add(rt.VarRef(readerId));
      } else {
        throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
      }
    }

    return rt.StructTerm(struct.functor, argTerms);
  }

  rt.Term _buildListTerm(
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
        headTerm = rt.VarRef(head.isReader
            ? runtime.heap.pairedReaderAddr(existingId)
            : existingId);
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;
        if (!head.isReader) {
          queryVarWriters[baseName] = writerId;
        }
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
        tailTerm = rt.VarRef(tail.isReader
            ? runtime.heap.pairedReaderAddr(existingId)
            : existingId);
      } else {
        final (writerId, readerId) = runtime.heap.allocateVariable();
        varNameToId[baseName] = writerId;
        if (!tail.isReader) {
          queryVarWriters[baseName] = writerId;
        }
        tailTerm = rt.VarRef(tail.isReader ? readerId : writerId);
      }
    } else {
      tailTerm = rt.ConstTerm(null);
    }

    return rt.StructTerm('.', [headTerm, tailTerm]);
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
        headTerm = rt.VarRef(head.isReader
            ? runtime.heap.pairedReaderAddr(existingId)
            : existingId);
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
      headTerm =
          _buildStructTermForConj(runtime, head, queryVarWriters, varNameToId);
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
        tailTerm = rt.VarRef(tail.isReader
            ? runtime.heap.pairedReaderAddr(existingId)
            : existingId);
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
}
