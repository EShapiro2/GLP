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
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/param_expansion.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/compiler/project_linker.dart';

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

/// madGLP system predicates (embedded).
///
/// Provides send_to_net/1, global_send/3, send_to_user/1.
/// Loaded by enableMadGLP().
const String _madPredicatesSource = r'''
-mode(system).  %% Uses reserved constants like '_w' and '_send'

%% madGLP System Predicates
%% See: madGLP-spec.md Section 4 and Section 12

%% send_to_net/1 - Process network output stream
procedure send_to_net(Stream(_)?).
send_to_net([msg(Q, T) | In]) :- ground(Q?) | global_send(msg(Q?, T?), '_w'(Q?, 0), Q?), send_to_net(In?).
send_to_net([]).

%% global_send/3 - Send via global link
procedure global_send(_?, _?, _?).
global_send(T, G, Q) :- known(T?) | '_send'(T?, G?, Q?).

%% send_to_user/1 - Process user output stream (ground terms only)
procedure send_to_user(Stream(_)?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).
''';

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

  /// When true, type errors abort program loading (default: true)
  bool strictTypes = true;

  /// Path to the root self.glp (programs/self.glp) for the type scope chain.
  late final String _rootSelfGlpPath;

  /// For madGLP: the MadContext for this engine
  MadContext? madContext;

  /// Access to the runtime (for madGLP integration)
  GlpRuntime get runtime => _runtime;

  /// Access to loaded programs
  Map<String, BytecodeProgram> get loadedPrograms =>
      Map.unmodifiable(_loadedPrograms);

  /// Constructor - registers standard predicates and loads stdlib.
  ///
  /// [stdlibDir] is the path to the stdlib directory (e.g., '../programs/stdlib').
  /// Loading stdlib is not optional — it's part of engine initialization.
  GlpEngine({required String stdlibDir}) {
    // Derive root self.glp path from stdlib dir (caller must pass absolute path)
    // e.g., '/abs/path/programs/stdlib' → '/abs/path/programs/self.glp'
    _rootSelfGlpPath = stdlibDir.replaceAll('/stdlib', '/self.glp');

    // Set prelude sources from programs/self.glp for PE and type checker
    final rootSelfFile = File(_rootSelfGlpPath);
    if (rootSelfFile.existsSync()) {
      final rootSource = rootSelfFile.readAsStringSync();
      setPreludeUnitClauseSource(rootSource);
      setPreludeEnvironmentSource(rootSource);
    }

    registerStandardPredicates(_runtime.systemPredicates);
    _loadStdlib(stdlibDir);
  }

  /// Clear all loaded programs except stdlib
  ///
  /// Useful for test scripts that need to reset state between tests
  /// without restarting the REPL process.
  void clear() {
    // Remember stdlib program
    BytecodeProgram? rootSelf = _loadedPrograms['__root_self__'];

    // Clear everything
    _loadedPrograms.clear();
    _loadedModules.clear();

    // Restore stdlib
    if (rootSelf != null) {
      _loadedPrograms['__root_self__'] = rootSelf;
    }
  }

  /// Load stdlib from programs/self.glp (private — called by constructor).
  void _loadStdlib(String stdlibDir) {
    // Derive root self.glp path from stdlib dir
    final rootSelfGlp = stdlibDir.replaceAll('/stdlib', '/self.glp');
    final file = File(rootSelfGlp);
    if (file.existsSync()) {
      try {
        final source = file.readAsStringSync();
        final compiler = GlpCompiler();
        final prog = compiler.compile(source);
        _loadedPrograms['__root_self__'] = prog;
      } catch (e) {
        // Silently skip failed load
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

    // Discover ancestor scope from self.glp hierarchy (if loading from a file)
    TypeEnvironment? ancestorScope;
    if (name != '_source_' && name != '__mad_predicates__' &&
        !name.startsWith('__stdlib_') && name != '__root_self__' &&
        File(name).existsSync()) {
      final rootDir = _findProjectRoot(name);
      if (rootDir != null) {
        final chain = discoverSelfChain(targetFile: name, rootDir: rootDir);
        if (chain.isNotEmpty) {
          ancestorScope = _buildAncestorScope(chain);
        }
      }
    }

    // Type check if program has procedure declarations
    if (module.procDeclarations.isNotEmpty) {
      final ast = Program(module.procedures, module.line, module.column);
      final partialEvaluator = PartialEvaluator();
      final transformedAst = partialEvaluator.transformDefinedGuards(ast);

      final typeResult = checkModule(module,
          transformedProcedures: transformedAst.procedures,
          ancestorScope: ancestorScope);
      if (!typeResult.isWellTyped) {
        final errors = typeResult.errors.map((e) => '  ${e.message} at line ${e.line}').join('\n');
        if (strictTypes) {
          throw Exception('Type checking failed:\n$errors');
        }
        // Non-strict mode: print warning and continue
        print('[TYPE WARNING] Type errors found:\n$errors');
      }
    }

    // Compile
    final program = _compiler.compile(source);
    _loadedPrograms[name] = program;

    final moduleInfo = _extractModuleInfo(source, program, name);
    _loadedModules[moduleInfo.name] = moduleInfo;

    return true;
  }

  /// Load an entire project directory via static linking.
  ///
  /// Discovers all modules, type-checks each independently, links into a
  /// single flat program, and compiles it. The result is loaded as a single
  /// program accessible via `combinedProgram`.
  ///
  /// [projectDir] is the path to the project root directory.
  /// [topModuleName] specifies the top module (for entry point aliases).
  ///   If null, auto-detects (the module with the most procedures).
  bool loadProject(String projectDir, {String? topModuleName}) {
    final modules = discoverProject(projectDir,
        rootSelfGlpPath: _rootSelfGlpPath);
    if (modules.isEmpty) {
      throw Exception('No modules found in $projectDir');
    }

    typeCheckProject(modules);

    // Auto-detect top module: prefer the orchestrator (has imported procedures)
    final top = topModuleName ?? _detectTopModule(modules);

    final linked = linkProject(modules, top);
    final program = _compiler.compileProgram(
      linked.program,
      procDeclarations: linked.procDeclarations,
    );
    _loadedPrograms['__project__'] = program;

    return true;
  }

  /// Detect the top module in a project.
  ///
  /// Prefers the module with imported procedure declarations (the orchestrator
  /// that depends on other modules via M#p(...) calls). Falls back to the
  /// module with the most procedures.
  String _detectTopModule(List<DiscoveredModule> modules) {
    final withImports = modules
        .where((m) => m.ast.procDeclarations.any((d) => d.imported))
        .toList();
    if (withImports.length == 1) {
      return withImports.first.moduleName;
    }
    // Fallback: module with the most procedures
    modules.sort(
        (a, b) => b.ast.procedures.length.compareTo(a.ast.procedures.length));
    return modules.first.moduleName;
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

  /// Enable madGLP mode for this engine.
  ///
  /// Loads madGLP system predicates (send_to_net, global_send, send_to_user)
  /// and creates MadContext for message routing.
  void enableMadGLP({required String agentId}) {
    loadSource(_madPredicatesSource, filename: '__mad_predicates__');
    madContext = MadContext(agentId: agentId, runtime: _runtime);
    // Make madContext accessible from body kernels via runtime
    _runtime.madContext = madContext;
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

    // Imports are no longer declared via -import(). Cross-module calls use Module # Goal.
    final imports = <String>[];

    return ModuleInfo(name: name, program: program, imports: imports);
  }

  String _moduleNameFromFilename(String filename) {
    final baseName = filename.split('/').last;
    if (baseName.endsWith('.glp')) {
      return baseName.substring(0, baseName.length - 4);
    }
    return baseName;
  }

  /// Walk up from the file's directory to find the topmost directory
  /// containing self.glp.
  String? _findProjectRoot(String filePath) {
    var dir = File(filePath).parent;
    String? root;
    while (true) {
      final selfGlp = File('${dir.path}/self.glp');
      if (selfGlp.existsSync()) {
        root = dir.path;
      }
      final parent = dir.parent;
      if (parent.path == dir.path) break; // filesystem root
      dir = parent;
    }
    return root;
  }

  /// Build prelude + chain scope (WITHOUT the target module — checkModule
  /// adds that via buildTypeEnvironment).
  TypeEnvironment _buildAncestorScope(List<String> chain) {
    var env = buildPreludeEnvironment();

    // Include root self.glp (programs/self.glp) as first scope layer
    final rootSelfGlp = File(_rootSelfGlpPath);
    if (rootSelfGlp.existsSync()) {
      env = _mergeModuleIntoEnv(env, rootSelfGlp.readAsStringSync());
    }

    for (final selfGlpPath in chain) {
      // Skip if this chain entry IS the root self.glp (avoid double-merging)
      if (File(selfGlpPath).absolute.path == rootSelfGlp.absolute.path) {
        continue;
      }
      env = _mergeModuleIntoEnv(env, File(selfGlpPath).readAsStringSync());
    }
    return env;
  }

  /// Parse GLP source and merge its types/procedures into an environment.
  TypeEnvironment _mergeModuleIntoEnv(TypeEnvironment env, String source) {
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final selfModule = parser.parseModule();

    // Extract templates before expansion removes them.
    // These chain to downstream modules for expansion of ancestor templates.
    final selfTemplates = <String, TypeDef>{};
    for (final td in selfModule.typeDefs) {
      if (td.isParameterized) {
        selfTemplates[td.name] = td;
      }
    }

    // Expand parameterized types (strips templates, keeps monomorphic defs)
    // Pass existing env type names so prelude types aren't mistaken for type params.
    // Pass ancestor templates so this module can expand references to them.
    final expandedModule = expandParameterizedTypes(selfModule,
        knownTypeNames: env.types.keys.toSet(),
        externalTemplates: env.typeTemplates);

    final types = <String, TypeDef>{};
    for (final t in expandedModule.typeDefs) {
      types[t.name] = t;
    }
    final procs = <String, ProcDecl>{};
    for (final p in expandedModule.procDeclarations) {
      procs[p.qualifiedKey] = p;
    }
    final paramProcs = <String, ProcDecl>{};
    for (final p in expandedModule.paramProcDecls) {
      paramProcs[p.qualifiedKey] = p;
    }
    return env.merge(TypeEnvironment(types, procs,
        paramProcDecls: paramProcs,
        typeTemplates: selfTemplates));
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
