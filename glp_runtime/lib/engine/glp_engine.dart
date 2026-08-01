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
import 'package:glp_runtime/compiler/primitive_layer.dart';
import 'package:glp_runtime/compiler/error.dart' show CompileError;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart' as tdfa;
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart' as wtc;
import 'package:glp_runtime/runtime/module_hierarchy.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/wire/flattening.dart'
    show
        canonicalPrint,
        exportDeclarationText,
        hashOfPrint,
        interfaceTypeDefsText;
import 'package:glp_runtime/wire/artefact.dart' show Artefact, ArtefactExport;

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
  final bool hasExports;
  final Set<String> exportedLabels;  // e.g., {'append/3', 'member/2'}

  /// True when the source has no `-module(...)` directive.
  /// Top-level programs (boot files, user programs) have all labels visible.
  /// Only explicitly declared modules have export-boundary filtering.
  final bool isTopLevel;

  ModuleInfo({required this.name, required this.program, required this.imports, required this.hasExports, this.exportedLabels = const {}, this.isTopLevel = false});
}

/// madGLP system predicates (embedded).
///
/// Provides send_to_net/1, send_to_remote/2, global_send/3, authorise_link/2,
/// send_to_user/1.
/// Loaded by enableMadGLP().
const String _madPredicatesSource = r'''
-mode(system).  %% Uses reserved constants like '_w' and '_send'

%% madGLP System Predicates
%% See: madGLP-spec.md Section 4 and Section 12

%% send_to_net/1 - Process network output stream
procedure send_to_net(Stream(_)?).
send_to_net([msg(Q, T) | In]) :- ground(Q?) | global_send(msg(Q?, T?), '_w'(Q?, 0), Q?), send_to_net(In?).
send_to_net([]).

%% send_to_remote/2 - Globalize any output stream to a specific remote agent
%% Used for parent-child streams that cross isolate boundaries.
procedure send_to_remote(Constant?, Stream(_)?).
send_to_remote(Agent, [Msg | In]) :- ground(Agent?), ground(Msg?) | global_send(Msg?, '_w'(Agent?, 0), Agent?), send_to_remote(Agent?, In?).
send_to_remote(_, []).

%% global_send/3 - Send via global link
procedure global_send(_?, _?, _?).
global_send(T, G, Q) :- known(T?) | '_send'(T?, G?, Q?).

%% send_to_user/1 is defined in the root self.glp (always loaded), so it is not
%% repeated here.  sign/2 and authorise_link/2 are likewise defined there, under
%% the ATTESTATION AND HELD LINKS heading, and are no longer repeated here: the
%% copies that stood here were a stale duplicate of the root definitions.  Both
%% kernels abort outside madGLP mode, so a call to sign/2 with madGLP disabled is
%% a runtime abort naming madGLP mode rather than a compile-time undefined
%% procedure.

%% valid_attestation/4 is a guard, not a wrapped body kernel — it is built into
%% the runtime guard machinery (seam spec §4 rework note); no GLP wrapper here.
''';

/// GLP Engine - the embeddable core for running GLP programs
class GlpEngine {
  final GlpCompiler _compiler = GlpCompiler();
  final GlpRuntime _runtime = GlpRuntime();

  /// Module VALUE per loaded unit — its artefact (h(M) + code) as the heap
  /// `Module` constant, built at load.
  final Map<String, rt.ModuleTerm> _loadedModuleValues = {};

  /// The loaded app's module value — what a REPL goal carries. Root self.glp is
  /// not an app, so it never sets this.
  rt.ModuleTerm? _appModule;

  /// The loaded app's module value: its artefact — h(M) and code — as the heap
  /// `Module` constant `self_module` returns. Null if no app unit is loaded.
  rt.ModuleTerm? get appModule => _appModule;
  final Map<String, BytecodeProgram> _loadedPrograms = {};
  final Map<String, ModuleInfo> _loadedModules = {};

  /// Cumulative type environment for checking REPL goals against the body part
  /// of Definition def:well-typed-clause (TGLP glp-semantics: a goal is
  /// well-typed iff well-typed as a body). Lazily seeded with the root scope +
  /// root self.glp, then extended with every loaded module/program. A goal is
  /// checked against this env before it runs; see [_checkGoalWellTyped].
  TypeEnvironment? _goalCheckEnv;

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

  /// Constructor - registers standard predicates and loads root self.glp.
  ///
  /// [rootSelfGlpPath] is the absolute path to programs/self.glp.
  /// Loading root self.glp is not optional — it's part of engine initialization.
  GlpEngine({required String rootSelfGlpPath}) {
    _rootSelfGlpPath = rootSelfGlpPath;

    // Set root scope sources from programs/self.glp for PE and type checker
    final rootSelfFile = File(_rootSelfGlpPath);
    if (rootSelfFile.existsSync()) {
      final rootSource = rootSelfFile.readAsStringSync();
      setRootScopeUnitClauseSource(rootSource);
      setRootScopeEnvironmentSource(rootSource);
    }

    registerStandardPredicates(_runtime.systemPredicates);
    registerModuleKernels(_runtime);
    _loadRootSelf();
  }

  /// Clear all loaded programs except root self.glp.
  ///
  /// Useful for test scripts that need to reset state between tests
  /// without restarting the REPL process.
  void clear() {
    // Remember root self.glp program
    BytecodeProgram? rootSelf = _loadedPrograms['__root_self__'];

    // Clear everything
    _loadedPrograms.clear();
    _loadedModules.clear();
    // Re-seed lazily to root scope + root self.glp on next goal check.
    _goalCheckEnv = null;

    // Restore root self.glp
    if (rootSelf != null) {
      _loadedPrograms['__root_self__'] = rootSelf;
    }
  }

  /// Load root self.glp (private — called by constructor).
  void _loadRootSelf() {
    final file = File(_rootSelfGlpPath);
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

    // Enforce "Admission to the Primitive Layer" (Rule A / Rule B) at load time.
    enforcePrimitiveLayer(
        File(name).existsSync() ? name : null, module, _rootSelfGlpPath);

    // A self-contained module (no cross-module call `M#p`, no `imported`
    // declaration) loaded from a real file IS a program (modules.tex §Design):
    // it is compiled through the SAME pipeline as a directory program — step-3
    // renaming included (every procedure becomes M:p), so each module's calls
    // resolve in its own scope and the loaded module never hijacks an ancestor
    // self.glp's internal call to a same-named procedure. Every procedure of the
    // module is an entry point (§Static Linking), reached by an unqualified
    // alias that shadows the root self.glp for a posted goal (see
    // combinedProgram). There is no dynamic-dispatch activation (path retired).
    //
    // The two internal sources — the madGLP prelude and the root self.glp — are
    // loaded by name rather than from the program hierarchy and cross-call
    // nothing, so the program test below does not apply to them.
    final isInternal =
        name == '__mad_predicates__' || name == '__root_self__';
    final isRealFile =
        !isInternal && name != '_source_' && File(name).existsSync();
    final selfContained = isInternal || _isSelfContained(module);

    // A source that is not self-contained is not a program at all: def:program
    // (modules.tex) admits a self-contained module or a directory with a
    // self.glp, and a loose source carrying an unresolved M#p is neither. Reject
    // it here rather than let it fall through to the direct compile path below,
    // which is what remains of the retired dynamic-dispatch path: that path
    // emits the retired Distribute instruction and the load appears to succeed,
    // failing only at run time as "WireFormatException: instruction not in the
    // wire ISA: Distribute". Composing several modules is by directory program;
    // composing several apps is by module values posted with run/2.
    //
    // The test covers source text as well as a real file: the per-isolate
    // loaders (multiagent/agent_runtime.dart, multiagent/isolate_manager.dart)
    // hand boot sources to `loadSource` under a synthetic name, and a `#` call
    // in one reached the run-time WireFormatException by exactly the route this
    // rejection was written to close.
    if (!selfContained) {
      throw CompileError(
        "'$name' is not a program: it ${_notSelfContainedCause(module)}. By "
        "def:program a program is a self-contained module or a directory with a "
        "self.glp, so a source with cross-module calls is not one. Load the "
        "directory that holds this module as a directory program — the linker "
        "then resolves its cross-module calls at compile time.",
        module.line,
        module.column,
        phase: 'loader',
      );
    }

    // Ancestor self.glp chain per modules.tex §Scope construction, anchored at
    // the hierarchy root (programs/) — the same discoverSelfChain bound as the
    // linker and directory loads. Reused below for the goal-check environment.
    List<String> chain = const [];
    if (isRealFile) {
      chain = discoverSelfChain(
          targetFile: name,
          rootDir: File(name).parent.path,
          programsDir: File(_rootSelfGlpPath).parent.absolute.path);
    }
    TypeEnvironment? ancestorScope;
    if (chain.isNotEmpty) {
      ancestorScope =
          buildAncestorScope(chain: chain, rootSelfGlpPath: _rootSelfGlpPath);
    }

    // Type check if program has procedure declarations. (Single-file/REPL
    // semantics: a parametric procedure inspecting its parameter with no
    // instantiation is rejected — checkModule's default.)
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

    // Compile. A self-contained module on disk goes through the linker (step-3
    // renaming, singleModulePath marks the loaded module so all its procedures
    // are entry points) and compileProgram — the same compiler entry as a
    // directory program — running the global SRSW pass it has no separate
    // per-module pass for. Source text has no file for the linker to discover,
    // so it keeps the direct compile path, as do the internal sources.
    final BytecodeProgram program;
    rt.ModuleTerm? moduleValue;
    if (isRealFile) {
      final modules =
          discoverSingleModule(name, rootSelfGlpPath: _rootSelfGlpPath);
      final linked =
          linkProgram(modules,
              rootDir: File(name).parent.path, singleModulePath: name);
      program = _compiler.compileProgram(linked.program,
          procDeclarations: linked.procDeclarations, skipGlobalSRSW: false);
      // This unit's module value — its artefact: h(M) + code.
      moduleValue = _moduleValueOf(_baseName(name), program, linked, modules);
    } else {
      program = _compiler.compile(source);
    }
    _loadedPrograms[name] = program;
    if (moduleValue != null) {
      _loadedModuleValues[name] = moduleValue;
      _appModule = moduleValue;
    }

    final moduleInfo = _extractModuleInfo(source, program, name);
    _loadedModules[moduleInfo.name] = moduleInfo;

    // Goal-check environment per modules.tex §Scope construction — the chain
    // is anchored at the hierarchy root (programs/), not at the file loaded
    // for execution (§Implicit ancestor scoping): layer every self.glp on the
    // path from programs/ down to the module's directory (the chain computed
    // above), later shadowing earlier, before the module's own definitions.
    if (isRealFile) {
      var goalEnv = _ensureGoalCheckBaseEnv();
      for (final selfGlpPath in chain) {
        goalEnv = mergeSelfGlpFileIntoScope(goalEnv, selfGlpPath);
      }
      _goalCheckEnv = goalEnv;
    }

    // Make this module's declarations available to the REPL goal checker.
    _extendGoalCheckEnv(module);

    return true;
  }

  /// Load an entire program directory via static linking.
  ///
  /// Discovers all modules, type-checks each independently, links into a
  /// single flat program, and compiles it. The result is loaded as a single
  /// program accessible via `combinedProgram`.
  ///
  /// [programDir] is the path to the program root directory. Entry-point
  /// aliases are generated for the exported procedures of root-level modules
  /// only (project-compilation spec §3.4).
  bool loadProgram(String programDir) {
    final modules = discoverProgram(programDir,
        rootSelfGlpPath: _rootSelfGlpPath);
    if (modules.isEmpty) {
      throw Exception('No modules found in $programDir');
    }

    // Gate (paper: modules §Static Linking — only a well-typed program is
    // compiled and run): checkedLinkedProgram type-checks the linked program and
    // returns it for compilation only if well-typed, else throws. There is no
    // other path to a compiled program.
    final linked = checkedLinkedProgram(modules, rootDir: programDir);
    final program = _compiler.compileProgram(
      linked.program,
      procDeclarations: linked.procDeclarations,
    );
    _loadedPrograms['__program__'] = program;

    // The program's module value — its artefact (h(M) + code): the value
    // `self_module` returns and a friend adopts.
    final moduleValue =
        _moduleValueOf(_baseName(programDir), program, linked, modules);
    _loadedModuleValues['__program__'] = moduleValue;
    _appModule = moduleValue;

    // Goal-check environment per modules.tex §Scope construction — the chain
    // is anchored at the hierarchy root (programs/), not at the directory
    // loaded for execution (§Implicit ancestor scoping). The base env carries
    // the root scope + root self.glp; layer every self.glp on the path from
    // programs/ down to the program root, later shadowing earlier — the same
    // chain the linker uses — then the program's own modules (loop below).
    var goalEnv = _ensureGoalCheckBaseEnv();
    var programRoot = Directory(programDir).absolute.path;
    while (programRoot.endsWith(Platform.pathSeparator)) {
      programRoot = programRoot.substring(0, programRoot.length - 1);
    }
    for (final selfGlpPath in discoverSelfChain(
        targetFile: '$programRoot${Platform.pathSeparator}self.glp',
        rootDir: programRoot,
        programsDir: File(_rootSelfGlpPath).parent.absolute.path)) {
      goalEnv = mergeSelfGlpFileIntoScope(goalEnv, selfGlpPath);
    }
    _goalCheckEnv = goalEnv;

    // Make the program's module declarations available to the REPL goal checker.
    for (final m in modules) {
      _extendGoalCheckEnv(m.ast);
    }

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

      // Reject an ill-typed goal before running it. Soundness of well-typing
      // (TGLP glp-semantics, Theorem thm:soundness) holds for runs from a
      // well-typed initial goal; a goal is well-typed iff well-typed as a body.
      final typeError = _checkGoalWellTyped(trimmed);
      if (typeError != null) {
        return ExecutionResult(
          status: ExecutionStatus.failed,
          error: typeError,
        );
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

  /// Get the combined bytecode program from all loaded sources.
  ///
  /// Returns the unfiltered merged program: every loaded label is present in
  /// `labels`. The runtime relies on this for intra-module body-call
  /// resolution — `Spawn(name/arity)` opcodes emitted by the compiler for
  /// same-module body calls (including calls to private helpers, spec §4.1)
  /// must resolve here.
  ///
  /// Module export boundaries (spec §4.1: private procedures visible only
  /// within their module and descendants) are enforced separately, at REPL
  /// entry-point lookup sites, via [_replEntryPointLabels]. Cross-module
  /// calls go through `Distribute`/`Transmit`, not `prog.labels`, so the
  /// boundary is not weakened by leaving `labels` unfiltered.
  BytecodeProgram get combinedProgram {
    // Root self.glp goes LAST so its primitives are the FALLBACK: label
    // indexing keeps the first occurrence, so a loaded module's own definition
    // (e.g. its merge/3) shadows the root's primitive of the same name (manual
    // §19.6: a module's definition shadows every ancestor's; modules.tex
    // §Static Linking step 3). Other loaded programs keep their insertion order.
    final allOps = <dynamic>[];
    for (final entry in _loadedPrograms.entries) {
      if (entry.key == '__root_self__') continue;
      allOps.addAll(entry.value.ops);
    }
    final rootSelf = _loadedPrograms['__root_self__'];
    if (rootSelf != null) allOps.addAll(rootSelf.ops);
    return BytecodeProgram(allOps);
  }

  /// Labels addressable as REPL entry points, per spec §4.1.
  ///
  /// The REPL is outside any module, so it can only invoke:
  ///   - All labels in root self.glp (ancestor scoping)
  ///   - All labels in a linked program (the linker has already encoded
  ///     export boundaries via name mangling and alias clauses)
  ///   - All labels of top-level programs (no `-module` directive)
  ///   - Only `exportedLabels` of explicitly declared modules
  Set<String> _replEntryPointLabels() {
    final labels = <String>{};
    final rootSelf = _loadedPrograms['__root_self__'];
    if (rootSelf != null) labels.addAll(rootSelf.labels.keys);
    final program = _loadedPrograms['__program__'];
    if (program != null) labels.addAll(program.labels.keys);
    for (final moduleInfo in _loadedModules.values) {
      if (moduleInfo.isTopLevel) {
        labels.addAll(moduleInfo.program.labels.keys);
      } else {
        labels.addAll(moduleInfo.exportedLabels);
      }
    }
    return labels;
  }

  // ============ Private Methods ============

  /// A module is self-contained (modules.tex §Design) if it makes no
  /// cross-module call `M#p` and declares no `imported procedure`. Such a module
  /// is a program in its own right and is linked/compiled like a directory.
  bool _isSelfContained(Module module) {
    if (module.procDeclarations.any((d) => d.imported)) return false;
    for (final proc in module.procedures) {
      for (final clause in proc.clauses) {
        for (final g in clause.body ?? const <Goal>[]) {
          if (_containsRemoteGoal(g)) return false;
        }
      }
    }
    return true;
  }

  /// Which clause of self-containment [module] fails, for the load-time
  /// rejection above. An `imported procedure` declaration is named first because
  /// it names the dependency; otherwise the procedure holding the first
  /// cross-module call is named.
  String _notSelfContainedCause(Module module) {
    for (final d in module.procDeclarations) {
      if (d.imported) {
        final target = d.modulePath == null ? d.key : '${d.modulePath}#${d.key}';
        return "declares 'imported procedure $target'";
      }
    }
    for (final proc in module.procedures) {
      for (final clause in proc.clauses) {
        for (final g in clause.body ?? const <Goal>[]) {
          if (_containsRemoteGoal(g)) {
            return 'makes a cross-module call in ${clause.head.functor}/'
                '${clause.head.args.length}';
          }
        }
      }
    }
    return 'is not self-contained';
  }

  bool _containsRemoteGoal(Goal g) {
    if (g is RemoteGoal) return true;
    if (g is SpawnGoal) return _containsRemoteGoal(g.innerGoal);
    return false;
  }

  /// Type-check a REPL goal against the loaded program's declarations.
  ///
  /// Returns null if the goal is well-typed (or cannot be parsed/checked here,
  /// in which case the execution path reports the parse problem). Returns a
  /// specific error message if the goal is ill-typed.
  ///
  /// The goal is parsed as a clause body so single goals and conjunctions are
  /// handled uniformly; a guard (if the user wrote one) is a body goal for
  /// type-checking, as in checkClauseFromAst. The check is the body part of
  /// Definition def:well-typed-clause; see [wtc.checkGoal].
  String? _checkGoalWellTyped(String trimmed) {
    final List<Goal> atoms;
    try {
      final parseInput = '_glp_query_ :- $trimmed.';
      final lexer = Lexer(parseInput);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final parsed = parser.parse();
      if (parsed.procedures.isEmpty || parsed.procedures[0].clauses.isEmpty) {
        return null;
      }
      final clause = parsed.procedures[0].clauses[0];
      atoms = [
        for (final g in clause.guards ?? const <Guard>[])
          Goal(g.predicate, g.args, g.line, g.column),
        ...?clause.body,
      ];
    } catch (_) {
      // A parse error surfaces in the execution path with its own message.
      return null;
    }
    if (atoms.isEmpty) return null;

    final env = _ensureGoalCheckBaseEnv();
    final dfa = tdfa.buildProgramDFA(env);
    final result = wtc.checkGoal(atoms, dfa, env);
    if (result.isWellTyped) return null;

    final detail = result.errors.map((e) => '  ${e.message}').join('\n');
    return 'Goal is not well-typed:\n$detail';
  }

  /// Build the [ByteRunner] and entry BYTE OFFSET for a query over a
  /// [CodeImage] of the program. The caller has already verified the entry
  /// exists (the REPL entry-point guard), so an unresolved offset here is an
  /// internal invariant violation between the object labels and the image
  /// symbols, not a user error.
  (GoalRunner, int) _runnerForQuery(
      BytecodeProgram program, String procedureLabel) {
    final image = codeImageFromProgram(program);
    final off = image.entryOffsetOf(procedureLabel);
    if (off == null) {
      throw StateError('no compiled byte entry for $procedureLabel');
    }
    return (ByteRunner(image), off);
  }

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

    if (entryPC == null || !_replEntryPointLabels().contains(procedureLabel)) {
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
    // The goal carries its module value — the loaded app's artefact (h(M) +
    // code) — read back by `self_module`.
    if (_appModule != null) {
      _runtime.setGoalModule(_goalId, _appModule);
    }

    final module = _findModuleForProcedure(procedureLabel);
    if (module != null) {
      final modCtx = _buildModuleContext(module, program);
      if (modCtx != null) {
        _runtime.setGoalModuleContext(_goalId, modCtx);
      }
    }

    final (runner, goalEntry) =
        _runnerForQuery(program, procedureLabel);
    final scheduler = Scheduler(rt: _runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();
    scheduler.setQueryVarNames(queryVarWriters);

    _runtime.gq.enqueue(GoalRef(_goalId, goalEntry));
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

    // Build one CodeImage + ByteRunner for the whole conjunction; per-goal
    // entry PCs are byte offsets resolved below.
    final image = codeImageFromProgram(program);
    final GoalRunner runner = ByteRunner(image);
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
      if (entryPC == null || !_replEntryPointLabels().contains(procedureLabel)) {
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
      // The goal carries its module value — the loaded app's artefact (h(M) +
      // code) — read back by `self_module`.
      if (_appModule != null) {
        _runtime.setGoalModule(_goalId, _appModule);
      }

      final module = _findModuleForProcedure(procedureLabel);
      if (module != null) {
        final modCtx = _buildModuleContext(module, program);
        if (modCtx != null) {
          _runtime.setGoalModuleContext(_goalId, modCtx);
        }
      }

      scheduler.setQueryVarNames(queryVarWriters);
      final goalEntry = image.entryOffsetOf(procedureLabel)!;
      _runtime.gq.enqueue(GoalRef(_goalId, goalEntry));
      _goalId++;

      final result = await scheduler.drainAsyncWithStatus(
        maxCycles: maxCycles,
        debug: debugTrace,
        showBindings: false,
        debugOutput: debugOutput,
      );

      // A failed conjunct does not end the query. The conjuncts of a posted
      // goal are one resolvent of independent goals, and Fail advances the
      // queue and continues (dGLP/madGLP Reduce): stopping here was the same
      // defect as the scheduler's break, one level up, and it was what dropped
      // the siblings of a failed conjunct at the REPL. Suspension already
      // continued; failure now does too, and the query is reported failed.
      if (result.status == ExecutionStatus.failed) {
        allSucceeded = false;
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

  static String _baseName(String path) {
    final parts = path.split('/').where((s) => s.isNotEmpty);
    return parts.isEmpty ? path : parts.last;
  }

  /// The module VALUE of a freshly linked unit: its compiled artefact — h(M)
  /// and code — wrapped as the heap `Module` constant (IGLP appendix
  /// §Self-Module: "the Module constant carries the artefact ... not code
  /// alone", since the adopter checks h(M) against the offer).
  ///
  /// h(M) is the source identity: SHA-256 of the canonical print of the linked,
  /// pruned program (code-format §Deterministic Flattening). It is computed
  /// here from the already-linked program rather than through `flattenProject`,
  /// which would re-run discovery and linking from disk.
  rt.ModuleTerm _moduleValueOf(
    String moduleName,
    BytecodeProgram program,
    LinkResult linked,
    List<DiscoveredModule> modules,
  ) {
    final typeDefs = <String, TypeDef>{};
    for (final mod in modules) {
      for (final td in mod.ast.typeDefs) {
        typeDefs.putIfAbsent(td.name, () => td);
      }
    }
    final hM = hashOfPrint(canonicalPrint(
      program: linked.program,
      procDeclarations: linked.procDeclarations,
      typeDefs: typeDefs.values.toList(),
    ));
    // The artefact's interface table: the module's exports are its entry
    // points — the linked program's bare (unprefixed) procedures (modules.tex
    // sec:static-linking; the DCE seed): a directory program's root-self.glp
    // exported procedures (the aliases), a single-module program's every
    // procedure. `run/2` admits a posted goal only against this set.
    //
    // Each export carries its declaration text, and the table carries the type
    // definitions those declarations reach, so the loader derives the exported
    // type automata from the artefact itself (code format §Program Artefact:
    // "Carrying text rather than compiled automata keeps one source of truth").
    // The linker keeps the entry points' declarations bare, alongside the
    // renamed `M:p` declarations of everything else, so the bare ones are the
    // interface's.
    final declByKey = <String, ProcDecl>{
      for (final d in linked.procDeclarations)
        if (!d.name.contains(':')) '${d.name}/${d.arity}': d,
    };
    final exports = <ArtefactExport>[];
    final exportDecls = <ProcDecl>[];
    for (final p in linked.program.procedures) {
      if (p.name.contains(':')) continue;
      final decl = declByKey['${p.name}/${p.arity}'];
      // An export with no declaration contributes no interface text; it is
      // undeclared in the source too, so there is nothing to derive from.
      if (decl != null) exportDecls.add(decl);
      exports.add(ArtefactExport(
          p.name, p.arity, decl == null ? '' : exportDeclarationText(decl)));
    }
    final artefact = Artefact.fromCompiled(
      ops: program.ops.cast<Object>(),
      hM: hM,
      moduleName: moduleName,
      isaVersion: 'glp-isa-1',
      typeDefsText:
          interfaceTypeDefsText(exportDecls: exportDecls, typeDefs: typeDefs),
      exports: exports,
    );
    return rt.ModuleTerm(artefact, name: moduleName);
  }

  ModuleInfo _extractModuleInfo(
      String source, BytecodeProgram program, String filename) {
    // -module removed: a module's name is its file (or, for self.glp, its
    // directory). Every loaded module is top-level — a single-module program
    // exports all its procedures (modules.tex sec:static-linking).
    final baseName = filename.split('/').last;
    final String name;
    if (baseName == 'self.glp') {
      final segs = filename.split('/');
      name = segs.length >= 2 ? segs[segs.length - 2] : 'self';
    } else {
      name = _moduleNameFromFilename(filename);
    }
    const isTopLevel = true;

    // Extract imported module names from `imported procedure Module#Proc(...)` declarations.
    // The order of unique module names determines the import index (1-based),
    // matching the compiler's ImportTable.addImport() order.
    final imports = <String>[];
    final importPattern = RegExp(r'imported\s+procedure\s+(\w+)#');
    for (final match in importPattern.allMatches(source)) {
      final moduleName = match.group(1)!;
      if (!imports.contains(moduleName)) {
        imports.add(moduleName);
      }
    }

    // Detect exported procedures from `exported procedure` declarations.
    // Extract functor names, then find matching labels in the compiled program.
    final exportedLabels = <String>{};
    final exportPattern = RegExp(r'exported\s+procedure\s+(\w+)\s*\(');
    for (final match in exportPattern.allMatches(source)) {
      final functor = match.group(1)!;
      // Find the label with this functor (functor/arity format)
      for (final label in program.labels.keys) {
        if (label.startsWith('$functor/')) {
          exportedLabels.add(label);
        }
      }
    }
    final hasExports = exportedLabels.isNotEmpty;

    return ModuleInfo(name: name, program: program, imports: imports, hasExports: hasExports, exportedLabels: exportedLabels, isTopLevel: isTopLevel);
  }

  String _moduleNameFromFilename(String filename) {
    final baseName = filename.split('/').last;
    if (baseName.endsWith('.glp')) {
      return baseName.substring(0, baseName.length - 4);
    }
    return baseName;
  }

  /// Seed (once) and return the base goal-check environment: the root scope
  /// plus root self.glp (buildAncestorScope with an empty chain).
  TypeEnvironment _ensureGoalCheckBaseEnv() {
    _goalCheckEnv ??=
        buildAncestorScope(chain: const [], rootSelfGlpPath: _rootSelfGlpPath);
    return _goalCheckEnv!;
  }

  /// Extend the goal-check environment with a loaded module's declarations, so
  /// goals referencing its procedures can be type-checked.
  void _extendGoalCheckEnv(Module module) {
    _goalCheckEnv = mergeModuleIntoScope(_ensureGoalCheckBaseEnv(), module);
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
