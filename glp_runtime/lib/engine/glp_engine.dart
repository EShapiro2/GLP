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
import 'package:path/path.dart' as ppath;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/primitive_layer.dart';
import 'package:glp_runtime/compiler/error.dart' show CompileError;
import 'package:glp_runtime/bytecode/opcodes.dart' show Op;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/param_expansion.dart'
    show UndefinedDeclarationTypeError;
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
import 'package:glp_runtime/wire/artefact.dart'
    show Artefact, ArtefactExport, glpIsaVersion;
import 'package:glp_runtime/analysis/type_checker/type_identity.dart'
    show TypeIdentityTables;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/compiler/certification.dart'
    show privilegedRootNames, privilegedCalls;

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

  /// The cycle limit stopped the run with goals still queued: the query did
  /// not finish, and none of the three above is what it did.
  bool get capped => status == ExecutionStatus.capped;
}

/// Module info for tracking loaded modules
class ModuleInfo {
  final String name;
  final BytecodeProgram program;
  final bool hasExports;
  final Set<String> exportedLabels;  // e.g., {'append/3', 'member/2'}

  /// True when the source has no `-module(...)` directive.
  /// Top-level programs (boot files, user programs) have all labels visible.
  /// Only explicitly declared modules have export-boundary filtering.
  final bool isTopLevel;

  ModuleInfo({required this.name, required this.program, required this.hasExports, this.exportedLabels = const {}, this.isTopLevel = false});
}

/// madGLP system predicates (embedded).
///
/// Provides send_to_net/1, send_to_remote/2, global_send/3, authorise_link/2,
/// send_to_user/1.
/// Loaded by enableMadGLP().
const String _madPredicatesSource = r'''
-mode(system).  %% Uses reserved constants like '_w' and '_send'

%% madGLP System Predicates
%% See: IGLP Definition global_send Predicate and Remark Network Output
%% Processing

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
%% the runtime guard machinery; no GLP wrapper here.
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

  /// The defining clauses, defined guards unfolded, of the procedures the units
  /// loaded so far define, by the "name/arity" the goal-check environment
  /// declares them under, the internal sources excepted: a posted goal's
  /// calls read their callee's clauses as a clause's calls do --- the types
  /// they fix for a parameter are tried, and a binding is to make them
  /// well-typed (TGLP appendix-implementation-notes.tex, "The instantiation of
  /// a call", cc4a891; [_checkGoalWellTyped]).
  final Map<String, List<Clause>> _goalClauses = {};

  /// Whether a procedure of [_goalClauses] is parametrically well-typed, asked
  /// once per key and load ([_goalProcedureIsParametric]).
  final Map<String, bool> _goalParametric = {};

  /// The self.glp files [_goalCheckEnv] carries as scope-chain entries, by
  /// absolute path. [scopeFor] layers a file's own ancestor chain on the scope
  /// and skips these, so a self.glp the program's chain already contributed is
  /// not merged a second time over the program's own modules.
  final Set<String> _scopeSelfGlps = {};

  /// Max execution cycles (default 10000)
  int maxCycles = 10000;

  /// Enable trace output (reductions)
  bool debugTrace = false;

  /// Enable debug output
  bool debugOutput = false;

  /// Path to the root self.glp (programs/self.glp) for the type scope chain.
  late final String _rootSelfGlpPath;

  /// The root: the directory of the root self.glp, which every module is
  /// named from (TGLP modules.tex, Compilation, third step; [modulePathName]).
  String get _rootDir => File(_rootSelfGlpPath).parent.absolute.path;

  /// For madGLP: the MadContext for this engine
  MadContext? madContext;

  /// The person's identity the runtime holds (Secure GLP §Assumptions: `sign`
  /// succeeds only under a key whose private half the runtime holds for its
  /// own person). It signs every certificate this engine's compiler writes and
  /// backs `self_key/1` and `sign/3`. Given at construction by a harness that
  /// also installs it on the agent's networking layer; generated fresh
  /// otherwise (glpc, the suite), where the paper names no key.
  PersonIdentity get identity => _runtime.identity!;

  /// The privileged names of the root scope — the kernels and predicates that
  /// reach the network or the person, and every root-scope procedure from
  /// which one is reachable — computed once from the root self.glp and the
  /// madGLP system predicates (compiler/certification.dart).
  late final Set<String> _privilegedRootNames;

  /// Access to the runtime (for madGLP integration)
  GlpRuntime get runtime => _runtime;

  /// Access to loaded programs
  Map<String, BytecodeProgram> get loadedPrograms =>
      Map.unmodifiable(_loadedPrograms);

  /// Constructor - registers standard predicates and loads root self.glp.
  ///
  /// [rootSelfGlpPath] is the absolute path to programs/self.glp.
  /// Loading root self.glp is not optional — it's part of engine initialization.
  /// The absolute path to programs/self.glp, for callers that build a scope of
  /// their own — the vGLP emitter does.
  String get rootSelfGlpPath => _rootSelfGlpPath;

  GlpEngine({required String rootSelfGlpPath, PersonIdentity? identity}) {
    _rootSelfGlpPath = rootSelfGlpPath;
    _runtime.identity = identity ?? PersonIdentity.generate();

    // Set root scope sources from programs/self.glp for PE and type checker
    final rootSelfFile = File(_rootSelfGlpPath);
    final rootSources = <String>[_madPredicatesSource];
    if (rootSelfFile.existsSync()) {
      final rootSource = rootSelfFile.readAsStringSync();
      setRootScopeUnitClauseSource(rootSource);
      setRootScopeEnvironmentSource(rootSource);
      rootSources.add(rootSource);
    }
    _privilegedRootNames = privilegedRootNames(rootSources);

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
    _scopeSelfGlps.clear();

    // Restore root self.glp
    if (rootSelf != null) {
      _loadedPrograms['__root_self__'] = rootSelf;
    }
  }

  /// Load root self.glp (private — called by constructor).
  ///
  /// A failure here is fatal and is raised as one. Every program in the tree
  /// resolves its predefined types and procedures through the root self.glp, so
  /// an engine whose root scope failed to compile fails every subsequent load
  /// for reasons that name nothing: the root scope comes back with no labels —
  /// no merge, no sign — and no diagnostic anywhere. This was swallowed until
  /// 2026-08-01, which made a broken root self.glp the least diagnosable failure
  /// in the system.
  void _loadRootSelf() {
    final file = File(_rootSelfGlpPath);
    if (!file.existsSync()) return;
    final source = file.readAsStringSync();
    final GlpCompiler compiler = GlpCompiler();
    final BytecodeProgram prog;
    try {
      prog = compiler.compile(source);
    } catch (e) {
      throw StateError(
          'root self.glp failed to compile: $_rootSelfGlpPath\n  $e\n'
          'Loading the root self.glp is not optional — it is part of engine '
          'initialization, and every program depends on the scope it defines.');
    }
    _loadedPrograms['__root_self__'] = prog;
    // The root runner: the root self.glp's procedures as a runner of their
    // own, which an activated module's goals reach for a body call to a
    // root-scope procedure --- merge/3, send/3 --- absent from its artefact
    // (engine_v2/interp.dart, _spawnInRoot). A REPL goal still runs on the
    // combined program, where the root is the fallback in one image.
    _runtime.runners['__root__'] =
        ByteRunner(codeImageFromProgram(prog, moduleName: '__root__'));
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
  ///
  /// With [scope], the module is type-checked in that scope instead of the
  /// ancestor self.glp chain of [filename]. This is the boot-source case
  /// (IGLP, Implementation Notes, "The scope a boot source is checked in"): a
  /// boot source is loaded on top of a program already in the engine, so it is
  /// checked in the scope the engine holds when it is handed over --- the
  /// linked program, the kernels the runtime has loaded, and the boot file's
  /// own ancestor chain --- which the loaders obtain from [scope] and
  /// [scopeFor]. A check that sees the ancestor chain alone refuses calls the
  /// engine resolves, a kernel loaded a moment earlier among them; and under a
  /// synthetic name there is no chain at all, so until 2026-09-18 a boot source
  /// was checked in the bare root scope and `send_to_net/1` was undefined in it.
  /// [scope] decides what the source is checked against; whether [filename] is
  /// a real file still decides how it is compiled (linker or direct).
  bool loadSource(String source, {String? filename, TypeEnvironment? scope}) {
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
    // combinedProgram).
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
    // it here, naming the cause and the remedy, rather than let it fall through
    // to the direct compile path below, whose code generator refuses an
    // unresolved M#p without either. Composing several modules is by directory
    // program; composing several apps is by module values posted with run/2.
    //
    // The test covers source text as well as a real file: the multi-isolate
    // loader (multiagent/isolate_manager.dart) hands a boot source to
    // `loadSource` under a synthetic name --- as multiagent/agent_runtime.dart
    // did until it came to load one program and no boot source beside it ---
    // and a `#` call in one reached the run-time WireFormatException by exactly
    // the route this rejection was written to close.
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
    // linker and directory loads. Used below for the goal-check environment.
    List<String> chain = const [];
    List<DiscoveredModule>? discovered;
    if (isRealFile) {
      chain = discoverSelfChain(
          targetFile: name,
          rootDir: File(name).parent.path,
          programsDir: File(_rootSelfGlpPath).parent.absolute.path);
      // The module as the linker discovers it: its ancestor scope with the
      // `-expose`d modules of the directories on its chain merged in
      // (modules.tex, "The -expose directive": an exposed module's exported
      // procedures are in the directory's scope as if defined in its self.glp).
      // The check below and the linker further down share this one discovery.
      discovered = discoverSingleModule(name, rootSelfGlpPath: _rootSelfGlpPath);
    }
    TypeEnvironment? ancestorScope = scope;
    if (ancestorScope == null && discovered != null) {
      // Until 2026-09-18 this was buildAncestorScope(chain) — the self.glp
      // chain alone, without the exposes — so a module calling a procedure its
      // directory's self.glp exposes (agent/4 of programs/tests/agent_roundtrip,
      // send_to_net/1 of system/mad_predicates, exposed by the root) was
      // refused as undefined by the check while the linker resolved it.
      ancestorScope = discovered
          .firstWhere((m) => m.filePath == name, orElse: () => discovered!.first)
          .ancestorScope;
    }

    // Type check, every source: a module with no procedure declarations is
    // checked like any other, and a clause of it then defines a procedure with
    // no declaration, which is an error (TGLP Definition "Typed GLP Program",
    // condition 1).  Until 2026-10-02 such a module was compiled and run with
    // no check.  (Single-file/REPL semantics: a parametric procedure inspecting
    // its parameter with no instantiation is rejected — checkModule's default.)
    final List<Procedure> checkedProcedures;
    {
      final ast = Program(module.procedures, module.line, module.column);
      final partialEvaluator = PartialEvaluator();
      final transformedAst = partialEvaluator.transformDefinedGuards(ast);
      checkedProcedures = transformedAst.procedures;

      final TypeCheckResult typeResult;
      try {
        typeResult = checkModule(module,
            transformedProcedures: transformedAst.procedures,
            ancestorScope: ancestorScope);
      } on UndefinedDeclarationTypeError catch (e) {
        // An undefined type name in a declaration of this source (Moded-Types,
        // "Declaration parameters"), named with its file.
        throw e.inFile(name);
      }
      if (!typeResult.isWellTyped) {
        final errors = typeResult.errors
            .map((e) => '  ${e.message} at line ${e.line}')
            .join('\n');
        // The object typechecked is the object compiled, and no diagnostic on a
        // load path is a warning: a program that does not check does not run.
        // This branch printed '[TYPE WARNING] Type errors found' and carried on
        // whenever `strictTypes` was off, which is how the multi-isolate loaders
        // (multiagent/isolate_manager.dart, multiagent/agent_runtime.dart) ran
        // programs the checker had rejected. There is no flag now: the load
        // fails here, naming the errors, and nothing runs.
        throw CompileError(
          'Type checking failed for \'$name\':\n$errors',
          typeResult.errors.first.line,
          typeResult.errors.first.column,
          phase: 'typecheck',
        );
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
      final modules = discovered!;
      final linked =
          linkProgram(modules,
              rootDir: File(name).parent.path, singleModulePath: name);
      // The object compiled is the LINKED program, so the scope its SRSW
      // relaxations are decided in is the flat module's, not the single
      // module's: the two name their types differently (step-3 renaming).
      program = _compiler.compileProgram(linked.program,
          procDeclarations: linked.procDeclarations,
          typeEnv: linkedProgramEnvironment(linkedFlatModule(modules, linked)));
      // This unit's module value — its artefact: h(M) + code.
      moduleValue = _moduleValueOf(_baseName(name), program, linked, modules,
          directory: File(name).parent.absolute.path);
    } else {
      program = _compiler.compile(source);
    }
    _refuseRedefinitionByLaterLoad(name, program);
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
        goalEnv = mergeSelfGlpFileIntoScope(goalEnv, selfGlpPath,
            root: _rootDir);
        _scopeSelfGlps.add(File(selfGlpPath).absolute.path);
      }
      _goalCheckEnv = goalEnv;
    }

    // Make this module's declarations available to the REPL goal checker,
    // and its clauses to the reading of a goal's calls.
    _extendGoalCheckEnv(module, label: moduleInfo.name);
    if (!isInternal) _addGoalClauses(checkedProcedures);

    return true;
  }

  /// Add the clauses of [procedures] to [_goalClauses], over what an earlier
  /// load gave a key unless [fillGapsOnly], as [_extendGoalCheckEnv] layers
  /// the declarations.
  void _addGoalClauses(List<Procedure> procedures, {bool fillGapsOnly = false}) {
    final byKey = <String, List<Clause>>{};
    for (final p in procedures) {
      for (final c in p.clauses) {
        byKey.putIfAbsent('${c.head.functor}/${c.head.arity}', () => []).add(c);
      }
    }
    for (final e in byKey.entries) {
      if (fillGapsOnly && _goalClauses.containsKey(e.key)) continue;
      _goalClauses[e.key] = e.value;
    }
    _goalParametric.clear();
  }

  /// Whether the procedure a posted goal calls by [key] is parametrically
  /// well-typed (TGLP parameterized-types.tex, Definition "Parametrically
  /// Well-Typed"): a call some parameter of which has no type supplied or
  /// fixed for it is refused unless it is (appendix-implementation-notes.tex,
  /// "The instantiation of a call").  A loaded procedure is certified by its
  /// abstract instance against its own clauses in the goal-check environment;
  /// any other is answered by [rootProcedureIsParametric].
  bool _goalProcedureIsParametric(String key) {
    final clauses = _goalClauses[key];
    if (clauses == null || clauses.isEmpty) {
      return rootProcedureIsParametric(key);
    }
    return _goalParametric[key] ??= () {
      try {
        return certifyParametricProcedures(_ensureGoalCheckBaseEnv(),
                (k) => k == key ? clauses : null)
            .certifiedKeys
            .contains(key);
      } on Object {
        return false;
      }
    }();
  }

  /// A later load that defines a procedure an earlier load defines is an
  /// error, not a definition the earlier one shadows: [combinedProgram] keeps
  /// the first label of each name, so until 2026-10-02 the later load's
  /// procedure of that name was never run and nothing said so.  A program is
  /// one compiled module (TGLP modules.tex, Compilation), and a goal names its
  /// procedure by the plain name an entry point carries, so it is the plain
  /// names that clash; a renamed `M:p` is a module's own.  The root self.glp
  /// is no earlier load --- every program may shadow it (TGLP
  /// appendix-root-self.tex) --- and a load under the name of an earlier one
  /// replaces it.
  void _refuseRedefinitionByLaterLoad(String name, BytecodeProgram program) {
    final internal = RegExp(r'_c\d+$');
    Set<String> plainProcedures(BytecodeProgram p) => {
          for (final l in p.labels.keys)
            if (l.contains('/') &&
                !l.contains(':') &&
                !l.endsWith('_end') &&
                !internal.hasMatch(l))
              l
        };
    final mine = plainProcedures(program);
    for (final e in _loadedPrograms.entries) {
      if (e.key == '__root_self__' || e.key == name) continue;
      final clash = mine.intersection(plainProcedures(e.value));
      if (clash.isEmpty) continue;
      throw CompileError(
        "'$name' defines ${(clash.toList()..sort()).join(', ')}, which the "
        "earlier load '${e.key}' defines: a later load of a same-named "
        'procedure is an error, not shadowed by the earlier one',
        0,
        0,
        phase: 'loader',
      );
    }
  }

  /// The scope the engine holds: the root scope, the root self.glp, every
  /// kernel and unit loaded so far, and the linked program with its ancestor
  /// chain --- the environment a goal posted to the engine is checked in, and
  /// the one a boot source is checked in (see [loadSource]).
  TypeEnvironment get scope => _ensureGoalCheckBaseEnv();

  /// [scope] with the ancestor self.glp chain of the file at [path] layered on
  /// it: the boot file's own chain, per the Implementation Notes. A self.glp
  /// the scope already carries as a chain entry is not merged again, so a boot
  /// file under the program root adds nothing and one in a deeper directory
  /// adds that directory's self.glp.
  TypeEnvironment scopeFor(String path) {
    var env = scope;
    final chain = discoverSelfChain(
        targetFile: path,
        rootDir: File(path).parent.path,
        programsDir: File(_rootSelfGlpPath).parent.absolute.path);
    for (final selfGlpPath in chain) {
      if (_scopeSelfGlps.contains(File(selfGlpPath).absolute.path)) continue;
      env = mergeSelfGlpFileIntoScope(env, selfGlpPath, root: _rootDir);
    }
    return env;
  }

  /// Load an entire program directory via static linking.
  ///
  /// Discovers all modules, type-checks each independently, links into a
  /// single flat program, and compiles it. The result is loaded as a single
  /// program accessible via `combinedProgram`.
  ///
  /// [programDir] is the path to the program root directory, which carries a
  /// self.glp or is refused (TGLP modules.tex, "Entry and the absence of a
  /// boot module").  The entry points are the procedures that self.glp
  /// exports, by definition or by forwarding, and an alias clause is
  /// generated for each (Compilation, fifth step: "the exported procedures of
  /// the program's self.glp ... These exports are the compiled module's entry
  /// points").
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
      typeEnv: linked.checkedEnv,
    );
    _refuseRedefinitionByLaterLoad('__program__', program);
    _loadedPrograms['__program__'] = program;

    // The program's module value — its artefact (h(M) + code): the value
    // `self_module` returns and a friend adopts.
    final moduleValue =
        _moduleValueOf(_baseName(programDir), program, linked, modules,
            directory: Directory(programDir).absolute.path);
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
      goalEnv = mergeSelfGlpFileIntoScope(goalEnv, selfGlpPath,
            root: _rootDir);
      _scopeSelfGlps.add(File(selfGlpPath).absolute.path);
    }
    _goalCheckEnv = goalEnv;

    // Make the program's module declarations available to the REPL goal checker,
    // IN SCOPE ORDER (modules.tex §Scope construction: root-first, later
    // definitions shadowing earlier). Each merge expands the module's
    // parameterised type references against what the environment holds SO FAR,
    // so a module merged before the `self.glp` that defines a template it names
    // loses that template and its declaration keeps an unresolved type. In
    // discovery order — the directory walk's — that is what happened to every
    // module in a subdirectory: `programs/social/spm/gsg/plays/play_befriend.glp:24`
    // names `UserEvent(V, Q, A)` from `gsg/self.glp` one level up, and the
    // program loaded but every goal posted to it failed the goal check with
    // `UnknownTypeError: UserEvent`. Shallower directories first, and a
    // directory's `self.glp` before its siblings, is the order §Scope
    // construction specifies and the order the linker's ancestor chain uses.
    final ordered = [...modules]..sort((a, b) {
        int depth(DiscoveredModule m) =>
            File(m.filePath).absolute.parent.path.split(Platform.pathSeparator).length;
        final byDepth = depth(a).compareTo(depth(b));
        if (byDepth != 0) return byDepth;
        // Within one directory, self.glp is the scope and comes first.
        if (a.isSelfGlp != b.isSelfGlp) return a.isSelfGlp ? -1 : 1;
        return a.filePath.compareTo(b.filePath);
      });
    // A module of a DESCENDANT directory contributes its declarations but must
    // not shadow the program root's types: a goal is posted to the program's
    // entry points, so it is checked in the ROOT's scope (Currencies Code,
    // 2026-09-03 — the same shadowing that stopped the linked program loading).
    for (final m in ordered) {
      final descendant =
          _normDir(File(m.filePath).parent.path) != _normDir(programRoot);
      _extendGoalCheckEnv(m.ast,
          typesFillGapsOnly: descendant, label: m.moduleName);
      _addGoalClauses(
          PartialEvaluator()
              .transformDefinedGuards(
                  Program(m.ast.procedures, m.ast.line, m.ast.column))
              .procedures,
          fillGapsOnly: descendant);
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

  /// In madGLP mode, the Sends that end a goal's run (IGLP, Definition madGLP
  /// Send; Implementation Notes, "Event-driven execution"): the outbox's
  /// messages placed on the channel [MadContext.onMessageReady] gives, which
  /// whoever enters the mode provides (the REPL's `:mad`).  Null outside it.
  void Function()? get _sends {
    final ctx = madContext;
    if (ctx == null) return null;
    return () => ctx.flushMessages();
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
  /// entry-point lookup sites, via [_replEntryPointLabels]. A cross-module
  /// call is resolved by the linker, and only to a procedure its qualifier
  /// exports (TGLP modules.tex, Compilation, fourth step), so the boundary is
  /// not weakened by leaving `labels` unfiltered.
  BytecodeProgram get combinedProgram {
    // Root self.glp goes LAST so its primitives are the FALLBACK: label
    // indexing keeps the first occurrence, so a loaded module's own definition
    // (e.g. its merge/3) shadows the root's primitive of the same name (manual
    // §19.6: a module's definition shadows every ancestor's; modules.tex
    // §Static Linking step 3). Other loaded programs keep their insertion order.
    final allOps = <Op>[];
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
  /// Returns null if the goal is well-typed, and an error message if it is
  /// ill-typed or does not parse as a clause body: the check is never skipped
  /// (TGLP modules.tex, Type-Compatible Attestation Between Agents: the initial
  /// goal "is type-checked before execution as a body goal").  Until
  /// 2026-10-02 a goal that did not parse here passed the check, and the
  /// execution path reported what it made of it.
  ///
  /// The goal is parsed as a clause body so single goals and conjunctions are
  /// handled uniformly; a guard (if the user wrote one) is a body goal for
  /// type-checking, as in checkClauseFromAst. The check is the body part of
  /// Definition def:well-typed-clause; see [wtc.checkGoal].
  String? _checkGoalWellTyped(String trimmed) {
    final List<Goal> atoms;
    try {
      // The head's name is quoted: an unquoted name beginning with `_` is an
      // anonymous variable (GLP-Spec appendix-guards.tex, "Naming and
      // admission of body kernels"), and as one the clause did not parse and
      // the check was passed by.
      final parseInput = "'_glp_query_' :- $trimmed.";
      final lexer = Lexer(parseInput);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final parsed = parser.parse();
      if (parsed.procedures.isEmpty || parsed.procedures[0].clauses.isEmpty) {
        return 'Goal does not parse as a clause body: $trimmed';
      }
      final clause = parsed.procedures[0].clauses[0];
      atoms = [
        for (final g in clause.guards ?? const <Guard>[])
          Goal(g.predicate, g.args, g.line, g.column),
        ...?clause.body,
      ];
    } on CompileError catch (e) {
      return 'Goal does not parse as a clause body: ${e.message}';
    }
    if (atoms.isEmpty) {
      return 'Goal does not parse as a clause body: $trimmed';
    }

    final env = _ensureGoalCheckBaseEnv();
    final dfa = tdfa.buildProgramDFA(env);
    final result = wtc.checkGoal(atoms, dfa, env,
        callee: wtc.CalleeClauses((k) => _goalClauses[k], verifyInstantiation),
        isParametric: _goalProcedureIsParametric);
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

    final queryVarWriters = <String, HeapCell>{};
    final varNameToId = <String, HeapCell>{};
    final argSlots = <int, rt.Term>{};

    for (int i = 0; i < args.length; i++) {
      _setupArgument(
          _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
    }

    // The goal's id is the runtime's next, as every goal's is.
    final goalId = _runtime.nextGoalId++;
    final env = CallEnv(args: argSlots);
    _runtime.setGoalEnv(goalId, env);
    _runtime.setGoalProgram(goalId, 'main');
    // The goal carries its module value — the loaded app's artefact (h(M) +
    // code) — read back by `self_module`.
    if (_appModule != null) {
      _runtime.setGoalModule(goalId, _appModule);
    }

    final (runner, goalEntry) =
        _runnerForQuery(program, procedureLabel);
    final scheduler = Scheduler(rt: _runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();
    scheduler.setQueryVarNames(queryVarWriters);

    _runtime.gq.enqueue(GoalRef(goalId, goalEntry));

    final result = await scheduler.drainAsyncWithStatus(
      maxCycles: maxCycles,
      debug: debugTrace,
      showBindings: false,
      debugOutput: debugOutput,
      send: _sends,
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
    // Quoted, as in _checkGoalWellTyped: unquoted, the head's name is an
    // anonymous variable, and the conjunction does not parse.
    final parseInput = "'_conj_wrapper_' :- $trimmed.";
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
    final queryVarWriters = <String, HeapCell>{};
    final varNameToId = <String, HeapCell>{};

    // Build one CodeImage + ByteRunner for the whole conjunction; per-goal
    // entry PCs are byte offsets resolved below.
    final image = codeImageFromProgram(program);
    final GoalRunner runner = ByteRunner(image);
    final scheduler = Scheduler(rt: _runtime, runners: {'main': runner});
    scheduler.resetDisplayNumbering();

    // The conjuncts together are the run's initial goal, its resolvent G_0
    // (GLP-Spec glp.tex, Definition "Transition System ..."; IGLP dglp.tex,
    // the dGLP configuration): they are put to the machine together and run
    // by one drain to quiescence, and the conjunction's status is that of the
    // whole run there, as madGLP reports it for an agent, which reduces its
    // whole resolvent, FIFO, until quiescent (IGLP madglp.tex, "Each madGLP
    // agent executes its local resolvent with FIFO scheduling").  Drained one
    // conjunct at a time and the statuses aggregated, a conjunct that waits
    // on a later one, p(X?) before q(X), was reported suspended although the
    // run then completed it; and a conjunct that never quiesces starved every
    // conjunct after it.

    // Every conjunct is found before any is put to the machine, so a refused
    // conjunction leaves nothing queued behind it.
    final entryLabels = _replEntryPointLabels();
    for (final goal in goals) {
      final procedureLabel = '${goal.functor}/${goal.args.length}';
      if (program.labels[procedureLabel] == null ||
          !entryLabels.contains(procedureLabel)) {
        return ExecutionResult(
          status: ExecutionStatus.failed,
          error: 'Predicate $procedureLabel not found',
        );
      }
    }

    for (final goal in goals) {
      final functor = goal.functor;
      final arity = goal.args.length;
      final args = goal.args;
      final procedureLabel = '$functor/$arity';

      final argSlots = <int, rt.Term>{};
      for (int i = 0; i < args.length; i++) {
        _setupConjunctionArg(
            _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
      }

      // Each conjunct's id is the runtime's next, as every goal's is.
      final goalId = _runtime.nextGoalId++;
      final env = CallEnv(args: argSlots);
      _runtime.setGoalEnv(goalId, env);
      _runtime.setGoalProgram(goalId, 'main');
      // The goal carries its module value — the loaded app's artefact (h(M) +
      // code) — read back by `self_module`.
      if (_appModule != null) {
        _runtime.setGoalModule(goalId, _appModule);
      }

      scheduler.setQueryVarNames(queryVarWriters);
      final goalEntry = image.entryOffsetOf(procedureLabel)!;
      _runtime.gq.enqueue(GoalRef(goalId, goalEntry));
    }

    // One drain, to quiescence or the cycle limit, over the whole run.  Its
    // status is the run's: failed if a goal of the run failed (Fail advances
    // the queue and the run continues, dGLP/madGLP Reduce), capped if the
    // limit stopped it with goals still queued, suspended if a goal of the run
    // waits at quiescence, and succeeded otherwise.
    final result = await scheduler.drainAsyncWithStatus(
      maxCycles: maxCycles,
      debug: debugTrace,
      showBindings: false,
      debugOutput: debugOutput,
      send: _sends,
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
    List<DiscoveredModule> modules, {
    String? directory,
  }) {
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
    // The certificate (code format §Program Artefact; SGSG Section 3 and G1):
    // written at every compilation under the compiling person's key, and
    // refused, naming the offending calls, where the linked, pruned program
    // reaches an OS-privileged predicate or kernel — by reachability from its
    // entry points, so that a wrapper does not pass. A refused module still
    // loads and runs here, as the OS's own boot and play programs must: it
    // carries its two identities under no signature, no loader admits it, and
    // decompose_module/4 has no compiler's key to give for it.
    final offending = privilegedCalls(linked.program, _privilegedRootNames,
        ownModules: {
          for (final m in modules)
            if (m.exposingDir == null) m.moduleName
        });
    if (offending.isNotEmpty) {
      print('[CERTIFICATE REFUSED] $moduleName reaches the network or the '
          'person: ${offending.join('; ')}');
    }
    final artefact = Artefact.fromCompiled(
      ops: program.ops.cast<Object>(),
      hM: hM,
      moduleName: moduleName,
      isaVersion: glpIsaVersion,
      typeDefsText:
          interfaceTypeDefsText(exportDecls: exportDecls, typeDefs: typeDefs),
      exports: exports,
      signer: offending.isEmpty ? identity : null,
    );
    // The module's declared type-identity table (TGLP §Dynamic Activation and
    // Implementation Notes, "The tables"): every procedure declared in the
    // linked program's scope, root scope included, keyed as the compiled module
    // carries it. `find_type/2` reads it from the calling goal's module value.
    // Built over the same flat module the program was type-checked as.  A
    // table that cannot be built is an error and the module does not load:
    // until 2026-10-02 the failure was a [TYPE WARNING] and the module loaded
    // without a table, find_type then erring on every key.
    final TypeIdentityTables declaredTypes;
    try {
      declaredTypes = linkedTypeIdentityTables(modules, linked);
    } catch (e) {
      throw CompileError(
          '$moduleName: the type-identity tables cannot be built: $e', 0, 0,
          phase: 'typecheck');
    }
    return rt.ModuleTerm(artefact,
        name: moduleName, declaredTypes: declaredTypes, directory: directory);
  }

  ModuleInfo _extractModuleInfo(
      String source, BytecodeProgram program, String filename) {
    // A module's name is its path from the root (TGLP modules.tex,
    // Compilation, third step; [modulePathName]): a self.glp's is its
    // directory's.  A source with no file keeps the name it was loaded under.
    // Every loaded module is top-level — a single-module program exports all
    // its procedures (modules.tex sec:static-linking).
    final String name = File(filename).existsSync()
        ? modulePathName(filename, _rootDir)
        : _moduleNameFromFilename(filename);
    const isTopLevel = true;

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

    return ModuleInfo(name: name, program: program, hasExports: hasExports, exportedLabels: exportedLabels, isTopLevel: isTopLevel);
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
    if (_goalCheckEnv == null) {
      _goalCheckEnv = buildAncestorScope(
          chain: const [], rootSelfGlpPath: _rootSelfGlpPath);
      final rootSelf = File(_rootSelfGlpPath);
      if (rootSelf.existsSync()) _scopeSelfGlps.add(rootSelf.absolute.path);
    }
    return _goalCheckEnv!;
  }

  /// Extend the goal-check environment with a loaded module's declarations, so
  /// goals referencing its procedures can be type-checked.
  void _extendGoalCheckEnv(Module module,
      {bool typesFillGapsOnly = false, String? label}) {
    _goalCheckEnv = mergeModuleIntoScope(_ensureGoalCheckBaseEnv(), module,
        typesFillGapsOnly: typesFillGapsOnly, label: label);
  }

  /// A directory path for comparison: absolute, `..` and `.` segments resolved
  /// lexically (a caller may pass `GLP/test/..`, as the test suite does), and no
  /// trailing separator.
  static String _normDir(String p) {
    var n = ppath.normalize(Directory(p).absolute.path);
    while (n.length > 1 && n.endsWith(Platform.pathSeparator)) {
      n = n.substring(0, n.length - 1);
    }
    return n;
  }

  void _setupArgument(
    GlpRuntime runtime,
    Term arg,
    int argSlot,
    Map<int, rt.Term> argSlots,
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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

        // A goal variable is reported whichever occurrence meets it first,
        // writer or reader: the outcome of the run gives every variable of
        // the initial goal its value (GLP-Spec glp.tex, Definition "cGLP
        // Proper Run, Outcome"), read at its writer.  One met only as a
        // reader is never bound and is reported unbound.
        queryVarWriters[baseName] = writerId;

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
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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

        // A goal variable is reported whichever occurrence meets it first,
        // writer or reader: the outcome of the run gives every variable of
        // the initial goal its value (GLP-Spec glp.tex, Definition "cGLP
        // Proper Run, Outcome"), read at its writer.  One met only as a
        // reader is never bound and is reported unbound.
        queryVarWriters[baseName] = writerId;

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
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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
          // Reported whichever occurrence meets it first (_setupArgument).
          queryVarWriters[baseName] = writerId;
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
      } else if (arg is UnderscoreTerm) {
        argTerms.add(_anonymousWriter(runtime, arg));
      } else {
        throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
      }
    }

    return rt.StructTerm(struct.functor, argTerms);
  }

  /// An anonymous writer `_` inside a goal argument: a writer with no paired
  /// reader, for a value the goal discards. Each occurrence is a distinct
  /// variable, so it is recorded in neither [varNameToId] (nothing can refer
  /// back to it) nor queryVarWriters (the REPL has no name to report it under).
  /// `_?` is not GLP — an anonymous reader would read a variable nothing can
  /// ever write — and is refused here rather than passed on as a bare reader.
  rt.Term _anonymousWriter(GlpRuntime runtime, UnderscoreTerm arg) {
    if (arg.isReader) {
      throw Exception(
          'Anonymous reader `_?` is not permitted (line ${arg.line}); '
          'an anonymous variable may only be a writer.');
    }
    final (writerId, _) = runtime.heap.allocateVariable();
    return rt.VarRef(writerId);
  }

  rt.Term _buildStructTermForConj(
    GlpRuntime runtime,
    StructTerm struct,
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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
          // Reported whichever occurrence meets it first (_setupArgument).
          queryVarWriters[baseName] = writerId;
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
      } else if (arg is UnderscoreTerm) {
        argTerms.add(_anonymousWriter(runtime, arg));
      } else {
        throw Exception('Unsupported struct argument type: ${arg.runtimeType}');
      }
    }

    return rt.StructTerm(struct.functor, argTerms);
  }

  rt.Term _buildListTerm(
    GlpRuntime runtime,
    ListTerm list,
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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
        // Reported whichever occurrence meets it first (_setupArgument).
        queryVarWriters[baseName] = writerId;
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
        // Reported whichever occurrence meets it first (_setupArgument).
        queryVarWriters[baseName] = writerId;
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
    Map<String, HeapCell> queryVarWriters,
    Map<String, HeapCell> varNameToId,
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
        // Reported whichever occurrence meets it first (_setupArgument).
        queryVarWriters[baseName] = writerId;
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
        // Reported whichever occurrence meets it first (_setupArgument).
        queryVarWriters[baseName] = writerId;
        tailTerm = tail.isReader ? rt.VarRef(readerId) : rt.VarRef(writerId);
      }
    } else {
      tailTerm = rt.ConstTerm(null);
    }

    return rt.StructTerm('.', [headTerm, tailTerm]);
  }
}
