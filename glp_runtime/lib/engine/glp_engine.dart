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
import 'package:glp_runtime/runtime/external_io.dart' show InputInjector;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/param_expansion.dart'
    show UndefinedDeclarationTypeError;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/root_scope.dart'
    show rootRenamed;
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
    show privilegedRootSeed, privilegedCalls;

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

/// A goal posted to the engine ([GlpEngine.postGoal]): checked, its terms on
/// the heap, and put on the machine's queue --- not yet run.
class PostedGoal {
  /// The scheduler over the code image the goal was posted on, which runs the
  /// goal and every goal it spawns.
  final Scheduler scheduler;

  /// The input streams the caller holds, by the name of their variable in the
  /// goal: the goal holds the variable's reader and the caller its writer,
  /// through which the caller extends the stream ([InputInjector.inject]) or
  /// closes it ([InputInjector.close]).  The goals an injection wakes are the
  /// caller's to enqueue and run.
  final Map<String, InputInjector> inputs;

  /// The writer of each variable of the goal, by name, where the run's outcome
  /// is read (GLP-Spec glp.tex, Definition "cGLP Proper Run, Outcome").
  final Map<String, HeapCell> variables;

  PostedGoal._(this.scheduler, this.inputs, this.variables);
}

/// The text of the constant [name] in a goal posted to the engine, read back
/// as that constant (GLP-Spec Definition "Logic Programs Syntax": the text
/// denotes the term): a name that unquoted would read as an atom stands as it
/// is, and any other --- one that would read as a variable, a number, an
/// operator or a keyword --- in single quotes, its quote and backslash
/// escaped.  For the hosts, which post their entry goal with the agent's id
/// and the constants of its spawn directive ([GlpEngine.postGoal]).
String glpConstantText(String name) {
  if (RegExp(r'^[a-z][a-zA-Z0-9_]*$').hasMatch(name) &&
      name != 'mod' &&
      name != 'procedure') {
    return name;
  }
  return "'${name.replaceAll(r'\', r'\\').replaceAll("'", r"\'")}'";
}

/// A goal the engine refuses to post ([GlpEngine.postGoal]), the refusal its
/// text: a goal that does not parse as a clause body or is not well-typed as
/// one, that calls no entry point of a loaded unit and no procedure of the
/// root, or that names an input it does not hold the reader of.
class GoalRefused implements Exception {
  final String message;
  GoalRefused(this.message);

  @override
  String toString() => message;
}

/// The entry points of a unit the engine holds ([GlpEngine.scopeFor]): their
/// keys, "name/arity", and the scope they were declared in, built when a
/// source is first handed over and kept.
class _UnitEntryPoints {
  final Set<String> keys;
  final TypeEnvironment Function() _declaringScope;
  late final TypeEnvironment declaringScope = _declaringScope();

  _UnitEntryPoints(this.keys, this._declaringScope);
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

  /// The entry points of each unit the engine holds, by the name it was
  /// loaded under, with the scope they were declared in: what a source handed
  /// over to the engine sees of the units it holds ([scope], [scopeFor]), and
  /// what a goal posted to it is checked against ([_checkGoalWellTyped]).
  final Map<String, _UnitEntryPoints> _unitEntryPoints = {};

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

    // Nothing is set for the process: every check, partial evaluation and
    // compilation below is given its scope (module_hierarchy.dart,
    // buildAncestorScope), the root self.glp its first layer.  Until
    // 2026-10-04 the root's text was set here into two process-wide sources,
    // the partial evaluator's and the type checker's, which every engine of
    // the process then shared.
    registerModuleKernels(_runtime);
    _loadRootSelf();
  }

  /// The scope a module directly under the root is checked in, `Π ⊔ d_1`
  /// (module_hierarchy.dart, [rootScope]): a file-less source's, given no
  /// other, and the one every scope the engine builds is built over, so the
  /// root's layer is one and its procedures are certified once per engine.
  late final TypeEnvironment _rootScope = rootScope(_rootSelfGlpPath);

  /// Clear all loaded programs except root self.glp.
  ///
  /// Useful for test scripts that need to reset state between tests
  /// without restarting the REPL process.
  void clear() {
    // Clear everything but the root, linked once at construction.
    _loadedPrograms.clear();
    _loadedModules.clear();
    _unitEntryPoints.clear();
  }

  /// The root self.glp linked as a program of its own and compiled
  /// ([checkedRootProgram]): its procedures under the names the renaming
  /// gives them, `:p` (TGLP modules.tex, Compilation, third step), every one
  /// kept.  A goal posted to the engine is a module at the root, linked with
  /// the root self.glp (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11"), and a
  /// call of it that no loaded program's entry point answers resolves here
  /// ([_goalLabel]).  It is no load: every program carries the root
  /// procedures it reaches in its own compiled module.
  late final BytecodeProgram _rootCode;

  /// Link, check and compile the root self.glp (private — called by
  /// constructor).
  ///
  /// A failure here is fatal and is raised as one, naming the root's file and
  /// line: the root self.glp is checked in step 2 like any module (TGLP
  /// modules.tex, Compilation, second step), against the language primitives,
  /// and a root that does not check is refused.  Until 2026-10-04 it was
  /// compiled here unchecked, and a type error in it loaded and ran
  /// (programs/tests/root_check/univ_typing_root.glp); until 2026-08-01 a
  /// failure to compile it was swallowed.  The `__root__` runner registered
  /// here until 2026-10-04 goes: an activated module carries the root
  /// procedures it reaches in its own artefact.
  void _loadRootSelf() {
    final root = rootModuleOf(_rootSelfGlpPath);
    if (root == null) {
      _rootCode = BytecodeProgram(const []);
      return;
    }
    try {
      final linked = checkedRootProgram(root);
      _rootCode = GlpCompiler().compileProgram(linked.program,
          procDeclarations: linked.procDeclarations,
          typeEnv: linked.checkedEnv);
    } catch (e) {
      throw StateError(
          'root self.glp does not check and compile: $_rootSelfGlpPath\n  $e\n'
          'The root self.glp is the first link of every program\'s chain and '
          'is checked with every program, and every goal posted to the engine '
          'is linked with it.');
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
  ///
  /// With [scope], the module is type-checked in that scope instead of the
  /// ancestor self.glp chain of [filename]. This is the boot-source case
  /// (IGLP, Implementation Notes, "The scope a boot source is checked in"): a
  /// boot source is loaded on top of a program already in the engine, so it is
  /// checked in the scope the engine holds when it is handed over --- "the
  /// linked program's entry points and the boot file's ancestor chain of
  /// self.glp declarations, the root among them" (8aafd09) --- which the
  /// loaders obtain from [scope] and [scopeFor]. A check that sees the
  /// ancestor chain alone refuses calls the engine resolves, the linked
  /// program's exports among them; and under a
  /// synthetic name there is no chain at all, so until 2026-09-18 a boot source
  /// was checked in the bare root scope and `send_to_net/1`, then loaded by
  /// [enableMadGLP], was undefined in it.  It is the root self.glp's since
  /// 2026-10-04 (GLP-Spec appendix-guards, "Output to the network").
  /// [scope] decides what the source is checked against.  A source with a
  /// real file behind it is linked as a single-module program with its
  /// ancestors; one with none is a module at the root, linked with the root
  /// self.glp as a one-module program, reaching a program the engine holds
  /// only through its entry points (GLP #3 Cowork, 2026-10-03 21:18 UTC,
  /// "16:11").  Either way the object compiled is the linked program, and it
  /// is the object checked.
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
    // There is no internal source: the root self.glp is linked once at
    // construction ([_rootCode]) and with every program, and the engine's
    // embedded madGLP source is gone, its send_to_net/1 and global_send/3
    // being the root self.glp's.
    final isRealFile = name != '_source_' && File(name).existsSync();
    final selfContained = _isSelfContained(module);

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

    List<DiscoveredModule>? discovered;
    if (isRealFile) {
      // The module as the linker discovers it: its ancestor scope with the
      // `-expose`d modules of the directories on its chain merged in
      // (modules.tex, "The -expose directive": an exposed module's exported
      // procedures are in the directory's scope as if defined in its self.glp).
      // The check below and the linker further down share this one discovery,
      // which refuses a file outside the root first ([requireUnderRoot]).
      discovered = discoverSingleModule(name,
          rootSelfGlpPath: _rootSelfGlpPath, rootScope: _rootScope);
    }
    TypeEnvironment? ancestorScope = scope;
    if (ancestorScope == null && discovered == null) {
      // A source with no file behind it and no scope given is checked
      // directly under the root, `Π ⊔ d_1`.
      ancestorScope = _rootScope.copy();
    }
    if (ancestorScope == null && discovered != null) {
      // Until 2026-09-18 this was buildAncestorScope(chain) — the self.glp
      // chain alone, without the exposes — so a module calling a procedure its
      // directory's self.glp exposes (agent/4 of programs/tests/agent_roundtrip,
      // and send_to_net/1 of system/mad_predicates while the root exposed it)
      // was refused as undefined by the check while the linker resolved it.
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
    {
      final ast = Program(module.procedures, module.line, module.column);
      final partialEvaluator = PartialEvaluator();
      final transformedAst =
          partialEvaluator.transformDefinedGuards(ast, scope: ancestorScope);

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
    // per-module pass for.  Source text with no file behind it is a module at
    // the root, linked with the root self.glp as a one-module program, and
    // compiled the same way (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11";
    // GLP's round six, item 4, "without linking" going with it): until
    // 2026-10-04 it was compiled directly, unlinked, its calls to the root
    // reaching the root's procedures by their bare names at run time.
    final BytecodeProgram program;
    rt.ModuleTerm? moduleValue;
    if (isRealFile) {
      final modules = discovered!;
      // The object compiled is the LINKED program, and it is the object
      // checked (TGLP modules.tex, Compilation: "The flat program is the
      // linked program of def:program, and it is the object checked"):
      // checkedLinkedProgram checks each module of the program against its
      // scope and then the flat program, as on the directory path, and returns
      // it only if it checks.  Until 2026-10-03 this path linked and compiled
      // with no check of the linked program, and with none of a module the
      // ancestors expose (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:01. 4").
      // The scope its SRSW relaxations are decided in is the flat module's,
      // not the single module's: the two name their types differently (step-3
      // renaming).
      final linked = checkedLinkedProgram(modules,
          rootDir: File(name).parent.path, singleModulePath: name);
      program = _compiler.compileProgram(linked.program,
          procDeclarations: linked.procDeclarations,
          typeEnv: linked.checkedEnv);
      // This unit's module value — its artefact: h(M) + code.
      moduleValue = _moduleValueOf(_baseName(name), program, linked, modules,
          directory: File(name).parent.absolute.path);
    } else {
      // The source and the root: the source checked in step 2 in the scope it
      // is handed over in, the root against the primitives; the source's
      // procedures kept by their names, its entry points; a call to an entry
      // point of a program the engine holds left to that program, which
      // stands between the root and the source in the scope (IGLP
      // Implementation Notes, "The scope a boot source is checked in"); and
      // the linked program checked over that scope, which declares them.
      final root = rootModuleOf(_rootSelfGlpPath);
      final modules = <DiscoveredModule>[
        DiscoveredModule(
          filePath: name,
          moduleName: _fileLessModuleName(name),
          ast: module,
          ancestorScope: ancestorScope!,
        ),
        if (root != null) root,
      ];
      final linked = checkedLinkedProgram(modules,
          rootDir: File(_rootSelfGlpPath).parent.path,
          singleModulePath: name,
          outerScope: ancestorScope,
          outerEntryPoints: _entryPoints());
      program = _compiler.compileProgram(linked.program,
          procDeclarations: linked.procDeclarations,
          typeEnv: linked.checkedEnv);
    }
    _refuseRedefinitionByLaterLoad(name, program);
    _loadedPrograms[name] = program;
    if (moduleValue != null) {
      _loadedModuleValues[name] = moduleValue;
      _appModule = moduleValue;
    }

    final moduleInfo = _extractModuleInfo(source, program, name);
    _loadedModules[moduleInfo.name] = moduleInfo;

    // Its entry points, every procedure it defines, declared in the scope it
    // was checked in ([scope]), which is what a goal posted to it is checked
    // against ([_checkGoalWellTyped]).
    final declaredIn = ancestorScope!;
    _unitEntryPoints[name] = _UnitEntryPoints(_plainProcedures(program),
        () => mergeModuleIntoScope(declaredIn, module, label: moduleInfo.name));

    return true;
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
    final mine = _plainProcedures(program);
    for (final e in _loadedPrograms.entries) {
      if (e.key == name) continue;
      final clash = mine.intersection(_plainProcedures(e.value));
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

  /// The procedures of [p] by their plain names, "name/arity": a loaded
  /// unit's entry points, which a goal names (TGLP modules.tex, "Entry and the
  /// absence of a boot module": "the procedures that may be posted are exactly
  /// the entry points").  A renamed `M:p`, the root's `:p` among them, is a
  /// module's own, and a clause's internal label is no procedure.
  static Set<String> _plainProcedures(BytecodeProgram p) => {
        for (final l in p.labels.keys)
          if (l.contains('/') &&
              !l.contains(':') &&
              !l.endsWith('_end') &&
              !_clauseLabel.hasMatch(l))
            l
      };

  static final RegExp _clauseLabel = RegExp(r'_c\d+$');

  /// The entry points of every unit the engine holds ([_plainProcedures]).
  Set<String> _entryPoints() => {
        for (final p in _loadedPrograms.values) ..._plainProcedures(p),
      };

  /// The label a goal's call to [functor]/[arity] runs at: an entry point of a
  /// loaded unit, which a goal posted to it names by plain name (TGLP
  /// modules.tex, "Entry and the absence of a boot module"), and otherwise the
  /// root's procedure of that name, renamed under the empty path ([_rootCode]),
  /// the goal being a module at the root, linked with the root self.glp (GLP
  /// #3 Cowork, 2026-10-03 21:18 UTC, "16:11").  The loaded unit's entry point
  /// stands nearer, as its declaration does in the goal-check environment
  /// ([_checkGoalWellTyped]).  Null where neither has it.
  String? _goalLabel(String functor, int arity) {
    final plain = '$functor/$arity';
    if (_entryPoints().contains(plain)) return plain;
    final rooted = rootRenamed(plain);
    if (_rootCode.labels.containsKey(rooted)) return rooted;
    return null;
  }

  /// The name a file-less source is linked under: the name it was loaded
  /// under, its extension dropped --- a module at the root, whose path from
  /// the root is that name, never the root's own empty path.
  String _fileLessModuleName(String name) {
    final n = _moduleNameFromFilename(name);
    return n.isEmpty ? '_source_' : n;
  }

  /// The scope a source with no file behind it is checked in when it is
  /// handed over to the engine: a module at the root, its ancestor chain the
  /// root self.glp alone, with the entry points of every unit the engine
  /// holds ([scopeFor]).
  TypeEnvironment get scope => _handOverScope(const []);

  /// The scope a boot source loaded from [path] is checked in, on top of the
  /// units the engine holds: "the linked program's entry points and the boot
  /// file's ancestor chain of self.glp declarations, the root among them"
  /// (IGLP, Implementation Notes, "The scope a boot source is checked in",
  /// 8aafd09) --- the language primitives, the self.glp of each directory from
  /// the root down to the boot file's, and each loaded unit's entry points
  /// with the types their signatures carry, and no other declaration or type
  /// of the units ([mergeEntryPointsIntoScope]).  So "a call to a procedure
  /// the program does not export is refused by the check, which names the
  /// call, and does not fail at run time", and a type no export carries is
  /// undefined in it.  Until 2026-10-04 this was the goal-check environment
  /// with the boot file's chain layered on it: the program's whole self.glp
  /// chain and every module's declarations, so a boot source could call an
  /// unexported procedure, pass the check, and fail at run time.
  ///
  /// A boot file outside the root is refused ([requireUnderRoot]): its
  /// compilation is under the root as every compilation is, and its chain is
  /// the one from the root down to its directory (TGLP modules.tex, "Scope
  /// construction").
  TypeEnvironment scopeFor(String path) {
    requireUnderRoot(path, _rootDir);
    return _handOverScope(discoverSelfChain(
        targetFile: path,
        rootDir: File(path).parent.path,
        programsDir: File(_rootSelfGlpPath).parent.absolute.path));
  }

  /// The primitives and the root self.glp, the self.glp files of [chain] in
  /// order, and the loaded units' entry points over them, a later unit's over
  /// an earlier one's: a source handed over stands on the units the engine
  /// holds, and a call of it that an entry point answers is left to that
  /// unit (program_linker.dart, `outerEntryPoints`).
  TypeEnvironment _handOverScope(List<String> chain) {
    var env = buildAncestorScope(
        chain: chain, rootSelfGlpPath: _rootSelfGlpPath, rootScope: _rootScope);
    for (final e in _unitEntryPoints.entries) {
      env = mergeEntryPointsIntoScope(env, e.value.declaringScope, e.value.keys,
          label: e.key);
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
        rootSelfGlpPath: _rootSelfGlpPath, rootScope: _rootScope);
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

    // Its entry points, the exports of its self.glp, declared in the scope
    // that self.glp was checked in, its own definitions merged ([scope]):
    // what a goal posted to the program is checked against, with the root
    // ([_checkGoalWellTyped]).  Until 2026-10-07 a goal was checked in an
    // environment of its own, the program's whole self.glp chain and every
    // module's declarations layered over the root, so a goal calling a
    // procedure the program does not export passed the check and was refused
    // only when no entry point answered it.
    final programRoot = _normDir(programDir);
    final programSelf = modules.firstWhere((m) =>
        m.isSelfGlp && _normDir(File(m.filePath).parent.path) == programRoot);
    _unitEntryPoints['__program__'] = _UnitEntryPoints(
        _plainProcedures(program),
        () => mergeModuleIntoScope(programSelf.ancestorScope, programSelf.ast,
            label: programSelf.moduleName));

    return true;
  }

  /// Run a goal and return the result
  ///
  /// [goalText] is the goal to run, e.g., "merge([1,2],[a,b],X)".  It is
  /// posted by [postGoal]'s path, checked there, and run here to quiescence or
  /// the cycle limit; a refused goal runs nothing and its refusal is the
  /// result's error.
  Future<ExecutionResult> runGoal(String goalText) async {
    try {
      final posted = _post(_goalText(goalText));

      // One drain, to quiescence or the cycle limit, over the whole run.  Its
      // status is the run's: failed if a goal of the run failed (Fail advances
      // the queue and the run continues, dGLP/madGLP Reduce), capped if the
      // limit stopped it with goals still queued, suspended if a goal of the
      // run waits at quiescence, and succeeded otherwise.
      final result = await posted.scheduler.drainAsyncWithStatus(
        maxCycles: maxCycles,
        debug: debugTrace,
        showBindings: false,
        debugOutput: debugOutput,
        send: _sends,
      );

      // Collect bindings
      final bindings = <String, rt.Term?>{};
      for (final entry in posted.variables.entries) {
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
    } catch (e) {
      return ExecutionResult(
        status: ExecutionStatus.failed,
        error: e.toString(),
      );
    }
  }

  /// Post a goal to the engine: check it, put it on the machine's queue, and
  /// return it posted, not yet run.  The one path by which a goal reaches the
  /// machine from outside it --- the REPL's ([runGoal]), a host's
  /// (multiagent/agent_runtime.dart) and the isolate boot's
  /// (multiagent/isolate_manager.dart): "The remaining producer of unchecked
  /// terms is the initial goal posted to the runtime, at boot or
  /// interactively; it is type-checked before execution as a body goal"
  /// (TGLP modules.tex, "Type-Compatible Attestation Between Agents",
  /// Definition def:well-typed-clause), and "the procedures that may be
  /// posted are exactly the entry points" (modules.tex, "Entry and the absence
  /// of a boot module"), with the root self.glp's ([_goalLabel]).
  ///
  /// [inputs] names the variables of the goal whose writers the caller holds:
  /// each occurs in the goal as a reader only, the goal consuming the stream
  /// the caller produces, and its writer is returned as an [InputInjector] in
  /// [PostedGoal.inputs], through which the caller extends the stream with
  /// the ground terms it is handed --- the person's acts (GSG, Appendix "The
  /// Prototype's Screens") --- or closes it.  A variable the goal writes is
  /// not the caller's to write, and one the goal does not hold is no input of
  /// the goal: either is refused, as is a goal that does not check or calls
  /// no entry point.  A refused goal puts nothing on the machine.
  ///
  /// Until 2026-10-04 a host posted its goal with an injector and no check
  /// (agent_runtime.dart, isolate_manager.dart), and [runGoal], which
  /// checked, handed the caller no writer.
  PostedGoal postGoal(String goalText, {List<String> inputs = const []}) =>
      _post(_goalText(goalText), inputs: inputs);

  /// [goalText] trimmed, its full stop dropped.
  static String _goalText(String goalText) {
    var trimmed = goalText.trim();
    if (trimmed.endsWith('.')) {
      trimmed = trimmed.substring(0, trimmed.length - 1).trim();
    }
    return trimmed;
  }

  /// The posting of [trimmed] ([postGoal]).  Throws [GoalRefused], naming
  /// the refusal, where the goal is not posted.
  PostedGoal _post(String trimmed, {List<String> inputs = const []}) {
    // Reject an ill-typed goal before running it. Soundness of well-typing
    // (TGLP glp-semantics, Theorem thm:soundness) holds for runs from a
    // well-typed initial goal; a goal is well-typed iff well-typed as a body.
    final typeError = _checkGoalWellTyped(trimmed);
    if (typeError != null) throw GoalRefused(typeError);

    // The goal's atoms: a conjunction's conjuncts, read as a clause body, or
    // the one goal, read as a clause.
    final conjunction = _isConjunction(trimmed);
    final List<Atom> goals;
    if (conjunction) {
      // Quoted, as in _checkGoalWellTyped: unquoted, the head's name is an
      // anonymous variable, and the conjunction does not parse.
      final ast =
          Parser(Lexer("'_conj_wrapper_' :- $trimmed.").tokenize()).parse();
      if (ast.procedures.isEmpty || ast.procedures[0].clauses.isEmpty) {
        throw GoalRefused('Could not parse conjunction');
      }
      final clause = ast.procedures[0].clauses[0];
      if (clause.body == null || clause.body!.isEmpty) {
        throw GoalRefused('No goals in conjunction');
      }
      goals = [
        for (final g in clause.body!) Atom(g.functor, g.args, g.line, g.column)
      ];
    } else {
      final ast = Parser(Lexer('$trimmed.').tokenize()).parse();
      if (ast.procedures.isEmpty) throw GoalRefused('No goal found');
      if (ast.procedures[0].clauses.isEmpty) {
        throw GoalRefused('No clauses in goal');
      }
      goals = [ast.procedures[0].clauses[0].head];
    }

    // Every conjunct is found, and every input is the goal's to read, before
    // any is put to the machine, so a refused goal leaves nothing queued
    // behind it.
    final program = combinedProgram;
    final labels = <String>[];
    for (final goal in goals) {
      final procedureLabel = _goalLabel(goal.functor, goal.args.length);
      if (procedureLabel == null || program.labels[procedureLabel] == null) {
        throw GoalRefused(
            'Predicate ${goal.functor}/${goal.args.length} not found');
      }
      labels.add(procedureLabel);
    }
    _requireInputs(goals, inputs);

    // One CodeImage + ByteRunner for the whole goal; each conjunct's entry is
    // a byte offset into it.  The caller has the labels from the image's own
    // program, so an unresolved offset is an internal invariant violation
    // between the object labels and the image symbols, not a user error.
    final image = codeImageFromProgram(program);
    final entries = [
      for (final l in labels)
        image.entryOffsetOf(l) ??
            (throw StateError('no compiled byte entry for $l'))
    ];
    final scheduler =
        Scheduler(rt: _runtime, runners: {'main': ByteRunner(image)});
    scheduler.resetDisplayNumbering();

    final queryVarWriters = <String, HeapCell>{};
    final varNameToId = <String, HeapCell>{};

    // Each input's pair: the caller holds the writer, and the goal's reader
    // occurrence is the pair's reader (_setupArgument).
    final injectors = <String, InputInjector>{};
    for (final name in inputs) {
      final (writer, _) = _runtime.heap.allocateVariable();
      varNameToId[name] = writer;
      queryVarWriters[name] = writer;
      injectors[name] = InputInjector(_runtime.heap, name, writer);
    }

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
    for (var g = 0; g < goals.length; g++) {
      final args = goals[g].args;
      final argSlots = <int, rt.Term>{};
      for (int i = 0; i < args.length; i++) {
        if (conjunction) {
          _setupConjunctionArg(
              _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
        } else {
          _setupArgument(
              _runtime, args[i], i, argSlots, queryVarWriters, varNameToId);
        }
      }

      // Each goal's id is the runtime's next, as every goal's is.
      final goalId = _runtime.nextGoalId++;
      _runtime.setGoalEnv(goalId, CallEnv(args: argSlots));
      _runtime.setGoalProgram(goalId, 'main');
      // The goal carries its module value — the loaded app's artefact (h(M) +
      // code) — read back by `self_module`.
      if (_appModule != null) {
        _runtime.setGoalModule(goalId, _appModule);
      }
      _runtime.gq.enqueue(GoalRef(goalId, entries[g]));
    }
    scheduler.setQueryVarNames(queryVarWriters);

    return PostedGoal._(scheduler, injectors, queryVarWriters);
  }

  /// Refuse [inputs] that are not the goal's to read: each must occur in
  /// [goals], and only as a reader, its writer being the caller's
  /// ([postGoal]).  An input named twice is one input.
  void _requireInputs(List<Atom> goals, List<String> inputs) {
    if (inputs.isEmpty) return;
    if (inputs.toSet().length != inputs.length) {
      throw GoalRefused('An input is named twice: ${inputs.join(', ')}');
    }
    final readers = <String>{};
    final writers = <String>{};
    void walk(Term? t) {
      if (t is VarTerm) {
        (t.isReader ? readers : writers).add(t.name);
      } else if (t is StructTerm) {
        t.args.forEach(walk);
      } else if (t is ListTerm) {
        walk(t.head);
        walk(t.tail);
      }
    }

    for (final goal in goals) {
      goal.args.forEach(walk);
    }
    for (final name in inputs) {
      if (writers.contains(name)) {
        throw GoalRefused(
            "Input $name occurs in the goal as a writer: its writer is the "
            "caller's, and the goal holds its reader $name? only");
      }
      if (!readers.contains(name)) {
        throw GoalRefused(
            'Input $name does not occur in the goal: the goal holds the '
            'reader $name? of each input');
      }
    }
  }

  /// Enable madGLP mode for this engine.
  ///
  /// Creates the MadContext for message routing.  It loads no GLP: the madGLP
  /// system predicates --- send_to_net/1 (GLP-Spec appendix-guards, "Output
  /// to the network") and global_send/3 (IGLP Definition "global_send
  /// Predicate") --- are the root self.glp's, loaded at construction.
  void enableMadGLP({required String agentId}) {
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
  /// Each loaded unit is one compiled module, the root procedures it reaches
  /// among its own under their renamed names, `:p` (TGLP modules.tex,
  /// Compilation, third to fifth steps); the root linked alone ([_rootCode])
  /// follows them, for a posted goal.  A root procedure two of them carry is
  /// one procedure compiled from one clause set, and the label the first
  /// carries is the one a call reaches.  A goal names a unit's entry point or
  /// a root procedure ([_goalLabel]); a cross-module call is resolved by the
  /// linker, and only to a procedure its qualifier exports (TGLP modules.tex,
  /// Compilation, fourth step), so the boundary is not weakened by leaving
  /// `labels` unfiltered.  Until 2026-10-04 the root self.glp was compiled
  /// here, last, as a fallback for every bare name a unit left unresolved.
  BytecodeProgram get combinedProgram {
    final allOps = <Op>[];
    for (final entry in _loadedPrograms.entries) {
      allOps.addAll(entry.value.ops);
    }
    allOps.addAll(_rootCode.ops);
    return BytecodeProgram(allOps);
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

    // The goal is checked in the root and the loaded units' entry points
    // ([scope]): "the procedures that may be posted are exactly the entry
    // points" (TGLP modules.tex, "Entry and the absence of a boot module"),
    // each with the transitive closure of the types its signature references
    // ("Procedure declarations"), the goal being a module at the root, linked
    // with the root self.glp (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11").
    // A callee's clauses are the ones its layer of that scope holds --- the
    // root self.glp's, or the unit's that declares the entry point --- read
    // in the scope they were declared in ([_verifyInLayer]), where its
    // parametricity is decided too ([scopeProcedureIsParametric]).  Until
    // 2026-10-07 the goal was checked in an environment of its own, every
    // loaded unit's self.glp chain and every module's declarations over the
    // root: a goal calling a procedure no entry point names passed the check
    // and was refused only at [_goalLabel] (`helper(secret(3), N)` posted to
    // programs/tests/boot_scope/program), and one calling a root procedure
    // that a program's self.glp redefines privately was checked against that
    // definition while the root's ran (programs/tests/post_goal/private_root).
    final env = scope;
    final dfa = tdfa.buildProgramDFA(env);
    final result = wtc.checkGoal(atoms, dfa, env,
        callee: wtc.CalleeClauses((k) => env.scopeLayers[k]?.clauses[k],
            (decl, at, clauses) => _verifyInLayer(env, decl, at, clauses)),
        isParametric: (k) => scopeProcedureIsParametric(env, k));
    if (result.isWellTyped) return null;

    final detail = result.errors.map((e) => '  ${e.message}').join('\n');
    return 'Goal is not well-typed:\n$detail';
  }

  /// Whether [clauses], the clauses of a procedure a posted goal calls, are
  /// well-typed by the declaration [decl] the call instantiates and accept its
  /// every input path ([verifyInstantiation]), read in the scope they were
  /// declared in: that of the layer of [goalScope] holding them
  /// ([ScopeLayer.env]), which declares their callees, with the types of
  /// [at], the scope the call was read in, filling its gaps.  A clause is
  /// well-typed in its own scope, and the goal's declares the root and the
  /// entry points alone.
  static bool _verifyInLayer(TypeEnvironment goalScope, ProcDecl decl,
      TypeEnvironment at, List<Clause> clauses) {
    final layer = goalScope.scopeLayers[decl.key];
    if (layer == null) return verifyInstantiation(decl, at, clauses);
    final own = layer.env;
    return verifyInstantiation(
        decl,
        TypeEnvironment(
          {...at.types, ...own.types},
          {...own.procedures, decl.key: decl},
          paramProcDecls: own.paramProcDecls,
          typeTemplates: {...at.typeTemplates, ...own.typeTemplates},
          typeOrigins: {...at.typeOrigins, ...own.typeOrigins},
          scopeLayers: own.scopeLayers,
        ),
        clauses);
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
    final offending = privilegedCalls(linked.program, privilegedRootSeed(),
        ownModules: {
          for (final m in modules)
            if (!m.collectedByExpose && !m.isRoot) m.moduleName
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
          runtime.heap.bindWriterConst(writerId, rt.nil);
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
          runtime.heap.bindWriterConst(writerId, rt.nil);
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
      return rt.ConstTerm(rt.nil);
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
    } else if (head is UnderscoreTerm) {
      headTerm = _anonymousWriter(runtime, head);
    } else {
      throw Exception('Unsupported list head type: ${head.runtimeType}');
    }

    // The tail as written, whatever term it is (GLP-Spec appendix-lp.tex,
    // Definition "Logic Programs Syntax": `[X|Xs]` is a list cell, a compound
    // term, and its subterms are terms).  Until 2026-10-07 a tail that was
    // neither a list nor a variable was built as ConstTerm(null), which the
    // display took for [], so `X = [a | b].` posted `[a]`; and a `_` head
    // threw.
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
    } else if (tail is ConstTerm) {
      tailTerm = rt.ConstTerm(tail.value);
    } else if (tail is StructTerm) {
      tailTerm = _buildStructTerm(runtime, tail, queryVarWriters, varNameToId);
    } else if (tail is UnderscoreTerm) {
      tailTerm = _anonymousWriter(runtime, tail);
    } else {
      throw Exception('Unsupported list tail type: ${tail.runtimeType}');
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
      return rt.ConstTerm(rt.nil);
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
    } else if (head is UnderscoreTerm) {
      headTerm = _anonymousWriter(runtime, head);
    } else {
      throw Exception('Unsupported list head type: ${head.runtimeType}');
    }

    // The tail as written ([_buildListTerm]).
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
    } else if (tail is ConstTerm) {
      tailTerm = rt.ConstTerm(tail.value);
    } else if (tail is StructTerm) {
      tailTerm =
          _buildStructTermForConj(runtime, tail, queryVarWriters, varNameToId);
    } else if (tail is UnderscoreTerm) {
      tailTerm = _anonymousWriter(runtime, tail);
    } else {
      throw Exception('Unsupported list tail type: ${tail.runtimeType}');
    }

    return rt.StructTerm('.', [headTerm, tailTerm]);
  }
}
