/// Program linker: static linking of multi-module GLP programs.
///
/// Given a program root directory, discovers all modules, type-checks each
/// independently, then produces a single flat Program AST where all
/// inter-module calls are resolved to renamed local procedures.
///
/// Specification: docs/modules/glp-project-compilation-spec.md
/// Plan: docs/modules/project-compilation-implementation-plan.md
library;

import 'dart:io';
import 'package:path/path.dart' as ppath;
import 'analyzer.dart' show Analyzer;
import 'ast.dart';
import 'error.dart' show CompileError;
import 'glp_printer.dart';
import 'lexer.dart';
import 'token.dart' show TokenType;
import 'parser.dart';
import 'partial_evaluator.dart';
import 'primitive_layer.dart';
import '../analysis/type_checker/root_scope.dart' show isBuiltinProcedure;
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_checker.dart';
import '../analysis/type_checker/type_identity.dart';
import '../analysis/type_checker/param_expansion.dart'
    show UndefinedDeclarationTypeError, buildAbstractInstance;
import '../analysis/type_checker/program_dfa.dart' show UnknownTypeError;
import '../runtime/module_hierarchy.dart';
import '../vglp/mediator.dart';
import '../vglp/canonical.dart'
    show
        CanonicalContext,
        VolitionalCall,
        checkWritersAtOutputs,
        compileCanonical,
        extractWidgetDeclarations,
        isPaperSyntaxSource,
        volitionalRecord;
import '../vglp/program_compilation.dart';

/// A discovered module in the program tree.
class DiscoveredModule {
  final String filePath;

  /// The module's name: its path from the root ([modulePathName]; TGLP
  /// modules.tex, Compilation, third step) --- `sglp/coins` for
  /// `programs/sglp/coins/self.glp`, `sglp/coins/coins` for
  /// `programs/sglp/coins/coins.glp`.  Every procedure `p/n` and type `T` of the
  /// module is renamed `<moduleName>:p/n` and `<moduleName>:T`, and the
  /// linker's registry is keyed by it.  Until 2026-10-02 it was the file's name
  /// (a `self.glp`'s, its directory's last segment), so two modules of one name
  /// shared a prefix and a registry entry, the second overwriting the first.
  final String moduleName;
  final Module ast;
  TypeEnvironment ancestorScope;
  final bool isSelfGlp;

  /// If an ancestor `self.glp` `-expose`s this module, the normalized
  /// directory of that exposing `self.glp`. Its EXPORTED procedures lift into
  /// that directory's subtree scope. Null for a module nothing exposes.  Set
  /// by [_resolveExposes] on a module the directory walk collected too, which
  /// stays one module (TGLP modules.tex, Compilation).
  String? exposingDir;

  /// Whether the module is in the program only because an `-expose` names it
  /// --- a module outside the directory walk, the root's
  /// `-expose(social#graph#routing#output)` among them --- as against one the
  /// walk collected, which is the program's own whether or not it is exposed
  /// too.
  final bool collectedByExpose;

  /// Whether the module is the root `self.glp`: the first link of every
  /// module's chain, `d_1` (TGLP modules.tex, Definition "Root, Scope": "The
  /// root self.glp is in the scope of every module compiled on the device,
  /// wherever that module's program sits below the root: it is d_1"), named by
  /// the empty path ([modulePathName]) and checked in step 2 against the
  /// language primitives alone ([rootModuleOf]).
  final bool isRoot;

  /// What the `-expose` that names this module lifts into [exposingDir]'s
  /// scope: its exported declarations, read in its own scope, and the types
  /// their signatures carry ([ExposedLift]).  Set by [_resolveExposes] with
  /// [exposingDir]; null for a module nothing exposes.
  ExposedLift? lift;

  /// The `self.glp` files on this module's chain that are not modules of the
  /// program, outermost first, each as a module of its type definitions
  /// alone: the scope of a module that only an `-expose` brings in
  /// ([collectedByExpose]) runs from the root down to its own directory
  /// (Definition "Root, Scope"), and those of its directories outside the
  /// program are in it.  Their types are its types, and the linked program
  /// carries those its declarations and types reference ([linkedFlatModule]);
  /// their procedures are not linked.  Empty for every other module.
  final List<DiscoveredModule> outerScope;

  /// The volitional procedures of the module, where it is compiled from a
  /// `.vglp` source --- the source the loader compiles, or the one beside a
  /// compiled module committed in its place --- by the signature of each
  /// asking clause, `q/(n+1)` ([volitionalRecord]): what a call of one from
  /// another module is checked by ([_CrossModuleResolver.checkVolitionalCalls]),
  /// the compiled declarations carrying no mark of volition.  Empty for every
  /// other module.  Set by [_attachVolitionalRecords].
  Map<String, VolitionalCall> volitional = const {};

  /// The compiled dispatcher entry `d/3` of the module, where it is compiled
  /// from a `.vglp` source whose program defines no type Output: its output
  /// end is anonymous and the dispatcher draws nothing of it ("A vGLP program
  /// declares the type of its output to its person by defining the type
  /// Output, a reserved name", vGLP 169b008, Definition "Canonical
  /// Compilation"; a program that writes to its person and defines none is
  /// refused, vGLP, 2026-10-13 19:25 UTC, item 1).  A call of it giving a
  /// variable for the program's end of the person channel is refused
  /// ([_CrossModuleResolver.checkVolitionalCalls]).  Null for every other
  /// module.  Set by [_attachVolitionalRecords].
  String? silentDispatch;

  DiscoveredModule({
    required this.filePath,
    required this.moduleName,
    required this.ast,
    required this.ancestorScope,
    this.isSelfGlp = false,
    this.exposingDir,
    this.collectedByExpose = false,
    this.isRoot = false,
    this.lift,
    this.outerScope = const [],
  });
}

/// The root `self.glp` at [rootSelfGlpPath] as a module of a program, or null
/// where there is none.
///
/// It joins every program as the first link of its chain (TGLP modules.tex,
/// Compilation, first step: "the compiler collects every .glp file of the
/// program's directory tree, together with the self.glp of each directory from
/// the root down to the program"), and its scope is the language primitives
/// alone, `Π ⊔ d_1` with `d_1` itself (Definition "Root, Scope"), against which
/// it is checked in step 2 like any module.  Its procedures and types are
/// renamed under the empty path in step 3, its name being "the module's path
/// from the root", so a module's call to one of them resolves to the root's own
/// procedure in step 4 and the root's internal calls stay inside it: no module
/// takes over a root helper by defining a procedure of its name.  Until
/// 2026-10-04 it was no module of any program: it was compiled once beside the
/// program, unchecked, and reached at run time by its bare names, so a module
/// defining `mwm1/4` hijacked the root's `mwm/2`, and a type error in it loaded
/// and ran.
///
/// The language primitives are the base of every scope and are not the root's
/// (modules.tex, "Two things are named self.glp and they enter a scope by
/// different routes"): the primitive types are built into the checker, and a
/// kernel or builtin guard the runtime implements is declared in the root by a
/// clause-less declaration that keeps its name in the linked program, a name
/// with no code binding only to the runtime's kernel or guard of that name
/// (IGLP code-format-fragment.tex, Loader, step 3).
DiscoveredModule? rootModuleOf(String? rootSelfGlpPath) {
  if (rootSelfGlpPath == null) return null;
  final file = File(rootSelfGlpPath);
  if (!file.existsSync()) return null;
  final module = Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule();
  enforcePrimitiveLayer(file.path, module, rootSelfGlpPath);
  return DiscoveredModule(
    filePath: file.path,
    moduleName: '',
    ast: module,
    ancestorScope: primitiveScope(),
    isSelfGlp: true,
    isRoot: true,
  );
}

/// Result of linking a program.
class LinkResult {
  final Program program;

  /// The linked declarations in SOURCE type names — the program's external
  /// interface, which an artefact's interface table carries and the parser
  /// reads back (`M:T` is no type name of the language).
  final List<ProcDecl> procDeclarations;

  /// The same declarations with every type reference resolved to the renamed
  /// type of the nearest scope defining it (modules.tex §Compilation, step 4).
  /// These are what the linked program is CHECKED against, where a type name
  /// alone no longer identifies a type: two sibling modules may define one.
  final List<ProcDecl> checkedDeclarations;

  /// Every checked declaration in the program's scope, BEFORE step-5 dead-code
  /// elimination restricts [checkedDeclarations] to the reachable procedures.
  /// The declared type-identity table is built over these (TGLP Implementation
  /// Notes, "The tables": every procedure declared in the module's scope):
  /// `find_type(P/N, T)` refers to a declaration, not to code, and a procedure
  /// that nothing calls is still declared.
  final List<ProcDecl> scopeDeclarations;

  /// The scope the linked program was CHECKED in --- the flat module's
  /// environment ([linkedProgramEnvironment]) --- where the result came from
  /// [checkedLinkedProgram], and null where it came from [linkProgram] alone.
  /// The compiler is given it so that the SRSW relaxations of a typed program
  /// are decided on the same types the checker decided by (TGLP typed-glp.tex,
  /// "Readers of ground types").
  final TypeEnvironment? checkedEnv;

  LinkResult(this.program, this.procDeclarations,
      {List<ProcDecl>? checkedDeclarations, List<ProcDecl>? scopeDeclarations,
      this.checkedEnv})
      : checkedDeclarations = checkedDeclarations ?? procDeclarations,
        scopeDeclarations =
            scopeDeclarations ?? checkedDeclarations ?? procDeclarations;

  /// This result with [checkedEnv] set.
  LinkResult withCheckedEnv(TypeEnvironment env) => LinkResult(
        program,
        procDeclarations,
        checkedDeclarations: checkedDeclarations,
        scopeDeclarations: scopeDeclarations,
        checkedEnv: env,
      );
}

/// Walk the program directory tree and discover all modules.
///
/// For each `.glp` file of the tree, none skipped:
/// - Parse into Module AST
/// - Name the module by its path from the root ([modulePathName]): the root is
///   the directory of [rootSelfGlpPath] where it is given --- the device's
///   root, `programs/` --- and the program's own directory otherwise
/// - Build ancestor type scope chain
///
/// `self.glp` files contribute both types AND procedures to the ancestor scope.
/// Their procedures are compiled to bytecode and renamed like any other module.
///
/// [rootScope] is `Π ⊔ d_1` for the root at [rootSelfGlpPath], built once by
/// the caller (module_hierarchy.dart, rootScope): every module's scope is built
/// over it, so the root's layer is one (buildAncestorScope).  Where none is
/// given it is built here, once for the program.
List<DiscoveredModule> discoverProgram(String rootDir,
    {String? rootSelfGlpPath, TypeEnvironment? rootScope}) {
  final root = Directory(rootDir);
  final programsDir = rootSelfGlpPath != null
      ? File(rootSelfGlpPath).parent.absolute.path
      : null;
  final rs = rootScope ?? _rootScopeOf(rootSelfGlpPath);

  // The program's vGLP sources, read before any module is compiled, and the
  // self.vglp layers, compiled first: each joins the module of the self.glp
  // beside it, which every module of its directory and below has in scope
  // (vGLP at a89a751, sections/parts/canonical-compilation.tex).
  final program = _vglpProgramOf(root, programsDir);
  final layers = program == null
      ? const <String, Module>{}
      : _compileLayers(program, root, programsDir, rootSelfGlpPath, rs);
  final modules = _discoverGlpModules(root, programsDir, rootSelfGlpPath, rs,
      layers: layers);

  // A .vglp source is compiled and joins the program as the module of its own
  // name (vGLP, Definition "Canonical Compilation").  This runs AFTER the
  // exposes are resolved, because the compilation types an answer writer by the
  // position it occurs at and those positions are often arguments of an exposed
  // procedure — `send_net` and the rest of social/graph/routing.
  _addVglpModules(modules, root, programsDir, rootSelfGlpPath, rs,
      program: program, layers: layers);
  _attachVolitionalRecords(modules, program);
  _rootLast(modules);
  return modules;
}

/// The program's vGLP sources ([VglpProgram]), or null where it has none:
/// the self.vglp of each directory of the tree, and of each directory above
/// it below the root, that holds a procedure, a declaration or a type --- a
/// layer of its directory --- and each .vglp module source of the tree that
/// joins the program, compiled on load or standing as its committed compiled
/// module; a source beside a hand-written module of its name does not join
/// it, the hand-written module standing.  "The self.vglp of the root holds
/// widget declarations only" (vGLP, Section 6.3, sections/parts/
/// canonical-compilation.tex): one holding anything else --- a procedure, a
/// declaration, a type, a clause, or text the front end does not read --- is
/// refused, naming the rule ([_refuseRootSelfVglp]).
VglpProgram? _vglpProgramOf(Directory root, String? programsDir) {
  if (!root.existsSync()) return null;
  final files = root
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.vglp'))
      .toList();
  final programsNorm = programsDir == null ? null : _normPath(programsDir);
  final layers = <String>{};
  final sources = <String>[];
  for (final f in files) {
    final name = ppath.basename(f.path);
    if (name == selfVglp) {
      if (programsNorm != null && _normPath(f.parent.path) == programsNorm) {
        _refuseRootSelfVglp(f.path);
      } else {
        layers.add(_normPath(f.path));
      }
      continue;
    }
    final glp = File(ppath.join(
        f.parent.path, '${name.substring(0, name.length - '.vglp'.length)}.glp'));
    if (glp.existsSync() &&
        !glp.readAsStringSync().startsWith(compiledHeader)) {
      continue;  // the hand-written module stands
    }
    sources.add(_normPath(f.path));
  }
  if (programsNorm != null) {
    var dir = _normPath(root.parent.path);
    while (dir != programsNorm && ppath.isWithin(programsNorm, dir)) {
      final f = File(ppath.join(dir, selfVglp));
      if (f.existsSync()) layers.add(_normPath(f.path));
      dir = _normPath(ppath.dirname(dir));
    }
    final rootLayer = File(ppath.join(programsNorm, selfVglp));
    if (rootLayer.existsSync() && !files.any((f) => _normPath(f.path) == _normPath(rootLayer.path))) {
      _refuseRootSelfVglp(rootLayer.path);
    }
  }
  final held = [
    for (final l in layers)
      if (_holdsAt(l)) l
  ]..sort((a, b) {
      final d = _depth(ppath.dirname(a)).compareTo(_depth(ppath.dirname(b)));
      return d != 0 ? d : a.compareTo(b);
    });
  if (held.isEmpty && sources.isEmpty) return null;
  return VglpProgram.of(
      layers: held, modules: sources, programsDir: programsDir);
}

/// Refuse the root's self.vglp at [path] unless it holds widget declarations
/// and nothing else: "the self.vglp of the root holds widget declarations
/// only" (vGLP, Section 6.3, sections/parts/canonical-compilation.tex).  What
/// is left of it once its widget declarations are taken out is comments and
/// blank space, or it is refused, whether or not the vGLP front end reads it.
void _refuseRootSelfVglp(String path) {
  final text = File(path).readAsStringSync();
  String? found;
  try {
    final rest = Lexer(extractWidgetDeclarations(text).stripped).tokenize();
    final t = rest.firstWhere((t) => t.type != TokenType.EOF,
        orElse: () => rest.last);
    if (t.type != TokenType.EOF) found = 'line ${t.line}: ${t.lexeme}';
  } on CompileError catch (e) {
    found = 'line ${e.line}: ${e.message}';
  }
  if (found == null) return;
  throw CompileError(
      '$path holds more than widget declarations ($found): "the self.vglp '
      'of the root holds widget declarations only" (vGLP, Section 6.3, '
      'sections/parts/canonical-compilation.tex)',
      1, 1, phase: 'analyzer');
}

/// Whether the self.vglp at [path] holds a procedure, a declaration or a
/// type: one of widget declarations alone is no layer.
bool _holdsAt(String path) => selfVglpHoldsAt(path);

/// Compile each self.vglp layer of [program], outermost first, by the
/// canonical compilation, its questions the program's, with neither
/// dispatcher nor construct processes, its names fresh against those of the
/// self.glp beside it, whose module it joins ([_withLayer]): the module of
/// each layer, by its directory.
///
/// A layer in a directory with no self.glp joins the empty module of its
/// virtual self.glp ([isVirtualSelfGlp]).  One beside a self.glp the compiler
/// wrote from it, its first line the compiler's header, is not compiled: that
/// self.glp is loaded as the directory's own, "linked as N.glp is for
/// N.vglp" (vGLP Cowork, 2026-10-16 11:25 UTC, item 1), the self.vglp
/// staying a source of [program].  Where [emitted] is given, for `:emit`,
/// each layer of a directory with no self.glp or with one the compiler wrote
/// is compiled, the latter as if it had none, and its text is recorded there
/// by the path of that self.glp ([emitVglpSources]).
Map<String, Module> _compileLayers(VglpProgram program, Directory root,
    String? programsDir, String? rootSelfGlpPath, TypeEnvironment? rs,
    {Map<String, String>? emitted}) {
  final layers = <String, Module>{};
  final resolveIn = _sourceTypes(program, root, programsDir, rootSelfGlpPath,
      rs, layers);
  for (final path in program.layers) {
    final dir = _normPath(File(path).parent.path);
    final selfGlp = ppath.join(dir, 'self.glp');
    final compiled = _isCompiledSelfGlp(selfGlp);
    if (compiled && emitted == null) continue;
    final self = compiled ? Module(line: 1, column: 1) : _parseFile(selfGlp);
    final above = _layeredScope(selfGlp, root.absolute.path, programsDir,
        rootSelfGlpPath, rs, layers);
    final selfTypes = {for (final td in self.typeDefs) td.name};
    final context = program.contextFor(path,
        resolvable: (n) =>
            selfTypes.contains(n) ||
            above.types.containsKey(n) ||
            above.typeTemplates.containsKey(n),
        resolveIn: resolveIn,
        reservedTypes: selfTypes,
        reservedProcedures: {
          for (final d in self.procDeclarations) d.name,
          for (final p in self.procedures) p.name,
        });
    final String text;
    try {
      text = compileCanonical(File(path).readAsStringSync(),
              reachingOf: program.readerFor(path), context: context)
          .source;
    } on CompileError catch (e) {
      throw CompileError('$path: ${e.message}', e.line, e.column,
          category: e.category);
    }
    final module = Parser(Lexer(text).tokenize()).parseModule();
    _withLayer(self, module, selfGlp, path);
    if (emitted != null && (compiled || isVirtualSelfGlp(selfGlp))) {
      emitted[selfGlp] = text;
    }
    if (!compiled) layers[dir] = module;
  }
  return layers;
}

/// Whether the self.glp at [path] is on disc and the compiler wrote it, its
/// first line the compiler's header.
bool _isCompiledSelfGlp(String path) {
  final f = File(path);
  return f.existsSync() && f.readAsStringSync().startsWith(compiledHeader);
}

/// The type a name denotes in the scope of a source of [program], the
/// source's own definitions first: a layer's, then those of the self.glp
/// beside it and the scope above; a module's, then its scope's.  Null where
/// none defines it, or where its scope cannot be built yet.
TypeDef? Function(String source, String name) _sourceTypes(
    VglpProgram program,
    Directory root,
    String? programsDir,
    String? rootSelfGlpPath,
    TypeEnvironment? rs,
    Map<String, Module> layers) {
  final scopes = <String, TypeEnvironment?>{};
  return (source, name) {
    final own = program.fronts[source]?.types[name];
    if (own != null) return own;
    final isLayer = program.layers.contains(source);
    final target = isLayer
        ? ppath.join(File(source).parent.path, 'self.glp')
        : source;
    // A self.glp the compiler wrote from the layer stands for it, and is no
    // part of the layer's scope ([_compileLayers]).
    if (isLayer && !_isCompiledSelfGlp(target)) {
      final self = _parseFile(target);
      for (final td in self.typeDefs) {
        if (td.name == name) return td;
      }
    }
    final scope = scopes.putIfAbsent(target, () {
      try {
        return _layeredScope(target, root.absolute.path, programsDir,
            rootSelfGlpPath, rs, layers);
      } catch (_) {
        return null;
      }
    });
    return scope?.typeTemplates[name] ?? scope?.types[name];
  };
}

/// The context of the module source at [path] ([CanonicalContext]), [scope]
/// its scope.
CanonicalContext _contextFor(VglpProgram program, String path,
        TypeEnvironment scope, TypeDef? Function(String, String) resolveIn) =>
    program.contextFor(path,
        resolvable: (n) =>
            scope.types.containsKey(n) || scope.typeTemplates.containsKey(n),
        resolveIn: resolveIn);

/// The module of the file at [path]; a virtual self.glp's is empty
/// ([selfGlpSource]).
Module _parseFile(String path) => Parser(Lexer(isSelfGlpFile(path)
            ? selfGlpSource(path)
            : File(path).readAsStringSync())
        .tokenize())
    .parseModule();

/// The module of the self.glp [self], at [selfPath], with the compiled text
/// of the self.vglp beside it, [layer], at [layerPath]: one module, one layer
/// of every scope below it, "as its self.glp holds those of GLP" (vGLP,
/// sections/parts/canonical-compilation.tex).  A type, a procedure or a
/// declaration of one name in both is refused, naming both files, as one
/// module's would be.
Module _withLayer(Module self, Module layer, String selfPath, String layerPath) {
  final selfTypes = {for (final td in self.typeDefs) td.name};
  final selfProcs = {
    for (final p in self.procedures) p.signature,
    for (final d in self.procDeclarations)
      if (!d.imported) d.key,
  };
  String? clash;
  for (final td in layer.typeDefs) {
    if (selfTypes.contains(td.name)) clash ??= 'the type ${td.name}';
  }
  for (final p in layer.procedures) {
    if (selfProcs.contains(p.signature)) {
      clash ??= 'the procedure ${p.signature}';
    }
  }
  if (clash != null) {
    throw CompileError(
        '$selfPath and $layerPath both define $clash: the self.vglp of a '
        'directory joins its self.glp, one layer of the scope (vGLP, '
        'sections/parts/canonical-compilation.tex), and one name is defined '
        'once in it',
        1, 1, phase: 'analyzer');
  }
  return Module(
    typeDefs: [...self.typeDefs, ...layer.typeDefs],
    procDeclarations: [...self.procDeclarations, ...layer.procDeclarations],
    paramProcDecls: [...self.paramProcDecls, ...layer.paramProcDecls],
    procedures: [...self.procedures, ...layer.procedures],
    compileMode: self.compileMode,
    exposes: self.exposes,
    displayDecls: self.displayDecls,
    volitionalDeclarations: self.volitionalDeclarations,
    line: self.line,
    column: self.column,
  );
}

/// The scope of the file at [targetFile] over its chain ([discoverSelfChain],
/// [buildAncestorScope]), each directory's layer ([layers]) merged with the
/// self.glp it joins, one module: the language primitives and the root, then
/// each self.glp of the chain, outermost first.  With no layer on the chain,
/// [buildAncestorScope]'s.
TypeEnvironment _layeredScope(String targetFile, String rootDir,
    String? programsDir, String? rootSelfGlpPath, TypeEnvironment? rs,
    Map<String, Module> layers) {
  final chain = discoverSelfChain(
    targetFile: File(targetFile).absolute.path,
    rootDir: rootDir,
    programsDir: programsDir,
  );
  if (layers.isEmpty ||
      !chain.any((p) => layers.containsKey(_normPath(File(p).parent.path)))) {
    return buildAncestorScope(
        chain: chain, rootSelfGlpPath: rootSelfGlpPath, rootScope: rs);
  }
  var env = buildAncestorScope(
      chain: const [], rootSelfGlpPath: rootSelfGlpPath, rootScope: rs);
  final rootSelf = rootSelfGlpPath != null && File(rootSelfGlpPath).existsSync()
      ? File(rootSelfGlpPath)
      : null;
  final r = rootSelf?.parent.path;
  for (final p in chain) {
    if (rootSelf != null &&
        File(p).absolute.path == rootSelf.absolute.path) {
      continue;
    }
    final layer = layers[_normPath(File(p).parent.path)];
    if (layer == null) {
      env = mergeSelfGlpFileIntoScope(env, p, root: r);
      continue;
    }
    final combined = _withLayer(_parseFile(p), layer, p, p);
    try {
      env = mergeModuleIntoScope(
          liftExposedTypes(env, combined, p, root: r), combined,
          label: r != null
              ? modulePathName(p, r)
              : ppath.basename(File(p).absolute.parent.path));
    } on UndefinedDeclarationTypeError catch (e) {
      throw e.inFile(p);
    }
  }
  return env;
}

/// Each module compiled from a `.vglp` source given the record of its
/// volitional procedures, read off that source ([volitionalRecord]): a module
/// the loader compiled from `N.vglp`, and a compiled `N.glp` committed beside
/// its `N.vglp` (its first line the compiler's header), which the loader links
/// in the source's place.  "Every call of a volitional procedure, in a clause
/// body or in the initial goal, holds a writer at each output position"
/// (vGLP, Definition "Volitional Procedure, ..., vGLP Program"), and "the GLP
/// module that forms the program's initial goal imports that module's
/// compiled procedures" (Definition "Canonical Compilation"): the record is
/// what the calls of that module are checked by.
void _attachVolitionalRecords(List<DiscoveredModule> modules,
    [VglpProgram? program]) {
  for (final m in modules) {
    final path = m.filePath;
    if (program != null && isSelfGlpFile(path)) {
      // A self.glp with its layer: the layer's volitional procedures.
      final front = program.fronts[
          _normPath(ppath.join(File(path).parent.path, selfVglp))];
      if (front != null &&
          program.layers.contains(
              _normPath(ppath.join(File(path).parent.path, selfVglp)))) {
        m.volitional = {
          for (final v in front.volitional.values) v.compiledKey: v
        };
      }
      continue;
    }
    if (path.endsWith('.vglp')) {
      m.volitional = volitionalRecord(File(path).readAsStringSync());
    } else if (path.endsWith('.glp') && !isSelfGlpFile(path)) {
      final source = File('${path.substring(0, path.length - 4)}.vglp');
      if (!source.existsSync()) continue;
      final compiled = File(path);
      if (!compiled.existsSync() ||
          !compiled.readAsStringSync().startsWith(compiledHeader)) {
        continue;
      }
      m.volitional = volitionalRecord(source.readAsStringSync());
    }
    if (m.volitional.isNotEmpty || path.endsWith('.vglp')) {
      m.silentDispatch = _silentDispatch(m.ast);
    }
  }
}

/// The compiled dispatcher entry of a module compiled from a `.vglp` source
/// whose program defines no Output: its procedure `d/3`, whose one clause
/// calls the dispatcher `d/4`, which draws nothing of the program's output;
/// a program defining Output has its `d/3` call `d/5`, the widget of Output
/// added (constructs.dart, `_Generator.build`).  Null where there is none.
String? _silentDispatch(Module ast) {
  final procs = {for (final p in ast.procedures) p.signature: p};
  for (final p in ast.procedures) {
    if (p.arity != 3 || p.clauses.length != 1) continue;
    if (!procs.containsKey('${p.name}/4')) continue;
    final body = p.clauses.single.body ?? const <Goal>[];
    if (body.any((g) => g.functor == p.name && g.arity == 4)) return p.name;
  }
  return null;
}

/// `Π ⊔ d_1` for the root at [rootSelfGlpPath], or null where there is none.
TypeEnvironment? _rootScopeOf(String? rootSelfGlpPath) =>
    rootSelfGlpPath != null && File(rootSelfGlpPath).existsSync()
        ? rootScope(rootSelfGlpPath)
        : null;

/// [modules] with the root `self.glp` moved to the end, the outermost scope
/// last, so that where a lookup over the modules takes the first of a name ---
/// a forwarding export's declaration, an artefact's type definitions --- a
/// program module's is taken before the root's.
void _rootLast(List<DiscoveredModule> modules) {
  final roots = modules.where((m) => m.isRoot).toList();
  if (roots.isEmpty) return;
  modules.removeWhere((m) => m.isRoot);
  modules.addAll(roots);
}

/// The directory module names are paths from: the device's root, the directory
/// of the root `self.glp` ([programsDir]), where it is known, and the program's
/// own directory [programRoot] otherwise.
String _nameRoot(String? programsDir, String programRoot) =>
    programsDir ?? Directory(programRoot).absolute.path;

/// The `.glp` modules of the tree, with their ancestor scopes and the exposes
/// resolved: everything of [discoverProgram] but the compiled `.vglp` sources,
/// which `:emit` compiles in this same scope and writes out instead.
List<DiscoveredModule> _discoverGlpModules(Directory root, String? programsDir,
    String? rootSelfGlpPath, TypeEnvironment? rs,
    {Map<String, Module> layers = const {}}) {
  if (!root.existsSync()) {
    throw ArgumentError('Program root directory not found: ${root.path}');
  }
  // A program lies at or below the root (TGLP modules.tex, "Scope
  // construction"); one outside it is refused here, before any module of it is
  // named or scoped.
  if (programsDir != null) requireUnderRoot(root.path, programsDir);

  final modules = <DiscoveredModule>[];
  final nameRoot = _nameRoot(programsDir, root.path);

  // The root `programs/` directory bounds the ancestor scope chain. When known,
  // discovery extends above the program root up to (excluding) this directory.

  // Recursively find all .glp files, and the virtual self.glp of each
  // directory of the tree that has one ([isVirtualSelfGlp]): a module of the
  // program as one on disc is, the self.vglp's layer joining it.
  final listed = root.listSync(recursive: true).whereType<File>().toList();
  final glpFiles = [
    ...listed.where((f) => f.path.endsWith('.glp')),
    for (final f in listed)
      if (ppath.basename(f.path) == selfVglp &&
          isVirtualSelfGlp(ppath.join(f.parent.path, 'self.glp')))
        File(ppath.join(f.parent.path, 'self.glp')),
  ];

  // Every .glp file of the tree is a module of the program, and none is
  // skipped (TGLP modules.tex, Compilation, first step: "the compiler collects
  // every .glp file of the program's directory tree").  Until 2026-10-03 a
  // file named boot_direct.glp or mad_boot.glp, and every file under a
  // directory named mad_boot, was left out by its name.
  for (final file in glpFiles) {
    final filename = file.path.split(Platform.pathSeparator).last;

    // Parse the module
    final source = filename == 'self.glp'
        ? selfGlpSource(file.path)
        : file.readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    var module = parser.parseModule();

    // Enforce "Admission to the Primitive Layer" (Rule A / Rule B) at load time.
    enforcePrimitiveLayer(file.path, module, rootSelfGlpPath);

    // A self.glp with the self.vglp beside it: one module ([_withLayer]).
    final layer = filename == 'self.glp'
        ? layers[_normPath(file.parent.path)]
        : null;
    if (layer != null) {
      module = _withLayer(module, layer, file.path,
          ppath.join(file.parent.path, selfVglp));
    }

    // The module's name is its path from the root: a self.glp's is its
    // directory's, any other module's its directory's and its file name.
    final moduleName = modulePathName(file.path, nameRoot);

    // Build ancestor scope chain (extends up to programs/ when known), each
    // directory's layer with its self.glp.
    final ancestorScope = _layeredScope(file.path, root.absolute.path,
        programsDir, rootSelfGlpPath, rs, layers);

    modules.add(DiscoveredModule(
      filePath: file.path,
      moduleName: moduleName,
      ast: module,
      ancestorScope: ancestorScope,
      isSelfGlp: filename == 'self.glp',
    ));
  }

  // Add the program's filesystem context (ancestor self.glp above the root) and
  // resolve -expose directives.
  _addAncestorContextAndExposes(
      modules, root.absolute.path, programsDir, rootSelfGlpPath, nameRoot, rs,
      layers: layers);
  return modules;
}

/// Compile each `.vglp` source of the tree and add it as a module.
///
/// A `.vglp` whose directory holds a `.glp` of the same name is SKIPPED, and
/// the hand-written module stands: switching a deployed program onto its
/// compiled agent is its own change, not a side effect of loading it.  A
/// directory whose program is written in vGLP alone has no such file, and its
/// sources compile and run.
///
/// A source in the paper's syntax compiles by the canonical compilation of
/// vGLP at 16b3b54; one in the old syntax keeps its old compilation against
/// the generic mediator, and is not compiled where the mediator is missing, as
/// before (compileVglpSource).
void _addVglpModules(List<DiscoveredModule> modules, Directory root,
    String? programsDir, String? rootSelfGlpPath, TypeEnvironment? rs,
    {VglpProgram? program, Map<String, Module> layers = const {}}) {
  final vglpFiles = root
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.vglp'))
      .where((f) {
    final filename = f.path.split(Platform.pathSeparator).last;
    // A self.vglp is no module: its layer joins its self.glp's.
    if (filename == selfVglp) return false;
    final stem = filename.substring(0, filename.length - '.vglp'.length);
    // the hand-written module stands
    return !File('${f.parent.path}${Platform.pathSeparator}$stem.glp')
        .existsSync();
  }).toList();
  if (vglpFiles.isEmpty) return;

  final mediator = _mediatorSource(programsDir);
  final texts = {for (final f in vglpFiles) f.path: f.readAsStringSync()};
  final nameRoot = _nameRoot(programsDir, root.path);

  final resolveIn = program == null
      ? null
      : _sourceTypes(program, root, programsDir, rootSelfGlpPath, rs, layers);
  for (final file in vglpFiles) {
    final text = texts[file.path]!;
    final paper =
        isPaperSyntaxSource(text) || (program?.reaches(file.path) ?? false);
    if (!paper && mediator == null) continue;

    final ancestorScope = _vglpScope(
        file, modules, root, programsDir, rootSelfGlpPath, rs, layers);

    final String compiledSource;
    try {
      compiledSource = compileVglpSource(text,
          mediator: mediator,
          scope: ancestorScope,
          path: file.path,
          reachingOf: program?.readerFor(file.path),
          context: program == null
              ? null
              : _contextFor(program, file.path, ancestorScope, resolveIn!));
    } on UndefinedDeclarationTypeError catch (e) {
      throw e.inFile(file.path);
    }
    final compiledAst =
        Parser(Lexer(compiledSource).tokenize()).parseModule();

    modules.add(DiscoveredModule(
      filePath: file.path,
      moduleName: modulePathName(file.path, nameRoot),
      ast: compiledAst,
      ancestorScope: ancestorScope,
    ));
  }
}

/// The scope a `.vglp` source is compiled in: its ancestor chain, with the
/// exposes merged.  The exposes are resolved already and were merged into the
/// scope of every module in [modules]; the compiled module joins after, so it
/// gets the same merge --- a .vglp source calls the exposed routers as its
/// sibling modules do.  The loader and `:emit` both compile in this scope, so
/// the emitted text is what the load produces in memory.
TypeEnvironment _vglpScope(File file, List<DiscoveredModule> modules,
    Directory root, String? programsDir, String? rootSelfGlpPath,
    TypeEnvironment? rs, [Map<String, Module> layers = const {}]) {
  final scope = _layeredScope(file.path, root.absolute.path, programsDir,
      rootSelfGlpPath, rs, layers);
  return _withExposedProcedures(file.path, scope, modules,
      modules.where((m) => m.exposingDir != null).toList());
}

/// The generic mediator source, `programs/vglp/`, which the compilation
/// instantiates into every compiled program.
MediatorSource? _mediatorSource(String? programsDir) {
  if (programsDir == null) return null;
  final dir = '$programsDir${Platform.pathSeparator}vglp';
  if (!Directory(dir).existsSync()) return null;
  return MediatorSource.fromDirectory(dir);
}

/// Discover a single self-contained module as a one-module program (modules.tex
/// §Design: "A Typed GLP program is either a self-contained module or a
/// directory with a self.glp module"). The module is the program's only own
/// module — it has no self.glp of its own, so every one of its procedures is an
/// entry point (§Static Linking). Its filesystem context (ancestor self.glp
/// above its directory, up to programs/) is added, so it links and runs through
/// the same pipeline as a directory program.
///
/// [rootScope] is as for [discoverProgram].
List<DiscoveredModule> discoverSingleModule(String filePath,
    {String? rootSelfGlpPath, TypeEnvironment? rootScope}) {
  final file = File(filePath);
  if (!file.existsSync()) {
    throw ArgumentError('Module file not found: $filePath');
  }
  final programsDir = rootSelfGlpPath != null
      ? File(rootSelfGlpPath).parent.absolute.path
      : null;
  // A program lies at or below the root (TGLP modules.tex, "Scope
  // construction"); one outside it is refused before it is read.
  if (programsDir != null) requireUnderRoot(filePath, programsDir);
  final rs = rootScope ?? _rootScopeOf(rootSelfGlpPath);

  final module =
      Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule();
  enforcePrimitiveLayer(file.path, module, rootSelfGlpPath);

  final dir = file.parent.absolute.path;
  final chain = discoverSelfChain(
      targetFile: file.absolute.path, rootDir: dir, programsDir: programsDir);
  final nameRoot = _nameRoot(programsDir, dir);

  final modules = <DiscoveredModule>[
    DiscoveredModule(
      filePath: file.path,
      moduleName: modulePathName(file.path, nameRoot),
      ast: module,
      ancestorScope: buildAncestorScope(
          chain: chain, rootSelfGlpPath: rootSelfGlpPath, rootScope: rs),
      isSelfGlp: false,
    ),
  ];

  // The module's own-directory self.glp is its nearest scope: it is in scope for
  // type checking, so it must also be linked, or its procedures are unresolved
  // at runtime. (The ancestor self.glp ABOVE the directory are added below.)
  final ownSelf = File('$dir${Platform.pathSeparator}self.glp');
  if (file.path.split(Platform.pathSeparator).last != 'self.glp' &&
      ownSelf.existsSync()) {
    final selfModule =
        Parser(Lexer(ownSelf.readAsStringSync()).tokenize()).parseModule();
    final selfChain = discoverSelfChain(
        targetFile: ownSelf.absolute.path, rootDir: dir, programsDir: programsDir);
    modules.add(DiscoveredModule(
      filePath: ownSelf.path,
      moduleName: modulePathName(ownSelf.path, nameRoot),
      ast: selfModule,
      ancestorScope: buildAncestorScope(
          chain: selfChain, rootSelfGlpPath: rootSelfGlpPath, rootScope: rs),
      isSelfGlp: true,
    ));
  }

  _addAncestorContextAndExposes(
      modules, dir, programsDir, rootSelfGlpPath, nameRoot, rs);
  _attachVolitionalRecords(modules);
  return modules;
}

/// Add a program's filesystem context to [modules]: the ancestor `self.glp`
/// files ABOVE [rootAbsPath] (up to but excluding `programs/`), linked like any
/// other module so their (multi-clause, parameterised) procedures resolve for
/// descendants; then resolve `-expose` directives (including the root
/// `programs/self.glp`'s, which is itself realised by the root-scope mechanism).
///
/// [nameRoot] is the directory every module is named from ([_nameRoot]).
void _addAncestorContextAndExposes(
    List<DiscoveredModule> modules,
    String rootAbsPath,
    String? programsDir,
    String? rootSelfGlpPath,
    String nameRoot,
    TypeEnvironment? rs,
    {Map<String, Module> layers = const {}}) {
  if (programsDir != null) {
    for (final selfPath in _ancestorSelfGlpFiles(rootAbsPath, programsDir)) {
      var selfModule =
          Parser(Lexer(selfGlpSource(selfPath)).tokenize()).parseModule();
      final layer = layers[_normPath(File(selfPath).parent.path)];
      if (layer != null) {
        selfModule = _withLayer(selfModule, layer, selfPath,
            ppath.join(File(selfPath).parent.path, selfVglp));
      }
      modules.add(DiscoveredModule(
        filePath: selfPath,
        moduleName: modulePathName(selfPath, nameRoot),
        ast: selfModule,
        ancestorScope: _layeredScope(selfPath, File(selfPath).parent.path,
            programsDir, rootSelfGlpPath, rs, layers),
        isSelfGlp: true,
      ));
    }
  }

  // The root self.glp is a module of every program, the first link of every
  // module's chain ([rootModuleOf]), and its -expose directives resolve like
  // any other self.glp's: its exposing directory is the root, whose subtree is
  // every discovered module.  Until 2026-10-04 it was parsed here as an
  // exposer-only seed and never linked.
  final rootModule = rootModuleOf(rootSelfGlpPath);
  if (rootModule != null &&
      !modules.any((m) => _normPath(m.filePath) == _normPath(rootModule.filePath))) {
    modules.add(rootModule);
  }

  _resolveExposes(modules, programsDir, rootSelfGlpPath, nameRoot, rs,
      layers: layers);
  _rootLast(modules);
}

/// Normalize a path: absolute, `..`/`.` resolved, no trailing slash.
String _normPath(String p) {
  var n = ppath.normalize(Directory(p).absolute.path);
  if (n.length > 1 && n.endsWith(Platform.pathSeparator)) {
    n = n.substring(0, n.length - 1);
  }
  return n;
}

/// True if [childDir] is [ancestorDir] or below it.
bool _dirUnder(String childDir, String ancestorDir) =>
    childDir == ancestorDir ||
    childDir.startsWith('$ancestorDir${Platform.pathSeparator}');

/// Resolve `-expose` directives among [modules] (mutates the list).
///
/// For each exposing module, each `-expose(a#b#c).` names the module file
/// `<exposing self.glp dir>/a/b/c.glp`. That file is parsed, added as a linkable
/// module tagged with the exposing directory, and its `-expose` directives are
/// followed transitively. Two modules exposed at one level that share an
/// exported name/arity is a compile-time error.  Each exposed module's lift ---
/// its exported declarations read in its own scope, and the types their
/// signatures carry ([ExposedLift]) --- is taken once, with the lifts of its
/// exposer's other directives ([exposedLifts]), over the exposer's ancestor
/// scope, and merged into the ancestorScope of every module in the exposing
/// directory's subtree.
void _resolveExposes(List<DiscoveredModule> modules, String? programsDir,
    String? rootSelfGlpPath, String nameRoot, TypeEnvironment? rs,
    {Map<String, Module> layers = const {}}) {
  final pending = <DiscoveredModule>[
    ...modules.where((m) => m.ast.exposes.isNotEmpty),
  ];
  final collectedFiles = <String>{};
  // exposingDir(norm) -> exported sig -> exposed module name (collision check)
  final perDirSig = <String, Map<String, String>>{};
  // Each exposed module, by its file, with the exposer whose directive set
  // its exposing directory: the lift is that directive's.
  final exposerOf = <DiscoveredModule, DiscoveredModule>{};
  // The self.glp files on the chains of the modules only an -expose brings
  // in that no module of the program holds, by file ([outerScopeOf]).
  final outer = <String, DiscoveredModule>{};

  // The self.glp files on the chain of the file at [path] that no module of
  // the program holds, outermost first, each a module of its type
  // definitions alone ([DiscoveredModule.outerScope]).
  List<DiscoveredModule> outerScopeOf(String path) {
    final scope = <DiscoveredModule>[];
    for (final self in discoverSelfChain(
        targetFile: File(path).absolute.path,
        rootDir: File(path).parent.path,
        programsDir: programsDir)) {
      final norm = _normPath(self);
      if (modules.any((m) => _normPath(m.filePath) == norm)) continue;
      final above = [...scope];
      scope.add(outer.putIfAbsent(norm, () {
        final full = Parser(Lexer(selfGlpSource(self)).tokenize())
            .parseModule();
        return DiscoveredModule(
          filePath: self,
          moduleName: modulePathName(self, nameRoot),
          ast: Module(typeDefs: [
            ...full.typeDefs,
            ...?layers[_normPath(File(self).parent.path)]?.typeDefs,
          ], line: full.line, column: full.column),
          ancestorScope: TypeEnvironment.empty(),
          isSelfGlp: true,
          collectedByExpose: true,
          outerScope: above,
        );
      }));
    }
    return scope;
  }

  while (pending.isNotEmpty) {
    final exposer = pending.removeLast();
    final exposerDir = File(exposer.filePath).parent.path;
    final exposingDirNorm = _normPath(exposerDir);
    final sigMap = perDirSig.putIfAbsent(exposingDirNorm, () => {});

    for (final path in exposer.ast.exposes) {
      final rel = path.split('#').join(Platform.pathSeparator);
      final file = File('$exposerDir${Platform.pathSeparator}$rel.glp');
      if (!file.existsSync()) {
        throw Exception('-expose: module file not found: ${file.path}\n'
            '  from -expose($path) in ${exposer.filePath}');
      }

      final exposedAst =
          Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule();
      final exposedName = modulePathName(file.path, nameRoot);

      // Collision: exported sigs unique among modules exposed at this level.
      for (final d in exposedAst.procDeclarations) {
        if (!d.exported) continue;
        final sig = '${d.name}/${d.arity}';
        final prev = sigMap[sig];
        if (prev != null && prev != exposedName) {
          throw Exception(
              '-expose collision at $exposingDirNorm: procedure $sig is '
              'exposed by both "$prev" and "$exposedName".');
        }
        sigMap[sig] = exposedName;
      }

      if (collectedFiles.contains(_normPath(file.path))) continue;
      collectedFiles.add(_normPath(file.path));

      // A file the program already holds --- one the directory walk collected,
      // or the module a single-file load names --- is that one module, now
      // exposed as well: each .glp file is one module, its procedures emitted
      // once (TGLP modules.tex, Compilation, first and third steps).  Until
      // 2026-10-03 a second module of the same file and name was added here,
      // and the linker emitted the file's procedures twice (tests/expose/basic:
      // util/strutil:twice/2, util/plist:pmerge/3; system/mad_predicates.glp
      // loaded alone).  Its -expose directives are on the worklist already.
      final held = modules.where(
          (m) => _normPath(m.filePath) == _normPath(file.path));
      if (held.isNotEmpty) {
        if (held.first.exposingDir == null) {
          held.first.exposingDir = exposingDirNorm;
          exposerOf[held.first] = exposer;
        }
        continue;
      }

      final exposedDM = DiscoveredModule(
        filePath: file.path,
        moduleName: exposedName,
        ast: exposedAst,
        ancestorScope: _layeredScope(file.path, file.parent.path, programsDir,
            rootSelfGlpPath, rs, layers),
        isSelfGlp: false,
        exposingDir: exposingDirNorm,
        collectedByExpose: true,
        outerScope: outerScopeOf(file.path),
      );
      exposerOf[exposedDM] = exposer;
      modules.add(exposedDM);
      if (exposedAst.exposes.isNotEmpty) pending.add(exposedDM);
    }
  }

  // The lifts, an exposer's together, over its ancestor scope, which no lift
  // has been merged into yet: what -expose lifts is the exposed module's and
  // is read in its own scope, the same for every module receiving it.
  final byExposer = <DiscoveredModule, List<DiscoveredModule>>{};
  for (final e in exposerOf.entries) {
    byExposer.putIfAbsent(e.value, () => []).add(e.key);
  }
  for (final e in byExposer.entries) {
    final exposer = e.key;
    final lifts = exposedLifts(exposer.ast, exposer.filePath,
        exposer.ancestorScope,
        root: nameRoot);
    final exposerDir = File(exposer.filePath).parent.path;
    for (var i = 0; i < exposer.ast.exposes.length; i++) {
      final rel = exposer.ast.exposes[i].split('#').join(Platform.pathSeparator);
      final norm = _normPath('$exposerDir${Platform.pathSeparator}$rel.glp');
      for (final m in e.value) {
        if (m.lift == null && _normPath(m.filePath) == norm) m.lift = lifts[i];
      }
    }
  }

  // Type-env lift: merge each exposed module's lift into the scope of every
  // module in the exposing subtree.
  final exposed = modules.where((m) => m.exposingDir != null).toList();
  if (exposed.isEmpty) return;
  for (final m in modules) {
    // A module only an -expose brings in keeps the scope of its own chain; one
    // the walk collected gets the lift whether or not it is exposed too.  The
    // root self.glp is checked against the language primitives alone: what it
    // exposes is for the modules below it.
    if (m.collectedByExpose || m.isRoot) continue;
    // An exposing self.glp's scope is its ancestors' (discoverSelfChain), so
    // the types its own directives lift are layered over it here, as every
    // scope below it has them from its chain (mergeSelfGlpFileIntoScope): "as
    // if defined in its self.glp", its own definitions over them, and over
    // its ancestors' (liftExposedTypes).  Until 2026-10-07 they filled the
    // gaps of the ancestors' scope ([_mergeExposed]).
    if (m.ast.exposes.isNotEmpty) {
      m.ancestorScope = liftExposedTypes(m.ancestorScope, m.ast, m.filePath,
          root: nameRoot);
    }
    m.ancestorScope =
        _withExposedProcedures(m.filePath, m.ancestorScope, modules, exposed);
  }
}

/// [base], the scope of the module at [filePath] built over its chain
/// ([buildAncestorScope]), with the procedures every `-expose` of [exposed]
/// that reaches it lifts, each at the level of its exposing directory: "as if
/// defined in its self.glp ... Shadowing applies as usual" (TGLP modules.tex,
/// "The -expose directive"; Definition (Root, Scope)).  So a lifted procedure
/// shadows a procedure of its name and arity that an ancestor of the exposing
/// directory declares, and one that a `self.glp` of the exposing directory or
/// of a directory between it and the module declares shadows it, the
/// exposing `self.glp`'s own declaration included; the module's own are
/// merged over the scope when it is checked.  The lifts are taken outermost
/// exposing directory first, so that a nearer one shadows a farther one.
/// Until 2026-10-07 a lifted procedure filled a gap only, and every
/// declaration already in [base] won, an ancestor's included
/// (programs/tests/expose/lift_shadows; GLP, 2026-10-04 13:21 UTC, "12:00",
/// item 5).
TypeEnvironment _withExposedProcedures(String filePath, TypeEnvironment base,
    List<DiscoveredModule> modules, List<DiscoveredModule> exposed) {
  final modDir = _normPath(File(filePath).parent.path);
  final reaching = [
    for (final e in exposed)
      if (_dirUnder(modDir, e.exposingDir!)) e
  ]..sort((a, b) => _depth(a.exposingDir!).compareTo(_depth(b.exposingDir!)));
  if (reaching.isEmpty) return base;
  // The procedures each self.glp of the module's chain declares, by its
  // directory; the module itself is not on its chain.
  final declaredAt = <String, Set<String>>{};
  for (final s in modules) {
    if (!(s.isRoot || s.isSelfGlp || isSelfGlpFile(s.filePath))) continue;
    if (_normPath(s.filePath) == _normPath(filePath)) continue;
    final sDir = _normPath(File(s.filePath).parent.path);
    if (!_dirUnder(modDir, sDir)) continue;
    declaredAt.putIfAbsent(sDir, () => {}).addAll([
      for (final d in s.ast.procDeclarations) d.qualifiedKey,
      for (final d in s.ast.paramProcDecls) d.qualifiedKey,
    ]);
  }
  var scope = base;
  for (final e in reaching) {
    final nearer = <String>{
      for (final at in declaredAt.entries)
        if (_dirUnder(at.key, e.exposingDir!)) ...at.value
    };
    scope = _mergeExposed(scope, e.lift!.scope,
        label: e.moduleName, nearer: nearer);
  }
  return scope;
}

/// The number of segments of the normalised directory [dir].
int _depth(String dir) => ppath.split(dir).length;

/// Merge an exposed module's lift [exposed] into [base], the scope it reaches
/// ([_withExposedProcedures]): each lifted procedure shadows the declaration
/// of its name and arity in [base], in both maps, so that a shadowed
/// parameterised template is dropped entirely and never drives call-site
/// instantiation (Case B), and the clauses of the shadowed one's layer go
/// with it (a lift carries none) --- except one of [nearer], which a
/// `self.glp` at or below the exposing directory declares, and which
/// shadows the lifted one instead.  The lift's types and templates fill gaps:
/// [base] holds them already, layered where they belong, where its chain
/// runs through the exposing directory (liftExposedTypes).
///
/// An exposed declaration's types are those of the scope it was declared in,
/// the exposed module's own ([ExposedLift]): a type [base] also defines
/// survives from [exposed] under `<origin>:T`, the exposed declarations
/// rewritten to it (TypeEnvironment.shadowedBy) --- as the linked program
/// resolves an exposed declaration's types in the exposed module's scope
/// ([renameDeclTypes] over the declaring file's [typeOwnersByModule]).
TypeEnvironment _mergeExposed(TypeEnvironment base, TypeEnvironment exposed,
    {String? label, Set<String> nearer = const {}}) {
  final ex = exposed.shadowedBy(base, ownLabel: label);
  final procedures = <String, ProcDecl>{...base.procedures};
  final paramProcDecls = <String, ProcDecl>{...base.paramProcDecls};
  final scopeLayers = <String, ScopeLayer>{...base.scopeLayers};
  for (final key in {...ex.procedures.keys, ...ex.paramProcDecls.keys}) {
    if (nearer.contains(key)) continue;
    procedures.remove(key);
    paramProcDecls.remove(key);
    scopeLayers.remove(key);
    final mono = ex.procedures[key];
    if (mono != null) procedures[key] = mono;
    final param = ex.paramProcDecls[key];
    if (param != null) paramProcDecls[key] = param;
  }
  final types = <String, TypeDef>{...base.types};
  for (final e in ex.types.entries) {
    types.putIfAbsent(e.key, () => e.value);
  }
  return TypeEnvironment(types, procedures,
      paramProcDecls: paramProcDecls,
      typeTemplates: {...ex.typeTemplates, ...base.typeTemplates},
      typeOrigins: {...ex.originsUnder(label), ...base.typeOrigins},
      scopeLayers: scopeLayers);
}

/// Collect `self.glp` files in ancestor directories ABOVE [rootDir], walking up
/// to but NOT including [programsDir], a virtual one ([isVirtualSelfGlp])
/// among them. Returns absolute paths, innermost-first.
List<String> _ancestorSelfGlpFiles(String rootDir, String programsDir) {
  // Normalize for comparison: absolute + resolve `..`/`.` (callers may pass
  // paths containing `..`) + strip trailing slash.
  String norm(String p) {
    var n = ppath.normalize(Directory(p).absolute.path);
    if (n.endsWith('/')) n = n.substring(0, n.length - 1);
    return n;
  }

  final programsNorm = norm(programsDir);
  final result = <String>[];
  var dir = Directory(rootDir).parent.absolute.path;

  while (true) {
    final dn = norm(dir);
    if (dn == programsNorm) break; // exclude programs/self.glp
    if (!dn.startsWith(programsNorm)) break; // above programs/ — stop
    final selfGlp = File('$dir${Platform.pathSeparator}self.glp');
    if (hasSelfGlp(dir)) result.add(selfGlp.absolute.path);
    final parent = Directory(dir).parent.path;
    if (parent == dir) break; // filesystem root safety
    dir = parent;
  }

  return result;
}

/// Step 2 of compilation (TGLP modules.tex, Compilation): "each module is
/// checked against its scope---for SRSW, its guards and its types---exactly
/// as a single-module program is, whether or not an entry point reaches it".
/// It runs after discovery (step 1) and before
/// renaming (step 3), so every error names the module's own file and line, and
/// it covers every discovered module — including one that no entry point
/// reaches, which step 5 (dead-code elimination) drops before the linked check
/// ever sees it. The linked check that follows is an addition to this one, not
/// a replacement: it is more stringent where a call supplies concrete types
/// across a `#` boundary, and blind where a module is unreachable.
///
/// Two points the per-module check settles, both as the paper puts them:
///
/// - A parameterised procedure with no instantiation in its own module is not
///   rejected here — [checkModule] is called with
///   `rejectUninstantiatedInspecting: false`, since a call in another module of
///   the program may instantiate it. A procedure that never inspects a
///   parameter is certified once for all instantiations by the abstract-
///   parameter route (parameterized-types.tex §Modular Checking via Abstract
///   Parameters), which [checkModule] runs regardless; one that does inspect a
///   parameter has no well-typing of its own and acquires one only per
///   instantiation, which the linked check supplies — or, where no call in the
///   program supplies one, the linked check rejects the program.
/// - Defined guards are unfolded per module before checking, as on the
///   single-file path (`GlpEngine.loadSource`): guard unfolding precedes type
///   checking, so input coverage is checked on the unfolded head.
///
/// Every module is checked, one with no procedure declarations included: a
/// clause of it then defines a procedure with no declaration, which is an error
/// (TGLP Definition "Typed GLP Program", condition 1), and a `self.glp` that
/// carries only type definitions checks trivially.  Until 2026-10-02 a module
/// with no declarations was skipped here and on the single-file path, so its
/// clauses were compiled and run with no check.
///
/// SRSW and the guards are checked per module after the types, by the pass
/// every compiled program runs ([Analyzer.checkSRSWAndGuards]), in the
/// module's own scope ([buildModuleTypeEnvironment]), where the relaxations
/// of a typed program are decided: a single-module program is a GLP program,
/// so each clause of it satisfies SRSW (GLP-Spec glp.tex, Definition "GLP
/// Program, Goals") and calls only guard predicates in its guards
/// (Definition "Guarded Clause"), and a module no entry point reaches is
/// refused for either as for a type (GLP, 2026-10-13 08:02 UTC, item 2).
/// Until 2026-10-13 they ran on the linked program alone, after step 5 had
/// dropped every module no entry point reaches, so such a module was checked
/// for its types and for nothing else.  A procedure is named in a violation
/// as step 3 names it in the linked program ([singleModulePath]'s own
/// procedures bare, every other `M:p`), so a violation reads as it did there.
///
/// Throws on type errors, naming each offending module's file path; then, on
/// the first module that violates SRSW or calls a non-guard in a guard, the
/// pass's [CompileError].
void checkModulesIndependently(List<DiscoveredModule> modules,
    {String? singleModulePath}) {
  final failures = <String>[];

  for (final mod in modules) {
    final pe = PartialEvaluator();
    final transformed = pe.transformDefinedGuards(
        Program(mod.ast.procedures, mod.ast.line, mod.ast.column),
        scope: mod.ancestorScope);

    final TypeCheckResult result;
    try {
      result = checkModule(
        mod.ast,
        transformedProcedures: transformed.procedures,
        ancestorScope: mod.ancestorScope,
        rejectUninstantiatedInspecting: false,
      );
    } on UndefinedDeclarationTypeError catch (e) {
      // An undefined type name in one of the module's declarations
      // (Moded-Types, "Declaration parameters"), named with its file.
      failures.add('  ${mod.filePath}:${e.line}: ${e.message}');
      continue;
    }
    if (result.isWellTyped) continue;

    for (final e in result.errors) {
      failures.add('  ${mod.filePath}:${e.line}: ${e.message}');
    }
  }

  if (failures.isNotEmpty) {
    throw Exception('Type checking failed for module(s) of the program:\n'
        '${failures.join('\n')}');
  }

  final singleNorm =
      singleModulePath != null ? _normPath(singleModulePath) : null;
  for (final mod in modules) {
    final keepBare = singleNorm != null && _normPath(mod.filePath) == singleNorm;
    Analyzer().checkSRSWAndGuards(
      Program(mod.ast.procedures, mod.ast.line, mod.ast.column),
      typeEnv: buildModuleTypeEnvironment(mod.ast,
          ancestorScope: mod.ancestorScope),
      procedurePrefix: keepBare ? '' : '${mod.moduleName}:',
    );
  }
}

/// Type-check a program on its LINKED program (paper: modules §Module-System
/// Design "Self-contained type checking", §Static Linking; def:program —
/// soundness is established on the linked program). Linking renames every
/// procedure to `M:p` and resolves every call (a cross-module `M' # p` becomes a
/// local `M':p`); the whole program is then one program in which a cross-module
/// call is an ordinary local call. The instantiation closure (§Parameterised
/// Procedure Declarations) therefore induces and checks a parameterised callee's
/// clauses at every instantiation a call supplies — in both directions and
/// through parametric intermediaries — which a per-module check, stopping at the
/// `#` boundary, does not. Renaming makes procedure names unambiguous across
/// modules and type identity is structural, so no merged-environment juggling is
/// needed. A parameterised procedure that inspects a parameter and that no call
/// in the program instantiates is not parametrically well-typed and has no
/// well-typing, and the program is rejected (parameterized-types.tex
/// sec:abstract-parameters); one that inspects none keeps its certificate from
/// the abstract instance and is not.
///
/// This is the SECOND of the two checks the paper specifies. Step 2 — each
/// module against its ancestor scope — runs first, in
/// [checkModulesIndependently]; a module no entry point reaches is checked
/// there and nowhere else, since step-5 dead-code elimination drops it before
/// the linked check.
///
/// A single-module program ([singleModulePath], the single-file path of
/// `GlpEngine.loadSource`) is checked by the same two steps: the flat program
/// is the object checked and then compiled, whichever path reaches it (TGLP
/// modules.tex, Compilation: "The flat program is the linked program of
/// def:program, and it is the object checked").  Until 2026-10-03 the
/// single-file path checked its module alone and compiled the linked program
/// unchecked (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:01. 4": "faults, fix
/// them").
///
/// Throws on type errors with details.
///
/// A file-less source --- a boot source, a source handed over as text --- is
/// a module at the root, linked with the root self.glp as a one-module
/// program (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11"): it is checked in
/// [outerScope], the scope the engine holds when it is handed over (IGLP
/// Implementation Notes, "The scope a boot source is checked in", 8aafd09:
/// "the linked program's entry points and the boot file's ancestor chain of
/// self.glp declarations, the root among them"), and reaches a loaded program
/// only through its entry
/// points, [outerEntryPoints], which stand between the root and the source in
/// its scope: a call to one stays bare, the loaded program's alias, and the
/// linked program is checked over [outerScope], which declares it.
LinkResult checkedLinkedProgram(List<DiscoveredModule> modules,
    {required String rootDir,
    String? singleModulePath,
    TypeEnvironment? outerScope,
    Set<String> outerEntryPoints = const {}}) {
  // A directory with no self.glp is not a program, and is rejected before
  // any of its modules is checked; a single-module program has none.
  if (singleModulePath == null) _requireProgramSelfGlp(modules, rootDir);

  // Step 2 (modules.tex §Static Linking): after discovery, before renaming,
  // each module is type-checked independently against its ancestor scope. The
  // linked check below is an addition to it, not a replacement.
  checkModulesIndependently(modules, singleModulePath: singleModulePath);

  // Soundness is established on the LINKED program (paper: modules §Module-System
  // Design "Self-contained type checking", §Static Linking; def:program). We link
  // first — renaming every procedure to `M:p` and resolving every call, including
  // each cross-module `M' # p` to a local `M':p` — then type-check the single
  // linked program. In it a cross-module call is an ordinary local call, so the
  // instantiation closure (§Parameterised Procedure Declarations) induces and
  // checks the callee's clauses at every instantiation the call supplies — in both
  // directions and through parametric intermediaries. Renaming makes every
  // procedure name unambiguous across modules, and type identity is structural, so
  // no per-module environment juggling is needed.
  // linkProgram applies all five steps, including step-5 DCE, so the program
  // type-checked and compiled below is restricted to its reachable procedures.
  final linked = linkProgram(modules,
      rootDir: rootDir,
      singleModulePath: singleModulePath,
      outerEntryPoints: outerEntryPoints);
  final flat = linkedFlatModule(modules, linked);

  final base = outerScope ?? primitiveScope();
  final pe = PartialEvaluator();
  final transformed = pe.transformDefinedGuards(linked.program, scope: base);

  // The flat program is the object checked, and every call in it is local, so
  // a parameterised procedure no call in it instantiates is one no call in the
  // program instantiates: where it inspects a parameter it is not parametrically
  // well-typed and the program is rejected (parameterized-types.tex
  // sec:abstract-parameters). A goal posted at run time is not a call in the
  // program. Until 2026-09-18 this passed false and printed a `[TYPE] N
  // parameterized procedure(s) unchecked in this program` line instead, so a
  // program pronounced well-typed carried clauses nothing had checked — which is
  // how typed_actors.glp carried an untagged value at a tagged-union position
  // for months (found 2026-08-03).
  //
  // It is checked against the language primitives alone: the root self.glp
  // is one of its modules, its types and procedures renamed under the empty
  // path with every other module's, and nothing else is in scope --- save,
  // for a file-less source, the scope it is handed over in, which declares
  // the loaded program's entry points it calls.
  final result = checkModule(
    flat,
    transformedProcedures: transformed.procedures,
    ancestorScope: base,
    rejectUninstantiatedInspecting: true,
  );

  if (!result.isWellTyped) {
    final errors = result.errors
        .map((e) => '  ${e.message} at line ${e.line}')
        .join('\n');
    throw Exception('Type checking failed for linked program:\n$errors');
  }

  return linked.withCheckedEnv(linkedProgramEnvironment(flat, base: base));
}

/// The root self.glp linked as a program of its own: what a goal posted at the
/// root is linked with (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11": "a
/// posted goal or a file-less source is a module at the root, linked with the
/// root self.glp as a one-module program; nothing ambient").  The root is
/// checked in step 2 against the language primitives, its procedures and types
/// renamed under the empty path and its calls resolved within it (TGLP
/// modules.tex, Compilation, steps 2--4), and the linked program checked over
/// the primitives alone.  Every procedure is kept: step 5 keeps what the
/// posted goal reaches, and a goal may call any procedure of the root, so the
/// engine links the root once and resolves each goal's calls into it, which is
/// semantically the goal's linked program ("Restricting the program to its
/// reachable procedures is semantically equivalent to the whole", fifth step).
///
/// Throws, naming the root's file and line, where the root does not check.
LinkResult checkedRootProgram(DiscoveredModule root) {
  final modules = [root];
  checkModulesIndependently(modules);
  final linked = linkAndResolveModules(modules,
      rootDir: File(root.filePath).parent.path);
  final flat = linkedFlatModule(modules, linked);
  final transformed = PartialEvaluator().transformDefinedGuards(linked.program);
  final result = checkModule(
    flat,
    transformedProcedures: transformed.procedures,
    ancestorScope: primitiveScope(),
    rejectUninstantiatedInspecting: true,
  );
  if (!result.isWellTyped) {
    final errors = result.errors
        .map((e) => '  ${e.message} at line ${e.line}')
        .join('\n');
    throw Exception(
        'Type checking failed for the root self.glp linked alone '
        '(${root.filePath}):\n$errors');
  }
  return linked.withCheckedEnv(linkedProgramEnvironment(flat));
}

/// The scope the linked program is checked in: the flat module's own
/// environment over the language primitives ([primitiveScope]), built exactly
/// as [checkModule] builds it.
TypeEnvironment linkedProgramEnvironment(Module flat,
        {TypeEnvironment? base}) =>
    buildModuleTypeEnvironment(flat, ancestorScope: base ?? primitiveScope());

/// The single flat Module the linked program is type-checked and compiled as:
/// the linked program's procedures, every module's own type definitions, and the
/// linked declarations.
///
/// The type definitions are the union of every module's own, deduplicated by
/// name AND ARITY (structural identity makes duplicates the same type), the
/// root self.glp's among them, renamed under the empty path; the primitive
/// types are the language's and defined by no module.
///
/// The arity is part of the key because it is part of the type constructor's
/// identity, exactly as `p/n` is a procedure's: `NetMsg` and `NetMsg(C)` are two
/// type constructors, not two spellings of one, and a scope may hold both — the
/// per-module environment does, keeping monomorphic types and parameterised
/// templates in separate maps. Keying this union by bare name dropped whichever
/// arity the directory walk reached second, and every reference to the dropped
/// one then failed to resolve in the linked program while the per-module check
/// passed. That is what made `programs/social/spm/{cva,gsg,secure_gsg}` unloadable:
/// `cva/self.glp`'s `NetMsg(C)` displaced the arity-0 `NetMsg` of
/// `programs/system/mad_predicates.glp`, which the root `self.glp` then
/// `-expose`d into every program, so `mad_predicates.glp:19`'s `NetStream` lost
/// its element type.  Its fixture, `programs/tests/type_name_collision/`, and
/// harness check X9 were removed on 2026-10-04: with this key backed out the
/// check no longer failed, each type being renamed by its module, and a check
/// that cannot fail goes (GLP #3 Cowork, 2026-10-04 09:06 UTC, "00:34").
///
/// The declarations are the linked declarations and nothing else: each module's
/// own, renamed with its procedures, and the entry-point aliases'.  A module
/// that redefines a root-scope operation (send/receive/new_channel/merge) by
/// clauses of its own defines a procedure of its own, `M:p` after the renaming,
/// and declares it as every procedure of a module is declared (TGLP modules.tex,
/// Definition "Typed Procedure, Module": "A typed procedure is a procedure
/// declaration ... immediately followed by a procedure for p/n.  A module is a
/// sequence of type definitions and typed procedures"); the linked program is a
/// typed GLP program, every procedure in it with exactly one declaration (TGLP
/// Definition "Typed GLP Program", condition 1), so a redefinition without one
/// is refused by the linked check, naming `M:p`.  Until 2026-10-03 such a `M:p`
/// borrowed a renamed copy of the root's declaration here.
///
/// With [allDeclarations], the declarations are the whole scope's
/// ([LinkResult.scopeDeclarations]) rather than the reachable subset — the
/// module `find_type/2`'s declared table is built over.
Module linkedFlatModule(List<DiscoveredModule> modules, LinkResult linked,
    {bool allDeclarations = false}) {
  // Step 3 for types (modules.tex §Compilation): every type `T` of module `M`
  // is renamed to `M:T`, on the same argument as procedures — two sibling
  // modules have distinct scopes, so two same-named types defined in them are
  // two types, which one flat namespace would otherwise make one, checking a
  // module against a definition it cannot see. Every reference was resolved to
  // the renamed type of the nearest scope defining it (step 4,
  // [_renamedTypeDefs] / [renameDeclTypes]), so the union below cannot collide
  // and cannot depend on the order the filesystem lists the modules in.
  //
  // Every module is named by its path from the root, and two files of one
  // name are rejected at linking ([_requireDistinctModuleNames]), so a renamed
  // type is defined by one file, and a file is one module of [modules] however
  // many routes reach it ([_resolveExposes]).  Until 2026-10-02 the name was
  // the file's, and two modules of one name defining one type were refused
  // here as a "Module-name collision".
  final owners = typeOwnersByModule(modules);
  final typeDefs = <String, TypeDef>{};
  for (final mod in modules) {
    for (final td in _renamedTypeDefs(mod, owners[mod.filePath]!)) {
      final key = '${td.name}/${td.typeParams.length}';
      typeDefs.putIfAbsent(key, () => td);
    }
  }

  final procDecls = [
    ...(allDeclarations ? linked.scopeDeclarations : linked.checkedDeclarations)
  ];

  // A module only an -expose brings in is checked in its own scope, which
  // runs through the self.glp files of its chain that are not modules of the
  // program ([DiscoveredModule.outerScope]), and what it lifts carries their
  // types to the modules receiving it ([ExposedLift]): the linked program
  // carries, renamed, each of their definitions its declarations and types
  // reference, and each those reference in turn, and no other.
  final outerDefs = <String, TypeDef>{};
  for (final o in _outerModules(modules)) {
    for (final td in _renamedTypeDefs(o, owners[o.filePath]!)) {
      outerDefs.putIfAbsent('${td.name}/${td.typeParams.length}', () => td);
    }
  }
  if (outerDefs.isNotEmpty) {
    final pending = <String>[];
    void refs(TypeExpr t) {
      if (t is TypeRef) {
        pending.add('${t.name}/${t.typeArgs.length}');
        t.typeArgs.forEach(refs);
      } else if (t is StructAlt) {
        t.args.forEach(refs);
      } else if (t is ListConsAlt) {
        refs(t.head);
        refs(t.tail);
      } else if (t is DiffListAlt) {
        refs(t.content);
        refs(t.hole);
      }
    }

    for (final td in typeDefs.values) {
      td.alternatives.forEach(refs);
    }
    for (final d in procDecls) {
      d.argTypes.forEach(refs);
    }
    while (pending.isNotEmpty) {
      final key = pending.removeLast();
      if (typeDefs.containsKey(key)) continue;
      final td = outerDefs[key];
      if (td == null) continue;
      typeDefs[key] = td;
      td.alternatives.forEach(refs);
    }
  }

  return Module(
    typeDefs: typeDefs.values.toList(),
    procDeclarations: procDecls,
    procedures: linked.program.procedures,
    line: 0,
    column: 0,
  );
}

/// The type-identity tables of a linked program (modules.tex §Dynamic
/// Activation): declared `p/n` → identity, which `find_type/2` reads, and
/// exported `p/n` → identity, the table a `Module` value carries.
///
/// Built on demand from the same flat module the program was type-checked as,
/// so the automata are the checked program's. Nothing on the load path calls
/// this: the kernels that consume the tables (`'_find_type'`, `'_run'`/3) are
/// step 2 of `/Grassroots/docs/typed-dynamic-activation-plan.md` and are IGLP's.
///
/// [entryTemplates] are the program's entries declared as instances of a
/// parameterised declaration of an ancestor scope ([entryTemplatesOf]); for
/// each, the exported table has beside it the type identity of D⟨ι(T)⟩ and
/// ι(T) ([TypeIdentityTables.opaque]; TGLP modules.tex, Dynamic Activation,
/// "Opaque types"), which the interface text's derivation records too.  The
/// tables are otherwise as they are without it.
TypeIdentityTables linkedTypeIdentityTables(
    List<DiscoveredModule> modules, LinkResult linked,
    {Map<String, EntryTemplate> entryTemplates = const {}}) {
  final tables = typeIdentityTablesForModule(
      linkedFlatModule(modules, linked, allDeclarations: true),
      ancestorScope: primitiveScope());
  if (entryTemplates.isEmpty) return tables;
  final opaque = <String, OpaqueEntry>{};
  for (final e in entryTemplates.entries) {
    if (!tables.exported.containsKey(e.key)) continue;
    final record = opaqueEntryOf(
        entry: e.value.entry,
        entryEnv: e.value.entryEnv,
        template: e.value.template,
        templateEnv: e.value.templateEnv);
    if (record != null) opaque[e.key] = record;
  }
  return tables.withOpaque(opaque);
}

/// An entry of a program declared, in the module whose exports are the
/// program's entry points, as the instance at a type of a parameterised
/// declaration of that module's ancestor scope of its key: the [entry]'s
/// declaration in its module's scope [entryEnv], and the [template] D in the
/// ancestor scope [templateEnv] that declares it (TGLP modules.tex, Dynamic
/// Activation, "Opaque types": "A guest declares its entry as the template's
/// instance at its own type").  Whether the entry is D's instance at a type,
/// and at which, is [opaqueEntryOf]'s.
class EntryTemplate {
  final ProcDecl entry;
  final TypeEnvironment entryEnv;
  final ProcDecl template;
  final TypeEnvironment templateEnv;

  /// The file of the entry's module, and of the ancestor `self.glp` that
  /// declares the template, the nearest that does; null where no module of
  /// the program declares it (a template an `-expose` lifts).  Each is the
  /// scope its declaration's types are resolved in ([qualifiedEntryInterfaces]).
  final String? entryFile;
  final String? templateFile;

  /// The entry's declaration as its module writes it, its types unexpanded.
  final ProcDecl? entrySource;
  const EntryTemplate(
      {required this.entry,
      required this.entryEnv,
      required this.template,
      required this.templateEnv,
      this.entryFile,
      this.templateFile,
      this.entrySource});
}

/// The program's entries declared as the instance at a type of a
/// parameterised declaration of their module's ancestor scope of their key,
/// by key ([opaqueEntryOf]).  An entry that shares its key with such a
/// declaration and is its instance at no type is not one of them: it shadows
/// the declaration, as before.  The entry module is
/// the one whose exports are the program's entry points (modules.tex,
/// Compilation, fifth step): a directory program's `self.glp` at [rootDir],
/// whose exported declarations are its entries, or the single module at
/// [singleModulePath], every declaration of which is one.  A parameterised
/// entry is a template of its own and is not one of them.
Map<String, EntryTemplate> entryTemplatesOf(List<DiscoveredModule> modules,
    {required String rootDir, String? singleModulePath}) {
  final singleNorm = singleModulePath == null ? null : _normPath(singleModulePath);
  final rootNorm = _normPath(rootDir);
  final out = <String, EntryTemplate>{};
  for (final m in modules) {
    final isEntryModule = singleNorm != null
        ? _normPath(m.filePath) == singleNorm
        : m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm;
    if (!isEntryModule) continue;
    TypeEnvironment? env;
    for (final d in m.ast.procDeclarations) {
      if (d.imported || d.isParameterized) continue;
      if (singleNorm == null && !d.exported) continue;
      final template = m.ancestorScope.paramProcDecls[d.key];
      if (template == null) continue;
      env ??= buildModuleTypeEnvironment(m.ast, ancestorScope: m.ancestorScope);
      final entry = env.procedures[d.key];
      if (entry == null) continue;
      if (opaqueEntryOf(
              entry: entry,
              entryEnv: env,
              template: template,
              templateEnv: m.ancestorScope) ==
          null) {
        continue;
      }
      String? templateFile;
      for (final s in _ancestorSelfGlps(m, modules)) {
        if (s.ast.procDeclarations.any(
                (t) => !t.imported && t.isParameterized && t.key == d.key) ||
            s.ast.paramProcDecls.any((t) => !t.imported && t.key == d.key)) {
          templateFile = s.filePath;
          break;
        }
      }
      out[d.key] = EntryTemplate(
          entry: entry,
          entryEnv: env,
          template: template,
          templateEnv: m.ancestorScope,
          entryFile: m.filePath,
          templateFile: templateFile,
          entrySource: d);
    }
  }
  return out;
}

/// An opaque guest's entry and its template as its artefact's interface text
/// carries them ([qualifiedEntryInterfaces]): every type name qualified by
/// the scope that defines it.
class QualifiedEntryInterface {
  final ProcDecl entry;
  final ProcDecl template;

  /// The definitions of the qualified types, by name: those the two
  /// declarations reach, and every type those reach.
  final Map<String, TypeDef> typeDefs;
  const QualifiedEntryInterface(this.entry, this.template, this.typeDefs);
}

/// The entry and the template of each of [entryTemplates] with every type
/// name qualified by the scope that defines it, so that in the interface text
/// an opaque guest's record carries, the template's types and the guest's own
/// cannot be one: a guest's own type of the name of a type the template
/// reaches is another name there (GLP #6, 2026-10-13 11:57 UTC: "it is the
/// implementation's to keep the template's types and the guest's apart in the
/// interface text (qualify the template's by their scope), compliance with
/// TGLP's identity, which is of automata, not names").
///
/// The qualification is the linker's own (TGLP modules.tex, Compilation,
/// third and fourth steps): each declaration's types are resolved in its own
/// module's scope, the entry's in the entry module's and the template's in
/// the `self.glp` that declares it, to `M:T`, M the path from the root of the
/// module defining T, the root's the empty path ([renameDeclTypes],
/// [_renamedTypeDefs]).  The text is GLP source, which has no `:` in a type
/// name, so `M:T` is spelt `T__M`, every character of M that is not a letter
/// or a digit a `_` --- `:SgChannel` is `SgChannel__`, `tests/opaque:Move`
/// `Move__tests_opaque` --- and where that spelling is a name the program's
/// modules define, or another qualified type's, `_` is appended until it is
/// neither, the qualified names taken in sorted order.  The identities, of
/// automata, do not turn on it (TGLP Implementation Notes, "The identity of
/// an opaque instance").
///
/// An entry whose template no module of the program declares, or whose
/// renamed declaration the link does not carry, is left out, and its text is
/// as before.
Map<String, QualifiedEntryInterface> qualifiedEntryInterfaces(
    List<DiscoveredModule> modules,
    LinkResult linked,
    Map<String, EntryTemplate> entryTemplates) {
  if (entryTemplates.isEmpty) return const {};
  final owners = typeOwnersByModule(modules);
  final renamed = <String, TypeDef>{};
  for (final mod in [...modules, ..._outerModules(modules)]) {
    final o = owners[mod.filePath];
    if (o == null) continue;
    for (final td in _renamedTypeDefs(mod, o)) {
      renamed.putIfAbsent(td.name, () => td);
    }
  }
  final bareNames = <String>{
    ...TypeRef.builtins,
    for (final mod in modules)
      for (final td in mod.ast.typeDefs) td.name,
  };

  // The two renamed declarations of each entry.
  final pairs = <String, (ProcDecl, ProcDecl)>{};
  for (final e in entryTemplates.entries) {
    final t = e.value;
    final entryOwners = t.entryFile == null ? null : owners[t.entryFile];
    final templateOwners =
        t.templateFile == null ? null : owners[t.templateFile];
    final source = t.entrySource;
    if (entryOwners == null || templateOwners == null || source == null) {
      continue;
    }
    final entry = ProcDecl(source.name, renameDeclTypes(source, entryOwners),
        source.line, source.column,
        typeParams: source.typeParams, exported: true);
    final template = ProcDecl(t.template.name,
        renameDeclTypes(t.template, templateOwners), t.template.line,
        t.template.column,
        typeParams: t.template.typeParams);
    pairs[e.key] = (entry, template);
  }
  if (pairs.isEmpty) return const {};

  // The qualified names the declarations reach, through the renamed
  // definitions, and their spellings.
  final reached = <String>{};
  void reach(TypeExpr t) {
    if (t is TypeRef) {
      if (t.name.contains(':') && reached.add(t.name)) {
        final td = renamed[t.name];
        if (td != null) td.alternatives.forEach(reach);
      }
      t.typeArgs.forEach(reach);
    } else if (t is StructAlt) {
      t.args.forEach(reach);
    } else if (t is ListConsAlt) {
      reach(t.head);
      reach(t.tail);
    } else if (t is DiffListAlt) {
      reach(t.content);
      reach(t.hole);
    }
  }

  for (final (entry, template) in pairs.values) {
    entry.argTypes.forEach(reach);
    template.argTypes.forEach(reach);
  }
  final spelt = <String, String>{};
  final used = <String>{...bareNames};
  for (final q in reached.toList()..sort()) {
    final colon = q.lastIndexOf(':');
    final scope = q.substring(0, colon).replaceAll(RegExp(r'[^A-Za-z0-9]'), '_');
    var name = '${q.substring(colon + 1)}__$scope';
    while (used.contains(name)) {
      name = '${name}_';
    }
    used.add(name);
    spelt[q] = name;
  }
  TypeExpr spell(TypeExpr t) {
    if (t is TypeRef) {
      return TypeRef(spelt[t.name] ?? t.name, t.line, t.column,
          isInput: t.isInput, typeArgs: [for (final a in t.typeArgs) spell(a)]);
    }
    if (t is StructAlt) {
      return StructAlt(
          t.functor, [for (final a in t.args) spell(a)], t.line, t.column);
    }
    if (t is ListConsAlt) {
      return ListConsAlt(spell(t.head), spell(t.tail), t.line, t.column);
    }
    if (t is DiffListAlt) {
      return DiffListAlt(spell(t.content), spell(t.hole), t.line, t.column);
    }
    return t;
  }

  ProcDecl spellDecl(ProcDecl d) => ProcDecl(
      d.name, [for (final a in d.argTypes) spell(a)], d.line, d.column,
      typeParams: d.typeParams, exported: d.exported);
  final defs = <String, TypeDef>{
    for (final q in reached)
      if (renamed[q] != null)
        spelt[q]!: TypeDef(
            spelt[q]!,
            [for (final a in renamed[q]!.alternatives) spell(a)],
            renamed[q]!.line,
            renamed[q]!.column,
            typeParams: renamed[q]!.typeParams),
  };
  return {
    for (final e in pairs.entries)
      e.key: QualifiedEntryInterface(
          spellDecl(e.value.$1), spellDecl(e.value.$2), defs),
  };
}

/// Whole-program type-check gate (paper: modules §Static Linking — "the unit of
/// compilation and execution is a program ... only a well-typed program is
/// compiled and run"). Throws unless the linked program is well-typed. For
/// callers that need only the verdict (e.g. a single-file gate that compiles the
/// source unrenamed); callers that compile the linked program use
/// [checkedLinkedProgram].
void typeCheckProgram(List<DiscoveredModule> modules, {required String rootDir}) {
  checkedLinkedProgram(modules, rootDir: rootDir);
}

/// Static linking of all modules into a single flat Program (modules.tex
/// sec:static-linking, all five steps).
///
/// Steps 1–4 ([linkAndResolveModules]) rename procedures (`p/n` → `M:p/n`),
/// resolve all calls, and generate entry-point aliases for the exports of the
/// program's self.glp;
/// step 5 ([eliminateDeadCode]) restricts the result to the reachable
/// procedures. This is the program of def:program that is type-checked and
/// compiled.
///
/// A directory with no self.glp is rejected before linking
/// ([_requireProgramSelfGlp]), and between steps 4 and 5 a directory with no
/// entry points is rejected ([_requireEntryPoints]).
LinkResult linkProgram(List<DiscoveredModule> modules,
    {required String rootDir,
    String? singleModulePath,
    Set<String> outerEntryPoints = const {}}) {
  if (singleModulePath == null) _requireProgramSelfGlp(modules, rootDir);
  final linked = linkAndResolveModules(modules,
      rootDir: rootDir,
      singleModulePath: singleModulePath,
      outerEntryPoints: outerEntryPoints);
  if (singleModulePath == null) {
    _requireEntryPoints(modules, linked, rootDir);
  }
  return eliminateDeadCode(linked);
}

/// A directory with no `self.glp` is not a program (modules.tex, "Entry and
/// the absence of a boot module": "A directory with no self.glp at all is
/// rejected for the prior reason: a program is a directory carrying a self.glp
/// or a self-contained module (Section~\ref{sec:mod-design}), and such a
/// directory is neither.  A directory of modules that is not a program---a
/// library reached by ancestor scoping, or a collection of examples compiled
/// one at a time---is used as those are used, and is not compiled as a program
/// at all").  Until 2026-10-02 such a directory took the exported procedures
/// of its root-level modules for its entry points.
void _requireProgramSelfGlp(List<DiscoveredModule> modules, String rootDir) {
  final rootNorm = _normPath(rootDir);
  final hasSelf = modules.any((m) =>
      m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm);
  if (hasSelf) return;
  throw Exception(
      'Not a program: $rootDir has no self.glp. A program is a directory '
      'carrying a self.glp or a self-contained module (modules.tex, '
      'Module-System Design), so a directory with no self.glp at all is '
      'rejected (modules.tex, "Entry and the absence of a boot module"); a '
      'directory of modules that is not a program --- a library, or a '
      'collection of examples --- is used as those are used, its modules '
      'loaded one at a time, and is not compiled as a program at all.');
}

/// A directory with no entry points is not a program (modules.tex §Static
/// Linking, "Entry and the absence of a boot module"): "A root `self.glp` that
/// exports no procedure therefore gives a program with no entry points, which
/// the fifth step restricts to the empty set of procedures. No initial goal
/// resolves against it, so it is not a program in the sense of def:program, and
/// the loader rejects it rather than linking it and reporting success."
///
/// The entry points are the bare (unprefixed) procedures step 4 generated: a
/// directory's are the aliases of its root `self.glp`'s exports, by definition
/// or by forwarding (§External access). Every other procedure carries a renamed
/// `M:p`, so an empty bare set is an empty entry-point set, which is what step 5
/// would restrict the program to. Rejecting here rather than after step 5 is
/// what the paper asks for: the loader rejects it rather than linking it.
///
/// Two things this is NOT. It is not the reachability check — a program with one
/// entry point that reaches nothing else is a program, and step 5 keeps it. And
/// it does not apply to a single module: a single-module program has no
/// `self.glp`, "exports all its procedures, so every one is an entry point"
/// (§Static Linking), and the linker keeps them bare rather than aliasing them,
/// so [linkProgram] tests it only on the directory path.
///
/// An `-expose`d procedure is not an entry point either (§`-expose`: it "is not
/// thereby exported by the root `self.glp`, so it is an entry point only if the
/// root `self.glp` exports it in its own right"), so a root `self.glp` whose
/// only exports are exposed ones is rejected here as well.
void _requireEntryPoints(
    List<DiscoveredModule> modules, LinkResult linked, String rootDir) {
  final hasEntryPoint =
      linked.program.procedures.any((p) => !p.name.contains(':'));
  if (hasEntryPoint) return;

  final rootNorm = _normPath(rootDir);
  final rootSelfPath = modules
      .firstWhere((m) =>
          m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm)
      .filePath;
  final cause = '$rootSelfPath exports no procedure';

  throw Exception(
      'Not a program: $rootDir has no entry points — $cause. A procedure is an '
      'entry point exactly when the root self.glp exports it, by declaring it '
      'exported and either defining it or forwarding it to the module that '
      'does (modules.tex §External access); an exposed procedure is not one. '
      'With no entry point no initial goal resolves against the directory, so '
      'it is not a program by def:program and is rejected rather than linked.');
}

/// Steps 1–4 of static linking: the pure rename-and-resolve transform, without
/// dead-code elimination.
///
/// Renames procedures (`p/n` → `M:p/n`), resolves all calls, and generates
/// entry-point aliases for the exported procedures of the `self.glp` of
/// [rootDir], the loaded program root; a directory with no `self.glp` gets
/// none, and [linkProgram] rejects it.
///
/// Returns a [LinkResult] with the renamed program and renamed proc declarations
/// (needed for SRSW type-based relaxation during compilation). This is the stage
/// to inspect when checking renaming/resolution/aliasing in isolation; the
/// program actually compiled is [linkProgram] (which also applies step 5).
///
/// [outerEntryPoints] are the entry points of a program the engine already
/// holds, by "name/arity", which a file-less source reaches between the root
/// and itself ([checkedLinkedProgram]): a call to one is left bare, where it
/// would otherwise resolve to a root procedure of the same name.
LinkResult linkAndResolveModules(List<DiscoveredModule> modules,
    {required String rootDir,
    String? singleModulePath,
    Set<String> outerEntryPoints = const {}}) {
  // Every module is named by its path from the root, so two files of one name
  // are two modules the renaming cannot tell apart.
  _requireDistinctModuleNames(modules);

  // Step 4 for types: the scope each module's type references resolve in.
  final typeOwners = typeOwnersByModule(modules);

  // The procedure registry: module name (its path from the root) → the
  // signatures of the procedures it defines.
  final registry = <String, Set<String>>{};
  for (final mod in modules) {
    final sigs = <String>{};
    for (final proc in mod.ast.procedures) {
      sigs.add('${proc.name}/${proc.arity}');
    }
    registry[mod.moduleName] = sigs;
  }

  // Build ancestor self.glp procedure map for each module.
  // Maps module name → { sig → ancestorModuleName } (inner-most ancestor wins).
  final selfGlpModules = modules.where((m) => m.isSelfGlp).toList();
  final ancestorSelfProcs = <String, Map<String, String>>{};

  for (final mod in modules) {
    final procs = <String, String>{}; // sig → ancestorModuleName

    // The scope of the module walked from the root down, a nearer definition
    // over a farther one (TGLP modules.tex, Definition "Root, Scope"): at each
    // directory of its chain, the EXPORTED procedures of the modules that
    // directory's `self.glp` `-expose`s, "as if defined in its self.glp ...
    // Shadowing applies as usual" ("The -expose directive"), and over them the
    // `self.glp`'s own procedures.  Local definitions are checked first in
    // `_resolveGoal`.  Until 2026-10-07 an exposed procedure filled a gap
    // only, every ancestor `self.glp`'s procedure of its name winning
    // (programs/tests/expose/lift_shadows).
    final ancestors =
        _ancestorSelfGlps(mod, selfGlpModules, flaggedOnly: true);
    final modDirNorm = _normPath(File(mod.filePath).parent.path);
    final reaching = [
      for (final em in modules)
        if (em.exposingDir != null &&
            !identical(em, mod) &&
            _dirUnder(modDirNorm, em.exposingDir!))
          em
    ];
    final levels = <String>{
      for (final s in ancestors) _normPath(File(s.filePath).parent.path),
      for (final em in reaching) em.exposingDir!,
    }.toList()
      ..sort((a, b) => _depth(a).compareTo(_depth(b)));

    for (final dir in levels) {
      for (final em in reaching) {
        if (em.exposingDir != dir) continue;
        for (final d in em.ast.procDeclarations) {
          if (!d.exported) continue;
          procs['${d.name}/${d.arity}'] = em.moduleName;
        }
      }
      for (final selfMod in ancestors) {
        if (_normPath(File(selfMod.filePath).parent.path) != dir) continue;
        for (final proc in selfMod.ast.procedures) {
          final sig = '${proc.name}/${proc.arity}';
          // An entry point of a program the engine holds is nearer than the
          // root for a file-less source, and its call stays bare.
          if (selfMod.isRoot && outerEntryPoints.contains(sig)) continue;
          procs[sig] = selfMod.moduleName;
        }
      }
    }

    ancestorSelfProcs[mod.moduleName] = procs;
  }

  final allProcedures = <Procedure>[];

  // The loaded module of a single-module program keeps its procedures under
  // their bare names: they are the program's entry points, "called by plain
  // name by a goal posted at the root" (modules.tex §Static Linking), so the
  // bare name is a plain-name handle on the real head — no forwarder clause,
  // whose flat arguments cannot carry a structured-mode term's nested holes.
  // Every SCOPE module (ancestor self.glp, own-dir self.glp, exposed module) is
  // still renamed to M:p, so its internal calls resolve to its OWN procedures
  // and the bare loaded module never hijacks an ancestor's same-named call.
  final singleNorm =
      singleModulePath != null ? _normPath(singleModulePath) : null;

  // Step 4's cross-module calls, resolved from the caller's directory, and
  // the calls that resolve to nothing, every one of them reported together.
  final resolver = _CrossModuleResolver(modules);
  final byName = {for (final m in modules) m.moduleName: m};

  // Process each module
  for (final mod in modules) {
    final localSigs = registry[mod.moduleName]!;
    final modAncestorProcs = ancestorSelfProcs[mod.moduleName] ?? {};
    final keepBare =
        singleNorm != null && _normPath(mod.filePath) == singleNorm;
    String remote(RemoteGoal g) => resolver.resolve(mod, g);

    for (final proc in mod.ast.procedures) {
      // Step 3 (modules.tex §Static Linking): rename every procedure p/n to
      // M:p/n, eliminating name collisions — except the loaded module's own
      // procedures, kept bare as the program's plain-name entry points.
      final renamedName = keepBare ? proc.name : '${mod.moduleName}:${proc.name}';
      final renamedClauses = <Clause>[];

      for (final clause in proc.clauses) {
        resolver.checkVolitionalCalls(mod, clause);
        _checkScopeVolitionalCalls(mod, clause, localSigs, modAncestorProcs,
            byName);
        final renamedHead = keepBare
            ? clause.head
            : Atom('${mod.moduleName}:${clause.head.functor}',
                clause.head.args, clause.head.line, clause.head.column);

        // Step 4: resolve every body call in this module's scope — local → p
        // becomes M:p (bare in the loaded module), ancestor self.glp →
        // ancestor:p, static cross-module M' # p → M':p (a local Spawn, not a
        // Distribute; manual §19.7).
        final resolvedBody = clause.body
            ?.map((g) => _resolveGoal(
                g, mod.moduleName, localSigs, modAncestorProcs, remote,
                keepLocalBare: keepBare))
            .toList();

        // A defined guard calls a user unit-clause procedure, which step 3
        // renames to M:g. Resolve the guard call in the same scope so it points
        // at the renamed unit clause; the partial evaluator unfolds it by that
        // name. Builtin guards and root-scope guards match no module procedure
        // and stay bare (root unit clauses are collected unrenamed).
        final resolvedGuards = clause.guards
            ?.map((g) => _resolveGuard(
                g, mod.moduleName, localSigs, modAncestorProcs,
                keepLocalBare: keepBare))
            .toList();

        renamedClauses.add(Clause(
          renamedHead,
          guards: resolvedGuards,
          body: resolvedBody,
          line: clause.line,
          column: clause.column,
        ));
      }

      allProcedures.add(Procedure(
        renamedName,
        proc.arity,
        renamedClauses,
        proc.line,
        proc.column,
      ));
    }
  }

  // A cross-module call whose qualifier names no module of the caller's
  // directory, or a procedure its module does not export, does not resolve,
  // and the program is rejected (modules.tex, Compilation, fourth step).
  resolver.throwIfUnresolved();

  // Build a program-wide procedure declaration index for mode-aware aliases.
  // Maps 'name/arity' → ProcDecl, collecting from all modules' non-imported decls.
  final declIndex = <String, ProcDecl>{};
  // The file each indexed declaration came from, so its types resolve in ITS
  // scope rather than the aliasing module's.
  final declIndexFile = <String, String>{};
  for (final mod in modules) {
    for (final d in mod.ast.procDeclarations) {
      if (d.imported) continue;
      final sig = '${d.name}/${d.arity}';
      // First declaration wins (could also prefer exported, but any is fine)
      if (declIndex.containsKey(sig)) continue;
      declIndex[sig] = d;
      declIndexFile[sig] = mod.filePath;
    }
  }

  // Generate entry-point aliases (modules.tex sec:static-linking step 5,
  // §External access). For a DIRECTORY program, the entry points are the
  // EXPORTED procedures of the ROOT self.glp — the self.glp at the loaded
  // program root — each given an unqualified forwarding alias so an external
  // goal calls it by plain name.  A directory with no root self.glp is not a
  // program and has none ([_requireProgramSelfGlp]).
  //
  // A SINGLE-MODULE program generates NO aliases: its own procedures are kept
  // bare above (keepBare), and those bare names ARE the entry points, a
  // plain-name handle directly on each real head. A forwarding alias is avoided
  // on purpose — its flat arguments cannot carry a structured-mode term's nested
  // reader/writer holes.
  final aliasDecls = <ProcDecl>[];
  final aliasCheckedDecls = <ProcDecl>[];
  if (singleNorm == null) {
    final rootNorm = _normPath(rootDir);
    final rootSelfMods = modules
        .where((m) =>
            m.isSelfGlp && _normPath(File(m.filePath).parent.path) == rootNorm)
        .toList();

    final aliasedSigs = <String, String>{}; // sig → owning module (conflict check)
    for (final mod in rootSelfMods) {
      for (final proc in mod.ast.procedures) {
        final isExported = mod.ast.procDeclarations.any(
            (d) => d.exported && d.name == proc.name && d.arity == proc.arity);
        if (!isExported) continue;

        final sig = '${proc.name}/${proc.arity}';
        final owner = aliasedSigs[sig];
        if (owner != null && owner != mod.moduleName) {
          throw Exception(
              'Entry-point conflict: procedure $sig is exported by both '
              '"$owner" and "${mod.moduleName}".');
        }
        if (owner != null) continue;
        aliasedSigs[sig] = mod.moduleName;

        // Look up ProcDecl for mode-aware alias generation.
        // First check the owning module, then the program-wide index.
        final own = _findProcDecl(mod, proc.name, proc.arity);
        final decl = own ?? declIndex[sig];
        final declFile = own != null ? mod.filePath : declIndexFile[sig];

        // The alias carries the exporting declaration under its bare name
        // (collected into the returned declarations below), so the linked-
        // program check uses the exporting module's declaration — shadowing a
        // root-scope declaration of the same name/arity (e.g. the root
        // self.glp's run/2), exactly as the module's own declaration shadows
        // it in the per-module check.
        if (decl != null) {
          aliasDecls.add(ProcDecl(proc.name, decl.argTypes, decl.line,
              decl.column,
              typeParams: decl.typeParams,
              exported: decl.exported,
              isBuiltin: decl.isBuiltin));
          aliasCheckedDecls.add(ProcDecl(
              proc.name,
              renameDeclTypes(decl, typeOwners[declFile ?? mod.filePath]!),
              decl.line,
              decl.column,
              typeParams: decl.typeParams,
              exported: decl.exported,
              isBuiltin: decl.isBuiltin));
        }

        final aliasClause = _makeAliasClause(
          proc.name,
          proc.arity,
          '${mod.moduleName}:${proc.name}',
          declaration: decl,
        );
        allProcedures.add(Procedure(proc.name, proc.arity, [aliasClause], 0, 0));
      }
    }
  }

  // Collect and rename proc declarations for SRSW relaxation. The loaded
  // module's declarations stay bare, matching its bare procedures.
  //
  // A kept-bare declaration is an entry point of the program and carries the
  // exported flag, whatever the source wrote: "a single-module program, having
  // no self.glp, exports all its procedures, so every one is an entry point"
  // (modules.tex sec:static-linking). A renamed `M:p` declaration is not an
  // entry point — an external goal cannot name it — so it carries the flag not
  // at all, and a directory program's entry points come from `aliasDecls`
  // below. This is what the exported type-identity table is built over
  // (analysis/type_checker/type_identity.dart), and it is the same set the
  // artefact's interface table records.
  final allDecls = <ProcDecl>[];
  final checkedDecls = <ProcDecl>[];
  final codelessSeen = <String>{};
  for (final mod in modules) {
    final keepBare =
        singleNorm != null && _normPath(mod.filePath) == singleNorm;
    final owners = typeOwners[mod.filePath]!;
    final defined = registry[mod.moduleName]!;
    for (final decl in mod.ast.procDeclarations) {
      if (decl.imported) continue; // Skip imported — they're in other modules
      // A clause-less declaration of a kernel or builtin guard the runtime
      // implements --- the root self.glp's declarations of the language
      // primitives --- keeps its name: a name with no code binds only to the
      // runtime's kernel or guard of that name (IGLP code-format-fragment.tex,
      // Loader, step 3), and every call to it was left bare in step 4.
      final codeless =
          !defined.contains(decl.key) && isBuiltinProcedure(decl.key);
      if (codeless && !codelessSeen.add(decl.key)) continue;
      final name = keepBare || codeless
          ? decl.name
          : '${mod.moduleName}:${decl.name}';
      allDecls.add(ProcDecl(
        name,
        decl.argTypes,
        decl.line,
        decl.column,
        typeParams: decl.typeParams,
        isBuiltin: decl.isBuiltin,
        exported: keepBare,
      ));
      // Step 4 for types: in the linked program each of this declaration's
      // types carries the prefix of the scope that defines it for THIS module.
      checkedDecls.add(ProcDecl(
        name,
        renameDeclTypes(decl, owners),
        decl.line,
        decl.column,
        typeParams: decl.typeParams,
        isBuiltin: decl.isBuiltin,
        exported: keepBare,
      ));
    }
  }

  allDecls.addAll(aliasDecls);
  checkedDecls.addAll(aliasCheckedDecls);

  return LinkResult(Program(allProcedures, 0, 0), allDecls,
      checkedDeclarations: checkedDecls);
}

/// Dead-code elimination: the linker's step 5 (modules.tex sec:static-linking).
///
/// Returns the linked program restricted to its \emph{reachable} procedures:
/// the root's exported procedures (the entry-point aliases — the bare,
/// unprefixed procedures the linker generated — and the renamed procedures they
/// call) and the transitive closure of procedures called in the body of a
/// reachable one (TGLP modules.tex, Compilation, fifth step: "the compiler
/// retains the reachable procedures: the exported procedures of the program's
/// self.glp, and every procedure called in the body of a reachable one").
/// Guards are followed too: a defined guard's call site is renamed to `M:g` in
/// step with its procedure (so the partial evaluator unfolds it after
/// linking), and is followed by that name exactly, as a body call is.  A name
/// is followed only as step 4 resolved it: until 2026-10-03 a guard left bare
/// was also followed by its base name, keeping every renamed `M:g` of that
/// base name in any module, which nothing calls.  Restricting the program to
/// its reachable procedures is semantically equivalent to the whole;
/// everything else is pruned.
/// The reachability seed is the bare (unprefixed) entry-point aliases the linker
/// generated: a directory's are the root self.glp's exported procedures, a
/// single module's are every one of its own procedures. Every other procedure
/// carries a renamed `M:p` name, so the unprefixed aliases are exactly the
/// entry points.
LinkResult eliminateDeadCode(LinkResult linked) {
  final procedures = linked.program.procedures;

  final byFullName = <String, Procedure>{};
  for (final p in procedures) {
    byFullName['${p.name}/${p.arity}'] = p;
  }

  final reachable = <String>{};
  final work = <String>[];
  void markFull(String key) {
    if (byFullName.containsKey(key) && reachable.add(key)) work.add(key);
  }

  void collectFromGoal(Goal g) {
    if (g is SpawnGoal) {
      collectFromGoal(g.innerGoal);
      return;
    }
    // Body calls carry resolved names: M:p for local/ancestor procedures (exact
    // match keeps the target), unqualified for root-scope calls (no procedure
    // here — left to the separately merged root self.glp).
    markFull('${g.functor}/${g.arity}');
  }

  // Seed: the bare (unprefixed) entry-point aliases the linker generated.
  for (final p in procedures) {
    if (!p.name.contains(':')) markFull('${p.name}/${p.arity}');
  }
  while (work.isNotEmpty) {
    final proc = byFullName[work.removeLast()]!;
    for (final clause in proc.clauses) {
      for (final g in clause.body ?? const <Goal>[]) {
        collectFromGoal(g);
      }
      for (final gd in clause.guards ?? const <Guard>[]) {
        // A defined guard carries its resolved name (M:g), followed exactly;
        // a guard left bare is a builtin or a root-scope guard, no procedure
        // of the program's.
        markFull('${gd.predicate}/${gd.args.length}');
      }
    }
  }

  final keptProcedures = procedures
      .where((p) => reachable.contains('${p.name}/${p.arity}'))
      .toList();
  // A declaration is kept with its procedure, and a clause-less declaration of
  // a kernel or builtin guard the runtime implements --- which has no
  // procedure, its name binding to the runtime's (IGLP code-format-fragment.tex,
  // Loader, step 3) --- is kept as it stands: it types the calls to the
  // primitive in the linked program, which is checked against the language
  // primitives alone ([checkedLinkedProgram]).
  bool kept(ProcDecl d) {
    final key = '${d.name}/${d.arity}';
    return reachable.contains(key) ||
        (!byFullName.containsKey(key) && isBuiltinProcedure(key));
  }
  final keptDecls = linked.procDeclarations.where(kept).toList();
  final keptChecked = linked.checkedDeclarations.where(kept).toList();

  return LinkResult(Program(keptProcedures, 0, 0), keptDecls,
      checkedDeclarations: keptChecked,
      scopeDeclarations: linked.scopeDeclarations);
}

/// Resolve a defined-guard call in a clause's guard list, mirroring
/// [_resolveGoal]'s scope order: a guard `g/n` that names a local or ancestor
/// `self.glp` unit-clause procedure is renamed to `M:g/n` so it matches the
/// renamed procedure; a builtin guard or a root-scope guard (no matching module
/// procedure) is left bare. Guards never cross module boundaries (no `M' # g`),
/// so there is no remote case.
Guard _resolveGuard(Guard guard, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs,
    {bool keepLocalBare = false}) {
  final sig = '${guard.predicate}/${guard.args.length}';
  if (localSigs.contains(sig)) {
    // Loaded module keeps bare names: a local guard stays bare.
    if (keepLocalBare) return guard;
    return Guard('$moduleName:${guard.predicate}', guard.args,
        guard.line, guard.column);
  }
  final ancestorModule = ancestorSelfProcs[sig];
  if (ancestorModule != null) {
    return Guard('$ancestorModule:${guard.predicate}', guard.args,
        guard.line, guard.column);
  }
  return guard;
}

/// Every module of [modules] is named by its path from the root
/// ([DiscoveredModule.moduleName]), and two files of one path name --- a
/// directory's `self.glp` and a module file of the directory's name beside the
/// directory --- are two modules step 3 would rename alike, so the program is
/// rejected naming both.  A file the directory walk collects and an
/// `-expose` names is listed once ([_resolveExposes]).
void _requireDistinctModuleNames(List<DiscoveredModule> modules) {
  final fileOfName = <String, String>{};
  for (final m in modules) {
    final path = _normPath(m.filePath);
    final prev = fileOfName.putIfAbsent(m.moduleName, () => path);
    if (prev != path) {
      throw Exception(
          'Two modules of one name: "${m.moduleName}" is the path from the '
          'root of both $prev and $path, so the renaming of modules.tex, '
          'Compilation, third step (every procedure and type renamed M:p and '
          'M:T, M the module\'s path from the root) cannot tell them apart.');
    }
  }
}

/// Every call `q(...)` in the body of [clause], a clause of [caller], of a
/// volitional procedure of a self.vglp in the caller's scope --- the
/// compiled asking clause, its ask stream last, which the self.glp's module
/// it joins holds ([DiscoveredModule.volitional]) --- holds a writer at each
/// output position, as a call `N # q(...)` does
/// ([_CrossModuleResolver.checkVolitionalCalls]): "every call of a
/// volitional procedure, in a clause body or in the initial goal, holds a
/// writer at each output position" (vGLP, Definition "Volitional
/// Procedure, ..., vGLP Program").  A call the caller's own procedure
/// answers is not one.
void _checkScopeVolitionalCalls(
    DiscoveredModule caller,
    Clause clause,
    Set<String> localSigs,
    Map<String, String> ancestorProcs,
    Map<String, DiscoveredModule> byName) {
  Iterable<Goal> calls(Goal g) sync* {
    if (g is SpawnGoal) {
      yield* calls(g.innerGoal);
    } else if (g is! RemoteGoal) {
      yield g;
    }
  }

  for (final g in clause.body ?? const <Goal>[]) {
    for (final call in calls(g)) {
      final sig = '${call.functor}/${call.arity}';
      if (localSigs.contains(sig)) continue;
      final owner = ancestorProcs[sig];
      if (owner == null) continue;
      final v = byName[owner]?.volitional[sig];
      if (v == null) continue;
      checkWritersAtOutputs(call, call.args, v,
          calledAs: v.name,
          where: 'in the clause ${GlpPrinter().printAtom(clause.head)} at '
              'line ${clause.line} of ${caller.filePath}');
    }
  }
}

/// The resolution of a cross-module call `M # p` (TGLP modules.tex, "Cross-
/// module type checking": "The qualifier M is a single child directory or
/// module file relative to the caller's directory: a directory is entered
/// through its self.glp, a module file through its own exported
/// declarations"; Compilation, fourth step: the call "resolves to the procedure
/// p that the qualifier exports ... renamed to its prefix").
///
/// The qualifier names `<caller's directory>/M/self.glp` or
/// `<caller's directory>/M.glp` (or the `.vglp` source compiled as that
/// module), and the call resolves to the procedure of the module so named,
/// `<its path from the root>:p`.  A qualifier naming neither, or both, and a
/// call to a procedure the module it names does not export, do not resolve:
/// each is recorded with its file and line, and [throwIfUnresolved] rejects the
/// program naming them all.  Until 2026-10-02 the qualifier was taken for a
/// module's name, which was its file's, so `M # p` reached whichever module of
/// that name the program held, wherever it lay, and a directory's `self.glp`
/// and a module of its directory's name were one module.
///
/// A qualifier naming a file that is not among the program's modules --- an
/// ancestor `self.glp` above the program's directory calling into a sibling of
/// it --- is renamed by the same path; the procedure is not in the program, and
/// a call to it that the entry points reach is undefined in the linked program.
///
/// Where the call resolves, the imported declaration that types it is compared
/// with the declaration its module exports ([_checkAgreement]), and an import
/// that does not agree with its export is recorded the same way and rejects the
/// program ([throwIfUnresolved]).
class _CrossModuleResolver {
  final Map<String, DiscoveredModule> _byPath = {};
  final List<String> _unresolved = [];
  final List<String> _disagreeing = [];
  final Set<String> _compared = {};
  final Map<DiscoveredModule, TypeEnvironment> _envs = {};

  _CrossModuleResolver(List<DiscoveredModule> modules) {
    for (final m in modules) {
      _byPath.putIfAbsent(_normPath(m.filePath), () => m);
    }
  }

  /// The name of the module [call], made in [caller], calls: the prefix its
  /// procedure is renamed to.
  String resolve(DiscoveredModule caller, RemoteGoal call) {
    final q = call.staticModuleName;
    final inner = call.goal;
    final where = '${caller.filePath}:${call.line}';
    final sig = '${inner.functor}/${inner.arity}';
    final callerDirName = moduleDirectoryName(caller.moduleName,
        isSelfGlp: isSelfGlpFile(caller.filePath));
    final name = callerDirName.isEmpty ? q : '$callerDirName/$q';
    if (inner is RemoteGoal) {
      _unresolved.add('  $where: $q # $inner: a qualifier of more than one '
          'segment is future work (modules.tex, Cross-module type checking)');
      return name;
    }

    final callerDir = _normPath(File(caller.filePath).parent.path);
    final candidates = _candidates(caller, q);
    if (candidates.isEmpty) {
      final bareDir = Directory(ppath.join(callerDir, q)).existsSync()
          ? ' (${ppath.join(callerDir, q)}/ has no self.glp, through which a '
              'directory is entered)'
          : '';
      _unresolved.add('  $where: $q # $sig: $q is neither a child directory '
          'with a self.glp nor a module file of $callerDir$bareDir');
      return name;
    }
    if (candidates.length > 1) {
      _unresolved.add('  $where: $q # $sig: $q names both ${candidates[0]} '
          'and ${candidates[1]}');
      return name;
    }

    final target = _byPath[_normPath(candidates.single)];
    if (target == null) return name;
    final exported = target.ast.procDeclarations.any((d) =>
        d.exported &&
        !d.imported &&
        d.name == inner.functor &&
        d.arity == inner.arity);
    if (!exported) {
      _unresolved.add('  $where: $q # $sig: ${target.filePath} does not '
          'export $sig');
    } else {
      _checkAgreement(caller, target, q, inner);
    }
    return target.moduleName;
  }

  /// The files the qualifier [q] of a call made in [caller] may name:
  /// `<caller's directory>/q/self.glp`, on disc or virtual
  /// ([isVirtualSelfGlp]), and `<caller's directory>/q.glp` or, where there
  /// is none, the `q.vglp` source compiled as that module.
  List<String> _candidates(DiscoveredModule caller, String q) {
    final callerDir = _normPath(File(caller.filePath).parent.path);
    final dirSelf = ppath.join(callerDir, q, 'self.glp');
    final glpFile = ppath.join(callerDir, '$q.glp');
    final vglpFile = ppath.join(callerDir, '$q.vglp');
    return <String>[
      if (hasSelfGlp(ppath.join(callerDir, q))) dirSelf,
      if (File(glpFile).existsSync())
        glpFile
      else if (File(vglpFile).existsSync())
        vglpFile,
    ];
  }

  /// Every call `N # q(...)` in the body of [clause], a clause of [caller],
  /// of a volitional procedure of the module N names --- the compiled asking
  /// clause, its ask stream last --- holds a writer at each output position:
  /// "every call of a volitional procedure, in a clause body or in the
  /// initial goal, holds a writer at each output position" (vGLP, Definition
  /// "Volitional Procedure, Interactive Type, Interactive Term, Ordinary
  /// Clause, vGLP Program"), and "the GLP module that forms the program's
  /// initial goal imports that module's compiled procedures" (Definition
  /// "Canonical Compilation").  By the rule of the `.vglp` front,
  /// [checkWritersAtOutputs], over the record N's source gives
  /// ([DiscoveredModule.volitional]); a refusal, a [CompileError], names the
  /// call, the clause, its file and the position.  A qualifier that names no
  /// one module is left to [resolve] to report.
  void checkVolitionalCalls(DiscoveredModule caller, Clause clause) {
    Iterable<RemoteGoal> remotes(Goal g) sync* {
      if (g is RemoteGoal) {
        yield g;
      } else if (g is SpawnGoal) {
        yield* remotes(g.innerGoal);
      }
    }

    for (final g in clause.body ?? const <Goal>[]) {
      for (final call in remotes(g)) {
        final inner = call.goal;
        if (inner is RemoteGoal) continue;
        final candidates = _candidates(caller, call.staticModuleName);
        if (candidates.length != 1) continue;
        final target = _byPath[_normPath(candidates.single)];
        if (target?.silentDispatch == inner.functor && inner.arity == 3) {
          _checkSilentOutput(call, inner, clause, caller);
          continue;
        }
        final v = target?.volitional['${inner.functor}/${inner.arity}'];
        if (v == null) continue;
        checkWritersAtOutputs(call, inner.args, v,
            calledAs: '${call.staticModuleName}#${v.name}',
            where: 'in the clause ${GlpPrinter().printAtom(clause.head)} at '
                'line ${clause.line} of ${caller.filePath}');
      }
    }
  }

  /// A call `N # d(Asks, PCh, MCh)` of the dispatcher of a program that
  /// defines no Output gives it the program's end of the person channel,
  /// MCh: anonymous, the program writing nothing to its person, or refused,
  /// naming Output (vGLP 169b008, Definition "Canonical Compilation"; vGLP,
  /// 2026-10-13 19:25 UTC, item 1).
  void _checkSilentOutput(
      RemoteGoal call, Goal inner, Clause clause, DiscoveredModule caller) {
    if (inner.args[2] is UnderscoreTerm) return;
    throw CompileError(
        'The call ${GlpPrinter().printGoal(call)}, in the clause '
        '${GlpPrinter().printAtom(clause.head)} at line ${clause.line} of '
        '${caller.filePath}, gives the program of ${call.staticModuleName} '
        'its end of the person channel, '
        '${GlpPrinter().printTerm(inner.args[2])}, and that program defines '
        'no type Output: a vGLP program declares the type of its output to '
        'its person by defining the type Output, a reserved name, and one '
        'whose output end is not anonymous and defines none is refused (vGLP '
        '169b008, Definition "Canonical Compilation"; vGLP, 2026-10-13 19:25 '
        'UTC, item 1)',
        call.line, call.column, phase: 'analyzer');
  }

  /// The scope [m] is checked in ([checkModule]'s environment).
  TypeEnvironment _envOf(DiscoveredModule m) => _envs.putIfAbsent(
      m, () => buildModuleTypeEnvironment(m.ast, ancestorScope: m.ancestorScope));

  /// The imported declaration of `q # p` in [caller]'s scope against the
  /// declaration [target] exports (TGLP modules.tex, "Cross-module type
  /// checking": "The imported declaration must agree with the declaration M
  /// exports: their type automata are equal (structural type compatibility)
  /// once the exported declaration's type parameters, if any, are
  /// instantiated (Definition Instantiation); subtyping applies to the
  /// arguments of a call, not between the two declarations").
  ///
  /// The imported declaration is the one the checker types the call by
  /// (well_typed_clause.dart, `_checkRemoteGoal`): `q#p/n` in the caller's
  /// scope, its parameterised template where it names type parameters.  A
  /// call with none is refused by that check ("No imported declaration"), and
  /// is not compared here.  Each declaration is expanded against its own
  /// module's scope, a parameter of either standing as an abstract type
  /// ([buildAbstractInstance]): the export's are instantiated by matching, and
  /// the import's are the ones "the importing module holds open", which "stay
  /// open across the module boundary" ([importExportDisagreement]).
  void _checkAgreement(DiscoveredModule caller, DiscoveredModule target,
      String q, Goal inner) {
    final key = '$q#${inner.functor}/${inner.arity}';
    if (!_compared.add('${_normPath(caller.filePath)}|$key')) return;

    final callerEnv = _envOf(caller);
    final importTemplate = callerEnv.paramProcDecls[key];
    final importDecl = importTemplate ?? callerEnv.procedures[key];
    if (importDecl == null) return;

    final targetEnv = _envOf(target);
    final exportKey = '${inner.functor}/${inner.arity}';
    final exportTemplate = targetEnv.paramProcDecls[exportKey];
    final exportDecl = exportTemplate ?? targetEnv.procedures[exportKey];
    if (exportDecl == null) return;

    // The source declarations, as written, for the report.
    ProcDecl? source(DiscoveredModule m, bool Function(ProcDecl) test) {
      for (final d in m.ast.procDeclarations) {
        if (test(d)) return d;
      }
      return null;
    }

    final importSource = source(
        caller,
        (d) =>
            d.imported &&
            d.modulePath == q &&
            d.name == inner.functor &&
            d.arity == inner.arity);
    final exportSource = source(
        target,
        (d) =>
            d.exported &&
            !d.imported &&
            d.name == inner.functor &&
            d.arity == inner.arity);

    (ProcDecl, Map<String, TypeDef>, Set<String>) instance(
        ProcDecl decl, bool parameterised, TypeEnvironment env) {
      if (!parameterised) return (decl, env.types, const {});
      final inst = buildAbstractInstance(
          decl, decl.typeParams, env.typeTemplates,
          knownMonoTypes: env.types.keys.toSet());
      return (
        inst.decl,
        {...env.types, ...inst.typeDefs},
        inst.abstractTypeNames
      );
    }

    final where = importSource != null
        ? '${caller.filePath}:${importSource.line}'
        : '${caller.filePath} (in its scope, line ${importDecl.line})';
    final importText = _declarationText(importSource ?? importDecl);
    final exportText = _declarationText(exportSource ?? exportDecl);
    final exportWhere =
        '${target.filePath}:${(exportSource ?? exportDecl).line}';

    final DeclarationDisagreement? found;
    try {
      final (iDecl, iTypes, iParams) =
          instance(importDecl, importTemplate != null, callerEnv);
      final (eDecl, eTypes, eParams) =
          instance(exportDecl, exportTemplate != null, targetEnv);
      found = importExportDisagreement(
        imported: iDecl,
        importedTypes: iTypes,
        importedParams: iParams,
        exported: eDecl,
        exportedTypes: eTypes,
        exportedParams: eParams,
      );
    } on UnknownTypeError catch (e) {
      _disagreeing.add('  $where: $importText\n'
          '    against $exportWhere: $exportText\n'
          '    the automaton of one of them names the undefined type '
          '${e.typeName}');
      return;
    }
    if (found == null) return;
    _disagreeing.add('  $where: $importText\n'
        '    does not agree with $exportWhere: $exportText\n'
        '    ${found.argument == 0 ? found.detail : 'argument ${found.argument}: ${found.detail}'}');
  }

  /// Rejects the program if any call [resolve] was given does not resolve, or
  /// resolves to an export its imported declaration does not agree with.
  void throwIfUnresolved() {
    if (_unresolved.isEmpty) {
      _throwIfDisagreeing();
      return;
    }

    throw Exception(
        'Cross-module calls that resolve to no exported procedure '
        '(modules.tex, Compilation, fourth step: a call M # p, whose '
        'qualifier is a single child directory or module file relative to '
        'the caller\'s directory, resolves to the procedure p that the '
        'qualifier exports --- a directory exports through its self.glp, a '
        'module file through its own exported declarations):\n'
        '${_unresolved.join('\n')}');
  }

  void _throwIfDisagreeing() {
    if (_disagreeing.isEmpty) return;
    throw Exception(
        'Imported declarations that do not agree with the declarations their '
        'modules export (modules.tex, Cross-module type checking: "The '
        'imported declaration must agree with the declaration M exports: '
        'their type automata are equal (structural type compatibility) once '
        'the exported declaration\'s type parameters, if any, are '
        'instantiated (Definition Instantiation); subtyping applies to the '
        'arguments of a call, not between the two declarations"):\n'
        '${_disagreeing.join('\n')}');
  }
}

/// [d] as written: its kind, its parameter list, its name and its argument
/// types.
String _declarationText(ProcDecl d) {
  final kind = d.imported ? 'imported ' : (d.exported ? 'exported ' : '');
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${kind}procedure$params ${d.qualifiedName}(${d.argTypes.join(', ')}).';
}

/// Resolve a single goal in a clause body.
///
/// Resolution order: local procedure → ancestor self.glp chain → root scope/stdlib.
/// A cross-module call `M' # p` is resolved by [remote], which gives the name
/// of the module it calls ([_CrossModuleResolver.resolve]).
Goal _resolveGoal(Goal goal, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs, String Function(RemoteGoal) remote,
    {bool keepLocalBare = false}) {
  // RemoteGoal: M' # p(...) → <the path of M'>:p(...)
  if (goal is RemoteGoal) {
    return Goal(
      '${remote(goal)}:${goal.goal.functor}',
      goal.goal.args,
      goal.line,
      goal.column,
    );
  }

  // SpawnGoal: resolve inner goal, keep wrapper
  if (goal is SpawnGoal) {
    final resolvedInner = _resolveGoal(
        goal.innerGoal, moduleName, localSigs, ancestorSelfProcs, remote,
        keepLocalBare: keepLocalBare);
    if (!identical(resolvedInner, goal.innerGoal)) {
      return SpawnGoal(resolvedInner, goal.agentId, goal.line, goal.column);
    }
    return goal;
  }

  // find_type(P/N, T) names a procedure rather than calling one. P/N is
  // resolved in the calling module's scope exactly as a call to P/N would be
  // (GLP-Spec catalogue, "Dynamic activation": the identity of the declaration
  // of P/N in the caller's scope), so the kernel's lookup key is the one the
  // compiled module carries (TGLP Implementation Notes, "The tables"): `M:p/n`
  // for the module's own procedure, `anc:p/n` for an ancestor self.glp's, bare
  // for an entry-point alias or a root-scope declaration.
  // The call itself is then resolved as any call is: find_type/2 is the root
  // self.glp's, and resolves to its renamed form; '_find_type' is the kernel
  // and stays bare.
  //
  // find_type(P/N, Hs, T) names the caller's parameterised declaration of P/N
  // (GLP-Spec catalogue, "Dynamic activation"), resolved the same way.
  // opaque_type(P/N, Module, Hs) names an entry of Module's exports, not a
  // declaration of the caller's scope, and its P/N is not resolved.
  if ((goal.functor == 'find_type' || goal.functor == '_find_type') &&
      (goal.arity == 2 || goal.arity == 3)) {
    final ref = _resolveProcedureRef(
        goal.args[0], moduleName, localSigs, ancestorSelfProcs,
        keepLocalBare: keepLocalBare);
    if (!identical(ref, goal.args[0])) {
      goal = Goal(goal.functor, [ref, ...goal.args.skip(1)], goal.line,
          goal.column);
    }
  }

  // Regular goal: check if it matches a local procedure
  final sig = '${goal.functor}/${goal.arity}';
  if (localSigs.contains(sig)) {
    // Loaded module keeps bare names: a local call stays bare.
    if (keepLocalBare) return goal;
    return Goal(
      '$moduleName:${goal.functor}',
      goal.args,
      goal.line,
      goal.column,
    );
  }

  // Check ancestor self.glp procedures
  final ancestorModule = ancestorSelfProcs[sig];
  if (ancestorModule != null) {
    return Goal(
      '$ancestorModule:${goal.functor}',
      goal.args,
      goal.line,
      goal.column,
    );
  }

  // Root scope/stdlib/body kernel — leave unchanged
  return goal;
}

/// Resolve a procedure reference `P/N` — the first argument of `find_type/2`
/// — in the same scope order as [_resolveGoal]: local → ancestor self.glp →
/// root scope (left bare). Anything that is not a ground `Name/Arity` term is
/// returned as it is; the kernel reports it.
Term _resolveProcedureRef(Term ref, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs,
    {bool keepLocalBare = false}) {
  if (ref is! StructTerm || ref.functor != '/' || ref.args.length != 2) {
    return ref;
  }
  final nameTerm = ref.args[0];
  final arityTerm = ref.args[1];
  if (nameTerm is! ConstTerm || nameTerm.value is! String) return ref;
  if (arityTerm is! ConstTerm || arityTerm.value is! int) return ref;
  final name = nameTerm.value as String;
  final sig = '$name/${arityTerm.value}';
  String? owner;
  if (localSigs.contains(sig)) {
    if (keepLocalBare) return ref;
    owner = moduleName;
  } else {
    owner = ancestorSelfProcs[sig];
  }
  if (owner == null) return ref;
  return StructTerm(
      '/',
      [ConstTerm('$owner:$name', nameTerm.line, nameTerm.column), arityTerm],
      ref.line,
      ref.column);
}

/// Find the ProcDecl for a procedure in a module (non-imported only).
ProcDecl? _findProcDecl(DiscoveredModule mod, String name, int arity) {
  for (final d in mod.ast.procDeclarations) {
    if (!d.imported && d.name == name && d.arity == arity) return d;
  }
  return null;
}

/// Create an alias clause with mode-aware argument forwarding.
///
/// Given a procedure declaration, generates:
///   p(V0, V1, V2) :- M:p(V0?, V1, V2).
/// where input args (declared with ?) get reader annotation in the body,
/// and output args (no ?) get writer annotation (pass-through).
///
/// Without a declaration, falls back to all-reader body args:
///   p(V0, V1, V2) :- M:p(V0?, V1?, V2?).
Clause _makeAliasClause(String name, int arity, String targetName,
    {ProcDecl? declaration}) {
  if (arity == 0) {
    // Zero-arity: p :- M:p.
    final head = Atom(name, [], 0, 0);
    final body = [Goal(targetName, [], 0, 0)];
    return Clause(head, body: body, line: 0, column: 0);
  }

  bool isInputArg(int i) => declaration != null && i < declaration.argTypes.length
      ? declaration.isInputArg(i)
      : true; // Fallback: assume input when no declaration

  // Head args (V prefix — underscore prefix causes issues in codegen).
  // Input arg (T?): the head captures the caller's value as a writer.
  // Output arg (T): the head is a reader hole that the body's writer fills.
  // (For arity>0 procedures with output args, a head writer there would pair
  // with a body writer — an SRSW violation; the head must be the reader.)
  final headArgs = List.generate(
      arity, (i) => VarTerm('V$i', !isInputArg(i), 0, 0) as Term);

  // Body args: input → reader (forward the value), output → writer (so the
  // callee fills it).
  final bodyArgs = List.generate(arity, (i) {
    final isInput = isInputArg(i);
    return VarTerm('V$i', isInput, 0, 0) as Term;
  });

  final head = Atom(name, headArgs, 0, 0);
  final body = [Goal(targetName, bodyArgs, 0, 0)];
  return Clause(head, body: body, line: 0, column: 0);
}

/// The module defining each type name visible to [mod], by the scope order of
/// modules.tex §Scope construction, walked from the root down, a nearer
/// definition over a farther one: at each directory of the module's chain,
/// the types what that directory's `self.glp` `-expose`s carries, "as if
/// defined in its self.glp ... Shadowing applies as usual" ("The -expose
/// directive"), and over them the `self.glp`'s own; then the module's own
/// definitions over all.  A name absent from the map is defined by no module
/// of the program --- a root-scope, primitive or system type --- and stays
/// bare.  Until 2026-10-07 what an ancestor `-expose`d filled gaps only
/// ([_mergeExposed] did the same), every `self.glp`'s type of its name
/// winning (programs/tests/expose/lift_shadows).
///
/// This is step 4 of §Compilation for types: "every type reference is resolved
/// the same way, to the renamed type of the nearest scope defining it".
Map<String, String> _visibleTypeOwners(
    DiscoveredModule mod, List<DiscoveredModule> modules) {
  final owners = <String, String>{};

  // A self.glp is an ancestor scope by its name and directory (modules.tex,
  // Definition "Root, Scope"), whatever it was loaded as.  The module a
  // single-file load names is not flagged [DiscoveredModule.isSelfGlp] even
  // where it is a self.glp, so until 2026-10-02 the modules it exposes did not
  // see its types: loading programs/tests/agent_roundtrip/self.glp left
  // typed_social_agent:inject_msg/5's Response unrenamed and undefined in the
  // flat module, and the type-identity tables were not built.
  //
  // A module only an -expose brings in has, besides, the self.glp files of
  // its chain that are not modules of the program ([DiscoveredModule.outerScope]),
  // taken in their places on the chain.
  final ancestors = _ancestorSelfGlps(mod, modules);
  final chain = mod.outerScope.isEmpty
      ? ancestors
      : _innermostFirst([...ancestors, ...mod.outerScope]);

  // What an exposer above lifts: the types the exposed signatures carry
  // ([ExposedLift]), each owned by the module that defines it --- in the
  // exposed module's own scope, which may be a self.glp the program does not
  // hold --- and no other type of the exposed module.  Until 2026-10-04 every
  // type the exposed module defined was visible here, and owned by it.
  final modDirNorm = _normPath(File(mod.filePath).parent.path);
  final reaching = [
    for (final em in modules)
      if (em.exposingDir != null &&
          !identical(em, mod) &&
          em.lift != null &&
          _dirUnder(modDirNorm, em.exposingDir!))
        em
  ];
  final levels = <String>{
    for (final s in chain) _normPath(File(s.filePath).parent.path),
    for (final em in reaching) em.exposingDir!,
  }.toList()
    ..sort((a, b) => _depth(a).compareTo(_depth(b)));

  for (final dir in levels) {
    for (final em in reaching) {
      if (em.exposingDir != dir) continue;
      // A type or template whose defining scope the lift does not record is
      // one of a scope above the exposing directory, which the walk has
      // given its owner already; it fills a gap only.
      final lift = em.lift!;
      for (final t in lift.scope.types.keys) {
        if (t.contains(':') || t.contains('<')) continue;
        final origin = lift.scope.typeOrigins[t];
        if (origin != null) {
          owners[t] = origin;
        } else {
          owners.putIfAbsent(t, () => em.moduleName);
        }
      }
      for (final t in lift.scope.typeTemplates.keys) {
        final origin = lift.templateOrigins[t];
        if (origin != null) {
          owners[t] = origin;
        } else {
          owners.putIfAbsent(t, () => em.moduleName);
        }
      }
    }
    for (final s in chain) {
      if (_normPath(File(s.filePath).parent.path) != dir) continue;
      for (final td in s.ast.typeDefs) {
        owners[td.name] = s.moduleName;
      }
    }
  }

  for (final td in mod.ast.typeDefs) {
    owners[td.name] = mod.moduleName;
  }
  return owners;
}

/// [selfGlps] inner-most first, the root self.glp last ([_ancestorSelfGlps]'s
/// order).
List<DiscoveredModule> _innermostFirst(List<DiscoveredModule> selfGlps) {
  int depth(DiscoveredModule s) =>
      ppath.split(_normPath(File(s.filePath).parent.path)).length;
  final roots = selfGlps.where((s) => s.isRoot).toList();
  final rest = selfGlps.where((s) => !s.isRoot).toList()
    ..sort((a, b) => depth(b).compareTo(depth(a)));
  return [...rest, ...roots];
}

/// The modules of the `self.glp` files that are on the chain of a module of
/// [modules] and are not modules of the program, each once
/// ([DiscoveredModule.outerScope]).
List<DiscoveredModule> _outerModules(List<DiscoveredModule> modules) {
  final byFile = <String, DiscoveredModule>{};
  for (final m in modules) {
    for (final o in m.outerScope) {
      byFile.putIfAbsent(_normPath(o.filePath), () => o);
    }
  }
  return byFile.values.toList();
}

/// The `self.glp` modules of [modules] whose directory is [mod]'s or above it
/// --- the ancestor scopes of [mod] among the program's modules (modules.tex,
/// Definition "Root, Scope") --- inner-most first; [mod] itself is not among
/// them.  A `self.glp` is one by its file name ([isSelfGlpFile]) unless
/// [flaggedOnly], which takes only the modules discovered as one
/// ([DiscoveredModule.isSelfGlp]).  Directories are compared normalised and
/// segment by segment: until 2026-10-02 a string prefix decided it, so
/// `a/bc/` took `a/b/self.glp` for an ancestor.
List<DiscoveredModule> _ancestorSelfGlps(
    DiscoveredModule mod, List<DiscoveredModule> modules,
    {bool flaggedOnly = false}) {
  final modPath = _normPath(mod.filePath);
  final modDir = _normPath(File(mod.filePath).parent.path);
  final ancestors = <DiscoveredModule>[];
  DiscoveredModule? root;
  for (final s in modules) {
    if (s.isRoot) {
      if (!identical(s, mod)) root = s;
      continue;
    }
    if (!(flaggedOnly ? s.isSelfGlp : (s.isSelfGlp || isSelfGlpFile(s.filePath)))) {
      continue;
    }
    if (identical(s, mod) || _normPath(s.filePath) == modPath) continue;
    if (_dirUnder(modDir, _normPath(File(s.filePath).parent.path))) {
      ancestors.add(s);
    }
  }
  int depth(DiscoveredModule s) =>
      ppath.split(_normPath(File(s.filePath).parent.path)).length;
  ancestors.sort((a, b) => depth(b).compareTo(depth(a)));
  // The root self.glp is the outermost link of every module's chain, d_1,
  // whatever directory the module's program sits in (TGLP modules.tex: "The
  // root self.glp is in the scope of every module compiled on the device ...:
  // it is d_1").
  if (root != null && !mod.isRoot) ancestors.add(root);
  return ancestors;
}

/// Whether the file at [path] is a `self.glp`.
bool isSelfGlpFile(String path) => ppath.basename(path) == 'self.glp';

/// [_visibleTypeOwners] for every module of the program, and for every
/// `self.glp` on the chain of one that is not a module of it
/// ([DiscoveredModule.outerScope]), keyed by file path — the one key that is
/// unique whatever two files are named.
Map<String, Map<String, String>> typeOwnersByModule(
    List<DiscoveredModule> modules) {
  final byFile = <String, Map<String, String>>{};
  for (final mod in [...modules, ..._outerModules(modules)]) {
    byFile[mod.filePath] = _visibleTypeOwners(mod, modules);
  }
  return byFile;
}

/// A module's type definitions renamed to `M:T`, with every type reference in
/// their alternatives resolved (steps 3 and 4 of §Compilation for types).
List<TypeDef> _renamedTypeDefs(
    DiscoveredModule mod, Map<String, String> owners) {
  return [
    for (final td in mod.ast.typeDefs)
      TypeDef(
        '${mod.moduleName}:${td.name}',
        [
          for (final alt in td.alternatives)
            _renameTypeExpr(alt, owners, td.typeParams.toSet())
        ],
        td.line,
        td.column,
        typeParams: td.typeParams,
      )
  ];
}

/// A declaration's argument types with every reference resolved to the renamed
/// type of the nearest scope defining it. The declaration's own type parameters
/// name no type and are left alone.
List<TypeExpr> renameDeclTypes(ProcDecl decl, Map<String, String> owners) => [
      for (final t in decl.argTypes)
        _renameTypeExpr(t, owners, decl.typeParams.toSet())
    ];

/// Resolve every type name in [expr] against [owners], leaving a type parameter
/// and a name no module defines (root-scope, primitive, system) bare.
TypeExpr _renameTypeExpr(
    TypeExpr expr, Map<String, String> owners, Set<String> typeParams) {
  if (expr is TypeRef) {
    final args = [
      for (final a in expr.typeArgs) _renameTypeExpr(a, owners, typeParams)
    ];
    final owner = typeParams.contains(expr.name) ? null : owners[expr.name];
    return TypeRef(owner == null ? expr.name : '$owner:${expr.name}',
        expr.line, expr.column,
        isInput: expr.isInput, typeArgs: args);
  }
  if (expr is StructAlt) {
    return StructAlt(
        expr.functor,
        [for (final a in expr.args) _renameTypeExpr(a, owners, typeParams)],
        expr.line,
        expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(_renameTypeExpr(expr.head, owners, typeParams),
        _renameTypeExpr(expr.tail, owners, typeParams), expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(_renameTypeExpr(expr.content, owners, typeParams),
        _renameTypeExpr(expr.hole, owners, typeParams), expr.line, expr.column);
  }
  return expr; // ConstantAlt, ListNilAlt, PrimitiveModeAlt: no type name
}

// Ancestor-scope assembly lives in module_hierarchy.dart (buildAncestorScope)
// — the one shared implementation; no linker-local copy.

/// Write the compiled GLP beside each `.vglp` source under [rootDir].
///
/// The scope each source is compiled in is the one the loader would give it —
/// the root scope, its ancestor `self.glp` chain and whatever an ancestor
/// `-expose`s — so the emitted text is exactly what the load produces in
/// memory (vGLP, Definition "Canonical Compilation").
///
/// The self.vglp of a directory under [rootDir] that holds procedures,
/// declarations or types, in a directory with no self.glp or with one the
/// compiler wrote, is written as that directory's self.glp, the compiler's
/// header its first line (vGLP Cowork, 2026-10-16 11:25 UTC, item 1: ":emit
/// writes it as that directory's self.glp, with the compiler's header,
/// linked as N.glp is for N.vglp").  A hand-written self.glp is never
/// overwritten: its self.vglp joins it in memory as the program loads.
List<String> emitVglpSources(String rootDir,
    {required String rootSelfGlpPath,
    void Function(String message)? onSkip}) {
  final root = Directory(rootDir).absolute;
  final programsDir = File(rootSelfGlpPath).parent.absolute.path;
  // A source in the old syntax compiles against the generic mediator, and
  // emitCompiledVglp refuses it where the mediator is missing; one in the
  // paper's syntax needs none.
  final mediator = _mediatorSource(programsDir);

  final rs = _rootScopeOf(rootSelfGlpPath);
  final program = _vglpProgramOf(root, programsDir);
  final selfTexts = <String, String>{};
  final layers = program == null
      ? const <String, Module>{}
      : _compileLayers(program, root, programsDir, rootSelfGlpPath, rs,
          emitted: selfTexts);
  final modules = _discoverGlpModules(root, programsDir, rootSelfGlpPath, rs,
      layers: layers);
  TypeEnvironment scopeFor(String vglpPath) => _vglpScope(File(vglpPath),
      modules, root, programsDir, rootSelfGlpPath, rs, layers);
  final resolveIn = program == null
      ? null
      : _sourceTypes(program, root, programsDir, rootSelfGlpPath, rs, layers);
  final written = emitCompiledVglp(root.path, mediator,
      scopeFor: scopeFor,
      contextFor: program == null
          ? null
          : (p) => _contextFor(program, p, scopeFor(p), resolveIn!),
      readerFor: program?.readerFor,
      onSkip: onSkip);
  final rootNorm = _normPath(root.path);
  for (final e in selfTexts.entries) {
    final dir = _normPath(File(e.key).parent.path);
    if (dir != rootNorm && !ppath.isWithin(rootNorm, dir)) continue;
    File(e.key).writeAsStringSync(e.value);
    written.add(e.key);
  }
  return written;
}
