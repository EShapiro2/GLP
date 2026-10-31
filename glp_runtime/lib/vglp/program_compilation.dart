// glp_runtime/lib/vglp/program_compilation.dart
//
// A `.vglp` source to the text of its GLP module, by the canonical
// compilation (canonical.dart), and the emission of that text beside the
// source; the widget declarations of a source's scope, read from the
// self.vglp beside each self.glp; and the program a tree's `.vglp` sources
// make, which procedures reach a question.
// Spec: vGLP, Section 6, "The Implementation: Compiling vGLP onto GLP", and
// Appendix "The Canonical Compilation".
//
// The old compilation, against a generic mediator, which the paper replaced
// by the dispatcher and the construct processes on 2026-09-23, is deleted
// (vGLP #22's task, Code_inbox 2026-10-31 15:20 UTC); git holds it.

import 'dart:io';

import '../compiler/ast.dart' as ast;
import '../compiler/error.dart';
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../analysis/type_checker/type_ast.dart';
import 'package:path/path.dart' as ppath;

import '../runtime/module_hierarchy.dart' show discoverSelfChain, hasSelfGlp;
import 'canonical.dart';
import 'dispatcher.dart' show DispatcherSource;

export 'canonical.dart' show compiledHeader;

/// Compile the text of a `.vglp` source to the text of its GLP module.
///
/// A source in the paper's syntax --- `procedure (T)*p(...)`, `(A)*p(...)` ---
/// compiles by the canonical compilation (canonical.dart), against the
/// [dispatcher]'s generic source, programs/vglp/dispatcher.glp, and in its
/// [scope].  A source in the paper's syntax at [path] is compiled with the
/// widget declarations of its scope, read from the self.vglp beside each
/// self.glp from the root, [programsDir], down (scopeWidgetDeclarations).
///
/// "A procedure that reaches a question is exported, imported and shared as
/// any procedure is, with its ask stream added" (vGLP at a89a751,
/// sections/parts/canonical-compilation.tex): a source at [path] that imports
/// one, by the source of the module beside it that its import names
/// (_reachingBeside), or by [reachingOf] where the loader gives it, gives it
/// and each call of it an ask stream; a source in neither syntax that so
/// reaches a question, through a procedure it imports or calls from its
/// scope, is compiled by the canonical compilation, as one in the paper's
/// syntax is.  [context] is the linked program the source is a module of
/// ([CanonicalContext]), where the loader compiles it.
///
/// A source that is in no syntax of the paper and reaches no question --- one
/// in the old syntax, with volition guards `*(...)`, which the paper replaced
/// on 2026-09-23 --- is refused: no compilation of it is defined.
String compileVglpSource(String text,
    {DispatcherSource? dispatcher,
    String? programsDir,
    TypeEnvironment? scope,
    String? path,
    ReachingReader? reachingOf,
    CanonicalContext? context}) {
  final ReachingReader? reader = reachingOf ??
      (path == null ? null : (module) => _reachingBeside(path, module));
  if (isPaperSyntaxSource(text) ||
      (reachingSignatures(text,
                  reachingOf: reader,
                  scopeReaching: context?.scopeReaching ?? const {}) ??
              const {})
          .isNotEmpty) {
    final layer = context?.layer ?? false;
    return compileCanonical(text,
            dispatcher: layer ? null : dispatcher,
            scope: scope,
            scopeWidgets: path != null && programsDir != null
                ? scopeWidgetDeclarations(path, programsDir)
                : const {},
            reachingOf: reader,
            context: context)
        .source;
  }
  throw StateError('${path ?? 'The source'} is not in the paper\'s syntax and '
      'reaches no question: a source in the old syntax, with volition guards '
      '*(...), is not compiled (vGLP, Section 6, "The Implementation: '
      'Compiling vGLP onto GLP")');
}

/// The procedures that reach a question of the module [module] that the
/// source at [importer] imports from, by its source ([reachingSignatures]),
/// or null where [module] names no vGLP module beside it.
///
/// The qualifier N of an import is "a single child directory or module file
/// relative to the caller's directory" (TGLP modules.tex, "Cross-module type
/// checking").  A module file N.vglp beside the source, which the loader
/// compiles, is read by its source; so is one whose N.glp beside it is its
/// compiled module, which the loader links in its place.  A module N.glp
/// written by hand, which stands, is a GLP module, as is any qualifier
/// naming no N.vglp beside the source --- a directory, entered through its
/// self.glp, or a path of several segments, which TGLP leaves to future
/// work: no procedure of it reaches a question.
Map<String, String>? _reachingBeside(String importer, String module) {
  if (module.contains('#')) return null;
  final dir = File(importer).parent.path;
  final sep = Platform.pathSeparator;
  final vglp = File('$dir$sep$module.vglp');
  if (!vglp.existsSync()) return null;
  final glp = File('$dir$sep$module.glp');
  if (glp.existsSync() &&
      !glp.readAsStringSync().startsWith(compiledHeader)) {
    return null;
  }
  return reachingSignatures(vglp.readAsStringSync());
}

/// Emit the compiled GLP beside each `.vglp` source under [rootDir], and return
/// the paths written.
///
/// This is the flag's half of the load: the loader compiles a `.vglp` in memory
/// and runs it, and this writes the same text to disc, which is what the paper's
/// platform section exhibits.
///
/// It never clobbers a hand-written module.  The emitted file is `<stem>.glp`,
/// and a `<stem>.glp` that exists and does not carry the compiler's header is
/// left alone and reported: switching a deployed program onto its compiled
/// agent is its own change.
///
/// [dispatcher] and [programsDir] are as compileVglpSource takes them.
/// [contextFor] and [readerFor] give, where the loader knows the program, the
/// linked program each source is a module of and the procedures of the
/// modules it imports from that reach a question, as the load gives them, so
/// that the text written is the one the load compiles.
List<String> emitCompiledVglp(String rootDir,
    {DispatcherSource? dispatcher,
    String? programsDir,
    required TypeEnvironment Function(String vglpPath) scopeFor,
    CanonicalContext? Function(String vglpPath)? contextFor,
    ReachingReader? Function(String vglpPath)? readerFor,
    void Function(String message)? onSkip}) {
  final root = Directory(rootDir);
  if (!root.existsSync()) {
    throw ArgumentError('Program root directory not found: $rootDir');
  }

  final written = <String>[];
  // In the order of their paths, the same on every file system.
  final sources = root
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.vglp'))
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));

  for (final file in sources) {
    // A self.vglp is no module: its widget declarations are those of its
    // self.glp's scope (scopeWidgetDeclarations), and its procedures and
    // types join its self.glp's module as the program loads, in memory
    // (program_linker.dart, the self.vglp layers).  A hand-written self.glp
    // is never overwritten; where the directory has none, or one the
    // compiler wrote, emitVglpSources writes the layer as its self.glp.
    if (file.uri.pathSegments.last == selfVglp) continue;
    final target = '${file.path.substring(0, file.path.length - 5)}.glp';
    final existing = File(target);
    if (existing.existsSync() &&
        !existing.readAsStringSync().startsWith(compiledHeader)) {
      onSkip?.call('$target is hand-written; not overwritten');
      continue;
    }

    final text = file.readAsStringSync();
    existing.writeAsStringSync(compileVglpSource(text,
        dispatcher: dispatcher,
        programsDir: programsDir,
        // The canonical compilation builds the construct processes from the
        // types of this scope.
        scope: scopeFor(file.path),
        path: file.path,
        reachingOf: readerFor?.call(file.path),
        context: contextFor?.call(file.path)));
    written.add(target);
  }
  return written;
}

/// The file of a directory's widget declarations, beside its self.glp.
const selfVglp = 'self.vglp';

/// The widget declarations in scope of the `.vglp` source at [path]: those of
/// the self.vglp beside each self.glp from the root, [programsDir], down to
/// the source's own directory, each more local one overriding a more global
/// one (Definition "Widget Declaration, Default Widget": "Widget declarations
/// are scoped as type declarations are: a declaration at the root holds for
/// every program, one in a module holds in that module, and a local
/// declaration overrides a global one"; vGLP #5 Cowork, 2026-10-03 08:16 UTC,
/// item 7).  The chain is the one the source's type scope is built from (TGLP
/// modules.tex, Definition "Root, Scope"), a directory with no self.glp
/// adding nothing; the source's own declarations override these in turn
/// (compileCanonical).
Map<String, String> scopeWidgetDeclarations(String path, String programsDir) {
  final chain = [
    '$programsDir${Platform.pathSeparator}self.glp',
    ...discoverSelfChain(
        targetFile: path,
        rootDir: File(path).parent.path,
        programsDir: programsDir),
  ];
  final out = <String, String>{};
  for (final selfGlp in chain) {
    final file = File(
        '${File(selfGlp).parent.path}${Platform.pathSeparator}$selfVglp');
    if (!hasSelfGlp(File(selfGlp).parent.path) || !file.existsSync()) {
      continue;
    }
    out.addAll(readSelfVglp(file.path));
  }
  return out;
}

/// The widget declarations of the self.vglp at [path], `T =::= W.`, by moded
/// type.  The self.vglp of a directory "holds the procedures, volitional or
/// not, and the widget declarations shared by every module in that directory
/// and below it" (vGLP at a89a751, sections/parts/canonical-compilation.tex);
/// its procedures and types are compiled as its directory's layer
/// ([VglpProgram]), and only its widget declarations are read here.
Map<String, String> readSelfVglp(String path) {
  try {
    return extractWidgetDeclarations(File(path).readAsStringSync())
        .byModedType;
  } on CompileError catch (e) {
    throw CompileError('$path: ${e.message}', e.line, e.column,
        category: e.category);
  }
}

// ---------------------------------------------------------------------------
// The program: every module's questions, and the self.vglp layers
// ---------------------------------------------------------------------------
//
// "A vGLP program is organised in modules as a GLP program is.  A .vglp
// source in the directory tree of a program is compiled into a module of that
// program, as a .glp source is, and the self.vglp of a directory holds the
// procedures, volitional or not, and the widget declarations shared by every
// module in that directory and below it, as its self.glp holds those of GLP.
// A procedure that reaches a question is exported, imported and shared as
// any procedure is, with its ask stream added, so the questions of a program
// are those of all its modules.  The compilation forms the type of the asks,
// the construct processes and the dispatcher for the linked program" (vGLP at
// a89a751, sections/parts/canonical-compilation.tex).
//
// The program's vGLP sources are read before any module is compiled: the
// self.vglp of each directory on the program's chains that holds a
// procedure, a declaration or a type --- its LAYER, which joins the module
// of the self.glp beside it --- and each .vglp module source of the tree,
// one the loader compiles or one whose compiled module, committed beside it,
// stands for it.  Their interactive types, in that order --- the layers
// outermost first, then the modules by path --- are the program's
// questions, which every compiled module's questions, construct processes
// and dispatcher are formed over, so that the asks of every module are of
// one type and its dispatcher serves them all; and which of their
// procedures reach a question is the least fixpoint over the calls of all of
// them ([programReaching]).  A program of one module with no layer has that
// module's questions, as before.

/// The vGLP sources of a program, read before any of its modules is
/// compiled ([VglpFront]), with the procedures of each that reach a question.
class VglpProgram {
  /// The self.vglp layers, outermost first.
  final List<String> layers;

  /// The module sources, by path.
  final List<String> modules;

  final Map<String, VglpFront> fronts;
  final Map<String, Set<String>> reaching;

  /// The device's root, programs/, above which no scope reaches.
  final String? programsDir;

  final Map<String, Set<String>> _selfGlpSigs = {};

  VglpProgram._(this.layers, this.modules, this.fronts, this.reaching,
      this.programsDir);

  /// The program of the sources at [layers] and [modules], each by its path:
  /// those read by the front end, a source in the old syntax keeping its old
  /// compilation and joining none.  A layer whose directory has no self.glp
  /// joins the empty module of its virtual self.glp, which `:emit` writes as
  /// that directory's self.glp (module_hierarchy.dart, isVirtualSelfGlp).
  static VglpProgram of(
      {required List<String> layers,
      required List<String> modules,
      String? programsDir}) {
    final fronts = <String, VglpFront>{};
    final layerPaths = <String>[];
    for (final path in layers) {
      final front = VglpFront.of(File(path).readAsStringSync());
      if (front == null) continue;
      fronts[_norm(path)] = front;
      layerPaths.add(_norm(path));
    }
    final modulePaths = <String>[];
    for (final path in [...modules]..sort()) {
      final front = VglpFront.of(File(path).readAsStringSync());
      if (front == null) continue;
      fronts[_norm(path)] = front;
      modulePaths.add(_norm(path));
    }
    final program = VglpProgram._(layerPaths, modulePaths, fronts, {},
        programsDir == null ? null : _norm(programsDir));
    program.reaching.addAll(programReaching(fronts, target: program._target));
    return program;
  }

  static String _norm(String p) => ppath.normalize(File(p).absolute.path);

  /// Whether the source at [path] is of the program and a procedure of it
  /// reaches a question.
  bool reaches(String path) => reaching[_norm(path)]?.isNotEmpty ?? false;

  /// The procedure of another source of the program that the call [g] of the
  /// source [key] names, as its key and signature, or null.  A call N # p
  /// names the p of the qualifier N: a child directory with a self.glp,
  /// entered through it and so through its layer, or a module file N.vglp
  /// beside the caller (TGLP modules.tex, "Cross-module type checking").  A
  /// call p names the nearest p of the caller's scope, a directory's self.glp
  /// and its layer one module: a self.glp's p, GLP's, reaches none.
  (String, String)? _target(String key, ast.Goal g) {
    final dir = File(key).parent.path;
    if (g is ast.RemoteGoal) {
      final inner = g.goal;
      if (inner is ast.RemoteGoal) return null;
      final q = g.staticModuleName;
      final sig = '${inner.functor}/${inner.arity}';
      final layer = _norm(ppath.join(dir, q, selfVglp));
      if (hasSelfGlp(ppath.join(dir, q))) {
        return fronts.containsKey(layer) && layers.contains(layer)
            ? (layer, sig)
            : null;
      }
      final module = _norm(ppath.join(dir, '$q.vglp'));
      return modules.contains(module) ? (module, sig) : null;
    }
    final sig = '${g.functor}/${g.args.length}';
    final isLayer = layers.contains(key);
    var first = true;
    for (final d in _directoriesFrom(_norm(dir))) {
      if (_selfGlpDefines(d, sig)) return null;
      final layer = _norm(ppath.join(d, selfVglp));
      if (!(first && isLayer) &&
          layers.contains(layer) &&
          fronts[layer]!.own.contains(sig)) {
        return (layer, sig);
      }
      first = false;
    }
    return null;
  }

  /// The directories of the scope chain from [dir] upward: [dir], then its
  /// parent and so on while within the device's root, [programsDir], up to
  /// the root itself; [dir] alone where there is no root or [dir] is not
  /// within it.
  Iterable<String> _directoriesFrom(String dir) sync* {
    var d = dir;
    while (true) {
      yield d;
      if (programsDir == null || d == programsDir) return;
      if (!ppath.isWithin(programsDir!, d)) return;
      d = ppath.dirname(d);
    }
  }

  /// Whether the self.glp of [dir] defines or declares [sig]: one written by
  /// the compiler from the self.vglp beside it, its first line the
  /// compiler's header, stands for that self.vglp, which is the source read.
  bool _selfGlpDefines(String dir, String sig) =>
      _selfGlpSigs.putIfAbsent(dir, () {
        final f = File(ppath.join(dir, 'self.glp'));
        if (!f.existsSync()) return const {};
        final text = f.readAsStringSync();
        if (text.startsWith(compiledHeader)) return const {};
        final m = Parser(Lexer(text).tokenize()).parseModule();
        return {
          for (final d in m.procDeclarations)
            if (!d.imported) d.key,
          for (final p in m.procedures) p.signature,
        };
      }).contains(sig);

  /// The procedures of the scope of the source at [path] that reach a
  /// question, by the signature a call names them by, each with its record
  /// where it is volitional ([CanonicalContext.scopeReaching]).
  Map<String, VolitionalCall?> scopeReachingFor(String path) {
    final key = _norm(path);
    final out = <String, VolitionalCall?>{};
    final sigs = {
      for (final l in layers)
        if (l != key) ...reaching[l]!
    };
    for (final sig in sigs) {
      final slash = sig.lastIndexOf('/');
      final n = int.parse(sig.substring(slash + 1));
      final t = _target(
          key,
          ast.Goal(sig.substring(0, slash),
              [for (var i = 0; i < n; i++) ast.UnderscoreTerm(0, 0)],
              0, 0));
      if (t == null || !reaching[t.$1]!.contains(t.$2)) continue;
      out[sig] = fronts[t.$1]!.volitional[t.$2];
    }
    return out;
  }

  /// The procedures that reach a question of the module a call of the source
  /// at [path] names by its qualifier ([ReachingReader]): by their source
  /// signatures, each to itself.
  ReachingReader readerFor(String path) {
    final key = _norm(path);
    return (module) {
      final t = _target(
          key,
          ast.RemoteGoal(ast.ConstTerm(module, 0, 0),
              ast.Goal('_', const [], 0, 0), 0, 0));
      if (t == null) return null;
      return {for (final s in reaching[t.$1]!) s: s};
    };
  }

  /// The context of the source at [path] ([CanonicalContext]): the
  /// program's interactive types, each resolving its names by [resolveIn]
  /// in the scope of the source declaring it; [resolvable] says which names
  /// the source's own scope defines.
  CanonicalContext contextFor(String path,
      {required bool Function(String name) resolvable,
      required TypeDef? Function(String source, String name) resolveIn,
      Set<String> reservedTypes = const {},
      Set<String> reservedProcedures = const {}}) {
    final key = _norm(path);
    final stems = _functorStems();
    final widgets = <String, Map<String, String>>{};
    Map<String, String>? widgetsAt(String s, int i) {
      final d = _declaredIn[(s, i)];
      if (d == null || programsDir == null) return null;
      return widgets.putIfAbsent(d, () => _widgetsIn(d));
    }
    return CanonicalContext(
      interactive: [
        for (final s in [...layers, ...modules])
          for (var i = 0; i < fronts[s]!.interactive.length; i++)
            ProgramInteractive(
                fronts[s]!.interactive[i].type,
                fronts[s]!.interactive[i].readerMode,
                fronts[s]!.interactive[i].params,
                fronts[s]!.interactive[i].line,
                fronts[s]!.interactive[i].column,
                (name) => resolveIn(s, name),
                source: s,
                functor: stems[(s, i)],
                widgets: widgetsAt(s, i)),
      ],
      scopeReaching: scopeReachingFor(key),
      resolvable: resolvable,
      reservedTypes: reservedTypes,
      reservedProcedures: reservedProcedures,
      layer: layers.contains(key),
      self: key,
      resolveSelf: (name) => resolveIn(key, name),
      definerOf: definerOf,
    );
  }

  /// The functor stem of each interactive type of the program, t without its
  /// mode suffix's coincidence count, by its source and its place in the
  /// source's interactive types: "t is the name of T in lowercase, the names
  /// of its type arguments, so formed, appended, with _w for writer mode and
  /// _r for reader mode ...  Where two modules of M define types of one name,
  /// the name of each is prefixed with the path of its module" (vGLP ed75782,
  /// sections/parts/canonical-compilation.tex).  Each name in T, T's own and
  /// its type arguments', is formed by itself ([_nameInFunctor]), in the
  /// scope of the source that declares the question: so Stream over two
  /// Cards gives stream_parent_card_r and stream_child_card_r (vGLP, 2026-10-
  /// 16 12:30 UTC, (b)).  A volitional procedure a source imports asks its
  /// exporter's question, and takes its exporter's functor.
  Map<(String, int), String> _functorStems() {
    final out = <(String, int), String>{};
    for (final s in [...layers, ...modules]) {
      final f = fronts[s]!;
      for (var i = 0; i < f.volitional.length; i++) {
        final t = f.interactive[i];
        _declaredIn[(s, i)] = s;
        out[(s, i)] = questionFunctorStemBy(t.type, t.readerMode,
            (name) => _nameInFunctor(s, name, t.params));
      }
    }
    for (final s in [...layers, ...modules]) {
      final f = fronts[s]!;
      final imported = f.importedVolitional.keys.toList();
      for (var j = 0; j < imported.length; j++) {
        final i = f.volitional.length + j;
        final t = f.interactive[i];
        final k = imported[j];
        final hash = k.lastIndexOf('#');
        final slash = k.lastIndexOf('/');
        final n = int.parse(k.substring(slash + 1));
        final target = _target(
            s,
            ast.RemoteGoal(
                ast.ConstTerm(k.substring(0, hash), 0, 0),
                ast.Goal(k.substring(hash + 1, slash),
                    [for (var a = 0; a < n; a++) ast.UnderscoreTerm(0, 0)],
                    0, 0),
                0,
                0));
        final exporter = target == null ? null : fronts[target.$1];
        final at = exporter == null
            ? -1
            : exporter.volitional.keys.toList().indexOf(target!.$2);
        if (at >= 0) _declaredIn[(s, i)] = target!.$1;
        out[(s, i)] = at >= 0
            ? out[(target!.$1, at)]!
            : questionFunctorStemBy(t.type, t.readerMode,
                (name) => _nameInFunctor(s, name, t.params));
      }
    }
    return out;
  }

  /// The name [name] as it enters a functor of a question the source at
  /// [source] declares, a type parameter of its procedure among [params]:
  /// in lowercase, and, where two modules of the program define types of
  /// that name ([_definers]), prefixed with the module that defines it in
  /// the source's scope ([definerOf], [modulePrefix]).  The root has no
  /// name to prefix with, and a clash with it is refused.
  String _nameInFunctor(String source, String name, List<String> params) {
    final lower = '${name[0].toLowerCase()}${name.substring(1)}';
    if (params.contains(name) || _definers(name).length < 2) return lower;
    final d = definerOf(source, name);
    if (d == null || d == programsDir) {
      throw CompileError(
          '$source asks a question whose type names $name, which two modules '
          'of the program define, and the module that defines it here is '
          '${d == null ? 'none of the program\'s scope chain' : 'the root'}: '
          'the name "is prefixed with the path of its module" (vGLP, '
          'sections/parts/canonical-compilation.tex), and that module has no '
          'path to prefix it with',
          1, 1, phase: 'analyzer');
    }
    return '${modulePrefix(d)}_$lower';
  }

  final Map<String, Set<String?>> _definersOf = {};

  /// The source that declares the volitional procedure asking each
  /// interactive type of the program, by the source and the type's place in
  /// its interactive types ([_functorStems]): the source's own, or the
  /// exporter of one it imports.
  final Map<(String, int), String> _declaredIn = {};

  /// The widget declarations in scope at the source [source], by moded
  /// type: its scope's, from the self.vglp beside each self.glp from the
  /// root down ([scopeWidgetDeclarations]), and its own, which override
  /// them; a self.vglp's own are its directory's, and so its scope's.
  Map<String, String> _widgetsIn(String source) => {
        ...scopeWidgetDeclarations(source, programsDir!),
        if (!layers.contains(source))
          ...extractWidgetDeclarations(File(source).readAsStringSync())
              .byModedType,
      };

  /// The modules of the program that define a type of the name [name] that
  /// enters a functor: for each question a source of the program declares
  /// whose type names it, its own or a type argument's, the module whose
  /// definition it denotes in that source's scope ([definerOf]).  A module
  /// whose definition of the name enters no question's type --- a self.glp
  /// writing a question's types again for the declarations of the
  /// procedures it imports --- adds no definer, so a type of one definition
  /// gives one functor (vGLP-mod3, Code #9's subagent's report).
  Set<String?> _definers(String name) => _definersOf.putIfAbsent(name, () {
        final out = <String?>{};
        for (final s in [...layers, ...modules]) {
          final f = fronts[s]!;
          for (var i = 0; i < f.volitional.length; i++) {
            final t = f.interactive[i];
            if (typeNamesOf(t.type).contains(name) &&
                !t.params.contains(name)) {
              out.add(definerOf(s, name));
            }
          }
        }
        return out;
      });

  /// The module whose definition the type name [name] denotes in the scope
  /// of the source at [source], as a key: the source itself, by its path,
  /// where it defines it; a directory, by its path, where the module of its
  /// self.glp and its layer defines it, the root's among them; null where no
  /// module of the source's chain defines it.  A directory's self.glp and
  /// the self.vglp beside it are one module (vGLP, sections/parts/canonical-
  /// compilation.tex: "as its self.glp holds those of GLP").
  String? definerOf(String source, String name) {
    final key = _norm(source);
    final isLayer = layers.contains(key);
    if (fronts[key]?.types.containsKey(name) ?? false) {
      return isLayer ? ppath.dirname(key) : key;
    }
    for (final d in _directoriesFrom(ppath.dirname(key))) {
      final layer = _norm(ppath.join(d, selfVglp));
      if (layer != key &&
          layers.contains(layer) &&
          fronts[layer]!.types.containsKey(name)) {
        return d;
      }
      if (_selfGlpTypes(d).contains(name)) return d;
    }
    return null;
  }

  /// The name of the module [definerOf] gives as [key]: a module file's,
  /// N for N.vglp, as a call N # p names it; a directory's, its own name, as
  /// a call of its parent names it.
  static String moduleNameOf(String key) => key.endsWith('.vglp')
      ? ppath.basenameWithoutExtension(key)
      : ppath.basename(key);

  /// The prefix a name of the module [key] ([definerOf]) takes in a functor.
  /// The paper's is "the path of its module, the names of its directories
  /// and its file joined by _" (vGLP ed75782); the base of that path, the
  /// program's root or the device's, is open (vGLP-mod3, Code #9's subagent's
  /// report), and the loader gives this compilation no program root, so the
  /// prefix stays the module's name, the last segment of its path, until it
  /// is decided.
  static String modulePrefix(String key) => moduleNameOf(key);

  final Map<String, Set<String>> _selfGlpTypeNames = {};

  /// The types the self.glp of [dir] defines.
  Set<String> _selfGlpTypes(String dir) =>
      _selfGlpTypeNames.putIfAbsent(dir, () {
        final f = File(ppath.join(dir, 'self.glp'));
        if (!f.existsSync()) return const {};
        final m = Parser(Lexer(f.readAsStringSync()).tokenize()).parseModule();
        return {for (final td in m.typeDefs) td.name};
      });
}
