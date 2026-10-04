/// Module hierarchy: self.glp chain discovery and type scope assembly.
///
/// Implements GLP module scoping per docs/modules/glp-module-system-spec.md:
/// - Directory-based hierarchy (Section 2)
/// - Implicit ancestor scoping (Section 3.1)
/// - Shadowing (Section 3.2)
/// - Sibling isolation (Section 3.3)
///
/// Specification: docs/modules/glp-module-system-spec.md Sections 2-3

import 'dart:io';
import 'package:path/path.dart' as ppath;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/param_expansion.dart';

/// The name of the module held by the file at [filePath]: its path from the
/// root [rootDir] (TGLP modules.tex, Compilation, third step: every procedure
/// and type is renamed `M:p/n` and `M:T`, "where M is the module's path from
/// the root").  A directory's `self.glp` is named by the directory's path, any
/// other module by its directory's path and its file name, the extension
/// dropped: `secure/app.glp` is `secure/app`, `secure/self.glp` is `secure`.
/// Segments are joined by `/`; the root's own `self.glp` is the empty path.
/// No file outside the root is named: a program there is refused before any
/// module of it is ([requireUnderRoot]).
String modulePathName(String filePath, String rootDir) {
  final file = ppath.normalize(File(filePath).absolute.path);
  final root = ppath.normalize(Directory(rootDir).absolute.path);
  final target = ppath.basename(file) == 'self.glp'
      ? ppath.dirname(file)
      : ppath.withoutExtension(file);
  final rel = ppath.relative(target, from: root);
  if (rel == '.') return '';
  return ppath.split(rel).join('/');
}

/// Whether [path], a file or a directory, lies at or below the directory
/// [root]: is [root] or lies under it, compared on normalised absolute paths.
bool liesAtOrBelow(String path, String root) {
  final p = ppath.normalize(File(path).absolute.path);
  final r = ppath.normalize(Directory(root).absolute.path);
  return ppath.equals(p, r) || ppath.isWithin(r, p);
}

/// The refusal of a program that does not lie at or below the root, or of a
/// source compiled in a scope of the root that does not ([requireUnderRoot]).
class OutsideRootError implements Exception {
  /// The program, or the source, as it was given.
  final String path;

  /// The root: the directory of the root `self.glp`.
  final String root;

  OutsideRootError(this.path, this.root);

  @override
  String toString() =>
      'Refused: $path lies outside the root $root.  "A device nominates one '
      'directory as its root, and every compilation on that device is under '
      'that root.  A program lies at or below the root, and the scope of each '
      'of its modules runs from the root down to that module" (TGLP '
      'modules.tex, "Scope construction").  Move it under $root.';
}

/// Refuse [path] --- a program's directory or file, or a source compiled in
/// the scope of the root --- unless it lies at or below [root], the directory
/// of the root `self.glp` (TGLP modules.tex, "Scope construction": "every
/// compilation on that device is under that root.  A program lies at or below
/// the root"; Definition "Root, Scope" defines the scope of a module M under
/// the root r only where r is dir(M) or an ancestor of it).  Until 2026-10-04
/// such a program was compiled in a scope that lacked its own directory's
/// `self.glp` and the root's exposes, [discoverSelfChain] stopping at once
/// outside the root.
void requireUnderRoot(String path, String root) {
  if (!liesAtOrBelow(path, root)) {
    throw OutsideRootError(
        path, ppath.normalize(Directory(root).absolute.path));
  }
}

/// The path from the root of the directory holding the module named [name]:
/// the module itself where it is a `self.glp` ([isSelfGlp]), otherwise its
/// name without the last segment.  The root is the empty path.
String moduleDirectoryName(String name, {required bool isSelfGlp}) {
  if (isSelfGlp) return name;
  final slash = name.lastIndexOf('/');
  return slash < 0 ? '' : name.substring(0, slash);
}

/// Discover the self.glp chain from root to target file's directory.
///
/// Walks up from the target file's directory to the root directory,
/// collecting self.glp files at each level. Returns them in root-first
/// order (outermost ancestor first, innermost last).
///
/// If the target file IS self.glp, the chain includes only ancestors
/// above it (not itself — the module's own definitions come from parsing it).
///
/// [targetFile]: absolute path to the .glp file being compiled
/// [rootDir]: absolute path to the project root directory
/// [programsDir]: absolute path to the root `programs/` directory. When given,
///   the chain extends ABOVE the project root up to (but excluding) this
///   directory — so intermediate ancestor `self.glp` files between the load
///   point and `programs/` are included. The root `programs/self.glp` itself is
///   NOT collected here (it is realised by the root-scope mechanism). When null,
///   the legacy bound applies: the walk stops at `rootDir` (inclusive).  A
///   target outside [programsDir] has an empty chain here; the program it
///   belongs to is refused before its chain is asked for ([requireUnderRoot]).
///
/// Returns: list of absolute paths to self.glp files, root-first order
List<String> discoverSelfChain({
  required String targetFile,
  required String rootDir,
  String? programsDir,
}) {
  // Normalize paths
  final root = Directory(rootDir).absolute.path;
  final target = File(targetFile).absolute.path;
  final targetName = target.split(Platform.pathSeparator).last;

  // Determine the starting directory for the walk.
  // If the target IS self.glp, start from its parent (don't include itself).
  // Otherwise, start from the target's directory.
  String startDir;
  if (targetName == 'self.glp') {
    // Target is self.glp — start from its parent directory
    startDir = File(target).parent.parent.path;
  } else {
    // Target is a regular module — start from its directory
    startDir = File(target).parent.path;
  }

  // Normalize a path for comparison: make absolute, resolve `..`/`.` segments
  // lexically (callers may pass paths containing `..`, e.g. `GLP/test/..`), and
  // strip any trailing slash.
  String norm(String p) {
    var n = ppath.normalize(Directory(p).absolute.path);
    if (n.endsWith('/')) n = n.substring(0, n.length - 1);
    return n;
  }

  final rootNorm = norm(root);
  final programsNorm = programsDir != null ? norm(programsDir) : null;

  // Walk from startDir upward, collecting self.glp files at each level.
  final chain = <String>[];
  var currentDir = startDir;

  while (true) {
    final currentNorm = norm(currentDir);

    // A directory is inside a bound when it lies under it, by path segments:
    // until 2026-10-04 this was a string prefix test, under which a sibling
    // `programs_x` of `programs` lay inside it.
    if (programsNorm != null) {
      // Extended bound: stop at programsDir WITHOUT collecting its self.glp,
      // and never walk above it.
      if (currentNorm == programsNorm) break;
      if (!ppath.isWithin(programsNorm, currentNorm)) break;
    } else {
      // Legacy bound: stop once we have gone above rootDir.
      if (currentNorm != rootNorm && !ppath.isWithin(rootNorm, currentNorm)) {
        break;
      }
    }

    final selfGlp = File('$currentDir${Platform.pathSeparator}self.glp');
    if (selfGlp.existsSync()) {
      chain.add(selfGlp.absolute.path);
    }

    // Legacy inclusive stop at rootDir (only when not extending to programsDir).
    if (programsNorm == null && currentNorm == rootNorm) break;

    final parent = Directory(currentDir).parent.path;
    if (parent == currentDir) break; // filesystem root safety
    currentDir = parent;
  }

  // Reverse: collected target-to-outermost, but callers want outermost-first.
  return chain.reversed.toList();
}

/// Merge a parsed module's types and declarations into a scope environment.
///
/// The single scope-merge primitive (modules.tex §Scope construction: later
/// definitions shadow earlier ones). Extracts the module's parameterized
/// templates before expansion removes them, so descendant scopes can expand
/// references to them; expands the module's own parameterized types against
/// the accumulated environment (known type names are not mistaken for type
/// parameters); merges with shadowing.
/// [typesFillGapsOnly] keeps the module's type definitions from shadowing what
/// [env] already defines: they fill gaps and nothing more. This is the rule for
/// a module of a DESCENDANT directory in the goal-check environment — a goal is
/// posted to the program's entry points and is therefore checked in the program
/// ROOT's scope, so a deeper `self.glp`'s redefinition of one of its type names
/// is that subtree's business and not the goal's. (Reported by Currencies Code,
/// 2026-09-03, against the linked program; the goal check took the deeper
/// definition the same way.)
///
/// [label] is the scope the module's types are recorded as defined in
/// ([TypeEnvironment.typeOrigins]): the prefix a definition is kept under
/// once a nearer scope defines its name, in either direction of shadowing.
TypeEnvironment mergeModuleIntoScope(TypeEnvironment env, ast.Module module,
    {bool typesFillGapsOnly = false, String? label}) {
  final templates = <String, TypeDef>{};
  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      templates[td.name] = td;
    }
  }
  final expanded = expandParameterizedTypes(module,
      knownTypeNames: env.types.keys.toSet(),
      externalTemplates: env.typeTemplates);
  final moduleEnv = buildScopeFromModule(expanded);
  // The layer's clauses join the scope's, by key: a defined guard of the
  // layer is unfolded, and its parameterised procedures certified, by them
  // ([TypeEnvironment.scopeLayers]).
  final clauses = <String, List<ast.Clause>>{};
  for (final proc in module.procedures) {
    for (final c in proc.clauses) {
      clauses
          .putIfAbsent('${c.head.functor}/${c.head.arity}', () => [])
          .add(c);
    }
  }
  final scopeLayer = ScopeLayer(clauses);
  final layer = TypeEnvironment(moduleEnv.types, moduleEnv.procedures,
      paramProcDecls: moduleEnv.paramProcDecls,
      typeTemplates: templates,
      scopeLayers: {for (final k in clauses.keys) k: scopeLayer});
  final merged =
      _mergeLayer(env, layer, typesFillGapsOnly: typesFillGapsOnly, label: label);
  // The scope the layer's procedures were declared in.
  scopeLayer.env = merged;
  return merged;
}

/// [layer], a module's environment, merged into [env] ([mergeModuleIntoScope]).
TypeEnvironment _mergeLayer(TypeEnvironment env, TypeEnvironment layer,
    {bool typesFillGapsOnly = false, String? label}) {
  if (!typesFillGapsOnly) {
    // Innermost-first shadowing, as [buildTypeEnvironment] applies it to a
    // module's own scope (modules.tex, "Scope construction": later
    // definitions shadow earlier): a procedure the module declares
    // monomorphic shadows an inherited parameterised template of the same
    // key, which therefore does not survive in paramProcDecls, else a call
    // the module's declaration types is read as a call to the template.
    // Until 2026-10-02 the template survived here, beside the module's
    // declaration, and a goal posted to book's merge_ordered.glp,
    // merge([1,3,5], [2,4,6], Zop), was read as a call to the root's
    // procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
    final merged = env.merge(layer, label: label);
    final shadowed = {
      for (final key in layer.procedures.keys)
        if (!layer.paramProcDecls.containsKey(key) &&
            merged.paramProcDecls.containsKey(key))
          key
    };
    if (shadowed.isEmpty) return merged;
    return TypeEnvironment(merged.types, merged.procedures,
        paramProcDecls: {
          for (final e in merged.paramProcDecls.entries)
            if (!shadowed.contains(e.key)) e.key: e.value
        },
        typeTemplates: merged.typeTemplates,
        typeOrigins: merged.typeOrigins,
        scopeLayers: merged.scopeLayers);
  }
  // The module under the scope rather than over it: its types and its
  // declarations fill gaps, and a type the scope already defines survives
  // from the module under `<label>:T`, the module's own declarations meaning
  // it (TypeEnvironment.shadowedBy) --- a declaration's types are those of
  // the scope it was declared in, whichever way the shadowing runs. Its
  // declarations fill gaps for the same reason its types do: "the procedures
  // that may be posted are exactly the entry points" (TGLP "Compilation",
  // entry and the absence of a boot module), the program root's exports, so
  // a descendant's procedure of the same name and arity as one in the root's
  // scope is not what a goal names. Until 2026-09-18 it shadowed the root's
  // declaration while its types were read as the root's, which passed a goal
  // against a declaration of the same shape and would have rejected any other.
  final under = layer.shadowedBy(env, ownLabel: label);
  // The scope's own declaration of a key shadows the module's, so a template
  // the module carries for a key the scope declares monomorphic does not
  // survive either.
  return TypeEnvironment(
      {...under.types, ...env.types},
      {...under.procedures, ...env.procedures},
      paramProcDecls: {
        for (final e in under.paramProcDecls.entries)
          if (!env.procedures.containsKey(e.key) ||
              env.paramProcDecls.containsKey(e.key))
            e.key: e.value,
        ...env.paramProcDecls
      },
      typeTemplates: {...under.typeTemplates, ...env.typeTemplates},
      typeOrigins: {...under.originsUnder(label), ...env.typeOrigins},
      scopeLayers: {...layer.scopeLayers, ...env.scopeLayers});
}

/// [env] with the entry points [keys] of a loaded program merged in, as a
/// layer of their own: each entry point's declaration as [declaringScope],
/// the scope it was declared in, holds it, and the transitive closure of the
/// types its signature references --- "A declaration carries the transitive
/// closure of the types its signature references, so types are not exported
/// separately" (TGLP modules.tex, "Procedure declarations") --- and no other
/// declaration or type of that scope.  An entry point's clauses come with it,
/// as their layer of [declaringScope] holds them, so that its parametricity is
/// decided in the scope it was declared in ([TypeEnvironment.scopeLayers]).
///
/// The scope a boot source is checked in is the boot file's ancestor chain
/// with the linked program's entry points merged in by this: "the linked
/// program's entry points and the boot file's ancestor chain of self.glp
/// declarations, the root among them ... A call to a procedure the program
/// does not export is refused by the check" (IGLP, Implementation Notes, "The
/// scope a boot source is checked in", 8aafd09).  [label] is the scope the
/// layer's types are recorded as defined in where [declaringScope] records
/// none.
TypeEnvironment mergeEntryPointsIntoScope(TypeEnvironment env,
    TypeEnvironment declaringScope, Iterable<String> keys,
    {String? label}) {
  final carried = _carriedDeclarations(declaringScope, keys);
  final layers = <String, ScopeLayer>{
    for (final key in keys)
      if (declaringScope.scopeLayers[key] != null &&
          (carried.procedures.containsKey(key) ||
              carried.paramProcDecls.containsKey(key)))
        key: declaringScope.scopeLayers[key]!
  };
  final layer = TypeEnvironment(carried.types, carried.procedures,
      paramProcDecls: carried.paramProcDecls,
      typeTemplates: carried.typeTemplates,
      typeOrigins: carried.typeOrigins,
      scopeLayers: layers);
  return _mergeLayer(env, layer, label: label);
}

/// The declarations [keys] of [declaringScope] and the transitive closure of
/// the types their signatures reference, with nothing else of the scope: "A
/// declaration carries the transitive closure of the types its signature
/// references, so types are not exported separately" (TGLP modules.tex,
/// "Procedure declarations").  The closure takes a monomorphic type, an
/// expansion instance `T<A>` and the template `T` it instantiates, and a
/// template a parameterised declaration names; a primitive type or a
/// declaration's own parameter is in neither.  Each type keeps the scope
/// [declaringScope] records it as defined in ([TypeEnvironment.typeOrigins]).
/// No clause layer is carried.
TypeEnvironment _carriedDeclarations(
    TypeEnvironment declaringScope, Iterable<String> keys) {
  final procedures = <String, ProcDecl>{};
  final paramProcDecls = <String, ProcDecl>{};
  final pending = <String>[];
  void collect(TypeExpr t) {
    if (t is TypeRef) {
      pending.add(t.name);
      t.typeArgs.forEach(collect);
    } else if (t is StructAlt) {
      t.args.forEach(collect);
    } else if (t is ListConsAlt) {
      collect(t.head);
      collect(t.tail);
    } else if (t is DiffListAlt) {
      collect(t.content);
      collect(t.hole);
    }
    // ConstantAlt, ListNilAlt and PrimitiveModeAlt name no type.
  }

  for (final key in keys) {
    final mono = declaringScope.procedures[key];
    final param = declaringScope.paramProcDecls[key];
    if (mono != null) {
      procedures[key] = mono;
      mono.argTypes.forEach(collect);
    }
    if (param != null) {
      paramProcDecls[key] = param;
      param.argTypes.forEach(collect);
    }
  }

  final types = <String, TypeDef>{};
  final templates = <String, TypeDef>{};
  while (pending.isNotEmpty) {
    final name = pending.removeLast();
    final td = declaringScope.types[name];
    if (td != null && !types.containsKey(name)) {
      types[name] = td;
      td.alternatives.forEach(collect);
    }
    final lt = name.indexOf('<');
    final templateName = lt < 0 ? name : name.substring(0, lt);
    final tt = declaringScope.typeTemplates[templateName];
    if (tt != null && !templates.containsKey(templateName)) {
      templates[templateName] = tt;
      tt.alternatives.forEach(collect);
    }
  }

  return TypeEnvironment(types, procedures,
      paramProcDecls: paramProcDecls,
      typeTemplates: templates,
      typeOrigins: {
        for (final t in types.keys)
          if (declaringScope.typeOrigins[t] != null)
            t: declaringScope.typeOrigins[t]!
      });
}

/// Merge a self.glp file into a scope environment: parse, then
/// [mergeModuleIntoScope], labelled by the directory the `self.glp` is the
/// scope of --- the name the linker gives that module, its path from [root]
/// ([modulePathName]); with no [root] given, the directory's last segment.
///
/// The types the `self.glp`'s `-expose` directives lift are in [env] before
/// the `self.glp` is merged ([liftExposedTypes]), so its own declarations and
/// every later layer resolve them.
TypeEnvironment mergeSelfGlpFileIntoScope(TypeEnvironment env, String path,
    {String? root, String? label}) {
  final source = File(path).readAsStringSync();
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final lifted = liftExposedTypes(env, module, path, root: root);
  try {
    return mergeModuleIntoScope(lifted, module,
        label: label ??
            (root != null ? modulePathName(path, root) : _directoryLabel(path)));
  } on UndefinedDeclarationTypeError catch (e) {
    // The declaration is the self.glp's: the error names its file.
    throw e.inFile(path);
  }
}

/// [env] with the types that [exposer]'s `-expose` directives lift into the
/// scope of its directory, [exposer] being the module of the file at
/// [exposerPath] and [env] the scope of that directory's ancestors.
///
/// TGLP modules.tex, "The -expose directive": `-expose(M).` "lifts the
/// exported procedures of module M (and the types their signatures carry) into
/// that directory's scope, as if defined in its self.glp"; what one directive
/// lifts is an [ExposedLift].  A scope is layered one `self.glp` at a time
/// (Definition (Root, Scope)), and a layer's declarations, and those of every
/// layer after it, are expanded against the types known when it is merged; so
/// a type an ancestor exposes must be known then, or a declaration naming it is
/// refused as naming an undefined type, or, naming no parameters, reads it as
/// one (programs/tests/expose/root_types_decl) --- and so must a type the
/// directory's own `self.glp` exposes, which its own declarations may name
/// (programs/tests/expose/names_lifted).
///
/// Only the types are lifted here: the exposed procedures, their collisions
/// and their entry-point status are the linker's (program_linker.dart,
/// `_resolveExposes`), which lifts them into each module of the exposing
/// subtree from the same [exposedLifts].  A lifted type fills a gap: a type
/// [env] already defines keeps its name, and a different lifted type of that
/// name is kept under `<origin>:T` ([TypeEnvironment.shadowedBy]), the lifted
/// types that reference it rewritten to that name.  A module path whose file
/// is missing lifts nothing here; the linker reports it.  Each lifted type is
/// recorded ([TypeEnvironment.typeOrigins]) as defined in the scope that
/// defines it, its path from [root] ([modulePathName]).
TypeEnvironment liftExposedTypes(
        TypeEnvironment env, ast.Module exposer, String exposerPath,
        {String? root}) =>
    _liftExposedTypes(env, exposer, exposerPath, root: root).scope;

/// What one `-expose(M).` lifts into the scope of the directory whose
/// `self.glp` carries it.
///
/// TGLP modules.tex, "The -expose directive": it "lifts the exported procedures
/// of module M (and the types their signatures carry) into that directory's
/// scope, as if defined in its self.glp"; and "Procedure declarations": "A
/// declaration carries the transitive closure of the types its signature
/// references, so types are not exported separately".  So the lift is M's
/// exported declarations and that closure, and no other type of M.  A
/// declaration's types are those of the scope it is declared in, M's own,
/// `Σ_r(M)` of Definition (Root, Scope) --- the root, the `self.glp` of every
/// directory from the root down to M's, and M --- so the signature is read
/// there: a type it names resolves where M's own clauses resolve it, in M, in a
/// `self.glp` between the exposing directory and M, in the exposing `self.glp`
/// or above it, and not in the scope of a module receiving the lift.
///
/// Until 2026-10-04 the signature was read in the receiving module's scope
/// with the exposing `self.glp`'s definitions beside it, and every type M
/// defined was lifted: a type from a `self.glp` between the two was undefined
/// (the root's `-expose(social#graph#routing#intro)` and social/graph/self.glp;
/// programs/tests/expose/own_scope), and a type no signature carries was in
/// scope (programs/sglp/self.glp's `-expose(monitor)` and monitor.glp's Queue
/// and Clock; programs/tests/expose/unlifted).
class ExposedLift {
  /// M's exported declarations as read in `Σ_r(M)`, and the transitive closure
  /// of the types and templates their signatures reference, each type recorded
  /// ([TypeEnvironment.typeOrigins]) as defined in the scope that defines it.
  /// It carries no clause layer.
  final TypeEnvironment scope;

  /// The scope each template of [scope] was defined in, where that is the
  /// exposing `self.glp`, a `self.glp` below it on the way to M, or M.
  final Map<String, String> templateOrigins;

  /// The scope M's own definitions are recorded as defined in.
  final String label;

  ExposedLift(this.scope, this.templateOrigins, this.label);

  /// The names of the types and templates carried.
  Set<String> get _names =>
      {...scope.types.keys, for (final t in scope.typeTemplates.keys) '$t()'};
}

/// The lifts of [exposer]'s `-expose` directives, one per directive in their
/// order, null where the module's file is missing or is being lifted already
/// ([lifting], by file path); [exposer] is the module of the file at
/// [exposerPath], and [above] the scope of its directory's ancestors, the scope
/// it is layered over.  Labels are paths from [root] ([modulePathName]).
///
/// Each lift reads its module's signatures in `Σ_r(M)` ([ExposedLift]), built
/// on [above]: [exposer], each `self.glp` from the directory below its own down
/// to M's, each with the types its own `-expose` directives lift, and M.  The
/// `self.glp` layers enter with their type definitions alone: a signature names
/// types and no procedure, and a layer's declarations may name a type that M's
/// own lift carries to it, which is M's definition and is in `Σ_r(M)` from M's
/// layer (programs/tests/expose/names_lifted).  For the same reason a module's
/// lift is not taken again inside its own `Σ_r(M)`.  [exposer]'s layer in
/// `Σ_r(M)` carries what its other directives lift, as it does in every scope
/// below it; so its lifts are computed together, by passes from none, each
/// pass reading every signature over the previous pass's lifts of the others,
/// until no lift changes.
List<ExposedLift?> exposedLifts(
    ast.Module exposer, String exposerPath, TypeEnvironment above,
    {String? root, Set<String> lifting = const {}}) {
  final exposerDir = File(exposerPath).parent.path;
  final targets = <int, (String, ast.Module)>{};
  final seen = <String>{};
  for (var i = 0; i < exposer.exposes.length; i++) {
    final file = File(
        '${ppath.joinAll([exposerDir, ...exposer.exposes[i].split('#')])}.glp');
    if (!file.existsSync()) continue;
    final norm = _normFile(file.path);
    if (lifting.contains(norm) || !seen.add(norm)) continue;
    targets[i] = (
      file.path,
      Parser(Lexer(file.readAsStringSync()).tokenize()).parseModule()
    );
  }

  var lifts = <int, ExposedLift>{};
  for (var pass = 0; pass <= targets.length; pass++) {
    final next = <int, ExposedLift>{
      for (final e in targets.entries)
        e.key: _exposedLift(e.value.$1, e.value.$2, exposerPath, exposer, above,
            siblings: [
              for (final s in lifts.entries)
                if (s.key != e.key) s.value
            ],
            root: root,
            lifting: lifting),
    };
    final stable = targets.length < 2 ||
        (pass > 0 &&
            next.entries.every((e) =>
                _sameNames(e.value._names, lifts[e.key]!._names)));
    lifts = next;
    if (stable) break;
  }
  return [for (var i = 0; i < exposer.exposes.length; i++) lifts[i]];
}

bool _sameNames(Set<String> a, Set<String> b) =>
    a.length == b.length && a.containsAll(b);

/// The lift of the module [exposed], the file at [exposedPath], by the
/// `-expose` of [exposer], the file at [exposerPath] ([exposedLifts]); [above]
/// is the scope of the exposing directory's ancestors, and [siblings] what
/// [exposer]'s other directives lift.
ExposedLift _exposedLift(String exposedPath, ast.Module exposed,
    String exposerPath, ast.Module exposer, TypeEnvironment above,
    {required List<ExposedLift> siblings,
    String? root,
    Set<String> lifting = const {}}) {
  final inProgress = {...lifting, _normFile(exposedPath)};
  final templateOrigins = <String, String>{};
  ast.Module typesOnly(ast.Module m) => ast.Module(
      typeDefs: m.typeDefs, exposes: m.exposes, line: m.line, column: m.column);
  void definedIn(ast.Module m, String label) {
    for (final td in m.typeDefs) {
      if (td.isParameterized) templateOrigins[td.name] = label;
    }
  }

  // The exposing self.glp's layer, over what its other directives lift.
  var env = above;
  for (final s in siblings) {
    env = _withLiftedTypes(env, s);
    templateOrigins.addAll(s.templateOrigins);
  }
  final exposerLabel = _scopeLabel(exposerPath, root);
  env = mergeModuleIntoScope(env, typesOnly(exposer), label: exposerLabel);
  definedIn(exposer, exposerLabel);

  // Each self.glp below it on the way to M, over what its directives lift.
  for (final path in _selfGlpsBelow(
      File(exposerPath).parent.path, File(exposedPath).parent.path)) {
    final self = typesOnly(
        Parser(Lexer(File(path).readAsStringSync()).tokenize()).parseModule());
    final lifted =
        _liftExposedTypes(env, self, path, root: root, lifting: inProgress);
    templateOrigins.addAll(lifted.templateOrigins);
    final label = _scopeLabel(path, root);
    env = mergeModuleIntoScope(lifted.scope, self, label: label);
    definedIn(self, label);
  }

  // M, its type definitions and its exported declarations.
  final exported = exposed.procDeclarations.where((d) => d.exported).toList();
  final label = _scopeLabel(exposedPath, root);
  try {
    env = mergeModuleIntoScope(
        env,
        ast.Module(
            typeDefs: exposed.typeDefs,
            procDeclarations: exported,
            line: exposed.line,
            column: exposed.column),
        label: label);
  } on UndefinedDeclarationTypeError catch (e) {
    throw e.inFile(exposedPath);
  }
  definedIn(exposed, label);

  final carried =
      _carriedDeclarations(env, [for (final d in exported) d.qualifiedKey]);
  return ExposedLift(
      carried,
      {
        for (final t in carried.typeTemplates.keys)
          if (templateOrigins[t] != null) t: templateOrigins[t]!
      },
      label);
}

/// [liftExposedTypes], with the template origins of what it lifted, and the
/// modules being lifted already, [lifting], not lifted again ([exposedLifts]).
({TypeEnvironment scope, Map<String, String> templateOrigins})
    _liftExposedTypes(TypeEnvironment env, ast.Module exposer,
        String exposerPath,
        {String? root, Set<String> lifting = const {}}) {
  if (exposer.exposes.isEmpty) {
    return (scope: env, templateOrigins: const <String, String>{});
  }
  var acc = env;
  final templateOrigins = <String, String>{};
  for (final lift in exposedLifts(exposer, exposerPath, env,
      root: root, lifting: lifting)) {
    if (lift == null) continue;
    acc = _withLiftedTypes(acc, lift);
    templateOrigins.addAll(lift.templateOrigins);
  }
  return (scope: acc, templateOrigins: templateOrigins);
}

/// [env] with the types and templates of [lift] filling its gaps: a lifted
/// type [env] defines differently is kept under `<origin>:T`
/// ([TypeEnvironment.shadowedBy]); the lifted declarations are not added.
TypeEnvironment _withLiftedTypes(TypeEnvironment env, ExposedLift lift) {
  final ex = lift.scope.shadowedBy(env, ownLabel: lift.label);
  final types = <String, TypeDef>{
    for (final e in ex.types.entries)
      if (!env.types.containsKey(e.key)) e.key: e.value,
  };
  final templates = <String, TypeDef>{
    for (final e in ex.typeTemplates.entries)
      if (!env.typeTemplates.containsKey(e.key)) e.key: e.value,
  };
  if (types.isEmpty && templates.isEmpty) return env;
  return TypeEnvironment({...env.types, ...types}, env.procedures,
      paramProcDecls: env.paramProcDecls,
      typeTemplates: {...env.typeTemplates, ...templates},
      typeOrigins: {
        ...env.typeOrigins,
        for (final t in types.keys) t: ex.typeOrigins[t] ?? lift.label
      },
      scopeLayers: env.scopeLayers);
}

/// The `self.glp` files of the directories below [fromDir] down to [toDir],
/// [toDir] included and [fromDir] not, outermost first; none where [toDir] is
/// not below [fromDir].
List<String> _selfGlpsBelow(String fromDir, String toDir) {
  final from = ppath.normalize(Directory(fromDir).absolute.path);
  final rel = ppath.relative(ppath.normalize(Directory(toDir).absolute.path),
      from: from);
  if (rel == '.' || rel.startsWith('..')) return const [];
  final files = <String>[];
  var dir = from;
  for (final segment in ppath.split(rel)) {
    dir = ppath.join(dir, segment);
    final self = File(ppath.join(dir, 'self.glp'));
    if (self.existsSync()) files.add(self.path);
  }
  return files;
}

/// The scope a module's definitions are recorded as defined in: its path from
/// [root] ([modulePathName]); with no [root], a `self.glp`'s directory's last
/// segment and any other module's file name.
String _scopeLabel(String path, String? root) => root != null
    ? modulePathName(path, root)
    : (ppath.basename(path) == 'self.glp'
        ? _directoryLabel(path)
        : ppath.basenameWithoutExtension(path));

/// [path] absolute and normalised.
String _normFile(String path) => ppath.normalize(File(path).absolute.path);

/// The last segment of the directory holding [path].
String _directoryLabel(String path) {
  final dir = File(path).absolute.parent.path;
  final parts = dir.split(Platform.pathSeparator).where((p) => p.isNotEmpty);
  return parts.isEmpty ? dir : parts.last;
}

/// The language primitives, `Π` of TGLP Definition "Root, Scope": the base of
/// every scope, for every module of every program, and not an ancestor
/// directory (modules.tex, "Two things are named self.glp and they enter a
/// scope by different routes").  By GLP-Spec appendix-guards.tex, "Predefined
/// types", they are the primitive types `Integer`, `Real`, `String`, `Module`
/// and `MutualRef`, which the checker builds into every automaton
/// (analysis/type_checker/program_dfa.dart), and the kernels and builtin
/// guards the runtime implements (root_scope.dart, `builtinProcedures`), whose
/// declarations the root self.glp states in GLP; everything else in the root
/// is GLP and reaches a scope by the chain (GLP #3 Cowork, 2026-10-03 21:18
/// UTC).  As an environment it therefore defines nothing: the empty scope.
TypeEnvironment primitiveScope() => TypeEnvironment.empty();

/// The scope a module directly under the root is checked in, `Π ⊔ d_1`, the
/// root self.glp at [rootSelfGlpPath] its one layer: what a module of no
/// further ancestor sees, a file-less source and a posted goal among them.
/// [primitiveScope] where there is no root.
TypeEnvironment rootScope(String? rootSelfGlpPath) =>
    buildAncestorScope(chain: const [], rootSelfGlpPath: rootSelfGlpPath);

/// Build the ancestor scope for a self.glp chain (root-first order).
///
/// TGLP Definition "Root, Scope": the scope begins with the language
/// primitives ([primitiveScope]); the root self.glp at [rootSelfGlpPath], d_1,
/// is layered next like any self.glp, under the empty path its renaming gives
/// it, and a chain entry equal to it is skipped; then each chain self.glp in
/// order, later shadowing earlier.  The target module itself is NOT merged
/// here — [assembleTypeScope] adds it; on the engine path checkModule adds it.
/// Until 2026-10-04 the base was a root-scope environment built from a source
/// the engine set once for the whole process.
///
/// This is the ONE implementation of ancestor-scope assembly, shared by the
/// linker, the engine's module check, and the engine's goal-check environment.
///
/// [rootScope] is `Π ⊔ d_1` built already ([rootScope] of this file) for the
/// same root: the scopes of one load, and of one engine, are built over it,
/// so the root's layer is one and its procedures are certified once
/// ([ScopeLayer]).  Each scope returned has maps of its own.
TypeEnvironment buildAncestorScope({
  required List<String> chain,
  String? rootSelfGlpPath,
  TypeEnvironment? rootScope,
}) {
  var env = primitiveScope();
  File? rootSelf;
  if (rootSelfGlpPath != null) {
    final f = File(rootSelfGlpPath);
    if (f.existsSync()) {
      rootSelf = f;
      env = rootScope != null
          ? rootScope.copy()
          : mergeSelfGlpFileIntoScope(env, f.path,
              root: f.parent.path, label: '');
    }
  }
  for (final selfGlpPath in chain) {
    if (rootSelf != null &&
        File(selfGlpPath).absolute.path == rootSelf.absolute.path) {
      continue;
    }
    env = mergeSelfGlpFileIntoScope(env, selfGlpPath,
        root: rootSelf?.parent.path);
  }
  return env;
}

/// Assemble the type scope for a module by layering ancestor definitions.
///
/// Builds a TypeEnvironment by:
/// 1. Starting with the language primitives and the root self.glp at
///    [rootSelfGlpPath], where one is given ([buildAncestorScope])
/// 2. Merging each self.glp in the chain (root-first, so children shadow parents)
/// 3. Merging the target module's own types and declarations (shadows all ancestors)
///
/// [chain]: list of self.glp file paths, root-first order (from discoverSelfChain)
/// [module]: the parsed Module AST of the target file
///
/// Returns: the assembled TypeEnvironment with all visible types and procedures
TypeEnvironment assembleTypeScope({
  required List<String> chain,
  required ast.Module module,
  String? rootSelfGlpPath,
}) {
  return mergeModuleIntoScope(
      buildAncestorScope(chain: chain, rootSelfGlpPath: rootSelfGlpPath),
      module);
}

/// Build a TypeEnvironment from a Module's types and procedure declarations.
///
/// Unlike _buildEnvironmentFromModule in type_environment_builder.dart,
/// this does NOT check for predefined type redefinition (because shadowing
/// ancestor types is allowed) and does NOT resolve aliases (that happens
/// after all scopes are assembled).
TypeEnvironment buildScopeFromModule(ast.Module module) {
  final types = <String, TypeDef>{};
  final procedures = <String, ProcDecl>{};
  final paramProcDecls = <String, ProcDecl>{};

  for (final typeDef in module.typeDefs) {
    types[typeDef.name] = typeDef;
  }

  for (final procDecl in module.procDeclarations) {
    procedures[procDecl.qualifiedKey] = procDecl;
  }

  for (final paramDecl in module.paramProcDecls) {
    paramProcDecls[paramDecl.qualifiedKey] = paramDecl;
  }

  return TypeEnvironment(types, procedures, paramProcDecls: paramProcDecls);
}
