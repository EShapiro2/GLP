// lib/analysis/type_checker/type_ast.dart
//
// AST nodes for GLP type declarations following Yardeni-Shapiro.
// Types are first-class syntactic elements parsed alongside clauses.

/// Classification of types by mode structure
/// Per spec (type-environment.md): Types are classified based on internal complementation
enum TypeClassification {
  output,      // No complementation in definition (pure output structure)
  input,       // Complement of an output type (not directly defined)
  interactive  // Contains internal complementation (_? or T? in alternatives)
}

/// Base class for type expressions
abstract class TypeExpr {
  final int line;
  final int column;

  TypeExpr(this.line, this.column);
}

/// Extension to extract common properties from TypeExpr subclasses
/// that can appear in procedure argument positions (TypeRef or PrimitiveModeAlt)
extension ProcArgTypeExpr on TypeExpr {
  /// Whether this type expression represents an input (consume) mode.
  /// - TypeRef with isInput=true (T?) → true
  /// - PrimitiveModeAlt with isInput=true (_?) → true
  /// - Otherwise → false
  bool get isInputMode {
    if (this is TypeRef) return (this as TypeRef).isInput;
    if (this is PrimitiveModeAlt) return (this as PrimitiveModeAlt).isInput;
    return false;
  }

  /// The type name for named types, or null for primitives.
  /// - TypeRef → the name
  /// - PrimitiveModeAlt → null
  String? get typeName {
    if (this is TypeRef) return (this as TypeRef).name;
    return null;
  }

  /// Whether this is a primitive type (_ or _?)
  bool get isPrimitive => this is PrimitiveModeAlt;
}

/// Reference to a named type: Nat, List, Number, String, Any
/// Optionally with input mode annotation (Type?)
/// Optionally with type arguments for parameterized types: Stream(Integer)
class TypeRef extends TypeExpr {
  final String name;
  final bool isInput;  // true if Type?, false if Type
  final List<TypeExpr> typeArgs;  // e.g., [TypeRef('Integer')] for Stream(Integer), [] for simple refs

  TypeRef(this.name, int line, int column, {this.isInput = false, this.typeArgs = const []})
      : super(line, column);

  bool get isParameterized => typeArgs.isNotEmpty;

  /// Mode dual operator per Definition 5.1
  /// Returns a new TypeRef with inverted mode
  TypeRef dual() => TypeRef(name, line, column, isInput: !isInput, typeArgs: typeArgs);

  @override
  String toString() {
    final argsStr = typeArgs.isNotEmpty ? '(${typeArgs.join(', ')})' : '';
    return isInput ? '$name$argsStr?' : '$name$argsStr';
  }

  /// The primitive types (not defined via ::=, implemented by the runtime).
  /// TGLP appendix "GLP Language Primitives": Integer, Real, String, Module.
  /// Number, Constant and Exp are NOT here — they are unions defined in the
  /// root self.glp.
  static const builtins = {'Integer', 'Real', 'String', 'Module'};

  /// System types (defined via ::= but not redefinable by user)
  static const systemTypes = {'Any', 'List'};

  bool get isBuiltin => builtins.contains(name);

  @override
  bool operator ==(Object other) =>
      other is TypeRef && other.name == name && other.isInput == isInput &&
      _listEquals(other.typeArgs, typeArgs);

  @override
  int get hashCode => Object.hash(name, isInput, Object.hashAll(typeArgs));

  static bool _listEquals(List<TypeExpr> a, List<TypeExpr> b) {
    if (a.length != b.length) return false;
    for (int i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
}

/// A constant alternative in a type: 0, [], foo
class ConstantAlt extends TypeExpr {
  final Object value;  // String (atom), int, or double
  
  ConstantAlt(this.value, int line, int column) : super(line, column);
  
  @override
  String toString() => value.toString();
}

/// A structure alternative: s(Nat), tree(Nat, Tree, Tree)
class StructAlt extends TypeExpr {
  final String functor;
  final List<TypeExpr> args;
  
  StructAlt(this.functor, this.args, int line, int column) : super(line, column);
  
  int get arity => args.length;
  
  @override
  String toString() => '$functor(${args.join(', ')})';
}

/// Empty list alternative: []
class ListNilAlt extends TypeExpr {
  ListNilAlt(int line, int column) : super(line, column);
  
  @override
  String toString() => '[]';
}

/// List cons alternative: [Head | Tail]
class ListConsAlt extends TypeExpr {
  final TypeExpr head;
  final TypeExpr tail;

  ListConsAlt(this.head, this.tail, int line, int column) : super(line, column);

  @override
  String toString() => '[$head | $tail]';
}

/// Primitive mode type alternative: _ (output) or _? (input)
/// Used in type definitions like: Any ::= _ ; _?.
class PrimitiveModeAlt extends TypeExpr {
  final bool isInput;  // false = _ (output), true = _? (input)

  PrimitiveModeAlt(this.isInput, int line, int column) : super(line, column);

  @override
  String toString() => isInput ? '_?' : '_';
}

/// Difference list alternative: List \ List?
/// Used for DiffList type: DiffList ::= List \ List?.
class DiffListAlt extends TypeExpr {
  final TypeExpr content;  // The content list
  final TypeExpr hole;     // The hole/tail

  DiffListAlt(this.content, this.hole, int line, int column) : super(line, column);

  @override
  String toString() => '$content \\ $hole';
}

/// A type definition: TypeName ::= alt1 ; alt2 ; ... .
/// Parameterized types have non-empty typeParams: Stream(X) ::= [] ; [X | Stream(X)].
class TypeDef {
  final String name;
  final List<String> typeParams;  // e.g., ['X'] for Stream(X), [] for monomorphic
  final List<TypeExpr> alternatives;
  final int line;
  final int column;

  TypeDef(this.name, this.alternatives, this.line, this.column, {this.typeParams = const []});

  bool get isParameterized => typeParams.isNotEmpty;

  /// Classify this type based on mode structure
  /// Per spec (type-environment.md v0.5):
  /// - output: no complementation in any alternative
  /// - interactive: contains internal complementation (_? or T?)
  TypeClassification get classification {
    for (final alt in alternatives) {
      if (_containsComplement(alt)) {
        return TypeClassification.interactive;
      }
    }
    return TypeClassification.output;
  }

  /// Check if a type expression contains any complementation
  static bool _containsComplement(TypeExpr expr) {
    if (expr is TypeRef && expr.isInput) return true;
    if (expr is PrimitiveModeAlt && expr.isInput) return true;

    if (expr is ListConsAlt) {
      return _containsComplement(expr.head) || _containsComplement(expr.tail);
    }
    if (expr is StructAlt) {
      return expr.args.any(_containsComplement);
    }
    if (expr is DiffListAlt) {
      return _containsComplement(expr.content) || _containsComplement(expr.hole);
    }

    return false;
  }

  @override
  String toString() => '$name ::= ${alternatives.join(' ; ')}.';
}

/// A procedure declaration: procedure name(Type1, Type2, ...).
///
/// Argument types can be:
/// - TypeRef: a named type reference (e.g., Nat, Stream?)
/// - PrimitiveModeAlt: a primitive type directly (e.g., _, _?)
///
/// Parameterized procedure declarations have non-empty typeParams:
///   procedure gethead(Stream(X)?, X).  → typeParams: ['X']
/// These are templates instantiated per call site by the type checker.
class ProcDecl {
  final String name;
  final List<TypeExpr> argTypes;  // TypeRef or PrimitiveModeAlt
  final List<String> typeParams;  // e.g., ['X'] for parameterized proc decls, [] for monomorphic
  final int line;
  final int column;
  final bool isBuiltin;  // True if implemented in Dart runtime (no GLP clauses)
  final bool exported;   // True if declared with 'exported procedure'
  final bool imported;   // True if declared with 'imported procedure'
  final String? modulePath;  // For imported procedures: module path (e.g., 'social' or 'ui#actors'), null for ancestor scope

  ProcDecl(this.name, this.argTypes, this.line, this.column, {this.typeParams = const [], this.isBuiltin = false, this.exported = false, this.imported = false, this.modulePath});

  bool get isParameterized => typeParams.isNotEmpty;

  int get arity => argTypes.length;

  String get key => '$name/$arity';

  /// Key for TypeEnvironment lookup, including module path for imported procedures.
  /// - Local/exported: 'factorial/2'
  /// - Imported with path: 'math#factorial/2'
  /// - Imported from ancestor (no path): 'factorial/2'
  String get qualifiedKey => '$qualifiedName/$arity';

  /// Get the mode for argument at index i (true = input mode)
  bool isInputArg(int i) {
    final arg = argTypes[i];
    if (arg is TypeRef) return arg.isInput;
    if (arg is PrimitiveModeAlt) return arg.isInput;
    return false;
  }

  /// Get the base type name for argument at index i
  /// Returns null for primitive types (_ or _?)
  String? getTypeName(int i) {
    final arg = argTypes[i];
    if (arg is TypeRef) return arg.name;
    return null;  // Primitive types have no name
  }

  /// The visibility prefix for this declaration
  String get _visibilityPrefix {
    if (exported) return 'exported ';
    if (imported) return 'imported ';
    return '';
  }

  /// The full qualified name (with module path for imported procedures)
  String get qualifiedName {
    if (modulePath != null) return '$modulePath#$name';
    return name;
  }

  @override
  String toString() => '${_visibilityPrefix}procedure $qualifiedName(${argTypes.join(', ')}).';
}

/// [expr] with every type name in [rename] replaced by its value, in the name
/// of a reference and in its type arguments, through every constructor. A
/// name in [typeParams] is a type parameter, names no type and is left alone.
TypeExpr renameTypeRefs(
    TypeExpr expr, Map<String, String> rename, Set<String> typeParams) {
  if (expr is TypeRef) {
    final args = [
      for (final a in expr.typeArgs) renameTypeRefs(a, rename, typeParams)
    ];
    final name = typeParams.contains(expr.name)
        ? expr.name
        : (rename[expr.name] ?? expr.name);
    return TypeRef(name, expr.line, expr.column,
        isInput: expr.isInput, typeArgs: args);
  }
  if (expr is StructAlt) {
    return StructAlt(
        expr.functor,
        [for (final a in expr.args) renameTypeRefs(a, rename, typeParams)],
        expr.line,
        expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(renameTypeRefs(expr.head, rename, typeParams),
        renameTypeRefs(expr.tail, rename, typeParams), expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(renameTypeRefs(expr.content, rename, typeParams),
        renameTypeRefs(expr.hole, rename, typeParams), expr.line, expr.column);
  }
  return expr; // ConstantAlt, ListNilAlt, PrimitiveModeAlt: no type name
}

/// Whether two type definitions are the same text: the same parameters and
/// the same alternatives in order, positions aside.
bool sameTypeDef(TypeDef a, TypeDef b) {
  if (a.typeParams.length != b.typeParams.length) return false;
  for (var i = 0; i < a.typeParams.length; i++) {
    if (a.typeParams[i] != b.typeParams[i]) return false;
  }
  if (a.alternatives.length != b.alternatives.length) return false;
  for (var i = 0; i < a.alternatives.length; i++) {
    if (!sameTypeExpr(a.alternatives[i], b.alternatives[i])) return false;
  }
  return true;
}

/// Whether two type expressions are the same text, positions aside.
bool sameTypeExpr(TypeExpr a, TypeExpr b) {
  if (a is TypeRef) {
    if (b is! TypeRef || a.name != b.name || a.isInput != b.isInput) {
      return false;
    }
    if (a.typeArgs.length != b.typeArgs.length) return false;
    for (var i = 0; i < a.typeArgs.length; i++) {
      if (!sameTypeExpr(a.typeArgs[i], b.typeArgs[i])) return false;
    }
    return true;
  }
  if (a is PrimitiveModeAlt) {
    return b is PrimitiveModeAlt && a.isInput == b.isInput;
  }
  if (a is ConstantAlt) return b is ConstantAlt && a.value == b.value;
  if (a is ListNilAlt) return b is ListNilAlt;
  if (a is ListConsAlt) {
    return b is ListConsAlt &&
        sameTypeExpr(a.head, b.head) &&
        sameTypeExpr(a.tail, b.tail);
  }
  if (a is StructAlt) {
    if (b is! StructAlt || a.functor != b.functor) return false;
    if (a.args.length != b.args.length) return false;
    for (var i = 0; i < a.args.length; i++) {
      if (!sameTypeExpr(a.args[i], b.args[i])) return false;
    }
    return true;
  }
  if (a is DiffListAlt) {
    return b is DiffListAlt &&
        sameTypeExpr(a.content, b.content) &&
        sameTypeExpr(a.hole, b.hole);
  }
  return false;
}

/// The type environment: all type definitions and procedure declarations in a module
class TypeEnvironment {
  final Map<String, TypeDef> types;
  final Map<String, ProcDecl> procedures;  // keyed by "name/arity"
  /// Parameterized procedure declaration templates, keyed by "name/arity".
  /// Used for call-site type parameter inference (Case B).
  final Map<String, ProcDecl> paramProcDecls;
  /// Parameterized type templates from root scope/ancestors.
  /// Passed to downstream expansions so they can expand references
  /// to templates defined in ancestor scopes.
  final Map<String, TypeDef> typeTemplates;
  /// The scope each type in [types] was defined in: the label its layer was
  /// merged under ([merge]'s `label`), and so the prefix it is kept under once
  /// a nearer scope defines its name ([shadowedBy]). A type with no entry came
  /// in unlabelled and is kept under `outer:`.
  final Map<String, String> typeOrigins;

  TypeEnvironment(this.types, this.procedures, {
      Map<String, ProcDecl>? paramProcDecls,
      this.typeTemplates = const {},
      this.typeOrigins = const {},
  }) : paramProcDecls = paramProcDecls ?? {};

  factory TypeEnvironment.empty() => TypeEnvironment({}, {});

  /// The merge `E ⊔ E'` of TGLP Definition "Root, Scope", [other] being `E'`:
  /// its definitions shadow this environment's by name.
  ///
  /// A type name both define is two types --- "two same-named types defined
  /// in them are two types, which one flat namespace would otherwise make one,
  /// checking a module against a definition it cannot see" (TGLP
  /// "Compilation", third step) --- and a declaration's types are those of the
  /// scope it was declared in. So this environment's definition of the name is
  /// not dropped: it is kept under `<origin>:T`, every reference to it in this
  /// environment's types, declarations and templates is rewritten to that
  /// name ([shadowedBy]), and [other]'s definition takes the bare name.
  /// [label] is the scope [other]'s types are recorded as defined in.
  ///
  /// Until 2026-09-18 the merge was a flat overwrite, so the root `self.glp`'s
  /// `procedure authorise_link(GlobalName?, Answer?)` read a descendant's
  /// `Answer` wherever one was defined, and `social/graph/core/agent.glp`'s
  /// `authorise_link(L, authorise)` was rejected against
  /// `social/graph/self.glp`'s `Answer` (IGLP, 2026-09-18).
  TypeEnvironment merge(TypeEnvironment other, {String? label}) {
    final kept = shadowedBy(other);
    return TypeEnvironment(
      {...kept.types, ...other.types},
      {...kept.procedures, ...other.procedures},
      paramProcDecls: {...kept.paramProcDecls, ...other.paramProcDecls},
      typeTemplates: {...kept.typeTemplates, ...other.typeTemplates},
      typeOrigins: {...kept.typeOrigins, ...other.originsUnder(label)},
    );
  }

  /// [typeOrigins] with every type of this environment that has no origin
  /// recorded under [label]; [typeOrigins] itself when [label] is null.
  Map<String, String> originsUnder(String? label) => label == null
      ? typeOrigins
      : {for (final t in types.keys) t: typeOrigins[t] ?? label};

  /// This environment as it survives beside [winner], whose definitions take
  /// the bare names: each type both define --- as different definitions; two
  /// equal definitions are one type, type identity being structural, and
  /// either serves --- is kept under `<origin>:T`, and every reference to it
  /// in this environment's types, declarations and templates is rewritten to
  /// that name. [ownLabel] is the origin of a type of this environment that
  /// has none recorded. This environment itself when nothing is shadowed.
  TypeEnvironment shadowedBy(TypeEnvironment winner, {String? ownLabel}) {
    final rename = <String, String>{};
    final taken = <String>{...types.keys, ...winner.types.keys};
    // Whether two definitions differ can turn on a rename made here --- an
    // expansion instance `Stream<Answer>` of two different `Answer`s reads
    // the same until its `Answer` is rewritten --- so the set is closed by
    // iteration.
    var grew = true;
    while (grew) {
      grew = false;
      for (final name in types.keys) {
        if (rename.containsKey(name)) continue;
        final theirs = winner.types[name];
        if (theirs == null) continue;
        if (sameTypeDef(_renameTypeDef(types[name]!, rename), theirs)) continue;
        final origin = typeOrigins[name] ?? ownLabel ?? 'outer';
        var fresh = '$origin:$name';
        for (var k = 2;
            taken.contains(fresh) || rename.containsValue(fresh);
            k++) {
          fresh = '$origin$k:$name';
        }
        rename[name] = fresh;
        grew = true;
      }
    }
    if (rename.isEmpty) return this;
    return TypeEnvironment(
      {
        for (final e in types.entries)
          rename[e.key] ?? e.key: _renameTypeDef(e.value, rename)
      },
      {
        for (final e in procedures.entries)
          e.key: _renameProcDecl(e.value, rename)
      },
      paramProcDecls: {
        for (final e in paramProcDecls.entries)
          e.key: _renameProcDecl(e.value, rename)
      },
      typeTemplates: {
        for (final e in typeTemplates.entries)
          e.key: _renameTypeDef(e.value, rename)
      },
      typeOrigins: {
        for (final e in typeOrigins.entries) rename[e.key] ?? e.key: e.value
      },
    );
  }

  static TypeDef _renameTypeDef(TypeDef td, Map<String, String> rename) {
    if (rename.isEmpty) return td;
    final params = td.typeParams.toSet();
    return TypeDef(
      rename[td.name] ?? td.name,
      [for (final alt in td.alternatives) renameTypeRefs(alt, rename, params)],
      td.line,
      td.column,
      typeParams: td.typeParams,
    );
  }

  static ProcDecl _renameProcDecl(ProcDecl d, Map<String, String> rename) {
    if (rename.isEmpty) return d;
    final params = d.typeParams.toSet();
    return ProcDecl(
      d.name,
      [for (final t in d.argTypes) renameTypeRefs(t, rename, params)],
      d.line,
      d.column,
      typeParams: d.typeParams,
      isBuiltin: d.isBuiltin,
      exported: d.exported,
      imported: d.imported,
      modulePath: d.modulePath,
    );
  }
  
  /// Look up a type definition
  TypeDef? getType(String name) => types[name];
  
  /// Look up a procedure declaration
  ProcDecl? getProcedure(String name, int arity) => procedures['$name/$arity'];
  
  /// Check if a type name is defined (including built-ins)
  bool hasType(String name) => types.containsKey(name) || TypeRef.builtins.contains(name);

  /// Check if a procedure is defined
  bool hasProcedure(String name, int arity) => procedures.containsKey('$name/$arity');

  /// Add a type definition to the environment
  void addType(TypeDef typeDef) {
    types[typeDef.name] = typeDef;
  }

  /// Add a procedure declaration to the environment
  void addProcedure(ProcDecl procDecl) {
    procedures[procDecl.qualifiedKey] = procDecl;
  }

  @override
  String toString() {
    final sb = StringBuffer();
    for (final t in types.values) {
      sb.writeln(t);
    }
    for (final p in procedures.values) {
      sb.writeln(p);
    }
    return sb.toString();
  }
}
