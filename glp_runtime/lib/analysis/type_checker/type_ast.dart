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

  /// Primitive types (not defined via ::=, handled specially by compiler)
  static const builtins = {'Integer', 'Real', 'Number', 'String'};

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

/// The type environment: all type definitions and procedure declarations in a module
class TypeEnvironment {
  final Map<String, TypeDef> types;
  final Map<String, ProcDecl> procedures;  // keyed by "name/arity"
  /// Parameterized procedure declaration templates, keyed by "name/arity".
  /// Used for call-site type parameter inference (Case B).
  final Map<String, ProcDecl> paramProcDecls;
  /// Parameterized type templates from prelude/ancestors.
  /// Passed to downstream expansions so they can expand references
  /// to templates defined in ancestor scopes.
  final Map<String, TypeDef> typeTemplates;

  TypeEnvironment(this.types, this.procedures, {
      Map<String, ProcDecl>? paramProcDecls,
      this.typeTemplates = const {},
  }) : paramProcDecls = paramProcDecls ?? {};

  factory TypeEnvironment.empty() => TypeEnvironment({}, {});

  /// Merge another environment into this one
  TypeEnvironment merge(TypeEnvironment other) {
    return TypeEnvironment(
      {...types, ...other.types},
      {...procedures, ...other.procedures},
      paramProcDecls: {...paramProcDecls, ...other.paramProcDecls},
      typeTemplates: {...typeTemplates, ...other.typeTemplates},
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
