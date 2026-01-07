// lib/analysis/type_checker/type_ast.dart
//
// AST nodes for GLP type declarations following Yardeni-Shapiro.
// Types are first-class syntactic elements parsed alongside clauses.

/// Base class for type expressions
abstract class TypeExpr {
  final int line;
  final int column;
  
  TypeExpr(this.line, this.column);
}

/// Reference to a named type: Nat, List, Number, String, Any
/// Optionally with input mode annotation (Type?)
class TypeRef extends TypeExpr {
  final String name;
  final bool isInput;  // true if Type?, false if Type

  TypeRef(this.name, int line, int column, {this.isInput = false})
      : super(line, column);

  /// Mode complementation operator (·)?
  /// Returns a new TypeRef with inverted mode
  TypeRef complement() => TypeRef(name, line, column, isInput: !isInput);

  @override
  String toString() => isInput ? '$name?' : name;

  /// Primitive types (not defined via ::=, handled specially by compiler)
  static const builtins = {'Number', 'String'};

  /// System types (defined via ::= but not redefinable by user)
  static const systemTypes = {'List'};

  bool get isBuiltin => builtins.contains(name);

  @override
  bool operator ==(Object other) =>
      other is TypeRef && other.name == name && other.isInput == isInput;

  @override
  int get hashCode => Object.hash(name, isInput);
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
class TypeDef {
  final String name;
  final List<TypeExpr> alternatives;
  final int line;
  final int column;

  TypeDef(this.name, this.alternatives, this.line, this.column);

  @override
  String toString() => '$name ::= ${alternatives.join(' ; ')}.';
}

/// A procedure declaration: procedure name(Type1, Type2, ...).
class ProcDecl {
  final String name;
  final List<TypeRef> argTypes;
  final int line;
  final int column;

  ProcDecl(this.name, this.argTypes, this.line, this.column);

  int get arity => argTypes.length;

  String get key => '$name/$arity';

  /// Get the mode for argument at index i (true = input mode)
  bool isInputArg(int i) => argTypes[i].isInput;

  /// Get complemented types (callee's perspective at call site)
  /// What caller provides as output, callee sees as input, and vice versa
  List<TypeRef> get calleeView =>
      argTypes.map((t) => t.complement()).toList();

  @override
  String toString() => 'procedure $name(${argTypes.join(', ')}).';
}

/// The type environment: all type definitions and procedure declarations in a module
class TypeEnvironment {
  final Map<String, TypeDef> types;
  final Map<String, ProcDecl> procedures;  // keyed by "name/arity"
  
  TypeEnvironment(this.types, this.procedures);
  
  factory TypeEnvironment.empty() => TypeEnvironment({}, {});
  
  /// Merge another environment into this one
  TypeEnvironment merge(TypeEnvironment other) {
    return TypeEnvironment(
      {...types, ...other.types},
      {...procedures, ...other.procedures},
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
