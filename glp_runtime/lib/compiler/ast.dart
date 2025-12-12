/// Abstract Syntax Tree nodes for GLP

// Base class for all AST nodes
abstract class AstNode {
  final int line;
  final int column;

  AstNode(this.line, this.column);
}

// Top-level program
class Program extends AstNode {
  final List<Procedure> procedures;

  Program(this.procedures, int line, int column) : super(line, column);

  @override
  String toString() => 'Program(${procedures.length} procedures)';
}

// Procedure: all clauses with same functor/arity
class Procedure extends AstNode {
  final String name;
  final int arity;
  final List<Clause> clauses;

  Procedure(this.name, this.arity, this.clauses, int line, int column)
      : super(line, column);

  String get signature => '$name/$arity';

  @override
  String toString() => 'Procedure($signature, ${clauses.length} clauses)';
}

// Clause: Head :- Guards | Body.
class Clause extends AstNode {
  final Atom head;
  final List<Guard>? guards;  // Optional guard list before |
  final List<Goal>? body;     // Optional body goals after |

  Clause(this.head, {this.guards, this.body, required int line, required int column})
      : super(line, column);

  @override
  String toString() {
    final guardsStr = guards != null && guards!.isNotEmpty ? ' :- ${guards!.join(", ")}' : '';
    final bodyStr = body != null && body!.isNotEmpty ? ' | ${body!.join(", ")}' : '';
    return 'Clause($head$guardsStr$bodyStr)';
  }
}

// Atom: predicate in clause head
class Atom extends AstNode {
  final String functor;
  final List<Term> args;

  Atom(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;

  @override
  String toString() => '$functor(${args.join(", ")})';
}

// Goal: predicate call in clause body
class Goal extends AstNode {
  final String functor;
  final List<Term> args;

  Goal(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;

  @override
  String toString() => '$functor(${args.join(", ")})';
}

// Guard: pure test in guard section
class Guard extends AstNode {
  final String predicate;
  final List<Term> args;
  final bool negated;  // true if ~G (guard negation)

  Guard(this.predicate, this.args, int line, int column, {this.negated = false}) : super(line, column);

  @override
  String toString() => negated ? '~$predicate(${args.join(", ")})' : '$predicate(${args.join(", ")})';
}

// Terms (expressions)
abstract class Term extends AstNode {
  Term(int line, int column) : super(line, column);
}

class VarTerm extends Term {
  final String name;
  final bool isReader;  // true for X?, false for X

  VarTerm(this.name, this.isReader, int line, int column) : super(line, column);

  @override
  String toString() => isReader ? '$name?' : name;
}

class StructTerm extends Term {
  final String functor;
  final List<Term> args;

  StructTerm(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;

  @override
  String toString() => '$functor(${args.join(", ")})';
}

class ListTerm extends Term {
  final Term? head;
  final Term? tail;

  // [H|T] -> ListTerm(H, T)
  // []    -> ListTerm(null, null)
  ListTerm(this.head, this.tail, int line, int column) : super(line, column);

  bool get isNil => head == null && tail == null;

  @override
  String toString() {
    if (isNil) return '[]';
    if (tail == null) return '[$head]';
    return '[$head|$tail]';
  }
}

class ConstTerm extends Term {
  final Object? value;  // String, int, double, or atom name

  ConstTerm(this.value, int line, int column) : super(line, column);

  @override
  String toString() {
    if (value is String) return '"$value"';
    return value.toString();
  }
}

class UnderscoreTerm extends Term {
  // Anonymous variable _
  UnderscoreTerm(int line, int column) : super(line, column);

  @override
  String toString() => '_';
}

// ============================================================================
// Module System AST Nodes
// ============================================================================

/// Module declaration: -module(name).
class ModuleDeclaration extends AstNode {
  final String name;

  ModuleDeclaration(this.name, int line, int column) : super(line, column);

  @override
  String toString() => '-module($name).';
}

/// Export declaration: -export([pred/arity, ...]).
class ExportDeclaration extends AstNode {
  final List<ProcRef> exports;

  ExportDeclaration(this.exports, int line, int column) : super(line, column);

  @override
  String toString() => '-export([${exports.join(", ")}]).';
}

/// Import declaration: -import([module1, module2, ...]).
class ImportDeclaration extends AstNode {
  final List<String> imports;

  ImportDeclaration(this.imports, int line, int column) : super(line, column);

  @override
  String toString() => '-import([${imports.join(", ")}]).';
}

/// Procedure reference: pred/arity
class ProcRef {
  final String name;
  final int arity;

  ProcRef(this.name, this.arity);

  String get signature => '$name/$arity';

  @override
  String toString() => signature;
}

/// Remote goal: Module # Goal
/// Used for cross-module procedure calls
class RemoteGoal extends Goal {
  final Term module;  // Can be ConstTerm (atom) or VarTerm (variable)
  final Goal goal;

  RemoteGoal(this.module, this.goal, int line, int column)
      : super('#', [module, _goalToTerm(goal)], line, column);

  /// Get module name if statically known, null if dynamic (variable)
  String? get staticModuleName {
    if (module is ConstTerm) {
      return (module as ConstTerm).value as String;
    }
    return null;
  }

  /// Check if module is dynamically resolved (variable)
  bool get isDynamic => module is VarTerm;

  @override
  String toString() => '$module # $goal';

  /// Convert a Goal to a StructTerm for storage in args
  static Term _goalToTerm(Goal g) {
    return StructTerm(g.functor, g.args, g.line, g.column);
  }
}

// ============================================================================
// Polymorphic Moded Types (PMT) AST Nodes
// ============================================================================

/// Mode declaration: TypeName(Params) := predicate(ModedArg, ...).
/// Example: Merge(A) := merge(List(A)?, List(A)?, List(A)).
class ModeDeclaration extends AstNode {
  final String typeName;           // e.g., "Merge"
  final List<String> typeParams;   // e.g., ["A"]
  final String predicate;          // e.g., "merge"
  final List<ModedArg> args;       // e.g., [reader, reader, writer]

  ModeDeclaration(
    this.typeName,
    this.typeParams,
    this.predicate,
    this.args,
    int line,
    int column,
  ) : super(line, column);

  int get arity => args.length;
  String get signature => '$predicate/$arity';

  @override
  String toString() {
    final paramsStr = typeParams.isEmpty ? '' : '(${typeParams.join(", ")})';
    return '$typeName$paramsStr := $predicate(${args.join(", ")}).';
  }
}

/// Moded argument in a mode declaration
/// Example: List(A)? is ModedArg("List", ["A"], isReader: true)
class ModedArg {
  final String typeName;           // e.g., "List" or "Num"
  final List<String> typeParams;   // e.g., ["A"]
  final bool isReader;             // true if T?, false if T

  ModedArg(this.typeName, this.typeParams, {required this.isReader});

  @override
  String toString() => isReader ? '$typeName${_paramsStr}?' : '$typeName$_paramsStr';

  String get _paramsStr => typeParams.isEmpty ? '' : '(${typeParams.join(", ")})';
}

/// Complete module structure
class Module extends AstNode {
  final ModuleDeclaration? declaration;
  final List<ExportDeclaration> exports;
  final List<ImportDeclaration> imports;
  final List<ModeDeclaration> modeDeclarations;
  final List<Procedure> procedures;

  Module({
    this.declaration,
    this.exports = const [],
    this.imports = const [],
    this.modeDeclarations = const [],
    this.procedures = const [],
    required int line,
    required int column,
  }) : super(line, column);

  /// Get module name, or null if anonymous
  String? get name => declaration?.name;

  /// Get all exported procedure signatures
  Set<String> get exportedSignatures {
    final result = <String>{};
    for (final decl in exports) {
      for (final ref in decl.exports) {
        result.add(ref.signature);
      }
    }
    return result;
  }

  /// Get all imported module names
  List<String> get importedModules {
    final result = <String>[];
    for (final decl in imports) {
      result.addAll(decl.imports);
    }
    return result;
  }

  @override
  String toString() => 'Module(${name ?? "_anonymous"}, ${procedures.length} procedures)';
}
