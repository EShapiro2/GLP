/// Abstract Syntax Tree nodes for GLP

import '../analysis/type_checker/type_ast.dart' show TypeDef, ProcDecl;

/// Compilation mode: controls compiler restrictions
enum CompileMode {
  /// User mode (default): underscore-prefixed constants are rejected
  user,
  /// System mode: underscore-prefixed constants are allowed
  system,
}

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
    if (value is String) {
      final s = value as String;
      // Don't double-quote if already quoted (string literals)
      if ((s.startsWith('"') && s.endsWith('"')) ||
          (s.startsWith("'") && s.endsWith("'"))) {
        return s;
      }
      return '"$value"';
    }
    return value.toString();
  }
}

class UnderscoreTerm extends Term {
  // Anonymous variable _ or _?
  final bool isReader;  // false for _, true for _?
  
  UnderscoreTerm(int line, int column, {this.isReader = false}) : super(line, column);

  @override
  String toString() => isReader ? '_?' : '_';
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

// ExportDeclaration, ImportDeclaration, and ProcRef removed in Phase 1.
// Visibility is now declared per-procedure via 'exported procedure'.

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

/// Spawn goal: Goal@AgentId
/// Used for isolate spawning in boot clauses
/// In dGLP mode: the @AgentId annotation is ignored, goal runs in single isolate
/// In madGLP mode: the goal is spawned in a separate isolate named AgentId
class SpawnGoal extends Goal {
  final Goal innerGoal;
  final String agentId;

  SpawnGoal(this.innerGoal, this.agentId, int line, int column)
      : super('@', [_goalToTerm(innerGoal), ConstTerm(agentId, line, column)], line, column);

  @override
  String toString() => '$innerGoal@$agentId';

  /// Convert a Goal to a StructTerm for storage in args
  static Term _goalToTerm(Goal g) {
    return StructTerm(g.functor, g.args, g.line, g.column);
  }
}

// ============================================================================
// Type Declarations (Yardeni-Shapiro syntax)
// ============================================================================
// Note: Type definitions and procedure declarations use types from
// analysis/type_checker/type_ast.dart (TypeDef, ProcDecl).
// These are imported by the parser and stored in Module.

/// Complete module structure
class Module extends AstNode {
  final ModuleDeclaration? declaration;
  final List<TypeDef> typeDefs;              // Type definitions: Name ::= alt ; alt.
  final List<ProcDecl> procDeclarations;     // Procedure declarations (each with exported flag)
  final List<ProcDecl> paramProcDecls;       // Parameterized proc decl templates (for call-site inference)
  final List<Procedure> procedures;
  final CompileMode compileMode;  // user (default) or system

  Module({
    this.declaration,
    this.typeDefs = const [],
    this.procDeclarations = const [],
    this.paramProcDecls = const [],
    this.procedures = const [],
    this.compileMode = CompileMode.user,
    required int line,
    required int column,
  }) : super(line, column);

  /// Get module name, or null if anonymous
  String? get name => declaration?.name;

  /// Get all exported procedure signatures (from procedure declarations with exported=true)
  Set<String> get exportedSignatures {
    final result = <String>{};
    for (final decl in procDeclarations) {
      if (decl.exported) {
        result.add(decl.key);
      }
    }
    return result;
  }

  @override
  String toString() => 'Module(${name ?? "_anonymous"}, ${procedures.length} procedures)';
}
