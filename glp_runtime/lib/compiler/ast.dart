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

/// One position of a volition guard's question, `X_l = T_l` (vGLP,
/// Definition "Guarded Clause, Volition-Guarded Clause, Volition Guard,
/// Question, Answer, Context, Else-Branch, Ordinary Clause, Procedure, vGLP
/// Program").
///
/// [writer] is the answer writer `X_l`, or null where the position abbreviates
/// `_ = T_l` — an anonymous writer, which requires [value] ground.  [value] is
/// the ground term `T_l`, or null where the position abbreviates `X_l = _` — an
/// anonymous value, which is the field the person fills.
class QuestionPosition {
  final VarTerm? writer;
  final Term? value;

  QuestionPosition({this.writer, this.value});

  /// A field of the construct: the person supplies the value (Definition
  /// "Manifest", `fields`).
  bool get isField => value == null;

  @override
  String toString() {
    if (writer == null) return '$value';
    if (value == null) return '$writer';
    return '$writer=$value';
  }
}

/// A volition guard preceding a clause: `*(X1=T1, ..., Xi=Ti, Y1?, ..., Yj?)`,
/// or bare `*` where i = j = 0 (vGLP, Definition "Guarded Clause, ...").
class VolitionGuard extends AstNode {
  final List<QuestionPosition> question;
  final List<VarTerm> context;  // the readers Y_l?

  VolitionGuard(this.question, this.context, int line, int column)
      : super(line, column);

  @override
  String toString() =>
      '*(${[...question.map((q) => '$q'), ...context.map((c) => '$c')].join(", ")})';
}

/// The else-branch of a volition-guarded clause, written after its body:
/// `*(T'1, ..., T'i) B'` (vGLP, Definition "Guarded Clause, ...").  Each
/// [answer] term is a ground term or a reader paired to a head writer that the
/// clause's guard makes ground.
class ElseBranch extends AstNode {
  final List<Term> answer;
  final List<Goal> body;

  ElseBranch(this.answer, this.body, int line, int column)
      : super(line, column);

  @override
  String toString() => '*(${answer.join(", ")}) ${body.join(", ")}';
}

// Clause: Head :- Guards | Body.
//
// A vGLP clause may carry a volition guard before its head and, if it does, an
// else-branch after its body.  Both are null in GLP, which is vGLP without
// volition-guarded clauses (vGLP, Definition "GLP, maGLP, cGLP"), and the
// parser only admits them for a .vglp source.
class Clause extends AstNode {
  final Atom head;
  final List<Guard>? guards;  // Optional guard list before |
  final List<Goal>? body;     // Optional body goals after |
  final VolitionGuard? volitionGuard;
  final ElseBranch? elseBranch;

  Clause(this.head, {this.guards, this.body, this.volitionGuard, this.elseBranch,
      required int line, required int column})
      : super(line, column);

  /// Whether this is a volition-guarded clause (vGLP, Definition "Guarded
  /// Clause, ...").
  bool get isVolitionGuarded => volitionGuard != null;

  @override
  String toString() {
    final volStr = volitionGuard != null ? '$volitionGuard ' : '';
    final guardsStr = guards != null && guards!.isNotEmpty ? ' :- ${guards!.join(", ")}' : '';
    final bodyStr = body != null && body!.isNotEmpty ? ' | ${body!.join(", ")}' : '';
    final elseStr = elseBranch != null ? ' $elseBranch' : '';
    return 'Clause($volStr$head$guardsStr$bodyStr$elseStr)';
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

// ModuleDeclaration removed: the -module(name) directive is no longer
// supported. A module's name is its file/directory path from the program root.

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
  // A module has no name in its source: its name is its file/directory path
  // from the program root, assigned by the loader/linker (-module removed).
  final List<TypeDef> typeDefs;              // Type definitions: Name ::= alt ; alt.
  final List<ProcDecl> procDeclarations;     // Procedure declarations (each with exported flag)
  final List<ProcDecl> paramProcDecls;       // Parameterized proc decl templates (for call-site inference)
  final List<Procedure> procedures;
  final CompileMode compileMode;  // user (default) or system
  final List<String> exposes;     // `-expose(M).` module paths (e.g. "lib#streams")

  Module({
    this.typeDefs = const [],
    this.procDeclarations = const [],
    this.paramProcDecls = const [],
    this.procedures = const [],
    this.compileMode = CompileMode.user,
    this.exposes = const [],
    required int line,
    required int column,
  }) : super(line, column);

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
  String toString() => 'Module(${procedures.length} procedures)';
}
