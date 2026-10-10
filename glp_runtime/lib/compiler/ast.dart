/// Abstract Syntax Tree nodes for GLP

import '../analysis/type_checker/type_ast.dart'
    show TypeDef, TypeExpr, ProcDecl;

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

  /// Whether this is a volitional procedure (vGLP, Definition "Guarded
  /// Clause, Volitional Procedure, Interactive Type, Interactive Term,
  /// Ordinary Clause, Procedure, vGLP Program"): its clauses are written
  /// `(A)*p(S1, ..., Sn) :- G | B`, each the guarded clause
  /// `p(S1, ..., Sn, A) :- G | B` of arity n+1, so [arity] is n+1, the
  /// volitional procedure's own arity n being one less.  The parser admits
  /// such clauses only after the declaration `procedure (T)*p(T1, ..., Tn).`
  /// and only such clauses there ([Module.volitionalDeclarations]).
  bool get isVolitional =>
      clauses.isNotEmpty && clauses.first.interactiveTerm != null;

  @override
  String toString() => 'Procedure($signature, ${clauses.length} clauses)';
}

/// The declaration of a volitional procedure p of arity n,
/// `procedure (T)*p(T1, ..., Tn).`, T its interactive type, in writer or
/// reader mode as an argument type is (vGLP, Definition "Guarded Clause,
/// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
/// Procedure, vGLP Program").
///
/// A clause of the procedure "is the guarded clause p(S1, ..., Sn, A) :- G |
/// B, of arity n+1", so [decl] declares those clauses: `p(T1, ..., Tn, T)`,
/// the interactive type last.  It is among the module's procedure
/// declarations as well, where it stands for the procedure's clauses.
class VolitionalDeclaration extends AstNode {
  final ProcDecl decl;

  VolitionalDeclaration(this.decl) : super(decl.line, decl.column);

  String get name => decl.name;

  /// n, the arity of the volitional procedure; its clauses have n+1
  /// arguments.
  int get arity => decl.argTypes.length - 1;

  /// p/n.
  String get signature => '$name/$arity';

  /// T, in its mode.
  TypeExpr get interactiveType => decl.argTypes.last;

  /// Whether T is in reader mode.
  bool get readerMode => decl.isInputArg(arity);

  @override
  String toString() {
    final types = decl.argTypes.sublist(0, arity).join(', ');
    return 'procedure ($interactiveType)*$name($types).';
  }
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

/// One item of a display declaration: `panel(N)`, `label(L)`, `field(X, W)`,
/// `view(K)`, `persistent` or `transient` (vGLP, Definition "Display
/// Declaration, Default Display").
class DisplayItem extends AstNode {
  final String name;
  final List<Term> args;

  DisplayItem(this.name, this.args, int line, int column) : super(line, column);

  @override
  String toString() => args.isEmpty ? name : '$name(${args.join(", ")})';
}

/// A display declaration (vGLP, Definition "Display Declaration, Default
/// Display").  Two forms: for a volition-guarded clause, naming its predicate
/// and its volition guard —
///
///     display p *(...) : panel(N), label(L), field(X, W), persistent.
///
/// and for a message pattern of the person channel —
///
///     display m : panel(N), view(K).
///
/// It fixes how a construct looks and how the program's output is viewed, and
/// changes nothing the manifest fixes, so every item has a default read off the
/// program and a program with no display declarations still renders.  The
/// compiled program carries its declarations unchanged, for the bridge to read.
class DisplayDecl extends AstNode {
  /// Clause form: the predicate of the volition-guarded clause, and its guard.
  final String? predicate;
  final VolitionGuard? guard;

  /// Message form: the pattern of the person-channel message.
  final Term? pattern;

  final List<DisplayItem> items;

  DisplayDecl({this.predicate, this.guard, this.pattern,
      required this.items, required int line, required int column})
      : super(line, column);

  bool get isClauseForm => predicate != null;

  @override
  String toString() => isClauseForm
      ? 'display $predicate $guard : ${items.join(", ")}.'
      : 'display $pattern : ${items.join(", ")}.';
}

// Clause: Head :- Guards | Body.
//
// A clause of a volitional procedure is written (A)*p(S1, ..., Sn) :- G | B
// and is the guarded clause p(S1, ..., Sn, A) :- G | B, of arity n+1 (vGLP,
// Definition "Guarded Clause, Volitional Procedure, Interactive Type,
// Interactive Term, Ordinary Clause, Procedure, vGLP Program"): its head is
// that guarded clause's, and [interactiveTerm] is A, the head's last argument.
// It is null for an ordinary clause, and always in GLP, which is vGLP without
// volitional procedures; the parser admits the form only for a .vglp source.
//
// A .vglp source not yet in that syntax may carry the volition guard of the
// Definition it replaced ("Guarded Clause, Volition-Guarded Clause, ...")
// before its head and, if it does, an else-branch after its body; both are
// null otherwise, and the parser admits them only for a .vglp source.
class Clause extends AstNode {
  final Atom head;
  final List<Guard>? guards;  // Optional guard list before |
  final List<Goal>? body;     // Optional body goals after |
  final VolitionGuard? volitionGuard;
  final ElseBranch? elseBranch;

  /// The interactive term A of a clause `(A)*p(S1, ..., Sn) :- G | B` of a
  /// volitional procedure, the last argument of [head]; null for an ordinary
  /// clause.
  final Term? interactiveTerm;

  Clause(this.head, {this.guards, this.body, this.volitionGuard, this.elseBranch,
      this.interactiveTerm, required int line, required int column})
      : super(line, column);

  /// Whether this is a volition-guarded clause, of the Definition "Guarded
  /// Clause, Volition-Guarded Clause, ..." that vGLP's Definition "Guarded
  /// Clause, Volitional Procedure, ..." replaced.  A clause of a volitional
  /// procedure is not one: it carries [interactiveTerm].
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

  Guard(this.predicate, this.args, int line, int column) : super(line, column);

  @override
  String toString() => '$predicate(${args.join(", ")})';
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
/// Used for cross-module procedure calls.  The module is named, a child
/// directory or module file of the caller's directory (TGLP modules.tex,
/// "Cross-module type checking"); the parser refuses a variable there.
class RemoteGoal extends Goal {
  final ConstTerm module;
  final Goal goal;

  RemoteGoal(this.module, this.goal, int line, int column)
      : super('#', [module, _goalToTerm(goal)], line, column);

  /// The module's name.
  String get staticModuleName => module.value as String;

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
  final List<DisplayDecl> displayDecls;  // `display ... : ... .` declarations

  /// The declarations `procedure (T)*p(T1, ..., Tn).` of the module's
  /// volitional procedures, in source order (vGLP, Definition "Guarded
  /// Clause, Volitional Procedure, ...").  Each one's [ProcDecl] is in
  /// [procDeclarations] too, and its procedure in [procedures].
  final List<VolitionalDeclaration> volitionalDeclarations;

  Module({
    this.typeDefs = const [],
    this.procDeclarations = const [],
    this.paramProcDecls = const [],
    this.procedures = const [],
    this.compileMode = CompileMode.user,
    this.exposes = const [],
    this.displayDecls = const [],
    this.volitionalDeclarations = const [],
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
