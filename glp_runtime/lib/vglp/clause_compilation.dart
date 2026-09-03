// glp_runtime/lib/vglp/clause_compilation.dart
//
// The clause transformation of the canonical compilation.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".
//
// A vGLP program M becomes the GLP program ⌈M⌉: the AGENT, each of its
// volition-guarded clauses compiled to clauses that pose the clause's question,
// take its then-branch on the person's answer and its else-branch on a timeout;
// and the MEDIATOR, which renders the questions and routes the answers.  This
// file is the agent's half — the per-clause rewriting.  The types are in
// types.dart and the mediator in mediator.dart, both in this directory.
//
// TWO DEPARTURES FROM THE DEFINITION'S LETTER, both forced by the moded type
// system, both stated in programs/vglp/self.glp and reported to the paper's
// session:
//
//   - ONE ESCROWED WRITER, NOT TWO.  The Definition escrows a selector and an
//     answer separately and guards the answer clause on `Sel? =?= then`.  Here
//     the two are one `Reply(A) ::= then(A) ; else`, and the compiled clause
//     SELECTS ITS BRANCH BY HEAD UNIFICATION: the answer clause's slot is
//     `ask(then(xs(X1,...,Xi)), _)` and the else clause's is `ask(else, _)`.
//     A second escrowed writer would have to be left unbound on the else path,
//     and no program clause can write that position.
//
//   - ONE SLOT TYPE FOR THE PROGRAM, not one per clause, so that `aborts` can
//     take a clause's slot list, whose slots belong to different clauses.

import '../compiler/ast.dart';
import '../analysis/type_checker/type_ast.dart' show ProcDecl;

/// The name a compiled procedure's added mediator argument carries, and the
/// stem of the slot arguments.  Both are made fresh against the clause's own
/// variables, so a program may use these names itself.
const _medStem = 'Med';
const _slotStem = 'S';
const _replyStem = 'R';
const _idStem = 'Id';

/// The compiled form of one vGLP procedure.
class CompiledProcedure {
  final String name;

  /// The arity of the compiled head: the source arity, plus one for the
  /// mediator channel, plus one per volition-guarded clause of the procedure.
  final int arity;

  final List<Clause> clauses;

  /// The volition-guarded clauses of the source procedure, in order; their
  /// position in this list is their slot number, from 1.
  final List<Clause> volitionGuarded;

  CompiledProcedure(this.name, this.arity, this.clauses, this.volitionGuarded);

  int get slotCount => volitionGuarded.length;
}

/// Compile one procedure of M (vGLP, Definition "Canonical Compilation").
///
/// [isProcedureOfM] tells a body goal that calls a procedure of the program —
/// which carries the mediator channel and slots — from a built-in goal, which
/// carries nothing.
CompiledProcedure compileProcedure(
  Procedure proc, {
  required ProcDecl decl,
  required bool Function(String name, int arity) isProcedureOfM,
  required String Function(Procedure, int) clauseName,
  required int Function(String name, int arity) slotCountOf,
}) {
  final volitionGuarded =
      proc.clauses.where((c) => c.isVolitionGuarded).toList();
  final m = volitionGuarded.length;

  final out = <Clause>[];
  var slotNumber = 0;
  for (final c in proc.clauses) {
    if (!c.isVolitionGuarded) {
      out.add(_compileOrdinary(c, proc, m,
          isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));
      continue;
    }
    slotNumber++;
    final j = slotNumber;
    final name = clauseName(proc, j);
    // In the order of the Definition: the answer clause, the else clause where
    // C has an else-branch, and the ask clause.
    out.add(_compileAnswer(c, proc, m, j, name,
        isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));
    if (c.elseBranch != null) {
      out.add(_compileElse(c, proc, m, j,
          isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));
    }
    out.add(_compileAsk(c, proc, decl, m, j, name));
  }
  if (m >= 1) out.add(_otherwiseClause(proc, decl, m));

  return CompiledProcedure(proc.name, proc.arity + 1 + m, out, volitionGuarded);
}

/// Last in a procedure with m >= 1, the clause
///
///     H'(Med, S1, ..., Sm) :- otherwise | true
///
/// which no safe run reaches, a goal's slots holding only asks of its own
/// clauses, and which input coverage requires: the slot type, one for every
/// slot of the program, admits every clause's answer, and a procedure whose
/// clauses are all volition-guarded with m = 1 would otherwise accept its own
/// answer alone at the slot.  The channel, every input position and every
/// slot is anonymous, since the clause reads nothing; each output position
/// carries the reader of a fresh writer, as H_a does, since a head produces
/// nothing at an output it does not bind.
Clause _otherwiseClause(Procedure proc, ProcDecl decl, int m) {
  final line = proc.line;
  final column = proc.column;
  final head = Atom(proc.name, [
    UnderscoreTerm(line, column),
    for (var k = 0; k < proc.arity; k++)
      k < decl.argTypes.length && !decl.isInputArg(k)
          ? _r('A${k + 1}')
          : UnderscoreTerm(line, column),
    for (var k = 0; k < m; k++) UnderscoreTerm(line, column),
  ], line, column);
  return Clause(head,
      guards: [Guard('otherwise', const [], line, column)],
      body: [Goal('true', const [], line, column)],
      line: line,
      column: column);
}

// ---------------------------------------------------------------------------
// The three compiled clauses of a volition-guarded clause, and the ordinary one
// ---------------------------------------------------------------------------

/// An ordinary clause C = H :- G | B becomes
///
///     H'(Med, S1, ..., Sm) :- G | aborts([S1?, ..., Sm?], Med?, Med1), B'
///
/// with B' the body given its branch of Med1 and slots `none`.  Where the
/// procedure has no volition-guarded clause the abort call is `aborts([], ...)`,
/// which is the identity on the channel, so it is omitted and Med carried
/// straight through: the Definition's formula with m = 0, one reduction fewer.
Clause _compileOrdinary(Clause c, Procedure proc, int m,
    {required bool Function(String, int) isProcedureOfM,
    required int Function(String, int) slotCountOf}) {
  final names = _NameSource(c);
  final med = names.fresh(_medStem);
  final slots = List.generate(m, (k) => names.fresh('$_slotStem${k + 1}'));

  final head = _extendHead(c.head, med, slots.map((s) => _w(s)).toList());
  final body = <Goal>[];
  final medIn = _abortsInto(body, slots, med, names);
  body.addAll(_compileBody(c.body ?? const [], medIn, names,
      isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));

  return Clause(head,
      guards: c.guards, body: body, line: c.line, column: c.column);
}

/// The answer clause of the j-th volition-guarded clause: the person answered,
/// so the reply carries the then-branch and the answer's values.
///
///     H'(Med, ..., ask(then(xs(X1, ..., Xi)), _), ...) :- G |
///         aborts([<the other slots>], Med?, Med1), B'
Clause _compileAnswer(Clause c, Procedure proc, int m, int j, String name,
    {required bool Function(String, int) isProcedureOfM,
    required int Function(String, int) slotCountOf}) {
  final names = _NameSource(c);
  final med = names.fresh(_medStem);
  final slots = List.generate(m, (k) => names.fresh('$_slotStem${k + 1}'));

  final slotArgs = <Term>[];
  for (var k = 0; k < m; k++) {
    slotArgs.add(k == j - 1
        ? _ask(_then(_xs(c.volitionGuard!, c, name)), _anon(c))
        : _w(slots[k]));
  }
  // The exposed slot is a pattern, not a variable, so it is not among the slots
  // aborted: its own ask is the one just answered, and the mediator has
  // consumed its entry.  "The answer and else clauses expose slot S_j and pass
  // the other slots through" (Definition "Canonical Compilation").
  final passed = [
    for (var k = 0; k < m; k++) if (k != j - 1) slots[k]
  ];

  final head = _extendHead(c.head, med, slotArgs);
  final body = <Goal>[];
  final medIn = _abortsInto(body, passed, med, names);
  body.addAll(_compileBody(c.body ?? const [], medIn, names,
      isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));

  return Clause(head,
      guards: c.guards, body: body, line: c.line, column: c.column);
}

/// The else clause: the deadline passed, the machine answers for the person.
///
///     H'(Med, ..., ask(else, _), ...) :- G[T'] |
///         aborts([<the other slots>], Med?, Med1), B'[T']
///
/// `G[T']` and `B'[T']` are the guard and the else-branch with the else answer
/// T'_l in place of X_l?, so no answer position is read and the reply carries
/// no values.
Clause _compileElse(Clause c, Procedure proc, int m, int j,
    {required bool Function(String, int) isProcedureOfM,
    required int Function(String, int) slotCountOf}) {
  final names = _NameSource(c);
  final med = names.fresh(_medStem);
  final slots = List.generate(m, (k) => names.fresh('$_slotStem${k + 1}'));
  final subst = _elseSubstitution(c);

  final slotArgs = <Term>[];
  for (var k = 0; k < m; k++) {
    slotArgs.add(k == j - 1
        ? _ask(ConstTerm('else', c.line, c.column), _anon(c))
        : _w(slots[k]));
  }
  final passed = [
    for (var k = 0; k < m; k++) if (k != j - 1) slots[k]
  ];

  final head = _extendHead(c.head, med, slotArgs);
  final guards = [
    for (final g in c.guards ?? const <Guard>[])
      Guard(g.predicate, g.args.map((t) => _substTerm(t, subst)).toList(),
          g.line, g.column, negated: g.negated)
  ];
  final elseBody = [
    for (final g in c.elseBranch!.body)
      Goal(g.functor, g.args.map((t) => _substTerm(t, subst)).toList(),
          g.line, g.column)
  ];

  final body = <Goal>[];
  final medIn = _abortsInto(body, passed, med, names);
  body.addAll(_compileBody(elseBody, medIn, names,
      isProcedureOfM: isProcedureOfM, slotCountOf: slotCountOf));

  return Clause(head,
      guards: guards, body: body, line: c.line, column: c.column);
}

/// The ask clause: the goal reaches the clause with the slot empty, poses the
/// question once, and recurses holding the readers of what it escrowed.
///
///     H'(Med, ..., none, ...) :- G_c |
///         send(ask(C, ctx(Y1?, ..., Yj?), R, Id), Med?, Med1),
///         H'(Med1?, ..., ask(R?, Id?), ...)
///
/// `G_c` is the conjuncts of G that read no answer position: the guard on the
/// context alone, which is what the goal can test before the person answers.
///
/// The ask clause reduces the goal WITHOUT performing its effect: it poses the
/// question and re-poses the goal.  So it cannot carry the source head's output
/// constructions — those are the effect, and only the clause that finally
/// reduces the goal may perform them.  It therefore passes each argument of H
/// through by mode, which is why the compilation needs M to be typed: at an
/// INPUT position the head keeps its pattern, so that `G_c` can read it, and
/// the re-posed goal is given the pattern's reader image; at an OUTPUT position
/// the head takes a fresh reader and the re-posed goal the paired writer, which
/// is GLP's own way of delegating an output to a body goal.
Clause _compileAsk(Clause c, Procedure proc, ProcDecl decl, int m, int j,
    String clauseName) {
  final names = _NameSource(c);
  final med = names.fresh(_medStem);
  final med1 = names.fresh('${_medStem}1');
  final reply = names.fresh(_replyStem);
  final id = names.fresh(_idStem);
  final slots = List.generate(m, (k) => names.fresh('$_slotStem${k + 1}'));

  final answerWriters = _answerWriterNames(c.volitionGuard!);

  final headArgs = <Term>[];
  final goalArgs = <Term>[];
  for (var k = 0; k < c.head.args.length; k++) {
    final isInput = k < decl.argTypes.length && decl.isInputArg(k);
    if (isInput) {
      // Keep the pattern, naming any anonymous writer so the goal can rebuild
      // it, and re-pose with the pattern's readers.
      final pattern = _nameAnonymous(c.head.args[k], names);
      headArgs.add(pattern);
      goalArgs.add(_readerImage(pattern));
    } else {
      final v = names.fresh('A${k + 1}');
      headArgs.add(_r(v));
      goalArgs.add(_w(v));
    }
  }

  final slotArgs = <Term>[];
  for (var k = 0; k < m; k++) {
    slotArgs.add(k == j - 1
        ? ConstTerm('none', c.line, c.column)
        : _w(slots[k]));
  }
  final head = Atom(c.head.functor,
      [_w(med), ...headArgs, ...slotArgs], c.head.line, c.head.column);

  final guardsC = _contextGuards(c, answerWriters);

  // The context term carries the clause's own functor, as the answer does.
  final Term ctx = c.volitionGuard!.context.isEmpty
      ? ConstTerm('ctx_$clauseName', c.line, c.column)
      : StructTerm('ctx_$clauseName',
          c.volitionGuard!.context.map<Term>((v) => _r(v.name)).toList(),
          c.line, c.column);

  final body = <Goal>[
    Goal('send', [
      StructTerm('ask', [
        ConstTerm(clauseName, c.line, c.column),
        ctx,
        _w(reply),
        _w(id),
        // An ask carries a deadline iff its clause has an else-branch: the
        // machine answers on the deadline only where the program says how.
        ConstTerm(c.elseBranch != null ? 'deadline' : 'no_deadline',
            c.line, c.column),
      ], c.line, c.column),
      _r(med),
      _w(med1),
    ], c.line, c.column),
    Goal(c.head.functor, [
      _r(med1),
      ...goalArgs,
      for (var k = 0; k < m; k++)
        if (k == j - 1)
          _ask(_r(reply), _r(id))
        else
          _r(slots[k]),
    ], c.line, c.column),
  ];

  return Clause(head,
      guards: guardsC, body: body, line: c.line, column: c.column);
}

// ---------------------------------------------------------------------------
// Body compilation: the mediator channel split among the body goals
// ---------------------------------------------------------------------------

/// B' is B with each body goal that calls a procedure of M given its branch of
/// the channel and its slots `none`; a built-in goal carries nothing
/// (Definition "Canonical Compilation": "split among the body goals of M's
/// procedures; built-in goals carry nothing").
///
/// The split is `med_split/3`, whose two branches' message streams merge into
/// the parent's — the "standard stream technique" the Definition uses without
/// comment.  For r such goals it emits r-1 splits, chained.
List<Goal> _compileBody(List<Goal> body, String med, _NameSource names,
    {required bool Function(String, int) isProcedureOfM,
    required int Function(String, int) slotCountOf}) {
  final indices = <int>[];
  for (var i = 0; i < body.length; i++) {
    final g = body[i];
    if (isProcedureOfM(g.functor, g.args.length)) indices.add(i);
  }

  final out = <Goal>[];
  // A body that is the single goal `true` is the guarded unit clause's idiom
  // and is empty; copied after the abort call it would be a call of true/0,
  // which no procedure defines.
  if (indices.isEmpty) {
    return [for (final g in body) if (!_isTrue(g)) g];
  }

  // One channel per calling goal, chained off `med`.
  final channels = <String>[];
  var current = med;
  for (var t = 0; t < indices.length; t++) {
    if (t == indices.length - 1) {
      channels.add(current);
    } else {
      final branch = names.fresh('${_medStem}b');
      final rest = names.fresh('${_medStem}r');
      out.add(Goal('med_split',
          [_r(current), _w(branch), _w(rest)], 0, 0));
      channels.add(branch);
      current = rest;
    }
  }

  var t = 0;
  for (var i = 0; i < body.length; i++) {
    final g = body[i];
    if (!isProcedureOfM(g.functor, g.args.length)) {
      if (!_isTrue(g)) out.add(g);
      continue;
    }
    final ch = channels[t++];
    final slots = slotCountOf(g.functor, g.args.length);
    out.add(Goal(g.functor, [
      _r(ch),
      ...g.args,
      for (var k = 0; k < slots; k++) ConstTerm('none', g.line, g.column),
    ], g.line, g.column));
  }
  return out;
}

bool _isTrue(Goal g) => g.functor == 'true' && g.args.isEmpty;

/// Emit the abort call and return the name of the channel the body continues
/// on.  With no slots to abort the call is the identity, so it is omitted.
String _abortsInto(List<Goal> body, List<String> slotsInList,
    String med, _NameSource names) {
  if (slotsInList.isEmpty) return med;
  final med1 = names.fresh('${_medStem}1');
  body.add(Goal('aborts', [
    _list(slotsInList.map<Term>((s) => _r(s)).toList()),
    _r(med),
    _w(med1),
  ], 0, 0));
  return med1;
}

// ---------------------------------------------------------------------------
// Pieces of the compiled terms
// ---------------------------------------------------------------------------

Atom _extendHead(Atom head, String med, List<Term> slotArgs) =>
    Atom(head.functor, [_w(med), ...head.args, ...slotArgs],
        head.line, head.column);

Term _ask(Term reply, Term id) => StructTerm('ask', [reply, id], 0, 0);
Term _then(Term xs) => StructTerm('then', [xs], 0, 0);

/// `xs_C(X1, ..., Xi)` over the clause's answer writers, an anonymous writer
/// at each position the volition guard leaves anonymous; the bare constant
/// `xs_C` for an empty question.  The functor is the clause's own, so that the
/// program's answer type, a union over its clauses, has distinct functors.
Term _xs(VolitionGuard g, Clause c, String name) => g.question.isEmpty
    ? ConstTerm('xs_$name', c.line, c.column)
    : StructTerm('xs_$name', [
        for (final q in g.question)
          q.writer == null
              ? UnderscoreTerm(c.line, c.column)
              : _w(q.writer!.name)
      ], c.line, c.column);

Term _anon(Clause c) => UnderscoreTerm(c.line, c.column);

VarTerm _w(String name) => VarTerm(name, false, 0, 0);
VarTerm _r(String name) => VarTerm(name, true, 0, 0);

Term _list(List<Term> items) {
  Term acc = ListTerm(null, null, 0, 0);
  for (var i = items.length - 1; i >= 0; i--) {
    acc = ListTerm(items[i], acc, 0, 0);
  }
  return acc;
}

/// The reader image of a head input pattern: every writer the head bound is
/// passed on as its reader, structure and constants unchanged.
Term _readerImage(Term t) {
  if (t is VarTerm) return t.isReader ? t : _r(t.name);
  if (t is StructTerm) {
    return StructTerm(t.functor, t.args.map(_readerImage).toList(),
        t.line, t.column);
  }
  if (t is ListTerm) {
    if (t.isNil) return t;
    return ListTerm(t.head == null ? null : _readerImage(t.head!),
        t.tail == null ? null : _readerImage(t.tail!), t.line, t.column);
  }
  return t;
}

/// Give every anonymous writer of a head input pattern a name, so that the
/// re-posed goal can rebuild the pattern from readers.
Term _nameAnonymous(Term t, _NameSource names) {
  if (t is UnderscoreTerm && !t.isReader) {
    return _w(names.fresh('V'));
  }
  if (t is StructTerm) {
    return StructTerm(t.functor,
        t.args.map((a) => _nameAnonymous(a, names)).toList(), t.line, t.column);
  }
  if (t is ListTerm) {
    if (t.isNil) return t;
    return ListTerm(t.head == null ? null : _nameAnonymous(t.head!, names),
        t.tail == null ? null : _nameAnonymous(t.tail!, names),
        t.line, t.column);
  }
  return t;
}

/// The names of the clause's answer writers — the named positions of its
/// volition guard's question.
Set<String> _answerWriterNames(VolitionGuard g) => {
      for (final q in g.question)
        if (q.writer != null) q.writer!.name
    };

/// `G_c`: the conjuncts of G that read no answer position.
List<Guard> _contextGuards(Clause c, Set<String> answerWriters) {
  final out = <Guard>[];
  for (final g in c.guards ?? const <Guard>[]) {
    if (g.args.any((t) => _readsAny(t, answerWriters))) continue;
    out.add(g);
  }
  return out;
}

bool _readsAny(Term t, Set<String> names) {
  if (t is VarTerm) return t.isReader && names.contains(t.name);
  if (t is StructTerm) return t.args.any((a) => _readsAny(a, names));
  if (t is ListTerm) {
    return (t.head != null && _readsAny(t.head!, names)) ||
        (t.tail != null && _readsAny(t.tail!, names));
  }
  return false;
}

/// The substitution putting the else answer T'_l in place of X_l?, at the named
/// positions of the clause's question (Definition "Guarded Clause, ...": the
/// else clause is the clause with T'_l for X_l).
Map<String, Term> _elseSubstitution(Clause c) {
  final subst = <String, Term>{};
  final q = c.volitionGuard!.question;
  final answer = c.elseBranch!.answer;
  for (var l = 0; l < q.length && l < answer.length; l++) {
    final w = q[l].writer;
    if (w != null) subst[w.name] = answer[l];
  }
  return subst;
}

Term _substTerm(Term t, Map<String, Term> subst) {
  if (t is VarTerm && t.isReader && subst.containsKey(t.name)) {
    return subst[t.name]!;
  }
  if (t is StructTerm) {
    return StructTerm(t.functor,
        t.args.map((a) => _substTerm(a, subst)).toList(), t.line, t.column);
  }
  if (t is ListTerm) {
    if (t.isNil) return t;
    return ListTerm(t.head == null ? null : _substTerm(t.head!, subst),
        t.tail == null ? null : _substTerm(t.tail!, subst), t.line, t.column);
  }
  return t;
}

// ---------------------------------------------------------------------------
// Fresh names
// ---------------------------------------------------------------------------

/// Names the compilation adds are made fresh against the clause's own
/// variables, so that a program using `Med` or `S1` itself still compiles.
class _NameSource {
  final Set<String> _taken = {};

  _NameSource(Clause c) {
    void scanTerm(Term t) {
      if (t is VarTerm) _taken.add(t.name);
      if (t is StructTerm) t.args.forEach(scanTerm);
      if (t is ListTerm) {
        if (t.head != null) scanTerm(t.head!);
        if (t.tail != null) scanTerm(t.tail!);
      }
    }

    c.head.args.forEach(scanTerm);
    for (final g in c.guards ?? const <Guard>[]) {
      g.args.forEach(scanTerm);
    }
    for (final g in c.body ?? const <Goal>[]) {
      g.args.forEach(scanTerm);
    }
    if (c.elseBranch != null) {
      for (final g in c.elseBranch!.body) {
        g.args.forEach(scanTerm);
      }
      c.elseBranch!.answer.forEach(scanTerm);
    }
    if (c.volitionGuard != null) {
      for (final q in c.volitionGuard!.question) {
        if (q.writer != null) _taken.add(q.writer!.name);
        if (q.value != null) scanTerm(q.value!);
      }
      for (final v in c.volitionGuard!.context) {
        _taken.add(v.name);
      }
    }
  }

  String fresh(String stem) {
    if (!_taken.contains(stem)) {
      _taken.add(stem);
      return stem;
    }
    var n = 1;
    while (_taken.contains('$stem$n')) {
      n++;
    }
    _taken.add('$stem$n');
    return '$stem$n';
  }
}
