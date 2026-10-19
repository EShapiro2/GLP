// lib/analysis/type_checker/clause_validation.dart
//
// Validates Term AST nodes in program clause contexts.
// Specification: GLP-Spec sections/glp.tex, Remark "Anonymous Variables"
// (297db75), and TGLP sections/full/typed-anonymous-variables.tex,
// "Anonymous variables" (2424f4c).

import '../../compiler/ast.dart';
import '../../compiler/error.dart';

/// Validates a clause head.
///
/// GLP-Spec Remark "Anonymous Variables" and TGLP "Anonymous variables":
/// "Anonymous readers (`_?`, `_Name?`) are not permitted in a clause, in the
/// head or the body: a reader whose writer is never written is a goal that can
/// never be satisfied, so a clause writes every writer it holds, closing a
/// stream it does not extend."  So an anonymous reader is refused at every
/// position of a head, a produced position included, whatever the
/// declaration.  The initial state of an agent, `agent(ch(_?,_),ch(_?,_))`, is
/// a goal and not a clause, and the runtime writes its anonymous readers
/// (GLP-Spec maGLP): it is never validated here.
///
/// Until 2026-10-14 an anonymous reader was admitted at a produced head
/// position, as TGLP's text before 2424f4c had it.
void validateClauseHead(Atom head) {
  for (final arg in head.args) {
    _checkAnonymousReader(arg);
  }
}

/// Validates a term in clause body context: an anonymous reader is refused,
/// by the same remark.  `_` (anonymous writer) is allowed.
void validateClauseBody(Term term) {
  _checkAnonymousReader(term);
}

/// Validates a term in guard context.
///
/// A guard is type-checked as part of the body (TGLP "Type checking of
/// guards"), so an anonymous reader is refused there too.
void validateGuard(Term term) {
  _checkAnonymousReader(term);
}

/// Walk [term], refusing every anonymous reader in it.
///
/// A list is walked along its spine in a loop and its elements by recursive
/// calls, in the order the recursion took them, so the depth used is the
/// nesting of the term's elements and not the length of a list (TGLP
/// well-typing.tex, Definition "Well-Typed Clause", puts no bound on a term's
/// size).
void _checkAnonymousReader(Term term) {
  Term? t = term;
  while (t is ListTerm) {
    if (t.head != null) _checkAnonymousReader(t.head!);
    t = t.tail;
  }
  if (t == null) return;

  if (_isAnonymousReader(t)) {
    throw CompileError(
      '${_spelling(t)} (anonymous reader) is not permitted in program clauses',
      t.line,
      t.column,
      phase: 'validation',
    );
  }

  if (t is StructTerm) {
    for (final arg in t.args) {
      _checkAnonymousReader(arg);
    }
  }
}

/// An anonymous variable in its reader form: bare `_?`, or a named anonymous
/// reader such as `_Out?` --- an anonymous variable being any variable whose
/// name begins with `_` (GLP-Spec Remark "Anonymous Variables").
bool _isAnonymousReader(Term term) {
  if (term is UnderscoreTerm) return term.isReader;
  if (term is VarTerm) return term.name.startsWith('_') && term.isReader;
  return false;
}

String _spelling(Term term) => term is VarTerm ? '${term.name}?' : '_?';
