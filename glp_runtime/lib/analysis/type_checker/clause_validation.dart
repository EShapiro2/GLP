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
void _checkAnonymousReader(Term term) {
  if (_isAnonymousReader(term)) {
    throw CompileError(
      '${_spelling(term)} (anonymous reader) is not permitted in program clauses',
      term.line,
      term.column,
      phase: 'validation',
    );
  }

  if (term is StructTerm) {
    for (final arg in term.args) {
      _checkAnonymousReader(arg);
    }
    return;
  }

  if (term is ListTerm) {
    if (term.head != null) _checkAnonymousReader(term.head!);
    if (term.tail != null) _checkAnonymousReader(term.tail!);
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
