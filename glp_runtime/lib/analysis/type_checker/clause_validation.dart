// lib/analysis/type_checker/clause_validation.dart
//
// Validates Term AST nodes in program clause contexts.
// Specification: TGLP (Moded-Types), sections/typed-glp.tex, "Anonymous
// variables" under SRSW Relaxations, and sections/well-typing.tex
// Definition "Moded Head".

import '../../compiler/ast.dart';
import '../../compiler/error.dart';
import 'mode.dart';
import 'moded_head.dart';
import 'moded_term.dart';
import 'type_ast.dart';

/// Validates a clause head.
///
/// TGLP "Anonymous variables": an anonymous variable denotes a fresh writer
/// with no paired reader.  In a clause head a produced position carries an
/// output placeholder rather than a writer, so an anonymous variable is
/// written `_?` there and denotes an output the clause never produces;
/// complementation (Definition "Moded Head", step 2) makes it that fresh
/// writer.  An anonymous reader at a consumed position remains forbidden: a
/// reader with no writer is a goal that can never be satisfied.
///
/// The position's mode is the mode of the moded head, so the declaration is
/// what decides: without one no position has a mode and every anonymous
/// reader is refused.
void validateClauseHead(Atom head, ProcDecl? decl, TypeEnvironment? typeEnv) {
  final moded = _modedHeadOrNull(head, decl, typeEnv);
  if (moded == null) {
    for (final arg in head.args) {
      _checkAnonymousReader(arg, null);
    }
    return;
  }
  for (int i = 0; i < head.args.length; i++) {
    _checkAnonymousReader(head.args[i], moded.args[i]);
  }
}

/// Validates a term in clause body context.
///
/// Rejects an anonymous reader, which in a body would be a reader with no
/// writer at a consumed position.  Allows `_` (anonymous writer).
void validateClauseBody(Term term) {
  _checkAnonymousReader(term, null);
}

/// Validates a term in guard context.
///
/// A guard is type-checked as part of the body (TGLP "Type checking of
/// guards"), so an anonymous reader is rejected there too.
void validateGuard(Term term) {
  _checkAnonymousReader(term, null);
}

/// The moded head for [head], or null where it cannot be built — no
/// declaration, a mismatched arity, or a term shape the construction does not
/// know.  A null moded head gives no position a mode.
ModedCompound? _modedHeadOrNull(
    Atom head, ProcDecl? decl, TypeEnvironment? typeEnv) {
  if (decl == null || head.arity != decl.arity) return null;
  ModedTerm built;
  try {
    built = modedHead(
        Goal(head.functor, head.args, head.line, head.column), decl,
        typeEnv: typeEnv);
  } on ArityMismatchError {
    return null;
  } on InvalidHeadError {
    return null;
  }
  if (built is! ModedCompound) return null;
  if (built.args.length != head.args.length) return null;
  return built;
}

/// Walk [term] beside its moded counterpart [moded], refusing every anonymous
/// reader whose position is not produced.  [moded] is null where the position
/// has no mode, and then every anonymous reader below it is refused.
void _checkAnonymousReader(Term term, ModedTerm? moded) {
  if (_isAnonymousReader(term)) {
    if (moded != null && moded.mode == Mode.produce) return;
    throw CompileError(
      '${_spelling(term)} (anonymous reader) is not permitted in program clauses',
      term.line,
      term.column,
      phase: 'validation',
    );
  }

  if (term is StructTerm) {
    final args = _modedArgs(moded, term.functor, term.arity);
    for (int i = 0; i < term.args.length; i++) {
      _checkAnonymousReader(term.args[i], args == null ? null : args[i]);
    }
    return;
  }

  if (term is ListTerm) {
    if (term.head == null && term.tail == null) return;
    final args = _modedArgs(moded, '[|]', 2);
    if (term.head != null) {
      _checkAnonymousReader(term.head!, args == null ? null : args[0]);
    }
    if (term.tail != null) {
      _checkAnonymousReader(term.tail!, args == null ? null : args[1]);
    }
  }
}

/// The moded arguments of [moded] when it is the compound [functor]/[arity],
/// and null otherwise — the moded term and the source term having parted
/// company, no position below carries a mode.
List<ModedTerm>? _modedArgs(ModedTerm? moded, String functor, int arity) {
  if (moded is! ModedCompound) return null;
  if (moded.functor != functor || moded.arity != arity) return null;
  if (moded.args.length != arity) return null;
  return moded.args;
}

/// An anonymous variable in its reader form: bare `_?`, or a named anonymous
/// reader such as `_Out?` — an anonymous variable being any variable whose
/// name begins with `_` (TGLP "Anonymous variables").
bool _isAnonymousReader(Term term) {
  if (term is UnderscoreTerm) return term.isReader;
  if (term is VarTerm) return term.name.startsWith('_') && term.isReader;
  return false;
}

String _spelling(Term term) => term is VarTerm ? '${term.name}?' : '_?';
