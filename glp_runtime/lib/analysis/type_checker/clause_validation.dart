// lib/analysis/type_checker/clause_validation.dart
//
// Validates Term AST nodes in program clause contexts.
// Per spec: /Users/udi/GLP/docs/type system/clause-validation.md

import '../../compiler/ast.dart';
import '../../compiler/error.dart';

/// Validates a term in clause head context.
/// 
/// Rejects _? (anonymous reader) which is never permitted in programs.
/// Allows _ (anonymous variable) to discard input values.
void validateClauseHead(Term term) {
  _checkNoAnonymousReader(term);
}

/// Validates a term in clause body context.
/// 
/// Rejects _? (anonymous reader) which is never permitted in programs.
/// Allows _ (anonymous variable) to discard output values.
void validateClauseBody(Term term) {
  _checkNoAnonymousReader(term);
}

/// Validates a term in guard context.
/// 
/// Rejects _? (anonymous reader) which is never permitted in programs.
/// Allows _ (anonymous variable) in guards.
void validateGuard(Term term) {
  _checkNoAnonymousReader(term);
}

/// Recursively check for anonymous readers and throw if found.
/// Per typed-glp-manual Section 9: Anonymous writers are allowed, anonymous readers are not.
/// This applies to both bare _? and named anonymous readers like _X?.
void _checkNoAnonymousReader(Term term) {
  if (term is UnderscoreTerm && term.isReader) {
    throw CompileError(
      '_? (anonymous reader) is not permitted in program clauses',
      term.line,
      term.column,
      phase: 'validation',
    );
  }

  // Named anonymous readers (_X?) are also rejected
  if (term is VarTerm && term.name.startsWith('_') && term.isReader) {
    throw CompileError(
      '${term.name}? (anonymous reader) is not permitted in program clauses',
      term.line,
      term.column,
      phase: 'validation',
    );
  }

  if (term is StructTerm) {
    for (final arg in term.args) {
      _checkNoAnonymousReader(arg);
    }
  }

  if (term is ListTerm) {
    if (term.head != null) _checkNoAnonymousReader(term.head!);
    if (term.tail != null) _checkNoAnonymousReader(term.tail!);
  }
}
