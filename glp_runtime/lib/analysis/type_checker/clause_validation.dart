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
/// Rejects both _ and _? in clause bodies:
/// - _? is never permitted in programs
/// - _ in body would produce unbound output value
void validateClauseBody(Term term) {
  _checkNoAnonymousReader(term);
  _checkNoAnonymousInBody(term);
}

/// Validates a term in guard context.
/// 
/// Rejects _? (anonymous reader) which is never permitted in programs.
/// Allows _ (anonymous variable) in guards.
void validateGuard(Term term) {
  _checkNoAnonymousReader(term);
}

/// Recursively check for _? and throw if found.
void _checkNoAnonymousReader(Term term) {
  if (term is UnderscoreTerm && term.isReader) {
    throw CompileError(
      '_? (anonymous reader) is not permitted in program clauses',
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

/// Recursively check for _ in body and throw if found.
void _checkNoAnonymousInBody(Term term) {
  if (term is UnderscoreTerm && !term.isReader) {
    throw CompileError(
      'Anonymous variable _ is not permitted in clause body (violates SRSW: no paired reader)',
      term.line,
      term.column,
      phase: 'validation',
    );
  }
  
  if (term is StructTerm) {
    for (final arg in term.args) {
      _checkNoAnonymousInBody(arg);
    }
  }
  
  if (term is ListTerm) {
    if (term.head != null) _checkNoAnonymousInBody(term.head!);
    if (term.tail != null) _checkNoAnonymousInBody(term.tail!);
  }
}
