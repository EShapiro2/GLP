// lib/analysis/type_checker/prelude.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.
//
// Specification: docs/modules/type-environment.md
// Paper Reference: Section 4.x (Guards)

/// The prelude source containing all predefined definitions
///
/// IMPORTANT: All type definitions and procedure declarations must come BEFORE
/// any clauses. The parser processes declarations first, then clauses.
const String typePrelude = r'''
% =============================================================================
% TYPE DEFINITIONS
% =============================================================================

% Collections
Stream ::= [] ; [_|Stream].
OpenStream ::= [_|Stream].
DiffList ::= Stream \ Stream?.

% Communication
Channel ::= ch(Stream?, Stream).

% Primitive types
Constant ::= Number ; String.

% Arithmetic expressions
% Exp accepts numeric literals and arithmetic operator expressions
% Note: Arguments are NOT moded (Exp, not Exp?) because expression types
% are homogeneous - consuming an expression means consuming its subexpressions.
Exp ::= Number ; +(Exp, Exp) ; -(Exp, Exp) ; *(Exp, Exp) ; /(Exp, Exp) ; //(Exp, Exp) ; mod(Exp, Exp) ; neg(Exp).

% =============================================================================
% PROCEDURE DECLARATIONS
% =============================================================================

% Type guards
procedure integer(Integer?).
procedure number(Number?).
procedure string(String?).
procedure atom(String?).
procedure constant(Constant?).
procedure compound(_?).
procedure is_list(Stream?).

% Groundness guards
procedure ground(_?).
procedure known(_?).
procedure unknown(_?).

% Arithmetic comparison guards
procedure <(Exp?, Exp?).
procedure >(Exp?, Exp?).
procedure =<(Exp?, Exp?).
procedure >=(Exp?, Exp?).
procedure =:=(Exp?, Exp?).
procedure =\=(Exp?, Exp?).

% Equality guard
procedure =?=(_?, _?).

% Univ operations (term ↔ list conversion)
procedure =..(_, Stream?).      % Compose: Stream? → Compound
procedure ..=(Stream, _?).      % Decompose: Compound? → Stream

% Defined guards and system predicates
% These have clauses below. They are unfolded when used in guard position
% (before |) but execute as normal goals in body position (after |).
% Procedure declarations are required so body uses can be type-checked.
procedure =(_, _?).
procedure dl_append(_?, _?, _).
procedure dl_to_list(_?, _).
procedure new_channel(_, _).
procedure send(_, _, _).
procedure receive(_, _, _).

% =============================================================================
% CLAUSES
% =============================================================================

% Unification clause
X = X?.

% Difference list clauses
dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel clauses
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types that cannot be redefined by user modules
/// Note: Only fundamental primitive types are protected.
/// Library-level types (DiffList, Channel) can be redefined by user programs.
const Set<String> predefinedTypeNames = {
  'Number',   // Primitive builtin
  'Integer',  // Primitive builtin
  'Real',     // Primitive builtin
  'String',   // Primitive builtin
  'Constant', // Primitive builtin (Number ; String)
  'Exp',      // Arithmetic expression type
  'Stream',   // Fundamental collection type
  'OpenStream', // Non-empty stream
  // Note: DiffList, Channel are NOT protected - they are library-level
};

/// Names of predefined procedures that cannot be redefined by user modules
/// Note: Only truly fundamental guards/operations are protected.
/// Library-level operations (channels, diff-lists) can be redefined by user programs.
const Set<String> predefinedProcedureNames = {
  // Type guards (fundamental - implemented by runtime)
  'integer',
  'number',
  'string',
  'atom',
  'constant',
  'compound',
  // Groundness guards (fundamental - implemented by runtime)
  'ground',
  'known',
  'unknown',
  // Comparison guards (fundamental - implemented by runtime)
  '<',
  '>',
  '=<',
  '>=',
  '=:=',
  '=\\=',
  // Equality (fundamental)
  '=?=',
  '=',
  // Univ operations (fundamental)
  '=..',
  '..=',
  // Note: is_list, dl_append, dl_to_list, new_channel, send, receive
  // are NOT protected - they are library-level and can be redefined
};

/// Built-in goals that don't need type checking
/// - true, otherwise: 0-arity control
/// - :=: arithmetic assignment, handled specially
const Set<String> builtinGoals = {
  'true',
  'otherwise',
  ':=',
};

/// True builtins: procedures implemented in Dart runtime with NO GLP clauses.
/// These are distinct from predefinedProcedureNames which includes procedures
/// with prelude clauses (like =, dl_append, etc.).
/// Keyed by "name/arity" for precise matching.
const Set<String> builtinProcedures = {
  // Type guards
  'integer/1',
  'number/1',
  'string/1',
  'atom/1',
  'constant/1',
  'compound/1',
  'is_list/1',
  // Groundness guards
  'ground/1',
  'known/1',
  'unknown/1',
  // Arithmetic comparison guards
  '</2',
  '>/2',
  '=</2',
  '>=/2',
  '=:=/2',
  '=\\=/2',
  // Structural equality guard
  '=?=/2',
  // Univ operations
  '=../2',
  '..=/2',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);

/// Check if a procedure (name/arity) is a true builtin (implemented in Dart, no GLP clauses)
bool isBuiltinProcedure(String nameArity) => builtinProcedures.contains(nameArity);
