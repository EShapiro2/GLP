// lib/analysis/type_checker/prelude.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.
//
// Specification: docs/modules/type-environment.md
// Paper Reference: Section 8 (Prelude)

/// The prelude source containing all predefined definitions
///
/// IMPORTANT: Procedure declarations must appear immediately before their clauses.
/// Type definitions can appear anywhere before first use.
/// Builtin procedure declarations (no clauses) can be grouped.
const String typePrelude = r'''
% =============================================================================
% TYPE DEFINITIONS
% =============================================================================

% Collections
Stream ::= [] ; [_|Stream].
OpenStream ::= [_|Stream].
DiffList ::= Stream \ Stream?.

% Communication
Channel ::= ch(Stream, Stream?).

% Primitive types
Constant ::= Number ; String.

% Arithmetic expressions
% Exp accepts numeric literals and arithmetic operator expressions
% Note: Arguments are NOT moded (Exp, not Exp?) because expression types
% are homogeneous - consuming an expression means consuming its subexpressions.
Exp ::= Number ; +(Exp, Exp) ; -(Exp, Exp) ; *(Exp, Exp) ; /(Exp, Exp) ; //(Exp, Exp) ; mod(Exp, Exp) ; neg(Exp).

% =============================================================================
% BUILTIN PROCEDURE DECLARATIONS (no clauses - implemented by runtime)
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
procedure no_readers(_?).

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

% MWM (Mutual Write Merge) runtime primitives
procedure _allocate_mutual_reference(_, _).
procedure is_mutual_ref(_?).
procedure _stream_append(_?, _?, _).
procedure _close_mutual_reference(_?).

% Output/debugging primitives
procedure write(_?).
procedure writeln(_?).

% =============================================================================
% SINGLE-UNIT-CLAUSE PROCEDURES
% =============================================================================
%
% These are regular procedures with no special status. They happen to be defined
% by exactly one clause that has no guards and no body (a "unit clause").
%
% They can be called from either guard position or body position:
%
%   - IN GUARD POSITION: The partial evaluator verifies the procedure is defined
%     by a single unit clause, then unfolds it at compile time. If not a single
%     unit clause, PE reports an error.
%
%   - IN BODY POSITION: Called as a normal procedure at runtime.
%
% Example: new_channel(Ch1, Ch2) can appear in a guard (unfolded at compile time
% to create cross-linked channel structures) or in a body (executed at runtime).
%
% =============================================================================

% Assignment (unification)
procedure =(_?, _).
X = X?.

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
dl_append(A\B?, B\C?, A?\C).

procedure dl_to_list(DiffList?, Stream).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

procedure send(_?, Channel?, Channel).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(_, Channel?, Channel).
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
  'no_readers',
  // Comparison guards (fundamental - implemented by runtime)
  '<',
  '>',
  '=<',
  '>=',
  '=:=',
  '=\\=',
  // Equality (fundamental)
  '=?=',
  // Univ operations (fundamental)
  '=..',
  '..=',
  // Note: is_list, dl_append, dl_to_list, new_channel, send, receive
  // are NOT protected - they are library-level and can be redefined
};

/// Built-in goals that don't need type checking
/// - true, otherwise: 0-arity control
/// - :=: arithmetic assignment, handled specially
/// - #: remote module call (Module # Goal) - inner goal is in remote module
const Set<String> builtinGoals = {
  'true',
  'otherwise',
  ':=',
  '#',
};

/// True builtins: procedures implemented in Dart runtime with NO GLP clauses.
/// These are distinct from predefinedProcedureNames which includes procedures
/// with prelude clauses (like new_channel).
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
  // Groundness/validation guards
  'ground/1',
  'known/1',
  'unknown/1',
  'no_readers/1',
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
  // MWM (Mutual Write Merge) runtime primitives
  '_allocate_mutual_reference/2',
  'is_mutual_ref/1',
  '_stream_append/3',
  '_close_mutual_reference/1',
  // Output/debugging primitives
  'write/1',
  'writeln/1',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);

/// Check if a procedure (name/arity) is a true builtin (implemented in Dart, no GLP clauses)
bool isBuiltinProcedure(String nameArity) => builtinProcedures.contains(nameArity);
