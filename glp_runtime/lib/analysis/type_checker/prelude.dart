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
procedure constant(Constant?).
procedure compound(_?).
procedure list(_?).
procedure equator(_?).

% Groundness guards
procedure ground(_?).
procedure known(_?).
procedure unknown(_?).
procedure no_readers(_?).

% Time guards
procedure wait(Number?).
procedure wait_until(Number?).

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

% madGLP network primitives
procedure _send(_?, _?, _?).

% =============================================================================
% SINGLE-UNIT-CLAUSE PROCEDURES (DEFINED GUARDS)
% =============================================================================
%
% These are regular procedures defined by exactly one clause with no guards and
% no body (a "unit clause").  They serve as defined guards: when called in guard
% position, the partial evaluator unfolds them at compile time.  In general,
% they are NOT expected to work as body predicates.
%
% The PE automatically includes these prelude unit clauses when processing any
% program, so user programs do not need to redefine them.  User programs may
% override a prelude unit clause by defining a procedure with the same
% name/arity.
%
% Example: new_channel(Ch1, Ch2) in a guard is unfolded at compile time to
% create cross-linked channel structures.
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
  'constant',
  'compound',
  'list',
  'equator',
  // Groundness guards (fundamental - implemented by runtime)
  'ground',
  'known',
  'unknown',
  'no_readers',
  // Time guards (fundamental - implemented by runtime)
  'wait',
  'wait_until',
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
  // Note: dl_append, dl_to_list, new_channel, send, receive
  // are NOT protected - they are library-level and can be redefined
};

/// Built-in goals that don't need type checking
/// - true, otherwise: 0-arity control
/// - :=: arithmetic assignment, handled specially
/// Note: # (remote module call) is handled as RemoteGoal before the builtin check
const Set<String> builtinGoals = {
  'true',
  'otherwise',
  ':=',
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
  'constant/1',
  'compound/1',
  'list/1',
  'equator/1',
  // Groundness/validation guards
  'ground/1',
  'known/1',
  'unknown/1',
  'no_readers/1',
  // Time guards
  'wait/1',
  'wait_until/1',
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
  // madGLP network primitives
  '_send/3',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);

/// Check if a procedure (name/arity) is a true builtin (implemented in Dart, no GLP clauses)
bool isBuiltinProcedure(String nameArity) => builtinProcedures.contains(nameArity);
