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
List ::= [_ | List] ; [].
Stream ::= [_ | Stream] ; [].
DiffList ::= List \ List?.

% Communication
Channel ::= ch(Stream?, Stream).

% =============================================================================
% PROCEDURE DECLARATIONS
% =============================================================================

% Type guards
procedure integer(Integer?).
procedure number(Number?).
procedure string(String?).
procedure atom(String?).
procedure constant(Number?).
procedure compound(_?).
procedure is_list(List?).

% Groundness guards
procedure ground(_?).
procedure known(_?).
procedure unknown(_?).

% Arithmetic comparison guards
procedure <(Number?, Number?).
procedure >(Number?, Number?).
procedure =<(Number?, Number?).
procedure >=(Number?, Number?).
procedure =:=(Number?, Number?).
procedure =\=(Number?, Number?).

% Equality guard
procedure =?=(_, _).

% Unification
procedure =(_, _?).

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(_?, Channel?, Channel).
procedure receive(_, Channel?, Channel).

% =============================================================================
% CLAUSES
% =============================================================================

% Unification clause
X? = X.

% Difference list clauses
dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel clauses
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types that cannot be redefined by user modules
const Set<String> predefinedTypeNames = {
  'Number',   // Primitive builtin
  'Integer',  // Primitive builtin
  'Real',     // Primitive builtin
  'String',   // Primitive builtin
  'List',     // Collection type
  'Stream',   // Collection type
  'DiffList', // Collection type
  'Channel',  // Communication type
};

/// Names of predefined procedures that cannot be redefined by user modules
const Set<String> predefinedProcedureNames = {
  // Type guards
  'integer',
  'number',
  'string',
  'atom',
  'constant',
  'compound',
  'is_list',
  // Groundness guards
  'ground',
  'known',
  'unknown',
  // Comparison guards
  '<',
  '>',
  '=<',
  '>=',
  '=:=',
  '=\\=',
  // Equality
  '=?=',
  '=',
  // Difference list
  'dl_append',
  'dl_to_list',
  // Channel
  'new_channel',
  'send',
  'receive',
};

/// Built-in goals that don't need type checking
/// - true, otherwise: 0-arity control
/// - :=: arithmetic assignment, handled specially
const Set<String> builtinGoals = {
  'true',
  'otherwise',
  ':=',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);
