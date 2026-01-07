// lib/analysis/type_checker/prelude.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.
//
// Note: The paper explicitly states that "Any ::= _ ; _?" is ILLEGAL
// because it violates BNF determinism (same position, different modes).
// Therefore we do NOT define a universal type. Users should use
// _ (output primitive) and _? (input primitive) directly in type definitions.

/// The prelude source containing all predefined definitions
const String typePrelude = r'''
% =============================================================================
% Predefined Types
% =============================================================================

% Primitive output type (program produces value)
Output ::= _.

% Primitive input type (program consumes value)
Input ::= _?.

% List with output-mode elements
List ::= [] ; [_ | List].

% Stream is just List (no ::< subtype syntax per paper)
Stream ::= [] ; [_ | Stream].

% Difference list: content list with input-mode hole
DiffList ::= List \ List?.

% Channel for bidirectional communication
Channel ::= ch(Stream?, Stream).

% =============================================================================
% Predefined Procedures (usable as defined guards)
% =============================================================================

% Ground guard - argument must be recursively ground
procedure ground(Input).

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(Output, Channel?, Channel).
procedure receive(Input, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types that cannot be redefined by user modules
const Set<String> predefinedTypeNames = {
  'Number',   // Primitive builtin
  'String',   // Primitive builtin
  'Output',   // Primitive output type
  'Input',    // Primitive input type
  'List',     // Collection type
  'Stream',   // Collection type
  'DiffList', // Difference list type
  'Channel',  // Communication type
};

/// Names of predefined procedures that cannot be redefined by user modules
const Set<String> predefinedProcedureNames = {
  'ground',
  'dl_append',
  'dl_to_list',
  'new_channel',
  'send',
  'receive',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);
