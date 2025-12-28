// lib/analysis/type_checker/prelude.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.

/// The prelude source containing all predefined definitions
const String typePrelude = r'''
% =============================================================================
% Predefined Types
% =============================================================================

% Universal type (accepts any ground term with any mode)
Any ::= _ ; _?.

% Output and Input mode types
Output ::= _.
Input ::= _?.

% Collections
List ::= [] ; [Any | List].
Stream ::= [Any | Stream].
DiffList ::= List \ List?.

% Communication
Channel ::= ch(Stream?, Stream).

% =============================================================================
% Predefined Procedures (usable as defined guards)
% =============================================================================

% Ground guard
procedure ground(Input).

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(Any, Channel?, Channel).
procedure receive(Any, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types that cannot be redefined by user modules
const Set<String> predefinedTypeNames = {
  'Number',   // Primitive builtin
  'String',   // Primitive builtin
  'Any',      // Universal type
  'Output',   // Output mode type
  'Input',    // Input mode type
  'List',     // Collection type
  'Stream',   // Collection type
  'DiffList', // Collection type
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
