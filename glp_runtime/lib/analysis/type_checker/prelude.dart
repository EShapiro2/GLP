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

% Output and Input types
Output ::= _.
Input ::= _?.

% Collections
List ::= [_ | List] ; [].
Stream ::= [_ | Stream] ; [].
DiffList ::= List \ List?.

% Communication
Channel ::= ch(Stream?, Stream).

% =============================================================================
% Predefined Procedures (usable as defined guards)
% =============================================================================

% Ground guard
procedure ground(Input).

% Unification (output = input)
procedure =(_, _?).
X? = X.

% Difference list operations
procedure dl_append(DiffList?, DiffList?, DiffList).
procedure dl_to_list(DiffList?, List).

dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channel operations
procedure new_channel(Channel, Channel).
procedure send(_?, Channel?, Channel).
procedure receive(_, Channel?, Channel).

new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
''';

/// Names of predefined types that cannot be redefined by user modules
const Set<String> predefinedTypeNames = {
  'Number',   // Primitive builtin
  'String',   // Primitive builtin
  'Output',   // All writers type
  'Input',    // All readers type
  'List',     // Collection type
  'Stream',   // Collection type
  'DiffList', // Collection type
  'Channel',  // Communication type
};

/// Names of predefined procedures that cannot be redefined by user modules
const Set<String> predefinedProcedureNames = {
  '=',
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
