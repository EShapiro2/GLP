// lib/analysis/type_checker/prelude.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.
//
// Specification: docs/modules/type-environment.md
// Paper Reference: Section 8 (Prelude)

/// The prelude source — now empty.
/// All type definitions, procedure declarations, and unit clauses
/// live in programs/self.glp and are loaded via the scope chain.
const String typePrelude = '';

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
  'module',
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
  // Map operations (fundamental builtins)
  'map_new',
  'map_put',
  '_map_get',
  'map_contains',
  'map_remove',
  'map_keys',
  'map_show',
  'fofmap_show',
  'map_list_append',
  // Structure manipulation
  '_copy',
  '_list_to_tuple',
  '_tuple_to_list',
  'struct_arg',
  'struct_arg_eq',
  'map_entry_arg_eq',
  'map_entry_arg_ge',
  // SharedBroadcastStream
  'sbs_new',
  'sbs_add_recipient',
  'sbs_write_update',
  'sbs_get_checkpoint',
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
  'module/1',
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
  // Map operations (O(1) key-value lookup)
  'map_new/1',
  'map_put/4',
  '_map_get/3',
  'map_contains/2',
  'map_get/3',
  'map_remove/3',
  'map_keys/2',
  'map_show/3',
  'fofmap_show/3',
  'map_list_append/4',
  // Structure manipulation
  '_copy/2',
  '_list_to_tuple/2',
  '_tuple_to_list/2',
  'struct_arg/3',
  'struct_arg_eq/3',
  'map_entry_arg_eq/4',
  'map_entry_arg_ge/4',
  // SharedBroadcastStream
  'sbs_new/2',
  'sbs_add_recipient/3',
  'sbs_write_update/4',
  'sbs_get_checkpoint/2',
  // madGLP network primitives
  '_send/3',
  // Output (system predicate)
  '_output/1',
};

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);

/// Check if a procedure (name/arity) is a true builtin (implemented in Dart, no GLP clauses)
bool isBuiltinProcedure(String nameArity) => builtinProcedures.contains(nameArity);
