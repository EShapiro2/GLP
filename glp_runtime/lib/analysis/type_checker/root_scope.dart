// lib/analysis/type_checker/root_scope.dart
//
// Predefined type and procedure definitions for GLP.
// These are prepended to every module before parsing.
// Redefinition of predefined types/procedures is an error.
//
// Specification: docs/modules/type-environment.md
// Paper Reference: Section 8 (Root scope)

/// The root scope source — now empty.
/// All type definitions, procedure declarations, and unit clauses
/// live in programs/self.glp and are loaded via the scope chain.
const String rootScopeTypes = '';

/// Names of predefined types that cannot be redefined by user modules
/// Note: Only fundamental primitive types are protected.
/// Library-level types (DiffList, Channel) can be redefined by user programs.
const Set<String> predefinedTypeNames = {
  'Integer',  // Primitive builtin
  'Real',     // Primitive builtin
  'String',   // Primitive builtin
  'Module',   // Primitive builtin
  'Number',   // Root self.glp union: Integer ; Real
  'Constant', // Root self.glp union: Number ; String ; Module
  'Exp',      // Root self.glp union: arithmetic expressions
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
  // Attestation guard (madGLP, seam spec §4)
  'valid_attestation',
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
  // Lexicographic comparison of ground constants
  '@<',
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
/// with root scope clauses (like new_channel).
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
  // Attestation guard (madGLP, seam spec §4)
  'valid_attestation/4',
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
  // Lexicographic comparison of ground constants
  '@</2',
  // Structural equality guard
  '=?=/2',
  // Univ operations
  '=../2',
  '..=/2',
  // MWM (Mutual Write Merge) guard
  'is_mutual_ref/1',
  // Body kernels ('_'-prefixed) are NOT listed: they are undeclared everywhere
  // and resolve from the runtime (runtime-base convention, e28b135f) — this set
  // only licenses clause-less DECLARATIONS, which kernels no longer have.
};

/// Reserved constant base-names: the `_`-prefixed names that denote a language
/// primitive (kernel predicate or reserved functor). Per TGLP appendix
/// "Admission to the Primitive Layer": a constant is reserved iff it NAMES a
/// primitive — a `_`-prefixed constant that names none (e.g. `'_user'`, `'_net'`)
/// is NOT reserved. Only modules under `-mode(system)` may name these; the
/// loader rejects any other module that does.
const Set<String> reservedConstantNames = {
  // MWM (Mutual Write Merge) runtime kernels
  '_allocate_mutual_reference',
  '_stream_append',
  '_close_mutual_reference',
  // madGLP network kernels and reserved global-name functors
  '_send',
  '_w',
  '_r',
  // attestation kernel (seam spec §4)
  '_sign',
  // output system predicate
  '_output',
  // module-dispatch kernels
  '_select',
  // module-as-value kernels (modules.tex §Dynamic Activation, "Modules as
  // values"): `_self_module` yields the running module as a `Module` value,
  // `_run` starts a goal on such a value.
  '_run',
  '_self_module',
};

/// True if [name] names a language primitive and so is a reserved constant.
bool isReservedConstantName(String name) => reservedConstantNames.contains(name);

/// Check if a type name is predefined
bool isPredefinedType(String name) => predefinedTypeNames.contains(name);

/// Check if a goal name is a builtin that doesn't need type checking
bool isBuiltinGoal(String name) => builtinGoals.contains(name);

/// Check if a procedure name/arity is predefined
bool isPredefinedProcedure(String name) => predefinedProcedureNames.contains(name);

/// Check if a procedure (name/arity) is a true builtin (implemented in Dart, no GLP clauses)
bool isBuiltinProcedure(String nameArity) => builtinProcedures.contains(nameArity);
