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
  // This set is what the runtime implements, in Dart, and nothing else. It does
  // not admit a GLP-implemented name: that would make one set mean two things —
  // what the runtime implements, and what may be declared without clauses — and
  // a set meaning two things checks neither (GLP-Spec, 2026-08-02).
  // send_to_net/1 was briefly here and is not; it reaches the tree through
  // -expose(system#mad_predicates) in root programs/self.glp.

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

  // ---------------------------------------------------------------------------
  // Body kernels
  // ---------------------------------------------------------------------------
  // Every kernel registered by `runtime/body_kernels.dart` and
  // `engine_v2/module_kernels.dart`, each with its clause-less declaration in
  // `programs/self.glp` (Udi, 2026-07-29). Kernels are runtime-implemented like
  // every other predicate here, and the body-atom type checker demands a
  // declaration for every body atom; without these a `programs/system/` module
  // that names a kernel fails to load with `Undefined procedure`.
  //
  // The signatures are GLP-Spec's `appendix-guards.tex` body-kernel table, which
  // is the authority. Its Network group also lists `'_peer_address'`,
  // `'_punch_udp'`, `'_place_declare'` and `'_place_remove'`; the table is ahead
  // of the code there and none of the four is registered, so none is listed —
  // this set is enumerated from the registry.
  //
  // Arithmetic
  '_add/3',
  '_sub/3',
  '_mul/3',
  '_div/3',
  '_idiv/3',
  '_mod/3',
  '_neg/2',
  // Math
  '_abs/2',
  '_sqrt/2',
  '_sin/2',
  '_cos/2',
  '_tan/2',
  '_exp/2',
  '_ln/2',
  '_log10/2',
  '_pow/3',
  '_asin/2',
  '_acos/2',
  '_atan/2',
  // Conversion
  '_integer/2',
  '_real/2',
  '_round/2',
  '_floor/2',
  '_ceil/2',
  // Structure
  '_list_to_tuple/2',
  '_tuple_to_list/2',
  // Copy
  '_copy/2',
  // Time and random
  '_now/1',
  '_random/4',
  // MWM (Mutual Write Merge)
  '_allocate_mutual_reference/2',
  '_stream_append/3',
  '_close_mutual_reference/1',
  // madGLP network
  '_send/3',
  '_authorise_link/2',
  // Signature
  '_sign/2',
  // Modules as values
  '_self_module/1',
  '_run/2',
  // I/O
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
