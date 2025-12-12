/// Phase 2: Unified Instruction Set
///
/// This file contains the v2 instruction set with unified variable instructions.
/// Key change: Instead of separate Writer/Reader instructions, use a single
/// instruction with an isReader flag.
///
/// Benefits:
/// - Simpler instruction set (fewer opcodes)
/// - Better code reuse in interpreter
/// - Clearer semantics
/// - Foundation for register allocation optimizations

library;

/// Base interface for v2 instructions
/// All v2 instructions implement this to distinguish them from v1 Op
abstract class OpV2 {}

// ============================================================================
// HEAD PHASE - Unified Instructions
// ============================================================================

/// Match variable in clause head (unifies HeadWriter and HeadReader)
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Tentatively bind in σ̂w
/// - isReader=true (reader): Add to Si if unbound
class HeadVariable implements OpV2 {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  HeadVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'head_reader' : 'head_writer';

  @override
  String toString() => '$mnemonic($varIndex)';
}

/// Get variable from argument register - first occurrence (unifies GetWriterVariable and GetReaderVariable)
/// Used in HEAD phase to load argument into clause variable for first occurrence
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Load argument as writer into varIndex
/// - isReader=true (reader): Load argument as reader into varIndex
class GetVariable implements OpV2 {
  final int varIndex;    // clause variable index
  final int argSlot;     // argument register
  final bool isReader;   // true for reader mode, false for writer mode

  GetVariable(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'get_reader_variable' : 'get_writer_variable';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

/// Get value from argument register - subsequent occurrence (unifies GetWriterValue and GetReaderValue)
/// Used in HEAD phase to unify argument with existing clause variable
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Unify argument with writer in varIndex
/// - isReader=true (reader): Unify argument with reader in varIndex
class GetValue implements OpV2 {
  final int varIndex;    // clause variable index
  final int argSlot;     // argument register
  final bool isReader;   // true for reader mode, false for writer mode

  GetValue(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'get_reader_value' : 'get_writer_value';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

// ============================================================================
// STRUCTURE TRAVERSAL - Unified Instructions
// ============================================================================

/// Match variable at current S position in structure (unifies UnifyWriter and UnifyReader)
/// Operates in READ or WRITE mode based on HeadStructure/PutStructure context
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Unify with writer variable
/// - isReader=true (reader): Unify with reader variable (may add to Si)
class UnifyVariable implements OpV2 {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  UnifyVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'unify_reader' : 'unify_writer';

  @override
  String toString() => '$mnemonic($varIndex)';
}

// ============================================================================
// BODY PHASE - Unified Instructions
// ============================================================================

/// Place variable into argument register (unifies PutWriter and PutReader)
/// Used in BODY phase to pass variables to spawned goals
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Place writer from varIndex into argSlot
/// - isReader=true (reader): Derive reader from writer, place into argSlot
class PutVariable implements OpV2 {
  final int varIndex;    // clause variable index holding writer ID
  final int argSlot;     // target argument register
  final bool isReader;   // true for reader mode, false for writer mode

  PutVariable(this.varIndex, this.argSlot, {required this.isReader});

  String get mnemonic => isReader ? 'put_reader' : 'put_writer';

  @override
  String toString() => '$mnemonic(X$varIndex, A$argSlot)';
}

/// Build structure argument (unifies SetWriter and SetReader)
/// Used in BODY phase WRITE mode to construct structure subterms
/// Behavior depends on isReader flag:
/// - isReader=false (writer): Create writer, store in varIndex, add WriterTerm to heap
/// - isReader=true (reader): Derive reader from writer in varIndex, add ReaderTerm to heap
class SetVariable implements OpV2 {
  final int varIndex;    // clause variable index
  final bool isReader;   // true for reader mode, false for writer mode

  SetVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'set_reader' : 'set_writer';

  @override
  String toString() => '$mnemonic(X$varIndex)';
}

// ============================================================================
// GUARD PHASE - Guard Instructions
// ============================================================================

/// Test if variable is unbound (value unknown)
/// Succeeds if the variable is unbound, fails if bound to any value.
/// Used for dispatch based on binding status.
class Unknown implements OpV2 {
  final int varIndex;    // clause variable index to test

  Unknown(this.varIndex);

  String get mnemonic => 'unknown';

  @override
  String toString() => 'unknown(X$varIndex)';
}

// ============================================================================
// Note: V1 migration functions removed - codegen now emits V2 directly
// ============================================================================
