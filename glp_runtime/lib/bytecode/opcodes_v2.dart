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

import 'opcodes.dart';

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
// GUARD PHASE - Unified Instructions
// ============================================================================

/// Type test guard (unifies IfWriter and IfReader)
/// Tests if variable has expected type (writer vs reader)
/// Behavior depends on isReader flag:
/// - isReader=false: Succeed if variable is a writer
/// - isReader=true: Succeed if variable is a reader
class IfVariable implements OpV2 {
  final int varIndex;    // clause variable index to test
  final bool isReader;   // true to test for reader, false to test for writer

  IfVariable(this.varIndex, {required this.isReader});

  String get mnemonic => isReader ? 'if_reader' : 'if_writer';

  @override
  String toString() => '$mnemonic(X$varIndex)';
}

// ============================================================================
// Migration Support
// ============================================================================

/// Convert old (v1) instruction to new (v2) instruction
/// Returns null if instruction doesn't have a unified equivalent
OpV2? migrateInstruction(Op oldOp) {
  // HEAD phase
  if (oldOp is HeadWriter) {
    return HeadVariable(oldOp.varIndex, isReader: false);
  }
  if (oldOp is HeadReader) {
    return HeadVariable(oldOp.varIndex, isReader: true);
  }

  // Structure traversal
  if (oldOp is UnifyWriter) {
    return UnifyVariable(oldOp.varIndex, isReader: false);
  }
  if (oldOp is UnifyReader) {
    return UnifyVariable(oldOp.varIndex, isReader: true);
  }

  // BODY phase - Put
  if (oldOp is PutWriter) {
    return PutVariable(oldOp.varIndex, oldOp.argSlot, isReader: false);
  }
  if (oldOp is PutReader) {
    return PutVariable(oldOp.varIndex, oldOp.argSlot, isReader: true);
  }

  // BODY phase - Set
  if (oldOp is SetWriter) {
    return SetVariable(oldOp.varIndex, isReader: false);
  }
  if (oldOp is SetReader) {
    return SetVariable(oldOp.varIndex, isReader: true);
  }

  // GUARD phase
  if (oldOp is IfWriter) {
    return IfVariable(oldOp.varIndex, isReader: false);
  }
  if (oldOp is IfReader) {
    return IfVariable(oldOp.varIndex, isReader: true);
  }

  // No migration available for this instruction
  return null;
}

/// Convert old instruction to new instruction (non-null)
/// Throws if instruction cannot be migrated
OpV2 migrateInstructionStrict(Op oldOp) {
  final result = migrateInstruction(oldOp);
  if (result == null) {
    throw UnsupportedError('Cannot migrate instruction: ${oldOp.runtimeType}');
  }
  return result;
}

/// Check if an instruction has a unified v2 equivalent
bool hasUnifiedEquivalent(Op oldOp) {
  return oldOp is HeadWriter || oldOp is HeadReader ||
         oldOp is UnifyWriter || oldOp is UnifyReader ||
         oldOp is PutWriter || oldOp is PutReader ||
         oldOp is SetWriter || oldOp is SetReader ||
         oldOp is IfWriter || oldOp is IfReader;
}
