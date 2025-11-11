/// Phase 2: Bytecode Program Migration
///
/// Utilities for migrating entire bytecode programs from v1 to v2 instruction set.
/// This allows testing unified instructions with existing compiled programs.

library;

import 'opcodes.dart';
import 'opcodes_v2.dart';
import 'runner.dart' show BytecodeProgram;

/// Result of migration: both v1 and v2 versions plus statistics
class MigrationResult {
  final BytecodeProgram original;
  final MixedBytecodeProgram migrated;
  final MigrationStats stats;

  MigrationResult(this.original, this.migrated, this.stats);
}

/// Statistics about instruction migration
class MigrationStats {
  final int totalInstructions;
  final int migratedInstructions;
  final int unmutableInstructions;
  final Map<String, int> instructionCounts;

  MigrationStats({
    required this.totalInstructions,
    required this.migratedInstructions,
    required this.unmutableInstructions,
    required this.instructionCounts,
  });

  double get migrationRate => totalInstructions > 0
      ? (migratedInstructions / totalInstructions) * 100
      : 0.0;

  @override
  String toString() {
    final buffer = StringBuffer();
    buffer.writeln('Migration Statistics:');
    buffer.writeln('  Total instructions: $totalInstructions');
    buffer.writeln('  Migrated to v2: $migratedInstructions');
    buffer.writeln('  Kept as v1: $unmutableInstructions');
    buffer.writeln('  Migration rate: ${migrationRate.toStringAsFixed(1)}%');
    buffer.writeln('  Instruction breakdown:');
    for (final entry in instructionCounts.entries) {
      buffer.writeln('    ${entry.key}: ${entry.value}');
    }
    return buffer.toString();
  }
}

/// Bytecode program that can contain both v1 (Op) and v2 (OpV2) instructions
/// This allows gradual migration and testing
class MixedBytecodeProgram {
  final List<dynamic> ops;  // List of Op or OpV2
  final Map<String, int> labels;

  MixedBytecodeProgram(this.ops, this.labels);

  /// Get instruction at PC (can be v1 or v2)
  dynamic operator [](int pc) => ops[pc];

  /// Check if instruction at PC is v2
  bool isV2(int pc) => ops[pc] is OpV2;

  /// Get as v1 instruction (returns null if v2)
  Op? asV1(int pc) => ops[pc] is Op ? ops[pc] as Op : null;

  /// Get as v2 instruction (returns null if v1)
  OpV2? asV2(int pc) => ops[pc] is OpV2 ? ops[pc] as OpV2 : null;

  int get length => ops.length;
}

/// Migrate a bytecode program from v1 to mixed v1/v2
/// Converts all unifiable instructions to v2, keeps others as v1
MigrationResult migrateProgram(BytecodeProgram program) {
  final migratedOps = <dynamic>[];
  final instructionCounts = <String, int>{};
  int migratedCount = 0;
  int unmutableCount = 0;

  for (final op in program.ops) {
    // Try to migrate
    final migrated = migrateInstruction(op);

    if (migrated != null) {
      // Successfully migrated to v2
      migratedOps.add(migrated);
      migratedCount++;
      final name = _getMnemonic(migrated);
      instructionCounts[name] = (instructionCounts[name] ?? 0) + 1;
    } else {
      // Keep as v1
      migratedOps.add(op);
      unmutableCount++;
      final name = op.runtimeType.toString();
      instructionCounts[name] = (instructionCounts[name] ?? 0) + 1;
    }
  }

  final stats = MigrationStats(
    totalInstructions: program.ops.length,
    migratedInstructions: migratedCount,
    unmutableInstructions: unmutableCount,
    instructionCounts: instructionCounts,
  );

  final migratedProgram = MixedBytecodeProgram(migratedOps, program.labels);

  return MigrationResult(program, migratedProgram, stats);
}

/// Check if two programs are structurally equivalent
/// (same labels, same instruction sequence ignoring v1/v2 differences)
bool programsEquivalent(BytecodeProgram p1, MixedBytecodeProgram p2) {
  // Check labels match
  if (p1.labels.length != p2.labels.length) return false;
  for (final entry in p1.labels.entries) {
    if (p2.labels[entry.key] != entry.value) return false;
  }

  // Check same number of instructions
  if (p1.ops.length != p2.ops.length) return false;

  // Check each instruction is equivalent
  for (int i = 0; i < p1.ops.length; i++) {
    if (!_instructionsEquivalent(p1.ops[i], p2.ops[i])) {
      return false;
    }
  }

  return true;
}

/// Check if two instructions are semantically equivalent
bool _instructionsEquivalent(Op v1, dynamic v2) {
  // If v2 is still v1, check identity
  if (v2 is Op) {
    return v1.runtimeType == v2.runtimeType && _sameFields(v1, v2);
  }

  // v2 is OpV2 - check if it's the migrated version of v1
  final expected = migrateInstruction(v1);
  if (expected == null) {
    // v1 has no v2 equivalent, so v2 shouldn't be OpV2
    return false;
  }

  // Check if v2 matches expected migration
  return v2.runtimeType == expected.runtimeType && _sameFieldsV2(expected, v2 as OpV2);
}

/// Check if two v1 instructions have same field values
bool _sameFields(Op a, Op b) {
  if (a is HeadWriter && b is HeadWriter) return a.varIndex == b.varIndex;
  if (a is HeadReader && b is HeadReader) return a.varIndex == b.varIndex;
  if (a is PutWriter && b is PutWriter) return a.varIndex == b.varIndex && a.argSlot == b.argSlot;
  if (a is PutReader && b is PutReader) return a.varIndex == b.varIndex && a.argSlot == b.argSlot;
  if (a is SetWriter && b is SetWriter) return a.varIndex == b.varIndex;
  if (a is SetReader && b is SetReader) return a.varIndex == b.varIndex;
  if (a is UnifyWriter && b is UnifyWriter) return a.varIndex == b.varIndex;
  if (a is UnifyReader && b is UnifyReader) return a.varIndex == b.varIndex;
  if (a is IfWriter && b is IfWriter) return a.varIndex == b.varIndex;
  if (a is IfReader && b is IfReader) return a.varIndex == b.varIndex;
  // For instructions without fields, just type match is enough
  return true;
}

/// Check if two v2 instructions have same field values
bool _sameFieldsV2(OpV2 a, OpV2 b) {
  if (a is HeadVariable && b is HeadVariable) {
    return a.varIndex == b.varIndex && a.isReader == b.isReader;
  }
  if (a is UnifyVariable && b is UnifyVariable) {
    return a.varIndex == b.varIndex && a.isReader == b.isReader;
  }
  if (a is PutVariable && b is PutVariable) {
    return a.varIndex == b.varIndex && a.argSlot == b.argSlot && a.isReader == b.isReader;
  }
  if (a is SetVariable && b is SetVariable) {
    return a.varIndex == b.varIndex && a.isReader == b.isReader;
  }
  if (a is IfVariable && b is IfVariable) {
    return a.varIndex == b.varIndex && a.isReader == b.isReader;
  }
  return false;
}

/// Get mnemonic for v2 instruction
String _getMnemonic(OpV2 op) {
  if (op is HeadVariable) return op.mnemonic;
  if (op is UnifyVariable) return op.mnemonic;
  if (op is PutVariable) return op.mnemonic;
  if (op is SetVariable) return op.mnemonic;
  if (op is IfVariable) return op.mnemonic;
  return op.runtimeType.toString();
}
