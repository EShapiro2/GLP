/// Phase 2: Instruction Migration Integration Tests
///
/// Validates that unified v2 instructions produce identical results to v1 instructions
/// Tests migration of real GLP programs to ensure semantic equivalence

import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/opcodes_v2.dart';
import 'package:glp_runtime/bytecode/program_migrator.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  group('Phase 2: Instruction Migration Tests', () {

    test('Test 1: Individual instruction migration', () {
      print('\n=== TEST 1: Individual Instruction Migration ===\n');

      // Test HeadWriter -> HeadVariable
      final hw = HeadWriter(5);
      final hwMigrated = migrateInstruction(hw);
      expect(hwMigrated, isA<HeadVariable>());
      expect((hwMigrated as HeadVariable).varIndex, equals(5));
      expect(hwMigrated.isReader, isFalse);
      print('✓ HeadWriter(5) -> ${hwMigrated.mnemonic}(5)');

      // Test HeadReader -> HeadVariable
      final hr = HeadReader(7);
      final hrMigrated = migrateInstruction(hr);
      expect(hrMigrated, isA<HeadVariable>());
      expect((hrMigrated as HeadVariable).varIndex, equals(7));
      expect(hrMigrated.isReader, isTrue);
      print('✓ HeadReader(7) -> ${hrMigrated.mnemonic}(7)');

      // Test PutWriter -> PutVariable
      final pw = PutWriter(3, 0);
      final pwMigrated = migrateInstruction(pw);
      expect(pwMigrated, isA<PutVariable>());
      expect((pwMigrated as PutVariable).varIndex, equals(3));
      expect(pwMigrated.argSlot, equals(0));
      expect(pwMigrated.isReader, isFalse);
      print('✓ PutWriter(3, 0) -> ${pwMigrated.mnemonic}(3, 0)');

      // Test PutReader -> PutVariable
      final pr = PutReader(4, 1);
      final prMigrated = migrateInstruction(pr);
      expect(prMigrated, isA<PutVariable>());
      expect((prMigrated as PutVariable).varIndex, equals(4));
      expect(prMigrated.argSlot, equals(1));
      expect(prMigrated.isReader, isTrue);
      print('✓ PutReader(4, 1) -> ${prMigrated.mnemonic}(4, 1)');

      // Test UnifyWriter -> UnifyVariable
      final uw = UnifyWriter(2);
      final uwMigrated = migrateInstruction(uw);
      expect(uwMigrated, isA<UnifyVariable>());
      expect((uwMigrated as UnifyVariable).varIndex, equals(2));
      expect(uwMigrated.isReader, isFalse);
      print('✓ UnifyWriter(2) -> ${uwMigrated.mnemonic}(2)');

      // Test UnifyReader -> UnifyVariable
      final ur = UnifyReader(6);
      final urMigrated = migrateInstruction(ur);
      expect(urMigrated, isA<UnifyVariable>());
      expect((urMigrated as UnifyVariable).varIndex, equals(6));
      expect(urMigrated.isReader, isTrue);
      print('✓ UnifyReader(6) -> ${urMigrated.mnemonic}(6)');

      // Test SetWriter -> SetVariable
      final sw = SetWriter(1);
      final swMigrated = migrateInstruction(sw);
      expect(swMigrated, isA<SetVariable>());
      expect((swMigrated as SetVariable).varIndex, equals(1));
      expect(swMigrated.isReader, isFalse);
      print('✓ SetWriter(1) -> ${swMigrated.mnemonic}(1)');

      // Test SetReader -> SetVariable
      final sr = SetReader(8);
      final srMigrated = migrateInstruction(sr);
      expect(srMigrated, isA<SetVariable>());
      expect((srMigrated as SetVariable).varIndex, equals(8));
      expect(srMigrated.isReader, isTrue);
      print('✓ SetReader(8) -> ${srMigrated.mnemonic}(8)');

      // Test IfWriter -> IfVariable
      final iw = IfWriter(9);
      final iwMigrated = migrateInstruction(iw);
      expect(iwMigrated, isA<IfVariable>());
      expect((iwMigrated as IfVariable).varIndex, equals(9));
      expect(iwMigrated.isReader, isFalse);
      print('✓ IfWriter(9) -> ${iwMigrated.mnemonic}(9)');

      // Test IfReader -> IfVariable
      final ir = IfReader(10);
      final irMigrated = migrateInstruction(ir);
      expect(irMigrated, isA<IfVariable>());
      expect((irMigrated as IfVariable).varIndex, equals(10));
      expect(irMigrated.isReader, isTrue);
      print('✓ IfReader(10) -> ${irMigrated.mnemonic}(10)');

      print('\n✓ All 10 instruction types migrated correctly\n');
    });

    test('Test 2: Program migration - simple unification', () {
      print('\n=== TEST 2: Program Migration - Simple Unification ===\n');

      // Program: p(X?) :- q(X).
      // This uses HeadReader and PutWriter
      final prog = BC.prog([
        BC.L('p/1'),
        BC.TRY(),
        // HEAD: p(X?) - first occurrence of X as reader
        HeadReader(1),  // Should migrate to HeadVariable(1, isReader: true)
        BC.COMMIT(),
        // BODY: spawn q(X) - X now used as writer
        PutWriter(1, 0),  // Should migrate to PutVariable(1, 0, isReader: false)
        BC.spawn('q', 1),
        BC.PROCEED(),
        BC.L('p_end'),
        BC.SUSP(),
      ]);

      print('Original program: ${prog.ops.length} instructions');

      final result = migrateProgram(prog);

      print(result.stats);

      // Validate migration
      expect(result.stats.totalInstructions, equals(prog.ops.length));
      expect(result.stats.migratedInstructions, equals(2));  // HeadReader + PutWriter

      // Check specific migrations
      final migratedProg = result.migrated;

      // Find HeadReader -> HeadVariable
      var found = false;
      for (int i = 0; i < migratedProg.length; i++) {
        if (migratedProg.isV2(i)) {
          final op = migratedProg.asV2(i)!;
          if (op is HeadVariable && op.isReader && op.varIndex == 1) {
            found = true;
            print('✓ Found migrated HeadVariable(1, isReader: true) at PC $i');
            break;
          }
        }
      }
      expect(found, isTrue, reason: 'HeadReader should be migrated to HeadVariable');

      // Find PutWriter -> PutVariable
      found = false;
      for (int i = 0; i < migratedProg.length; i++) {
        if (migratedProg.isV2(i)) {
          final op = migratedProg.asV2(i)!;
          if (op is PutVariable && !op.isReader && op.varIndex == 1 && op.argSlot == 0) {
            found = true;
            print('✓ Found migrated PutVariable(1, 0, isReader: false) at PC $i');
            break;
          }
        }
      }
      expect(found, isTrue, reason: 'PutWriter should be migrated to PutVariable');

      print('\n✓ Program migration successful\n');
    });

    test('Test 3: Program migration - structure building', () {
      print('\n=== TEST 3: Program Migration - Structure Building ===\n');

      // Program: p(f(X, Y?)) :- true.
      // Uses HeadStructure, UnifyWriter, UnifyReader
      final prog = BC.prog([
        BC.L('p/1'),
        BC.TRY(),
        // HEAD: p(f(X, Y?))
        BC.headStruct('f', 2, 0),  // Not migrated (no reader/writer variant)
        UnifyWriter(1),            // Should migrate to UnifyVariable(1, isReader: false)
        UnifyReader(2),            // Should migrate to UnifyVariable(2, isReader: true)
        BC.COMMIT(),
        BC.PROCEED(),
        BC.L('p_end'),
        BC.SUSP(),
      ]);

      print('Original program: ${prog.ops.length} instructions');

      final result = migrateProgram(prog);

      print(result.stats);

      expect(result.stats.migratedInstructions, equals(2));  // UnifyWriter + UnifyReader

      // Verify UnifyWriter -> UnifyVariable
      var foundWriter = false;
      var foundReader = false;
      for (int i = 0; i < result.migrated.length; i++) {
        if (result.migrated.isV2(i)) {
          final op = result.migrated.asV2(i)!;
          if (op is UnifyVariable) {
            if (!op.isReader && op.varIndex == 1) {
              foundWriter = true;
              print('✓ Found migrated UnifyVariable(1, isReader: false) at PC $i');
            }
            if (op.isReader && op.varIndex == 2) {
              foundReader = true;
              print('✓ Found migrated UnifyVariable(2, isReader: true) at PC $i');
            }
          }
        }
      }
      expect(foundWriter, isTrue);
      expect(foundReader, isTrue);

      print('\n✓ Structure building migration successful\n');
    });

    test('Test 4: Program migration - guard instructions', () {
      print('\n=== TEST 4: Program Migration - Guard Instructions ===\n');

      // Program with type guards
      final prog = BC.prog([
        BC.L('test/2'),
        BC.TRY(),
        BC.getVar(1, 0),
        BC.getVar(2, 1),
        IfWriter(1),   // Should migrate to IfVariable(1, isReader: false)
        IfReader(2),   // Should migrate to IfVariable(2, isReader: true)
        BC.COMMIT(),
        BC.PROCEED(),
        BC.L('test_end'),
        BC.SUSP(),
      ]);

      final result = migrateProgram(prog);

      print(result.stats);

      expect(result.stats.migratedInstructions, equals(2));  // IfWriter + IfReader

      var foundIfWriter = false;
      var foundIfReader = false;
      for (int i = 0; i < result.migrated.length; i++) {
        if (result.migrated.isV2(i)) {
          final op = result.migrated.asV2(i)!;
          if (op is IfVariable) {
            if (!op.isReader && op.varIndex == 1) {
              foundIfWriter = true;
              print('✓ Found migrated IfVariable(1, isReader: false) at PC $i');
            }
            if (op.isReader && op.varIndex == 2) {
              foundIfReader = true;
              print('✓ Found migrated IfVariable(2, isReader: true) at PC $i');
            }
          }
        }
      }
      expect(foundIfWriter, isTrue);
      expect(foundIfReader, isTrue);

      print('\n✓ Guard instruction migration successful\n');
    });

    test('Test 5: Non-migrateable instructions preserved', () {
      print('\n=== TEST 5: Non-Migrateable Instructions Preserved ===\n');

      // Program with instructions that have no v2 equivalent
      final prog = BC.prog([
        BC.L('test/1'),
        BC.TRY(),
        BC.headConst('a', 0),   // No v2 equivalent
        BC.COMMIT(),
        BC.putConst('b', 0),    // No v2 equivalent
        BC.spawn('q', 1),        // No v2 equivalent
        BC.PROCEED(),
        BC.L('test_end'),
        BC.SUSP(),
      ]);

      final result = migrateProgram(prog);

      print(result.stats);

      expect(result.stats.migratedInstructions, equals(0));
      expect(result.stats.unmutableInstructions, equals(prog.ops.length));

      // Verify all instructions remain as v1
      for (int i = 0; i < result.migrated.length; i++) {
        expect(result.migrated.isV2(i), isFalse,
               reason: 'Instruction at PC $i should remain v1');
      }

      print('✓ All non-migrateable instructions preserved as v1\n');
    });

    test('Test 6: Mixed program migration', () {
      print('\n=== TEST 6: Mixed Program Migration ===\n');

      // Program with both migrateable and non-migrateable instructions
      final prog = BC.prog([
        BC.L('merge/3'),
        BC.TRY(),
        // HEAD
        BC.headConst([], 0),     // Not migrated
        BC.getVar(1, 1),         // Not migrated
        HeadReader(2),           // MIGRATED to HeadVariable
        BC.COMMIT(),
        // BODY
        PutReader(1, 0),         // MIGRATED to PutVariable
        PutWriter(2, 1),         // MIGRATED to PutVariable
        BC.spawn('p', 2),        // Not migrated
        BC.PROCEED(),
        BC.L('merge_end'),
        BC.SUSP(),
      ]);

      final result = migrateProgram(prog);

      print(result.stats);

      expect(result.stats.migratedInstructions, equals(3));  // HeadReader + 2x Put
      expect(result.stats.totalInstructions, equals(prog.ops.length));

      print('✓ Mixed program correctly migrated\n');
      print('  Migration rate: ${result.stats.migrationRate.toStringAsFixed(1)}%');
    });
  });
}
