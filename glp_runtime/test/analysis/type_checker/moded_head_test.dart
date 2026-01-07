/// Tests for moded head construction
/// Specification: docs/modules/moded-head.md
/// Paper Reference: Definition 4.6

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('Moded Head', () {
    late TypeEnvironment env;
    late ProcDecl mergeDecl;

    setUp(() {
      env = TypeEnvironment();

      env.addType(TypeDef(
        'Stream',
        [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
      ));

      mergeDecl = ProcDecl('merge', 3, [
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: true),
        TypeRef('Stream', isComplement: false)
      ]);
      env.addProcedure(mergeDecl);
    });

    group('modedHead', () {
      // Positive controls
      test('constructs moded head with correct modes from type', () {
        // H = merge([X|Xs], Ys, [X?|Zs?])
        final head = Compound('merge', [
          Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
          Variable('Ys', isReader: false),
          Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
        ]);

        final modedH = modedHead(head, mergeDecl);

        // Root should be consume
        expect(modedH.mode, equals(Mode.consume));

        // Arg 1 and 2 should be consume (Stream?)
        expect((modedH as ModedCompound).args[0].mode, equals(Mode.consume));
        expect(modedH.args[1].mode, equals(Mode.consume));

        // Arg 3 should be produce (Stream)
        expect(modedH.args[2].mode, equals(Mode.produce));
      });

      test('flips all variables to their pairs', () {
        // H = merge([X|Xs], Ys, [X?|Zs?])
        final head = Compound('merge', [
          Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
          Variable('Ys', isReader: false),
          Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
        ]);

        final modedH = modedHead(head, mergeDecl);
        final mc = modedH as ModedCompound;

        // X (writer) -> X? (reader)
        final arg1List = mc.args[0] as ModedCompound;
        expect((arg1List.args[0] as ModedVariable).isReader, isTrue);

        // Xs (writer) -> Xs? (reader)
        expect((arg1List.args[1] as ModedVariable).isReader, isTrue);

        // Ys (writer) -> Ys? (reader)
        expect((mc.args[1] as ModedVariable).isReader, isTrue);

        // X? (reader) -> X (writer)
        final arg3List = mc.args[2] as ModedCompound;
        expect((arg3List.args[0] as ModedVariable).isReader, isFalse);

        // Zs? (reader) -> Zs (writer)
        expect((arg3List.args[1] as ModedVariable).isReader, isFalse);
      });

      test('moded head is I/O moded', () {
        final head = Compound('merge', [
          Variable('Xs', isReader: false),
          Variable('Ys', isReader: false),
          Variable('Zs', isReader: true)
        ]);

        final modedH = modedHead(head, mergeDecl);

        expect(isIO(modedH), isTrue);
      });

      test('constants get correct mode from type position', () {
        // merge([], Ys, Ys?)
        final head = Compound('merge', [
          Constant([]),  // nil
          Variable('Ys', isReader: false),
          Variable('Ys', isReader: true)
        ]);

        final modedH = modedHead(head, mergeDecl);
        final mc = modedH as ModedCompound;

        // [] at position 1 (Stream?) should have consume mode
        expect((mc.args[0] as ModedConstant).mode, equals(Mode.consume));
      });
    });

    group('moded head paths', () {
      test('extracts correct paths from moded head', () {
        // H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
        final head = Compound('merge', [
          Compound('.', [Variable('X', isReader: false), Variable('Xs', isReader: false)]),
          Variable('Ys', isReader: false),
          Compound('.', [Variable('X', isReader: true), Variable('Zs', isReader: true)])
        ]);

        final modedH = modedHead(head, mergeDecl);
        final paths = modedH.paths;

        // Should have 5 paths (to X?, Xs?, Ys?, X, Zs)
        expect(paths.length, equals(5));

        // Path to X? should have modes ↓ -> ↓ -> ↓
        final pathToXReader = paths.firstWhere(
          (p) => p.leaf.symbol == 'X?'
        );
        expect(pathToXReader.steps[0].mode, equals(Mode.consume));
        expect(pathToXReader.steps[1].mode, equals(Mode.consume));

        // Path to X (writer in output) should have modes ↓ -> ↑ -> ↑
        final pathToXWriter = paths.firstWhere(
          (p) => p.leaf.symbol == 'X'
        );
        expect(pathToXWriter.steps[0].mode, equals(Mode.consume));
        expect(pathToXWriter.steps[1].mode, equals(Mode.produce));
      });
    });
  });
}
