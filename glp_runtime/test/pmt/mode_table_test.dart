import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/pmt/mode_table.dart';

void main() {
  group('ModeTable', () {
    test('addDeclaration stores modes correctly', () {
      final table = ModeTable();

      // Merge(A) := merge(List(A)?, List(A)?, List(A)).
      final decl = ModeDeclaration(
        'Merge',
        ['A'],
        'merge',
        [
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: false),
        ],
        1,
        1,
      );

      table.addDeclaration(decl);

      expect(table.hasDeclaration('merge', 3), isTrue);
      expect(table.getModes('merge', 3), [Mode.reader, Mode.reader, Mode.writer]);
    });

    test('getModes returns null for missing declaration', () {
      final table = ModeTable();

      expect(table.getModes('unknown', 2), isNull);
      expect(table.hasDeclaration('unknown', 2), isFalse);
    });

    test('multiple declarations are stored independently', () {
      final table = ModeTable();

      // Not := not(Num?, Num).
      table.addDeclaration(ModeDeclaration(
        'Not',
        [],
        'not',
        [
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: false),
        ],
        1,
        1,
      ));

      // And := and(Num?, Num?, Num).
      table.addDeclaration(ModeDeclaration(
        'And',
        [],
        'and',
        [
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: false),
        ],
        2,
        1,
      ));

      // Xor := xor(Num?, Num?, Num).
      table.addDeclaration(ModeDeclaration(
        'Xor',
        [],
        'xor',
        [
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: false),
        ],
        3,
        1,
      ));

      expect(table.length, 3);
      expect(table.getModes('not', 2), [Mode.reader, Mode.writer]);
      expect(table.getModes('and', 3), [Mode.reader, Mode.reader, Mode.writer]);
      expect(table.getModes('xor', 3), [Mode.reader, Mode.reader, Mode.writer]);
    });

    test('duplicate declaration throws error', () {
      final table = ModeTable();

      final decl1 = ModeDeclaration(
        'Not',
        [],
        'not',
        [
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: false),
        ],
        1,
        1,
      );

      final decl2 = ModeDeclaration(
        'NotAgain',
        [],
        'not',  // Same predicate/arity
        [
          ModedArg('Bool', [], isReader: true),
          ModedArg('Bool', [], isReader: false),
        ],
        5,
        1,
      );

      table.addDeclaration(decl1);

      expect(
        () => table.addDeclaration(decl2),
        throwsA(isA<DuplicateModeDeclarationError>()),
      );
    });

    test('DuplicateModeDeclarationError has correct message', () {
      final error = DuplicateModeDeclarationError('merge/3', 10, 5);
      expect(
        error.toString(),
        'Duplicate mode declaration for merge/3 at line 10, column 5',
      );
    });

    test('getDeclaration returns original declaration', () {
      final table = ModeTable();

      final decl = ModeDeclaration(
        'Merge',
        ['A'],
        'merge',
        [
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: false),
        ],
        1,
        1,
      );

      table.addDeclaration(decl);

      final retrieved = table.getDeclaration('merge', 3);
      expect(retrieved, same(decl));
      expect(retrieved?.typeName, 'Merge');
      expect(retrieved?.typeParams, ['A']);
    });

    test('signatures returns all declared signatures', () {
      final table = ModeTable();

      table.addDeclaration(ModeDeclaration('Not', [], 'not',
          [ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: false)], 1, 1));
      table.addDeclaration(ModeDeclaration('And', [], 'and',
          [ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: false)], 2, 1));

      expect(table.signatures.toSet(), {'not/2', 'and/3'});
    });

    test('isEmpty returns true for empty table', () {
      final table = ModeTable();
      expect(table.isEmpty, isTrue);
      expect(table.length, 0);

      table.addDeclaration(ModeDeclaration('Not', [], 'not',
          [ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: false)], 1, 1));

      expect(table.isEmpty, isFalse);
      expect(table.length, 1);
    });

    test('fromDeclarations builds table from list', () {
      final declarations = [
        ModeDeclaration('Not', [], 'not',
            [ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: false)], 1, 1),
        ModeDeclaration('And', [], 'and',
            [ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: true), ModedArg('Num', [], isReader: false)], 2, 1),
      ];

      final table = ModeTable.fromDeclarations(declarations);

      expect(table.length, 2);
      expect(table.getModes('not', 2), [Mode.reader, Mode.writer]);
      expect(table.getModes('and', 3), [Mode.reader, Mode.reader, Mode.writer]);
    });

    test('different arities are treated as different predicates', () {
      final table = ModeTable();

      // append/2 (two lists)
      table.addDeclaration(ModeDeclaration(
        'Append2',
        ['A'],
        'append',
        [
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: false),
        ],
        1,
        1,
      ));

      // append/3 (three lists)
      table.addDeclaration(ModeDeclaration(
        'Append3',
        ['A'],
        'append',
        [
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: true),
          ModedArg('List', ['A'], isReader: false),
        ],
        2,
        1,
      ));

      expect(table.length, 2);
      expect(table.getModes('append', 2), [Mode.reader, Mode.writer]);
      expect(table.getModes('append', 3), [Mode.reader, Mode.reader, Mode.writer]);
    });
  });
}
