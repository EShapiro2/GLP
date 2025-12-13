import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/pmt/mode_table.dart';
import 'package:glp_runtime/compiler/pmt/checker.dart';
import 'package:glp_runtime/compiler/pmt/errors.dart';

void main() {
  group('PmtChecker', () {
    late ModeTable modeTable;
    late PmtChecker checker;

    setUp(() {
      modeTable = ModeTable();
      checker = PmtChecker(modeTable);
    });

    group('valid SRSW clauses', () {
      test('merge([], Ys, Ys) with mode (List?, List?, List) - 1 writer, 1 reader', () {
        // Mode: Merge(A) := merge(List(A)?, List(A)?, List(A)).
        modeTable.addDeclaration(ModeDeclaration(
          'Merge', ['A'], 'merge',
          [
            ModedArg('List', ['A'], isReader: true),
            ModedArg('List', ['A'], isReader: true),
            ModedArg('List', ['A'], isReader: false),
          ],
          1, 1,
        ));

        // merge([], Ys, Ys).
        // Head arg 1 (reader): [] - no vars
        // Head arg 2 (reader): Ys → writer (head inverts)
        // Head arg 3 (writer): Ys → reader (head inverts)
        final clause = Clause(
          Atom('merge', [
            ListTerm(null, null, 1, 7),
            VarTerm('Ys', false, 1, 11),
            VarTerm('Ys', false, 1, 15),
          ], 1, 1),
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('merge', 3)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors, isEmpty, reason: 'Ys has 1 writer, 1 reader - valid SRSW');
      });

      test('grounded variable allows multiple readers', () {
        // Mode: HalfAdder := half_adder(Num?, Num?, Num, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'HalfAdder', [], 'half_adder',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // Mode: Xor := xor(Num?, Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Xor', [], 'xor',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // Mode: And := and(Num?, Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'And', [], 'and',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // half_adder(A, B, Sum, Carry) :- ground(A), ground(B) | xor(A, B, Sum), and(A, B, Carry).
        // A: writer (head reader arg) + reader (xor arg) + reader (and arg) = 1w, 2r, grounded
        // B: writer (head reader arg) + reader (xor arg) + reader (and arg) = 1w, 2r, grounded
        // Sum: reader (head writer arg) + writer (xor arg) = 1w, 1r
        // Carry: reader (head writer arg) + writer (and arg) = 1w, 1r
        final clause = Clause(
          Atom('half_adder', [
            VarTerm('A', false, 1, 12),
            VarTerm('B', false, 1, 15),
            VarTerm('Sum', false, 1, 18),
            VarTerm('Carry', false, 1, 23),
          ], 1, 1),
          guards: [
            Guard('ground', [VarTerm('A', false, 1, 35)], 1, 32),
            Guard('ground', [VarTerm('B', false, 1, 47)], 1, 44),
          ],
          body: [
            Goal('xor', [
              VarTerm('A', false, 1, 55),
              VarTerm('B', false, 1, 58),
              VarTerm('Sum', false, 1, 61),
            ], 1, 51),
            Goal('and', [
              VarTerm('A', false, 1, 70),
              VarTerm('B', false, 1, 73),
              VarTerm('Carry', false, 1, 76),
            ], 1, 66),
          ],
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('half_adder', 4)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors, isEmpty, reason: 'A and B are grounded, multiple readers OK');
      });

      test('type guard (number) allows multiple readers', () {
        // Mode: Double := double(Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Double', [], 'double',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // Mode: Add := add(Num?, Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Add', [], 'add',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // double(X, Y) :- number(X) | add(X, X, Y).
        // X: writer (head reader) + reader (add) + reader (add) = 1w, 2r, grounded by number
        // Y: reader (head writer) + writer (add) = 1w, 1r
        final clause = Clause(
          Atom('double', [
            VarTerm('X', false, 1, 8),
            VarTerm('Y', false, 1, 11),
          ], 1, 1),
          guards: [
            Guard('number', [VarTerm('X', false, 1, 20)], 1, 17),
          ],
          body: [
            Goal('add', [
              VarTerm('X', false, 1, 28),
              VarTerm('X', false, 1, 31),
              VarTerm('Y', false, 1, 34),
            ], 1, 24),
          ],
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('double', 2)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors, isEmpty, reason: 'X is grounded by number/1 guard');
      });
    });

    group('invalid SRSW clauses', () {
      test('eq(X, X) with mode (Num?, Num?) - 2 writers, 0 readers', () {
        // Mode: Eq := eq(Num?, Num?).
        modeTable.addDeclaration(ModeDeclaration(
          'Eq', [], 'eq',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
          ],
          1, 1,
        ));

        // eq(X, X).
        // Both args are reader mode → both X occurrences are writers (head inverts)
        // X: 2 writers, 0 readers
        final clause = Clause(
          Atom('eq', [
            VarTerm('X', false, 1, 4),
            VarTerm('X', false, 1, 7),
          ], 1, 1),
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('eq', 2)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors.length, 2);
        expect(errors.any((e) => e.message.contains('2 writer occurrences')), isTrue);
        expect(errors.any((e) => e.message.contains('no reader occurrence')), isTrue);
      });

      test('dup(X, X, X) with mode (Num?, Num, Num) - 1 writer, 2 readers, no ground', () {
        // Mode: Dup := dup(Num?, Num, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Dup', [], 'dup',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // dup(X, X, X).
        // Arg 1 (reader): X → writer (head inverts)
        // Arg 2 (writer): X → reader (head inverts)
        // Arg 3 (writer): X → reader (head inverts)
        // X: 1 writer, 2 readers, NOT grounded
        final clause = Clause(
          Atom('dup', [
            VarTerm('X', false, 1, 5),
            VarTerm('X', false, 1, 8),
            VarTerm('X', false, 1, 11),
          ], 1, 1),
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('dup', 3)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors.length, 1);
        expect(errors[0].message, contains('2 reader occurrences'));
        expect(errors[0].message, contains('add ground(X) guard'));
      });

      test('bad(X, Y) with mode (Num?, Num) - X no reader, Y no writer', () {
        // Mode: Bad := bad(Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Bad', [], 'bad',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // bad(X, Y).
        // Arg 1 (reader): X → writer (head inverts)
        // Arg 2 (writer): Y → reader (head inverts)
        // X: 1 writer, 0 readers
        // Y: 0 writers, 1 reader
        final clause = Clause(
          Atom('bad', [
            VarTerm('X', false, 1, 5),
            VarTerm('Y', false, 1, 8),
          ], 1, 1),
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('bad', 2)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors.length, 2);
        expect(errors.any((e) => e.message.contains('X') && e.message.contains('no reader')), isTrue);
        expect(errors.any((e) => e.message.contains('Y') && e.message.contains('no writer')), isTrue);
      });
    });

    group('checkProcedure', () {
      test('skips procedure without mode declaration', () {
        // No mode declaration for foo/1
        final proc = Procedure('foo', 1, [
          Clause(
            Atom('foo', [VarTerm('X', false, 1, 5)], 1, 1),
            line: 1, column: 1,
          ),
        ], 1, 1);

        final errors = checker.checkProcedure(proc);
        expect(errors, isEmpty, reason: 'No mode declaration = no checking');
      });

      test('checks all clauses in procedure', () {
        // Mode: Eq := eq(Num?, Num?).
        modeTable.addDeclaration(ModeDeclaration(
          'Eq', [], 'eq',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
          ],
          1, 1,
        ));

        // Two invalid clauses
        final proc = Procedure('eq', 2, [
          Clause(
            Atom('eq', [VarTerm('X', false, 1, 4), VarTerm('X', false, 1, 7)], 1, 1),
            line: 1, column: 1,
          ),
          Clause(
            Atom('eq', [VarTerm('Y', false, 2, 4), VarTerm('Y', false, 2, 7)], 2, 1),
            line: 2, column: 1,
          ),
        ], 1, 1);

        final errors = checker.checkProcedure(proc);
        // Each clause has 2 errors (2 writers, 0 readers)
        expect(errors.length, 4);
      });
    });

    group('ground extraction from various guards', () {
      test('comparison guard (>) grounds both operands', () {
        // Mode: Max := max(Num?, Num?, Num).
        modeTable.addDeclaration(ModeDeclaration(
          'Max', [], 'max',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: false),
          ],
          1, 1,
        ));

        // max(A, B, A) :- A > B | true.
        // A: writer (head reader) + reader (head writer via arg3) + reader (guard) = 1w, 2r, grounded by >
        // B: writer (head reader) + reader (guard) = 1w, 1r, grounded by >
        final clause = Clause(
          Atom('max', [
            VarTerm('A', false, 1, 5),
            VarTerm('B', false, 1, 8),
            VarTerm('A', false, 1, 11),
          ], 1, 1),
          guards: [
            Guard('>', [
              VarTerm('A', false, 1, 18),
              VarTerm('B', false, 1, 22),
            ], 1, 16),
          ],
          body: [Goal('true', [], 1, 28)],
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('max', 3)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors, isEmpty, reason: 'A is grounded by > guard');
      });

      test('=?= guard grounds both operands', () {
        // Mode: Same := same(Num?, Num?).
        modeTable.addDeclaration(ModeDeclaration(
          'Same', [], 'same',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
          ],
          1, 1,
        ));

        // Mode: Use := use(Num?, Num?).
        modeTable.addDeclaration(ModeDeclaration(
          'Use', [], 'use',
          [
            ModedArg('Num', [], isReader: true),
            ModedArg('Num', [], isReader: true),
          ],
          1, 1,
        ));

        // same(X, Y) :- X =?= Y | use(X, Y).
        // X: writer (head reader) + reader (guard) + reader (body) = 1w, 2r, grounded
        // Y: writer (head reader) + reader (guard) + reader (body) = 1w, 2r, grounded
        final clause = Clause(
          Atom('same', [
            VarTerm('X', false, 1, 6),
            VarTerm('Y', false, 1, 9),
          ], 1, 1),
          guards: [
            Guard('=?=', [
              VarTerm('X', false, 1, 16),
              VarTerm('Y', false, 1, 22),
            ], 1, 14),
          ],
          body: [
            Goal('use', [
              VarTerm('X', false, 1, 32),
              VarTerm('Y', false, 1, 35),
            ], 1, 28),
          ],
          line: 1, column: 1,
        );

        final modes = modeTable.getModes('same', 2)!;
        final errors = checker.checkClause(clause, modes);

        expect(errors, isEmpty, reason: 'X and Y grounded by =?= guard');
      });
    });
  });

  group('PmtError', () {
    test('toString formats correctly', () {
      final error = PmtError('Test message', 10, 5);
      expect(error.toString(), 'PMT Error at 10:5: Test message');
    });

    test('equality works', () {
      final e1 = PmtError('msg', 1, 2);
      final e2 = PmtError('msg', 1, 2);
      final e3 = PmtError('msg', 1, 3);

      expect(e1, equals(e2));
      expect(e1, isNot(equals(e3)));
    });
  });

  group('PmtErrors', () {
    test('toString formats multiple errors', () {
      final errors = PmtErrors([
        PmtError('Error 1', 1, 1),
        PmtError('Error 2', 2, 1),
      ]);
      expect(errors.toString(), contains('Error 1'));
      expect(errors.toString(), contains('Error 2'));
    });

    test('toString handles empty list', () {
      final errors = PmtErrors([]);
      expect(errors.toString(), 'PmtErrors: (none)');
    });
  });
}
