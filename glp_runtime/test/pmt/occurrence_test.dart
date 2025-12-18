import 'package:test/test.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/pmt/mode_table.dart';
import 'package:glp_runtime/compiler/pmt/occurrence.dart';

void main() {
  group('OccurrenceClassifier', () {
    late ModeTable modeTable;
    late OccurrenceClassifier classifier;

    setUp(() {
      modeTable = ModeTable();
      // Add merge/3 mode: Merge(A) := merge(List(A)?, List(A)?, List(A)).
      modeTable.addDeclaration(ModeDeclaration(
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
      ));
      classifier = OccurrenceClassifier(modeTable);
    });

    test('simple clause: merge([], Ys, Ys) - head only', () {
      // merge([], Ys, Ys).
      // Mode: [reader, reader, writer]
      // Head arg 1 (reader): [] - no variables
      // Head arg 2 (reader): Ys → writer occurrence (head inverts)
      // Head arg 3 (writer): Ys → reader occurrence (head inverts)
      final clause = Clause(
        Atom('merge', [
          ListTerm(null, null, 1, 7),  // []
          VarTerm('Ys', false, 1, 11), // Ys (writer syntax)
          VarTerm('Ys', false, 1, 15), // Ys (writer syntax)
        ], 1, 1),
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('merge', 3)!;
      final occurrences = classifier.classifyClause(clause, headModes);

      expect(occurrences.length, 2);

      // First Ys at arg position 1 (reader mode) → writer occurrence
      expect(occurrences[0].variable, 'Ys');
      expect(occurrences[0].type, OccurrenceType.writer);
      expect(occurrences[0].line, 1);
      expect(occurrences[0].column, 11);

      // Second Ys at arg position 2 (writer mode) → reader occurrence
      expect(occurrences[1].variable, 'Ys');
      expect(occurrences[1].type, OccurrenceType.reader);
      expect(occurrences[1].line, 1);
      expect(occurrences[1].column, 15);

      // Group by variable
      final grouped = groupByVariable(occurrences);
      expect(grouped['Ys']!.length, 2);

      // Count occurrences
      final counts = countOccurrences(grouped['Ys']!);
      expect(counts.writers, 1);
      expect(counts.readers, 1);
    });

    test('recursive clause: merge([X|Xs], Ys, [X|Zs]) :- merge(Ys, Xs, Zs).', () {
      // Head: merge([X|Xs], Ys, [X|Zs])
      // Mode: [reader, reader, writer]
      // Head arg 0 (reader): X, Xs → both writer occurrences
      // Head arg 1 (reader): Ys → writer occurrence
      // Head arg 2 (writer): X, Zs → both reader occurrences

      // Body: merge(Ys, Xs, Zs)
      // Body arg 0 (reader): Ys → reader occurrence
      // Body arg 1 (reader): Xs → reader occurrence
      // Body arg 2 (writer): Zs → writer occurrence

      final clause = Clause(
        Atom('merge', [
          ListTerm(
            VarTerm('X', false, 1, 8),
            VarTerm('Xs', false, 1, 10),
            1, 7,
          ),
          VarTerm('Ys', false, 1, 15),
          ListTerm(
            VarTerm('X', false, 1, 20),
            VarTerm('Zs', false, 1, 22),
            1, 19,
          ),
        ], 1, 1),
        body: [
          Goal('merge', [
            VarTerm('Ys', false, 1, 35),
            VarTerm('Xs', false, 1, 39),
            VarTerm('Zs', false, 1, 43),
          ], 1, 29),
        ],
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('merge', 3)!;
      final occurrences = classifier.classifyClause(clause, headModes);

      // Group by variable
      final grouped = groupByVariable(occurrences);

      // X: 1 writer (head reader arg), 1 reader (head writer arg)
      expect(grouped['X']!.length, 2);
      final xCounts = countOccurrences(grouped['X']!);
      expect(xCounts.writers, 1);
      expect(xCounts.readers, 1);

      // Xs: 1 writer (head reader arg), 1 reader (body reader arg)
      expect(grouped['Xs']!.length, 2);
      final xsCounts = countOccurrences(grouped['Xs']!);
      expect(xsCounts.writers, 1);
      expect(xsCounts.readers, 1);

      // Ys: 1 writer (head reader arg), 1 reader (body reader arg)
      expect(grouped['Ys']!.length, 2);
      final ysCounts = countOccurrences(grouped['Ys']!);
      expect(ysCounts.writers, 1);
      expect(ysCounts.readers, 1);

      // Zs: 1 reader (head writer arg), 1 writer (body writer arg)
      expect(grouped['Zs']!.length, 2);
      final zsCounts = countOccurrences(grouped['Zs']!);
      expect(zsCounts.writers, 1);
      expect(zsCounts.readers, 1);
    });

    test('clause with guards: all guard variables are readers', () {
      // half_adder(A, B, Sum, Carry) :- ground(A), ground(B) | ...
      // Guards only read, so A and B in guards are reader occurrences

      // Add half_adder mode
      modeTable.addDeclaration(ModeDeclaration(
        'HalfAdder',
        [],
        'half_adder',
        [
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: true),
          ModedArg('Num', [], isReader: false),
          ModedArg('Num', [], isReader: false),
        ],
        1,
        1,
      ));

      final clause = Clause(
        Atom('half_adder', [
          VarTerm('A', false, 1, 12),
          VarTerm('B', false, 1, 15),
          VarTerm('Sum', false, 1, 18),
          VarTerm('Carry', false, 1, 23),
        ], 1, 1),
        guards: [
          Guard('ground', [VarTerm('A', true, 1, 38)], 1, 31),
          Guard('ground', [VarTerm('B', true, 1, 50)], 1, 43),
        ],
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('half_adder', 4)!;
      final occurrences = classifier.classifyClause(clause, headModes);
      final grouped = groupByVariable(occurrences);

      // A: 1 writer (head reader arg), 1 reader (guard)
      final aCounts = countOccurrences(grouped['A']!);
      expect(aCounts.writers, 1);
      expect(aCounts.readers, 1);

      // B: 1 writer (head reader arg), 1 reader (guard)
      final bCounts = countOccurrences(grouped['B']!);
      expect(bCounts.writers, 1);
      expect(bCounts.readers, 1);

      // Sum: 1 reader (head writer arg)
      final sumCounts = countOccurrences(grouped['Sum']!);
      expect(sumCounts.writers, 0);
      expect(sumCounts.readers, 1);

      // Carry: 1 reader (head writer arg)
      final carryCounts = countOccurrences(grouped['Carry']!);
      expect(carryCounts.writers, 0);
      expect(carryCounts.readers, 1);
    });

    test('nested structures are traversed', () {
      // foo(pair(X, Y), Z).
      // Mode: [reader, writer]

      modeTable.addDeclaration(ModeDeclaration(
        'Foo',
        [],
        'foo',
        [
          ModedArg('Pair', [], isReader: true),
          ModedArg('Any', [], isReader: false),
        ],
        1,
        1,
      ));

      final clause = Clause(
        Atom('foo', [
          StructTerm('pair', [
            VarTerm('X', false, 1, 10),
            VarTerm('Y', false, 1, 13),
          ], 1, 5),
          VarTerm('Z', false, 1, 17),
        ], 1, 1),
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('foo', 2)!;
      final occurrences = classifier.classifyClause(clause, headModes);
      final grouped = groupByVariable(occurrences);

      // X, Y in reader arg → writer occurrences
      expect(countOccurrences(grouped['X']!).writers, 1);
      expect(countOccurrences(grouped['Y']!).writers, 1);

      // Z in writer arg → reader occurrence
      expect(countOccurrences(grouped['Z']!).readers, 1);
    });

    test('body goal without mode declaration is skipped', () {
      // foo(X) :- unknown_pred(X).
      // unknown_pred has no mode declaration

      modeTable.addDeclaration(ModeDeclaration(
        'Foo',
        [],
        'foo',
        [ModedArg('Any', [], isReader: true)],
        1,
        1,
      ));

      final clause = Clause(
        Atom('foo', [VarTerm('X', false, 1, 5)], 1, 1),
        body: [
          Goal('unknown_pred', [VarTerm('X', false, 1, 20)], 1, 12),
        ],
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('foo', 1)!;
      final occurrences = classifier.classifyClause(clause, headModes);

      // Only head X is classified (as writer since arg is reader)
      // Body goal is skipped since unknown_pred has no mode
      expect(occurrences.length, 1);
      expect(occurrences[0].variable, 'X');
      expect(occurrences[0].type, OccurrenceType.writer);
    });

    test('Occurrence equality and toString', () {
      final occ1 = Occurrence('X', OccurrenceType.writer, 1, 5);
      final occ2 = Occurrence('X', OccurrenceType.writer, 1, 5);
      final occ3 = Occurrence('X', OccurrenceType.reader, 1, 5);

      expect(occ1, equals(occ2));
      expect(occ1, isNot(equals(occ3)));
      expect(occ1.toString(), 'X:writer@1:5');
      expect(occ3.toString(), 'X:reader@1:5');
    });

    test('empty clause head with no args', () {
      modeTable.addDeclaration(ModeDeclaration(
        'Done',
        [],
        'done',
        [],
        1,
        1,
      ));

      final clause = Clause(
        Atom('done', [], 1, 1),
        line: 1,
        column: 1,
      );

      final headModes = modeTable.getModes('done', 0)!;
      final occurrences = classifier.classifyClause(clause, headModes);

      expect(occurrences, isEmpty);
    });
  });

  group('groupByVariable', () {
    test('groups occurrences correctly', () {
      final occurrences = [
        Occurrence('X', OccurrenceType.writer, 1, 1),
        Occurrence('Y', OccurrenceType.reader, 1, 5),
        Occurrence('X', OccurrenceType.reader, 1, 10),
      ];

      final grouped = groupByVariable(occurrences);

      expect(grouped.keys.toSet(), {'X', 'Y'});
      expect(grouped['X']!.length, 2);
      expect(grouped['Y']!.length, 1);
    });

    test('empty list returns empty map', () {
      expect(groupByVariable([]), isEmpty);
    });
  });

  group('countOccurrences', () {
    test('counts writers and readers', () {
      final occurrences = [
        Occurrence('X', OccurrenceType.writer, 1, 1),
        Occurrence('X', OccurrenceType.reader, 1, 5),
        Occurrence('X', OccurrenceType.reader, 1, 10),
      ];

      final counts = countOccurrences(occurrences);
      expect(counts.writers, 1);
      expect(counts.readers, 2);
    });

    test('empty list returns zero counts', () {
      final counts = countOccurrences([]);
      expect(counts.writers, 0);
      expect(counts.readers, 0);
    });
  });
}
