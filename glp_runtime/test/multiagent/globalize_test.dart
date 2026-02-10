/// Tests for Globalize operation
///
/// Derived from madGLP-spec.md Section 5.1: Globalize
///
/// Given agent p, remote agent q, and term T, globalization produces T_p↑
/// with global names substituted for variables, creates table entries for
/// writers, and spawns global_send goals for readers.

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

void main() {
  group('Globalize', () {
    test('writer variable: creates entry, no spawn', () {
      // Given: term with writer variable Y at address 100
      final table = GlobalWritersTable('p');
      final variables = [TermVar.writer(100, readerAddr: 101)];

      // When: globalize(Y, 'q')
      final result = globalize(
        variables: variables,
        localAgent: 'p',
        remoteAgent: 'q',
        table: table,
      );

      // Then:
      //   - term becomes _w(p, 1) (indices start at 1, 0 is reserved)
      expect(result.globalNames.length, 1);
      expect(result.globalNames[0], GlobalName.writer('p', 1));

      //   - entry (Y, q) at index 1 in table
      // Spec Section 5.1: "create entry (Y, q) at index i in W'_p"
      expect(table.globalizeEntryCount, 1);
      final entry = table.lookupByIndex(1);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 100); // writer address Y
      expect(entry.remoteAgent, 'q');

      //   - NO spawn info
      // Spec Section 5.1: "No goal is spawned—p will receive the assignment."
      expect(result.spawns, isEmpty);
    });

    test('reader variable: spawns global_send info, no entry', () {
      // Given: term with reader variable Y? at address 201, writer at 200
      final table = GlobalWritersTable('p');
      final variables = [TermVar.reader(201, writerAddr: 200)];

      // When: globalize(Y?, 'q')
      final result = globalize(
        variables: variables,
        localAgent: 'p',
        remoteAgent: 'q',
        table: table,
      );

      // Then:
      //   - term becomes _r(p, 1) (indices start at 1, 0 is reserved)
      expect(result.globalNames.length, 1);
      expect(result.globalNames[0], GlobalName.reader('p', 1));

      //   - spawn info for global_send(Y?, _r(p,1), q)
      //   - readerAddr is actually the writer address (used as onBind key)
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 200); // writer addr for onBind
      expect(result.spawns[0].globalName, GlobalName.reader('p', 1));
      expect(result.spawns[0].destAgent, 'q');

      //   - NO table entry created (only index allocated)
      // Spec Section 5.1: "No entry is created—the global_send goal handles
      // outgoing communication."
      expect(table.globalizeEntryCount, 0);
      expect(table.nextIndex, 2); // Index 1 was allocated
    });

    test('mixed term: correct handling of both', () {
      // Given: term [X, Y?] with writer X at 100 and reader Y? at 201
      final table = GlobalWritersTable('p');
      final variables = [TermVar.writer(100, readerAddr: 101), TermVar.reader(201, writerAddr: 200)];

      // When: globalize([X, Y?], 'q')
      final result = globalize(
        variables: variables,
        localAgent: 'p',
        remoteAgent: 'q',
        table: table,
      );

      // Then:
      //   - term becomes [_w(p, 1), _r(p, 2)]
      expect(result.globalNames.length, 2);
      expect(result.globalNames[0], GlobalName.writer('p', 1));
      expect(result.globalNames[1], GlobalName.reader('p', 2));

      //   - entry (X, q) at index 1
      // Spec Section 5.1: writer → entry
      expect(table.globalizeEntryCount, 1);
      final entry = table.lookupByIndex(1);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 100); // writer address X
      expect(entry.remoteAgent, 'q');

      //   - spawn info for global_send(Y?, _r(p,2), q)
      // Spec Section 5.1: reader → spawn
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 200); // writer addr for onBind
      expect(result.spawns[0].globalName, GlobalName.reader('p', 2));

      // Spec Section 5.3: "Writer globalized at p: creates entry.
      // Reader globalized at p: spawns global_send."
    });

    test('nested structure: recursive globalization', () {
      // Given: term foo(bar(X), Y?) with nested structure
      // Variables in order of occurrence: X at 100, Y? at 201
      final table = GlobalWritersTable('p');
      final variables = [TermVar.writer(100, readerAddr: 101), TermVar.reader(201, writerAddr: 200)];

      // When: globalize(foo(bar(X), Y?), 'q')
      final result = globalize(
        variables: variables,
        localAgent: 'p',
        remoteAgent: 'q',
        table: table,
      );

      // Then: correctly processes variables at all nesting levels
      // Same as mixed term - the globalize function processes a flat list
      // of variables extracted from the term (extraction is separate)
      expect(result.globalNames.length, 2);
      expect(result.globalNames[0].isWriter, true);
      expect(result.globalNames[1].isReader, true);

      // Spec Section 5.1 implies recursive traversal - we test the flat
      // variable list processing; term traversal is a separate concern
    });

    test('index allocation is sequential', () {
      // Given: term [X, Y, Z?] with multiple variables
      final table = GlobalWritersTable('p');
      final variables = [
        TermVar.writer(100, readerAddr: 101), // X
        TermVar.writer(200, readerAddr: 201), // Y
        TermVar.reader(301, writerAddr: 300), // Z?
      ];

      // When: globalize([X, Y, Z?], 'q')
      final result = globalize(
        variables: variables,
        localAgent: 'p',
        remoteAgent: 'q',
        table: table,
      );

      // Then: indices 1, 2, 3 allocated in order (0 reserved for serializer)
      // Spec Section 3.2: "A single counter is used for index allocation"
      expect(result.globalNames[0], GlobalName.writer('p', 1));
      expect(result.globalNames[1], GlobalName.writer('p', 2));
      expect(result.globalNames[2], GlobalName.reader('p', 3));

      expect(table.nextIndex, 4);
    });
  });
}
