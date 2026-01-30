/// Tests for GlobalWritersTable
///
/// Derived from madGLP-spec.md Section 3: Global Writers Table
///
/// The global writers table tracks local writers that await incoming
/// assignments from remote agents. Two entry types:
/// - GlobalizeEntry (X, q): created when exporting a reader
/// - LocalizeEntry (X, q, i): created when importing a writer global name

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';

void main() {
  group('GlobalWritersTable', () {
    // Entry creation tests (spec Section 3.2)

    test('addGlobalizeEntry allocates sequential indices', () {
      // Given: empty table
      final table = GlobalWritersTable('p');

      // When: add two Globalize entries
      final i1 = table.addGlobalizeEntry(100, 'q');
      final i2 = table.addGlobalizeEntry(200, 'r');

      // Then: indices are 0, 1
      // Spec: "A single counter is used for index allocation at each agent"
      expect(i1, 0);
      expect(i2, 1);
      expect(table.nextIndex, 2);
    });

    test('addLocalizeEntry stores remote index', () {
      // Given: empty table
      final table = GlobalWritersTable('q');

      // When: addLocalizeEntry(100, 'p', 5) for _w(p,5)
      table.addLocalizeEntry(100, 'p', 5);

      // Then: findByRemote('p', 5) returns entry with writerAddr=100
      // Spec Section 3.1: "LocalizeEntry (X, q, i): X is local writer,
      // q is remote agent, i is index in q's global name"
      final entry = table.findByRemote('p', 5);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 100);
      expect(entry.remoteAgent, 'p');
      expect(entry.remoteIndex, 5);
    });

    // Lookup tests

    test('lookupByIndex returns GlobalizeEntry at index', () {
      // Given: table with GlobalizeEntry at index 0
      final table = GlobalWritersTable('p');
      final i = table.addGlobalizeEntry(100, 'q');

      // When: lookupByIndex(0)
      final entry = table.lookupByIndex(i);

      // Then: returns entry with correct writerAddr
      // Spec Section 11.2: "For entries created by Globalize, lookup is
      // direct by index—the entry at index i corresponds to _r(p, i)"
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 100);
      expect(entry.remoteAgent, 'q');
    });

    test('findByRemote searches LocalizeEntries', () {
      // Given: multiple LocalizeEntries with different (agent, index) pairs
      final table = GlobalWritersTable('q');
      table.addLocalizeEntry(100, 'p', 0);
      table.addLocalizeEntry(200, 'p', 1);
      table.addLocalizeEntry(300, 'r', 0);

      // When/Then: each findByRemote returns correct entry or null
      // Spec Section 11.2: "For entries created by Localize, lookup requires
      // searching for a matching (q, i) pair"
      expect(table.findByRemote('p', 0)?.writerAddr, 100);
      expect(table.findByRemote('p', 1)?.writerAddr, 200);
      expect(table.findByRemote('r', 0)?.writerAddr, 300);
      expect(table.findByRemote('p', 2), isNull);
      expect(table.findByRemote('s', 0), isNull);
    });

    // Entry removal tests (spec Section 3.2)

    test('removeGlobalizeEntry leaves gaps (indices not reused)', () {
      // Given: entries at indices 0, 1
      final table = GlobalWritersTable('p');
      table.addGlobalizeEntry(100, 'q'); // index 0
      table.addGlobalizeEntry(200, 'r'); // index 1

      // When: removeGlobalizeEntry(0)
      table.removeGlobalizeEntry(0);

      // Then: lookupByIndex(0) returns null
      expect(table.lookupByIndex(0), isNull);
      expect(table.lookupByIndex(1), isNotNull);

      // And: new entry gets index 2, not reusing 0
      // Spec: "Indices are never reused. Implementations may use a sparse
      // representation (e.g., a map from index to entry)"
      final i3 = table.addGlobalizeEntry(300, 's');
      expect(i3, 2);
    });

    test('removeLocalizeEntry by remote agent and index', () {
      // Given: LocalizeEntry for (p, 5)
      final table = GlobalWritersTable('q');
      table.addLocalizeEntry(100, 'p', 5);

      // Verify entry exists
      expect(table.findByRemote('p', 5), isNotNull);

      // When: removeLocalizeEntry('p', 5)
      table.removeLocalizeEntry('p', 5);

      // Then: findByRemote('p', 5) returns null
      // Spec Section 3.2: "When an assignment message arrives and the
      // corresponding writer is bound, the entry is removed"
      expect(table.findByRemote('p', 5), isNull);
    });
  });
}
