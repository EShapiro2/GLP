/// Tests for GlobalWritersTable
///
/// Derived from madGLP-spec.md Section 3: Global Writers Table
///
/// The global writers table tracks local writers that await incoming
/// assignments from remote agents. Two entry types:
/// - GlobalizeEntry (X, q): created when exporting a reader
/// - LocalizeEntry (X, q, i): created when importing a writer global name
///
/// Index 0 is reserved for the serializer (network input stream).
/// Regular indices start at 1.

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';

void main() {
  group('GlobalWritersTable', () {
    // Index-0 serializer tests (spec Section 4.1)

    test('index 0 is reserved for serializer', () {
      // Given: empty table
      final table = GlobalWritersTable('p');

      // Then: nextIndex starts at 1 (0 is reserved)
      // Spec Section 3.2: "Index 0 is reserved for the network input serializer.
      // The counter starts at 1"
      expect(table.nextIndex, 1);
    });

    test('initializeSerializerEntry sets up index 0', () {
      // Given: empty table
      final table = GlobalWritersTable('p');

      // When: initialize serializer entry
      table.initializeSerializerEntry(999);

      // Then: serializer writer address is set
      // Spec Section 4.1: "At boot time, each agent p creates a permanent entry
      // at index 0 mapping `_r(p, 0)` to the local writer N_p"
      expect(table.hasSerializerEntry, isTrue);
      expect(table.serializerWriterAddr, 999);
    });

    test('updateSerializerWriter updates the entry', () {
      // Given: table with initialized serializer
      final table = GlobalWritersTable('p');
      table.initializeSerializerEntry(999);

      // When: update serializer writer
      table.updateSerializerWriter(1001);

      // Then: new writer address is stored
      // Spec Section 8.3: "Update the entry to `(N'_q, *)` at index 0"
      expect(table.serializerWriterAddr, 1001);
    });

    test('removeGlobalizeEntry does not remove index 0', () {
      // Given: table with serializer entry
      final table = GlobalWritersTable('p');
      table.initializeSerializerEntry(999);

      // When: try to remove index 0
      table.removeGlobalizeEntry(0);

      // Then: serializer entry is still there
      // Spec Section 4.1: "This entry is never removed."
      expect(table.hasSerializerEntry, isTrue);
      expect(table.serializerWriterAddr, 999);
    });

    // Entry creation tests (spec Section 3.2)

    test('addGlobalizeEntry allocates sequential indices starting at 1', () {
      // Given: empty table
      final table = GlobalWritersTable('p');

      // When: add two Globalize entries
      final i1 = table.addGlobalizeEntry(100, 'q');
      final i2 = table.addGlobalizeEntry(200, 'r');

      // Then: indices are 1, 2 (0 is reserved for serializer)
      // Spec: "A single counter is used for index allocation at each agent"
      expect(i1, 1);
      expect(i2, 2);
      expect(table.nextIndex, 3);
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
      // Given: entries at indices 1, 2 (0 is reserved for serializer)
      final table = GlobalWritersTable('p');
      table.addGlobalizeEntry(100, 'q'); // index 1
      table.addGlobalizeEntry(200, 'r'); // index 2

      // When: removeGlobalizeEntry(1)
      table.removeGlobalizeEntry(1);

      // Then: lookupByIndex(1) returns null
      expect(table.lookupByIndex(1), isNull);
      expect(table.lookupByIndex(2), isNotNull);

      // And: new entry gets index 3, not reusing 1
      // Spec: "Indices are never reused. Implementations may use a sparse
      // representation (e.g., a map from index to entry)"
      final i3 = table.addGlobalizeEntry(300, 's');
      expect(i3, 3);
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
