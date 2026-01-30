/// Tests for GlobalWritersTable
///
/// Derived from madGLP-spec.md Section 3: Global Writers Table
///
/// The global writers table tracks local writers that await incoming
/// assignments from remote agents. Two entry types:
/// - GlobalizeEntry (X, q): created when exporting a reader
/// - LocalizeEntry (X, q, i): created when importing a writer global name

import 'package:test/test.dart';

void main() {
  group('GlobalWritersTable', () {
    // Entry creation tests (spec Section 3.2)

    test('addGlobalizeEntry allocates sequential indices', () {
      // Given: empty table
      // When: add two Globalize entries
      // Then: indices are 0, 1
      //
      // Spec: "A single counter is used for index allocation at each agent"
    }, skip: 'Not yet implemented');

    test('addLocalizeEntry stores remote index', () {
      // Given: empty table
      // When: addLocalizeEntry(100, 'p', 5) for _w(p,5)
      // Then: findByRemote('p', 5) returns entry with writerAddr=100
      //
      // Spec Section 3.1: "LocalizeEntry (X, q, i): X is local writer,
      // q is remote agent, i is index in q's global name"
    }, skip: 'Not yet implemented');

    // Lookup tests

    test('lookupByIndex returns GlobalizeEntry at index', () {
      // Given: table with GlobalizeEntry at index 0
      // When: lookupByIndex(0)
      // Then: returns entry with correct writerAddr
      //
      // Spec Section 11.2: "For entries created by Globalize, lookup is
      // direct by index—the entry at index i corresponds to _r(p, i)"
    }, skip: 'Not yet implemented');

    test('findByRemote searches LocalizeEntries', () {
      // Given: multiple LocalizeEntries with different (agent, index) pairs
      // When: findByRemote('p', 0), findByRemote('p', 1), findByRemote('r', 0)
      // Then: each returns correct entry or null
      //
      // Spec Section 11.2: "For entries created by Localize, lookup requires
      // searching for a matching (q, i) pair"
    }, skip: 'Not yet implemented');

    // Entry removal tests (spec Section 3.2)

    test('removeGlobalizeEntry leaves gaps (indices not reused)', () {
      // Given: entries at indices 0, 1
      // When: removeGlobalizeEntry(0), then add new entry
      // Then: lookupByIndex(0) returns null, new entry at index 2
      //
      // Spec: "Indices are never reused. Implementations may use a sparse
      // representation (e.g., a map from index to entry)"
    }, skip: 'Not yet implemented');

    test('removeLocalizeEntry by remote agent and index', () {
      // Given: LocalizeEntry for (p, 5)
      // When: removeLocalizeEntry('p', 5)
      // Then: findByRemote('p', 5) returns null
      //
      // Spec Section 3.2: "When an assignment message arrives and the
      // corresponding writer is bound, the entry is removed"
    }, skip: 'Not yet implemented');
  });
}
