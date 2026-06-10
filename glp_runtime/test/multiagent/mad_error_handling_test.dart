/// Tests for madGLP error handling
///
/// Derived from madGLP-spec.md Section 12: Invariants (negative cases)
///
/// These tests verify proper error handling when invariants are violated
/// or edge cases are encountered.

import 'package:test/test.dart';

void main() {
  group('Error Handling', () {
    test('receive for non-existent GlobalizeEntry throws', () {
      // Given: no entry at index 5 in table
      // When: receive message _r(p, 5) := T
      // Then: throws StateError
      //
      // Spec Section 8.3: "Agent p finds entry (X, q) at index i in W_p"
      // If entry doesn't exist, this is an error condition
    }, skip: 'Not yet implemented');

    test('early _r assignment with no LocalizeEntry is held until the entry exists', () {
      // Issue 7 / spec §8.3 (Early Messages): a `_r(p, i) := T` arriving before
      // its LocalizeEntry exists is HELD (keyed by (p, i)) and delivered when
      // localize() creates the entry — it is NOT dropped or thrown. This
      // overrides the earlier "throws StateError" contract.
      //
      // Implemented and tested elsewhere: mad_transactions_test.dart (unit:
      // held → delivered) and reverse_order_delivery_test.dart (integration:
      // reverse-order cold-call reaches the same outcome).
    }, skip: 'Covered by mad_transactions_test.dart and reverse_order_delivery_test.dart');

    test('duplicate LocalizeEntry rejected', () {
      // Given: entry (X, p, 5) already exists
      // When: addLocalizeEntry(Y, p, 5)
      // Then: throws ArgumentError
      //
      // Spec Section 12: "Entry Lifecycle: Every global writers table entry
      // is created exactly once"
    }, skip: 'Not yet implemented');

    test('global_send on already-known reader is no-op', () {
      // Given: reader X? already bound (known)
      // When: attempt to register global_send callback
      // Then: callback fires immediately or registration is skipped (no error)
      //
      // This is an edge case where the reader becomes known before
      // callback registration completes
    }, skip: 'Not yet implemented');

    test('removing non-existent entry is safe', () {
      // Given: no entry at index 3
      // When: removeGlobalizeEntry(3)
      // Then: no error (idempotent removal)
      //
      // Implementation choice: removal of missing entry is a no-op
    }, skip: 'Not yet implemented');
  });
}
