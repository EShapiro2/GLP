/// Tests for Localize operation
///
/// Derived from madGLP-spec.md Section 5.2: Localize
///
/// Given agent q, remote agent p, and globalized term T_p↑, localization
/// produces T_q↓ with fresh local pairs, creates table entries, and spawns
/// global_send goals.

import 'package:test/test.dart';

void main() {
  group('Localize', () {
    test('_w(p,i): creates entry with remote index, returns reader', () {
      // Given: globalized term _w(p, 5)
      // When: localize at agent q
      // Then:
      //   - creates fresh pair (Y_q, Y_q?)
      //   - entry (Y_q, p, 5) added to table
      //   - term gets Y_q? (the reader)
      //   - NO spawn info
      //
      // Spec Section 5.2: "If _w(p, i): create fresh local pair (Y_q, Y_q?),
      // allocate the next index k, add entry (Y_q, p, i), and replace
      // _w(p, i) with Y_q? (the reader). No goal is spawned."
    }, skip: 'Not yet implemented');

    test('_r(p,i): spawns global_send, returns writer', () {
      // Given: globalized term _r(p, 3)
      // When: localize at agent q
      // Then:
      //   - creates fresh pair (Z_q, Z_q?)
      //   - term gets Z_q (the writer)
      //   - spawn info for global_send(Z_q?, _r(p,3), p)
      //   - NO table entry
      //
      // Spec Section 5.2: "If _r(p, i): create fresh local pair (Z_q, Z_q?),
      // replace _r(p, i) with Z_q (the writer), and spawn goal
      // global_send(Z_q?, _r(p,i), p). No entry is created."
    }, skip: 'Not yet implemented');

    test('mixed global names: correct handling', () {
      // Given: term [_w(p, 0), _r(p, 1)]
      // When: localize at agent q
      // Then:
      //   - term becomes [Y_q?, Z_q]
      //   - entry for Y_q with remote (p, 0)
      //   - spawn info for Z_q watching Z_q?
      //
      // Spec Section 5.3: Globalize-Localize Correspondence
    }, skip: 'Not yet implemented');
  });
}
