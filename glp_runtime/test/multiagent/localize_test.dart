/// Tests for Localize operation
///
/// Derived from madGLP-spec.md Section 5.2: Localize
///
/// Given agent q, remote agent p, and globalized term T_p↑, localization
/// produces T_q↓ with fresh local pairs, spawns global_send goals for _w
/// names, and creates table entries for _r names.

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

void main() {
  group('Localize', () {
    test('_w(p,i): spawns global_send, returns writer', () {
      // Given: globalized term _w(p, 5)
      final table = GlobalWritersTable('q');
      final globalNames = [GlobalName.writer('p', 5)];

      var nextAddr = 100;
      (int, int) allocateAddr() {
        final w = nextAddr++;
        final r = nextAddr++;
        return (w, r);
      }

      // When: localize at agent q
      final result = localize(
        globalNames: globalNames,
        localAgent: 'q',
        table: table,
        freshAddrAllocator: allocateAddr,
      );

      // Then:
      //   - creates fresh pair (Y_q, Y_q?)
      expect(result.freshPairs.length, 1);
      expect(result.freshPairs[0].writerAddr, 100);
      expect(result.freshPairs[0].readerAddr, 101);

      //   - term gets Y_q (the writer)
      // Spec Section 5.2: "replace _w(p, i) with Y_q (the writer)"
      expect(result.useReader[0], false);

      //   - spawn info for global_send(Y_q?, _w(p,5), p)
      //   - readerAddr is actually the writer address (used as onBind key)
      // Spec Section 5.2: "spawn global_send(Y_q?, _w(p,i), p)"
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 100); // writer addr for onBind
      expect(result.spawns[0].globalName, GlobalName.writer('p', 5));
      expect(result.spawns[0].destAgent, 'p');

      //   - NO table entry
      // Spec Section 5.2: "No entry is created—the global_send goal handles
      // outgoing communication."
      expect(table.localizeEntryCount, 0);
    });

    test('_r(p,i): creates entry with remote index, returns reader', () {
      // Given: globalized term _r(p, 3)
      final table = GlobalWritersTable('q');
      final globalNames = [GlobalName.reader('p', 3)];

      var nextAddr = 200;
      (int, int) allocateAddr() {
        final w = nextAddr++;
        final r = nextAddr++;
        return (w, r);
      }

      // When: localize at agent q
      final result = localize(
        globalNames: globalNames,
        localAgent: 'q',
        table: table,
        freshAddrAllocator: allocateAddr,
      );

      // Then:
      //   - creates fresh pair (Z_q, Z_q?)
      expect(result.freshPairs.length, 1);
      expect(result.freshPairs[0].writerAddr, 200);
      expect(result.freshPairs[0].readerAddr, 201);

      //   - entry (Z_q, p, 3) added to table
      // Spec Section 5.2: "add entry (Z_q, p, i)"
      expect(table.localizeEntryCount, 1);
      final entry = table.findByRemote('p', 3);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 200);
      expect(entry.remoteAgent, 'p');
      expect(entry.remoteIndex, 3);

      //   - term gets Z_q? (the reader)
      // Spec Section 5.2: "replace _r(p, i) with Z_q? (the reader)"
      expect(result.useReader[0], true);

      //   - NO spawn info
      // Spec Section 5.2: "No goal is spawned—q will receive the assignment."
      expect(result.spawns, isEmpty);
    });

    test('mixed global names: correct handling', () {
      // Given: term [_w(p, 1), _r(p, 2)]
      final table = GlobalWritersTable('q');
      final globalNames = [
        GlobalName.writer('p', 1),
        GlobalName.reader('p', 2),
      ];

      var nextAddr = 300;
      (int, int) allocateAddr() {
        final w = nextAddr++;
        final r = nextAddr++;
        return (w, r);
      }

      // When: localize at agent q
      final result = localize(
        globalNames: globalNames,
        localAgent: 'q',
        table: table,
        freshAddrAllocator: allocateAddr,
      );

      // Then:
      //   - term becomes [Y_q, Z_q?]
      expect(result.freshPairs.length, 2);
      expect(result.useReader[0], false); // Y_q (writer) for _w
      expect(result.useReader[1], true); // Z_q? (reader) for _r

      //   - spawn info for Y_q's gs: global_send(Y_q?, _w(p,1), p)
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 300); // First pair's writer address
      expect(result.spawns[0].globalName, GlobalName.writer('p', 1));
      expect(result.spawns[0].destAgent, 'p');

      //   - entry for Z_q with remote (p, 2)
      expect(table.localizeEntryCount, 1);
      final entry = table.findByRemote('p', 2);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 302); // Second pair's writer address

      // Spec Section 5.3: Globalize-Localize Correspondence
    });
  });
}
