/// Tests for Localize operation
///
/// Derived from madGLP-spec.md Section 5.2: Localize
///
/// Given agent q, remote agent p, and globalized term T_p↑, localization
/// produces T_q↓ with fresh local pairs, creates table entries, and spawns
/// global_send goals.

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

void main() {
  group('Localize', () {
    test('_w(p,i): creates entry with remote index, returns reader', () {
      // Given: globalized term _w(p, 5)
      final table = GlobalWritersTable('q');
      final globalNames = [GlobalName.writer('p', 5)];

      var nextAddr = 100;
      int allocateAddr() => nextAddr++;

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

      //   - entry (Y_q, p, 5) added to table
      // Spec Section 5.2: "add entry (Y_q, p, i)"
      expect(table.localizeEntryCount, 1);
      final entry = table.findByRemote('p', 5);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 100);
      expect(entry.remoteAgent, 'p');
      expect(entry.remoteIndex, 5);

      //   - term gets Y_q? (the reader)
      expect(result.useReader[0], true);

      //   - NO spawn info
      expect(result.spawns, isEmpty);
    });

    test('_r(p,i): spawns global_send, returns writer', () {
      // Given: globalized term _r(p, 3)
      final table = GlobalWritersTable('q');
      final globalNames = [GlobalName.reader('p', 3)];

      var nextAddr = 200;
      int allocateAddr() => nextAddr++;

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

      //   - term gets Z_q (the writer)
      expect(result.useReader[0], false);

      //   - spawn info for global_send(Z_q?, _r(p,3), p)
      // Spec Section 5.2: "spawn goal global_send(Z_q?, _r(p,i), p)"
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 200);
      expect(result.spawns[0].globalName, GlobalName.reader('p', 3));
      expect(result.spawns[0].destAgent, 'p');

      //   - NO table entry
      expect(table.localizeEntryCount, 0);
    });

    test('mixed global names: correct handling', () {
      // Given: term [_w(p, 0), _r(p, 1)]
      final table = GlobalWritersTable('q');
      final globalNames = [
        GlobalName.writer('p', 0),
        GlobalName.reader('p', 1),
      ];

      var nextAddr = 300;
      int allocateAddr() => nextAddr++;

      // When: localize at agent q
      final result = localize(
        globalNames: globalNames,
        localAgent: 'q',
        table: table,
        freshAddrAllocator: allocateAddr,
      );

      // Then:
      //   - term becomes [Y_q?, Z_q]
      expect(result.freshPairs.length, 2);
      expect(result.useReader[0], true); // Y_q? (reader) for _w
      expect(result.useReader[1], false); // Z_q (writer) for _r

      //   - entry for Y_q with remote (p, 0)
      expect(table.localizeEntryCount, 1);
      final entry = table.findByRemote('p', 0);
      expect(entry, isNotNull);
      expect(entry!.writerAddr, 300); // First allocated address

      //   - spawn info for Z_q watching Z_q?
      expect(result.spawns.length, 1);
      expect(result.spawns[0].readerAddr, 301); // Second allocated address
      expect(result.spawns[0].globalName, GlobalName.reader('p', 1));
      expect(result.spawns[0].destAgent, 'p');

      // Spec Section 5.3: Globalize-Localize Correspondence
    });
  });
}
