// GAS's catalog on a running downloads server (GAS_DOWNLOADS, e.g.
// https://talmonlab.bgu.ac.il/downloads): read, and a contract the app does
// not carry got by its hash, checked and installed; self-skips without it.
import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/contracts.dart';
import 'package:gas_phone/main.dart' show unbundle;

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  // The binding answers every HTTP request 400; this test talks to GAS.
  setUp(() => HttpOverrides.global = null);
  test('the catalog, and a contract got from it', () async {
    final server = Platform.environment['GAS_DOWNLOADS'];
    if (server == null) return markTestSkipped('GAS_DOWNLOADS not set');
    final dir = Directory.systemTemp.createTempSync('gas_catalog_');
    addTearDown(() => dir.deleteSync(recursive: true));
    final store = ContractStore(await unbundle(dir.path), server: server,
        madeServer: Platform.environment['GAS_MADE_SERVER'] ?? made);
    await store.installStarters();
    final cat = await store.catalog();
    expect(cat.map((c) => c.id), containsAll(['GKK', 'GC', 'AGORA', 'GSN']));
    final mine = {for (final c in store.installed()) c.hash};
    expect(cat.where((c) => mine.contains(c.hash)).map((c) => c.id), containsAll(['GKK', 'GC', 'AGORA']),
        reason: 'the starters are the catalog\'s');
    final gsn = cat.firstWhere((c) => c.id == 'GSN');
    final got = await store.get(gsn.hash);
    expect(got.hash, gsn.hash);
    expect(File('${store.program(got)}/platform.vglp').existsSync(), isTrue);
    expect(store.installed().map((c) => c.id), contains('GSN'));

    // An app made with the app maker, in no catalog: got from GAS's made
    // apps by its hash (GAS_MADE_HASH, one made on that server).
    final madeHash = Platform.environment['GAS_MADE_HASH'];
    if (madeHash != null) {
      final m = await store.get(madeHash);
      expect(m.hash, madeHash);
      expect(File('${store.program(m)}/self.glp').existsSync(), isTrue);
    }
  });
}
