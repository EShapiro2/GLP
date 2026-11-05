// The app's starters installed from its bundle tree (GAS_TREE, made by
// ../phone/tool/sync_assets.sh), each only after its warranties check on
// this machine (contracts.dart, warranty.dart): each is listed with what was
// found of them, its contract unit where the harness reads it; a package
// whose code is changed and hashed again is refused, and so is one whose
// contract unit writes to the person itself.  Self-skips without the tree.
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:gas_core/contracts.dart';
import 'package:test/test.dart';

void copyTree(Directory from, Directory to) {
  for (final e in from.listSync(recursive: true)) {
    final rel = e.path.substring(from.path.length);
    if (e is Directory) {
      Directory('${to.path}$rel').createSync(recursive: true);
    } else if (e is File) {
      File('${to.path}$rel').parent.createSync(recursive: true);
      e.copySync('${to.path}$rel');
    }
  }
}

void main() {
  test('the starters install only as warranted', () async {
    final tree = Directory(Platform.environment['GAS_TREE'] ?? '../phone/assets/tree');
    if (!Directory('${tree.path}/packages').existsSync()) {
      return markTestSkipped('no bundle tree: run ../phone/tool/sync_assets.sh');
    }
    final base = Directory.systemTemp.createTempSync('gas_install_');
    addTearDown(() => base.deleteSync(recursive: true));
    copyTree(tree.absolute, base);
    final store = ContractStore(base.path);
    await store.installStarters();
    final mine = store.installed();
    expect(mine.map((c) => c.id), containsAll(['GKK', 'GC', 'AGORA']));
    for (final c in mine) {
      expect(File('${store.program(c)}/contract/platform.vglp').existsSync(), isTrue);
      final w = store.warrantyOf(c)!;
      expect(w['verdict'], startsWith('syntactically_grassroots'));
      expect(w['compiler'], matches(RegExp(r'^[0-9a-f]{64}$')));
      expect(w['creator'], matches(RegExp(r'^[0-9a-f]{64}$')));
    }
    expect(Directory('${base.path}/programs/vglp/cva/gas_worlds/certified').listSync(), isEmpty,
        reason: 'the directory a unit is compiled in is removed after');

    final pkg = Directory('${base.path}/packages')
        .listSync()
        .whereType<File>()
        .firstWhere((f) => jsonDecode(f.readAsStringSync())['id'] == 'GKK');
    final j = jsonDecode(pkg.readAsStringSync()) as Map;

    // The platform changed, the package hashed again: other code than warranted.
    final changed = jsonDecode(jsonEncode(j)) as Map;
    final files = changed['files'] as Map;
    files['contract/platform.vglp'] =
        (files['contract/platform.vglp'] as String).replaceFirst('tab("Contacts", knows)', 'tab("People", knows)');
    await expectLater(store.install(Uint8List.fromList(utf8.encode(jsonEncode(changed)))),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('other code'))));

    // The contract unit writes to the person itself: refused a certificate.
    final rogue = jsonDecode(jsonEncode(j)) as Map;
    final rf = rogue['files'] as Map;
    rf['contract/self.glp'] = (rf['contract/self.glp'] as String)
        .replaceFirst('    to_person(M?, T?).', '    to_person(M?, T?),\n    send_to_user([hello]).');
    await expectLater(store.install(Uint8List.fromList(utf8.encode(jsonEncode(rogue)))),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('reaches'))));

    // A package of the GAS before warranties.
    final old = jsonDecode(jsonEncode(j)) as Map..['gas'] = 1;
    await expectLater(store.install(Uint8List.fromList(utf8.encode(jsonEncode(old)))), throwsFormatException);
  }, timeout: const Timeout(Duration(minutes: 5)));
}
