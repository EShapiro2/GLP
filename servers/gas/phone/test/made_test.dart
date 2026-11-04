// An app made by GAS's app maker (servers/gas/tools/make.py), its package
// GAS_MADE (a pkg-<hash>.json) installed as the phone installs one: two
// persons establish, acquaint, and run its acts.  The default package is
// HELLO, GKK with a greeting (make.py's own example): Ann greets, and Ben's
// view holds greeted(Ann).  Self-skips without GAS_MADE.
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/contracts.dart';

import 'people.dart';

void main() {
  test('a made app runs between two persons', () async {
    final path = Platform.environment['GAS_MADE'];
    if (path == null) return markTestSkipped('GAS_MADE not set');
    final why = await cannotRun('gkk');
    if (why != null) return markTestSkipped(why);
    final base = Directory.systemTemp.createTempSync('gas_made_');
    addTearDown(() => base.deleteSync(recursive: true));
    // The installed contract runs in the repository's scope, its directory
    // beside the other worlds as on the phone: the base's programs/ is the
    // repository's.
    Link('${base.path}/programs').createSync(programs);
    final store = ContractStore(base.path, server: 'http://127.0.0.1:9');
    final c = await store.install(Uint8List.fromList(await File(path).readAsBytes()));
    addTearDown(() => Directory(store.program(c)).deleteSync(recursive: true));
    final [ann, ben] = await people(c.name, 2, program: Directory(store.program(c)).resolveSymbolicLinksSync());
    final all = [ann, ben];
    for (final p in all) {
      await propose(p, S('establish', [p.me]));
    }
    await propose(ann, S('acquaint', [ann.me, ben.me]));
    await until(all, () => hasAtom(ann, 'knows', [ben.me]) && hasAtom(ben, 'knows', [ann.me]), 'acquainted');
    if (c.id == 'HELLO') {
      await propose(ann, S('greet', [ann.me, ben.me]));
      await until(all, () => hasAtom(ben, 'greeted', [ann.me]), 'Ben greeted by Ann');
    }
  }, timeout: const Timeout(Duration(minutes: 5)));
}
