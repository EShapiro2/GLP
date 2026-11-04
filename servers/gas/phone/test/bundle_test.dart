// The app's first steps on a phone, off one: the bundle unpacked from the
// app's assets into a directory, its starter contracts installed as packages
// checked against their hashes, each where the agent and the page read it;
// a package that does not match its hash refused; and the links read.
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/contracts.dart';
import 'package:gas_phone/main.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  test('the bundle unpacks, and its starters install', () async {
    final dir = Directory.systemTemp.createTempSync('gas_bundle_');
    addTearDown(() => dir.deleteSync(recursive: true));
    final base = await unbundle(dir.path);
    final store = ContractStore(base);
    await store.installStarters();
    final mine = store.installed();
    expect(mine.map((c) => c.id), containsAll(['GKK', 'GC', 'AGORA']));
    expect(File('$base/programs/self.glp').existsSync(), isTrue);
    expect(File('$base/www/app.html').existsSync(), isTrue);
    final gkk = mine.firstWhere((c) => c.id == 'GKK');
    expect(gkk.name, 'gkk_${gkk.hash.substring(0, 8)}');
    expect(File('${store.program(gkk)}/self.glp').existsSync(), isTrue);
    final spec = jsonDecode(File('$base/www/specs/${gkk.name}.json').readAsStringSync()) as Map;
    expect((spec['meta'] as Map)['icon'], isNotEmpty);
    // Installing again changes nothing.
    await store.installStarters();
    expect(store.installed().length, mine.length);

    final pkg = Directory('$base/packages').listSync().whereType<File>().firstWhere((f) => f.path.contains('pkg-'));
    final bytes = Uint8List.fromList([...await pkg.readAsBytes()]);
    bytes[bytes.length - 2] ^= 1;
    await expectLater(store.install(bytes, expect: gkk.hash), throwsFormatException);
  });

  test('GAS links', () {
    final k = 'a' * 64, h = 'b' * 64;
    final add = GasLink.parse('Join me in GKK on GAS: https://talmonlab.bgu.ac.il/gas/add.html#$k&c=$h');
    expect([add?.person, add?.contract], [k, h]);
    final get = GasLink.parse(contractLink(h));
    expect([get?.person, get?.contract], [null, h]);
    expect(GasLink.parse('gas://add/$k?c=$h')?.contract, h);
    expect(GasLink.parse('gas://get/$h')?.contract, h);
    expect(GasLink.parse('https://talmonlab.bgu.ac.il/gas/add.html#$k')?.person, k);
    expect(GasLink.parse('hello'), isNull);
  });
}
