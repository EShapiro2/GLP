// The app's first steps on a phone, off one: the bundle unpacked from the
// app's assets into a directory, the contracts it carries listed, and a
// person's agent of GKK started from the unpacked tree alone.
import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:gas_phone/main.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  test('the bundle unpacks, and lists the contracts it carries', () async {
    final dir = Directory.systemTemp.createTempSync('gas_bundle_');
    addTearDown(() => dir.deleteSync(recursive: true));
    final base = await unbundle(dir.path);
    final list = await contracts(base);
    expect(list.map((c) => c.$1), containsAll(['gkk', 'gc', 'agora']));
    expect(File('$base/programs/self.glp').existsSync(), isTrue);
    expect(File('$base/www/app.html').existsSync(), isTrue);
    expect(File('$base/programs/vglp/cva/gas_worlds/live_gkk/self.glp').existsSync(), isTrue);
  });
}
