import 'dart:io';

import 'package:gpw_name_service/gpw_name_service.dart';
import 'package:test/test.dart';

void main() {
  late Directory tmp;
  late SigningKey person;

  setUp(() async {
    tmp = Directory.systemTemp.createTempSync('gpw_area');
    person = await SigningKey.generate();
    File('${tmp.path}/index.html')
        .writeAsStringSync('<html><body>hello</body></html>');
    Directory('${tmp.path}/notes').createSync();
    File('${tmp.path}/notes/one.html').writeAsStringSync('<p>one</p>');
  });

  tearDown(() => tmp.deleteSync(recursive: true));

  Future<List<int>?> fetchFromDisk(String path) async {
    final f = File('${tmp.path}$path');
    return f.existsSync() ? f.readAsBytesSync() : null;
  }

  test('signed area verifies clean', () async {
    await signArea(tmp, person, 'me.peoplesweb.org', 1);
    expect(await verifyArea(fetchFromDisk, person.publicKeyB64), isEmpty);
  });

  test('a withheld page is detected', () async {
    await signArea(tmp, person, 'me.peoplesweb.org', 1);
    File('${tmp.path}/notes/one.html').deleteSync();
    final problems = await verifyArea(fetchFromDisk, person.publicKeyB64);
    expect(problems, hasLength(1));
    expect(problems.single, contains('withheld: /notes/one.html'));
  });

  test('tampered page bytes are detected as forgery', () async {
    await signArea(tmp, person, 'me.peoplesweb.org', 1);
    File('${tmp.path}/index.html').writeAsStringSync('<html>tampered</html>');
    final problems = await verifyArea(fetchFromDisk, person.publicKeyB64);
    expect(problems.single, contains('forged: /index.html'));
  });

  test('a manifest signed by another key is rejected', () async {
    final impostor = await SigningKey.generate();
    await signArea(tmp, impostor, 'me.peoplesweb.org', 1);
    final problems = await verifyArea(fetchFromDisk, person.publicKeyB64);
    expect(problems.single, contains('forged: area manifest'));
  });

  test('missing area manifest is withholding', () async {
    expect((await verifyArea(fetchFromDisk, person.publicKeyB64)).single,
        contains('no area manifest'));
  });
}
