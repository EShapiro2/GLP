// GAS on the web (bin/webagents.dart), as a browser meets it: a person made by
// api/start, known by their token after, refused without one; their apps, GKK
// alone; GKK opened, their agent running on the server; and the server's
// count of them.  Needs the app's bundle tree (GAS_TREE, made by
// ../phone/tool/sync_assets.sh) and libsodium; self-skips without the tree.
import 'dart:convert';
import 'dart:io';

import 'package:test/test.dart';

void main() {
  test('a person of GAS on the web, and their agent', () async {
    final tree = Platform.environment['GAS_TREE'] ?? '../phone/assets/tree';
    if (!Directory('$tree/programs').existsSync()) return markTestSkipped('no bundle tree: run ../phone/tool/sync_assets.sh');
    final data = Directory.systemTemp.createTempSync('gasweb_');
    addTearDown(() => data.deleteSync(recursive: true));
    final s = await ServerSocket.bind('127.0.0.1', 0);
    final port = s.port;
    await s.close();
    final p = await Process.start(Platform.resolvedExecutable, ['run', 'bin/webagents.dart'],
        environment: {'DATA': data.path, 'TREE': Directory(tree).absolute.path, 'PORT': '$port', 'RELAY_URL': 'http://127.0.0.1:9'});
    addTearDown(p.kill);
    final up = await p.stdout.transform(utf8.decoder).transform(const LineSplitter()).first;
    expect(up, startsWith('gasweb on'));

    final http = HttpClient();
    Future<(int, Object?)> call(String path, {String? token, Object? body}) async {
      final r = body == null
          ? await http.getUrl(Uri.parse('http://127.0.0.1:$port/$path'))
          : await http.postUrl(Uri.parse('http://127.0.0.1:$port/$path'));
      if (token != null) r.headers.set('X-GAS-Token', token);
      if (body != null) r.write(jsonEncode(body));
      final res = await r.close();
      final t = await utf8.decodeStream(res);
      return (res.statusCode, t.isEmpty ? null : jsonDecode(t));
    }

    expect((await call('api/home')).$1, 401);
    final (_, start) = await call('api/start', body: {});
    final token = (start as Map)['token'] as String;
    final (code, home) = await call('api/home', token: token);
    expect(code, 200);
    expect((home as Map)['hosted'], isTrue);
    final apps = [for (final a in home['apps'] as List) a['id']];
    expect(apps, ['GKK']);
    final gkk = (home['apps'] as List).first['name'] as String;
    expect((await call('api/open', token: token, body: {'name': gkk})).$2, containsPair('ok', true));
    var status = '';
    for (var i = 0; i < 120 && status != 'ready'; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 500));
      status = (((await call('api/$gkk/state', token: token)).$2 as Map)['status'] ?? '') as String;
    }
    expect(status, 'ready');
    expect((await call('api/health')).$2, containsPair('agents', 1));
    // The page's own file, anyone's.
    final page = await (await http.getUrl(Uri.parse('http://127.0.0.1:$port/'))).close();
    expect(page.statusCode, 200);
    http.close(force: true);
  }, timeout: const Timeout(Duration(minutes: 3)));
}
