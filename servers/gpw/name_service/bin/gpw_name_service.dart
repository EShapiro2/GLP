/// Entry point.  Usage:
///   gpw_name_service --state-dir /var/lib/gpw [--zone peoplesweb.org]
///                    [--host 127.0.0.1] [--port 8053] [--knotc knotc]
///
/// The server's Ed25519 counter-signing key lives at
/// `<state-dir>/server_key.json` (created on first run, mode 0600).
library;

import 'dart:convert';
import 'dart:io';

import 'package:gpw_name_service/gpw_name_service.dart';

Future<void> main(List<String> args) async {
  final opts = _parse(args);
  final stateDir = opts['state-dir'];
  if (stateDir == null) {
    stderr.writeln('gpw_name_service: --state-dir is required');
    exitCode = 2;
    return;
  }
  final zone = opts['zone'] ?? 'peoplesweb.org';
  final host = InternetAddress(opts['host'] ?? '127.0.0.1');
  final port = int.parse(opts['port'] ?? '8053');

  Directory(stateDir).createSync(recursive: true);
  final key = await _loadOrCreateKey('$stateDir/server_key.json');
  final service = NameService(
    zone: zone,
    store: NameStore(stateDir),
    zoneWriter: KnotcZoneWriter(zone, knotc: opts['knotc'] ?? 'knotc'),
    serverKey: key,
  );
  await serve(service, host, port);
  stdout.writeln('gpw_name_service: zone $zone, state $stateDir, '
      'listening on ${host.address}:$port, '
      'server key ${key.publicKeyB64}');
}

Future<SigningKey> _loadOrCreateKey(String path) async {
  final f = File(path);
  if (f.existsSync()) {
    final json = jsonDecode(f.readAsStringSync()) as Map;
    return SigningKey.fromSeed(unb64url(json['seed'] as String));
  }
  final key = await SigningKey.generate();
  f.writeAsStringSync(jsonEncode({'seed': b64url(await key.seed())}));
  await Process.run('chmod', ['600', path]);
  return key;
}

Map<String, String> _parse(List<String> args) {
  final opts = <String, String>{};
  for (var i = 0; i < args.length; i++) {
    final a = args[i];
    if (!a.startsWith('--') || i + 1 >= args.length) {
      stderr.writeln('gpw_name_service: bad argument "$a"');
      exit(2);
    }
    opts[a.substring(2)] = args[++i];
  }
  return opts;
}
