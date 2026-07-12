/// Entry point.  Usage:
///   gpw_name_service --state-dir /var/lib/gpw [--zone peoplesweb.org]
///                    [--host 127.0.0.1] [--port 8053] [--knotc knotc]
///                    [--push-port 9517] [--public-address ip:port]
///                    [--mirrors-dir /var/lib/gpw/mirrors]
///
/// The server's Ed25519 counter-signing key lives at
/// `<state-dir>/server_key.json` (created on first run, mode 0600).
///
/// With --push-port and --public-address, the push channel starts: the
/// mirroring agent's identity at `<state-dir>/push-identity.json`, Closed
/// trust, known peers driven by the bindings (registration rule), verified
/// pushes written into --mirrors-dir.
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
  final store = NameStore(stateDir);

  // The push channel, when configured (Stage 4).
  PushService? push;
  PushRegistrar registrar = NoopRegistrar();
  final pushPort = opts['push-port'];
  final publicAddress = opts['public-address'];
  if (pushPort != null && publicAddress != null) {
    push = await PushService.start(
      identityPath: '$stateDir/push-identity.json',
      port: int.parse(pushPort),
      publicAddress: publicAddress,
      writer: MirrorWriter(
        zone: zone,
        store: store,
        mirrorsDir:
            Directory(opts['mirrors-dir'] ?? '$stateDir/mirrors'),
      ),
      knownPeersPath: '$stateDir/known-peers.json',
    );
    registrar = LayerRegistrar(push.network);
  }

  final service = NameService(
    zone: zone,
    store: store,
    zoneWriter: KnotcZoneWriter(zone, knotc: opts['knotc'] ?? 'knotc'),
    serverKey: key,
    registrar: registrar,
  );
  if (push != null) {
    service.syncRegistrar();
    service.pushInfo = {
      'publicKey': push.publicKeyB64,
      'address': publicAddress,
    };
  }
  await serve(service, host, port);
  stdout.writeln('gpw_name_service: zone $zone, state $stateDir, '
      'listening on ${host.address}:$port, '
      'server key ${key.publicKeyB64}'
      '${push == null ? '' : ', push agent ${push.publicKeyB64} '
          'at $publicAddress'}');
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
