/// Reference client and signer for the GPW name-server service.  This is the
/// signing side of the wire formats (what the phone will do); used to
/// exercise the service end to end.
///
/// Usage:
///   gpw_client keygen <keyfile>
///   gpw_client deposit --key <keyfile> --url <base> --name <label> --epoch <n>
///              [--zone peoplesweb.org] [--custodian <pubkey>]... [--threshold <k>]
///              [--replace-old <pubkey> --custodian-key <keyfile>...]
///   gpw_client repoint --key <keyfile> --url <base> --name <label>
///              --mirror <host-or-ip> --epoch <n> [--zone ...]
///   gpw_client retire --key <keyfile> --url <base> --name <label> --epoch <n>
///              [--redirect <host>] [--zone ...]
///   gpw_client get --url <base> --name <label>
///   gpw_client area-sign --key <keyfile> --dir <area-dir> --address <web-address>
///              --epoch <n>
///   gpw_client area-verify --mirror-url <base> (--pubkey <b64url> | --ns-url <base> --name <label>)
///   gpw_client push --key <keyfile> --dir <signed-area-dir> --address <web-address>
///              (--service-pk <b64url> --service-addr <ip:port> | --ns-url <base>)
///              [--timeout <seconds>]
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    show GrassrootsIdentity, HeadlessGrassrootsNetwork, initHeadlessSodium;
import 'package:gpw_name_service/gpw_name_service.dart';

Future<void> main(List<String> args) async {
  if (args.isEmpty) {
    stderr.writeln('gpw_client: no command');
    exit(2);
  }
  final cmd = args.first;
  final rest = args.sublist(1);
  switch (cmd) {
    case 'keygen':
      await _keygen(rest);
    case 'deposit':
      await _deposit(_Opts(rest));
    case 'repoint':
      await _repoint(_Opts(rest));
    case 'retire':
      await _retire(_Opts(rest));
    case 'get':
      await _get(_Opts(rest));
    case 'area-sign':
      await _areaSign(_Opts(rest));
    case 'area-verify':
      await _areaVerify(_Opts(rest));
    case 'push':
      await _push(_Opts(rest));
    default:
      stderr.writeln('gpw_client: unknown command "$cmd"');
      exit(2);
  }
}

Future<void> _keygen(List<String> args) async {
  if (args.length != 1) {
    stderr.writeln('gpw_client keygen <keyfile>');
    exit(2);
  }
  final key = await SigningKey.generate();
  File(args[0]).writeAsStringSync(jsonEncode({
    'seed': b64url(await key.seed()),
    'publicKey': key.publicKeyB64,
  }));
  await Process.run('chmod', ['600', args[0]]);
  stdout.writeln(key.publicKeyB64);
}

Future<SigningKey> _load(String path) async {
  final json = jsonDecode(File(path).readAsStringSync()) as Map;
  return SigningKey.fromSeed(unb64url(json['seed'] as String));
}

String _now() => DateTime.now().toUtc().toIso8601String();

Future<void> _deposit(_Opts o) async {
  final key = await _load(o.req('key'));
  final custodians = o.all('custodian');
  final body = <String, Object?>{
    'format': 'gpw/name-manifest/1',
    'zone': o['zone'] ?? 'peoplesweb.org',
    'webName': o.req('name'),
    'publicKey': key.publicKeyB64,
    'identityRecord': {
      'custodians': custodians.isEmpty ? [key.publicKeyB64] : custodians,
      'threshold': int.parse(o['threshold'] ?? '1'),
    },
    'epoch': int.parse(o.req('epoch')),
    'issuedAt': _now(),
  };
  final oldKey = o['replace-old'];
  if (oldKey != null) {
    final sigs = <Map<String, String>>[];
    for (final path in o.all('custodian-key')) {
      final ck = await _load(path);
      sigs.add({'key': ck.publicKeyB64, 'signature': await ck.signJson(body)});
    }
    body['replaces'] = {'oldKey': oldKey, 'custodianSignatures': sigs};
  }
  await _send(o, 'PUT', 'names/${o.req('name')}',
      {'body': body, 'signature': await key.signJson(body)});
}

Future<void> _repoint(_Opts o) async {
  final key = await _load(o.req('key'));
  final body = {
    'format': 'gpw/repoint/1',
    'zone': o['zone'] ?? 'peoplesweb.org',
    'webName': o.req('name'),
    'mirror': o.req('mirror'),
    'epoch': int.parse(o.req('epoch')),
    'issuedAt': _now(),
  };
  await _send(o, 'POST', 'names/${o.req('name')}/repoint',
      {'body': body, 'signature': await key.signJson(body)});
}

Future<void> _retire(_Opts o) async {
  final key = await _load(o.req('key'));
  final body = {
    'format': 'gpw/retirement/1',
    'zone': o['zone'] ?? 'peoplesweb.org',
    'webName': o.req('name'),
    if (o['redirect'] != null) 'redirect': o['redirect'],
    'epoch': int.parse(o.req('epoch')),
    'issuedAt': _now(),
  };
  await _send(o, 'POST', 'names/${o.req('name')}/retire',
      {'body': body, 'signature': await key.signJson(body)});
}

Future<void> _get(_Opts o) async {
  await _send(o, 'GET', 'names/${o.req('name')}', null);
}

Future<void> _areaSign(_Opts o) async {
  final key = await _load(o.req('key'));
  final manifest = await signArea(Directory(o.req('dir')), key,
      o.req('address'), int.parse(o.req('epoch')));
  stdout.writeln('signed: ${(manifest['body'] as Map)['pages']}');
}

Future<void> _areaVerify(_Opts o) async {
  final mirror = o.req('mirror-url');
  String? pubkey = o['pubkey'];
  final client = HttpClient();
  try {
    Future<List<int>?> fetch(String path) async {
      final req = await client.getUrl(Uri.parse('$mirror$path'));
      final res = await req.close();
      if (res.statusCode != 200) {
        await res.drain<void>();
        return null;
      }
      return [for (final chunk in await res.toList()) ...chunk];
    }

    if (pubkey == null) {
      // The bound key comes from the name server's counter-signed manifest.
      final ns = o.req('ns-url');
      final req = await client
          .getUrl(Uri.parse('$ns/gpw/v1/names/${o.req('name')}'));
      final res = await req.close();
      final text = await res.transform(utf8.decoder).join();
      if (res.statusCode != 200) {
        stderr.writeln('name server: ${res.statusCode} $text');
        exit(1);
      }
      final manifest = jsonDecode(text) as Map;
      final body = manifest['body'] as Map;
      pubkey = body['publicKey'] as String;
      if (!await verifyJson(
          body, manifest['signature'] as String, pubkey)) {
        stderr.writeln('name manifest signature does not verify');
        exit(1);
      }
    }

    final problems = await verifyArea(fetch, pubkey);
    if (problems.isEmpty) {
      stdout.writeln('area verifies: nothing withheld, nothing forged');
    } else {
      problems.forEach(stderr.writeln);
      exitCode = 1;
    }
  } finally {
    client.close();
  }
}

/// The phone side of the push channel, per the GLP-Networking-API answer:
/// `putPeerAddress(servicePk, address)` then `send(servicePk, payload)`,
/// the payload being the gpw/area-push/1 snapshot of a signed area.  Waits
/// for the service's gpw/push-ack/1.
Future<void> _push(_Opts o) async {
  final keyJson =
      jsonDecode(File(o.req('key')).readAsStringSync()) as Map;
  final seed = unb64url(keyJson['seed'] as String);
  final payload = buildAreaPush(Directory(o.req('dir')), o.req('address'));

  // Service coordinates: given directly, or from the name service.
  String? servicePk = o['service-pk'];
  String? serviceAddr = o['service-addr'];
  if (servicePk == null || serviceAddr == null) {
    final ns = o.req('ns-url');
    final client = HttpClient();
    try {
      final req = await client.getUrl(Uri.parse('$ns/gpw/v1/push-info'));
      final res = await req.close();
      final text = await res.transform(utf8.decoder).join();
      if (res.statusCode != 200) {
        stderr.writeln('push-info: ${res.statusCode} $text');
        exit(1);
      }
      final info = jsonDecode(text) as Map;
      servicePk = info['publicKey'] as String;
      serviceAddr = info['address'] as String;
    } finally {
      client.close();
    }
  }

  final sodium = await initHeadlessSodium();
  final identity = await GrassrootsIdentity.create(
    keyPair: await Ed25519().newKeyPairFromSeed(seed),
    nickname: 'gpw-push',
  );
  final network = HeadlessGrassrootsNetwork(
    identity: identity,
    sodium: sodium,
  );
  final ack = Completer<Map>();
  network.onMessageReceived = (messageId, senderPk, bytes, transport) {
    try {
      final m = jsonDecode(utf8.decode(bytes)) as Map;
      if (m['format'] == 'gpw/push-ack/1' && !ack.isCompleted) {
        ack.complete(m);
      }
    } catch (_) {}
  };
  if (!await network.start()) {
    stderr.writeln('push: failed to bind a UDP socket');
    exit(1);
  }
  network.putPeerAddress(unb64url(servicePk), serviceAddr);
  await network.send(
      unb64url(servicePk), Uint8List.fromList(payload));

  final timeout = Duration(seconds: int.parse(o['timeout'] ?? '20'));
  try {
    final result = await ack.future.timeout(timeout);
    stdout.writeln(jsonEncode(result));
    if (result['accepted'] != true) exitCode = 1;
  } on TimeoutException {
    stderr.writeln('push: no ack within ${timeout.inSeconds}s '
        '(service down, wrong address, or key not registered)');
    exitCode = 1;
  } finally {
    await network.dispose();
  }
  exit(exitCode);
}

Future<void> _send(
    _Opts o, String method, String path, Object? payload) async {
  final base = o.req('url');
  final uri = Uri.parse('$base/gpw/v1/$path');
  final client = HttpClient();
  try {
    final req = await client.openUrl(method, uri);
    if (payload != null) {
      req.headers.contentType = ContentType.json;
      req.write(jsonEncode(payload));
    }
    final res = await req.close();
    final text = await res.transform(utf8.decoder).join();
    stdout.writeln('${res.statusCode} $text');
    if (res.statusCode >= 400) exitCode = 1;
  } finally {
    client.close();
  }
}

class _Opts {
  _Opts(List<String> args) {
    for (var i = 0; i < args.length; i++) {
      final a = args[i];
      if (!a.startsWith('--') || i + 1 >= args.length) {
        stderr.writeln('gpw_client: bad argument "$a"');
        exit(2);
      }
      _multi.putIfAbsent(a.substring(2), () => []).add(args[++i]);
    }
  }

  final Map<String, List<String>> _multi = {};

  String? operator [](String k) => _multi[k]?.last;
  List<String> all(String k) => _multi[k] ?? const [];
  String req(String k) {
    final v = this[k];
    if (v == null) {
      stderr.writeln('gpw_client: --$k is required');
      exit(2);
    }
    return v;
  }
}
