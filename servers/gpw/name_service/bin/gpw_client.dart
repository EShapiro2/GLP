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
library;

import 'dart:convert';
import 'dart:io';

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
