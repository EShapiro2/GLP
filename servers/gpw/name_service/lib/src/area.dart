/// Building and verifying a public area for the mirror: the signed area
/// manifest (gpw/area-manifest/1), per-page detached signatures (gpw/page/1),
/// and the serving layout — page bytes at their paths, the manifest at
/// /.gpw/area-manifest.json, page signatures at /.gpw/pages/<path>.sig.json.
/// Withholding is detectable against the manifest: every listed page must be
/// served and hash to its listed SHA-256.
library;

import 'dart:convert';
import 'dart:io';

import 'package:cryptography/cryptography.dart';

import 'crypto.dart';

const areaManifestPath = '/.gpw/area-manifest.json';

String pageSigPath(String pagePath) => '/.gpw/pages$pagePath.sig.json';

Future<String> sha256B64(List<int> bytes) async =>
    b64url((await Sha256().hash(bytes)).bytes);

/// Sign the area in [dir]: writes `.gpw/area-manifest.json` and
/// `.gpw/pages/<path>.sig.json` beside the pages.  Every regular file except
/// the `.gpw` tree is a page; [epoch] is used as the area epoch and as each
/// page's epoch (the reference signer keeps one epoch for a hand-made area).
Future<Map<String, Object?>> signArea(
    Directory dir, SigningKey key, String webAddress, int epoch) async {
  final root = dir.path;
  final pages = <Map<String, Object?>>[];
  final sigs = <String, Map<String, Object?>>{};
  final now = DateTime.now().toUtc().toIso8601String();

  final files = dir
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => !f.path.substring(root.length).startsWith('/.gpw/'))
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));
  for (final f in files) {
    final path = f.path.substring(root.length);
    final hash = await sha256B64(f.readAsBytesSync());
    pages.add({'path': path, 'epoch': epoch, 'sha256': hash});
    final pageBody = {
      'format': 'gpw/page/1',
      'webAddress': webAddress,
      'path': path,
      'epoch': epoch,
      'sha256': hash,
      'issuedAt': now,
    };
    sigs[path] = {
      'body': pageBody,
      'signature': await key.signJson(pageBody)
    };
  }

  final body = {
    'format': 'gpw/area-manifest/1',
    'webAddress': webAddress,
    'publicKey': key.publicKeyB64,
    'areaEpoch': epoch,
    'pages': pages,
    'issuedAt': now,
  };
  final manifest = {'body': body, 'signature': await key.signJson(body)};

  File('$root$areaManifestPath').createSync(recursive: true);
  File('$root$areaManifestPath').writeAsStringSync(jsonEncode(manifest));
  for (final e in sigs.entries) {
    final f = File('$root${pageSigPath(e.key)}');
    f.createSync(recursive: true);
    f.writeAsStringSync(jsonEncode(e.value));
  }
  return manifest;
}

/// Fetch page or manifest bytes by absolute path; null when absent.
typedef Fetch = Future<List<int>?> Function(String path);

/// Verify a served area against its manifest and [publicKey] (the key bound
/// to the web-name).  Returns human-readable problems; empty means the area
/// verifies — nothing withheld, nothing forged.
Future<List<String>> verifyArea(Fetch fetch, String publicKey) async {
  final problems = <String>[];
  final rawManifest = await fetch(areaManifestPath);
  if (rawManifest == null) {
    return ['withheld: $areaManifestPath (no area manifest served)'];
  }
  final Map manifest;
  final Map body;
  try {
    manifest = jsonDecode(utf8.decode(rawManifest)) as Map;
    body = manifest['body'] as Map;
  } catch (_) {
    return ['malformed area manifest'];
  }
  if (body['format'] != 'gpw/area-manifest/1' ||
      body['publicKey'] != publicKey ||
      !await verifyJson(body, manifest['signature'] as String, publicKey)) {
    return ['forged: area manifest not signed by the bound key'];
  }
  for (final page in body['pages'] as List) {
    final path = page['path'] as String;
    final bytes = await fetch(path);
    if (bytes == null) {
      problems.add('withheld: $path (listed in the manifest, not served)');
      continue;
    }
    if (await sha256B64(bytes) != page['sha256']) {
      problems.add('forged: $path (bytes do not match the manifest hash)');
    }
    final rawSig = await fetch(pageSigPath(path));
    if (rawSig == null) {
      problems.add('withheld: ${pageSigPath(path)} (page signature)');
      continue;
    }
    try {
      final sig = jsonDecode(utf8.decode(rawSig)) as Map;
      final sigBody = sig['body'] as Map;
      if (sigBody['format'] != 'gpw/page/1' ||
          sigBody['path'] != path ||
          sigBody['sha256'] != page['sha256'] ||
          sigBody['epoch'] != page['epoch'] ||
          !await verifyJson(sigBody, sig['signature'] as String, publicKey)) {
        problems.add('forged: ${pageSigPath(path)} (page signature invalid)');
      }
    } catch (_) {
      problems.add('malformed page signature for $path');
    }
  }
  return problems;
}
