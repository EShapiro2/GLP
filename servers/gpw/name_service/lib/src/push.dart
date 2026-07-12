/// The push side of the mirroring service (GPW Stage 4).
///
/// Registration rule (Udi, 2026-07-12): the name service's bindings drive
/// the networking layer's known-peer registry — a deposit binding a web-name
/// registers that key, retirement removes it, Replace swaps old for new.
/// Every received push is verified — signature, epoch discipline, manifest
/// hashes — before it touches the mirror, so the mirror only ever holds what
/// the person signed.
///
/// The transport itself (Noise sessions, UDX) is GLP-Networking-API's; this
/// file is everything above it: the registrar seam the bindings drive, the
/// gpw/area-push/1 payload, and the verified atomic write into the mirror.
library;

import 'dart:convert';
import 'dart:io';

import 'crypto.dart';
import 'area.dart';
import 'objects.dart';
import 'store.dart';

/// The known-peer registry seam.  The transport-backed implementation calls
/// the networking layer's putKnownPeer/removeKnownPeer; tests record calls.
abstract class PushRegistrar {
  void register(String publicKeyB64);
  void unregister(String publicKeyB64);
}

class NoopRegistrar implements PushRegistrar {
  @override
  void register(String publicKeyB64) {}
  @override
  void unregister(String publicKeyB64) {}
}

class RecordingRegistrar implements PushRegistrar {
  final Set<String> registered = {};
  @override
  void register(String publicKeyB64) => registered.add(publicKeyB64);
  @override
  void unregister(String publicKeyB64) => registered.remove(publicKeyB64);
}

/// Build a gpw/area-push/1 payload from a signed area directory (the output
/// of signArea): the signed area manifest, every listed page's bytes, and
/// every page's detached signature.  This is what the phone sends.
List<int> buildAreaPush(Directory dir, String webAddress) {
  final root = dir.path;
  final manifest = jsonDecode(
      File('$root$areaManifestPath').readAsStringSync()) as Map;
  final pages = <String, String>{};
  final pageSignatures = <String, Object?>{};
  for (final page in (manifest['body'] as Map)['pages'] as List) {
    final path = page['path'] as String;
    pages[path] = base64Encode(File('$root$path').readAsBytesSync());
    pageSignatures[path] =
        jsonDecode(File('$root${pageSigPath(path)}').readAsStringSync());
  }
  return utf8.encode(jsonEncode({
    'format': 'gpw/area-push/1',
    'webAddress': webAddress,
    'manifest': manifest,
    'pages': pages,
    'pageSignatures': pageSignatures,
  }));
}

/// The result of applying a push: accepted, or rejected with a reason.
class PushResult {
  PushResult.accepted(this.webAddress, this.areaEpoch)
      : accepted = true,
        reason = null;
  PushResult.rejected(this.reason)
      : accepted = false,
        webAddress = null,
        areaEpoch = null;

  final bool accepted;
  final String? webAddress;
  final int? areaEpoch;
  final String? reason;

  Map<String, Object?> toJson() => {
        'format': 'gpw/push-ack/1',
        'accepted': accepted,
        if (webAddress != null) 'webAddress': webAddress,
        if (areaEpoch != null) 'areaEpoch': areaEpoch,
        if (reason != null) 'reason': reason,
      };
}

/// Verifies a received push against the name service's bindings and writes
/// the area into the mirror atomically.  The mirror only ever holds what the
/// person signed.
class MirrorWriter {
  MirrorWriter({
    required this.zone,
    required this.store,
    required this.mirrorsDir,
  });

  final String zone;
  final NameStore store;
  final Directory mirrorsDir;

  /// The area epoch currently served for [webAddress], or null.
  int? servedAreaEpoch(String webAddress) {
    final f = File('${mirrorsDir.path}/$webAddress$areaManifestPath');
    if (!f.existsSync()) return null;
    try {
      final m = jsonDecode(f.readAsStringSync()) as Map;
      return (m['body'] as Map)['areaEpoch'] as int?;
    } catch (_) {
      return null;
    }
  }

  Future<PushResult> applyPush(String senderPkB64, List<int> payload) async {
    final Map push;
    try {
      push = jsonDecode(utf8.decode(payload)) as Map;
    } catch (_) {
      return PushResult.rejected('payload is not JSON');
    }
    if (push['format'] != 'gpw/area-push/1') {
      return PushResult.rejected('not a gpw/area-push/1');
    }
    final webAddress = push['webAddress'];
    if (webAddress is! String ||
        !webAddress.endsWith('.$zone') ||
        !validHostname(webAddress)) {
      return PushResult.rejected('webAddress is not a name in this zone');
    }
    final label =
        webAddress.substring(0, webAddress.length - zone.length - 1);
    if (!validLabel(label)) {
      return PushResult.rejected('webAddress is not a single label in this zone');
    }

    // The sender must be the key bound to the web-name.
    final state = store.load(label);
    if (state == null || state.retired) {
      return PushResult.rejected('web-name not bound');
    }
    final boundKey = state.manifestBody['publicKey'] as String;
    if (senderPkB64 != boundKey) {
      return PushResult.rejected('sender is not the bound key');
    }

    // The manifest must be signed by the bound key, for this web address.
    final manifest = push['manifest'];
    if (manifest is! Map) return PushResult.rejected('missing manifest');
    final body = manifest['body'];
    final signature = manifest['signature'];
    if (body is! Map || signature is! String) {
      return PushResult.rejected('malformed manifest envelope');
    }
    if (body['format'] != 'gpw/area-manifest/1' ||
        body['webAddress'] != webAddress ||
        body['publicKey'] != boundKey ||
        !await verifyJson(body, signature, boundKey)) {
      return PushResult.rejected('manifest not signed by the bound key');
    }

    // Epoch discipline: the latest area epoch wins.
    final areaEpoch = body['areaEpoch'];
    if (areaEpoch is! int || areaEpoch < 0) {
      return PushResult.rejected('missing or invalid areaEpoch');
    }
    final served = servedAreaEpoch(webAddress);
    if (served != null && areaEpoch <= served) {
      return PushResult.rejected('areaEpoch not above the served area');
    }

    // Every listed page: bytes present and hashing per the manifest, and a
    // page signature by the bound key over exactly those fields.
    final pages = push['pages'];
    final pageSigs = push['pageSignatures'];
    if (pages is! Map || pageSigs is! Map) {
      return PushResult.rejected('missing pages or pageSignatures');
    }
    final verified = <String, List<int>>{};
    final sidecars = <String, String>{};
    for (final page in body['pages'] as List) {
      final path = page['path'] as String;
      if (!path.startsWith('/') || path.contains('..')) {
        return PushResult.rejected('invalid page path $path');
      }
      final b64 = pages[path];
      if (b64 is! String) {
        return PushResult.rejected('page $path listed but not carried');
      }
      final List<int> bytes;
      try {
        bytes = base64Decode(b64);
      } catch (_) {
        return PushResult.rejected('page $path is not base64');
      }
      if (await sha256B64(bytes) != page['sha256']) {
        return PushResult.rejected('page $path does not hash per the manifest');
      }
      final sig = pageSigs[path];
      if (sig is! Map) return PushResult.rejected('page $path has no signature');
      final sigBody = sig['body'];
      final sigSig = sig['signature'];
      if (sigBody is! Map ||
          sigSig is! String ||
          sigBody['format'] != 'gpw/page/1' ||
          sigBody['webAddress'] != webAddress ||
          sigBody['path'] != path ||
          sigBody['epoch'] != page['epoch'] ||
          sigBody['sha256'] != page['sha256'] ||
          !await verifyJson(sigBody, sigSig, boundKey)) {
        return PushResult.rejected('page $path signature invalid');
      }
      verified[path] = bytes;
      sidecars[path] = jsonEncode(sig);
    }

    // Everything verified — write to a staging directory and swap atomically.
    final live = Directory('${mirrorsDir.path}/$webAddress');
    final staging = Directory('${mirrorsDir.path}/.staging-$webAddress');
    final old = Directory('${mirrorsDir.path}/.old-$webAddress');
    if (staging.existsSync()) staging.deleteSync(recursive: true);
    if (old.existsSync()) old.deleteSync(recursive: true);
    for (final e in verified.entries) {
      final f = File('${staging.path}${e.key}');
      f.createSync(recursive: true);
      f.writeAsBytesSync(e.value);
    }
    final mf = File('${staging.path}$areaManifestPath');
    mf.createSync(recursive: true);
    mf.writeAsStringSync(jsonEncode(manifest));
    for (final e in sidecars.entries) {
      final f = File('${staging.path}${pageSigPath(e.key)}');
      f.createSync(recursive: true);
      f.writeAsStringSync(e.value);
    }
    if (live.existsSync()) live.renameSync(old.path);
    staging.renameSync(live.path);
    if (old.existsSync()) old.deleteSync(recursive: true);

    return PushResult.accepted(webAddress, areaEpoch);
  }
}
