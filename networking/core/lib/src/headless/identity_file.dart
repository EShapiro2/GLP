import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';

import '../models/identity.dart';

/// Load a [GrassrootsIdentity] from a JSON identity file
/// (`{"seed": <hex>, "nickname": <string>}`), creating the file with a fresh
/// random seed on first run — the same format `bootstrap_anchor` uses.
Future<GrassrootsIdentity> loadOrCreateIdentity(
  String path, {
  String nickname = 'headless',
}) async {
  final file = File(path);
  Uint8List seed;
  String name = nickname;
  if (file.existsSync()) {
    final decoded = jsonDecode(file.readAsStringSync()) as Map;
    seed = _fromHex(decoded['seed'] as String);
    name = (decoded['nickname'] as String?) ?? nickname;
  } else {
    final random = Random.secure();
    seed = Uint8List.fromList(
      List<int>.generate(32, (_) => random.nextInt(256)),
    );
    file.parent.createSync(recursive: true);
    file.writeAsStringSync(
      jsonEncode({'seed': _toHex(seed), 'nickname': name}),
      flush: true,
    );
  }
  final keyPair = await Ed25519().newKeyPairFromSeed(seed);
  return GrassrootsIdentity.create(keyPair: keyPair, nickname: name);
}

String _toHex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List _fromHex(String hex) {
  final out = Uint8List(hex.length ~/ 2);
  for (var i = 0; i < out.length; i++) {
    out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
  }
  return out;
}
