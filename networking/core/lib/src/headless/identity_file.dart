import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';

import '../models/identity.dart';

/// Load a [GrassrootsIdentity] from a JSON identity file, `{"seed": <hex>}`
/// --- the 32-byte Ed25519 seed of the key pair, which is the identity and
/// all of it (GNA §2.1) --- creating the file with a fresh random seed on
/// first run.  A file carrying any other entry, or no seed, is not an
/// identity file and is refused (`networking/CLAUDE.md`, no legacy code).
Future<GrassrootsIdentity> loadOrCreateIdentity(String path) async {
  final file = File(path);
  final Uint8List seed;
  if (file.existsSync()) {
    seed = _seedOf(jsonDecode(file.readAsStringSync()), path);
  } else {
    final random = Random.secure();
    seed = Uint8List.fromList(
      List<int>.generate(32, (_) => random.nextInt(256)),
    );
    file.parent.createSync(recursive: true);
    file.writeAsStringSync(jsonEncode({'seed': _toHex(seed)}), flush: true);
  }
  final keyPair = await Ed25519().newKeyPairFromSeed(seed);
  return GrassrootsIdentity.create(keyPair: keyPair);
}

/// The seed an identity file holds: `{"seed": <64 hex digits>}` and nothing
/// else.
Uint8List _seedOf(Object? decoded, String path) {
  if (decoded is! Map ||
      decoded.length != 1 ||
      decoded['seed'] is! String ||
      !RegExp(r'^[0-9a-fA-F]{64}$').hasMatch(decoded['seed'] as String)) {
    throw FormatException(
        'identity file $path is not {"seed": <64 hex digits>}');
  }
  return _fromHex(decoded['seed'] as String);
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
