import 'dart:convert';
import 'dart:io';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/headless/identity_file.dart';

/// The headless identity file is the key pair's seed and nothing else (GNA
/// §2.1: the key pair is the identity), and a file in any other form is
/// refused rather than read tolerantly (`networking/CLAUDE.md`, no legacy
/// code).
void main() {
  late Directory dir;
  setUp(() => dir = Directory.systemTemp.createTempSync('identity_file_'));
  tearDown(() => dir.deleteSync(recursive: true));

  test('created on first run as {"seed": <hex>}, and the same key pair '
      'loaded from it after', () async {
    final path = '${dir.path}/id.json';
    final created = await loadOrCreateIdentity(path);
    final written = jsonDecode(File(path).readAsStringSync()) as Map;
    expect(written.keys, ['seed']);
    expect(written['seed'], matches(RegExp(r'^[0-9a-f]{64}$')));

    final loaded = await loadOrCreateIdentity(path);
    expect(loaded.publicKey, created.publicKey);
    expect(loaded.privateKey, created.privateKey);
  });

  test('a file carrying a nickname beside the seed is refused', () async {
    final path = '${dir.path}/old.json';
    File(path).writeAsStringSync(
        jsonEncode({'seed': '11' * 32, 'nickname': 'headless'}));
    expect(() => loadOrCreateIdentity(path), throwsFormatException);
  });

  test('a file with no seed, or a seed not of 32 bytes, is refused', () async {
    final noSeed = '${dir.path}/no-seed.json';
    File(noSeed).writeAsStringSync(jsonEncode({}));
    expect(() => loadOrCreateIdentity(noSeed), throwsFormatException);

    final shortSeed = '${dir.path}/short-seed.json';
    File(shortSeed).writeAsStringSync(jsonEncode({'seed': '11' * 16}));
    expect(() => loadOrCreateIdentity(shortSeed), throwsFormatException);
  });
}
