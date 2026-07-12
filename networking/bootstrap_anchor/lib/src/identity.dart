import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';
import 'package:cryptography/cryptography.dart';

/// Server-side identity for the rendezvous agent.
///
/// The rendezvous agent is an independent GLP agent with its own Ed25519
/// keypair. It generates this keypair once via [generate] and persists it
/// to disk. On subsequent startups it loads the existing identity via
/// [loadOrCreate].
///
/// This is the spec-aligned model: the rendezvous agent's identity is not
/// derived from any other agent's key. Any agent can run a rendezvous
/// server, and clients discover it through out-of-band configuration
/// (entering the server's public key + address in settings).
class AnchorIdentity {
  final Uint8List publicKey;
  final Uint8List privateKey;
  final SimpleKeyPair keyPair;
  String nickname;

  AnchorIdentity._({
    required this.publicKey,
    required this.privateKey,
    required this.keyPair,
    required this.nickname,
  });

  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// Generate a fresh Ed25519 keypair.
  static Future<AnchorIdentity> generate({required String nickname}) async {
    final algorithm = Ed25519();
    final keyPair = await algorithm.newKeyPair();
    return _fromKeyPair(keyPair, nickname);
  }

  /// Load identity from a JSON file, or generate and persist a new one.
  ///
  /// JSON format: `{ "seed": "<64-hex>", "nickname": "..." }`
  static Future<AnchorIdentity> loadOrCreate({
    required String path,
    required String nickname,
  }) async {
    final file = File(path);
    if (await file.exists()) {
      final json = jsonDecode(await file.readAsString()) as Map<String, dynamic>;
      final seedHex = json['seed'] as String;
      final storedNickname = json['nickname'] as String? ?? nickname;
      return fromSeedHex(seedHex: seedHex, nickname: storedNickname);
    }

    // Generate fresh identity and persist
    final identity = await generate(nickname: nickname);
    final seed = await identity.keyPair.extractPrivateKeyBytes();
    final seedHex = seed
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    final json = {
      'seed': seedHex,
      'nickname': nickname,
    };
    await file.writeAsString(
      const JsonEncoder.withIndent('  ').convert(json),
    );
    return identity;
  }

  /// Reconstruct identity from a 32-byte Ed25519 seed.
  static Future<AnchorIdentity> fromSeed({
    required Uint8List seed,
    required String nickname,
  }) async {
    if (seed.length != 32) {
      throw ArgumentError('Seed must be 32 bytes, got ${seed.length}');
    }
    final algorithm = Ed25519();
    final keyPair = await algorithm.newKeyPairFromSeed(seed);
    return _fromKeyPair(keyPair, nickname);
  }

  /// Reconstruct identity from a hex-encoded seed string.
  static Future<AnchorIdentity> fromSeedHex({
    required String seedHex,
    required String nickname,
  }) async {
    final seed = _hexDecode(seedHex);
    return fromSeed(seed: seed, nickname: nickname);
  }

  static Future<AnchorIdentity> _fromKeyPair(
    SimpleKeyPair keyPair,
    String nickname,
  ) async {
    final pk = await keyPair.extractPublicKey();
    final publicKey = Uint8List.fromList(pk.bytes);
    final seed = await keyPair.extractPrivateKeyBytes();
    final privateKey = Uint8List.fromList([...seed, ...pk.bytes]);

    return AnchorIdentity._(
      publicKey: publicKey,
      privateKey: privateKey,
      keyPair: keyPair,
      nickname: nickname,
    );
  }

  static Uint8List _hexDecode(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }
}
