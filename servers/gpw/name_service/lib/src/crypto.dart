/// Ed25519 signing and verification over JCS canonical bytes, and the
/// base64url (unpadded) key/signature encoding of the GPW wire formats.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';

import 'jcs.dart';

final _ed25519 = Ed25519();

String b64url(List<int> bytes) => base64UrlEncode(bytes).replaceAll('=', '');

Uint8List unb64url(String s) {
  final padded = s.padRight(s.length + (4 - s.length % 4) % 4, '=');
  return base64Url.decode(padded);
}

/// A server-side Ed25519 key pair (the counter-signing key).
class SigningKey {
  SigningKey(this.keyPair, this.publicKeyB64);

  final SimpleKeyPair keyPair;
  final String publicKeyB64;

  static Future<SigningKey> generate() async {
    final kp = await _ed25519.newKeyPair();
    final pub = await kp.extractPublicKey();
    return SigningKey(kp, b64url(pub.bytes));
  }

  static Future<SigningKey> fromSeed(List<int> seed) async {
    final kp = await _ed25519.newKeyPairFromSeed(seed);
    final pub = await kp.extractPublicKey();
    return SigningKey(kp, b64url(pub.bytes));
  }

  Future<List<int>> seed() => keyPair.extractPrivateKeyBytes();

  /// Signature (base64url) over the JCS canonical bytes of [value].
  Future<String> signJson(Object? value) async {
    final sig = await _ed25519.sign(jcsBytes(value), keyPair: keyPair);
    return b64url(sig.bytes);
  }
}

/// Verify [signatureB64] by [publicKeyB64] over the JCS bytes of [value].
Future<bool> verifyJson(
    Object? value, String signatureB64, String publicKeyB64) async {
  final Uint8List key, sig;
  try {
    key = unb64url(publicKeyB64);
    sig = unb64url(signatureB64);
  } on FormatException {
    return false;
  }
  if (key.length != 32 || sig.length != 64) return false;
  return _ed25519.verify(
    jcsBytes(value),
    signature: Signature(sig,
        publicKey: SimplePublicKey(key, type: KeyPairType.ed25519)),
  );
}
