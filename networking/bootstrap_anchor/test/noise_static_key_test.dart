import 'dart:typed_data';

import 'package:bootstrap_anchor/src/identity.dart';
import 'package:bootstrap_anchor/src/libsodium_loader.dart';
import 'package:bootstrap_anchor/src/noise_session_manager.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

/// Regression test for the anchor↔client Noise static-key mismatch.
///
/// The client derives the anchor's expected Noise static as
/// `pkToCurve25519(anchorEd25519Pubkey)` and aborts the handshake if the
/// handshake-delivered static differs (libsodium SUMO static-key
/// verification). The anchor must therefore derive its static via the same
/// birational map — not via an unrelated SHA-256 seed — or every client↔anchor
/// session aborts and cloud rendezvous hole-punching never bootstraps.
void main() {
  late SodiumSumo sodium;

  setUpAll(() async {
    sodium = await SodiumSumoInit.init(loadLibsodium);
  });

  test('anchor Noise static public key equals pkToCurve25519(identity pubkey)',
      () async {
    final seed = Uint8List.fromList(List<int>.generate(32, (i) => i + 1));
    final identity =
        await AnchorIdentity.fromSeed(seed: seed, nickname: 'anchor');

    final manager = NoiseSessionManager(identity: identity, sodium: sodium);
    final derived = await manager.staticPublicKey();

    // This is exactly the value the client recomputes and compares against.
    final expected = sodium.crypto.sign.pkToCurve25519(identity.publicKey);

    expect(derived, equals(expected));
    expect(derived.length, 32);
    expect(derived.any((b) => b != 0), isTrue,
        reason: 'static public key must not be all-zero');
  });

  test('derivation is deterministic for the same identity', () async {
    final seed = Uint8List.fromList(List<int>.generate(32, (i) => 0xA0 + i));
    final identity =
        await AnchorIdentity.fromSeed(seed: seed, nickname: 'anchor');

    final a = await NoiseSessionManager(identity: identity, sodium: sodium)
        .staticPublicKey();
    final b = await NoiseSessionManager(identity: identity, sodium: sodium)
        .staticPublicKey();

    expect(a, equals(b));
  });
}
