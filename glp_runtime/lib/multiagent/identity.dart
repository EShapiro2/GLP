/// The person's identity as the runtime holds it: an Ed25519 key pair.
///
/// Secure GLP (core.tex, §Assumptions and §The Seam): `sign` succeeds only
/// under a key whose private half the runtime holds for its own person, and
/// the module certificate is that person's signature at compilation. The
/// runtime therefore holds one identity, from construction, and every kernel
/// that signs — `'_sign'`/3, and the compiler writing a certificate — signs
/// under it; `'_self_key'`/1 answers its public half. A harness that installs
/// the agent's key pair on its networking layer ([GlpNetwork.putIdentity])
/// installs the same pair here, so the layer, the certificate and `self_key`
/// agree on who the person is.
///
/// Where the compiler runs with no agent — `glpc`, the suite — the paper says
/// nothing about whose key certifies (reported to IGLP, 2026-09-08): the
/// runtime generates a fresh pair at construction and holds it for the person
/// of that process. Keys are real Ed25519 (`ed25519_edwards`, synchronous,
/// as `SimulationNetworkClient` uses).
library;

import 'dart:typed_data';

import 'package:ed25519_edwards/ed25519_edwards.dart' as ed;

import 'glp_network.dart' show PubKey;

class PersonIdentity {
  /// The public key: what `self_key/1` assigns and a certificate names.
  final PubKey pub;

  /// The 64-byte `ed25519_edwards` private key.
  final Uint8List priv;

  PersonIdentity(this.pub, this.priv);

  /// A fresh key pair.
  factory PersonIdentity.generate() {
    final kp = ed.generateKey();
    return PersonIdentity(PubKey.fromList(kp.publicKey.bytes),
        Uint8List.fromList(kp.privateKey.bytes));
  }

  /// The pair as [GlpNetwork.putIdentity] takes it.
  ({PubKey pub, Uint8List priv}) get keyPair => (pub: pub, priv: priv);

  /// Sign [message] under this identity's private key.
  Uint8List sign(Uint8List message) => ed.sign(ed.PrivateKey(priv), message);

  /// Whether [signature] over [message] is valid under [signer].
  static bool verify(PubKey signer, Uint8List message, Uint8List signature) {
    try {
      return ed.verify(ed.PublicKey(signer.bytes), message, signature);
    } catch (_) {
      return false;
    }
  }
}
