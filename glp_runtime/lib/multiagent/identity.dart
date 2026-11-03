/// The person's identity as the runtime holds it: an Ed25519 key pair.
///
/// Secure GLP (core.tex, §Assumptions and §The Seam): `sign` succeeds only
/// under a key whose private half the runtime holds for its own person, and
/// the module certificate is that person's signature at compilation. The
/// runtime therefore holds one identity, from construction, and every kernel
/// that signs — `'_sign'`/3, and the compiler writing a certificate — signs
/// under it; `'_self_key'`/1 answers its public half. A harness that
/// instantiates the agent's networking layer with its key pair (GNA,
/// "Identity") installs the same pair here, so the layer, the certificate and
/// `self_key` agree on who the person is.
///
/// Where the compiler runs with no agent — `glpc`, the suite — the paper says
/// nothing about whose key certifies (reported to IGLP, 2026-09-08): the
/// runtime generates a fresh pair at construction and holds it for the person
/// of that process. Keys are real Ed25519 (`ed25519_edwards`, synchronous,
/// as `SimulationNetworkClient` uses).
library;

import 'dart:convert' show utf8;
import 'dart:io' show Platform, Process;
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;
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

  /// The key pair of the Ed25519 seed [seed], 32 bytes.
  factory PersonIdentity.fromSeed(Uint8List seed) {
    final priv = ed.newKeyFromSeed(seed);
    return PersonIdentity(PubKey.fromList(ed.public(priv).bytes),
        Uint8List.fromList(priv.bytes));
  }

  /// The agent's key pair, made when the runtime spawns the agent, before
  /// anything is compiled or sent (GLP-Spec appendix-guards.tex, "Identity
  /// and signature"), from the seed IGLP names: "the SHA-256 of the clock,
  /// the device's hardware identifiers and the persona's name, which is the
  /// agent's name p in the boot clause, combined by exclusive or" (IGLP,
  /// Implementation Notes, "Boot").  Each of the three is hashed to 32 bytes
  /// with SHA-256, and the seed is the exclusive or of the three hashes.  The
  /// clock is the microseconds since the epoch; the hardware identifiers are
  /// those [hardwareIdentifiers] reads.
  factory PersonIdentity.forPersona(String persona) {
    final parts = [
      '${DateTime.now().microsecondsSinceEpoch}',
      hardwareIdentifiers(),
      persona,
    ];
    final seed = Uint8List(32);
    for (final part in parts) {
      final h = crypto.sha256.convert(utf8.encode(part)).bytes;
      for (var i = 0; i < 32; i++) {
        seed[i] ^= h[i];
      }
    }
    return PersonIdentity.fromSeed(seed);
  }

  /// The device's hardware identifiers, read once per process: on macOS the
  /// platform UUID the firmware reports (`IOPlatformUUID`), and everywhere
  /// the host's name, the operating system and its version, and the number
  /// of processors, which is what a process can read where no hardware UUID
  /// is open to it.
  static String hardwareIdentifiers() => _hardware ??= _readHardware();
  static String? _hardware;

  static String _readHardware() {
    final ids = <String>[];
    if (Platform.isMacOS) {
      try {
        final r = Process.runSync(
            'ioreg', ['-rd1', '-c', 'IOPlatformExpertDevice']);
        final m = RegExp(r'"IOPlatformUUID" = "([^"]+)"')
            .firstMatch('${r.stdout}');
        if (m != null) ids.add(m.group(1)!);
      } on Object {
        // No platform UUID is open to the process: the rest stand.
      }
    }
    ids.addAll([
      Platform.localHostname,
      Platform.operatingSystem,
      Platform.operatingSystemVersion,
      '${Platform.numberOfProcessors}',
    ]);
    return ids.join('|');
  }

  /// The pair as a networking layer is instantiated with it
  /// (`SimulationNetworkClient.keyPair`).
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
