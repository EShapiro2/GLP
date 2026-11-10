/// The agent's identity: the key pair a host makes and keeps, and what the runtime holds of it.
///
/// The private key is the networking layer's alone (GNA api.tex, "Identity": "The key pair is generated and persisted above the networking layer and is supplied to the layer's constructor when it is instantiated"; decision 39, the key pair private to the layer, which gives out the public key and signs; the GLP paper f265fec, appendix-guards, "Identity and signature": the runtime "gives it to the networking layer, which holds the private key and signs with it"; IGLP 07a2b00, Implementation Notes, "The contract": "the runtime supplies the key pair to the layer when it instantiates it, the layer keeping it unchanged for its lifetime").  So the runtime holds no private key: it holds an [AgentSigner], the layer's public key and a signature by the layer's `sign`, and every kernel that signs --- `'_sign'`/3, and the compiler writing a certificate --- signs by it; `'_self_key'`/1 answers its public key.  [PersonIdentity] is the key pair as a host makes it (the seed IGLP names, "Boot") and keeps it (the store, "Persistence"), to hand to the layer at its instantiation, and the runtime keeps no copy of it beyond that; a host that holds one gives the runtime a [LayerSigner] over the layer.
///
/// An engine made with no signer --- glpc's compiler, the suite, where the paper names no key (reported to IGLP, 2026-09-08) --- makes a fresh key pair and hands it to a layer of its own, the simulation's client with no router ([SimulationNetworkClient.alone]), and signs by that layer; a bare runtime, made with no engine, holds no key, and `self_key/1` and `sign/3` abort in it.  The REPL and the harnesses bind the simulation's layer, which signs with the key pair it was made with, as the networking layer does.  Keys are real Ed25519 (`ed25519_edwards`, synchronous, as `SimulationNetworkClient` uses).
library;

import 'dart:convert' show utf8;
import 'dart:io' show Platform, Process;
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;
import 'package:ed25519_edwards/ed25519_edwards.dart' as ed;

import 'glp_network.dart' show GlpNetwork, PubKey;

/// What the runtime holds of the agent's identity: the public key, and a signature made by the holder of the private key, the networking layer (the GLP paper f265fec, "Identity and signature"; IGLP 07a2b00, "The contract").  No realisation holds the private key but a layer's: [LayerSigner].
abstract class AgentSigner {
  /// The public key: what `self_key/1` assigns and a certificate names.
  PubKey get pub;

  /// Sign [message] under the private key, which the layer holds.
  Uint8List sign(Uint8List message);
}

/// The agent's signer over its networking layer, [network], whose `getIdentity()` is the public key and whose `sign(bytes)` signs under the private key it holds (GNA, decision 39; GlpNetwork, "The layer holds the private key").  The layer of an agent over the network, and the simulation's client in a simulation and in the REPL.
class LayerSigner implements AgentSigner {
  final GlpNetwork network;

  const LayerSigner(this.network);

  @override
  PubKey get pub => network.getIdentity();

  @override
  Uint8List sign(Uint8List message) => network.sign(message);
}

/// An Ed25519 key pair, as a host makes it for an agent and hands it to the agent's networking layer; the runtime holds no [PersonIdentity] (the library comment above).
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
