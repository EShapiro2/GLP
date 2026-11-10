/// The networking layer of the Grassroots App on a phone: GNA's layer,
/// instantiated once for the agent, running the PAN over Bluetooth, the LAN
/// over the local network and the WAN over the Internet, and the runtime's
/// network interface over it, GNA's glue ([GrassrootsGlpNetwork]).  The WAN is
/// run for the rendezvous server and reconnection over the Internet (GSA #2
/// Cowork, Code_inbox.md 2026-11-08 07:11 UTC: "the phone's layer runs the WAN
/// too").  The WAN has no trust level: the layer connects over it only to
/// known peers and declared rendezvous servers, and answers inbound WAN
/// contact only from a key in the known-peer set (GNA, Section "Networking
/// API": "a trust level on the WAN cannot be written"; Section "Session
/// Establishment": "on the WAN except on a rendezvous server, it answers an
/// IK first message only from a key in the known-peer set"), which the
/// operating system fills with every person its person knows (GSA paper,
/// Appendix "The Operating System", "Nearby"; place.glp, declare(peer(K))).
/// A test build accepts development attestations (GSA #2 Cowork, the same
/// mail: "a test build accepts development attestations").
///
/// The key pair (GNA, Section "Identity": "The key pair is generated and
/// persisted above the networking layer and is supplied to the layer's
/// constructor when it is instantiated"; IGLP, Implementation Notes, "Boot":
/// the runtime "makes the agent's key pair from a seed drawn from outside the
/// program --- the SHA-256 of the clock, the device's hardware identifiers and
/// the persona's name ... --- and keeps it across restarts ... so the key pair
/// is made once, at the agent's first start"; "a deployed agent is spawned by
/// its application with ... its key made the same way"): read from the
/// platform's secure store, where the layer's embedding keeps it
/// ([IdentityStore]), or, at the first start, made from that seed
/// ([PersonIdentity.forPersona]) and kept there.
///
/// The layer (GNA, Section "Instantiation"): the key pair, the underlays it
/// runs, PAN, LAN and WAN, and the platform's attestation, the native producer
/// where the app registers one, and otherwise a platform with no attestation,
/// whose peers report it unattested (GNA, Section "Session Establishment",
/// Attestation).  The trust of both proximity underlays is Closed until the
/// operating system declares it open, on GPE:1 (GNA, "Trust levels": "both
/// levels default to Closed"; GSA paper, Appendix "The Operating System",
/// "Nearby").
library;

import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:redux/redux.dart';
import 'app_identity.dart' show RunningAppAttestation;
import 'package:sodium_libs/sodium_libs_sumo.dart';

/// The layer on the phone, the runtime's interface over it, and the agent's
/// key, the layer's: the layer holds the key pair, private half included, and
/// signs with it; the host and the runtime hold the public key (GNA,
/// decision 39; the GLP paper f265fec, appendix-guards, "Identity and
/// signature"; IGLP 07a2b00, Implementation Notes, "The contract").  Until
/// 2026-11-10 the host held the pair and handed it to the runtime.
class PhoneNetwork {
  final GrassrootsNetwork layer;
  final glp.GlpNetwork network;
  final glp.PubKey key;

  /// Whether the layer brought up a transport, Bluetooth or the local
  /// network ([GrassrootsNetwork.initialize]).
  final bool transports;

  /// The layer's attestation: the platform's, signing the version of the app
  /// whose program the agent runs, which the host gives it once the runtime
  /// has decided it ([RunningAppAttestation.give]); until then no session is
  /// attested.
  final RunningAppAttestation attestation;

  const PhoneNetwork._(this.layer, this.network, this.key, this.transports,
      this.attestation);

  /// Instantiate the layer for the agent of [persona], with the agent's key
  /// pair, and start it.  The store the layer reduces its actions in runs
  /// [middleware] on each action before it is reduced: in a debuggable build
  /// the reporter's (lib/debuglog/app_reporting.dart), and none otherwise.
  static Future<PhoneNetwork> open(
      {required String persona,
      List<Middleware<AppState>> middleware = const []}) async {
    final sodium = await SodiumSumoInit.init();
    final identity = await _identity(persona);
    final attestation = RunningAppAttestation(NativePlatformAttestation(
        verifier: const AttestationVerifier(allowAppAttestDevelopment: true)));
    final layer = GrassrootsNetwork(
      identity: identity,
      underlays: const {Underlay.pan, Underlay.lan, Underlay.wan},
      platformAttestation: attestation,
      store: Store<AppState>(appReducer,
          initialState: const AppState(), middleware: middleware),
      sodium: sodium,
    );
    final network = GrassrootsGlpNetwork(layer);
    final transports = await layer.initialize();
    if (transports) await layer.start();
    return PhoneNetwork._(
      layer,
      network,
      glp.PubKey.fromList(identity.publicKey),
      transports,
      attestation,
    );
  }

  /// The agent's key pair: the one kept, or, at the first start, one made
  /// from the seed of the clock, the device and [persona], and kept.
  static Future<GrassrootsIdentity> _identity(String persona) async {
    final kept = await IdentityStore.getIdentity();
    if (kept != null) return kept;
    final seed =
        Uint8List.sublistView(PersonIdentity.forPersona(persona).priv, 0, 32);
    final identity = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(seed));
    await IdentityStore.putIdentity(identity);
    return identity;
  }

  Future<void> dispose() => layer.dispose();
}
