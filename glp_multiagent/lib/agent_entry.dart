/// The entry of a deployed agent: the application's part in starting it, on
/// the phone, on the Mac and in the tests alike.
///
/// At the agent's first start the entry makes its key pair and saves it in
/// the agent's store; at every start it reads the pair from the store,
/// instantiates the layer with it, and hands the agent's isolate the pair and
/// the way to the layer (IGLP, Implementation Notes, "Boot": "keeps it across
/// restarts ... so the key pair is made once, at the agent's first start.
/// The runtime supplies the key pair to the networking layer whenever it
/// instantiates the layer"; GNA, "Identity": the key pair "is generated and
/// persisted above the networking layer and is supplied to the layer's
/// constructor when it is instantiated").  [AgentRuntime]'s check that the
/// pair's public key is the layer's then holds by construction.
///
/// The layer stays in the application's isolate and the agent runs in its
/// own ([LayerPort]).  Which layer an application runs is the [AgentLayerMaker]
/// it gives, the rest being one path ("transport integration is a backend
/// swap", IGLP, "One interface"): the Flutter layer in the app on a phone
/// (`phone_layer.dart`), the headless layer on the Mac.
///
/// The agent's directory holds its store, `<persona>.state` and
/// `<persona>.state.key`, and beside it the layer's queue,
/// `<persona>.queue.json` (Implementation Notes, "Persistence": "by a store
/// the host gives it, beside the layer's queue").
library;

import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/port_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    show GrassrootsIdentity;

import 'isolate_protocol.dart' show AgentDeployment;

/// A key and an address: a peer's, or a rendezvous server's.
typedef KeyAtAddress = ({PubKey key, String? address});

/// What the application declares to the layer before the agent needs it, as
/// the seam predicates declare it (IGLP, Definition "Seam Predicates"):
/// `declare(rendezvous(P, A))` for each of [rendezvous], and `declare(peer(P))`
/// with `declare(address(P, A))`, where its address is known, for each of
/// [peers].
class AgentDeclarations {
  final List<KeyAtAddress> rendezvous;
  final List<KeyAtAddress> peers;

  const AgentDeclarations({this.rendezvous = const [], this.peers = const []});

  /// The declarations in text, as a build gives them: each of the two a comma
  /// separated list of `<key-hex>@<ip:port>`, the address of a peer optional.
  factory AgentDeclarations.parse(
          {String rendezvous = '', String peers = ''}) =>
      AgentDeclarations(rendezvous: _parse(rendezvous), peers: _parse(peers));

  static List<KeyAtAddress> _parse(String text) => [
        for (final entry in text.split(','))
          if (entry.isNotEmpty)
            (
              key: PubKey.fromHex(entry.split('@').first),
              address: entry.contains('@')
                  ? entry.substring(entry.indexOf('@') + 1)
                  : null,
            ),
      ];
}

/// A networking layer as the application holds it: the runtime's interface
/// over it, its start and its disposal.
class AgentLayer {
  final GlpNetwork network;
  final Future<bool> Function() start;
  final Future<void> Function() dispose;
  AgentLayer(this.network, {required this.start, required this.dispose});
}

/// Instantiate the layer of [identity], its queue kept in the file
/// [queuePath], not yet started.
typedef AgentLayerMaker = Future<AgentLayer> Function(
    GrassrootsIdentity identity, String queuePath);

/// The layer's identity of [keyPair]: the pair of the same seed, the first 32
/// bytes of the private key, in the type the layer takes.
Future<GrassrootsIdentity> identityOf(AgentKeyPair keyPair) async =>
    GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.sublistView(keyPair.priv, 0, 32)));

/// An agent's key pair and running layer, its isolate not yet spawned.
class AgentEntry {
  final AgentKeyPair keyPair;
  final AgentLayer layer;
  final String _statePath;
  final LayerPort _port;

  AgentEntry._(this.keyPair, this.layer, this._statePath, this._port);

  /// Start the layer of the agent of [persona], whose directory is
  /// [directory], the key pair read from its store or, at its first start,
  /// made and saved there.  Throws where the layer does not start.
  static Future<AgentEntry> open({
    required String persona,
    required String directory,
    required AgentLayerMaker makeLayer,
  }) async {
    final statePath = '$directory/$persona.state';
    final keyPair =
        await keptKeyPair(FileAgentStateStore(statePath), persona);
    final layer = await makeLayer(
        await identityOf(keyPair), '$directory/$persona.queue.json');
    // Before the layer starts, so that no event of it is missed.
    final port = LayerPort(layer.network);
    if (!await layer.start()) {
      port.close();
      await layer.dispose();
      throw StateError('the networking layer of $persona did not start');
    }
    return AgentEntry._(keyPair, layer, statePath, port);
  }

  /// Give the layer [declarations].  Called while the layer runs, before the
  /// agent needs them.
  void declare(AgentDeclarations declarations) {
    final network = layer.network;
    for (final (:key, :address) in declarations.rendezvous) {
      network.putRendezvousServer(key, address!);
    }
    for (final (:key, :address) in declarations.peers) {
      network.putKnownPeer(key);
      if (address != null) network.putPeerAddress(key, address);
    }
  }

  /// What the agent's isolate is given ([InitAgent.deployment]).
  AgentDeployment get deployment =>
      AgentDeployment(keyPair, _statePath, _port.sendPort);

  /// End the layer; the agent's isolate has ended.
  Future<void> close() async {
    _port.close();
    await layer.dispose();
  }
}
