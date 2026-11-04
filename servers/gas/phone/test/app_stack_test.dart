// The app's own stack as main.dart builds it, from the unpacked bundle and its
// starter GKK installed as a package --- GNA's GrassrootsNetwork with
// Bluetooth and the local network, through GrassrootsGlpNetwork, wrapped by
// the relay's network, and the person's agent over it --- up to the agent
// started, without the radios (the layer is not initialized).  The headless
// tests use another layer; this one caught the agent refusing to start over
// a wrapper that set the place callback on a layer with no place backend.
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:gas_core/contracts.dart';
import 'package:gas_phone/main.dart' show unbundle;
import 'package:gas_core/person_host.dart';
import 'package:gas_core/relay_network.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart' show initHeadlessSodium;
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:redux/redux.dart';


void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  test('the agent starts over the app\'s own network stack', () async {
    final sodium = await initHeadlessSodium();
    final dir = Directory.systemTemp.createTempSync('gas_stack_');
    addTearDown(() => dir.deleteSync(recursive: true));
    final base = await unbundle('${dir.path}/gas');
    final store = ContractStore(base);
    await store.installStarters();
    final gkk = store.installed().firstWhere((c) => c.id == 'GKK');
    final identity = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(List.filled(32, 5)));
    final layer = GrassrootsNetwork(
      identity: identity,
      underlays: {Underlay.pan, Underlay.lan},
      store: Store<AppState>(appReducer, initialState: AppState()),
      sodium: sodium,
    );
    final net = RelayGlpNetwork(
      inner: GrassrootsGlpNetwork(layer),
      sodium: sodium,
      privateKey: Uint8List.fromList(identity.privateKey),
      app: gkk.name,
      dir: '${dir.path}/relay',
      relayUrl: 'http://127.0.0.1:9',
    );
    addTearDown(net.dispose);
    final h = PersonHost(
      program: store.program(gkk),
      rootSelfGlp: '$base/programs/self.glp',
      keyPair: (pub: glp.PubKey.fromList(identity.publicKey), priv: Uint8List.fromList(identity.privateKey)),
      network: net,
    );
    await h.start(calendar: false);
    expect(h.status, 'ready');
  });
}
