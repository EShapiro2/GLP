/// The networking layer of the app: [phoneLayer], the [AgentLayerMaker] the
/// app's entry gives ([AgentEntry]).
///
/// On a phone the layer of the Flutter package, over GNA's backend glue, runs
/// the LAN and the WAN; the PAN comes with the phones themselves, whose
/// Info.plist will then declare the use of Bluetooth.  On the Mac, which the
/// Flutter package has no target for, the headless layer (IGLP,
/// Implementation Notes, "One interface": "transport integration is a backend
/// swap").
library;

import 'dart:io';

import 'package:grassroots_networking/grassroots_networking.dart'
    show GrassrootsNetwork;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'agent_entry.dart';

Future<AgentLayer> phoneLayer(
    GrassrootsIdentity identity, String queuePath) async {
  final sodium = await SodiumSumoInit.init();
  final queue = FileOutboundQueueStore(queuePath);
  if (Platform.isIOS || Platform.isAndroid) {
    final layer = GrassrootsNetwork(
      identity: identity,
      underlays: const {Underlay.lan, Underlay.wan},
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      outboundQueueStore: queue,
    );
    return AgentLayer(GrassrootsGlpNetwork(layer),
        start: layer.initialize, dispose: layer.dispose);
  }
  final layer = HeadlessGrassrootsNetwork(
    identity: identity,
    sodium: sodium,
    outboundQueueStore: queue,
  );
  return AgentLayer(HeadlessGlpNetwork(layer),
      start: layer.start, dispose: layer.dispose);
}
