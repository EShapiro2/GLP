/// The Mac's agent of a run over the LAN (lan_agents.dart): the callee, on the
/// headless layer (GLP #12, 2026-11-06 07:13 UTC: "The Mac side runs on
/// HeadlessGlpNetwork, a backend swap"), listening on the Mac's LAN address,
/// which the harness gives as LAN_HOST.  Run by `tool/simctl_lan_agents.sh`,
/// `flutter test test/helpers/lan_mac_agent.dart`, and not by the suite, its
/// file being no `_test`.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/agent_entry.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    show
        FileOutboundQueueStore,
        GrassrootsIdentity,
        HeadlessGrassrootsNetwork,
        initHeadlessSodium;
import 'package:grassroots_networking_glue/headless_glp_network.dart';

import '../programs_dir.dart';
import 'lan_agents.dart';

const _run = String.fromEnvironment('LAN_RUN');
const _board = String.fromEnvironment('LAN_BB');
const _host = String.fromEnvironment('LAN_HOST');

void main() {
  test('the Mac\'s agent answers the cold call', () async {
    final sodium = await initHeadlessSodium();
    Future<AgentLayer> headless(
        GrassrootsIdentity identity, String queue) async {
      final layer = HeadlessGrassrootsNetwork(
        identity: identity,
        sodium: sodium,
        outboundQueueStore: FileOutboundQueueStore(queue),
      );
      return AgentLayer(HeadlessGlpNetwork(layer),
          start: layer.start, dispose: layer.dispose);
    }

    final dir = Directory('${programsDir()}/tests').createTempSync('glp_lan_');
    addTearDown(() => dir.deleteSync(recursive: true));
    final play = '${dir.path}/play.glp';
    File(play).writeAsStringSync(lanPlay);
    final directory = '${dir.path}/agents';

    final board = Blackboard(_board, _run);
    final entry = await AgentEntry.open(
        persona: 'callee', directory: directory, makeLayer: headless);
    final port = (entry.layer.network as HeadlessGlpNetwork).layer.boundPort;
    final underlay = await runCallee(board, entry,
        persona: 'callee',
        program: play,
        rootSelf: '${programsDir()}/self.glp',
        address: '$_host:$port');
    final differs = await secondStart(entry,
        persona: 'callee', directory: directory, makeLayer: headless);
    await board.put('callee_report', 'mac over $underlay; ${differs ?? 'same key'}');
    expect(differs, isNull);
  }, timeout: Timeout.none);
}
