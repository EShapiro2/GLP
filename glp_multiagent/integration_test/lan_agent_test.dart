/// A simulator's agent of a run over the LAN (test/helpers/lan_agents.dart),
/// by the app's own entry and layer ([AgentEntry], [phoneLayer]): LAN_ROLE
/// `caller` or `callee`, the blackboard at LAN_BB, its keys under LAN_RUN.
/// The harness is `tool/simctl_lan_agents.sh`.
///
/// A simulator's agent gives the blackboard no address: the layer of the
/// Flutter package does not tell its port, and the peer finds it by DNS-SD,
/// knowing its key (GNA, "LAN Discovery").
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/agent_entry.dart';
import 'package:glp_multiagent/glp_sources.dart';
import 'package:glp_multiagent/phone_layer.dart';
import 'package:integration_test/integration_test.dart';
import 'package:path_provider/path_provider.dart';

import '../test/helpers/lan_agents.dart';

const _role = String.fromEnvironment('LAN_ROLE');
const _run = String.fromEnvironment('LAN_RUN');
const _board = String.fromEnvironment('LAN_BB');

void main() {
  IntegrationTestWidgetsFlutterBinding.ensureInitialized();

  testWidgets('the agent of role $_role on the simulator', (tester) async {
    await tester.runAsync(() async {
      final glp = await resolveGlpPaths();
      final play =
          '${File(glp.rootSelfGlp).parent.path}/tests/lan_play.glp';
      await File(play).parent.create(recursive: true);
      await File(play).writeAsString(lanPlay);
      final support = await getApplicationSupportDirectory();
      final directory = '${support.path}/agents-$_run';

      final board = Blackboard(_board, _run);
      final entry = await AgentEntry.open(
          persona: _role, directory: directory, makeLayer: phoneLayer);
      final underlay = _role == 'caller'
          ? await runCaller(board, entry,
              persona: _role, program: play, rootSelf: glp.rootSelfGlp)
          : await runCallee(board, entry,
              persona: _role,
              program: play,
              rootSelf: glp.rootSelfGlp,
              address: null);
      final differs = await secondStart(entry,
          persona: _role, directory: directory, makeLayer: phoneLayer);
      await board.put('${_role}_report',
          'simulator over $underlay; ${differs ?? 'same key'}');
      expect(differs, isNull);
    });
  }, timeout: Timeout.none);
}
