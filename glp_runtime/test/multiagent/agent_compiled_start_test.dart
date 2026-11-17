/// An agent started from a compiled program its host gives it (IGLP,
/// Implementation Notes, "Boot": "A host may give an agent, at its start, the
/// compiled program it runs in place of the program's source: a module value,
/// which the loader verifies as it verifies a module the agent adopts, and
/// whose certificate's key the host accepts; the agent runs it and compiles
/// nothing"): here the same agent at a restart, given the program its store
/// keeps (Persistence, "A restart resumes from the state the store holds").
@Timeout.none
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/stored_program.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// The agent tells its person `went` when its person says `go`, on the
/// person channel.
const String _echo = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, Out?), ch(_, [])) :- echo(In?, Out).

procedure echo(Stream(Constant)?, Stream(Constant)).
echo([go | In], [went | Out?]) :- echo(In?, Out).
echo([], []).
''';

void main() {
  late String root;
  late Directory dir;
  late String echo;
  final out = <String, List<String>>{};

  setUpAll(() {
    dir = Directory('../programs/tests').createTempSync('glp_agent_compiled_');
    echo = '${dir.path}/echo.glp';
    File(echo).writeAsStringSync(_echo);
    root = File('../programs/self.glp').absolute.path;
  });
  tearDownAll(() => dir.deleteSync(recursive: true));
  setUp(out.clear);

  AgentRuntime host(String id, String program, AgentStateStore store,
      ({PubKey pub, Uint8List priv}) kp,
      {StoredProgram? compiled}) {
    final agent = AgentRuntime(
      agentId: id,
      program: program,
      rootSelfGlpPath: root,
      keyPair: kp,
      stateStore: store,
      compiled: compiled,
    );
    agent.onOutput = out.putIfAbsent(id, () => []).add;
    agent.onSendMadMessage = (to, payload) async {};
    return agent;
  }

  test('the same agent restarted is given the program its store keeps, '
      'resumes from its saved state and compiles nothing', () async {
    final kp = generateKeyPair();
    final store = MemoryAgentStateStore();
    final a1 = host('a', echo, store, kp);
    await a1.initialize();
    await a1.injectUserInput(ConstTerm('go'));
    expect(await a1.save(), isTrue);
    a1.dispose();
    final from = out['a']!.length;

    final kept = StoredProgram.fromBytes((await store.loadProgram())!);
    final a2 = host('a', '${dir.path}/no_such_source.glp', store, kp,
        compiled: kept);
    await a2.initialize();
    final after = out['a']!.sublist(from).join('\n');
    expect(after, contains('Restored the agent\'s saved state'));
    expect(a2.updateRefusal, isNull);
    a2.dispose();
  });
}
