/// An agent started from a compiled program its host gives it (IGLP,
/// Implementation Notes, "Boot": "A host may give an agent, at its start, the
/// compiled program it runs in place of the program's source: a module value,
/// which the loader verifies as it verifies a module the agent adopts, and
/// whose certificate's key the host accepts; the agent runs it and compiles
/// nothing"): a new agent at its first start, given the program its host
/// compiled under the host's key ([StoredProgram.compile]); and the same agent
/// at a restart, given the program its store keeps (Persistence, "A restart
/// resumes from the state the store holds").
@Timeout.none
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/stored_program.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// An agent that renders to its person itself: it calls send_to_user/1, an
/// OS-privileged predicate, and its compilation is refused a certificate.
const String _privileged = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, Out?), ch(_, [])) :- Out = [], send_to_user([hello]).
''';

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
    File('${dir.path}/privileged.glp').writeAsStringSync(_privileged);
    File('${dir.path}/gone.glp')
        .writeAsStringSync(_echo.replaceAll('went', 'gone'));
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

  test('a new agent given the program its host compiled under the host\'s '
      'key runs it at its first start and compiles nothing', () async {
    final host_ = PersonIdentity.generate();
    final given = StoredProgram.compile(echo,
        rootSelfGlpPath: root, host: host_);
    expect((given.module!.artefact as dynamic).certificate.compilerKey,
        host_.pub, reason: 'the certificate is under the host\'s key');
    // Through the bytes the host keeps on disc, and with no source.
    final bytes = given.toBytes();
    final a = host('a', '${dir.path}/no_such_source.glp',
        MemoryAgentStateStore(), generateKeyPair(),
        compiled: StoredProgram.fromBytes(bytes));
    await a.initialize();
    await a.injectUserInput(ConstTerm('go'));
    expect(out['a']!.join('\n'), contains('< went'));
    a.dispose();
  });

  test('four agents of four keys are given the same bytes', () async {
    final bytes =
        StoredProgram.compile(echo, rootSelfGlpPath: root,
                host: PersonIdentity.generate())
            .toBytes();
    for (final id in ['p1', 'p2', 'p3', 'p4']) {
      final a = host(id, '${dir.path}/no_such_source.glp',
          MemoryAgentStateStore(), generateKeyPair(),
          compiled: StoredProgram.fromBytes(bytes));
      await a.initialize();
      await a.injectUserInput(ConstTerm('go'));
      expect(out[id]!.join('\n'), contains('< went'));
      a.dispose();
    }
  });

  test('an agent given a directory program its host compiled starts it',
      () async {
    final bytes = StoredProgram.compile('../programs/apps/gkk',
            rootSelfGlpPath: root, host: PersonIdentity.generate())
        .toBytes();
    final a = host('a', '${dir.path}/no_such_source', MemoryAgentStateStore(),
        generateKeyPair(),
        compiled: StoredProgram.fromBytes(bytes));
    await a.initialize();
    expect(out['a']!.join('\n'), contains('[GOAL] Started agent'));
    a.dispose();
  });

  test('a compiled program whose code is not its module\'s is not run',
      () async {
    final given = StoredProgram.compile(echo,
        rootSelfGlpPath: root, host: PersonIdentity.generate());
    // The module of the echo program, with the code of another program.
    final other = StoredProgram.compile('${dir.path}/gone.glp',
        rootSelfGlpPath: root, host: PersonIdentity.generate());
    final a = host('a', '${dir.path}/no_such_source.glp',
        MemoryAgentStateStore(), generateKeyPair(),
        compiled: StoredProgram.compiled(other.code, given.module));
    await expectLater(a.initialize(), throwsArgumentError);
    a.dispose();
  });

  test('a program refused a certificate cannot be compiled to be given', () {
    expect(
        () => StoredProgram.compile('${dir.path}/privileged.glp',
            rootSelfGlpPath: root, host: PersonIdentity.generate()),
        throwsA(isA<StateError>().having((e) => e.message, 'message',
            contains('cannot be given compiled'))));
  });
}

