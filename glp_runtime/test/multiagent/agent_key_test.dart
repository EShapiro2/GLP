/// The agent's key pair is made once, at its first start, and kept across
/// restarts (IGLP, Implementation Notes, "Boot": "makes the agent's key pair
/// from a seed drawn from outside the program ... and keeps it across
/// restarts (Persistence), so the key pair is made once, at the agent's first
/// start"; "Persistence": the store keeps "the agent's key pair").
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:test/test.dart';

const _program = r'''
exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])).
''';

void main() {
  late Directory dir;
  late String program;
  late String root;

  setUpAll(() {
    final programs = '${Directory.current.parent.path}/programs';
    dir = Directory('$programs/tests').createTempSync('glp_agent_key_');
    program = '${dir.path}/keys.glp';
    File(program).writeAsStringSync(_program);
    root = '$programs/self.glp';
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  for (final (name, make) in <(String, AgentStateStore Function(String))>[
    ('in a file', (id) => FileAgentStateStore('${dir.path}/s/$id.state')),
    ('in memory', (_) => MemoryAgentStateStore()),
  ]) {
    test('a store $name: the key pair is made and saved at the first start, '
        'and read at every later one', () async {
      final store = make('kept');
      expect(await store.loadKeyPair(), isNull);
      final first = await keptKeyPair(store, 'alice');
      final saved = await store.loadKeyPair();
      expect(saved?.pub, first.pub);
      expect(saved?.priv, first.priv);
      final second = await keptKeyPair(store, 'alice');
      expect(second.pub, first.pub);
      expect(second.priv, first.priv);
    });
  }

  test('a file of the wrong length is refused, and no other key is made',
      () async {
    final store = FileAgentStateStore('${dir.path}/s/bad.state');
    await File('${dir.path}/s/bad.state.key').create(recursive: true);
    await File('${dir.path}/s/bad.state.key').writeAsBytes(Uint8List(5));
    await expectLater(keptKeyPair(store, 'alice'), throwsStateError);
  });

  test('a host given a store and no key pair makes it at the first start and '
      'has it again at the restart', () async {
    final store = FileAgentStateStore('${dir.path}/s/host.state');
    final first = AgentRuntime(
        agentId: 'alice',
        program: program,
        rootSelfGlpPath: root,
        stateStore: store);
    await first.initialize();
    expect(first.firstStart, isTrue);
    final key = first.publicKey!;
    expect((await store.loadKeyPair())?.pub, key,
        reason: 'the key pair is kept at the first start, before any save');
    // The app goes to the background, which saves the state (IGLP d70ec2e,
    // Implementation Notes, "Persistence"), and terminates.
    expect(await first.save(), isTrue);

    final second = AgentRuntime(
        agentId: 'alice',
        program: program,
        rootSelfGlpPath: root,
        stateStore: store);
    await second.initialize();
    expect(second.firstStart, isFalse);
    expect(second.publicKey, key);
  });
}
