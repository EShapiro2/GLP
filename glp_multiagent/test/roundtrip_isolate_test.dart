/// Headless end-to-end round-trip over the REAL two-isolate path.
///
/// Spawns alice + bob as agent isolates (exactly like the live app), routes
/// every AgentSendMad as DeliverMad to the target, and drives:
///   alice: connect(bob)  →  bob shows befriend(alice, req(1))
///   bob:   decision(yes, alice, req(1))  →  BOTH see connected(...)
///
/// This exercises the cross-isolate response variable (a `_w` mutual-ref
/// reader) — the exact case the mediator polarity fix had to handle. A
/// single-heap test would not, since there the response slot is a plain
/// writer.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

const _programs = '/Users/udi/Grassroots/GLP/programs';
const _fixtureDir = '$_programs/tests/agent_roundtrip';
const _sourceFiles = [
  '$_fixtureDir/self.glp',
  '$_fixtureDir/typed_social_agent.glp',
  '$_fixtureDir/typed_ui_mediator.glp',
  '$_fixtureDir/play_ui_boot.glp',
];

void main() {
  test('connect → accept → connected round-trips on both sides', () async {
    final sources = _sourceFiles.map((p) => File(p).readAsStringSync()).toList();
    final rootSelf = File('$_programs/self.glp').absolute.path;

    final reply = ReceivePort();
    final ports = <String, SendPort>{};
    final output = <String, List<String>>{'alice': [], 'bob': []};
    final logs = <String, List<String>>{'alice': [], 'bob': []};
    final ready = <String>{};

    reply.listen((msg) {
      if (msg is AgentReady) {
        ports[msg.agentId] = msg.commandPort;
        ready.add(msg.agentId);
      } else if (msg is AgentOutput) {
        output[msg.agentId]?.add(msg.line);
      } else if (msg is AgentLog) {
        logs[msg.agentId]?.add('[${msg.tag}] ${msg.message}');
      } else if (msg is AgentSendMad) {
        // Route the opaque payload to the destination isolate.
        ports[msg.to]?.send(DeliverMad(msg.agentId, msg.payload));
      } else if (msg is AgentError) {
        output[msg.agentId]?.add('[ERROR] ${msg.error}');
      }
    });

    Future<void> spawn(String id, List<String> friends) => Isolate.spawn(
          agentIsolateEntry,
          InitAgent(
            agentId: id,
            glpSources: sources,
            glpSourcePaths: _sourceFiles, // self.glp ancestor-scope discovery
            rootSelfGlpPath: rootSelf,
            friends: friends,
            replyPort: reply.sendPort,
            deferStart: true,
          ),
        );

    Future<bool> waitUntil(bool Function() cond,
        {Duration timeout = const Duration(seconds: 15)}) async {
      final deadline = DateTime.now().add(timeout);
      while (DateTime.now().isBefore(deadline)) {
        if (cond()) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return cond();
    }

    bool has(String who, String needle) =>
        output[who]!.any((l) => l.contains(needle));

    await spawn('alice', ['bob']);
    await spawn('bob', ['alice']);

    // All ports registered (deferred-start), then release GLP init.
    expect(await waitUntil(() => ready.length == 2), isTrue,
        reason: 'both isolates ready');
    ports['alice']!.send(StartAgent());
    ports['bob']!.send(StartAgent());

    expect(
        await waitUntil(() =>
            has('alice', 'Ready! Commands') && has('bob', 'Ready! Commands')),
        isTrue,
        reason: 'both initialized');

    // Alice cold-calls Bob.
    ports['alice']!.send(UserInput('connect(bob)'));
    expect(await waitUntil(() => has('bob', 'befriend(alice, req(1))')), isTrue,
        reason: 'bob received the befriend notify');

    // Bob accepts.
    ports['bob']!.send(UserInput('decision(yes, alice, req(1))'));

    final bobConnected =
        await waitUntil(() => has('bob', 'connected(alice)'));
    final aliceConnected =
        await waitUntil(() => has('alice', 'connected(bob)'));

    File('/private/tmp/rt-alice-log.txt').writeAsStringSync(logs['alice']!.join('\n'));
    File('/private/tmp/rt-bob-log.txt').writeAsStringSync(logs['bob']!.join('\n'));
    // ignore: avoid_print
    print('ALICE OUT:\n${output['alice']!.join('\n')}');
    // ignore: avoid_print
    print('BOB OUT:\n${output['bob']!.join('\n')}');

    expect(bobConnected, isTrue, reason: 'bob emits connected(alice)');
    expect(aliceConnected, isTrue, reason: 'alice emits connected(bob)');

    for (final p in ports.values) {
      p.send(DisposeAgent());
    }
    reply.close();
  });
}
