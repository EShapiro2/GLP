/// Single-isolate scenario: ONE heap, real agent/4 + ui_mediator/5 for bob,
/// alice, charlie; in-heap crossbar (no MAD). Bob is the live-UI agent (UserIn
/// injected, notifies observed); alice/charlie are scenario-driven. Validates
/// the substrate the app will use.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

const _fixture = '/Users/udi/Grassroots/GLP/programs/tests/agent_roundtrip';

void main() {
  test('scenario auto-drives bob inbox; accept -> connected (single isolate)',
      () async {
    final paths = [
      '$_fixture/self.glp',
      '$_fixture/typed_social_agent.glp',
      '$_fixture/typed_ui_mediator.glp',
      '$_fixture/play_scenario_boot.glp',
    ];
    final sources = paths.map((p) => File(p).readAsStringSync()).toList();

    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];

    final logs = <String>[];
    reply.listen((m) {
      if (m is AgentReady) bob = m.commandPort;
      else if (m is AgentOutput) out.add(m.line);
      else if (m is AgentLog) logs.add('[${m.tag}] ${m.message}');
      else if (m is AgentError) out.add('[ERROR] ${m.error}');
    });

    await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'bob',
        glpSources: sources,
        glpSourcePaths: paths,
        rootSelfGlpPath:
            File('/Users/udi/Grassroots/GLP/programs/self.glp').absolute.path,
        friends: const ['alice', 'charlie'],
        replyPort: reply.sendPort,
        deferStart: false,
      ),
    );

    Future<bool> waitFor(String needle,
        {Duration t = const Duration(seconds: 12)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (out.any((l) => l.contains(needle))) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return out.any((l) => l.contains(needle));
    }

    await waitFor('Ready! Commands');
    // alice and charlie both cold-call bob -> two befriend cards.
    final gotAlice = await waitFor('befriend(alice, ');
    final gotCharlie = await waitFor('befriend(charlie, ');

    // Accept each card with its actual req id (assigned by bob's mediator in
    // arrival order, so parse it rather than assume).
    final reqOf = RegExp(r'befriend\((\w+), (req\(\d+\))\)');
    final accepted = <String>{};
    for (final l in out.where((l) => l.contains('befriend('))) {
      final m = reqOf.firstMatch(l);
      if (m != null && accepted.add(m.group(1)!)) {
        bob?.send(UserInput('decision(yes, ${m.group(1)}, ${m.group(2)})'));
      }
    }
    final connAlice = await waitFor('connected(alice)');
    final connCharlie = await waitFor('connected(charlie)');

    await Future<void>.delayed(const Duration(milliseconds: 300));
    File('/private/tmp/scen-log.txt').writeAsStringSync(logs.join('\n'));
    // ignore: avoid_print
    print('BOB OUTPUT:\n${out.where((l) => l.startsWith('< ')).join('\n')}');

    bob?.send(DisposeAgent());
    reply.close();

    expect(gotAlice && gotCharlie, isTrue, reason: 'two befriend cards');
    expect(connAlice, isTrue, reason: 'accept alice -> connected(alice)');
    expect(connCharlie, isTrue, reason: 'accept charlie -> connected(charlie)');
  });
}
