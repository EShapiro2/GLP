/// Live GrassApp scenario, single isolate: alice/charlie cold-call Bob; on
/// accept they message him. Checks the messaging path (text over the friend
/// channel → `received`) actually reaches Bob's UI output.
import 'dart:async';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'programs_dir.dart';

final _programs = programsDir();

/// The one program Bob's agent runs: the scenario, with agent/4 and
/// ui_mediator/5 reached through agent_roundtrip/self.glp.
final _program = '$_programs/tests/agent_roundtrip/play_grassapp';

void main() {
  test('accept a friend → actor messages Bob → received reaches Bob', () async {
    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];
    // An agent error is the test's failure, not a line among the output: until
    // 2026-09-18 it went into `out`, which nothing inspected for it, and a load
    // refused by the type checker showed as a 30 s timeout.
    final errors = <String>[];
    // Bob's first stats follow his initialisation, the initial run included.
    var started = false;
    reply.listen((m) {
      if (m is AgentReady) bob = m.commandPort;
      else if (m is AgentOutput) out.add(m.line);
      else if (m is AgentStats) started = true;
      else if (m is AgentError) errors.add(m.error);
    });

    await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'Bob',
        program: _program,
        rootSelfGlpPath: '$_programs/self.glp',
        replyPort: reply.sendPort,
        deferStart: false,
      ),
    );

    Future<bool> waitFor(String s,
        {Duration t = const Duration(seconds: 15)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (errors.isNotEmpty) {
          reply.close();
          fail('Bob reported an error: ${errors.first}');
        }
        if (out.any((l) => l.contains(s))) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return out.any((l) => l.contains(s));
    }

    final end = DateTime.now().add(const Duration(seconds: 15));
    while (!started && errors.isEmpty && DateTime.now().isBefore(end)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    if (errors.isNotEmpty) fail('Bob reported an error: ${errors.first}');
    expect(started, isTrue, reason: 'Bob initialised');
    final card = await waitFor('befriend(alice, req(1))');
    bob?.send(UserInput(const GStruct('decision',
        [GAtom('yes'), GAtom('alice'), GStruct('req', [GInt(1)])])));
    final connected = await waitFor('connected(alice)');
    // Alice's actor sends a message once connected; it should reach Bob.
    final got = await waitFor("received(alice");

    await Future<void>.delayed(const Duration(milliseconds: 300));
    // ignore: avoid_print
    print('BOB OUT:\n${out.where((l) => l.startsWith('< ')).join('\n')}');
    bob?.send(DisposeAgent());
    reply.close();

    expect(card, isTrue, reason: 'befriend(alice) card');
    expect(connected, isTrue, reason: 'connected(alice)');
    expect(got, isTrue, reason: 'received(alice, ...) — messaging over friend channel');
  });
}
