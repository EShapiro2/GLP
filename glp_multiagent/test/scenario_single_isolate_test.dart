/// Single-isolate scenario: ONE heap, real agent/4 + ui_mediator/5 for bob,
/// alice, charlie; in-heap crossbar (no MAD). Bob is the live-UI agent (UserIn
/// injected, notifies observed); alice/charlie are scenario-driven. Validates
/// the substrate the app will use.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'programs_dir.dart';

final _programs = programsDir();

/// The one program bob's agent runs: the scenario, with agent/4 and
/// ui_mediator/5 reached through agent_roundtrip/self.glp.
final _program = '$_programs/tests/agent_roundtrip/play_scenario';

void main() {
  test('scenario auto-drives bob inbox; accept -> connected (single isolate)',
      () async {
    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];

    final logs = <String>[];
    // An agent error is the test's failure, not a line among the output: until
    // 2026-09-18 it went into `out`, which nothing inspected for it, and a load
    // refused by the type checker showed as a 30 s timeout.
    final errors = <String>[];
    // Bob's first stats follow his initialisation, the initial run included.
    var started = false;
    reply.listen((m) {
      if (m is AgentReady) bob = m.commandPort;
      else if (m is AgentOutput) out.add(m.line);
      else if (m is AgentLog) logs.add('[${m.tag}] ${m.message}');
      else if (m is AgentStats) started = true;
      else if (m is AgentError) errors.add(m.error);
    });

    await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'bob',
        program: _program,
        rootSelfGlpPath: '$_programs/self.glp',
        replyPort: reply.sendPort,
        deferStart: false,
      ),
    );

    Future<bool> waitFor(String needle,
        {Duration t = const Duration(seconds: 12)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (errors.isNotEmpty) {
          reply.close();
          fail('bob reported an error: ${errors.first}');
        }
        if (out.any((l) => l.contains(needle))) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return out.any((l) => l.contains(needle));
    }

    final end = DateTime.now().add(const Duration(seconds: 12));
    while (!started && errors.isEmpty && DateTime.now().isBefore(end)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    if (errors.isNotEmpty) fail('bob reported an error: ${errors.first}');
    expect(started, isTrue, reason: 'bob initialised');
    // alice and charlie both cold-call bob -> two befriend cards.
    final gotAlice = await waitFor('befriend(alice, ');
    final gotCharlie = await waitFor('befriend(charlie, ');

    // Accept each card with its actual req id (assigned by bob's mediator in
    // arrival order, so parse it rather than assume).
    final reqOf = RegExp(r'befriend\((\w+), req\((\d+)\)\)');
    final accepted = <String>{};
    for (final l in out.where((l) => l.contains('befriend('))) {
      final m = reqOf.firstMatch(l);
      if (m != null && accepted.add(m.group(1)!)) {
        bob?.send(UserInput(GStruct('decision', [
          const GAtom('yes'),
          GAtom(m.group(1)!),
          GStruct('req', [GInt(int.parse(m.group(2)!))]),
        ])));
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
