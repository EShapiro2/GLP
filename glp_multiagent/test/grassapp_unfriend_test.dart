/// Live GrassApp (coins) scenario, single isolate: charlie cold-calls Bob, is
/// accepted, pays him, then UNFRIENDS him (paper §4 "End friend"). Bob's UI must
/// surface `unfriended(charlie)` — the "Integrate unfriend" path, end-to-end
/// through both agents and both mediators.
import 'dart:async';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'programs_dir.dart';

final _programs = programsDir();
final _ga = '$_programs/grassapp';

void main() {
  test('charlie pays then unfriends Bob → unfriended(charlie) reaches Bob',
      () async {

    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];
    // Bob's first stats follow his initialisation, the initial run included.
    var started = false;
    reply.listen((m) {
      if (m is AgentReady) {
        bob = m.commandPort;
      } else if (m is AgentOutput) {
        out.add(m.line);
      } else if (m is AgentStats) {
        started = true;
      } else if (m is AgentError) {
        out.add('[ERROR] ${m.error}');
      }
    });

    await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'Bob',
        // programs/grassapp is a program (SGSG, d27e4d6a): loaded as one.
        program: _ga,
        // The boot play's entry point (SGSG, 8412aae7).
        goalLabel: 'scenario_init/3',
        rootSelfGlpPath: '$_programs/self.glp',
        replyPort: reply.sendPort,
        deferStart: false,
      ),
    );

    Future<bool> waitFor(String s,
        {Duration t = const Duration(seconds: 20)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (out.any((l) => l.contains(s))) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return out.any((l) => l.contains(s));
    }

    // The req(N) of a befriend offer from [who] (order-independent).
    Future<String?> reqFor(String who) async {
      final re = RegExp('befriend\\($who, (req\\(\\d+\\))\\)');
      final end = DateTime.now().add(const Duration(seconds: 20));
      while (DateTime.now().isBefore(end)) {
        for (final l in out) {
          final m = re.firstMatch(l);
          if (m != null) return m.group(1);
        }
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return null;
    }

    final end = DateTime.now().add(const Duration(seconds: 20));
    while (!started && DateTime.now().isBefore(end)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }

    // Accept both cold-call friend offers, then let the actors run.
    GTerm accept(String who, String req) => GStruct('decision',
        [const GAtom('yes'), GAtom(who), tryParseTerm(req)!]);
    final aliceReq = await reqFor('alice');
    if (aliceReq != null) bob?.send(UserInput(accept('alice', aliceReq)));
    final charlieReq = await reqFor('charlie');
    if (charlieReq != null) bob?.send(UserInput(accept('charlie', charlieReq)));

    final connected = await waitFor('connected(charlie)');
    // Charlie pays Bob then unfriends him; Bob's UI surfaces the removal.
    final unfriended = await waitFor('unfriended(charlie)');

    await Future<void>.delayed(const Duration(milliseconds: 300));
    // ignore: avoid_print
    print('BOB OUT:\n${out.where((l) => l.startsWith('< ')).join('\n')}');
    bob?.send(DisposeAgent());
    reply.close();

    expect(connected, isTrue, reason: 'connected(charlie)');
    expect(unfriended, isTrue,
        reason:
            'unfriended(charlie) — End friend / Integrate unfriend round-trip');
  });
}
