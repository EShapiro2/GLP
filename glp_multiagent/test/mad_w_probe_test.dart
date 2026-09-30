/// Two isolates, `_w` carried through a channel whose Out position is an
/// unassigned writer (no UI, no mediator).
///
/// Alice MAD-sends a bare writer wrapped in w/1 to bob. Two things are pinned:
/// bob_produced, which is the `_w`-backed reader matching the nested clause
/// head w(W?) and is the admissibility the fixture exists to show; and
/// bob_ch_matched, which is bob_consumer's first clause committing once S is
/// bound: its head is ch([wrapped(Y?)|In], Out?), the form receive/3's unit
/// clause unfolds to, this fixture supplies ch(S?, _), and by GLP-Spec "Writer
/// MGU" the writer at Out is assigned the head's reader. `otherwise` does not
/// fire behind it, since it succeeds only if all previous clauses fail
/// (appendix-guards.tex), and the first clause suspends until S is bound and
/// then commits.
///
/// Each outcome is asserted singly. The old disjunction matched||otherwise was
/// vacuous: one of bob_consumer's two clauses always fires.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

import 'programs_dir.dart';

void main() {
  test('_w matches the nested head; the channel head commits with a writer at Out',
      () async {
    final programs = programsDir();
    final probe = File('$programs/tests/mad_w_probe.glp').readAsStringSync();
    final rootSelf = '$programs/self.glp';

    final reply = ReceivePort();
    final ports = <String, SendPort>{};
    final out = <String, List<String>>{'alice': [], 'bob': []};
    final ready = <String>{};

    reply.listen((msg) {
      if (msg is AgentReady) {
        ports[msg.agentId] = msg.commandPort;
        ready.add(msg.agentId);
      } else if (msg is AgentOutput) {
        out[msg.agentId]?.add(msg.line);
      } else if (msg is AgentSendMad) {
        ports[msg.to]?.send(DeliverMad(msg.agentId, msg.payload));
      } else if (msg is AgentError) {
        out[msg.agentId]?.add('[ERROR] ${msg.error}');
      }
    });

    Future<void> spawn(String id) => Isolate.spawn(
          agentIsolateEntry,
          InitAgent(
            agentId: id,
            glpSources: [probe],
            rootSelfGlpPath: rootSelf,
            friends: id == 'alice' ? ['bob'] : ['alice'],
            replyPort: reply.sendPort,
            deferStart: true,
          ),
        );

    Future<bool> waitUntil(bool Function() c,
        {Duration t = const Duration(seconds: 12)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (c()) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return c();
    }

    await spawn('alice');
    await spawn('bob');
    await waitUntil(() => ready.length == 2);
    ports['alice']!.send(StartAgent());
    ports['bob']!.send(StartAgent());

    // Wait for BOTH reports. bob_producer and bob_consumer are concurrent, and
    // the order they reach the output is not fixed: under load bob_produced can
    // arrive after bob_ch_matched. Waiting on one and then reading the other
    // is a race, and it is what turned this test red inside the full suite while
    // it passed run alone.
    await waitUntil(() =>
        out['bob']!.any((l) => l.contains('bob_produced')) &&
        out['bob']!.any((l) => l.contains('bob_ch_matched')));
    final produced = out['bob']!.any((l) => l.contains('bob_produced'));
    final otherwise = out['bob']!.any((l) => l.contains('bob_ch_otherwise'));
    final matched = out['bob']!.any((l) => l.contains('bob_ch_matched'));

    // ignore: avoid_print
    print('BOB OUT: ${out['bob']}');
    // ignore: avoid_print
    print('ALICE OUT: ${out['alice']}');

    for (final p in ports.values) {
      p.send(DisposeAgent());
    }
    reply.close();

    expect(produced, isTrue,
        reason: 'the `_w`-backed reader matched the nested head w(W?)');
    expect(matched, isTrue,
        reason: 'the writer at Out is assigned the head reader Out? (Writer '
            'MGU), so bob_consumer commits once S is bound');
    expect(otherwise, isFalse,
        reason: 'otherwise waits while the first clause suspends, and that '
            'clause then commits');
  });
}
