/// Two isolates, `_w` carried through a channel whose Out position is closed
/// (no UI, no mediator).
///
/// Alice MAD-sends a bare writer wrapped in w/1 to bob. Two things are pinned:
/// bob_produced, which is the `_w`-backed reader matching the nested clause
/// head w(W?) and is the admissibility the fixture exists to show; and
/// bob_ch_matched, which is receive/3 committing because this fixture supplies
/// ch(S?, closed) — a constant where receive/3's unit clause reads.
/// mad_w_probe_test.dart is the same file with a writer at Out, and does not.
///
/// Each outcome is asserted singly. The old disjunction matched||otherwise was
/// vacuous: one of bob_consumer's two clauses always fires.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

void main() {
  test('_w matches the nested head; receive/3 commits on a constant at Out',
      () async {
    final probe = File(
            '/Users/udi/Grassroots/GLP/programs/tests/mad_w_clean.glp')
        .readAsStringSync();
    final rootSelf =
        File('/Users/udi/Grassroots/GLP/programs/self.glp').absolute.path;

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
    // the order they reach the output is not fixed, so waiting on one and then
    // reading the other is a race.
    await waitUntil(() =>
        out['bob']!.any((l) => l.contains('bob_produced')) &&
        out['bob']!.any((l) => l.contains('bob_ch_matched')));
    final produced = out['bob']!.any((l) => l.contains('bob_produced'));
    final matched = out['bob']!.any((l) => l.contains('bob_ch_matched'));
    final otherwise = out['bob']!.any((l) => l.contains('bob_ch_otherwise'));

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
        reason: 'receive/3 commits with a constant at Out');
    expect(otherwise, isFalse,
        reason: 'ch(S?, closed) is the clean file; ch(S?, _) is mad_w_probe');
  });
}
