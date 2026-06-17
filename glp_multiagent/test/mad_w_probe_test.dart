/// Minimal two-isolate `_w` writer-match probe (no UI, no mediator).
/// Alice MAD-sends a bare writer wrapped in w/1 to bob; bob tries to match the
/// received payload against the nested reader pattern w(X?). Discriminates
/// whether a `_w`-backed reader is admissible in a clause head.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

void main() {
  test('does a _w-backed reader match a nested clause head w(X?)?', () async {
    final probe = File(
            '/Users/udi/Grassroots/GLP/programs/tests/mad_w_probe.glp')
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

    final matched = await waitUntil(
        () => out['bob']!.any((l) => l.contains('bob_ch_matched')));
    final otherwise =
        out['bob']!.any((l) => l.contains('bob_ch_otherwise'));

    // ignore: avoid_print
    print('BOB OUT: ${out['bob']}');
    // ignore: avoid_print
    print('ALICE OUT: ${out['alice']}');

    for (final p in ports.values) {
      p.send(DisposeAgent());
    }
    reply.close();

    // The report we want either way: matched (=> `_w` ok, bug is elsewhere) or
    // otherwise (=> `_w` reader not admissible in a nested head — the bug).
    expect(matched || otherwise, isTrue,
        reason: 'bob classified the payload (matched=$matched, otherwise=$otherwise)');
  });
}
