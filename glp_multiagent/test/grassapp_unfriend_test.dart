/// Live GrassApp (coins) scenario, single isolate: charlie cold-calls Bob, is
/// accepted, pays him, then UNFRIENDS him (paper §4 "End friend"). Bob's UI must
/// surface `unfriended(charlie)` — the "Integrate unfriend" path, end-to-end
/// through both agents and both mediators.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

const _ga = '/Users/udi/Grassroots/GLP/programs/book/grassapp';

void main() {
  test('charlie pays then unfriends Bob → unfriended(charlie) reaches Bob',
      () async {
    final paths = [
      '$_ga/self.glp',
      '$_ga/grassapp_agent.glp',
      '$_ga/grassapp_mediator.glp',
      '$_ga/play_grassapp_boot.glp',
    ];
    final sources = paths.map((p) => File(p).readAsStringSync()).toList();

    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];
    reply.listen((m) {
      if (m is AgentReady) {
        bob = m.commandPort;
      } else if (m is AgentOutput) {
        out.add(m.line);
      } else if (m is AgentError) {
        out.add('[ERROR] ${m.error}');
      }
    });

    await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'Bob',
        glpSources: sources,
        glpSourcePaths: paths,
        rootSelfGlpPath:
            File('/Users/udi/Grassroots/GLP/programs/self.glp').absolute.path,
        friends: const ['alice', 'charlie'],
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

    await waitFor('Ready! Commands');

    // Accept both cold-call friend offers, then let the actors run.
    final aliceReq = await reqFor('alice');
    bob?.send(UserInput('decision(yes, alice, $aliceReq)'));
    final charlieReq = await reqFor('charlie');
    bob?.send(UserInput('decision(yes, charlie, $charlieReq)'));

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
