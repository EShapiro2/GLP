/// Live GrassApp scenario, single isolate: alice/charlie cold-call Bob; on
/// accept they message him. Checks the messaging path (text over the friend
/// channel → `received`) actually reaches Bob's UI output.
import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';

const _sg = '/Users/udi/Grassroots/GLP/programs/book/social_graph';

void main() {
  test('accept a friend → actor messages Bob → received reaches Bob', () async {
    final paths = [
      '$_sg/self.glp',
      '$_sg/typed_social_agent.glp',
      '$_sg/typed_ui_mediator.glp',
      '$_sg/play_grassapp_boot.glp',
    ];
    final sources = paths.map((p) => File(p).readAsStringSync()).toList();

    final reply = ReceivePort();
    SendPort? bob;
    final out = <String>[];
    reply.listen((m) {
      if (m is AgentReady) bob = m.commandPort;
      else if (m is AgentOutput) out.add(m.line);
      else if (m is AgentError) out.add('[ERROR] ${m.error}');
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
        {Duration t = const Duration(seconds: 15)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (out.any((l) => l.contains(s))) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return out.any((l) => l.contains(s));
    }

    await waitFor('Ready! Commands');
    final card = await waitFor('befriend(alice, req(1))');
    bob?.send(UserInput('decision(yes, alice, req(1))'));
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
