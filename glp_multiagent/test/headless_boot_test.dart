/// A boot of N agents over the real networking layer, each an `AgentRuntime`
/// over its own headless layer through GNA's adapter, on UDP loopback, every
/// agent named by its key (IGLP, Remark "Agent Names"; Implementation Notes,
/// "Networking Seam"): the pattern of agent_runtime_headless_test.dart over
/// a program directory and N agents, in one process and in one process per
/// agent (test/helpers/headless_boot.dart; Code #22's rung 3, 2026-11-02
/// 07:37 UTC).  The program is programs/tests/mad_keyed: one callee and N-1
/// callers, each caller cold-calling the callee, named by the key its host
/// passes, with a writer the callee assigns.
///
/// Needs a native libsodium, as GNA's headless tests do; self-skips when it
/// is absent.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

import 'helpers/headless_boot.dart';
import 'programs_dir.dart';

void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;
  late String program;
  late String root;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
    program = '${programsDir()}/tests/mad_keyed';
    root = '${programsDir()}/self.glp';
  });

  /// One callee and [callers] callers, the callee first: it waits, and is
  /// bound before any caller's cold call is sent.
  List<HeadlessAgentSpec> agentsOf(int callers) => [
        const HeadlessAgentSpec('callee', 'callee_init/2'),
        for (var i = 1; i <= callers; i++)
          HeadlessAgentSpec('caller$i', 'caller_init/3',
              extraArgs: const [PeerKey('callee')]),
      ];

  test(
      'one process: three agents named by their keys, two cold calls cross '
      'to the callee and two assignments cross back', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final queues = Directory.systemTemp.createTempSync('glp_headless_boot_');
    addTearDown(() => queues.deleteSync(recursive: true));
    final boot = await HeadlessBoot.start(
      agents: agentsOf(2),
      program: program,
      rootSelfGlpPath: root,
      sodium: sodium!,
      queueDir: queues.path,
    );
    addTearDown(boot.dispose);
    for (final spec in boot.specs) {
      expect(boot.hosts[spec.id]!.name, boot.keyOf[spec.id],
          reason: '${spec.id} is named by its key');
    }

    // Each cold call crosses: the callee's program receives it on its network
    // input, addressed to its own name, its key; and each assignment crosses
    // back to its caller.
    final answered1 =
        await boot.line('caller1', (l) => l.startsWith('< answered('));
    final answered2 =
        await boot.line('caller2', (l) => l.startsWith('< answered('));
    expect(answered1, contains('forty_two'));
    expect(answered2, contains('forty_two'));
    final called = boot.outputs['callee']!
        .where((l) => l.startsWith('< called('))
        .toList();
    expect(called, hasLength(2));
    for (final l in called) {
      expect(l, contains(boot.keyOf['callee']!));
    }
  }, timeout: const Timeout(Duration(minutes: 3)));

  test(
      'one process per agent: the same three agents in three processes, the '
      'cold calls and the assignments crossing between them', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final boot = await HeadlessBootProcesses.start(
      agents: agentsOf(2),
      program: program,
      rootSelfGlpPath: root,
      helper: 'test/helpers/headless_boot_agent.dart',
    );
    addTearDown(boot.dispose);

    final answered1 =
        await boot.line('caller1', (l) => l.startsWith('answered('));
    final answered2 =
        await boot.line('caller2', (l) => l.startsWith('answered('));
    expect(answered1, contains('forty_two'));
    expect(answered2, contains('forty_two'));
    final called = await boot.line('callee', (l) => l.startsWith('called('));
    expect(called, contains(boot.keyOf['callee']!));
    // The second call's report may follow the first by a moment.
    await lineOf(boot.outputs['callee']!, (l) =>
        boot.outputs['callee']!.where((x) => x.startsWith('called(')).length ==
            2 &&
        l.startsWith('called('));
  }, timeout: const Timeout(Duration(minutes: 4)));
}
