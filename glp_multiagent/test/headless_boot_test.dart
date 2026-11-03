/// Boots of madGLP agents over the real networking layer, each agent an
/// `AgentRuntime` over its own headless layer through GNA's adapter, on UDP
/// loopback, every agent named by its key (IGLP 670d36a, Remark "Agent
/// Names"; Implementation Notes, "Boot", "Networking Seam"), in one process
/// and in one process per agent (test/helpers/headless_boot.dart; Code #22's
/// rung 3, 2026-11-02 07:37 UTC and 2026-11-03 09:06 UTC).
///
/// 1. The entry form: programs/tests/mad_keyed, one callee and N-1 callers,
///    each caller cold-calling the callee, named by the key its host passes,
///    with a writer the callee assigns.
/// 2. The boot form: the madGLP probes Section MB of test/run_all_tests.sh
///    runs under the simulation --- MB1 mad_held_links, MB3
///    mad_stream_writer, MB4 mad_two_crossings, MB5's link probes --- each
///    boot file's agents over its program directory, the person goal's
///    readable names given the agents' keys, each probe held to the end state
///    MB holds it to, a key where MB has a name.
/// 3. linkprobe14, held to MB's check, which holds in every order (GLP #11
///    Cowork, 2026-11-03 16:06 UTC, item 3): no message from bob reaches
///    alice, neither a value on the held link nor an answer to a request of
///    hers, and alice may see bob's greeting, which then came from carol.
///    Over the real layer there is no report that the run is over, so the
///    check is taken once alice has received carol's message and alice's
///    greeting has reached bob or carol, whichever its link leads to.
///
/// Needs a native libsodium, as GNA's headless tests do; self-skips when it
/// is absent.  runprobe's mini-app artefact is written first, as MB writes it,
/// by the REPL's `:artefact`.
///
/// No test has a clock (Timeout.none): every wait is on an event, a line
/// coming or an agent's process exiting or writing an error, and a run that
/// never ends is the suite's watchdog's to stop (test/run_all_tests.sh,
/// Section Q; IsolateManager's boot, 20de107f).
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:sodium/sodium_sumo.dart';

import 'helpers/headless_boot.dart';
import 'programs_dir.dart';

/// A line an agent is held to: the agent's id and a pattern over the line
/// as the agent's transcript shows it, `{alice}` standing for alice's key.
typedef Expected = (String agent, String pattern);

/// A probe: its boot file under programs/tests, the lines it reaches, and
/// how many times a line must come where MB counts (linkprobe11's two
/// greetings).
class Probe {
  final String boot;
  final List<Expected> lines;
  final Map<String, int> counts;
  const Probe(this.boot, this.lines, {this.counts = const {}});
}

/// Section MB's checks, line for line (test/run_all_tests.sh, Section MB).
const probes = [
  Probe('mad_held_links_boot.glp', [
    ('carol', r"held\({carol}, _w\({alice}, [0-9]+\), {bob}\)"),
    ('alice', r"held\({alice}, _w\({alice}, [0-9]+\), {carol}\)"),
    ('alice', r'delivered\(hello_from_carol\)'),
  ]),
  Probe('mad_stream_writer_boot.glp', [
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('mad_two_crossings_boot.glp', [
    ('bob', r'saw\(hello\)'),
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/linkprobe7_boot.glp', [
    ('bob', r'saw\(root\)'),
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/linkprobe9_boot.glp', [
    ('bob', r'saw\(hello\)'),
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/linkprobe10_boot.glp', [
    ('bob', r'saw\(hello\)'),
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/linkprobe11_boot.glp', [
    ('bob', r'saw\(hello\)'),
  ], counts: {
    'bob saw(hello)': 2
  }),
  Probe('linkprobes/linkprobe12_boot.glp', [
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/runprobe_boot.glp', [
    ('bob', r'saw\(hello\)'),
    ('bob', r'saw\(probe\)'),
    ('alice', r'got\(hi\)'),
  ]),
  Probe('linkprobes/linkprobe13_boot.glp', [
    ('alice', r'saw\(hello_from\({bob}\)\)'),
    ('bob', r'saw\(hello_from\({alice}\)\)'),
  ]),
];

void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;
  late String programs;
  late String root;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
    programs = programsDir();
    root = '$programs/self.glp';
    // runprobe's mini-app, certified by the REPL as MB's `:artefact` does.
    final lp = '$programs/tests/linkprobes';
    final r = Process.runSync(
        'bash',
        [
          '-c',
          "printf ':artefact $lp/probeapp $lp/runprobe\\n:quit\\n' | bin/glpc"
        ],
        workingDirectory: '$programs/../glp_runtime');
    if (!File('$lp/runprobe/probeapp.glpw').existsSync()) {
      throw StateError('runprobe/probeapp.glpw was not written: '
          'exit ${r.exitCode}');
    }
  });

  /// One callee and [callers] callers, the callee first: it waits, and is
  /// bound before any caller's cold call is sent.
  List<HeadlessAgentSpec> keyedAgents(int callers) => [
        const HeadlessAgentSpec('callee', 'callee_init/2'),
        for (var i = 1; i <= callers; i++)
          HeadlessAgentSpec('caller$i', 'caller_init/3',
              extraArgs: const [PeerKey('callee')]),
      ];

  /// Whether [line], an agent's transcript line, matches [pattern] with the
  /// agents' keys in the names' places.
  RegExp patternOf(String pattern, Map<String, String> keyOf) {
    var p = pattern;
    for (final MapEntry(key: id, value: key) in keyOf.entries) {
      p = p.replaceAll('{$id}', "'?$key'?");
    }
    return RegExp(p);
  }

  Future<void> holdTo(
      Probe probe,
      Map<String, String> keyOf,
      Map<String, AgentLines> outputs,
      Future<String> Function(String agent, bool Function(String)) line,
      String Function(String) strip) async {
    for (final (agent, pattern) in probe.lines) {
      final re = patternOf(pattern, keyOf);
      final got = await line(agent, (l) => re.hasMatch(strip(l)));
      expect(got, isNotNull);
    }
    for (final MapEntry(key: what, value: n) in probe.counts.entries) {
      final agent = what.split(' ').first;
      final re = patternOf(RegExp.escape(what.substring(agent.length + 1)),
          keyOf);
      final out = outputs[agent]!;
      await out.first((l) =>
          out.lines.where((x) => re.hasMatch(strip(x))).length == n &&
          re.hasMatch(strip(l)));
    }
  }

  /// linkprobe14's check (Section MB, MB5), once alice has received carol's
  /// message and alice's greeting has reached bob or carol: [received] the
  /// senders' keys of what each agent was delivered, [strip] an output line's
  /// transcript mark.
  Future<void> holdLinkprobe14(
      Map<String, String> keyOf,
      Map<String, AgentLines> outputs,
      Map<String, AgentLines> received,
      String Function(String) strip) async {
    await received['alice']!.first((k) => k == keyOf['carol']);
    await Future.any([
      for (final at in ['bob', 'carol'])
        received[at]!.first((k) => k == keyOf['alice']),
    ]);
    expect(received['alice']!.lines, isNot(contains(keyOf['bob'])),
        reason: 'no message from bob reaches alice on the held link');
    final fromCarol = patternOf(r'saw\(hello_from\({bob}\)\)', keyOf);
    for (final l in outputs['alice']!.lines.map(strip)) {
      expect(fromCarol.hasMatch(l), isTrue,
          reason: 'what alice sees is bob\'s greeting, from carol: $l');
    }
  }

  group('one process', () {
    test(
        'the entry form: three agents named by their keys, two cold calls '
        'cross to the callee and two assignments cross back', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final queues = Directory.systemTemp.createTempSync('glp_headless_boot_');
      addTearDown(() => queues.deleteSync(recursive: true));
      final boot = await HeadlessBoot.start(
        agents: keyedAgents(2),
        program: '$programs/tests/mad_keyed',
        rootSelfGlpPath: root,
        sodium: sodium!,
        queueDir: queues.path,
      );
      addTearDown(boot.dispose);
      for (final spec in boot.specs) {
        expect(boot.hosts[spec.id]!.name, boot.keyOf[spec.id],
            reason: '${spec.id} is named by its key');
      }
      final answered1 =
          await boot.line('caller1', (l) => l.startsWith('< answered('));
      final answered2 =
          await boot.line('caller2', (l) => l.startsWith('< answered('));
      expect(answered1, contains('forty_two'));
      expect(answered2, contains('forty_two'));
      final called = boot.outputs['callee']!
          .lines
          .where((l) => l.startsWith('< called('))
          .toList();
      expect(called, hasLength(2));
      for (final l in called) {
        expect(l, contains(boot.keyOf['callee']!));
      }
    }, timeout: Timeout.none);

    for (final probe in probes) {
      test('the boot form, ${probe.boot}: the end state MB holds it to',
          () async {
        if (sodiumUnavailable != null) {
          markTestSkipped(sodiumUnavailable!);
          return;
        }
        final f = HeadlessAgentSpec.ofBootFile('$programs/tests/${probe.boot}');
        final queues =
            Directory.systemTemp.createTempSync('glp_headless_boot_');
        addTearDown(() => queues.deleteSync(recursive: true));
        final boot = await HeadlessBoot.ofBootFile(f,
            rootSelfGlpPath: root, sodium: sodium!, queueDir: queues.path);
        addTearDown(boot.dispose);
        for (final spec in f.agents) {
          expect(boot.hosts[spec.id]!.name, boot.keyOf[spec.id]);
        }
        await holdTo(probe, boot.keyOf, boot.outputs, boot.line,
            (l) => l.startsWith('< ') ? l.substring(2) : l);
      }, timeout: Timeout.none);
    }

    test('linkprobe14: the check MB holds it to, in every order', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final f = HeadlessAgentSpec.ofBootFile(
          '$programs/tests/linkprobes/linkprobe14_boot.glp');
      final queues = Directory.systemTemp.createTempSync('glp_headless_boot_');
      addTearDown(() => queues.deleteSync(recursive: true));
      final boot = await HeadlessBoot.ofBootFile(f,
          rootSelfGlpPath: root, sodium: sodium!, queueDir: queues.path);
      addTearDown(boot.dispose);
      await holdLinkprobe14(boot.keyOf, boot.outputs, boot.received,
          (l) => l.startsWith('< ') ? l.substring(2) : l);
    }, timeout: Timeout.none);
  });

  group('one process per agent', () {
    // The agent program, compiled once before any agent process starts.
    late HeadlessAgentProgram agent;
    setUpAll(() async {
      agent = await HeadlessAgentProgram.compile();
    });
    tearDownAll(() => agent.dispose());

    /// The boot's failure: a [StateError] naming [id] and [when].
    Matcher failsNaming(String id, String when) => throwsA(isA<StateError>()
        .having((e) => e.message, 'message', startsWith('$id '))
        .having((e) => e.message, 'message', contains(when)));

    test(
        'an agent whose process exits before it is ready fails the boot, '
        'named, on the exit', () async {
      // Its boot file is not there: the process fails reading it, before its
      // layer starts and before it prints READY.
      final missing = '$programs/tests/no_such_dir_boot.glp';
      await expectLater(
          HeadlessBootProcesses.start(
            agents: const [HeadlessAgentSpec('alice', 'agent/2')],
            program: '$programs/tests/no_such_dir',
            rootSelfGlpPath: root,
            agent: agent,
            bootFile: HeadlessBootFile(const [], '', '', missing),
          ),
          failsNaming('alice', 'before it was ready'));
    }, timeout: Timeout.none);

    test(
        'an agent whose program does not load fails the boot, named, before '
        'it starts', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      await expectLater(
          HeadlessBootProcesses.start(
            agents: keyedAgents(1),
            program: '$programs/tests/no_such_program',
            rootSelfGlpPath: root,
            agent: agent,
          ),
          failsNaming('callee', 'before it started'));
    }, timeout: Timeout.none);

    test(
        'the entry form: the same three agents in three processes, the cold '
        'calls and the assignments crossing between them', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final boot = await HeadlessBootProcesses.start(
        agents: keyedAgents(2),
        program: '$programs/tests/mad_keyed',
        rootSelfGlpPath: root,
        agent: agent,
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
      final callee = boot.outputs['callee']!;
      await callee.first((l) =>
          callee.lines.where((x) => x.startsWith('called(')).length == 2 &&
          l.startsWith('called('));
    }, timeout: Timeout.none);

    for (final probe in probes) {
      test('the boot form, ${probe.boot}: the end state MB holds it to',
          () async {
        if (sodiumUnavailable != null) {
          markTestSkipped(sodiumUnavailable!);
          return;
        }
        final f = HeadlessAgentSpec.ofBootFile('$programs/tests/${probe.boot}');
        final boot = await HeadlessBootProcesses.ofBootFile(f,
            rootSelfGlpPath: root, agent: agent);
        addTearDown(boot.dispose);
        await holdTo(probe, boot.keyOf, boot.outputs, boot.line, (l) => l);
      }, timeout: Timeout.none);
    }

    test('linkprobe14: the check MB holds it to, in every order', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final f = HeadlessAgentSpec.ofBootFile(
          '$programs/tests/linkprobes/linkprobe14_boot.glp');
      final boot = await HeadlessBootProcesses.ofBootFile(f,
          rootSelfGlpPath: root, agent: agent);
      addTearDown(boot.dispose);
      await holdLinkprobe14(boot.keyOf, boot.outputs, boot.received, (l) => l);
    }, timeout: Timeout.none);
  });
}
