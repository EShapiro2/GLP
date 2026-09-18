import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Base directories (repo-relative from glp_runtime/).
const _bondsV2Dir = '../programs/currencies/bonds_v2';
const _madBootDir = '$_bondsV2Dir/mad_boot';
const _rootSelfGlp = '../programs/self.glp';

/// Helper: load boot file, configure project dir, boot, run, and assert what
/// the play produced.
///
/// The play runs to quiescence: manager.settle() returns when every agent has
/// drained and flushed everything it was handed and the traffic that produced
/// has itself settled. It replaced a fixed wall-clock delay, which judged the
/// play at an arbitrary moment and went red under load rather than when
/// anything broke.
///
/// Settling alone proved nothing: until 2026-09-18 every one of the twelve boots
/// was checked against the bare root scope, so `agent/7`, `ui_mediator/5`,
/// `send_to_net/1` and every actor entry were undefined in all of them, each
/// agent's goal failed at once, and the play settled green having produced
/// nothing. Each boot routes every command and notify of its agents through
/// `send_to_person_tagged/3` as `tagged(Id, cmd(_))` and `tagged(Id, notify(_))`,
/// so a play that ran is one in which every agent the boot clause spawns
/// produced at least one such line; that is asserted here, per agent, from
/// [IsolateManager.outputOf]. The lines each play produces beyond that are to
/// be pinned once the twelve load: on 2026-09-18 the type check refused all
/// twelve on their own source (the `actor_dispatch` channel polarity, `tee`'s
/// `Stream(_)` outputs), so nothing could be observed to pin.
Future<void> _runPlay(IsolateManager manager, String bootFilename) async {
  final bootFile = File('$_madBootDir/$bootFilename');
  if (!bootFile.existsSync()) {
    print('Skipping: ${bootFile.path} not found');
    return;
  }

  final bootSource = bootFile.readAsStringSync();
  final loader = BootLoader();
  final config = loader.load(bootSource);
  config.programDir = _bondsV2Dir;
  config.rootSelfGlpPath = _rootSelfGlp;
  config.bootPath = bootFile.path;

  await manager.boot(config, traceConfig: TraceConfig(glp: false, mad: false));
  manager.start();
  await manager.settle();

  expect(manager.faults, isEmpty, reason: 'an agent threw during the play');
  for (final directive in config.directives) {
    final id = directive.agentId;
    final produced = manager.outputOf(id);
    expect(produced.any((l) => l.contains('tagged($id,')), isTrue,
        reason: '$bootFilename: $id produced no tagged(...) line; '
            'its output was $produced');
  }
}

void main() {
  group('Bonds V2 Multi-Isolate', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    // fplay1: solo (alice only)
    test('fplay1 runs across isolates (1 agent)', () async {
      await _runPlay(manager, 'mad_fplay1.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay2-6, 8-9: 2 agents (alice, bob)
    for (final n in [2, 3, 4, 5, 6, 8, 9]) {
      test('fplay$n runs across isolates (2 agents)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay4b: 2 agents with time (alice, bob)
    test('fplay4b runs across isolates (2 agents, time)', () async {
      await _runPlay(manager, 'mad_fplay4b.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay10-11: 2 agents with time (alice, bob)
    for (final n in [10, 11]) {
      test('fplay$n runs across isolates (2 agents, time)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay12: village — 6 agents (alice, bob, charlie, diana, eve, frank)
    test('fplay12 runs across isolates (village, 6 agents)', () async {
      await _runPlay(manager, 'mad_fplay12.glp');
    }, timeout: Timeout(Duration(seconds: 45)));
  });
}
