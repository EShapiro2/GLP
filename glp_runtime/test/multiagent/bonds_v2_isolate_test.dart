import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Base directories (repo-relative from glp_runtime/).
const _bondsV2Dir = '../programs/bonds_v2';
const _madBootDir = '$_bondsV2Dir/mad_boot';
const _rootSelfGlp = '../programs/self.glp';

/// Helper: load boot file, configure project dir, boot and run.
Future<void> _runPlay(IsolateManager manager, String bootFilename,
    {int timeoutSec = 10}) async {
  final bootFile = File('$_madBootDir/$bootFilename');
  if (!bootFile.existsSync()) {
    print('Skipping: ${bootFile.path} not found');
    return;
  }

  final bootSource = bootFile.readAsStringSync();
  final loader = BootLoader();
  final config = loader.load(bootSource);
  config.projectDir = _bondsV2Dir;
  config.rootSelfGlpPath = _rootSelfGlp;

  await manager.boot(config, traceConfig: TraceConfig(glp: false, mad: false));
  manager.start();
  await Future.delayed(Duration(seconds: timeoutSec));
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
      await _runPlay(manager, 'mad_fplay12.glp', timeoutSec: 15);
    }, timeout: Timeout(Duration(seconds: 45)));
  });
}
