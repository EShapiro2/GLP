import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Base directories (repo-relative from glp_runtime/).
const _cssnV2Dir = '../programs/cssn_modules_v2';
const _madBootDir = '$_cssnV2Dir/mad_boot';
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
  config.projectDir = _cssnV2Dir;
  config.rootSelfGlpPath = _rootSelfGlp;

  await manager.boot(config, traceConfig: TraceConfig(glp: false, mad: false));
  manager.start();
  await Future.delayed(Duration(seconds: timeoutSec));
}

void main() {
  group('CSSN v2 Multi-Isolate', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    // fplay1-3: 3 adults (alice, bob, charlie)
    for (final n in [1, 2, 3]) {
      test('fplay$n runs across isolates (3 adults)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay4-7: 4 agents (alice, bob, carol, dave)
    for (final n in [4, 5, 6, 7]) {
      test('fplay$n runs across isolates (4 agents)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay8: 2 adults (alice, bob)
    test('fplay8 runs across isolates (2 adults)', () async {
      await _runPlay(manager, 'mad_fplay8.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay9-10: 3 agents (alice, bob, dave)
    for (final n in [9, 10]) {
      test('fplay$n runs across isolates (3 agents)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay11: 6 agents (alice, bob, charlie, carol, dave, eve)
    test('fplay11 runs across isolates (6 agents)', () async {
      await _runPlay(manager, 'mad_fplay11.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay12: 5 agents (alice, bob, charlie, dave, eve)
    test('fplay12 runs across isolates (5 agents)', () async {
      await _runPlay(manager, 'mad_fplay12.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay13: village — 6 agents (alice, bob, frank, carol, dave, eve)
    test('fplay13 runs across isolates (village, 6 agents)', () async {
      await _runPlay(manager, 'mad_fplay13.glp', timeoutSec: 15);
    }, timeout: Timeout(Duration(seconds: 45)));
  });
}
