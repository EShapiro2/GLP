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

    // fplay4-7: 2 families (alice+carol, bob+dave)
    for (final n in [4, 5, 6, 7]) {
      test('fplay$n runs across isolates (2 families)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay8: 2 adults (alice, bob)
    test('fplay8 runs across isolates (2 adults)', () async {
      await _runPlay(manager, 'mad_fplay8.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay9-10: alice adult + bob+dave family
    for (final n in [9, 10]) {
      test('fplay$n runs across isolates (adult + family)', () async {
        await _runPlay(manager, 'mad_fplay$n.glp');
      }, timeout: Timeout(Duration(seconds: 30)));
    }

    // fplay11: 3 families (alice+carol, bob+dave, charlie+eve)
    test('fplay11 runs across isolates (3 families)', () async {
      await _runPlay(manager, 'mad_fplay11.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay12: alice adult + bob+dave family + charlie+eve family
    test('fplay12 runs across isolates (adult + 2 families)', () async {
      await _runPlay(manager, 'mad_fplay12.glp');
    }, timeout: Timeout(Duration(seconds: 30)));

    // fplay13: village (alice+carol, bob+dave+eve, frank)
    test('fplay13 runs across isolates (village)', () async {
      await _runPlay(manager, 'mad_fplay13.glp', timeoutSec: 15);
    }, timeout: Timeout(Duration(seconds: 45)));
  });
}
