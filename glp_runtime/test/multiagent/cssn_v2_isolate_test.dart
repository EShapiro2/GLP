import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Base directories (repo-relative from glp_runtime/).
const _cssnV2Dir = '../programs/cssn_modules_v2';
const _madBootDir = '$_cssnV2Dir/mad_boot';
const _rootSelfGlp = '../programs/self.glp';

void main() {
  group('CSSN v2 Multi-Isolate', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    test('fplay1 runs across isolates (3 adults)', () async {
      final bootFile = File('$_madBootDir/mad_fplay1.glp');
      if (!bootFile.existsSync()) {
        print('Skipping: ${bootFile.path} not found');
        return;
      }

      final bootSource = bootFile.readAsStringSync();
      final loader = BootLoader();
      final config = loader.load(bootSource);
      config.projectDir = _cssnV2Dir;
      config.rootSelfGlpPath = _rootSelfGlp;

      // Verify boot parsing
      expect(config.directives.length, equals(3));
      expect(
        config.directives.map((d) => d.agentId).toList(),
        equals(['alice', 'bob', 'charlie']),
      );
      expect(
        config.directives.every((d) => d.goalFunctor == 'agent_init'),
        isTrue,
      );
      expect(
        config.directives.every((d) => d.goalArity == 3),
        isTrue,
      );
      expect(
        config.directives.every((d) => d.constantArgs.length == 1 && d.constantArgs[0] == '1'),
        isTrue,
      );

      await manager.boot(config,
          traceConfig: TraceConfig(glp: false, mad: true));

      // Start and let event-driven execution run the protocol
      manager.start();

      // fplay1: alice connects to charlie via bob's intro → both accept
      // Allow enough time for the cold-call + intro + accept protocol
      await Future.delayed(Duration(seconds: 10));

      // Termination is external — we shut down in tearDown
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
