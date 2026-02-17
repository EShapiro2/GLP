import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Base directory for GLP source files (repo-relative from glp_runtime/).
const _socialGraphDir = '../programs/typed_book/social_graph';

/// Read a GLP file from the social_graph directory, or skip the test.
String? _readGlpFile(String filename) {
  final file = File('$_socialGraphDir/$filename');
  if (!file.existsSync()) {
    print('Skipping: $filename not found at ${file.path}');
    return null;
  }
  return file.readAsStringSync();
}

void main() {
  group('Project IsolateManager', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    test('runs full play with actor scripts (no UI)', () async {
      final source = _readGlpFile('project_play_madglp_boot.glp');
      final agentSource = _readGlpFile('project_typed_social_agent.glp');
      final actorSource = _readGlpFile('project_typed_actors.glp');

      if (source == null || agentSource == null || actorSource == null) return;

      final loader = BootLoader();
      final config = loader.load(source);
      config.sharedSources = [agentSource, actorSource];

      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
          isTrue);

      await manager.boot(config,
          traceConfig: TraceConfig(glp: true, mad: true));

      manager.start();
      await Future.delayed(Duration(seconds: 5));
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
