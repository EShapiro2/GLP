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
  group('IsolateManager', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    test('boots three agents from boot config', () async {
      // Minimal program that completes immediately
      final source = '''
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

procedure agent_init(_?, _?).
agent_init(_, _) :- true.
''';

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;

      await manager.boot(config);

      // Start and let event-driven execution run
      manager.start();
      await Future.delayed(Duration(milliseconds: 200));

      // Test verifies boot + start don't crash with trivial goals
    }, timeout: Timeout(Duration(seconds: 10)));

    test('runs full play with actor scripts (no UI)', () async {
      final source = _readGlpFile('play_madglp_boot.glp');
      final agentSource = _readGlpFile('typed_social_agent.glp');
      final actorSource = _readGlpFile('typed_actors.glp');

      if (source == null || agentSource == null || actorSource == null) return;

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      config.sharedSources = [agentSource, actorSource];

      // Should parse correctly with agent_init goal (actors spawned internally)
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
          isTrue);

      await manager.boot(config,
          traceConfig: TraceConfig(glp: true, mad: true));

      // Start and let event-driven execution run the protocol
      manager.start();
      await Future.delayed(Duration(seconds: 5));

      // Termination is external — we shut down in tearDown
    }, timeout: Timeout(Duration(seconds: 30)));

    test('runs full play with UI mediator and UI actors', () async {
      final source = _readGlpFile('play_ui_madglp_boot.glp');
      final agentSource = _readGlpFile('typed_social_agent.glp');
      final mediatorSource = _readGlpFile('typed_ui_mediator.glp');
      final uiActorSource = _readGlpFile('typed_ui_actors.glp');

      if (source == null || agentSource == null ||
          mediatorSource == null || uiActorSource == null) return;

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      config.sharedSources = [agentSource, mediatorSource, uiActorSource];

      // Should parse correctly with agent_init goal
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
          isTrue);

      await manager.boot(config,
          traceConfig: TraceConfig(glp: true, mad: true));

      // Start and let event-driven execution run the protocol
      manager.start();
      await Future.delayed(Duration(seconds: 5));

      // Termination is external — we shut down in tearDown
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
