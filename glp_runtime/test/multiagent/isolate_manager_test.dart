import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

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

procedure agent_init(_?, Channel?).
agent_init(_, _) :- true.
''';

      final loader = BootLoader();
      final config = loader.load(source);

      await manager.boot(config);
      
      // All agents should be ready
      expect(manager.completedAgents, isEmpty); // Not completed yet, just ready
      
      // Start and tick
      manager.start();
      manager.tick();
      
      // Wait briefly for completion
      await Future.delayed(Duration(milliseconds: 100));
      manager.tick();
      await Future.delayed(Duration(milliseconds: 100));
      
      // Agents with trivial goals should complete
      expect(manager.allCompleted, isTrue);
    }, timeout: Timeout(Duration(seconds: 10)));

    test('runs full play with actor scripts (no UI)', () async {
      // Try to find the boot file and shared typed files
      final bootPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/play_madglp_boot.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_madglp_boot.glp',
        'programs/typed_book/social_graph/play_madglp_boot.glp',
      ];
      final agentPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/typed_social_agent.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/typed_social_agent.glp',
        'programs/typed_book/social_graph/typed_social_agent.glp',
      ];
      final actorPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/typed_actors.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/typed_actors.glp',
        'programs/typed_book/social_graph/typed_actors.glp',
      ];

      String? source;
      for (final path in bootPaths) {
        final file = File(path);
        if (file.existsSync()) {
          source = file.readAsStringSync();
          break;
        }
      }

      String? agentSource;
      for (final path in agentPaths) {
        final file = File(path);
        if (file.existsSync()) {
          agentSource = file.readAsStringSync();
          break;
        }
      }

      String? actorSource;
      for (final path in actorPaths) {
        final file = File(path);
        if (file.existsSync()) {
          actorSource = file.readAsStringSync();
          break;
        }
      }

      if (source == null) {
        print('Skipping: play_madglp_boot.glp not found');
        return;
      }

      if (agentSource == null) {
        print('Skipping: typed_social_agent.glp not found');
        return;
      }

      if (actorSource == null) {
        print('Skipping: typed_actors.glp not found');
        return;
      }

      final sharedSource = '$agentSource\n$actorSource';

      final loader = BootLoader();
      final config = loader.load(source);
      config.sharedSource = sharedSource;  // Add shared agent code

      // Should parse correctly with agent_init goal (actors spawned internally)
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
          isTrue);

      await manager.boot(config);

      // Start and tick repeatedly to drive the protocol
      manager.start();

      for (var i = 0; i < 50; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: 50));
        if (manager.allCompleted) break;
      }

      // All agents should complete the full protocol
      expect(manager.allCompleted, isTrue,
          reason: 'Completed agents: ${manager.completedAgents}');
      expect(manager.completedAgents, containsAll(['alice', 'bob', 'charlie']));
    }, timeout: Timeout(Duration(seconds: 30)));

    test('runs full play with UI mediator and UI actors', () async {
      // Try to find the boot file and shared typed files
      final bootPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/play_ui_madglp_boot.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_ui_madglp_boot.glp',
        'programs/typed_book/social_graph/play_ui_madglp_boot.glp',
      ];
      final agentPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/typed_social_agent.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/typed_social_agent.glp',
        'programs/typed_book/social_graph/typed_social_agent.glp',
      ];
      final mediatorPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/typed_ui_mediator.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/typed_ui_mediator.glp',
        'programs/typed_book/social_graph/typed_ui_mediator.glp',
      ];
      final uiActorPaths = [
        '/home/user/GLP/programs/typed_book/social_graph/typed_ui_actors.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/typed_ui_actors.glp',
        'programs/typed_book/social_graph/typed_ui_actors.glp',
      ];

      String? source;
      for (final path in bootPaths) {
        final file = File(path);
        if (file.existsSync()) {
          source = file.readAsStringSync();
          break;
        }
      }

      String? agentSource;
      for (final path in agentPaths) {
        final file = File(path);
        if (file.existsSync()) {
          agentSource = file.readAsStringSync();
          break;
        }
      }

      String? mediatorSource;
      for (final path in mediatorPaths) {
        final file = File(path);
        if (file.existsSync()) {
          mediatorSource = file.readAsStringSync();
          break;
        }
      }

      String? uiActorSource;
      for (final path in uiActorPaths) {
        final file = File(path);
        if (file.existsSync()) {
          uiActorSource = file.readAsStringSync();
          break;
        }
      }

      if (source == null) {
        print('Skipping: play_ui_madglp_boot.glp not found');
        return;
      }

      if (agentSource == null) {
        print('Skipping: typed_social_agent.glp not found');
        return;
      }

      if (mediatorSource == null) {
        print('Skipping: typed_ui_mediator.glp not found');
        return;
      }

      if (uiActorSource == null) {
        print('Skipping: typed_ui_actors.glp not found');
        return;
      }

      final sharedSource = '$agentSource\n$mediatorSource\n$uiActorSource';

      final loader = BootLoader();
      final config = loader.load(source);
      config.sharedSource = sharedSource;

      // Should parse correctly with agent_init goal
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_init'),
          isTrue);

      await manager.boot(config);

      // Start and tick repeatedly to drive the protocol
      manager.start();

      for (var i = 0; i < 50; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: 50));
        if (manager.allCompleted) break;
      }

      // All agents should complete the full protocol
      expect(manager.allCompleted, isTrue,
          reason: 'Completed agents: ${manager.completedAgents}');
      expect(manager.completedAgents, containsAll(['alice', 'bob', 'charlie']));
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
