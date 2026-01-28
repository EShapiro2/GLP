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
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie.

procedure agent_init(_?, Channel?, Channel?).
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    true | true.
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

    test('boots from actual play_alice_bob_charlie_boot.glp', () async {
      // Try to find the actual file
      final paths = [
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_boot.glp',
        'programs/typed_book/social_graph/play_alice_bob_charlie_boot.glp',
      ];
      
      String? source;
      for (final path in paths) {
        final file = File(path);
        if (file.existsSync()) {
          source = file.readAsStringSync();
          break;
        }
      }
      
      if (source == null) {
        print('Skipping: play_alice_bob_charlie_boot.glp not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);

      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));

      await manager.boot(config);
      
      // Agents should be ready (but will suspend waiting for input)
      manager.start();
      manager.tick();
      
      await Future.delayed(Duration(milliseconds: 200));
      
      // Agents won't complete without UI input, but they should be running
      expect(manager.allCompleted, isFalse);
    }, timeout: Timeout(Duration(seconds: 10)));
  });
}
