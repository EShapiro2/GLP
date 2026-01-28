import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Try to load a boot file from known paths
String? loadBootFile(String filename) {
  final paths = [
    '/home/user/GLP/programs/typed_book/social_graph/$filename',
    '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/$filename',
    'programs/typed_book/social_graph/$filename',
  ];
  for (final path in paths) {
    final file = File(path);
    if (file.existsSync()) {
      return file.readAsStringSync();
    }
  }
  return null;
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
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie.

procedure agent_init(_?, Channel?, Channel?).
agent_init(_, _, _) :- true.
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
        '/home/user/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_boot.glp',
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

      // Agents should be ready and will complete (agent_init completes immediately)
      manager.start();
      manager.tick();

      await Future.delayed(Duration(milliseconds: 200));
      manager.tick();
      await Future.delayed(Duration(milliseconds: 200));

      // Agents complete (agent_init doesn't suspend waiting for UI)
      expect(manager.allCompleted, isTrue);
      expect(manager.completedAgents, containsAll(['alice', 'bob', 'charlie']));
    }, timeout: Timeout(Duration(seconds: 10)));

    test('ping-pong: minimal network message exchange', () async {
      // Try to find the ping-pong test boot file
      final paths = [
        '/home/user/GLP/programs/typed_book/social_graph/ping_pong_test_boot.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/ping_pong_test_boot.glp',
        'programs/typed_book/social_graph/ping_pong_test_boot.glp',
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
        print('Skipping: ping_pong_test_boot.glp not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);

      // Should have 2 agents: alice (ping) and bob (pong)
      expect(config.directives.length, equals(2));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob']));

      await manager.boot(config);
      manager.start();

      // Protocol:
      // 1. Alice sends ping(alice) to bob
      // 2. Bob receives ping, sends pong to alice
      // 3. Alice receives pong, completes
      // 4. Bob completes
      //
      // This requires IRMA to actually route messages!
      for (var i = 0; i < 20; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: 100));
        if (manager.allCompleted) {
          print('Ping-pong completed after ${i + 1} ticks');
          break;
        }
      }

      expect(manager.allCompleted, isTrue,
          reason: 'Completed agents: ${manager.completedAgents}');
      expect(manager.completedAgents, containsAll(['alice', 'bob']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('cold-call: shared response variable across isolates', () async {
      final source = loadBootFile('cold_call_test_boot.glp');
      if (source == null) {
        print('Skipping: cold_call_test_boot.glp not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);

      // Should have 2 agents: alice (initiator) and bob (responder)
      expect(config.directives.length, equals(2));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob']));

      await manager.boot(config);
      manager.start();

      // Protocol:
      // 1. Alice sends ping(Resp) to bob (Resp is fresh writer, exported)
      // 2. Bob receives ping(V), matches ping(pong) → binds V to pong
      // 3. IRMA routes assignment back to Alice
      // 4. Alice's Resp? becomes pong → completes
      for (var i = 0; i < 20; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: 100));
        if (manager.allCompleted) {
          print('Cold-call completed after ${i + 1} ticks');
          break;
        }
      }

      expect(manager.allCompleted, isTrue,
          reason: 'Completed agents: ${manager.completedAgents}');
      expect(manager.completedAgents, containsAll(['alice', 'bob']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('friend-introduction: 3-agent shared channel routing', () async {
      final source = loadBootFile('friend_intro_test_boot.glp');
      if (source == null) {
        print('Skipping: friend_intro_test_boot.glp not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);

      // Should have 3 agents: alice, bob (introducer), charlie
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));

      await manager.boot(config);
      manager.start();

      // Protocol:
      // 1. Bob creates channel pair, sends endpoints to Alice and Charlie
      // 2. Alice writes hello_charlie on her write channel
      // 3. IRMA routes: Alice → Bob (creator) → Charlie
      // 4. Charlie reads hello_charlie, writes hello_alice
      // 5. IRMA routes: Charlie → Bob (creator) → Alice
      // 6. Alice reads hello_alice → all complete
      for (var i = 0; i < 30; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: 100));
        if (manager.allCompleted) {
          print('Friend-introduction completed after ${i + 1} ticks');
          break;
        }
      }

      expect(manager.allCompleted, isTrue,
          reason: 'Completed agents: ${manager.completedAgents}');
      expect(manager.completedAgents, containsAll(['alice', 'bob', 'charlie']));
    }, timeout: Timeout(Duration(seconds: 20)));

    test('runs full play with actor scripts (no UI)', () async {
      // Try to find the test boot file with actors
      final paths = [
        '/home/user/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_test_boot.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_test_boot.glp',
        'programs/typed_book/social_graph/play_alice_bob_charlie_test_boot.glp',
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
        print('Skipping: play_alice_bob_charlie_test_boot.glp not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);

      // Should parse correctly with agent_with_actor goal
      expect(config.directives.length, equals(3));
      expect(config.directives.map((d) => d.agentId).toList(),
          equals(['alice', 'bob', 'charlie']));
      expect(config.directives.every((d) => d.goalFunctor == 'agent_with_actor'),
          isTrue);

      await manager.boot(config);

      // Start and tick repeatedly to drive the protocol
      manager.start();

      // The full protocol requires multiple message exchanges:
      // 1. Alice cold-calls Bob (Bob accepts)
      // 2. Alice sends message to Bob
      // 3. Bob cold-calls Charlie (Charlie accepts)
      // 4. Charlie sends message to Bob
      // 5. Bob introduces Alice to Charlie (both accept)
      // 6. Alice sends message to Charlie
      // 7. Charlie responds to Alice
      //
      // Each exchange needs ticks to process
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
