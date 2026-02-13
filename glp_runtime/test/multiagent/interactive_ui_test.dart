/// Test that reproduces the Flutter interactive UI path without Flutter.
///
/// Uses AgentRuntime (same as Flutter) with InputInjector for user input.
/// Simulates the interactive protocol: Alice connect(bob), Bob decision(yes,...).
import 'dart:typed_data';
import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() {
  group('Interactive UI (AgentRuntime)', () {
    test('Alice connect(bob), Bob decision — reproduces Flutter path', () async {
      // Load GLP source (same files as Flutter app)
      final glpDir = '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph';
      final files = [
        'typed_social_agent.glp',
        'typed_ui_mediator.glp',
        'play_ui_boot.glp',
      ];

      final sources = <String>[];
      for (final f in files) {
        final file = File('$glpDir/$f');
        if (!file.existsSync()) {
          print('Skipping: $f not found');
          return;
        }
        sources.add(file.readAsStringSync());
      }
      final glpSource = sources.join('\n');

      // Pending messages: from -> (to, payload)
      final pendingMessages = <(String, String, Uint8List)>[];

      // Create agents (same as Flutter's AgentRuntime usage)
      final alice = AgentRuntime(agentId: 'alice', glpSource: glpSource);
      final bob = AgentRuntime(agentId: 'bob', glpSource: glpSource);

      final aliceOutput = <String>[];
      final bobOutput = <String>[];

      alice.onOutput = (line) {
        print('[ALICE OUT] $line');
        aliceOutput.add(line);
      };
      alice.onLog = (tag, msg) => print('[ALICE LOG] $msg');
      alice.onSendMadMessage = (to, payload) async {
        print('[ALICE SEND] -> $to (${payload.length} bytes)');
        pendingMessages.add(('alice', to, payload));
      };

      bob.onOutput = (line) {
        print('[BOB OUT] $line');
        bobOutput.add(line);
      };
      bob.onLog = (tag, msg) => print('[BOB LOG] $msg');
      bob.onSendMadMessage = (to, payload) async {
        print('[BOB SEND] -> $to (${payload.length} bytes)');
        pendingMessages.add(('bob', to, payload));
      };

      // Initialize both
      print('=== INIT ALICE ===');
      await alice.initialize();
      print('=== INIT BOB ===');
      await bob.initialize();

      // Deliver any pending messages
      Future<void> deliverPending() async {
        while (pendingMessages.isNotEmpty) {
          final msgs = List.of(pendingMessages);
          pendingMessages.clear();
          for (final (from, to, payload) in msgs) {
            print('=== DELIVER $from -> $to ===');
            if (to == 'alice') {
              await alice.onMadMessageReceived(from, payload);
            } else if (to == 'bob') {
              await bob.onMadMessageReceived(from, payload);
            }
          }
        }
      }

      // Step 1: Alice types connect(bob)
      print('\n=== STEP 1: Alice connect(bob) ===');
      await alice.injectUserInput('connect(bob)');
      await deliverPending();

      // Bob should show befriend(alice, req(1))
      print('\nBob output so far: $bobOutput');
      expect(bobOutput.any((l) => l.contains('befriend(alice')),
          isTrue, reason: 'Bob should show befriend(alice, req(1))');

      // Step 2: Bob types decision(yes, alice, req(1))
      print('\n=== STEP 2: Bob decision(yes, alice, req(1)) ===');
      await bob.injectUserInput('decision(yes, alice, req(1))');
      await deliverPending();

      // Both should show connected(...)
      print('\nAlice output: $aliceOutput');
      print('Bob output: $bobOutput');
      expect(bobOutput.any((l) => l.contains('connected(')),
          isTrue, reason: 'Bob should show connected(...)');

      alice.dispose();
      bob.dispose();
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
