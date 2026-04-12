/// Diagnostic: Four agents (Alice, Bob, Carol, Dave) with project modules.
/// Simulates what main_cssg_mad_modules.dart does — linked project + mad_boot.
///
/// Run: dart test/debug_four_agents_modules.dart
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() async {
  final projectDir = '../programs/cssg_modules';
  final bootSource = File('../programs/cssg_modules/mad_boot.glp').readAsStringSync();
  final rootSelfGlpPath = File('../programs/self.glp').absolute.path;

  print('=== Four-agent modules diagnostic (Play 4) ===\n');

  final pendingMessages = <String, List<(String, Uint8List)>>{};
  final outputs = <String, List<String>>{
    'alice': [], 'bob': [], 'carol': [], 'dave': [],
  };

  AgentRuntime makeAgent(String id, String goal, List<String> extra) {
    final agent = AgentRuntime(
      agentId: id,
      glpSources: [bootSource],
      rootSelfGlpPath: rootSelfGlpPath,
      goalLabel: goal,
      extraArgs: extra,
      projectDir: projectDir,
    );
    agent.onOutput = (line) {
      outputs[id]!.add(line);
    };
    agent.onLog = (tag, msg) {
      if (msg.contains('RUN:') || msg.contains('ERROR') || msg.contains('SEND_MAD')) {
        print('[$id] $msg');
      }
    };
    agent.onSendMadMessage = (to, payload) async {
      pendingMessages.putIfAbsent(to, () => []).add((id, payload));
    };
    return agent;
  }

  final alice = makeAgent('alice', 'parent_init/4', ['carol', '4']);
  final bob = makeAgent('bob', 'parent_init/4', ['dave', '4']);
  final carol = makeAgent('carol', 'child_init/3', ['4']);
  final dave = makeAgent('dave', 'child_init/3', ['4']);

  final agents = {'alice': alice, 'bob': bob, 'carol': carol, 'dave': dave};

  // Initialize all agents
  for (final entry in agents.entries) {
    print('--- Initializing ${entry.key} ---');
    await entry.value.initialize();
  }

  // Route messages
  print('\n--- Routing messages ---');
  var rounds = 0;
  while (pendingMessages.isNotEmpty && rounds < 30) {
    rounds++;
    final snapshot = Map<String, List<(String, Uint8List)>>.from(pendingMessages);
    pendingMessages.clear();

    for (final entry in snapshot.entries) {
      final dest = entry.key;
      final agent = agents[dest];
      if (agent == null) {
        print('  Round $rounds: Unknown destination: $dest');
        continue;
      }
      for (final (from, payload) in entry.value) {
        print('  Round $rounds: $from -> $dest (${payload.length} bytes)');
        await agent.onMadMessageReceived(from, payload);
      }
    }
  }

  // Summary
  print('\n--- Summary ---');
  print('Rounds: $rounds');

  final taggedRegex = RegExp(r'^< tagged\((\w+), (cmd|notify)\((.+)\)\)$');
  for (final id in ['alice', 'bob', 'carol', 'dave']) {
    final tagged = outputs[id]!.where((l) => l.contains('tagged(')).toList();
    print('\n$id tagged output (${tagged.length}):');
    for (final l in tagged) {
      final m = taggedRegex.firstMatch(l);
      if (m != null) {
        print('  ${m.group(2)}: ${m.group(3)}');
      } else {
        print('  $l');
      }
    }
  }

  print('\n=== Done ===');
}
