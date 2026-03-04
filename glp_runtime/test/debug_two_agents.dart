/// Diagnostic: Two agents (Alice parent, Carol child) with message routing.
///
/// Run: dart test/debug_two_agents.dart
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() async {
  final projectDir = '../programs/cssg_modules';
  final bootSource = File('../programs/cssg_modules/mad_boot.glp').readAsStringSync();
  final stdlibDir = '../programs/stdlib';

  print('=== Two-agent diagnostic (Alice + Carol, play 4) ===\n');

  // Pending messages: destination → list of (from, payload)
  final pendingMessages = <String, List<(String, Uint8List)>>{};

  // Output lines per agent
  final outputs = <String, List<String>>{'alice': [], 'carol': []};

  // Create Alice (parent)
  final alice = AgentRuntime(
    agentId: 'alice',
    glpSources: [bootSource],
    stdlibDir: stdlibDir,
    goalLabel: 'parent_init/4',
    extraArgs: ['carol', '4'],
    projectDir: projectDir,
  );

  // Create Carol (child)
  final carol = AgentRuntime(
    agentId: 'carol',
    glpSources: [bootSource],
    stdlibDir: stdlibDir,
    goalLabel: 'child_init/3',
    extraArgs: ['4'],
    projectDir: projectDir,
  );

  // Wire Alice callbacks
  alice.onOutput = (line) {
    outputs['alice']!.add(line);
    print('[ALICE OUT] $line');
  };
  alice.onLog = (tag, msg) {
    print('[ALICE LOG] $msg');
  };
  alice.onSendMadMessage = (to, payload) async {
    print('[ALICE MAD] -> $to (${payload.length} bytes)');
    pendingMessages.putIfAbsent(to, () => []).add(('alice', payload));
  };

  // Wire Carol callbacks
  carol.onOutput = (line) {
    outputs['carol']!.add(line);
    print('[CAROL OUT] $line');
  };
  carol.onLog = (tag, msg) {
    print('[CAROL LOG] $msg');
  };
  carol.onSendMadMessage = (to, payload) async {
    print('[CAROL MAD] -> $to (${payload.length} bytes)');
    pendingMessages.putIfAbsent(to, () => []).add(('carol', payload));
  };

  // Enable GLP trace for Carol to see exact unification
  carol.glpTraceEnabled = true;

  // Phase 1: Initialize both agents
  print('--- Initializing Alice ---');
  await alice.initialize();
  print('Alice initialized. Pending to: ${pendingMessages.keys.toList()}\n');

  print('--- Initializing Carol ---');
  await carol.initialize();
  print('Carol initialized. Pending to: ${pendingMessages.keys.toList()}\n');

  // Phase 2: Route pending messages
  print('--- Routing messages ---');
  var rounds = 0;
  while (pendingMessages.isNotEmpty && rounds < 20) {
    rounds++;
    final snapshot = Map<String, List<(String, Uint8List)>>.from(pendingMessages);
    pendingMessages.clear();

    for (final entry in snapshot.entries) {
      final dest = entry.key;
      for (final (from, payload) in entry.value) {
        print('\n  Round $rounds: $from -> $dest (${payload.length} bytes)');
        if (dest == 'alice') {
          await alice.onMadMessageReceived(from, payload);
        } else if (dest == 'carol') {
          await carol.onMadMessageReceived(from, payload);
        } else {
          print('  Unknown destination: $dest');
        }
      }
    }
  }

  print('\n--- Summary ---');
  print('Rounds: $rounds');

  // Check for tagged output
  final aliceTagged = outputs['alice']!.where((l) => l.contains('tagged(')).toList();
  final carolTagged = outputs['carol']!.where((l) => l.contains('tagged(')).toList();
  print('Alice tagged: ${aliceTagged.length}');
  print('Carol tagged: ${carolTagged.length}');

  print('\n=== Done ===');
}
