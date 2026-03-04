/// Diagnostic: Two agents WITHOUT project modules (monolithic sources)
///
/// Simulates what main_cssg_mad.dart does — loads 4 GLP source files directly.
///
/// Run: dart test/debug_two_agents_mono.dart
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() async {
  final glpDir = '../programs/typed_book/cssg';
  final stdlibDir = '../programs/stdlib';

  // Load the 4 source files (same as main_cssg_mad.dart)
  final sources = [
    File('$glpDir/typed_social_agent.glp').readAsStringSync(),
    File('$glpDir/typed_ui_mediator.glp').readAsStringSync(),
    File('$glpDir/typed_ui_actors.glp').readAsStringSync(),
    File('$glpDir/play_ui_madglp_boot.glp').readAsStringSync(),
  ];

  print('=== Two-agent diagnostic MONOLITHIC (Alice + Carol, play 4) ===\n');

  final pendingMessages = <String, List<(String, Uint8List)>>{};
  final outputs = <String, List<String>>{'alice': [], 'carol': []};

  // Create Alice (parent)
  final alice = AgentRuntime(
    agentId: 'alice',
    glpSources: sources,
    stdlibDir: stdlibDir,
    goalLabel: 'parent_init/4',
    extraArgs: ['carol', '4'],
    // NO projectDir
  );

  // Create Carol (child)
  final carol = AgentRuntime(
    agentId: 'carol',
    glpSources: sources,
    stdlibDir: stdlibDir,
    goalLabel: 'child_init/3',
    extraArgs: ['4'],
    // NO projectDir
  );

  // Wire Alice
  alice.onOutput = (line) {
    outputs['alice']!.add(line);
    if (line.contains('tagged(') || line.contains('[GOAL]')) print('[ALICE OUT] $line');
  };
  alice.onLog = (tag, msg) {
    if (msg.contains('INIT:') || msg.contains('RUN:') || msg.contains('MAD') || msg.contains('ERROR')) {
      print('[ALICE LOG] $msg');
    }
  };
  alice.onSendMadMessage = (to, payload) async {
    print('[ALICE MAD] -> $to (${payload.length} bytes)');
    pendingMessages.putIfAbsent(to, () => []).add(('alice', payload));
  };

  // Wire Carol
  carol.onOutput = (line) {
    outputs['carol']!.add(line);
    if (line.contains('tagged(') || line.contains('[GOAL]')) print('[CAROL OUT] $line');
  };
  carol.onLog = (tag, msg) {
    if (msg.contains('INIT:') || msg.contains('RUN:') || msg.contains('MAD') || msg.contains('ERROR')) {
      print('[CAROL LOG] $msg');
    }
  };
  carol.onSendMadMessage = (to, payload) async {
    print('[CAROL MAD] -> $to (${payload.length} bytes)');
    pendingMessages.putIfAbsent(to, () => []).add(('carol', payload));
  };

  carol.glpTraceEnabled = true;

  print('--- Initializing Alice ---');
  await alice.initialize();
  print('Alice initialized. Pending to: ${pendingMessages.keys.toList()}\n');

  print('--- Initializing Carol ---');
  await carol.initialize();
  print('Carol initialized. Pending to: ${pendingMessages.keys.toList()}\n');

  // Route messages
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

  final aliceTagged = outputs['alice']!.where((l) => l.contains('tagged(')).toList();
  final carolTagged = outputs['carol']!.where((l) => l.contains('tagged(')).toList();
  print('Alice tagged: ${aliceTagged.length}');
  for (final l in aliceTagged) print('  $l');
  print('Carol tagged: ${carolTagged.length}');
  for (final l in carolTagged) print('  $l');

  print('\n=== Done ===');
}
