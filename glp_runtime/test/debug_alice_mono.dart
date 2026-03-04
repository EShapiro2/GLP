/// Diagnostic: Just Alice WITHOUT modules — trace all goal reductions
///
/// Run: dart test/debug_alice_mono.dart
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() async {
  final glpDir = '../programs/typed_book/cssg';
  final stdlibDir = '../programs/stdlib';

  final sources = [
    File('$glpDir/typed_social_agent.glp').readAsStringSync(),
    File('$glpDir/typed_ui_mediator.glp').readAsStringSync(),
    File('$glpDir/typed_ui_actors.glp').readAsStringSync(),
    File('$glpDir/play_ui_madglp_boot.glp').readAsStringSync(),
  ];

  print('=== Alice-only monolithic diagnostic ===\n');

  final alice = AgentRuntime(
    agentId: 'alice',
    glpSources: sources,
    stdlibDir: stdlibDir,
    goalLabel: 'parent_init/4',
    extraArgs: ['carol', '4'],
  );

  alice.onOutput = (line) {
    print('[OUT] $line');
  };
  alice.onLog = (tag, msg) {
    print('[LOG] $msg');
  };
  alice.onSendMadMessage = (to, payload) async {
    print('[MAD] -> $to (${payload.length} bytes)');
  };

  alice.glpTraceEnabled = true;

  print('--- Initializing Alice ---');
  await alice.initialize();

  print('\n=== Done ===');
}
