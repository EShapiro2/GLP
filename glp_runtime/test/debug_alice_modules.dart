/// Diagnostic: Just Alice with modules — trace all goal reductions
///
/// Run: dart test/debug_alice_modules.dart
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() async {
  final projectDir = '../programs/cssg_modules';
  final bootSource = File('../programs/cssg_modules/mad_boot.glp').readAsStringSync();
  final stdlibDir = '../programs/stdlib';

  print('=== Alice-only modules diagnostic ===\n');

  final alice = AgentRuntime(
    agentId: 'alice',
    glpSources: [bootSource],
    stdlibDir: stdlibDir,
    goalLabel: 'parent_init/4',
    extraArgs: ['carol', '4'],
    projectDir: projectDir,
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

  // Full trace to see all goal reductions
  alice.glpTraceEnabled = true;

  print('--- Initializing Alice ---');
  await alice.initialize();

  print('\n=== Done ===');
}
