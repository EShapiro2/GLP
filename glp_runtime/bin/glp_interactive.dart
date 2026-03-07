/// GLP Interactive Mode - dGLP-based interactive agent system
///
/// Runs 3 agents (alice, bob, charlie) in a shared-heap single process.
/// Dart injects user commands via InputInjector; output goes to stdout
/// as tagged(agentId, cmd/notify(...)) lines.
///
/// Protocol:
///   Input (stdin):  "alice connect(bob)\n"
///   Output (stdout): "tagged(alice, cmd(connect(bob)))\n"
///                    "tagged(bob, notify(befriend(alice, req(1))))\n"
///                    "__ready__\n"
///
/// The __ready__ marker signals that the engine has finished processing
/// and is waiting for the next command.
library;

import 'dart:io';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

void main(List<String> args) async {
  // Resolve source directory from args or default
  final srcDir = args.isNotEmpty
      ? args[0]
      : '../programs/typed_book/gsn';

  final engine = GlpEngine(stdlibDir: '../programs/stdlib');
  engine.maxCycles = 50000;

  // Set up output callback: print tagged terms to stdout
  engine.runtime.outputCallback = (text) {
    stdout.writeln(text);
  };

  // Load GLP source files
  final files = [
    '$srcDir/typed_social_agent.glp',
    '$srcDir/typed_ui_mediator.glp',
    '$srcDir/typed_ui_actors.glp',      // for tee/network3/send_to_user_tagged
    '$srcDir/play_ui_sim_boot.glp',     // for tee/network3/send_to_user_tagged
    '$srcDir/interactive_boot.glp',
  ];

  for (final path in files) {
    try {
      engine.loadFile(path);
    } catch (e) {
      stderr.writeln('[ERROR] Failed to load $path: $e');
      exit(1);
    }
  }

  // Run the boot goal — sets up agents with open input streams
  final bootResult = await engine.runGoal(
      'interactive_boot(AliceIn, BobIn, CharlieIn)');

  if (bootResult.failed) {
    stderr.writeln('[ERROR] Boot goal failed: ${bootResult.error}');
    exit(1);
  }

  // Get writer addresses for each agent's input stream
  final writers = engine.lastQueryVarWriters;
  final aliceWriter = writers['AliceIn'];
  final bobWriter = writers['BobIn'];
  final charlieWriter = writers['CharlieIn'];

  if (aliceWriter == null || bobWriter == null || charlieWriter == null) {
    stderr.writeln('[ERROR] Could not get stream writers from boot goal');
    stderr.writeln('  Writers: $writers');
    exit(1);
  }

  // Create input injectors
  final injectors = <String, InputInjector>{
    'alice': InputInjector(engine.runtime.heap, 'alice', aliceWriter),
    'bob': InputInjector(engine.runtime.heap, 'bob', bobWriter),
    'charlie': InputInjector(engine.runtime.heap, 'charlie', charlieWriter),
  };

  // Signal ready
  stdout.writeln('__ready__');

  // Interactive loop
  while (true) {
    final line = stdin.readLineSync();
    if (line == null) break; // EOF

    final trimmed = line.trim();
    if (trimmed.isEmpty) {
      stdout.writeln('__ready__');
      continue;
    }
    if (trimmed == ':quit' || trimmed == ':q') break;

    // Parse: "alice connect(bob)" → agent=alice, cmd=connect(bob)
    final spaceIdx = trimmed.indexOf(' ');
    if (spaceIdx < 0) {
      stderr.writeln('[ERROR] Expected: <agent> <command>');
      stdout.writeln('__ready__');
      continue;
    }

    final agentName = trimmed.substring(0, spaceIdx).toLowerCase();
    final cmdStr = trimmed.substring(spaceIdx + 1).trim();

    final injector = injectors[agentName];
    if (injector == null) {
      stderr.writeln('[ERROR] Unknown agent: $agentName (use alice, bob, charlie)');
      stdout.writeln('__ready__');
      continue;
    }

    // Parse the command as a GLP term
    rt.Term term;
    try {
      term = engine.parseTerm(cmdStr);
    } catch (e) {
      stderr.writeln('[ERROR] Could not parse command: $cmdStr ($e)');
      stdout.writeln('__ready__');
      continue;
    }

    // Inject the term into the agent's input stream
    final activations = injector.inject(term);
    for (final goal in activations) {
      engine.runtime.gq.enqueue(goal);
    }

    // Run the engine until quiescent (processes all cascading effects)
    try {
      await engine.runPendingGoals();
    } catch (e) {
      stderr.writeln('[ERROR] Engine error: $e');
    }

    // Signal ready for next command
    stdout.writeln('__ready__');
  }
}
