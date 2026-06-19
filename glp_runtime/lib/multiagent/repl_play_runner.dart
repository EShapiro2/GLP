/// ReplPlayRunner — runs simulated dGLP plays via REPL subprocess.
///
/// Spawns the REPL as a subprocess, pipes load commands and a play goal,
/// parses tagged output lines, and delivers them via callbacks.
///
/// Tagged output format from GLP: tagged(alice, cmd(connect(bob)))
/// Parsed into: agentId="alice", kind="cmd", content="connect(bob)"
///
/// Narrative kinds (play 12): friend, say, act, event
/// e.g. tagged(alice, friend(bob)) → agentId="alice", kind="friend", content="bob"
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

/// Parsed output line from a simulated play.
class PlayOutput {
  final String agentId; // e.g. "alice"
  final String kind; // "cmd", "notify", "friend", "say", "act", or "event"
  final String content; // e.g. "connect(bob)"

  PlayOutput(this.agentId, this.kind, this.content);
}

/// Runs simulated dGLP plays via a REPL subprocess.
///
/// Usage:
///   final runner = ReplPlayRunner(repoRoot: '/Users/udi/Grassroots/GLP');
///   runner.onOutput = (output) { /* route to UI panel */ };
///   runner.onLog = (line) { /* trace log */ };
///   runner.onError = (error) { /* display error */ };
///   runner.onDone = (exitCode) { /* play finished */ };
///   await runner.run(1); // runs fplay1
///   runner.kill(); // abort if needed
class ReplPlayRunner {
  final String repoRoot;

  /// Called for each parsed tagged output line.
  void Function(PlayOutput output)? onOutput;

  /// Called for non-tagged REPL output (banner, load messages, etc.).
  void Function(String line)? onLog;

  /// Called for stderr lines and exceptions.
  void Function(String error)? onError;

  /// Called when the REPL process exits.
  void Function(int exitCode)? onDone;

  Process? _process;

  /// Which GLP files to load (relative to glp_runtime/).
  final List<String> glpFiles;

  /// CSSG GLP files (child-safe social graph, plays 1–7).
  static const cssgFiles = [
    '../programs/book/cssg/typed_social_agent.glp',
    '../programs/book/cssg/typed_ui_mediator.glp',
    '../programs/book/cssg/typed_ui_actors.glp',
    '../programs/book/cssg/play_ui_sim_boot.glp',
  ];

  /// Bonds GLP files (grassroots bonds, plays 1–11).
  static const bondsFiles = [
    '../programs/book/bonds/agent.glp',
    '../programs/book/bonds/mediator.glp',
    '../programs/book/bonds/actors.glp',
    '../programs/book/bonds/boot.glp',
  ];

  /// Bonds GLP files for play 12 (adds play12 actor files).
  static const bondsPlay12Files = [
    ...bondsFiles,
    '../programs/book/bonds/play12/alice.glp',
    '../programs/book/bonds/play12/bob.glp',
    '../programs/book/bonds/play12/charlie.glp',
    '../programs/book/bonds/play12/diana.glp',
    '../programs/book/bonds/play12/eve.glp',
    '../programs/book/bonds/play12/frank.glp',
  ];

  /// CSSN GLP files (child-safe social networking, plays 1–10).
  static const cssnFiles = [
    '../programs/book/cssn/typed_social_agent.glp',
    '../programs/book/cssn/typed_ui_mediator.glp',
    '../programs/book/cssn/typed_ui_actors.glp',
    '../programs/book/cssn/play_ui_sim_boot.glp',
  ];

  /// CSSN v2 modules project (village scenario, fplay13).
  /// Loaded as a project directory — the REPL treats the path as a project root.
  static const cssnVillageFiles = [
    '../programs/cssn',
  ];

  /// Regex for parsing tagged output lines.
  /// Matches cmd, notify (protocol), and friend, say, act, event (narrative).
  static final _taggedRegex =
      RegExp(r'^tagged\((\w+), (cmd|notify|friend|say|act|event)\((.+)\)\)$');

  ReplPlayRunner({required this.repoRoot, this.glpFiles = cssgFiles});

  bool get isRunning => _process != null;

  /// Run a simulated play (1, 2, or 3).
  Future<void> run(int playNumber) async {
    final runtimeDir = '$repoRoot/glp_runtime';
    final dartExe = _findDart();

    onLog?.call('REPL: repoRoot=$repoRoot');
    onLog?.call('REPL: runtimeDir=$runtimeDir');
    onLog?.call('REPL: dart=$dartExe');

    // Verify paths before spawning
    if (!Directory(runtimeDir).existsSync()) {
      onError?.call('Directory not found: $runtimeDir');
      return;
    }
    final replScript = '$runtimeDir/bin/glp_repl.dart';
    if (!File(replScript).existsSync()) {
      onError?.call('REPL script not found: $replScript');
      return;
    }

    try {
      final process = await Process.start(
        dartExe,
        ['run', 'bin/glp_repl.dart'],
        workingDirectory: runtimeDir,
      );
      _process = process;
      onLog?.call('REPL: process started (pid=${process.pid})');

      // Feed load commands + play goal + quit
      final commands = [
        for (final f in glpFiles) f,
        'fplay$playNumber.',
        ':quit',
      ].join('\n');
      process.stdin.writeln(commands);
      await process.stdin.close();

      // Parse stdout
      process.stdout
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen(_parseLine);

      // Log stderr
      process.stderr
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen((line) {
        onError?.call('REPL stderr: $line');
      });

      // Wait for exit
      final exitCode = await process.exitCode;
      _process = null;
      onLog?.call('REPL: exited with code $exitCode');
      onDone?.call(exitCode);
    } catch (e) {
      _process = null;
      onError?.call('REPL: failed to start: $e');
    }
  }

  /// Kill the REPL subprocess if running.
  void kill() {
    _process?.kill();
    _process = null;
  }

  /// Parse a single stdout line from the REPL.
  /// The REPL may prefix the first output line with "GLP> ", so strip it.
  void _parseLine(String line) {
    final stripped = line.startsWith('GLP> ') ? line.substring(5) : line;
    final match = _taggedRegex.firstMatch(stripped);
    if (match == null) {
      onLog?.call('REPL: $line');
      return;
    }

    final agentId = match.group(1)!;
    final kind = match.group(2)!;
    final content = match.group(3)!;
    onOutput?.call(PlayOutput(agentId, kind, content));
  }

  /// Find the dart executable. Prefer the one next to the Flutter SDK,
  /// fall back to PATH.
  String _findDart() {
    // Check common macOS Flutter/Dart locations
    final candidates = [
      '/usr/local/bin/dart',
      '${Platform.environment['HOME']}/flutter/bin/dart',
      '${Platform.environment['HOME']}/development/flutter/bin/dart',
      '${Platform.environment['HOME']}/.pub-cache/bin/dart',
    ];
    for (final path in candidates) {
      if (File(path).existsSync()) return path;
    }
    // Fall back to PATH (works from terminal, may not from app bundle)
    return 'dart';
  }
}
