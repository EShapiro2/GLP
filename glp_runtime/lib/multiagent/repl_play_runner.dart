/// ReplPlayRunner — runs simulated dGLP plays via REPL subprocess.
///
/// Spawns the REPL as a subprocess, pipes load commands and a play goal,
/// parses tagged output lines, and delivers them via callbacks.
///
/// Tagged output format from GLP: tagged(alice, cmd(connect(bob)))
/// Parsed into: agentId="alice", kind="cmd", content="connect(bob)"
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

/// Parsed output line from a simulated play.
class PlayOutput {
  final String agentId; // e.g. "alice"
  final String kind; // "cmd" or "notify"
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

  /// CSSG GLP files to load (relative to glp_runtime/).
  static const _cssgFiles = [
    '../programs/typed_book/cssg/typed_social_agent.glp',
    '../programs/typed_book/cssg/typed_ui_mediator.glp',
    '../programs/typed_book/cssg/typed_ui_actors.glp',
    '../programs/typed_book/cssg/play_ui_sim_boot.glp',
  ];

  /// Regex for parsing tagged output lines.
  static final _taggedRegex =
      RegExp(r'^tagged\((\w+), (cmd|notify)\((.+)\)\)$');

  ReplPlayRunner({required this.repoRoot});

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
        for (final f in _cssgFiles) f,
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
