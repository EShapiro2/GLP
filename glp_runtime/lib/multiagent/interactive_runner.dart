/// InteractiveRunner — runs dGLP interactive mode via subprocess.
///
/// Spawns glp_interactive.dart as a subprocess, keeps it alive for
/// bidirectional communication:
///   - Sends tagged commands to stdin: "alice connect(bob)\n"
///   - Reads tagged output from stdout: "tagged(alice, cmd(connect(bob)))\n"
///   - Uses "__ready__" marker to know when the engine is quiescent.
///
/// Uses the same dGLP single-process architecture as fplay1-14,
/// so the receive guard works correctly (shared heap, no serialization).
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'repl_play_runner.dart' show PlayOutput;

/// Runs interactive dGLP mode via subprocess.
///
/// Usage:
///   final runner = InteractiveRunner(repoRoot: '/path/to/GLP');
///   runner.onOutput = (output) { /* route to UI panel */ };
///   await runner.start();
///   runner.sendCommand('alice', 'connect(bob)');
///   runner.stop();
class InteractiveRunner {
  final String repoRoot;

  /// Called for each parsed tagged output line.
  void Function(PlayOutput output)? onOutput;

  /// Called for non-tagged output (logs, etc.).
  void Function(String line)? onLog;

  /// Called for stderr lines.
  void Function(String error)? onError;

  /// Called when the subprocess exits.
  void Function(int exitCode)? onDone;

  /// Called when the engine is ready for the next command.
  void Function()? onReady;

  Process? _process;
  bool _ready = false;

  /// Regex for parsing tagged output lines.
  static final _taggedRegex =
      RegExp(r'^tagged\((\w+), (cmd|notify)\((.+)\)\)$');

  InteractiveRunner({required this.repoRoot});

  bool get isRunning => _process != null;
  bool get isReady => _ready;

  /// Start the interactive subprocess.
  Future<void> start() async {
    final runtimeDir = '$repoRoot/glp_runtime';
    final dartExe = _findDart();

    final interactiveScript = '$runtimeDir/bin/glp_interactive.dart';
    if (!File(interactiveScript).existsSync()) {
      onError?.call('Interactive script not found: $interactiveScript');
      return;
    }

    try {
      final process = await Process.start(
        dartExe,
        ['run', 'bin/glp_interactive.dart', '../programs/typed_book/gsn'],
        workingDirectory: runtimeDir,
      );
      _process = process;
      onLog?.call('Interactive: process started (pid=${process.pid})');

      // Parse stdout continuously
      process.stdout
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen(_parseLine);

      // Log stderr
      process.stderr
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen((line) {
        onError?.call('Interactive stderr: $line');
      });

      // Handle exit
      process.exitCode.then((exitCode) {
        _process = null;
        _ready = false;
        onLog?.call('Interactive: exited with code $exitCode');
        onDone?.call(exitCode);
      });
    } catch (e) {
      _process = null;
      onError?.call('Interactive: failed to start: $e');
    }
  }

  /// Send a command to a specific agent.
  ///
  /// [agentName] is "alice", "bob", or "charlie".
  /// [command] is the GLP term, e.g., "connect(bob)".
  void sendCommand(String agentName, String command) {
    if (_process == null) {
      onError?.call('Interactive: not running');
      return;
    }
    _ready = false;
    _process!.stdin.writeln('$agentName $command');
  }

  /// Stop the interactive subprocess.
  void stop() {
    if (_process != null) {
      _process!.stdin.writeln(':quit');
      _process!.kill();
      _process = null;
      _ready = false;
    }
  }

  /// Parse a single stdout line.
  void _parseLine(String line) {
    if (line == '__ready__') {
      _ready = true;
      onReady?.call();
      return;
    }

    final match = _taggedRegex.firstMatch(line);
    if (match == null) {
      onLog?.call('Interactive: $line');
      return;
    }

    final agentId = match.group(1)!;
    final kind = match.group(2)!;
    final content = match.group(3)!;
    onOutput?.call(PlayOutput(agentId, kind, content));
  }

  /// Find the dart executable.
  String _findDart() {
    final candidates = [
      '/usr/local/bin/dart',
      '${Platform.environment['HOME']}/flutter/bin/dart',
      '${Platform.environment['HOME']}/development/flutter/bin/dart',
      '${Platform.environment['HOME']}/.pub-cache/bin/dart',
    ];
    for (final path in candidates) {
      if (File(path).existsSync()) return path;
    }
    return 'dart';
  }
}
