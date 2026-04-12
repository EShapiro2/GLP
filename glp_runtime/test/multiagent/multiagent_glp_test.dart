/// Multi-agent GLP tests via IsolateManager
///
/// These tests convert the old IRMA-era multi-isolate tests into GLP programs
/// using the goal@isolate boot format. Each test loads a .glp file with a boot
/// clause and runs it through the IsolateManager.
///
/// Execution is event-driven: boot, start, wait for protocol to settle, shutdown.
///
/// Original IRMA tests: lib/multiagent/archive-irma-2026-01-30/tests/
/// Converted GLP programs: programs/typed_book/multiagent_tests/

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Load a GLP file using repo-relative path (from glp_runtime/).
String? loadFile(String relativePath) {
  final file = File('../$relativePath');
  if (file.existsSync()) {
    return file.readAsStringSync();
  }
  print('Skipping: $relativePath not found at ${file.path}');
  return null;
}

void main() {
  group('Multi-agent GLP tests', () {
    late IsolateManager manager;

    setUp(() {
      manager = IsolateManager();
    });

    tearDown(() async {
      await manager.shutdown();
    });

    /// Helper: load, boot, start, and let event-driven execution run.
    ///
    /// Trace parameters (off by default for clean test output):
    /// - [traceGlp]: GLP-level trace (reductions, suspensions, failures)
    /// - [traceMad]: MAD infrastructure trace (send, globalize, localize)
    /// - [traceAgents]: Only trace these agents (null = all)
    Future<void> runGlpTest(String glpFile, {
      int settleMs = 2000,
      bool traceGlp = false,
      bool traceMad = false,
      Set<String>? traceAgents,
    }) async {
      final source = loadFile(glpFile);
      if (source == null) {
        print('Skipping: $glpFile not found');
        return;
      }

      final loader = BootLoader();
      final config = loader.load(source);
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;

      final traceConfig = TraceConfig(
        glp: traceGlp,
        mad: traceMad,
        agents: traceAgents,
      );

      await manager.boot(config, traceConfig: traceConfig);
      manager.start();

      // Let event-driven execution settle
      await Future.delayed(Duration(milliseconds: settleMs));

      // Termination is external — shutdown happens in tearDown
    }

    test('shared variable: agent1 sends, agent2 receives', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/shared_variable_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('imported reader: one-way list flow', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/imported_reader_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('reversed flow: agent2 sends to agent1', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/reversed_flow_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('coop stream: producer + merge across agents', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/coop_stream_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('two-hop flow: agent1 -> agent2 -> agent1 round-trip', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/two_hop_flow_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('bidirectional exchange: symmetric send/receive', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/bidirectional_exchange_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('three-agent pipeline: produce -> transform -> consume', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('three-agent merge: two producers feed into one merger', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/three_agent_merge_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('distribute: one producer broadcasts to two consumers', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/distribute_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('minimal race: send unbound reader', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/minimal_race_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('send reader: send unbound reader, instantiate later', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/send_reader_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

    test('writer response: send writer, receiver writes back', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/writer_response_boot.glp');
    }, timeout: Timeout(Duration(seconds: 15)));

  });
}
