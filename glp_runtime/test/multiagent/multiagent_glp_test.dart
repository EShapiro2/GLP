/// Multi-agent GLP tests via IsolateManager
///
/// These tests convert the old IRMA-era multi-isolate tests into GLP programs
/// using the goal@isolate boot format. Each test loads a .glp file with a boot
/// clause and runs it through the IsolateManager.
///
/// Original IRMA tests: lib/multiagent/archive-irma-2026-01-30/tests/
/// Converted GLP programs: programs/typed_book/multiagent_tests/

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Try to find a file at multiple possible paths.
String? loadFile(String relativePath) {
  final paths = [
    '/Users/udi/Grassroots/GLP/$relativePath',
    '/home/user/GLP/$relativePath',
    relativePath,
  ];
  for (final path in paths) {
    final file = File(path);
    if (file.existsSync()) {
      return file.readAsStringSync();
    }
  }
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

    /// Helper: load, boot, run, and verify all agents complete.
    ///
    /// Trace parameters (off by default for clean test output):
    /// - [traceGlp]: GLP-level trace (reductions, suspensions, failures)
    /// - [traceMad]: MAD infrastructure trace (send, globalize, localize)
    /// - [traceAgents]: Only trace these agents (null = all)
    Future<void> runGlpTest(String glpFile, {
      int maxTicks = 50,
      int tickDelayMs = 50,
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

      final traceConfig = TraceConfig(
        glp: traceGlp,
        mad: traceMad,
        agents: traceAgents,
      );

      await manager.boot(config, traceConfig: traceConfig);
      manager.start();

      for (var i = 0; i < maxTicks; i++) {
        manager.tick();
        await Future.delayed(Duration(milliseconds: tickDelayMs));
        if (manager.allCompleted) break;
      }

      expect(manager.allCompleted, isTrue,
          reason: 'Not all agents completed. Completed: ${manager.completedAgents}');
    }

    test('shared variable: agent1 sends, agent2 receives', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/shared_variable_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('imported reader: one-way list flow', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/imported_reader_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('reversed flow: agent2 sends to agent1', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/reversed_flow_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('coop stream: producer + merge across agents', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/coop_stream_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('two-hop flow: agent1 -> agent2 -> agent1 round-trip', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/two_hop_flow_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('bidirectional exchange: symmetric send/receive', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/bidirectional_exchange_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('three-agent pipeline: produce -> transform -> consume', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2', 'agent3']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('three-agent merge: two producers feed into one merger', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/three_agent_merge_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2', 'merger']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('distribute: one producer broadcasts to two consumers', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/distribute_boot.glp');
      expect(manager.completedAgents, containsAll(['producer', 'consumer1', 'consumer2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('minimal race: send unbound reader', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/minimal_race_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('send reader: send unbound reader, instantiate later', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/send_reader_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('writer response: send writer, receiver writes back', () async {
      await runGlpTest('programs/typed_book/multiagent_tests/writer_response_boot.glp');
      expect(manager.completedAgents, containsAll(['agent1', 'agent2']));
    }, timeout: Timeout(Duration(seconds: 15)));

    test('alice-bob-charlie: cold-call + messaging + friend introduction', () async {
      await runGlpTest('programs/typed_book/social_graph/play_alice_bob_charlie_test_boot.glp',
          maxTicks: 200, tickDelayMs: 50);
      expect(manager.completedAgents, containsAll(['alice', 'bob', 'charlie']));
    }, timeout: Timeout(Duration(seconds: 60)),
    skip: 'Blocked by GLP runtime bug: body = does not bind through variable chains (see eq_channel_test.dart)');
  });
}
