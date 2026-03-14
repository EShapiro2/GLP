/// CSSG GLP Dispatch Integration Test
///
/// Tests that boot.glp's cross-module # dispatch works through the GLP
/// dispatch chain: Distribute → GLP channel → serve → _activate → _select → procedure.
///
/// Previously, # dispatch was broken ("suspends without output") and only
/// boot_direct.glp (direct calls) worked. Phases 1–5 of the dynamic dispatch
/// implementation plan should fix this.

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart' show setPreludeUnitClauseSource;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart' show setPreludeEnvironmentSource;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/glp_activation.dart';
import 'package:glp_runtime/bytecode/runner.dart';

/// Source for the serve/2 system predicate
const serveSource = '''
-mode(system).

procedure serve(Any?, Any?).

serve(Module, [Goal | In]) :-
    ground(Module?) |
    '_activate'(Module?, Goal?),
    serve(Module?, In?).

serve(_, []) :-
    otherwise |
    true.
''';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setPreludeUnitClauseSource(source);
    setPreludeEnvironmentSource(source);
  }

  final cssgRoot = '../programs/cssg_modules';

  // Verify the project directory exists
  if (!Directory(cssgRoot).existsSync()) {
    print('cssg_modules directory not found at $cssgRoot, skipping tests');
    return;
  }

  group('CSSG GLP dispatch', () {
    late GlpCompiler compiler;
    late BytecodeProgram serveBytecode;
    late BytecodeProgram agentBytecode;
    late BytecodeProgram mediatorBytecode;
    late BytecodeProgram actorsBytecode;
    late BytecodeProgram bootBytecode;

    setUpAll(() {
      compiler = GlpCompiler();

      // Compile self.glp (root stdlib) for runtime procedures
      final stdlibBytecode = compiler.compile(
          File('../programs/self.glp').readAsStringSync());

      // Read source files
      final agentSource = File('$cssgRoot/agent.glp').readAsStringSync();
      final mediatorSource =
          File('$cssgRoot/ui/mediator.glp').readAsStringSync();
      final actorsSource =
          File('$cssgRoot/ui/actors.glp').readAsStringSync();
      final bootSource = File('$cssgRoot/boot.glp').readAsStringSync();

      // Compile serve.glp
      serveBytecode = compiler.compile(serveSource);

      // Compile each module separately, then merge with stdlib.
      // Each module needs stdlib procedures available at runtime.
      agentBytecode = compiler.compile(agentSource).merge(stdlibBytecode);
      mediatorBytecode =
          compiler.compile(mediatorSource).merge(stdlibBytecode);
      actorsBytecode = compiler.compile(actorsSource).merge(stdlibBytecode);

      // Compile boot.glp (orchestrator with # dispatch), merge with stdlib
      bootBytecode = compiler.compile(bootSource).merge(stdlibBytecode);
    });

    test('modules compile with _select/1 dispatch tables', () {
      // Each module with exports should have _select/1
      expect(agentBytecode.labels.containsKey('_select/1'), isTrue,
          reason: 'agent.glp should have _select/1 (has exported procedures)');
      expect(mediatorBytecode.labels.containsKey('_select/1'), isTrue,
          reason:
              'mediator.glp should have _select/1 (has exported procedures)');
      expect(actorsBytecode.labels.containsKey('_select/1'), isTrue,
          reason:
              'actors.glp should have _select/1 (has exported procedures)');

      // boot.glp should NOT have _select/1 (no exports)
      expect(bootBytecode.labels.containsKey('_select/1'), isFalse,
          reason: 'boot.glp has no exported procedures');

      // boot.glp should have fplay1/0
      expect(bootBytecode.labels.containsKey('fplay1/0'), isTrue,
          reason: 'boot.glp should have fplay1');
    });

    test('fplay1 via GLP dispatch produces correct output', () {
      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);

      // Activate each module — creates GLP channel, spawns serve
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: agentBytecode,
        moduleName: 'agent',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mediatorBytecode,
        moduleName: 'mediator',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: actorsBytecode,
        moduleName: 'actors',
      );

      // Verify channels registered
      expect(rt.glpChannels.containsKey('agent'), isTrue);
      expect(rt.glpChannels.containsKey('mediator'), isTrue);
      expect(rt.glpChannels.containsKey('actors'), isTrue);

      // Register boot runner
      rt.runners[bootBytecode] = BytecodeRunner(bootBytecode);

      // Create scheduler and drain — all serve loops suspend
      final scheduler = Scheduler(rt: rt);
      var result = scheduler.drainWithStatus(maxCycles: 300);
      expect(result.status, equals(ExecutionStatus.suspended),
          reason: 'All serve loops should suspend waiting for input');

      // Set up fplay1 goal
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, bootBytecode);

      // Import index mapping — determined by order of first # reference
      // in boot.glp code generation:
      //   play1 line 183: actors # alice1(...) → index 1
      //   play1 line 185: agent # agent(...)   → index 2
      //   play1 line 187: mediator # ui_mediator(...) → index 3
      final replCtx = ReplModuleContext(
        moduleName: 'boot',
        imports: {
          1: ReplModuleTarget('actors', actorsBytecode),
          2: ReplModuleTarget('agent', agentBytecode),
          3: ReplModuleTarget('mediator', mediatorBytecode),
        },
      );
      rt.setGoalModuleContext(goalId, replCtx);

      // Enqueue fplay1
      final fplayPc = bootBytecode.labels['fplay1/0']!;
      rt.gq.enqueue(GoalRef(goalId, fplayPc));

      // Drain with high cycle budget — complex concurrent execution
      result = scheduler.drainWithStatus(maxCycles: 50000);

      // Print output for diagnostics
      if (output.isNotEmpty) {
        print('=== fplay1 output (${output.length} lines) ===');
        for (final line in output) {
          print('  $line');
        }
      } else {
        print('=== fplay1 produced no output ===');
        print('Status: ${result.status}');
        print('Goals ran: ${result.goalsRan.length}');
      }

      // Verify the play produced output (the key test — previously this
      // was "suspends without output")
      expect(output, isNotEmpty,
          reason:
              'fplay1 should produce tagged output via _output body kernel');

      // Verify expected tagged output patterns (matching cssg_modules_test.sh)
      final outputStr = output.join('\n');
      expect(outputStr, contains('tagged(alice'),
          reason: 'Output should contain tagged messages for alice');
      expect(outputStr, contains('connected(bob)'),
          reason: 'Alice should get connected(bob)');
      expect(outputStr, contains('connected(alice)'),
          reason: 'Charlie should get connected(alice)');
    });

    /// Helper: run a play procedure through GLP dispatch and return output.
    List<String> runPlay(String playName) {
      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);

      // Activate modules
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: agentBytecode,
        moduleName: 'agent',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mediatorBytecode,
        moduleName: 'mediator',
      );
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: actorsBytecode,
        moduleName: 'actors',
      );

      // Register boot runner
      rt.runners[bootBytecode] = BytecodeRunner(bootBytecode);

      // Drain serve loops
      final scheduler = Scheduler(rt: rt);
      scheduler.drainWithStatus(maxCycles: 300);

      // Set up play goal
      final goalId = rt.nextGoalId++;
      rt.setGoalEnv(goalId, CallEnv(args: {}));
      rt.setGoalProgram(goalId, bootBytecode);
      rt.setGoalModuleContext(
        goalId,
        ReplModuleContext(
          moduleName: 'boot',
          imports: {
            1: ReplModuleTarget('actors', actorsBytecode),
            2: ReplModuleTarget('agent', agentBytecode),
            3: ReplModuleTarget('mediator', mediatorBytecode),
          },
        ),
      );

      final pc = bootBytecode.labels['$playName/0']!;
      rt.gq.enqueue(GoalRef(goalId, pc));

      scheduler.drainWithStatus(maxCycles: 50000);
      return output;
    }

    test('fplay2 via GLP dispatch (Alice accepts, Charlie rejects)', () {
      final output = runPlay('fplay2');
      expect(output, isNotEmpty, reason: 'fplay2 should produce output');
      final s = output.join('\n');
      expect(s, contains('rejected'),
          reason: 'fplay2: rejection should appear');
    });

    test('fplay3 via GLP dispatch (both reject)', () {
      final output = runPlay('fplay3');
      expect(output, isNotEmpty, reason: 'fplay3 should produce output');
    });

    test('fplay4 via GLP dispatch (CSSG all accept)', () {
      final output = runPlay('fplay4');
      expect(output, isNotEmpty, reason: 'fplay4 should produce output');
      final s = output.join('\n');
      expect(s, contains('connected(dave)'),
          reason: 'fplay4: Carol should get connected(dave)');
    });

    test('fplay5 via GLP dispatch (Bob rejects)', () {
      final output = runPlay('fplay5');
      expect(output, isNotEmpty, reason: 'fplay5 should produce output');
    });

    test('fplay6 via GLP dispatch (Carol rejects)', () {
      final output = runPlay('fplay6');
      expect(output, isNotEmpty, reason: 'fplay6 should produce output');
    });

    test('fplay7 via GLP dispatch (Dave rejects)', () {
      final output = runPlay('fplay7');
      expect(output, isNotEmpty, reason: 'fplay7 should produce output');
    });
  });
}
