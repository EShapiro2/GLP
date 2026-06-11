/// Project linker tests: static linking of multi-module GLP projects.
///
/// Tests discovery, type checking, renaming, call resolution, and
/// end-to-end compilation of the social/child_safe project. Module-local
/// name-collision handling is covered separately by the dedicated
/// test/programs/linker_collision fixture (child_safe no longer has
/// module-local duplicate procedures — merge/3 was lifted to root self.glp).
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/project_linker.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart' show setRootScopeUnitClauseSource;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart' show setRootScopeEnvironmentSource;
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setRootScopeUnitClauseSource(source);
    setRootScopeEnvironmentSource(source);
  }
  final cssgRoot = '../programs/social/child_safe';
  // Dedicated minimal fixture: sole coverage of module-local name-collision
  // handling (two sibling modules each defining dup/1).
  final collisionRoot = 'test/programs/linker_collision';
  final rootSelfPath = rootSelfGlp.existsSync() ? rootSelfGlp.absolute.path : null;

  if (!Directory(cssgRoot).existsSync()) {
    print('child_safe directory not found at $cssgRoot, skipping tests');
    return;
  }

  group('Project discovery', () {
    test('discovers all modules in child_safe', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);

      // Should find 6 modules: agent, child_agent, mediator, actors, boot, cssg
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, contains('agent'));
      expect(names, contains('child_agent'));
      expect(names, contains('mediator'));
      expect(names, contains('actors'));
      expect(names, contains('boot'));
      expect(names, contains('cssg'));
      expect(names.length, equals(6));
    });

    test('excludes self.glp from modules', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, isNot(contains('self')));
    });

    test('excludes boot_direct.glp from modules', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      final filenames = modules.map((m) => m.filePath).toList();
      expect(filenames.any((f) => f.contains('boot_direct')), isFalse,
          reason: 'boot_direct.glp should be excluded');
    });

    test('modules have correct ancestor scopes', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);

      // All modules should have ancestor scope with self.glp types
      for (final mod in modules) {
        expect(mod.ancestorScope, isNotNull,
            reason: '${mod.moduleName} should have an ancestor scope');
      }
    });
  });

  group('Type checking', () {
    test('all modules type-check successfully', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      // Should not throw
      expect(() => typeCheckProject(modules), returnsNormally);
    });
  });

  group('Linking', () {
    late List<DiscoveredModule> modules;
    late LinkResult linkResult;
    late Program linked;

    setUp(() {
      modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      linkResult = linkProject(modules, 'boot');
      linked = linkResult.program;
    });

    test('procedures are renamed with module prefix', () {
      final procNames = linked.procedures.map((p) => p.name).toSet();

      // agent.glp procedures should have agent: prefix
      expect(procNames, contains('agent:agent'));

      // boot.glp procedures should have boot: prefix
      expect(procNames, contains('boot:tee'));
      expect(procNames, contains('boot:play1'));
      expect(procNames, contains('boot:fplay1'));

      // mediator.glp procedures should have mediator: prefix
      expect(procNames, contains('mediator:ui_mediator'));

      // actors.glp procedures should have actors: prefix
      expect(procNames, contains('actors:alice1'));
      expect(procNames, contains('actors:bob1'));
    });

    test('cross-module calls are resolved', () {
      // Find boot:play1 and check that its body has agent:agent, not # dispatch
      final bootPlay1 = linked.procedures
          .firstWhere((p) => p.name == 'boot:play1');

      // Collect all goal functors in the body
      final bodyFunctors = <String>{};
      for (final clause in bootPlay1.clauses) {
        if (clause.body != null) {
          for (final goal in clause.body!) {
            bodyFunctors.add(goal.functor);
          }
        }
      }

      // Cross-module calls should be resolved
      expect(bodyFunctors, contains('actors:alice1'),
          reason: 'actors # alice1 should become actors:alice1');
      expect(bodyFunctors, contains('agent:agent'),
          reason: 'agent # agent should become agent:agent');
      expect(bodyFunctors, contains('mediator:ui_mediator'),
          reason: 'mediator # ui_mediator should become mediator:ui_mediator');

      // No # dispatch should remain
      expect(bodyFunctors, isNot(contains('#')),
          reason: 'No RemoteGoal # dispatch should remain');
    });

    test('local calls are resolved', () {
      // boot:play1 calls tee, which is local to boot — should become boot:tee
      final bootPlay1 = linked.procedures
          .firstWhere((p) => p.name == 'boot:play1');

      final bodyFunctors = <String>{};
      for (final clause in bootPlay1.clauses) {
        if (clause.body != null) {
          for (final goal in clause.body!) {
            bodyFunctors.add(goal.functor);
          }
        }
      }

      expect(bodyFunctors, contains('boot:tee'),
          reason: 'Local tee call should become boot:tee');
      expect(bodyFunctors, contains('boot:sink'),
          reason: 'Local sink call should become boot:sink');
      expect(bodyFunctors, contains('boot:network3'),
          reason: 'Local network3 call should become boot:network3');
    });

    test('prelude calls are preserved unprefixed', () {
      // send_to_user_tagged calls '_output' which is a body kernel — should stay unprefixed
      final sendTagged = linked.procedures
          .firstWhere((p) => p.name == 'boot:send_to_user_tagged');

      final bodyFunctors = <String>{};
      for (final clause in sendTagged.clauses) {
        if (clause.body != null) {
          for (final goal in clause.body!) {
            bodyFunctors.add(goal.functor);
          }
        }
      }

      // _output is parsed with quotes stripped — functor is just _output
      expect(bodyFunctors, contains('_output'),
          reason: '_output body kernel should remain unprefixed');
    });

    test('entry point aliases exist for top module', () {
      // boot is the top module — all its procedures should have unprefixed aliases
      final bootModule = modules.firstWhere((m) => m.moduleName == 'boot');
      final bootProcNames = bootModule.ast.procedures.map((p) => p.name).toSet();

      // Check that aliases exist
      for (final name in bootProcNames) {
        final aliases = linked.procedures
            .where((p) => p.name == name && !p.name.contains(':'))
            .toList();
        expect(aliases, isNotEmpty,
            reason: 'Entry point alias should exist for $name');
      }
    });

    test('entry point alias calls renamed procedure', () {
      // play1 alias should call boot:play1
      final play1Alias = linked.procedures
          .firstWhere((p) => p.name == 'play1');
      expect(play1Alias.clauses.length, equals(1));

      final body = play1Alias.clauses.first.body;
      expect(body, isNotNull);
      expect(body!.length, equals(1));
      expect(body.first.functor, equals('boot:play1'));
    });
  });

  group('Module name-collision (dedicated fixture)', () {
    // Sole coverage of module-local name-collision handling: mod_a and mod_b
    // each define dup/1; linking must disambiguate into mod_a:dup and mod_b:dup
    // with no bare collision. See test/programs/linker_collision/.
    late Program linked;

    setUp(() {
      final modules = discoverProject(collisionRoot, rootSelfGlpPath: rootSelfPath);
      linked = linkProject(modules, 'mod_a').program;
    });

    test('colliding procedures are disambiguated by module prefix', () {
      final procNames = linked.procedures.map((p) => p.name).toSet();
      expect(procNames, contains('mod_a:dup'));
      expect(procNames, contains('mod_b:dup'));
    });

    test('no bare collision; both prefixed names exist', () {
      final prefixedProcs = linked.procedures
          .where((p) => p.name.contains(':'))
          .map((p) => p.name)
          .toSet();
      // No bare 'dup' among prefixed procedures.
      expect(prefixedProcs.contains('dup'), isFalse);
      expect(prefixedProcs, contains('mod_a:dup'));
      expect(prefixedProcs, contains('mod_b:dup'));
    });

    test('both colliding definitions survive as distinct procedures', () {
      final dupProcs =
          linked.procedures.where((p) => p.name.endsWith(':dup')).toList();
      expect(dupProcs.length, greaterThanOrEqualTo(2),
          reason: 'mod_a:dup and mod_b:dup should both exist');
      final dupNames = dupProcs.map((p) => p.name).toSet();
      expect(dupNames, contains('mod_a:dup'));
      expect(dupNames, contains('mod_b:dup'));
    });
  });

  group('End-to-end compilation', () {
    test('linked program compiles to bytecode', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      final result = linkProject(modules, 'boot');

      final compiler = GlpCompiler();
      final bytecode = compiler.compileProgram(
        result.program,
        procDeclarations: result.procDeclarations,
      );

      // Should have procedure labels
      expect(bytecode.labels, isNotEmpty);
      expect(bytecode.labels.containsKey('boot:play1/0'), isTrue);
      expect(bytecode.labels.containsKey('boot:fplay1/0'), isTrue);
      expect(bytecode.labels.containsKey('play1/0'), isTrue,
          reason: 'Entry point alias should be in bytecode');
      expect(bytecode.labels.containsKey('fplay1/0'), isTrue,
          reason: 'Entry point alias should be in bytecode');
    });

    test('fplay1 produces correct output', () {
      final modules = discoverProject(cssgRoot, rootSelfGlpPath: rootSelfPath);
      final result = linkProject(modules, 'boot');
      final compiler = GlpCompiler();
      final bytecode = compiler.compileProgram(
        result.program,
        procDeclarations: result.procDeclarations,
      );

      // Compile self.glp (root stdlib) and merge
      final stdlibProg = compiler.compile(
          File('../programs/self.glp').readAsStringSync());
      var program = bytecode.merge(stdlibProg);

      // Set up runtime
      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);

      // Register runner
      rt.runners[program] = BytecodeRunner(program);

      // Create scheduler
      final scheduler = Scheduler(rt: rt);

      // Set up fplay1 goal
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, program);

      // Enqueue via entry point alias
      final fplayPc = program.labels['fplay1/0']!;
      rt.gq.enqueue(GoalRef(goalId, fplayPc));

      // Run
      final execResult = scheduler.drainWithStatus(maxCycles: 50000);

      // Print output for diagnostics
      if (output.isNotEmpty) {
        print('=== Static link fplay1 output (${output.length} lines) ===');
        for (final line in output) {
          print('  $line');
        }
      } else {
        print('=== Static link fplay1 produced no output ===');
        print('Status: ${execResult.status}');
      }

      // Verify output
      expect(output, isNotEmpty,
          reason: 'fplay1 should produce tagged output');

      final outputStr = output.join('\n');
      expect(outputStr, contains('tagged(alice'),
          reason: 'Output should contain tagged messages for alice');
      expect(outputStr, contains('connected(bob)'),
          reason: 'Alice should get connected(bob)');
      expect(outputStr, contains('connected(alice)'),
          reason: 'Charlie should get connected(alice)');
    });
  });
}
