/// Program linker tests: static linking of multi-module GLP programs.
///
/// Tests discovery, type checking, renaming, call resolution, entry-point
/// aliasing (§3.4: exported root-level procedures only), and end-to-end
/// compilation of the cssn program (a live multi-module platform that consumes
/// the routing modules exposed from the root self.glp). Module-local
/// name-collision handling is covered by the dedicated test/programs/linker_collision
/// fixture; the nested-subprogram entry-alias rule by test/programs/linker_nested.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart' show setRootScopeUnitClauseSource;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart' show setRootScopeEnvironmentSource;
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';

void main() {
  // Set prelude sources from programs/self.glp (same as GlpEngine constructor)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setRootScopeUnitClauseSource(source);
    setRootScopeEnvironmentSource(source);
  }
  final cssnRoot = '../programs/cssn';
  // Dedicated minimal fixture: sole coverage of module-local name-collision
  // handling (two sibling modules each defining dup/1).
  final collisionRoot = 'test/programs/linker_collision';
  final rootSelfPath = rootSelfGlp.existsSync() ? rootSelfGlp.absolute.path : null;

  if (!Directory(cssnRoot).existsSync()) {
    print('cssn directory not found at $cssnRoot, skipping tests');
    return;
  }

  group('Program discovery', () {
    test('discovers all modules in cssn', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);

      // The program's own 6 modules: agent, child_agent, mediator, actors,
      // boot, cssn — plus the 4 routing modules exposed from the root self.glp
      // (social/graph/routing/{output,inject,intro,befriend}), discovered as linkable
      // across the whole programs/ subtree (module-system spec §3.3).  Any left
      // unreachable are pruned by DCE at compile time; discovery still lists them.
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, contains('agent'));
      expect(names, contains('child_agent'));
      expect(names, contains('mediator'));
      expect(names, contains('actors'));
      expect(names, contains('boot'));
      expect(names, contains('cssn'));
      // -module removed: module names derive from the filename, so the routing
      // modules are output/inject/intro/befriend (social/graph/routing/<name>.glp).
      expect(names, contains('output'));
      expect(names, contains('inject'));
      expect(names, contains('intro'));
      expect(names, contains('befriend'));
      // At least the 6 own modules + 4 exposed routing modules; cssn also carries
      // a village/ subtree of actor scenarios, so the total is larger.
      expect(names.length, greaterThanOrEqualTo(10));
    });

    test('excludes self.glp from modules', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, isNot(contains('self')));
    });

    test('excludes boot_direct.glp from modules', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      final filenames = modules.map((m) => m.filePath).toList();
      expect(filenames.any((f) => f.contains('boot_direct')), isFalse,
          reason: 'boot_direct.glp should be excluded');
    });

    test('modules have correct ancestor scopes', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);

      // All modules should have ancestor scope with self.glp types
      for (final mod in modules) {
        expect(mod.ancestorScope, isNotNull,
            reason: '${mod.moduleName} should have an ancestor scope');
      }
    });
  });

  group('Type checking', () {
    test('all modules type-check successfully', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      // Should not throw
      expect(() => typeCheckProgram(modules, rootDir: cssnRoot), returnsNormally);
    });
  });

  group('Linking', () {
    late List<DiscoveredModule> modules;
    late LinkResult linkResult;
    late Program linked;

    setUp(() {
      modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      // Inspect renaming/resolution on the pure step (pre-DCE).
      linkResult = linkAndResolveModules(modules, rootDir: cssnRoot);
      linked = linkResult.program;
    });

    test('procedures are renamed with module prefix', () {
      final procNames = linked.procedures.map((p) => p.name).toSet();

      // agent.glp procedures should have agent: prefix
      expect(procNames, contains('agent:agent'));

      // boot.glp procedures should have boot: prefix
      expect(procNames, contains('boot:tee'));
      expect(procNames, contains('boot:fplay1'));

      // mediator.glp procedures should have mediator: prefix
      expect(procNames, contains('mediator:ui_mediator'));

      // actors.glp procedures should have actors: prefix
      expect(procNames, contains('actors:alice1'));
      expect(procNames, contains('actors:bob1'));
    });

    test('cross-module calls are resolved', () {
      // Find boot:fplay1 and check that its body has agent:agent, not # dispatch
      final bootPlay1 = linked.procedures
          .firstWhere((p) => p.name == 'boot:fplay1');

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
      // boot:fplay1 calls tee/network3/send_to_user_tagged, local to boot —
      // each should be prefixed boot:.
      final bootPlay1 = linked.procedures
          .firstWhere((p) => p.name == 'boot:fplay1');

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
      expect(bodyFunctors, contains('boot:send_to_user_tagged'),
          reason: 'Local send_to_user_tagged call should become boot:send_to_user_tagged');
      expect(bodyFunctors, contains('boot:network3'),
          reason: 'Local network3 call should become boot:network3');
    });

    test('prelude calls are preserved unprefixed', () {
      // send_to_user_tagged calls send_to_user, a root self.glp system predicate
      // — it must stay unprefixed (linker leaves prelude calls bare).
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

      expect(bodyFunctors, contains('send_to_user'),
          reason: 'send_to_user prelude call should remain unprefixed');
    });

    test('entry-point aliases exist for the root self.glp forwarded entries', () {
      // modules.tex sec:static-linking step 5: the entry points are the root
      // self.glp's exports, here forwarders for each boot play, so every boot
      // play has a bare alias (and the non-exported helper tee has none).
      final bootModule = modules.firstWhere((m) => m.moduleName == 'boot');
      final exported = bootModule.ast.procDeclarations
          .where((d) => d.exported)
          .map((d) => d.name)
          .toSet();
      expect(exported, contains('fplay1'));
      expect(exported, contains('fplay2'));

      for (final name in exported) {
        final aliases = linked.procedures
            .where((p) => p.name == name && !p.name.contains(':'))
            .toList();
        expect(aliases, isNotEmpty,
            reason: 'Entry point alias should exist for exported $name');
      }

      // A non-exported boot helper (tee) must NOT receive a bare alias.
      final teeAlias = linked.procedures
          .where((p) => p.name == 'tee' && !p.name.contains(':'))
          .toList();
      expect(teeAlias, isEmpty,
          reason: 'non-exported boot:tee must not be bare-aliased');
    });

    test('entry point alias calls renamed procedure', () {
      // The entry point is the root self.glp's exported fplay1, so the bare
      // alias calls cssn:fplay1 (the root self.glp's forwarder, which in turn
      // calls boot:fplay1). modules.tex sec:static-linking step 5.
      final play1Alias = linked.procedures
          .firstWhere((p) => p.name == 'fplay1');
      expect(play1Alias.clauses.length, equals(1));

      final body = play1Alias.clauses.first.body;
      expect(body, isNotNull);
      expect(body!.length, equals(1));
      expect(body.first.functor, equals('cssn:fplay1'));
    });
  });

  group('Module name-collision (dedicated fixture)', () {
    // Sole coverage of module-local name-collision handling: mod_a and mod_b
    // each define dup/1; linking must disambiguate into mod_a:dup and mod_b:dup
    // with no bare collision. See test/programs/linker_collision/.
    late Program linked;

    setUp(() {
      final modules = discoverProgram(collisionRoot, rootSelfGlpPath: rootSelfPath);
      // Renaming/disambiguation is a pre-DCE concern: mod_a/mod_b export
      // nothing and call nothing, so both dups are dead and would be pruned by
      // linkProgram. Inspect the pure rename step.
      linked = linkAndResolveModules(modules, rootDir: collisionRoot).program;
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

  group('Dead-code elimination (step 5, dedicated fixture)', () {
    // boot exports `run`, which calls `helper`; `dead` is never called. The
    // pure link transform keeps every renamed procedure; eliminateDeadCode
    // (the step-5 hand-off to the compiler) keeps only the reachable ones.
    // See test/programs/linker_dce/.
    const dceRoot = 'test/programs/linker_dce';

    test('pure link (linkAndResolveModules) keeps all renamed procedures, including dead ones', () {
      final modules = discoverProgram(dceRoot, rootSelfGlpPath: rootSelfPath);
      final names = linkAndResolveModules(modules, rootDir: dceRoot)
          .program
          .procedures
          .map((p) => p.name)
          .toSet();
      expect(names, contains('boot:run'));
      expect(names, contains('boot:helper'));
      expect(names, contains('boot:dead'));
    });

    test('linkProgram (with step-5 DCE) keeps reachable procedures and prunes unreachable ones', () {
      final modules = discoverProgram(dceRoot, rootSelfGlpPath: rootSelfPath);
      final pruned = linkProgram(modules, rootDir: dceRoot);
      final names = pruned.program.procedures.map((p) => p.name).toSet();
      expect(names, contains('run'), reason: 'entry-point alias kept');
      expect(names, contains('boot:run'), reason: 'root export kept');
      expect(names, contains('boot:helper'),
          reason: 'reachable from run kept');
      expect(names, isNot(contains('boot:dead')),
          reason: 'unreachable pruned');
      // Declarations are pruned in step with their procedures.
      final declNames = pruned.procDeclarations.map((d) => d.name).toSet();
      expect(declNames, isNot(contains('boot:dead')));
    });
  });

  group('Exposed procedures are not entry points (modules.tex §Design)', () {
    // An -exposed procedure is callable by name in the subtree but is NOT an
    // entry point unless the root self.glp exports it in its own right; entry
    // points are the reachability roots (§Static Linking step 5). expose/basic
    // exposes util#strutil (twice) and util#plist (pmerge) and exports its own
    // use_exposed (a forwarder that calls twice).
    const exposeRoot = '../programs/tests/expose/basic';

    test('only the root self.glp export is a bare entry point; exposed are not', () {
      final modules = discoverProgram(exposeRoot, rootSelfGlpPath: rootSelfPath);
      final linked = linkProgram(modules, rootDir: exposeRoot).program;
      final bare = linked.procedures
          .where((p) => !p.name.contains(':'))
          .map((p) => p.name)
          .toSet();
      expect(bare, contains('use_exposed'),
          reason: 'root self.glp own export is an entry point');
      expect(bare, isNot(contains('twice')),
          reason: 'an exposed procedure is not an entry point');
      expect(bare, isNot(contains('pmerge')));
      // The used exposed procedure is still present — reachable via use_exposed.
      final all = linked.procedures.map((p) => p.name).toSet();
      expect(all.any((n) => n.endsWith('twice')), isTrue);
    });
  });

  group('Nested sub-program entry aliases (dedicated fixture)', () {
    // Sole coverage of the §3.4 nested-subprogram rule. Parent has root-level
    // `boot` (exported `play`) and nested `child/` (own self.glp) with `leaf`
    // (exported `greet`). See test/programs/linker_nested/.
    const nestedRoot = 'test/programs/linker_nested';
    const nestedChild = 'test/programs/linker_nested/child';

    test('whole subtree links; nested module present by prefixed name', () {
      final modules =
          discoverProgram(nestedRoot, rootSelfGlpPath: rootSelfPath);
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, contains('boot'));
      expect(names, contains('leaf'));

      // Renaming is a pre-DCE concern: the nested leaf:greet is not reached
      // from the root export boot:play, so linkProgram would prune it. Inspect
      // the pure rename step.
      final linked = linkAndResolveModules(modules, rootDir: nestedRoot).program;
      final procNames = linked.procedures.map((p) => p.name).toSet();
      // Both modules' procedures are renamed and present.
      expect(procNames, contains('boot:play'));
      expect(procNames, contains('leaf:greet'));
    });

    test("root's exported play is aliased; nested export is not", () {
      final modules =
          discoverProgram(nestedRoot, rootSelfGlpPath: rootSelfPath);
      final linked = linkProgram(modules, rootDir: nestedRoot).program;
      final bare = linked.procedures
          .where((p) => !p.name.contains(':'))
          .map((p) => p.name)
          .toSet();
      // Root-level export aliased; nested export NOT aliased from the parent.
      expect(bare, contains('play'));
      expect(bare, isNot(contains('greet')));
    });

    test('nested dir loads standalone with its own aliases', () {
      final modules =
          discoverProgram(nestedChild, rootSelfGlpPath: rootSelfPath);
      final linked = linkProgram(modules, rootDir: nestedChild).program;
      final procNames = linked.procedures.map((p) => p.name).toSet();
      final bare = linked.procedures
          .where((p) => !p.name.contains(':'))
          .map((p) => p.name)
          .toSet();
      // When child/ is the loaded root, leaf is root-level: greet is aliased.
      expect(procNames, contains('leaf:greet'));
      expect(bare, contains('greet'));
    });
  });

  group('End-to-end compilation', () {
    test('linked program compiles to bytecode', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      final result = linkProgram(modules, rootDir: cssnRoot);

      final compiler = GlpCompiler();
      final bytecode = compiler.compileProgram(
        result.program,
        procDeclarations: result.procDeclarations,
      );

      // Should have procedure labels
      expect(bytecode.labels, isNotEmpty);
      expect(bytecode.labels.containsKey('boot:fplay1/0'), isTrue);
      expect(bytecode.labels.containsKey('fplay1/0'), isTrue,
          reason: 'Entry point alias should be in bytecode');
    });

    test('fplay1 produces correct output', () {
      final modules = discoverProgram(cssnRoot, rootSelfGlpPath: rootSelfPath);
      final result = linkProgram(modules, rootDir: cssnRoot);
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
      final image = codeImageFromProgram(program);
      rt.runners[program] = ByteRunner(image);

      // Create scheduler
      final scheduler = Scheduler(rt: rt);

      // Set up fplay1 goal
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, program);

      // Enqueue via entry point alias
      final fplayPc = image.entryOffsetOf('fplay1/0')!;
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
