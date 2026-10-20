/// Program linker tests: static linking of multi-module GLP programs.
///
/// Tests discovery, type checking, renaming, call resolution, entry-point
/// aliasing (§3.4: exported root-level procedures only), and end-to-end
/// compilation of the linker_tree fixture, ../programs/tests/linker_tree: a
/// multi-module program with a nested directory agents/ carrying its own
/// self.glp and self.vglp, whose three vGLP agents, each beside its compiled
/// .glp, are modules of it as any .glp is, and which consumes the routing
/// modules exposed from the root self.glp.  It is the child-safe social
/// network's hand-written program as it stood at GLP d2495778, its comments
/// removed and its names changed, so that no paper's program is this test's
/// fixture; until 2026-10-20 the test read programs/cssn itself.  Module-local
/// name-collision handling is covered by the dedicated ../programs/tests/linker_collision
/// fixture; the nested-subprogram entry-alias rule by ../programs/tests/linker_nested.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show checkModule;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';

void main() {
  // The root self.glp: every program below is discovered with it, its first
  // module and the first layer of every module's scope.
  final rootSelfGlp = File('../programs/self.glp');
  final treeRoot = '../programs/tests/linker_tree';
  // The fixture's path from the root, by which each of its modules is named
  // (modules.tex, Compilation, third step).
  const tree = 'tests/linker_tree';
  // Dedicated minimal fixture: sole coverage of module-local name-collision
  // handling (two sibling modules each defining dup/1).
  final collisionRoot = '../programs/tests/linker_collision';
  final rootSelfPath = rootSelfGlp.existsSync() ? rootSelfGlp.absolute.path : null;

  if (!Directory(treeRoot).existsSync()) {
    print('linker_tree directory not found at $treeRoot, skipping tests');
    return;
  }

  group('Program discovery', () {
    test('discovers all modules in linker_tree', () {
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);

      // The program's own modules: the nested directory's (the three agents,
      // compiled from their vGLP sources, the mini-app entry, agents) and
      // the plays' (plays, person, host, linker_tree) --- plus the 4 routing
      // modules exposed from the root self.glp
      // (social/graph/routing/{output,inject,intro,befriend}), discovered as
      // linkable across the whole programs/ subtree (module-system spec §3.3).
      final names = modules.map((m) => m.moduleName).toSet();
      for (final n in ['adult', 'parent', 'child', 'miniapp', 'agents',
          'linker_tree', 'plays', 'person', 'host', 'output', 'inject',
          'intro', 'befriend']) {
        expect(names.any((m) => m == n || m.endsWith('/$n')), isTrue,
            reason: '$n is a module of the program: $names');
      }
    });

    test('excludes self.glp from modules', () {
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, isNot(contains('self')));
    });

    // TGLP modules.tex, Compilation, first step: "the compiler collects every
    // .glp file of the program's directory tree".  Until 2026-10-03 this test
    // was "excludes boot_direct.glp from modules", asserting the skip the
    // paper forbids (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:01. 6"); until
    // CSSN's port it named cssn/mad_boot/, which the port retired.
    test('collects every .glp file of the tree', () {
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      final discovered =
          modules.map((m) => File(m.filePath).absolute.path).toSet();
      final files = Directory(treeRoot)
          .listSync(recursive: true)
          .whereType<File>()
          .where((f) => f.path.endsWith('.glp') && !f.path.endsWith('/self.glp'))
          .map((f) => f.absolute.path)
          .toList();
      expect(files, isNotEmpty);
      expect(discovered, containsAll(files),
          reason: 'every .glp file of the tree is a module');
    });

    test('modules have correct ancestor scopes', () {
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);

      // All modules should have ancestor scope with self.glp types
      for (final mod in modules) {
        expect(mod.ancestorScope, isNotNull,
            reason: '${mod.moduleName} should have an ancestor scope');
      }
    });
  });

  group('Type checking', () {
    test('all modules type-check successfully', () {
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      // Should not throw
      expect(() => typeCheckProgram(modules, rootDir: treeRoot), returnsNormally);
    });
  });

  group('Linking', () {
    late List<DiscoveredModule> modules;
    late LinkResult linkResult;
    late Program linked;

    setUp(() {
      modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      // Inspect renaming/resolution on the pure step (pre-DCE).
      linkResult = linkAndResolveModules(modules, rootDir: treeRoot);
      linked = linkResult.program;
    });

    test('procedures are renamed with module prefix', () {
      final procNames = linked.procedures.map((p) => p.name).toSet();
      expect(procNames, contains('$tree/agents/adult:adult'));
      expect(procNames, contains('$tree/agents/parent:parent'));
      expect(procNames, contains('$tree/agents/child:child'));
      expect(procNames, contains('$tree/plays:village'));
      expect(procNames, contains('$tree/plays:party'));
      expect(procNames, contains('$tree/person:person'));
      expect(procNames, contains('$tree/host:host'));
    });

    List<String> bodyOf(String name) => [
          for (final c in linked.procedures.firstWhere((p) => p.name == name).clauses)
            if (c.body != null) for (final g in c.body!) g.functor
        ];

    test('cross-module calls are resolved', () {
      final body = bodyOf('$tree/plays:party').toSet();
      expect(body, contains('$tree/agents:app'),
          reason: 'agents # app should become $tree/agents:app');
      expect(body, contains('$tree/host:host'));
      expect(body, contains('$tree/person:person'));
      expect(body, isNot(contains('#')),
          reason: 'No RemoteGoal # dispatch should remain');
      // The adult agent hands a party to the parent agent, and the parent
      // agent hands it back, across modules.
      expect(bodyOf('$tree/agents/adult:adult'),
          contains('$tree/agents/parent:parent'));
      expect(bodyOf('$tree/agents/parent:after_kids'),
          contains('$tree/agents/adult:adult'));
    });

    test('local calls are resolved', () {
      final body = bodyOf('$tree/plays:village').toSet();
      expect(body, contains('$tree/plays:party'));
      expect(body, contains('$tree/plays:village_script'));
    });

    test('a root procedure is called by the root\'s name for it', () {
      // log calls send_to_user, a procedure of the root self.glp, which the
      // linker names by the root's path from itself, the empty one
      // (modules.tex, Compilation, third step): :send_to_user.
      final body = bodyOf('$tree/plays:log');
      expect(body, contains(':send_to_user'), reason: '$body');
    });

    test('entry-point aliases exist for the root self.glp forwarded entries', () {
      // modules.tex sec:static-linking step 5: the entry points are the root
      // self.glp's exports, here forwarders for each play, so every play has
      // a bare alias (and the non-exported helper party has none).
      final plays = modules.firstWhere((m) => m.moduleName == '$tree/plays');
      final exported = plays.ast.procDeclarations
          .where((d) => d.exported)
          .map((d) => d.name)
          .toSet();
      expect(exported, contains('village'));
      expect(exported, contains('play_twice'));
      for (final name in exported) {
        expect(linked.procedures.where((p) => p.name == name), isNotEmpty,
            reason: 'Entry point alias should exist for exported $name');
      }
      expect(linked.procedures.where((p) => p.name == 'party'), isEmpty,
          reason: 'non-exported plays:party must not be bare-aliased');
    });

    test('entry point alias calls renamed procedure', () {
      final alias = linked.procedures.firstWhere((p) => p.name == 'village');
      expect(alias.clauses.length, equals(1));
      final body = alias.clauses.first.body;
      expect(body, isNotNull);
      expect(body!.length, equals(1));
      expect(body.first.functor, equals('$tree:village'));
    });
  });

  group('Module name-collision (dedicated fixture)', () {
    // Sole coverage of module-local name-collision handling: mod_a and mod_b
    // each define dup/1; linking must disambiguate them, each renamed by its
    // module's path from the root (modules.tex, Compilation, third step), into
    // tests/linker_collision/mod_a:dup and tests/linker_collision/mod_b:dup
    // with no bare collision. See ../programs/tests/linker_collision/.
    const a = 'tests/linker_collision/mod_a:dup';
    const b = 'tests/linker_collision/mod_b:dup';
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
      expect(procNames, contains(a));
      expect(procNames, contains(b));
    });

    test('no bare collision; both prefixed names exist', () {
      final prefixedProcs = linked.procedures
          .where((p) => p.name.contains(':'))
          .map((p) => p.name)
          .toSet();
      // No bare 'dup' among prefixed procedures.
      expect(prefixedProcs.contains('dup'), isFalse);
      expect(prefixedProcs, contains(a));
      expect(prefixedProcs, contains(b));
    });

    test('both colliding definitions survive as distinct procedures', () {
      final dupProcs =
          linked.procedures.where((p) => p.name.endsWith(':dup')).toList();
      expect(dupProcs.length, greaterThanOrEqualTo(2),
          reason: '$a and $b should both exist');
      final dupNames = dupProcs.map((p) => p.name).toSet();
      expect(dupNames, contains(a));
      expect(dupNames, contains(b));
    });
  });

  group('Dead-code elimination (step 5, dedicated fixture)', () {
    // boot exports `run`, which calls `helper`; `dead` is never called; the
    // directory's self.glp exports `run` by forwarding it to boot (a directory
    // with no self.glp is not a program). The pure link transform keeps every
    // renamed procedure; eliminateDeadCode (the step-5 hand-off to the
    // compiler) keeps only the reachable ones. See ../programs/tests/linker_dce/.
    const dceRoot = '../programs/tests/linker_dce';

    // Each procedure is renamed by its module's path from the root
    // (modules.tex, Compilation, third step).
    const boot = 'tests/linker_dce/boot';

    test('pure link (linkAndResolveModules) keeps all renamed procedures, including dead ones', () {
      final modules = discoverProgram(dceRoot, rootSelfGlpPath: rootSelfPath);
      final names = linkAndResolveModules(modules, rootDir: dceRoot)
          .program
          .procedures
          .map((p) => p.name)
          .toSet();
      expect(names, contains('$boot:run'));
      expect(names, contains('$boot:helper'));
      expect(names, contains('$boot:dead'));
    });

    test('linkProgram (with step-5 DCE) keeps reachable procedures and prunes unreachable ones', () {
      final modules = discoverProgram(dceRoot, rootSelfGlpPath: rootSelfPath);
      final pruned = linkProgram(modules, rootDir: dceRoot);
      final names = pruned.program.procedures.map((p) => p.name).toSet();
      expect(names, contains('run'), reason: 'entry-point alias kept');
      expect(names, contains('tests/linker_dce:run'),
          reason: "the self.glp's forwarder kept");
      expect(names, contains('$boot:run'), reason: 'root export kept');
      expect(names, contains('$boot:helper'),
          reason: 'reachable from run kept');
      expect(names, isNot(contains('$boot:dead')),
          reason: 'unreachable pruned');
      // Declarations are pruned in step with their procedures.
      final declNames = pruned.procDeclarations.map((d) => d.name).toSet();
      expect(declNames, isNot(contains('$boot:dead')));
    });

    test('a name is followed only as step 4 resolved it, never by base name',
        () {
      // modules.tex, Compilation, fifth step: the reachable procedures are the
      // exported procedures and "every procedure called in the body of a
      // reachable one".  m:run's guards are the resolved defined guard
      // m:ready/1 and the builtin known/1, left bare; n:known/1 is a procedure
      // nothing calls.  Until 2026-10-03 the bare guard was also followed by
      // its base name, and n:known/1 was kept.
      VarTerm w(String n) => VarTerm(n, false, 0, 0);
      VarTerm r(String n) => VarTerm(n, true, 0, 0);
      Procedure proc(String name, Clause c) =>
          Procedure(name, c.head.args.length, [c], 0, 0);
      final program = Program([
        proc('run',
            Clause(Atom('run', [w('X')], 0, 0),
                body: [Goal('m:run', [r('X')], 0, 0)], line: 0, column: 0)),
        proc(
            'm:run',
            Clause(Atom('m:run', [w('X')], 0, 0),
                guards: [
                  Guard('m:ready', [r('X')], 0, 0),
                  Guard('known', [r('X')], 0, 0),
                ],
                line: 0,
                column: 0)),
        proc('m:ready',
            Clause(Atom('m:ready', [ConstTerm(1, 0, 0)], 0, 0),
                line: 0, column: 0)),
        proc('n:known',
            Clause(Atom('n:known', [ConstTerm(1, 0, 0)], 0, 0),
                line: 0, column: 0)),
      ], 0, 0);
      final kept = eliminateDeadCode(LinkResult(program, const []))
          .program
          .procedures
          .map((p) => p.name)
          .toSet();
      expect(kept, {'run', 'm:run', 'm:ready'});
    });
  });

  group('A redefined root operation needs a declaration (dedicated fixture)',
      () {
    // modules.tex, Definition "Typed Procedure, Module": a module is a sequence
    // of type definitions and typed procedures, each a declaration followed by
    // its procedure.  m.glp of undeclared/ redefines merge/3 by clauses with no
    // declaration of its own; until 2026-10-03 the linker gave its renamed
    // m:merge/3 a copy of the root's declaration.  declared/ carries the
    // declaration.  See ../programs/tests/linker_root_redef/.
    const redef = '../programs/tests/linker_root_redef';

    test('step 2 refuses a redefinition with no declaration of its own', () {
      final dir = '$redef/undeclared';
      final modules = discoverProgram(dir, rootSelfGlpPath: rootSelfPath);
      expect(
          () => checkedLinkedProgram(modules, rootDir: dir),
          throwsA(predicate((e) => e.toString().contains(
              'linker_root_redef/undeclared/m.glp:3: Procedure merge/3 is '
              'defined in this module and declared only in an enclosing '
              'scope'))));
    });

    test('so does the module loaded alone, checked against its scope', () {
      final m = '$redef/undeclared/m.glp';
      expect(
          () => checkModulesIndependently(
              discoverSingleModule(m, rootSelfGlpPath: rootSelfPath)),
          throwsA(predicate((e) => e.toString().contains(
              'm.glp:3: Procedure merge/3 is defined in this module and '
              'declared only in an enclosing scope'))));
    });

    test('the linked program borrows no declaration, and its check refuses M:p',
        () {
      final dir = '$redef/undeclared';
      final modules = discoverProgram(dir, rootSelfGlpPath: rootSelfPath);
      final flat = linkedFlatModule(modules, linkProgram(modules, rootDir: dir));
      expect(flat.procDeclarations.map((d) => d.name),
          isNot(contains('tests/linker_root_redef/undeclared/m:merge')));
      expect(
          checkModule(flat).errors.map((e) => e.message),
          contains(startsWith('Procedure '
              'tests/linker_root_redef/undeclared/m:merge/3 has no type '
              'declaration')));
    });

    test('a redefinition with its own declaration links and checks', () {
      final dir = '$redef/declared';
      final modules = discoverProgram(dir, rootSelfGlpPath: rootSelfPath);
      final linked = checkedLinkedProgram(modules, rootDir: dir);
      expect(linked.checkedDeclarations.map((d) => d.name),
          contains('tests/linker_root_redef/declared/m:merge'));
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
    // (exported `greet`). See ../programs/tests/linker_nested/.
    const nestedRoot = '../programs/tests/linker_nested';
    const nestedChild = '../programs/tests/linker_nested/child';

    test('whole subtree links; nested module present by prefixed name', () {
      final modules =
          discoverProgram(nestedRoot, rootSelfGlpPath: rootSelfPath);
      // Each module is named by its path from the root (modules.tex,
      // Compilation, third step).
      final names = modules.map((m) => m.moduleName).toSet();
      expect(names, contains('tests/linker_nested/boot'));
      expect(names, contains('tests/linker_nested/child/leaf'));

      // Renaming is a pre-DCE concern: the nested leaf's greet is not reached
      // from the root export play, so linkProgram would prune it. Inspect the
      // pure rename step.
      final linked = linkAndResolveModules(modules, rootDir: nestedRoot).program;
      final procNames = linked.procedures.map((p) => p.name).toSet();
      // Both modules' procedures are renamed and present.
      expect(procNames, contains('tests/linker_nested/boot:play'));
      expect(procNames, contains('tests/linker_nested/child/leaf:greet'));
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
      // When child/ is the loaded root, its self.glp exports greet: aliased.
      expect(procNames, contains('tests/linker_nested/child/leaf:greet'));
      expect(bare, contains('greet'));
    });
  });

  group('End-to-end compilation', () {
    test('linked program compiles to bytecode', () {
      // The object compiled is the checked linked program, compiled in the
      // environment it was checked in, as the engine compiles a program
      // (TGLP modules.tex, Compilation).
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      final linked = checkedLinkedProgram(modules, rootDir: treeRoot);

      final compiler = GlpCompiler();
      final bytecode = compiler.compileProgram(
        linked.program,
        procDeclarations: linked.procDeclarations,
        typeEnv: linked.checkedEnv,
      );

      expect(bytecode.labels, isNotEmpty);
      expect(bytecode.labels.containsKey('$tree/plays:village/0'), isTrue);
      expect(bytecode.labels.containsKey('village/0'), isTrue,
          reason: 'Entry point alias should be in bytecode');
    });

    test('play_both_ways produces correct output', () {
      // Two adults befriend each other at once, each through its stand-in
      // host; each mini-app adopts one conversation (linker_tree,
      // plays.glp).
      final modules = discoverProgram(treeRoot, rootSelfGlpPath: rootSelfPath);
      final linked = checkedLinkedProgram(modules, rootDir: treeRoot);
      final compiler = GlpCompiler();
      final bytecode = compiler.compileProgram(
        linked.program,
        procDeclarations: linked.procDeclarations,
        typeEnv: linked.checkedEnv,
      );
      final stdlibProg = compiler.compile(
          File('../programs/self.glp').readAsStringSync());
      final program = bytecode.merge(stdlibProg);

      final rt = GlpRuntime();
      final output = <String>[];
      rt.outputCallback = (s) => output.add(s);
      final image = codeImageFromProgram(program);
      rt.runners[program] = ByteRunner(image);
      final scheduler = Scheduler(rt: rt);
      final goalId = rt.nextGoalId++;
      rt.setGoalEnv(goalId, CallEnv(args: {}));
      rt.setGoalProgram(goalId, program);
      rt.gq.enqueue(GoalRef(goalId, image.entryOffsetOf('play_both_ways/0')!));
      scheduler.drainWithStatus(maxCycles: 500000);

      final outputStr = output.join('\n');
      expect(outputStr, contains('tagged(alice, connected(bob))'),
          reason: outputStr);
      expect(outputStr, contains('tagged(bob, connected(alice))'),
          reason: outputStr);
    });
  });
}
