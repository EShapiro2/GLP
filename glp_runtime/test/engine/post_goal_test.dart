/// The engine's posting call, GlpEngine.postGoal: every goal posted to the
/// runtime is checked before it runs, by one path.
///
/// TGLP modules.tex, "Type-Compatible Attestation Between Agents": "The
/// remaining producer of unchecked terms is the initial goal posted to the
/// runtime, at boot or interactively; it is type-checked before execution as
/// a body goal"; and "Entry and the absence of a boot module": "the
/// procedures that may be posted are exactly the entry points".  The call
/// checks the goal, puts it on the machine's queue, and returns the injector
/// of each input whose writer the caller holds --- the person's stream, the
/// network's --- which the REPL ([GlpEngine.runGoal]), the host
/// (multiagent/agent_runtime.dart) and the isolate boot
/// (multiagent/isolate_manager.dart) all post through.  Until 2026-10-04 the
/// two hosts put their goal on the queue by its label, unchecked, and
/// [GlpEngine.runGoal], which checked, handed the caller no writer.
///
/// Fixtures: programs/tests/post_goal/.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;
const _dir = '../programs/tests/post_goal';

GlpEngine _engineWith(String file) {
  final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
  expect(engine.loadFile('$_dir/$file'), isTrue);
  return engine;
}

/// [t] dereferenced through, as functor(args) and constants.
String _shown(GlpEngine engine, rt.Term? t) {
  if (t == null) return '_';
  final d = engine.runtime.heap.dereference(t);
  if (d is rt.ConstTerm) return '${d.value}';
  if (d is rt.StructTerm) {
    return '${d.functor}(${d.args.map((a) => _shown(engine, a)).join(', ')})';
  }
  return '_';
}

/// [run] refuses its goal, naming [reason], and leaves nothing on the queue.
void _refused(GlpEngine engine, void Function() run, String reason) {
  final queued = engine.runtime.gq.length;
  expect(
      run,
      throwsA(isA<GoalRefused>().having(
          (e) => e.message, 'refusal', contains(reason))));
  expect(engine.runtime.gq.length, queued,
      reason: 'a refused goal puts nothing on the machine');
}

void main() {
  group('postGoal', () {
    test('a goal reading a stream the caller extends: the injector it returns',
        () {
      final engine = _engineWith('count.glp');
      final posted = engine.postGoal('count(Xs?, 0, N)', inputs: ['Xs']);
      expect(posted.inputs.keys, ['Xs']);

      // The goal waits on the stream until the caller writes it.
      var r = posted.scheduler.drainToQuiescence();
      expect(r.status, ExecutionStatus.suspended);
      expect(engine.runtime.heap.isBound(posted.variables['N']!), isFalse);

      final xs = posted.inputs['Xs']!;
      for (final e in ['a', 'b', 'c']) {
        xs.inject(rt.ConstTerm(e)).forEach(engine.runtime.gq.enqueue);
        r = posted.scheduler.drainToQuiescence();
        expect(r.status, ExecutionStatus.suspended);
      }
      xs.close().forEach(engine.runtime.gq.enqueue);
      r = posted.scheduler.drainToQuiescence();
      expect(r.status, ExecutionStatus.succeeded);
      final n = engine.runtime.heap
          .dereference(rt.VarRef(posted.variables['N']!));
      expect(n, isA<rt.ConstTerm>().having((c) => c.value, 'value', 3));
    });

    test('an ill-typed goal is refused before it runs', () {
      final engine = _engineWith('count.glp');
      _refused(engine, () => engine.postGoal('count(Xs?, zero, N)',
          inputs: ['Xs']), 'Goal is not well-typed');
    });

    test('a goal calling no procedure the engine holds is refused', () {
      final engine = _engineWith('count.glp');
      _refused(engine, () => engine.postGoal('no_such(1)'),
          'Undefined procedure: no_such/1');
    });

    test('an input the goal writes is not the caller\'s to write', () {
      final engine = _engineWith('count.glp');
      _refused(
          engine,
          () => engine.postGoal('count([a], 0, Xs)', inputs: ['Xs']),
          'Input Xs occurs in the goal as a writer');
    });

    test('an input the goal does not hold is refused', () {
      final engine = _engineWith('count.glp');
      _refused(
          engine,
          () => engine.postGoal('count([a], 0, N)', inputs: ['Ys']),
          'Input Ys does not occur in the goal');
    });

    test('runGoal posts by the same path: its refusal is the check\'s', () async {
      final engine = _engineWith('count.glp');
      final result = await engine.runGoal('count([a, b], zero, N)');
      expect(result.status, ExecutionStatus.failed);
      expect(result.error, contains('Goal is not well-typed'));
      final ok = await engine.runGoal('count([a, b], 0, N)');
      expect(ok.status, ExecutionStatus.succeeded);
      expect(ok.bindings['N'],
          isA<rt.ConstTerm>().having((c) => c.value, 'value', 2));
    });
  });

  group('a posted goal is checked in the root and the entry points', () {
    // TGLP modules.tex, "Entry and the absence of a boot module": "A compiled
    // module is entered by posting a goal to it, and the goal calls an entry
    // point by plain name ... the procedures that may be posted are exactly
    // the entry points"; "Procedure declarations": "A declaration carries the
    // transitive closure of the types its signature references".  Until
    // 2026-10-07 the goal was checked in an environment of its own, every
    // loaded unit's self.glp chain and every module's declarations over the
    // root.

    test('a procedure the program does not export is refused by the check',
        () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(engine.loadProgram('../programs/tests/boot_scope/program'),
          isTrue);
      // helper/2 is worker.glp's, and the program does not export it: until
      // 2026-10-07 the goal passed the check and was refused only when no
      // entry point answered it ("Predicate helper/2 not found").
      _refused(engine, () => engine.postGoal('helper(secret(3), N)'),
          'Undefined procedure: helper/2');
      final ok = await engine.runGoal('serve([req(1), req(2)], N)');
      expect(ok.status, ExecutionStatus.succeeded, reason: '${ok.error}');
      expect(ok.bindings['N'],
          isA<rt.ConstTerm>().having((c) => c.value, 'value', 3));
    });

    test('a root procedure a self.glp redefines privately is the root\'s',
        () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(engine.loadProgram('../programs/tests/post_goal/private_root'),
          isTrue);
      // The program's merge/3 is over integers and private; a posted merge/3
      // is the root's, procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)),
      // and is checked against it.  Until 2026-10-07 the check read the
      // program's declaration while the root's procedure ran.
      final merged = await engine.runGoal('merge([a], [b], Zs)');
      expect(merged.status, ExecutionStatus.succeeded,
          reason: '${merged.error}');
      final zs = _shown(engine, merged.bindings['Zs']);
      expect(zs, anyOf('.(a, .(b, nil))', '.(b, .(a, nil))'));
      _refused(engine, () => engine.postGoal('merge(1, 2, N)'),
          'Goal is not well-typed');
      final run = await engine.runGoal('run(N)');
      expect(run.status, ExecutionStatus.succeeded, reason: '${run.error}');
      expect(run.bindings['N'],
          isA<rt.ConstTerm>().having((c) => c.value, 'value', 3));
    });
  });

  group('the host posts through it', () {
    test('AgentRuntime posts its entry goal checked, the person\'s stream its injector',
        () async {
      final lines = <String>[];
      final agent = AgentRuntime(
        agentId: 'alice',
        program: File('$_dir/echo_agent.glp').absolute.path,
        rootSelfGlpPath: _rootSelf,
        goalLabel: 'agent_init/3',
      )..onOutput = lines.add;
      await agent.initialize();
      expect(agent.initialized, isTrue);
      await agent.injectUserInput(rt.ConstTerm('hello'));
      expect(lines, contains('< heard(hello)'));
    });

    test('AgentRuntime loads a module file as a program, with what its chain '
        'exposes', () async {
      // agent.glp calls double/2, which its directory's self.glp exposes.
      // A module file is a program (TGLP modules.tex, "Hierarchy mirrors the
      // file system"), linked with its chain and the modules the chain exposes
      // (Compilation, first step; "The -expose directive"), not a boot source
      // handed to the engine (IGLP, Implementation Notes, "The scope a boot
      // source is checked in").  Until 2026-10-07 the host checked it in that
      // scope, which holds the chain without the procedures it exposes, and
      // refused the call.
      final lines = <String>[];
      final agent = AgentRuntime(
        agentId: 'alice',
        program: File('$_dir/exposed/agent.glp').absolute.path,
        rootSelfGlpPath: _rootSelf,
        goalLabel: 'agent_init/3',
      )..onOutput = lines.add;
      await agent.initialize();
      expect(agent.initialized, isTrue);
      await agent.injectUserInput(rt.ConstTerm(21));
      expect(lines, contains('< doubled(42)'));
    });

    test('AgentRuntime refuses an entry goal that is not well-typed', () async {
      final agent = AgentRuntime(
        agentId: 'alice',
        program: File('$_dir/integer_id_agent.glp').absolute.path,
        rootSelfGlpPath: _rootSelf,
        goalLabel: 'agent_init/2',
      );
      await expectLater(
          agent.initialize(),
          throwsA(isA<GoalRefused>().having(
              (e) => e.message, 'refusal', contains('not well-typed'))));
      expect(agent.initialized, isFalse);
    });

    test('the isolate boot refuses a spawn goal that is not well-typed',
        () async {
      final manager = IsolateManager();
      addTearDown(manager.shutdown);
      final config = BootLoader().load('''
procedure boot.
boot :- agent_init(alice, _)@alice.

${File('$_dir/integer_id_agent.glp').readAsStringSync()}''');
      config.rootSelfGlpPath = _rootSelf;
      await expectLater(
        manager.boot(config),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('failed to initialize') &&
              s.contains('agent_init/2 refused') &&
              s.contains('not well-typed');
        }, 'the agent reports the refused goal, so boot fails fast')),
      );
    }, timeout: Timeout(Duration(seconds: 30)));
  });
}
