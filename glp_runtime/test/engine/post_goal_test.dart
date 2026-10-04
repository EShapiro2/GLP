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
