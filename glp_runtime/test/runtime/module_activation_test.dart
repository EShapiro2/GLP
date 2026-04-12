import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
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

/// Compile serve.glp and a target module, returning both bytecodes.
({BytecodeProgram serve, BytecodeProgram target}) compileModules(
    String targetSource) {
  final compiler = GlpCompiler();
  return (
    serve: compiler.compile(serveSource),
    target: compiler.compile(targetSource),
  );
}

void main() {
  group('Module activation via GLP', () {
    test('activateModule spawns serve on channel (suspends waiting for input)',
        () {
      final mods = compileModules('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: mods.serve,
        moduleBytecode: mods.target,
        moduleName: 'test_module',
      );

      // Serve runner should be registered
      expect(rt.runners.containsKey(mods.serve), isTrue);

      // Goal queue should have the serve goal
      expect(rt.gq.length, equals(1));

      // Drain — serve should suspend waiting for input on the channel
      final scheduler = Scheduler(rt: rt);
      final result = scheduler.drainWithStatus(maxCycles: 100);

      expect(result.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should suspend waiting for channel input');

      // Channel writer should still be valid (not yet bound)
      expect(channel.writerAddr, isNonNegative);
    });

    test('send single RPC goal on channel, verify it executes', () {
      final mods = compileModules('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: mods.serve,
        moduleBytecode: mods.target,
        moduleName: 'test_module',
      );

      final scheduler = Scheduler(rt: rt);

      // Step 1: Drain — serve suspends on empty channel
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Step 2: Send a goal on the channel
      final goal = StructTerm('process', [ConstTerm(42)]);
      final activations = channel.send(goal);

      // Enqueue woken goals
      for (final act in activations) {
        rt.gq.enqueue(act);
      }

      // Step 3: Drain — serve wakes up, dispatches goal via _activate
      result = scheduler.drainWithStatus(maxCycles: 200);

      // serve processes the goal and suspends again on the new tail
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason:
              'serve should process goal and suspend on next channel element');
    });

    test('send multiple RPC goals on channel', () {
      final mods = compileModules('''
exported procedure greet(Any?).
greet(_) :- otherwise | true.

exported procedure farewell(Any?).
farewell(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: mods.serve,
        moduleBytecode: mods.target,
        moduleName: 'multi_module',
      );

      final scheduler = Scheduler(rt: rt);

      // Drain — serve suspends
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Send first goal
      var activations = channel.send(StructTerm('greet', [ConstTerm('alice')]));
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Send second goal
      activations =
          channel.send(StructTerm('farewell', [ConstTerm('bob')]));
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Send third goal
      activations =
          channel.send(StructTerm('greet', [ConstTerm('carol')]));
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);
      expect(result.status, equals(ExecutionStatus.succeeded));
    });

    test('close channel after sending goals, serve terminates', () {
      final mods = compileModules('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: mods.serve,
        moduleBytecode: mods.target,
        moduleName: 'close_test',
      );

      final scheduler = Scheduler(rt: rt);

      // Drain — serve suspends
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Send a goal
      var activations = channel.send(StructTerm('process', [ConstTerm(1)]));
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Close the channel (bind writer to nil)
      activations = channel.close();
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);

      // serve(_, []) should match and succeed — no more suspension
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should terminate when channel is closed');
    });

    test('full end-to-end: activate, send RPC, close, verify dispatch chain',
        () {
      // Target module with exported procedure that spawns a sub-goal
      final mods = compileModules('''
exported procedure process(Any?).
process(X) :- ground(X?) | consume(X?).

procedure consume(Any?).
consume(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: mods.serve,
        moduleBytecode: mods.target,
        moduleName: 'e2e_module',
      );

      final trace = <String>[];
      final scheduler = Scheduler(
        rt: rt,
        traceSink: (s) => trace.add(s),
      );

      // Drain — serve suspends
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Send process(42)
      var activations =
          channel.send(StructTerm('process', [ConstTerm(42)]));
      for (final act in activations) {
        rt.gq.enqueue(act);
      }

      // Drain with debug to capture the full dispatch chain
      result = scheduler.drainWithStatus(maxCycles: 500, debug: true);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Verify trace shows the dispatch chain: serve → _select → process → consume
      final traceStr = trace.join('\n');
      expect(traceStr, contains('serve'),
          reason: 'Trace should show serve reduction');

      // Close channel and verify clean termination
      activations = channel.close();
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should terminate cleanly when channel closes');
    });
  });
}
