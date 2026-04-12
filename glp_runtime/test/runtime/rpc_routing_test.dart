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

void main() {
  group('Phase 5: RPC routing via GLP channels', () {
    test('Distribute routes via GLP channel when target is activated', () {
      final compiler = GlpCompiler();

      // 1. Compile target module B with exported procedure
      final bBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');
      // _select/1 generation was removed — module exports are dispatched via
      // GLP channels now, tested by the remaining assertions.

      // 2. Compile serve.glp
      final serveBytecode = compiler.compile(serveSource);

      // 3. Compile caller module A with cross-module call to 'target_b'
      final aBytecode = compiler.compile('''
procedure caller(Any?).
caller(X) :- ground(X?) | target_b # process(X?).
''');
      expect(aBytecode.labels.containsKey('caller/1'), isTrue);

      // 4. Set up runtime and activate module B
      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: bBytecode,
        moduleName: 'target_b',
      );

      // Verify channel is registered
      expect(rt.glpChannels.containsKey('target_b'), isTrue);
      expect(rt.glpChannels['target_b'], same(channel));

      // 5. Drain — serve suspends waiting for input
      final scheduler = Scheduler(rt: rt);
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // 6. Set up caller goal with ReplModuleContext
      final callerGoalId = rt.nextGoalId++;
      final argAddr = rt.heap.storeTermOnHeap(ConstTerm(42));
      final env = CallEnv(args: {0: VarRef(argAddr)});
      rt.setGoalEnv(callerGoalId, env);
      rt.setGoalProgram(callerGoalId, aBytecode);

      // Set ReplModuleContext — importIndex 1 maps to target_b
      final replCtx = ReplModuleContext(
        moduleName: 'caller_a',
        imports: {1: ReplModuleTarget('target_b', bBytecode)},
      );
      rt.setGoalModuleContext(callerGoalId, replCtx);

      // Register caller runner
      if (!rt.runners.containsKey(aBytecode)) {
        rt.runners[aBytecode] = BytecodeRunner(aBytecode);
      }

      // 7. Enqueue caller goal
      final callerPc = aBytecode.labels['caller/1']!;
      rt.gq.enqueue(GoalRef(callerGoalId, callerPc));

      // 8. Drain — caller runs, Distribute finds GLP channel, sends goal,
      //    serve wakes, dispatches via _activate → _select → process
      result = scheduler.drainWithStatus(maxCycles: 500);

      // serve should process the goal and suspend on the next channel element
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason:
              'serve should process the RPC goal and suspend waiting for next input');
    });

    test('Distribute routes via GLP channel with debug trace', () {
      final compiler = GlpCompiler();

      final bBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);

      final aBytecode = compiler.compile('''
procedure caller(Any?).
caller(X) :- ground(X?) | target_b # process(X?).
''');

      final rt = GlpRuntime();
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: bBytecode,
        moduleName: 'target_b',
      );

      final trace = <String>[];
      final scheduler = Scheduler(
        rt: rt,
        traceSink: (s) => trace.add(s),
      );
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Set up caller with ReplModuleContext
      final callerGoalId = rt.nextGoalId++;
      final argAddr = rt.heap.storeTermOnHeap(ConstTerm(42));
      final env = CallEnv(args: {0: VarRef(argAddr)});
      rt.setGoalEnv(callerGoalId, env);
      rt.setGoalProgram(callerGoalId, aBytecode);

      final replCtx = ReplModuleContext(
        moduleName: 'caller_a',
        imports: {1: ReplModuleTarget('target_b', bBytecode)},
      );
      rt.setGoalModuleContext(callerGoalId, replCtx);

      if (!rt.runners.containsKey(aBytecode)) {
        rt.runners[aBytecode] = BytecodeRunner(aBytecode);
      }

      rt.gq.enqueue(GoalRef(callerGoalId, aBytecode.labels['caller/1']!));

      // Run with debug tracing
      result = scheduler.drainWithStatus(
        maxCycles: 500,
        debug: true,
      );

      expect(result.status, equals(ExecutionStatus.succeeded));

      // Verify trace shows the dispatch chain
      final traceStr = trace.join('\n');
      expect(traceStr, contains('serve'),
          reason: 'Trace should show serve reduction');
    });

    test('multiple Distribute RPCs route through GLP channel', () {
      final compiler = GlpCompiler();

      final bBytecode = compiler.compile('''
exported procedure greet(Any?).
greet(_) :- otherwise | true.

exported procedure farewell(Any?).
farewell(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);

      // Caller sends two RPCs to target_b
      final aBytecode = compiler.compile('''
procedure run_both(Any?, Any?).
run_both(X, Y) :- ground(X?), ground(Y?) | target_b # greet(X?), target_b # farewell(Y?).
''');

      final rt = GlpRuntime();
      activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: bBytecode,
        moduleName: 'target_b',
      );

      final scheduler = Scheduler(rt: rt);
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Set up caller
      final callerGoalId = rt.nextGoalId++;
      final arg0Addr = rt.heap.storeTermOnHeap(ConstTerm('alice'));
      final arg1Addr = rt.heap.storeTermOnHeap(ConstTerm('bob'));
      final env = CallEnv(args: {0: VarRef(arg0Addr), 1: VarRef(arg1Addr)});
      rt.setGoalEnv(callerGoalId, env);
      rt.setGoalProgram(callerGoalId, aBytecode);

      final replCtx = ReplModuleContext(
        moduleName: 'caller_a',
        imports: {1: ReplModuleTarget('target_b', bBytecode)},
      );
      rt.setGoalModuleContext(callerGoalId, replCtx);

      if (!rt.runners.containsKey(aBytecode)) {
        rt.runners[aBytecode] = BytecodeRunner(aBytecode);
      }

      rt.gq.enqueue(GoalRef(callerGoalId, aBytecode.labels['run_both/2']!));

      // Drain — both RPCs should route through GLP channel
      result = scheduler.drainWithStatus(maxCycles: 1000);
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should process both RPCs and suspend');
    });

    test('activateModule registers channel in glpChannels', () {
      final compiler = GlpCompiler();

      final bBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);

      final rt = GlpRuntime();
      expect(rt.glpChannels.isEmpty, isTrue);

      final channel = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: bBytecode,
        moduleName: 'my_module',
      );

      expect(rt.glpChannels.containsKey('my_module'), isTrue);
      expect(rt.glpChannels['my_module'], same(channel));
    });

    test('close channel after RPC routing, serve terminates', () {
      final compiler = GlpCompiler();

      final bBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);

      final aBytecode = compiler.compile('''
procedure caller(Any?).
caller(X) :- ground(X?) | target_b # process(X?).
''');

      final rt = GlpRuntime();
      final channel = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: bBytecode,
        moduleName: 'target_b',
      );

      final scheduler = Scheduler(rt: rt);
      var result = scheduler.drainWithStatus(maxCycles: 100);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Run caller — RPC routes through GLP channel
      final callerGoalId = rt.nextGoalId++;
      final argAddr = rt.heap.storeTermOnHeap(ConstTerm(99));
      final env = CallEnv(args: {0: VarRef(argAddr)});
      rt.setGoalEnv(callerGoalId, env);
      rt.setGoalProgram(callerGoalId, aBytecode);

      final replCtx = ReplModuleContext(
        moduleName: 'caller_a',
        imports: {1: ReplModuleTarget('target_b', bBytecode)},
      );
      rt.setGoalModuleContext(callerGoalId, replCtx);

      if (!rt.runners.containsKey(aBytecode)) {
        rt.runners[aBytecode] = BytecodeRunner(aBytecode);
      }

      rt.gq.enqueue(GoalRef(callerGoalId, aBytecode.labels['caller/1']!));

      result = scheduler.drainWithStatus(maxCycles: 500);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Close channel — serve should terminate
      final activations = channel.close();
      for (final act in activations) {
        rt.gq.enqueue(act);
      }
      result = scheduler.drainWithStatus(maxCycles: 200);
      expect(result.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should terminate when channel is closed');
    });
  });
}
