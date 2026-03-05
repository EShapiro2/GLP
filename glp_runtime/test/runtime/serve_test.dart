import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
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

/// Build a GLP list on the heap from Dart list of Terms.
/// Returns the heap address of the list head.
/// GLP list: [a, b, c] = .(a, .(b, .(c, nil)))
int buildGlpListOnHeap(HeapFCP heap, List<Term> elements) {
  // Build from back to front: nil, then cons cells
  Term list = ConstTerm('nil');
  for (var i = elements.length - 1; i >= 0; i--) {
    list = StructTerm('.', [elements[i], list]);
  }
  return heap.storeTermOnHeap(list);
}

void main() {
  group('serve/2 system predicate', () {
    test('serve.glp compiles with correct labels', () {
      final compiler = GlpCompiler();
      final bytecode = compiler.compile(serveSource);

      expect(bytecode.labels.containsKey('serve/2'), isTrue,
          reason: 'serve/2 entry point should be generated');
      expect(bytecode.labels['serve/2'], equals(0),
          reason: 'serve/2 should be the first procedure');
    });

    test('serve dispatches single goal to target module', () {
      // 1. Compile target module with exports (generates _select/1)
      final compiler = GlpCompiler();
      final targetBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');
      expect(targetBytecode.labels.containsKey('_select/1'), isTrue);

      // 2. Compile serve.glp
      final serveBytecode = compiler.compile(serveSource);

      // 3. Set up runtime
      final rt = GlpRuntime();

      // 4. Create ModuleTerm and store on heap
      final moduleTerm = ModuleTerm(targetBytecode, name: 'target');
      final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

      // 5. Build goal list: [process(42)]
      final goalTerm = StructTerm('process', [ConstTerm(42)]);
      final listAddr = buildGlpListOnHeap(rt.heap, [goalTerm]);

      // 6. Create serve/2 goal
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(listAddr)});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, serveBytecode);

      // 7. Enqueue and set up scheduler
      final servePc = serveBytecode.labels['serve/2']!;
      rt.gq.enqueue(GoalRef(goalId, servePc));

      final serveRunner = BytecodeRunner(serveBytecode);
      final scheduler = Scheduler(
        rt: rt,
        runners: {serveBytecode: serveRunner},
      );

      // 8. Run
      final drainResult = scheduler.drainWithStatus(maxCycles: 200);

      // 9. Verify success
      expect(drainResult.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should dispatch process(42) successfully');
      expect(drainResult.goalsRan.length, greaterThanOrEqualTo(2),
          reason: 'Should run at least serve goal + _select/_process goals');
    });

    test('serve dispatches multiple goals to target module', () {
      final compiler = GlpCompiler();
      final targetBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);
      final rt = GlpRuntime();

      // Module on heap
      final moduleTerm = ModuleTerm(targetBytecode, name: 'target');
      final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

      // Goal list: [process(1), process(2), process(3)]
      final goals = [
        StructTerm('process', [ConstTerm(1)]),
        StructTerm('process', [ConstTerm(2)]),
        StructTerm('process', [ConstTerm(3)]),
      ];
      final listAddr = buildGlpListOnHeap(rt.heap, goals);

      // Enqueue serve
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(listAddr)});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, serveBytecode);
      rt.gq.enqueue(GoalRef(goalId, serveBytecode.labels['serve/2']!));

      final scheduler = Scheduler(
        rt: rt,
        runners: {serveBytecode: BytecodeRunner(serveBytecode)},
      );
      final drainResult = scheduler.drainWithStatus(maxCycles: 500);

      expect(drainResult.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should dispatch all three goals');
      // At minimum: 3 serve iterations + 3 _select + 3 process + 1 serve([]) = 10
      expect(drainResult.goalsRan.length, greaterThanOrEqualTo(4),
          reason: 'Should run serve + multiple dispatched goals');
    });

    test('serve handles empty stream (base case)', () {
      final compiler = GlpCompiler();
      // Need a target module even though no goals will be dispatched
      final targetBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);
      final rt = GlpRuntime();

      // Module on heap
      final moduleTerm = ModuleTerm(targetBytecode, name: 'target');
      final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

      // Empty goal list: []
      final listAddr = buildGlpListOnHeap(rt.heap, []);

      // Enqueue serve
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(listAddr)});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, serveBytecode);
      rt.gq.enqueue(GoalRef(goalId, serveBytecode.labels['serve/2']!));

      final scheduler = Scheduler(
        rt: rt,
        runners: {serveBytecode: BytecodeRunner(serveBytecode)},
      );
      final drainResult = scheduler.drainWithStatus(maxCycles: 100);

      expect(drainResult.status, equals(ExecutionStatus.succeeded),
          reason: 'serve with empty list should succeed immediately');
    });

    test('serve dispatches to module with multiple exports', () {
      final compiler = GlpCompiler();
      final targetBytecode = compiler.compile('''
exported procedure greet(Any?).
greet(_) :- otherwise | true.

exported procedure farewell(Any?).
farewell(_) :- otherwise | true.
''');
      expect(targetBytecode.labels.containsKey('_select/1'), isTrue);

      final serveBytecode = compiler.compile(serveSource);
      final rt = GlpRuntime();

      final moduleTerm = ModuleTerm(targetBytecode, name: 'multi_export');
      final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

      // Mix of goals to different exports
      final goals = [
        StructTerm('greet', [ConstTerm('alice')]),
        StructTerm('farewell', [ConstTerm('bob')]),
        StructTerm('greet', [ConstTerm('carol')]),
      ];
      final listAddr = buildGlpListOnHeap(rt.heap, goals);

      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(listAddr)});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, serveBytecode);
      rt.gq.enqueue(GoalRef(goalId, serveBytecode.labels['serve/2']!));

      final scheduler = Scheduler(
        rt: rt,
        runners: {serveBytecode: BytecodeRunner(serveBytecode)},
      );
      final drainResult = scheduler.drainWithStatus(maxCycles: 500);

      expect(drainResult.status, equals(ExecutionStatus.succeeded),
          reason: 'serve should dispatch goals to different exports');
    });

    test('full dispatch chain: serve → _activate → _select → procedure', () {
      // Compile target module with an exported procedure that has observable behavior
      final compiler = GlpCompiler();
      final targetBytecode = compiler.compile('''
exported procedure process(Any?).
process(X) :- ground(X?) | consume(X?).

procedure consume(Any?).
consume(_) :- otherwise | true.
''');

      final serveBytecode = compiler.compile(serveSource);
      final rt = GlpRuntime();

      // Store module and goal list on heap
      final moduleTerm = ModuleTerm(targetBytecode, name: 'chain_test');
      final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

      final goals = [StructTerm('process', [ConstTerm(42)])];
      final listAddr = buildGlpListOnHeap(rt.heap, goals);

      // Set up serve goal
      final goalId = rt.nextGoalId++;
      final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(listAddr)});
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, serveBytecode);
      rt.gq.enqueue(GoalRef(goalId, serveBytecode.labels['serve/2']!));

      // Run with debug tracing to capture reductions
      final trace = <String>[];
      final scheduler = Scheduler(
        rt: rt,
        runners: {serveBytecode: BytecodeRunner(serveBytecode)},
        traceSink: (s) => trace.add(s),
      );
      final drainResult = scheduler.drainWithStatus(maxCycles: 500, debug: true);

      expect(drainResult.status, equals(ExecutionStatus.succeeded),
          reason: 'Full dispatch chain should succeed');

      // Verify the trace shows the dispatch chain
      final traceStr = trace.join('\n');
      expect(traceStr, contains('serve'),
          reason: 'Trace should show serve reduction');
    });
  });
}
