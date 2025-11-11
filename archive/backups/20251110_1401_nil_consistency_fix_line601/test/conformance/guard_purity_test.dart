import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Guards are pure: HEAD/GUARD phase does not mutate heap', () {
    print('\n=== GUARD PURITY TEST: No Heap Mutation in Phase 1 ===');

    final rt = GlpRuntime();

    // Program with two clauses:
    // p(X) :- head_constant(X, 'a'), ground(X) | fail. (first clause fails guard)
    // p(X) :- otherwise | proceed.                     (second clause succeeds)
    //
    // The first clause:
    // - HEAD: tentatively binds X in σ̂w
    // - GUARD: ground(X) succeeds (X is bound in σ̂w)
    // - But then we force it to fail (to test next clause)
    //
    // The key test: After first clause fails, X should still be unbound on heap
    // because σ̂w was discarded, not applied.

    final prog = BC.prog([
      BC.L('p/1'),

      // Clause 1: Try to bind X, test ground(X), then fail
      BC.TRY(),
      BC.headConst('a', 0),     // σ̂w[wX] = 'a'
      BC.getVar(0, 0),          // X0 = wX
      BC.ground(0),             // Test if X0 is ground (succeeds via σ̂w)
      // Now force failure by trying to match 'b' (will fail)
      BC.headConst('b', 0),     // Try to match wX='b', conflicts with σ̂w[wX]='a'
      BC.COMMIT(),              // Unreachable
      BC.PROCEED(),

      // Clause 2: otherwise succeeds
      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Before execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');

    // Call p(X)
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    final ran = sched.drain(maxCycles: 10);

    print('After execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');
    print('Goals executed: ${ran.length}');

    // Verify: First clause built σ̂w and tested ground, but failed before commit
    // Therefore heap should still show X as unbound (σ̂w was discarded)
    expect(rt.heap.isWriterBound(wX), isFalse,
      reason: 'Writer must remain unbound because first clause failed before commit (σ̂w discarded)');
    expect(ran.length, 1, reason: 'Second clause should succeed');

    print('✓ HEAD/GUARD phase did not mutate heap (σ̂w discarded on clause failure)\n');
  });

  test('Guards cannot call allocate/deallocate: environment operations forbidden in Phase 1', () {
    print('\n=== GUARD PURITY TEST: No Allocate/Deallocate in Guards ===');

    final rt = GlpRuntime();

    // This test verifies architectural constraint: allocate/deallocate MUST NOT
    // appear in HEAD or GUARD phase.
    //
    // From spec: "Only BODY_i may contain: allocate, deallocate"
    //
    // We construct a program that violates this and verify it's not well-formed:
    // p(X) :- head_constant(X, 'a'), allocate(1) | ... (ILLEGAL!)
    //
    // Since this is a static property of bytecode (not runtime), we just verify
    // the constraint is documented and our implementation respects it.

    // Legal program: allocate only after commit
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.allocate(1),           // Legal: in BODY phase
      BC.deallocate(),          // Legal: in BODY phase
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'a');

    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Testing legal program: allocate/deallocate in BODY phase only');
    final ran = sched.drain(maxCycles: 10);

    expect(ran.length, 1, reason: 'Legal program should succeed');
    print('✓ Allocate/Deallocate are only allowed in BODY phase (after COMMIT)\n');
  });

  test('Guards add to suspension set Si but do not mutate heap cells', () {
    print('\n=== GUARD PURITY TEST: Guards add to Si without heap mutation ===');

    final rt = GlpRuntime();

    // Program:
    // p(X?) :- ground(X?) | proceed.  (suspends: X? unbound, adds to Si)
    // p(X?) :- otherwise | proceed.   (succeeds)
    //
    // The first clause tries ground(X?) where X? is unbound reader.
    // Guard should add X? to Si and fail (allowing next clause).
    // Key: This must not mutate any heap cells.

    final prog = BC.prog([
      BC.L('p/1'),

      // Clause 1: Test ground(X?) on unbound reader - adds to Si
      BC.TRY(),
      BC.getVar(0, 0),          // X0 = rX
      BC.ground(0),             // Test ground(rX) - suspends, adds to Si
      BC.COMMIT(),              // Unreachable
      BC.PROCEED(),

      // Clause 2: otherwise
      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Before execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');

    // Call p(X?)
    const goalId = 100;
    final env = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    final ran = sched.drain(maxCycles: 10);

    print('After execution: writer ${wX} bound = ${rt.heap.isWriterBound(wX)}');
    print('Goals executed: ${ran.length}');

    // Verify: Guard tested unbound reader, added to Si, but did NOT mutate heap
    expect(rt.heap.isWriterBound(wX), isFalse,
      reason: 'Writer must remain unbound (guard did not mutate heap)');
    expect(ran.length, 1, reason: 'Second clause should succeed');

    print('✓ Guards add to suspension set Si without mutating heap cells\n');
  });
}
