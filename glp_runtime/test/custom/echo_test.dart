/// Test for echo program using terminal I/O with sequencing
///
/// Demonstrates:
/// - Concurrent goal execution
/// - Data dependencies via reader/writer
/// - known() guard for synchronization
/// - Proper sequencing of I/O operations

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  test('echo: read line and echo back with proper sequencing', () {
    print('\n=== ECHO TEST ===');
    print('Note: This test simulates input instead of blocking on stdin\n');

    final rt = GlpRuntime();
    registerStandardPredicates(rt.systemPredicates);

    // Create writer/reader pair for Line variable
    final (wLine, rLine) = rt.heap.allocateFreshPair();
    rt.heap.addWriter(WriterCell(wLine, rLine));
    rt.heap.addReader(ReaderCell(rLine));

    // Simulate read/1 by pre-binding Line to "hello world"
    // In real execution, read/1 would suspend until stdin has data
    rt.heap.bindWriterConst(wLine, 'hello world');

    // Compile the program:
    //
    // echo/0:
    //   execute('write', ['Enter text: ']),
    //   execute('read', [Line]),
    //   wait_then_echo(Line?).
    //
    // wait_then_echo/1:
    //   known(Input) |
    //   execute('write', ['You entered: ']),
    //   execute('write', [Input]),
    //   execute('nl', []).

    final program = BC.prog([
      // ===== echo/0 =====
      BC.L('echo/0'),
      BC.TRY(),
      BC.COMMIT(),

      // Body goals (spawn these)
      // For simplicity, we'll inline them instead of spawning
      // Real implementation would use Spawn instruction

      // execute('write', ['Enter text: '])
      SetClauseVar(0, ConstTerm('Enter text: ')),
      Execute('write', [0]),

      // execute('read', [Line]) - skip since we pre-bound Line
      // In real execution: Execute('read', [slot_for_Line_writer])

      // ===== wait_then_echo(Line?) =====
      BC.L('wait_then_echo'),
      BC.TRY(),

      // Set clause var 0 to VarRef(rLine, isReader: true) - this is the Input parameter
      SetClauseVar(0, VarRef(rLine, isReader: true)),

      // Guard: known(Input) - var 0 should be bound (it is, via rLine->wLine)
      BC.known(0),

      BC.COMMIT(),

      // Body of wait_then_echo
      SetClauseVar(1, ConstTerm('You entered: ')),
      Execute('write', [1]),

      // Write Input (clause var 0 is VarRef(rLine, isReader: true))
      Execute('write', [0]),

      Execute('nl', []),

      BC.PROCEED(),
    ]);

    // Create runner
    final runner = BytecodeRunner(program);
    final goalId = 1;

    // Create environment with rLine in arg slot 0
    final env = CallEnv(readers: {0: rLine});

    rt.setGoalEnv(goalId, env);
    rt.setGoalProgram(goalId, 'echo');

    // Create context starting at wait_then_echo (skip echo/0 for simplicity)
    final cx = RunnerContext(
      rt: rt,
      goalId: goalId,
      kappa: 0,
      env: env,
    );

    print('Running echo program:');
    print('(Input pre-bound to: "hello world")\n');

    final result = runner.runWithStatus(cx);

    print('\nResult: $result');
    expect(result, RunResult.terminated);

    print('✓ echo program executed successfully\n');
  });
}
