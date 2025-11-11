/// Test for hello world program using terminal I/O

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  test('hello_world/0: prints hello world', () {
    print('\n=== HELLO WORLD TEST ===');

    // Create runtime and register system predicates
    final rt = GlpRuntime();
    registerStandardPredicates(rt.systemPredicates);

    // Compile hello_world/0 program
    final program = BC.prog([
      // hello_world/0 entry at PC 0
      BC.L('hello_world/0'),
      BC.TRY(),

      // Head: hello_world (no arguments)

      // Commit (no guards)
      BC.COMMIT(),

      // Body: Set clause variable 0 to ConstTerm, then execute write
      SetClauseVar(0, ConstTerm('hello world')),

      // execute('write', [0])
      Execute('write', [0]),

      // execute('nl', [])
      Execute('nl', []),

      BC.PROCEED(),
    ]);

    // Create runner and goal
    final runner = BytecodeRunner(program);
    final goalId = 1;

    // Create empty environment (no arguments)
    final env = CallEnv();

    // Set up goal
    rt.setGoalEnv(goalId, env);
    rt.setGoalProgram(goalId, 'hello');

    // Create context and run
    final cx = RunnerContext(
      rt: rt,
      goalId: goalId,
      kappa: 0,  // Start at PC 0
      env: env,
    );

    print('\nRunning hello_world/0:');
    final result = runner.runWithStatus(cx);

    print('\nResult: $result');
    expect(result, RunResult.terminated);

    print('✓ hello_world/0 executed successfully\n');
  });
}
