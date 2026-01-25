/// Interactive echo demo - reads from stdin and echoes back
///
/// Run with: dart run bin/echo_demo.dart

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';

void main() {
  print('=== GLP Echo Demo ===');
  print('This demonstrates concurrent GLP with I/O sequencing\n');

  final rt = GlpRuntime();
  registerStandardPredicates(rt.systemPredicates);

  // Create writer/reader pair for Line variable
  final (wLine, rLine) = rt.heap.allocateFreshPair();
  rt.heap.addWriter(WriterCell(wLine, rLine));
  rt.heap.addReader(ReaderCell(rLine));

  // Compile the echo program:
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

    // execute('write', ['Enter text: '])
    SetClauseVar(0, ConstTerm('Enter text: ')),
    Execute('write', [0]),

    // execute('read', [Line])
    SetClauseVar(1, WriterTerm(wLine)),
    Execute('read', [1]),

    // ===== wait_then_echo(Line?) =====
    BC.L('wait_then_echo'),
    BC.TRY(),

    // Set clause var 0 to ReaderTerm(rLine) - this is the Input parameter
    SetClauseVar(0, ReaderTerm(rLine)),

    // Guard: known(Input)
    BC.known(0),

    BC.COMMIT(),

    // Body of wait_then_echo
    SetClauseVar(1, ConstTerm('You entered: ')),
    Execute('write', [1]),

    // Write Input (clause var 0 is ReaderTerm(rLine))
    Execute('write', [0]),

    Execute('nl', []),

    BC.PROCEED(),
  ]);

  // Create runner
  final runner = BytecodeRunner(program);
  final goalId = 1;

  final env = CallEnv();
  rt.setGoalEnv(goalId, env);
  rt.setGoalProgram(goalId, 'echo');

  final cx = RunnerContext(
    rt: rt,
    goalId: goalId,
    kappa: 0,
    env: env,
  );

  print('Running GLP echo program...\n');

  final result = runner.runWithStatus(cx);

  print('\n\nProgram terminated with result: $result');
}
