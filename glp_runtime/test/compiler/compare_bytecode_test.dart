import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;

void main() {
  test('Compare compiled vs hand-written bytecode', () {
    print('\n=== BYTECODE COMPARISON ===\n');

    // Compile
    final compiler = GlpCompiler();
    final source = '''
      p(X) :- q(X?).
      q(a).
    ''';
    final program = compiler.compile(source);

    print('COMPILED BYTECODE:');
    for (int i = 0; i < program.ops.length; i++) {
      final op = program.ops[i];
      print('$i: $op');
    }

    print('\nLABELS:');
    program.labels.forEach((k, v) => print('  $k => $v'));

    // Now show what hand-written would look like
    print('\n\nEXPECTED HAND-WRITTEN EQUIVALENT:');
    print('0: Label(p/1)');
    print('1: ClauseTry()');
    print('2: GetVariable(0, 0)  // X from arg0');
    print('3: Commit()');
    print('4: PutReader(0, 0)    // arg0 = X?');
    print('5: Requeue(q/1, 1)    // tail call q(X?)');
    print('6: Label(p/1_end)');
    print('7: NoMoreClauses()');
    print('8: Label(q/1)');
    print('9: ClauseTry()');
    print('10: HeadConstant(a, 0)');
    print('11: Commit()');
    print('12: Proceed()');
    print('13: Label(q/1_end)');
    print('14: NoMoreClauses()');

    // Verify key instructions
    expect(program.ops[0], isA<bc.Label>());
    expect(program.ops[1], isA<bc.ClauseTry>());
    expect(program.ops[2], isA<bc.GetVariable>());
    expect(program.ops[3], isA<bc.Commit>());
    expect(program.ops[4], isA<bc.PutReader>());
    expect(program.ops[5], isA<bc.Requeue>());

    // Check the Requeue has correct label
    final requeue = program.ops[5] as bc.Requeue;
    print('\nRequeue instruction:');
    print('  procedureLabel: ${requeue.procedureLabel}');
    print('  arity: ${requeue.arity}');

    expect(requeue.procedureLabel, 'q/1');
    expect(requeue.arity, 1);
  });
}
