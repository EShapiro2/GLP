import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Compare: simple_body_test.dart hand-written vs compiled', () {
    print('\n' + '=' * 70);
    print('BYTECODE COMPARISON: simple_body_test.dart');
    print('=' * 70 + '\n');

    // HAND-WRITTEN bytecode from simple_body_test.dart
    print('HAND-WRITTEN BYTECODE:');
    print('Source: p(a). forward(X) :- p(X). [SRSW violation - X appears twice]');
    print('');
    final handWritten = BC.prog([
      // p(a).
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),

      // forward(X) :- p(X).
      BC.L('forward/1'),
      BC.TRY(),
      BC.getVar(0, 0),            // Get X from arg0 into clause var 0
      BC.COMMIT(),
      // BODY:
      BC.putWriter(0, 0),         // arg0 = X (writer)
      BC.requeue('p/1', 1),       // Tail call p(X)

      BC.L('forward/1_end'),
      BC.SUSP(),
    ]);

    for (int i = 0; i < handWritten.ops.length; i++) {
      print('  $i: ${handWritten.ops[i]}');
    }
    print('\nHAND-WRITTEN LABELS:');
    handWritten.labels.forEach((k, v) => print('  $k => $v'));

    // COMPILED bytecode (SRSW-compliant version)
    print('\n' + '-' * 70);
    print('COMPILED BYTECODE (SRSW-compliant):');
    print('Source: p(a). forward(X) :- p(X?). [Uses reader in body]');
    print('');

    final compiler = GlpCompiler();
    final source = '''
      p(a).
      forward(X) :- p(X?).
    ''';
    final compiled = compiler.compile(source);

    for (int i = 0; i < compiled.ops.length; i++) {
      print('  $i: ${compiled.ops[i]}');
    }
    print('\nCOMPILED LABELS:');
    compiled.labels.forEach((k, v) => print('  $k => $v'));

    // COMPARISON
    print('\n' + '=' * 70);
    print('COMPARISON');
    print('=' * 70);
    print('');
    print('p/1 clause:');
    print('  Hand-written: TRY, headConst(a,0), COMMIT, PROCEED');
    print('  Compiled:     TRY, HeadConstant(a,0), Commit, Proceed');
    print('  ✓ MATCH (just different case/naming)');
    print('');
    print('forward/1 clause:');
    print('  Hand-written: TRY, getVar(0,0), COMMIT, putWriter(0,0), requeue(p/1,1)');
    print('  Compiled:     TRY, GetVariable(0,0), Commit, PutReader(0,0), Requeue(p/1,1)');
    print('  ✗ DIFFERENCE: PutWriter vs PutReader');
    print('');
    print('Reason for difference:');
    print('  Hand-written source "forward(X) :- p(X)." violates SRSW (X twice)');
    print('  This uses PutWriter to forward the same writer variable');
    print('  ');
    print('  SRSW-compliant source "forward(X) :- p(X?)." uses reader');
    print('  This uses PutReader to pass the paired reader');
    print('');
    print('Semantic difference:');
    print('  PutWriter: passes writer to callee, callee can bind it');
    print('  PutReader: passes reader to callee, callee reads from writer');
    print('');
    print('Conclusion:');
    print('  Hand-written test uses VM feature (writer forwarding) that');
    print('  cannot be expressed in valid SRSW GLP source.');
    print('  The closest SRSW-compliant version has different semantics.');
    print('');

    // Verify the key difference
    final handForwardLabel = handWritten.labels['forward/1']!;
    final compForwardLabel = compiled.labels['forward/1']!;

    final handBodyInstr = handWritten.ops[handForwardLabel + 4];
    final compBodyInstr = compiled.ops[compForwardLabel + 4];

    expect(handBodyInstr, isA<bc.PutWriter>());
    expect(compBodyInstr, isA<bc.PutReader>());

    print('✓ Verified: hand-written uses PutWriter, compiled uses PutReader\n');
  });
}
