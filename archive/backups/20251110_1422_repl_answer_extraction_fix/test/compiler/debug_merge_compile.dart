import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  test('Debug merge clause compilation', () {
    final source = '''
      clause(merge([X|Xs], Ys, [X?|Zs?]), merge(Ys?, Xs?, Zs)).
    ''';

    final compiler = GlpCompiler();
    final program = compiler.compile(source);

    print('\n=== COMPILED BYTECODE ===');
    for (int i = 0; i < program.ops.length; i++) {
      final op = program.ops[i];
      final opStr = op.toString();
      print('$i: $opStr');
    }

    print('\n=== HAND-WRITTEN EXPECTED (excerpt) ===');
    print('HeadStructure(merge, 3, 0)');
    print('  UnifyWriter(10) // extract 1st arg to temp');
    print('  UnifyWriter(2)  // Ys');
    print('  UnifyWriter(11) // extract 3rd arg to temp');
    print('HeadStructure([|], 2, 10) // match extracted list');
    print('  UnifyWriter(0)  // X');
    print('  UnifyWriter(1)  // Xs');
    print('HeadStructure([|], 2, 11) // match extracted list');
    print('  UnifyReader(0)  // X?');
    print('  UnifyReader(3)  // Zs?');
    print('HeadStructure(merge, 3, 1) // 2nd head arg');
    print('  UnifyReader(2)  // Ys?');
    print('  UnifyReader(1)  // Xs?');
    print('  UnifyWriter(3)  // Zs');
  });
}
