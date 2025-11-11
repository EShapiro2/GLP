import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = 'merge2(Xs, Zs?) :- merge(Xs?,[a],Ys), merge(Ys?,[b],Zs).';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode for merge2:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    String details = '';

    final opStr = op.toString();
    if (opStr.contains('Put') || opStr.contains('Spawn') || opStr.contains('Requeue')) {
      details = ' $op';
    }

    print('  $i: ${op.runtimeType}$details');
  }
}
