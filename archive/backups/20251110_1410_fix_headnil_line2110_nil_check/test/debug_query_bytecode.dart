import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = 'query(Xs1?, Ys?) :- run((merge([],[],Xs1), merge([],Xs1?,Ys))).';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode for query/2:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    print('  $i: $op');
  }
}
