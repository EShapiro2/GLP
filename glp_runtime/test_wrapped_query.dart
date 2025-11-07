import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final compiler = GlpCompiler();
  final wrappedQuery = 'query_wrapper :- merge([1,2,3], [a,b], Xs).';
  final result = compiler.compileWithMetadata(wrappedQuery);

  print('Variable map: ${result.variableMap}');
  print('Labels: ${result.program.labels}');
  print('');
  print('Bytecode:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    print('  $i: $op');
  }
}
