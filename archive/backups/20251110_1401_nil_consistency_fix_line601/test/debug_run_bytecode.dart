import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = 'run((A, B)) :- run(A?), run(B?).';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode for run/1:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    print('  $i: $op');
  }
}
