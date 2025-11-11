import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = 'merge([],[],[]).';
  
  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);
  
  print('=== merge([],[],[]) ===');
  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    print('  $i: ${op.runtimeType} - $op');
  }
  
  print('\nLabels: ${result.program.labels}');
}
