import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = '''
    clause(p(X?), (q(Y), r(Y?,X))).
  ''';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode for: clause(p(X?), (q(Y), r(Y?,X))).');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    print('  $i: $op');
  }
}
