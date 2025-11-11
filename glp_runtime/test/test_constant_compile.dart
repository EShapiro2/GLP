import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final compiler = GlpCompiler();

  print('=== Testing: test_nil([]) ===');
  final result = compiler.compile('test_nil([]).');
  print('Bytecode:');
  for (int i = 0; i < result.ops.length; i++) {
    print('  $i: ${result.ops[i]}');
  }
}
