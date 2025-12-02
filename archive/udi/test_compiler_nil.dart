import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final compiler = GlpCompiler();

  // Compile a simple clause with empty list in Execute
  final source = 'test :- run(merge([],[],X)).';
  final program = compiler.compile(source);

  print('Compiled bytecode for: $source');
  for (int i = 0; i < program.ops.length; i++) {
    print('  $i: ${program.ops[i]}');
  }
}
