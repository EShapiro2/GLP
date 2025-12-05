import 'package:glp_runtime/compiler/compiler.dart';

void main(List<String> args) {
  final src = args[0];
  final compiler = GlpCompiler();
  final prog = compiler.compile(src);
  
  for (int i = 0; i < prog.ops.length; i++) {
    print('  $i: ${prog.ops[i]}');
  }
  print('\nLabels:');
  for (final entry in prog.labels.entries) {
    print('  ${entry.key} -> PC ${entry.value}');
  }
}
