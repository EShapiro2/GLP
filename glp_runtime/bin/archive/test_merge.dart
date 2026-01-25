import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final compiler = GlpCompiler();
  final program = compiler.compile("""
    merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
    merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
    merge([], [], []).
  """);
  
  print('=== Bytecode for merge/3 ===');
  for (int i = 0; i < program.ops.length; i++) {
    final op = program.ops[i];
    print('$i: $op');
  }
}
