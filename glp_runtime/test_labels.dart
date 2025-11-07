import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  final compiler = GlpCompiler();
  
  // Load merge program
  final mergeSource = '''
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
''';
  
  final mergeProgram = compiler.compile(mergeSource);
  
  // Compile query wrapper
  final wrappedQuery = 'query_wrapper :- merge([1,2,3], [a,b], Xs).';
  final queryProgram = compiler.compile(wrappedQuery);
  
  // Combine programs
  final allOps = [...mergeProgram.ops, ...queryProgram.ops];
  final combined = BytecodeProgram(allOps);
  
  print('Labels in combined program:');
  for (final entry in combined.labels.entries) {
    print('  ${entry.key} -> PC ${entry.value}');
  }
}
