import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final compiler = GlpCompiler();
  
  // Load merge program
  final mergeSource = '''
% Clause 1: Take from first stream
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).

% Clause 2: Take from second stream
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).

% Clause 3: Base case - both empty
merge([], [], []).
''';
  
  final mergeProgram = compiler.compile(mergeSource);
  
  // Compile query wrapper
  final wrappedQuery = 'query_wrapper :- merge([1,2,3], [a,b], Xs).';
  final queryProgram = compiler.compile(wrappedQuery);
  
  // Combine programs
  final allOps = [...mergeProgram.ops, ...queryProgram.ops];
  
  print('Combined program has ${allOps.length} instructions');
  print('');
  
  // Show labels
  for (int i = 0; i < allOps.length; i++) {
    final op = allOps[i];
    if (op.toString().contains('Label')) {
      print('  $i: $op');
    }
  }
}
