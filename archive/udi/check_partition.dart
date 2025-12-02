import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = '''
partition ([X | Xs], A, Smaller?, [X? | Larger?]) :-
    A? < X? | partition (Xs?, A?, Smaller, Larger).
partition ([], _, [], []).
  ''';
  
  final result = GlpCompiler().compile(source);
  
  print('=== First partition clause ===');
  var i = 0;
  for (final op in result.ops) {
    print('$i: $op');
    i++;
    if (op.toString().contains('Commit')) break;
  }
}
