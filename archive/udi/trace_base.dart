import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = '''
qsort([], Rest?, Rest).
qsort([X | Unsorted], Sorted?, Rest?) :-
  number(X?) |
  partition(Unsorted?, X?, Smaller, Larger),
  qsort(Smaller?, Sorted, [X? | Sorted1?]),
  qsort(Larger?, Sorted1, Rest).

partition([X | Xs], A, Smaller?, [X? | Larger?]) :-
  A? < X? | partition(Xs?, A?, Smaller, Larger).
partition([X | Xs], A, [X? | Smaller?], Larger?) :-
  A? >= X? | partition(Xs?, A?, Smaller, Larger).
partition([], _, [], []).
  ''';
  
  final program = GlpCompiler().compile(source);
  final rt = Runtime(program);
  
  // Enable debug for specific goals
  rt.debugGoalIds.add(10003); // The failing qsort([], W1013, [])
  
  rt.query('qsort', [
    StructTerm('.', [ConstTerm(1), ConstTerm('nil')]),
  ], numExpectedResults: 1);
  
  print('\nFinal result: ${rt.results}');
}
