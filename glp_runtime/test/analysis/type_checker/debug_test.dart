// Quick debug test to see what errors are reported

import 'test_helpers.dart';

void main() {
  final result = checkTypes('''
    MyList ::= [] ; [_ | MyList].
    procedure length(MyList?, Number).
    length([], 0).
    length([_ | Xs], N) :- length(Xs?, M), N := M? + 1.
  ''');
  print('isWellTyped: ${result.isWellTyped}');
  print('errors: ${result.errors}');
  print('warnings: ${result.warnings}');
}
