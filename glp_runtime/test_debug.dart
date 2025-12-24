import 'test/analysis/type_checker/test_helpers.dart';

void main() {
  // Test the basic List case
  final result = checkTypes('''
    MyList ::= [] ; [_ | MyList].
    procedure length(MyList?, Number).
    length([], 0).
    length([_ | Xs], N) :- length(Xs?, M), N := M? + 1.
  ''');

  print('=== BASIC LIST TEST ===');
  print('Is well-typed: ${result.isWellTyped}');
  print('Errors (${result.errors.length}):');
  for (final error in result.errors) {
    print('  - $error');
  }

  // Test the nested Any case
  final result2 = checkTypes('''
procedure process_list(List).

process_list([]).
process_list([H? | T]) :- process_list(T?).
''');

  print('\n=== NESTED ANY TEST ===');
  print('Is well-typed: ${result2.isWellTyped}');
  print('Errors (${result2.errors.length}):');
  for (final error in result2.errors) {
    print('  - $error');
  }
}
