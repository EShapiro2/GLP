import 'test/analysis/type_checker/test_helpers.dart';

void main() {
  print('=== Complete list procedure test ===');

  final result = checkTypes('''
    MyList ::= [] ; [_ | MyList].
    procedure length(MyList?, Number).
    length([], 0).
    length([_ | Xs], N) :- length(Xs?, M), N := M? + 1.
  ''');

  print('\nErrors: ${result.errors.length}');
  for (final error in result.errors) {
    print('  - $error');
  }

  print('\nWarnings: ${result.warnings.length}');
  for (final warning in result.warnings) {
    print('  - $warning');
  }

  print('\nWell-typed: ${result.isWellTyped}');
}
