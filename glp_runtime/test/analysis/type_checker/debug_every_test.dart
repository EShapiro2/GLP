// Temporary debug test for MyEvery equivalence issue
import 'package:test/test.dart';
import 'test_helpers.dart';
import '../../../lib/analysis/type_checker/type_compiler.dart';
import '../../../lib/analysis/type_checker/type_parser.dart';

void main() {
  test('DEBUG: MyEvery vs MyAny', () {
    print('\n=== TEST 1: MyEvery (self-dual with exact type) ===');
    final source1 = '''
      MyEvery ::= _ ; _?.
      procedure foo(MyEvery, MyEvery?).
      foo(X, X?).
      foo(X?, X).
    ''';

    final typeEnv1 = parseTypes(source1);
    final compiler1 = TypeCompiler(typeEnv1);
    final myEveryDFA = compiler1.compile('MyEvery');

    print('MyEvery DFA:');
    print('  Start state: ${myEveryDFA.startState}');
    print('  Is primitive: ${myEveryDFA.isPrimitiveState(myEveryDFA.startState)}');
    print('  Modes at start: ${myEveryDFA.getModesAt(myEveryDFA.startState)}');
    print('  Bi-moded: ${myEveryDFA.isPrimitiveState(myEveryDFA.startState) && myEveryDFA.getModesAt(myEveryDFA.startState).length == 2}');

    final result1 = checkTypes(source1);
    print('\nType check result:');
    print('  isWellTyped: ${result1.isWellTyped}');
    for (final error in result1.errors) {
      print('  ERROR: $error');
    }

    print('\n=== TEST 2: MyAny (subtype of MyEvery) ===');
    final source2 = '''
      MyEvery ::= _ ; _?.
      MyAny ::< MyEvery.
      procedure foo(MyAny, MyAny?).
      foo(X, X?).
    ''';

    final typeEnv2 = parseTypes(source2);
    final compiler2 = TypeCompiler(typeEnv2);
    final myAnyDFA = compiler2.compile('MyAny');

    print('MyAny DFA:');
    print('  Start state: ${myAnyDFA.startState}');
    print('  Is primitive: ${myAnyDFA.isPrimitiveState(myAnyDFA.startState)}');
    print('  Modes at start: ${myAnyDFA.getModesAt(myAnyDFA.startState)}');
    print('  Bi-moded: ${myAnyDFA.isPrimitiveState(myAnyDFA.startState) && myAnyDFA.getModesAt(myAnyDFA.startState).length == 2}');

    final result2 = checkTypes(source2);
    print('\nType check result:');
    print('  isWellTyped: ${result2.isWellTyped}');
    for (final error in result2.errors) {
      print('  ERROR: $error');
    }

    print('\n=== TEST 3: Actual predefined_operations_test case ===');
    final source3 = '''
      MyEvery ::= _ ; _?.
      MyAny ::< MyEvery.
      procedure foo(MyAny, MyAny?).
      foo(X, X?).
    ''';

    final result3 = checkTypes(source3);
    print('Type check result (same as TEST 2):');
    print('  isWellTyped: ${result3.isWellTyped}');
    for (final error in result3.errors) {
      print('  ERROR: $error');
    }
  });
}
