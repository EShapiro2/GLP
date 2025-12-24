import 'package:test/test.dart';
import 'test_helpers.dart';
import '../../../lib/analysis/type_checker/type_compiler.dart';
import '../../../lib/analysis/type_checker/type_parser.dart';

void main() {
  test('DEBUG: DiffList DFA structure', () {
    final source = '''
      MyEvery ::= _ ; _?.
      MyAny ::< MyEvery.
      MyList ::= [MyAny | MyList] ; [].
      MyDiffList ::= MyList \\ MyList?.

      procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
      my_dl_append(A\\B?, B\\C?, A?\\C).
    ''';

    final typeEnv = parseTypes(source);
    final compiler = TypeCompiler(typeEnv);

    // Compile MyDiffList
    final diffListDFA = compiler.compile('MyDiffList');

    print('=== MyDiffList DFA ===');
    print('Start state: ${diffListDFA.startState}');
    print('States: ${diffListDFA.states.map((s) => s.name).toList()}');
    print('Final states: ${diffListDFA.finalStates.map((s) => s.name).toList()}');
    print('');
    print('Transitions from start:');
    for (final entry in diffListDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == diffListDFA.startState) {
        print('  $pathElem -> ${entry.value.name}');
      }
    }
    print('');
    print('Primitive state modes:');
    for (final entry in diffListDFA.primitiveStateModes.entries) {
      print('  ${entry.key.name}: ${entry.value}');
    }
    print('');

    // Run type check
    final result = checkTypes(source);
    print('=== Type Check Result ===');
    print('isWellTyped: ${result.isWellTyped}');
    for (final error in result.errors) {
      print('ERROR: $error');
    }
  });
}
