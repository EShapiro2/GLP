import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';

void main() {
  final typeSource = '''
    InvStream ::= [] ; [_? | InvStream].
  ''';

  final typeEnv = parseTypes(typeSource);
  final compiler = TypeCompiler(typeEnv);
  final invStreamDFA = compiler.compile('InvStream');

  print('=== InvStream DFA ===');
  print('Start state: ${invStreamDFA.startState.name}');
  print('\nTransitions:');
  for (final entry in invStreamDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${pathElem.symbol} mode=${pathElem.mode}]--> ${to.name}');
  }

  print('\nPrimitive state modes:');
  for (final entry in invStreamDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }

  print('\nFinal states:');
  for (final state in invStreamDFA.finalStates) {
    print('  ${state.name}');
  }
}
