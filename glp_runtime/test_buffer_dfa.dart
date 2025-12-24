import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/clause_contribution.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  final typeSource = '''
    Buffer ::= [] ; [_? | Buffer].
    procedure producer(Number?, Buffer).
  ''';

  final typeEnv = parseTypes(typeSource);
  final compiler = TypeCompiler(typeEnv);
  final bufferDFA = compiler.compile('Buffer');

  print('=== Buffer DFA (Declared Type) ===');
  print('Start state: ${bufferDFA.startState.name}');
  print('\nTransitions:');
  for (final entry in bufferDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${pathElem.symbol} mode=${pathElem.mode}]--> ${to.name}');
  }

  print('\nPrimitive state modes:');
  for (final entry in bufferDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }

  print('\nFinal states:');
  for (final state in bufferDFA.finalStates) {
    print('  ${state.name}');
  }

  // Now check the clause contribution for producer
  print('\n=== Producer Clause Contribution ===');
  final clauseSource = 'producer(N, [N? | Xs]).';
  final lexer = Lexer(clauseSource);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final clause = program.procedures.first.clauses.first;
  final pattern = clause.head.args[1]; // Second argument: [N? | Xs]

  print('Pattern: $pattern');
  print('Pattern type: ${pattern.runtimeType}');

  if (pattern is ast.ListTerm) {
    print('  head: ${pattern.head} (${pattern.head.runtimeType})');
    if (pattern.head is ast.VarTerm) {
      print('    isReader: ${(pattern.head as ast.VarTerm).isReader}');
    }
    print('  tail: ${pattern.tail} (${pattern.tail.runtimeType})');
    if (pattern.tail is ast.VarTerm) {
      print('    isReader: ${(pattern.tail as ast.VarTerm).isReader}');
    }
  }

  // Compute clause contribution
  final contributionComputer = ClauseContributionComputer(typeEnv);
  final varTypes = {'N': compiler.compile('Number'), 'Xs': bufferDFA};
  final contributionDFA = contributionComputer.computeArgContribution(
    pattern,
    varTypes,
    bufferDFA,
  );

  print('\n=== Clause Contribution DFA ===');
  print('Transitions:');
  for (final entry in contributionDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${pathElem.symbol} mode=${pathElem.mode}]--> ${to.name}');
  }

  print('\nPrimitive state modes:');
  for (final entry in contributionDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }

  print('\n=== Comparison ===');
  print('Are DFAs equivalent: ${contributionDFA.isEquivalent(bufferDFA)}');
  print('Is contribution subset of declared: ${contributionDFA.isSubsetOf(bufferDFA)}');
  print('Is declared subset of contribution: ${bufferDFA.isSubsetOf(contributionDFA)}');
}
