import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/clause_contribution.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  // Parse the type
  final typeSource = 'MyList ::= [] ; [_ | MyList].';
  final typeEnv = parseTypes(typeSource);
  final compiler = TypeCompiler(typeEnv);
  final declaredDFA = compiler.compile('MyList');

  print('=== DECLARED TYPE: MyList ===');
  print('Transitions:');
  for (final entry in declaredDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem]--> ${to.name}');
  }
  print('Primitive state modes:');
  for (final entry in declaredDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }

  // Parse the clause pattern [_ | Xs]
  final clauseSource = 'test([_ | Xs]).';
  final lexer = Lexer(clauseSource);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final clause = program.procedures.first.clauses.first;
  final pattern = clause.head.args.first;

  print('\n=== CLAUSE PATTERN: [_ | Xs] ===');
  print('Pattern type: ${pattern.runtimeType}');
  if (pattern is ast.ListTerm) {
    print('  head: ${pattern.head} (${pattern.head.runtimeType})');
    print('  tail: ${pattern.tail} (${pattern.tail.runtimeType})');
  }

  // Compute clause contribution
  final contributionComputer = ClauseContributionComputer(typeEnv);
  final varTypes = {'Xs': declaredDFA};  // Xs has type MyList
  final contributionDFA = contributionComputer.computeArgContribution(
    pattern,
    varTypes,
    declaredDFA,
  );

  print('\n=== CLAUSE CONTRIBUTION DFA ===');
  print('Transitions:');
  for (final entry in contributionDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem]--> ${to.name}');
  }
  print('Primitive state modes:');
  for (final entry in contributionDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }

  // Check if they're equal
  print('\n=== COMPARISON ===');
  print('Are DFAs structurally equal: ${contributionDFA == declaredDFA}');

  // Also test standard List
  print('\n\n=== STANDARD LIST TYPE ===');
  final listDFA = compiler.compile('List');
  print('List transitions:');
  for (final entry in listDFA.transitions.entries) {
    final (from, pathElem) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[$pathElem]--> ${to.name}');
  }
  print('List primitive state modes:');
  for (final entry in listDFA.primitiveStateModes.entries) {
    print('  ${entry.key.name}: ${entry.value}');
  }
}
