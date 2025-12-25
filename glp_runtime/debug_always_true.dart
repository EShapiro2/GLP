import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/clause_contribution.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_to_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/lexer.dart' as glp;
import 'package:glp_runtime/compiler/parser.dart' as glp;

void main() {
  // Type definitions (for type checker)
  final typeSource = '''
Bool ::= true ; false.

procedure always_true(Nat?, Bool).
always_true(X, true) :- ground(X?) | true.
''';

  // GLP source (for parser)
  final glpSource = '''
always_true(X, true) :- ground(X?) | true.
''';

  // Parse types
  final typeEnv = parseTypes(typeSource);

  // Compile declared Bool DFA
  final compiler = TypeCompiler(typeEnv);
  final declaredDFA = compiler.compile('Bool');

  print('=== Declared DFA (Bool) ===');
  print(declaredDFA);
  print('');

  // Parse clause to get the pattern
  final glpLexer = glp.Lexer(glpSource);
  final glpTokens = glpLexer.tokenize();
  final glpParser = glp.Parser(glpTokens);
  final program = glpParser.parse();

  // Find the always_true clause
  final clause = program.procedures
      .firstWhere((p) => p.name == 'always_true')
      .clauses
      .first;

  print('=== Clause ===');
  print('Head: ${clause.head}');
  print('Arg 2: ${clause.head.args[1]}');
  print('Arg 2 type: ${clause.head.args[1].runtimeType}');
  print('');

  // Build inferred DFA for argument 2
  final contributionComputer = ClauseContributionComputer(typeEnv);
  final pattern = clause.head.args[1];

  // Convert pattern to type expression
  final typeExpr = contributionComputer.patternToTypeExpr(
    pattern,
    <String, String>{}, // empty varTypeNames
    false, // declaredIsInput = false (output arg)
  );
  print('=== Type Expression from Pattern ===');
  print('TypeExpr: $typeExpr');
  print('TypeExpr type: ${typeExpr.runtimeType}');
  print('');

  // Compile to NFA then DFA
  final nfaCompiler = TypeNFACompiler(typeEnv);
  final nfa = nfaCompiler.compileExpr(typeExpr);
  print('=== NFA ===');
  print(nfa);
  print('');

  final dfaConverter = NFAToDFAConverter(nfa);
  final inferredDFA = dfaConverter.convert();
  print('=== Inferred DFA ===');
  print(inferredDFA);
  print('');

  // Check subset
  print('=== Subset Check ===');
  print('declaredDFA.isSubsetOf(inferredDFA) = ${declaredDFA.isSubsetOf(inferredDFA)}');
  print('inferredDFA.isSubsetOf(declaredDFA) = ${inferredDFA.isSubsetOf(declaredDFA)}');

  // Check if inferredDFA start state is primitive
  print('');
  print('=== Start State Analysis ===');
  print('inferred start: ${inferredDFA.startState}');
  print('inferred isPrimitive(start): ${inferredDFA.isPrimitiveState(inferredDFA.startState)}');
  print('inferred modes at start: ${inferredDFA.getModesAt(inferredDFA.startState)}');
}
