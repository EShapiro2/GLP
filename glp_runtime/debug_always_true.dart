import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/clause_contribution.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/nfa_to_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/moded_label.dart';
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

  // Check subset - detailed debugging
  print('=== Subset Check Details ===');

  // Step 1: Compute complement of inferredDFA
  print('Step 1: Computing complement of inferredDFA...');
  final inferredComplement = inferredDFA.modedComplement();
  print('Inferred Complement DFA:');
  print(inferredComplement);
  print('');

  // Step 2: Compute intersection (declared ∩ inferred̄)
  print('Step 2: Computing intersection (declared ∩ inferredComplement)...');
  final intersection = declaredDFA.modedIntersect(inferredComplement);
  print('Intersection DFA:');
  print(intersection);
  print('');

  // Step 3: Check if intersection is empty
  print('Step 3: Checking if intersection is empty...');
  print('intersection.isModedEmpty = ${intersection.isModedEmpty}');
  print('');

  // Step 4: Final result
  print('Step 4: Final isSubsetOf result...');
  print('declaredDFA.isSubsetOf(inferredDFA) = ${declaredDFA.isSubsetOf(inferredDFA)}');
  print('');

  // Additional: Check what paths each DFA accepts
  print('=== Path Acceptance Tests ===');
  final truePath = [ModedLabel.constant('true')];
  final falsePath = [ModedLabel.constant('false')];

  print('declaredDFA.acceptsStructuralPath([true]) = ${declaredDFA.acceptsStructuralPath(truePath)}');
  print('declaredDFA.acceptsStructuralPath([false]) = ${declaredDFA.acceptsStructuralPath(falsePath)}');
  print('inferredDFA.acceptsStructuralPath([true]) = ${inferredDFA.acceptsStructuralPath(truePath)}');
  print('inferredDFA.acceptsStructuralPath([false]) = ${inferredDFA.acceptsStructuralPath(falsePath)}');
  print('inferredComplement.acceptsStructuralPath([true]) = ${inferredComplement.acceptsStructuralPath(truePath)}');
  print('inferredComplement.acceptsStructuralPath([false]) = ${inferredComplement.acceptsStructuralPath(falsePath)}');
  print('intersection.acceptsStructuralPath([true]) = ${intersection.acceptsStructuralPath(truePath)}');
  print('intersection.acceptsStructuralPath([false]) = ${intersection.acceptsStructuralPath(falsePath)}');
}
