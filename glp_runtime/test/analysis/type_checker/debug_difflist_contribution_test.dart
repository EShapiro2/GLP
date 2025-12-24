import 'package:test/test.dart';
import 'test_helpers.dart';
import '../../../lib/analysis/type_checker/type_compiler.dart';
import '../../../lib/analysis/type_checker/type_parser.dart';
import '../../../lib/analysis/type_checker/type_dfa.dart';
import '../../../lib/analysis/type_checker/clause_contribution.dart';
import '../../../lib/compiler/lexer.dart';
import '../../../lib/compiler/parser.dart';

void main() {
  test('DEBUG: DiffList clause contribution', () {
    final source = r'''
      MyEvery ::= _ ; _?.
      MyAny ::< MyEvery.
      MyList ::= [MyAny | MyList] ; [].
      MyDiffList ::= MyList \ MyList?.

      procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
      my_dl_append(A\B?, B\C?, A?\C).
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
    print('All transitions:');
    for (final entry in diffListDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      print('  ${fromState.name} --[$pathElem]--> ${entry.value.name}');
    }
    print('');
    print('Primitive state modes:');
    for (final entry in diffListDFA.primitiveStateModes.entries) {
      print('  ${entry.key.name}: ${entry.value}');
    }
    print('');

    // Parse the clause
    final clauseLines = source.split('\n').where((line) {
      final trimmed = line.trim();
      return trimmed.startsWith('my_dl_append') &&
             !trimmed.startsWith('procedure');
    }).join('\n');

    final lexer = Lexer(clauseLines);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final program = parser.parse();
    final clause = program.procedures[0].clauses[0];

    print('=== Clause ===');
    print('Head: ${clause.head}');
    print('Number of args: ${clause.head.args.length}');
    print('');

    // Examine each argument pattern
    for (var i = 0; i < clause.head.args.length; i++) {
      final arg = clause.head.args[i];
      print('Argument $i: $arg');
      print('  Type: ${arg.runtimeType}');
      if (arg.toString().contains(r'\')) {
        print('  Pattern contains backslash: ${arg}');
      }
      print('');
    }

    // First, infer variable types from the pattern
    print('=== Inferring Variable Types ===');
    final pattern0 = clause.head.args[0];
    print('Pattern: $pattern0');
    print('Pattern type: ${pattern0.runtimeType}');

    // We need to call _inferVariableTypes, but it's private
    // Instead, let's examine what types the type checker assigned
    // For now, let's manually create what should be inferred:
    // A and B should both be MyList based on DiffList structure
    final myListDFA = compiler.compile('MyList');

    print('');
    print('MyList DFA (expected type for A and B):');
    print('  Start state: ${myListDFA.startState}');
    print('  Transitions: ${myListDFA.transitions.length}');
    print('');

    // Compute contribution for first argument
    print('=== Computing Contribution for Arg 0 ===');
    try {
      final contributionComputer = ClauseContributionComputer(typeEnv);

      // Simulate what _inferVariableTypes would assign
      final varTypes = <String, TypeDFA>{
        'A': myListDFA,
        'B': myListDFA,
      };

      print('VarTypes:');
      for (final entry in varTypes.entries) {
        print('  ${entry.key}: ${entry.value.startState.name}');
      }
      print('');

      final contribution0 = contributionComputer.computeArgContribution(
        pattern0,
        varTypes,
        diffListDFA,
      );

      print('');
      print('Contribution DFA:');
      print('  Start state: ${contribution0.startState}');
      print('  States: ${contribution0.states.map((s) => s.name).toList()}');
      print('  Final states: ${contribution0.finalStates.map((s) => s.name).toList()}');
      print('');
      print('  Transitions:');
      for (final entry in contribution0.transitions.entries) {
        final (fromState, pathElem) = entry.key;
        print('    ${fromState.name} --[$pathElem]--> ${entry.value.name}');
      }
      print('');
      print('  Primitive state modes:');
      for (final entry in contribution0.primitiveStateModes.entries) {
        print('    ${entry.key.name}: ${entry.value}');
      }
      print('');

      // Check if it's a subset of declared type
      final isSubset = contribution0.isSubsetOf(diffListDFA);
      print('Is contribution ⊆ declared DFA? $isSubset');

    } catch (e, stack) {
      print('ERROR computing contribution: $e');
      print('Stack trace: $stack');
    }

    // Run type check to see actual error
    final result = checkTypes(source);
    print('');
    print('=== Type Check Result ===');
    print('isWellTyped: ${result.isWellTyped}');
    for (final error in result.errors) {
      print('ERROR: $error');
    }
  });
}
