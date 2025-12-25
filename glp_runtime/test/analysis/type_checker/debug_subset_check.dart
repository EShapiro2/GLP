import 'package:test/test.dart';
import 'test_helpers.dart';
import '../../../lib/analysis/type_checker/type_compiler.dart';
import '../../../lib/analysis/type_checker/type_parser.dart';
import '../../../lib/analysis/type_checker/type_dfa.dart';
import '../../../lib/analysis/type_checker/clause_contribution.dart';
import '../../../lib/compiler/lexer.dart';
import '../../../lib/compiler/parser.dart';

void main() {
  test('DEBUG: Subset check for DiffList', () {
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

    // Compile declared and contribution DFAs
    final declaredDFA = compiler.compile('MyDiffList');
    final myListDFA = compiler.compile('MyList');

    // Build contribution DFA for pattern \(A, B?)
    final contributionComputer = ClauseContributionComputer(typeEnv);
    final varTypes = <String, TypeDFA>{
      'A': myListDFA,
      'B': myListDFA,
    };

    // Parse the pattern
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
    final pattern0 = clause.head.args[0];

    final contributionDFA = contributionComputer.computeArgContribution(
      pattern0,
      varTypes,
      declaredDFA,
    );

    print('=== Declared DFA ===');
    print('Alphabet: ${declaredDFA.alphabet.map((e) => e.toString()).toList()}');
    print('');

    print('=== Contribution DFA ===');
    print('Alphabet: ${contributionDFA.alphabet.map((e) => e.toString()).toList()}');
    print('');

    // Now trace through subset check
    print('=== Subset Check: contribution ⊆ declared ===');
    print('');

    // Step 1: Complement of declared DFA
    print('Step 1: Computing complement of declared DFA');
    print('  Original declared primitive modes:');
    for (final entry in declaredDFA.primitiveStateModes.entries) {
      print('    ${entry.key.name}: ${entry.value}');
    }
    final declaredComplement = declaredDFA.modedComplement();
    print('  Complement states: ${declaredComplement.states.map((s) => s.name).toList()}');
    print('  Complement final states: ${declaredComplement.finalStates.map((s) => s.name).toList()}');
    print('  Complement primitive modes (${declaredComplement.primitiveStateModes.length} entries):');
    if (declaredComplement.primitiveStateModes.isEmpty) {
      print('    (NONE)');
    } else {
      for (final entry in declaredComplement.primitiveStateModes.entries) {
        print('    ${entry.key.name}: ${entry.value.isEmpty ? "(empty set)" : entry.value}');
      }
    }
    print('');

    // Step 2: Intersection
    print('Step 2: Computing intersection of contribution ∩ complement');
    final intersection = contributionDFA.intersect(declaredComplement);
    print('  Intersection start: ${intersection.startState.name}');
    print('  Intersection states: ${intersection.states.map((s) => s.name).toList()}');
    print('  Intersection final states: ${intersection.finalStates.map((s) => s.name).toList()}');
    print('  Intersection transitions:');
    for (final entry in intersection.transitions.entries) {
      final (from, pathElem) = entry.key;
      print('    ${from.name} --[$pathElem]--> ${entry.value.name}');
    }
    print('  Intersection primitive modes (${intersection.primitiveStateModes.length} entries):');
    if (intersection.primitiveStateModes.isEmpty) {
      print('    (NONE)');
    } else {
      for (final entry in intersection.primitiveStateModes.entries) {
        print('    ${entry.key.name}: ${entry.value.isEmpty ? "(empty set)" : entry.value}');
      }
    }
    print('');

    // Step 3: Check if intersection is empty
    print('Step 3: Check if intersection is empty (moded)');
    final isEmpty = intersection.isModedEmpty;
    print('  Result: $isEmpty');
    print('');

    if (!isEmpty) {
      print('ERROR: Intersection is NOT empty, so subset check fails!');
      print('This means contribution accepts something that declared complement accepts.');
      print('In other words, contribution is NOT a subset of declared.');
    } else {
      print('SUCCESS: Intersection is empty, subset check passes!');
    }
  });
}
