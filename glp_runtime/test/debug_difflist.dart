import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  final source = '''
    MyList ::= [_ | MyList] ; [].
    MyDiffList ::= MyList \\ MyList?.
    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A?\\B, B?\\C, A\\C?).
  ''';

  // Parse types
  final typeEnv = parseTypes(source);

  print('=== Type Definitions ===');
  for (final typeDef in typeEnv.types.values) {
    print('Type ${typeDef.name}:');
    for (final alt in typeDef.alternatives) {
      print('  Alternative: $alt (${alt.runtimeType})');
    }
  }

  print('');
  print('=== Procedure Declaration ===');
  final procDecl = typeEnv.procedures['my_dl_append/3']!;
  print('my_dl_append/3:');
  for (int i = 0; i < procDecl.argTypes.length; i++) {
    final argType = procDecl.argTypes[i];
    print('  Arg $i: ${argType.name} isInput=${argType.isInput}');
  }

  // Parse clause
  final clauseLines = source.split('\n').where((line) {
    final trimmed = line.trim();
    return !trimmed.contains('::=') &&
           !trimmed.startsWith('procedure ') &&
           trimmed.isNotEmpty;
  }).join('\n');

  final lexer = Lexer(clauseLines);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();
  final clause = program.procedures.first.clauses.first;

  print('');
  print('=== Clause Head ===');
  print('Head: ${clause.head.functor}/${clause.head.arity}');
  for (int i = 0; i < clause.head.args.length; i++) {
    final arg = clause.head.args[i];
    print('  Arg $i: $arg (${arg.runtimeType})');
  }

  print('');
  print('=== Building Moded Head ===');
  // Convert Atom to Goal
  final headGoal = ast.Goal(clause.head.functor, clause.head.args, clause.head.line, clause.head.column);
  final modedHeadTerm = modedHead(headGoal, procDecl, typeEnv: typeEnv);
  print('Moded Head: $modedHeadTerm');

  // Extract and print paths
  print('');
  print('=== Paths from Moded Head ===');
  final allPaths = paths(modedHeadTerm);
  for (final path in allPaths) {
    print('Path: $path');
    print('  Steps: ${path.steps.map((s) => '${s.symbol}[mode=${s.mode}, idx=${s.argIndex}]').join(' -> ')}');
    print('  Leaf: ${path.leaf.symbol}, isReader=${path.leaf.isReader}, mode=${path.leaf.mode}');
  }

  // Build and print the DFA for MyDiffList type
  print('');
  print('=== DFA for MyDiffList ===');
  final compiler = TypeCompiler(typeEnv);
  final diffListDFA = compiler.compile('MyDiffList');
  print('Start state: ${diffListDFA.startState}');
  print('Transitions:');
  for (final entry in diffListDFA.transitions.entries) {
    final (from, label) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${label.symbol}]--> ${to.name}');
  }
  print('Primitive state modes:');
  for (final state in diffListDFA.states) {
    final modes = diffListDFA.getModesAt(state);
    if (modes.isNotEmpty) {
      print('  ${state.name}: $modes');
    }
  }

  // Apply complement for head context
  print('');
  print('=== DFA for MyDiffList (after complement for input position head) ===');
  var complementDFA = diffListDFA.applyModeComplement();
  complementDFA = complementDFA.applyModeComplement(); // Second complement for input position
  print('Primitive state modes after complement:');
  for (final state in complementDFA.states) {
    final modes = complementDFA.getModesAt(state);
    if (modes.isNotEmpty) {
      print('  ${state.name}: $modes');
    }
  }

  if (modedHeadTerm is ModedCompound) {
    for (int i = 0; i < modedHeadTerm.args.length; i++) {
      print('  Arg $i: ${modedHeadTerm.args[i]}');
      final arg = modedHeadTerm.args[i];
      if (arg is ModedCompound) {
        for (int j = 0; j < arg.args.length; j++) {
          print('    Subarg $j: ${arg.args[j]}');
        }
      }
    }
  }
}
