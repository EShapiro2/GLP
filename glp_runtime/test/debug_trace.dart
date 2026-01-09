import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_term.dart';
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
  final compiler = TypeCompiler(typeEnv);

  print('=== Type Definition: MyDiffList ===');
  final diffListDFA = compiler.compile('MyDiffList');
  print('Transitions (before complement):');
  for (final entry in diffListDFA.transitions.entries) {
    final (from, label) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${label.symbol}:${label.mode}]--> ${to.name}');
  }

  print('');
  print('=== MyDiffList after complement (for input argument) ===');
  final complementDFA = diffListDFA.applyModeComplement();
  print('Transitions:');
  for (final entry in complementDFA.transitions.entries) {
    final (from, label) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${label.symbol}:${label.mode}]--> ${to.name}');
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
  print('=== Clause head: ${clause.head.functor}(${clause.head.args.join(", ")}) ===');
  print('Arg 0 (A?\\B): reader=${(clause.head.args[0] as ast.StructTerm).args[0].toString()}, writer=${(clause.head.args[0] as ast.StructTerm).args[1].toString()}');

  // Build moded head
  final procDecl = typeEnv.procedures['my_dl_append/3']!;
  final headGoal = ast.Goal(clause.head.functor, clause.head.args, clause.head.line, clause.head.column);
  final modedHeadTerm = modedHead(headGoal, procDecl, typeEnv: typeEnv);

  print('');
  print('=== Moded Head (after variable flip) ===');
  print(modedHeadTerm);
  if (modedHeadTerm is ModedCompound) {
    for (int i = 0; i < modedHeadTerm.args.length; i++) {
      final arg = modedHeadTerm.args[i];
      print('  Arg $i: $arg (mode=${arg.mode})');
      if (arg is ModedCompound) {
        for (int j = 0; j < arg.args.length; j++) {
          final subarg = arg.args[j];
          print('    Subarg $j: $subarg (mode=${subarg.mode})');
        }
      }
    }
  }

  // Extract paths
  print('');
  print('=== Paths from Moded Head ===');
  final allPaths = paths(modedHeadTerm);
  for (final path in allPaths) {
    print('Path: $path');
    for (int i = 0; i < path.steps.length; i++) {
      final step = path.steps[i];
      print('  Step $i: symbol=${step.symbol}, mode=${step.mode}, argIdx=${step.argIndex}, isVar=${step.isVariable}, isReader=${step.isReader}');
    }
  }

  // Build procedure DFA for head (with complement)
  print('');
  print('=== Building Procedure DFA for head (complement=true) ===');
  final procState = DFAState('my_dl_append/3');
  var states = <DFAState>{procState};
  var transitions = <(DFAState, PathElement), DFAState>{};
  var primitiveStateModes = <DFAState, Set<Mode>>{};
  var finalStates = <DFAState>{};

  for (int i = 0; i < procDecl.arity; i++) {
    final argType = procDecl.argTypes[i];
    var argDFA = compiler.compile(argType.name);

    print('Arg $i: ${argType.name} isInput=${argType.isInput}');

    // Apply complement for INPUT args only
    if (argType.isInput) {
      print('  Applying complement (input arg)');
      argDFA = argDFA.applyModeComplement();
    }

    print('  Transitions:');
    for (final entry in argDFA.transitions.entries) {
      final (from, label) = entry.key;
      final to = entry.value;
      print('    ${from.name} --[${label.symbol}:${label.mode}]--> ${to.name}');
    }

    // Add transition from procedure state to argument type state
    final pathElem = PathElement.functor(procDecl.name, procDecl.arity, i + 1);
    transitions[(procState, pathElem)] = argDFA.startState;

    // Merge the argument DFA states and transitions
    states.addAll(argDFA.states);
    transitions.addAll(argDFA.transitions);
    primitiveStateModes.addAll(argDFA.primitiveStateModes);
    finalStates.addAll(argDFA.finalStates);
  }

  final procDFA = TypeDFA(
    states: states,
    startState: procState,
    finalStates: finalStates,
    transitions: transitions,
    primitiveStateModes: primitiveStateModes,
  );

  // Check each path
  print('');
  print('=== Checking paths against DFA ===');
  for (final path in allPaths) {
    print('');
    print('Checking path: ${path.leaf.symbol}');
    final result = checkPathAgainstDFA(path, procDFA);
    print('  Result: consistent=${result.isConsistent}, reason=${result.reason}');
  }
}
