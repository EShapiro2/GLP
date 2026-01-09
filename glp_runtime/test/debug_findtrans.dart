import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_compiler.dart';
import 'package:glp_runtime/analysis/type_checker/type_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/moded_label.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

/// Sentinel state for universal acceptance at primitive type positions
final _universalAcceptingState = DFAState('_UNIVERSAL_', isFinal: true);

/// Debug version of _findTransition with verbose output
DFAState? debugFindTransition(DFAState from, ModedLabel label, TypeDFA dfa) {
  print('  Looking for transition from ${from.name} on ${label.pathElement}:${label.mode}');

  // Universal state accepts any structure
  if (from == _universalAcceptingState) {
    print('  -> Universal state, returning universal');
    return _universalAcceptingState;
  }

  // Primitive states accept any structure
  if (dfa.isPrimitiveState(from)) {
    print('  -> Primitive state, returning universal');
    return _universalAcceptingState;
  }

  // Search for matching transition
  for (final entry in dfa.transitions.entries) {
    final (fromState, transPathElem) = entry.key;
    if (fromState != from) continue;

    print('    Checking transition: ${transPathElem.symbol}:${transPathElem.mode} -> ${entry.value.name}');

    if (transPathElem.symbol != label.pathElement) {
      print('      Symbol mismatch: ${transPathElem.symbol} != ${label.pathElement}');
      continue;
    }

    // Mode matching
    if (transPathElem.mode == null) {
      print('      DFA has no mode, accepting');
      return entry.value;
    }
    if (transPathElem.mode == label.mode) {
      print('      Mode match!');
      return entry.value;
    }
    print('      Mode mismatch: ${transPathElem.mode} != ${label.mode}');
  }

  print('  -> No matching transition found');
  return null;
}

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

  // Build moded head
  final procDecl = typeEnv.procedures['my_dl_append/3']!;
  final headGoal = ast.Goal(clause.head.functor, clause.head.args, clause.head.line, clause.head.column);
  final modedHeadTerm = modedHead(headGoal, procDecl, typeEnv: typeEnv);

  // Extract paths
  final allPaths = paths(modedHeadTerm);

  // Build procedure DFA for head (with complement)
  final procState = DFAState('my_dl_append/3');
  var states = <DFAState>{procState};
  var transitions = <(DFAState, PathElement), DFAState>{};
  var primitiveStateModes = <DFAState, Set<Mode>>{};
  var finalStates = <DFAState>{};

  for (int i = 0; i < procDecl.arity; i++) {
    final argType = procDecl.argTypes[i];
    var argDFA = compiler.compile(argType.name);

    // Apply complement for INPUT args only
    if (argType.isInput) {
      argDFA = argDFA.applyModeComplement();
    }

    // Add transition from procedure state to argument type state
    final pathElem = PathElement.functor(procDecl.name, procDecl.arity, i + 1);
    transitions[(procState, pathElem)] = argDFA.startState;

    // Merge the argument DFA
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

  print('=== All DFA transitions ===');
  for (final entry in procDFA.transitions.entries) {
    final (from, label) = entry.key;
    final to = entry.value;
    print('  ${from.name} --[${label.symbol}:${label.mode}]--> ${to.name}');
  }

  // Check first path (to variable A)
  print('');
  print('=== Checking path to A ===');
  final pathToA = allPaths.firstWhere((p) => p.leaf.symbol == 'A');
  print('Path: $pathToA');

  var state = procDFA.startState;
  print('Start state: ${state.name}');

  for (int i = 0; i < pathToA.length - 1; i++) {
    final step = pathToA.steps[i];
    final nextStep = pathToA.steps[i + 1];

    print('');
    print('Step $i -> ${i+1}:');
    print('  currentStep: ${step.symbol}, argIdx=${step.argIndex}, mode=${step.mode}');
    print('  nextStep: ${nextStep.symbol}, argIdx=${nextStep.argIndex}, mode=${nextStep.mode}');

    // Build transition label
    final parts = step.symbol.split('/');
    final functor = parts[0];
    final arity = int.tryParse(parts[1]) ?? 0;
    final label = ModedLabel.functor(functor, arity, nextStep.argIndex, mode: nextStep.mode);
    print('  label: ${label.pathElement}:${label.mode}');

    // Find transition
    final nextState = debugFindTransition(state, label, procDFA);
    if (nextState == null) {
      print('  TRANSITION FAILED!');
      break;
    }
    print('  Moved to state: ${nextState.name}');
    state = nextState;
  }
}
