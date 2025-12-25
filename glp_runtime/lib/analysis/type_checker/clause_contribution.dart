// lib/analysis/type_checker/clause_contribution.dart
//
// Computes clause contributions T_{C}^α(S) for fixpoint checking.
// Given a clause head pattern and inferred variable types,
// computes the DFA representing all ground terms the clause can produce.

import 'dart:collection';

import '../../compiler/ast.dart' as ast;
import 'type_dfa.dart';
import 'type_ast.dart';
import 'mode.dart';
import 'moded_label.dart';

/// Computes clause contributions for fixpoint checking
class ClauseContributionComputer {
  final TypeEnvironment typeEnv;

  ClauseContributionComputer(this.typeEnv);

  /// Compute DFA for all ground terms matching a pattern
  /// with variables instantiated to their inferred types
  TypeDFA computeArgContribution(
    ast.Term pattern,
    Map<String, TypeDFA> varTypes,
    TypeDFA declaredDFA,
  ) {
    if (pattern is ast.VarTerm) {
      final name = pattern.name;
      final varType = varTypes[name] ?? TypeDFA.empty();

      // If declared type at this position is primitive, contribution must inherit
      // the primitive modes for DFA equivalence checking to work correctly.
      if (declaredDFA.isPrimitiveState(declaredDFA.startState)) {
        final declaredModes = declaredDFA.getModesAt(declaredDFA.startState);
        if (declaredModes.isNotEmpty) {
          final primitiveState = DFAState('_VAR_', isFinal: true);
          return TypeDFA(
            states: {primitiveState},
            startState: primitiveState,
            finalStates: {primitiveState},
            transitions: {},
            primitiveStateModes: {primitiveState: declaredModes},
          );
        }
      }

      return varType;
    }

    if (pattern is ast.ConstTerm) {
      final value = pattern.value;
      if (value == null) {
        return TypeDFA.empty();
      }

      if (value is String) {
        return TypeDFA.singleton(value);
      } else if (value is num) {
        return TypeDFA.singleton(value.toString());
      } else {
        return TypeDFA.singleton(value.toString());
      }
    }

    if (pattern is ast.StructTerm) {
      final argDFAs = <TypeDFA>[];
      final argModes = <Mode?>[];

      for (int i = 0; i < pattern.args.length; i++) {
        final argPattern = pattern.args[i];
        final argIndex = i + 1;
        final argDeclaredDFA = _extractStructArgDFA(declaredDFA, pattern.functor, pattern.arity, argIndex);
        final argContribution = computeArgContribution(argPattern, varTypes, argDeclaredDFA);
        argDFAs.add(argContribution);

        final symbol = '${pattern.functor}(${pattern.arity},$argIndex)';
        final declaredModes = _getDeclaredModesAtPosition(declaredDFA, symbol);
        Mode? argMode;
        if (declaredModes.length == 1) {
          argMode = declaredModes.first;
        } else if (declaredModes.length > 1 && argPattern is ast.VarTerm) {
          argMode = argPattern.isReader ? Mode.input : Mode.output;
        }
        argModes.add(argMode);
      }

      return _buildStructDFA(pattern.functor, pattern.arity, argDFAs, argModes);
    }

    if (pattern is ast.ListTerm) {
      if (pattern.isNil) {
        return TypeDFA.singleton('[]');
      }

      final headDeclaredDFA = _extractListHeadDFA(declaredDFA);
      final tailDeclaredDFA = _extractListTailDFA(declaredDFA);
      final headDFA = computeArgContribution(pattern.head!, varTypes, headDeclaredDFA);
      final tailDFA = computeArgContribution(pattern.tail!, varTypes, tailDeclaredDFA);

      const headSymbol = '[|](2,1)';
      final headDeclaredModes = _getDeclaredModesAtPosition(declaredDFA, headSymbol);
      Mode? headMode;
      if (headDeclaredModes.length == 1) {
        headMode = headDeclaredModes.first;
      } else if (headDeclaredModes.length > 1 && pattern.head is ast.VarTerm) {
        headMode = (pattern.head as ast.VarTerm).isReader ? Mode.input : Mode.output;
      }

      const tailSymbol = '[|](2,2)';
      final tailDeclaredModes = _getDeclaredModesAtPosition(declaredDFA, tailSymbol);
      Mode? tailMode;
      if (tailDeclaredModes.length == 1) {
        tailMode = tailDeclaredModes.first;
      } else if (tailDeclaredModes.length > 1 && pattern.tail is ast.VarTerm) {
        tailMode = (pattern.tail as ast.VarTerm).isReader ? Mode.input : Mode.output;
      }

      return _buildListConsDFA(headDFA, tailDFA, headMode: headMode, tailMode: tailMode);
    }

    if (pattern is ast.UnderscoreTerm) {
      if (declaredDFA.primitiveStateModes.isEmpty &&
          declaredDFA.transitions.isEmpty &&
          declaredDFA.finalStates.isNotEmpty) {
        return _createUniversalPrimitiveDFA();
      }
      return declaredDFA;
    }

    return TypeDFA.empty();
  }

  /// Build DFA accepting f(v1,...,vn) where vi ∈ L(argDFAs[i])
  TypeDFA _buildStructDFA(String functor, int arity, List<TypeDFA> argDFAs, [List<Mode?>? argModes]) {
    if (argDFAs.any((dfa) => dfa.isModedEmpty)) {
      return TypeDFA.empty();
    }

    if (argDFAs.isEmpty) {
      return TypeDFA.singleton(functor);
    }

    final startState = DFAState('start');
    final states = <DFAState>{startState};
    final transitions = <(DFAState, ModedLabel), DFAState>{};
    final finalStates = <DFAState>{};
    final primitiveStateModes = <DFAState, Set<Mode>>{};

    for (int i = 0; i < argDFAs.length; i++) {
      final argDFA = argDFAs[i];
      final argIndex = i + 1;
      final argMode = (argModes != null && i < argModes.length) ? argModes[i] : null;
      final label = ModedLabel.functor(functor, arity, argIndex, mode: argMode);

      final argStartInProduct = _renameState(argDFA.startState, 'arg$argIndex');
      transitions[(startState, label)] = argStartInProduct;

      _mergeSubDFA(
        argDFA,
        'arg$argIndex',
        states,
        transitions,
        finalStates,
        primitiveStateModes,
      );
    }

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: transitions,
      primitiveStateModes: primitiveStateModes,
    );
  }

  /// Build DFA accepting [h|t] where h ∈ L(head) and t ∈ L(tail)
  TypeDFA _buildListConsDFA(TypeDFA headDFA, TypeDFA tailDFA, {Mode? headMode, Mode? tailMode}) {
    if (headDFA.isModedEmpty || tailDFA.isModedEmpty) {
      return TypeDFA.empty();
    }

    final startState = DFAState('start');
    final states = <DFAState>{startState};
    final transitions = <(DFAState, ModedLabel), DFAState>{};
    final finalStates = <DFAState>{};
    final primitiveStateModes = <DFAState, Set<Mode>>{};

    final headLabel = ModedLabel.listHead(mode: headMode);
    final headStartInProduct = _renameState(headDFA.startState, 'head');
    transitions[(startState, headLabel)] = headStartInProduct;
    _mergeSubDFA(headDFA, 'head', states, transitions, finalStates, primitiveStateModes);

    final tailLabel = ModedLabel.listTail(mode: tailMode);
    final tailStartInProduct = _renameState(tailDFA.startState, 'tail');
    transitions[(startState, tailLabel)] = tailStartInProduct;
    _mergeSubDFA(tailDFA, 'tail', states, transitions, finalStates, primitiveStateModes);

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: transitions,
      primitiveStateModes: primitiveStateModes,
    );
  }

  DFAState _renameState(DFAState state, String prefix) {
    return DFAState('$prefix.${state.name}', isFinal: state.isFinal);
  }

  void _mergeSubDFA(
    TypeDFA subDFA,
    String prefix,
    Set<DFAState> states,
    Map<(DFAState, ModedLabel), DFAState> transitions,
    Set<DFAState> finalStates,
    Map<DFAState, Set<Mode>> primitiveStateModes,
  ) {
    final stateMap = <DFAState, DFAState>{};
    for (final state in subDFA.states) {
      final renamed = _renameState(state, prefix);
      stateMap[state] = renamed;
      states.add(renamed);
      if (subDFA.finalStates.contains(state)) {
        finalStates.add(renamed);
      }
    }

    for (final entry in subDFA.transitions.entries) {
      final (fromState, label) = entry.key;
      final toState = entry.value;

      final renamedFrom = stateMap[fromState]!;
      final renamedTo = stateMap[toState]!;

      transitions[(renamedFrom, label)] = renamedTo;
    }

    for (final entry in subDFA.primitiveStateModes.entries) {
      final state = entry.key;
      final modes = entry.value;
      final renamed = stateMap[state];
      if (renamed != null) {
        primitiveStateModes[renamed] = modes;
      }
    }
  }

  TypeDFA _createUniversalPrimitiveDFA() {
    final primitiveState = DFAState('_prim', isFinal: true);
    return TypeDFA(
      states: {primitiveState},
      startState: primitiveState,
      finalStates: {primitiveState},
      transitions: {},
      primitiveStateModes: {primitiveState: {Mode.output}},
    );
  }

  TypeDFA _extractStructArgDFA(TypeDFA declaredDFA, String functor, int arity, int argIndex) {
    final structuralSymbol = '$functor($arity,$argIndex)';
    return _extractSubDFABySymbol(declaredDFA, structuralSymbol);
  }

  TypeDFA _extractListHeadDFA(TypeDFA declaredDFA) {
    const headSymbol = '[|](2,1)';
    return _extractSubDFABySymbol(declaredDFA, headSymbol);
  }

  TypeDFA _extractListTailDFA(TypeDFA declaredDFA) {
    const tailSymbol = '[|](2,2)';
    return _extractSubDFABySymbol(declaredDFA, tailSymbol);
  }

  TypeDFA _extractSubDFABySymbol(TypeDFA declaredDFA, String symbol) {
    DFAState? targetState;
    for (final entry in declaredDFA.transitions.entries) {
      final (fromState, label) = entry.key;
      if (fromState == declaredDFA.startState && label.pathElement == symbol) {
        targetState = entry.value;
        break;
      }
    }

    if (targetState == null) {
      return TypeDFA.empty();
    }

    final reachableStates = <DFAState>{};
    final newTransitions = <(DFAState, ModedLabel), DFAState>{};
    final queue = Queue<DFAState>();
    queue.add(targetState);
    final visited = <DFAState>{};

    while (queue.isNotEmpty) {
      final current = queue.removeFirst();
      if (visited.contains(current)) continue;
      visited.add(current);
      reachableStates.add(current);

      for (final entry in declaredDFA.transitions.entries) {
        final (fromState, label) = entry.key;
        final toState = entry.value;

        if (fromState == current) {
          newTransitions[(fromState, label)] = toState;
          if (!visited.contains(toState)) {
            queue.add(toState);
          }
        }
      }
    }

    final newFinalStates = reachableStates.intersection(declaredDFA.finalStates);

    final newPrimitiveStateModes = <DFAState, Set<Mode>>{};
    for (final state in reachableStates) {
      if (declaredDFA.primitiveStateModes.containsKey(state)) {
        newPrimitiveStateModes[state] = declaredDFA.primitiveStateModes[state]!;
      }
    }

    return TypeDFA(
      states: reachableStates,
      startState: targetState,
      finalStates: newFinalStates,
      transitions: newTransitions,
      primitiveStateModes: newPrimitiveStateModes,
    );
  }

  Set<Mode?> _getDeclaredModesAtPosition(TypeDFA declaredDFA, String symbol) {
    final modes = <Mode?>{};
    for (final entry in declaredDFA.transitions.entries) {
      final (_, label) = entry.key;
      if (label.pathElement == symbol) {
        modes.add(label.mode);
      }
    }
    return modes;
  }
}
