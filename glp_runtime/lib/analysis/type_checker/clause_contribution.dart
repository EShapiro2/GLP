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

/// Computes clause contributions for fixpoint checking
class ClauseContributionComputer {
  final TypeEnvironment typeEnv;

  ClauseContributionComputer(this.typeEnv);

  /// Compute DFA for all ground terms matching a pattern
  /// with variables instantiated to their inferred types
  ///
  /// Given pattern f(X, g(Y)) and varTypes {X: Nat, Y: Bool},
  /// returns DFA accepting {f(n, g(b)) | n ∈ Nat, b ∈ Bool}
  ///
  /// [declaredDFA] is the DFA for the declared type at this pattern position.
  /// Used for underscore (_) patterns which accept any value of the declared type.
  TypeDFA computeArgContribution(
    ast.Term pattern,
    Map<String, TypeDFA> varTypes,
    TypeDFA declaredDFA,
  ) {
    if (pattern is ast.VarTerm) {
      // Variable: return its inferred type DFA
      // Handle both writer (X) and reader (X?) - same variable name
      final name = pattern.name;
      return varTypes[name] ?? TypeDFA.empty();
    }

    if (pattern is ast.ConstTerm) {
      // Constant: can be int, double, or String (atom/string literal)
      final value = pattern.value;
      if (value == null) {
        return TypeDFA.empty();
      }

      // Convert to string representation for DFA
      if (value is String) {
        // String literals are quoted in toString, atoms are not
        // Use the value directly as the constant symbol
        return TypeDFA.singleton(value);
      } else if (value is num) {
        // Numbers use toString representation
        return TypeDFA.singleton(value.toString());
      } else {
        // Other types: use toString
        return TypeDFA.singleton(value.toString());
      }
    }

    if (pattern is ast.StructTerm) {
      // f(t1, ..., tn): DFA accepting f(v1, ..., vn)
      // where each vi is in L(argDFA[i])
      final argDFAs = <TypeDFA>[];
      final argModes = <Mode?>[];

      for (int i = 0; i < pattern.args.length; i++) {
        final argPattern = pattern.args[i];
        final argIndex = i + 1;
        final argDeclaredDFA = _extractStructArgDFA(declaredDFA, pattern.functor, pattern.arity, argIndex);
        final argContribution = computeArgContribution(argPattern, varTypes, argDeclaredDFA);
        argDFAs.add(argContribution);

        // Determine mode for this position based on declared type and pattern
        Mode? argMode;
        final declaredHasMode = _declaredPositionHasMode(declaredDFA, pattern.functor, pattern.arity, argIndex);
        if (declaredHasMode && argPattern is ast.VarTerm) {
          argMode = argPattern.isReader ? Mode.input : Mode.output;
        }
        argModes.add(argMode);
      }

      return _buildStructDFA(pattern.functor, pattern.arity, argDFAs, argModes);
    }

    if (pattern is ast.ListTerm) {
      if (pattern.isNil) {
        // Empty list: singleton DFA
        return TypeDFA.singleton('[]');
      }
      // [H|T]: DFA for list cons
      final headDeclaredDFA = _extractListHeadDFA(declaredDFA);
      final tailDeclaredDFA = _extractListTailDFA(declaredDFA);
      final headDFA = computeArgContribution(pattern.head!, varTypes, headDeclaredDFA);
      final tailDFA = computeArgContribution(pattern.tail!, varTypes, tailDeclaredDFA);

      // Determine modes for head and tail based on declared type and pattern
      Mode? headMode;
      Mode? tailMode;

      final headHasMode = _declaredListHeadHasMode(declaredDFA);
      if (headHasMode && pattern.head is ast.VarTerm) {
        headMode = (pattern.head as ast.VarTerm).isReader ? Mode.input : Mode.output;
      }

      final tailHasMode = _declaredListTailHasMode(declaredDFA);
      if (tailHasMode && pattern.tail is ast.VarTerm) {
        tailMode = (pattern.tail as ast.VarTerm).isReader ? Mode.input : Mode.output;
      }

      return _buildListConsDFA(headDFA, tailDFA, headMode: headMode, tailMode: tailMode);
    }

    if (pattern is ast.UnderscoreTerm) {
      // Anonymous variable accepts any value of the declared type
      // If declaredDFA has no useful info (extracted sub-DFA from nested position),
      // create a universal primitive DFA
      if (declaredDFA.primitiveStateModes.isEmpty &&
          declaredDFA.transitions.isEmpty &&
          declaredDFA.finalStates.isNotEmpty) {
        // This is a primitive final state with no mode info - create universal DFA
        return _createUniversalPrimitiveDFA();
      }
      return declaredDFA;
    }

    // Unknown term type - should not happen
    return TypeDFA.empty();
  }

  /// Build DFA accepting f(v1,...,vn) where vi ∈ L(argDFAs[i])
  ///
  /// Strategy: Build a DFA with states and transitions that mirror
  /// the structure functor/arity with constraints on each argument position
  ///
  /// [argModes] specifies optional mode for each argument position.
  /// Mode is non-null only when the declared type has a moded TypeRef at that position.
  TypeDFA _buildStructDFA(String functor, int arity, List<TypeDFA> argDFAs, [List<Mode?>? argModes]) {
    // If any argument has empty type, the structure has empty type
    if (argDFAs.any((dfa) => dfa.isEmpty)) {
      return TypeDFA.empty();
    }

    // Nullary functor = constant
    if (argDFAs.isEmpty) {
      return TypeDFA.singleton(functor);
    }

    // Create start state
    final startState = DFAState('start');
    final states = <DFAState>{startState};
    final transitions = <(DFAState, PathElement), DFAState>{};
    final finalStates = <DFAState>{};

    // For each argument position, create transitions
    // that constrain that argument to its DFA
    for (int i = 0; i < argDFAs.length; i++) {
      final argDFA = argDFAs[i];
      final argIndex = i + 1; // PathElement uses 1-based indexing
      final argMode = (argModes != null && i < argModes.length) ? argModes[i] : null;
      final pathElem = PathElement.functor(functor, arity, argIndex, mode: argMode);

      // The transition from start on this arg-position path element
      // leads to a copy of the argument's DFA
      final argStartInProduct = _renameState(argDFA.startState, 'arg$argIndex');

      transitions[(startState, pathElem)] = argStartInProduct;

      // Add all states and transitions from argument DFA with renaming
      _mergeSubDFA(
        argDFA,
        'arg$argIndex',
        states,
        transitions,
        finalStates,
      );
    }

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: transitions,
    );
  }

  /// Build DFA accepting [h|t] where h ∈ L(head) and t ∈ L(tail)
  ///
  /// This is essentially _buildStructDFA for the special list cons constructor
  ///
  /// [headMode] and [tailMode] specify optional modes for head and tail positions.
  /// Mode is non-null only when the declared type has a moded TypeRef at that position.
  TypeDFA _buildListConsDFA(TypeDFA headDFA, TypeDFA tailDFA, {Mode? headMode, Mode? tailMode}) {
    // If either head or tail has empty type, the list has empty type
    if (headDFA.isEmpty || tailDFA.isEmpty) {
      return TypeDFA.empty();
    }

    final startState = DFAState('start');
    final states = <DFAState>{startState};
    final transitions = <(DFAState, PathElement), DFAState>{};
    final finalStates = <DFAState>{};

    // Head transition
    final headElem = PathElement.listHead(mode: headMode);
    final headStartInProduct = _renameState(headDFA.startState, 'head');
    transitions[(startState, headElem)] = headStartInProduct;
    _mergeSubDFA(headDFA, 'head', states, transitions, finalStates);

    // Tail transition
    final tailElem = PathElement.listTail(mode: tailMode);
    final tailStartInProduct = _renameState(tailDFA.startState, 'tail');
    transitions[(startState, tailElem)] = tailStartInProduct;
    _mergeSubDFA(tailDFA, 'tail', states, transitions, finalStates);

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: transitions,
    );
  }

  /// Rename a state by prefixing its name
  DFAState _renameState(DFAState state, String prefix) {
    return DFAState('$prefix.${state.name}', isFinal: state.isFinal);
  }

  /// Merge a sub-DFA into the product DFA with renamed states
  void _mergeSubDFA(
    TypeDFA subDFA,
    String prefix,
    Set<DFAState> states,
    Map<(DFAState, PathElement), DFAState> transitions,
    Set<DFAState> finalStates,
  ) {
    // Create mapping from original states to renamed states
    final stateMap = <DFAState, DFAState>{};
    for (final state in subDFA.states) {
      final renamed = _renameState(state, prefix);
      stateMap[state] = renamed;
      states.add(renamed);
      if (subDFA.finalStates.contains(state)) {
        finalStates.add(renamed);
      }
    }

    // Copy transitions with renamed states
    for (final entry in subDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      final toState = entry.value;

      final renamedFrom = stateMap[fromState]!;
      final renamedTo = stateMap[toState]!;

      transitions[(renamedFrom, pathElem)] = renamedTo;
    }
  }

  /// Create a DFA that accepts any single primitive value (for _ patterns)
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

  /// Extract the sub-DFA for a struct argument position
  /// Extracts based on structural symbol only, trying all possible modes
  TypeDFA _extractStructArgDFA(TypeDFA declaredDFA, String functor, int arity, int argIndex) {
    // Try to find any transition with this structural symbol, regardless of mode
    final structuralSymbol = '$functor($arity,$argIndex)';
    return _extractSubDFABySymbol(declaredDFA, structuralSymbol);
  }

  /// Extract the sub-DFA for list head position
  /// Extracts based on structural symbol only, trying all possible modes
  TypeDFA _extractListHeadDFA(TypeDFA declaredDFA) {
    const headSymbol = '[|](2,1)';
    return _extractSubDFABySymbol(declaredDFA, headSymbol);
  }

  /// Extract the sub-DFA for list tail position
  /// Extracts based on structural symbol only, trying all possible modes
  TypeDFA _extractListTailDFA(TypeDFA declaredDFA) {
    const tailSymbol = '[|](2,2)';
    return _extractSubDFABySymbol(declaredDFA, tailSymbol);
  }

  /// Extract sub-DFA by structural symbol, ignoring mode
  TypeDFA _extractSubDFABySymbol(TypeDFA declaredDFA, String symbol) {
    // Find any transition from start state with this structural symbol
    DFAState? targetState;
    for (final entry in declaredDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == declaredDFA.startState && pathElem.symbol == symbol) {
        targetState = entry.value;
        break;  // Found a transition with this symbol
      }
    }

    if (targetState == null) {
      return TypeDFA.empty();
    }

    // Build a sub-DFA rooted at targetState
    final reachableStates = <DFAState>{};
    final newTransitions = <(DFAState, PathElement), DFAState>{};
    final queue = Queue<DFAState>();
    queue.add(targetState);
    final visited = <DFAState>{};

    while (queue.isNotEmpty) {
      final current = queue.removeFirst();
      if (visited.contains(current)) continue;
      visited.add(current);
      reachableStates.add(current);

      // Find all transitions from current state
      for (final entry in declaredDFA.transitions.entries) {
        final (fromState, pathSymbol) = entry.key;
        final toState = entry.value;

        if (fromState == current) {
          newTransitions[(fromState, pathSymbol)] = toState;
          if (!visited.contains(toState)) {
            queue.add(toState);
          }
        }
      }
    }

    // Determine which reachable states are final
    final newFinalStates = reachableStates.intersection(declaredDFA.finalStates);

    // Extract primitive state modes for reachable states
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

  /// Check if declared DFA has a moded transition at this struct position
  bool _declaredPositionHasMode(
    TypeDFA declaredDFA,
    String functor,
    int arity,
    int argIndex,
  ) {
    final structuralSymbol = '$functor($arity,$argIndex)';

    for (final entry in declaredDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == declaredDFA.startState &&
          pathElem.symbol == structuralSymbol &&
          pathElem.mode != null) {
        return true;
      }
    }
    return false;
  }

  /// Check if declared DFA has a moded transition for list head
  bool _declaredListHeadHasMode(TypeDFA declaredDFA) {
    const headSymbol = '[|](2,1)';

    for (final entry in declaredDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == declaredDFA.startState &&
          pathElem.symbol == headSymbol &&
          pathElem.mode != null) {
        return true;
      }
    }
    return false;
  }

  /// Check if declared DFA has a moded transition for list tail
  bool _declaredListTailHasMode(TypeDFA declaredDFA) {
    const tailSymbol = '[|](2,2)';

    for (final entry in declaredDFA.transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == declaredDFA.startState &&
          pathElem.symbol == tailSymbol &&
          pathElem.mode != null) {
        return true;
      }
    }
    return false;
  }

}
