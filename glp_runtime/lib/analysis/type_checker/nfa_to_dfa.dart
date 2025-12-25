// lib/analysis/type_checker/nfa_to_dfa.dart
//
// Converts NFA to DFA via subset construction.
// Correctly handles primitive state modes during state merging.

import 'dart:collection';

import 'mode.dart';
import 'moded_label.dart';
import 'type_nfa.dart';
import 'type_dfa.dart';

/// Converts TypeNFA to TypeDFA via subset construction
class NFAToDFAConverter {
  final TypeNFA nfa;
  final Map<String, DFAState> _subsetToState = {};
  int _stateCounter = 0;

  NFAToDFAConverter(this.nfa);

  /// Convert NFA to DFA
  TypeDFA convert() {
    final dfaStates = <DFAState>{};
    final dfaTransitions = <(DFAState, ModedLabel), DFAState>{};
    final dfaFinalStates = <DFAState>{};
    final dfaPrimitiveModes = <DFAState, Set<Mode>>{};

    // Start state is ε-closure of NFA start state
    final startSubset = nfa.epsilonClosure(nfa.startState);
    final startDFA = _getOrCreateDFAState(startSubset);
    dfaStates.add(startDFA);

    // Compute properties for start state
    _computeStateProperties(startSubset, startDFA, dfaFinalStates, dfaPrimitiveModes);

    // Worklist algorithm
    final worklist = Queue<Set<NFAState>>();
    final visited = <String>{};

    worklist.add(startSubset);

    while (worklist.isNotEmpty) {
      final currentSubset = worklist.removeFirst();
      final currentKey = _subsetKey(currentSubset);

      if (visited.contains(currentKey)) continue;
      visited.add(currentKey);

      final currentDFA = _subsetToState[currentKey]!;
      dfaStates.add(currentDFA);

      // Collect all labels reachable from this subset
      final labels = nfa.collectLabels(currentSubset);

      for (final label in labels) {
        // Compute target subset: move then ε-closure
        var targetSubset = nfa.move(currentSubset, label);
        targetSubset = nfa.epsilonClosureSet(targetSubset);

        if (targetSubset.isEmpty) continue;

        final targetDFA = _getOrCreateDFAState(targetSubset);
        dfaTransitions[(currentDFA, label)] = targetDFA;

        _computeStateProperties(targetSubset, targetDFA, dfaFinalStates, dfaPrimitiveModes);

        final targetKey = _subsetKey(targetSubset);
        if (!visited.contains(targetKey)) {
          worklist.add(targetSubset);
        }
      }
    }

    return TypeDFA(
      states: dfaStates,
      startState: startDFA,
      finalStates: dfaFinalStates,
      transitions: dfaTransitions,
      primitiveStateModes: dfaPrimitiveModes,
    );
  }

  /// Create canonical key for a subset of NFA states
  String _subsetKey(Set<NFAState> subset) {
    final names = subset.map((s) => s.name).toList()..sort();
    return names.join(',');
  }

  /// Get or create DFA state for a subset
  DFAState _getOrCreateDFAState(Set<NFAState> subset) {
    final key = _subsetKey(subset);
    return _subsetToState.putIfAbsent(key, () {
      // Create a readable name
      final name = subset.length == 1
          ? subset.first.name
          : 'd${_stateCounter++}';
      return DFAState(name);
    });
  }

  /// Compute accepting status and primitive modes for a DFA state
  void _computeStateProperties(
    Set<NFAState> subset,
    DFAState dfaState,
    Set<DFAState> dfaFinalStates,
    Map<DFAState, Set<Mode>> dfaPrimitiveModes,
  ) {
    // DFA state is final if ANY NFA state in subset is final (and not primitive)
    final anyStructuralFinal = subset.any((s) =>
        nfa.finalStates.contains(s) && !nfa.isPrimitiveState(s));
    if (anyStructuralFinal) {
      dfaFinalStates.add(dfaState);
    }

    // Primitive modes: UNION of modes from all primitive NFA states in subset
    final combinedModes = <Mode>{};
    for (final nfaState in subset) {
      if (nfa.isPrimitiveState(nfaState)) {
        combinedModes.addAll(nfa.getModesAt(nfaState));
      }
    }
    if (combinedModes.isNotEmpty) {
      dfaPrimitiveModes[dfaState] = combinedModes;
    }
  }
}
