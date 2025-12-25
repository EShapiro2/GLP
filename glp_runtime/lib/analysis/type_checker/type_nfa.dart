// lib/analysis/type_checker/type_nfa.dart
//
// Nondeterministic finite automaton for moded types.
// NFA representation enables correct handling of union types.

import 'mode.dart';
import 'moded_label.dart';

/// NFA state
class NFAState {
  final String name;

  NFAState(this.name);

  @override
  bool operator ==(Object other) => other is NFAState && name == other.name;

  @override
  int get hashCode => name.hashCode;

  @override
  String toString() => name;
}

/// Moded Type NFA
///
/// Corresponds to paper Section 7.4 NFA Construction.
/// Union types (T ::= A ; B) create nondeterminism: from the same state,
/// multiple transitions on different labels are possible.
class TypeNFA {
  final Set<NFAState> states;
  final NFAState startState;
  final Set<NFAState> finalStates;

  /// Nondeterministic transitions: (state, label) → {states}
  final Map<(NFAState, ModedLabel), Set<NFAState>> transitions;

  /// Mode annotation μ: Q → P({output, input}) for primitive states
  /// A state q is primitive iff μ(q) ≠ ∅
  final Map<NFAState, Set<Mode>> primitiveStateModes;

  /// ε-transitions for type reference expansion
  final Map<NFAState, Set<NFAState>> epsilonTransitions;

  TypeNFA({
    required this.states,
    required this.startState,
    required this.finalStates,
    required this.transitions,
    required this.primitiveStateModes,
    required this.epsilonTransitions,
  });

  /// Check if state is primitive (has mode annotation)
  bool isPrimitiveState(NFAState state) =>
      primitiveStateModes.containsKey(state) &&
      primitiveStateModes[state]!.isNotEmpty;

  /// Get modes at a primitive state
  Set<Mode> getModesAt(NFAState state) =>
      primitiveStateModes[state] ?? <Mode>{};

  /// Compute ε-closure of a single state
  Set<NFAState> epsilonClosure(NFAState state) {
    final closure = <NFAState>{state};
    final worklist = [state];

    while (worklist.isNotEmpty) {
      final current = worklist.removeLast();
      for (final target in epsilonTransitions[current] ?? <NFAState>{}) {
        if (closure.add(target)) {
          worklist.add(target);
        }
      }
    }

    return closure;
  }

  /// Compute ε-closure of a set of states
  Set<NFAState> epsilonClosureSet(Set<NFAState> stateSet) {
    final result = <NFAState>{};
    for (final state in stateSet) {
      result.addAll(epsilonClosure(state));
    }
    return result;
  }

  /// Get all labels reachable from a set of states
  Set<ModedLabel> collectLabels(Set<NFAState> stateSet) {
    final labels = <ModedLabel>{};
    for (final state in stateSet) {
      for (final key in transitions.keys) {
        if (key.$1 == state) {
          labels.add(key.$2);
        }
      }
    }
    return labels;
  }

  /// Get target states for a transition from a set of states
  Set<NFAState> move(Set<NFAState> stateSet, ModedLabel label) {
    final result = <NFAState>{};
    for (final state in stateSet) {
      final targets = transitions[(state, label)];
      if (targets != null) {
        result.addAll(targets);
      }
    }
    return result;
  }

  @override
  String toString() {
    final sb = StringBuffer('NFA:\n');
    sb.writeln('  States: ${states.map((s) => s.name).join(", ")}');
    sb.writeln('  Start: ${startState.name}');
    sb.writeln('  Final: ${finalStates.map((s) => s.name).join(", ")}');
    sb.writeln('  Primitive modes:');
    for (final entry in primitiveStateModes.entries) {
      sb.writeln('    ${entry.key.name}: ${entry.value}');
    }
    sb.writeln('  Transitions:');
    for (final entry in transitions.entries) {
      sb.writeln('    ${entry.key.$1.name} --[${entry.key.$2}]--> {${entry.value.map((s) => s.name).join(", ")}}');
    }
    sb.writeln('  ε-transitions:');
    for (final entry in epsilonTransitions.entries) {
      sb.writeln('    ${entry.key.name} --ε--> {${entry.value.map((s) => s.name).join(", ")}}');
    }
    return sb.toString();
  }
}
