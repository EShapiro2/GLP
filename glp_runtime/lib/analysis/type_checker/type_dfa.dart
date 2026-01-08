// lib/analysis/type_checker/type_dfa.dart
//
// DFA representation for moded regular types following Yardeni-Shapiro.
// Each type compiles to a DFA that accepts exactly the moded paths in that type.
//
// Key insight: A term t is in a moded type S iff paths^m(t) ⊆ paths^m(S),
// where paths^m(S) is a moded regular language recognized by a DFA.

import 'dart:collection';

import 'mode.dart';
import 'moded_label.dart';

/// Classification of primitive states by their mode set (spec 5.7.1)
enum PrimitiveKind {
  outputOnly,  // μ(q) = {Mode.produce}
  inputOnly,   // μ(q) = {Mode.consume}
  biModed,     // μ(q) = {Mode.produce, Mode.consume}
}

/// DFA state
class DFAState {
  final String name;
  final bool isFinal;

  DFAState(this.name, {this.isFinal = false});

  @override
  String toString() => isFinal ? '$name*' : name;

  @override
  bool operator ==(Object other) => other is DFAState && name == other.name;

  @override
  int get hashCode => name.hashCode;
}

/// A path through a term tree (sequence of moded labels)
/// Used for path-based type checking operations
class TermPath {
  final List<ModedLabel> elements;

  TermPath(this.elements);

  factory TermPath.empty() => TermPath([]);

  TermPath append(ModedLabel elem) => TermPath([...elements, elem]);

  bool get isEmpty => elements.isEmpty;
  int get length => elements.length;

  @override
  String toString() => elements.join(' · ');

  @override
  bool operator ==(Object other) {
    if (other is! TermPath) return false;
    if (elements.length != other.elements.length) return false;
    for (int i = 0; i < elements.length; i++) {
      if (elements[i] != other.elements[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode => Object.hashAll(elements);
}

/// DFA for recognizing moded paths of a regular type
class TypeDFA {
  final Set<DFAState> states;
  final DFAState startState;
  final Set<DFAState> finalStates;
  final Map<(DFAState, ModedLabel), DFAState> transitions;

  /// Mode information at primitive type states.
  ///
  /// A state appears in this map iff it corresponds to a primitive type
  /// position (_ or _?) in a type definition:
  /// - {Mode.produce} for _ (program produces value)
  /// - {Mode.consume} for _? (program consumes value)
  /// - {Mode.produce, Mode.consume} for Every ::= _ ; _?
  ///
  /// States not in this map are structural (non-primitive) positions.
  final Map<DFAState, Set<Mode>> primitiveStateModes;

  /// Alphabet: all moded labels that appear in transitions
  late final Set<ModedLabel> alphabet;

  TypeDFA({
    required this.states,
    required this.startState,
    required this.finalStates,
    required this.transitions,
    Map<DFAState, Set<Mode>>? primitiveStateModes,
  }) : primitiveStateModes = primitiveStateModes ?? {} {
    alphabet = transitions.keys.map((k) => k.$2).toSet();
  }

  /// Check if state is a primitive type position
  bool isPrimitiveState(DFAState state) =>
      primitiveStateModes.containsKey(state) &&
      primitiveStateModes[state]!.isNotEmpty;

  /// Get accepted modes at a primitive state (empty for non-primitive)
  Set<Mode> getModesAt(DFAState state) =>
      primitiveStateModes[state] ?? {};

  /// Get the primitive classification of a state, or null if not primitive
  PrimitiveKind? getPrimitiveKind(DFAState state) {
    final modes = primitiveStateModes[state];
    if (modes == null || modes.isEmpty) return null;
    if (modes.length == 2) return PrimitiveKind.biModed;
    return modes.contains(Mode.produce)
        ? PrimitiveKind.outputOnly
        : PrimitiveKind.inputOnly;
  }

  /// Create DFA accepting empty language (no strings accepted)
  factory TypeDFA.empty() {
    final q0 = DFAState('q0');
    return TypeDFA(
      states: {q0},
      startState: q0,
      finalStates: {},
      transitions: {},
    );
  }

  /// Create DFA accepting exactly one constant
  factory TypeDFA.singleton(String constant) {
    final q0 = DFAState('q0');
    final q1 = DFAState('q1', isFinal: true);
    final label = ModedLabel.constant(constant);

    return TypeDFA(
      states: {q0, q1},
      startState: q0,
      finalStates: {q1},
      transitions: {
        (q0, label): q1,
      },
    );
  }

  /// Find transition by symbol only (ignoring mode)
  DFAState? transitionBySymbol(DFAState from, String symbol) {
    for (final entry in transitions.entries) {
      final (fromState, label) = entry.key;
      if (fromState == from && label.pathElement == symbol) {
        return entry.value;
      }
    }
    return null;
  }

  /// Check if DFA accepts a moded path (sequence of labels + leaf mode)
  bool acceptsModedPath(List<ModedLabel> path, Mode leafMode) {
    var current = startState;

    // Check if start state is primitive (accepts empty path with matching mode)
    if (path.isEmpty && isPrimitiveState(current)) {
      return getModesAt(current).contains(leafMode);
    }

    for (final label in path) {
      final next = transitions[(current, label)];
      if (next == null) return false;
      current = next;
    }

    // Check acceptance condition (two cases from paper Definition 7.8)
    if (isPrimitiveState(current)) {
      // Case (b): primitive acceptance - mode must be in μ(current)
      return getModesAt(current).contains(leafMode);
    } else {
      // Case (a): structural acceptance - must be in finalStates
      return finalStates.contains(current);
    }
  }

  /// Check if DFA accepts a structural path (ignoring modes in labels)
  /// Used for ground path checking where we verify structure, not modes
  bool acceptsStructuralPath(List<ModedLabel> path) {
    var current = startState;

    for (final label in path) {
      // Find transition by symbol only, ignoring mode
      DFAState? next;
      for (final entry in transitions.entries) {
        final (fromState, transLabel) = entry.key;
        if (fromState == current && transLabel.pathElement == label.pathElement) {
          next = entry.value;
          break;
        }
      }
      if (next == null) return false;
      current = next;
    }

    // Accept if we reach a final state or a primitive state
    return finalStates.contains(current) || isPrimitiveState(current);
  }

  /// Check if moded language is empty (spec 5.7.4)
  bool get isEmpty {
    final visited = <DFAState>{};
    final worklist = <DFAState>[startState];

    while (worklist.isNotEmpty) {
      final state = worklist.removeLast();
      if (visited.contains(state)) continue;
      visited.add(state);

      // Check if this state can accept
      if (isPrimitiveState(state)) {
        // Primitive: accepts if modes non-empty
        if (getModesAt(state).isNotEmpty) return false;
      } else if (finalStates.contains(state)) {
        // Structural final: accepts
        return false;
      }

      // Add successors
      for (final entry in transitions.entries) {
        final (from, _) = entry.key;
        if (from == state) {
          worklist.add(entry.value);
        }
      }
    }

    return true;
  }

  /// Check language equivalence
  bool isEquivalent(TypeDFA other) => isSubsetOf(other) && other.isSubsetOf(this);

  /// Accept a TermPath (compatibility wrapper)
  bool acceptsPath(TermPath path) => acceptsStructuralPath(path.elements);

  /// Get state after following a path
  DFAState? stateAfterPath(TermPath path) {
    var current = startState;

    // If start state is primitive, return it
    if (isPrimitiveState(current)) {
      return current;
    }

    for (final label in path.elements) {
      // Find transition by symbol only, ignoring mode
      DFAState? next;
      for (final entry in transitions.entries) {
        final (fromState, transLabel) = entry.key;
        if (fromState == current && transLabel.pathElement == label.pathElement) {
          next = entry.value;
          break;
        }
      }
      if (next == null) return null;
      current = next;

      // If we reach a primitive state, return it
      if (isPrimitiveState(current)) {
        return current;
      }
    }

    return current;
  }

  /// Check if L^m(this) ⊆ L^m(other)
  bool isSubsetOf(TypeDFA other) {
    // Optimization: bi-moded start state accepts everything
    if (other.isPrimitiveState(other.startState) &&
        other.getModesAt(other.startState).length == 2) {
      return true;
    }

    // L^m(A) ⊆ L^m(B) iff L^m(A) ∩ L^m(B̄) = ∅
    // IMPORTANT: Complement must be complete over BOTH alphabets.
    // Otherwise, labels in A but not in B would have no transition in B̄,
    // causing incorrect rejection (the label would go nowhere instead of sink).
    final combinedAlphabet = alphabet.union(other.alphabet);
    final otherComplement = other.complement(combinedAlphabet);
    final intersection = intersect(otherComplement);
    return intersection.isEmpty;
  }

  /// Check if L^struct(this) ⊆ L^struct(other)
  ///
  /// Structural subset: ignores modes, compares only path structure.
  /// Used for containment checking where we care about VALUES, not dataflow.
  ///
  /// Per spec v1.13 Section 6.1: Containment checks what values are produced,
  /// independent of the reader/writer mode annotations on variables.
  bool structuralIsSubsetOf(TypeDFA other) {
    // Optimization: bi-moded start state accepts everything structurally
    if (other.isPrimitiveState(other.startState) &&
        other.getModesAt(other.startState).isNotEmpty) {
      return true;
    }

    // Build structural versions (all modes stripped to null)
    final structuralThis = _toStructuralDFA();
    final structuralOther = other._toStructuralDFA();

    // Now do regular subset check on structural DFAs
    return structuralThis.isSubsetOf(structuralOther);
  }

  /// Convert to structural DFA by normalizing all modes to Mode.produce
  /// and merging all primitive states into a single canonical state.
  ///
  /// This allows comparing structure without mode differences affecting
  /// the subset check. Both DFAs are normalized to the same mode so
  /// complement and intersect work correctly.
  ///
  /// All primitive states (including Any, _, _?, prim_N) are merged into
  /// a single _PRIM_ state so that they're considered structurally equivalent.
  TypeDFA _toStructuralDFA() {
    // Create canonical primitive state
    final canonicalPrim = DFAState('_PRIM_');

    // Build mapping from primitive states to canonical
    final stateMapping = <DFAState, DFAState>{};
    for (final state in states) {
      if (primitiveStateModes.containsKey(state) ||
          state.name == 'Any' ||
          state.name.startsWith('prim_')) {
        stateMapping[state] = canonicalPrim;
      } else {
        stateMapping[state] = state;
      }
    }

    // Map start state
    final newStartState = stateMapping[startState] ?? startState;

    final newTransitions = <(DFAState, ModedLabel), DFAState>{};
    final newStates = <DFAState>{newStartState};

    for (final entry in transitions.entries) {
      final (fromState, label) = entry.key;
      final toState = entry.value;

      // Map states to their canonical forms
      final mappedFrom = stateMapping[fromState] ?? fromState;
      final mappedTo = stateMapping[toState] ?? toState;

      newStates.add(mappedFrom);
      newStates.add(mappedTo);

      // Normalize mode to output (consistent mode for structural comparison)
      final structuralLabel = ModedLabel(label.pathElement, mode: Mode.produce);
      newTransitions[(mappedFrom, structuralLabel)] = mappedTo;
    }

    // Map final states
    final newFinalStates = finalStates.map((s) => stateMapping[s] ?? s).toSet();

    // All primitive states map to canonical, with output mode only
    final newPrimitiveModes = <DFAState, Set<Mode>>{};
    if (primitiveStateModes.isNotEmpty ||
        states.any((s) => s.name == 'Any' || s.name.startsWith('prim_'))) {
      newPrimitiveModes[canonicalPrim] = {Mode.produce};
      newStates.add(canonicalPrim);
    }

    return TypeDFA(
      states: newStates,
      startState: newStartState,
      finalStates: newFinalStates,
      transitions: newTransitions,
      primitiveStateModes: newPrimitiveModes,
    );
  }

  /// Moded intersection (product construction with mode intersection)
  TypeDFA intersect(TypeDFA other) {
    final productStates = <String, DFAState>{};
    final newTransitions = <(DFAState, ModedLabel), DFAState>{};
    final newFinalStates = <DFAState>{};
    final newPrimitiveModes = <DFAState, Set<Mode>>{};

    int stateCounter = 0;
    String productKey(DFAState a, DFAState b) => '(${a.name},${b.name})';

    DFAState getProductState(DFAState qA, DFAState qB) {
      final key = productKey(qA, qB);
      return productStates.putIfAbsent(key, () => DFAState('p${stateCounter++}'));
    }

    final startProduct = getProductState(startState, other.startState);
    final worklist = Queue<(DFAState, DFAState)>();
    final visited = <String>{};

    worklist.add((startState, other.startState));

    while (worklist.isNotEmpty) {
      final (qA, qB) = worklist.removeFirst();
      final key = productKey(qA, qB);
      if (visited.contains(key)) continue;
      visited.add(key);

      final productState = getProductState(qA, qB);

      // Compute primitive modes for product state
      final modesA = primitiveStateModes[qA] ?? <Mode>{};
      final modesB = other.primitiveStateModes[qB] ?? <Mode>{};

      if (modesA.isNotEmpty && modesB.isNotEmpty) {
        // Both primitive: intersect modes
        final intersectedModes = modesA.intersection(modesB);
        if (intersectedModes.isNotEmpty) {
          newPrimitiveModes[productState] = intersectedModes;
        }
      } else if (modesA.isNotEmpty) {
        newPrimitiveModes[productState] = modesA;
      } else if (modesB.isNotEmpty) {
        newPrimitiveModes[productState] = modesB;
      } else {
        // Both structural: check if both final
        if (finalStates.contains(qA) && other.finalStates.contains(qB)) {
          newFinalStates.add(productState);
        }
      }

      // Add transitions for common labels
      final combinedAlphabet = alphabet.union(other.alphabet);
      for (final label in combinedAlphabet) {
        final nextA = transitions[(qA, label)];
        final nextB = other.transitions[(qB, label)];

        if (nextA != null && nextB != null) {
          final nextProduct = getProductState(nextA, nextB);
          newTransitions[(productState, label)] = nextProduct;

          final nextKey = productKey(nextA, nextB);
          if (!visited.contains(nextKey)) {
            worklist.add((nextA, nextB));
          }
        }
      }
    }

    return TypeDFA(
      states: productStates.values.toSet(),
      startState: startProduct,
      finalStates: newFinalStates,
      transitions: newTransitions,
      primitiveStateModes: newPrimitiveModes,
    );
  }

  /// Make DFA complete by adding a sink state
  TypeDFA complete([Set<ModedLabel>? withAlphabet]) {
    final useAlphabet = withAlphabet ?? alphabet;
    final sink = DFAState('_sink_');
    final newStates = {...states, sink};
    final newTransitions = Map<(DFAState, ModedLabel), DFAState>.from(transitions);

    for (final state in states) {
      for (final label in useAlphabet) {
        if (!newTransitions.containsKey((state, label))) {
          newTransitions[(state, label)] = sink;
        }
      }
    }

    // Sink loops to itself on all symbols
    for (final label in useAlphabet) {
      newTransitions[(sink, label)] = sink;
    }

    return TypeDFA(
      states: newStates,
      startState: startState,
      finalStates: finalStates,
      transitions: newTransitions,
      primitiveStateModes: primitiveStateModes,
    );
  }

  /// Moded complement (spec 5.7.3)
  /// Takes optional alphabet to complete over before complementing.
  /// IMPORTANT: Always pass the relevant alphabet to ensure correct complement.
  TypeDFA complement([Set<ModedLabel>? withAlphabet]) {
    final completed = complete(withAlphabet);

    // Complement final states
    final newFinalStates = completed.states.difference(completed.finalStates);

    // Complement mode sets at primitive states
    final newPrimitiveModes = <DFAState, Set<Mode>>{};
    for (final state in completed.states) {
      final modes = completed.primitiveStateModes[state];
      if (modes != null && modes.isNotEmpty) {
        final complementModes = {Mode.produce, Mode.consume}.difference(modes);
        newPrimitiveModes[state] = complementModes;
      }
    }

    return TypeDFA(
      states: completed.states,
      startState: completed.startState,
      finalStates: newFinalStates,
      transitions: completed.transitions,
      primitiveStateModes: newPrimitiveModes,
    );
  }

  /// Normalize null modes to output mode
  ///
  /// This is used before union operations to ensure consistent moding.
  /// Constants (like []) produce null modes, while variable positions produce
  /// explicit modes. Normalizing ensures they use the same alphabet.
  TypeDFA normalizeNullModes() {
    final newTransitions = <(DFAState, ModedLabel), DFAState>{};
    for (final entry in transitions.entries) {
      final (fromState, label) = entry.key;
      final toState = entry.value;

      // Normalize null to output
      final normalizedMode = label.mode ?? Mode.produce;
      final normalizedLabel = ModedLabel(label.pathElement, mode: normalizedMode);
      newTransitions[(fromState, normalizedLabel)] = toState;
    }

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: newTransitions,
      primitiveStateModes: primitiveStateModes,
    );
  }

  /// Apply mode complement: flip all modes (output ↔ input)
  /// Used for reader arguments (Type?) - flip all modes in transitions and primitive states
  TypeDFA applyModeComplement() {
    // Flip mode in each transition label
    final newTransitions = <(DFAState, ModedLabel), DFAState>{};
    for (final entry in transitions.entries) {
      final (fromState, label) = entry.key;
      final toState = entry.value;

      final flippedMode = label.mode?.flip;  // null stays null
      final flippedLabel = ModedLabel(label.pathElement, mode: flippedMode);
      newTransitions[(fromState, flippedLabel)] = toState;
    }

    // Flip modes in primitive state mode sets
    final newPrimitiveModes = <DFAState, Set<Mode>>{};
    for (final entry in primitiveStateModes.entries) {
      final state = entry.key;
      final modes = entry.value;
      final flippedModes = modes.map((m) => m.flip).toSet();
      newPrimitiveModes[state] = flippedModes;
    }

    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: finalStates,
      transitions: newTransitions,
      primitiveStateModes: newPrimitiveModes,
    );
  }

  /// Union of two DFAs
  TypeDFA union(TypeDFA other) {
    // Optimization: empty union is identity
    // This avoids complex complement operations that may lose primitive state info
    if (isEmpty && transitions.isEmpty) {
      return other;
    }
    if (other.isEmpty && other.transitions.isEmpty) {
      return this;
    }

    // L(A) ∪ L(B) = complement(complement(A) ∩ complement(B))
    // IMPORTANT: All complements must be complete over combined alphabet
    final combinedAlphabet = alphabet.union(other.alphabet);
    final compA = complement(combinedAlphabet);
    final compB = other.complement(combinedAlphabet);
    final intersection = compA.intersect(compB);
    final result = intersection.complement(combinedAlphabet);

    // IMPORTANT: Remove _sink_ from final states.
    // The sink state represents rejected paths and should never be accepting.
    // The complement operation may incorrectly make _sink_ final because it
    // wasn't final in the intersection (complete() adds it as non-final).
    final sinkState = DFAState('_sink_');
    final cleanedFinals = result.finalStates.where((s) => s.name != '_sink_').toSet();

    return TypeDFA(
      states: result.states,
      startState: result.startState,
      finalStates: cleanedFinals,
      transitions: result.transitions,
      primitiveStateModes: result.primitiveStateModes,
    );
  }

  @override
  String toString() {
    final sb = StringBuffer();
    sb.writeln('DFA:');
    sb.writeln('  States: ${states.map((s) => s.name).join(', ')}');
    sb.writeln('  Start: ${startState.name}');
    sb.writeln('  Final: ${finalStates.map((s) => s.name).join(', ')}');
    sb.writeln('  Primitive modes:');
    for (final entry in primitiveStateModes.entries) {
      sb.writeln('    ${entry.key.name}: ${entry.value}');
    }
    sb.writeln('  Transitions:');
    for (final entry in transitions.entries) {
      sb.writeln('    ${entry.key.$1.name} --[${entry.key.$2}]--> ${entry.value.name}');
    }
    return sb.toString();
  }
}

/// DFA that accepts only numbers (for Number type)
class NumberTypeDFA extends TypeDFA {
  NumberTypeDFA() : super(
    states: {DFAState('Number')},
    startState: DFAState('Number'),
    finalStates: {},
    transitions: {},
    primitiveStateModes: {DFAState('Number'): {Mode.produce, Mode.consume}},
  );

  @override
  bool acceptsModedPath(List<ModedLabel> path, Mode leafMode) {
    // Number accepts empty path (it's a primitive bi-moded type)
    if (path.isEmpty) return true;
    // A path of length 1 with a numeric constant
    if (path.length != 1) return false;
    final sym = path[0].pathElement;
    return double.tryParse(sym) != null || int.tryParse(sym) != null;
  }

  @override
  bool acceptsStructuralPath(List<ModedLabel> path) {
    // Number accepts empty path (primitive type)
    if (path.isEmpty) return true;
    // A path of length 1 with a numeric constant
    if (path.length != 1) return false;
    final sym = path[0].pathElement;
    return double.tryParse(sym) != null || int.tryParse(sym) != null;
  }

  @override
  bool get isEmpty => false;

  @override
  bool isSubsetOf(TypeDFA other) {
    if (other is NumberTypeDFA) return true;
    if (other is StringTypeDFA) return false;
    if (other.isPrimitiveState(other.startState) &&
        other.getModesAt(other.startState).length == 2) return true;
    return false;
  }
}

/// DFA that accepts only strings (for String type)
class StringTypeDFA extends TypeDFA {
  StringTypeDFA() : super(
    states: {DFAState('String')},
    startState: DFAState('String'),
    finalStates: {},
    transitions: {},
    primitiveStateModes: {DFAState('String'): {Mode.produce, Mode.consume}},
  );

  @override
  bool acceptsModedPath(List<ModedLabel> path, Mode leafMode) {
    if (path.isEmpty) return true;
    if (path.length != 1) return false;
    final sym = path[0].pathElement;
    return sym.startsWith('"') || sym.startsWith("'");
  }

  @override
  bool acceptsStructuralPath(List<ModedLabel> path) {
    if (path.isEmpty) return true;
    if (path.length != 1) return false;
    final sym = path[0].pathElement;
    return sym.startsWith('"') || sym.startsWith("'");
  }

  @override
  bool get isEmpty => false;

  @override
  bool isSubsetOf(TypeDFA other) {
    if (other is StringTypeDFA) return true;
    if (other is NumberTypeDFA) return false;
    if (other.isPrimitiveState(other.startState) &&
        other.getModesAt(other.startState).length == 2) return true;
    return false;
  }
}
