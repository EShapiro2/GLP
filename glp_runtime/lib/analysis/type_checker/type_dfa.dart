// lib/analysis/type_checker/type_dfa.dart
//
// DFA representation for regular types following Yardeni-Shapiro.
// Each type compiles to a DFA that accepts exactly the terms in that type.
//
// Key insight: A term t is in a regular type S iff paths(t) ⊆ paths(S),
// where paths(S) is a regular language recognized by a DFA.

import 'dart:collection';

import 'mode.dart';

/// Classification of primitive states by their mode set (spec 5.8.1)
enum PrimitiveKind {
  outputOnly,  // μ(q) = {Mode.output}
  inputOnly,   // μ(q) = {Mode.input}
  biModed,     // μ(q) = {Mode.output, Mode.input}
}

/// A path element in term tree traversal
/// Format: functor(arity, argIndex) or constant
///
/// May carry optional mode annotation for moded TypeRef positions.
/// Mode is non-null ONLY when the position has a moded TypeRef (T or T?).
/// For PrimitiveModeAlt positions (_, _?), mode is null (tracked via primitiveStateModes).
class PathElement {
  final String symbol;  // e.g., "s(1,1)", "cons(2,1)", "0", "[]"
  final Mode? mode;     // non-null ONLY for moded TypeRef positions

  PathElement(this.symbol, {this.mode});

  /// Create path element for functor argument position
  factory PathElement.functor(String name, int arity, int argIndex, {Mode? mode}) {
    return PathElement('$name($arity,$argIndex)', mode: mode);
  }

  /// Create path element for constant
  factory PathElement.constant(Object value) {
    return PathElement(value.toString());
  }

  /// Create path element for list cons head
  factory PathElement.listHead({Mode? mode}) => PathElement('[|](2,1)', mode: mode);

  /// Create path element for list cons tail
  factory PathElement.listTail({Mode? mode}) => PathElement('[|](2,2)', mode: mode);

  /// Create path element for empty list
  factory PathElement.nil() => PathElement('[]');

  @override
  String toString() => mode != null
      ? '$symbol${mode == Mode.input ? "?" : ""}'
      : symbol;

  @override
  bool operator ==(Object other) =>
      other is PathElement && symbol == other.symbol && mode == other.mode;

  @override
  int get hashCode => Object.hash(symbol, mode);
}

/// A path in a term tree: sequence of path elements from root to leaf
class TermPath {
  final List<PathElement> elements;
  
  TermPath(this.elements);
  
  factory TermPath.empty() => TermPath([]);
  
  TermPath append(PathElement elem) => TermPath([...elements, elem]);
  
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

/// DFA for recognizing paths of a regular type
class TypeDFA {
  final Set<DFAState> states;
  final DFAState startState;
  final Set<DFAState> finalStates;
  final Map<(DFAState, PathElement), DFAState> transitions;

  /// Mode information at primitive type states.
  ///
  /// A state appears in this map iff it corresponds to a primitive type
  /// position (_ or _?) in a type definition:
  /// - {Mode.output} for _ (program produces value)
  /// - {Mode.input} for _? (program consumes value)
  /// - {Mode.output, Mode.input} for Every ::= _ ; _?
  ///
  /// States not in this map are structural (non-primitive) positions.
  final Map<DFAState, Set<Mode>> primitiveStateModes;

  /// Alphabet: all path elements that appear in transitions
  late final Set<PathElement> alphabet;

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
      primitiveStateModes.containsKey(state);

  /// Get accepted modes at a primitive state (empty for non-primitive)
  Set<Mode> getModesAt(DFAState state) =>
      primitiveStateModes[state] ?? {};

  /// Get the primitive classification of a state, or null if not primitive (spec 5.8.1)
  PrimitiveKind? getPrimitiveKind(DFAState state) {
    final modes = primitiveStateModes[state];
    if (modes == null || modes.isEmpty) return null;
    if (modes.length == 2) return PrimitiveKind.biModed;
    return modes.contains(Mode.output)
        ? PrimitiveKind.outputOnly
        : PrimitiveKind.inputOnly;
  }

  /// Create DFA accepting empty language (no strings accepted)
  factory TypeDFA.empty() {
    final q0 = DFAState('q0');
    return TypeDFA(
      states: {q0},
      startState: q0,
      finalStates: {},  // No accepting states
      transitions: {},  // No transitions
    );
  }

  /// Create DFA accepting exactly one constant string
  factory TypeDFA.singleton(String constant) {
    final q0 = DFAState('q0');
    final q1 = DFAState('q1', isFinal: true);
    final elem = PathElement.constant(constant);

    return TypeDFA(
      states: {q0, q1},
      startState: q0,
      finalStates: {q1},
      transitions: {
        (q0, elem): q1,
      },
    );
  }

  /// Find transition by symbol only (ignoring mode)
  DFAState? _transitionBySymbol(DFAState from, String symbol) {
    for (final entry in transitions.entries) {
      final (fromState, pathElem) = entry.key;
      if (fromState == from && pathElem.symbol == symbol) {
        return entry.value;
      }
    }
    return null;
  }

  /// Check if DFA accepts a single path
  bool acceptsPath(TermPath path) {
    var current = startState;

    // Check if start state is primitive (accepts any structure at this position)
    if (isPrimitiveState(current)) {
      return true;
    }

    for (final elem in path.elements) {
      final next = _transitionBySymbol(current, elem.symbol);
      if (next == null) return false;
      current = next;

      // If we reach a primitive state, stop descent and accept
      if (isPrimitiveState(current)) {
        return true;
      }
    }

    return finalStates.contains(current);
  }
  
  /// Check if DFA accepts all paths of a term
  bool accepts(Set<TermPath> termPaths) {
    return termPaths.every(acceptsPath);
  }
  
  /// Get the state reached after following a path (or null if undefined)
  DFAState? stateAfterPath(TermPath path) {
    var current = startState;

    // If start state is primitive, return it (stop descent)
    if (isPrimitiveState(current)) {
      return current;
    }

    for (final elem in path.elements) {
      final next = _transitionBySymbol(current, elem.symbol);
      if (next == null) return null;
      current = next;

      // If we reach a primitive state, return it (stop descent)
      if (isPrimitiveState(current)) {
        return current;
      }
    }

    return current;
  }
  
  /// Compute intersection of two DFAs (product construction)
  TypeDFA intersect(TypeDFA other) {
    // Same DFA instance: intersection is self
    if (identical(this, other)) {
      return this;
    }

    // Same semantic DFA type (Number ∩ Number = Number, etc.)
    if (this is NumberTypeDFA && other is NumberTypeDFA) {
      return this;
    }
    if (this is StringTypeDFA && other is StringTypeDFA) {
      return this;
    }

    // Handle NumberTypeDFA vs _builtin_Number state
    if (this is NumberTypeDFA && other.startState.name == '_builtin_Number') {
      return this;
    }
    if (other is NumberTypeDFA && startState.name == '_builtin_Number') {
      return other;
    }

    // Handle StringTypeDFA vs _builtin_String state
    if (this is StringTypeDFA && other.startState.name == '_builtin_String') {
      return this;
    }
    if (other is StringTypeDFA && startState.name == '_builtin_String') {
      return other;
    }

    // Handle incompatible built-in types
    if (this is NumberTypeDFA && other.startState.name == '_builtin_String') {
      return TypeDFA.empty();
    }
    if (this is StringTypeDFA && other.startState.name == '_builtin_Number') {
      return TypeDFA.empty();
    }
    if (other is NumberTypeDFA && startState.name == '_builtin_String') {
      return TypeDFA.empty();
    }
    if (other is StringTypeDFA && startState.name == '_builtin_Number') {
      return TypeDFA.empty();
    }

    // Handle primitive type (with modes) ∩ semantic type
    // Any ∩ Number = Number, Every ∩ String = String, etc.
    // If this has primitive states and other is semantic, return semantic (more specific)
    if (primitiveStateModes.isNotEmpty && other is NumberTypeDFA) {
      return other;
    }
    if (primitiveStateModes.isNotEmpty && other is StringTypeDFA) {
      return other;
    }
    if (other.primitiveStateModes.isNotEmpty && this is NumberTypeDFA) {
      return this;
    }
    if (other.primitiveStateModes.isNotEmpty && this is StringTypeDFA) {
      return this;
    }

    // Standard product construction with mode intersection
    final newStates = <DFAState>{};
    final newTransitions = <(DFAState, PathElement), DFAState>{};
    final newFinalStates = <DFAState>{};
    final newPrimitiveStateModes = <DFAState, Set<Mode>>{};

    // Product state naming
    String productName(DFAState a, DFAState b) => '(${a.name},${b.name})';

    // Compute mode intersection for product state
    void computeModesForProduct(DFAState state1, DFAState state2, DFAState productState) {
      final isPrim1 = isPrimitiveState(state1);
      final isPrim2 = other.isPrimitiveState(state2);

      // If both states are primitive, intersect their mode sets (even if result is empty)
      if (isPrim1 && isPrim2) {
        final modes1 = getModesAt(state1);
        final modes2 = other.getModesAt(state2);
        final intersectedModes = modes1.intersection(modes2);
        // Always add to preserve primitive status, even if intersection is empty
        newPrimitiveStateModes[productState] = intersectedModes;
      }
      // If only one is primitive, use its modes
      else if (isPrim1) {
        newPrimitiveStateModes[productState] = getModesAt(state1);
      } else if (isPrim2) {
        newPrimitiveStateModes[productState] = other.getModesAt(state2);
      }
    }

    // BFS to build reachable product states
    final startName = productName(startState, other.startState);
    final newStart = DFAState(startName,
        isFinal: finalStates.contains(startState) && other.finalStates.contains(other.startState));

    final queue = Queue<(DFAState, DFAState, DFAState)>();
    final visited = <String, DFAState>{};

    visited[startName] = newStart;
    newStates.add(newStart);
    if (newStart.isFinal) newFinalStates.add(newStart);
    computeModesForProduct(startState, other.startState, newStart);
    queue.add((startState, other.startState, newStart));

    // Combined alphabet
    final combinedAlphabet = alphabet.union(other.alphabet);
    
    while (queue.isNotEmpty) {
      final (state1, state2, productState) = queue.removeFirst();
      
      for (final sym in combinedAlphabet) {
        final next1 = transitions[(state1, sym)];
        final next2 = other.transitions[(state2, sym)];
        
        if (next1 != null && next2 != null) {
          final nextName = productName(next1, next2);
          var nextProduct = visited[nextName];
          
          if (nextProduct == null) {
            final isFinal = finalStates.contains(next1) && other.finalStates.contains(next2);
            nextProduct = DFAState(nextName, isFinal: isFinal);
            visited[nextName] = nextProduct;
            newStates.add(nextProduct);
            if (isFinal) newFinalStates.add(nextProduct);
            computeModesForProduct(next1, next2, nextProduct);
            queue.add((next1, next2, nextProduct));
          }
          
          newTransitions[(productState, sym)] = nextProduct;
        }
      }
    }
    
    return TypeDFA(
      states: newStates,
      startState: newStart,
      finalStates: newFinalStates,
      transitions: newTransitions,
      primitiveStateModes: newPrimitiveStateModes,
    );
  }
  
  /// Check if this DFA accepts a subset of another's language
  /// Uses moded operations when primitiveStateModes are present (spec 5.8.2)
  bool isSubsetOf(TypeDFA other) {
    // Handle special built-in types as 'other'
    if (other is NumberTypeDFA) {
      return _allAcceptedPathsSatisfy((path) => other.acceptsPath(path));
    }
    if (other is StringTypeDFA) {
      return _allAcceptedPathsSatisfy((path) => other.acceptsPath(path));
    }

    // Optimization (spec 5.8.2): bi-moded start state accepts everything
    if (other.isPrimitiveState(other.startState) &&
        other.getModesAt(other.startState).length == 2) {
      return true;
    }

    // Use moded operations if either DFA has primitive states
    if (primitiveStateModes.isNotEmpty || other.primitiveStateModes.isNotEmpty) {
      // L^m(this) ⊆ L^m(other) iff L^m(this) ∩ L^m(other̄) = ∅
      final otherComplement = other.modedComplement();
      final intersection = intersect(otherComplement);
      return intersection.isModedEmpty;
    }

    // Standard unmoded subset check
    final combinedAlphabet = alphabet.union(other.alphabet);
    final thisCompleted = complete(combinedAlphabet);
    final otherCompleted = other.complete(combinedAlphabet);
    final complementOther = otherCompleted.complement();
    final intersection = thisCompleted.intersect(complementOther);
    return intersection.isEmpty;
  }

  /// Check if all paths accepted by this DFA satisfy a predicate
  /// Used for subset checking against special built-in types
  bool _allAcceptedPathsSatisfy(bool Function(TermPath) predicate) {
    // For empty DFA, trivially true
    if (isEmpty) return true;

    // For singleton DFAs, check the single path
    if (transitions.isEmpty && finalStates.contains(startState)) {
      // Single state that is final - accepts empty path only
      return predicate(TermPath([]));
    }

    // Enumerate all accepting paths via DFS
    // This works for small DFAs (singletons, small constructed DFAs)
    final acceptingPaths = <TermPath>[];
    _collectAcceptingPaths(startState, [], acceptingPaths, <DFAState>{}, 10);

    // Check all collected paths
    return acceptingPaths.every(predicate);
  }

  /// Collect accepting paths from a state (with depth limit)
  void _collectAcceptingPaths(
    DFAState state,
    List<PathElement> currentPath,
    List<TermPath> results,
    Set<DFAState> visited,
    int maxDepth,
  ) {
    if (maxDepth <= 0) return;
    if (visited.contains(state)) return;
    visited.add(state);

    // If this is a final state, add current path
    if (finalStates.contains(state)) {
      results.add(TermPath(List.from(currentPath)));
    }

    // Explore all outgoing transitions
    for (final entry in transitions.entries) {
      final (fromState, pathElem) = entry.key;
      final toState = entry.value;

      if (fromState == state) {
        currentPath.add(pathElem);
        _collectAcceptingPaths(toState, currentPath, results, Set.from(visited), maxDepth - 1);
        currentPath.removeLast();
      }
    }

    visited.remove(state);
  }
  
  /// Check if DFA language is empty
  bool get isEmpty {
    // If we have primitive states, language is non-empty (accepts all values at those positions)
    if (primitiveStateModes.isNotEmpty) {
      return false;
    }

    // Standard BFS to see if any final state is reachable
    final visited = <DFAState>{};
    final queue = Queue<DFAState>();

    queue.add(startState);
    visited.add(startState);

    while (queue.isNotEmpty) {
      final current = queue.removeFirst();
      if (finalStates.contains(current)) return false;

      for (final sym in alphabet) {
        final next = transitions[(current, sym)];
        if (next != null && !visited.contains(next)) {
          visited.add(next);
          queue.add(next);
        }
      }
    }

    return true;
  }

  /// Check if moded language is empty (spec 5.8.4)
  /// Empty iff no reachable state is either:
  /// - (a) structural final (in finalStates with empty μ), or
  /// - (b) primitive with non-empty modes
  bool get isModedEmpty {
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

  /// Check if two DFAs accept the same language
  bool isEquivalent(TypeDFA other) {
    return isSubsetOf(other) && other.isSubsetOf(this);
  }
  
  /// Complement DFA for structural (unmoded) operations
  /// Note: This assumes DFA is complete. Does NOT handle modes.
  TypeDFA complement() {
    final newFinalStates = states.difference(finalStates);
    return TypeDFA(
      states: states,
      startState: startState,
      finalStates: newFinalStates,
      transitions: transitions,
      primitiveStateModes: primitiveStateModes,
    );
  }

  /// Moded complement (spec 5.8.5)
  /// Complements both final states AND mode sets at primitive states.
  TypeDFA modedComplement() {
    final completed = complete();

    // Complement final states (standard)
    final newFinalStates = completed.states.difference(completed.finalStates);

    // Complement mode sets at primitive states
    final newPrimitiveModes = <DFAState, Set<Mode>>{};
    for (final state in completed.states) {
      final modes = completed.primitiveStateModes[state];
      if (modes != null && modes.isNotEmpty) {
        final complementModes = {Mode.output, Mode.input}.difference(modes);
        // Always add the state to preserve primitive status, even if complement is empty
        newPrimitiveModes[state] = complementModes;
      }
      // Non-primitive states: no change to primitiveStateModes
    }

    return TypeDFA(
      states: completed.states,
      startState: completed.startState,
      finalStates: newFinalStates,
      transitions: completed.transitions,
      primitiveStateModes: newPrimitiveModes,
    );
  }
  
  /// Make DFA complete by adding a sink state for missing transitions
  /// If [withAlphabet] is provided, use that alphabet; otherwise use this DFA's alphabet
  TypeDFA complete([Set<PathElement>? withAlphabet]) {
    final useAlphabet = withAlphabet ?? alphabet;
    final sink = DFAState('_sink_');
    final newStates = {...states, sink};
    final newTransitions = Map<(DFAState, PathElement), DFAState>.from(transitions);

    for (final state in states) {
      for (final sym in useAlphabet) {
        if (!newTransitions.containsKey((state, sym))) {
          newTransitions[(state, sym)] = sink;
        }
      }
    }

    // Sink loops to itself on all symbols
    for (final sym in useAlphabet) {
      newTransitions[(sink, sym)] = sink;
    }

    return TypeDFA(
      states: newStates,
      startState: startState,
      finalStates: finalStates,
      transitions: newTransitions,
      primitiveStateModes: primitiveStateModes,  // Preserve primitive modes
    );
  }
  
  /// Union of two DFAs (using NFA conversion would be cleaner, but product works)
  TypeDFA union(TypeDFA other) {
    // L(A) ∪ L(B) = complement(complement(A) ∩ complement(B))
    // Must complete both DFAs with respect to combined alphabet
    final combinedAlphabet = alphabet.union(other.alphabet);
    final compA = complete(combinedAlphabet).complement();
    final compB = other.complete(combinedAlphabet).complement();
    return compA.intersect(compB).complement();
  }
  
  @override
  String toString() {
    final sb = StringBuffer();
    sb.writeln('DFA:');
    sb.writeln('  States: ${states.map((s) => s.name).join(', ')}');
    sb.writeln('  Start: ${startState.name}');
    sb.writeln('  Final: ${finalStates.map((s) => s.name).join(', ')}');
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
    states: {DFAState('q0'), DFAState('qNum', isFinal: true)},
    startState: DFAState('q0'),
    finalStates: {DFAState('qNum', isFinal: true)},
    transitions: {},  // Transitions added dynamically based on actual numbers seen
  ) {
    // Numbers are recognized by their literal value
    // This is a pseudo-DFA - actual checking done in accepts()
  }
  
  @override
  bool acceptsPath(TermPath path) {
    // A path of length 1 with a numeric constant
    if (path.length != 1) return false;
    final sym = path.elements[0].symbol;
    return _isNumeric(sym);
  }

  bool _isNumeric(String s) {
    return double.tryParse(s) != null || int.tryParse(s) != null;
  }

  @override
  bool get isEmpty => false;  // Accepts all numbers (non-empty language)

  @override
  bool isSubsetOf(TypeDFA other) {
    // Number ⊆ Number: true
    if (other is NumberTypeDFA) return true;
    // Number ⊆ String: false (disjoint)
    if (other is StringTypeDFA) return false;
    // Number ⊆ Any/Every: true (all numbers are values)
    if (other.primitiveStateModes.isNotEmpty) return true;
    // For other structured types: false (numbers don't match structures)
    return false;
  }
}

/// DFA that accepts only strings (for String type)
class StringTypeDFA extends TypeDFA {
  StringTypeDFA() : super(
    states: {DFAState('q0'), DFAState('qStr', isFinal: true)},
    startState: DFAState('q0'),
    finalStates: {DFAState('qStr', isFinal: true)},
    transitions: {},
  );
  
  @override
  bool acceptsPath(TermPath path) {
    // String paths are recognized specially
    if (path.length != 1) return false;
    // In practice, strings would be marked distinctly
    // For now, any quoted value
    return path.elements[0].symbol.startsWith('"') ||
           path.elements[0].symbol.startsWith("'");
  }

  @override
  bool get isEmpty => false;  // Accepts all strings (non-empty language)

  @override
  bool isSubsetOf(TypeDFA other) {
    // String ⊆ String: true
    if (other is StringTypeDFA) return true;
    // String ⊆ Number: false (disjoint)
    if (other is NumberTypeDFA) return false;
    // String ⊆ Any/Every: true (all strings are values)
    if (other.primitiveStateModes.isNotEmpty) return true;
    // For other structured types: false
    return false;
  }
}
