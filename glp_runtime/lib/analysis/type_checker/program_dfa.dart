// lib/analysis/type_checker/program_dfa.dart
//
// ProgramDFA implementation per spec: docs/modules/type-dfa.md v0.7
// Paper Reference: Section 4.1 (lines 32-44), Definition 4.3 (lines 247-262)

import 'type_ast.dart';
import 'mode.dart';

/// A state in the program DFA.
///
/// States represent:
/// - Defined types (e.g., `Stream`, `CounterCall`)
/// - Procedures (e.g., `merge/3`, `sum/2`)
/// - System types: `Integer`, `String` (NOT final)
/// - Final states: `_`, `_?`, `_FINAL_`
class DFAState {
  final String name;
  final bool isFinal;

  DFAState(this.name, {required this.isFinal});

  /// True for `_` or `_?`
  bool get isWildcard => name == '_' || name == '_?';

  /// True for `_` (produced wildcard)
  bool get isProducedWildcard => name == '_';

  /// True for `_?` (consumed wildcard)
  bool get isConsumedWildcard => name == '_?';

  /// True for `Integer` type state
  bool get isIntegerType => name == 'Integer';

  /// True for `String` type state
  bool get isStringType => name == 'String';

  /// True for `_FINAL_` (anonymous final for constant/literal matches)
  bool get isAnonymousFinal => name == '_FINAL_';

  @override
  String toString() => name;

  @override
  bool operator ==(Object other) =>
      other is DFAState && other.name == name;

  @override
  int get hashCode => name.hashCode;
}

/// A transition label in the program DFA.
///
/// Labels encode:
/// - symbol: functor name or constant value
/// - arity: number of arguments (0 for constants)
/// - argIndex: 1-based argument position (0 for constants)
/// - mode: mode at this position (null for procedure arg transitions and constants)
class TransitionLabel {
  final String symbol;
  final int arity;
  final int argIndex;
  final Mode? mode;

  TransitionLabel._(this.symbol, this.arity, this.argIndex, this.mode);

  /// Create a functor transition label.
  factory TransitionLabel.functor(String name, int arity, int argIndex, {Mode? mode}) {
    return TransitionLabel._(name, arity, argIndex, mode);
  }

  /// Create a constant transition label.
  factory TransitionLabel.constant(Object value) {
    return TransitionLabel._(value.toString(), 0, 0, null);
  }

  @override
  String toString() {
    if (arity == 0) return symbol;
    final modeStr = mode != null ? ':${mode == Mode.produce ? '↑' : '↓'}' : '';
    return '$symbol($arity,$argIndex)$modeStr';
  }

  @override
  bool operator ==(Object other) =>
      other is TransitionLabel &&
      other.symbol == symbol &&
      other.arity == arity &&
      other.argIndex == argIndex &&
      other.mode == mode;

  @override
  int get hashCode => Object.hash(symbol, arity, argIndex, mode);
}

/// The single DFA for a typed GLP program.
class ProgramDFA {
  final Map<String, DFAState> states;
  final Map<(DFAState, TransitionLabel), DFAState> _transitions;

  ProgramDFA(this.states, this._transitions);

  /// Get a state by name.
  DFAState getState(String name) {
    final state = states[name];
    if (state == null) {
      throw StateError('State not found: $name');
    }
    return state;
  }

  /// Get the target state for a transition, or null if no such transition.
  DFAState? transition(DFAState from, TransitionLabel label) {
    return _transitions[(from, label)];
  }
}

/// Error thrown when a type name is not found in the environment.
class UnknownTypeError extends Error {
  final String typeName;
  UnknownTypeError(this.typeName);

  @override
  String toString() => 'UnknownTypeError: $typeName';
}

/// Build the single program DFA from the type environment.
///
/// Implements spec algorithm: buildProgramDFA(env)
ProgramDFA buildProgramDFA(TypeEnvironment env) {
  final states = <String, DFAState>{};
  final transitions = <(DFAState, TransitionLabel), DFAState>{};

  // Create final states (only _ and _? are true finals accepting variables)
  states['_'] = DFAState('_', isFinal: true);
  states['_?'] = DFAState('_?', isFinal: true);
  states['_FINAL_'] = DFAState('_FINAL_', isFinal: true);

  // Create system type states (NOT final - they have conceptual transitions to _FINAL_)
  states['Integer'] = DFAState('Integer', isFinal: false);
  states['String'] = DFAState('String', isFinal: false);

  // Create states for defined types
  for (final typeName in env.types.keys) {
    states[typeName] = DFAState(typeName, isFinal: false);
  }

  // Create states for procedures
  for (final procKey in env.procedures.keys) {
    states[procKey] = DFAState(procKey, isFinal: false);
  }

  // Add transitions from type definitions
  for (final entry in env.types.entries) {
    final typeName = entry.key;
    final typeDef = entry.value;
    final fromState = states[typeName]!;
    for (final alt in typeDef.alternatives) {
      _addTypeTransitions(fromState, alt, Mode.produce, states, transitions);
    }
  }

  // Add transitions from procedure declarations
  for (final entry in env.procedures.entries) {
    final procKey = entry.key;
    final procDecl = entry.value;
    final fromState = states[procKey]!;
    for (var i = 0; i < procDecl.arity; i++) {
      final argType = procDecl.argTypes[i];
      final label = TransitionLabel.functor(procDecl.name, procDecl.arity, i + 1);
      final targetState = _resolveTypeExpr(argType, states);
      transitions[(fromState, label)] = targetState;
    }
  }

  return ProgramDFA(states, transitions);
}

/// Add transitions from a type alternative.
///
/// Implements spec algorithm: addTypeTransitions
void _addTypeTransitions(
  DFAState fromState,
  TypeExpr alt,
  Mode contextMode,
  Map<String, DFAState> states,
  Map<(DFAState, TransitionLabel), DFAState> transitions,
) {
  if (alt is ConstantAlt) {
    final label = TransitionLabel.constant(alt.value);
    transitions[(fromState, label)] = states['_FINAL_']!;
  } else if (alt is ListNilAlt) {
    final label = TransitionLabel.constant('[]');
    transitions[(fromState, label)] = states['_FINAL_']!;
  } else if (alt is ListConsAlt) {
    final headMode = _modeOf(alt.head, contextMode);
    final tailMode = _modeOf(alt.tail, contextMode);

    final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: headMode);
    final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: tailMode);

    transitions[(fromState, headLabel)] = _resolveTypeExpr(alt.head, states);
    transitions[(fromState, tailLabel)] = _resolveTypeExpr(alt.tail, states);
  } else if (alt is StructAlt) {
    for (var i = 0; i < alt.args.length; i++) {
      final argType = alt.args[i];
      final argMode = _modeOf(argType, contextMode);
      final label = TransitionLabel.functor(alt.functor, alt.args.length, i + 1, mode: argMode);
      transitions[(fromState, label)] = _resolveTypeExpr(argType, states);
    }
  } else if (alt is DiffListAlt) {
    final contentMode = _modeOf(alt.content, contextMode);
    final holeMode = _modeOf(alt.hole, contextMode);

    final contentLabel = TransitionLabel.functor('\\\\', 2, 1, mode: contentMode);
    final holeLabel = TransitionLabel.functor('\\\\', 2, 2, mode: holeMode);

    transitions[(fromState, contentLabel)] = _resolveTypeExpr(alt.content, states);
    transitions[(fromState, holeLabel)] = _resolveTypeExpr(alt.hole, states);
  }
  // PrimitiveModeAlt is handled differently - it's a leaf, not a constructor
}

/// Resolve a type expression to a DFA state.
///
/// Implements spec algorithm: resolveTypeExpr
DFAState _resolveTypeExpr(TypeExpr typeExpr, Map<String, DFAState> states) {
  if (typeExpr is PrimitiveModeAlt) {
    // _ and _? are final states
    return typeExpr.isInput ? states['_?']! : states['_']!;
  } else if (typeExpr is TypeRef) {
    // Integer and String are type states (not final)
    if (typeExpr.name == 'Integer') return states['Integer']!;
    if (typeExpr.name == 'String') return states['String']!;
    // Note: isInput flag is NOT used here - complementation happens during path checking
    final state = states[typeExpr.name];
    if (state == null) {
      throw UnknownTypeError(typeExpr.name);
    }
    return state;
  }
  throw StateError('Cannot resolve type expression: $typeExpr');
}

/// Compute mode for a type expression at a given context mode.
///
/// Implements spec algorithm: modeOf
/// T? flips mode, T keeps mode
Mode _modeOf(TypeExpr typeExpr, Mode contextMode) {
  if (typeExpr is TypeRef && typeExpr.isInput) {
    return contextMode.flip;
  }
  if (typeExpr is PrimitiveModeAlt && typeExpr.isInput) {
    return contextMode.flip;
  }
  return contextMode;
}

// ============================================================================
// Leaf Consistency Checking (Definition 4.3)
// ============================================================================

/// Represents a leaf term in a term path.
class LeafTerm {
  final String? name;       // Variable name, or null for constants
  final bool isVariable;
  final bool isReader;      // Only meaningful if isVariable
  final Mode? mode;         // Mode at this position
  final Object? value;      // Constant value, or null for variables
  final bool isInteger;     // True if value is an integer
  final bool isString;      // True if value is a string

  LeafTerm._({
    this.name,
    required this.isVariable,
    this.isReader = false,
    this.mode,
    this.value,
    this.isInteger = false,
    this.isString = false,
  });

  /// Create a writer variable leaf.
  factory LeafTerm.writer(String name, {required Mode mode}) {
    return LeafTerm._(name: name, isVariable: true, isReader: false, mode: mode);
  }

  /// Create a reader variable leaf.
  factory LeafTerm.reader(String name, {required Mode mode}) {
    return LeafTerm._(name: name, isVariable: true, isReader: true, mode: mode);
  }

  /// Create an integer constant leaf.
  factory LeafTerm.integerConstant(int value) {
    return LeafTerm._(isVariable: false, value: value, isInteger: true);
  }

  /// Create a string constant leaf.
  factory LeafTerm.stringConstant(String value) {
    return LeafTerm._(isVariable: false, value: value, isString: true);
  }

  /// Create a constant leaf (atom or other).
  factory LeafTerm.constant(Object value) {
    return LeafTerm._(isVariable: false, value: value);
  }
}

/// Result of checking leaf consistency.
class LeafConsistencyResult {
  final bool isConsistent;
  final DFAState? type;
  final String? reason;

  LeafConsistencyResult.consistent(this.type)
      : isConsistent = true,
        reason = null;

  LeafConsistencyResult.inconsistent(this.reason)
      : isConsistent = false,
        type = null;
}

/// Check leaf consistency per Definition 4.3.
///
/// Implements spec algorithm: checkLeafConsistency
LeafConsistencyResult checkLeafConsistency(
  LeafTerm leaf,
  DFAState state,
  ProgramDFA dfa, {
  required bool complement,
}) {
  // Compute expected mode with complement adjustment
  Mode? expectedMode = leaf.mode;
  if (complement && expectedMode != null) {
    expectedMode = expectedMode.flip;
  }

  // For wildcard states, complement flips which wildcard behavior applies
  // _ with complement behaves like _?, and vice versa
  final effectiveProducedWildcard = complement ? state.isConsumedWildcard : state.isProducedWildcard;
  final effectiveConsumedWildcard = complement ? state.isProducedWildcard : state.isConsumedWildcard;

  // Case: Produced wildcard final state (_) or _? with complement
  if (effectiveProducedWildcard) {
    // Definition 4.3 case 3(b): type path ends in _ and term has produce mode
    if (leaf.isVariable && !leaf.isReader && expectedMode == Mode.produce) {
      return LeafConsistencyResult.consistent(state);
    }
    return LeafConsistencyResult.inconsistent('_ expects writer at produce position');
  }

  // Case: Consumed wildcard final state (_?) or _ with complement
  if (effectiveConsumedWildcard) {
    // Definition 4.3 case 3(a): type path ends in _? and term has consume mode
    if (leaf.isVariable && leaf.isReader && expectedMode == Mode.consume) {
      return LeafConsistencyResult.consistent(state);
    }
    return LeafConsistencyResult.inconsistent('_? expects reader at consume position');
  }

  // Case: Integer type state (conceptual infinite transitions)
  if (state.isIntegerType) {
    if (leaf.isInteger) {
      // Conceptually: follow transition labeled with this integer to _FINAL_
      return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
    }
    if (leaf.isVariable) {
      // Definition 4.3 case 2: term path is prefix ending in variable
      if (leaf.isReader && expectedMode == Mode.consume) {
        return LeafConsistencyResult.consistent(state);
      }
      if (!leaf.isReader && expectedMode == Mode.produce) {
        return LeafConsistencyResult.consistent(state);
      }
      return LeafConsistencyResult.inconsistent('Variable mode mismatch at Integer');
    }
    return LeafConsistencyResult.inconsistent('Integer type requires integer literal or variable');
  }

  // Case: String type state (conceptual infinite transitions)
  if (state.isStringType) {
    if (leaf.isString) {
      // Conceptually: follow transition labeled with this string to _FINAL_
      return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
    }
    if (leaf.isVariable) {
      if (leaf.isReader && expectedMode == Mode.consume) {
        return LeafConsistencyResult.consistent(state);
      }
      if (!leaf.isReader && expectedMode == Mode.produce) {
        return LeafConsistencyResult.consistent(state);
      }
      return LeafConsistencyResult.inconsistent('Variable mode mismatch at String');
    }
    return LeafConsistencyResult.inconsistent('String type requires string literal or variable');
  }

  // Case: Anonymous final state (reached via exact constant match)
  if (state.isAnonymousFinal) {
    // Definition 4.3 case 1: equal length, last symbols consistent
    // We only reach here if a constant transition was followed, so it matched
    return LeafConsistencyResult.consistent(state);
  }

  // Case: Non-final type state with variable
  // Definition 4.3 case 2: term path is prefix ending in reader/writer
  if (leaf.isVariable) {
    if (leaf.isReader && expectedMode == Mode.consume) {
      return LeafConsistencyResult.consistent(state); // Case 2(a)
    }
    if (!leaf.isReader && expectedMode == Mode.produce) {
      return LeafConsistencyResult.consistent(state); // Case 2(b)
    }
    return LeafConsistencyResult.inconsistent('Variable mode mismatch at type position');
  }

  // Case: Non-final type state with constant - must follow transition
  // Definition 4.3 case 1: check if constant matches a transition
  if (leaf.value != null) {
    final constLabel = TransitionLabel.constant(leaf.value!);
    final nextState = dfa.transition(state, constLabel);
    if (nextState != null) {
      return LeafConsistencyResult.consistent(nextState);
    }
  }

  return LeafConsistencyResult.inconsistent('Constant at type state without matching transition');
}
