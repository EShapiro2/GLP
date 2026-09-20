// lib/analysis/type_checker/program_dfa.dart
//
// ProgramDFA implementation per spec: docs/modules/type-dfa.md v0.9
// Paper Reference: Section 4.1 (lines 19-24, 47-53), Definition 4.3 (lines 283-298)

import 'type_ast.dart';
import 'mode.dart';

/// A state in the program DFA.
///
/// States represent:
/// - Defined types in complement pairs (e.g., `Stream`, `Stream?`)
/// - System types: `Integer`/`Integer?`, `String`/`String?`
/// - Primitive finals: `_` (produced), `_?` (consumed)
/// - Anonymous final: `_FINAL_` (for constant/literal matches)
/// - Procedure states: `merge/3` (no complement)
class DFAState {
  final String baseName;
  final bool isDual;
  final bool isFinal;
  final bool isProcedure;  // Fix 3.1: distinguish procedure states

  DFAState(this.baseName, {
    required this.isDual,
    required this.isFinal,
    this.isProcedure = false,
  });

  /// Returns the full name: baseName? if dual, else baseName
  String get name => isDual ? '$baseName?' : baseName;

  /// Returns the dual state
  DFAState get dual =>
      DFAState(baseName,
        isDual: !isDual,
        isFinal: isFinal,
        isProcedure: isProcedure);

  /// True for `_` or `_?`
  bool get isWildcard => baseName == '_';

  /// True for `_` (produced wildcard, non-complement)
  bool get isProducedWildcard => baseName == '_' && !isDual;

  /// True for `_?` (consumed wildcard, complement)
  bool get isConsumedWildcard => baseName == '_' && isDual;

  /// True for `Integer` type state (either complement or not)
  bool get isIntegerType => baseName == 'Integer';

  /// True for `Real` type state (either complement or not)
  bool get isRealType => baseName == 'Real';

  /// True for `String` type state (either complement or not)
  bool get isStringType => baseName == 'String';

  /// True for `Module` type state (either complement or not).
  /// A module term is a compiled module reference; `Module` is a primitive
  /// (TGLP appendix "GLP Language Primitives") and an alternative of the
  /// root-`self.glp` type `Constant`.
  bool get isModuleType => baseName == 'Module';

  /// True for `_FINAL_` (anonymous final for constant/literal matches)
  bool get isAnonymousFinal => baseName == '_FINAL_';

  // Fix 3.2: Computed properties for type classification
  /// True for the primitive types: `_`, Integer, Real, String, Module.
  /// `Number` and `Constant` are NOT primitive — they are unions defined in the
  /// root `self.glp` (TGLP appendix "GLP Language Primitives"), so they behave
  /// as ordinary defined types here and their relation to the primitives comes
  /// out of their definitions.
  bool get isPrimitiveType =>
      isWildcard || isIntegerType || isRealType || isStringType || isModuleType;

  /// True for user-defined types (not primitive, not procedure, not anonymous final)
  bool get isUserDefinedType =>
      !isPrimitiveType && !isProcedure && !isAnonymousFinal;

  @override
  String toString() => name;

  @override
  bool operator ==(Object other) =>
      other is DFAState &&
      other.baseName == baseName &&
      other.isDual == isDual;

  @override
  int get hashCode => Object.hash(baseName, isDual);
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
  factory TransitionLabel.functor(String name, int arity, int argIndex,
      {Mode? mode}) {
    return TransitionLabel._(name, arity, argIndex, mode);
  }

  /// Create a constant transition label.
  factory TransitionLabel.constant(Object value) {
    return TransitionLabel._(value.toString(), 0, 0, null);
  }

  /// Returns a dual label with flipped mode
  TransitionLabel get dual =>
      TransitionLabel._(symbol, arity, argIndex, mode?.flip);

  @override
  String toString() {
    if (arity == 0) return symbol;
    final modeStr =
        mode != null ? ':${mode == Mode.produce ? '↑' : '↓'}' : '';
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

/// An automaton for a single type (either T or T?).
class Automaton {
  final DFAState startState;
  final Map<(DFAState, TransitionLabel), DFAState> _transitions;
  
  /// The primitive type names this type accepts, transitively through its bare
  /// type-name alternatives.  A bare alternative is not a functor and so is not
  /// a transition; it stands for everything the named type accepts, and the
  /// type names a path passes through are not themselves path positions (TGLP
  /// Definition "Moded Paths").  So `Number ::= Integer ; Real.` gives
  /// {Integer, Real}, and `Constant ::= Number ; String ; Module.` gives
  /// {Integer, Real, String, Module}.  This is what makes `Integer <: Constant`
  /// derive from the definitions rather than from a built-in order.
  final Set<String> acceptedPrimitives;

  Automaton(this.startState, this._transitions, {this.acceptedPrimitives = const {}});

  /// Get the target state for a transition, or null if no such transition.
  DFAState? transition(DFAState from, TransitionLabel label) {
    return _transitions[(from, label)];
  }

  /// Get all transitions (for iteration by coverage checker).
  Map<(DFAState, TransitionLabel), DFAState> get transitions => _transitions;

  /// Create dual automaton by flipping all states and modes.
  Automaton get dual {
    final newTransitions = <(DFAState, TransitionLabel), DFAState>{};
    for (final entry in _transitions.entries) {
      final (fromState, label) = entry.key;
      final toState = entry.value;
      newTransitions[(fromState.dual, label.dual)] =
          toState.dual;
    }
    return Automaton(startState.dual, newTransitions,
        acceptedPrimitives: acceptedPrimitives);
  }
}

/// The complete DFA for a typed GLP program.
class ProgramDFA {
  final Map<String, DFAState> states;
  final Map<String, Automaton> automata;

  ProgramDFA(this.states, this.automata);

  /// Get a state by name (e.g., "Stream" or "Stream?").
  DFAState getState(String name) {
    final state = states[name];
    if (state == null) {
      throw StateError('State not found: $name');
    }
    return state;
  }

  /// Get an automaton by type name (e.g., "Stream" or "Stream?").
  Automaton getAutomaton(String typeName) {
    final automaton = automata[typeName];
    if (automaton == null) {
      throw StateError('Automaton not found: $typeName');
    }
    return automaton;
  }
}

/// Error thrown when a type name is not found in the environment.
///
/// Carries the source location of the offending type reference so the checker
/// boundary (checkModule) can surface it as a locatable diagnostic rather than
/// letting an unhandled error escape and kill the caller (known-issues Issue 19).
class UnknownTypeError extends Error {
  final String typeName;
  final int line;
  final int column;
  UnknownTypeError(this.typeName, [this.line = 0, this.column = 0]);

  @override
  String toString() => 'UnknownTypeError: $typeName at line $line, column $column';
}

/// Build the complete program DFA from the type environment.
///
/// Implements spec algorithm: buildProgramDFA(env)
ProgramDFA buildProgramDFA(TypeEnvironment env) {
  final states = <String, DFAState>{};
  final automata = <String, Automaton>{};

  // Create system states (complement pairs)
  states['_'] = DFAState('_', isDual: false, isFinal: true);
  states['_?'] = DFAState('_', isDual: true, isFinal: true);
  states['Integer'] = DFAState('Integer', isDual: false, isFinal: false);
  states['Integer?'] = DFAState('Integer', isDual: true, isFinal: false);
  states['Real'] = DFAState('Real', isDual: false, isFinal: false);
  states['Real?'] = DFAState('Real', isDual: true, isFinal: false);
  states['String'] = DFAState('String', isDual: false, isFinal: false);
  states['String?'] = DFAState('String', isDual: true, isFinal: false);
  states['Module'] = DFAState('Module', isDual: false, isFinal: false);
  states['Module?'] = DFAState('Module', isDual: true, isFinal: false);
  states['_FINAL_'] = DFAState('_FINAL_', isDual: false, isFinal: true);

  // Create automata for system types
  automata['_'] = _finalAutomaton(states['_']!);
  automata['_?'] = _finalAutomaton(states['_?']!);
  automata['Integer'] = _primitiveTypeAutomaton(states['Integer']!, states['_FINAL_']!);
  automata['Integer?'] = _primitiveTypeAutomaton(states['Integer?']!, states['_FINAL_']!);
  automata['Real'] = _primitiveTypeAutomaton(states['Real']!, states['_FINAL_']!);
  automata['Real?'] = _primitiveTypeAutomaton(states['Real?']!, states['_FINAL_']!);
  automata['String'] = _primitiveTypeAutomaton(states['String']!, states['_FINAL_']!);
  automata['String?'] = _primitiveTypeAutomaton(states['String?']!, states['_FINAL_']!);
  automata['Module'] = _primitiveTypeAutomaton(states['Module']!, states['_FINAL_']!);
  automata['Module?'] = _primitiveTypeAutomaton(states['Module?']!, states['_FINAL_']!);

  // Create states for ALL defined types FIRST
  // (Automata may reference other types, so all states must exist before building automata)
  for (final entry in env.types.entries) {
    final typeName = entry.key;
    // Create both T and T? states for each type
    states[typeName] = DFAState(typeName, isDual: false, isFinal: false);
    states['$typeName?'] = DFAState(typeName, isDual: true, isFinal: false);
  }

  // THEN build automata for all defined types
  for (final entry in env.types.entries) {
    final typeName = entry.key;
    final typeDef = entry.value;

    // Build both T and T? automata
    // T automaton: modes as declared
    // T? automaton: all modes flipped, all target states dualized
    automata[typeName] =
        _buildTypeAutomaton(typeDef, states, env.types, isDual: false);
    automata['$typeName?'] =
        _buildTypeAutomaton(typeDef, states, env.types, isDual: true);
  }

  // Create procedure states (no complement)
  for (final entry in env.procedures.entries) {
    states[entry.key] = DFAState(entry.key,
      isDual: false,
      isFinal: false,
      isProcedure: true);
  }

  // Build procedure automata
  for (final entry in env.procedures.entries) {
    final procKey = entry.key;
    final procDecl = entry.value;
    automata[procKey] = _buildProcedureAutomaton(procDecl, states);
  }

  return ProgramDFA(states, automata);
}

/// Create a final automaton (no transitions).
Automaton _finalAutomaton(DFAState state) {
  return Automaton(state, {});
}

/// Create a primitive type automaton (conceptual infinite transitions to _FINAL_).
Automaton _primitiveTypeAutomaton(DFAState state, DFAState finalState) {
  // No explicit transitions - handled specially in leaf consistency
  return Automaton(state, {});
}

/// Build an automaton for a type definition.
///
/// Implements spec algorithm: buildTypeAutomaton
Automaton _buildTypeAutomaton(
  TypeDef typeDef,
  Map<String, DFAState> states,
  Map<String, TypeDef> types, {
  required bool isDual,
}) {
  final typeName = typeDef.name;
  final startStateName = isDual ? '$typeName?' : typeName;
  final startState = states[startStateName]!;

  final transitions = <(DFAState, TransitionLabel), DFAState>{};

  for (final alt in typeDef.alternatives) {
    _addTypeTransitions(
        startState, alt, Mode.produce, states, transitions, isDual,
        types: types, inherited: {typeName});
  }

  return Automaton(startState, transitions,
      acceptedPrimitives: primitiveClosure(typeDef, types));
}

/// The primitive type names [typeDef] accepts through its bare type-name
/// alternatives, transitively.  See [Automaton.acceptedPrimitives].
Set<String> primitiveClosure(TypeDef typeDef, Map<String, TypeDef> types) {
  final result = <String>{};
  final visiting = <String>{};

  void walk(TypeDef def) {
    if (!visiting.add(def.name)) return; // guard against a definition cycle
    for (final alt in def.alternatives) {
      if (alt is! TypeRef || alt.isParameterized) continue;
      if (TypeRef.builtins.contains(alt.name)) {
        result.add(alt.name);
        continue;
      }
      final target = types[alt.name];
      if (target != null) walk(target);
    }
  }

  walk(typeDef);
  return result;
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
  bool isDual, {
  Map<String, TypeDef> types = const {},
  Set<String> inherited = const {},
}) {
  if (alt is ConstantAlt) {
    final label = TransitionLabel.constant(alt.value);
    transitions[(fromState, label)] = states['_FINAL_']!;
  } else if (alt is ListNilAlt) {
    final label = TransitionLabel.constant('[]');
    transitions[(fromState, label)] = states['_FINAL_']!;
  } else if (alt is ListConsAlt) {
    var headMode = _modeOf(alt.head, contextMode);
    var tailMode = _modeOf(alt.tail, contextMode);

    // Apply complement to modes if building complement automaton
    if (isDual) {
      headMode = headMode.flip;
      tailMode = tailMode.flip;
    }

    final headLabel = TransitionLabel.functor('[|]', 2, 1, mode: headMode);
    final tailLabel = TransitionLabel.functor('[|]', 2, 2, mode: tailMode);

    transitions[(fromState, headLabel)] =
        _resolveTypeExpr(alt.head, states, isDual);
    transitions[(fromState, tailLabel)] =
        _resolveTypeExpr(alt.tail, states, isDual);
  } else if (alt is StructAlt) {
    for (var i = 0; i < alt.args.length; i++) {
      final argType = alt.args[i];
      var argMode = _modeOf(argType, contextMode);
      if (isDual) {
        argMode = argMode.flip;
      }
      final label =
          TransitionLabel.functor(alt.functor, alt.args.length, i + 1, mode: argMode);
      transitions[(fromState, label)] =
          _resolveTypeExpr(argType, states, isDual);
    }
  } else if (alt is DiffListAlt) {
    var contentMode = _modeOf(alt.content, contextMode);
    var holeMode = _modeOf(alt.hole, contextMode);
    if (isDual) {
      contentMode = contentMode.flip;
      holeMode = holeMode.flip;
    }

    final contentLabel = TransitionLabel.functor('\\', 2, 1, mode: contentMode);
    final holeLabel = TransitionLabel.functor('\\', 2, 2, mode: holeMode);

    transitions[(fromState, contentLabel)] =
        _resolveTypeExpr(alt.content, states, isDual);
    transitions[(fromState, holeLabel)] =
        _resolveTypeExpr(alt.hole, states, isDual);
  } else if (alt is TypeRef && !alt.isParameterized) {
    // "If A_j references another type S or S?: transitions are inherited from
    // S or S? respectively" (TGLP appendix sec:type-automaton, Type definition
    // transitions) — the union of types with disjoint top-level functors
    // (typed-glp.tex: "An alternative in a type definition may also be a type
    // name S, providing type union"). Without this a term of the named type had
    // no transition from the naming type's state at all: reported by Currencies
    // Code on 2026-09-03 for `T ::= A ; b.  A ::= a(Integer, Constant).`
    // A primitive alternative (`T ::= Integer ; b`) carries no transitions and
    // is accepted through [Automaton.acceptedPrimitives] instead.
    final target = types[alt.name];
    if (target != null && !inherited.contains(alt.name)) {
      // `S?` inherits the dual's transitions: XOR, exactly as [_resolveTypeExpr]
      // composes a nested complement with the automaton's own.
      final inheritedDual = alt.isInput != isDual;
      for (final a in target.alternatives) {
        _addTypeTransitions(
            fromState, a, contextMode, states, transitions, inheritedDual,
            types: types, inherited: {...inherited, alt.name});
      }
    }
  }
  // PrimitiveModeAlt is handled in resolveTypeExpr - it's a leaf, not a constructor
}

/// Resolve a type expression to a DFA state.
///
/// Implements spec algorithm: resolveTypeExpr
/// Uses XOR logic for nested complements.
DFAState _resolveTypeExpr(
    TypeExpr typeExpr, Map<String, DFAState> states, bool isDual) {
  if (typeExpr is PrimitiveModeAlt) {
    // Determine base state: isInput means _? base
    final baseIsComplement = typeExpr.isInput;
    // XOR with automaton complement flag
    final finalIsComplement = baseIsComplement != isDual; // XOR
    return finalIsComplement ? states['_?']! : states['_']!;
  } else if (typeExpr is TypeRef) {
    // Determine base state
    final baseIsComplement = typeExpr.isInput;
    // XOR with automaton complement flag
    final finalIsComplement = baseIsComplement != isDual; // XOR

    if (typeExpr.name == 'Integer') {
      return finalIsComplement ? states['Integer?']! : states['Integer']!;
    }
    if (typeExpr.name == 'Real') {
      return finalIsComplement ? states['Real?']! : states['Real']!;
    }
    if (typeExpr.name == 'String') {
      return finalIsComplement ? states['String?']! : states['String']!;
    }
    if (typeExpr.name == 'Module') {
      return finalIsComplement ? states['Module?']! : states['Module']!;
    }

    final targetName = typeExpr.name;
    final targetState = finalIsComplement ? states['$targetName?'] : states[targetName];
    if (targetState == null) {
      throw UnknownTypeError(targetName, typeExpr.line, typeExpr.column);
    }
    return targetState;
  }
  throw StateError('Cannot resolve type expression: $typeExpr');
}

/// Compute mode for a type expression at a given context mode.
///
/// Implements spec algorithm: modeOf
/// T? flips mode, T keeps mode (before complement is applied)
Mode _modeOf(TypeExpr typeExpr, Mode contextMode) {
  if (typeExpr is TypeRef && typeExpr.isInput) {
    return contextMode.flip;
  }
  if (typeExpr is PrimitiveModeAlt && typeExpr.isInput) {
    return contextMode.flip;
  }
  return contextMode;
}

/// Build an automaton for a procedure declaration.
///
/// Implements spec algorithm: buildProcedureAutomaton
Automaton _buildProcedureAutomaton(
    ProcDecl procDecl, Map<String, DFAState> states) {
  final procState = states[procDecl.qualifiedKey]!;
  final transitions = <(DFAState, TransitionLabel), DFAState>{};

  for (var i = 0; i < procDecl.arity; i++) {
    final argType = procDecl.argTypes[i];
    final label =
        TransitionLabel.functor(procDecl.name, procDecl.arity, i + 1, mode: null);

    // Target is the declared type directly (Stream? or Stream)
    final targetStateName = _getFullTypeName(argType);
    final targetState = states[targetStateName];
    if (targetState == null) {
      throw UnknownTypeError(targetStateName, argType.line, argType.column);
    }
    transitions[(procState, label)] = targetState;
  }

  return Automaton(procState, transitions);
}

/// Get the full type name including ? if input mode.
String _getFullTypeName(TypeExpr typeExpr) {
  if (typeExpr is PrimitiveModeAlt) {
    return typeExpr.isInput ? '_?' : '_';
  }
  if (typeExpr is TypeRef) {
    return typeExpr.isInput ? '${typeExpr.name}?' : typeExpr.name;
  }
  throw StateError('Cannot get type name from: $typeExpr');
}

// ============================================================================
// Leaf Consistency Checking (Definition 4.3)
// ============================================================================

/// Represents a leaf term in a term path.
class LeafTerm {
  final String? name; // Variable name, or null for constants
  final bool isVariable;
  final bool isReader; // Only meaningful if isVariable
  final Mode? mode; // Mode at this position
  final Object? value; // Constant value, or null for variables
  final bool isInteger; // True if value is an integer
  final bool isReal; // True if value is a real (floating-point)
  final bool isString; // True if value is a string

  LeafTerm._({
    this.name,
    required this.isVariable,
    this.isReader = false,
    this.mode,
    this.value,
    this.isInteger = false,
    this.isReal = false,
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
  factory LeafTerm.integerConstant(int value, {Mode? mode}) {
    return LeafTerm._(isVariable: false, value: value, isInteger: true, mode: mode);
  }

  /// Create a real constant leaf.
  factory LeafTerm.realConstant(double value, {Mode? mode}) {
    return LeafTerm._(isVariable: false, value: value, isReal: true, mode: mode);
  }

  /// Create a string constant leaf.
  factory LeafTerm.stringConstant(String value, {Mode? mode}) {
    return LeafTerm._(isVariable: false, value: value, isString: true, mode: mode);
  }

  /// Create a constant leaf (atom or other).
  factory LeafTerm.constant(Object value, {Mode? mode}) {
    return LeafTerm._(isVariable: false, value: value, mode: mode);
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
/// Implements the simplified algorithm using Mode Correspondence Property:
/// - For variables: check path's structural mode matches variable's implicit mode
/// - For constants: check type accepts this constant
LeafConsistencyResult checkLeafConsistency(
  LeafTerm leaf,
  DFAState state,
  ProgramDFA dfa,
) {
  // Case 1: Variable leaf — check path mode matches variable's implicit mode
  // By Mode Correspondence Property, we don't need to inspect state.isDual
  if (leaf.isVariable) {
    // Reader X? must be at consume position (mode ↓)
    // Writer X must be at produce position (mode ↑)
    if (leaf.isReader && leaf.mode == Mode.consume) {
      return LeafConsistencyResult.consistent(state);
    }
    if (!leaf.isReader && leaf.mode == Mode.produce) {
      return LeafConsistencyResult.consistent(state);
    }
    final expected = leaf.isReader ? '↓ (consume)' : '↑ (produce)';
    final actual = leaf.mode == Mode.consume ? '↓ (consume)' : '↑ (produce)';
    return LeafConsistencyResult.inconsistent(
        'Variable mode mismatch: ${leaf.isReader ? "reader" : "writer"} requires $expected, got $actual');
  }

  // Case 2: Constant leaf — check type accepts this constant

  // Case 2a: At anonymous final state (already matched a constant transition)
  if (state.isAnonymousFinal) {
    return LeafConsistencyResult.consistent(state);
  }

  // Case 2b: At primitive type state (Integer, Real, Number, String)
  if (state.isIntegerType) {
    if (leaf.isInteger) {
      return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
    }
    return LeafConsistencyResult.inconsistent('Integer type requires integer literal');
  }

  if (state.isRealType) {
    if (leaf.isReal) {
      return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
    }
    return LeafConsistencyResult.inconsistent('Real type requires real literal');
  }

  if (state.isStringType) {
    if (leaf.isString) {
      return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
    }
    return LeafConsistencyResult.inconsistent('String type requires string literal');
  }

  // Row 8 of the consistency table: a module term matches Module.  A module
  // term is produced by the runtime, never written as a literal, so no source
  // leaf reaches here.
  if (state.isModuleType) {
    return LeafConsistencyResult.inconsistent('Module type requires a module term');
  }

  // Case 2c: At wildcard state
  // Per spec v0.6: _ accepts any produced term (mode ↑), _? accepts any consumed term (mode ↓)
  if (state.isProducedWildcard) {
    if (leaf.mode == Mode.produce) {
      return LeafConsistencyResult.consistent(state);
    }
    return LeafConsistencyResult.inconsistent(
        '_ expects produced term (mode ↑), got consumed term');
  }
  if (state.isConsumedWildcard) {
    if (leaf.mode == Mode.consume) {
      return LeafConsistencyResult.consistent(state);
    }
    return LeafConsistencyResult.inconsistent(
        '_? expects consumed term (mode ↓), got produced term');
  }

  // Case 2d: At user-defined type state — check for matching transition or primitive
  if (leaf.value != null) {
    final automaton = dfa.automata[state.name];
    if (automaton != null) {
      // First, try explicit constant transition
      final constLabel = TransitionLabel.constant(leaf.value!);
      final nextState = automaton.transition(state, constLabel);
      if (nextState != null) {
        return LeafConsistencyResult.consistent(nextState);
      }
      
      // Second, descend the type's bare type-name alternatives to the
      // primitives they stand for, and match the literal against those
      // (consistency table rows 4-6).  `Number ::= Integer ; Real.` accepts a
      // numeric literal; `Constant ::= Number ; String ; Module.` accepts any
      // of them — and, since `[]` is a String, an empty-list literal too.
      final primitives = automaton.acceptedPrimitives;
      if (leaf.isInteger && primitives.contains('Integer')) {
        return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
      }
      if (leaf.isReal && primitives.contains('Real')) {
        return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
      }
      if (leaf.isString && primitives.contains('String')) {
        return LeafConsistencyResult.consistent(dfa.states['_FINAL_']);
      }
    }
  }

  return LeafConsistencyResult.inconsistent(
      'No alternative of ${state.name} matches the constant ${leaf.value}');
}
