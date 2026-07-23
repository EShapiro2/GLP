// lib/analysis/type_checker/subtyping.dart
//
// Subtyping for GLP output types.
// Specification: docs/type system/subtyping.md
// Paper Reference: Section 4.6, Definition 4.7 (Subtyping)
//
// A <: B iff every simple prefix of A is accepted by B,
// and at mode inversion points the direction reverses (contravariance).

import 'program_dfa.dart';

/// Check if output type A is a subtype of output type B.
///
/// Both stateA and stateB must be output types (isDual == false).
/// Uses coinductive algorithm with visited set for cycle detection.
///
/// Paper Reference: Definition 4.7 (Subtyping)
bool isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa) {
  return _isSubtype(stateA, stateB, dfa, <String>{});
}

/// Structural type identity (typed-program §20.3): two base type names denote
/// the same type if they are equal, or if their output automata are mutually
/// subtypes — e.g. the named list alias `OutputsList` / `FriendStream` and
/// `Stream<OutputEntry>` / `Stream<FriendMsg>`.  Equality already honours
/// structural identity; this lets the duality / same-base checks honour it too,
/// so a named alias is not treated as a distinct type.  The single primitive all
/// type-identity comparisons go through (DISCIPLINE §1.3), in place of comparing
/// `baseName` strings.
bool sameBaseType(String baseA, String baseB, ProgramDFA dfa) {
  if (baseA == baseB) return true;
  final DFAState a, b;
  try {
    a = dfa.getState(baseA);
    b = dfa.getState(baseB);
  } on StateError {
    return false; // an unknown base name cannot be structurally matched
  }
  if (a.isDual || b.isDual) return false; // compare output (non-dual) states
  return isSubtype(a, b, dfa) && isSubtype(b, a, dfa);
}

/// Core coinductive subtyping algorithm.
///
/// Spec section 4.1: isSubtype(stateA, stateB, dfa, visited)
bool _isSubtype(
    DFAState stateA, DFAState stateB, ProgramDFA dfa, Set<String> visited) {
  // Coinductive: if we've already assumed this pair, succeed (spec 4.5)
  final pairKey = '${stateA.name}:${stateB.name}';
  if (visited.contains(pairKey)) return true;
  visited.add(pairKey);

  // Reflexivity
  if (stateA == stateB) return true;

  // Both must be output types (not dual)
  assert(!stateA.isDual && !stateB.isDual);

  // Wildcard/final handling (spec 4.4)
  // T <: _ for every output type T (Definition "Primitive Subtype Order"),
  // and at a prefix endpoint `_` matches only `_` (Definition "Prefix
  // Acceptance").  _FINAL_ is treated as equivalent to _ for subtyping.
  if (stateB.isWildcard || stateB.isAnonymousFinal) return true;
  if (stateA.isWildcard || stateA.isAnonymousFinal) return false;

  // Primitive subtype order (Definition "Primitive Subtype Order") — the base
  // case of the relation.
  if (stateA.isPrimitiveType && stateB.isPrimitiveType) {
    return _checkPrimitiveSubtype(stateA, stateB);
  }

  // c <: Constant for every symbolic constant c, lifted through union: a type
  // whose alternatives are all symbolic constants, or primitives below
  // Constant, produces only constants and so is a subtype of Constant.
  if (stateB.isConstantType) {
    return _isConstantUnion(stateA, dfa);
  }

  // A primitive and a user-defined type are unrelated except through the
  // wildcard and Constant cases above.
  if (stateA.isPrimitiveType || stateB.isPrimitiveType) return false;

  // User-defined types: check transitions (spec 4.1)
  final automA = dfa.getAutomaton(stateA.name);
  final automB = dfa.getAutomaton(stateB.name);

  // Every transition from A must have a matching transition in B
  for (final entry in automA.transitions.entries) {
    final (fromState, label) = entry.key;
    // Only check transitions from the start state of A
    if (fromState != stateA) continue;

    final targetA = entry.value;
    final targetB = automB.transition(stateB, label);

    // A has a transition B lacks → not a subtype
    if (targetB == null) return false;

    // Skip trivially equal targets
    if (targetA == targetB) continue;

    // Check target compatibility (spec 4.2)
    if (!_checkTargetSubtype(targetA, targetB, dfa, visited)) return false;
  }

  return true;
}

/// Target compatibility check (spec section 4.2).
///
/// Handles covariance for output positions and contravariance at mode inversions.
bool _checkTargetSubtype(
    DFAState targetA, DFAState targetB, ProgramDFA dfa, Set<String> visited) {
  // Case 1: Both output types → covariant recursion
  if (!targetA.isDual && !targetB.isDual) {
    return _isSubtype(targetA, targetB, dfa, visited);
  }

  // Case 2: Both dual types → contravariant recursion (reversed)
  if (targetA.isDual && targetB.isDual) {
    final innerA = dfa.getState(targetA.baseName); // output type A'
    final innerB = dfa.getState(targetB.baseName); // output type B'
    return _isSubtype(innerB, innerA, dfa, visited); // REVERSED
  }

  // Case 3: Mixed → incompatible mode structure
  return false;
}

/// The primitive subtype order (Definition "Primitive Subtype Order",
/// `TGLP/sections/typed-glp.tex`): the least partial order with
/// `Integer <: Number`, `Real <: Number`, `Number <: Constant`,
/// `String <: Constant`, `c <: Constant` for every symbolic constant `c`, and
/// `T <: _` for every output type `T`.  Being least, it is reflexive and
/// transitive and relates nothing else — in particular `Constant` is not a
/// subtype of `Integer`, so the order is directional.
///
/// The table gives each primitive's supertypes, its own transitive closure.
/// `_` is handled by the caller (it is the top of the order); the symbolic
/// constants are not DFA states of their own — a type whose alternatives are
/// all symbolic constants is handled by [_isConstantUnion].
const Map<String, Set<String>> _primitiveSupertypes = {
  'Integer': {'Integer', 'Number', 'Constant'},
  'Real': {'Real', 'Number', 'Constant'},
  'Number': {'Number', 'Constant'},
  'String': {'String', 'Constant'},
  'Constant': {'Constant'},
};

/// Check the primitive subtype order between two primitive states.
bool _checkPrimitiveSubtype(DFAState stateA, DFAState stateB) {
  // _ is top for output types (handled in caller, but be safe)
  if (stateB.isWildcard) return true;
  if (stateA.isWildcard) return false;

  final supertypes = _primitiveSupertypes[stateA.baseName];
  if (supertypes == null) return stateA.baseName == stateB.baseName;
  return supertypes.contains(stateB.baseName);
}

/// True if every value of user-defined type [state] is a constant, so that
/// `state <: Constant` by the primitive order: each alternative is either a
/// symbolic constant (a nullary alternative, which in the DFA is a constant
/// transition) or a primitive below Constant.  A type with no alternative at
/// all admits no value and is not counted.
bool _isConstantUnion(DFAState state, ProgramDFA dfa) {
  final automaton = dfa.automata[state.name];
  if (automaton == null) return false;

  var hasAlternative = false;
  for (final entry in automaton.transitions.entries) {
    final (fromState, label) = entry.key;
    if (fromState != state) continue;
    hasAlternative = true;
    // A constant transition carries arity 0; anything else is a compound
    // alternative, whose values are not constants.
    if (label.arity != 0) return false;
  }

  for (final primitive in automaton.acceptedPrimitives) {
    hasAlternative = true;
    if (!(_primitiveSupertypes[primitive]?.contains('Constant') ?? false)) {
      return false;
    }
  }

  return hasAlternative;
}
