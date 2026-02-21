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
  // _FINAL_ is treated as equivalent to _ for subtyping purposes
  if (stateB.isWildcard || stateB.isAnonymousFinal) return true;
  if (stateA.isWildcard || stateA.isAnonymousFinal) return false;

  // Primitive type lattice (spec 4.3)
  if (stateA.isPrimitiveType || stateB.isPrimitiveType) {
    return _checkPrimitiveSubtype(stateA, stateB);
  }

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

/// Primitive type subtyping lattice (spec section 4.3).
///
/// Integer <: Number, Real <: Number.
/// Otherwise, primitive types must be identical.
bool _checkPrimitiveSubtype(DFAState stateA, DFAState stateB) {
  // _ is top for output types (handled in caller, but be safe)
  if (stateB.isWildcard) return true;
  if (stateA.isWildcard) return false;

  // Integer <: Number
  if (stateA.isIntegerType && stateB.isNumberType) return true;

  // Real <: Number
  if (stateA.isRealType && stateB.isNumberType) return true;

  // Otherwise must be identical
  return stateA.baseName == stateB.baseName;
}
