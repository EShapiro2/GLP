// lib/analysis/type_checker/subtyping.dart
//
// Subtyping for GLP output types.
// Specification: TGLP (Moded-Types), sections/well-typing.tex §Subtyping;
// algorithm in sections/appendix-implementation-notes.tex §Subtype Checking
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

/// Structural type identity (TGLP sections/modules.tex, "Structural type
/// compatibility": two types with the same type automaton are compatible,
/// regardless of their names or defining modules): two base type names denote
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
  // At a prefix endpoint `_` matches only `_` (Definition "Prefix Acceptance"),
  // so `_` is top: every output type is below it, and it is below nothing else.
  // _FINAL_ is treated as equivalent to _ for subtyping.
  if (stateB.isWildcard || stateB.isAnonymousFinal) return true;
  if (stateA.isWildcard || stateA.isAnonymousFinal) return false;

  // Two primitives are related only when identical (Definition "Prefix
  // Acceptance": an output type S matches S or `_`).  There is no built-in
  // order among them — `Integer <: Number` comes from `Number`'s definition,
  // below.
  if (stateA.isPrimitiveType && stateB.isPrimitiveType) {
    return stateA.baseName == stateB.baseName;
  }

  // A primitive against a defined type: the defined type accepts it when the
  // primitive is among those its bare type-name alternatives reach.  This is
  // what makes `Integer <: Number` and `Integer <: Constant` derive from
  // `Number ::= Integer ; Real.` and `Constant ::= Number ; String ; Module.`
  if (stateA.isPrimitiveType) {
    final automB = dfa.automata[stateB.name];
    return automB != null && automB.acceptedPrimitives.contains(stateA.baseName);
  }

  // A defined type is never below a primitive: a primitive has no alternatives
  // to accept it.  This keeps the relation directional — `Constant` is not a
  // subtype of `Integer`.
  if (stateB.isPrimitiveType) return false;

  // User-defined types: check transitions (spec 4.1)
  final automA = dfa.getAutomaton(stateA.name);
  final automB = dfa.getAutomaton(stateB.name);

  // Everything A's bare type-name alternatives accept, B must accept too.
  if (!automB.acceptedPrimitives.containsAll(automA.acceptedPrimitives)) {
    return false;
  }

  // Every transition from A must have a matching transition in B
  for (final entry in automA.transitions.entries) {
    final (fromState, label) = entry.key;
    // Only check transitions from the start state of A
    if (fromState != stateA) continue;

    final targetA = entry.value;
    final targetB = automB.transition(stateB, label);

    if (targetB == null) {
      // A constant alternative is a value of a primitive type, so B accepts it
      // whenever B accepts that primitive: `Ack ::= ok ; error.` is below
      // `Constant ::= Number ; String ; Module.` because `ok` and `error` are
      // strings.  This does not run the other way — an arbitrary String is not
      // an `Ack` — because B's primitives must still be contained in A's, which
      // is checked above.
      if (label.arity == 0 &&
          automB.acceptedPrimitives.contains(_primitiveOfConstant(label.symbol))) {
        continue;
      }
      // A has an alternative B lacks → not a subtype
      return false;
    }

    // Skip trivially equal targets
    if (targetA == targetB) continue;

    // Check target compatibility (spec 4.2)
    if (!_checkTargetSubtype(targetA, targetB, dfa, visited)) return false;
  }

  return true;
}

/// The primitive type a constant alternative belongs to.  `[]` and every
/// unquoted constant are strings — GLP has one representation for a quoted
/// string and an unquoted constant.
String _primitiveOfConstant(String symbol) {
  if (int.tryParse(symbol) != null) return 'Integer';
  if (double.tryParse(symbol) != null) return 'Real';
  return 'String';
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

