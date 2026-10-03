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
/// [open] names base types that stand for type parameters no map has bound
/// yet: a comparison that reaches one, on either side and at either polarity,
/// holds, since a map could bind the parameter to whatever stands opposite it.
/// So with [open] non-empty a false result holds under every binding of the
/// parameters, and is what a call with no instantiation is refused by
/// (well_typed_clause.dart, `_noInstantiationReason`); a true result promises
/// nothing about any one binding.  Empty by default, which is the relation of
/// TGLP well-typing.tex Definition "Subtyping" itself.
///
/// Paper Reference: Definition 4.7 (Subtyping)
bool isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa,
    {Set<String> open = const {}}) {
  return _isSubtype(stateA, stateB, dfa, <String>{}, open);
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
bool _isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa,
    Set<String> visited, Set<String> open) {
  // Coinductive: if we've already assumed this pair, succeed (spec 4.5)
  final pairKey = '${stateA.name}:${stateB.name}';
  if (visited.contains(pairKey)) return true;
  visited.add(pairKey);

  // Reflexivity
  if (stateA == stateB) return true;

  // A parameter no map has bound yet matches whatever stands opposite it.
  if (open.contains(stateA.baseName) || open.contains(stateB.baseName)) {
    return true;
  }

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

  // A defined type against a primitive: below it exactly when every simple
  // prefix of it is accepted by the primitive (TGLP well-typing.tex,
  // Definitions "Prefix Acceptance" and "Subtyping"), which a primitive's
  // automaton decides by its endpoint alone: every primitive the defined type's
  // bare type-name alternatives reach is that primitive ("output type S matches
  // S"), and every transition its start state has of its own is a constant of
  // that primitive ("a constant matches String"), an integer or real literal
  // alternative of `Integer` or `Real` as the defined-type case below reads it
  // ([_primitiveOfConstant]).  So `Key ::= String.` is below `String`, as
  // `String` is below it by the case above, and `Ack ::= ok ; error.` is below
  // `String` as it is below `Key`: the two have one automaton, and the relation
  // is decided by the automata, not by whether the supertype is named a
  // primitive (TGLP well-typing.tex: "<: is checked by a finite simulation
  // between the DFAs of A and B"; modules.tex, "Structural type
  // compatibility").  A compound alternative, or a primitive other than this
  // one, puts a type below no primitive: `Constant` is not below `Integer`.  A
  // type accepting nothing --- an abstract type (parameterized-types.tex,
  // Definition "Abstract Type") --- is below no primitive either.
  //
  // Until 2026-09-18 no defined type was below a primitive, which was
  // unobservable while `Key ::= String.` was erased as an alias (IGLP,
  // 2026-09-18: type_environment_builder.dart `_isSimpleAlias`); once it is a
  // type, a `Key` produced by `self_key/1` and consumed at a `String?`
  // position (programs/social/graph/core/agent.glp:81) needs this case.  Until
  // 2026-10-02 a type with an alternative of its own was below no primitive,
  // so `Ack` was below `Key` and not below `String`.
  if (stateB.isPrimitiveType) {
    final automA = dfa.getAutomaton(stateA.name);
    var accepts = false;
    for (final p in automA.acceptedPrimitives) {
      if (p != stateB.baseName) return false;
      accepts = true;
    }
    for (final entry in automA.transitions.entries) {
      if (entry.key.$1 != stateA) continue;
      final label = entry.key.$2;
      if (label.arity != 0 ||
          _primitiveOfConstant(label.symbol) != stateB.baseName) {
        return false;
      }
      accepts = true;
    }
    return accepts;
  }

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

    // At a position a parameter not yet bound reaches, the mode is the
    // binding's, input or output, so there the label is matched without it.
    if (targetB == null && open.isNotEmpty) {
      final other = _transitionIgnoringMode(automB, stateB, label);
      if (other != null &&
          (open.contains(targetA.baseName) || open.contains(other.baseName))) {
        continue;
      }
    }

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
    if (!_checkTargetSubtype(targetA, targetB, dfa, visited, open)) {
      return false;
    }
  }

  return true;
}

/// The target of [automaton]'s transition from [from] with [label]'s functor,
/// arity and argument position, whatever its mode; null where there is none.
DFAState? _transitionIgnoringMode(
    Automaton automaton, DFAState from, TransitionLabel label) {
  for (final entry in automaton.transitions.entries) {
    final (f, l) = entry.key;
    if (f == from &&
        l.symbol == label.symbol &&
        l.arity == label.arity &&
        l.argIndex == label.argIndex) {
      return entry.value;
    }
  }
  return null;
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
bool _checkTargetSubtype(DFAState targetA, DFAState targetB, ProgramDFA dfa,
    Set<String> visited, Set<String> open) {
  // A parameter no map has bound yet, at either polarity: a map may bind it to
  // an input type as well as an output one, so it matches either.
  if (open.contains(targetA.baseName) || open.contains(targetB.baseName)) {
    return true;
  }

  // Case 1: Both output types → covariant recursion
  if (!targetA.isDual && !targetB.isDual) {
    return _isSubtype(targetA, targetB, dfa, visited, open);
  }

  // Case 2: Both dual types → contravariant recursion (reversed)
  if (targetA.isDual && targetB.isDual) {
    final innerA = dfa.getState(targetA.baseName); // output type A'
    final innerB = dfa.getState(targetB.baseName); // output type B'
    return _isSubtype(innerB, innerA, dfa, visited, open); // REVERSED
  }

  // Case 3: Mixed → incompatible mode structure
  return false;
}

