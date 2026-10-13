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
  return _isSubtype(stateA, stateB, dfa, <String>{}, open, null);
}

/// Whether output type [stateA], with `Opaque` replaced throughout by the
/// opaque type [labelled] (an opaque type labelled h, [DFAState.isRunOpaqueType]),
/// is a subtype of output type [stateB]: the one step back from `Opaque`
/// (TGLP modules.tex, "Opaque types": "in a goal that run activates at an
/// opaque instance at h, a variable of type T° may stand at a position typed
/// T, for a type T whose opaque types are labelled h"; GLP, 2026-10-13 20:04
/// UTC, point 3: "the step back substitutes Opaque -> h throughout the type of
/// the run goal's argument").  The replacement follows [stateA]'s own
/// structure through every mode inversion, where the comparison reverses
/// (well_typed_clause.dart, `_stepBack`).
bool isSubtypeWithOpaqueAs(
    DFAState stateA, DFAState stateB, DFAState labelled, ProgramDFA dfa) {
  return _isSubtype(
      stateA, stateB, dfa, <String>{}, const {}, _OpaqueAs(labelled, true));
}

/// The replacement of `Opaque` by an opaque type labelled h on one side of a
/// comparison ([isSubtypeWithOpaqueAs]): on the left where [onLeft], and the
/// side changes at each mode inversion, where the comparison reverses.
class _OpaqueAs {
  final DFAState labelled;
  final bool onLeft;
  const _OpaqueAs(this.labelled, this.onLeft);
  _OpaqueAs get reversed => _OpaqueAs(labelled, !onLeft);
  DFAState left(DFAState s) => onLeft && s.isOpaqueUnion ? labelled : s;
  DFAState right(DFAState s) => !onLeft && s.isOpaqueUnion ? labelled : s;
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

/// Whether a parameter's abstract type is below `Opaque`, as every opaque type
/// is ([isSubtype]); false only inside [withParametersNotBelowOpaque].
bool _parameterBelowOpaque = true;

/// [f] run with a parameter's abstract type below `Opaque` no longer: the
/// probe by which certification finds whether a parametrically well-typed
/// procedure is so only because a parameter's term stands at an `Opaque`
/// position (type_checker.dart, [certifyParametricProcedures]).  An opaque
/// type labelled h at which a goal run/3 activates is read
/// ([DFAState.isRunOpaqueType]) stays below it: it is no parameter's.
T withParametersNotBelowOpaque<T>(T Function() f) {
  final was = _parameterBelowOpaque;
  _parameterBelowOpaque = false;
  try {
    return f();
  } finally {
    _parameterBelowOpaque = was;
  }
}

/// The subtyping relation of TGLP well-typing.tex, Definition "Subtyping",
/// extended by T <: T° for every type T (parameterized-types.tex, Definition
/// "Opaque Type": "For a type T, T° is T with every opaque type replaced by
/// Opaque, and the subtyping relation of Definition Subtyping is extended by
/// T <: T° for every type T"; GLP, 2026-10-13 20:04 UTC, point 3).  The
/// extension holds at every pair the simulation compares, a mode inversion's
/// reversed pair among them, as condition 2 of Definition "Subtyping" asks
/// "B' is a subtype of A'" of the relation so extended: so
/// `RootChannel(h) <: RootChannel(Opaque)` though `Stream(Opaque)` is not below
/// `Stream(h)` at the channel's input position (Opaque <: h does NOT follow).
bool _isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa,
    Set<String> visited, Set<String> open, _OpaqueAs? as) {
  if (as != null) {
    stateA = as.left(stateA);
    stateB = as.right(stateB);
  }
  if (_isSubtypeCore(stateA, stateB, dfa, visited, open, as)) return true;
  if (open.contains(stateA.baseName) || open.contains(stateB.baseName)) {
    return false;
  }
  return _reachesOpaque(stateB, dfa) &&
      _isOpaqueClosureOf(stateA, stateB, dfa, <String>{});
}

/// Whether [stateB] is [stateA]° (Definition "Opaque Type"): [stateA] with
/// every opaque type --- a parameter's abstract type, an opaque type labelled
/// h --- replaced by `Opaque`, and nothing else changed, at every position and
/// at both modes.  Two states with the same transitions are one type here, by
/// their automata (TGLP modules.tex, "Structural type compatibility").  A
/// parameter's abstract type counts as an opaque type except inside
/// [withParametersNotBelowOpaque], as it does at a bare position ([_isSubtypeCore]).
bool _isOpaqueClosureOf(
    DFAState a, DFAState b, ProgramDFA dfa, Set<String> visited) {
  if (a.isDual != b.isDual) return false;
  if (a.isDual) {
    final DFAState oa, ob;
    try {
      oa = dfa.getState(a.baseName);
      ob = dfa.getState(b.baseName);
    } on StateError {
      return false;
    }
    return _isOpaqueClosureOf(oa, ob, dfa, visited);
  }
  if (!visited.add('${a.name}:${b.name}')) return true;
  if (a.isAbstractType) {
    return b.isOpaqueUnion && (_parameterBelowOpaque || a.isRunOpaqueType);
  }
  if (a.isOpaqueUnion || b.isOpaqueUnion) return a.isOpaqueUnion && b.isOpaqueUnion;
  if (b.isAbstractType) return false;
  if (a.isPrimitiveType || a.isAnonymousFinal || a.isProcedure ||
      b.isPrimitiveType || b.isAnonymousFinal || b.isProcedure) {
    return a == b;
  }
  final automA = dfa.automata[a.name];
  final automB = dfa.automata[b.name];
  if (automA == null || automB == null) return false;
  if (automA.acceptedPrimitives.length != automB.acceptedPrimitives.length ||
      !automA.acceptedPrimitives.containsAll(automB.acceptedPrimitives)) {
    return false;
  }
  final fromA = {
    for (final e in automA.transitions.entries)
      if (e.key.$1 == a) e.key.$2: e.value
  };
  final fromB = {
    for (final e in automB.transitions.entries)
      if (e.key.$1 == b) e.key.$2: e.value
  };
  if (fromA.length != fromB.length) return false;
  for (final e in fromA.entries) {
    final tb = fromB[e.key];
    if (tb == null) return false;
    if (!_isOpaqueClosureOf(e.value, tb, dfa, visited)) return false;
  }
  return true;
}

/// Whether `Opaque` occurs in output type [s] at some position, at either
/// mode: where it does not, no type has [s] for its T° but [s] itself, which
/// the simulation decides already.  Memoised per program DFA by state name,
/// a DFA growing only by new states ([addTypesToProgramDFA]).
bool _reachesOpaque(DFAState s, ProgramDFA dfa) {
  final memo = _reachesOpaqueMemo[dfa] ??= <String, bool>{};
  final known = memo[s.name];
  if (known != null) return known;
  final seen = <String>{};
  final stack = <DFAState>[s];
  var found = false;
  while (stack.isNotEmpty && !found) {
    var x = stack.removeLast();
    if (x.isDual) {
      final o = dfa.states[x.baseName];
      if (o == null) continue;
      x = o;
    }
    if (!seen.add(x.name)) continue;
    if (x.isOpaqueUnion) {
      found = true;
      break;
    }
    final m = memo[x.name];
    if (m == true) {
      found = true;
      break;
    }
    if (m == false) continue;
    final autom = dfa.automata[x.name];
    if (autom == null) continue;
    for (final e in autom.transitions.entries) {
      if (e.key.$1 == x) stack.add(e.value);
    }
  }
  memo[s.name] = found;
  return found;
}

final Expando<Map<String, bool>> _reachesOpaqueMemo = Expando();

/// Core coinductive subtyping algorithm: Definition "Subtyping"'s two
/// conditions, each pair below compared by [_isSubtype], the relation the
/// extension by T <: T° completes.
///
/// Spec section 4.1: isSubtype(stateA, stateB, dfa, visited)
bool _isSubtypeCore(DFAState stateA, DFAState stateB, ProgramDFA dfa,
    Set<String> visited, Set<String> open, _OpaqueAs? as) {
  // Coinductive: if we've already assumed this pair, succeed (spec 4.5)
  final pairKey =
      '${stateA.name}:${stateB.name}${as == null ? '' : ':${as.onLeft}'}';
  if (visited.contains(pairKey)) return true;
  visited.add(pairKey);

  // Reflexivity.  Under a replacement of `Opaque` ([isSubtypeWithOpaqueAs])
  // only where no `Opaque` lies below, which the replacement would change.
  if (stateA == stateB && (as == null || !_reachesOpaque(stateA, dfa))) {
    return true;
  }

  // A parameter no map has bound yet matches whatever stands opposite it.
  if (open.contains(stateA.baseName) || open.contains(stateB.baseName)) {
    return true;
  }

  // Both must be output types (not dual)
  assert(!stateA.isDual && !stateB.isDual);

  // An abstract type (TGLP parameterized-types.tex, Definition "Abstract
  // Type"): "As the endpoint of a simple prefix it matches only itself
  // (Definition Prefix Acceptance), and not the wildcard."  Its one simple
  // prefix ends at itself, so by Definition "Subtyping", condition 1, it is
  // below itself alone --- not below `_`, not below another abstract type
  // (Definition "Parametrically Well-Typed": "a distinct abstract type" for
  // each parameter) --- and no other type, whose simple prefixes end at
  // something else, is below it.  The pair identical returned above; here
  // either side abstract is a pair of two types.  Until 2026-10-12 an
  // abstract type, with no simple prefix of its own, was below every type, the
  // wildcard among them, and two were one type by their automata: a
  // parameter's value was certified where a concrete type, `_` or another
  // parameter was declared (programs/tests/strict_certification/).
  //
  // `Opaque`, the union of all opaque types (Definition "Opaque Type"): "a
  // state with no outgoing transitions, distinct from the wildcard and from
  // every opaque type, matched by itself and by every opaque type, and by
  // nothing else".  So every opaque type --- a parameter's abstract type, an
  // opaque type labelled h --- is below it, and nothing else is: not `_`, not
  // a concrete type, whose simple prefixes end at something else.  And it is
  // below itself alone: not below `_`, as an opaque type is not ("a program
  // can only pass a term of it on", GLP-Spec appendix-guards.tex, "Predefined
  // types"), and not below any one opaque type --- the one step back from it,
  // in a goal that run/3 activates, is the clause check's
  // (well_typed_clause.dart, [checkClause]), not a subtyping.
  if (stateA.isOpaqueUnion || stateB.isOpaqueUnion) {
    return stateB.isOpaqueUnion &&
        stateA.isAbstractType &&
        (_parameterBelowOpaque || stateA.isRunOpaqueType);
  }

  if (stateA.isAbstractType || stateB.isAbstractType) {
    return stateA.baseName == stateB.baseName;
  }

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
          primitiveOfConstant(label.constant) != stateB.baseName) {
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
          automB.acceptedPrimitives.contains(primitiveOfConstant(label.constant))) {
        continue;
      }
      // A has an alternative B lacks → not a subtype
      return false;
    }

    // Skip trivially equal targets, except under a replacement of `Opaque`
    // where one lies below them ([isSubtypeWithOpaqueAs]).
    if (targetA == targetB &&
        (as == null ||
            !_reachesOpaque(
                targetA.isDual ? dfa.getState(targetA.baseName) : targetA,
                dfa))) {
      continue;
    }

    // Check target compatibility (spec 4.2)
    if (!_checkTargetSubtype(targetA, targetB, dfa, visited, open, as)) {
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

/// Target compatibility check (spec section 4.2).
///
/// Handles covariance for output positions and contravariance at mode inversions.
bool _checkTargetSubtype(DFAState targetA, DFAState targetB, ProgramDFA dfa,
    Set<String> visited, Set<String> open, _OpaqueAs? as) {
  // A parameter no map has bound yet, at either polarity: a map may bind it to
  // an input type as well as an output one, so it matches either.
  if (open.contains(targetA.baseName) || open.contains(targetB.baseName)) {
    return true;
  }

  // Case 1: Both output types → covariant recursion
  if (!targetA.isDual && !targetB.isDual) {
    return _isSubtype(targetA, targetB, dfa, visited, open, as);
  }

  // Case 2: Both dual types → contravariant recursion (reversed)
  if (targetA.isDual && targetB.isDual) {
    final innerA = dfa.getState(targetA.baseName); // output type A'
    final innerB = dfa.getState(targetB.baseName); // output type B'
    return _isSubtype(
        innerB, innerA, dfa, visited, open, as?.reversed); // REVERSED
  }

  // Case 3: Mixed → incompatible mode structure
  return false;
}

