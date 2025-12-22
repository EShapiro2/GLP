# YS Type Checking: Full Implementation Plan

**Status:** TENTATIVE DRAFT
**Date:** 2025-12-22
**Purpose:** Plan for implementing complete Yardeni-Shapiro type checking with fixpoint verification

## What YS Type Checking Actually Requires

From YS paper Figure 3 (page 141):

```
For each clause C in P do:
  For every ground path x in the body of C do:
    If x ∉ paths(S) then
      T_{C}^α(S) := φ
      goto 1.
    endif
  od
  For each variable Y that appears in C do:
    For each occurrence of Y in the body of C do:
      Infer the maximal set of values that this occurrence can obtain.
    od
    Intersect all the above sets getting the variable's type.
  od.
  Construct T_{C}^α(S) using the type of the variables.
  1: If T_{C}^α(S) = φ then print a warning that clause C is useless.
od
Find the tuple-distributive closure of the union of the T_{C}^α(S)'s computed.
If the result is equal to S and no clause is useless then succeed else fail.
```

**Well-typing definition (page 136):** P is well typed by S iff:
1. P does not contain useless clauses relative to S (weakly well-typed)
2. T_P^α(S) = S (S is a fixpoint)

## Current Implementation Status

| Step | YS Requirement | Current Status |
|------|----------------|----------------|
| 1 | Ground path checking | ✓ Implemented |
| 2 | Variable type inference | ✓ Implemented |
| 3 | Clause contribution T_{C}^α(S) | ✗ NOT implemented |
| 4 | Union of contributions | ✗ NOT implemented |
| 5 | Tuple-distributive closure | ✗ NOT implemented |
| 6 | Fixpoint equality check | ✗ NOT implemented |

**The current code computes variable types but never uses them to compute clause contributions or check fixpoint.**

## Implementation Plan

### Phase 1: Clause Contribution Computation

**Goal:** Given a clause head with inferred variable types, compute the DFA representing T_{C}^α(S) - all ground instances the clause can produce.

**Key insight from YS:** The clause contribution is built from the head structure with variables replaced by their inferred types.

```dart
/// Compute T_{C}^α(S) for a single clause
/// Returns a DFA representing all ground head instances this clause can produce
class ClauseContributionComputer {
  final TypeCompiler compiler;

  /// Given clause head pattern and inferred variable types,
  /// compute the DFA of all ground terms matching this pattern
  TypeDFA computeContribution(
    ast.Term headArg,           // The head argument pattern
    Map<String, TypeDFA> varTypes,  // Inferred types for each variable
    TypeDFA declaredType,       // The declared type for this position
  ) {
    // Base cases:
    // - Constant c: DFA accepting only {c}
    // - Variable X: varTypes[X] (the inferred type)
    //
    // Recursive cases:
    // - f(t1,...,tn): Build DFA that accepts f(v1,...,vn) where
    //   each vi is accepted by computeContribution(ti, varTypes, ...)
    // - [H|T]: Build DFA for list cons with head/tail contributions
  }
}
```

**For multi-argument procedures:** Each argument position gets its own contribution DFA. The full clause contribution is the product (for tuple-distributivity).

### Phase 2: DFA Union Operation

**Goal:** Combine contributions from multiple clauses.

```dart
extension TypeDFAOperations on TypeDFA {
  /// Union of two DFAs (accepts strings accepted by either)
  /// Result may be NFA, needs determinization
  TypeDFA union(TypeDFA other) {
    // Standard NFA union construction:
    // - New start state with ε-transitions to both start states
    // - Final states = union of final states
    // Then determinize via subset construction
  }
}
```

### Phase 3: Tuple-Distributive Closure

**Definition:** A set S is tuple-distributive if whenever p(a₁,...,aₙ) ∈ S and p(b₁,...,bₙ) ∈ S, then p(c₁,...,cₙ) ∈ S for all cᵢ ∈ {aᵢ, bᵢ}.

**For single-argument predicates (unary)**, tuple-distributive closure is just the set itself.

**For multi-argument predicates**, we need the Cartesian product of per-position types:
```
α(S) = { p(a₁,...,aₙ) | ∃ p(b₁,...,bₙ) ∈ S where aᵢ is in position i of some tuple }
```

```dart
/// For procedure p/n, given list of clause contributions per argument position,
/// compute tuple-distributive closure
List<TypeDFA> tupleDistributiveClosure(
  List<List<TypeDFA>> perClausePerArgContributions,
  int arity,
) {
  // For each argument position i:
  //   result[i] = union of all clause contributions at position i
  // This gives the tuple-distributive closure
  final result = <TypeDFA>[];
  for (int i = 0; i < arity; i++) {
    TypeDFA positionUnion = TypeDFA.empty();
    for (final clauseContrib in perClausePerArgContributions) {
      positionUnion = positionUnion.union(clauseContrib[i]);
    }
    result.add(positionUnion);
  }
  return result;
}
```

### Phase 4: Fixpoint Equality Check

**Goal:** Check if inferred type equals declared type.

```dart
extension TypeDFAEquality on TypeDFA {
  /// Check if two DFAs accept the same language
  /// Uses standard algorithm: L(A) = L(B) iff L(A) ⊆ L(B) and L(B) ⊆ L(A)
  /// Subset check: L(A) ⊆ L(B) iff L(A) ∩ L(B̄) = ∅
  bool isEquivalent(TypeDFA other) {
    // Minimize both DFAs, then check structural isomorphism
    // Or: check mutual subset inclusion
    final thisMinimal = this.minimize();
    final otherMinimal = other.minimize();
    return thisMinimal.isSubsetOf(otherMinimal) &&
           otherMinimal.isSubsetOf(thisMinimal);
  }

  bool isSubsetOf(TypeDFA other) {
    // L(this) ⊆ L(other) iff L(this) ∩ L(complement(other)) = ∅
    final complement = other.complement();
    final intersection = this.intersect(complement);
    return intersection.isEmpty;
  }

  TypeDFA complement() {
    // Swap final and non-final states (requires complete DFA)
  }
}
```

## Detailed Implementation Steps

### Step 1: Add DFA Operations to type_dfa.dart

```dart
// New methods needed:
class TypeDFA {
  // Existing...

  /// Union: accepts if either DFA accepts
  TypeDFA union(TypeDFA other);

  /// Complement: accepts if this DFA rejects (requires completion)
  TypeDFA complement();

  /// Check if this accepts subset of other's language
  bool isSubsetOf(TypeDFA other);

  /// Check language equality
  bool isEquivalent(TypeDFA other);

  /// Minimize DFA (for efficient comparison)
  TypeDFA minimize();

  /// Complete DFA (add sink state for missing transitions)
  TypeDFA complete();

  /// Check if language is empty
  bool get isEmpty;

  /// Create DFA accepting single constant
  static TypeDFA singleton(String constant);
}
```

### Step 2: Create clause_contribution.dart

```dart
/// Computes T_{C}^α(S) for a clause
class ClauseContributionComputer {
  final TypeEnvironment typeEnv;
  final TypeCompiler compiler;

  ClauseContributionComputer(this.typeEnv, this.compiler);

  /// Compute contribution for one argument position
  /// Returns DFA of all ground terms this clause can produce at this position
  TypeDFA computeArgContribution(
    ast.Term pattern,
    Map<String, TypeDFA> varTypes,
  ) {
    if (pattern is ast.VarTerm) {
      // Variable: return its inferred type
      return varTypes[pattern.name] ?? TypeDFA.empty();
    }

    if (pattern is ast.ConstTerm) {
      // Constant: singleton DFA
      return TypeDFA.singleton(pattern.value.toString());
    }

    if (pattern is ast.StructTerm) {
      // f(t1,...,tn): build product DFA
      // Accepts f(v1,...,vn) where vi ∈ L(computeArgContribution(ti))
      final argDFAs = pattern.args
          .map((arg) => computeArgContribution(arg, varTypes))
          .toList();
      return _buildStructDFA(pattern.functor, argDFAs);
    }

    if (pattern is ast.ListTerm) {
      if (pattern.isNil) {
        return TypeDFA.singleton('[]');
      }
      final headDFA = computeArgContribution(pattern.head!, varTypes);
      final tailDFA = computeArgContribution(pattern.tail!, varTypes);
      return _buildListConsDFA(headDFA, tailDFA);
    }

    return TypeDFA.empty();
  }

  TypeDFA _buildStructDFA(String functor, List<TypeDFA> argDFAs);
  TypeDFA _buildListConsDFA(TypeDFA head, TypeDFA tail);
}
```

### Step 3: Update type_checker.dart with Fixpoint Check

```dart
TypeCheckResult _checkProcedure(ProcDecl decl, List<ast.Clause> clauses) {
  // ... existing ground path and variable inference code ...

  // NEW: Compute clause contributions
  final contributionComputer = ClauseContributionComputer(typeEnv, compiler);
  final perClauseContributions = <List<TypeDFA>>[];

  for (final clause in clauses) {
    if (clauseIsUseless[clause]) continue;

    final clauseContrib = <TypeDFA>[];
    for (int i = 0; i < decl.arity; i++) {
      final argPattern = clause.head.args[i];
      final varTypes = inferredVarTypes[clause]!;
      clauseContrib.add(
        contributionComputer.computeArgContribution(argPattern, varTypes)
      );
    }
    perClauseContributions.add(clauseContrib);
  }

  // NEW: Compute tuple-distributive closure (union per position)
  final inferredType = <TypeDFA>[];
  for (int i = 0; i < decl.arity; i++) {
    var positionType = TypeDFA.empty();
    for (final contrib in perClauseContributions) {
      positionType = positionType.union(contrib[i]);
    }
    inferredType.add(positionType);
  }

  // NEW: Check fixpoint - inferred must equal declared
  for (int i = 0; i < decl.arity; i++) {
    final declared = argDFAs[i];
    final inferred = inferredType[i];

    if (!inferred.isEquivalent(declared)) {
      // Determine if inferred ⊂ declared (incomplete) or inferred ⊃ declared (too broad)
      if (inferred.isSubsetOf(declared)) {
        errors.add(TypeError(
          'Procedure ${decl.name}/${decl.arity} argument ${i+1}: '
          'clauses do not cover full declared type (incomplete definition)',
          decl.line, decl.column,
        ));
      } else {
        errors.add(TypeError(
          'Procedure ${decl.name}/${decl.arity} argument ${i+1}: '
          'clauses produce values outside declared type',
          decl.line, decl.column,
        ));
      }
    }
  }

  return TypeCheckResult(errors, warnings);
}
```

## Test Cases Needed

### Positive Controls (should pass)

```prolog
% Complete definition of append
List ::= [] ; [Any | List].
procedure append(List, List, List).
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```prolog
% Complete definition of nat
Nat ::= 0 ; s(Nat).
procedure isNat(Nat).
isNat(0).
isNat(s(N)) :- isNat(N).
```

### Negative Controls (should fail)

```prolog
% INCOMPLETE: missing [] case
List ::= [] ; [Any | List].
procedure length(List, Nat).
length([_|Xs], s(N)) :- length(Xs, N).
% ERROR: does not cover [] case
```

```prolog
% INCOMPLETE: missing s(N) case
Nat ::= 0 ; s(Nat).
procedure isZero(Nat).
isZero(0).
% ERROR: does not cover s(Nat) case
```

```prolog
% TOO BROAD: produces values outside type
Nat ::= 0 ; s(Nat).
procedure makeNat(Nat).
makeNat(0).
makeNat(s(N)) :- makeNat(N).
makeNat(foo).  % ERROR: foo not in Nat
```

## Complexity Considerations

**From the paper:** Type checking is EXPTIME-complete for regular types.

The expensive operations are:
- DFA complement (requires completion + state flip)
- DFA intersection (product construction)
- DFA minimization (for efficient equivalence)

For practical use, we may want:
- Lazy evaluation where possible
- Caching of compiled type DFAs
- Early termination when errors found

## Open Questions Before Proceeding

1. **Scope:** Should we implement this for the full moded type system, or start with unmoded structural types first?

2. **DFA operations:** The existing TypeDFA class has `intersect()` but not `union()`, `complement()`, `minimize()`, or `isEquivalent()`. Should we add all of these, or is there a simpler approach?

3. **Error reporting:** When fixpoint fails, how much detail do we want? (e.g., "missing case for []" vs "inferred ⊊ declared")

4. **Performance:** Should we add caching/memoization for DFA operations?

5. **Integration:** Should fixpoint checking be:
   - Always enabled (strict mode)?
   - Optional via flag?
   - Warning vs error?

## References

- Yardeni, E., & Shapiro, E. (1990). A type system for logic programs. *Journal of Logic Programming*, 10(2), 125-153.
- Figure 3, page 141: Type checking algorithm
- Definition 3.1, page 136: Well-typing
- Definition 2.3, page 130: Tuple-distributive closure
