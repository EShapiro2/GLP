# Module: well-typed-program

**Version**: 0.7  
**Date**: 2025-01-13  
**Status**: DRAFT  
**Paper References**: Definition [Well-typed GLP program], Definition [Type Automaton], Definition [Type Complementarity]

## Purpose

Determines when a typed GLP program P = (Cs, D) is well-typed. This is the top-level type checking entry point.

## Dependencies

- `well-typed-clause` — checkClause(), ClauseCheckResult
- `type-dfa` — ProgramDFA, Automaton, DFAState, getTransitions()
- `type-environment` — TypeEnvironment, ProcDecl

## Definitions

### Definition: Well-typed GLP Program

> A typed GLP program P = (Cs, D) is **well-typed** if:
>
> 1. **Covariance:** Every clause C ∈ Cs is well-typed by D.
> 2. **Contravariance:** Every input path in every procedure type in D has a clause C ∈ Cs that accepts it.

### Covariance (Condition 1)

Every clause must satisfy all three conditions of well-typed clause:
- Head is well-typed
- Body atoms are well-typed  
- Variable pairs have complementary types

This ensures that clauses **produce** terms within the declared types.

### Contravariance (Condition 2)

For every procedure with input arguments, every possible input must be accepted by at least one clause.

This ensures that the program can **consume** all values allowed by the input types.

**Structural coverage checking:** We traverse the input type automaton and verify that each transition (alternative) is covered by some clause head.

### Definition: Wildcard Types and Final States (Paper Section 4.2)

From the paper's Definition [Type Automaton]:

> **Wildcard states:** The states `_` and `_?` accept any term:
> - `_` accepts any produced term (a writer or ground term with mode ↑)
> - `_?` accepts any consumed term (a reader or ground term with mode ↓)
> - **These are final states; no outgoing transitions are required.**

**CRITICAL:** Wildcard types `_` and `_?` are **final states** in the type automaton. This has two important consequences:

1. **For covariance (clause well-typing):** A term at a position typed `_` or `_?` is automatically well-typed—the wildcard accepts any term structure.

2. **For contravariance (coverage checking):** A procedure argument typed `_?` requires **NO coverage checking**. Since `_?` is a final state with no outgoing transitions, there are no alternatives to cover. The type simply declares "any consumed term is acceptable here"—it does NOT require clauses to handle all possible terms.

### Distinction: Type Acceptance vs. Clause Coverage

The type `_?` means:
- **Type automaton perspective:** "This position accepts any consumed term" — the automaton immediately accepts (final state)
- **NOT:** "Clauses must cover all possible input terms"

A procedure declared as `procedure reduce(_?, _)` is well-typed as long as:
1. Each clause's head is well-typed (covariance) — any structure at argument 1 is fine since `_?` accepts anything
2. No coverage check is needed for argument 1 (contravariance) — `_?` is a final state

The clauses can match specific structures like `reduce(foo(X), ...)` without needing a catch-all clause. The type declaration describes what the **type system** accepts, not what the **clauses** must cover.

### Definition: Type Complementarity (Paper Section 4.3)

> Let S and T be GLP types. S is **complementary to** T, written S ⊴ T, if for every moded path in paths(S), one of the following holds at each position along the path:
>
> 1. At positions with mode produce (↑): the alternative at that position in S is among the alternatives at the corresponding position in T.
> 2. At positions with mode consume (↓): the alternative at that position in T is among the alternatives at the corresponding position in S.

At each mode complement encountered along a path, the inclusion obligation reverses: producers must stay within their declared alternatives (covariance), while consumers must accept all alternatives the other party might produce (contravariance).

### Definition: Complementarily-Typed Variables

> Let X and X? be a variable pair in a GLP clause, with X assigned type S and X? assigned type T. The pair is **complementarily typed** if S ⊴ T.

Type complementarity generalizes exact complementation: if S = T?, then trivially S ⊴ T.

## Public Interface

### Types

#### `class ProgramCheckResult`

```dart
class ProgramCheckResult {
  final bool isWellTyped;
  final List<ClauseCheckResult> clauseResults;
  final List<CoverageError> coverageErrors;
  
  /// Summary statistics
  int get totalClauses => clauseResults.length;
  int get wellTypedClauses => clauseResults.where((r) => r.isWellTyped).length;
}

class CoverageError {
  final String procedure;      // e.g., "merge/3"
  final int argIndex;          // 1-based argument index
  final String uncoveredLabel; // e.g., "[]" or "[|]/2"
  final String path;           // Human-readable path to uncovered position
}
```

### Functions

#### `ProgramCheckResult checkProgram(List<Clause> clauses, ProgramDFA dfa, TypeEnvironment env)`

Checks if program (Cs, D) is well-typed per the paper definition.

**Preconditions:**
- `clauses` is a list of valid GLP clauses
- `dfa` is the compiled ProgramDFA for the type environment
- `env` contains type definitions and procedure declarations

**Postconditions:** Returns ProgramCheckResult where:
- `isWellTyped` is true iff both covariance and contravariance hold
- `clauseResults` contains individual check results for each clause
- `coverageErrors` lists uncovered input paths (empty if all covered)

**Errors:**
- Throws `UndeclaredProcedureError` if any clause uses an undeclared procedure
- Throws `UndefinedTypeError` if any procedure references an undefined type

## Algorithms

### Algorithm: Program Well-Typing Check

```
checkProgram(clauses, dfa, env):
  clauseResults = []
  coverageErrors = []
  
  // Condition 1: Covariance — check each clause
  for clause in clauses:
    result = checkClause(clause, dfa, env)
    clauseResults.add(result)
  
  // Condition 2: Contravariance — check coverage for each input argument
  for proc in env.procedures:
    procClauses = clauses.filter(c => 
      c.head.functor == proc.name && c.head.arity == proc.arity)
    
    for argIndex in 1..proc.arity:
      argType = proc.argTypes[argIndex - 1]
      if argType.isInput:
        // CRITICAL: Skip coverage check for wildcard input type _?
        // Wildcard types are final states - no transitions to cover
        if isWildcardType(argType):
          continue  // No coverage check needed
        
        errors = checkCoverage(procClauses, proc, argIndex, dfa, env)
        coverageErrors.addAll(errors)
  
  isWellTyped = clauseResults.all(r => r.isWellTyped) && coverageErrors.isEmpty
  
  return ProgramCheckResult(isWellTyped, clauseResults, coverageErrors)

isWildcardType(argType):
  // Returns true if the argument type is the universal wildcard _?
  // The base type name (without ?) would be "_"
  return argType.baseName == "_"
```

### Algorithm: Structural Coverage Check

```
checkCoverage(clauses, proc, argIndex, dfa, env):
  // Get the input type automaton directly (already the T? automaton)
  inputTypeName = getFullTypeName(proc.argTypes[argIndex - 1])
  inputAutomaton = dfa.getAutomaton(inputTypeName)
  
  // CRITICAL: If start state is final (wildcard), no coverage needed
  if inputAutomaton.startState.isFinal:
    return []  // No errors - wildcard accepts everything
  
  errors = []
  visited = {}
  
  checkStateCoverage(
    inputAutomaton.startState, 
    clauses, 
    argIndex, 
    "",           // pathPrefix
    visited, 
    errors, 
    inputAutomaton,
    dfa
  )
  
  return errors

checkStateCoverage(state, clauses, argIndex, pathPrefix, visited, errors, automaton, dfa):
  // Prevent infinite recursion on recursive types
  if state.name in visited:
    return  // Already checked this state
  visited.add(state.name)
  
  // CRITICAL: Final states (including wildcards _/_?) need no coverage
  // They have no outgoing transitions - nothing to cover
  if state.isFinal:
    return
  
  // Primitive type states (Integer?, String?, Number?) are also effectively final
  // Variables of appropriate type will match them
  if state.isPrimitiveType:
    return
  
  // Get all transitions (alternatives) from this state
  transitions = automaton.getTransitions(state)
  
  for (label, targetState) in transitions:
    // Check if some clause accepts this transition at this argument position
    if clauseAcceptsLabel(clauses, argIndex, label, pathPrefix):
      // If target is a different user-defined type, switch automata
      nextAutomaton = automaton
      if targetState.isUserDefinedType && targetState.baseName != state.baseName:
        nextAutomaton = dfa.getAutomaton(targetState.name)
      
      // Recursively check the target state
      newPath = pathPrefix.isEmpty ? label.toString() : "$pathPrefix → $label"
      checkStateCoverage(targetState, clauses, argIndex, newPath, 
                         visited, errors, nextAutomaton, dfa)
    else:
      // No clause covers this alternative
      errors.add(CoverageError(
        procedure: proc.key,
        argIndex: argIndex,
        uncoveredLabel: label.toString(),
        path: pathPrefix.isEmpty ? label.toString() : "$pathPrefix → $label"
      ))

clauseAcceptsLabel(clauses, argIndex, label, pathPrefix):
  for clause in clauses:
    acceptedLabels = getAcceptedLabels(clause, argIndex)
    
    if acceptedLabels == ALL_LABELS:
      // Variable at this position - accepts anything
      return true
    
    if label in acceptedLabels:
      // Clause explicitly matches this label
      return true
  
  return false
```

### Algorithm: Get Accepted Labels (from clause head)

```
getAcceptedLabels(clause, argIndex):
  arg = clause.head.args[argIndex - 1]
  
  match arg:
    Variable(_):
      return ALL_LABELS  // Variable accepts anything
    
    Compound(functor, subArgs):
      return {TransitionLabel.functor(functor, subArgs.length)}
    
    Constant(value):
      return {TransitionLabel.constant(value)}
    
    ListNil:
      return {TransitionLabel.constant('[]')}
    
    ListCons(_, _):
      return {TransitionLabel.functor('[|]', 2)}
```

## Examples

### Example 1: Well-Typed merge Program

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).
```

**Covariance:** All 4 clauses are well-typed ✓

**Contravariance for argument 1 (Stream?):**

Input type automaton transitions from `Stream?`:
- `([], 0, ↓)` → ✓
- `([|], 2, 1, ↓)` → `_?`
- `([|], 2, 2, ↓)` → `Stream?`

Coverage:
- `[]`: Clauses 3 and 4 match (clause 3 has `[]`, clause 4 has variable `Xs`) ✓
- `[|]`: Clauses 1 and 2 match (clause 1 has `[X|Xs]`, clause 2 has variable `Xs`) ✓
  - Head position: Variables accept any `_?` ✓
  - Tail position: Variables accept any `Stream?` ✓

**Contravariance for argument 2 (Stream?):** Similar analysis ✓

**Result: Well-typed program**

### Example 2: INVALID — Covariance Failure

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([], Ys, 42).  // Integer at Stream position!
```

**Problem:** Clause produces integer `42` at output position expecting `Stream`.

**Error:** `clauseResults[0].errors = [HeadNotWellTypedError(...)]`

### Example 3: INVALID — Contravariance Failure  

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
// Missing clauses for [] at argument 1!
```

**Problem:** Input type `Stream?` includes `[]` alternative, but no clause accepts nil at argument 1.

**Error:** 
```dart
CoverageError(
  procedure: "merge/3",
  argIndex: 1,
  uncoveredLabel: "[]",
  path: "[]"
)
```

### Example 4: Deep Coverage with Nested Types

```
Tree ::= leaf(Integer) ; node(Tree, Tree).
procedure sum(Tree?, Integer).

sum(leaf(N), N?).
sum(node(L, R), S) :- sum(L?, X), sum(R?, Y), add(X?, Y?, S).
```

**Contravariance for argument 1 (Tree?):**

- `leaf`: Clause 1 matches with `leaf(N)` 
  - Inside leaf: Variable N accepts any `Integer?` ✓
- `node`: Clause 2 matches with `node(L, R)`
  - Left subtree: Variable L accepts any `Tree?` ✓
  - Right subtree: Variable R accepts any `Tree?` ✓

**Result: Well-typed**

### Example 5: Interactive Type Coverage

```
CounterCall ::= add ; clear ; read(Integer?).
procedure monitor(Integer?, Stream(CounterCall)?).

monitor(N, [add|In]) :- monitor(N+1?, In?).
monitor(N, [clear|In]) :- monitor(0?, In?).
monitor(N, [read(N?)|In]) :- monitor(N?, In?).
monitor(N, []).
```

**Contravariance for argument 2 (Stream(CounterCall)?):**

At `CounterCall?` state, transitions:
- `(add, 0, ↓)` → ✓ : Clause 1 matches ✓
- `(clear, 0, ↓)` → ✓ : Clause 2 matches ✓
- `(read, 1, 1, ↑)` → `Integer` : Clause 3 matches ✓

At `Stream(CounterCall)?` state:
- `([], 0, ↓)` → ✓ : Clause 4 matches ✓
- `([|], 2, 1, ↓)` → `CounterCall?` : All clauses 1-3 match ✓
- `([|], 2, 2, ↓)` → `Stream(CounterCall)?` : All clauses 1-3 have variable tail ✓

**Result: Well-typed**

### Example 6: Wildcard Input Type — No Coverage Required

```
Nat ::= 0 ; s(Nat).

procedure natural_number(Nat?).
procedure reduce(_?, _).

natural_number(0).
natural_number(s(X)) :- natural_number(X?).

reduce(natural_number(0), true).
reduce(natural_number(s(X)), natural_number(X?)).
```

**Covariance:** 
- `natural_number` clauses: Well-typed ✓
- `reduce` clauses: Well-typed ✓ (any structure at `_?` position is accepted)

**Contravariance for `natural_number` argument 1 (Nat?):**
- `0`: Clause 1 matches ✓
- `s`: Clause 2 matches ✓

**Contravariance for `reduce` argument 1 (_?):**
- **NO COVERAGE CHECK NEEDED** — `_?` is a final state
- The type `_?` means "accept any consumed term"
- It does NOT mean "clauses must handle all possible terms"

**Result: Well-typed program**

### Example 7: Metainterpreter with reduce(_?, _)

```
procedure reduce(_?, _).

reduce(append([], Ys, Ys?), true).
reduce(append([X|Xs], Ys, [X?|Zs?]), append(Xs?, Ys?, Zs)).
reduce(member(X, [X?|_]), true).
reduce(member(X, [_|Xs]), member(X?, Xs?)).
```

**Analysis:**
- Each `reduce` clause is well-typed: the head structures are valid consumed terms at `_?`
- NO coverage error for argument 1: `_?` is a final state, no transitions to cover
- The clauses intentionally only handle specific goal forms — this is correct behavior

**Result: Well-typed program**

## Error Conditions

| Condition | Error |
|-----------|-------|
| Clause not well-typed | In `clauseResults` with errors |
| Input alternative not covered | `CoverageError` |
| Procedure not declared | `UndeclaredProcedureError` (thrown) |
| Type not defined | `UndefinedTypeError` (thrown) |

## Notes

### Why Structural Coverage Instead of DFA Operations?

Previous approaches used DFA union/intersection/subset:
```
inputDFA ⊆ union(clausePatternDFAs)
```

Structural coverage is simpler:
1. No need to build clause pattern DFAs
2. No need for DFA set operations  
3. Directly answers "which alternative is missing?"
4. Naturally handles recursive types via visited set

### Variable Coverage

A variable at an argument position is a **wildcard** — it accepts all values of the declared type. This is why `getAcceptedLabels` returns `ALL_LABELS` for variables.

### Wildcard Types Are Final States

The types `_` and `_?` are **final states** in the type automaton:
- They have no outgoing transitions
- They immediately accept any term of the appropriate mode
- Coverage checking terminates immediately for these types

This is different from a user-defined type with alternatives—those have transitions that must be covered.

### Type Complementarity vs Exact Complement

The paper defines type complementarity (S ⊴ T) as a generalization of exact complementation. For most programs, paired variables have exactly complementary types (S = T?). Type complementarity allows asymmetric client/server interactions where a client uses a subset of operations.

The current implementation checks exact complementation. Full type complementarity checking would require comparing automata paths with the inclusion/reversal rules from Definition [Type Complementarity].

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Replace DFA set operations with structural coverage |
| 0.6 | 2025-01-12 | Update for ProgramDFA; add paper references; add type complementarity definitions; add interactive type example |
| 0.7 | 2025-01-13 | **CRITICAL FIX:** Clarify that wildcard types `_` and `_?` are final states requiring NO coverage checking. Add Examples 6 and 7 demonstrating `reduce(_?, _)`. Update algorithms to skip coverage check for wildcard input types. |
