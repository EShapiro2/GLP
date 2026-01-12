# Type Checker Module Architecture

**Version**: 0.2  
**Date**: 2025-01-08  
**Status**: DRAFT  

## 1. Task of the Type Checker

The type checker determines whether a typed GLP program P = (Cs, D) is **well-typed** per Definition 4.10:

1. **Covariance:** Every clause C ∈ Cs is well-typed by D
2. **Contravariance:** Every input path in every procedure type in D has a clause that accepts it

**Input:** A parsed GLP program consisting of:
- Clauses (Cs) — the logic program
- Type declarations (D) — type definitions and procedure declarations

**Output:** 
- Success: program is well-typed
- Failure: list of type errors with locations and explanations

## 2. How It Is Realized

The type checking algorithm proceeds in phases:

### Phase 1: Build Type Environment
Parse type definitions and procedure declarations into a queryable structure.

### Phase 2: Compile Types to DFAs  
Each type definition compiles to a DFA. For procedure arguments declared as `T?` (input), the DFA is mode-complemented.

### Phase 3: Check Covariance (Per-Clause)
For each clause C with head H and body B:
1. Construct moded head H' using `modedHead(H, procDecl)` — this creates an I/O moded term with variables flipped
2. For each moded path in H', traverse the type DFA checking consistency and collecting variable types
3. For each body atom A, construct produced moded term A' and check similarly
4. Check all variable pairs have complementary types

### Phase 4: Check Contravariance (Per-Procedure)
For each procedure with input arguments, use **structural coverage checking**:
1. Traverse the input type DFA
2. At each state, for each outgoing transition (alternative), verify some clause head matches
3. Recurse into sub-states (with visited set for recursive types)
4. Report uncovered alternatives

## 3. Module List and Responsibilities

| Module | Responsibility |
|--------|----------------|
| `mode` | Define `Mode` enum (consume ↓, produce ↑) with flip operation |
| `type-environment` | Store and lookup type definitions and procedure declarations |
| `type-dfa` | Compile types to DFAs; DFA traversal; mode complementation |
| `moded-term` | Moded term representation; path extraction; term complement |
| `moded-head` | Construct moded head H' from clause head H (Definition 4.6) |
| `well-typed-term` | Check moded term against type DFA; assign variable types |
| `well-typed-clause` | Check clause well-typing (Definition 4.8) |
| `well-typed-program` | Check program well-typing including coverage (Definition 4.10) |

## 4. Module Dependencies

```
                    ┌─────────────────┐
                    │ well-typed-     │
                    │ program         │
                    └────────┬────────┘
                             │
              ┌──────────────┴──────────────┐
              ▼                             ▼
     ┌─────────────────┐          ┌─────────────────┐
     │ well-typed-     │          │    type-dfa     │
     │ clause          │          │                 │
     └────────┬────────┘          └────────┬────────┘
              │                            │
    ┌─────────┴─────────┐                  │
    ▼                   ▼                  │
┌────────────┐  ┌───────────────┐          │
│ moded-head │  │ well-typed-   │          │
│            │  │ term          │◄─────────┘
└─────┬──────┘  └───────┬───────┘
      │                 │
      └────────┬────────┘
               ▼
        ┌─────────────┐
        │ moded-term  │
        └──────┬──────┘
               │
               ▼
        ┌─────────────┐     ┌─────────────────┐
        │    mode     │◄────│ type-environment│
        └─────────────┘     └─────────────────┘
               ▲                    │
               │                    ▼
               │            ┌─────────────────┐
               └────────────│  compiler/ast   │
                            └─────────────────┘
```

### Dependency Table

| Module | Depends On |
|--------|------------|
| `mode` | (none — leaf module) |
| `type-environment` | `mode` |
| `moded-term` | `mode`, `compiler/ast` |
| `type-dfa` | `mode`, `type-environment` |
| `moded-head` | `moded-term`, `type-environment` |
| `well-typed-term` | `moded-term`, `type-dfa` |
| `well-typed-clause` | `moded-head`, `well-typed-term`, `type-environment` |
| `well-typed-program` | `well-typed-clause`, `type-dfa`, `type-environment` |

## 5. Operations by Data Type

### 5.1 Mode

| Operation | Description | Module |
|-----------|-------------|--------|
| `Mode.consume` | The consume mode (↓) | `mode` |
| `Mode.produce` | The produce mode (↑) | `mode` |
| `mode.flip` | Flip mode: consume ↔ produce | `mode` |

### 5.2 GLP Terms (compiler/ast)

| Operation | Description | Module |
|-----------|-------------|--------|
| `term.isVariable` | Check if term is a variable | `compiler/ast` |
| `term.isReader` | Check if variable is reader (X?) | `compiler/ast` |
| `term.isWriter` | Check if variable is writer (X) | `compiler/ast` |
| `term.functor` | Get compound term's functor | `compiler/ast` |
| `term.arity` | Get compound term's arity | `compiler/ast` |
| `term.args` | Get compound term's arguments | `compiler/ast` |

### 5.3 Moded Terms

| Operation | Description | Module |
|-----------|-------------|--------|
| `ModedTerm` | Class representing term with mode annotations | `moded-term` |
| `ModedPath` | Class representing path through moded term | `moded-term` |
| `paths(modedTerm)` | Extract all moded paths from term | `moded-term` |
| `complement(modedTerm)` | Flip all modes and all variables | `moded-term` |

### 5.4 Moded Head

| Operation | Description | Module |
|-----------|-------------|--------|
| `modedHead(head, procDecl)` | Construct moded head H' per Definition 4.6: (1) build I/O moded term from head guided by procedure type, (2) flip all variables | `moded-head` |

### 5.5 Type DFAs

| Operation | Description | Module |
|-----------|-------------|--------|
| `compileType(typeName, env)` | Compile type definition to DFA | `type-dfa` |
| `complementDFA(dfa)` | Flip all modes in DFA (for T? arguments) | `type-dfa` |
| `stateAfterLabel(state, label)` | Get target state for transition | `type-dfa` |
| `getTransitions(state)` | Get all outgoing transitions from state | `type-dfa` |
| `isLeafState(state)` | Check if state is a leaf (primitive or constant) | `type-dfa` |
| `getLeafType(state)` | Get type at leaf state (`_`, `_?`, `Integer`, etc.) | `type-dfa` |

### 5.6 Well-Typed Term

| Operation | Description | Module |
|-----------|-------------|--------|
| `checkTermAgainstDFA(modedPath, dfa)` | Traverse DFA along path, check consistency, return variable type if path ends in variable | `well-typed-term` |
| `checkModedTerm(modedTerm, dfa)` | Check all paths in moded term against DFA, collect variable types | `well-typed-term` |

### 5.7 Well-Typed Clause

| Operation | Description | Module |
|-----------|-------------|--------|
| `checkClause(clause, env)` | Check clause well-typing per Definition 4.8 | `well-typed-clause` |
| `getAcceptedAlternatives(clause, argIndex)` | Get which type alternatives the clause head accepts at given argument position | `well-typed-clause` |

### 5.8 Well-Typed Program

| Operation | Description | Module |
|-----------|-------------|--------|
| `checkProgram(clauses, env)` | Check program well-typing per Definition 4.10 | `well-typed-program` |
| `checkCoverage(clauses, procDecl, argIndex)` | Structural coverage check for input argument | `well-typed-program` |

### 5.9 Type Environment

| Operation | Description | Module |
|-----------|-------------|--------|
| `getType(name)` | Lookup type definition | `type-environment` |
| `getProcedure(name, arity)` | Lookup procedure declaration | `type-environment` |
| `isLeafType(name)` | Check if type is a leaf type | `type-environment` |

## 6. Leaf Types

A **leaf type** is a type that terminates a type path. Leaf types are:

| Leaf Type | Description | Mode |
|-----------|-------------|------|
| `_` | Any produced term | intrinsic produce (↑) |
| `_?` | Any consumed term | intrinsic consume (↓) |
| `Integer` | Any integer constant | from context |
| `String` | Any string constant | from context |
| constant (e.g., `[]`, `1`) | Exact value | from context |

A type path ends when it reaches a leaf type. The DFA has these as accepting states.

## 7. Key Algorithms

### 7.1 Moded Head Construction (Definition 4.6)

```
modedHead(head, procDecl):
  // Step 1: Build I/O moded term
  // - Root mode is consume (↓)
  // - Arguments get mode from procDecl: Type → produce (↑), Type? → consume (↓)
  // - Nested types propagate/flip mode per complementation
  ioTerm = buildIOModedTerm(head, procDecl)
  
  // Step 2: Flip all variables
  // - Writer X becomes reader X?
  // - Reader X? becomes writer X
  return flipAllVariables(ioTerm)
```

### 7.2 Path Consistency Check (via DFA traversal)

```
checkPathAgainstDFA(modedPath, dfa):
  state = dfa.startState
  
  for each step in modedPath (except leaf):
    transition = findTransition(state, step.label, step.mode)
    if transition is null:
      return Inconsistent
    state = transition.target
  
  leafStep = modedPath.leaf
  
  if leafStep is variable:
    // Case 2: term path ends in variable
    if leafStep.isReader and state.mode == consume:
      return Consistent(variableType: getLeafType(state))
    if leafStep.isWriter and state.mode == produce:
      return Consistent(variableType: getLeafType(state))
    return Inconsistent
  
  if leafStep is constant:
    // Case 1: equal length paths
    if isLeafState(state) and constantMatches(leafStep.value, state):
      return Consistent
    return Inconsistent
```

### 7.3 Structural Coverage Check

```
checkCoverage(clauses, procDecl, argIndex, env):
  inputDFA = compileType(procDecl.argTypes[argIndex], env)
  inputDFA = complementDFA(inputDFA)  // T? needs complemented DFA
  
  return checkStateCoverage(inputDFA.startState, clauses, argIndex, visited={})

checkStateCoverage(state, clauses, argIndex, visited):
  if state in visited:
    return Covered  // Recursive type, already checking
  visited.add(state)
  
  if isLeafState(state):
    return Covered  // Leaf states are covered by variables
  
  for transition in getTransitions(state):
    // Check if some clause accepts this transition
    if not someClauseAccepts(clauses, argIndex, transition):
      return Uncovered(transition)
    
    // Recurse into target state
    result = checkStateCoverage(transition.target, clauses, argIndex, visited)
    if result is Uncovered:
      return result
  
  return Covered
```

## 8. Module Interface Summary

```
┌─────────────────────────────────────────────────────────────────┐
│                     well-typed-program                          │
│  checkProgram(clauses, env) → ProgramCheckResult                │
│  checkCoverage(clauses, procDecl, argIndex) → CoverageResult    │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┴───────────────┐
              ▼                               ▼
┌───────────────────────┐          ┌─────────────────────┐
│   well-typed-clause   │          │      type-dfa       │
│ checkClause()         │          │ compileType()       │
│ getAcceptedAlts()     │          │ complementDFA()     │
└───────────────────────┘          │ getTransitions()    │
              │                    │ isLeafState()       │
    ┌─────────┴─────────┐          └─────────────────────┘
    ▼                   ▼                    │
┌────────────┐  ┌───────────────────┐        │
│ moded-head │  │  well-typed-term  │◄───────┘
│ modedHead()│  │ checkModedTerm()  │
└────────────┘  │ checkPathAgainst  │
      │         │ DFA()             │
      │         └───────────────────┘
      │                   │
      └─────────┬─────────┘
                ▼
         ┌─────────────┐
         │ moded-term  │
         │ ModedTerm   │
         │ ModedPath   │
         │ paths()     │
         │ complement()│
         └─────────────┘
                │
                ▼
         ┌─────────────┐
         │    mode     │
         │ Mode enum   │
         │ flip        │
         └─────────────┘
```

## 9. Removed Modules

The following modules from the previous spec are **removed**:

| Removed Module | Reason |
|----------------|--------|
| `path-consistency` | Merged into `well-typed-term`. Path consistency is just DFA traversal. |
| `clause-contribution` | Merged into `well-typed-program`. Coverage uses structural checking, not DFA set operations. |

## 10. Removed DFA Operations

The following DFA operations are **not needed**:

| Removed Operation | Reason |
|-------------------|--------|
| `union(a, b)` | Structural coverage check doesn't need DFA union |
| `intersect(a, b)` | Not needed |
| `complement(dfa)` (set complement) | Not needed. `complementDFA` (mode flip) is different. |
| `isEmpty(dfa)` | Not needed |
| `isSubsetOf(a, b)` | Structural coverage replaces subset check |
| `isEquivalent(a, b)` | Not needed |
| `findAcceptingPath(dfa)` | Error messages use structural information instead |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-08 | Initial draft |
| 0.2 | 2025-01-08 | Resolved all open questions; simplified DFA operations; removed path-consistency and clause-contribution modules; added leaf types section |
