# GLP Specification Guide

This document guides implementation of GLP according to the formal specification in `docs/glp_spec.tex`.

## Essential Reading

To understand the GLP implementation requirements, focus on these sections in order:

1. **Section 3** (line 235): GLP syntax, operational semantics, and properties
2. **Definition 10** (line 317): Formal GLP Transition System
3. **Section 4** (line 382): Programming examples demonstrating GLP behavior
4. **Appendix: Implementation-Ready Transition System** (line 1493): Workstation implementation spec

**Skip**: Mathematical proofs, multiagent details (unless implementing distributed version), security mechanisms.

## Core Concepts

### Reader/Writer Variables

- **Writer** `X`: Single-assignment variable (promise) - can be written to exactly once
- **Reader** `X?`: Paired read-only access to writer's future value
- **SRSW Requirement**: In any clause, each variable (reader or writer) occurs **at most once**
- This eliminates need for distributed unification - just point-to-point communication

### Writer Unification (Definition in Section 3)

Writer unification of two terms has three possible outcomes:

1. **Succeeds with σ**: They have a writer mgu σ (most general unifier that only binds writers)
2. **Suspends on W**: They have a regular mgu σ, but it would require binding readers (suspension set W = readers that would need to be instantiated)
3. **Fails**: No unification possible

**Key constraints on writer substitution σ**:
- Only binds writers: Vσ ⊂ V
- Does not bind writers to writers (would abandon paired readers)
- No cycles through readers: X? does not occur in Xσ (prevents circular terms)

### GLP Transition System (Definition 10, line 317)

An **asynchronous resolvent** is a pair `(G, σ)` where:
- G ∈ 𝒢?(M): a goal (multiset of atoms) that may contain readers and writers
- σ: a reader substitution (binds only readers)

**Transitions**: `(G, σ) → (G', σ')`

1. **Reduce**: Pick atom A ∈ G, find first applicable clause C ∈ M
   - GLP reduction of A with C succeeds with result (B, σ̂)
   - G' = (G \ {A} ∪ B)σ̂  (remove A, add body B, apply writer substitution)
   - σ' = σ ∘ σ̂?  (compose with reader counterpart of writer substitution)

2. **Communicate**: Apply pending reader assignment
   - σ̂ = {X? := T} ∈ σ  (pick a reader binding from σ)
   - G' = Gσ̂  (apply to goal)
   - σ' = σ \ σ̂  (remove from pending substitutions)

**GLP Fairness**: A goal that can be reduced is eventually reduced.

## Implementation-Ready GLP (Appendix, line 1493)

The workstation implementation uses deterministic scheduling and explicit suspension/failure tracking.

### Configuration Structure

**irGLP Configuration** `(Q, S, F)`:
- **Q ∈ 𝒜\***: Sequence (FIFO queue) of **active goals**
- **S ⊆ 𝒜 × 2^(V?)**: Set of **suspended goals** with suspension sets: `(Goal, {readers blocking it})`
- **F ⊆ 𝒜**: Set of **failed goals**

### Transition Rules

Given configuration `(Q, S, F)` with `Q = A·Q'` (A is head of queue):

#### 1. REDUCE (success case)

If GLP reduction of A with first applicable clause C succeeds with (B, σ̂):

- **Compute reactivation set R**: `R = {G : (G, W) ∈ S ∧ X? ∈ W ∧ X?σ̂? ≠ X?}`
  - Find all suspended goals whose blocking readers are instantiated by σ̂?
- **Activate**: `S' = S \ {(G, W) : G ∈ R}` (remove from suspended)
- **Schedule**: `Q' = (Q' · B · R)σ̂σ̂?`
  - Append body B and reactivated goals R to queue
  - Apply both writer substitution σ̂ and its reader counterpart σ̂?
- **F' = F** (unchanged)

**Key difference from Definition 10**: Reader substitutions applied **immediately** rather than stored in σ. This is appropriate for workstation where all variables are local.

#### 2. SUSPEND

If GLP reduction of A with all clauses suspends, and `W = ⋃(C∈M) W_C ≠ ∅`:

- **W_C**: suspension set from attempting clause C
- **W**: union of all suspension sets across all clause attempts
- `Q' = Q'` (remove A from queue)
- `S' = S ∪ {(A, W)}` (add A to suspended with blocking readers W)
- `F' = F`

**Critical**: Goals suspend only after trying **all clauses**. Accumulate blockers:
- **Si**: clause-local blockers (reset on each ClauseTry)
- **U**: goal-accumulated blockers (union across all tried clauses)
- After scanning all clauses: suspend on U if non-empty

#### 3. FAIL

Otherwise (no clause succeeds or suspends):

- `Q' = Q'` (remove A from queue)
- `S' = S`
- `F' = F ∪ {A}` (mark as failed)

## Key Properties (Section 3)

### SRSW Invariant (Proposition at line 359)
If initial goal G₀ satisfies SRSW, then every goal in the run satisfies SRSW.

### Acyclicity (Proposition at line 366)
The occurs check in readers prevents formation of circular terms.

### Monotonicity (Proposition at line 378)
**Unlike LP**, in GLP: If atom A ∈ Gᵢ can reduce with clause C, then for any j > i:
- Either A has been reduced by step j, OR
- There exists A' ∈ Gⱼ where A' = Aτ (τ instantiates only readers) and A' can still reduce with C

**Implication**: Once a goal becomes reducible, it stays reducible (readers may get more instantiated, but this doesn't cause failure). This is the foundation for suspension/reactivation.

## Programming Model (Section 4 examples)

### Stream Merger (canonical example)
```
merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).  % output from first stream
merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).  % output from second stream
merge([],[],[]).                                  % terminate
```

- **Deterministic clause selection**: First applicable clause wins
- **Fairness**: Switching input order in recursive call ensures dovetailing
- **Writer/Reader pairs**: `X` writer in head becomes `X?` reader in body

### Concurrent Monitor (stateful service)
```
monitor([add(N)|Reqs],Sum) :-
    Sum1 := Sum? + N?, monitor(Reqs?,Sum1?).
```

- State carried in tail-recursive calls via Sum parameter
- Writer Sum1 created, reader Sum1? passed to recursive call
- Guards like `ground(Sum?)` enable conditional behavior

## Implementation Checklist

### Core Runtime Must:

1. **Maintain triple (Q, S, F)** for active/suspended/failed goals
2. **FIFO scheduling** from Q (deterministic, not nondeterministic)
3. **Clause scanning**: Try clauses in order, accumulate suspension sets
4. **Suspension**: Only after trying ALL clauses, suspend on union of blockers
5. **Reactivation**: When reader X? is bound, reactivate all goals in S that have X? in their suspension set
6. **Single-shot reactivation**: Each suspended goal reactivates at most once per suspension (use armed flag)
7. **Immediate substitution application**: Apply σ̂σ̂? immediately to queue (workstation model)

### Writer Unification Must:

1. Only bind writers (never readers)
2. Never bind writer to writer (would abandon paired reader)
3. Perform occurs check on readers (X? must not occur in Xσ)
4. Return suspension set W when unification requires reader instantiation
5. Fail when terms don't unify

### SRSW Enforcement:

1. Each variable occurs at most once in each clause (syntactic check)
2. Runtime must preserve SRSW invariant across reductions
3. `ground(X?)` guard relaxes single-reader for ground terms only

## Bytecode Instruction Model

The GLP bytecode is modeled after the Warren Abstract Machine (WAM) and Flat Concurrent Prolog (FCP) abstract machines, adapted for GLP's three-valued unification (success/suspend/fail) and SRSW semantics.

### Code Organization Hierarchy

**CRITICAL DISTINCTION** - There are three levels in the code organization:

1. **Module**: The complete bytecode program containing all procedures
   - A module is the compiled program as a whole (the bytecode array)
   - Each process/goal is associated with exactly one module

2. **Procedure**: A named predicate consisting of all clauses with the same head functor/arity
   - Example: `p/1` contains all clauses for predicate `p` with 1 argument
   - Each procedure has an entry point PC (κ) marking its first clause
   - Example: `p/1` might start at PC 42, `q/2` at PC 100

3. **Clause**: A single rule within a procedure (head :- body)
   - Each clause starts with `clause_try`
   - Multiple clauses for same procedure are tried sequentially

### Process Activation and Suspension

**KEY PRINCIPLE**: When a process suspends and later reactivates, it **restarts from the procedure entry point (κ)**, NOT from the module beginning.

- **κ (kappa)**: The PC of the first clause of the procedure being executed
- **Suspension**: Stores the goal with its κ value
- **Reactivation**: Goal resumes at PC = κ, trying the first clause of its procedure again

**Example**:
```
Module containing two procedures:
  PC 0:  p/1 clause 1 start (κ_p = 0)
  PC 10: p/1 clause 2 start
  PC 20: q/2 clause 1 start (κ_q = 20)
  PC 35: q/2 clause 2 start

If a goal executing q/2 suspends:
  - It stores κ = 20 (q/2's entry point)
  - On reactivation, it restarts at PC 20 (first clause of q/2)
  - NOT at PC 0 (beginning of module)
  - NOT at PC 35 (where it might have suspended)
```

**Rationale**: Reactivation means "the context has changed (some reader was bound), try the procedure again from the beginning." This allows clause selection to reconsider all clauses with the new bindings.

### Normative Specifications

- **GLP Bytecode v2.16 (NORMATIVE)**: See `docs/glp-bytecode-v216-complete.md` for complete instruction set specification
- **GLP Runtime System v2.1**: See `docs/glp-runtime-spec.txt` for Dart runtime architecture and implementation guidance

### References

- **WAM**: See `docs/wam.pdf` for Warren's tutorial on Prolog abstract machine
- **FCP**: See `docs/1-s2.0-0743106689900113-main.pdf` for Flat Concurrent Prolog abstract machine paper
- **FCP Implementation**: https://github.com/EShapiro2/FCP

### Key Concepts (from v2.16 Normative Spec)

**See `docs/glp-bytecode-v216-complete.md` for complete instruction set details.**

1. **Three execution phases per clause**:
   - **HEAD**: Tentative unification, builds σ̂w (tentative writer substitution)
   - **GUARDS**: Pure tests, may add to suspension set Si
   - **BODY**: Mutations allowed only after commit

2. **Tentative bindings σ̂w** (sigma-hat-w):
   - Map: `writerId → term` (implemented as `Map<int, Object?>` in Dart)
   - Accumulated during HEAD/GUARD without mutating heap
   - **Committed atomically** at `commit` OR **discarded** at `clause_next`

3. **Suspension sets**:
   - **Si**: clause-local blocked readers (cleared at each `clause_try`)
   - **U**: goal-accumulated blocked readers (union across all tried clauses)
   - Goal suspends if U non-empty after all clauses tried

4. **WAM-style structure traversal**:
   - **Mode register**: READ (matching existing structure) / WRITE (building new structure)
   - **S register**: Current position in structure traversal
   - HEAD instructions operate tentatively, BODY instructions mutate heap

5. **Control flow**:
   - `clause_try Ci`: Initialize Si, σ̂w for clause attempt
   - `clause_next Cj`: Discard σ̂w, Si → U, jump to next clause
   - `commit`: Apply σ̂w to heap, process ROQs, enter BODY
   - `suspend`: If U non-empty, suspend goal; else fail
   - `spawn P/n`: Create concurrent goal (fair scheduling)
   - `requeue P/n`: Tail-call optimization with budget (26 iterations, then yield to event queue)

### Instruction Categories

**HEAD instructions** (pure, tentative, build σ̂w):
- `head_structure f/n, Ai` - Match structure, set S register
- `head_writer Xi` - Process writer in head (READ/WRITE mode)
- `head_reader Xi` - Process reader in head (may suspend)
- `head_constant c, Ai` - Match constant (tentatively bind writers)
- `head_nil Ai`, `head_list Ai` - Special cases

**Structure traversal** (after head_structure, operate on S):
- `writer Xi` - Extract/create writer at S position
- `reader Xi` - Verify/create reader at S position
- `constant c` - Match/write constant at S position
- `void n` - Skip/allocate n anonymous variables

**GUARD instructions** (pure, may suspend or fail):
- `guard P, Args` - Call guard predicate (pure test)
- `ground X` - Test if X is ground
- `known X` - Test if X is bound
- `otherwise` - Default catch-all

**BODY instructions** (mutating, after commit):
- `put_structure f/n, Ai` - Allocate structure on heap
- `put_writer Xi, Ai` - Place writer in argument
- `put_reader Xi, Ai` - Place reader in argument
- `put_constant c, Ai`, `put_nil Ai`, `put_list Ai` - Place values

**Control**:
- `clause_try Ci`, `clause_next Cj`, `suspend`, `commit`
- `spawn P/n`, `requeue P/n`, `proceed`
- `allocate n`, `deallocate` - Environment frame management
- `reactivate X`, `abandon X` - Suspension management

### Execution Model Summary

```
Goal execution:
  κ = clause-selection entry PC
  U = ∅  (goal-level suspension set)

  FOR EACH CLAUSE Ci:
    clause_try Ci:
      Si = ∅
      σ̂w = ∅

    Execute HEAD instructions:
      Build σ̂w tentatively (no heap mutation)
      Accumulate Si (blocked readers)
      May FAIL → jump to next clause

    Execute GUARD instructions:
      Pure tests
      May add to Si or FAIL

    Decision:
      if FAILED:
        → jump to next clause (discard σ̂w, ignore Si)
      elif Si non-empty:
        → clause_next: discard σ̂w, Si → U, try next clause
      else:
        → commit: apply σ̂w to heap, wake goals, enter BODY

  no_more_clauses:
    if U ≠ ∅:
      SUSPEND goal on U (resume at κ when any reader in U bound)
    else:
      FAIL goal definitively
```

## Current Implementation Status

The v2.16 VM in `lib/bytecode/v216/` implements:
- ✓ Clause scanning with Si/U suspension accumulation
- ✓ σ̂w (sigma-hat-w): tentative writer substitution applied at COMMIT
- ✓ HEAD/GUARD pure execution, BODY mutations after COMMIT
- ✓ Unit goals and unit clauses (single predicate)

**Bytecode runner** in `lib/bytecode/` currently has:
- ✓ Basic opcode structure
- ✗ Incomplete HEAD instructions (missing value encoding)
- ✗ No structure traversal (S register)
- ✗ sigmaHat was Set<int>, needs Map<int, Object?> for tentative bindings

**To align with spec**:
- Need full (Q, S, F) configuration structure with reactivation
- Need proper suspension on reader variables with queue management
- Need multi-goal scheduling and fairness mechanisms
- Need to implement complete instruction set as documented above
- Current VM is single-goal; spec requires goal queue Q
