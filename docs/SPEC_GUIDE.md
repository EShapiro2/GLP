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
- **SRSW Requirement**: Variables occur as reader/writer PAIRS in clauses, with exactly one writer AND one reader (exception: ground guard allows multiple readers)
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

### WxW (No Writer-to-Writer Binding) Restriction

GLP prohibits writer-to-writer binding to ensure no readers are abandoned:
- If writers X and Y unified, their readers X? and Y? would have no writer to provide values
- Runtime must FAIL immediately on writer-to-writer unification attempts
- This is NOT a suspension case - it's a definitive failure

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

## Guards vs System Predicates Called via Execute

**CRITICAL DISTINCTION**: GLP has two types of runtime operations with fundamentally different semantics:

### Guards (Three-Valued: Success/Suspend/Fail)

**Guards are pure tests** that check runtime conditions **without side effects**:
- Syntax: `Head :- Guard1, Guard2, ... | Body.`
- Appear after clause head, separated by `|` (guard separator)
- Enable conditional clause selection
- **Three-valued semantics**: success, suspend, or fail
- **Patient**: Suspend on unbound variables rather than fail
- Execute during HEAD/GUARDS phase (before commit)
- Never have side effects

#### Type Guards

**Currently Implemented**:
- ✅ `ground(X)` - succeeds if X contains no unbound variables, fails otherwise
- ✅ `known(X)` - succeeds if X is bound, fails if unbound variable

**Planned Type Guards**:
- ⏳ `number(X)` - succeeds if X bound to number, suspends if X unbound, fails if X bound to non-number
- ⏳ `integer(X)` - succeeds if X bound to integer, suspends if X unbound, fails otherwise
- ⏳ `writer(X)` - succeeds if X is a writer variable, fails if reader or ground
- ⏳ `reader(X)` - succeeds if X is a reader variable, fails if writer or ground

#### Arithmetic Comparison Guards

All arithmetic guards suspend on unbound readers and fail on type errors:

**Planned Comparison Guards**:
- ⏳ `X < Y` - less than (suspends if either unbound, fails if non-numeric)
- ⏳ `X =< Y` - less than or equal (note: `=<` not `<=` per Prolog convention)
- ⏳ `X > Y` - greater than
- ⏳ `X >= Y` - greater than or equal
- ⏳ `X =:= Y` - arithmetic equality (evaluates expressions, compares results)
- ⏳ `X =\= Y` - arithmetic inequality (evaluates expressions, compares results)

**Precedence and Associativity**:
```
Comparison operators: 700 (non-associative)
  < =< > >= =:= =\=
Additive: 500 (left-associative)
  + -
Multiplicative: 400 (left-associative)
  * / mod
Unary minus: 200 (non-associative)
  -
```

**Example**:
```prolog
% Quicksort with comparison guards
partition(Pivot, [], [], []).
partition(Pivot, [X | Xs?], [X | Smaller], Greater) :-
    X? < Pivot? |    % Guard: suspend if X or Pivot unbound
    partition(Pivot?, Xs?, Smaller, Greater).
partition(Pivot, [X | Xs?], Smaller, [X | Greater]) :-
    X? >= Pivot? |   % Guard: suspend if X or Pivot unbound
    partition(Pivot?, Xs?, Smaller, Greater).
```

#### Control Guards

**Currently Implemented**:
- ✅ `otherwise` - succeeds if all previous clauses for this procedure failed (not suspended)

**Planned**:
- ⏳ `true` - always succeeds (equivalent to no guard)

#### Unification Guards

**Planned**:
- ⏳ `X = Y` - unification guard (suspends on unbound readers, fails if cannot unify)
- ⏳ `X \= Y` - non-unification guard (succeeds if cannot unify, suspends on unbound readers)

### CRITICAL: Ground Guards - Exception to Strict SRSW

The SRSW syntactic restriction requires "exactly one of each" in a clause. However, there is ONE exception:

**When a guard guarantees groundness, multiple READER occurrences are allowed.**

Why this is safe:
- Ground terms contain no unbound writers
- Multiple readers cannot violate single-writer when no writer can be exposed
- This exception is ESSENTIAL for concurrent programming patterns

This is NOT a violation but a controlled relaxation under specific conditions.

**Guards that imply groundness**:
- ✅ `ground(X)` - explicitly tests for groundness
- ⏳ `integer(X)` - integers are always ground (when implemented)
- ⏳ `number(X)` - numbers are always ground (when implemented)

**Correct patterns**:
```prolog
% ✅ CORRECT - ground guard allows multiple X? occurrences
broadcast(X, Y1, Y2, Y3) :- ground(X) |
    send(X?, Y1),     % X? appears 3 times - OK!
    send(X?, Y2),
    send(X?, Y3).

% ✅ CORRECT - integer guard implies groundness (when implemented)
distribute(N, R1, R2) :- integer(N) |
    execute('evaluate', [N? * 2, R1]),   % N? appears twice - OK!
    execute('evaluate', [N? * 3, R2]).

% ✅ CORRECT - ground guard with arithmetic
compute_twice(X, Y1, Y2) :- ground(X) |
    execute('evaluate', [X? + 1, Y1]),
    execute('evaluate', [X? * 2, Y2]).
```

**Incorrect patterns**:
```prolog
% ❌ WRONG - no ground guard, SRSW violation
bad_broadcast(X, Y1, Y2) :-
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X? appears twice without ground guard

% ❌ WRONG - known(X) does NOT imply ground
bad_use(X, Y1, Y2) :- known(X) |
    send(X?, Y1),    % SRSW VIOLATION!
    send(X?, Y2).    % X could be f(Y) where Y is unbound
```

**Key Insight**: This relaxation enables broadcasting and multi-reader patterns essential for concurrent programming.

**Example**:
```prolog
% Using implemented guards
factorial(N, F) :- known(N), ground(N) | compute_fact(N?, F).

% Using metainterpreter pattern
run(A) :- otherwise | clause(A?, B), run(B?).
```

#### Lexer/Parser Integration

**Token Definitions** (add to lexer):
```dart
// Comparison operators (precedence 700, non-associative)
'<'     → LESS
'=<'    → LESS_EQUAL     // Prolog convention, not <=
'>'     → GREATER
'>='    → GREATER_EQUAL
'=:='   → ARITH_EQUAL
'=\\='  → ARITH_NOT_EQUAL

// Unification guards (precedence 700)
'='     → UNIFY
'\\='   → NOT_UNIFIABLE
```

**Operator Precedence Table**:
```
1200  :- (rule separator)
1100  | (guard separator)
 700  < =< > >= =:= =\= = \= (comparison/test operators, non-associative)
 500  + - (additive, left-associative)
 400  * / mod (multiplicative, left-associative)
 200  - (unary minus, non-associative)
```

**Parser Rules** (extend guard production):
```
guard ::= 'ground' '(' term ')'
        | 'known' '(' term ')'
        | 'integer' '(' term ')'
        | 'number' '(' term ')'
        | 'writer' '(' term ')'
        | 'reader' '(' term ')'
        | 'otherwise'
        | 'true'
        | expr COMPARISON_OP expr     // X < Y, X =< Y, etc.
        | term '=' term               // Unification guard
        | term '\\=' term             // Non-unification guard

COMPARISON_OP ::= '<' | '=<' | '>' | '>=' | '=:=' | '=\\='
```

**Precedence Handling**:
- Use Pratt parsing for expression operators
- Comparison operators are non-associative (reject `X < Y < Z`)
- Guard separator `|` binds less tightly than all comparison operators

### System Predicates Called via Execute (Two-Valued: Success/Abort)

**System predicates provide immediate operations** called via the `execute/2` goal:
- Syntax: `execute('predicate_name', [Arg1, Arg2, ...])`
- Execute synchronously during BODY phase (after commit)
- **Two-valued semantics**: SUCCESS or ABORT (never suspend)
- **Require all inputs bound** - unbound reader in arguments causes runtime abort
- Execute in order as part of instruction stream
- May have side effects (I/O, file operations, etc.)

**Abort Conditions**:
- Unbound reader in arguments
- Type mismatch (e.g., non-numeric in arithmetic)
- Domain error (e.g., division by zero)
- System error (e.g., file not found)

**Standard System Predicates**:
- `evaluate` - arithmetic evaluation (aborts on unbound reader or type error)
- `write`, `nl`, `read` - terminal I/O
- `file_read`, `file_write`, `file_exists` - file I/O
- `file_open`, `file_close`, `file_read_handle`, `file_write_handle` - handle-based file I/O
- `directory_list` - directory operations
- `current_time`, `unique_id`, `variable_name` - system information
- `copy_term`, `distribute_stream`, `copy_term_multi` - term operations
- `link`, `load_module` - module loading

**IMPORTANT**: Safe execution pattern requires guards before execute:

```prolog
% WRONG - execute without guards
unsafe_divide(X, Y, Z) :-
  execute('evaluate', [X? / Y?, Z]).  % ABORT if X unbound or Y = 0

% CORRECT - guards ensure safety
safe_divide(X, Y, Z) :-
  number(X), number(Y), Y =\= 0 |     % guards ensure preconditions
  execute('evaluate', [X? / Y?, Z]).  % safe to execute
```

### Arithmetic Expressions

**Syntax**: Infix notation with standard precedence
```
expr ::= number | variable | -expr
       | expr + expr | expr - expr
       | expr * expr | expr / expr | expr mod expr
       | (expr)
```

**Semantics**:
- Parser transforms infix → prefix: `X + Y` becomes `+(X, Y)`
- Evaluation via `execute('evaluate', [Expr?, Result])`
- Three-valued: success (all operands integers), suspend (unbound reader), fail (non-integer)
- Type system: integers only (no floats)
- Division by zero fails

**Example**:
```prolog
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).
% Parser transforms to: execute('evaluate', [+(X?, Y?), Z])
```

### Migration Note for Existing Programs

Programs using arithmetic through explicit prefix notation will continue to work unchanged:

```prolog
% Existing code (prefix notation) - STILL VALID
add(X, Y, Z) :- execute('evaluate', [+(X?, Y?), Z]).
compute(Z) :- execute('evaluate', [*(+(2, 3), 4), Z]).
```

The parser enhancement allows the more natural infix syntax as syntactic sugar:

```prolog
% New code (infix notation) - EQUIVALENT
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).
compute(Z) :- execute('evaluate', [(2 + 3) * 4, Z]).
```

**Both forms compile to identical bytecode**. The infix notation is purely a parser-level transformation—the runtime, bytecode instructions, and `evaluate/2` implementation remain unchanged. This is a **backward-compatible enhancement**.

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

### Anonymous Variable `_` in SRSW

The anonymous variable `_` is exempt from SRSW checking:

- **`_` is a writer that nobody reads** - it's a placeholder for values that are discarded
- Each `_` occurrence creates a fresh, independent variable (no sharing)
- Use `_` in abort clauses where the result is never bound:

```prolog
% CORRECT: _ as result in abort clause (value never bound)
_ := X / Y :-
  number(X?), number(Y?), Y? =:= 0 |
  abort("Division by zero").

% WRONG: Result? with no writer violates SRSW
Result? := X / Y :-
  number(X?), number(Y?), Y? =:= 0 |
  abort("Division by zero").
```

**Key insight**: `_` satisfies SRSW because it's a writer occurrence, but since it's anonymous, there's no expectation of a paired reader.

### SRSW is Mandatory

**All GLP code must be compiled with SRSW checking enabled.** There is no option to disable SRSW checking. The compiler will reject any code that violates SRSW.

- Do NOT work around SRSW violations - fix them properly
- If SRSW seems too restrictive for a pattern, discuss the design
- The ground guard exception exists for legitimate multi-reader patterns

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
