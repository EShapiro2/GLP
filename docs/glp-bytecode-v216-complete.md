# GLP Bytecode Instruction Set Specification — v2.16 (Normative)

## 0. ISA Conventions
- **σ̂w** denotes the clause-local (goal-local) tentative writer substitution; it exists only during Phase 1 and is discarded at clause_next and suspend.
- **Phases per clause Ci**: HEAD_i ; GUARDS_i ; BODY_i.
- **Registers**: A (arguments), X (temporaries). Env stack E.
- **κ** denotes the clause-selection entry PC of the current procedure (the PC where the first clause of the procedure begins).

### Code Organization Hierarchy

There are three levels in the code organization:

1. **Module**: The complete bytecode program containing all procedures
2. **Procedure**: A named predicate (e.g., `p/1`, `merge/3`) consisting of all clauses with the same head functor/arity
3. **Clause**: A single rule within a procedure (head :- body)

**Key Principle**: Each goal maintains a κ value pointing to the entry PC of the procedure it is currently executing. When a goal suspends, it stores κ. On reactivation, execution resumes at PC = κ (the first clause of that procedure), NOT at the beginning of the module or at the suspension point.

## 1. Register Architecture

### Data Registers
- **A1-An**: Argument registers for passing parameters between procedures
- **X1-Xn**: Temporary registers for local computation within a clause
- **Y1-Yn**: Permanent registers stored in environment frames

### Control Registers
- **PC**: Program counter
- **CP**: Continuation pointer (return address for deterministic calls)
- **E**: Environment pointer (current frame for permanent variables)
- **H**: Heap pointer (next free heap location)
- **S**: Structure pointer (current position in structure traversal)
- **Mode**: Current unification mode (READ/WRITE)

Note: The E, CP, and Y registers are used exclusively for deterministic environment frames that store permanent variables and return addresses. GLP uses committed-choice semantics where clause selection occurs only during initial head unification, with no ability to backtrack once a clause body begins execution.

## 2. Control Instructions

### 2.1 clause_try Ci
**Phase**: clause head/guards entry.  
**Effect**: initialize Si := ∅; initialize σ̂w := ∅.

### 2.2 clause_next Cj
**Phase**: clause head/guards exit on FAIL or SUSPEND_READY.
**Effect**: if Si ≠ ∅ then U := U ∪ Si; discard σ̂w; clear Si; jump to label of Cj.
**Purpose**: Accumulate suspension sets across clause attempts. When a clause fails or suspends, any readers added to Si during that clause are unioned into the goal-level suspension set U before trying the next clause.

### 2.3 try_next_clause
**Status**: IMPLEMENTED but UNUSED - functionality overlaps with clause_next

**Implementation**: Has handler in runner.dart (line 967) that calls `_softFailToNextClause()` and jumps to next `clause_try`.

**Usage**: Never emitted by assembler or used in tests. The `clause_next` instruction (section 2.2) is preferred for all clause transitions as it handles both the Si union and the jump in a single instruction.

**Semantic difference**: `try_next_clause` would be used WITHIN a clause when a guard fails, whereas `clause_next` is used at the END of a clause. In practice, all tests use `clause_next` exclusively.

### 2.4 no_more_clauses
**Operation**: All clauses exhausted without success  
**Behavior**:
- If suspension set non-empty: suspend goal on those readers
- Otherwise: mark goal as permanently failed
- No recovery or retry mechanism exists

## 3. Commit

### 3.1 commit
**Phase**: boundary before BODY_i.  
**Timing**: occurs immediately before the first BODY instruction.  
**Effect**: atomically apply σ̂w to the heap; for each writer bound by σ̂w, bind paired RO and process ROQ in FIFO order; clear σ̂w; control enters BODY_i.

## 4. Environment and Phase Discipline (Instruction-Level)

### 4.1 Head and Guard opcodes
**Env effect**: E' = E. They may: read cells; record tentative writer bindings in σ̂w; add readers to Si; raise FAIL.  
**They MUST NOT**: allocate or deallocate frames; mutate RO cells; perform I/O.

### 4.2 Body opcodes
Only BODY_i may contain:
- **allocate**: push a new environment frame.
- **deallocate**: pop environment frame.

## 5. Guard Purity
**Allowed primitives**: tag/status tests; equality/inequality on constants; structural equality on ground terms; integer comparisons on ground values.  
A guard that demands an uninstantiated reader adds that reader to Si and continues scanning; short-circuiting is only permitted when result is decidable independently of suspended subexpressions.

## 6. Head Processing Instructions

All head_* operations are **tentative**: they update the σ̂w (tentative writer substitution) and/or the suspension set S **without mutating heap cells** during clause try. Heap mutations happen only at **commit**.

### 6.1 head_structure f/n, Ai
**Operation**: Process structure with functor f and arity n in argument register Ai  
**Behavior**:
- Dereference the value in Ai
- If structure with matching functor: enter READ mode, set S to first argument
- If writer variable: record pending binding Ai = f(...) in σ̂w; no heap mutation during clause try
- If reader variable: add to suspension set S
- Otherwise: fail and discard σ̂w

### 6.2 head_writer Xi
**Operation**: Process writer variable in clause head  
**Behavior**:
- In READ mode: extract value from current structure position (S) into Xi
- In WRITE mode: record new writer creation in σ̂w
- Operate against tentative state, not actual heap
- Increment S after operation

### 6.3 head_reader Xi
**Operation**: Process reader variable in clause head
**Behavior**:
- In READ mode (inside structure): verify value at S against paired writer Xi in tentative state
- In WRITE mode (inside structure): record reader constraint in σ̂w
- **When used as top-level argument** (via GetValue after GetVariable):
  - If argument is an unbound writer W: bind W to the value of reader Xi in σ̂w
  - If argument is a bound writer: verify it matches reader Xi's value
  - If argument is an unbound reader R: FAIL (the clause-local reader Xi can never be bound in the future, so this unification can never succeed in the future)
- If writer Xi is unbound in tentative state: add to suspension set
- Increment S after operation

**Writer MGU semantics for reader in argument position**:
When a reader Xi? appears in a head argument position and the corresponding
goal argument is an unbound writer W, the Writer MGU binds W to the term
that Xi references. If the goal argument is an unbound reader R, unification
fails definitively because the clause-local reader Xi has no future binding
that could make the unification succeed.

### 6.4 head_constant c, Ai
**Operation**: Match constant c with argument Ai  
**Behavior**:
- Dereference value in Ai
- If matching constant: succeed
- If writer variable: record Ai = c in σ̂w
- If reader variable: add to suspension set S
- Otherwise: fail and discard σ̂w

### 6.5 head_nil Ai
**Operation**: Match empty list [] with argument Ai  
**Behavior**:
- Special case of head_constant for empty list
- Same unification semantics as head_constant

### 6.6 head_list Ai
**Operation**: Process list structure [H|T] in argument Ai  
**Behavior**:
- Equivalent to head_structure './2', Ai
- Optimized instruction for common list operations

## 7. Body Construction Instructions

All put_*/body construction instructions are **heap-mutating**: they allocate and write structures/values to the heap as part of **body execution**.

### 7.1 put_structure f/n, Ai
**Operation**: Create structure with functor f/n in argument Ai  
**Behavior**:
- Allocate structure header on heap at position H
- Store functor f/n at heap
- Set Ai to point to this structure
- Enter WRITE mode for subsequent writer/reader instructions
- Increment H

### 7.2 put_writer Xi, Ai
**Operation**: Place writer variable Xi in argument Ai  
**Behavior**:
- Copy writer reference from Xi to Ai
- Track for SRSW enforcement

### 7.3 put_reader Xi, Ai
**Operation**: Place reader Xi? (reader of writer Xi) in argument Ai  
**Behavior**:
- Create reader reference to writer Xi
- Place in argument register Ai
- Mark as reader for suspension handling

### 7.4 put_constant c, Ai
**Operation**: Place constant c in argument Ai  
**Behavior**:
- Store constant value directly in Ai

### 7.5 put_nil Ai
**Operation**: Place empty list in argument Ai  
**Behavior**:
- Special case of put_constant for []

### 7.6 put_list Ai
**Operation**: Begin list construction in argument Ai  
**Behavior**:
- Equivalent to put_structure './2', Ai

## 8. Structure Building Instructions

These instructions fill in structure arguments after head_structure or put_structure.

**Nested Structures** (from WAM Technical Note 309, Section 8.4):
When a nested structure occurs during structure traversal:
- **In HEAD mode**: Use `unifyWriter Xi` to extract the nested structure into a temporary variable Xi, then after the current unify sequence completes, use `headStruct(f, n, Xi)` to match the extracted structure against pattern f/n
- **In BODY mode**: Pre-build the nested structure with `putStructure(f, n, Xi)` before the unify sequence, then reference it with `unifyValue Xi` during traversal

**Example**: For `clause(merge([X|Xs],Ys,[X?|Zs?]), ...)`:
```
headStruct('merge', 3, 0)      // Match merge/3, enter READ mode, S=0
  unifyWriter(10)              // Extract arg at S=0 into X10 (the list [X|Xs])
  unifyWriter(2)               // Extract arg at S=1 into X2 (Ys)
  unifyWriter(11)              // Extract arg at S=2 into X11 (the list [X?|Zs?])
// Now match the extracted nested structures
headStruct('[|]', 2, 10)       // Match X10 against [|]/2
  unifyWriter(0)               // X
  unifyWriter(1)               // Xs
headStruct('[|]', 2, 11)       // Match X11 against [|]/2
  unifyReader(0)               // X?
  unifyReader(3)               // Zs?
```

### 8.1 writer Xi
**Operation**: Process writer variable in structure
**Behavior**:
- In READ mode:
  - Extract value at S (may be constant, structure, writer variable, or reader term)
  - Store the extracted value in clause variable Xi
  - If unbound writer: record writer-to-writer unification in σ̂w
  - If bound writer: use its value (including bound structures)
  - If reader with bound paired writer: dereference and use paired writer's value
  - If reader with unbound paired writer: store the reader term itself in Xi
- In WRITE mode: create new writer variable, store at H and in Xi
- Increment S (READ) or H (WRITE)

**Note**: Writer-to-writer unification follows Writer MGU semantics. In READ mode, this instruction extracts any term (including nested structures and reader terms) for later matching. When a reader term is extracted, subsequent operations (like head_structure on that clause variable) will handle suspension if the reader remains unbound at match time.

### 8.2 reader Xi
**Operation**: Process reader variable in structure
**Behavior**:
- In READ mode:
  - If value at S is reader R: verify R pairs with Xi
  - If value at S is unbound writer: bind it to reader Xi in σ̂w
  - If value at S is bound writer/constant: verify it equals Xi's paired writer value
  - Add to Si if Xi's paired writer is unbound
- In WRITE mode: create reader reference at H
- Increment S (READ) or H (WRITE)

**Note**: Reader unification follows Writer MGU semantics - readers can only be read, not written

### 8.3 constant c
**Operation**: Process constant in structure
**Behavior**:
- In READ mode:
  - If value at S is constant c: succeed
  - If value at S is unbound writer W: bind W = c in σ̂w (writer MGU)
  - If value at S is reader R with unbound paired writer: add R to Si (suspend)
  - If value at S is reader R with bound paired writer ≠ c: fail
  - Otherwise: fail
- In WRITE mode: write constant c at H
- Increment S (READ) or H (WRITE)

**Rationale**: Follows Writer MGU semantics from GLP spec Definition 2.1. In READ mode, we perform writer unification: constants unify with unbound writers by binding them, and with readers by checking their paired writer's value.

### 8.4 void n
**Operation**: Process n anonymous variables
**Behavior**:
- In READ mode: skip n positions (S += n)
- In WRITE mode: create n unbound variables (H += n)

### 8.5 Variables in Structures (Non-Ground Structures)

When building structures in BODY mode that contain variables (non-ground structures), variables must retain their identity as variable references rather than being converted to constants.

**Behavior for variables in structures**:
- Writer variables: Use `set_writer Xi` instruction to place writer reference in structure
- Reader variables: Use `set_reader Xi` instruction to place reader reference in structure
- Fresh variables: Allocate new register with `set_writer Xi` in WRITE mode

**Example**: Building `merge([],[],X)` where X is a writer in register 5:
```
put_structure 'merge', 3, A0    // Begin structure, enter WRITE mode
  set_constant 'nil'             // First argument: []
  set_constant 'nil'             // Second argument: []
  set_writer 5                   // Third argument: variable X (not constant!)
```

**Note**: This is essential for metainterpreter patterns where goal terms contain unbound variables.

## 9. Control Flow Instructions

### 9.1 spawn P/n
**Operation**: Spawn new concurrent goal for procedure P with arity n
**Behavior**:
- Create new goal with fresh goal ID
- Copy current argument registers to new goal's environment (CallEnv)
- Set new goal's κ to the entry PC of procedure P (first clause of P)
- Register the environment with the runtime via `setGoalEnv(goalId, env)`
- Enqueue the new goal in the scheduler's goal queue
- Arguments passed via environment (slot → writer/reader ID mapping)
- Used for all body goals except the final one
- Ensures fair scheduling among concurrent goals
- The spawning goal continues execution at the next instruction

### 9.2 requeue P/n
**Operation**: Tail-call to procedure P, replacing current goal
**Behavior**:
- Update current goal's environment with new arguments from argument registers
- **Update current goal's κ to the entry PC of procedure P** (critical for suspension/reactivation)
- Clear all clause-local state (σ̂w, Si, U, clauseVars, inBody flag)
- Jump PC to the entry point of procedure P
- Reuse current goal ID and process frame (no new goal created)
- Decrement tail-recursion counter (initially 26)
- If counter > 0: Continue execution immediately
- If counter = 0: Reset counter, yield to event queue before continuing
- Used only for the final goal in a clause body (tail position)
- If this goal later suspends, it will reactivate at procedure P's entry, not the original procedure
- Prevents unbounded queue growth while ensuring fairness

**Example**: Consider `boot :- p(X), p(X?).` compiled as:
```
boot/0:                    % κ = 7 for boot
  clause_try
  commit
  put_writer X, A1         % Create writer X
  spawn p/1                % Spawn new goal for p(X), new goal has κ = 0
  put_reader X, A1         % Create reader X?
  requeue p/1              % Tail-call: update THIS goal's κ from 7 to 0!
                           % Now executing as p(X?)
p/1:                       % κ = 0 for p
  clause_try
  head_constant a, A1
  commit
  proceed
```
When the original boot goal executes `requeue p/1`, its κ changes from 7 (boot/0) to 0 (p/1). If it suspends while executing p(X?), it will reactivate at PC 0 (p/1's first clause), not PC 7 (boot/0's entry).

### 9.3 proceed
**Operation**: Complete current procedure  
**Behavior**:
- Return control to continuation point in CP
- Process continues with next instruction after spawn

### 9.4 allocate n
**Operation**: Create environment frame with n permanent variables  
**Behavior**:
- Push new frame on local stack
- Save E and CP in frame
- Update E to point to new frame

### 9.5 deallocate
**Operation**: Remove current environment frame  
**Behavior**:
- Restore E and CP from current frame
- Pop frame from local stack

## 10. Suspension Management

**Note**: Suspension management in GLP is handled by **runtime operations**, not explicit bytecode instructions. The following operations occur automatically during execution:

### 10.1 Reactivation (automatic during commit)
**Trigger**: When `commit` binds a writer X
**Behavior**:
- Runtime calls `CommitOps.applySigmaHat()` which:
  - Binds writer X on heap
  - Binds paired reader X?
  - Calls `ROQueues.processOnBind(X?)` to find all goals suspended on X?
  - Enqueues reactivated goals in FIFO order to goal queue
  - Uses single-shot hanger mechanism (armed flag) to prevent duplicate reactivation

### 10.2 Abandonment (explicit runtime operation)
**Trigger**: When program explicitly abandons a writer (e.g., exception handling, cancellation)
**Behavior**:
- Runtime calls `AbandonOps.abandonWriter(writerId)` which:
  - Marks writer as abandoned
  - Processes ROQ for paired reader, reactivating suspended goals
  - Reactivated goals detect abandonment and fail upon resumption

**Implementation note**: These are not bytecode instructions but rather runtime operations invoked automatically by `commit` or explicitly via runtime API calls. No explicit `reactivate`/`abandon` bytecode instructions exist or are needed.

## 11. Guard Instructions

Guards execute in **Phase 1** (head+guards) and are **pure tests**; they may succeed, fail, or suspend, but **do not mutate heap state**. On failure, the tentative σ̂w is discarded and the next clause head is tried; on suspension, the **goal** is suspended.

### 11.1 guard P, Args
**Operation**: Call guard predicate P  
**Behavior**:
- Execute guard without side effects
- If succeeds: continue
- If fails: try next clause
- If suspends: suspend entire goal

### 11.2 ground X
**Operation**: Succeeds if X is ground (contains no unbound variables)
**Three-valued semantics**:
1. If X is ground → **SUCCEED** (continue to next instruction, pc++)
2. If X contains unbound readers (but no unbound writers) → **SUSPEND** (add unbound readers to Si, continue to next instruction, pc++)
3. If X contains unbound writers → **FAIL** (soft-fail to next clause via clause_next)

**Rationale**: Due to SRSW restriction, unbound readers may become ground when their paired writers are bound, so suspension is appropriate. Unbound writers cannot be awaited (unknown future binding), so failure is definitive.

**Usage**: Enables multiple reader occurrences by testing groundness before use.

### 11.3 known X
**Operation**: Succeeds if X is not an unbound variable
**Three-valued semantics**:
1. If X is bound (to any value, including structures with variables) → **SUCCEED** (continue, pc++)
2. If X is an unbound reader → **SUSPEND** (add reader to Si, continue, pc++)
3. If X is an unbound writer → **FAIL** (soft-fail to next clause)

**Difference from ground**: `known(X)` only tests whether X itself is bound, not whether X contains unbound variables internally. `ground(f(Y))` fails if Y is unbound, but `known(f(Y))` succeeds because f(Y) is a bound structure.

### 11.4 otherwise
**Operation**: Default guard
**Behavior**:
- Succeeds if all previous clauses failed
- Used for catch-all clauses

### 11.5 if_writer X
**Operation**: Type test - succeeds if X is a writer variable
**Three-valued semantics**:
1. If X is WriterTerm → **SUCCEED** (continue, pc++)
2. If X is not WriterTerm (reader or constant) → **FAIL** (soft-fail to next clause)

**Usage**: Enables type-based dispatching and pattern discrimination
**Example**:
```
process(X) :- if_writer(X) | ... handle writer case
process(X) :- if_reader(X) | ... handle reader case
process(X) :- otherwise    | ... handle constant case
```

### 11.6 if_reader X
**Operation**: Type test - succeeds if X is a reader variable
**Three-valued semantics**:
1. If X is ReaderTerm → **SUCCEED** (continue, pc++)
2. If X is not ReaderTerm (writer or constant) → **FAIL** (soft-fail to next clause)

**Usage**: Complements if_writer for complete type discrimination

## 12. System Instructions

### 12.1 get_variable Xi, Ai
**Operation**: Load argument into register  
**Behavior**:
- Copy value from Ai to Xi
- First occurrence of variable in clause head
- When used during **head matching**, this records a **tentative association** in **σ̂w** (no heap mutation)

### 12.2 get_value Xi, Ai
**Operation**: Unify argument with register  
**Behavior**:
- Perform writer MGU between Xi and Ai
- Subsequent occurrence of variable
- When used during **head matching**, this computes a **tentative writer MGU** and updates **σ̂w** (no heap mutation)

### 12.3 set Xi
**Status**: NOT IMPLEMENTED - reserved for future optimization
**Operation**: Initialize argument position
**Behavior**:
- Would set argument register pointer to Xi
- Would be used before sequence of put instructions
- Current implementation handles argument setup directly in put_* instructions

## 13. Utility Instructions

### 13.1 nop
**Operation**: No operation  
**Behavior**:
- Advance PC without other effects
- Used for alignment or patching

### 13.2 halt
**Operation**: Terminate execution  
**Behavior**:
- Mark goal as completed
- Return control to scheduler

### 13.3 label L
**Operation**: Mark jump target  
**Behavior**:
- No runtime effect
- Provides symbolic address for jumps

## 14. Instruction Encoding

**Status**: NOT IMPLEMENTED - current implementation uses Dart objects

The current Dart implementation represents instructions as Dart class instances (see `opcodes.dart`), not as byte-encoded binary format. Each instruction is a Dart object implementing the `Op` interface.

**Future binary encoding** could use:
- **Opcode**: 1 byte (0-255)
- **Operands**: Variable length based on instruction type
- **Registers**: 1 byte for variable indices
- **Functors**: 2 bytes (index into symbol table)
- **Constants**: Variable length with type tag
- **Labels**: 2 bytes (relative offset)
- **Arities**: 1 byte (0-255)

**Compact forms** (potential optimization, not implemented):
- **head_list_writer**: Combines head_list + head_writer
- **put_list_writer**: Combines put_list + put_writer
- **get_constant_proceed**: Combines get_constant + proceed

## 15. Execution Model

### Writer MGU Algorithm
The writer MGU (Most General Unifier) differs from standard unification:
1. Only writers can be bound (not readers)
2. Writers cannot be bound to other writers
3. Readers can only be verified against their paired writers
4. Suspension occurs when readers block unification

### Suspension Mechanism
Goals suspend when encountering unbound readers:
1. `no_more_clauses` instruction triggers suspension if U non-empty
2. Runtime calls `suspendGoal(goalId, kappa, readers)` with U set
3. For each reader in U, suspension note added to reader's ROQ (Read-Only Queue)
4. Suspension note contains: goalId, entry PC (kappa), and single-shot hanger
5. When reader's paired writer binds (during commit), ROQ processes suspension notes
6. Reactivated goals enqueued to GQ (goal queue) with PC=kappa (first clause)
7. Single-shot hanger (armed flag) prevents duplicate reactivation

### SRSW Enforcement
The Single-Reader/Single-Writer constraint operates at two levels:
- **Compile time**: Each clause verified to contain at most one occurrence of any writer/reader
- **Runtime**: Variable table tracks usage across clauses/agents to detect dynamic violations

## 16. Memory Layout (Dart Implementation)

**Note**: The Dart implementation uses object-oriented data structures, not traditional WAM-style heap cells.

### Heap Organization (`lib/runtime/heap.dart`)
The heap manages writer and reader cells:
- **WriterCell**: `WriterCell(writerId, readerId)` - tracks writer ID and paired reader ID
- **ReaderCell**: `ReaderCell(readerId)` - tracks reader ID
- **Bindings**: `Map<int, Object?> writerValue` - maps writer IDs to their bound values
- **Terms**: `WriterTerm(writerId)`, `ReaderTerm(readerId)`, `StructTerm(functor, args)`, `ConstTerm(value)`

### Runtime State (`lib/runtime/runtime.dart`)
- **GQ (Goal Queue)**: FIFO queue of `GoalRef(goalId, pc)` - active goals ready to execute
- **ROQ (Read-Only Queues)**: `Map<int, Queue<SuspensionNote>>` - per-reader suspension queues
- **Goal Environments**: `Map<int, CallEnv>` - maps goalId to argument bindings
- **Goal Programs**: `Map<int, Object?>` - maps goalId to program key for multi-program execution

### Stack Frames (`allocate`/`deallocate`)
**Status**: Partially implemented - environment frames for permanent variables
- Each `allocate N` creates a frame with N slots for Y variables
- Frame contains: previous E pointer, continuation PC, permanent variable slots
- `deallocate` restores previous E and CP

### Variable Table (Multiagent Support)
**Status**: NOT IMPLEMENTED - reserved for future multiagent security features
- Would track: variable ID, creator agent, writer/reader status, cryptographic attestation
- Current implementation uses simple integer IDs without agent tracking

## 17. Interaction with Runtime Structures

### Suspension Set Accumulation
- **Si**: Clause-local suspension set (cleared at each `clause_try`)
- **U**: Goal-level suspension set accumulated across all attempted clauses
- **clause_next**: Unions Si into U before jumping to next clause (if Si non-empty)
- **Important**: Both FAIL and SUSPEND cases execute `clause_next`, which unions Si to U
  - This means U accumulates readers from ALL clauses, not just "SUSPEND_READY" clauses
  - Empty Si (definitive fail) contributes nothing to U, but instruction still executes

### Reactivation Entry Point
- **κ (kappa)**: Entry PC for the goal (typically first clause of predicate)
- When goal suspends via `no_more_clauses`, it saves: goalId, kappa, U
- When reactivated (reader binds), goal resumes at PC = kappa (NOT at suspension point)
- This means goal re-attempts all clauses from the beginning on reactivation

### Scheduler Interaction
- All reactivations append to the tail of GQ (goal queue)
- Reactivation is NEVER executed inline - always via scheduler
- FIFO ordering ensures fairness across concurrent goals
- Tail-recursion budget (`requeue` instruction) prevents starvation

## 18. System Predicates (Execute Mechanism)

**Status**: FULLY IMPLEMENTED

System predicates are external Dart functions callable from GLP bytecode via the `execute` instruction. They follow three-valued semantics (success/suspend/failure) and properly handle unbound readers.

### 18.1 execute Predicate, Args
**Operation**: Call external system predicate
**Behavior**:
- Look up predicate by name in SystemPredicateRegistry
- Pass arguments as list of Terms
- Return SystemResult: success, suspend, or failure
- If suspend: adds unbound readers to Si (clause-local suspension set)

**Phase**: Can be used in guards (Phase 1) or body (Phase 3)
- In guards: pure test, no heap mutation allowed
- In body: may mutate heap after commit

### 18.2 Implemented System Predicates

**Arithmetic**:
- `evaluate(Expression, Result)` - Arithmetic evaluation with operators: +, -, *, /, mod
  - Suspends on unbound readers in expression
  - Binds or verifies result
  - Fails on division by zero or type errors

**Utilities**:
- `current_time(Time)` - Binds Time to current milliseconds since epoch
- `unique_id(ID)` - Generates unique sequential integer IDs
- `variable_name(Var, Name)` - Returns string name for writer/reader (e.g., "W123", "R456")
- `copy_term(Term, Copy)` - Deep copy of term (suspends on unbound readers)

**File I/O - Simple**:
- `file_read(Path, Contents)` - Read entire file as string
- `file_write(Path, Contents)` - Write string to file (overwrites)
- `file_exists(Path)` - Test if file exists

**File I/O - Handle-Based**:
- `file_open(Path, Mode, Handle)` - Open file, return handle
  - Modes: 'read', 'write', 'append', 'read_write'
  - Handle is integer ID managed by runtime
- `file_close(Handle)` - Close file handle
- `file_read_handle(Handle, Contents)` - Read remaining contents from open file
- `file_write_handle(Handle, Contents)` - Write to open file

**Directory Operations**:
- `directory_list(Path, Entries)` - List directory contents as list of filenames

**Terminal I/O**:
- `write(Term)` - Write term to stdout
- `nl()` - Write newline to stdout
- `read(Term)` - Read line from stdin (blocks until input available)

**Module Loading**:
- `link(ModulePath, Handle)` - Load dynamic library via FFI, return handle
  - Path can be string or list of strings
  - Uses dart:ffi DynamicLibrary.open()
- `load_module(FileName, Module)` - Load GLP bytecode module from file
  - Returns module as Map with metadata
  - TODO: Bytecode deserialization format specification

### 18.3 Suspension Semantics

All system predicates follow consistent suspension rules:
1. Extract arguments, checking term types
2. If argument is ReaderTerm, check if paired writer is bound
3. If writer unbound, add reader to `call.suspendedReaders` and return SystemResult.suspend
4. If all inputs ground, execute predicate logic
5. Bind output variables or verify against existing bindings

**Example execution flow**:
```
execute('evaluate', [+(ReaderTerm(r5), ConstTerm(10)), WriterTerm(w1)])

1. Check r5's paired writer (w5)
2. If w5 unbound → return suspend with suspendedReaders = {r5}
3. If w5 bound to ConstTerm(7) → compute 7 + 10 = 17
4. Bind w1 to ConstTerm(17)
5. Return success
```

### 18.4 Registry and Extension

System predicates registered in `lib/runtime/system_predicates_impl.dart`:

```dart
void registerStandardPredicates(SystemPredicateRegistry registry) {
  registry.register('evaluate', evaluatePredicate);
  registry.register('current_time', currentTimePredicate);
  // ... 21 total predicates
}
```

**Adding new predicates**:
1. Implement predicate function: `SystemResult myPredicate(GlpRuntime rt, SystemCall call)`
2. Handle argument extraction with suspension on unbound readers
3. Return SystemResult.success/failure/suspend
4. Register in `registerStandardPredicates()`

### 18.5 Deferred Predicates

**Channel primitives** (deferred for future implementation):
- `create_merger(InputList, Output)` - N-to-1 stream merger
- `distribute_stream(Input, OutputList)` - 1-to-N stream distributor
- `copy_term(Term, Copy1, Copy2)` - Multi-output deep copy

These require additional runtime support for stream merging and multi-reader coordination.