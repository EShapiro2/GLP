# GLP Bytecode Instruction Set Specification — v2.16 (Normative)

## 0. ISA Conventions
- **σ̂w** denotes the clause-local (goal-local) tentative writer substitution; it exists only during Phase 1 and is discarded at clause_next and suspend.
- **Phases per clause Ci**: HEAD_i ; GUARDS_i ; BODY_i.
- **Registers**: A (arguments), X (temporaries). Env stack E.
- **κ** denotes the clause-selection entry PC of the current predicate.

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
**Effect**: discard σ̂w; jump to label of Cj.

### 2.3 suspend
**Phase**: control  
**OP-SUSPEND**: if U≠∅ then create a fresh hanger H, enqueue notes ⟨r,H⟩ for each r∈U, set goal Suspended, stop; else fail the goal.  
**Reactivation**: resumes at κ (clause-selection entry).

### 2.4 try_next_clause
**Operation**: Attempt next clause if current fails during selection phase  
**Behavior**:
- If current clause head fails to unify, discard σ̂w and try next clause
- If head unifies but guard fails, discard σ̂w and try next clause  
- No heap mutations to undo since binding is deferred
- Once any clause commits, this instruction becomes unreachable

### 2.5 no_more_clauses
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
- In READ mode: verify value at S against paired writer Xi in tentative state
- In WRITE mode: record reader constraint in σ̂w
- If writer Xi is unbound in tentative state: add to suspension set
- Increment S after operation

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

### 8.1 writer Xi
**Operation**: Process writer variable in structure  
**Behavior**:
- In READ mode: extract value at S into Xi
- In WRITE mode: create new writer variable, store at H and in Xi
- Increment S (READ) or H (WRITE)

### 8.2 reader Xi
**Operation**: Process reader variable in structure  
**Behavior**:
- In READ mode: verify against paired writer Xi
- In WRITE mode: create reader reference at H
- Add to suspension set if writer unbound
- Increment S (READ) or H (WRITE)

### 8.3 constant c
**Operation**: Process constant in structure  
**Behavior**:
- In READ mode: verify value at S equals c
- In WRITE mode: write constant c at H
- Increment S (READ) or H (WRITE)

### 8.4 void n
**Operation**: Process n anonymous variables  
**Behavior**:
- In READ mode: skip n positions (S += n)
- In WRITE mode: create n unbound variables (H += n)

## 9. Control Flow Instructions

### 9.1 spawn P/n
**Operation**: Spawn new process for procedure P with arity n  
**Behavior**:
- Save continuation pointer in CP
- Create new task in scheduler queue with fresh tail-recursion budget
- Transfer control to procedure P
- Arguments passed in A1-An
- Used for all body goals except the final one
- Ensures fair scheduling among concurrent goals

### 9.2 requeue P/n
**Operation**: Requeue current process with procedure P  
**Behavior**:
- Transfer control to procedure P without saving CP
- Reuse current process frame
- Decrement tail-recursion counter (initially 26)
- If counter > 0: Place at tail of microtask queue
- If counter = 0: Reset counter, schedule via event queue
- Used only for the final goal in a clause body
- Prevents unbounded queue growth while ensuring fairness

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

## 10. Suspension Management Instructions

### 10.1 reactivate X
**Operation**: Reactivate goals suspended on X  
**Behavior**:
- Find all goals suspended on reader X?
- Move from suspended to active queue in FIFO order
- Triggered when writer X receives value

### 10.2 abandon X
**Operation**: Mark variable X as abandoned  
**Behavior**:
- Set abandonment flag for X
- Immediately reactivate all goals suspended on X? in FIFO order
- After reactivation, clear X?'s suspension queue
- Reactivated goals will fail upon resumption due to abandonment

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
**Operation**: Test if X is ground (contains no variables)  
**Behavior**:
- Succeed if X contains no unbound variables
- Used to enable multiple reader occurrences
- Pure test, no side effects

### 11.3 known X
**Operation**: Test if X is not a variable  
**Behavior**:
- Succeed if X is not an unbound variable
- May contain variables internally
- Pure test operation

### 11.4 otherwise
**Operation**: Default guard  
**Behavior**:
- Succeeds if all previous clauses failed
- Used for catch-all clauses

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
**Operation**: Initialize argument position  
**Behavior**:
- Set argument register pointer to Xi
- Used before sequence of put instructions

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

### Format
Each instruction encoded as:
- **Opcode**: 1 byte (0-255)
- **Operands**: Variable length based on instruction type

### Operand Encoding
- **Registers**: 1 byte (0-127 for X registers, 128-255 for Y registers)
- **Functors**: 2 bytes (index into symbol table)
- **Constants**: Variable length with type tag
- **Labels**: 2 bytes (relative offset)
- **Arities**: 1 byte (0-255)

### Compact Forms
Frequently used instruction combinations can be fused:
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
1. Goal moved to suspension queue with suspension set
2. Suspension set contains all readers causing suspension
3. When any reader's writer receives value, goal reactivated
4. Reactivated goals re-attempt reduction

### SRSW Enforcement
The Single-Reader/Single-Writer constraint operates at two levels:
- **Compile time**: Each clause verified to contain at most one occurrence of any writer/reader
- **Runtime**: Variable table tracks usage across clauses/agents to detect dynamic violations

## 16. Memory Layout

### Heap Organization
- **Structures**: Functor cell followed by arguments
- **Lists**: Optimized two-cell representation
- **Writers**: Self-referential cells when unbound
- **Readers**: Reference to paired writer
- **Constants**: Direct storage with type tags

### Stack Frames
Environment frames contain:
- Previous environment pointer
- Continuation pointer
- Permanent variables (Y1-Yn)
- Suspension information

### Variable Table
Tracks shared variables:
- Variable ID
- Creator agent (for multiagent)
- Writer/Reader status
- Binding state
- Suspension list

## 17. Interaction with Runtime Structures
- **U** is the predicate-local union accumulated across SUSPEND_READY clauses only; FAIL does not contribute to U.
- **κ** is the clause-selection entry; suspend resumes at κ.
- All reactivations append to the tail of GQ; they are never executed inline.