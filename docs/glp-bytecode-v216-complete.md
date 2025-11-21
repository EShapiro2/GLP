# GLP Bytecode Instruction Set Specification — v2.16 (Normative)

## 0. ISA Conventions
- **σ̂w** denotes the goal-local tentative writer substitution; it exists only during HEAD/GUARD phases and is discarded at clause_next.
- **U** denotes the goal-level suspension set (readers on which the goal is blocked).
- **Suspension model**: On first unbound reader encountered in HEAD/GUARD, add to U and immediately try next clause (no clause-local accumulation).
- **Phases per clause Ci**: HEAD_i ; GUARDS_i ; BODY_i.
- **Registers**: A (arguments), X (temporaries). Env stack E.
- **κ** denotes the clause-selection entry PC of the current procedure (the PC where the first clause of the procedure begins).

### Variable Object Model

GLP uses FCP's **two-cell variable system** with shared suspension records:

**Variable Objects (Two Cells per Variable)**:

1. **Writer Cell**:
   - Heap address (writerAddr)
   - Content: Pointer to reader cell OR bound value
   - Tag: RoTag when unbound, value tag when bound
   - Never updated after initial binding

2. **Reader Cell**:
   - Heap address (readerAddr)
   - Content: ONE of:
     - Back-pointer to writer (initial state, tag WrtTag)
     - Suspension list head (when processes waiting)
     - Bound value (after writer binds)
   - Content is REPLACED, not extended

**Suspension Records** (shared across variables):
- Lightweight objects with goalId, resumePC, next pointer
- Same record appears in multiple reader cells' suspension lists
- Activated once (first variable that binds), then disarmed (goalId nulled)

**Variable Lifecycle**:
1. **Allocate**: Create writer cell pointing to reader, reader pointing back to writer
2. **Suspend**: Prepend SuspensionRecord to reader cell (replacing back-pointer)
3. **Bind**: Dereference value, update both cells, walk/activate suspension list
4. **Activate**: Process walks suspension list, enqueues armed goals, disarms records

**Key Principle**: TWO heap cells per variable, suspension lists stored IN reader cells, shared records prevent double-activation.

**Dart Implementation**: Can use integer IDs mapping to (writerAddr, readerAddr) pairs. VarRef(varId, isReader: bool) distinguishes access mode, but both reference the same two-cell variable.

### Code Organization Hierarchy

There are three levels in the code organization:

1. **Module**: The complete bytecode program containing all procedures
2. **Procedure**: A named predicate (e.g., `p/1`, `merge/3`) consisting of all clauses with the same head functor/arity
3. **Clause**: A single rule within a procedure (head :- body)

**Key Principle**: Each goal maintains a κ value pointing to the entry PC of the procedure it is currently executing. When a goal suspends, it stores κ. On reactivation, execution resumes at PC = κ (the first clause of that procedure), NOT at the beginning of the module or at the suspension point.

## 0.5 Understanding Bytecode Output

When examining bytecode dumps, it's critical to understand what is being compiled:

**GLP Clause**: `qsort([], Rest?, Rest) :- true`
**Bytecode For**: The HEAD pattern `qsort([], Rest?, Rest)` and BODY `true`
**NOT For**: Any wrapper structures like `clause(...)` used in REPL or testing

Example bytecode for `qsort([], Rest?, Rest) :- true`:
```
PC 0: Label qsort/3
PC 1: ClauseTry
PC 2: HeadStructure qsort/3, A0    # Match qsort functor in argument A0
PC 3: UnifyConstant nil             # First arg: []
PC 4: UnifyReader X0                # Second arg: Rest? (reader)
PC 5: UnifyWriter X1                # Third arg: Rest (writer)
PC 6: Commit
PC 7: Proceed                       # Body: true (empty body)
```

The `clause/2` wrapper seen in some displays is metadata, not part of the compiled bytecode.

### ⚠️ Debug Display vs Actual Bytecode

Debug displays often show:
```
clause(head_pattern, body_pattern)
```

This is NOT what gets compiled. The bytecode only represents:
- HEAD: `head_pattern`
- BODY: `body_pattern`

Never expect bytecode instructions for the `clause/2` wrapper itself. The wrapper is a display convention for metainterpreters, not a structural element of the compiled clause.

## 1. Register Architecture

### Data Registers
- **A1-An**: Argument registers for passing parameters between procedures
- **X1-Xn**: Temporary registers for local computation within a clause
- **Y1-Yn**: Permanent registers stored in environment frames

### 1.1 Argument Register Semantics

**A1-An registers are heterogeneous term storage**, not just variable references. Each argument register can hold:

1. **Variable Reference**: `VarRef(varId, isReader: bool)` — reference to two-cell heap variable
   - `varId`: identifies the variable (maps to writer/reader cell pair in heap)
   - `isReader: false` → writer access mode (write-once, goal can bind)
   - `isReader: true` → reader access mode (read-only, suspends if unbound)
   - Same `varId` can appear in multiple arguments with different access modes
   - Enforces SRSW: variables occur as reader/writer PAIRS with exactly one writer AND one reader per clause (exception: ground guard allows multiple readers)
   - **Implementation**: Uses existing `VarRef` from `lib/runtime/terms.dart`

2. **Constant Term**: `ConstTerm(value)` — immediate value (atom, number, nil)
   - No heap allocation required
   - Stored directly in argument register
   - **Implementation**: Uses existing `ConstTerm` from `lib/runtime/terms.dart`

3. **Structure Term**: `StructTerm(functor, args)` — compound term with nested arguments
   - Built incrementally via put_structure + set_* instructions
   - Arguments can be variables, constants, or nested structures
   - **Implementation**: Uses existing `StructTerm` from `lib/runtime/terms.dart`

**Implementation Requirement**:
- Runtime must support `Map<int, Term>` for argument register storage
- `CallEnv` class must use `Map<int, Term> argBySlot` (not separate writer/reader ID maps)
- All Get* and Put* instructions operate on terms, not just variable IDs
- Argument passing via Spawn/Call must preserve term types
- **Existing Code**: `lib/runtime/terms.dart` defines `Term`, `VarRef`, `ConstTerm`, `StructTerm`

**WAM/FCP Alignment**: This matches the classical WAM design where argument registers are typed storage locations, and FCP's argument passing which supports arbitrary terms.

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
**Effect**: initialize σ̂w := ∅.

### 2.2 clause_next Cj
**Phase**: clause head/guards exit on FAIL.
**Effect**: discard σ̂w; jump to label of Cj.
**Purpose**: Try next clause after current clause fails.
**Note**: U accumulates readers directly from HEAD/GUARD instructions (no clause-local Si in v2.16+).

### 2.3 try_next_clause
**Status**: IMPLEMENTED but UNUSED - functionality overlaps with clause_next

**Implementation**: Has handler in runner.dart (line 967) that calls `_softFailToNextClause()` and jumps to next `clause_try`.

**Usage**: Never emitted by assembler or used in tests. The `clause_next` instruction (section 2.2) is preferred for all clause transitions.

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
**Timing**: occurs immediately after GUARDS phase, before first BODY instruction.
**Precondition**: HEAD and GUARD phases completed successfully. U may contain readers from previous failed clause attempts only (not from current successful clause).

**Effect** (FCP emulate.h do_commit1 lines 217-258):

1. For each binding `writerId → value` in σ̂w:

   a. **Dereference target by address** (FCP line 226 `deref_ptr(Pb)`):
      - Convert varId to address: `(wAddr,_) = varTable[varId]`
      - Follow pointers: `while(isPointer(cells[addr])) addr = cells[addr].targetAddr`
      - No reverse lookup - addresses followed directly like C pointers
      - Prevents W1009→R1014→nil chains

   b. **Get writer's paired reader cell**:
      - `readerAddr = writerCell.pointsTo`

   c. **Save reader's current content** (FCP line 301):
      - `suspensionList = heap[readerAddr]`
      - This is either: back-pointer (no suspensions), suspension list, or old bound value

   d. **Bind both cells** (FCP lines 233, 303):
      - `heap[writerAddr] = dereferencedValue`  // Writer now points to ultimate value
      - `heap[readerAddr] = dereferencedValue`  // Reader updated too

   e. **Walk saved suspension list** (FCP lines 245-254):
      - If suspensionList is a SuspensionRecord:
        - For each record in list:
          - If `record.armed` (goalId not null):
            - Enqueue `GoalRef(record.goalId, record.resumePC)` to goal queue
            - `record.disarm()` (set goalId = null)
          - Move to next record
        - Free suspension records (optional - GC will collect)

2. Clear σ̂w
3. Set `inBody = true` (enable heap mutations)
4. Control enters BODY_i

**Critical FCP line 233**: `*Pa = Ref_Word(Var_Val(*Pb))`
When binding W to target T where T is already bound, extract T's value and bind W to that value directly. This prevents variable chains and ensures all writers point to ground terms or unbound readers, never to bound intermediate readers.

**FCP Pointer Semantics**:
FCP uses raw memory pointers: `p = *p` to follow references.
We use array indices as addresses: `addr = cells[addr].targetAddr`.
Both avoid reverse lookups by following forward references only.

## 4. Environment and Phase Discipline (Instruction-Level)

### 4.1 Head and Guard opcodes
**Env effect**: E' = E. They may: read cells; record tentative writer bindings in σ̂w; add readers to U and fail to next clause; raise FAIL.
**They MUST NOT**: allocate or deallocate frames; mutate RO cells; perform I/O.

### 4.2 Body opcodes
Only BODY_i may contain:
- **allocate**: push a new environment frame.
- **deallocate**: pop environment frame.

## 5. Guard Purity
**Allowed primitives**: tag/status tests; equality/inequality on constants; structural equality on ground terms; integer comparisons on ground values.
A guard that demands an uninstantiated reader adds that reader to U, fails to next clause immediately; short-circuiting is only permitted when result is decidable independently of suspended subexpressions.

## 6. Head Processing Instructions

All head_* operations are **tentative**: they update the σ̂w (tentative writer substitution) and/or the suspension set U **without mutating heap cells** during clause try. Heap mutations happen only at **commit**.

**Key principle**: When a HEAD instruction encounters an unbound reader, it adds the reader to U and immediately fails to the next clause. The clause never reaches commit if any HEAD instruction suspends.

### ⚠️ CRITICAL: HEAD Matching vs Display Format

HEAD instructions match the actual clause head pattern, not any display wrapper:
- Clause: `merge([H|T], L, [H|R]) :- ...`
- HEAD matches: `merge/3` structure with its arguments
- NOT: `clause(merge(...), ...)` wrapper

When debugging bytecode:
1. Identify the actual GLP clause being compiled
2. Ignore display wrappers like `clause/2`
3. HEAD instructions correspond to the clause head pattern only

### 6.1 head_structure f/n, Ai
**Operation**: Process structure with functor f and arity n in argument register Ai
**Behavior**:
- Dereference the value in Ai
- If structure with matching functor: enter READ mode, set S to first argument
- If writer variable: record pending binding Ai = f(...) in σ̂w; no heap mutation during clause try
- If reader variable: add reader to U and fail to next clause
- Otherwise: fail to next clause

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
- If matching constant: succeed (continue to next instruction)
- If writer variable: record Ai = c in σ̂w
- If reader variable: add reader to U and fail to next clause
- Otherwise: fail to next clause

### 6.5 head_nil Ai
**Operation**: Match empty list [] with argument Ai
**Behavior**:
- Dereference value in Ai
- If value is []: succeed (continue to next instruction)
- If writer variable: record Ai = [] in σ̂w
- If reader variable: add reader to U and fail to next clause
- Otherwise: fail to next clause

### 6.6 head_list Ai
**Operation**: Process list structure [H|T] in argument Ai
**Behavior**:
- Dereference value in Ai
- If list structure [H|T]: enter READ mode, set S to first argument
- If writer variable: record pending binding Ai = [H|T] in σ̂w
- If reader variable: add reader to U and fail to next clause
- Otherwise: fail to next clause

## 7. Body Construction Instructions

All put_*/body construction instructions used for **goal spawning** are **heap-mutating**: they allocate and write structures/values to the heap as part of **body execution**.

**Exception**: `put_reader` and `put_writer` used for **guard argument setup** (in HEAD/GUARD phase) are **pure register loads** and do not mutate the heap. They simply copy variable references from clause variables (Xi) into argument registers (Ai) for guard evaluation.

### 7.1 put_structure f/n, Ai
**Operation**: Create structure with functor f/n in argument Ai
**Behavior**:
- Allocate structure header on heap at position H
- Store functor f/n at heap
- Build `StructTerm(f, args)` incrementally via subsequent set_* instructions
- **Runtime**: Must populate `CallEnv.argBySlot[i] = StructTerm(f, args)`
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
- Store `ConstTerm(c)` directly in argument register Ai
- **Runtime**: Must populate `CallEnv.argBySlot[i] = ConstTerm(c)`
- No heap allocation required for immediate constants

**Example**: `put_constant(42, A1)` → A1 contains `ConstTerm(42)`

### 7.5 put_nil Ai
**Operation**: Place empty list in argument Ai
**Behavior**:
- Store `ConstTerm(null)` in argument register Ai (nil represented as null)
- **Runtime**: Must populate `CallEnv.argBySlot[i] = ConstTerm(null)`
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
  - If unbound writer: FAIL (writer-to-writer binding prohibited by WxW)
  - If bound writer: use its value (including bound structures)
  - If reader with bound paired writer: dereference and use paired writer's value
  - If reader with unbound paired writer: store the reader term itself in Xi
- In WRITE mode:
  - If Xi is unbound (first use): allocate fresh variable ID (creates writer/reader pair), store `VarRef(newId, isReader: false)` at H and in clauseVars[i]
  - If Xi contains VarRef (subsequent use): extract varId from existing VarRef, create `VarRef(varId, isReader: false)`, store at H
    - Rationale: Under SRSW, varId identifies the variable; isReader specifies access mode (writer vs reader)
    - clauseVars[i] always stores the writer (base variable) regardless of whether first occurrence is reader or writer, allowing the subsequent occurrence (which will be in opposite mode per SRSW) to access the same variable
- Increment S (READ) or H (WRITE)

**Note**: Writer-to-writer unification follows Writer MGU semantics. In READ mode, this instruction extracts any term (including nested structures and reader terms) for later matching. When a reader term is extracted, subsequent operations (like head_structure on that clause variable) will handle suspension if the reader remains unbound at match time.

### 8.2 reader Xi
**Operation**: Process reader variable in structure
**Behavior**:
- In READ mode:
  - If value at S is reader R: verify R pairs with Xi
  - If value at S is unbound writer W:
    - If Xi not yet allocated (first occurrence): allocate fresh variable ID (creates writer/reader pair), bind W to VarRef(newId, isReader: true) in σ̂w, store VarRef(newId, isReader: false) in clauseVars[i]
    - If Xi already allocated: clauseVars[i] contains VarRef(writerId, isReader: false), bind W to VarRef(writerId, isReader: true) in σ̂w
  - If value at S is bound writer/constant: verify it equals Xi's paired writer value
  - If Xi's paired writer is unbound: add to U and immediately try next clause
- In WRITE mode:
  - If Xi is unbound (first use): allocate fresh variable ID (creates writer/reader pair), store `VarRef(newId, isReader: true)` at H, store `VarRef(newId, isReader: false)` in clauseVars[i]
  - If Xi contains VarRef (subsequent use): extract varId from existing VarRef, create `VarRef(varId, isReader: true)`, store at H
    - Rationale: Under SRSW, varId identifies the variable; isReader specifies access mode
    - clauseVars[i] always stores the writer (base variable) regardless of whether first occurrence is reader or writer, allowing the subsequent occurrence (which will be in opposite mode per SRSW) to access the same variable
  - If Xi contains ground term (ConstTerm/StructTerm): allocate fresh variable ID, bind it to the ground term, create `VarRef(newId, isReader: true)`, store at H
    - Rationale: When X is bound to constant 1 in HEAD and BODY needs X?, we create a fresh variable bound to 1 and return a reader to it
    - Example: In `qsort([X|Xs], S, [X?|S1?])`, after HEAD matches X=1, BODY builds `[X?|S1?]` by creating a fresh variable V bound to 1, storing `VarRef(V, isReader: true)` in the list
- Increment S (READ) or H (WRITE)

**Note**: Reader unification follows Writer MGU semantics - readers can only be read, not written

### 8.3 constant c
**Operation**: Process constant in structure
**Behavior**:
- In READ mode:
  - If value at S is constant c: succeed
  - If value at S is unbound writer W: bind W = c in σ̂w (writer MGU)
  - If value at S is reader R with unbound paired writer: add R to U and immediately try next clause
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

### 8.6 Push/Pop: Nested Structure State Management

When processing nested structures in HEAD mode (e.g., `p(f(X,Y))` where `f(X,Y)` is nested within the argument), the runtime must save and restore the structure processing state to properly handle the nesting.

**Structure Processing State**: The triple `(S, mode, currentStructure)` where:
- `S`: Current position within the structure being processed (integer index)
- `mode`: READ or WRITE mode for structure traversal
- `currentStructure`: Reference to the structure being processed
  - In READ mode: StructTerm from the heap
  - In WRITE mode: _TentativeStruct being built in σ̂w

**Instruction**: `push Xi`
**Operation**: Save current structure processing state
**Behavior**:
1. Create state object: `state = (S, mode, currentStructure)`
2. Store in clause variable: `clauseVars[i] = state`
3. Continue to next instruction (no other state changes)

**Instruction**: `pop Xi`
**Operation**: Restore previously saved structure processing state
**Behavior**:
1. Retrieve state: `state = clauseVars[i]`
2. Restore: `S = state.S`, `mode = state.mode`, `currentStructure = state.currentStructure`
3. Continue to next instruction

**Invariants**:
- Each `push` must have exactly one corresponding `pop`
- Push/pop pairs follow stack discipline (properly nested)
- State saved in clause variables survives across instruction boundaries
- After `pop`, S points to the position in the parent structure where we left off

**Following FCP AM**: This design directly follows the Flat Concurrent Prolog Abstract Machine's approach to nested structure handling, where the machine maintains a stack of structure processing contexts.

### 8.7 UnifyStructure: Nested Structure Processing

**Instruction**: `unify_structure f/n`
**Operation**: Process nested structure at current S position within parent structure
**Purpose**: Enter a nested structure for processing (FCP AM's `unify_compound`)

**READ Mode Behavior** (matching existing structure):
1. Get value at current position: `value = currentStructure.args[S]`
2. Check if value is StructTerm with functor `f` and arity `n`
3. If match:
   - Set `currentStructure = value` (enter the nested structure)
   - Set `S = 0` (ready to process first argument of nested structure)
   - Continue to next instruction
4. If mismatch:
   - Soft-fail to next clause (discard σ̂w, jump to next clause_try)

**WRITE Mode Behavior** (building new structure):
1. Create new tentative structure: `nested = _TentativeStruct(f, n, [null, ..., null])`
2. Place in parent structure: `currentStructure.args[S] = nested`
3. Set `currentStructure = nested` (enter the nested structure)
4. Set `S = 0` (ready to write first argument of nested structure)
5. Continue to next instruction

**Key Properties**:
- UnifyStructure does NOT increment S (parent's S remains unchanged)
- Pop does NOT increment S (restores parent's saved S value)
- After Pop, an explicit unify instruction (UnifyWriter Xi or UnifyVariable Xi) must follow to:
  1. Place the nested structure from register Xi at parent position S
  2. Increment S to the next sibling position
- This follows FCP AM design where Pop is always followed by unify_val
- Changes `currentStructure` to point to the nested structure
- Resets S to 0 to begin processing nested structure's arguments
- In WRITE mode, creates _TentativeStruct that will be converted to StructTerm at commit
- Follows three-valued unification: success (match), suspend (N/A for structures), fail (mismatch)

**Commit-Time Conversion**: When `commit` executes, all _TentativeStruct objects in σ̂w are recursively converted to StructTerm objects before being applied to the heap. This ensures nested structures are properly materialized.

### 8.8 Nested Structure Pattern

Nested structures in HEAD arguments use the Push/UnifyStructure/Pop pattern to maintain proper structure processing state across nesting levels.

**Pattern** (following FCP AM):
```
head_structure 'p', 1, A0         # Match outer structure p/1, enter it (S=0)
  push X10                        # Save (S=0, mode, p_struct) to X10
  unify_structure 'f', 2          # Enter nested f/2 at position S=0
    unify_writer X0               # Process f's first arg, S becomes 1
    unify_writer X1               # Process f's second arg, S becomes 2
  pop X10                         # Restore (S=0, mode, p_struct) from X10
                                  # X10 now contains the built f/2 structure
  unify_writer X10                # Place f/2 at S=0 and increment S to 1
                                  # NOW S=1 for any subsequent args of p/1
commit
```

**Concrete Example** - Matching `clause(qsort([X|Xs], Sorted, Rest), Body)`:
```
head_structure 'clause', 2, A0   # Match clause/2, S=0
  push X10                        # Save (S=0, mode, clause_struct) to X10
  unify_structure 'qsort', 3      # Enter qsort/3 at S=0
    push X11                      # Save (S=0, mode, qsort_struct) to X11
    unify_structure '.', 2        # Enter list at S=0
      unify_writer X0             # Match head X, S=1
      unify_writer X1             # Match tail Xs, S=2
    pop X11                       # Restore (S=0, mode, qsort_struct), X11=list
    unify_writer X11              # Place list at S=0, S=1
                                  # NOW S=1 (second arg of qsort)
    unify_writer X2               # Match Sorted at S=1, S=2
    unify_writer X3               # Match Rest at S=2, S=3
  pop X10                         # Restore (S=0, mode, clause_struct), X10=qsort
  unify_writer X10                # Place qsort at S=0, S=1
                                  # NOW S=1 (second arg of clause)
  unify_writer X4                 # Match Body at S=1, S=2
commit
```

**Why This Pattern is Necessary**:
- Without Push/Pop, entering a nested structure would lose track of position in parent
- The S register and currentStructure must be saved before entering nesting
- After processing nested structure, must restore parent context to continue
- Allows arbitrary nesting depth (limited only by clause variable space)

**Comparison to WAM**:
- WAM uses a separate approach with argument registers and structure mode
- GLP follows FCP AM which uses explicit state management
- This design is clearer for concurrent execution where structures may be incrementally built

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
- Clear all clause-local state (σ̂w, U, clauseVars, inBody flag)
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

**Argument Setup**: Before a guard instruction, use `put_reader` and/or `put_writer` to load guard arguments into argument registers. These operations are pure loads when used for guards (no heap mutation, no variable allocation). The guard handler reads arguments from the argument registers (argReaders/argWriters maps).

**Example**:
```
put_reader X2, A0    % Load A? into argument register 0
put_reader X0, A1    % Load X? into argument register 1
guard <, 2           % Evaluate A? < X? with 2 arguments
```

**Note**: Section 19 describes a future design using dedicated guard opcodes (e.g., `guard_less X0, X1`) that reference variable indices directly, eliminating the need for argument register setup. Until those are implemented, guards use the generic `guard P, arity` instruction with `put_reader`/`put_writer` for argument passing.

### 11.2 ground X
**Operation**: Succeeds if X is ground (contains no unbound variables)
**Three-valued semantics**:
1. If X is ground → **SUCCEED** (continue to next instruction, pc++)
2. If X contains unbound readers (but no unbound writers) → **SUSPEND** (add first unbound reader to U, immediately try next clause)
3. If X contains unbound writers → **FAIL** (soft-fail to next clause via clause_next)

**Rationale**: Due to SRSW restriction, unbound readers may become ground when their paired writers are bound, so suspension is appropriate. Unbound writers cannot be awaited (unknown future binding), so failure is definitive.

**Usage**: Enables multiple reader occurrences by testing groundness before use.

### 11.3 known X
**Operation**: Succeeds if X is not an unbound variable
**Three-valued semantics**:
1. If X is bound (to any value, including structures with variables) → **SUCCEED** (continue, pc++)
2. If X is an unbound reader → **SUSPEND** (add reader to U, immediately try next clause)
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

### 11.7 Arithmetic Guards (Planned)

**Implementation Status**: ⏳ Specified but not yet fully implemented

### Arithmetic and Guard Predicates

For comprehensive documentation on GLP's arithmetic system and guard predicates, see:

- **[glp-predicates-taxonomy.md](glp-predicates-taxonomy.md)** — Complete taxonomy of guard predicates, guard kernels, body kernels, and system predicates
- **[glp-arithmetic-spec.md](glp-arithmetic-spec.md)** — Arithmetic evaluation, `:=` system predicate, and guard vs body arithmetic
- **[body_kernels_spec.md](body_kernels_spec.md)** — Reference for 27 body kernel predicates

**Current Implementation**: `evaluate/2` system predicate (Dart-implemented) provides recursive arithmetic evaluation. See Section 18 below.

**Planned**: `:=` system predicate (GLP-defined) with body kernel support as specified in the documents above.
```

## 12. Mode-Aware Argument Loading (FCP-style)

**Version**: 2.16.1
**Status**: NORMATIVE
**Added**: November 2025 (replaces deprecated GetVariable/GetValue)

### 12.0 Overview

Mode-aware argument loading distinguishes between reader and writer modes at the bytecode level, enabling correct SRSW semantics and mode conversion. These opcodes replace the deprecated occurrence-based GetVariable/GetValue with explicit mode information.

The four opcodes handle all combinations of:
- **Occurrence**: First vs subsequent appearance of a variable
- **Mode**: Writer vs reader expected by the clause

**Design Principles**:

1. **Mode is determined by clause syntax**: `X` = writer, `X?` = reader
2. **Mode conversion happens during argument loading**: When argument mode differs from clause expectation
3. **Fresh variables enable reader views**: Allocating fresh variables provides isolation for reader semantics
4. **Three-valued unification**: Success, suspend (on unbound reader), or fail

### 12.0.1 Argument Term Types

**Get* instructions must handle all term types in argument registers** (per Section 1.1):

```
arg = CallEnv.getArg(slot)

if arg is VarRef(varId, isReader: bool):
    // Handle variable (writer or reader access mode)
    // Mode conversion may be needed

if arg is ConstTerm(value):
    // Handle constant - bind clause variable or verify match
    // No suspension possible (constants are always bound)

if arg is StructTerm(functor, args):
    // Handle structure - unify with clause pattern
    // May contain variables, constants, or nested structures
```

**GetWriterValue/GetReaderValue with constant arguments**:
- When argument is `ConstTerm(c)`: bind clause variable to constant in σ̂w
- When constant doesn't match expected value: soft-fail to next clause
- No suspension (constants are always bound)

**GetWriterValue/GetReaderValue with structure arguments**:
- When argument is `StructTerm(f, args)`: unify structure with clause variable
- Structure arguments may trigger recursive unification or mode conversion

---

### 12.1 get_writer_variable Xi, Ai

**Operation**: Load argument into clause writer variable (first occurrence)

**Syntax**: `get_writer_variable Xi, Ai`
- `Xi`: Clause variable register index
- `Ai`: Argument slot containing goal argument

**Behavior**: Stores argument value in clauseVars[Xi] for subsequent use.

**Execution Cases**:

**Case 1: Argument is writer (most common)**
```
If arg.isWriter:
  clauseVars[Xi] = arg.writerId
```
No mode conversion needed - direct storage of writer ID.

**Case 2: Argument is reader**
```
If arg.isReader:
  wid = heap.writerIdForReader(arg.readerId)
  If heap.isWriterBound(wid):
    value = heap.valueOfWriter(wid)
    clauseVars[Xi] = value  // Store dereferenced value
  Else:
    U.add(arg.readerId)     // Add to U and try next clause
    clause_next            // Immediately try next clause
```
Reader-to-writer requires the reader to be bound. If unbound, add to U and try next clause.

**Case 3: Argument is known term**
```
If arg.isKnown:
  clauseVars[Xi] = arg.knownTerm
```
Ground terms stored directly.

**Example**:
```prolog
% Clause: p(X, ...)
% Called: p(W1017, ...)  where W1017 is unbound writer
% Bytecode: get_writer_variable X0, A0
% Result: clauseVars[0] = 1017
```

---

### 12.2 get_reader_variable Xi, Ai

**Operation**: Load argument into clause reader variable (first occurrence)

**Syntax**: `get_reader_variable Xi, Ai`
- `Xi`: Clause variable register index
- `Ai`: Argument slot containing goal argument

**Behavior**: Implements mode conversion when needed, creating fresh variables for writer-to-reader conversion.

**Execution Cases**:

**Case 1: Argument is writer (requires mode conversion)**
```
If arg.isWriter:
  freshVar = heap.allocateFreshVar()
  heap.addVariable(freshVar)
  σ̂w[arg.writerId] = VarRef(freshVar, isReader: true)
  clauseVars[Xi] = freshVar
```

**Critical**: The fresh variable is allocated UNBOUND. It will be bound later if the clause body provides a value through subsequent GetWriterValue on the same variable.

**Case 2: Argument is reader**
```
If arg.isReader:
  clauseVars[Xi] = arg.readerId
```
Reader-to-reader needs no conversion.

**Case 3: Argument is known term**
```
If arg.isKnown:
  freshVar = heap.allocateFreshVar()
  heap.addVariable(freshVar)
  σ̂w[freshVar] = arg.knownTerm  // Bind fresh var to the term
  clauseVars[Xi] = freshVar
```
Known terms create a fresh variable bound to the term value.

**Example (Mode Conversion)**:
```prolog
% Clause: helper(X?, X)
% Called: helper(a, Y)  where a is ConstTerm('a'), Y is unbound writer
%
% PC 0: get_reader_variable X0, A0
%   - A0 contains ConstTerm('a')
%   - Allocate freshVar = 1000
%   - σ̂w[1000] = ConstTerm('a')
%   - clauseVars[0] = 1000
%
% PC 1: get_writer_value X0, A1
%   - Will unify Y with value from clauseVars[0]
```

---

### 12.3 get_writer_value Xi, Ai

**Operation**: Unify argument with clause writer variable (subsequent occurrence)

**Syntax**: `get_writer_value Xi, Ai`
- `Xi`: Clause variable register index
- `Ai`: Argument slot containing goal argument

**Precondition**: Variable Xi was previously loaded by GetWriterVariable or GetReaderVariable

**Behavior**: Performs writer MGU between stored value and argument.

**Execution Cases**:

**Case 1: Both are writer IDs**
```
storedValue = clauseVars[Xi]
arg = getArg(Ai)

If storedValue.isWriterId && arg.isWriter:
  If storedValue == arg.writerId:
    // Same writer - succeed (idempotent)
  Else:
    // Different writers - writer MGU
    If both unbound:
      σ̂w[arg.writerId] = VarRef(storedValue, isReader: false)
    Else if one bound:
      σ̂w[unbound] = boundValue
    Else:
      // Both bound - unify values
```

**Case 2: Stored is fresh variable from GetReaderVariable**
```
If storedValue.isFreshVar && arg.isWriter:
  // Check if fresh var was bound in σ̂w
  If σ̂w[storedValue] exists:
    value = σ̂w[storedValue]
    σ̂w[arg.writerId] = value
  Else:
    // Fresh var still unbound - bind writer to it
    σ̂w[arg.writerId] = VarRef(storedValue, isReader: false)
```

**Case 3: Argument is reader**
```
If arg.isReader:
  // Reader cannot be assigned - must be bound
  wid = heap.writerIdForReader(arg.readerId)
  value = heap.valueOfWriter(wid)
  // Unify with stored value (success/fail/suspend)
```

**Example (Completing Mode Conversion)**:
```prolog
% Continuing helper(a, Y) example:
% clauseVars[0] = 1000 (fresh var)
% σ̂w[1000] = ConstTerm('a')
%
% PC 1: get_writer_value X0, A1
%   - storedValue = 1000
%   - arg = Y (unbound writer)
%   - σ̂w[Y] = ConstTerm('a')  // Bind Y to value of fresh var
%
% At commit: Y gets bound to 'a'
```

---

### 12.4 get_reader_value Xi, Ai

**Operation**: Unify argument with clause reader variable (subsequent occurrence)

**Syntax**: `get_reader_value Xi, Ai`
- `Xi`: Clause variable register index
- `Ai`: Argument slot containing goal argument

**Precondition**: Variable Xi was previously loaded by GetReaderVariable

**Note**: Multiple reader occurrences are only legal with ground() guard per SRSW.

**Behavior**: Verifies reader consistency or establishes reader binding.

**Execution Cases**:

**Case 1: Both are readers**
```
storedValue = clauseVars[Xi]  // Reader ID
arg = getArg(Ai)              // Reader

If arg.isReader:
  If storedValue == arg.readerId:
    // Same reader - succeed
  Else:
    // Different readers - both must be bound to unify
    // (SRSW should prevent this without ground guard)
```

**Case 2: Argument is writer**
```
If arg.isWriter:
  // Reader expects consistency
  // Writer must match reader's bound value
  Perform three-valued unification
```

**Case 3: Argument is known term**
```
If arg.isKnown:
  // Reader must be bound to same value
  Check consistency or suspend
```

---

### 12.5 Mode Conversion Table

| Clause Expects | Arg Provides | First Occ (Variable) | Subsequent (Value) |
|---------------|-------------|---------------------|-------------------|
| Writer (X) | Writer | Direct store | Writer MGU |
| Writer (X) | Reader | Deref or suspend | Unify with bound value |
| Writer (X) | Known | Store term | Unify terms |
| Reader (X?) | Writer | Fresh var + σ̂w | Propagate binding |
| Reader (X?) | Reader | Direct store | Verify same |
| Reader (X?) | Known | Fresh var + bind | Verify value |

---

### 12.6 Interaction with Commit

At commit (Phase 1 → Phase 2 transition):
1. All bindings in σ̂w are applied atomically to the heap
2. Fresh variables allocated for mode conversion become real heap variables
3. VarRef(freshVar, isReader: true) bindings give writers reader access

**Critical**: Fresh variables enable the key semantic - a writer in the goal gets a reader view of a variable that the clause body will write to.

---

### 12.7 Example: Complete Mode Conversion

```prolog
% Clause: identity(X?, X).
% Called: identity(input, Output)
% Where: input = ConstTerm('data'), Output = unbound writer
```

**Compilation**:
```
0: ClauseTry
1: GetReaderVariable X0, A0    // X? - first occurrence
2: GetWriterValue X0, A1       // X - subsequent occurrence
3: Commit
4: Proceed
```

**Execution Trace**:
```
PC 1: GetReaderVariable X0, A0
  A0 = ConstTerm('data')
  Allocate freshVar = 2000
  σ̂w[2000] = ConstTerm('data')
  clauseVars[0] = 2000

PC 2: GetWriterValue X0, A1
  storedValue = 2000
  A1 = Output (unbound writer)
  σ̂w[Output] = ConstTerm('data')  // Via fresh var's binding

PC 3: Commit
  Apply σ̂w to heap
  Output now bound to 'data'
```

---

### 12.8 Deprecation Notice

The following opcodes are DEPRECATED and should not be used in new code:
- `GetVariable` - replaced by GetWriterVariable/GetReaderVariable
- `GetValue` - replaced by GetWriterValue/GetReaderValue

For backward compatibility, treat deprecated opcodes as writer-mode variants.

---

### 12.9 Implementation Notes

1. **Fresh Variable Identity**: Fresh variables are regular heap variables but allocated during HEAD phase. They become permanent at commit.

2. **Reader-of-Reader Prevention**: The mode conversion design prevents reader-of-reader chains. A writer gets a reader view of a fresh variable, not a reader of another reader.

3. **Suspension Semantics**: When suspending on an unbound reader, the entire clause attempt suspends. The goal will be reactivated when the reader's writer is bound.

4. **SRSW Validation**: The compiler must validate SRSW constraints. Multiple reader occurrences require ground() guard.

5. **Known Terms**: Terms passed as arguments (constants, structures) are treated as bound values for unification purposes.

---

## 13. System Instructions (Deprecated)

### 13.1 get_variable Xi, Ai
**Status**: DEPRECATED - Use get_writer_variable instead
**Operation**: Load argument into register
**Behavior**:
- Copy value from Ai to Xi
- First occurrence of variable in clause head
- When used during **head matching**, this records a **tentative association** in **σ̂w** (no heap mutation)

### 13.2 get_value Xi, Ai
**Status**: DEPRECATED - Use get_writer_value instead
**Operation**: Unify argument with register
**Behavior**:
- Perform writer MGU between Xi and Ai
- Subsequent occurrence of variable
- When used during **head matching**, this computes a **tentative writer MGU** and updates **σ̂w** (no heap mutation)

### 13.3 set Xi
**Status**: NOT IMPLEMENTED - reserved for future optimization
**Operation**: Initialize argument position
**Behavior**:
- Would set argument register pointer to Xi
- Would be used before sequence of put instructions
- Current implementation handles argument setup directly in put_* instructions

## 13.5 Common Bytecode Misunderstandings

### Mistaking Display Format for Bytecode Structure

**Wrong Understanding**:
"The clause `clause(qsort([], Rest?, Rest), true)` should compile to:
- HeadStructure clause/2
- Inner HeadStructure qsort/3"

**Correct Understanding**:
The `clause/2` wrapper is display metadata. The actual bytecode only compiles:
- `qsort([], Rest?, Rest)` as the HEAD
- `true` as the BODY

**Why This Matters**: When debugging with bytecode dumps, you must distinguish between:
1. **Display format** used by metainterpreters: `clause(head, body)`
2. **Actual GLP clause** being compiled: `head :- body`
3. **Generated bytecode** matching only the clause structure, not the display wrapper

### Confusing Argument Instructions with Structure Building

**Wrong**: "GetVariable at PC 2 should be HeadStructure"
**Right**: GetVariable/GetConstant load arguments for the current structure being matched

**Example**:
```
Clause: p(X, Y) :- ...
Bytecode:
  PC 0: GetVariable X0, A0    # Load first argument into X0
  PC 1: GetVariable X1, A1    # Load second argument into X1
```

GetVariable appears because the HEAD is simple (just a functor with variable arguments). HeadStructure only appears when matching against a compound structure in an argument position.

### Assuming All Structures Need HeadStructure

**Wrong**: "Every structure in the clause needs HeadStructure"
**Right**: Only structures in argument positions need HeadStructure. Arguments use Unify*/Get* instructions.

**Example**:
```
Clause: append([H|T], L, [H|R]) :- ...
HEAD bytecode:
  PC 0: HeadStructure append/3, A0   # Match main functor
  PC 1: HeadList A0                  # First arg is a list structure [H|T]
  PC 2: ...                          # Process list elements
```

The list `[H|T]` uses HeadList, not a nested HeadStructure, because lists have special instructions.

### Metainterpreter Clause Representation

When using a metainterpreter pattern like:
```prolog
clause(qsort([], Rest?, Rest), true).
```

This is a **data structure** representing GLP clauses for reflection, NOT the actual clause being compiled. The bytecode compiles the metainterpreter's clause head `clause/2`, not the nested qsort clause it represents.

**Actual compilation**:
- Input: `clause(qsort(...), true).` as a GLP clause
- HEAD: matches `clause/2` structure
- Arguments: `qsort(...)` and `true` are data terms

## 14. Utility Instructions

### 14.1 nop
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

## 15. Instruction Encoding

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

## 16. Execution Model

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
- **Compile time (syntactic restriction)**: Each variable occurs as a reader/writer
  PAIR with exactly one writer AND one reader per clause (unless ground guard allows
  multiple readers)
- **Runtime (invariant)**: No new occurrences created that violate SRSW

### WxW (No Writer-to-Writer Binding) Restriction

GLP prohibits writer-to-writer binding to ensure no readers are abandoned:
- If writers X and Y unified, their readers X? and Y? would have no writer to provide values
- Runtime must FAIL immediately on writer-to-writer unification attempts
- This is NOT a suspension case - it's a definitive failure

## 17. Memory Layout (Dart Implementation)

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

## 18. Interaction with Runtime Structures

### Suspension Model (v2.16+)
- **U**: Goal-level suspension set
- **Suspension behavior**: On encountering first unbound reader in HEAD/GUARD:
  1. Add reader to U immediately
  2. Execute clause_next to try next clause
  3. No clause-local accumulation (no Si)
- **clause_next**: Discards σ̂w and jumps to next clause
- **Important**: Goal suspends only after ALL clauses tried (U non-empty at `no_more_clauses`)
- **Simpler than old model**: No need to union clause-local sets - suspension goes directly to U

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

## 19. System Predicates (Execute Mechanism)

**Status**: FULLY IMPLEMENTED

System predicates are external Dart functions callable from GLP bytecode via the `execute` instruction. They follow three-valued semantics (success/suspend/failure) and properly handle unbound readers.

### 18.1 execute Predicate, Args
**Operation**: Call external system predicate
**Behavior**:
- Look up predicate by name in SystemPredicateRegistry
- Pass arguments as list of Terms
- Return SystemResult: success, suspend, or failure
- If suspend: adds first unbound reader to U and tries next clause

**Phase**: Can be used in guards (Phase 1) or body (Phase 3)
- In guards: pure test, no heap mutation allowed
- In body: may mutate heap after commit

### 18.2 Implemented System Predicates

**IMPORTANT**: System predicates called via execute/2 have **two-valued semantics** (success/abort), NOT three-valued (success/suspend/fail). They abort on unbound readers rather than suspending.

### Arithmetic System

**Current Implementation**: `evaluate/2` system predicate (Dart-implemented)

**Syntax**: `execute('evaluate', [Expression, Result])`

**Semantics**: **Two-valued** (success or abort)
- All operands bound numbers → recursively evaluate and unify result → SUCCESS
- Any operand unbound → ABORT "Unbound reader in arithmetic"
- Non-numeric operand → ABORT "Type error: expected number"
- Division by zero → ABORT "Arithmetic error: division by zero"

**Expression Format**: Prefix structure notation using functors:
- `+(X, Y)`, `-(X, Y)`, `*(X, Y)`, `/(X, Y)`, `mod(X, Y)`

**Example**:
```prolog
% UNSAFE - aborts if X or Y unbound
add(X, Y, Z) :- execute('evaluate', [X? + Y?, Z]).

% SAFE - guards ensure inputs bound
safe_add(X, Y, Z) :-
  number(X), number(Y) |
  execute('evaluate', [X? + Y?, Z]).
```

**Future Design**: For the planned `:=` system predicate and comprehensive arithmetic support, see:
- **[glp-arithmetic-spec.md](glp-arithmetic-spec.md)** — Complete arithmetic specification
- **[glp-predicates-taxonomy.md](glp-predicates-taxonomy.md)** — System predicates, body kernels, and guard predicates

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

---

## 20. Guard Predicates

**Status**: SPECIFICATION COMPLETE - Implementation pending

Guards provide read-only tests that determine clause selection. They appear between the HEAD and BODY phases, execute with three-valued semantics, and MUST NOT mutate heap state.

### 19.1 Guard Execution Model

**Phase**: Between HEAD and BODY (after HEAD unification, before COMMIT)
**Semantics**: Three-valued (SUCCESS/FAILURE/SUSPEND)
**Purity**: No heap mutations, no side effects, deterministic
**Expression Evaluation**: Guards may contain arithmetic expressions that are evaluated before comparison

**Execution Flow**:
1. HEAD phase completes, σ̂w built
2. Guards execute left-to-right
3. Guard SUCCESS: continue to next guard or COMMIT
4. Guard FAILURE: discard σ̂w, try next clause
5. Guard SUSPEND: add first unbound reader to U, discard σ̂w, try next clause
6. All guards succeed: COMMIT applies σ̂w, enter BODY

### 19.2 Arithmetic Expression Evaluation in Guards

Guards like `X? + 1 < Y? * 2` require expression evaluation:

1. **Recursively evaluate** arithmetic subexpressions (+, -, *, /, mod)
2. **If all operands ground**: compute numeric result
3. **If any operand contains unbound readers**: add first unbound reader to U, try next clause
4. **Apply comparison** to evaluated results

**Note**: This uses the same arithmetic evaluator as evaluate/2 but with three-valued semantics (can suspend).

**Example**:
```prolog
qsort([X|Xs], Sorted) :- X? < Pivot? | partition(X?, Xs?, Less, Greater), ...
```

If Pivot is unbound: guard adds Pivot's reader to U, tries next clause.

### 19.3 Arithmetic Comparison Guards

These guards compare numeric expressions with three-valued semantics.

#### 19.3.1 guard_less Xi, Xj
**Source**: `X < Y` in guard position
**Operation**: Evaluate Xi < Xj
**Behavior**:
- Evaluate expressions in Xi and Xj
- If both ground numbers: succeed if Xi < Xj, else fail
- If unbound readers in either: add first unbound reader to U, try next clause
- Type error (non-numeric): fail

**Compilation**:
```prolog
p(X, Y) :- X? < Y? | body.
```
→
```
get_variable X0, A0
get_variable X1, A1
guard_less X0, X1
commit
[body instructions]
```

#### 19.3.2 guard_greater Xi, Xj
**Source**: `X > Y`
**Operation**: Evaluate Xi > Xj
**Behavior**: As guard_less with inverted comparison

#### 19.3.3 guard_less_equal Xi, Xj
**Source**: `X =< Y` (Prolog syntax, not <=)
**Operation**: Evaluate Xi =< Xj
**Behavior**: As guard_less with inclusive comparison

#### 19.3.4 guard_greater_equal Xi, Xj
**Source**: `X >= Y`
**Operation**: Evaluate Xi >= Xj
**Behavior**: As guard_less with inclusive inverted comparison

#### 19.3.5 guard_arith_equal Xi, Xj
**Source**: `X =:= Y` (arithmetic equality)
**Operation**: Evaluate Xi =:= Xj
**Behavior**:
- Evaluates both sides as arithmetic expressions
- Compares numeric values for equality (IEEE 754 for floats)
- Suspends if either side contains unbound readers
- Type error if non-numeric: fail

#### 19.3.6 guard_arith_not_equal Xi, Xj
**Source**: `X =\= Y` (arithmetic inequality)
**Operation**: Evaluate Xi =\= Xj
**Behavior**: Negation of guard_arith_equal

**Note**: All six comparison guards follow the same pattern: evaluate expressions, compare if ground, suspend if unbound readers present.

**SRSW Implication**: When an arithmetic comparison guard **succeeds**, both operands are guaranteed to be ground (bound to numbers). This allows multiple reader occurrences of those variables in the clause body without violating SRSW, as ground values contain no unbound writers.

**Example**:
```prolog
% Multiple readers allowed after arithmetic guard
partition([X | Xs], Pivot, [X? | Smaller?], Larger?) :-
    X? < Pivot? |  % X and Pivot are ground after this succeeds
    partition(Xs?, Pivot?, Smaller, Larger).  % X? appears twice - OK!
```

### 19.4 Type Guards

These guards test term types and properties.

#### 19.4.1 guard_ground Xi
**Source**: `ground(X)` in guard position
**Operation**: Test if Xi is ground (contains no variables)
**Behavior**:
- Recursively check Xi for any unbound variables
- Succeed if fully ground (no variables)
- Suspend if contains unbound readers
- Fail if contains unbound writers

**Example**:
```prolog
safe_div(X, Y, Z?) :- ground(X), ground(Y), Y? =\= 0 | execute('evaluate', [X? / Y?, Z]).
```

#### 19.4.2 guard_known Xi
**Source**: `known(X)` in guard position
**Operation**: Test if Xi is not a variable
**Behavior**:
- Succeed if Xi is bound to any term (even if that term contains variables)
- Suspend if Xi is unbound reader
- Fail if Xi is unbound writer

**Difference from ground**: `known([X])` succeeds even if X is unbound, `ground([X])` fails.

#### 19.4.3 guard_integer Xi
**Source**: `integer(X)` in guard position
**Operation**: Test if Xi is an integer
**Behavior**:
- Succeed if Xi is bound to integer value
- Fail if Xi is bound to non-integer (including float)
- Suspend if Xi is unbound reader

#### 19.4.4 guard_number Xi
**Source**: `number(X)` in guard position
**Operation**: Test if Xi is numeric (integer or real)
**Behavior**: As guard_integer but accepts any numeric type (int or float)

#### 19.4.5 guard_writer Xi
**Source**: `writer(X)` in guard position
**Operation**: Test if Xi is an unbound writer
**Behavior**:
- Succeed if Xi is unbound writer variable
- Fail otherwise
- **Non-monotonic**: can succeed then fail after binding

#### 19.4.6 guard_reader Xi
**Source**: `reader(X)` in guard position
**Operation**: Test if Xi is an unbound reader
**Behavior**:
- Succeed if Xi is unbound reader variable
- Fail otherwise
- **Non-monotonic**: can succeed then fail after paired writer binds

### 19.5 Control Guards

#### 19.5.1 guard_true
**Source**: `true` in guard position
**Operation**: Always succeeds
**Behavior**: Unconditional success (no-op guard)

#### 19.5.2 guard_otherwise
**Source**: `otherwise` in guard position
**Operation**: Succeeds if all previous clauses failed
**Compiler directive**: Compiler must track clause ordering
**Behavior**: Success if reached, typically used in last clause

**Example**:
```prolog
classify(X, pos) :- X? > 0 | true.
classify(X, neg) :- X? < 0 | true.
classify(X, zero) :- otherwise | true.
```

### 19.6 Unification Guards

#### 19.6.1 guard_unify Xi, Xj
**Source**: `X = Y` in guard position
**Operation**: Attempt unification
**Behavior**:
- Perform tentative writer MGU (add bindings to σ̂w)
- Success if unifiable
- Fail if not unifiable (structure mismatch)
- Suspend if unbound readers block unification

**Example**:
```prolog
p(X, Y) :- X = f(Y, Z?) | body.  % Adds X=f(Y,Z) to σ̂w if succeeds
```

#### 19.6.2 guard_not_unifiable Xi, Xj
**Source**: `X \= Y` in guard position
**Operation**: Test non-unifiability
**Behavior**: Negation of guard_unify (no bindings added to σ̂w)

### 19.7 Lexer/Parser Integration

#### Token Definitions

| Source | Token Type    | Priority | Associativity | Bytecode Instruction      |
|--------|---------------|----------|---------------|---------------------------|
| `<`    | LESS          | 700      | non-assoc     | guard_less                |
| `>`    | GREATER       | 700      | non-assoc     | guard_greater             |
| `=<`   | LESS_EQ       | 700      | non-assoc     | guard_less_equal          |
| `>=`   | GREATER_EQ    | 700      | non-assoc     | guard_greater_equal       |
| `=:=`  | ARITH_EQ      | 700      | non-assoc     | guard_arith_equal         |
| `=\=`  | ARITH_NE      | 700      | non-assoc     | guard_arith_not_equal     |
| `=`    | UNIFY         | 800      | non-assoc     | guard_unify (in guards)   |
| `\=`   | NOT_UNIFY     | 800      | non-assoc     | guard_not_unifiable       |

#### Operator Precedence (from lowest to highest)

1. **Guard separator**: `|` - 1100
2. **Conjunction**: `,` - 1000
3. **Unification**: `=`, `\=` - 800
4. **Comparison**: `<`, `>`, `=<`, `>=`, `=:=`, `=\=` - 700
5. **Addition**: `+`, `-` - 500
6. **Multiplication**: `*`, `/`, `mod` - 400
7. **Primary**: variables, numbers, parentheses - highest

#### Lexer Rules

```
// IMPORTANT: Check multi-character operators FIRST

// Two-character operators
'=<'   → LESS_EQ        // Prolog style (not <=)
'>='   → GREATER_EQ
'\\='  → NOT_UNIFY      // Backslash + equals

// Three-character operators
'=:='  → ARITH_EQ       // Arithmetic equality
'=\\=' → ARITH_NE       // Arithmetic inequality (backslash)

// Single-character operators (check AFTER multi-char)
'<'    → LESS
'>'    → GREATER
'='    → UNIFY
```

**Ordering Critical**: Lexer must check `=<` before `<`, `=:=` before `=`, etc.

### 19.8 Guards vs. System Predicates

**Key Distinction**: Guards are three-valued built-in tests; system predicates are two-valued external functions.

| Aspect               | Guards                                  | System Predicates                |
|----------------------|-----------------------------------------|----------------------------------|
| **Examples**         | `guard_less`, `guard_ground`            | `evaluate/2`, `write/1`          |
| **Semantics**        | Three-valued (SUCCESS/FAIL/SUSPEND)     | Two-valued (SUCCESS/ABORT)       |
| **Phase**            | Before COMMIT (guards phase)            | After COMMIT (body phase)        |
| **Heap Access**      | Read-only                               | Read/Write                       |
| **σ̂w Access**        | Read-only                               | N/A (already committed)          |
| **Side Effects**     | Forbidden (pure)                        | Allowed (I/O, arithmetic)        |
| **Purpose**          | Clause selection                        | Computation/IO                   |
| **Suspension**       | On unbound readers (three-valued)       | Abort on unbound readers         |
| **Instruction Type** | Built-in bytecode instructions          | External function calls          |

### 19.9 Compilation Example

**Source**:
```prolog
qsort([Pivot|Rest], Sorted) :-
    Pivot? < 100, known(Rest) |
    partition(Pivot?, Rest?, Less, Greater),
    qsort(Less?, SortedLess),
    qsort(Greater?, SortedGreater),
    append(SortedLess?, [Pivot|SortedGreater?]?, Sorted).
```

**Compiled Bytecode**:
```
clause_try qsort/2, 0       % Start first clause
head_cons A0                % Match [Pivot|Rest]
get_variable X0, A0_head    % Pivot (from list head)
get_variable X1, A0_tail    % Rest (from list tail)
get_variable X2, A1         % Sorted

% Guards (before COMMIT)
guard_less X0, 100          % Pivot? < 100 (suspends if Pivot unbound)
guard_known X1              % known(Rest) (suspends if Rest unbound)

commit                      % Apply σ̂w, enter BODY

% Body instructions
put_reader X0               % Pivot? for partition arg
put_reader X1               % Rest? for partition arg
put_writer X3               % Less
put_writer X4               % Greater
spawn partition/4

put_reader X3               % Less? for recursive qsort
put_writer X5               % SortedLess
spawn qsort/2

put_reader X4               % Greater? for recursive qsort
put_writer X6               % SortedGreater
spawn qsort/2

put_reader X5               % SortedLess?
put_structure [Pivot|...]   % Build [Pivot|SortedGreater?]
put_writer X2               % Sorted
spawn append/3

proceed
```

### 19.10 Implementation Requirements

Guards must satisfy these requirements:

1. **Purity**: No heap mutations, no side effects, deterministic
2. **Expression Evaluation**: Handle mixed int/real arithmetic per IEEE 754
3. **Suspension Tracking**: Properly track all unbound readers encountered
4. **Guard Failure**: Discard σ̂w and try next clause
5. **Left-to-Right**: Guards evaluate in order, short-circuit on failure/suspension
6. **Type Coercion**: Follow Prolog conventions (integer + real = real)
7. **Canonical Ordering**: For term comparison: numbers < atoms < strings < lists < structures

### 19.11 Implementation Status

**Status**: SPECIFICATION COMPLETE

**Implementation Phases**:
1. ✅ Specification written (this section)
2. ⏳ Lexer tokens (comparison operators)
3. ⏳ Parser support (guard position, precedence)
4. ⏳ Bytecode instructions (guard_less, guard_greater, etc.)
5. ⏳ Runtime implementation (expression evaluation, three-valued logic)
6. ⏳ Testing (unit tests, integration tests)

**See Also**:
- Section 11: Existing guard instructions (ground, known, if_writer, if_reader)
- Section 18: System predicates (execute mechanism)
- parser-spec.md: Parser implementation details