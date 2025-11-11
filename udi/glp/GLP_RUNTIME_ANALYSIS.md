# GLP Runtime Codebase - Very Thorough Analysis

## 1. OVERALL ARCHITECTURE

### Directory Structure
```
/Users/udi/GLP/glp_runtime/
├── lib/
│   ├── bytecode/          # Bytecode VM and instruction set
│   │   ├── runner.dart    # Main execution engine (2415 lines)
│   │   ├── opcodes.dart   # v1 instruction definitions
│   │   ├── opcodes_v2.dart # v2 unified instruction set (Phase 2)
│   │   ├── asm.dart       # Assembly helpers for test/manual construction
│   │   └── program_migrator.dart
│   ├── compiler/          # GLP source to bytecode compilation
│   │   ├── compiler.dart  # Entry point (4-phase pipeline)
│   │   ├── lexer.dart     # Tokenization (handles reader syntax X?)
│   │   ├── parser.dart    # Syntax analysis (HEAD :- GUARD | BODY)
│   │   ├── analyzer.dart  # Semantic analysis (SRSW validation)
│   │   ├── codegen.dart   # Code generation to bytecode
│   │   ├── ast.dart       # AST node definitions
│   │   ├── token.dart     # Token types
│   │   ├── error.dart     # Compilation error handling
│   │   └── result.dart    # CompilationResult with metadata
│   ├── runtime/           # Execution environment and heap
│   │   ├── runtime.dart   # GlpRuntime: main runtime state
│   │   ├── machine_state.dart # GoalRef, GoalQueue, SigmaHat types
│   │   ├── heap.dart      # Dual writer/reader ID heap model
│   │   ├── heap_v2.dart   # Single-ID variable heap (Phase 2)
│   │   ├── heap_v2_adapter.dart # Backward-compatible adapter
│   │   ├── roq.dart       # Reader Queue (ROQueues)
│   │   ├── suspend.dart   # SuspensionNote structure
│   │   ├── hanger.dart    # Single-reactivation enforcer
│   │   ├── cells.dart     # WriterCell and ReaderCell
│   │   ├── commit.dart    # σ̂w application (CommitOps)
│   │   ├── abandon.dart   # Writer abandonment
│   │   ├── suspend_ops.dart # Suspension mechanics
│   │   ├── scheduler.dart # Goal queue drain loop
│   │   ├── fairness.dart  # Tail-recursion budget management
│   │   ├── terms.dart     # Term types (ConstTerm, StructTerm, etc.)
│   │   ├── system_predicates.dart # System predicate registry
│   │   ├── system_predicates_impl.dart # Builtin predicates
│   │   └── modules.dart   # Module loading framework
│   ├── lint/
│   │   └── linter.dart    # Syntax and semantic checking
│   ├── engine/
│   │   └── claude_adapter.dart # Interface for integration
│   └── glp_runtime.dart   # Library export
├── bin/                   # Executables (REPL, etc.)
├── test/
│   ├── bytecode/          # VM instruction tests
│   ├── custom/            # High-level algorithm tests
│   ├── refactoring/       # Phase migration tests
│   ├── conformance/       # Spec compliance tests
│   ├── lint/              # Linter tests
│   └── *_test.dart        # Debug/trace tests
└── pubspec.yaml
```

### Four-Phase Compilation Pipeline
From `compiler.dart:32-62` and execution model:

1. **Lexical Analysis** (`lexer.dart`)
   - Tokenizes GLP source code
   - Recognizes reader syntax: `X?` becomes READER token
   - Handles atoms, variables, operators (`:- | .`)

2. **Syntax Analysis** (`parser.dart`)
   - Parses token stream to AST
   - Clause structure: `Head :- Guards | Body.`
   - Builds Program → Procedures → Clauses → Terms

3. **Semantic Analysis** (`analyzer.dart`)
   - Validates SRSW: Single Reader Single Writer
   - Counts variable occurrences in each clause
   - Builds VariableTable with register assignments
   - Checks guard context (ground guards allow multiple reader occurrences)

4. **Code Generation** (`codegen.dart`)
   - Transforms annotated AST to bytecode ops
   - Allocates registers: argument slots (A0-A9), temp/clause vars (Y10+)
   - Emits labels and instructions for three phases

---

## 2. CORE COMPONENTS

### 2.1 Bytecode VM (runner.dart, opcodes.dart)

**BytecodeProgram** (runner.dart:17-28):
- `List<dynamic> ops`: Can hold both v1 Op and v2 OpV2 instructions
- `Map<LabelName, int> labels`: Auto-indexed from Label instructions
- Supports label lookup for jumps

**BytecodeRunner** (runner.dart:129-2415):
- `runWithStatus(RunnerContext cx)`: Main execution loop
- Processes single instruction stream for one goal until suspension/termination
- Returns: `terminated`, `suspended`, `yielded`, `outOfReductions`

#### Three-Phase Execution Model

**HEAD PHASE** (reader.dart:298-640+):
- Matches clause head pattern against goal arguments
- Writer variable: tentatively bind in σ̂w (if unbound)
- Reader variable: add to Si if writer unbound (blocks on binding)
- Constant: check equality
- Structure: enter READ/WRITE mode based on binding state

Key instructions:
- `HeadConstant`: Pattern match constant
- `HeadStructure`: Pattern match functor/arity, set S register and mode
- `HeadWriter`/`HeadReader`: Process structure fields
- `GetVariable`/`GetValue`: Load arguments to clause variables

**GUARD PHASE** (runner.dart:141-147 in codegen):
- Pure tests to refine clause selection
- `Ground(varIndex)`: Tests if variable is ground (no unbound readers)
- `Known(varIndex)`: Tests if variable is known
- `Otherwise`: Succeeds if Si empty (all clauses failed)
- `IfWriter`/`IfReader`: Type checks

Failed guards trigger soft-fail: union Si to U, jump to next clause

**BODY PHASE** (runner.dart:153-160 in codegen):
- Execute after Commit (σ̂w applied)
- Build new structures and spawn/requeue goals
- `PutConstant`/`PutStructure`: Create argument terms
- `SetWriter`/`SetReader`/`SetConstant`: Fill structure fields in WRITE mode
- `Spawn`: Create new goal with CallEnv
- `Requeue`: Tail call (update kappa, jump to procedure entry)
- `Proceed`: Goal completes successfully

#### v2.16 Instruction Set (44 main opcode types + variants)

**Control Flow** (runner.dart:1174-1313):
- `ClauseTry`: Initialize Si=∅, σ̂w=∅ at clause entry
- `Commit` (1174-1272): Apply σ̂w, enter BODY phase
  - If Si non-empty, skip commit and jump to next clause
  - Convert tentative structures (_TentativeStruct) to real StructTerms
  - Call CommitOps.applySigmaHatV216 to bind writers and wake readers
- `ClauseNext`: Move to next clause, union Si→U, clear state
- `TryNextClause`: Soft-fail from HEAD/GUARD
- `NoMoreClauses`: Check U to suspend or fail (1295-1313)
- `SuspendEnd`: Mark suspension point (deprecated with NoMoreClauses)
- `Proceed`: Goal succeeds

**Unified Structure Traversal**:
- `HeadConstant`, `HeadStructure`: Pattern matching
- `UnifyConstant`, `UnifyWriter`, `UnifyReader`: Element matching in READ mode
- `UnifyVoid`: Skip/wildcard (arity count)
- Mode tracking: `S` register (position), `mode` (read/write), `currentStructure`

**Argument Manipulation (BODY phase)**:
- `PutWriter(varIndex, argSlot)`: Place writer in A_i
- `PutReader(varIndex, argSlot)`: Derive and place reader in A_i
- `PutConstant(value, argSlot)`: Create bound fresh pair, place reader
- `PutStructure(functor, arity, argSlot)`: Allocate structure writer/reader
- `PutNil(argSlot)`, `PutList(argSlot)`: List optimizations

**Structure Building (BODY WRITE mode)**:
- `SetWriter(varIndex)`: Allocate fresh writer, store WriterTerm
- `SetReader(varIndex)`: Get reader from writer, store ReaderTerm
- `SetConstant(value)`: Store ConstTerm

**Goal Control**:
- `Spawn(label, arity)`: Create new goal, enqueue to GoalQueue
- `Requeue(label, arity)`: Tail call, update kappa, jump to entry PC
- `Execute(name)`: Call system predicate (Dart function)

**Guards**:
- `Ground(varIndex)`: Check if variable is ground
- `Known(varIndex)`: Check if variable is known (bound)
- `Otherwise`: Succeeds if Si empty
- `IfWriter`/`IfReader`: Type guards

**List Operations**:
- `HeadNil(argSlot)`: Match []
- `HeadList(argSlot)`: Match [H|T]
- Lists encoded as structures: `.` functor with (head, tail) args

### 2.2 Compiler (codegen.dart, analyzer.dart, parser.dart)

**Code Generation Pipeline** (codegen.dart:59-120):
```
Program AST 
  → Analyzer.analyze() → AnnotatedProgram (with VariableInfo)
    → CodeGenerator.generate() → BytecodeProgram
```

**CodeGenContext** (codegen.dart:12-55):
- `instructions`: Accumulator for bytecode ops
- `labels`: Map label name to PC
- `tempAllocation`: Fresh variable allocation
- `inHead`, `inGuard`, `inBody`: Phase tracking
- `seenHeadVars`: First-occurrence tracking (GetVariable vs GetValue)

**Clause Code Generation** (codegen.dart:121-164):
1. Emit `ClauseTry()` → initialize Si, σ̂w
2. Generate HEAD phase from head arguments
3. Generate GUARD phase (if present)
4. Emit `Commit()`
5. Generate BODY phase
6. Emit `Proceed()` or jump to next clause

**VariableTable** (analyzer.dart:44-104):
- Tracks writer/reader occurrences per variable
- Validates SRSW:
  - Writers: ≤1 occurrence
  - Readers: ≤1 occurrence (unless grounded by guard)
- Allocates registers: 0-9 for arguments, 10+ for temps/locals

**Analyzer** (analyzer.dart:148+):
- Walks AST, builds VariableInfo for each clause
- Records occurrence locations
- Validates ground guards allow multiple reader occurrences
- Assigns register indices

### 2.3 Runtime System

#### GlpRuntime (runtime.dart:16-162)

Central state machine:

```dart
class GlpRuntime {
  Heap heap;              // Writer/reader storage and bindings
  ROQueues roq;           // Reader suspension queues
  GoalQueue gq;           // Active goals to execute
  SystemPredicateRegistry systemPredicates;
  
  Map<GoalId, CallEnv> _goalEnvs;     // Arg mappings per goal
  Map<GoalId, int> _budgets;          // Tail-recursion budgets
  Map<GoalId, Object?> _goalPrograms; // Program per goal
  int nextGoalId;
}
```

Key methods:
- `commitWriters(writerIds)`: Apply σ̂w, wake readers via ROQ
- `abandonWriter(writerId)`: Mark writer as abandoned, wake readers
- `suspendGoal(goalId, kappa, readers)`: Create Hanger, enqueue SuspensionNotes
- `tailReduce(g)`: Fairness check on tail recursion budget
- File/library handle management for system predicates

#### Heap Models

**Original Heap** (heap.dart:4-50):
- Dual ID model: each variable has WriterId and ReaderId
- `Map<int, WriterCell>`: writer endpoint data
- `Map<int, ReaderCell>`: reader endpoint data
- `Map<int, Term> writerValue`: actual bindings (post-commit)
- Writer bindings determine if reader is bound

**HeapV2** (heap_v2.dart:9-160):
- Single-ID model: one ID per variable
- `Map<int, VariableCell>`: single cell per variable
- `Map<int, Set<int>> _roq`: reader suspension tracking
- Methods: `allocateFreshVar()`, `bindVariable(varId, Term)`, `dereference(Term)`
- FCP-style path compression optimization available

**HeapV2Adapter** (heap_v2_adapter.dart:16-160):
- Backward-compatible wrapper: presents Heap interface, uses HeapV2 internally
- Maps WriterId/ReaderId ↔ single varId
- Maintains bidirectional mappings
- Default in GlpRuntime (runtime.dart:38): `heap ?? HeapV2Adapter()`
- Syncs parent bindings to V2 when needed (especially REPL direct writes)

#### Suspension and Reactivation

**Suspension Mechanism** (suspend_ops.dart:6-18, hanger.dart):
1. When reader unbound and no more clauses: `suspendGoal()`
2. Creates `Hanger(goalId, kappa, armed=true)` 
3. Enqueues `SuspensionNote(readerId, hanger)` to each reader's queue in ROQueues
4. Goal returns `RunResult.suspended`

**ROQueues** (roq.dart:5-44):
- `Map<ReaderId, QueueList<SuspensionNote>>`: FIFO queues per reader
- `enqueue(readerId, note)`: Add suspension
- `processOnBind(readerId)`: Called when writer binds
  - FIFO order processing
  - For each note: if `hanger.armed=true`, flip to false, produce GoalRef
  - Returns list of GoalRef for reactivation
  - Single reactivation guaranteed (hanger armed flag)

**Reactivation** (commit.dart:48-62):
- In `Commit`: after binding writers, call `roq.processOnBind(readerId)`
- Woken goals enqueued to GoalQueue with kappa (clause selection PC)
- Scheduler drains queue: `Scheduler.drain(maxCycles=1000)`

#### Fairness and Budgets

**Tail Recursion Budget** (fairness.dart, machine_state.dart:24):
- `tailRecursionBudgetInit = 26`: Initial budget per goal
- Each tail call (Requeue) decrements budget
- When budget reaches 0: yield (requeue goal at low priority)
- `nextTailBudget(current)`: Returns `current - 1` (0 triggers yield)
- `resetTailBudget()`: Back to 26

**Scheduler** (scheduler.dart:63-137):
- `drain(maxCycles, debug)`: Main execution loop
- Dequeues goal, looks up runner, creates RunnerContext, executes
- Tracks spawned goals for trace output
- Supports debug callback for reduction/suspension tracing

#### Call Environment and Argument Passing

**CallEnv** (runner.dart:31-48):
- `Map<int, int> writerBySlot`: A0→WriterId, A1→WriterId, ...
- `Map<int, int> readerBySlot`: A0→ReaderId, A1→ReaderId, ...
- Methods: `w(slot)`, `r(slot)` to get writer/reader
- `update()` called on Requeue to set new arguments for tail call

**RunnerContext** (runner.dart:70-127):
- Per-goal execution state
- `goalId`: unique goal identifier
- `kappa`: procedure entry PC (restart point on suspension)
- `env`: CallEnv (argument mappings)
- `sigmaHat`: tentative writer bindings in current clause
- `si`: clause-local suspension set (unbound readers in this clause)
- `U`: union across clauses (accumulated suspension)
- `clauseVars`: local variable bindings during HEAD phase
- `argWriters`/`argReaders`: current argument registers (for Spawn/Requeue)
- `mode`/`S`: structure traversal state (READ vs WRITE, position)
- `currentStructure`: structure being traversed/built
- `E`: environment frame pointer (permanent variables Y registers)

### 2.4 System Predicates

**SystemPredicate** (system_predicates.dart:54-57):
```dart
typedef SystemPredicate = SystemResult Function(GlpRuntime rt, SystemCall call);
```

**Registry** (system_predicates.dart:63-79):
- `Map<String, SystemPredicate>`: predicate name → implementation
- `lookup(name)`: retrieve by name
- Registered during runtime initialization

**Implemented Predicates** (from README):
- Arithmetic: `evaluate/2`
- Utilities: `current_time/1`, `unique_id/1`, `variable_name/2`, `copy_term/2`
- File I/O: `file_read/2`, `file_write/2`, `file_exists/1`, `file_open/3`, `file_close/1`, `file_read_handle/2`, `file_write_handle/2`
- Directory: `directory_list/2`
- Terminal: `write/1`, `nl/0`, `read/1`
- Module Loading: `link/2`, `load_module/2`

**Execution** (runner.dart:262):
- `Execute(name)`: Call system predicate by name
- Marshals arguments, checks return result
- Can suspend if predicate returns `SystemResult.suspend`

---

## 3. KEY DATA STRUCTURES AND RELATIONSHIPS

### Term Hierarchy (terms.dart:1-35)

```
Term (abstract)
├── ConstTerm(value: Object?)      # Constants: atoms, numbers, null (=[])
├── StructTerm(functor, args)      # f(t1,...,tn)
├── ReaderTerm(readerId)           # R_i: reference to reader in structure
├── WriterTerm(writerId)           # W_i: reference to writer in structure
└── [HeapV2] VarRef(varId)         # V_i: single-ID variable reference
```

### Execution State Flow

```
Goal (id, kappa=entry_PC, env=CallEnv)
  ↓ enqueue to GoalQueue
  ↓ Scheduler dequeues, executes BytecodeRunner.runWithStatus(RunnerContext)
  ↓ RunnerContext tracks: sigmaHat, Si, U, clauseVars, argWriters/Readers
  ├─ HEAD: Match pattern, populate sigmaHat/Si, choose clause
  ├─ GUARD: Pure tests, may jump to next clause
  ├─ COMMIT: Apply sigmaHat, bind writers, wake readers via ROQ
  ├─ BODY: Build structures, spawn/requeue goals
  └─ Exit states:
     ├─ terminated: all clauses failed, U empty
     ├─ suspended: U non-empty, suspensions enqueued to ROQ
     ├─ yielded: tail budget exhausted, goal requeued
     └─ outOfReductions: reduction budget exceeded
```

### Variable Binding Process

1. **Writer W unbound** (in HEAD/GUARD):
   - Tentative binding in σ̂w map
   - Stored as native value or ConstTerm/StructTerm

2. **Reader R unbound** (in HEAD/GUARD):
   - Added to Si (clause-local suspension set)
   - If clause commits: Si→U accumulation
   - If no clause commits: U non-empty → suspend

3. **Commit** (after HEAD+GUARD succeed):
   - σ̂w applied: CommitOps.applySigmaHatV216()
   - Writers bound to heap.writerValue map
   - Writer cell's readerId found
   - ROQ.processOnBind(readerId) called
   - Woken goals: hanger.armed check → single reactivation

---

## 4. EXECUTION FLOW: FROM SOURCE TO RESULT

### Example: merge([1,2], [a,b], Zs) from GLP code

**Source:**
```glp
merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys, Zs).
```

**Compilation:**
1. Lexer: tokenize to ATOM(merge), LPAREN, LBRACKET, ...
2. Parser: build Procedure with 2 clauses
3. Analyzer: 
   - Clause 1: Ys writer occurs twice → SRSW violation? No, one is head match
   - Actually: first Ys is pattern match (reader context), second is result (writer context)
   - Register allocation: arg 0→A0, arg 1→A1, arg 2→A2
4. Codegen:
   - Clause 1 label: "merge/3"
   - ClauseTry → HeadNil(0) → HeadVariable(Ys,1) → HeadVariable(Ys,2) → Commit → Proceed
   - Clause 2: HeadList(0) → extract head/tail → HeadVariable(Ys,1) → ... → Spawn/Requeue

**Runtime Execution:**
1. Initialize: create writer/reader pairs for [1,2], [a,b], Zs
2. Goal: merge/3 entry at PC 0
3. Clause 1: HeadNil fails (arg0 not empty list)
4. Clause 2: HeadList matches [1,2]
   - Extract X=1, Xs=[2]
   - σ̂w += {writerId(result): ['[|]', WriterTerm(1), ...]}
5. Spawn: merge(Xs?, Ys?, Zs)
6. Commit: bind tentative structure to result
7. Proceed: return success
8. New goal for merge([2], [a,b], Zs) enqueued
9. Scheduler drains, continues merging recursively

### Three-Phase Execution Timeline for Single Clause

**Instruction execution sequence**:
```
PC=0: Label "merge/3_c1"
PC=1: ClauseTry          → Si=∅, σ̂w=∅
PC=2: HeadConstant([], 0) → if reader(arg0): Si.add(readerId)
PC=3: HeadConstant([], 1) → ...
PC=4: HeadConstant([], 2) → ...
PC=5: Commit            → if Si empty, apply σ̂w, bind writers, wake readers
                          else jump to next clause
PC=6-N: BODY (Put*/Set*/Spawn/Requeue)
PC=N+1: Proceed         → RunResult.terminated (success)
```

If any GUARD fails before Commit: soft-fail, jump to ClauseTry or NoMoreClauses

---

## 5. TEST COVERAGE

### Test Categories

**Bytecode VM Tests** (`test/bytecode/`):
- `asm_smoke_test.dart`: Assembly helper functionality
- `spec_control_flow_test.dart`: ClauseNext, NoMoreClauses, clause selection logic
- `commit_before_body_heap_test.dart`: σ̂w application timing
- `scheduler_suspend_wake_test.dart`: Suspension/reactivation mechanics
- `ground_instruction_test.dart`: Ground/Known guard tests
- `list_instructions_test.dart`: HeadNil, HeadList, list operations
- `union_end_to_end_test.dart`: Si→U accumulation across clauses
- `env_frame_test.dart`: Y register (permanent variable) environment frames
- `fairness_scheduler_loop_test.dart`: Tail budget and yielding
- `known_instruction_test.dart`: Known guard behavior
- `utility_instructions_test.dart`: Misc instruction tests

**Custom/Algorithm Tests** (`test/custom/`):
- `merge_test.dart`: Base merge algorithm (two streams to one)
- `metainterp_*_test.dart`: Metainterpreter tests (basic, with merges, circular patterns)
- `simple_metainterp_test.dart`: Basic run(X) test with clause database
- `system_predicates_test.dart`: File I/O, arithmetic, utilities
- `list_test.dart`: List unification and operations
- `p_q_*_test.dart`, `trace_*_test.dart`: Various two-goal patterns
- `interactive_test*.dart`: REPL-like interactive execution

**Refactoring/Migration Tests** (`test/refactoring/`):
- `heap_v2_integration_test.dart`: HeapV2 integration
- `heap_compatibility_test.dart`: V1 vs V2 heap equivalence
- `instruction_migration_test.dart`: v1 Op to v2 OpV2 migration
- `instruction_integration_v2_test.dart`: Unified instruction set

**Conformance Tests** (`test/conformance/`):
- Spec-level compliance checking

**Lint Tests** (`test/lint/`):
- `linter_ok_test.dart`: Valid GLP code
- `linter_suspend_once_test.dart`: SRSW violation detection
- `linter_body_precommit_test.dart`: Body phase restrictions

### What Tests Verify

1. **Three-phase execution correctness**: HEAD fails early, GUARD refines, BODY only after Commit
2. **SRSW enforcement**: Single reader/writer occurrence per clause variable
3. **Suspension/reactivation**: Goals blocked on unbound readers, woken when bound
4. **Fairness**: Tail-recursive goals yield after budget exhaustion
5. **Heap consistency**: Binding propagates correctly through writer/reader pairs
6. **Clause selection**: ClauseNext/NoMoreClauses control flow, Si→U union
7. **Argument passing**: Spawn/Requeue correctly marshal arguments to CallEnv
8. **Structure building**: PutStructure/SetWriter/SetReader create complex terms
9. **System predicates**: File I/O, arithmetic evaluation, utilities work
10. **Metainterpreting**: Program can query/execute other programs

---

## 6. SRSW (Single Reader Single Writer) ENFORCEMENT

### Design

**Single Writer**: Each clause variable can be written at most once
- Exception: Reader syntax X? creates paired writer, reader can occur multiple times
- Head position: pattern match is "reading" semantics
- Body position: creating structure is "writing" semantics

**Single Reader**: Each clause variable reader can occur at most once
- Exception: After ground guard, reader is "known" to be ground term, multiple occurrences allowed
- Enforced by VariableTable.verifySRSW() (analyzer.dart:75-97)

### Validation Points

1. **Analyzer** (analyzer.dart):
   - Walks all occurrences in clause
   - Counts writer vs reader occurrences
   - Tracks ground guards that allow multiple readers

2. **Linter** (lint/linter.dart):
   - Syntax-level checks before compilation
   - Rejects multi-writer/reader clauses

### Heap Consistency

SRSW prevents data races:
- Writer binding is write-once (no concurrent mutations)
- Reader sees consistent snapshot after writer binds
- Multiple readers on same writer → all see same value
- Single reader binding ensures no reader conflicts (each reader → different writer)

---

## 7. SUSPENSION AND RESUMPTION DETAILS

### Suspension Trigger (runner.dart:1295-1303)

```dart
if (op is NoMoreClauses) {
  if (cx.U.isNotEmpty) {
    cx.rt.suspendGoal(goalId: cx.goalId, kappa: cx.kappa, readers: cx.U);
    return RunResult.suspended;
  }
  // U empty → fail definitively
  return RunResult.terminated;
}
```

When U (accumulated suspension set across all clauses) non-empty:
- Goal created Hanger with armed=true
- SuspensionNotes enqueued to each reader's ROQ
- Goal exits immediately (no more execution)

### Resumption Trigger (commit.dart:10-44)

When writer W bound (in Commit):
```dart
final r = wc.readerId;                 // Get paired reader
final acts = roq.processOnBind(r);    // Wake suspended goals
```

ROQ.processOnBind processes FIFO queue for reader:
- For each SuspensionNote:
  - If hanger.armed=true: set armed=false, produce GoalRef
  - If armed=false: produce nothing (already woken)
- Single reactivation guaranteed (armed flag prevents double-wake)

Resume at: kappa (procedure entry, clause selection restart)

---

## 8. HEAP MANAGEMENT (V1 vs V2)

### Version 1: Dual-ID Model (heap.dart)

```
WriterCell {id: 1, readerId: 2}  ← Paired in heap
ReaderCell {id: 2}

writerValue[1] = ConstTerm('a')  ← Binding after commit
```

Advantages:
- Direct implementation of spec semantics
- Clear writer/reader distinction

### Version 2: Single-ID Model (heap_v2.dart)

```
VariableCell(varId=1) {value: ConstTerm('a')}
_roq[1] = {goal123, goal456}     ← Suspensions on this variable
```

Advantages:
- FCP design: one ID per variable
- Path compression optimization (dereference cache)
- Cleaner dereference logic (no separate writer lookup)

### Adapter (heap_v2_adapter.dart)

Default in GlpRuntime (runtime.dart:38):
- Presents Heap (V1) interface
- Uses HeapV2 internally
- Maintains ID mappings: WriterId↔VarId, ReaderId↔VarId
- Syncs bindings between layers (especially for REPL direct writes)

---

## 9. FILE AND CODE REFERENCES

### Critical Implementation Points

#### Execution Engine
- **runner.dart:186-2415**: Main runWithStatus loop
  - Lines 1174-1272: Commit instruction, σ̂w application
  - Lines 1278-1313: Clause control (ClauseNext, NoMoreClauses)
  - Lines 1683-1735: Spawn (create new goal)
  - Lines 1738-1800: Requeue (tail call)
  - Lines 1805-1818: GetVariable (load argument)

#### Compilation
- **compiler.dart:32-62**: Four-phase pipeline entry point
- **codegen.dart:121-164**: Clause code generation (HEAD/GUARD/BODY)
- **analyzer.dart:75-97**: SRSW validation

#### Runtime State
- **runtime.dart:43-51**: commitWriters (apply σ̂w, wake readers)
- **runtime.dart:63-74**: suspendGoal (create Hanger, enqueue)
- **machine_state.dart:4-55**: Type definitions (GoalRef, GoalQueue, etc.)

#### Suspension/Reactivation
- **suspend_ops.dart:6-18**: Create Hanger and enqueue
- **roq.dart:31-43**: processOnBind (FIFO wake with hanger guard)
- **hanger.dart:4-10**: Armed flag enforcement

#### Heap
- **heap_v2_adapter.dart:28-44**: allocateFreshPair (create writer/reader)
- **heap_v2_adapter.dart:97-120**: isWriterBound (with V2 sync check)
- **commit.dart:10-44**: applySigmaHatV216 (bind writers, wake readers)

---

## 10. INSTRUCTION CATEGORIES AND SEMANTICS

### Control Flow (19 instructions)
- **ClauseTry**: Entry, init Si=∅, σ̂w=∅
- **Commit**: Apply σ̂w, enter body, process ROQ
- **ClauseNext/NoMoreClauses**: Clause selection control
- **Proceed**: Goal success
- **SuspendEnd**: Legacy suspend point

### HEAD Phase (5 instructions)
- **HeadConstant**: Match constant pattern
- **HeadStructure**: Match structure, set mode/S
- **HeadWriter/HeadReader**: Extract structure arguments
- **HeadNil/HeadList**: List pattern matching

### GUARD Phase (6 instructions)
- **Ground**: Check variable is ground
- **Known**: Check variable is bound
- **Otherwise**: Succeed if Si empty
- **IfWriter/IfReader**: Type guards
- **Guard**: Call guard predicate

### BODY Phase (15+ instructions)
- **PutConstant/PutStructure**: Create argument terms
- **PutWriter/PutReader**: Place variable in argument
- **PutNil/PutList**: List construction
- **SetWriter/SetReader/SetConstant**: Fill structure fields
- **BodySetConst**: Bind writer post-commit

### Goal Control (2 instructions)
- **Spawn**: Create new goal
- **Requeue**: Tail call

### Variable Access (2 instructions)
- **GetVariable**: Load argument (first occurrence)
- **GetValue**: Load argument (subsequent)

### System Integration (1 instruction)
- **Execute**: Call system predicate

---

## SUMMARY

The GLP runtime is a well-structured, spec-compliant implementation of a concurrent logic programming language with:

1. **Multi-phase execution**: HEAD pattern matching, GUARD refinement, BODY structure building
2. **Principled suspension**: Single-reactivation guarantee via Hanger, FIFO ROQ
3. **SRSW compliance**: Enforced at compile-time (analyzer) and checked at lint-time
4. **Fair scheduling**: Tail-recursion budgets, yielding for fairness
5. **Heap consistency**: Writer-once semantics, atomic bindings via σ̂w
6. **Clean architecture**: Separated compiler pipeline, VM, runtime, and heap layers
7. **Comprehensive testing**: VM, algorithms, refactoring, compliance, linter

Total ~9000 LOC in lib/, ~2000 LOC in tests/, supporting full GLP specification including metainterpretation and concurrent goal execution.
