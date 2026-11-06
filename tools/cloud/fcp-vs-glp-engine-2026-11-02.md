# The FCP abstract machine and compiler against the GLP engine

Cloud #1, 2026-11-02.  Read: the EFCP emulator (`EFCP/Efcp-release/Logix/EMULATOR`: `fcp.h`, `macros.h`, `emulate.h`, `opcodes.h`, `emulate.c`, `unify.c`), Kliger's compiler (`CheckedOut/Logix/system/ndg`: phase0 `preproc`, phase1 `decision_graph`, `indexing_function`, `compile_node`, `tell`, `body`, phase2 `ctl`, `case_psi`, `ask_guard`, the assembler's instruction grammar); IGLP's Implementation Notes (heap, clause try, event-driven execution) and Code Format (instruction table); GLP's `glp_runtime` (`heap_fcp.dart`, `runtime.dart`, `scheduler.dart`, `runner.dart`, `interp.dart`, `commit.dart`, `suspend_ops.dart`, `codegen.dart`, `opcodes.dart`).  The papers (Houri and Shapiro 1987; Kliger and Shapiro 1988; Kliger 1992) are not in the FCP repository; this reads the code.

## 1. The FCP machine, as the emulator has it

**Words and heap.**  One heap, an array of tagged 32-bit words.  Tags: reference, writable variable, read-only variable, integer, real, string, nil, list (a flag on a reference, integer or nil), tuple, vector.  A term's arguments are the words after its header; a sub-argument is reached by address (`deref_subarg`).  Strings and modules are heap objects with a header.

**Variables.**  A writable variable is one word; unbound, it is "zeroed" (points to itself).  A read-only variable is a word pointing at its writable.  A suspension queue hangs off the variable's value field: a list of suspension records, each a pointer to a process record.  Binding a writable writes the value into its word.

**Worker state (`workerT`).**  `X[4096]` temporary registers; `HP` heap top; `CP` the current process record; `HB` the heap top at the start of the current clause attempt; the assignment trail (`AsgnTrl`, tentative bindings of writables made during head and guard), the change trail (destructive sharing of lists and tuples made by `unify`); the suspension table (`SusTbl`, the variables the attempt met unbound); the error table; the active queue `QF`/`QB` with a high-priority back `HQB`; the suspended queue; free lists of process records by size; the time slice `TS` (500 reductions).

**Process record.**  A heap vector: header, procedure code pointer, next, then the arguments.  At activation the arguments are loaded into `X[1..n]` (`load_pr_args`); at suspension or at the end of a time slice they are stored back (`store_pr_args`).  `set_cp_arity` re-sizes the record in place for a tail call of another arity.

**Clause attempt.**  Code is a decision graph, not a clause list: `deref` an argument once into a register, then `switch_on_tag`, `if_not_*`, `branch_integer`, `case_hash_string`, `if_int_*` on it.  A head binding of a writable is a real heap write, trailed (`asgn`).  A variable the attempt needs bound goes into the suspension table (`suspend_on`, `if_var_suspend`, `unify` on a read-only).  The attempt ends in `suspend0/1/2`: `end_of_branch` restores `HP = HB`, undoes both trails, and either branches to the next clause or, at the last, suspends the process when the table is non-empty and fails it when empty.  Suspension (`suspend_pr_label`): one suspension record for the process, hung on every variable in the table.

**Commit (`commit1`).**  Walks the assignment trail without undoing it: a read-only variable that got a value has its queue activated (`pr_activate` enqueues each record's process and empties the queue).  `Reductions++`.

**Body.**  `allocate_pr`/`alloc_pr_enqu` take a record of the right size from the free list, the `copy_*` instructions fill its arguments from registers or sub-arguments, `enqueue` links it on the queue.  The last goal is `execute`/`iterate`: the current record is reused, its arguments are in the registers, the heap check and the time slice are tested, and control jumps to the callee's code.  `halt` returns the record to the free list.

**Guards.**  Kernels are opcodes (`0x140` onward) in the same switch with three exits: succeeded, suspended, failed, each a branch offset in the instruction.  The general `unify` is a tag-pair table; `ask_unify` collects the variables it would have bound into the suspension table and undoes the trail.

**Compiler.**  Phase 0 rewrites each clause into {Ask, Tell, Body}: head unification is unfolded into ask tests on argument paths (`psi`) and post-commit tells; clauses are renamed apart.  Phase 1 builds the decision graph: the indexing function chooses the path to case on (`minvariability` by default), `case(value(psi), [type and value branches..., ow])` with the residual clauses compiled recursively under each branch; a guard becomes `case(Guard, [true, false, ow])`.  Phase 2 emits CTL instructions with a dictionary of what is known about each path (its register, whether dereferenced, its type), so an argument is dereferenced once and tested by tag; a peephole pass follows; the assembler resolves labels to byte offsets.

## 2. The GLP engine, as `glp_runtime` has it

**Heap.**  Dart objects: `HeapCell` {content, tag, id, pairedReader, cached `VarRef`}; a variable pair is two cells pointing at each other; a bound writer is a value cell; suspensions hang on the writer in a `WriterContent`.  Terms are Dart objects: `ConstTerm`, `StructTerm` with a `List<Term>`, `VarRef`.  Dereference follows pointers and compresses the starting writer.  Collection is Dart's.  This is IGLP's Implementation Notes, "Heap", and it follows FCP as the note says.

**Goal.**  A `GoalRef(id, pc)` in the queue; a `GoalRecord` {`env`: `CallEnv`, a `Map<int, Term>` of the arguments; program; module; budget; wait reader}.  Each run of a goal makes a `RunnerContext`: `sigmaHat` map, `Si` set, `U` set, `unknownVars` set, `unknownPlaceholders` map, `unknownKeys` set, `writerPairs` list, `clauseVars` map (the clause's variables by index), `parentStack` list, `argSlots` map (the arguments of the goal being built), `spawnedGoals` list, and a formatter closure.

**Clause attempt.**  Clauses are a sequence: `clause_try`, the head instructions, the guards, `commit`, the body, and `clause_next` to the next; `no_more_clauses` at the end.  Every clause's head is executed in full against the goal, each dereferencing the arguments again.  A static index exists for procedures of six or more clauses with four or more first-level structure keys on one argument (`_clauseKeys`).  Head bindings of goal writers go into `sigmaHat` (never into the heap); a structure met by a goal writer is built as a `_TentativeStruct` of clause-variable placeholders.  A reader met unbound goes into `Si`; a mismatch fails the clause; `_applyNextClauseByte` adds `Si` to `U` when the clause suspended and clears the maps.  At `no_more_clauses` the goal suspends on `U` with one shared `SuspensionRecord` on every reader's writer, as FCP does.

**Commit.**  `execCommit` converts the tentative structures to `StructTerm`s, checks writer-to-writer, then `applySigmaHatFCP` binds each writer (collecting the activations), re-dereferences chains, fires callbacks; the activations are enqueued.

**Body.**  `put_*` fill `argSlots`; `spawn` copies `argSlots` into a new `CallEnv` map, makes a `GoalRecord`, enqueues; `requeue` continues the tail goal in line under the bounded-tail budget, as IGLP's "Event-driven execution" says.  A codeless symbol is a body kernel looked up by name and arity.

**Guards.**  `guard proc, arity` calls a guard by symbol; `ground`, `known`, `unknown`, `no_readers`, `ground_equal` collect the variables of a term into a set (`_termVariables`) and decide; `otherwise` tests `U`.

## 3. Where GLP deviates from the FCP machine

Each item: the FCP design, the GLP design, what the profile says it costs (the sGLP social graph, 100 agents; fib; the plays), and whether following FCP touches a paper.

1. **Goal arguments and registers.**  FCP: the process record is a vector of argument words; at activation they are loaded into the worker's X registers by index.  GLP: `CallEnv.argBySlot` is a `Map<int, Term>` made per goal, `argSlots` a map per run, `clauseVars` a map per run.  Cost: the hash containers are a third of the engine's time; `put_variable` alone is 11 to 16% of a run, nearly all map insertion.  Paper: IGLP says nothing about how registers are held; none.

2. **Per-run state.**  FCP: one worker state, reset per attempt by pointer resets (`HB = HP`, the trail and table pointers).  GLP: a new `RunnerContext` with eleven containers and a closure for every goal run.  Cost: `RunnerContext.RunnerContext` 2 to 4%, plus the containers' first insertions.  Paper: none.

3. **Tentative bindings.**  FCP: a head binding is written into the heap and trailed; a failed or suspended attempt restores the heap top and undoes the trail (`end_of_branch`); commit walks the trail to wake queues.  GLP: `sigmaHat` map plus tentative structures, converted and applied at commit.  Both leave no trace of a failed clause; FCP's is a store and a trail push per binding.  Paper: IGLP, "Clause try": "The tentative substitution reaches the heap only at commit".  Following FCP changes that sentence to "is written to the heap and trailed, and undone unless the clause commits"; observable behaviour is the same, the engine being sequential within an attempt.  **Needs discussion.**

4. **Clause selection.**  FCP: a decision graph, the argument dereferenced once and switched on by tag, functor and value.  GLP: every clause tried in sequence, the arguments dereferenced in each; the static key index covers only wide procedures.  Cost: `execGuard` and the head instructions re-dereferencing; the 40-clause `:=` was the case that got the index.  Paper: the Code Format's instruction table (IGLP) has no switch instruction; a decision-graph compiler needs `switch_on_tag`, `switch_on_functor`, `switch_on_constant` or their like, and the compiler phase that builds the graph must respect GLP's term-matching table (writer and reader columns), which FCP's head unification does not have.  **Needs discussion: a Code Format change and a compiler phase.**

5. **Guards.**  FCP: kernels are opcodes with three branch exits; type tests compile to `if_not_*` on a dereferenced register.  GLP: `guard` by symbol with runtime dispatch; the variable-set guards build a set per call.  Cost: `_termVariables` and `_readersOfGoal` 3 to 5%.  Paper: the Code Format reserves `0x47` to `0x4F` for the comparison guards; type guards as instructions would be more.  **Needs discussion if taken beyond the reserved range.**

6. **Suspension bookkeeping.**  FCP: the suspension record on the variables' queues and nothing else.  GLP: the same record, plus the runtime's `suspended` map from reader to goals, kept for reporting and removed from at every goal taken (`_removeFromSuspended`, 1.5%).  Paper: none; the REPL's "blocking readers" report reads that map.

7. **Tail call.**  FCP: `iterate`/`execute` reuse the process record, the time slice decides when to yield.  GLP: `requeue` in line under the bounded-tail budget.  Same design; the record reuse is item 1's.

8. **Body-kernel lookup.**  FCP: kernels are opcodes.  GLP: a name-and-arity lookup per spawn of a codeless symbol (now a two-level map).  Small.

9. **Terms.**  FCP: arguments in the heap, reached by address.  GLP: Dart lists in `StructTerm`, copied when a tentative structure is converted.  Host-language difference; the copying is item 3's.

## 4. What I propose, in order

**Stage A, the machine, no paper text changed:** items 1, 2, 6.  The worker's registers as arrays reused across goals; the goal's arguments as a list in its record, loaded into the argument registers at activation as `load_pr_args` does; the clause variables as a register array; one runner context per runner, reset per attempt.  The suspended-goals map kept only as the report needs it.  Gate as before: outputs identical, the REPL's suspended list and blocking readers included.

**Stage B, needs Udi's word on the paper:** item 3, the trail.  IGLP's "Clause try" sentence would be amended; the engine then binds in the heap and undoes, which removes the σ̂ map, the tentative structures and the conversion at commit.

**Stage C, a compiler project:** item 4, the decision graph, with the Code Format extended; and item 5 with it.  This is where FCP's design gives the most for head-heavy programs, and it is the largest change: the GLP project's approval of the instructions, then the compiler phase written to GLP's matching table.

The engine's two wins so far (the image built once; the goal record) and the type checker's are outside this list and stand.
