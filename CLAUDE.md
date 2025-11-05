# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GLP (Grassroots Logic Programs) is a secure, multiagent, concurrent logic programming language designed for implementing grassroots platforms. This repository contains the GLP runtime implementation in Dart, along with a formal specification (ESOP 2026) and a virtual machine for executing GLP bytecode.

**Key Concept**: GLP extends logic programs with paired single-reader/single-writer (SRSW) logic variables, providing secure communication channels among cryptographically-identified participants through encrypted, signed, and attested messages.

## Build and Test Commands

All commands should be run from the `glp_runtime/` directory:

```bash
cd glp_runtime
```

### Testing
- Run all tests: `dart test`
- Run specific test file: `dart test test/v216_vm_min_test.dart`
- Run tests matching pattern: `dart test -n "pattern"`
- Run with tags: `dart test -t tagname`

### Linting and Analysis
- Analyze code: `dart analyze`
- Treat warnings as fatal: `dart analyze --fatal-warnings`

### Running
- Run the demo: `dart run bin/demo.dart`
- Run main: `dart run bin/glp_runtime.dart`

### Compilation
- Compile to executable: `dart compile exe bin/glp_runtime.dart -o output_name`
- Compile to JavaScript: `dart compile js bin/glp_runtime.dart`

## Architecture

### Core Runtime (`lib/runtime/`)

The GLP runtime implements a concurrent logic programming execution model with SRSW variables:

- **`runtime.dart`**: Main `GlpRuntime` class orchestrating commit, abandon, and suspend operations
  - `commitWriters()`: Applies σ̂w (sigma-hat-w) substitution to bind writer variables and activates suspended goals
  - `abandonWriter()`: Handles writer abandonment and cleanup
  - `suspendGoal()`: Suspends a goal on unbound reader variables
  - `tailReduce()`: Manages tail-recursion budget to ensure fairness

- **`heap.dart`**: Manages writer and reader cells, tracks variable bindings
  - Stores `WriterCell` and `ReaderCell` objects indexed by ID
  - Maintains `writerValue` map for committed bindings

- **`roq.dart`**: Read-Only Queues (ROQueues) for suspension/reactivation
  - Each reader has a FIFO queue of `SuspensionNote` entries
  - `processOnBind()`: Activates suspended goals when a reader is bound (single-shot via `Hanger.armed`)

- **`commit.dart`**: `CommitOps.applySigmaHat()` applies tentative writer substitutions atomically
- **`suspend_ops.dart`**: `SuspendOps.suspendGoal()` creates hangers and enqueues suspension notes
- **`hanger.dart`**: Single-shot activation mechanism (`armed` flag prevents duplicate reactivation)
- **`machine_state.dart`**: Type definitions and data structures (`GoalId`, `Pc`, `GoalStatus`, `GoalQueue`, etc.)

**Key invariant**: Writers bind exactly once (at commit), readers block until bound, and suspended goals reactivate at most once per suspension via the armed hanger mechanism.

### Bytecode Abstract Machine (`lib/bytecode/`)

**NORMATIVE SPECS**:
- **Instruction Set**: `docs/glp-bytecode-v216-complete.md` - Complete v2.16 instruction set (normative)
- **Runtime Architecture**: `docs/glp-runtime-spec.txt` - Dart runtime system specification v2.1
- **Summary**: `SPEC_GUIDE.md#Bytecode Instruction Model` - Quick reference and key concepts

The GLP bytecode is modeled after WAM (Warren Abstract Machine) and FCP (Flat Concurrent Prolog) abstract machines, adapted for three-valued unification (success/suspend/fail).

**Key instruction categories** (see normative spec for details):
- **HEAD**: `head_structure`, `head_writer`, `head_reader`, `head_constant` - tentative unification, build σ̂w
- **Structure traversal**: `writer`, `reader`, `constant`, `void` - operate on S register position
- **GUARDS**: `guard`, `ground`, `known`, `otherwise` - pure tests
- **Control**: `clause_try`, `clause_next`, `commit`, `suspend`, `spawn`, `requeue`, `proceed`
- **BODY**: `put_structure`, `put_writer`, `put_reader`, `put_constant`, `allocate`, `deallocate`

**Data structures**:
- `Map<int, Object?> sigmaHat` - σ̂w: tentative writer bindings (writerId → term)
- `Set<int> si` - Si: clause-local blocked readers (cleared at each clause_try)
- `Set<int> U` - U: goal-accumulated blocked readers across all tried clauses
- `int S` - Structure subterm pointer (WAM-style)
- `Mode` - READ/WRITE mode for structure traversal

**Three-phase execution per clause**:
1. **HEAD**: Tentative unification, builds σ̂w without heap mutation
2. **GUARDS**: Pure tests, may add to Si
3. **BODY**: Mutations allowed only after commit

**Decision flow**:
- Failed during HEAD/GUARD → abandon σ̂w, ignore Si, next clause
- Succeeded with Si non-empty → `clause_next`: discard σ̂w, Si→U, next clause
- Succeeded with Si empty → `commit`: apply σ̂w to heap, wake suspended goals, enter BODY
- After all clauses: `suspend` if U non-empty, else fail definitively

### v2.16 Virtual Machine (`lib/bytecode/v216/`)

A minimal VM for executing unit goals (single predicates) over unit clauses with HEAD/GUARD/BODY structure:

- **`opcodes.dart`**: Sealed class hierarchy of GLP bytecode instructions
  - Control: `Label`, `Goto`, `ClauseTry`, `ClauseNext`, `NoMoreClauses`, `Commit`, `Proceed`, `SuspendNow`
  - HEAD (pure): `HeadWriter`, `HeadReader`, `HeadConstant`, `HeadStructure`
  - GUARDS (pure): `GuardKnown`, `GuardGround`, `GuardEqConst`, `Otherwise`
  - BODY (mutating after Commit): `PutConstant`, `Allocate`, `Deallocate`, `Spawn`, `Requeue`

- **`model.dart`**: Term model for VM execution
  - `Writer`: Variable set at COMMIT when σ̂w is applied
  - `Reader`: Variable that may block; suspension references these
  - `Const`: Ground constant
  - `CallFrame`: Execution context with `pendingWrites` (σ̂w), `blockersSi` (clause-local), `blockersU` (goal-accumulated)

- **`runner.dart`**: `VM` class implementing the v2.16 execution semantics
  - HEAD/GUARDS are pure; σ̂w computed tentatively
  - `Commit` applies σ̂w atomically, entering BODY phase where mutations occur
  - Clause failure: soft-fail with `ClauseNext`, union Si → U, try next clause
  - `NoMoreClauses`: suspend if U non-empty, else fail
  - See `test/v216_vm_min_test.dart` for usage examples

**Execution flow**: Try clauses in order → accumulate blockers (Si per clause, U across clauses) → if all clauses fail with blockers, suspend; otherwise fail definitively.

**Note**: The v2.16 VM is a simplified model for unit goals. The full bytecode abstract machine (see above) implements the complete WAM-style instruction set with structure traversal and proper tentative binding semantics.

### Other Components

- **`lib/engine/claude_adapter.dart`**: Placeholder for future integration with Claude's execution environment
- **`lib/lint/`**: Linting utilities (not fully implemented)
- **`docs/glp_spec.tex`**: Formal LaTeX specification submitted to ESOP 2026 conference

## Testing Structure

Tests are organized under `test/`:
- `bytecode/`: Tests for bytecode operations
- `conformance/`: Conformance tests against specification
- `custom/`: Custom test scenarios (merge, p/q programs, circular dependencies)
- `v216_vm_min_test.dart`: Core VM test demonstrating writer/reader/suspend/fail outcomes

**IMPORTANT - Test Preservation**:
- **ALL tests must be committed to git** to prevent loss across conversations
- When creating new tests or modifying existing ones, always commit them immediately
- Tests serve as executable documentation and regression prevention
- Run `dart test` before committing to ensure all tests pass
- Include test descriptions in commit messages to document what functionality is being tested

### VM Trace Output

The bytecode runner (`lib/bytecode/runner.dart`) automatically prints execution traces:
- `>>> TRY: Goal X at PC Y` - Starting clause execution
- `>>> REDUCTION: Goal X at PC Y (commit succeeded, σ̂w has N bindings)` - Successful commit
- `>>> ACTIVATION: Goal X awakened at PC Y` - Goal reactivated from suspension
- `>>> SUSPENSION: Goal X suspended on readers: {reader_ids}` - Goal suspended on unbound readers
- `>>> FAIL: Goal X (all clauses exhausted, U empty)` - Definitive failure

To see only VM trace output in tests: `dart test <file> 2>&1 | grep ">>>"`

### Reduction Budget

The VM supports a reduction budget to limit execution:
- Set `reductionBudget: <number>` when creating `RunnerContext`
- Default is `null` (unlimited)
- Each instruction execution increments the reduction counter
- Returns `RunResult.outOfReductions` when budget exhausted
- Useful for testing infinite loops and circular dependencies

Example:
```dart
final cx = RunnerContext(
  rt: rt,
  goalId: goalId,
  kappa: 0,
  env: env,
  reductionBudget: 100,  // Limit to 100 instruction executions
);
runner.runWithStatus(cx);
```

## Development Context

- This is an active research implementation aligned with a formal specification
- Recent work (commits) focused on v2.16 VM with σ̂w commit semantics and Si/U suspension model
- The runtime models concurrent execution with fairness (tail-recursion budgets) and single-shot reactivation (armed hangers)
- Writer variables bind at commit; reader variables may cause suspension until bound

### Working with Goals and Programs

**Goal Management**:
- Each goal remembers which program it's executing via `runtime.setGoalProgram(goalId, programKey)`
- The scheduler uses a `Map<Object?, BytecodeRunner>` to look up the correct runner per goal
- This prevents bugs where reactivated goals would use the wrong program's runner

**Creating Multi-Program Tests**:
```dart
// Create runners for different programs
final runnerQ = BytecodeRunner(progQ);
final runnerP = BytecodeRunner(progP);

// Register with scheduler using string keys
final sched = Scheduler(rt: rt, runners: {
  'q': runnerQ,
  'p': runnerP,
});

// Associate each goal with its program
rt.setGoalProgram(goalId1, 'q');
rt.setGoalProgram(goalId2, 'p');
```

**Circular Dependencies**:
- Goals can create circular dependencies via reader/writer pairs (e.g., `merge(Xs?,[a],Ys), merge(Ys?,[b],Xs)`)
- Circular dependencies create infinite streams that ping-pong between goals
- Use reduction budgets to prevent infinite loops in tests
- Example patterns tested: two-way circles (Xs↔Ys), three-way circles (Xs→Ys→Zs→Xs)

**Suspension and Reactivation**:
- Goals suspend when encountering unbound readers (added to U set)
- When a writer binds (via commit), all goals suspended on its paired reader are activated
- Activated goals are enqueued and restart from PC = kappa (clause 1)
- The ROQ (Read-Only Queue) maintains FIFO order for fairness

## Terminology - CRITICAL

**NEVER use "pattern matching" terminology** when discussing GLP or logic programs. This causes conceptual errors and implementation bugs.

**ALWAYS use the correct terms**:
- **Unification**: The process of making two terms equal by finding substitutions
- **Writer MGU** (Most General Unifier): Unification that only binds writers, never readers
- **Writer extension**: Adding new bindings to σ̂w during HEAD phase
- **Reader MGU**: Verifying readers against their paired writers

GLP uses **three-valued unification**:
1. **Success**: Terms unify, σ̂w extended or verified
2. **Suspend**: Unbound reader encountered, add to Si/U
3. **Fail**: Terms cannot unify (mismatch)

The HEAD phase performs **tentative unification** building σ̂w without heap mutation. COMMIT applies σ̂w atomically to the heap.

## Research Sources - REQUIRED

**CRITICAL: NEVER IMPROVISE. ALWAYS READ THE SOURCES.**

When implementing or debugging abstract machine semantics, you **MUST** consult primary sources **BEFORE** writing any code:

1. **WAM Paper**: `/Users/udi/GLP/docs/wam.pdf` - Warren's Abstract Machine (Technical Note 309, 1983)
   - Definitive source for Prolog abstract machine design
   - Explains heap allocation, environments, variable renaming, structure creation
   - **READ THIS** for any instruction implementation (put_structure, get_structure, unify_*, etc.)

2. **GLP Bytecode Spec**: `docs/glp-bytecode-v216-complete.md` - Complete v2.16 instruction set
   - **NORMATIVE** specification for all GLP bytecode instructions
   - **READ THIS FIRST** before implementing any instruction

3. **GLP Spec**: `/Users/udi/GLP/docs/glp_spec.pdf` - Formal GLP specification (ESOP 2026)
   - Normative semantics for GLP language
   - Defines three-valued unification, suspension, commitment

4. **FCP Paper**: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf` - Flat Concurrent Prolog paper
   - Reference for concurrent logic programming and reader/writer variables

### Implementation Protocol

**BEFORE implementing ANY instruction or abstract machine feature:**

1. **STOP** - Do not write code yet
2. **READ** the relevant section in the WAM paper or bytecode spec
3. **UNDERSTAND** the exact semantics from the source
4. **IMPLEMENT** based on what the source says, not what you think it should do
5. **VERIFY** your understanding by citing specific sections

**FORBIDDEN ACTIONS:**
- ❌ "Improvising" or "making it up" based on general knowledge
- ❌ Guessing semantics from instruction names
- ❌ Using secondary sources (StackOverflow, tutorials, etc.)
- ❌ Relying on memory of how Prolog "usually works"
- ❌ Implementing "similar" instructions without checking the spec

**If you catch yourself saying "improvising" or "I think it should..." - STOP and read the source.**
