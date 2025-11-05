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
- `custom/`: Custom test scenarios
- `v216_vm_min_test.dart`: Core VM test demonstrating writer/reader/suspend/fail outcomes

## Development Context

- This is an active research implementation aligned with a formal specification
- Recent work (commits) focused on v2.16 VM with σ̂w commit semantics and Si/U suspension model
- The runtime models concurrent execution with fairness (tail-recursion budgets) and single-shot reactivation (armed hangers)
- Writer variables bind at commit; reader variables may cause suspension until bound
