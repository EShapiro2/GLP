# GLP Runtime Codebase - Complete Analysis Index

## Overview

This directory contains a comprehensive analysis of the `/Users/udi/GLP/glp_runtime` codebase, a production-quality implementation of the GLP (Grassroots Logic Programming) concurrent logic programming language in Dart.

**Key Statistics:**
- Total LOC: ~9,000 (lib/), ~2,000 (test/)
- Main components: 36 Dart files in lib/, 50+ test files
- Execution engine: 2,415 lines (runner.dart)
- Four-phase compiler pipeline
- Comprehensive test coverage (VM, algorithms, refactoring, conformance)

---

## Analysis Documents

### 1. GLP_RUNTIME_ANALYSIS.md (711 lines)
**Primary comprehensive analysis document**

Contains detailed sections:

1. **Overall Architecture** - Directory structure, four-phase compilation pipeline
2. **Core Components** - Bytecode VM, compiler, runtime system, system predicates
3. **Key Data Structures** - Term hierarchy, execution state flow, variable binding
4. **Execution Flow** - From source code to result with merge() example
5. **Test Coverage** - 50+ tests across 6 categories
6. **SRSW Enforcement** - Single Reader Single Writer constraint validation
7. **Suspension and Resumption** - Detailed mechanics with code references
8. **Heap Management** - V1 dual-ID vs V2 single-ID comparison
9. **File and Code References** - Critical implementation points with line numbers
10. **Instruction Categories** - Complete taxonomy of 44+ instruction types

**Best for:** Deep understanding of architecture, execution model, and implementation details

### 2. GLP_ARCHITECTURE_DIAGRAM.txt (157 lines)
**Visual ASCII representation of system architecture**

Includes:
- Compilation pipeline flow
- Bytecode program structure
- Runtime state machine
- Suspension/reactivation flow
- Three-phase execution sequence
- Variable binding lifecycle
- SRSW constraint enforcement
- Key design insights

**Best for:** Quick reference, presentations, understanding data flow visually

---

## Key Concepts

### Three-Phase Execution Model

Each clause execution follows a strict three-phase pattern:

```
HEAD PHASE           GUARD PHASE              BODY PHASE
─────────────────    ─────────────────        ──────────────────
Pattern matching     Pure constraint tests    Structure building
Tentative bindings   Type checks              Goal spawning
Suspension tracking  Clause refinement        Tail calls
```

### SRSW Constraint

**Single Reader Single Writer** ensures safe concurrent execution:
- Each variable has exactly one writer (one binding)
- Each reader appears once (unless grounded by guard)
- Enforced at compile-time and runtime

### Suspension Mechanism

Goals suspend on unbound readers via:
1. **Hanger**: Armed flag ensures single reactivation
2. **ROQueues**: FIFO suspension queue per reader
3. **SuspensionNote**: Links goal to reader binding event

### Heap Models

- **HeapV1** (heap.dart): Dual WriterId/ReaderId model
- **HeapV2** (heap_v2.dart): Single-ID FCP design
- **HeapV2Adapter** (heap_v2_adapter.dart): Backward-compatible bridge

---

## File Organization

### Compiler (/lib/compiler)
- **compiler.dart**: Four-phase pipeline entry
- **lexer.dart**: Tokenization with reader syntax (X?)
- **parser.dart**: Syntax analysis (HEAD :- GUARD | BODY)
- **analyzer.dart**: SRSW validation and VariableTable
- **codegen.dart**: Bytecode generation with phase tracking
- **ast.dart**: AST node definitions
- **token.dart**: Token types

### Bytecode VM (/lib/bytecode)
- **runner.dart**: Main execution engine (2415 lines)
  - runWithStatus(): Main loop
  - Commit (lines 1174-1272): Apply σ̂w, wake readers
  - ClauseNext/NoMoreClauses (lines 1278-1313): Control flow
  - Spawn (lines 1683-1735): Create new goal
  - Requeue (lines 1738-1800): Tail call
- **opcodes.dart**: v1 instruction set (44 types)
- **opcodes_v2.dart**: v2 unified instructions
- **asm.dart**: Assembly helpers

### Runtime (/lib/runtime)
- **runtime.dart**: GlpRuntime state machine
- **machine_state.dart**: Type definitions
- **heap.dart**: Dual-ID heap
- **heap_v2.dart**: Single-ID heap
- **heap_v2_adapter.dart**: Backward-compatible adapter
- **roq.dart**: Reader suspension queues
- **hanger.dart**: Single-reactivation guard
- **suspend_ops.dart**: Suspension mechanics
- **commit.dart**: σ̂w application
- **scheduler.dart**: Goal queue drainage
- **fairness.dart**: Tail-recursion budgets
- **system_predicates.dart**: Predicate registry
- **modules.dart**: Module loading

### Tests (/test)
- **bytecode/**: 11 VM instruction tests
- **custom/**: 45+ algorithm tests (merge, metainterp)
- **refactoring/**: Heap V1↔V2 migration tests
- **conformance/**: Spec compliance tests
- **lint/**: Linter validation tests
- Debug files: trace_*, debug_*, interactive_*

---

## Critical Code Paths

### Execution (runner.dart)

```
runWithStatus(RunnerContext cx)
  ├─ Loop: while pc < prog.ops.length
  ├─ Get op = prog.ops[pc]
  ├─ Handle by type:
  │  ├─ ClauseTry → initialize Si, σ̂w
  │  ├─ HeadConstant/Structure → pattern match
  │  ├─ Ground/Known → guard tests
  │  ├─ Commit → apply σ̂w (1174-1272)
  │  ├─ ClauseNext/NoMoreClauses → control flow
  │  ├─ Spawn → create goal (1683-1735)
  │  ├─ Requeue → tail call (1738-1800)
  │  └─ Proceed → success
  └─ Return: terminated/suspended/yielded/outOfReductions
```

### Suspension (suspend_ops.dart:6-18, roq.dart:31-43)

```
suspendGoal()
  ├─ Create Hanger(goalId, kappa, armed=true)
  ├─ Enqueue SuspensionNote(readerId, hanger) to ROQ
  └─ Return RunResult.suspended

processOnBind(readerId)  [called on writer binding]
  ├─ FIFO process SuspensionNotes
  ├─ For each note:
  │  ├─ if hanger.armed=true: set armed=false, produce GoalRef
  │  └─ if armed=false: skip
  └─ Return [GoalRef] for reactivation
```

### Compilation (compiler.dart:32-62)

```
GlpCompiler.compileWithMetadata(source)
  ├─ Phase 1: Lexer.tokenize()
  ├─ Phase 2: Parser.parse() → AST
  ├─ Phase 3: Analyzer.analyze() → AnnotatedProgram
  │           └─ VariableTable.verifySRSW()
  └─ Phase 4: CodeGenerator.generate() → BytecodeProgram
```

---

## Key Insights

### Single-Reactivation via Hanger

The `Hanger` class enforces exactly-once wakeup:
```dart
class Hanger {
  bool armed = true;  // First wake: armed→false, produce GoalRef
}                     // Second wake: armed=false, skip
```

This guarantees goals suspended on multiple readers wake only once.

### σ̂w (Sigma-hat) Semantics

Tentative writer bindings in HEAD phase are applied atomically in Commit:
1. Non-empty Si → skip commit, try next clause
2. Empty Si → apply all σ̂w entries to heap, wake readers

This creates atomic transactions despite concurrent goal execution.

### Three-Phase Guarantees

- **HEAD fails before GUARD** (no unintended bindings)
- **GUARD tests before BODY** (clause selection refined)
- **BODY only after COMMIT** (no partial state)

---

## Test Categories Explained

### Bytecode Tests (11 files)
- Instruction-level correctness
- VM state transitions
- Suspension/reactivation mechanics
- Fairness and budgeting

### Custom Tests (45+ files)
- Algorithm correctness (merge, metainterpreter)
- Multi-goal concurrent execution
- List operations and pattern matching
- System predicates (file I/O, arithmetic)

### Refactoring Tests (4 files)
- HeapV1 ↔ HeapV2 compatibility
- Unified instruction set migration
- Phase 2 integration validation

### Conformance Tests
- Specification compliance checking
- Edge cases and error handling

### Lint Tests (3 files)
- SRSW violation detection
- Body-phase restriction enforcement
- Valid code acceptance

---

## Quick Reference

### Most Important Files (in order)

1. **runner.dart** (2415 lines)
   - Execution engine
   - All instruction handling
   - Structure traversal (mode, S register)
   - Spawn/Requeue logic

2. **codegen.dart** (500+ lines)
   - Three-phase code generation
   - Variable allocation
   - Label management

3. **analyzer.dart** (300+ lines)
   - SRSW validation
   - VariableTable and register assignment

4. **runtime.dart** (150+ lines)
   - GlpRuntime state machine
   - suspension/commitment coordination

5. **roq.dart** (44 lines)
   - ROQueues with FIFO processing
   - Hanger-guarded single reactivation

6. **heap_v2_adapter.dart** (160+ lines)
   - Backward-compatible heap wrapper
   - V1 ↔ V2 ID mapping

### Important Constants

- `tailRecursionBudgetInit = 26` (machine_state.dart:24)
- `nextId = 1000` (heap allocation offset)
- Argument slots: 0-9 (A0-A9)
- Temp/clause vars: 10+ (Y10+)

### Important Types

- `Term`: ConstTerm, StructTerm, WriterTerm, ReaderTerm, [VarRef]
- `RunResult`: terminated, suspended, yielded, outOfReductions
- `UnifyMode`: read, write
- `GoalStatus`: active, suspended, failed, succeeded
- `SystemResult`: success, failure, suspend

---

## How to Use These Documents

**For new team members:**
1. Start with GLP_ARCHITECTURE_DIAGRAM.txt for visual overview
2. Read GLP_RUNTIME_ANALYSIS.md sections 1-4
3. Explore key files in order listed above
4. Study test files in custom/ for usage patterns

**For feature implementation:**
1. Section 2.1 for bytecode instructions
2. Section 2.2 for compiler phases
3. Section 4 for execution flow understanding
4. Search file:line references for exact implementation

**For debugging:**
1. Section 7 (Suspension/Resumption) for blocking issues
2. Section 6 (SRSW) for type checking issues
3. Section 8 (Heap) for binding issues
4. Section 4 (Execution Flow) for control flow issues

**For performance:**
1. Section 2.3 (Runtime System) for scheduling
2. Section 8 (Heap) for alternative models
3. Fairness section (tail budget) for fairness tuning

---

## References

- **GLP Specification**: Parent directory docs/
- **REPL**: bin/glp_repl.dart
- **Integration**: lib/engine/claude_adapter.dart
- **Linter**: lib/lint/linter.dart

---

Generated: 2025-11-10
Analysis Depth: Very Thorough
Total Documentation: 868 lines (711 analysis + 157 diagram)
Code Coverage: Compiler, VM, Runtime, Heap, Scheduler, Tests

