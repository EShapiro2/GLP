# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## CRITICAL CONTEXT: AI-Managed Project

**This entire project is built through AI coordination**:
- User has deep understanding of GLP semantics but does not write Dart code
- User coordinates between Claude Chat (web interface) and Claude Code (you)
- Claude Chat handles architecture and code generation
- Claude Code handles execution, testing, and small fixes

**Current State**: Commit 7be7d83 is KNOWN WORKING (~170 tests passing)

## Division of Labor with Claude Chat

### Claude Chat (Web Interface) Handles:
- Architecture decisions and design discussions
- Complete code file generation
- Understanding and explaining existing code
- Debugging logic from error messages
- Creating comprehensive test cases

### Claude Code (You) Handles:
- Running tests and commands (`dart test`, `dart run`)
- Showing output and errors
- Making small, directed fixes (only when explicitly requested)
- Git operations
- File system operations

**CRITICAL RULE**: When facing complex architectural decisions, say: "This requires architectural understanding. Please consult Claude Chat for the design, then provide me with specific implementation instructions."

## CRITICAL: Request Current Files at Conversation Start

**At the beginning of each new conversation**:
1. **STOP** - Do not assume you have the latest code
2. **ASK** - "To proceed, I need to see the current state. Please share:"
   - Current error messages or test failures
   - The specific files being worked on
   - Recent changes made in other sessions
3. **VERIFY** - Check git status and recent commits
4. **ONLY THEN** proceed with the requested work

**Why**: The project evolves rapidly across multiple AI sessions. Working with outdated assumptions causes regressions and wastes time.

## START HERE - MANDATORY READING ORDER

**BEFORE doing ANY implementation work, you MUST read these documents in order:**

1. **`SPEC_GUIDE.md`** - Start here for overview of GLP execution model and specification structure
2. **`docs/glp-bytecode-v216-complete.md`** - NORMATIVE instruction set specification (complete and authoritative)
3. **`docs/glp-runtime-spec.txt`** - NORMATIVE Dart runtime architecture specification

**ONLY AFTER reading these three documents should you:**
- Write any code
- Implement any instructions
- Design any bytecode sequences
- Debug any execution issues

**If you have not read these documents in this session, STOP NOW and read them before proceeding.**

## Working Principles - CRITICAL

### Test Before Changing
```bash
# ALWAYS run this first
dart test
# Note how many tests pass (should be ~170)
```
If tests are failing BEFORE your changes, STOP and inform the user.

### Preserve Working Code
**NEVER remove these critical components**:
- âŒ `_ClauseVar` code - required for HEAD phase unresolved variables
- âŒ `_TentativeStruct` code - handles HEAD structure building  
- âŒ Fallback cases - handle edge conditions
- âŒ Any code you don't fully understand

The current implementation may differ from standard WAM - respect existing patterns!

### Complete Solutions, Not Partial Victories

When the user agrees to a task ("yes", "please do", "fix X"), deliver a **COMPLETE, FULLY WORKING SOLUTION**:

1. **Think through ALL implications** - What else needs to work for this to be truly complete?
2. **Test comprehensively** - Don't stop at the first successful case
3. **Fix ALL related bugs** - If spawned goals need program context, fix it NOW, not when asked
4. **Only declare done when EVERYTHING works** - Not just the happy path

**FORBIDDEN**:
- âŒ Declaring victory after partial success
- âŒ Stopping when basic case works but edge cases fail
- âŒ Waiting for user to discover bugs you should have caught
- âŒ Making user repeat requirements that were already implicit

**Example**: If asked to "make REPL variable bindings work":
- âœ… Test simple queries, lists, structures, AND metainterpreter
- âœ… Fix program inheritance for spawned goals proactively
- âœ… Ensure ALL query types work before saying "done"
- âŒ Stop after `merge([1,2,3], [a,b], Xs).` works and wait for user to ask about `run(p(X)).`

### Discussion Before Implementation

**CRITICAL: When the user gives you a comment or feedback, STOP and DISCUSS before coding:**

1. **STOP immediately** - Do not write any code
2. **DISCUSS** - Talk through your understanding, ask clarifying questions
3. **WAIT for agreement** - Only continue coding when the discussion is clearly over
4. **NEVER mix discussion with implementation** - These are separate phases

**Pattern**:
- User gives feedback/comment â†’ You discuss and clarify â†’ User signals discussion is done â†’ You implement
- **FORBIDDEN**: User gives feedback â†’ You immediately start coding while talking

### When User Provides Code from Claude Chat

1. **Save it exactly as provided** - no modifications
2. **Test immediately**:
   ```bash
   dart test
   git diff  # Show what changed
   ```
3. **Report results** - don't try to fix if it fails
4. **If it fails**: Ask "Should I revert, or would you like to consult Claude Chat for a fix?"

### Efficiency in Development

**AVOID creating unnecessary test files**:
- âŒ Don't create temporary .dart files to inspect bytecode when you can read the code
- âŒ Don't write test files when you can test in the existing REPL or test suite
- âœ… Work directly with existing tools and infrastructure
- âœ… Only create files when they're permanent additions to the codebase

**AVOID asking unnecessary questions**:
- âŒ Don't ask "should I continue?" when the task is clear
- âŒ Don't ask for confirmation on obvious next steps
- âŒ Don't interrupt workflow with procedural questions
- âœ… Ask only when there are genuinely ambiguous choices
- âœ… Use AskUserQuestion tool for real technical decisions, not process
- âœ… Make forward progress autonomously when the path is clear

**The system may ask for tool confirmation automatically - ignore those patterns and focus on making real progress.**

## Project Overview

GLP (Grassroots Logic Programs) is a secure, multiagent, concurrent logic programming language designed for implementing grassroots platforms. This repository contains the GLP runtime implementation in Dart, along with a formal specification (ESOP 2026) and a virtual machine for executing GLP bytecode.

**Key Concept**: GLP extends logic programs with paired single-reader/single-writer (SRSW) logic variables, providing secure communication channels among cryptographically-identified participants through encrypted, signed, and attested messages.

## Repository Structure

**CRITICAL: The correct working directory is `/Users/udi/GLP/glp_runtime/`**

### Directory Layout

```
/Users/udi/GLP/
â”œâ”€â”€ docs/                           # Specifications and papers
â”‚   â”œâ”€â”€ glp-bytecode-v216-complete.md  (NORMATIVE instruction set)
â”‚   â”œâ”€â”€ glp-runtime-spec.txt           (NORMATIVE runtime architecture)
â”‚   â”œâ”€â”€ glp_spec.pdf                   (Formal GLP spec)
â”‚   â””â”€â”€ wam.pdf                        (WAM paper)
â”œâ”€â”€ SPEC_GUIDE.md                   # Start here - overview of specs
â”œâ”€â”€ CLAUDE.md                       # This file
â””â”€â”€ glp_runtime/                    # Main Dart project (WORK HERE)
    â”œâ”€â”€ lib/
    â”‚   â”œâ”€â”€ runtime/                # Core runtime (heap, scheduler, etc.)
    â”‚   â”‚   â”œâ”€â”€ runtime.dart
    â”‚   â”‚   â”œâ”€â”€ heap.dart
    â”‚   â”‚   â”œâ”€â”€ cells.dart
    â”‚   â”‚   â”œâ”€â”€ terms.dart
    â”‚   â”‚   â”œâ”€â”€ machine_state.dart
    â”‚   â”‚   â”œâ”€â”€ scheduler.dart
    â”‚   â”‚   â””â”€â”€ roq.dart
    â”‚   â”œâ”€â”€ bytecode/               # Bytecode VM (IMPLEMENT HERE)
    â”‚   â”‚   â”œâ”€â”€ opcodes.dart        # Instruction definitions
    â”‚   â”‚   â”œâ”€â”€ asm.dart            # Bytecode assembly helpers
    â”‚   â”‚   â””â”€â”€ runner.dart         # Bytecode interpreter
    â”‚   â””â”€â”€ bytecode/v216/          # OLD - single-goal experimental VM (ignore)
    â”œâ”€â”€ test/
    â”‚   â”œâ”€â”€ custom/                 # Custom test scenarios
    â”‚   â””â”€â”€ conformance/            # Conformance tests
    â””â”€â”€ bin/
        â””â”€â”€ glp_runtime.dart
```

### Important Notes

1. **DO NOT create nested `glp_runtime/glp_runtime/` directories** - work in `/Users/udi/GLP/glp_runtime/`
2. **The main bytecode implementation goes in `lib/bytecode/`** (opcodes.dart, runner.dart, asm.dart)
3. **The runtime system is in `lib/runtime/`** (heap, scheduler, cells, etc.) - these files already exist
4. **Ignore `lib/bytecode/v216/`** - this is an old experimental single-goal VM, not the main implementation
5. **Tests go in `test/custom/` or `test/conformance/`** depending on type

## Known Working Behavior

These tests MUST continue working:
```bash
# Run all tests
dart test
# Should show ~170 passing

# Test REPL
dart run bin/glp_runtime.dart
> run(merge([1,5,3,3],[a,a,a,v,a,c],Xs1)).
# Should execute MORE than 2 goals and bind Xs1

# Test conjunction
> run((merge([1,2,3], Xs), merge(Xs?, [4,5], Ys))).
# Should work with shared variables
```

## Build and Test Commands

All commands should be run from the `glp_runtime/` directory:

```bash
cd /Users/udi/GLP/glp_runtime
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
  - `commitWriters()`: Applies ÏƒÌ‚w (sigma-hat-w) substitution to bind writer variables and activates suspended goals
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

### Bytecode VM (`lib/bytecode/`)

The bytecode VM implements v2.16 specification with three-phase execution:

**Key State Variables**:
- `Map<int, Object?> sigmaHat` (ÏƒÌ‚w) - Tentative writer substitution built during HEAD, applied at commit
- `Set<int> si` - Clause-local suspension set (readers blocking current clause)  
- `Set<int> U` - Goal-level suspension set (union of all Si across failed clause tries)
- `int S` - Structure subterm pointer (WAM-style)
- `Mode` - READ/WRITE mode for structure traversal

**Three-phase execution per clause**:
1. **HEAD**: Tentative unification, builds ÏƒÌ‚w without heap mutation
2. **GUARDS**: Pure tests, may add to Si
3. **BODY**: Mutations allowed only after commit

**Decision flow**:
- Failed during HEAD/GUARD â†’ abandon ÏƒÌ‚w, ignore Si, next clause
- Succeeded with Si non-empty â†’ `clause_next`: discard ÏƒÌ‚w, Siâ†’U, next clause
- Succeeded with Si empty â†’ `commit`: apply ÏƒÌ‚w to heap, wake suspended goals, enter BODY
- After all clauses: `suspend` if U non-empty, else fail definitively

### Historical/Experimental Code

**`lib/bytecode/v216/` and `test/v216_vm_min_test.dart`**: These contain a simplified single-goal VM that is NOT the main implementation. This was an early experiment and should be moved to an `OLD/` directory. The actual bytecode implementation follows the normative specifications in `docs/glp-bytecode-v216-complete.md` and `docs/glp-runtime-spec.txt`.

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
- `>>> REDUCTION: Goal X at PC Y (commit succeeded, ÏƒÌ‚w has N bindings)` - Successful commit
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
- Recent work (commits) focused on v2.16 VM with ÏƒÌ‚w commit semantics and Si/U suspension model
- The runtime models concurrent execution with fairness (tail-recursion budgets) and single-shot reactivation (armed hangers)
- Writer variables bind at commit; reader variables may cause suspension until bound

### System Predicates Implementation Status

**Fully Implemented** (all non-channel predicates for Logix support):
- âœ… **Arithmetic**: `evaluate/2` - Full arithmetic expression evaluation (+, -, *, /, mod)
- âœ… **Utilities**: `current_time/1`, `unique_id/1`, `variable_name/2`, `copy_term/2`
- âœ… **File I/O**:
  - Simple: `file_read/2`, `file_write/2`, `file_exists/1`
  - Handle-based: `file_open/3`, `file_close/1`, `file_read_handle/2`, `file_write_handle/2`
- âœ… **Directory**: `directory_list/2`
- âœ… **Terminal I/O**: `write/1`, `nl/0`, `read/1`
- âœ… **Module Loading**:
  - `link/2` - FFI/dynamic library loading via dart:ffi
  - `load_module/2` - Bytecode module loading (basic implementation, needs serialization format)

- âœ… **Channel Primitives**:
  - `distribute_stream/2` - 1-to-N stream distribution with deep copy
  - `copy_term_multi/3` - Deep copy term to two independent outputs
  - Note: `create_merger/2` implemented as pure GLP clauses (see merge tests)

All system predicates follow three-valued semantics (success/suspend/failure) and properly handle unbound readers.

**Coverage**: Complete system predicate suite for Logix OS implementation. All critical primitives implemented.

### Modules and Process Activation (from FCP)

**CRITICAL DISTINCTION** (from FCP Section 3.5):
- **Module code** = compiled program as data structure (result of compilation, not active)
- **Activation** = abstract notion of set of processes whose code resides in the module
- **Process record** = saved state containing: procedure address (PC), arguments, next-process reference

**Process Activation Semantics**:
1. **Process record stores PC** - the saved program counter indicating where to execute
2. **Scheduler dequeues process** - loads PC, arguments, and module context from process record
3. **Execution starts at stored PC** - NOT at beginning of program
4. **Each process belongs to a module** - the compiled code it executes from

**Key Principle**: When a process/goal is enqueued with PC=N, execution MUST start at PC=N, not at PC=0.

**Implementation Requirements**:
- `RunnerContext.kappa` holds the entry PC for the goal
- `BytecodeRunner.runWithStatus()` must initialize `pc = cx.kappa` (NOT `pc = 0`)
- Goals can have different entry points within the same program (e.g., run/1_start, clause/2_start)
- Reactivated goals restart at their stored kappa (typically the first clause)

**Reference**: See FCP paper Section 4.1 "Processes" and Appendix 2 "Bootstrap and Control Flow"

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
- Example patterns tested: two-way circles (Xsâ†”Ys), three-way circles (Xsâ†’Ysâ†’Zsâ†’Xs)

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
- **Writer extension**: Adding new bindings to ÏƒÌ‚w during HEAD phase
- **Reader MGU**: Verifying readers against their paired writers

GLP uses **three-valued unification**:
1. **Success**: Terms unify, ÏƒÌ‚w extended or verified
2. **Suspend**: Unbound reader encountered, add to Si/U
3. **Fail**: Terms cannot unify (mismatch)

The HEAD phase performs **tentative unification** building ÏƒÌ‚w without heap mutation. COMMIT applies ÏƒÌ‚w atomically to the heap.

## Research Sources - REQUIRED

**CRITICAL: NEVER IMPROVISE. ALWAYS READ THE SOURCES.**

### Primary Specifications (MANDATORY - Read First)

**You MUST read these three documents BEFORE any implementation work:**

1. **`SPEC_GUIDE.md`** - Overview of GLP execution model and specification structure
   - Read this FIRST to understand the overall architecture
   - Describes (Q, S, F) triple, execution phases, instruction categories

2. **`docs/glp-bytecode-v216-complete.md`** - Complete v2.16 instruction set (NORMATIVE)
   - **NORMATIVE** specification for all GLP bytecode instructions
   - Defines exact semantics of every instruction
   - **READ THIS** before implementing ANY instruction

3. **`docs/glp-runtime-spec.txt`** - Dart runtime architecture (NORMATIVE)
   - Defines scheduler architecture, goal state, data structures
   - Specifies tail-recursion budget, suspension/reactivation mechanisms
   - **READ THIS** before implementing runtime components

### Secondary References (Consult as Needed)

4. **WAM Paper**: `/Users/udi/GLP/docs/wam.pdf` - Warren's Abstract Machine (Technical Note 309, 1983)
   - Definitive source for Prolog abstract machine design
   - Explains heap allocation, environments, variable renaming, structure creation
   - **READ THIS** for understanding WAM-derived instructions (put_structure, get_structure, unify_*, etc.)

5. **GLP Spec**: `/Users/udi/GLP/docs/glp_spec.pdf` - Formal GLP specification (ESOP 2026)
   - Normative semantics for GLP language
   - Defines three-valued unification, suspension, commitment

6. **FCP Implementation**: https://github.com/EShapiro2/FCP - Flat Concurrent Prolog full source code
   - **AUTHORITATIVE** reference for module system, process activation, and goal scheduling
   - Complete working implementation of abstract machine semantics
   - Paper: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf` (Houri & Shapiro, 1989)
   - **READ THE SOURCE CODE** for understanding modules, process records, and activation semantics

### Implementation Protocol

**BEFORE implementing ANY instruction or abstract machine feature:**

1. **STOP** - Do not write code yet
2. **READ** the relevant section in the WAM paper or bytecode spec
3. **UNDERSTAND** the exact semantics from the source
4. **IMPLEMENT** based on what the source says, not what you think it should do
5. **VERIFY** your understanding by citing specific sections

**FORBIDDEN ACTIONS:**
- âŒ "Improvising" or "making it up" based on general knowledge
- âŒ Guessing semantics from instruction names
- âŒ Using secondary sources (StackOverflow, tutorials, etc.)
- âŒ Relying on memory of how Prolog "usually works"
- âŒ Implementing "similar" instructions without checking the spec

**If you catch yourself saying "improvising" or "I think it should..." - STOP and read the source.**

## Git Safety Protocol

### Before Any Work Session
```bash
git status          # Ensure clean state
git log -1 --oneline  # Note current commit
dart test | tail -n 5  # Baseline test count
```

### Creating Safety Checkpoints
```bash
# Before risky changes
git add -A
git commit -m "Checkpoint: before attempting X"
```

### If Things Break
```bash
# Immediate revert
git reset --hard HEAD~1
# Or go to known-good state
git reset --hard 7be7d83
```

## Error Response Template

When something fails:
```
The operation failed with the following error:

[Complete error message]

Current test status: X/170 passing

The error appears to be [brief description]. 

Options:
1. Revert the change (recommended if tests were passing before)
2. Consult Claude Chat for architectural guidance
3. Attempt a minimal fix (only if the issue is clear)

What would you like me to do?
```

## Summary

You are part of an AI team building GLP. Claude Chat handles architecture and code generation. You handle execution, testing, and small fixes. Always preserve working code, especially `_ClauseVar` and `_TentativeStruct`. When in doubt, consult Claude Chat for design decisions. The code at commit 7be7d83 works correctly - respect it.
