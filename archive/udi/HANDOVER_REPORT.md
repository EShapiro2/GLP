# GLP Runtime - Detailed Handover Report
**Date:** 2025-11-14
**Session:** SRSW Variable Access Mode Fix and New Bug Discovery
**Last Commit:** a886da3 (fix: UnifyWriter/UnifyReader SRSW variable access modes)

---

## Executive Summary

### What Was Accomplished
✅ **Fixed SRSW Variable Access Mode Bug** - `run(merge([a,b],[b],X))` now works correctly
✅ **Updated Specification** - Clarified UnifyWriter/UnifyReader WRITE mode semantics in docs/glp-bytecode-v216-complete.md
✅ **Updated Implementation** - Fixed runner.dart UnifyWriter/UnifyReader instructions
✅ **Added REPL Tests** - Added two new test cases to udi/run_repl_tests.sh
⚠️ **Discovered New Bug** - `run(insertion_sort([3,4,2,3,6,1,2],Xs))` now fails (returns `Xs = [3]` instead of correct sorted list)

### Current Status
- **Test Suite:** 86 passing, 2 failing (same baseline as before fix)
- **REPL:** Compiled with SRSW fix (buildTime: 2025-11-14T11:37:22Z)
- **Git:** Uncommitted changes in CLAUDE.md, isort.glp, run_repl_tests.sh
- **Known Issue:** Insertion sort via metainterpreter failing due to goal incorrectly succeeding when it should suspend

---

## Current Bug Investigation

### Bug Description
After the SRSW fix, a **new bug** was exposed:
- **Direct call works:** `insertion_sort([3,4,2,3,6,1,2],Xs)` → `Xs = [1, 2, 2, 3, 3, 4, 6]` ✅
- **Via metainterpreter fails:** `run(insertion_sort([3,4,2,3,6,1,2],Xs))` → `Xs = [3]` ❌

### Root Cause Analysis
From trace analysis, identified **Goal 10155** as the first incorrect reduction:
```
Goal 10155: clause/2(insert(3?,R1326?,W1321)?, W1329) :- true
```

**Expected behavior:** Should **SUSPEND** because R1326 is unbound
**Actual behavior:** Incorrectly **SUCCEEDS**, matching `clause(insert(X, [], [X?]), true)`

This causes the wrong clause to be selected, leading to incorrect results.

### Investigation Status
- ✅ Confirmed trace formatting dereferences variables correctly (not a display issue)
- ✅ Identified goal 10155 as first incorrect reduction
- ⚠️ Root cause not yet found
- 🔍 **Next Step:** Need to examine compiled bytecode for `clause(insert(X, [], [X?]), true)` to see if the two occurrences of X (as writer and reader) are causing variable identity confusion

### Blocker
No bytecode disassembler/dump tool available. User mentioned there was one previously, or a compiler output mode was being developed, but got sidetracked by bugs. **Need bytecode inspection tooling** to proceed with investigation.

---

## File Locations

### Core Implementation Files

#### Runtime (`/Users/udi/GLP/glp_runtime/lib/`)
```
runtime/
├── terms.dart              - Term definitions including VarRef (single-ID)
├── heap.dart               - OLD heap (two-ID system, deprecated)
├── heap_v2.dart           - NEW heap (single-ID system, not fully integrated)
├── heap_v2_adapter.dart   - Adapter with problems (duplicate storage)
├── runtime.dart           - Main GlpRuntime orchestration
├── scheduler.dart         - Goal scheduling and trace formatting
├── roq.dart              - Read-Only Queues for suspension/reactivation
├── cells.dart            - WriterCell and ReaderCell definitions
├── machine_state.dart    - Machine state management
└── commit.dart           - Commit operations

bytecode/
├── opcodes.dart          - Instruction definitions
├── runner.dart           - ⭐ MAIN EXECUTION ENGINE (contains SRSW fix)
├── asm.dart             - Bytecode assembly helpers
└── v216/                - OLD experimental VM (ignore)

compiler/
├── lexer.dart           - Tokenization
├── parser.dart          - AST generation
├── analyzer.dart        - Semantic analysis
├── codegen.dart         - Bytecode generation
└── token.dart          - Token definitions
```

#### Tests (`/Users/udi/GLP/glp_runtime/test/`)
```
custom/                   - Custom test scenarios (86 passing, 2 failing)
├── merge_test.dart
├── metainterp_*_test.dart
├── system_predicates_test.dart
└── ... (many others)

bytecode/                - Bytecode operation tests (DELETED - see git status)
compiler/                - Compiler tests (DELETED - see git status)
conformance/             - Conformance tests
```

⚠️ **IMPORTANT:** Many test files show as deleted in git status but this may be from a reorganization. Need to verify with `git diff` before committing.

### User Workspace (`/Users/udi/GLP/udi/`)

#### GLP Test Programs (`udi/glp/`)
```
Key files:
├── isort.glp            - ⭐ Insertion sort (exposing current bug)
├── run1.glp             - ⭐ Metainterpreter with merge clauses
├── run2.glp             - Extended metainterpreter
├── merge.glp            - Basic merge implementation
├── arithmetic_fixed.glp - Fixed arithmetic (uses evaluate/2)
├── factorial_fixed.glp  - Fixed factorial
└── [48 other .glp files] - Various test programs
```

#### REPL and Test Infrastructure
```
udi/
├── glp_repl.dart          - ⭐ REPL application (buildTime: 2025-11-14T11:37:22Z)
├── run_repl_tests.sh      - ⭐ REPL test suite (MODIFIED - uncommitted)
├── glp_repl               - Compiled REPL executable
└── bin/                   - Compiled bytecode files (*.glpc)
```

### Documentation (`/Users/udi/GLP/docs/`)

#### Normative Specifications (REQUIRED READING)
```
SPEC_GUIDE.md                      - ⭐ START HERE - Overview
glp-bytecode-v216-complete.md      - ⭐ NORMATIVE instruction set (MODIFIED)
glp-runtime-spec.txt               - ⭐ NORMATIVE runtime architecture
single-id-migration.md             - Single-ID variable system design
glp_spec.pdf                       - Formal GLP spec (ESOP 2026)
wam.pdf                           - Warren's Abstract Machine
1-s2.0-0743106689900113-main.pdf  - FCP implementation paper
```

#### Refactoring Plans
```
glp_refactoring_v3_complete.md    - ⭐ Complete single-ID migration plan
glp_refactoring_revised.md        - Earlier version
```

#### Session Reports (Historical Context)
```
SESSION_SUMMARY_NOV11.md
SINGLE_ID_MIGRATION_STATUS.md
test-failures-analysis.md
NESTED_STRUCTURE_BUG_FINAL_REPORT.md
... (many other historical reports)
```

### Root Level
```
/Users/udi/GLP/
├── CLAUDE.md                      - ⭐ CRITICAL - Instructions for Claude Code (MODIFIED)
├── README.md                      - Project overview
├── SPEC_GUIDE.md                 - Spec overview (duplicate of docs/SPEC_GUIDE.md)
└── [Many status/report .md files] - Historical context
```

---

## Git Status

### Uncommitted Changes
```
Modified:
  CLAUDE.md                 - Unknown changes (need to review)
  udi/glp/isort.glp         - Insertion sort test program
  udi/run_repl_tests.sh     - Added two new REPL tests

Deleted (in git status):
  Many test files in glp_runtime/test/bytecode/ and test/compiler/
  (Need to verify if intentional or from reorganization)

Untracked:
  Many new .md reports in udi/ directory
  claude_web_context.tar.gz
  docs/guards-reference.md
  docs/parser-spec.md
```

### Recent Commits
```
a886da3 (HEAD) fix: UnifyWriter/UnifyReader SRSW variable access modes
4f6074c        fix: Make Execute pass Terms not slots per spec 18.1
aeb0437        Checkpoint: Before adding JSON bytecode serialization
7d6c04c        docs: Add comprehensive REPL test report
55d6e84        fix: API migration for metainterp tests and cleanup
```

### Known Working State
**Commit 7be7d83** is documented in CLAUDE.md as KNOWN WORKING (~170 tests passing)

**Current state:** 86 passing, 2 failing (different test count - need to investigate)

---

## Missing from CLAUDE.md

### 1. "add:" Protocol for REPL Tests
**MISSING:** When user says "add:", Claude Code should add test cases to `/Users/udi/GLP/udi/run_repl_tests.sh`

**Should Add Section:**
```markdown
## REPL Test Protocol

When the user says "add:" followed by a test description, add a new test case to `/Users/udi/GLP/udi/run_repl_tests.sh`.

**Format:**
```bash
run_test "Test Description" \
    "filename.glp" \
    "query." \
    "expected_pattern"
```

**Location:** Add to appropriate section (BASIC TESTS, METAINTERPRETER TESTS, etc.)

**Example:**
User: "add: test merge via metainterpreter"
Action: Add to METAINTERPRETER TESTS section:
```bash
run_test "Merge via Metainterpreter" \
    "run1.glp" \
    "run(merge([a,b],[b],X))." \
    "X = \[a, b, b\]"
```

### 2. Bytecode Inspection Tools
**MISSING:** Documentation of bytecode disassembly/dump tools

**Current State:**
- User mentioned there was a disassembler or compiler output mode
- Development was sidetracked by bugs
- No `dump_bytecode.dart` currently exists
- Cannot examine compiled bytecode to debug issues

**Should Add:**
```markdown
## Bytecode Inspection

**CURRENT STATUS:** Bytecode disassembler/dump tool not yet implemented.

**Planned Tools:**
- Bytecode dump utility to display compiled programs
- Disassembler to convert binary bytecode to human-readable assembly
- Debug mode in compiler to output bytecode during compilation

**Workaround:** Manually trace through compiler/codegen.dart to understand bytecode generation.

**Priority:** HIGH - needed for debugging compilation issues
```

### 3. REPL Build/Test Protocol
**MISSING:** Step-by-step protocol for REPL changes

**Should Add:**
```markdown
## REPL Development Protocol

When making changes that affect REPL behavior:

1. **Make changes** to glp_runtime/lib/ or udi/glp_repl.dart
2. **Update buildTime** in udi/glp_repl.dart with current timestamp and description
3. **Recompile REPL:**
   ```bash
   cd /Users/udi/GLP/udi
   dart compile exe glp_repl.dart -o glp_repl
   ```
4. **Test in REPL** - User will verify changes
5. **If user confirms success** - Proceed with dart test
6. **If all tests pass** - Commit changes

**IMPORTANT:** Always let user test REPL changes before running full test suite.
```

### 4. Test Count Discrepancy
**ISSUE:** CLAUDE.md says "~170 tests passing" but current baseline is "86 passing, 2 failing"

**Need to Clarify:**
- Were tests deleted/reorganized?
- Is 86 the new correct baseline?
- Where did the other ~84 tests go?

**Should Update:**
```markdown
## Known Working Behavior

**Current Baseline (as of commit a886da3):**
- 86 tests passing
- 2 tests failing (three-way circular merge - intentional, non-terminating)
- Test count reduced from ~170 due to test suite reorganization

These tests MUST continue working:
[current examples]
```

### 5. Refactoring Status
**MISSING:** Current status in refactoring plan

**Should Add:**
```markdown
## Refactoring Status

**Current Plan:** docs/glp_refactoring_v3_complete.md

**Completed Phases:**
- ✅ Phase 0: Baseline capture
- ✅ Phase 1: Single ID variable system (HeapV2 created)
- ✅ Phase 1.5: HeapV2 integration validation

**Current Phase:** Phase 2 - Complete Single-ID Migration
- Status: **PAUSED** due to SRSW bug discovery
- VarRef moved to terms.dart ✅
- Runner.dart still has WriterTerm/ReaderTerm references
- HeapV2Adapter has problems (duplicate storage)

**Next Steps:**
1. Fix SRSW-related bugs first
2. Resume Phase 2 migration after bugs resolved
3. Eventually remove heap.dart (old two-ID system)

**DO NOT proceed with refactoring** until current bugs are fixed.
```

---

## Where We Are in Refactoring Plan

### Refactoring Plan Location
**File:** `/Users/udi/GLP/docs/glp_refactoring_v3_complete.md`

### Completed Phases

#### ✅ Phase 0: BASELINE CAPTURE
- Commit: 86538ca
- 191/206 tests passing (92.7%)
- Writer+1=Reader pattern confirmed

#### ✅ Phase 1: SINGLE ID VARIABLE SYSTEM
- HeapV2 implemented with single-ID design
- 98.9% performance improvement
- Compatibility tests passing

#### ✅ Phase 1.5: HEAPV2 INTEGRATION VALIDATION
- Integration test harness created
- Side-by-side comparison completed
- Critical paths validated

#### ✅ Phase 1.5b: HEAPV2 ADAPTER IMPLEMENTATION
- **Status:** Complete but problematic
- **Issue:** Duplicate storage in both heaps
- **Issue:** Dereferencing inconsistency
- **Decision:** May need to remove adapter and do direct migration

### Current Phase

#### ⚠️ Phase 2: COMPLETE SINGLE-ID MIGRATION
**Status:** PAUSED - Bug fix takes priority

**Progress:**
- ✅ Step 2.0.2: VarRef moved to terms.dart
- ⚠️ Remaining: 53 occurrences of WriterTerm/ReaderTerm in runner.dart
- ⚠️ Remaining: Direct heap migration (remove adapter)

**Blocker:** SRSW variable access mode bugs must be fixed first before continuing migration

### Planned Phases (Not Started)

#### Phase 2.5: Integration Validation
- Comprehensive testing with HeapV2
- Performance validation
- Regression testing

#### Phase 3: Array-Based Registers
- Replace clauseVars map with array
- Remove isReader flag (use access mode context instead)
- Optimize register access

#### Phase 4: Comprehensive Integration
- Remove HeapV2Adapter
- Direct HeapV2 usage throughout
- Update all tests

#### Phase 5: Migration Completion
- Remove old heap.dart
- Clean up deprecated code
- Final documentation update

### Decision: Refactoring Paused
**Rationale:**
1. SRSW variable access mode bugs discovered
2. Must fix bugs before continuing refactoring
3. Refactoring might invalidate bug fixes or vice versa
4. Clean implementation first, then migrate

**Resume Condition:** After all SRSW-related bugs are fixed and test suite is stable

---

## Next Steps

### Immediate (Before Refactoring)

#### 1. Create Bytecode Inspection Tool (HIGH PRIORITY)
**Blocker:** Cannot debug current bug without seeing compiled bytecode

**Options:**
- Create `dump_bytecode.dart` utility
- Add debug flag to compiler to print bytecode during compilation
- Add disassembler to runner.dart for runtime inspection

**Goal:** Examine compiled code for `clause(insert(X, [], [X?]), true)` to understand variable identity issue

#### 2. Fix Insertion Sort Bug
**Current Issue:** Goal 10155 incorrectly succeeds when it should suspend

**Investigation Plan:**
1. Get bytecode inspection working
2. Examine how `clause(insert(X, [], [X?]), true)` compiles
3. Check if X and X? maintain same varId
4. Verify UnifyWriter/UnifyReader handle variable identity correctly
5. Fix bug in compilation or execution
6. Test insertion_sort via metainterpreter
7. Verify no regressions

#### 3. Commit Current Changes
**Uncommitted:**
- udi/run_repl_tests.sh (new REPL tests)
- udi/glp/isort.glp (insertion sort test)
- CLAUDE.md (unknown changes - need review)

**Action:**
```bash
git diff CLAUDE.md                    # Review changes
git add udi/run_repl_tests.sh udi/glp/isort.glp
git commit -m "test: Add REPL tests for merge and insertion_sort via metainterpreter"
# Review CLAUDE.md changes separately
```

#### 4. Verify Test Deletions
**Many test files show as deleted** in git status

**Action:**
```bash
git status | grep "deleted:"
git diff --stat HEAD                  # See what was deleted
```

**Decision:** Confirm if intentional cleanup or accidental deletion

### After Bug Fixes

#### 5. Resume Refactoring (Phase 2)
**Only after:**
- ✅ Insertion sort bug fixed
- ✅ All REPL tests passing
- ✅ Dart test suite stable (86+ passing)
- ✅ No known SRSW-related issues

**Then proceed with:**
- Phase 2 remaining steps (remove WriterTerm/ReaderTerm from runner.dart)
- Phase 2.5 integration validation
- Continue with refactoring plan

#### 6. Create Bytecode Disassembler (MEDIUM PRIORITY)
**For future debugging:**
- Standalone tool to dump compiled bytecode
- Human-readable assembly format
- Integration with test suite for regression testing

---

## Key Reminders for Next Session

### 1. CRITICAL Context Files
Before making ANY changes, read:
1. `/Users/udi/GLP/CLAUDE.md` - Instructions for Claude Code
2. `/Users/udi/GLP/docs/SPEC_GUIDE.md` - Execution model overview
3. `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md` - Instruction set spec
4. `/Users/udi/GLP/docs/glp-runtime-spec.txt` - Runtime architecture

### 2. Test Before Changing
```bash
cd /Users/udi/GLP/glp_runtime
dart test | tail -5          # Note baseline: 86 passing, 2 failing
```

### 3. REPL Testing Protocol
After any VM changes:
1. Update buildTime in udi/glp_repl.dart
2. Recompile: `cd /Users/udi/GLP/udi && dart compile exe glp_repl.dart -o glp_repl`
3. Let user test with REPL first
4. Only then run dart test

### 4. When User Says "add:"
Add test case to `/Users/udi/GLP/udi/run_repl_tests.sh`

### 5. Current Bug Investigation
**Focus:** Goal 10155 incorrectly succeeds when it should suspend
**Need:** Bytecode inspection tool to examine `clause(insert(X, [], [X?]), true)`
**File:** `/Users/udi/GLP/udi/glp/isort.glp` contains failing test

### 6. Refactoring Status
**PAUSED** - Do not proceed with Phase 2 until bugs fixed
**Plan:** `/Users/udi/GLP/docs/glp_refactoring_v3_complete.md`

---

## Questions for User (When Appropriate)

1. **Test count discrepancy:** CLAUDE.md says ~170 tests, but current is 86 passing. Is 86 the new correct baseline?

2. **Test deletions:** Many files in test/bytecode/ and test/compiler/ show as deleted in git status. Was this intentional cleanup?

3. **CLAUDE.md changes:** What changes were made to CLAUDE.md? (Need to review git diff)

4. **Bytecode inspection:** Was there a previous dump_bytecode.dart or compiler output mode that got lost? Should we create one now?

5. **Refactoring priority:** After current bugs are fixed, should we resume Phase 2 refactoring, or focus on other features first?

---

## Summary

**Working:** SRSW variable access mode fix for merge via metainterpreter
**Broken:** Insertion sort via metainterpreter (new bug from SRSW fix)
**Blocker:** No bytecode inspection tool to debug compilation
**Status:** Refactoring paused, bug fixing in progress
**Next:** Create bytecode dump tool, fix Goal 10155 issue, commit changes

**Files to track:**
- `/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart` - SRSW fix applied here
- `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md` - Spec updated for SRSW
- `/Users/udi/GLP/udi/run_repl_tests.sh` - New tests added (uncommitted)
- `/Users/udi/GLP/udi/glp/isort.glp` - Failing test case
