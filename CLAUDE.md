# Instructions for Claude Code (Terminal Interface)

## 🔴 CRITICAL - START OF EVERY CONVERSATION
1. **READ CLAUDE.md** - Always read this file first
2. **IDENTIFY CURRENT MODE** - Discussion or Implementation  
3. **FOLLOW MODE RULES** - Never mix modes
4. **ASK FOR CURRENT STATE** - Request latest code/errors from user
5. **READ SPECS AS NEEDED** - Don't read all specs upfront, only when relevant to task

## Core Rules

### Accuracy and Honesty
- **NEVER BS, GUESS, SPECULATE, OR HALLUCINATE**
- **IF UNSURE, SAY SO** - "I'm not sure, need to check X"
- **READ THE SPEC FIRST** - Check bytecode/runtime specs before any code changes
- **NEVER REMOVE CONTENT** - Never delete anything without explicit user approval

### Communication Style  
- **BE TERSE** - Brief, direct responses
- **NO LONG EXPLANATIONS** - Get to the point
- **MISTAKES**: Just acknowledge - no apologies or promises
- **NO VERBOSE POLITENESS** - Skip the fluff

## Your Role
You are the **executor and tester** for the GLP Runtime project. You run commands, show output, and implement code based on Claude Chat's architectural guidance.

## Key Context
- **Project**: GLP (Grassroots Logic Programs) - a secure concurrent logic programming language
- **Implementation Language**: Dart
- **Current State**: Commit 7be7d83 is KNOWN WORKING (~170 tests passing)
- **User Expertise**: Deep understanding of GLP semantics but does not write code
- **Working Directory**: `/Users/udi/GLP/glp_runtime/`

## Working Modes

### Discussion Mode (DEFAULT)
- **NO CODE CHANGES** - Not even small fixes
- **BRIEF RESPONSES** - Show output, explain what you see
- **STAY ON TOPIC** - Don't jump ahead
- **WAIT FOR AGREEMENT** - Explicit "let's implement" signal needed

### Implementation Mode  
- **ONLY AFTER EXPLICIT AGREEMENT**
- **FOLLOW CLAUDE CHAT'S GUIDANCE** - Implement what was discussed
- **TEST IMMEDIATELY** - Run tests after each change
- **REPORT RESULTS** - Show exactly what changed

## Mode Transition Protocol
1. User must explicitly say: "Discussion complete, let's implement" or similar
2. Confirm understanding: "Moving to implementation mode"
3. Only then modify code

## Working with Udi's Design Process

- **DO NOT agree too quickly** - Udi often changes his mind during design discussions
- **ASK clarifying questions** before implementing
- **POINT OUT inconsistencies or potential issues**
- **WAIT for design to stabilize** before updating specs or code
- **PUSH BACK** if something seems problematic
- Design discussions should reach clear agreement before implementation begins

## Division of Labor

### Claude Chat Handles:
- **Architecture decisions** - Overall design patterns, data structure choices
- **Algorithm design** - Complex logic flow, novel approaches
- **Complete file generation** - For difficult algorithms requiring design
- **Specification consistency** - Ensuring docs match implementation

### You Handle:
- **Code generation from guidance** - Turn Claude Chat's instructions into code
- **Running commands** - `dart test`, `dart run`, git operations
- **Showing output** - Complete error messages and test results
- **File operations** - Reading, writing, modifying files
- **Small targeted fixes** - Only when explicitly requested (see definition below)

### Code Generation Scope - Who Does What

**Examples of code generation you handle:**
- Implementing handlers for new opcodes based on spec
- Adding validation checks as directed
- Modifying existing logic following specific instructions
- Writing test cases based on requirements
- Converting "change line X to Y" instructions into code
- Implementing "Add handler for opcode Z with logic A, B, C"

**Claude Chat generates complete code for:**
- Novel algorithms requiring design (e.g., new unification approach)
- Complex refactoring affecting multiple files  
- Redesigning major subsystems
- When you say "This requires architectural understanding"

### Small Targeted Fixes - Definition

**Small targeted fixes include:**
- Changing operators/conditions (>, >=, ==, !=)
- Adding null/bounds checks
- Fixing typos or off-by-one errors
- Updating variable names
- Adding debug print statements
- Removing debug statements

**NOT small (escalate to Claude Chat):**
- Algorithm changes
- Adding new data structures
- Changing control flow significantly
- Modifying function signatures
- Adding new methods/classes
- Changing error handling patterns

### When to Escalate to Claude Chat

**Always escalate these decisions:**
- Choosing data structures (Map vs List, etc.)
- Error handling approach
- Performance optimization strategies
- Architectural patterns
- Algorithm selection
- API design

**Don't escalate obvious fixes:**
- Off-by-one errors
- Null pointer fixes
- Typos in strings
- Missing semicolons

**Use this message:** "This requires architectural understanding. Please consult Claude Chat for the design, then provide me with specific implementation instructions."

## Directory Structure

```
/Users/udi/GLP/
├── CLAUDE.md                    # ← This file - ESSENTIAL for Claude Code
├── README.md                    # ← Project readme
│
├── docs/                        # ← NORMATIVE SPECIFICATIONS
│   ├── glp-bytecode-v216-complete.md  # ← Instruction set spec
│   ├── glp-runtime-spec.txt           # ← Runtime architecture spec
│   ├── glp_spec.pdf                   # ← Formal GLP spec (ESOP 2026)
│   ├── wam.pdf                        # ← WAM paper
│   └── 1-s2.0-0743106689900113-main.pdf  # ← FCP implementation
│
├── glp_runtime/                 # ← MAIN DART PROJECT (git tracked)
│   ├── lib/
│   │   ├── bytecode/           # ← VM implementation (runner.dart, opcodes.dart)
│   │   ├── compiler/           # ← GLP→bytecode compiler
│   │   └── runtime/            # ← Heap, scheduler, cells, terms
│   ├── test/                   # ← All tests (bytecode/, custom/, conformance/)
│   └── bin/                    # ← Demos, utilities
│
├── udi/                         # ← USER WORKSPACE (git tracked)
│   ├── glp/                    # ← User GLP source files (*.glp)
│   ├── bin/                    # ← Compiled bytecode files (*.glpc)
│   └── glp_repl.dart          # ← REPL application
│
└── archive/                     # ← OLD/HISTORICAL FILES (ignore)
```

## Mandatory Reading Order

**BEFORE any implementation:**

1. **`SPEC_GUIDE.md`** - Start here for overview of GLP execution model
2. **`docs/glp-bytecode-v216-complete.md`** - NORMATIVE instruction set specification
3. **`docs/glp-runtime-spec.txt`** - NORMATIVE Dart runtime architecture
4. **`docs/single-id-migration.md`** - Single-ID variable system design (CURRENT)

**Read these AS NEEDED, not all at conversation start.**

## Implementation Guidance Protocol

When Claude Chat provides guidance like:
```
File: lib/bytecode/runner.dart
Line 684: Replace GetVariable handler
Logic: Check if Xi is reader, if arg is writer, allocate fresh var...
```

You:
1. Open the file
2. Find the specific location
3. Implement the described logic
4. Test immediately
5. Report results

## Test Protocols

### REPL Test Protocol

**REPL tests are part of the standard test suite** - run them alongside `dart test`.

**Location:** `/Users/udi/GLP/udi/run_repl_tests.sh`

**When to run:**
- After REPL recompilation
- Before commits (with dart test)
- When testing REPL-related changes
- As part of baseline/regression testing

**Adding new tests:**
When user says "add:" followed by description, add to `run_repl_tests.sh`:

```bash
run_test "Test description" \
    "file.glp" \
    "query." \
    "expected_pattern"
```

### REPL Development Protocol
1. Make changes to `glp_runtime/lib/` or `udi/glp_repl.dart`
2. Update buildTime in `glp_repl.dart` to current timestamp
3. Recompile: `cd /Users/udi/GLP/udi && dart compile exe glp_repl.dart -o glp_repl`
4. Let user test REPL first (they will run queries)
5. If user confirms success, run BOTH test suites:
   - `dart test`
   - `bash run_repl_tests.sh`
6. Report full test results from both suites

### Standard Test Protocol

**ALWAYS run BOTH test suites:**

```bash
# Unit tests (glp_runtime/test/)
cd /Users/udi/GLP/glp_runtime
dart test

# REPL integration tests (udi/run_repl_tests.sh)
cd /Users/udi/GLP/udi
bash run_repl_tests.sh
```

**Before ANY changes:**
```bash
dart test                    # Note baseline unit test count
bash run_repl_tests.sh       # Note baseline REPL test count
```

**After changes:**
```bash
dart test                    # Compare to baseline
bash run_repl_tests.sh       # Compare to baseline
```

**Report both results:**
```
Unit tests: 86/89 passing
REPL tests: 13/18 passing
```

**For specific test:**
```bash
dart test test/specific_test.dart
```

## Working Principles

### 0. FCP AM Adherence
- **ALWAYS follow FCP AM design precisely** - no shortcuts, "improvements", or simplifications
- **If considering any deviation from FCP AM**: STOP and discuss with user first
- **Exception only**: general unification not needed due to SRSW (already agreed)
- **Default assumption**: If FCP does it that way, we do it that way unless there is a simpler way due to the SRSW restriction

### 1. Test Before Changing
```bash
# ALWAYS run BOTH test suites first
dart test                 # Unit tests - should be ~86/89 passing
bash run_repl_tests.sh    # REPL tests - note baseline
```
If tests failing BEFORE changes, STOP and inform user.

### 2. Preserve Working Code
**NEVER remove without explicit approval:**
- `_ClauseVar` - HEAD phase unresolved variables
- `_TentativeStruct` - HEAD structure building
- Fallback cases - edge conditions
- Any code you don't understand

The current implementation may differ from standard WAM - respect existing patterns!

### 3. When User Provides Code from Claude Chat
1. Save exactly as provided - no modifications
2. Test immediately:
   ```bash
   dart test
   git diff  # Show what changed
   ```
3. Report results
4. If fails: "Should I revert, or consult Claude Chat for a fix?"

### 4. Complete Solutions, Not Partial Victories

When implementing a solution:
1. **Think through ALL implications** 
2. **Test comprehensively** - Don't stop at first successful case
3. **Fix ALL related bugs** - If spawned goals need program context, fix it NOW
4. **Only declare done when EVERYTHING works** 

### 5. Discussion Before Implementation

**CRITICAL: When user gives feedback, STOP and DISCUSS before coding:**

1. **STOP immediately** - Do not write any code
2. **DISCUSS** - Talk through understanding, ask clarifying questions
3. **WAIT for agreement** - Only continue when discussion clearly over
4. **NEVER mix discussion with implementation**

## Research Sources

### Primary Specifications (MANDATORY - Read First)

1. **`SPEC_GUIDE.md`** - Overview of GLP execution model
2. **`docs/glp-bytecode-v216-complete.md`** - Complete v2.16 instruction set
3. **`docs/glp-runtime-spec.txt`** - Dart runtime architecture

### Secondary References (Consult as Needed)

4. **WAM Paper**: `/Users/udi/GLP/docs/wam.pdf` - Warren's Abstract Machine
5. **GLP Spec**: `/Users/udi/GLP/docs/glp_spec.pdf` - Formal GLP specification
6. **FCP Implementation**: 
   - **Local Source**: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
   - **GitHub Mirror**: https://github.com/EShapiro2/FCP
   - **Paper**: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`

## Critical Implementation Details

### GLP-Specific Knowledge
- **SRSW Constraint**: Single-Reader/Single-Writer - each variable occurs at most once per clause
- **Three-Phase Execution**: HEAD (tentative unification) → GUARDS (pure tests) → BODY (mutations)
- **Suspension Mechanism**: Goals suspend on unbound readers, reactivate when writers are bound
- **Writer MGU**: Only binds writers, never readers; never binds writer to writer

### Three-Valued Unification
1. **Success**: Terms unify, σ̂w extended or verified
2. **Suspend**: Unbound reader encountered, add to Si/U
3. **Fail**: Terms cannot unify (mismatch)

### Current Architecture
- `RunnerContext`: Maintains execution state including `clauseVars`, `sigmaHat`, `si`, `U`
- `BytecodeRunner`: Executes bytecode instructions
- `_TentativeStruct`: Handles structure building in HEAD phase
- `_ClauseVar`: Represents unresolved variables during HEAD phase (CRITICAL - DO NOT REMOVE)
- Structure completion: Tracked by `argsProcessed >= structureArity`

## Refactoring Status

**Current Plan:** `/Users/udi/GLP/docs/glp_refactoring_v3_complete.md`

**Completed Phases:**
- ✅ Phase 0: Baseline capture (commit 86538ca)
- ✅ Phase 1: Single ID variable system (HeapV2 created)
- ✅ Phase 1.5: HeapV2 integration validation
- ✅ Phase 1.5b: HeapV2Adapter implementation (has problems - duplicate storage)

**Current Phase:** Phase 2 - Complete Single-ID Migration
- Status: **PAUSED** due to SRSW bug discovery and fixes
- VarRef moved to terms.dart ✅
- Runner.dart still has some WriterTerm/ReaderTerm references
- HeapV2Adapter has problems (duplicate storage, dereferencing inconsistency)

**IMPORTANT:** Do NOT proceed with refactoring until current SRSW-related bugs are fixed.

## Bytecode Inspection Tools

### dump_bytecode.dart - Bytecode Disassembler ✅

**Location:** `/Users/udi/GLP/udi/dump_bytecode.dart`

**Usage:**
```bash
cd /Users/udi/GLP/udi
dart dump_bytecode.dart glp/<filename>.glp
```

**What it does:**
- Compiles a .glp source file
- Outputs complete bytecode disassembly showing all instructions with PC addresses
- Shows procedure entry points and clause boundaries

**Example:**
```bash
# Dump bytecode to file for analysis
dart dump_bytecode.dart glp/qsort.glp > /tmp/qsort_bytecode.txt

# View specific bytecode section
grep -A 30 "39:" /tmp/qsort_bytecode.txt  # View bytecode starting at PC 39
```

**Output format:**
```
PC 39: ClauseTry
PC 40: HeadNil
PC 41: GetReaderVariable
PC 42: GetWriterValue
PC 43: Commit
PC 44: Proceed
```

**When to use:**
- Debugging compilation issues
- Understanding how clauses are compiled
- Verifying opcode sequences
- Investigating variable mode conversions
- Checking clause structure and guard placements
- Analyzing HEAD/GUARD/BODY instruction placement

## Known Working Tests
These must continue passing:
```bash
dart test  # Should show ~170 passing
```

REPL tests:
```
> run(merge([1,5,3,3],[a,a,a,v,a,c],Xs1)).
# Should execute MORE than 2 goals and bind Xs1

> run((merge([1,2,3], Xs), merge(Xs?, [4,5], Ys))).
# Should work with shared variables
```

## Git Safety Protocol

### Before Any Work
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

## Multi-Claude Git Collaboration Protocol

### Branch Rules
- **Main branch** (`main`) is the source of truth - contains all merged, stable work
- **Each Claude session** works on its own branch: `claude/...-<session-id>`
- **Permissions:**
  - Each Claude can pull from any branch (main, other claude branches)
  - Each Claude can only push to its own branch
  - Only the user can merge into main

### Workflow Diagram
```
main ◄─── merge (user only) ◄───┬──────────────┐
                                │              │
              pull              │              │
                ▼               │              │
Claude A: work → push → branch-A               │
Claude B: work → push → branch-B ──────────────┘
```

### Claude's Responsibilities

**At session start:**
1. Pull from main: `git pull origin main`
2. Run baseline tests: `dart test` and `bash run_repl_tests.sh`
3. Work on your branch

**During work:**
1. Commit frequently with clear messages
2. Test after each change
3. Push to your branch: `git push -u origin claude/<your-branch-name>`

**Before ending session:**
1. Ensure all work is committed
2. Push to your branch
3. Tell user: "My changes are on branch `claude/xxx`. To merge to main, run:"
   ```bash
   git fetch origin claude/<branch-name>
   git checkout main
   git merge origin/claude/<branch-name>
   git push origin main
   ```

### User's Responsibilities

**To merge Claude's work into main:**
```bash
# Step 1: Fetch Claude's branch
git fetch origin claude/<branch-name>

# Step 2: Switch to main
git checkout main

# Step 3: Merge Claude's branch
git merge origin/claude/<branch-name>

# Step 4: Push to main
git push origin main
```

**To verify merge:**
- Ask Claude to pull main and run tests
- Or run tests yourself: `dart test && bash udi/run_repl_tests.sh`

### Common Issues

**"not something we can merge" error:**
- You need to fetch first: `git fetch origin claude/<branch-name>`
- Then merge: `git merge origin/claude/<branch-name>`

**Divergent branches:**
- Use `git pull origin main --no-rebase` to merge main into your branch
- Or use `git pull origin main --rebase` if you prefer rebasing

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

## Efficiency in Development

**AVOID creating unnecessary test files:**
- ❌ Don't create temporary .dart files to inspect bytecode when you can read code
- ❌ Don't write test files when you can test in existing REPL or test suite
- ✅ Work directly with existing tools and infrastructure
- ✅ Only create files when they're permanent additions

**AVOID asking unnecessary questions:**
- ❌ Don't ask "should I continue?" when task is clear
- ❌ Don't ask for confirmation on obvious next steps
- ✅ Ask only when genuinely ambiguous choices
- ✅ Make forward progress autonomously when path is clear

## Summary
You are part of an AI team building GLP. Claude Chat handles architecture and designs the solution. You implement based on guidance, execute tests, and show results. Always preserve working code. When in doubt, consult Claude Chat for design decisions. For the mode-aware opcodes work: start in Discussion Mode to review specs, then transition to Implementation Mode after approval.
- never modify code without consulting the spec. There are only three possibilities: 1. The spec are clear, the code needs to be revised to match the spec.  2. The specs are not clear. They should be clarified before deciding how to revise the code.  3. The specs seem incorrect. They should be discussed and possibly revised before doing any code work.
- when you work on bug, work till the program is working
- when suspecting a code to be incorrect, first check the spec to see if it is consistent with it
- always work with correct and complete and clear spec. never move forward without such spec.
- check the repl test suite before unit testing
- always start with baseline tests and commit!
- accomodate my requests, and stay on topic until they are fulfilled
- please collect during a section the commands that you need approval from the user and place them in claude/settings.local.json
- please always commitm and test baseline before attemptin to fix the next bug
- read and follow the Mandatory protocol for debugging the GLP implementation with GLP programs
- made sure claude.md points to the correct file
- read again clause.md, and if its not there update it:  NEVER proceed in implemenetation without a spec that guides it. code should be revised only if it violates the spec.  if the spec is not clear, revise it first.
- when we are discussing, do not move away from the discussion or do anything else until user agrees that the discussion is over
- i want  dart run glp_repl.dart  please remember that
- always test all repl tests after a change

## #remember Directive

When the user says `#remember <something>`, add that information to this CLAUDE.md file so it persists across sessions.

## Git Collaboration Protocol (Multiple Claude Code Sessions)

1. **Main branch** (`main`) is the source of truth - contains all merged, stable work
2. **Each Claude session** works on its own branch (`claude/...-<session-id>`)
3. **Permissions**:
   - Each Claude can **pull from any branch** (main, other claude branches)
   - Each Claude can **only push to its own branch** (403 error otherwise)
   - Only the **user** can merge into main
4. **Workflow**:
   - Pull from `main` at session start to get latest work
   - Create commits on your own branch
   - Push to your branch when done
   - User merges completed work into `main`
5. **At session end**: Ensure all work is committed and pushed to your branch