# Instructions for Claude Code (Terminal Interface)

## 🔴 CRITICAL - START OF EVERY CONVERSATION
1. **READ CLAUDE.md** - Always read this file first
2. **INSTALL DART** - Check `/home/user/dart-sdk/bin/dart --version`. If missing, see "Dart Installation" section below
3. **SET DART PATH** - `export PATH="/home/user/dart-sdk/bin:$PATH"`
4. **MOUNT FCP** - Clone FCP repo: `git clone --depth 1 https://github.com/EShapiro2/FCP.git /tmp/FCP`
5. **MOUNT Art-of-GLP-2025** - Clone Art-of-GLP-2025 repo: `git clone --depth 1 https://github.com/EShapiro2/Art-of-GLP-2025.git /tmp/Art-of-GLP-2025`
6. **IDENTIFY CURRENT MODE** - Discussion or Implementation
7. **FOLLOW MODE RULES** - Never mix modes
8. **ASK FOR CURRENT STATE** - Request latest code/errors from user
9. **READ SPECS AS NEEDED** - Don't read all specs upfront, only when relevant to task
10. **WAIT FOR INSTRUCTIONS** - After setup is complete, do nothing until instructed by Claude Web or user

### Dart Installation (if needed)

**IMPORTANT**: The project requires Dart SDK ^3.9.4. Use version 3.10.1 or later.

```bash
# Check if dart exists and version is sufficient
/home/user/dart-sdk/bin/dart --version 2>/dev/null || echo "Dart not found"

# If not found or wrong version, install 3.10.1:
cd /home/user && \
curl -L -o dart-sdk.zip "https://storage.googleapis.com/dart-archive/channels/stable/release/3.10.1/sdk/dartsdk-linux-x64-release.zip" && \
unzip -o dart-sdk.zip && \
rm dart-sdk.zip

# Set PATH for this session
export PATH="/home/user/dart-sdk/bin:$PATH"

# Verify
dart --version
```

**What DOESN'T work in this environment:**
- `curl -fsSL https://dart.dev/get-dart | sh` → 403 Forbidden
- `apt-get install dart` → package not found
- `busybox unzip` → command not found
- Dart 3.2.0 or earlier → SDK version mismatch (project needs ^3.9.4)
- `tail`, `head`, `grep` shell commands → not available (use full output or Dart tools)

### FCP Reference Repository
The FCP (Flat Concurrent Prolog) implementation is available for reference:
- **Location**: `/tmp/FCP` (cloned at startup)
- **Reference Release**: `/tmp/FCP/Savannah` - this is the authoritative FCP release for GLP
- **Key Docs**: `/tmp/FCP/Savannah/efcp/Logix/CONSTANTS.txt` - term syntax definitions
- **GitHub**: https://github.com/EShapiro2/FCP

### Art-of-GLP-2025 Paper Repository
The Art of GLP book and LaTeX sources:
- **Location**: `/tmp/Art-of-GLP-2025` (cloned at startup)
- **Main file**: `/tmp/Art-of-GLP-2025/main_AofGLP.tex`
- **GitHub**: https://github.com/EShapiro2/Art-of-GLP-2025

## GLP Fundamentals (READ FIRST)

### Reader/Writer Basics
- `X` in a clause is a **writer** (syntactically, by definition)
- `X?` is the paired **reader** of X (syntactically, by definition)
- This is NOT a runtime property — it's determined by syntax

### Guards
- Guards test properties of their arguments **as passed**
- `writer(X)` asks: "Is X a writer?" — Yes if X, No if X?
- `reader(X?)` asks: "Is X? a reader?" — Yes
- `ground(X?)` asks: "Is the value of X? ground?"

### Arithmetic Guards Imply Groundness
Arithmetic comparison guards (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) can only succeed when their arguments are ground numbers. Therefore, if such a guard succeeds, its arguments are guaranteed to be ground, and the SRSW analyzer permits multiple reader occurrences of those variables in the clause body.

```glp
% Valid: X? < Y? proves X and Y are ground, so X? and Y? can appear multiple times
foo(X, Y, R?) :- X? < Y? | R = pair(X?, Y?).

% Also works with complex expressions - all variables are marked ground
bar(X, Y, R?) :- X? + 1 < Y? * 2 | R = sum(X?, Y?).
```

### When Debugging
- Start from language semantics, not implementation details
- If confused, ask: "What does this mean in GLP terms?"
- Don't reason about VarRefs/isReader flags — reason about readers and writers

## Core Rules

### Never Implement Without a Plan
- **NEVER start implementation without an agreed upon plan**
- First discuss and document the design
- Get explicit user agreement on the plan
- Only then proceed to implementation

### Instructions from Claude Web
When receiving instructions from Claude Web (via user copy-paste):
- **REVIEW FIRST** - Read and understand the instructions before executing
- **RAISE CONCERNS** - Let Udi know if you have comments, questions, or see potential issues
- **DON'T BLINDLY EXECUTE** - Wait for confirmation if something seems unclear or problematic
- Only proceed with execution after review is complete and any concerns are addressed

### Accuracy and Honesty
- **NEVER BS, GUESS, SPECULATE, OR HALLUCINATE**
- **IF UNSURE, SAY SO** - "I'm not sure, need to check X"
- **READ THE SPEC FIRST** - Check bytecode/runtime specs before any code changes
- **NEVER REMOVE CONTENT** - Never delete anything without explicit user approval

### Reading Specs Correctly
When checking specs:
1. **Quote the spec exactly** — don't paraphrase or interpret
2. **Answer only what the spec says** — don't add conclusions or inferences
3. **If spec covers the case**: "The spec says X"
4. **If spec is silent**: "The spec doesn't address Y"
5. **NEVER** say "the spec is clear" then spend 10 minutes explaining it

Example of WRONG spec reading:
> "Spec says: writer(X) — pass the variable directly, not via reader"

Example of CORRECT spec reading:
> "Spec 19.4.5 says: 'writer(X) in guard position - Test if Xi is an unbound writer. Succeed if Xi is unbound writer variable. Fail otherwise.'"

### Handling Unexpected GLP Behavior
When encountering unexpected behavior of GLP, **STOP!** Find out:
1. Is the unexpected behavior consistent with the spec?
2. If so, is the spec clear?
3. If inconsistent with the spec, we have a bug.

Present your findings and discuss what to do next:
- Improve the spec
- Fix the bug
- Add explanations to the docs so that the behavior becomes expected

### Bug Protocol
**NEVER bypass or circumvent a bug.** When you discover a bug:
1. **STOP immediately** - Do not attempt workarounds or alternative approaches
2. **Report precisely** - Describe what's wrong, what was expected, what actually happens
3. **Wait for discussion** - Let the user decide how to proceed
4. **No speculation** - Report facts, not guesses about causes or fixes

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
- **Current State**: 101 REPL tests + 25 unit tests passing (as of Dec 2025)
- **User Expertise**: Deep understanding of GLP semantics but does not write code
- **Working Directory**: `/Users/udi/GLP/` (user's Mac)

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

## Two Environments: Claude (Linux) vs User (Mac)

**CRITICAL: There are TWO different environments:**

| Environment | Path | Used by |
|-------------|------|---------|
| Claude Code (Linux) | `/home/user/GLP` | Claude running commands |
| User's Mac | `/Users/udi/GLP` | User running commands |

**When giving instructions TO THE USER (merge commands, etc.), ALWAYS use Mac paths (`/Users/udi/GLP`).**

---

## Practical Environment Info (Linux - Claude Code)

**Before running commands, VERIFY - don't guess:**
- Run `ls` to check directories exist
- Run `pwd` to confirm current directory
- Check file locations with `ls` before referencing them

**REPL Location and Usage:**

**Use compiled executable for faster testing** (recommended):
```bash
cd /home/user/GLP/glp_runtime
# Compile once (if not already compiled or after code changes):
export PATH="/home/user/dart-sdk/bin:$PATH"
dart compile exe bin/glp_repl.dart -o glp_repl

# Run tests with compiled executable (much faster - milliseconds vs seconds):
echo -e 'filename.glp\ngoal.' | ./glp_repl
```

**Alternative: Use dart run** (slower, recompiles each time):
```bash
cd /home/user/GLP/glp_runtime
export PATH="/home/user/dart-sdk/bin:$PATH"
echo -e 'filename.glp\ngoal.' | dart run bin/glp_repl.dart
```

**REPL Test Scripts (Linux):**
```bash
# Full REPL tests - 218 comprehensive tests (ALWAYS run this)
bash /home/user/GLP/test/full_run_repl_tests.sh

# Book examples - 141 files (tests compilation only)
bash /home/user/GLP/test/run_book_tests.sh
```

**Key paths:**
- REPL: `/home/user/GLP/glp_runtime/bin/glp_repl.dart`
- stdlib: `/home/user/GLP/programs/stdlib/`
- GLP programs: `/home/user/GLP/programs/`
- Test files: `/home/user/GLP/programs/tests/`

**Commands that DON'T exist in this environment:**
- `timeout` - not available
- `tail`, `head`, `grep` - not available (already noted above)

**REPL commands:**
- `:trace` - toggle tracing (not `trace goal.`)
- `:debug` - toggle debug output
- Load file first, then run goals

## Directory Structure

```
/Users/udi/GLP/
├── CLAUDE.md                    # ← This file - ESSENTIAL for Claude Code
├── README.md                    # ← Project readme
│
├── docs/                        # ← NORMATIVE SPECIFICATIONS
│   ├── glp-bytecode-v216-complete.md  # ← Instruction set spec
│   ├── glp-runtime-spec.txt           # ← Runtime architecture spec
│   ├── wam.pdf                        # ← WAM paper
│   └── 1-s2.0-0743106689890113-main.pdf  # ← FCP implementation
│
├── glp_runtime/                 # ← MAIN DART PROJECT
│   ├── lib/
│   │   ├── bytecode/           # ← VM implementation (runner.dart, opcodes.dart)
│   │   ├── compiler/           # ← GLP→bytecode compiler
│   │   └── runtime/            # ← Heap, scheduler, cells, terms
│   ├── test/                   # ← Dart unit tests
│   ├── bin/
│   │   └── glp_repl.dart      # ← REPL source
│   └── glp_repl               # ← Compiled REPL executable
│
├── programs/                    # ← ALL GLP SOURCE FILES (380 files total)
│   ├── stdlib/                 # ← Standard library (6 files)
│   ├── book/                   # ← Art of GLP book examples (140 files)
│   │   ├── recursive/         # ← arithmetic_trees/, list_processing/, structure_processing/
│   │   ├── streams/           # ← producers_consumers/, objects_monitors/, buffered_communication/
│   │   ├── social_graph/      # ← Agent protocols, plays/
│   │   ├── social_networks/   # ← Network protocols
│   │   ├── meta/              # ← Metainterpreters (plain/, enhanced/, debugging/)
│   │   ├── constants/         # ← Logic gates, circuits
│   │   ├── cryptocurrencies/  # ← GC protocol
│   │   └── constitutional_consensus/  # ← Consensus protocols
│   ├── tests/                  # ← REPL test files (115 files)
│   ├── lib/                    # ← Reusable library modules (8 files)
│   ├── archive/                # ← Historical/experimental (76 files)
│   └── misc/                   # ← Miscellaneous examples (26 files)
│
└── test/                        # ← TEST SCRIPTS
    ├── full_run_repl_tests.sh  # ← Full REPL tests (181 tests)
    └── run_book_tests.sh       # ← Book examples compilation test (141 files)
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

### Test Suites Overview

| Suite | Location | Tests | Purpose |
|-------|----------|-------|---------|
| Full REPL | `test/full_run_repl_tests.sh` | 218 | Comprehensive REPL tests |
| Book | `test/run_book_tests.sh` | 141 | Book examples compile check |
| Unit | `glp_runtime/test/` | ~27 | Dart unit tests |

### Standard Test Protocol

**ALWAYS run the full REPL tests before and after changes:**

```bash
cd /home/user/GLP/glp_runtime

# Full REPL tests (ALWAYS run this)
bash ../test/full_run_repl_tests.sh

# Book examples (compilation test)
bash ../test/run_book_tests.sh

# Unit tests
dart test
```

**Expected results:**
- Full REPL: 218/218 pass
- Book: 84/141 pass (57 fail due to SRSW violations in book code)
- Unit: All pass

### REPL Development Protocol
1. Make changes to `glp_runtime/lib/` or `glp_runtime/bin/glp_repl.dart`
2. Recompile: `cd /home/user/GLP/glp_runtime && dart compile exe bin/glp_repl.dart -o glp_repl`
3. Run full tests: `bash ../test/full_run_repl_tests.sh`
4. Report results

### Adding New Tests

Add to `test/full_run_repl_tests.sh` using the `run_test` function:

```bash
run_test "Test description" \
    "programs/tests/file.glp" \
    "query." \
    "expected_pattern"
```

### Bug Fix Test Protocol

**When a bug is detected and fixed:**
1. Add the test case that exposed the bug to `test/full_run_repl_tests.sh`
2. The test should verify the fix works (not just that it doesn't crash)
3. This prevents regression - the bug should never reappear

### Test Troubleshooting

If REPL tests fail unexpectedly, check these common causes:

1. **Working directory** - Tests must run from proper location. The script handles this via `cd "$GLP_RUNTIME"`, but verify you're starting from `/home/user/GLP`
2. **DART variable** - Should auto-detect or be `/home/user/dart-sdk/bin/dart`
3. **Path resolution** - `$GLP_DIR` should resolve to absolute path

**Standard test invocation:**
```bash
cd /home/user/GLP
bash test/full_run_repl_tests.sh
```

**Debug individual test manually:**
```bash
cd /home/user/GLP/glp_runtime
export PATH="/home/user/dart-sdk/bin:$PATH"
echo -e '/home/user/GLP/programs/tests/repl/TESTFILE.glp\nQUERY.' | dart run bin/glp_repl.dart
```

## Working Principles

### 0. FCP AM Adherence
- **ALWAYS follow FCP AM design precisely** - no shortcuts, "improvements", or simplifications
- **If considering any deviation from FCP AM**: STOP and discuss with user first
- **Exception only**: general unification not needed due to SRSW (already agreed)
- **Default assumption**: If FCP does it that way, we do it that way unless there is a simpler way due to the SRSW restriction

### 1. Test Before Changing
```bash
# ALWAYS run test suites first
cd /home/user/GLP/glp_runtime
bash ../test/full_run_repl_tests.sh    # 181 REPL tests
dart test                              # Unit tests
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

## 🔴 MANDATORY: Debugging Protocol for GLP Programs

**READ AND FOLLOW:** `docs/Mandatory protocol for debugging the GLP implementation with GLP programs.txt`

This protocol is required when debugging GLP programs. Do not skip steps. Stop and report to user if any step fails.

## Research Sources

### Primary Specifications (MANDATORY - Read First)

1. **`SPEC_GUIDE.md`** - Overview of GLP execution model
2. **`docs/glp-bytecode-v216-complete.md`** - Complete v2.16 instruction set
3. **`docs/glp-runtime-spec.txt`** - Dart runtime architecture

### Secondary References (Consult as Needed)

4. **WAM Paper**: `/Users/udi/GLP/docs/wam.pdf` - Warren's Abstract Machine
5. **GLP Spec**: `/tmp/GLP-2025/main GLP 2025.tex` - Formal GLP specification (paper source)
6. **FCP Implementation**: 
   - **Local Source**: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
   - **GitHub Mirror**: https://github.com/EShapiro2/FCP
   - **Paper**: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`

## Critical Implementation Details

### GLP-Specific Knowledge
- **SRSW Constraint**: Single-Reader/Single-Writer - each variable occurs at most once per clause
- **SRSW is MANDATORY**: All GLP code must pass SRSW checking. NEVER invent or use a `skipSRSW` option.
- **Anonymous variable `_`**: A writer that nobody reads - exempt from SRSW checking. Use in abort clauses where result is never bound.
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

**Status:** Single-ID variable system migration completed. The HeapV2 refactoring was superseded by direct fixes to the SRSW handling in the existing system.

**Completed Work:**
- ✅ SRSW checking is mandatory for all code (including stdlib)
- ✅ Anonymous variable `_` support for abort clauses
- ✅ Reader/writer mode handling fixed in clause heads
- ✅ Test suites passing (27 unit tests, 181 REPL tests)

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
cd /home/user/GLP/glp_runtime
bash ../test/full_run_repl_tests.sh  # Should show 181 passing
dart test                            # Should show ~27 passing
```

Example REPL tests:
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
dart test  # Run baseline tests (note: tail/head commands not available)
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
2. Run baseline tests: `dart test` and `bash test/full_run_repl_tests.sh`
3. Work on your branch

**During work:**
1. Commit frequently with clear messages
2. Test after each change
3. Push to your branch: `git push -u origin claude/<your-branch-name>`

**After completing a task and pushing:**
When a task is completed, committed, and pushed, ALWAYS provide the user with merge instructions so they can integrate the work into main. Use the exact format below with the actual branch name:

```bash
cd /Users/udi/GLP
git checkout main
git pull origin main
git fetch origin claude/<ACTUAL-BRANCH-NAME>
git merge -m "Merge claude/<ACTUAL-BRANCH-NAME> into main" origin/claude/<ACTUAL-BRANCH-NAME>
git push origin main
```

**Before ending session:**
1. Ensure all work is committed
2. Push to your branch
3. Tell user the merge commands using the **EXACT FORMAT BELOW** (copy-paste ready):

**🔴 MANDATORY FORMAT for merge instructions - USE THIS EXACTLY:**
```bash
cd /Users/udi/GLP
git checkout main
git pull origin main
git fetch origin claude/<ACTUAL-BRANCH-NAME>
git merge -m "Merge claude/<ACTUAL-BRANCH-NAME> into main" origin/claude/<ACTUAL-BRANCH-NAME>
git push origin main
```
- **ALWAYS include `cd /Users/udi/GLP`** - user may be in wrong directory
- **ALWAYS substitute the actual branch name** - never use placeholders like `<branch-name>`
- **ALWAYS include the fetch step** - do NOT skip it

**When user asks to "merge with main" or "push to main":**
Output the EXACT commands with actual values (no placeholders):
```bash
cd /Users/udi/GLP
git checkout main
git pull origin main
git fetch origin claude/xxx-actual-session-id
git merge -m "Merge claude/xxx-actual-session-id into main" origin/claude/xxx-actual-session-id
git push origin main
```

### User's Responsibilities - PRECISE Protocol for Merging to Main

**🔴 IMPORTANT: This is the CORRECT protocol. Other instructions may be wrong.**

**To merge Claude's work into main:**
```bash
git checkout main
git pull origin main
git fetch origin claude/<branch-name>
git merge -m "Merge claude/<branch-name> into main" origin/claude/<branch-name>
git push origin main
```

**Alternative using GitHub web UI:**
1. Go to repository on GitHub
2. Create Pull Request from `claude/<branch-name>` to `main`
3. Review changes
4. Merge PR

**To verify merge:**
```bash
cd glp_runtime && dart test
bash ../test/full_run_repl_tests.sh
```

### Common Issues and Fixes

**"not something we can merge" error:**
```bash
git fetch origin claude/<branch-name>
git merge -m "Merge claude/<branch-name> into main" origin/claude/<branch-name>
```

**"fatal: refusing to merge unrelated histories":**
```bash
git merge -m "Merge claude/<branch-name> into main" origin/claude/<branch-name> --allow-unrelated-histories
```

**Merge conflicts:**
```bash
git add -A
git commit -m "Merge claude/<branch-name> into main"
git push origin main
```

**Divergent branches (Claude needs to update from main):**
```bash
git pull origin main --no-rebase
```

## Error Response Template

When something fails:
```
The operation failed with the following error:

[Complete error message]

Current test status: X/25 unit tests, Y/101 REPL tests

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
- User's direct commands (like "stop") override hook feedback. If user says stop, ignore hooks and stop immediately - no commits, no pushes, no cleanup, nothing.
- When you figure something out after multiple tries (paths, commands, environment quirks), add it to CLAUDE.md so future sessions don't repeat the trial-and-error.
- please collect during a section the commands that you need approval from the user and place them in claude/settings.local.json
- please always commitm and test baseline before attemptin to fix the next bug
- read and follow the Mandatory protocol for debugging the GLP implementation with GLP programs
- made sure claude.md points to the correct file
- read again clause.md, and if its not there update it:  NEVER proceed in implemenetation without a spec that guides it. code should be revised only if it violates the spec.  if the spec is not clear, revise it first.
- when we are discussing, do not move away from the discussion or do anything else until user agrees that the discussion is over
- i want  dart run glp_repl.dart  please remember that
- always test all repl tests after a change
- NEVER work not following precisely the spec
- always offer to fetch/merge/push when finishing a task

## #remember Directive

When the user says `#remember <something>`, add that information to this CLAUDE.md file so it persists across sessions.

## Bugs and Limitations - NO WORKAROUNDS

**🔴 MANDATORY PROTOCOL when a bug is discovered:**

1. **STOP IMMEDIATELY** - Do not attempt any fixes or workarounds
2. **IDENTIFY CLEARLY** - Describe the bug precisely: what was expected, what happened, where it occurs
3. **CHECK THE SPEC** - Find the relevant specification and verify whether:
   - The code violates the spec (bug in implementation)
   - The spec is unclear (spec needs clarification first)
   - The spec seems incorrect (spec needs discussion/revision)
4. **REPORT AND DISCUSS** - Present findings to user and wait for agreement before any action
5. **DO NOT PROCEED** - No code changes until discussion concludes with clear agreement

This protocol applies to ALL bugs - runtime errors, unexpected behavior, test failures, etc.

### Known Parser Limitation: =.. not supported in clause bodies

**Bug:** The `=..` operator cannot be used as a goal in clause bodies.

```glp
% This FAILS:
compose(List, Tuple) :- Tuple? =.. List?.
% Error: "Expected predicate name or comparison" at =..

% This WORKS (in clause head):
X? =.. [Y|Ys] :- list(Ys?) | list_to_tuple([Y|Ys], X).
```

**Status:** Not yet fixed. Parser needs to recognize `=..` as a valid goal in bodies.

### Known REPL Limitation: Structs inside lists in goals

**Bug:** The REPL can't parse compound terms (structs) inside lists in goal arguments.

```glp
% This FAILS in REPL goal:
distribute_indexed([send(1,a), send(2,b)], Y, Z).
% Error: Exception: Unsupported list head type: StructTerm

% This WORKS:
distribute_indexed([], Y, Z).
```

**What works:**
- Simple lists: `[a, b, c]` ✓
- Nested lists: `[[a,b], [1,2]]` ✓
- Variables in lists: `[X?, Y?]` ✓

**What fails:**
- Structs in lists: `[send(1,a), foo(x)]` ✗
- Any compound term as list element in a goal

**Location:** `glp_repl.dart` - functions `_buildListTermForConj` and `_buildListTerm` handle `ConstTerm`, `VarTerm`, and `ListTerm`, but not `StructTerm`.

**Impact:** Can't test predicates that take lists of structures as input (indexed distributor, binary distributor, message routing).

**Status:** Not yet fixed. Need to add StructTerm case to list building functions.

## GrassrootsApp Testing Framework

See [grassroots-testing-framework.md](docs/grassroots-testing-framework.md) for the theater-style testing approach:
- **Agents**: Personal agents from the GLP paper
- **Actors**: Simulated users following scripts
- **Plays**: Test scenarios in `GrassrootsApp/plays/`

Key files:
- `GrassrootsApp/glp/agent.glp` - Personal agent implementation
- `GrassrootsApp/glp/network.glp` - 2-agent network switch
- `GrassrootsApp/plays/play01_cold_call/` - First test scenario

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

## Important Insights (Lessons Learned)

### Runtime Bugs Found and Fixed

1. **Position-sensitive UnifyVariable bug** (Nov 2025): When writer occurs before reader in clause head, the reader handler was ignoring the existing value. Fix: Check `existingValue` before creating fresh variables.

2. **ROQ suspension list corruption**: Wrapper nodes were being added incorrectly. Fixed with proper node management.

### Key Patterns

1. **Accumulator patterns in reduce clauses**: Both arg orderings now work:
   - `reduce(sum_acc([], Acc?, Acc), true)` - reader first, writer second
   - `reduce(sum_acc([], Acc, Acc?), true)` - writer first, reader second

2. **Running the REPL**: Use compiled executable `./glp_repl` for faster testing, or `dart run bin/glp_repl.dart` if exe not compiled. Run from `glp_runtime/` directory.

3. **Test file patterns**: The test suite uses a specific format - see `test/full_run_repl_tests.sh` for the `run_test` function.

4. **CRITICAL - Reader/Writer modes in clause heads**: A reader in the head can ONLY be bound to a writer in the goal. If an argument of a goal is expected to be a reader or a ground term (non-variable), then the corresponding head argument MUST be a writer, not a reader!
   - WRONG: `Result := N? :- number(N?) | ...` - N? is reader, but goal `X := 3` has ground term 3
   - RIGHT: `Result := N :- number(N?) | ...` - N is writer, can receive ground term 3

### Common Mistakes to Avoid

1. **Don't create fresh variables when clauseVars already has a value** - check first
2. **Don't overwrite clauseVars[i] without checking existing value**
3. **Always run BOTH test suites** - unit tests AND REPL tests
4. **Don't modify code without checking the spec first**
5. **Don't push to main** - only push to your claude branch

### Metainterpreter Pattern

The standard metainterpreter pattern for arithmetic:
```prolog
run(true).
run((A, B)) :- run(A?), run(B?).
run(A) :- otherwise | reduce(A?, B), run(B?).

% Handle := in metainterpreter
reduce((X?:=T), true) :- X:=T?.
```

This enables `run(factorial(5, F))` to work with arithmetic.