# Instructions for Claude Code (Terminal Interface)

## 🔴 CRITICAL - NEVER ASSUME, ALWAYS VERIFY

**Before referencing any file, path, or fact:**
1. **VERIFY FILE EXISTS** - Use `ls`, `find`, or directory listing before referencing any file path
2. **VERIFY FILE LOCATION** - Don't assume paths from memory or previous sessions; always check
3. **VERIFY FILE CONTENTS** - Read the actual file before describing what's in it
4. **VERIFY DIRECTORY STRUCTURE** - List directories before assuming their contents
5. **NO HALLUCINATED PATHS** - If you can't verify a path exists, say so

This applies to:
- Test files and their locations
- Source code files
- Documentation
- Any file or directory mentioned in instructions

## 🔴 CRITICAL - START OF EVERY CONVERSATION

**MANDATORY READING - Complete these IN ORDER before ANY other action:**

1. **READ CLAUDE.md** - Read this entire file to completion
2. **ACKNOWLEDGE CLAUDE.md** - State "I have read CLAUDE.md completely"
3. **READ docs/DISCIPLINE.md** - Read to completion
4. **ACKNOWLEDGE DISCIPLINE.md** - State "I have read DISCIPLINE.md completely"
5. **READ docs/typed-glp-manual.md** - Read to completion
6. **ACKNOWLEDGE typed-glp-manual.md** - State "I have read typed-glp-manual.md completely"
7. **STOP AND WAIT** - Do not read any other files. Wait for user direction.

**DO NOT read handovers, specs, code, or any other files until user gives direction.**

### After User Gives Direction
8. **INSTALL DART** - Only when needed: Check `/home/user/dart-sdk/bin/dart --version`. If missing, see "Dart Installation" section below
6. **SET DART PATH** - `export PATH="/home/user/dart-sdk/bin:$PATH"`
7. **MOUNT FCP** - Clone FCP repo: `git clone --depth 1 https://github.com/EShapiro2/FCP.git /tmp/FCP`
8. **MOUNT Art-of-GLP-2025** - Clone Art-of-GLP-2025 repo: `git clone --depth 1 https://github.com/EShapiro2/Art-of-GLP-2025.git /tmp/Art-of-GLP-2025`
9. **IDENTIFY CURRENT MODE** - Discussion or Implementation
10. **FOLLOW MODE RULES** - Never mix modes
11. **ASK FOR CURRENT STATE** - Request latest code/errors from user
12. **READ SPECS AS NEEDED** - Don't read all specs upfront, only when relevant to task

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

### GitHub Directory Zip Downloads
When user asks for a zip of a GitHub directory, use this format:
```
https://download-directory.github.io/?url=https://github.com/EShapiro2/GLP/tree/BRANCH/path/to/directory
```

Example:
```
https://download-directory.github.io/?url=https://github.com/EShapiro2/GLP/tree/claude/moded-type-helper-7svFn/glp_runtime/lib/analysis/type_checker
```

## Core Rules

### Do Exactly What Is Asked
- **When the user asks something, do exactly as asked and nothing else**
- Do not add extra steps, analysis, or actions beyond the specific request
- If clarification is needed, ask first rather than assuming
- **NEVER EXCEED THE SCOPE** of instructions given by Claude Web or the user

### 🔴 NEVER Deviate From Instructions
- **NEVER decide on your own not to implement a change you were instructed to implement**
- **NEVER revert a change you were instructed to make without explicit permission**
- **Perform every task to completion** - or if impossible, STOP and report why
- **NEVER divert** from the instructed task to do something else
- **NEVER continue** with actions not based on instructions
- If you encounter an obstacle: STOP, REPORT, WAIT for direction

### 🔴 GLP Code Modification Protocol

**Before modifying any `.glp` file:**
1. Show the proposed change (old code → new code)
2. Wait for explicit user approval
3. Only then make the edit

**Before running or tracing GLP code in the REPL:**
1. Show the user which file will be loaded
2. Show the goal that will be executed
3. Wait for approval (or use pre-approved commands from settings)

Never modify or run GLP code without showing the user first.

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
- **DO NOT EXCEED SCOPE** - Execute only what is specified in the instructions, nothing more
- Only proceed with execution after review is complete and any concerns are addressed

**Note on instruction format (2026-01):** Both Claude Web and Claude Code now use Opus. Instructions do not need to be verbatim code - general but clear instructions are acceptable. Claude Code can interpret design intent and implement appropriately. The key requirements are:
- Clear specification of WHAT needs to be done
- Reference to relevant spec/paper sections
- Success criteria (what tests should pass)
- File paths when relevant

Verbatim code is still welcome when precision is critical, but not mandatory.

**Workflow reminder (2026-01):** When Claude Web makes changes to tracked files (like implementation plans), always ask the user to **push those changes before** giving instructions to Claude Code. This prevents merge conflicts when Claude Code later tries to commit changes to the same files.

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

### Code Changes Must Follow Spec
- **Every code change must be backed by reference to the spec**
- **NEVER make any change that is not implied by the spec**
- **NEVER make any change that is inconsistent with the spec**
- **If the spec is not clear, STOP and ask for clarification** before making any code changes

### Handling Unexpected GLP Behavior
When encountering unexpected behavior of GLP, **STOP!** Find out:
1. Is the unexpected behavior consistent with the spec?
2. If so, is the spec clear?
3. If inconsistent with the spec, we have a bug.

Present your findings and discuss what to do next:
- Improve the spec
- Fix the bug
- Add explanations to the docs so that the behavior becomes expected

### GLP Bug Reporting Format
When a suspected GLP bug is found, report it in THIS EXACT FORMAT with no intervening text or explanations:

**Failing Goal:**
```
<the goal that fails>
```

**Type and Procedure Declarations:**
```prolog
<relevant type definitions>
<procedure declaration>
```

**Suspected Clause(s):**
```prolog
<the clause(s) that should match but don't>
```

Then STOP and wait for discussion. Do NOT attempt to fix. Do NOT add explanations between the sections.

### Discussion Mode is Default - No Rushing to Execution
When discussing issues or bugs:
1. **Stay in discussion mode** - Do NOT start implementing, building, or running code
2. **Wait for explicit approval** - User must explicitly say to proceed with implementation
3. **Present findings only** - Report what you found, then STOP and WAIT
4. **No "let me just try"** - Even small tests or builds require approval during discussion
5. **Ask questions** - If something is unclear, ask rather than assuming and executing

### Bug Protocol
**NEVER bypass or circumvent a bug.** When you discover a bug:
1. **STOP immediately** - Do not attempt workarounds or alternative approaches
2. **Report precisely** - Describe what's wrong, what was expected, what actually happens
3. **Wait for discussion** - Let the user decide how to proceed
4. **No speculation** - Report facts, not guesses about causes or fixes

### Spec Consistency and Single Source of Truth

**Before implementing any feature or fix:**
1. **Identify ALL spec documents** that cover the affected area
2. **Verify they are consistent** with each other
3. **If conflicts exist**: STOP, harmonize specs first, then implement
4. **Never implement against conflicting specs**

**Single source of truth for each subsystem:**
- Each subsystem should have ONE authoritative spec document
- Other documents should REFERENCE, not duplicate content
- When updating: update the authoritative spec, verify references still make sense
- Example: `docs/heap/heap-pointer-architecture-spec.md` is authoritative for heap design; `docs/glp-runtime-spec.txt` references it

**Implementation decisions MUST be derived from spec:**
- If the spec covers the case: implement exactly as specified
- If the spec is silent: STOP and discuss, then update spec before implementing
- If the spec is ambiguous: STOP and clarify spec before implementing
- **NEVER make arbitrary implementation decisions** — all decisions must trace to spec

**"Robustness" is often a workaround in disguise:**
- If a function is being called with invalid input, the BUG is in the caller
- Don't make the function accept invalid input to be "robust"
- Fix the caller to pass valid input
- Example: If `writerForReader(addr)` receives a writer address, don't make it "handle" that — fix the caller

### Communication Style
- **BE TERSE** - Brief, direct responses
- **NO LONG EXPLANATIONS** - Get to the point
- **MISTAKES**: Just acknowledge - no apologies or promises
- **NO VERBOSE POLITENESS** - Skip the fluff
- **ONE-LINER SHELL COMMANDS** - When giving shell commands to user, always use single-line format (no comments, no multi-line). User can copy-paste directly.

### Showing GLP Code
- **ALWAYS show full context**: type declarations, procedure declarations, and full clauses
- **NO intervening text** between related code blocks
- **Group related definitions together** in a single code block

## Your Role
You are the **executor and tester** for the GLP Runtime project. You run commands, show output, and implement code based on Claude Chat's architectural guidance.

## Key Context
- **Project**: GLP (Grassroots Logic Programs) - a secure concurrent logic programming language
- **Implementation Language**: Dart
- **Current State**: 101 REPL tests + 25 unit tests passing (as of Dec 2025)
- **User Expertise**: Deep understanding of GLP semantics but does not write code
- **Working Directory**: `/Users/udi/Grassroots/GLP/` (user's Mac)

## Working Modes

### Discussion Mode (DEFAULT)
- **🔴 ABSOLUTE RULE: NO ACTIONS DURING DISCUSSION** - You CANNOT proceed with ANY actions (coding, testing, running commands, git operations) until user explicitly confirms the discussion is over with phrases like "discussion over", "let's implement", "go ahead", etc.
- **🔴 "stop" MEANS STOP** - If user says "stop" or "wait", STOP IMMEDIATELY. Do not finish current action. Do not clean up. Just stop.
- **NO CODE CHANGES** - Not even small fixes
- **BRIEF RESPONSES** - Show output, explain what you see
- **STAY ON TOPIC** - Don't jump ahead
- **WAIT FOR EXPLICIT SIGNAL** - User must explicitly end discussion before you can act

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
| User's Mac | `/Users/udi/Grassroots/GLP` | User running commands |

**When giving instructions TO THE USER (merge commands, etc.), ALWAYS use Mac paths (`/Users/udi/Grassroots/GLP`).**

---

## Practical Environment Info (Linux - Claude Code)

**Before running commands, VERIFY - don't guess:**
- Run `ls` to check directories exist
- Run `pwd` to confirm current directory
- Check file locations with `ls` before referencing them

## GLP Unified Tool: The REPL

**The REPL is the only tool for all GLP operations.** Old standalone tools have been archived to `glp_runtime/bin/archive/` and must NOT be executed. This restriction is absolute - do not run archived tools even if they seem relevant. The REPL runs the complete pipeline:
1. **SRSW Analysis** → Verify single-reader/single-writer
2. **Partial Evaluation** → Evaluate defined guards
3. **Type Checking** → Verify mode/type correctness
4. **Compilation** → Generate bytecode
5. **Execution** → Run goals

**There are NO separate tools.** Old standalone tools (check_types.dart, glp_pe.dart, glpc.dart, etc.) have been archived to `glp_runtime/bin/archive/`.

### REPL Usage

**IMPORTANT:** Always use `echo -e` with pipe, NOT heredoc (`<<<`). Heredoc requires user approval for each command.

**Correct pattern (no approval needed):**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load ../programs/path/to/file.glp\ngoal.' | dart run bin/glp_repl.dart
```

**Wrong pattern (requires approval - avoid):**
```bash
dart run bin/glp_repl.dart <<< 'load file.glp'  # DON'T USE - needs approval
```

**Or compile for faster repeated testing:**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart compile exe bin/glp_repl.dart -o glp_repl
echo -e 'load ../programs/path/to/file.glp\ngoal.' | ./glp_repl
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
/Users/udi/Grassroots/GLP/
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

1. **`docs/glp-bytecode-v216-complete.md`** - NORMATIVE instruction set specification
2. **`docs/glp-runtime-spec.txt`** - NORMATIVE Dart runtime architecture
3. **`docs/typed-glp-manual.md`** - MANDATORY for GLP programming patterns and interactive protocols

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

### 🔴 MANDATORY: REPL Test Protocol for GLP System Changes

**Before ANY change to the underlying GLP system, and before any other major change:**

1. **Run REPL tests** - `bash test/full_run_repl_tests.sh`
2. **Commit and push** - Create a baseline checkpoint
3. **Only then begin implementation**

**After implementation is done:**

1. **Run REPL tests again** - `bash test/full_run_repl_tests.sh`
2. **When successful, commit and push**

This ensures:
- You have a known-good baseline to compare against
- Any test failures can be attributed to your changes (not pre-existing issues)
- You can easily revert if something breaks

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

### New Feature Test Protocol

**When a new feature or revision is implemented and tested:**
1. Add REPL tests for the new feature to `test/full_run_repl_tests.sh`
2. Tests should cover the main use cases of the feature
3. This ensures the feature continues to work as the codebase evolves

### Deferred: Dynamic RPC Testing

**Status**: Dynamic RPC (`M? # goal`) and `reduce/2` for guarded rules are currently broken.
**Reason**: Module system needs revision to integrate with type system.
**Action**: Frozen until module system revision. Do not attempt to fix these tests.

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

1. **`docs/glp-bytecode-v216-complete.md`** - Complete v2.16 instruction set
2. **`docs/glp-runtime-spec.txt`** - Dart runtime architecture

### Secondary References (Consult as Needed)

4. **WAM Paper**: `/Users/udi/Grassroots/GLP/docs/wam.pdf` - Warren's Abstract Machine
5. **GLP Spec**: `/tmp/GLP-2025/main GLP 2025.tex` - Formal GLP specification (paper source)
6. **FCP Implementation**: 
   - **Local Source**: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
   - **GitHub Mirror**: https://github.com/EShapiro2/FCP
   - **Paper**: `/Users/udi/Grassroots/GLP/docs/1-s2.0-0743106689900113-main.pdf`

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

## Bytecode Inspection Tools

### dump_bytecode.dart - Bytecode Disassembler ✅

**Location:** `/Users/udi/Grassroots/GLP/udi/dump_bytecode.dart`

**Usage:**
```bash
cd /Users/udi/Grassroots/GLP/udi
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

### Commit Message Rule
**ALWAYS use single-line commit messages.** Never use multi-line commit messages - they confuse the shell.
```bash
# CORRECT:
git commit -m "Fix Channel definition to match prelude"

# WRONG (causes shell quote issues):
git commit -m "Fix Channel definition

- Updated transitions
- Fixed modes"
```

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

### Session-Specific Branch Restrictions

**CRITICAL:** Each Claude Code session can ONLY push to its own branch (branch name includes session ID). Attempting to push to another session's branch will result in HTTP 403 error.

**If you need to continue work from a previous session's branch:**

Option 1 - Pull from previous branch, work on your own:
```bash
git fetch origin claude/<previous-session-branch>
git checkout -b claude/<your-session-branch> origin/claude/<previous-session-branch>
# Work and commit
git push -u origin claude/<your-session-branch>
```

Option 2 (Recommended) - User merges previous work to main first:
```bash
# User runs on their Mac:
cd /Users/udi/Grassroots/GLP
git checkout main
git pull origin main
git fetch origin claude/<previous-session-branch>
git merge -m "Merge previous work" origin/claude/<previous-session-branch>
git push origin main
```
Then new Claude session pulls from main and starts fresh.

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
cd /Users/udi/Grassroots/GLP
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
cd /Users/udi/Grassroots/GLP
git checkout main
git pull origin main
git fetch origin claude/<ACTUAL-BRANCH-NAME>
git merge -m "Merge claude/<ACTUAL-BRANCH-NAME> into main" origin/claude/<ACTUAL-BRANCH-NAME>
git push origin main
```
- **ALWAYS include `cd /Users/udi/Grassroots/GLP`** - user may be in wrong directory
- **ALWAYS substitute the actual branch name** - never use placeholders like `<branch-name>`
- **ALWAYS include the fetch step** - do NOT skip it

**When user asks to "merge with main" or "push to main":**
Output the EXACT commands with actual values (no placeholders):
```bash
cd /Users/udi/Grassroots/GLP
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
- don't use boxed questions (AskUserQuestion), ask in plain text conversation
- read and follow the Mandatory protocol for debugging the GLP implementation with GLP programs
- made sure claude.md points to the correct file
- read again clause.md, and if its not there update it:  NEVER proceed in implemenetation without a spec that guides it. code should be revised only if it violates the spec.  if the spec is not clear, revise it first.
- when we are discussing, do not move away from the discussion or do anything else until user agrees that the discussion is over
- 🔴 CRITICAL: You CANNOT continue working (coding, running tests, making changes) while we are discussing. You must WAIT for explicit confirmation that the discussion is over before proceeding with any implementation work.
- i want  dart run glp_repl.dart  please remember that
- always test all repl tests after a change
- NEVER work not following precisely the spec
- always offer to fetch/merge/push when finishing a task

## 🔴 ABSOLUTE RULE: Spec-First Development

**NO IMPLEMENTATION WITHOUT SPEC. NO EXCEPTIONS.**

Before writing ANY code:
1. **IDENTIFY** which spec(s) cover this area
2. **READ** the spec and quote the relevant section
3. **VERIFY** the spec is clear enough to implement from
4. **IF SPEC IS UNCLEAR OR MISSING**: STOP. Discuss with user. Clarify/write spec FIRST.
5. **ONLY THEN** implement, and the implementation MUST match the spec exactly

**If you find yourself:**
- Making the code "work" without spec backing → STOP
- Adding logic that isn't in the spec → STOP
- Fixing something by guessing what the behavior should be → STOP
- Using try-catch or null checks to "handle" cases the spec doesn't address → STOP

**The correct action is ALWAYS:**
1. STOP implementation
2. Report: "The spec does not cover X. Here's what I found: [quote spec]. We need to clarify/extend the spec before I can implement this."
3. WAIT for discussion and spec update
4. ONLY THEN proceed with implementation that matches the updated spec

## #remember Directive

When the user says `#remember <something>`, add that information to this CLAUDE.md file so it persists across sessions.

## Multi-Stage Task Persistence

**Problem:** When conversations run out of context and get compacted, multi-stage task lists are lost.

**Solution:** For any multi-stage effort, write the plan to `docs/current_plan.md`.

**Protocol:**
1. When starting a multi-stage task (3+ steps), create/update `docs/current_plan.md`
2. Format: numbered list with checkboxes, current step marked
3. Update the file as you complete each step
4. Delete the file when the task is complete

**Example format:**
```markdown
# Current Plan: [Task Name]

Started: 2026-02-01

## Steps
- [x] 1. Update papers (moded-types, glp-iclp)
- [x] 2. Update spec (guards-reference.md)
- [ ] 3. Implement in runtime ← CURRENT
- [ ] 4. Add tests
- [ ] 5. Run full test suite

## Context
[Brief description of what we're doing and why]
```

**At session start:** Check if `docs/current_plan.md` exists. If so, read it and resume from the current step.

## maGLP Development Constraints

**🔴 CRITICAL: maGLP work cannot modify core GLP implementation**

When working on maGLP (multi-agent GLP) code:
- You can ONLY modify files in `glp_runtime/lib/multiagent/` and `glp_runtime/test/multiagent/`
- You CANNOT modify core GLP files (`runner.dart`, `heap_fcp.dart`, `compiler/`, etc.) without explicit discussion and approval
- If a bug in core GLP is blocking maGLP work, STOP and report it - do not attempt workarounds or fixes
- Test infrastructure must work within the constraints of the existing GLP implementation

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

## Flutter Multiagent App Build Process

When modifying `glp_runtime` code that affects the Flutter multiagent app (`glp_multiagent`):

1. **Path dependency**: The Flutter app uses `glp_runtime` via path dependency in pubspec.yaml
2. **Clean rebuild required**: After modifying glp_runtime, you MUST do a clean Flutter rebuild:
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_multiagent
   pkill -f "glp_multiagent" 2>/dev/null  # Kill running app
   flutter clean                            # Clear cached builds
   flutter pub get                          # Re-resolve dependencies
   flutter build macos                      # Rebuild
   ```
3. **Verify rebuild**: Check the build timestamp matches your changes
4. **Clear log before testing**: `rm -f /private/tmp/glp_multiagent_trace.log`
5. **Launch and check log**: The app logs to `/private/tmp/glp_multiagent_trace.log`

**Common mistake**: Running `flutter build macos` without `flutter clean` may use cached dependencies and miss your glp_runtime changes.