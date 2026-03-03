# Module System Phase 1: Syntax — Claude Code Instructions

**Date:** 2026-02-22
**Spec:** `docs/modules/glp-module-system-spec.md`
**Implementation plan:** `docs/modules/module-implementation-plan.md`
**Discipline:** `docs/DISCIPLINE.md`

---

## Overview

Phase 1 changes the module syntax. After this phase:
- `exported procedure p(T1?, T2)` declares a public procedure
- `imported procedure mod#p(T1?, T2)` declares a cross-module dependency with full type signature
- `imported procedure p(T1?, T2)` declares an ancestor-scope dependency (no path)
- `-export([...])` and `-import([...])` are no longer recognized
- `-module(name).` remains (optional, defaults to filename)
- `Module # Goal` call syntax remains unchanged
- All existing tests continue to pass (existing programs don't use `-export`/`-import`)

---

## Prerequisites

**Create a working branch:**

```bash
cd /Users/udi/Grassroots/GLP && git checkout -b claude/module-phase1
```

All work happens on this branch. Do not touch `main`.

**⚠️ You are working on the user's live filesystem.** The branch is your safety net. If anything goes wrong, `git checkout main` restores all files. Commit frequently so you can revert individual changes.

**Before any code changes, record the baseline:**

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-baseline.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-baseline.txt 2>&1
```

Read both files. Note passing test counts. These counts must not decrease.

---

## Step 1: Read Existing Code

Read these files to understand the current module parsing:

- `glp_runtime/lib/compiler/lexer.dart` — current token types
- `glp_runtime/lib/compiler/parser.dart` — current declaration parsing
- `glp_runtime/lib/compiler/ast.dart` — current AST nodes
- `glp_runtime/test/module/` — existing module tests

Also read:
- `docs/modules/glp-module-system-spec.md` — the spec you are implementing
- `docs/DISCIPLINE.md` — development rules (follow strictly)

Do NOT modify any files in this step.

---

## Step 2: Write Failing Tests (RED)

Create `glp_runtime/test/module/module_syntax_v2_test.dart` with tests for:

### 2a. `exported procedure` parsing

```dart
// Test: exported procedure declaration is parsed correctly
// Input: 'exported procedure factorial(Integer?, Integer).'
// Expected: ProcedureDeclaration with exported=true, name='factorial', 
//           arg types [Integer?, Integer]

// Test: plain procedure declaration has exported=false
// Input: 'procedure helper(Integer?, Integer).'
// Expected: ProcedureDeclaration with exported=false

// Test: exported procedure with moded types
// Input: 'exported procedure agent(Constant?, AgentChannel?, NetChannel?).'
// Expected: ProcedureDeclaration with exported=true, correct arg types
```

### 2b. `imported procedure` parsing

```dart
// Test: imported procedure with module path
// Input: 'imported procedure social#agent(Constant?, AgentChannel?).'
// Expected: ProcedureDeclaration with imported=true, 
//           modulePath='social', name='agent', arg types [Constant?, AgentChannel?]

// Test: imported procedure with deep path
// Input: 'imported procedure ui#actors#render(Widget?, Screen).'
// Expected: ProcedureDeclaration with imported=true,
//           modulePath='ui#actors', name='render', arg types [Widget?, Screen]

// Test: imported procedure from ancestor scope (no path)
// Input: 'imported procedure merge(Stream?, Stream?, Stream).'
// Expected: ProcedureDeclaration with imported=true, modulePath=null,
//           name='merge', arg types [Stream?, Stream?, Stream]

// Test: imported procedure with qualified types using #
// Input: 'imported procedure social#agent(Constant?, social#AgentChannel?).'
// Expected: types reference social#AgentChannel correctly
```

### 2c. Rejection of old syntax

```dart
// Test: -export([...]) is rejected (parse error)
// Input: '-export([factorial/2]).'
// Expected: parse error

// Test: -import([...]) is rejected (parse error)
// Input: '-import([math]).'
// Expected: parse error
```

### 2d. `-module(name).` still works

```dart
// Test: -module(name) still parses
// Input: '-module(math).'
// Expected: ModuleDeclaration with name='math'
```

### 2e. `Module # Goal` still works

```dart
// Test: remote goal parsing unchanged
// Input: 'math # factorial(5, R)'
// Expected: RemoteGoal with module='math', goal=factorial(5,R)
```

### 2f. Type-only file (no procedures)

```dart
// Test: file with only type definitions and no procedures parses successfully
// Input: 'Response ::= accept(Channel) ; no.\nAgentContent ::= befriend(Constant, Response?).'
// Expected: Module with type definitions, empty procedure list, no errors
```

Run the tests — they should FAIL (red). Commit:
```
test(modules): add Phase 1 syntax tests for exported/imported procedure (red)
```

---

## Step 3: Modify AST (GREEN — part 1)

### 3a. Add visibility fields to procedure declaration

In `ast.dart`, find the procedure declaration AST node. Add:
- `bool exported` (default `false`) — this is an exported procedure
- `bool imported` (default `false`) — this is an imported procedure declaration
- `String? modulePath` (default `null`) — for imported procedures, the module path (e.g., `'social'` or `'ui#actors'`). `null` means ancestor scope.

A procedure declaration is exactly one of: plain (private), exported, or imported. It is an error for both `exported` and `imported` to be `true`.

### 3b. Remove ExportDeclaration and ImportDeclaration

If `ExportDeclaration` and `ImportDeclaration` AST nodes exist, remove them. If other code references them, update those references (they should produce errors, which you fix).

### 3c. Update Module AST

The `Module` AST node should no longer have export/import lists. It has:
- `String? name` (from `-module`)
- List of type definitions
- List of procedure declarations (each with visibility: plain, exported, or imported)

Commit after AST compiles cleanly:
```
refactor(ast): add exported/imported fields to procedure, remove export/import nodes
```

---

## Step 4: Modify Parser (GREEN — part 2)

### 4a. Parse `exported procedure`

When the parser sees the token `exported` followed by `procedure`, parse it as a procedure declaration with `exported=true`.

The keyword `exported` should be recognized only in this position (before `procedure`). It is NOT a reserved word in other contexts.

### 4b. Parse `imported procedure`

When the parser sees `imported` followed by `procedure`, parse the procedure name which may include a `#`-separated module path:

- `imported procedure merge(...)` — `imported=true`, `modulePath=null`, `name='merge'`
- `imported procedure social#agent(...)` — `imported=true`, `modulePath='social'`, `name='agent'`
- `imported procedure ui#actors#render(...)` — `imported=true`, `modulePath='ui#actors'`, `name='render'`

The last `#`-separated component is the procedure name. Everything before it is the module path.

Types in the argument list may also use `#` for qualified type references (e.g., `social#AgentChannel?`). The type parser must handle this.

The keyword `imported` should be recognized only in this position (before `procedure`). It is NOT a reserved word in other contexts.

### 4c. Reject `-export` and `-import`

If the parser encounters `-export(` or `-import(`, emit a parse error: "The -export() declaration is no longer supported. Use 'exported procedure' instead." / "The -import() declaration is no longer supported. Use 'imported procedure' instead."

### 4d. Allow type-only files

Ensure the parser does not require at least one procedure. A file with only type definitions is valid.

### 4e. Keep `-module(name).` and `Module # Goal` unchanged

No changes needed here — just verify they still work.

Run the new tests — they should now PASS (green). Commit:
```
feat(parser): implement exported/imported procedure syntax, reject old export/import
```

---

## Step 5: Regression Check

Run the full test suites:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-after.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-after.txt 2>&1
```

Compare to baseline. If any tests that passed before now fail:

1. **STOP** — do not proceed
2. Identify what broke and why
3. Fix without breaking the new tests
4. Re-run until all tests pass

If existing module tests used `-export`/`-import` syntax, those tests need updating to the new syntax. This is expected — update them and note it in the commit.

Commit when all tests pass:
```
fix(tests): update existing module tests to new syntax
```

---

## Step 6: Final Verification and Commit

Run all tests one final time. Verify counts match or exceed baseline.

If everything passes, commit and report:
- Number of new tests added
- Number of existing tests updated
- Final test counts (Dart unit tests, REPL tests)
- Any issues encountered

---

## STOP HERE

Do NOT proceed to Phase 2 (hierarchy/scoping). Phase 2 requires design review.

Write a handover to `docs/handover/module-phase1-handover.md` following the format in `docs/DISCIPLINE.md` Part III.

---

## Rules (from DISCIPLINE.md)

- **Read before writing.** Read all files listed in Step 1 before any changes.
- **Red-green testing.** Tests fail first, then pass after implementation.
- **Commit after every revision.** Small, atomic commits.
- **No regressions.** Existing test counts must not decrease.
- **No workarounds.** If something doesn't work as expected, stop and report.
- **Spec is source of truth.** `docs/modules/glp-module-system-spec.md` defines what to implement.
- **Follow user's explicit instructions exactly.** Do not reinterpret or "improve."
