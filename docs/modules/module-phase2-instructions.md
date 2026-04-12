# Module System Phase 2: Hierarchy and Scoping — Claude Code Instructions

**Date:** 2026-02-22
**Spec:** `docs/modules/glp-module-system-spec.md`
**Implementation plan:** `docs/modules/module-implementation-plan.md`
**Discipline:** `docs/DISCIPLINE.md`
**Prerequisite:** Phase 1 complete on branch `claude/module-phase1`

---

## Overview

Phase 2 adds hierarchy awareness to the module system. After this phase:
- The compiler walks up from a `.glp` file's directory, collecting `self.glp` files
- Type definitions and procedure declarations from ancestor `self.glp` files are visible in descendant modules (implicit scoping)
- Child definitions shadow parent definitions (same name replaces)
- `self.glp` is parsed as a regular module — it can contain types, procedure declarations, and clauses
- Siblings do NOT see each other's definitions — only `self.glp` definitions are shared
- The project root is the directory passed to the compiler (no marker file)

---

## Key Design Decisions (from FCP)

These are settled — do not revisit:

1. **Project root:** The root is the directory passed to the compiler/runtime as an argument. No marker file. No walking up forever.

2. **Sibling visibility:** `agent.glp` does NOT see definitions from `mediator.glp`. Siblings only share what's in their common `self.glp`. Cross-sibling access requires `#` and `imported procedure`.

3. **`self.glp` is a regular module:** Parsed as a normal `Module` AST. Can have `-module(name)`, types, procedure declarations (exported/imported), and clauses. Its special role is only in scoping: its definitions are visible to all files in its directory and descendants.

4. **Compiler discovers the chain:** Given a target file path and a root path, the compiler walks up from the target's directory to the root, collecting `self.glp` files. The chain is assembled automatically.

---

## Prerequisites

**Continue on the existing branch:**

```bash
cd /Users/udi/Grassroots/GLP && git checkout claude/module-phase1
```

**Record baseline:**

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-baseline-p2.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-baseline-p2.txt 2>&1
```

Read both files. Note passing test counts.

---

## Step 1: Read Existing Code

Read these files before any changes:

- `docs/modules/glp-module-system-spec.md` — the spec (Sections 2, 3, 5.3)
- `docs/DISCIPLINE.md` — development rules
- `glp_runtime/lib/runtime/module_loader.dart` — current loader (flat, no hierarchy)
- `glp_runtime/lib/runtime/loaded_module.dart` — current module representation
- `glp_runtime/lib/analysis/type_checker/type_ast.dart` — `TypeEnvironment`, `TypeDef`, `ProcDecl`
- `glp_runtime/lib/compiler/parser.dart` — `parseModule()` method
- `glp_runtime/lib/analysis/type_checker/prelude.dart` — prelude types and procedures

Do NOT modify any files in this step.

---

## Step 2: Write Failing Tests (RED)

Create `glp_runtime/test/module/module_hierarchy_test.dart` with tests for:

### 2a. `self.glp` chain discovery

```dart
// Test: given a file path and root path, discover the self.glp chain
// Setup: create temp directory structure:
//   root/
//     self.glp          — defines TypeA
//     sub/
//       self.glp        — defines TypeB
//       module.glp      — the target file
// Expected: chain is [root/self.glp, root/sub/self.glp] in that order (root first)
```

### 2b. Type scope assembly from ancestor chain

```dart
// Test: types from ancestor self.glp files are visible in descendant module
// Setup:
//   root/self.glp contains: Response ::= accept(Channel) ; no.
//   root/sub/module.glp contains: procedure foo(Response?, Constant).
// Expected: module.glp's type environment includes Response from root/self.glp

// Test: types from multiple ancestor levels
// Setup:
//   root/self.glp contains: Response ::= accept(Channel) ; no.
//   root/sub/self.glp contains: AgentContent ::= befriend(Constant, Response?).
//   root/sub/module.glp contains: procedure foo(AgentContent?, Response?).
// Expected: module.glp sees both Response (from root) and AgentContent (from sub)
```

### 2c. Shadowing

```dart
// Test: child self.glp shadows parent's type definition
// Setup:
//   root/self.glp contains: Response ::= accept(Channel) ; no.
//   root/sub/self.glp contains: Response ::= accept(Channel) ; no ; maybe.
//   root/sub/module.glp uses Response
// Expected: module.glp sees the child's 3-alternative Response, not parent's 2-alternative

// Test: module's own type shadows ancestor's
// Setup:
//   root/self.glp contains: Foo ::= a ; b.
//   root/module.glp contains: Foo ::= x ; y ; z.
// Expected: module.glp's Foo has 3 alternatives (its own definition)
```

### 2d. Sibling isolation

```dart
// Test: sibling files do NOT see each other's types
// Setup:
//   root/self.glp contains: SharedType ::= a ; b.
//   root/agent.glp contains: AgentType ::= x ; y.
//   root/mediator.glp references AgentType
// Expected: mediator.glp does NOT have AgentType in scope (only SharedType from self.glp)
```

### 2e. Type-only `self.glp`

```dart
// Test: self.glp with only type definitions (no procedures) works
// Setup:
//   root/self.glp contains only type definitions
//   root/module.glp uses those types
// Expected: types are visible in module.glp
```

### 2f. Prelude as root ancestor

```dart
// Test: prelude types are always visible (root of all chains)
// Setup:
//   root/module.glp uses Integer, Constant, Stream (prelude types)
// Expected: prelude types available without any self.glp
```

### 2g. Procedure declarations from ancestor self.glp

```dart
// Test: exported procedure declarations in self.glp are visible to descendants
// Setup:
//   root/self.glp contains: exported procedure shared_proc(Integer?, Integer).
//   root/module.glp references shared_proc
// Expected: module.glp sees the procedure declaration from self.glp
```

Run the tests — they should FAIL (red). Commit:
```
test(modules): add Phase 2 hierarchy and scoping tests (red)
```

---

## Step 3: Implement `self.glp` Chain Discovery

Create a utility function (in `module_loader.dart` or a new file like `module_hierarchy.dart`) that:

1. Takes a target file path and a root directory path
2. Walks up from the target file's directory to the root
3. At each directory level, checks if `self.glp` exists
4. Returns the chain as a list of file paths, ordered root-first (outermost ancestor first, innermost last)

Example:
```
discoverSelfChain(
  targetFile: '/project/ui/actors.glp',
  rootDir: '/project'
)
→ ['/project/self.glp', '/project/ui/self.glp']  // if both exist
```

If a `self.glp` doesn't exist at a level, skip it. The chain may be empty.

Commit:
```
feat(modules): implement self.glp chain discovery
```

---

## Step 4: Implement Type Scope Assembly

Create a function that:

1. Takes the `self.glp` chain (list of file paths) and the target module's own types
2. Parses each `self.glp` file into a `Module` AST
3. Builds a `TypeEnvironment` by layering scopes:
   - Start with the prelude (`TypeEnvironment` from `prelude.dart`)
   - Merge root `self.glp`'s types and procedure declarations
   - Merge next level's `self.glp`'s types and procedure declarations (shadows parent)
   - Continue to innermost `self.glp`
   - Finally merge the target module's own types and declarations (shadows all ancestors)
4. Returns the assembled `TypeEnvironment`

**Shadowing rule:** When merging, if a child defines a type with the same name as a parent, the child's definition replaces the parent's. Use `Map` semantics — later entries overwrite earlier ones.

**Important:** Both `TypeDef` entries AND `ProcDecl` entries from ancestor `self.glp` files should be included. Types and procedure declarations are both part of the scope.

Commit:
```
feat(modules): implement type scope assembly from self.glp chain
```

---

## Step 5: Integrate with Compilation Pipeline

Modify the compilation path so that when compiling a module:

1. The compiler receives (or discovers) the root directory and target file path
2. It calls chain discovery to find the `self.glp` chain
3. It calls scope assembly to build the effective `TypeEnvironment`
4. The type checker uses this assembled environment instead of just the module's own types

This means the type checker's entry point needs to accept an external type environment (the assembled ancestor scope) that the module's own definitions are merged into.

**Where to integrate:** Find where `TypeEnvironment` is currently built for type checking (likely in the type checker or compilation pipeline). Add the ancestor scope assembly before the module's own types are added.

**For testing:** Provide a way to compile a module with an explicit root directory, so tests can set up temp directory structures and compile against them.

Commit:
```
feat(modules): integrate hierarchy scoping into compilation pipeline
```

---

## Step 6: Regression Check

Run the full test suites:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-after-p2.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-after-p2.txt 2>&1
```

Compare to baseline. No new failures allowed.

Commit when all tests pass:
```
fix(tests): ensure Phase 2 hierarchy changes cause no regressions
```

---

## Step 7: Final Verification

Run all tests one final time. Verify:
- All new Phase 2 tests pass (green)
- All Phase 1 tests still pass
- All existing tests still pass
- Test counts match or exceed baseline

Report:
- Number of new tests added
- Final test counts
- Any issues encountered

---

## STOP HERE

Do NOT proceed to Phase 3 (cross-module type checking). Phase 3 requires design review.

Write a handover to `docs/handover/module-phase2-handover.md`.

---

## Rules (from DISCIPLINE.md)

- **Read before writing.** Read all files listed in Step 1 before any changes.
- **Red-green testing.** Tests fail first, then pass after implementation.
- **Commit after every revision.** Small, atomic commits.
- **No regressions.** Existing test counts must not decrease.
- **No workarounds.** If something doesn't work as expected, stop and report.
- **Spec is source of truth.** `docs/modules/glp-module-system-spec.md` defines what to implement.
- **Follow user's explicit instructions exactly.** Do not reinterpret or "improve."
- **self.glp is a regular module.** Parse it with `parseModule()`. No special parsing.
- **Siblings are isolated.** Only `self.glp` definitions are shared, never sibling file definitions.
