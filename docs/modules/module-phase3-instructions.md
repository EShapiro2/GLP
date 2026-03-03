# Module System Phase 3: Cross-Module Type Checking — Claude Code Instructions

**Date:** 2026-02-22
**Spec:** `docs/modules/glp-module-system-spec.md` (Section 5.1)
**Implementation plan:** `docs/modules/module-implementation-plan.md`
**Discipline:** `docs/DISCIPLINE.md`
**Prerequisite:** Phase 2 complete on branch `claude/module-phase1`

---

## Overview

Phase 3 adds cross-module type checking. After this phase:
- `M # proc(X?, Y)` in a clause body is type-checked against the local `imported procedure M#proc(...)` declaration
- If no matching imported declaration exists, a type error is reported
- The type checker does NOT access module M — all checking is local
- Regular (non-remote) body goals continue to work as before

---

## How It Currently Works

The type checker (`well_typed_clause.dart`) processes body goals in `_checkBodyAtomWithTerm`. Currently:

1. `SpawnGoal` (Goal@Agent) → recurse on inner goal
2. `isBuiltinGoal(atom.functor)` → skip (no type checking)
3. Otherwise → look up procedure in `TypeEnvironment`, type-check arguments

`RemoteGoal` extends `Goal` with functor `#`. Since `#` is in `builtinGoals` (see `prelude.dart`), all remote calls are currently **skipped**.

**Phase 3 changes this:** Before the builtin check, handle `RemoteGoal` by looking up the imported declaration and type-checking against it.

---

## Key Data Structures

**`RemoteGoal`** (in `ast.dart`):
- `module`: `Term` — the module reference (e.g., `ConstTerm('math')`)
- `goal`: `Goal` — the actual call (e.g., `Goal('factorial', [5, R])`)
- `staticModuleName`: `String?` — module name if statically known
- Inherited `functor`: always `'#'`

**`ProcDecl`** (in `type_ast.dart`):
- `name`: `String` — procedure name (e.g., `'factorial'`)
- `modulePath`: `String?` — for imported procedures (e.g., `'math'`, `'ui#actors'`, or `null`)
- `imported`: `bool`
- `key`: `String` — `'name/arity'` (does NOT include module path)
- `qualifiedName`: `String` — `'path#name'` or `'name'`

**`TypeEnvironment`** (in `type_ast.dart`):
- `procedures`: `Map<String, ProcDecl>` keyed by `'name/arity'`
- `getProcedure(name, arity)`: looks up by `'name/arity'`

---

## The Problem

When the type checker encounters `math # factorial(5?, R)`:
- It needs to find `imported procedure math#factorial(Integer?, Integer)`
- But `TypeEnvironment.procedures` is keyed by `'factorial/2'`, not `'math#factorial/2'`
- Multiple modules might export `factorial/2` with different types

**Solution:** Store imported procedures with a qualified key that includes the module path.

---

## Prerequisites

**Continue on the existing branch:**

```bash
cd /Users/udi/Grassroots/GLP && git checkout claude/module-phase1
```

**Record baseline:**

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-baseline-p3.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-baseline-p3.txt 2>&1
```

---

## Step 1: Read Existing Code

Read these files before any changes:

- `docs/modules/glp-module-system-spec.md` — Section 5.1 (Static Cross-Module Calls)
- `docs/DISCIPLINE.md`
- `glp_runtime/lib/analysis/type_checker/well_typed_clause.dart` — `_checkBodyAtomWithTerm`, the central integration point
- `glp_runtime/lib/analysis/type_checker/type_ast.dart` — `ProcDecl`, `TypeEnvironment`
- `glp_runtime/lib/analysis/type_checker/prelude.dart` — `builtinGoals`, `isBuiltinGoal`
- `glp_runtime/lib/compiler/ast.dart` — `RemoteGoal` class

Do NOT modify any files in this step.

---

## Step 2: Write Failing Tests (RED)

Create `glp_runtime/test/module/module_typecheck_test.dart` with tests for:

### 2a. Remote goal type-checks against imported declaration

```dart
// Test: math # factorial(N?, R) type-checks against imported procedure math#factorial(Integer?, Integer)
// Source:
//   imported procedure math#factorial(Integer?, Integer).
//   procedure compute(Integer?, Integer).
//   compute(N, R) :- true | math # factorial(N?, R).
// Expected: type check passes (no errors)
```

### 2b. Remote goal fails without imported declaration

```dart
// Test: math # factorial(N?, R) fails if no imported declaration exists
// Source:
//   procedure compute(Integer?, Integer).
//   compute(N, R) :- true | math # factorial(N?, R).
// Expected: type error — no imported declaration for math#factorial
```

### 2c. Remote goal fails on type mismatch

```dart
// Test: type mismatch between call args and imported declaration
// Source:
//   imported procedure math#factorial(Integer?, Integer).
//   MyType ::= foo ; bar.
//   procedure compute(MyType?, Integer).
//   compute(M, R) :- true | math # factorial(M?, R).
// Expected: type error — M? has type MyType?, expected Integer?
```

### 2d. Deep module path

```dart
// Test: ui#actors#render type-checks against imported procedure ui#actors#render(...)
// Source:
//   imported procedure ui#actors#render(Integer?, Integer).
//   procedure start(Integer?, Integer).
//   start(X, Y) :- true | ui#actors # render(X?, Y).
// Expected: type check passes
```

### 2e. Imported ancestor procedure (no path)

```dart
// Test: imported procedure without path (ancestor scope) type-checks local calls
// Source:
//   imported procedure merge(Stream?, Stream?, Stream).
//   procedure combine(Stream?, Stream?, Stream).
//   combine(A, B, C) :- true | merge(A?, B?, C).
// Expected: type check passes — merge found via imported declaration
```

### 2f. Multiple imported procedures

```dart
// Test: multiple imported declarations, each checked independently
// Source:
//   imported procedure math#factorial(Integer?, Integer).
//   imported procedure io#print(String?).
//   procedure main.
//   main :- true | math # factorial(5, R), io # print(hello).
// Expected: type check passes for both
```

### 2g. Dynamic remote goal (variable module) skipped

```dart
// Test: M # goal(X) where M is a variable — skip type checking (can't resolve)
// Source:
//   procedure dispatch(_, Integer?, Integer).
//   dispatch(M, X, Y) :- true | M # compute(X?, Y).
// Expected: no type error (dynamic dispatch not type-checked)
```

Run the tests — they should FAIL (red). Commit:
```
test(modules): add Phase 3 cross-module type checking tests (red)
```

---

## Step 3: Add Qualified Key to ProcDecl

In `type_ast.dart`, add a `qualifiedKey` getter to `ProcDecl`:

```dart
/// Key for TypeEnvironment lookup, including module path for imported procedures.
/// - Local/exported: 'factorial/2'
/// - Imported with path: 'math#factorial/2'  
/// - Imported from ancestor (no path): 'factorial/2'
String get qualifiedKey => '$qualifiedName/$arity';
```

Commit:
```
refactor(type_ast): add qualifiedKey getter to ProcDecl
```

---

## Step 4: Store Imported Procedures by Qualified Key

When imported procedures are added to `TypeEnvironment`, they should be keyed by `qualifiedKey` instead of `key`. This means:

- `imported procedure math#factorial(Integer?, Integer)` → stored as `'math#factorial/2'`
- `imported procedure merge(Stream?, Stream?, Stream)` → stored as `'merge/3'` (no path, same as key)
- `procedure helper(Integer?, Integer)` → stored as `'helper/2'` (same as key)
- `exported procedure factorial(Integer?, Integer)` → stored as `'factorial/2'` (same as key)

**Where to change:** In `_buildScopeFromModule` (in `module_hierarchy.dart`) and any other place where `ProcDecl` entries are added to `TypeEnvironment`. Use `procDecl.qualifiedKey` instead of `procDecl.key` for imported procedures with a module path.

Also update `TypeEnvironment.addProcedure` to use `qualifiedKey`:

```dart
void addProcedure(ProcDecl procDecl) {
  procedures[procDecl.qualifiedKey] = procDecl;
}
```

**Important:** This should NOT break existing code because for non-imported procedures, `qualifiedKey == key`. Only imported procedures with a `modulePath` get a different key.

Commit:
```
refactor(type_ast): store procedures by qualifiedKey in TypeEnvironment
```

---

## Step 5: Handle RemoteGoal in Type Checker

In `well_typed_clause.dart`, modify `_checkBodyAtomWithTerm` to handle `RemoteGoal` BEFORE the builtin goal check:

```dart
(WellTypedResult, ModedTerm?) _checkBodyAtomWithTerm(
  ast.Goal atom,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  // Handle SpawnGoal (Goal@Agent) - type-check the inner goal
  if (atom is ast.SpawnGoal) {
    return _checkBodyAtomWithTerm(atom.innerGoal, atomIndex, dfa, env);
  }

  // Handle RemoteGoal (M # proc(...)) - type-check against imported declaration
  if (atom is ast.RemoteGoal) {
    return _checkRemoteGoal(atom, atomIndex, dfa, env);
  }

  // Skip builtin goals (true, otherwise, :=)
  // Note: '#' removed from builtinGoals — RemoteGoal handled above
  if (isBuiltinGoal(atom.functor)) {
    return (WellTypedResult.success({}), null);
  }

  // ... rest unchanged ...
}
```

Implement `_checkRemoteGoal`:

```dart
(WellTypedResult, ModedTerm?) _checkRemoteGoal(
  ast.RemoteGoal remote,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  // Dynamic dispatch (variable module) — skip type checking
  if (remote.isDynamic) {
    return (WellTypedResult.success({}), null);
  }

  // Static dispatch — look up imported declaration
  final moduleName = remote.staticModuleName!;
  final goalFunctor = remote.goal.functor;
  final goalArity = remote.goal.arity;
  
  // Look up: 'moduleName#goalFunctor/arity'
  final qualifiedKey = '$moduleName#$goalFunctor/$goalArity';
  final procDecl = env.procedures[qualifiedKey];
  
  if (procDecl == null) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(
          symbol: qualifiedKey,
          argIndex: 0,
          mode: Mode.produce,
        )]),
        'No imported declaration for $moduleName#$goalFunctor/$goalArity — '
        'add "imported procedure $moduleName#$goalFunctor(...)" to this module',
      ),
    ]), null);
  }

  // Type-check the inner goal's arguments against the imported declaration
  // Reuse existing body atom checking with the imported procedure's types
  try {
    final modedAtomTerm = producedTerm(remote.goal, procDecl, typeEnv: env);
    final result = _checkModedTermPerArg(modedAtomTerm, procDecl, dfa);
    return (result, modedAtomTerm);
  } on ArityMismatchError catch (e) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]), null);
  }
}
```

**Also:** Remove `'#'` from `builtinGoals` in `prelude.dart`. The `RemoteGoal` case now handles it before the builtin check. If for some reason a regular Goal with functor `#` exists (shouldn't happen, but defensively), it will fall through to the normal procedure lookup.

Commit:
```
feat(type_checker): type-check RemoteGoal against imported procedure declarations
```

---

## Step 6: Regression Check

Run the full test suites:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-after-p3.txt 2>&1
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh > /private/tmp/repl-after-p3.txt 2>&1
```

Compare to baseline. No new failures allowed.

**Potential regression:** Removing `#` from `builtinGoals` might cause existing programs with `#` calls to get type errors (they were previously skipped). Check if any typed test programs use `M # Goal` without imported declarations. If so, those tests need `imported procedure` declarations added — this is correct behavior, not a regression.

Commit when all tests pass:
```
fix(tests): update existing tests for cross-module type checking
```

---

## Step 7: Final Verification

Run all tests one final time. Report:
- Number of new tests added
- Final test counts
- Any issues encountered

---

## STOP HERE

Do NOT proceed to Phase 4 (dynamic load-time verification). Phase 4 requires design review.

Write a handover to `docs/handover/module-phase3-handover.md`.

---

## Rules (from DISCIPLINE.md)

- **Read before writing.** Read all files listed in Step 1 before any changes.
- **Red-green testing.** Tests fail first, then pass after implementation.
- **Commit after every revision.** Small, atomic commits.
- **No regressions.** Existing test counts must not decrease.
- **No workarounds.** If something doesn't work as expected, stop and report.
- **Spec is source of truth.** Section 5.1 — type checking is LOCAL via imported declarations.
- **Do NOT access other modules.** The type checker uses only the local imported declaration. This is the whole point.
- **Dynamic dispatch is skipped.** When the module is a variable, skip type checking — it's resolved at runtime.
