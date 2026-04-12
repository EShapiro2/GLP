# Bug Report: Dart Test Failures — UnknownTypeError: Stream?

**Date:** 2026-03-13
**Reporter:** Claude Chat (analysis session with Udi)
**Severity:** Medium (does not affect REPL or runtime; affects Dart unit tests only)

## Summary

53 Dart unit tests fail with `UnknownTypeError: Stream?` (and similar) when building the ProgramDFA. The failures are concentrated in CSSG `ui_mediator` tests and multiagent tests. The REPL pipeline (428 tests) is unaffected.

## Root Cause

The `buildProgramDFA` function in `program_dfa.dart` constructs DFA states from the `TypeEnvironment`. It creates states for every type name in `env.types` (lines ~240-244):

```dart
for (final entry in env.types.entries) {
    final typeName = entry.key;
    states[typeName] = DFAState(typeName, isDual: false, isFinal: false);
    states['$typeName?'] = DFAState(typeName, isDual: true, isFinal: false);
}
```

When a procedure declaration references a parametric type like `Stream(FriendMsg)?`, the `_getFullTypeName` function (line ~370) extracts just the base name:

```dart
String _getFullTypeName(TypeExpr typeExpr) {
    if (typeExpr is TypeRef) {
        return typeExpr.isInput ? '${typeExpr.name}?' : typeExpr.name;
    }
    ...
}
```

For `Stream(FriendMsg)?`, this returns `"Stream?"`. But `Stream` is a parametric template defined in the prelude — it is NOT in `env.types` as a monomorphic type. Only its expanded instantiations (e.g., `Stream<FriendMsg>`) would be in `env.types` after expansion. So `states["Stream?"]` is null, and `buildProgramDFA` throws `UnknownTypeError: Stream?`.

## Why REPL Works But Dart Tests Fail

The REPL pipeline runs `expandParameterizedTypes` (in `param_expansion.dart`) on each module BEFORE constructing the `TypeEnvironment` and `ProgramDFA`. This expansion rewrites:
- `Stream(FriendMsg)` → `Stream<FriendMsg>` (a new monomorphic TypeDef)
- Procedure declarations referencing `Stream(FriendMsg)?` → `Stream<FriendMsg>?`

After expansion, `env.types` contains `"Stream<FriendMsg>"` as a key, and `_getFullTypeName` returns `"Stream<FriendMsg>?"`, which resolves correctly.

Some Dart unit tests construct a `TypeEnvironment` directly (by parsing a `.glp` file and extracting types/procedures) WITHOUT running `expandParameterizedTypes` first. These tests worked before the parametric types refactoring because all types were monomorphic (e.g., `FriendStream`, `FriendChannel`). After the refactoring replaced these with parametric instantiations (`Stream(FriendMsg)`, `Channel(Stream(FriendMsg), Stream(FriendMsg))`), the same test path now encounters unresolved parametric references.

## Affected Tests

Tests that load CSSG/CSSN module files and build a ProgramDFA without running `expandParameterizedTypes`. The error manifests as:
- `UnknownTypeError: Stream?` — from `Stream(X)?` references in procedure declarations
- `UnknownTypeError: Channel?` — from `Channel(In, Out)?` references
- Possibly `UnknownTypeError: DiffList?` or `UnknownTypeError: OpenStream?`

## Fix Options

### Option A: Fix the test infrastructure
Ensure every Dart test that builds a `TypeEnvironment` from a parsed module runs `expandParameterizedTypes` first. This is the correct fix — it makes the test pipeline match the REPL pipeline.

Find all test files that call `buildProgramDFA` or construct a `TypeEnvironment` and ensure they call `expandParameterizedTypes(module, externalTemplates: preludeTemplates)` before extracting types. The prelude templates (`Stream`, `Channel`, `DiffList`, `OpenStream`) must be passed as `externalTemplates` so the expansion can resolve them.

### Option B: Make `buildProgramDFA` resilient
Add a fallback in `_buildProcedureAutomaton` that, when encountering an unknown type name containing type arguments (detectable by checking for `<` in the name or by checking `TypeRef.typeArgs`), either skips the procedure or maps to a wildcard state. This is a band-aid, not a fix.

### Recommended: Option A

## Files Involved

- `glp_runtime/lib/analysis/type_checker/program_dfa.dart` — `_getFullTypeName`, `_buildProcedureAutomaton`, `buildProgramDFA`
- `glp_runtime/lib/analysis/type_checker/param_expansion.dart` — `expandParameterizedTypes` (the expansion that must run first)
- `glp_runtime/lib/analysis/type_checker/type_ast.dart` — `TypeRef.typeArgs`, `TypeEnvironment`
- Test files that construct TypeEnvironment/ProgramDFA without expansion (to be identified by searching for `buildProgramDFA` and `TypeEnvironment` in test/)

## Reproduction

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test 2>&1 | grep "UnknownTypeError"
```

## Baseline Context

- Before CSSN parametric types refactoring: 33 Dart test failures (pre-existing, different causes)
- After CSSN parametric types refactoring: 53 Dart test failures (+20, all `UnknownTypeError`)
- REPL tests: 428/428 pass (unaffected)
