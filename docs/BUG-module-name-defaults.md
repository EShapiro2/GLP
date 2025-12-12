# Bug: Module name defaults not working (PARTIALLY FIXED)

**Date:** December 2025
**Severity:** Low (workaround available)
**Component:** REPL module system
**Status:** Partially fixed - `-module(name)` now defaults to filename, but `-import([...])` is still required

## Summary

~~Module RPC requires explicit `-module(name).` declaration.~~ **FIXED:** Module name now defaults to filename.

**Still Required:** The importing module must have `-import([module_names]).` declaration for module context to be built.

## Current Behavior

In `glp_runtime/bin/glp_repl.dart` lines 579-581:

```dart
final moduleMatch = RegExp(r'-module\((\w+)\)\.').firstMatch(source);
if (moduleMatch == null) {
  return null;  // Not a module file
}
```

Without `-module(name).` declaration, the file returns `null` and is not registered in `loadedModules`. RPC calls then fail silently - output variables remain unbound with no error message.

## Expected Behavior

If no `-module(name).` declaration exists, the module name should default to the filename without extension:
- `math_module.glp` → module name `math_module`
- `utils.glp` → module name `utils`

## Test Case

**File: math_module.glp** (no declarations)
```glp
%% math_module.glp - Simple math module

factorial(0, 1).
factorial(N, F?) :-
    N? > 0 |
    N1 := N? - 1,
    factorial(N1?, F1),
    F := N? * F1?.
```

**File: main_module.glp** (no declarations)
```glp
%% main_module.glp - Demonstrates cross-module RPC

compute(Result?) :-
    math_module # factorial(5, F),
    Result = result(factorial(F?)).
```

**REPL session:**
```
AofGLP/book_examples/modules/math_module.glp
AofGLP/book_examples/modules/main_module.glp
compute(R).
```

**Actual result:** `R = result(factorial(X3))` - F is unbound
**Expected result:** `R = result(factorial(120))`

## Suggested Fix

In `_extractModuleInfo()`, if no `-module(name).` found, derive name from filename:

```dart
ModuleInfo? _extractModuleInfo(String source, BytecodeProgram program, String filename) {
  // Extract -module(name)
  final moduleMatch = RegExp(r'-module\((\w+)\)\.').firstMatch(source);

  String moduleName;
  if (moduleMatch != null) {
    moduleName = moduleMatch.group(1)!;
  } else {
    // Default to filename without extension
    moduleName = path.basenameWithoutExtension(filename);
  }

  // ... rest of function
}
```

Also need to pass the filename to this function from where it's called.

## Workaround

Add explicit module declarations:

```glp
-module(math_module).
-export([factorial/2]).

factorial(0, 1).
...
```

## Related Files

- `glp_runtime/bin/glp_repl.dart` - REPL module loading
- `AofGLP/book_examples/modules/` - Test files for this bug
