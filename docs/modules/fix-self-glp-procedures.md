# Fix: Compile self.glp Procedures in Project Linker

**Date**: 2026-03-11
**Spec**: `docs/modules/glp-project-compilation-spec.md` (updated 2026-03-11)
**Paper**: Moded-Types `sections/modules.tex` (updated 2026-03-11)

## Problem

`project_linker.dart` skips `self.glp` files entirely during project discovery (line ~70: `if (filename == 'self.glp') continue;`). This means any procedures defined in a module-level `self.glp` are never compiled to bytecode. Per the updated spec, `self.glp` files contribute both types AND procedures to the ancestor scope. Their procedures must be compiled, renamed, and available to sibling modules.

## Design

Per the spec (§3.1–3.3):
- `self.glp` procedures are **renamed** like any other module (prevents collisions in nested hierarchies)
- Sibling modules call them **without qualification** in source — the linker resolves to the renamed form
- Resolution order: local procedure → ancestor self.glp chain (inner first) → root prelude
- Ancestor self.glp procedures do NOT require `imported` declarations — the type checker already has them in the ancestor scope (same as types and root prelude procedures like `merge`)

---

## Step 1: Create Test Programs (BEFORE implementation)

Per DISCIPLINE.md §2.4: write tests first, both positive and negative.

### Positive Test 1: self.glp shared procedure

Create `programs/tests/module_self_procs/`:

**self.glp:**
```glp
-module(selfprocs).

procedure shared_double(Integer?, Integer).
shared_double(X, Y?) :- Y := X? * 2.
```

**worker.glp:**
```glp
-module(worker).

exported procedure do_work(Integer?, Integer).
do_work(X, Y?) :- shared_double(X?, Y).
```

**boot.glp:**
```glp
-module(boot).

imported procedure worker#do_work(Integer?, Integer).

exported procedure test_self_proc(Integer?, Integer).
test_self_proc(X, Y?) :- worker # do_work(X?, Y).
```

Worker calls `shared_double` unqualified — no `imported` declaration needed because it's inherited from ancestor `self.glp`, just like types and root prelude procedures.

Expected: `test_self_proc(5, R).` → `R = 10`.

### Positive Test 2: self.glp shadowing in nested hierarchy

Create `programs/tests/module_self_shadow/`:

**self.glp:**
```glp
-module(shadow_root).

Tag ::= outer ; inner.

procedure get_tag(Tag).
get_tag(outer).
```

**sub/self.glp:**
```glp
-module(shadow_sub).

procedure get_tag(Tag).
get_tag(inner).
```

**sub/inner.glp:**
```glp
-module(inner).

exported procedure inner_tag(Tag).
inner_tag(T?) :- get_tag(T).
```

**boot.glp:**
```glp
-module(boot).

imported procedure inner#inner_tag(Tag).

exported procedure test_shadow(Tag, Tag).
test_shadow(OuterT?, InnerT?) :-
    get_tag(OuterT),
    inner # inner_tag(InnerT).
```

Boot sees root `self.glp`'s `get_tag` → `outer`. Inner sees `sub/self.glp`'s `get_tag` → `inner`.

Expected: `test_shadow(X, Y).` → `X = outer, Y = inner`.

### Positive Test 3: Local procedure shadows self.glp

Create `programs/tests/module_self_local_shadow/`:

**self.glp:**
```glp
-module(localshadow).

Result ::= from_self ; from_local.

procedure helper(Result).
helper(from_self).
```

**worker.glp:**
```glp
-module(worker).

procedure helper(Result).
helper(from_local).

exported procedure test_local(Result).
test_local(R?) :- helper(R).
```

**boot.glp:**
```glp
-module(boot).

imported procedure worker#test_local(Result).

exported procedure test_local_shadow(Result).
test_local_shadow(R?) :- worker # test_local(R).
```

Worker's local `helper` shadows self.glp's `helper`.

Expected: `test_local_shadow(R).` → `R = from_local`.

### Negative Test: Type error in self.glp procedure

Create `programs/tests/module_self_type_error/`:

**self.glp:**
```glp
-module(typeerr).

procedure bad_proc(Integer?, Integer).
bad_proc(X, hello).
```

**worker.glp:**
```glp
-module(worker).

exported procedure test(Integer?, Integer).
test(X, Y?) :- bad_proc(X?, Y).
```

Expected: Loading the project should fail with a type error (`hello` is a String, arg 2 expects Integer).

### Test Script Entries

Add to `test/run_all_tests.sh` a new Section I after Section H:

```bash
# =============================================================================
# Section I: self.glp Procedure Tests
# =============================================================================
echo "=== Section I: self.glp Procedure Tests ==="
echo ""

SELFPROC_TESTS="$GLP_DIR/programs/tests"

# --- I1: self.glp shared procedure ---
echo "--- I1: self.glp shared procedure ---"
i1=$($DART run "$REPL" <<HEREDOC
$SELFPROC_TESTS/module_self_procs
test_self_proc(5, R).
:quit
HEREDOC
2>&1)

check "self.glp shared proc loads" "Loaded project" "$i1"
check "self.glp shared proc result" "R = 10" "$i1"

# --- I2: self.glp shadowing ---
echo "--- I2: self.glp shadowing ---"
i2=$($DART run "$REPL" <<HEREDOC
$SELFPROC_TESTS/module_self_shadow
test_shadow(X, Y).
:quit
HEREDOC
2>&1)

check "self.glp shadow loads" "Loaded project" "$i2"
check "self.glp shadow outer" "X = outer" "$i2"
check "self.glp shadow inner" "Y = inner" "$i2"

# --- I3: Local shadows self.glp ---
echo "--- I3: Local shadows self.glp ---"
i3=$($DART run "$REPL" <<HEREDOC
$SELFPROC_TESTS/module_self_local_shadow
test_local_shadow(R).
:quit
HEREDOC
2>&1)

check "local shadow loads" "Loaded project" "$i3"
check "local shadow result" "R = from_local" "$i3"

# --- I4: Type error in self.glp (negative) ---
echo "--- I4: Type error in self.glp (negative) ---"
i4=$($DART run "$REPL" <<HEREDOC
$SELFPROC_TESTS/module_self_type_error
:quit
HEREDOC
2>&1)

check "self.glp type error rejected" "Type checking failed\|type.*error\|Error" "$i4"
check_not "self.glp type error not loaded" "Loaded project" "$i4"
```

---

## Step 2: Implementation

All changes are in `glp_runtime/lib/compiler/project_linker.dart`.

### 2.1 Remove the self.glp skip in `discoverProject()`

Remove these two lines (~70-71):
```dart
// Skip self.glp (type definitions only, no procedures)
if (filename == 'self.glp') continue;
```

### 2.2 Fix module naming for self.glp without `-module()`

When the filename is `self.glp` and there's no `-module()`, derive the name from the parent directory.

```dart
final moduleName = module.name ??
    (filename == 'self.glp'
        ? _moduleNameFromDirPath(file.parent.path)
        : _moduleNameFromFilename(filename));
```

Add helper:
```dart
/// Extract module name from directory path (last component).
String _moduleNameFromDirPath(String dirPath) {
  final parts = dirPath.split(Platform.pathSeparator);
  return parts.last;
}
```

### 2.3 Add `isSelfGlp` field to `DiscoveredModule`

```dart
class DiscoveredModule {
  final String filePath;
  final String moduleName;
  final Module ast;
  final TypeEnvironment ancestorScope;
  final bool isSelfGlp;

  DiscoveredModule({
    required this.filePath,
    required this.moduleName,
    required this.ast,
    required this.ancestorScope,
    this.isSelfGlp = false,
  });
}
```

Set `isSelfGlp: true` when discovering:
```dart
modules.add(DiscoveredModule(
  filePath: file.path,
  moduleName: moduleName,
  ast: module,
  ancestorScope: ancestorScope,
  isSelfGlp: filename == 'self.glp',
));
```

### 2.4 Build ancestor self.glp procedure map in `linkProject()`

Before the main processing loop:

```dart
// Build ancestor self.glp procedure map for each module.
// Maps module name → { sig → ancestorModuleName } (inner-most ancestor wins).
final selfGlpModules = modules.where((m) => m.isSelfGlp).toList();
final ancestorSelfProcs = <String, Map<String, String>>{};

for (final mod in modules) {
  final modDir = File(mod.filePath).parent.absolute.path;
  final procs = <String, String>{}; // sig → ancestorModuleName

  // Walk self.glp modules from inner-most to outer-most.
  // Inner-most wins (first entry in putIfAbsent).
  // Sort by path length descending (longer path = more nested = inner).
  final ancestors = selfGlpModules
      .where((s) {
        if (identical(s, mod)) return false; // skip self
        final selfDir = File(s.filePath).parent.absolute.path;
        return modDir.startsWith(selfDir);
      })
      .toList()
    ..sort((a, b) => b.filePath.length.compareTo(a.filePath.length));

  for (final selfMod in ancestors) {
    for (final proc in selfMod.ast.procedures) {
      final sig = '${proc.name}/${proc.arity}';
      procs.putIfAbsent(sig, () => selfMod.moduleName);
    }
  }

  ancestorSelfProcs[mod.moduleName] = procs;
}
```

### 2.5 Pass ancestor map to `_resolveGoal()`

Change the signature:
```dart
Goal _resolveGoal(Goal goal, String moduleName, Set<String> localSigs,
    Map<String, String> ancestorSelfProcs)
```

Add ancestor check between local and prelude:
```dart
final sig = '${goal.functor}/${goal.arity}';
if (localSigs.contains(sig)) {
  return Goal('$moduleName:${goal.functor}', goal.args, goal.line, goal.column);
}

// Check ancestor self.glp procedures
final ancestorModule = ancestorSelfProcs[sig];
if (ancestorModule != null) {
  return Goal('$ancestorModule:${goal.functor}', goal.args, goal.line, goal.column);
}

// Prelude/stdlib/body kernel — leave unchanged
return goal;
```

Update call sites in `linkProject()` (both in the main loop and SpawnGoal recursion).

### 2.6 Handle self.glp's own calls to its ancestor chain

When a `self.glp` file itself calls a procedure from an outer `self.glp`, those calls also need resolution. The ancestor map built in §2.4 already handles this because the `self.glp` is a module like any other, and `identical(s, mod)` skips only itself, not its ancestors.

### 2.7 Update doc comments

Update the file's doc comment and `discoverProject()`'s doc comment.

---

## Step 3: Validation

1. Run `bash test/run_all_tests.sh`. All existing 390 tests + new Section I tests must pass.
2. Commit after tests pass.

## What NOT to change

- Root `programs/self.glp` is loaded separately by `_loadStdlib()` in `glp_engine.dart`
- `_buildAncestorScope()` in the linker still builds the type scope from self.glp files
- Type checking is unchanged — ancestor proc decls are already in the scope
