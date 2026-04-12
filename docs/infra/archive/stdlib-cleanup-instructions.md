# Infrastructure Cleanup: Remove `stdlib` Indirection

**Date**: 2026-03-11  
**Author**: Claude Chat  
**Status**: Ready for Claude Code execution

---

## Problem

The GLP engine locates `programs/self.glp` via a nonexistent `programs/stdlib` directory. The REPL constructs a path to this phantom directory, passes it to `GlpEngine(stdlibDir: ...)`, and the engine does `stdlibDir.replaceAll('/stdlib', '/self.glp')` — a string manipulation hack. There is no `programs/stdlib` directory. Additionally, `File('../programs/stdlib').absolute.path` does not canonicalize `..` segments, breaking when the REPL is invoked from different working directories.

## Goal

1. Replace all `stdlibDir` parameters/fields with `rootSelfGlpPath` — the absolute, canonical path to `programs/self.glp`.
2. Remove the `.replaceAll('/stdlib', '/self.glp')` hack.
3. Resolve paths using `Platform.script.resolve(...)` in the REPL (relative to script location, not CWD).
4. All callers pass the actual absolute path to `programs/self.glp`. No indirection.
5. All tests pass (399/399 REPL tests).

## Principle

From `docs/infra/absolute-paths.md`: All file paths must be resolved to absolute at the entry point (REPL, test script, Flutter app, etc.) before passing to the engine. The engine and everything below it must never resolve relative paths.

---

## Scope: Files to Modify

### Core (glp_runtime)

**1. `glp_runtime/bin/glp_repl.dart`** (the entry point)

Current:
```dart
final stdlibDir = File('../programs/stdlib').absolute.path;
final engine = GlpEngine(stdlibDir: stdlibDir);
print('Loaded stdlib');
```

Change to:
```dart
// Resolve programs/self.glp relative to this script's location.
// Platform.script points to glp_runtime/bin/glp_repl.dart.
// Two levels up (../../) reaches the GLP repo root; then programs/self.glp.
final rootSelfGlpPath = Platform.script.resolve('../../programs/self.glp').toFilePath();
final engine = GlpEngine(rootSelfGlpPath: rootSelfGlpPath);
print('Loaded root self.glp');
```

Also update the `:clear` print message from `'Cleared loaded programs (stdlib retained)'` to `'Cleared loaded programs (root self.glp retained)'`.

**2. `glp_runtime/lib/engine/glp_engine.dart`**

- Rename constructor parameter: `stdlibDir` → `rootSelfGlpPath`
- Remove the `.replaceAll('/stdlib', '/self.glp')` hack (two occurrences — constructor and `_loadStdlib`)
- Rename `_loadStdlib` → `_loadRootSelf`
- Use `rootSelfGlpPath` directly — it IS the path to `programs/self.glp`
- Update all doc comments that mention "stdlib"

Constructor becomes:
```dart
/// Constructor - registers standard predicates and loads root self.glp.
///
/// [rootSelfGlpPath] is the absolute path to programs/self.glp.
/// Loading root self.glp is not optional — it's part of engine initialization.
GlpEngine({required String rootSelfGlpPath}) {
    _rootSelfGlpPath = rootSelfGlpPath;
    
    // Set prelude sources from programs/self.glp for PE and type checker
    final rootSelfFile = File(_rootSelfGlpPath);
    if (rootSelfFile.existsSync()) {
      final rootSource = rootSelfFile.readAsStringSync();
      setPreludeUnitClauseSource(rootSource);
      setPreludeEnvironmentSource(rootSource);
    }
    
    registerStandardPredicates(_runtime.systemPredicates);
    _loadRootSelf();
}
```

The `_loadRootSelf` method uses `_rootSelfGlpPath` directly (no more `replaceAll`):
```dart
void _loadRootSelf() {
    final file = File(_rootSelfGlpPath);
    if (file.existsSync()) {
      try {
        final source = file.readAsStringSync();
        final compiler = GlpCompiler();
        final prog = compiler.compile(source);
        _loadedPrograms['__root_self__'] = prog;
      } catch (e) {
        // Silently skip failed load
      }
    }
}
```

**3. `glp_runtime/lib/multiagent/boot_loader.dart`**

In `BootConfig`:
- Rename field: `stdlibDir` → `rootSelfGlpPath`  
- Change default from `'../programs/stdlib'` to **no default** (require explicit path)

```dart
/// Absolute path to programs/self.glp
String rootSelfGlpPath;

BootConfig({
    required this.directives,
    required this.fullSource,
    required this.source,
    this.sharedSources,
    this.projectDir,
    required this.rootSelfGlpPath,
});
```

**4. `glp_runtime/lib/multiagent/isolate_manager.dart`**

In `AgentConfig`:
- Rename field: `stdlibDir` → `rootSelfGlpPath`
- Update doc comment

In `_agentIsolateEntry`:
- Change: `GlpEngine(stdlibDir: config.stdlibDir)` → `GlpEngine(rootSelfGlpPath: config.rootSelfGlpPath)`

In `boot()` method:
- Change: `stdlibDir: config.stdlibDir` → `rootSelfGlpPath: config.rootSelfGlpPath`

**5. `glp_runtime/lib/multiagent/agent_runtime.dart`**

- Rename field: `stdlibDir` → `rootSelfGlpPath`
- Update constructor parameter name
- Update the `GlpEngine(stdlibDir: ...)` call to `GlpEngine(rootSelfGlpPath: ...)`

### Flutter app (glp_multiagent)

**6. `glp_multiagent/lib/isolate_protocol.dart`**

In `InitAgent`:
- Rename field: `stdlibDir` → `rootSelfGlpPath`
- Update constructor parameter name

In `_runAgent`:
- Change: `AgentRuntime(... stdlibDir: init.stdlibDir ...)` → `AgentRuntime(... rootSelfGlpPath: init.rootSelfGlpPath ...)`

**7. `glp_multiagent/lib/main_cssg_mad.dart`**

Replace:
```dart
const _stdlibDir = '../programs/stdlib';
```
With a function that resolves the absolute path:
```dart
String _resolveRootSelfGlpPath() {
  // From glp_multiagent/, the repo root is one level up.
  final repoRoot = Directory.current.parent.path;
  final candidate = '$repoRoot/programs/self.glp';
  if (File(candidate).existsSync()) return candidate;
  // Fallback to absolute path
  const fallback = '/Users/udi/Grassroots/GLP/programs/self.glp';
  if (File(fallback).existsSync()) return fallback;
  return candidate;
}
```

Update all usages of `_stdlibDir` → `_resolveRootSelfGlpPath()` (or cache it).

**8. `glp_multiagent/lib/main_sg_mad.dart`**

Same change as #7. Replace `const _stdlibDir = '../programs/stdlib';` with the resolution function. Update all usages.

**9. `glp_multiagent/lib/main_cssg_mad_modules.dart`**

Same change as #7. Replace `const _stdlibDir = '../programs/stdlib';` with the resolution function. Update all usages.

### Test files

Search all test files under `glp_runtime/test/` for references to `stdlibDir`, `stdlib`, or `GlpEngine(`. Update any that construct `GlpEngine` with the old parameter name. For test files, the resolution should be:
```dart
final rootSelfGlpPath = File('../../programs/self.glp').resolveSymbolicLinksSync();
```
or relative to the test file's known location. The key requirement: pass an absolute, canonical path.

---

## Execution Order

1. **Baseline**: Pull main, run `bash test/run_all_tests.sh` — expect 399/399.
2. **Commit baseline**.
3. **Core rename**: Modify files 1–5 (glp_runtime) atomically. All five must change together since they form a call chain.
4. **Test**: Run `bash test/run_all_tests.sh` — must still be 399/399.
5. **Commit**: `refactor(infra): rename stdlibDir to rootSelfGlpPath, remove stdlib hack`
6. **Flutter app**: Modify files 6–9 (glp_multiagent). These compile independently of the REPL tests.
7. **Test files**: Search and update any test files.
8. **Final test**: Run `bash test/run_all_tests.sh` — must still be 399/399.
9. **Commit and push**.

---

## What NOT to Change

- Do NOT rename `prelude.dart`, `buildPreludeEnvironment`, `setPreludeEnvironmentSource`, or `_preludeEnvironmentSource`. That is a separate, larger cleanup deferred to later.
- Do NOT change the `__root_self__` key in `_loadedPrograms`. That internal key is fine.
- Do NOT change any `.glp` files.
- Do NOT change test expectations or test scripts.

---

## Verification

After all changes:
```bash
cd /Users/udi/Grassroots/GLP
bash test/run_all_tests.sh    # Must be 399/399
```

The REPL must work from any working directory:
```bash
# From GLP root (how test scripts invoke it):
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart

# From GLP root directly:
cd /Users/udi/Grassroots/GLP
dart run glp_runtime/bin/glp_repl.dart
```

Both must print `Loaded root self.glp` and work correctly.
