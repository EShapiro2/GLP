# Handover Report: self.glp in Module System + stdlib Cleanup

**Date**: 2026-03-11
**Status**: In progress — paper and specs done, linker fix done, path bug partially fixed, stdlib cleanup not started

---

## Summary

This session established that `self.glp` files can define both types and procedures (not just types). The paper, both specs, and the project linker code were updated. During testing, two infrastructure bugs were discovered and partially fixed. A larger cleanup (removing vestigial `stdlib` references) remains.

---

## Completed Work

### 1. Paper (Moded-Types `sections/modules.tex`)

Three commits on `main`:
- `525aeca` — self.glp defines types and procedures
- `7aeaff8` — uniform ancestor scoping, self.glp procedures visible without import
- `5b4bf0d` — self.glp procedures ARE renamed (needed for nested hierarchy collision prevention)

Key changes: "Hierarchy mirrors the file system" paragraph, "Implicit ancestor scoping", "Self-contained type checking", "Ancestor scoping" formula ($E_i = E_{i-1} \cup T_i \cup P_i$), static linking steps, implementation section.

### 2. Specs (GLP repo)

**`docs/modules/glp-project-compilation-spec.md`** — Updated §2 (input), §3.1 (discovery), §3.2 (renaming — self.glp renamed like any module), §3.3 (call resolution — ancestor chain lookup), §3.7 (output).

**`docs/modules/glp-module-system-spec.md`** — Updated §2.2 (self.glp may contain types, proc decls, and proc clauses).

### 3. Project Linker (`project_linker.dart`)

Commit `d3d65e3d` — 399/399 tests pass:
- Removed self.glp skip in `discoverProject()`
- Added `isSelfGlp` field to `DiscoveredModule`
- Added `_moduleNameFromDirPath()` for self.glp without `-module()`
- Built ancestor self.glp procedure map in `linkProject()` (inner-most wins)
- Extended `_resolveGoal()`: local → ancestor self.glp → root prelude
- 9 new tests in Section I (3 positive, 1 negative)

### 4. Linker Ancestor Scope Bug Fix (`project_linker.dart`)

Commit `197fc7d3` — the linker's `_buildAncestorScope` was a naive parse-and-merge that didn't handle parameterized types. Fixed to use `expandParameterizedTypes` with `knownTypeNames` and `externalTemplates`, matching `assembleTypeScope` in `module_hierarchy.dart`. Made `buildScopeFromModule` public (was `_buildScopeFromModule`) — single source of truth per DISCIPLINE.md §1.3. Root self.glp path passed explicitly via `rootSelfGlpPath` parameter.

### 5. Documentation

- `docs/modules/fix-self-glp-procedures.md` — instruction file for linker fix
- `docs/bugs/project-linker-missing-prelude-types.md` — bug report
- `docs/bugs/fix-project-linker-prelude.md` — fix instruction
- `docs/infra/absolute-paths.md` — principle: all paths absolute at entry point

---

## Partially Fixed: Path Resolution

**Problem**: `_rootSelfGlpPath` was a relative path (`../programs/self.glp`). `File(...).absolute.path` doesn't canonicalize `..` — it just prepends CWD. When the REPL runs from different directories, the path breaks.

**Current state** (commit `e2f2edb1`): 
- `glp_repl.dart` uses `Platform.script.resolve('../../programs/stdlib').toFilePath()` — resolves relative to script location, canonicalizes `..`
- `glp_engine.dart` expects absolute paths from caller (no self-resolution)
- 399 tests pass

**Remaining problem**: The path still goes through a `stdlib` indirection. The REPL passes a path to a nonexistent `stdlib` directory, and the engine does `stdlibDir.replaceAll('/stdlib', '/self.glp')` to derive the root self.glp path. This is a string manipulation hack — there is no stdlib directory.

---

## Not Started: stdlib Cleanup

### What is stdlib?

There was once a `programs/stdlib` directory. It no longer exists. But the name persists as a path-manipulation anchor used to locate `programs/self.glp`. The engine receives `stdlibDir` and does `.replaceAll('/stdlib', '/self.glp')`.

### Scope of stdlib references

Files that reference `stdlib` (parameter names, comments, defaults):

1. **`glp_runtime/bin/glp_repl.dart`** — passes `stdlib` path to engine constructor
2. **`glp_runtime/lib/engine/glp_engine.dart`** — constructor parameter `stdlibDir`, `_loadStdlib()` method, `.replaceAll('/stdlib', '/self.glp')` hack, doc comments
3. **`glp_runtime/lib/multiagent/isolate_manager.dart`** — `AgentConfig.stdlibDir` field, passed to `GlpEngine(stdlibDir: ...)`
4. **`glp_runtime/lib/multiagent/boot_loader.dart`** — `BootConfig.stdlibDir` field with default `'../programs/stdlib'`

**Not yet checked**: test files, Flutter app (`glp_multiagent/`), any other callers of `GlpEngine()`.

### What needs to happen

Rename `stdlibDir` → `rootSelfGlpPath` everywhere. Remove the `.replaceAll` hack. Pass the actual root self.glp path directly. All callers must resolve to absolute path before passing to the engine.

**This is a multi-file refactor that must be done carefully.** The full scope must be determined before starting (search for `stdlibDir`, `stdlib`, `_loadStdlib` across the entire codebase). Changes must be atomic — all callers updated together.

### Related: "prelude" is also a misnomer

`prelude.dart`, `buildPreludeEnvironment`, `setPreludeEnvironmentSource`, `_preludeEnvironmentSource`, `typePrelude` — all refer to the root self.glp. There is no separate prelude concept. These should eventually be renamed. This is a separate, larger cleanup.

---

## Other Claude Code Sessions

There is a separate Claude Code session working on cssn_modules_v2 / cssg_modules_v2. It was blocked by the path resolution bug. After the fix (`e2f2edb1`), it needs to pull main and continue. That Claude should NOT be modifying engine/REPL/linker infrastructure — it's a user of the module system, not a maintainer.

---

## Test Status

399/399 REPL tests pass on current `main`, including:
- Sections A–E: typed runtime, positive/negative type check, SRSW, guards
- Sections F–H: CSSG, SG-SIM, CSSN module projects
- Section I: 9 new self.glp procedure tests (shared proc, shadowing, local shadow, type error)

---

## Next Steps (in order)

1. **Tell the cssn_modules_v2 Claude to pull main and continue testing**
2. **stdlib cleanup** — audit full scope, rename `stdlibDir` → `rootSelfGlpPath` across all files, remove `.replaceAll` hack, test
3. **"prelude" rename** — separate cleanup, lower priority
4. **Parameterized types Step 2.4e** — the other Claude is working on this independently
