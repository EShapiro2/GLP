# Bug Fix: Project Linker Missing Root self.glp

**Bug**: `docs/bugs/project-linker-missing-prelude-types.md`

## Terminology Note

**"Prelude" is a misnomer.** There is no separate prelude concept in GLP. `programs/self.glp` is simply the root `self.glp` — the outermost ancestor in the `self.glp` hierarchy. All code and documentation referring to "prelude" (including `prelude.dart`, `buildPreludeEnvironment`, `setPreludeEnvironmentSource`, `_preludeEnvironmentSource`, `typePrelude`) should eventually be renamed to reflect this. For now, this fix works within the existing naming but does not perpetuate it.

## Root Cause

`discoverProject(projectDir)` uses `projectDir` as its root (e.g. `programs/cssg_modules/`). The ancestor chain walk stops there. `programs/self.glp` — the root `self.glp` — is above the project directory and is never included in any module's ancestor chain.

The existing code works around this with a global variable (`_preludeEnvironmentSource`) set during engine construction. But the linker's `_buildAncestorScope` calls `buildPreludeEnvironment()` which depends on this global being set. When it isn't (or when `typePrelude` is `''`), the base environment is empty — no `Constant`, no `Stream`, nothing.

## Fix

Pass the root `self.glp` path into the linker. Include it at the bottom of every ancestor chain. No globals needed.

### 1. `discoverProject` — add `rootSelfGlpPath` parameter

```dart
List<DiscoveredModule> discoverProject(String rootDir, {String? rootSelfGlpPath})
```

Pass it through to `_buildAncestorScope`.

### 2. `_buildAncestorScope` — prepend root self.glp

```dart
TypeEnvironment _buildAncestorScope(List<String> chain, {String? rootSelfGlpPath}) {
  var env = TypeEnvironment({}, {});  // DFA primitives only

  // Root self.glp is the outermost ancestor
  final fullChain = [
    if (rootSelfGlpPath != null) rootSelfGlpPath,
    ...chain,
  ];

  for (final selfGlpPath in fullChain) {
    // ... existing parse-and-merge logic
  }
  return env;
}
```

Remove the call to `buildPreludeEnvironment()`. The root self.glp IS the base of the chain.

**Note:** Starting with an empty `TypeEnvironment` is correct because the DFA primitives (`Integer`, `Real`, `Number`, `String`) are hardcoded in the DFA builder — they don't come from any `self.glp` file.

### 3. `glp_engine.dart` `loadProject` — pass root path

```dart
bool loadProject(String projectDir, {String? topModuleName}) {
  final modules = discoverProject(projectDir, rootSelfGlpPath: _rootSelfGlpPath);
  ...
}
```

## Test

All 399 tests must pass, including Section I (self.glp procedure tests).
