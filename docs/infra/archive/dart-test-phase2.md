# Phase 2: Fix Isolate Tests — Missing rootSelfGlpPath

## What this phase does

Fixes 16 failures in 3 multiagent test files. Root cause: `BootConfig.rootSelfGlpPath` defaults to `''`, so isolates can't find `programs/self.glp`, so parameterized type templates (`Stream`, `Channel`) are missing, so `enableMadGLP()` crashes on `Stream(_)`.

## Files to fix

- `test/multiagent/multiagent_glp_test.dart` (12 failures)
- `test/multiagent/isolate_manager_test.dart` (3 failures)
- `test/multiagent/multiagent_modules_test.dart` (1 failure)

## Fix

In each file, find where `config` (a `BootConfig`) is created or where `loader.load(source)` returns a config. After that line, add:

```dart
config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
```

For `multiagent_glp_test.dart`, this goes in the `runGlpTest` helper function, right after the config is built.

For `isolate_manager_test.dart`, same — after each `loader.load(source)` call.

**Additional fix in `isolate_manager_test.dart`:** The first test has inline source with `procedure agent_init(_?, Channel?).` — change `Channel?` to `_?` because bare `Channel` without parameters no longer exists.

For `multiagent_modules_test.dart`, same rootSelfGlpPath pattern.

**Important:** These tests set `strictTypes = false`. After fixing rootSelfGlpPath, they will pass but may print type warnings about undefined `send_to_net/1` etc. This is acceptable. Do NOT change `strictTypes` to `true`.

## Verify

```bash
cd glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
dart test test/multiagent/isolate_manager_test.dart
dart test test/multiagent/multiagent_modules_test.dart
```

## Commit

```bash
git add -A && git commit -m "Fix rootSelfGlpPath in multiagent isolate tests"
```

Then proceed to Phase 3: read `docs/infra/dart-test-phase3.md`.
