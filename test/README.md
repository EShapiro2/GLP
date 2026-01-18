# GLP Test Suite

**Location**: `/Users/udi/Grassroots/GLP/test/`  
**Last Updated**: 2026-01-18  
**Full Protocol**: See `docs/DISCIPLINE.md` Part II for complete testing protocol

---

## Quick Reference: Baseline Check

Run this before and after any code change:

```bash
cd /Users/udi/Grassroots/GLP
cd glp_runtime && dart test && cd ..
bash test/full_run_repl_tests.sh
```

Expected results (as of 2026-01-18): 236 unit tests passing, 222 REPL tests passing.

---

## Test Suites

### Primary Test Suites (Always Run)

**Dart Unit Tests** — Core runtime, compiler, type checker, and multiagent tests.

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```

**REPL Tests** — End-to-end tests that run GLP programs through the REPL.

```bash
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh
```

### Secondary Test Suites (Run When Relevant)

**Typed REPL Tests** — Type checker tests via REPL.

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_typechecker_repl_tests.sh
```

**Book Compilation Tests** — Verifies all book example programs compile.

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_book_tests.sh
```

**Multiagent Tests Only** — Subset of unit tests for multiagent work.

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/
```

**Flutter Build** — Verifies the multiagent Flutter app builds.

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos
```

---

## Test Counts (as of 2026-01-18)

| Suite | Count | Status |
|-------|-------|--------|
| Dart Unit Tests | 236 | Passing |
| REPL Tests | 222 | Passing |
| Multiagent Tests | 139 | Passing (subset of unit tests) |
| Book Compilation | 141 files | Compiles |
| Flutter Build | — | Builds successfully |

---

## Test File Locations

| Suite | Location |
|-------|----------|
| Unit tests | `glp_runtime/test/` |
| REPL test script | `test/full_run_repl_tests.sh` |
| REPL test programs | `programs/tests/` |
| Type checker tests | `glp_runtime/test/analysis/type_checker/` |
| Multiagent tests | `glp_runtime/test/multiagent/` |
| Book programs | `programs/book/` and `programs/typed_book/` |

---

## When Tests Fail

If tests fail after a change: Stop, investigate, and fix before proceeding. Do not make additional changes that could obscure the failure. See `docs/DISCIPLINE.md` Section 2.3 for the complete protocol.

If tests fail before a change (baseline failure): Report the failure before starting work. The baseline must be healthy before making changes.

