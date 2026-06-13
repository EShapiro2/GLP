# GLP Test Suite

**Location**: `/Users/udi/Grassroots/GLP/test/`
**Last Updated**: 2026-02-13
**Full Protocol**: See `docs/DISCIPLINE.md` Part II for complete testing protocol

---

## Quick Reference: Baseline Check

Run this before and after any code change:

```bash
cd /Users/udi/Grassroots/GLP
cd glp_runtime && dart test && cd ..
bash test/run_all_tests.sh
```

Expected results (as of 2026-02-13): 285 unit tests passing (5 skipped, 15 pre-existing failures), 316 unified tests passing.

---

## Test Suites

### Primary Test Suites (Always Run)

**Dart Unit Tests** — Core runtime, compiler, type checker, and multiagent tests.

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```

**Unified Test Suite** — All REPL-based tests: typed runtime tests, type-check positive/negative tests, SRSW violation tests, and guard tests.

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

The unified test suite has five sections:

| Section | Tests | Description |
|---------|-------|-------------|
| A: Typed Runtime Tests | 176 | Load typed programs, run queries, check output |
| B: Positive Type Check | 97 | Verify typed programs load successfully |
| C: Negative Type Tests | 39 | Verify ill-typed programs are rejected |
| D: SRSW Violations | 3 | Verify SRSW violations are detected |
| E: Invalid Guard | 1 | Verify `true` in guard position is rejected |

### Secondary Test Suites (Run When Relevant)

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

## Test Counts (as of 2026-02-13)

| Suite | Count | Status |
|-------|-------|--------|
| Dart Unit Tests | 285 pass, 5 skip, 15 fail | 15 failures are pre-existing (type checker, isolate timeout) |
| Unified Tests | 316 | All passing |
| Book Compilation | — | Script broken (binary path issue) |
| Flutter Build | — | Builds successfully |

---

## Test File Locations

| Suite | Location |
|-------|----------|
| Unit tests | `glp_runtime/test/` |
| Unified test script | `test/run_all_tests.sh` |
| Typed test programs | `programs/tests/typed/` |
| Typed book programs | `programs/book/` |
| Type checker test files | `glp_runtime/test/programs/typechecker/` |
| Moded type test files | `glp_runtime/test/programs/moded_types/` |
| Multiagent tests | `glp_runtime/test/multiagent/` |

**Archived**: Old test scripts (`full_run_repl_tests.sh`, `run_typechecker_repl_tests.sh`) are in `test/archive/`. Old untyped REPL test programs are in `programs/tests/archive/repl/`.

---

## When Tests Fail

If tests fail after a change: Stop, investigate, and fix before proceeding. Do not make additional changes that could obscure the failure. See `docs/DISCIPLINE.md` Section 2.3 for the complete protocol.

If tests fail before a change (baseline failure): Report the failure before starting work. The baseline must be healthy before making changes.
