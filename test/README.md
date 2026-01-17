# GLP Test Suite

**Location**: `/Users/udi/Grassroots/GLP/test/`  
**Last Updated**: 2026-01-17

## Official Test Scripts

### 1. Main REPL Tests (full_run_repl_tests.sh)
**Purpose**: Comprehensive REPL test suite - 218 tests  
**Command**:
```bash
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh
```
**Output**: Prints pass/fail for each test, summary at end

### 2. Typed REPL Tests (run_typechecker_repl_tests.sh)
**Purpose**: Type checker REPL tests  
**Command**:
```bash
cd /Users/udi/Grassroots/GLP && bash test/run_typechecker_repl_tests.sh
```

### 3. Book Tests (run_book_tests.sh)
**Purpose**: Compile all book examples (141 files)  
**Command**:
```bash
cd /Users/udi/Grassroots/GLP && bash test/run_book_tests.sh
```

### 4. Dart Unit Tests
**Purpose**: Unit tests for glp_runtime (including multiagent - 139 tests)  
**Command**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
```

**Multiagent only**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/
```

## Quick Reference

| Test Suite | Command | Tests |
|------------|---------|-------|
| Main REPL | `bash test/full_run_repl_tests.sh` | 218 |
| Typed REPL | `bash test/run_typechecker_repl_tests.sh` | varies |
| Book | `bash test/run_book_tests.sh` | 141 files |
| Dart Unit | `dart test` (in glp_runtime) | ~150+ |
| Multiagent | `dart test test/multiagent/` | 139 |

## Running All Tests

```bash
cd /Users/udi/Grassroots/GLP

# REPL tests
bash test/full_run_repl_tests.sh

# Book tests  
bash test/run_book_tests.sh

# Unit tests
cd glp_runtime && dart test && cd ..
```
