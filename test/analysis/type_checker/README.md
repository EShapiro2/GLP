# Type Checker Test Suite

Test infrastructure for validating Yardeni-Shapiro structural type checking implementation.

## Directory Structure

```
test/analysis/type_checker/
├── valid/structural/       - Programs that should pass type checking
└── invalid/structural/     - Programs that should fail type checking
```

## Test Files

### Positive Controls (valid/structural/)

These programs should pass type checking without errors:

1. **head_only.glp** - Simple head-only clauses with no body goals
   - Tests basic type checking of clause heads
   - No recursive calls or body goals

2. **recursive_call.glp** - Recursive list append
   - Tests body goal checking against procedure declarations
   - Tests variable type consistency across head and recursive body call

3. **multi_predicate.glp** - Multiple predicates with body calls
   - Tests append and reverse with reverse_acc helper
   - Tests body goal checking across multiple procedure calls

### Negative Controls (invalid/structural/)

These programs should FAIL type checking with appropriate errors:

1. **body_wrong_type.glp** - Ground path type mismatch in body
   - `bad([]) :- expect_nat([]).`
   - ERROR: `[]` is a List, but `expect_nat` expects Nat
   - Should fail ground path checking

2. **variable_inconsistent.glp** - Variable type inconsistency between head and body
   - `bad(X) :- helper(X?).` where `bad` expects Nat but `helper` expects List
   - ERROR: X inferred as Nat from head, but used as List in body
   - Should fail variable type intersection

3. **ground_path_invalid_body.glp** - Invalid constant in body goal
   - `bad(_) :- bad(foo).`
   - ERROR: atom `foo` is not a valid Nat
   - Should fail ground path checking in body

4. **body_undeclared_predicate.glp** - Body goal with no procedure declaration
   - `foo(X) :- bar(X?).` where `bar/1` has no declaration
   - Expected: PASS (skip type checking for undeclared predicates)
   - This is a control to verify undeclared predicates don't cause errors

## Current Status

All test files compile and load successfully as of 2025-12-20.

**Note:** Negative controls currently PASS (load without errors). This indicates one of:
- Type checker is not fully enabled in REPL
- Body checking implementation needs verification
- Tests need adjustment to trigger type errors

## Running Tests

```bash
cd /home/user/GLP/glp_runtime
export PATH="/home/user/dart-sdk/bin:$PATH"

# Test positive controls (should all pass)
echo -e '/home/user/GLP/test/analysis/type_checker/valid/structural/head_only.glp\n:quit' | dart run bin/glp_repl.dart
echo -e '/home/user/GLP/test/analysis/type_checker/valid/structural/recursive_call.glp\n:quit' | dart run bin/glp_repl.dart
echo -e '/home/user/GLP/test/analysis/type_checker/valid/structural/multi_predicate.glp\n:quit' | dart run bin/glp_repl.dart

# Test negative controls (should fail with type errors when body checking is complete)
echo -e '/home/user/GLP/test/analysis/type_checker/invalid/structural/body_wrong_type.glp\n:quit' | dart run bin/glp_repl.dart
echo -e '/home/user/GLP/test/analysis/type_checker/invalid/structural/variable_inconsistent.glp\n:quit' | dart run bin/glp_repl.dart
echo -e '/home/user/GLP/test/analysis/type_checker/invalid/structural/ground_path_invalid_body.glp\n:quit' | dart run bin/glp_repl.dart
```

## Known Issues

### Parser Bug
As of 2025-12-20, the parser has issues with some type names in procedure declarations (e.g., `Nat`).
This bug is being addressed separately. Test files have been adapted to use types that work (`List`, `Goal`, etc.)
until the parser is fixed.

### SRSW Constraints
All test files must satisfy SRSW (Single-Reader/Single-Writer) constraints:
- Each variable appears at most once in writer position
- Each variable appears at most once in reader position
- Anonymous variable `_` used for writers with no readers

Original test specifications from Phase 0 plan were adjusted to comply with SRSW.
