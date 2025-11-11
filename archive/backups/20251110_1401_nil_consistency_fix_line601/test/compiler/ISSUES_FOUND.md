# Issues Found During Systematic Comparison

## VM Bugs

### 1. PutReader with unbound writer causes suspension
**Location**: `integration_body_test.dart`, `verify_body_reader_test.dart`
**Pattern**: `p(X) :- q(X?). q(a).`
**Issue**: When `p(X)` tries to call `q(X?)`, PutReader suspends because X is unbound. The goal should spawn/requeue with the reader argument, and q should then bind via the reader.
**Status**: NOT FIXED YET

---

## Compiler Issues

*None found yet - all working correctly*

---

## Tests with SRSW Violations (Not Valid GLP)

### 1. simple_body_test.dart
**Hand-written source**: `forward(X) :- p(X).` (X appears twice as writer)
**SRSW-compliant**: `forward(X) :- p(X?).` (uses reader in body)
**Difference**: PutWriter vs PutReader - different semantics

---

## Test Results Summary

### ✅ Working (Verified)
1. simple_body_test.dart - bytecode comparison only (SRSW version has different semantics)
2. simple_p_q_test.dart - full comparison + execution ✅
3. merge_test.dart base case - full comparison + execution ✅
4. merge_test.dart clause 1 - full comparison + execution ✅

### ⚠️ VM Bugs Preventing Execution
- integration_body_test.dart (3 tests) - PutReader/Requeue issue

### 🔄 To Be Tested (34 remaining)
- boot_test.dart
- circular_trace_test.dart
- clause_only_test.dart
- custom_template_test.dart
- interactive_test.dart (and 2-6)
- list_test.dart
- merge_123_ab_test.dart
- merge_circular_budget_test.dart
- merge_circular_pure_trace.dart
- merge_reverse_order_test.dart
- merge_sequential_123_abcd_test.dart
- merge_three_way_circular_direct_test.dart
- metainterp_* tests (8 files)
- p_fact_program_test.dart
- p_fact_writer_then_reader_test.dart
- p_q_trace_test.dart
- playground_test.dart
- put_structure_test.dart
- simple_metainterp_test.dart
- struct_test.dart
- trace_q_p_test.dart

---

## Next Steps (After Full Pass)
1. Fix VM bug with PutReader/Requeue
2. Run all tests again to verify everything works
3. Commit all comparison tests to git
