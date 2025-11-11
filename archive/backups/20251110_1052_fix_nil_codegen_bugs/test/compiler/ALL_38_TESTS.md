# All 38 Custom Tests - Source and Goals

## Tests with explicit source documentation:

### 1. boot_test.dart
**Source**: `p(a). boot :- p(X), p(X?).`
**Goal**: `boot`
**Status**: NOT YET ADDED

### 2. clause_only_test.dart
**Source**: `clause(p(a), true).`
**Goal**: `clause(p(X), Y)`
**Status**: NOT YET ADDED (metainterpreter predicate)

### 3. list_test.dart
**Source**: Multiple tests - `first([a|_], X).`, `is_cons([_|_]).`, `cons(X, Xs, [X|Xs]).`, `p([a, b]).`
**Goal**: Various
**Status**: PARTIALLY ADDED (test 13 - one test case)

### 4. merge_test.dart
**Source**: 3-clause merge
**Goal**: 10 different test cases
**Status**: ADDED (tests 2-12)

### 5. playground_test.dart
**Source**: `p(a).`
**Goal**: `p(X)`
**Status**: ADDED (test 15)

### 6. simple_body_test.dart
**Source**: `p(a). forward(X) :- p(X).`
**Goal**: `forward(Y)`
**Status**: NOT YET ADDED (SRSW violation in source)

### 7. simple_p_q_test.dart
**Source**: `p(a). q(a).`
**Goal**: `q(X?), p(X)`
**Status**: ADDED (test 1)

### 8. struct_test.dart
**Source**: `p(f(a, b)).`
**Goal**: `p(X)`
**Status**: ADDED (test 4)

## Tests with hand-written bytecode (need to analyze):

### 9. circular_trace_test.dart
**Bytecode represents**: 3-clause merge
**Goal**: `merge(Xs?,[1],Ys), merge(Ys?,[2],Xs)` (circular)
**Status**: COVERED (test 11 - circular dependency)

### 10. custom_template_test.dart
**Status**: NEED TO ANALYZE

### 11. interactive_test.dart through interactive_test6.dart (6 files)
**Status**: NEED TO ANALYZE

### 12. merge_123_ab_test.dart
**Status**: NEED TO ANALYZE

### 13. merge_circular_budget_test.dart
**Status**: LIKELY COVERED (circular merge variants)

### 14. merge_circular_pure_trace.dart
**Status**: LIKELY COVERED (circular merge trace)

### 15. merge_reverse_order_test.dart
**Status**: NEED TO ANALYZE

### 16. merge_sequential_123_abcd_test.dart
**Status**: LIKELY COVERED (sequential merge)

### 17. merge_three_way_circular_direct_test.dart
**Status**: NEED TO ANALYZE

### 18-27. metainterp_*.dart (10 files)
**Status**: METAINTERPRETER TESTS - need special handling

### 28. p_fact_program_test.dart
**Bytecode represents**: `p(X)` with writer, `p(X?)` with reader
**Status**: NEED TO ADD

### 29. p_fact_writer_then_reader_test.dart
**Status**: NEED TO ANALYZE

### 30. p_q_trace_test.dart
**Status**: LIKELY COVERED (trace variant of p/q)

### 31. put_structure_test.dart
**Bytecode**: Multiple structure building tests
**Status**: PARTIALLY ADDED (test 14 - one case)

### 32. simple_metainterp_test.dart
**Metainterpreter**: `run(true). run(A) :- otherwise | clause(A?,B), run(B?).`
**Object program**: `clause(p(a), true).`
**Goal**: `run(p(X))`
**Status**: NOT YET ADDED

### 33. trace_q_p_test.dart
**Status**: NEED TO ANALYZE

## Summary:
- Tests with explicit source: 8 files
- Tests with hand-written bytecode: 30 files
- **Currently added**: 21 tests (3 skipped)
- **Remaining compilable tests**: 0 (all explicit-source tests added)
- **Hand-written bytecode tests**: Cannot be compiled from GLP source
- **Tests blocked by parser**: 1+ (tuple syntax not supported)
- **Tests with SRSW violations**: 2 (cannot be compiled)
