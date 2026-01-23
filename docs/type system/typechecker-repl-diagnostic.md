# Typechecker REPL Test Diagnostic Report

**Generated**: 2026-01-23
**Test Suite**: run_typechecker_repl_tests.sh v2.2
**Results**: 139/222 passed (83 failures)

## 1. Failure Breakdown by Error Type

| Error Type | Count | Description |
|------------|-------|-------------|
| SRSW violations | ~35 | Parser rejects before type checking (loading error) |
| Type errors (mode mismatch) | ~30 | Writer/reader mode conflicts |
| Uncovered alternatives | ~5 | Missing type coverage |
| Unknown failure | 3 | File not found or other |
| File not found | 1 | `positive/paper/merge.glp` missing |

## 2. Complete Test Results

### Positive Tests (183 total, 44 failed)

```
PASS: positive/merge_basic
PASS: positive/append_list
PASS: positive/copy_stream
PASS: positive/dl_append
PASS: positive/new_channel
PASS: positive/monitor
PASS: positive/int_list_sum
PASS: positive/nat_operations
PASS: positive/process_complete
FAIL: positive/paper/merge (unknown failure)
PASS: positive/disjoint_primitives
PASS: positive/book/universal_accepts_structured
PASS: valid/append
PASS: valid/counter
PASS: valid/simple_io
PASS: valid/embedded/counter_show
PASS: valid/embedded/double_involution
PASS: valid/embedded/double_involution_error
PASS: valid/embedded/input_with_input_embedded
PASS: valid/embedded/input_with_output_embedded
PASS: valid/embedded/output_with_input_embedded
PASS: valid/embedded/output_with_output_embedded
PASS: valid/embedded/reader_at_input_embedded
PASS: valid/embedded/writer_at_output_embedded
PASS: valid/universal/any_copy
PASS: valid/universal/any_multi_clause
FAIL: valid/universal/any_with_body (unexpected type errors)
PASS: valid/universal/list_with_any_element
PASS: valid/universal/any_constant_at_output
PASS: valid/universal/any_constant_at_input
PASS: valid/universal/any_empty_list
PASS: recursive/arithmetic_trees/natural_numbers
PASS: recursive/arithmetic_trees/plus
FAIL: recursive/arithmetic_trees/lesseq (loading error)
FAIL: recursive/arithmetic_trees/factorial (loading error)
FAIL: recursive/arithmetic_trees/fibonacci (loading error)
PASS: recursive/arithmetic_trees/gcd_integer
FAIL: recursive/arithmetic_trees/hanoi (loading error)
PASS: recursive/arithmetic_trees/times
FAIL: recursive/arithmetic_trees/min (loading error)
PASS: recursive/arithmetic_trees/exp
PASS: recursive/arithmetic_trees/ackermann
FAIL: recursive/arithmetic_trees/primes (loading error)
PASS: recursive/arithmetic_trees/sum_list
PASS: recursive/list_processing/append
PASS: recursive/list_processing/bubble_sort
PASS: recursive/list_processing/copy
PASS: recursive/list_processing/delete
PASS: recursive/list_processing/dl_append
PASS: recursive/list_processing/filter_even
PASS: recursive/list_processing/flatten
PASS: recursive/list_processing/inner_product
PASS: recursive/list_processing/inner_product_iter
PASS: recursive/list_processing/insertion_sort
PASS: recursive/list_processing/is_list
FAIL: recursive/list_processing/length (loading error)
PASS: recursive/list_processing/map_inc
PASS: recursive/list_processing/maxlist
PASS: recursive/list_processing/member
PASS: recursive/list_processing/merge_ordered
PASS: recursive/list_processing/merge_sort
PASS: recursive/list_processing/nth
PASS: recursive/list_processing/polygon_area
PASS: recursive/list_processing/prefix
PASS: recursive/list_processing/quicksort
PASS: recursive/list_processing/reverse
PASS: recursive/list_processing/reverse_naive
PASS: recursive/list_processing/translate
PASS: recursive/list_processing/variants/flatten_original
PASS: recursive/list_processing/variants/quicksort_original
FAIL: recursive/structure_processing/ancestor (unknown failure)
PASS: recursive/structure_processing/binary_tree
FAIL: recursive/structure_processing/distribute_nonground (unexpected type errors)
FAIL: recursive/structure_processing/heapify (unknown failure)
PASS: recursive/structure_processing/list_to_bst
FAIL: recursive/structure_processing/observe (unexpected type errors)
PASS: recursive/structure_processing/observe_minimal
PASS: recursive/structure_processing/observe_play
PASS: recursive/structure_processing/substitute
PASS: recursive/structure_processing/traversals
PASS: recursive/structure_processing/tree_sum
PASS: streams/producers_consumers/biased_merge
FAIL: streams/producers_consumers/channels (loading error)
PASS: streams/producers_consumers/cooperative
FAIL: streams/producers_consumers/cooperative_producers (loading error)
PASS: streams/producers_consumers/distribute
PASS: streams/producers_consumers/distribute_binary
PASS: streams/producers_consumers/distribute_ground
PASS: streams/producers_consumers/distribute_indexed
FAIL: streams/producers_consumers/dynamic_merger (loading error)
PASS: streams/producers_consumers/fair_merge
PASS: streams/producers_consumers/merge_dynamic
PASS: streams/producers_consumers/merge_simple
PASS: streams/producers_consumers/merge_tree
FAIL: streams/producers_consumers/mwm (unexpected type errors)
PASS: streams/producers_consumers/observer
FAIL: streams/producers_consumers/observers (unexpected type errors)
FAIL: streams/producers_consumers/parallel_table (loading error)
PASS: streams/producers_consumers/producer_consumer
PASS: streams/producers_consumers/producer_consumer_countdown
FAIL: streams/buffered_communication/bounded_buffer (unexpected type errors)
FAIL: streams/buffered_communication/bounded_buffer_original (loading error)
FAIL: streams/buffered_communication/switch2x2 (unexpected type errors)
PASS: streams/objects_monitors/counter
FAIL: streams/objects_monitors/many_counters (unexpected type errors)
FAIL: streams/objects_monitors/monitor (unexpected type errors)
PASS: streams/objects_monitors/monitor_test
FAIL: streams/objects_monitors/network_switch (loading error)
FAIL: streams/objects_monitors/network_switch_3way (loading error)
FAIL: streams/objects_monitors/observed_monitor (loading error)
FAIL: streams/objects_monitors/play_absolute (loading error)
PASS: streams/objects_monitors/plus_constraint
FAIL: streams/objects_monitors/queue_manager (unexpected type errors)
PASS: constants/circuits
PASS: constants/gates
PASS: constants/gates_simple
FAIL: meta/plain/certainty_meta (unexpected type errors)
FAIL: meta/plain/failsafe_meta (unexpected type errors)
PASS: meta/plain/plain_meta
FAIL: meta/enhanced/abortable_meta (unexpected type errors)
FAIL: meta/enhanced/control_meta (unexpected type errors)
FAIL: meta/enhanced/snapshot_meta (unexpected type errors)
PASS: meta/enhanced/snapshot_meta_cp
FAIL: meta/enhanced/termination_detection_meta (unexpected type errors)
PASS: meta/enhanced/termination_meta
PASS: meta/enhanced/timestamped_tree_meta
FAIL: meta/enhanced/tracing_meta (unexpected type errors)
FAIL: meta/debugging/runtime_control_meta (unexpected type errors)
FAIL: modules/main_module (unexpected type errors)
PASS: modules/math_module
FAIL: social_graph/agent (unexpected type errors)
FAIL: social_graph/agent_full (unexpected type errors)
PASS: social_graph/agent_simple
FAIL: social_graph/attestation_guards (unexpected type errors)
PASS: social_graph/channel
FAIL: social_graph/cold_call (unexpected type errors)
FAIL: social_graph/friend_introduction (unexpected type errors)
FAIL: social_graph/network (unexpected type errors)
FAIL: social_graph/network2 (unexpected type errors)
FAIL: social_graph/network3 (unexpected type errors)
FAIL: social_graph/network4 (unexpected type errors)
FAIL: social_graph/play_4agent (loading error)
FAIL: social_graph/play_4agents (loading error)
FAIL: social_graph/play_alice_bob (loading error)
FAIL: social_graph/play_cold_call (loading error)
FAIL: social_graph/play_introduction (loading error)
FAIL: social_graph/plays/play01_cold_call/alice (loading error)
FAIL: social_graph/plays/play01_cold_call/bob (loading error)
FAIL: social_graph/plays/play01_cold_call/main (loading error)
FAIL: social_graph/response_handling (unexpected type errors)
FAIL: social_graph/response_handling_unfolded (unexpected type errors)
FAIL: social_graph/stream_security (unexpected type errors)
FAIL: social_graph/streams (unexpected type errors)
PASS: social_graph/test_4player
PASS: social_networks/broadcast
FAIL: social_networks/direct_messaging (unexpected type errors)
FAIL: social_networks/dm_simple (loading error)
FAIL: social_networks/feed (loading error)
FAIL: social_networks/feed_server (loading error)
FAIL: social_networks/follower_mgmt (loading error)
FAIL: social_networks/group_formation (loading error)
FAIL: social_networks/group_messaging (loading error)
FAIL: social_networks/interlaced_streams (loading error)
FAIL: social_networks/play_child_safe (unexpected type errors)
FAIL: social_networks/play_dm (unexpected type errors)
FAIL: social_networks/play_feed (unexpected type errors)
FAIL: social_networks/play_group_interlaced (unexpected type errors)
FAIL: social_networks/play_group_manager (unexpected type errors)
FAIL: social_networks/replicate (loading error)
PASS: social_networks/replicate2
PASS: social_networks/replicate3
FAIL: constitutional_consensus/consensus (unexpected type errors)
FAIL: constitutional_consensus/play_agents (unexpected type errors)
FAIL: constitutional_consensus/play_high_throughput (unexpected type errors)
FAIL: constitutional_consensus/play_low_throughput (unexpected type errors)
FAIL: constitutional_consensus/test_blocklace (unexpected type errors)
FAIL: constitutional_consensus/test_waves (unexpected type errors)
FAIL: cryptocurrencies/gc (loading error)
FAIL: cryptocurrencies/play_mutual_credit (unexpected type errors)
FAIL: cryptocurrencies/play_payment (unexpected type errors)
FAIL: cryptocurrencies/play_redemption (unexpected type errors)
PASS: cryptocurrencies/test_balance
PASS: cryptocurrencies/test_repayments
```

### Negative Tests (37 total, all passed)

```
PASS: merge_missing_both_nil (rejected: loading error)
PASS: merge_missing_first_nil (rejected: loading error)
PASS: merge_missing_cons (rejected: loading error)
PASS: merge_wrong_constant (rejected: loading error)
PASS: merge_wrong_functor (rejected: loading error)
PASS: merge_undefined_proc (rejected: loading error)
PASS: merge_wrong_mode (rejected: type errors)
PASS: merge_type_mismatch (rejected: type errors)
PASS: merge_swapped_vars (rejected: type errors)
PASS: merge_undefined_type (rejected: type errors)
PASS: merge_incomplete (rejected: type errors)
PASS: missing_coverage (rejected: type errors)
PASS: non_complementary_types (rejected: type errors)
PASS: append_bad_type (rejected: type errors)
PASS: constant_at_wrong_type (rejected: type errors)
PASS: functor_mismatch (rejected: type errors)
PASS: channel_non_complementary (rejected: type errors)
PASS: reader_at_input (rejected: SRSW violation)
PASS: writer_at_output (rejected: type errors)
PASS: call_mode_mismatch (rejected: type errors)
PASS: embedded_mode_error (rejected: type errors)
PASS: counter_wrong_mode (rejected: type errors)
PASS: accumulator_wrong_mode (rejected: SRSW violation)
PASS: channel_wrong_inversion (rejected: SRSW violation)
PASS: correct_type_wrong_annotation (rejected: SRSW violation)
PASS: double_nesting_error (rejected: SRSW violation)
PASS: list_tail_mode_error (rejected: SRSW violation)
PASS: mixed_clauses (rejected: SRSW violation)
PASS: nested_struct_wrong_mode (rejected: SRSW violation)
PASS: pair_list_wrong_mode (rejected: SRSW violation)
PASS: recursive_type_deep_error (rejected: SRSW violation)
PASS: response_slot_no_embedded (rejected: SRSW violation)
PASS: any_list_cons (rejected: SRSW violation)
PASS: any_mixed_clauses (rejected: SRSW violation)
PASS: any_reduce_pattern (rejected: SRSW violation)
PASS: any_struct_at_input (rejected: SRSW violation)
PASS: any_struct_at_output (rejected: SRSW violation)
```

### SRSW Tests (2 total, all passed)

```
PASS: merge_reader_at_input (rejected: loading error)
PASS: merge_writer_at_output (rejected: loading error)
```

## 3. Sample Failure Details

### Sample 1: SRSW Violation - factorial.glp

**File**: `/home/user/GLP/programs/typed_book/recursive/arithmetic_trees/factorial.glp`

**Error**:
```
Error loading factorial.glp: SRSW violations found:
  • reduce/2: Line 20: Variable "T" has no reader (guard occurrences only count if grounded)
```

**Category**: Loading error (SRSW violation)

---

### Sample 2: Mode Mismatch - agent.glp

**File**: `/home/user/GLP/programs/typed_book/social_graph/agent.glp`

**Error**:
```
Type errors in agent.glp:
  ✗ Body atom 3 (social_graph) is not well-typed:
  Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: ([|]/2, 0, input) → (,/2, 1, input) → (UserOut, 2, input)
  Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: ([|]/2, 0, input) → ([|]/2, 2, input) → (,/2, 1, input) → (NetOut, 2, input)

Error loading agent.glp: SRSW violations found:
  • agent/3: Line 18: Variable "ChUser" has no reader (guard occurrences only count if grounded)
  • agent/3: Line 18: Variable "ChNet" has no reader (guard occurrences only count if grounded)
  • agent/3: Line 19: Variable "UserOut" has no reader (guard occurrences only count if grounded)
  • agent/3: Line 19: Variable "NetOut" has no reader (guard occurrences only count if grounded)
```

**Category**: Type error + SRSW violation

---

### Sample 3: File Not Found - positive/paper/merge.glp

**File**: `/home/user/GLP/glp_runtime/test/programs/typechecker/positive/paper/merge.glp`

**Error**:
```
Error: File not found: /home/user/GLP/glp_runtime/test/programs/typechecker/positive/paper/merge.glp
```

**Category**: Missing file

---

### Sample 4: Mode Mismatch with `=` - certainty_meta.glp

**File**: `/home/user/GLP/programs/typed_book/meta/plain/certainty_meta.glp`

**Error**:
```
Type errors in certainty_meta.glp:
  ✗ Body atom 3 (=) is not well-typed:
  Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: (Z, 0, input)
  Inconsistent path: Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (X?, 0, output)
```

**Category**: Type error (mode mismatch in unification)

---

### Sample 5: Coverage Gap + Mode Mismatch - bounded_buffer.glp

**File**: `/home/user/GLP/programs/typed_book/streams/buffered_communication/bounded_buffer.glp`

**Error**:
```
Type errors in bounded_buffer.glp:
  ✗ receive_unbounded argument 2: uncovered alternative "[]" at path: Stream → []
  ✗ Head of close is not well-typed:
  Inconsistent path: No transition for \(2,1):↓ from state Stream?
  Path: (\/2, 0, input) → ([|]/2, 1, input) → (end_of_stream, 1, input)
  ✗ close argument 1: uncovered alternative "[]" at path: Stream → []
  ✗ Body atom 2 (open) is not well-typed:
  Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: (\/2, 0, output) → (T, 2, input)

Error loading bounded_buffer.glp: SRSW violations found:
  • sq_num_buffered/3: Line 41: Writer variable "Buf" occurs 2 times
  • sq_num_buffered/3: Line 41: Writer variable "Tail" occurs 2 times
  • integers/3: Line 48: Writer variable "NTail" occurs 2 times
```

**Category**: Type error (coverage gap + mode mismatch) + SRSW violation

---

### Sample 6: Complex SRSW - channels.glp

**File**: `/home/user/GLP/programs/typed_book/streams/producers_consumers/channels.glp`

**Error**:
```
Error loading channels.glp: SRSW violations found:
  • read/3: Line 27: Writer variable "Left2" occurs 2 times
  • read/3: Line 27: Variable "Left2" has no reader (guard occurrences only count if grounded)
  • serialize/2: Line 55: Reader variable "Channel2?" occurs 2 times without ground guard
  • serialize/2: Line 55: Variable "Channel2" has no writer (must have exactly one)
  • subset/2: Line 67: Reader variable "Message?" occurs 2 times without ground guard
  • subset/2: Line 67: Variable "Message" has no writer (must have exactly one)
```

**Category**: Loading error (multiple SRSW violations)

---

### Sample 7: Any Type in Body - any_with_body.glp

**File**: `/home/user/GLP/glp_runtime/test/programs/moded_types/valid/universal/any_with_body.glp`

**Error**:
```
Type errors in any_with_body.glp:
  ✗ Body atom 0 (=) is not well-typed:
  Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
  Path: (Y, 0, input)
  Inconsistent path: Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (X?, 0, output)
```

**Category**: Type error (mode mismatch with Any type and `=`)

## 4. Test Script Format

From `test/run_typechecker_repl_tests.sh`:

```bash
#!/bin/bash
# GLP Type Checker Test Suite - v2.2

POSITIVE_FILES=(
    # typechecker/positive
    "$TEST_DIR/positive/merge_basic.glp"
    "$TEST_DIR/positive/append_list.glp"
    ...

    # moded_types/valid
    "$MODED_DIR/valid/append.glp"
    ...

    # typed_book programs
    "$BOOK_DIR/recursive/arithmetic_trees/factorial.glp"
    ...
)

NEGATIVE_FILES=(
    "$TEST_DIR/negative/merge_missing_both_nil.glp"
    ...
)

SRSW_FILES=(
    "$TEST_DIR/srsw/merge_reader_at_input.glp"
    "$TEST_DIR/srsw/merge_writer_at_output.glp"
)
```

**Pass Criteria**:
- Positive test: Loads without "Error loading" or "Type errors"
- Negative test: Produces "Error" or "Type errors" (expected rejection)
- SRSW test: Produces "loading error" (rejected by parser)

## 5. Failure Categories Summary

### Category A: SRSW Violations (~35 tests)

Programs violate Single-Reader/Single-Writer constraint. Parser rejects before type checking.

Common patterns:
- `Writer variable "X" occurs 2 times`
- `Variable "X" has no reader (guard occurrences only count if grounded)`
- `Reader variable "X?" occurs 2 times without ground guard`
- `Variable "X" has no writer (must have exactly one)`

**Files**: factorial, fibonacci, lesseq, hanoi, min, primes, length, channels, cooperative_producers, dynamic_merger, parallel_table, bounded_buffer_original, network_switch, network_switch_3way, observed_monitor, play_absolute, play_4agent, play_4agents, play_alice_bob, play_cold_call, play_introduction, plays/play01_cold_call/*, dm_simple, feed, feed_server, follower_mgmt, group_formation, group_messaging, interlaced_streams, replicate, gc

### Category B: Mode Mismatch Type Errors (~30 tests)

Type checker detects writer↑/reader↓ conflicts, especially with `=` unification.

Common patterns:
- `Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)`
- `Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)`

**Files**: any_with_body, distribute_nonground, observe, mwm, observers, bounded_buffer, switch2x2, many_counters, monitor, queue_manager, certainty_meta, failsafe_meta, abortable_meta, control_meta, snapshot_meta, termination_detection_meta, tracing_meta, runtime_control_meta, main_module, agent, agent_full, attestation_guards, cold_call, friend_introduction, network, network2, network3, network4, response_handling, response_handling_unfolded, stream_security, streams, direct_messaging, play_child_safe, play_dm, play_feed, play_group_interlaced, play_group_manager, consensus, play_agents, play_high_throughput, play_low_throughput, test_blocklace, test_waves, play_mutual_credit, play_payment, play_redemption

### Category C: Coverage Gaps (~5 tests)

Type checker detects uncovered type alternatives.

Common pattern:
- `uncovered alternative "[]" at path: Stream → []`

**Files**: bounded_buffer, close procedures

### Category D: Unknown/Missing (~4 tests)

- `positive/paper/merge.glp` - File not found
- `ancestor` - Unknown failure
- `heapify` - Unknown failure

## 6. Root Cause Analysis

### Issue 1: SRSW Violations in Typed Book Programs

Many typed_book programs were written before SRSW enforcement. They use patterns like:
- Multiple occurrences of writer variables
- Variables without proper reader/writer pairing
- Reader variables used multiple times without ground guards

**Remediation**: Fix the .glp source files to comply with SRSW

### Issue 2: Mode Checking for `=` Unification

The type checker rejects `X = Y?` patterns where:
- `X` is a writer at input position (expects ↑ produce)
- `Y?` is a reader at output position (expects ↓ consume)

This may be overly strict for general unification.

**Remediation**: Review type checking rules for `=` in body positions

### Issue 3: Any Type Body Handling

Programs using `Any` type with body goals (like `=`) fail mode checking.

**Remediation**: Verify Any type handling in body atom checking

### Issue 4: Missing Test File

`positive/paper/merge.glp` is referenced but doesn't exist.

**Remediation**: Create the file or remove from test list
