# Book Program Fixes Tracker

**Created**: 2025-12-18
**Baseline**: 84 passing, 57 failing (from CLAUDE.md)
**Target**: 141 passing, 0 failing

## Summary by Category

| Category | Count | Status |
|----------|-------|--------|
| A: Multiple writer occurrences | 23 | Pending |
| B: Multiple reader without ground | 6 | Pending |
| C: Writer with no reader | 6 | Pending |
| D: Arity mismatch | 4 | Pending |
| E: Parser error | 15 | Pending |
| F: Missing writer | 1 | Pending |
| G: Unknown/numbered failures | 2 | Pending |
| **Total** | **57** | |

## Detailed Failure List

### Category A: Multiple Writer Occurrences (23 files)

#### A1. prefix.glp
- **Path**: `programs/book/recursive/list_processing/prefix.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A2. inner_product.glp
- **Path**: `programs/book/recursive/list_processing/inner_product.glp`
- **Error**: SRSW violation: Writer variable "IP" occurs 2 times in clause
- **Status**: Pending

#### A3. merge_ordered.glp
- **Path**: `programs/book/recursive/list_processing/merge_ordered.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A4. ancestor.glp
- **Path**: `programs/book/recursive/structure_processing/ancestor.glp`
- **Error**: SRSW violation: Writer variable "Z" occurs 2 times in clause
- **Status**: Pending

#### A5. heapify.glp
- **Path**: `programs/book/recursive/structure_processing/heapify.glp`
- **Error**: SRSW violation: Writer variable "Heap" occurs 2 times in clause
- **Status**: Pending

#### A6. traversals.glp
- **Path**: `programs/book/recursive/structure_processing/traversals.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A7. channels.glp
- **Path**: `programs/book/streams/producers_consumers/channels.glp`
- **Error**: SRSW violation: Writer variable "Message" occurs 2 times in clause
- **Status**: Pending

#### A8. cooperative_producers.glp
- **Path**: `programs/book/streams/producers_consumers/cooperative_producers.glp`
- **Error**: SRSW violation: Writer variable "Xs" occurs 2 times in clause
- **Status**: Pending

#### A9. distribute.glp
- **Path**: `programs/book/streams/producers_consumers/distribute.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 3 times in clause
- **Status**: Pending

#### A10. distribute_indexed.glp
- **Path**: `programs/book/streams/producers_consumers/distribute_indexed.glp`
- **Error**: SRSW violation: Writer variable "Out2" occurs 2 times in clause
- **Status**: Pending

#### A11. parallel_table.glp
- **Path**: `programs/book/streams/producers_consumers/parallel_table.glp`
- **Error**: SRSW violation: Writer variable "LowKey" occurs 2 times in clause
- **Status**: Pending

#### A12. queue_manager.glp
- **Path**: `programs/book/streams/objects_monitors/queue_manager.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A13. bounded_buffer.glp
- **Path**: `programs/book/streams/buffered_communication/bounded_buffer.glp`
- **Error**: SRSW violation: Writer variable "Msg" occurs 2 times in clause
- **Status**: Pending

#### A14. switch2x2.glp
- **Path**: `programs/book/streams/buffered_communication/switch2x2.glp`
- **Error**: SRSW violation: Writer variable "M" occurs 2 times in clause
- **Status**: Pending

#### A15. agent.glp
- **Path**: `programs/book/social_graph/agent.glp`
- **Error**: SRSW violation: Writer variable "ChUser" occurs 2 times in clause
- **Status**: Pending

#### A16. stream_security.glp
- **Path**: `programs/book/social_graph/stream_security.glp`
- **Error**: SRSW violation: Writer variable "T" occurs 2 times in clause
- **Status**: Pending

#### A17. network.glp
- **Path**: `programs/book/social_graph/network.glp`
- **Error**: SRSW violation: Writer variable "Q" occurs 2 times in clause
- **Status**: Pending

#### A18. streams.glp
- **Path**: `programs/book/social_graph/streams.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A19. main.glp (play01_cold_call)
- **Path**: `programs/book/social_graph/plays/play01_cold_call/main.glp`
- **Error**: SRSW violation: Writer variable "AUserCh" occurs 2 times in clause
- **Status**: Pending

#### A20. direct_messaging.glp
- **Path**: `programs/book/social_networks/direct_messaging.glp`
- **Error**: SRSW violation: Writer variable "From" occurs 2 times in clause
- **Status**: Pending

#### A21. feed.glp
- **Path**: `programs/book/social_networks/feed.glp`
- **Error**: SRSW violation: Writer variable "Content" occurs 2 times in clause
- **Status**: Pending

#### A22. replicate.glp
- **Path**: `programs/book/social_networks/replicate.glp`
- **Error**: SRSW violation: Writer variable "X" occurs 2 times in clause
- **Status**: Pending

#### A23. gc.glp
- **Path**: `programs/book/cryptocurrencies/gc.glp`
- **Error**: SRSW violation: Writer variable "Stream1" occurs 2 times in clause
- **Status**: Pending

---

### Category B: Multiple Reader Without Ground Guard (6 files)

#### B1. fibonacci.glp
- **Path**: `programs/book/recursive/arithmetic_trees/fibonacci.glp`
- **Error**: SRSW violation: Reader variable "B?" occurs 2 times without ground guard
- **Status**: Pending

#### B2. filter_even.glp
- **Path**: `programs/book/recursive/list_processing/filter_even.glp`
- **Error**: SRSW violation: Reader variable "X?" occurs 2 times without ground guard
- **Status**: Pending

#### B3. many_counters.glp
- **Path**: `programs/book/streams/objects_monitors/many_counters.glp`
- **Error**: SRSW violation: Reader variable "Input?" occurs 2 times without ground guard
- **Status**: Pending

#### B4. play_absolute.glp
- **Path**: `programs/book/streams/objects_monitors/play_absolute.glp`
- **Error**: SRSW violation: Reader variable "T0?" occurs 6 times without ground guard
- **Status**: Pending

#### B5. response_handling.glp
- **Path**: `programs/book/social_graph/response_handling.glp`
- **Error**: SRSW violation: Reader variable "FOut?" occurs 2 times without ground guard
- **Status**: Pending

#### B6. group_messaging.glp
- **Path**: `programs/book/social_networks/group_messaging.glp`
- **Error**: SRSW violation: Reader variable "Id?" occurs 2 times without ground guard
- **Status**: Pending

---

### Category C: Writer With No Reader (6 files)

**Pattern**: These are mostly in abort clauses where the variable should use `_` instead of a named writer.

#### C1. play_cold_call.glp
- **Path**: `programs/book/social_graph/play_cold_call.glp`
- **Error**: SRSW violation: Variable "AliceFs1" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

#### C2. failsafe_meta.glp
- **Path**: `programs/book/meta/plain/failsafe_meta.glp`
- **Error**: SRSW violation: Variable "M" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

#### C3. abortable_meta.glp
- **Path**: `programs/book/meta/enhanced/abortable_meta.glp`
- **Error**: SRSW violation: Variable "M" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

#### C4. control_meta.glp
- **Path**: `programs/book/meta/enhanced/control_meta.glp`
- **Error**: SRSW violation: Variable "M" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

#### C5. termination_detection_meta.glp
- **Path**: `programs/book/meta/enhanced/termination_detection_meta.glp`
- **Error**: SRSW violation: Variable "M" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

#### C6. tracing_meta.glp
- **Path**: `programs/book/meta/enhanced/tracing_meta.glp`
- **Error**: SRSW violation: Variable "M" has writer but no reader (must have at least one reader)
- **Proposed fix**: Replace with anonymous variable `_` in abort clause
- **Status**: Pending

---

### Category D: Arity Mismatch (4 files)

**Pattern**: Test expects different arity than defined in file. May need to check test script or file contents.

#### D1. sum_list.glp
- **Path**: `programs/book/recursive/arithmetic_trees/sum_list.glp`
- **Error**: Clause for sum/3 found, expected sum/2
- **Status**: Pending - needs investigation

#### D2. maxlist.glp
- **Path**: `programs/book/recursive/list_processing/maxlist.glp`
- **Error**: Clause for maxlist/3 found, expected maxlist/2
- **Status**: Pending - needs investigation

#### D3. inner_product_iter.glp
- **Path**: `programs/book/recursive/list_processing/inner_product_iter.glp`
- **Error**: Clause for inner_product/4 found, expected inner_product/3
- **Status**: Pending - needs investigation

#### D4. monitor.glp
- **Path**: `programs/book/streams/objects_monitors/monitor.glp`
- **Error**: Clause for monitor/2 found, expected monitor/1
- **Status**: Pending - needs investigation

---

### Category E: Parser Error (15 files)

#### E1. biased_merge.glp
- **Path**: `programs/book/streams/producers_consumers/biased_merge.glp`
- **Error**: Expected "]" after list elements
- **Status**: Pending - needs parser fix or syntax correction

#### E2. attestation_guards.glp
- **Path**: `programs/book/social_graph/attestation_guards.glp`
- **Error**: Unexpected character: \
- **Status**: Pending - needs syntax investigation

#### E3. agent_full.glp
- **Path**: `programs/book/social_graph/agent_full.glp`
- **Error**: Unexpected character: \
- **Status**: Pending - needs syntax investigation

#### E4. group_formation.glp
- **Path**: `programs/book/social_networks/group_formation.glp`
- **Error**: Expected ")" after arguments
- **Status**: Pending - needs syntax investigation

#### E5. consensus.glp
- **Path**: `programs/book/constitutional_consensus/consensus.glp`
- **Error**: Expected "." at end of clause
- **Status**: Pending - needs syntax investigation

#### E6. play_mutual_credit.glp
- **Path**: `programs/book/cryptocurrencies/play_mutual_credit.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E7. play_payment.glp
- **Path**: `programs/book/cryptocurrencies/play_payment.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E8. play_redemption.glp
- **Path**: `programs/book/cryptocurrencies/play_redemption.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E9. test_balance.glp
- **Path**: `programs/book/cryptocurrencies/test_balance.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E10. test_repayments.glp
- **Path**: `programs/book/cryptocurrencies/test_repayments.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E11. play_agents.glp
- **Path**: `programs/book/constitutional_consensus/play_agents.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E12. play_high_throughput.glp
- **Path**: `programs/book/constitutional_consensus/play_high_throughput.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E13. play_low_throughput.glp
- **Path**: `programs/book/constitutional_consensus/play_low_throughput.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E14. test_blocklace.glp
- **Path**: `programs/book/constitutional_consensus/test_blocklace.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

#### E15. test_waves.glp
- **Path**: `programs/book/constitutional_consensus/test_waves.glp`
- **Error**: Expected predicate name
- **Status**: Pending - needs syntax investigation

---

### Category F: Missing Writer (1 file)

#### F1. friend_introduction.glp
- **Path**: `programs/book/social_graph/friend_introduction.glp`
- **Error**: SRSW violation: Variable "QPIn" must have exactly one writer occurrence (found 0)
- **Status**: Pending - needs investigation

---

### Category G: Unknown/Numbered Failures (2 files)

These appear as "FAIL: 2" or "FAIL: 3" in output without clear file identification. They may be duplicates or test script artifacts.

---

## Notes

### Patterns Observed

1. **Category A (Multiple Writers)**: Most common pattern. Typical in:
   - Stream processing (distribute, channels, producers)
   - Network communication (switches, agents)
   - Data structures being built incrementally
   - Likely need accumulator pattern refactoring

2. **Category B (Multiple Readers)**: Reader used multiple times without ground guards
   - Classic case: fibonacci with recursive calls using same reader
   - Need to restructure to use writer/reader pairs correctly

3. **Category C (Writer No Reader)**: Specific to abort/otherwise clauses
   - Should use anonymous variable `_` instead of named variable
   - Easy fix: simple substitution in 6 files

4. **Category D (Arity Mismatch)**: May indicate:
   - Test script expects different version
   - File contains accumulator version while test expects simple version
   - Need to investigate each case

5. **Category E (Parser Errors)**: Two subcategories:
   - Backslash character issues (2 files)
   - "Expected predicate name" errors (11 files, mostly in crypto/consensus)
   - May indicate missing parser features or syntax evolution

### Priority Recommendations

1. **Quick wins**: Category C (6 files) - simple `_` substitution
2. **Next**: Category D (4 files) - investigate test vs implementation
3. **Core fixes**: Categories A & B (29 files) - SRSW refactoring
4. **Parser investigation**: Category E (15 files) - may need parser fixes first

### Next Steps

1. Start with Category C fixes to reduce failure count quickly
2. Investigate one file from each category to understand fix patterns
3. Develop systematic transformation rules for Category A patterns
4. Review parser errors to determine if they're code or parser issues
