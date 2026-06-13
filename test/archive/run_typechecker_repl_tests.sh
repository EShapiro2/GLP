#!/bin/bash
# GLP Type Checker Test Suite - v2.2
# Runs all tests in a single REPL session
# Updated: 2026-01-13 - Added ALL book programs

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$SCRIPT_DIR/../glp_runtime"
TEST_DIR="$RUNTIME_DIR/test/programs/typechecker"
MODED_DIR="$RUNTIME_DIR/test/programs/moded_types"
BOOK_DIR="$SCRIPT_DIR/../programs/book"
REPL="bin/glp_repl.dart"

cd "$RUNTIME_DIR"

echo "======================================"
echo "   GLP Type Checker Test Suite v2.2   "
echo "======================================"
echo ""

# =============================================================================
# POSITIVE TESTS - Should load successfully (well-typed programs)
# =============================================================================
POSITIVE_FILES=(
    # === typechecker/positive ===
    "$TEST_DIR/positive/merge_basic.glp"
    "$TEST_DIR/positive/append_list.glp"
    "$TEST_DIR/positive/copy_stream.glp"
    "$TEST_DIR/positive/dl_append.glp"
    "$TEST_DIR/positive/new_channel.glp"
    "$TEST_DIR/positive/monitor.glp"
    "$TEST_DIR/positive/int_list_sum.glp"
    "$TEST_DIR/positive/nat_operations.glp"
    "$TEST_DIR/positive/process_complete.glp"
    # "$TEST_DIR/positive/paper/merge.glp"  # File doesn't exist
    "$TEST_DIR/positive/disjoint_primitives.glp"
    "$TEST_DIR/positive/book/universal_accepts_structured.glp"
    
    # === moded_types/valid (no duplicates) ===
    "$MODED_DIR/valid/append.glp"
    "$MODED_DIR/valid/counter.glp"
    "$MODED_DIR/valid/simple_io.glp"
    
    # === moded_types/valid/embedded ===
    "$MODED_DIR/valid/embedded/counter_show.glp"
    "$MODED_DIR/valid/embedded/double_involution.glp"
    "$MODED_DIR/valid/embedded/double_involution_error.glp"
    "$MODED_DIR/valid/embedded/input_with_input_embedded.glp"
    "$MODED_DIR/valid/embedded/input_with_output_embedded.glp"
    "$MODED_DIR/valid/embedded/output_with_input_embedded.glp"
    "$MODED_DIR/valid/embedded/output_with_output_embedded.glp"
    "$MODED_DIR/valid/embedded/reader_at_input_embedded.glp"
    "$MODED_DIR/valid/embedded/writer_at_output_embedded.glp"
    
    # === moded_types/valid/universal ===
    "$MODED_DIR/valid/universal/any_copy.glp"
    "$MODED_DIR/valid/universal/any_multi_clause.glp"
    "$MODED_DIR/valid/universal/any_with_body.glp"
    "$MODED_DIR/valid/universal/list_with_any_element.glp"
    "$MODED_DIR/valid/universal/any_constant_at_output.glp"
    "$MODED_DIR/valid/universal/any_constant_at_input.glp"
    "$MODED_DIR/valid/universal/any_empty_list.glp"
    
    # =========================================================================
    # TYPED BOOK PROGRAMS
    # =========================================================================
    
    # === book/recursive/arithmetic_trees ===
    "$BOOK_DIR/recursive/arithmetic_trees/natural_numbers.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/plus.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/lesseq.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/factorial.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/fibonacci.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/gcd_integer.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/hanoi.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/times.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/min.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/exp.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/ackermann.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/primes.glp"
    "$BOOK_DIR/recursive/arithmetic_trees/sum_list.glp"
    
    # === book/recursive/list_processing ===
    "$BOOK_DIR/recursive/list_processing/append.glp"
    "$BOOK_DIR/recursive/list_processing/bubble_sort.glp"
    "$BOOK_DIR/recursive/list_processing/copy.glp"
    "$BOOK_DIR/recursive/list_processing/delete.glp"
    "$BOOK_DIR/recursive/list_processing/dl_append.glp"
    "$BOOK_DIR/recursive/list_processing/filter_even.glp"
    "$BOOK_DIR/recursive/list_processing/flatten.glp"
    "$BOOK_DIR/recursive/list_processing/inner_product.glp"
    "$BOOK_DIR/recursive/list_processing/inner_product_iter.glp"
    "$BOOK_DIR/recursive/list_processing/insertion_sort.glp"
    "$BOOK_DIR/recursive/list_processing/is_list.glp"
    "$BOOK_DIR/recursive/list_processing/length.glp"
    "$BOOK_DIR/recursive/list_processing/map_inc.glp"
    "$BOOK_DIR/recursive/list_processing/maxlist.glp"
    "$BOOK_DIR/recursive/list_processing/member.glp"
    "$BOOK_DIR/recursive/list_processing/merge_ordered.glp"
    "$BOOK_DIR/recursive/list_processing/merge_sort.glp"
    "$BOOK_DIR/recursive/list_processing/nth.glp"
    "$BOOK_DIR/recursive/list_processing/polygon_area.glp"
    "$BOOK_DIR/recursive/list_processing/prefix.glp"
    "$BOOK_DIR/recursive/list_processing/quicksort.glp"
    "$BOOK_DIR/recursive/list_processing/reverse.glp"
    "$BOOK_DIR/recursive/list_processing/reverse_naive.glp"
    "$BOOK_DIR/recursive/list_processing/translate.glp"
    "$BOOK_DIR/recursive/list_processing/variants/flatten_original.glp"
    "$BOOK_DIR/recursive/list_processing/variants/quicksort_original.glp"
    
    # === book/recursive/structure_processing ===
    # "$BOOK_DIR/recursive/structure_processing/ancestor.glp"  # File is .disabled
    "$BOOK_DIR/recursive/structure_processing/binary_tree.glp"
    "$BOOK_DIR/recursive/structure_processing/distribute_nonground.glp"
    # "$BOOK_DIR/recursive/structure_processing/heapify.glp"  # File is .disabled
    "$BOOK_DIR/recursive/structure_processing/list_to_bst.glp"
    "$BOOK_DIR/recursive/structure_processing/observe.glp"
    "$BOOK_DIR/recursive/structure_processing/observe_minimal.glp"
    "$BOOK_DIR/recursive/structure_processing/observe_play.glp"
    "$BOOK_DIR/recursive/structure_processing/substitute.glp"
    "$BOOK_DIR/recursive/structure_processing/traversals.glp"
    "$BOOK_DIR/recursive/structure_processing/tree_sum.glp"
    
    # === book/streams/producers_consumers ===
    "$BOOK_DIR/streams/producers_consumers/biased_merge.glp"
    "$BOOK_DIR/streams/producers_consumers/channels.glp"
    "$BOOK_DIR/streams/producers_consumers/cooperative.glp"
    "$BOOK_DIR/streams/producers_consumers/cooperative_producers.glp"
    "$BOOK_DIR/streams/producers_consumers/distribute.glp"
    "$BOOK_DIR/streams/producers_consumers/distribute_binary.glp"
    "$BOOK_DIR/streams/producers_consumers/distribute_ground.glp"
    "$BOOK_DIR/streams/producers_consumers/distribute_indexed.glp"
    "$BOOK_DIR/streams/producers_consumers/dynamic_merger.glp"
    "$BOOK_DIR/streams/producers_consumers/fair_merge.glp"
    "$BOOK_DIR/streams/producers_consumers/merge_dynamic.glp"
    "$BOOK_DIR/streams/producers_consumers/merge_simple.glp"
    "$BOOK_DIR/streams/producers_consumers/merge_tree.glp"
    "$BOOK_DIR/streams/producers_consumers/mwm.glp"
    "$BOOK_DIR/streams/producers_consumers/observer.glp"
    "$BOOK_DIR/streams/producers_consumers/observers.glp"
    "$BOOK_DIR/streams/producers_consumers/parallel_table.glp"
    "$BOOK_DIR/streams/producers_consumers/producer_consumer.glp"
    "$BOOK_DIR/streams/producers_consumers/producer_consumer_countdown.glp"
    
    # === book/streams/buffered_communication ===
    "$BOOK_DIR/streams/buffered_communication/bounded_buffer.glp"
    "$BOOK_DIR/streams/buffered_communication/bounded_buffer_original.glp"
    "$BOOK_DIR/streams/buffered_communication/switch2x2.glp"
    
    # === book/streams/objects_monitors ===
    "$BOOK_DIR/streams/objects_monitors/counter.glp"
    "$BOOK_DIR/streams/objects_monitors/many_counters.glp"
    "$BOOK_DIR/streams/objects_monitors/monitor.glp"
    "$BOOK_DIR/streams/objects_monitors/monitor_test.glp"
    "$BOOK_DIR/streams/objects_monitors/network_switch.glp"
    "$BOOK_DIR/streams/objects_monitors/network_switch_3way.glp"
    "$BOOK_DIR/streams/objects_monitors/observed_monitor.glp"
    "$BOOK_DIR/streams/objects_monitors/play_absolute.glp"
    "$BOOK_DIR/streams/objects_monitors/plus_constraint.glp"
    "$BOOK_DIR/streams/objects_monitors/queue_manager.glp"
    
    # === book/constants ===
    "$BOOK_DIR/constants/circuits.glp"
    "$BOOK_DIR/constants/gates.glp"
    "$BOOK_DIR/constants/gates_simple.glp"
    
    # === book/meta/plain ===
    "$BOOK_DIR/meta/plain/certainty_meta.glp"
    "$BOOK_DIR/meta/plain/failsafe_meta.glp"
    "$BOOK_DIR/meta/plain/plain_meta.glp"
    
    # === book/meta/enhanced ===
    "$BOOK_DIR/meta/enhanced/abortable_meta.glp"
    "$BOOK_DIR/meta/enhanced/control_meta.glp"
    "$BOOK_DIR/meta/enhanced/snapshot_meta.glp"
    "$BOOK_DIR/meta/enhanced/snapshot_meta_cp.glp"
    "$BOOK_DIR/meta/enhanced/termination_detection_meta.glp"
    "$BOOK_DIR/meta/enhanced/termination_meta.glp"
    "$BOOK_DIR/meta/enhanced/timestamped_tree_meta.glp"
    "$BOOK_DIR/meta/enhanced/tracing_meta.glp"
    
    # === book/meta/debugging ===
    "$BOOK_DIR/meta/debugging/runtime_control_meta.glp"
    
    # === book/modules ===
    "$BOOK_DIR/modules/main_module.glp"
    "$BOOK_DIR/modules/math_module.glp"
    
    # === book/social_graph ===
    "$BOOK_DIR/social_graph/agent.glp"
    "$BOOK_DIR/social_graph/agent_full.glp"
    "$BOOK_DIR/social_graph/agent_simple.glp"
    "$BOOK_DIR/social_graph/attestation_guards.glp"
    "$BOOK_DIR/social_graph/channel.glp"
    "$BOOK_DIR/social_graph/cold_call.glp"
    "$BOOK_DIR/social_graph/friend_introduction.glp"
    "$BOOK_DIR/social_graph/network.glp"
    "$BOOK_DIR/social_graph/network2.glp"
    "$BOOK_DIR/social_graph/network3.glp"
    "$BOOK_DIR/social_graph/network4.glp"
    "$BOOK_DIR/social_graph/play_4agent.glp"
    "$BOOK_DIR/social_graph/play_4agents.glp"
    "$BOOK_DIR/social_graph/play_alice_bob.glp"
    "$BOOK_DIR/social_graph/play_cold_call.glp"
    "$BOOK_DIR/social_graph/play_introduction.glp"
    "$BOOK_DIR/social_graph/plays/play01_cold_call/alice.glp"
    "$BOOK_DIR/social_graph/plays/play01_cold_call/bob.glp"
    "$BOOK_DIR/social_graph/plays/play01_cold_call/main.glp"
    "$BOOK_DIR/social_graph/response_handling.glp"
    "$BOOK_DIR/social_graph/response_handling_unfolded.glp"
    "$BOOK_DIR/social_graph/stream_security.glp"
    "$BOOK_DIR/social_graph/streams.glp"
    "$BOOK_DIR/social_graph/test_4player.glp"
    
    # === book/social_networks ===
    "$BOOK_DIR/social_networks/broadcast.glp"
    "$BOOK_DIR/social_networks/direct_messaging.glp"
    "$BOOK_DIR/social_networks/dm_simple.glp"
    "$BOOK_DIR/social_networks/feed.glp"
    "$BOOK_DIR/social_networks/feed_server.glp"
    "$BOOK_DIR/social_networks/follower_mgmt.glp"
    "$BOOK_DIR/social_networks/group_formation.glp"
    "$BOOK_DIR/social_networks/group_messaging.glp"
    "$BOOK_DIR/social_networks/interlaced_streams.glp"
    "$BOOK_DIR/social_networks/play_child_safe.glp"
    "$BOOK_DIR/social_networks/play_dm.glp"
    "$BOOK_DIR/social_networks/play_feed.glp"
    "$BOOK_DIR/social_networks/play_group_interlaced.glp"
    "$BOOK_DIR/social_networks/play_group_manager.glp"
    "$BOOK_DIR/social_networks/replicate.glp"
    "$BOOK_DIR/social_networks/replicate2.glp"
    "$BOOK_DIR/social_networks/replicate3.glp"
    
    # === book/constitutional_consensus ===
    "$BOOK_DIR/constitutional_consensus/consensus.glp"
    "$BOOK_DIR/constitutional_consensus/play_agents.glp"
    "$BOOK_DIR/constitutional_consensus/play_high_throughput.glp"
    "$BOOK_DIR/constitutional_consensus/play_low_throughput.glp"
    "$BOOK_DIR/constitutional_consensus/test_blocklace.glp"
    "$BOOK_DIR/constitutional_consensus/test_waves.glp"
    
    # === book/cryptocurrencies ===
    "$BOOK_DIR/cryptocurrencies/gc.glp"
    "$BOOK_DIR/cryptocurrencies/play_mutual_credit.glp"
    "$BOOK_DIR/cryptocurrencies/play_payment.glp"
    "$BOOK_DIR/cryptocurrencies/play_redemption.glp"
    "$BOOK_DIR/cryptocurrencies/test_balance.glp"
    "$BOOK_DIR/cryptocurrencies/test_repayments.glp"
)

# =============================================================================
# NEGATIVE TESTS - Should be rejected (ill-typed programs)
# =============================================================================
NEGATIVE_FILES=(
    # === typechecker/negative/coverage (contravariance violations) ===
    "$TEST_DIR/negative/coverage/merge_missing_both_nil.glp"
    "$TEST_DIR/negative/coverage/merge_missing_first_nil.glp"
    "$TEST_DIR/negative/coverage/merge_missing_cons.glp"
    
    # === typechecker/negative/head (covariance - head type errors) ===
    "$TEST_DIR/negative/head/merge_wrong_constant.glp"
    "$TEST_DIR/negative/head/merge_wrong_functor.glp"
    
    # === typechecker/negative/body (body type errors) ===
    "$TEST_DIR/negative/body/merge_undefined_proc.glp"
    "$TEST_DIR/negative/body/merge_wrong_mode.glp"
    
    # === typechecker/negative/complementarity (complementarity violations) ===
    "$TEST_DIR/negative/complementarity/merge_type_mismatch.glp"
    "$TEST_DIR/negative/complementarity/merge_swapped_vars.glp"
    
    # === typechecker/negative/type_def (type definition errors) ===
    "$TEST_DIR/negative/type_def/merge_undefined_type.glp"
    
    # === typechecker/negative (top level) ===
    "$TEST_DIR/negative/merge_incomplete.glp"
    "$TEST_DIR/negative/missing_coverage.glp"
    "$TEST_DIR/negative/non_complementary_types.glp"
    "$TEST_DIR/negative/append_bad_type.glp"
    "$TEST_DIR/negative/constant_at_wrong_type.glp"
    "$TEST_DIR/negative/functor_mismatch.glp"
    "$TEST_DIR/negative/channel_non_complementary.glp"
    
    # === moded_types/invalid (SRSW violations - caught by parser) ===
    "$MODED_DIR/invalid/reader_at_input.glp"
    "$MODED_DIR/invalid/writer_at_output.glp"
    "$MODED_DIR/invalid/call_mode_mismatch.glp"
    "$MODED_DIR/invalid/embedded_mode_error.glp"
    
    # === moded_types/invalid/embedded ===
    "$MODED_DIR/invalid/embedded/counter_wrong_mode.glp"
    
    # === moded_types/invalid/deep ===
    "$MODED_DIR/invalid/deep/accumulator_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/channel_wrong_inversion.glp"
    "$MODED_DIR/invalid/deep/correct_type_wrong_annotation.glp"
    "$MODED_DIR/invalid/deep/double_nesting_error.glp"
    "$MODED_DIR/invalid/deep/list_tail_mode_error.glp"
    "$MODED_DIR/invalid/deep/mixed_clauses.glp"
    "$MODED_DIR/invalid/deep/nested_struct_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/pair_list_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/recursive_type_deep_error.glp"
    "$MODED_DIR/invalid/deep/response_slot_no_embedded.glp"
    
    # === moded_types/invalid/universal ===
    "$MODED_DIR/invalid/universal/any_list_cons.glp"
    "$MODED_DIR/invalid/universal/any_mixed_clauses.glp"
    "$MODED_DIR/invalid/universal/any_reduce_pattern.glp"
    "$MODED_DIR/invalid/universal/any_struct_at_input.glp"
    "$MODED_DIR/invalid/universal/any_struct_at_output.glp"
)

# =============================================================================
# SRSW TESTS - Rejected by parser for SRSW violations (separate category)
# =============================================================================
SRSW_FILES=(
    "$TEST_DIR/negative/head/merge_reader_at_input.glp"
    "$TEST_DIR/negative/head/merge_writer_at_output.glp"
)

# Build REPL input: load all files, then quit
REPL_INPUT=""
for f in "${POSITIVE_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
for f in "${NEGATIVE_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
for f in "${SRSW_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
REPL_INPUT+=":quit"$'\n'

# Run single REPL session
TOTAL_POSITIVE=${#POSITIVE_FILES[@]}
TOTAL_NEGATIVE=${#NEGATIVE_FILES[@]}
TOTAL_SRSW=${#SRSW_FILES[@]}
echo "Running $TOTAL_POSITIVE positive + $TOTAL_NEGATIVE negative + $TOTAL_SRSW SRSW tests..."
echo ""

output=$(echo "$REPL_INPUT" | dart run "$REPL" 2>&1)

# Parse results
PASS=0
FAIL=0
FAILED_POSITIVE=()

echo "--- Positive Tests (should load successfully) ---"
for f in "${POSITIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    dir=$(dirname "$f" | sed "s|$BOOK_DIR/||" | sed "s|$TEST_DIR/||" | sed "s|$MODED_DIR/||")
    # Check if this file had type errors or SRSW violations
    if echo "$output" | grep -q "Type errors in $f"; then
        echo "FAIL: $dir/$name (unexpected type errors)"
        FAILED_POSITIVE+=("$f")
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "SRSW violations in $f"; then
        echo "FAIL: $dir/$name (unexpected SRSW violations)"
        FAILED_POSITIVE+=("$f")
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Error loading $f"; then
        echo "FAIL: $dir/$name (loading error)"
        FAILED_POSITIVE+=("$f")
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Loaded: $f"; then
        echo "PASS: $dir/$name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $dir/$name (unknown failure)"
        FAILED_POSITIVE+=("$f")
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "--- Negative Tests (should be rejected by type checker) ---"
for f in "${NEGATIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    if echo "$output" | grep -B1 -A5 "$f" | grep -q "Type errors"; then
        echo "PASS: $name (rejected: type errors)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "SRSW violations"; then
        echo "PASS: $name (rejected: SRSW violation)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Error loading"; then
        echo "PASS: $name (rejected: loading error)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (should have been rejected)"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "--- SRSW Tests (should be rejected by parser for SRSW violations) ---"
for f in "${SRSW_FILES[@]}"; do
    name=$(basename "$f" .glp)
    if echo "$output" | grep -B1 -A5 "$f" | grep -q "SRSW violations"; then
        echo "PASS: $name (rejected: SRSW violation)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Type errors"; then
        echo "WARN: $name (rejected for type errors, expected SRSW)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Error loading"; then
        echo "PASS: $name (rejected: loading error)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (should have been rejected for SRSW)"
        FAIL=$((FAIL + 1))
    fi
done

TOTAL=$((PASS + FAIL))

echo ""
echo "======================================"
echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
echo "======================================"

# Show failed positive tests summary
if [ ${#FAILED_POSITIVE[@]} -gt 0 ]; then
    echo ""
    echo "Failed positive tests (need investigation):"
    for f in "${FAILED_POSITIVE[@]}"; do
        echo "  - $f"
    done
fi

if [ $FAIL -eq 0 ]; then
    echo ""
    echo "ALL TYPE CHECKER TESTS PASSED!"
    exit 0
else
    echo ""
    echo "SOME TESTS FAILED - see above for details"
    exit 1
fi
