#!/bin/bash
# GLP Book Examples Test Suite - Single REPL Session
# Tests that all book example files compile without errors

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_ROOT="$SCRIPT_DIR/.."
GLP_RUNTIME="$GLP_ROOT/glp_runtime"
BOOK_DIR="$GLP_ROOT/programs/book"

cd "$GLP_RUNTIME"

# Use compiled REPL
REPL="./glp_repl"
if [ ! -x "$REPL" ]; then
    echo "Compiled REPL not found, compiling..."
    DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
    $DART compile exe bin/glp_repl.dart -o glp_repl
fi

echo "======================================"
echo "   GLP Book Examples Test Suite       "
echo "======================================"
echo ""

# Run REPL with all book files in single session
output=$($REPL <<'REPL_INPUT'
/home/user/GLP/programs/book/recursive/arithmetic_trees/factorial.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/fibonacci.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/gcd_integer.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/exp.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/ackermann.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/hanoi.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/min.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/natural_numbers.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/plus.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/primes.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/sum_list.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/times.glp
/home/user/GLP/programs/book/recursive/arithmetic_trees/lesseq.glp
/home/user/GLP/programs/book/recursive/list_processing/append.glp
/home/user/GLP/programs/book/recursive/list_processing/copy.glp
/home/user/GLP/programs/book/recursive/list_processing/delete.glp
/home/user/GLP/programs/book/recursive/list_processing/is_list.glp
/home/user/GLP/programs/book/recursive/list_processing/length.glp
/home/user/GLP/programs/book/recursive/list_processing/member.glp
/home/user/GLP/programs/book/recursive/list_processing/nth.glp
/home/user/GLP/programs/book/recursive/list_processing/prefix.glp
/home/user/GLP/programs/book/recursive/list_processing/reverse.glp
/home/user/GLP/programs/book/recursive/list_processing/translate.glp
/home/user/GLP/programs/book/recursive/list_processing/maxlist.glp
/home/user/GLP/programs/book/recursive/list_processing/inner_product.glp
/home/user/GLP/programs/book/recursive/list_processing/inner_product_iter.glp
/home/user/GLP/programs/book/recursive/list_processing/polygon_area.glp
/home/user/GLP/programs/book/recursive/list_processing/bubble_sort.glp
/home/user/GLP/programs/book/recursive/list_processing/insertion_sort.glp
/home/user/GLP/programs/book/recursive/list_processing/merge_ordered.glp
/home/user/GLP/programs/book/recursive/list_processing/merge_sort.glp
/home/user/GLP/programs/book/recursive/list_processing/quicksort.glp
/home/user/GLP/programs/book/recursive/list_processing/filter_even.glp
/home/user/GLP/programs/book/recursive/list_processing/flatten.glp
/home/user/GLP/programs/book/recursive/list_processing/map_inc.glp
/home/user/GLP/programs/book/recursive/structure_processing/ancestor.glp
/home/user/GLP/programs/book/recursive/structure_processing/binary_tree.glp
/home/user/GLP/programs/book/recursive/structure_processing/distribute_nonground.glp
/home/user/GLP/programs/book/recursive/structure_processing/heapify.glp
/home/user/GLP/programs/book/recursive/structure_processing/substitute.glp
/home/user/GLP/programs/book/recursive/structure_processing/traversals.glp
/home/user/GLP/programs/book/recursive/structure_processing/observe.glp
/home/user/GLP/programs/book/recursive/structure_processing/observe_minimal.glp
/home/user/GLP/programs/book/recursive/structure_processing/observe_play.glp
/home/user/GLP/programs/book/recursive/structure_processing/list_to_bst.glp
/home/user/GLP/programs/book/recursive/structure_processing/tree_sum.glp
/home/user/GLP/programs/book/streams/producers_consumers/channels.glp
/home/user/GLP/programs/book/streams/producers_consumers/cooperative_producers.glp
/home/user/GLP/programs/book/streams/producers_consumers/distribute.glp
/home/user/GLP/programs/book/streams/producers_consumers/distribute_binary.glp
/home/user/GLP/programs/book/streams/producers_consumers/distribute_ground.glp
/home/user/GLP/programs/book/streams/producers_consumers/distribute_indexed.glp
/home/user/GLP/programs/book/streams/producers_consumers/dynamic_merger.glp
/home/user/GLP/programs/book/streams/producers_consumers/fair_merge.glp
/home/user/GLP/programs/book/streams/producers_consumers/merge_tree.glp
/home/user/GLP/programs/book/streams/producers_consumers/mwm.glp
/home/user/GLP/programs/book/streams/producers_consumers/observer.glp
/home/user/GLP/programs/book/streams/producers_consumers/parallel_table.glp
/home/user/GLP/programs/book/streams/producers_consumers/producer_consumer.glp
/home/user/GLP/programs/book/streams/producers_consumers/producer_consumer_countdown.glp
/home/user/GLP/programs/book/streams/producers_consumers/biased_merge.glp
/home/user/GLP/programs/book/streams/objects_monitors/counter.glp
/home/user/GLP/programs/book/streams/objects_monitors/many_counters.glp
/home/user/GLP/programs/book/streams/objects_monitors/monitor.glp
/home/user/GLP/programs/book/streams/objects_monitors/monitor_test.glp
/home/user/GLP/programs/book/streams/objects_monitors/network_switch.glp
/home/user/GLP/programs/book/streams/objects_monitors/network_switch_3way.glp
/home/user/GLP/programs/book/streams/objects_monitors/observed_monitor.glp
/home/user/GLP/programs/book/streams/objects_monitors/play_absolute.glp
/home/user/GLP/programs/book/streams/objects_monitors/plus_constraint.glp
/home/user/GLP/programs/book/streams/objects_monitors/queue_manager.glp
/home/user/GLP/programs/book/streams/buffered_communication/bounded_buffer.glp
/home/user/GLP/programs/book/streams/buffered_communication/switch2x2.glp
/home/user/GLP/programs/book/social_graph/agent.glp
/home/user/GLP/programs/book/social_graph/agent_simple.glp
/home/user/GLP/programs/book/social_graph/attestation_guards.glp
/home/user/GLP/programs/book/social_graph/cold_call.glp
/home/user/GLP/programs/book/social_graph/friend_introduction.glp
/home/user/GLP/programs/book/social_graph/network2.glp
/home/user/GLP/programs/book/social_graph/network3.glp
/home/user/GLP/programs/book/social_graph/network4.glp
/home/user/GLP/programs/book/social_graph/response_handling.glp
/home/user/GLP/programs/book/social_graph/response_handling_unfolded.glp
/home/user/GLP/programs/book/social_graph/stream_security.glp
/home/user/GLP/programs/book/social_graph/play_4agent.glp
/home/user/GLP/programs/book/social_graph/play_4agents.glp
/home/user/GLP/programs/book/social_graph/play_alice_bob.glp
/home/user/GLP/programs/book/social_graph/play_cold_call.glp
/home/user/GLP/programs/book/social_graph/play_introduction.glp
/home/user/GLP/programs/book/social_graph/test_4player.glp
/home/user/GLP/programs/book/social_networks/broadcast.glp
/home/user/GLP/programs/book/social_networks/direct_messaging.glp
/home/user/GLP/programs/book/social_networks/dm_simple.glp
/home/user/GLP/programs/book/social_networks/feed.glp
/home/user/GLP/programs/book/social_networks/feed_server.glp
/home/user/GLP/programs/book/social_networks/follower_mgmt.glp
/home/user/GLP/programs/book/social_networks/group_formation.glp
/home/user/GLP/programs/book/social_networks/group_messaging.glp
/home/user/GLP/programs/book/social_networks/interlaced_streams.glp
/home/user/GLP/programs/book/social_networks/replicate.glp
/home/user/GLP/programs/book/social_networks/replicate2.glp
/home/user/GLP/programs/book/social_networks/replicate3.glp
/home/user/GLP/programs/book/social_networks/play_child_safe.glp
/home/user/GLP/programs/book/social_networks/play_dm.glp
/home/user/GLP/programs/book/social_networks/play_feed.glp
/home/user/GLP/programs/book/social_networks/play_group_interlaced.glp
/home/user/GLP/programs/book/social_networks/play_group_manager.glp
/home/user/GLP/programs/book/meta/plain/plain_meta.glp
/home/user/GLP/programs/book/meta/plain/certainty_meta.glp
/home/user/GLP/programs/book/meta/plain/failsafe_meta.glp
/home/user/GLP/programs/book/meta/enhanced/abortable_meta.glp
/home/user/GLP/programs/book/meta/enhanced/control_meta.glp
/home/user/GLP/programs/book/meta/enhanced/snapshot_meta.glp
/home/user/GLP/programs/book/meta/enhanced/snapshot_meta_cp.glp
/home/user/GLP/programs/book/meta/enhanced/termination_detection_meta.glp
/home/user/GLP/programs/book/meta/enhanced/termination_meta.glp
/home/user/GLP/programs/book/meta/enhanced/timestamped_tree_meta.glp
/home/user/GLP/programs/book/meta/enhanced/tracing_meta.glp
/home/user/GLP/programs/book/meta/debugging/runtime_control_meta.glp
/home/user/GLP/programs/book/constants/gates.glp
/home/user/GLP/programs/book/constants/gates_simple.glp
/home/user/GLP/programs/book/constants/circuits.glp
/home/user/GLP/programs/book/cryptocurrencies/gc.glp
/home/user/GLP/programs/book/cryptocurrencies/play_mutual_credit.glp
/home/user/GLP/programs/book/cryptocurrencies/play_payment.glp
/home/user/GLP/programs/book/cryptocurrencies/play_redemption.glp
/home/user/GLP/programs/book/cryptocurrencies/test_balance.glp
/home/user/GLP/programs/book/cryptocurrencies/test_repayments.glp
/home/user/GLP/programs/book/constitutional_consensus/consensus.glp
/home/user/GLP/programs/book/constitutional_consensus/play_agents.glp
/home/user/GLP/programs/book/constitutional_consensus/play_high_throughput.glp
/home/user/GLP/programs/book/constitutional_consensus/play_low_throughput.glp
/home/user/GLP/programs/book/constitutional_consensus/test_blocklace.glp
/home/user/GLP/programs/book/constitutional_consensus/test_waves.glp
:quit
REPL_INPUT
2>&1)

# Count results by parsing output
PASS=0
FAIL=0

while IFS= read -r line; do
    if [[ "$line" == *"Loaded:"* ]]; then
        # Extract filename from "✓ Loaded: /path/to/file.glp"
        filename="${line##*/}"
        filename="${filename%.glp}.glp"
        echo "PASS: $filename"
        ((PASS++))
    elif [[ "$line" == *"Error loading"* ]]; then
        # Extract filename and error
        filename="${line##*/}"
        filename="${filename%%:*}"
        echo "FAIL: $filename"
        # Print error detail
        echo "  $line"
        ((FAIL++))
    elif [[ "$line" == *"Error: File not found"* ]]; then
        filename="${line##*/}"
        echo "FAIL: $filename (file not found)"
        ((FAIL++))
    fi
done <<< "$output"

TOTAL=$((PASS + FAIL))

echo ""
echo "======================================"
echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
echo "======================================"

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED (SRSW violations in book code)"
    exit 1
fi
