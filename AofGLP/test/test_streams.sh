#!/bin/bash
# test_streams.sh - Test stream examples (producers/consumers, objects/monitors, buffered)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.sh"

echo "=========================================="
echo "Testing: Stream Examples"
echo "=========================================="

# Producers and Consumers
echo ""
echo "--- Producers and Consumers ---"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/channels.glp" "channels.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/cooperative_producers.glp" "cooperative_producers.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/distribute.glp" "distribute.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/distribute_binary.glp" "distribute_binary.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/distribute_ground.glp" "distribute_ground.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/distribute_indexed.glp" "distribute_indexed.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/dynamic_merger.glp" "dynamic_merger.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/fair_merge.glp" "fair_merge.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/merge_tree.glp" "merge_tree.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/mwm.glp" "mwm.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/observer.glp" "observer.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/parallel_table.glp" "parallel_table.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/producers_consumers/producer_consumer.glp" "producer_consumer.glp compiles"

# Objects and Monitors
echo ""
echo "--- Objects and Monitors ---"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/counter.glp" "counter.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/many_counters.glp" "many_counters.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/monitor.glp" "monitor.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/monitor_test.glp" "monitor_test.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/network_switch.glp" "network_switch.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/network_switch_3way.glp" "network_switch_3way.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/observed_monitor.glp" "observed_monitor.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/play_absolute.glp" "play_absolute.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/plus_constraint.glp" "plus_constraint.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/objects_monitors/queue_manager.glp" "queue_manager.glp compiles"

# Buffered Communication
echo ""
echo "--- Buffered Communication ---"
test_compile "$GLP_ROOT/AofGLP/streams/buffered_communication/bounded_buffer.glp" "bounded_buffer.glp compiles"
test_compile "$GLP_ROOT/AofGLP/streams/buffered_communication/switch2x2.glp" "switch2x2.glp compiles"

print_summary "Stream Examples"
exit $?
