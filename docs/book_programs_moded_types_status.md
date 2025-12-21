# GLP Book Programs - Moded Types Status Report
Generated: Sun Dec 21 13:45:22 UTC 2025

## Summary

| Category | Count |
|----------|-------|
| ✅ PASS (type+mode+SRSW) | 116 |
| ⚠️ FAIL_MODE (mode errors) | 0 |
| ⚠️ FAIL_SRSW (SRSW violations) | 14 |
| ❌ FAIL_BOTH (mode + SRSW) | 12 |
| ⏭️ NO_PROCEDURES (skipped) | 24 |
| **TOTAL** | 166 |

## Passing Programs

- constants/circuits.glp
- constants/gates.glp
- constants/gates_simple.glp
- meta/basic/metainterpreter.glp
- meta/debugging/runtime_control_meta.glp
- meta/enhanced/control_meta.glp
- meta/enhanced/snapshot_meta.glp
- meta/enhanced/snapshot_meta_cp.glp
- meta/enhanced/termination_detection_meta.glp
- meta/enhanced/termination_meta.glp
- meta/enhanced/timestamped_tree_meta.glp
- meta/enhanced/tracing_meta.glp
- meta/plain/certainty_meta.glp
- meta/plain/failsafe_meta.glp
- meta/plain/plain_meta.glp
- modules/main_module.glp
- modules/math_module.glp
- recursive/arithmetic_trees/ackermann.glp
- recursive/arithmetic_trees/exp.glp
- recursive/arithmetic_trees/factorial.glp
- recursive/arithmetic_trees/fibonacci.glp
- recursive/arithmetic_trees/gcd_integer.glp
- recursive/arithmetic_trees/hanoi.glp
- recursive/arithmetic_trees/inner_product.glp
- recursive/arithmetic_trees/lesseq.glp
- recursive/arithmetic_trees/min.glp
- recursive/arithmetic_trees/natural_numbers.glp
- recursive/arithmetic_trees/plus.glp
- recursive/arithmetic_trees/primes.glp
- recursive/arithmetic_trees/sum_list.glp
- recursive/arithmetic_trees/times.glp
- recursive/list_processing/append.glp
- recursive/list_processing/bsort.glp
- recursive/list_processing/bubble_sort.glp
- recursive/list_processing/copy.glp
- recursive/list_processing/delete.glp
- recursive/list_processing/filter_even.glp
- recursive/list_processing/flatten.glp
- recursive/list_processing/inner_product.glp
- recursive/list_processing/inner_product_iter.glp
- recursive/list_processing/insertion_sort.glp
- recursive/list_processing/is_list.glp
- recursive/list_processing/isort.glp
- recursive/list_processing/length.glp
- recursive/list_processing/map_inc.glp
- recursive/list_processing/maxlist.glp
- recursive/list_processing/member.glp
- recursive/list_processing/merge_ordered.glp
- recursive/list_processing/merge_sort.glp
- recursive/list_processing/nth.glp
- recursive/list_processing/polygon_area.glp
- recursive/list_processing/prefix.glp
- recursive/list_processing/quicksort.glp
- recursive/list_processing/reverse.glp
- recursive/list_processing/translate.glp
- recursive/structure_processing/ancestor.glp
- recursive/structure_processing/binary_tree.glp
- recursive/structure_processing/distribute_nonground.glp
- recursive/structure_processing/list_to_bst.glp
- recursive/structure_processing/observe.glp
- recursive/structure_processing/observe_minimal.glp
- recursive/structure_processing/observe_play.glp
- recursive/structure_processing/substitute.glp
- recursive/structure_processing/traversals.glp
- recursive/structure_processing/tree_sum.glp
- recursive/trees/hanoi.glp
- social_graph/agent.glp
- social_graph/agent_simple.glp
- social_graph/channel.glp
- social_graph/cold_call.glp
- social_graph/friend_introduction.glp
- social_graph/network2.glp
- social_graph/network3.glp
- social_graph/network4.glp
- social_graph/play_4agent.glp
- social_graph/play_4agents.glp
- social_graph/play_alice_bob.glp
- social_graph/play_introduction.glp
- social_graph/plays/play01_cold_call/alice.glp
- social_graph/plays/play01_cold_call/bob.glp
- social_graph/response_handling_unfolded.glp
- social_networks/broadcast.glp
- social_networks/dm_simple.glp
- social_networks/feed.glp
- social_networks/feed_server.glp
- social_networks/follower_mgmt.glp
- social_networks/interlaced_streams.glp
- social_networks/replicate2.glp
- social_networks/replicate3.glp
- streams/basic/copy.glp
- streams/basic/producer_consumer.glp
- streams/circuits/gates.glp
- streams/generators/primes.glp
- streams/merge/merge.glp
- streams/merge/opmerge.glp
- streams/objects_monitors/counter.glp
- streams/objects_monitors/monitor_test.glp
- streams/objects_monitors/network_switch.glp
- streams/objects_monitors/network_switch_3way.glp
- streams/objects_monitors/observed_monitor.glp
- streams/objects_monitors/plus_constraint.glp
- streams/producers_consumers/cooperative.glp
- streams/producers_consumers/distribute.glp
- streams/producers_consumers/distribute_binary.glp
- streams/producers_consumers/distribute_ground.glp
- streams/producers_consumers/distribute_indexed.glp
- streams/producers_consumers/dynamic_merger.glp
- streams/producers_consumers/fair_merge.glp
- streams/producers_consumers/merge_dynamic.glp
- streams/producers_consumers/merge_simple.glp
- streams/producers_consumers/merge_tree.glp
- streams/producers_consumers/mwm.glp
- streams/producers_consumers/observer.glp
- streams/producers_consumers/producer_consumer.glp
- streams/producers_consumers/producer_consumer_countdown.glp
- streams/transform/multiply.glp

## Programs with Mode Errors Only

These need mode annotation fixes:

## Programs with SRSW Violations Only

These have correct mode annotations but pre-existing SRSW issues in book code:

### cryptocurrencies/gc.glp
Errors:   SRSW violations found:;  • handle/4: Line 59: Reader variable "NewBal?" occurs 2 times without ground guard;  • handle/4: Line 71: Reader variable "NewBal?" occurs 2 times without ground guard;

### meta/enhanced/abortable_meta.glp
Errors:   SRSW violations found:;  • run/3: Line 16: Reader variable "M?" occurs 2 times without ground guard;  • run/3: Line 20: Variable "Eq" has no reader;

### recursive/structure_processing/heapify.glp
Errors:   SRSW violations found:;  • adjust/4: Line 17: Reader variable "X?" occurs 3 times without ground guard;  • adjust/4: Line 17: Reader variable "HeapL?" occurs 2 times without ground guard;

### social_graph/agent_full.glp
Errors:   SRSW violations found:;  • agent_loop/4: Line 32: Writer variable "Resp" occurs 2 times;  • agent_loop/4: Line 32: Variable "Resp" has no reader;

### social_graph/attestation_guards.glp
Errors:   SRSW violations found:;  • process_message/2: Line 29: Reader variable "Msg?" occurs 2 times without ground guard;  • reduce/2: Line 29: Reader variable "Msg?" occurs 2 times without ground guard at Line 0, Column 0;

### social_graph/network.glp
Errors:   SRSW violations found:;  • network/2: Line 7: Writer variable "Q" occurs 2 times;  • network/2: Line 13: Writer variable "P" occurs 2 times;

### social_graph/play_cold_call.glp
Errors:   SRSW violations found:;  • test_alice_round1/2: Line 13: Writer variable "Resp1" occurs 2 times;  • test_alice_round1/2: Line 13: Variable "Resp1" has no reader;

### social_graph/response_handling.glp
Errors:   SRSW violations found:;  • bind_response/7: Line 23: Reader variable "FOut?" occurs 2 times without ground guard;  • bind_response/7: Line 23: Variable "FOut" has no writer (must have exactly one);

### social_networks/replicate.glp
Errors:   SRSW violations found:;  • replicate2/3: Line 15: Writer variable "X" occurs 2 times;  • replicate2/3: Line 15: Reader variable "X?" occurs 2 times without ground guard;

### streams/objects_monitors/many_counters.glp
Errors:   SRSW violations found:;  • use_many_counters/2: Line 13: Reader variable "Input?" occurs 2 times without ground guard;  • use_many_counters/2: Line 13: Variable "Input" has no writer (must have exactly one);

### streams/objects_monitors/monitor.glp
Errors:   SRSW violations found:;  • monitor/2: Line 21: Writer variable "V" occurs 2 times;  • monitor/2: Line 21: Variable "V" has no reader;

### streams/objects_monitors/queue_manager.glp
Errors:   SRSW violations found:;  • qm/3: Line 16: Writer variable "X" occurs 2 times;  • qm/3: Line 16: Variable "X" has no reader;

### streams/producers_consumers/biased_merge.glp
Errors:   SRSW violations found:;  • bmerge/6: Line 18: Reader variable "By?" occurs 2 times without ground guard;  • reduce/2: Line 18: Reader variable "By?" occurs 2 times without ground guard at Line 0, Column 0;

### streams/producers_consumers/parallel_table.glp
Errors:   SRSW violations found:;  • table/4: Line 12: Reader variable "LowKey?" occurs 2 times without ground guard;  • table/4: Line 12: Reader variable "Value?" occurs 2 times without ground guard;

## Programs with Both Mode and SRSW Errors

### social_graph/plays/play01_cold_call/main.glp
Errors: [TYPE ERROR] Mode error: reader ANetCh at input position in agent argument 2;[TYPE ERROR] Mode error: reader BNetCh at input position in agent argument 2;[TYPE ERROR] Mode error: reader UserChA at input position in alice_actor argument 0;

### social_graph/stream_security.glp
Errors: [TYPE ERROR] Mode error: writer Xs at output position in produce_batch_a argument 0;[TYPE ERROR] Mode error: writer Xs1 at output position in produce_batch_a argument 1;[TYPE ERROR] Mode error: writer Done at output position in produce_batch_a argument 2;

### social_graph/streams.glp
Errors: [TYPE ERROR] Mode error: reader X at input position in observe argument 0;[TYPE ERROR] Mode error: writer Y at output position in observe argument 1;[TYPE ERROR] Mode error: writer Xs at output position in tag_stream argument 1;

### social_networks/direct_messaging.glp
Errors: [TYPE ERROR] Mode error: writer Fs1 at output position in establish argument 4;[TYPE ERROR] Mode error: writer In1 at output position in establish argument 6;[TYPE ERROR] Mode error: writer In1 at output position in handle_friend argument 8;

### social_networks/group_formation.glp
Errors: [TYPE ERROR] Mode error: writer Id at output position in social_graph argument 0;[TYPE ERROR] Mode error: writer Id at output position in social_graph argument 0;[TYPE ERROR] Mode error: writer Id at output position in social_graph argument 0;

### social_networks/group_messaging.glp
Errors: [TYPE ERROR] Mode error: reader Streams at input position in interlace argument 3;[TYPE ERROR] Mode error: reader Msgs at input position in compose_messages argument 2;  SRSW violations found:;

### streams/buffered_communication/bounded_buffer_original.glp
Errors: [TYPE ERROR] Mode error: writer Buf at output position in integers argument 2;  SRSW violations found:;  • sq_num_buffered/3: Line 46: Writer variable "Buf" occurs 2 times;

### streams/buffered_communication/switch2x2.glp
Errors: [TYPE ERROR] Mode error: writer NBuf at output position in send argument 2;[TYPE ERROR] Mode error: writer Msg at output position in receive argument 0;[TYPE ERROR] Mode error: writer NBuf at output position in receive argument 2;

### streams/objects_monitors/play_absolute.glp
Errors: [TYPE ERROR] Mode error: writer V at output position in alice argument 0;[TYPE ERROR] Mode error: writer V at output position in bob_at argument 2;[TYPE ERROR] Mode error: writer V at output position in carol_at argument 2;

### streams/producers_consumers/channels.glp
Errors: [TYPE ERROR] Mode error: writer Message at output position in read argument 0;[TYPE ERROR] Mode error: writer Channel at output position in read argument 2;[TYPE ERROR] Mode error: writer Message at output position in read argument 0;

### streams/producers_consumers/cooperative_producers.glp
Errors: [TYPE ERROR] Mode error: writer Xs at output position in handover argument 1;[TYPE ERROR] Mode error: writer Next at output position in handover argument 2;  SRSW violations found:;

### streams/producers_consumers/observers.glp
Errors: [TYPE ERROR] Mode error: reader Sum at input position in consumer argument 2;[TYPE ERROR] Mode error: reader Result at input position in consumer argument 2;[TYPE ERROR] Mode error: writer Result at output position in consumer argument 2;

## Programs Without Procedure Declarations

These files have no procedure declarations and were not migrated:

- constitutional_consensus/consensus.glp
- constitutional_consensus/play_agents.glp
- constitutional_consensus/play_high_throughput.glp
- constitutional_consensus/play_low_throughput.glp
- constitutional_consensus/test_blocklace.glp
- constitutional_consensus/test_waves.glp
- cryptocurrencies/play_mutual_credit.glp
- cryptocurrencies/play_payment.glp
- cryptocurrencies/play_redemption.glp
- cryptocurrencies/test_balance.glp
- cryptocurrencies/test_repayments.glp
- recursive/list_processing/dl_append.glp
- recursive/list_processing/reverse_naive.glp
- recursive/list_processing/variants/flatten_original.glp
- recursive/list_processing/variants/quicksort_original.glp
- social_graph/test_4player.glp
- social_networks/play_child_safe.glp
- social_networks/play_dm.glp
- social_networks/play_feed.glp
- social_networks/play_group_interlaced.glp
- social_networks/play_group_manager.glp
- streams/buffered_communication/bounded_buffer.glp
- variants/bounded_buffer_difference_list.glp
- variants/quicksort_backslash.glp
