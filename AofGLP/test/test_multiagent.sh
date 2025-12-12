#!/bin/bash
# test_multiagent.sh - Test multiagent examples (social graph, social networks)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.sh"

echo "=========================================="
echo "Testing: Multiagent Examples"
echo "=========================================="

# Social Graph
echo ""
echo "--- Social Graph ---"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/agent.glp" "agent.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/agent_glpsam.glp" "agent_glpsam.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/attestation_guards.glp" "attestation_guards.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/cold_call_glpsam.glp" "cold_call_glpsam.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/friend_introduction_glpsam.glp" "friend_introduction_glpsam.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/network2.glp" "network2.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/network3.glp" "network3.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/network4.glp" "network4.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/response_handling.glp" "response_handling.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/response_handling_glpsam.glp" "response_handling_glpsam.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/stream_security.glp" "stream_security.glp compiles"

# Social Graph Plays (compile + execute)
echo ""
echo "--- Social Graph Plays ---"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/play_4agent.glp" "play_4agent.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/play_4agents.glp" "play_4agents.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/play_alice_bob.glp" "play_alice_bob.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/play_cold_call.glp" "play_cold_call.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/play_introduction.glp" "play_introduction.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_graph/test_4player.glp" "test_4player.glp compiles"

# Social Networks
echo ""
echo "--- Social Networks ---"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/broadcast.glp" "broadcast.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/direct_messaging.glp" "direct_messaging.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/dm_simple.glp" "dm_simple.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/feed.glp" "feed.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/feed_server.glp" "feed_server.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/follower_mgmt.glp" "follower_mgmt.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/group_formation.glp" "group_formation.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/group_messaging.glp" "group_messaging.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/interlaced_streams.glp" "interlaced_streams.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/replicate.glp" "replicate.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/replicate2.glp" "replicate2.glp compiles"
test_compile "$GLP_ROOT/AofGLP/multiagent/social_networks/replicate3.glp" "replicate3.glp compiles"

# Social Networks Plays (compile + execute)
echo ""
echo "--- Social Networks Plays ---"
test_goal "$GLP_ROOT/AofGLP/multiagent/social_networks/play_dm.glp" "play_dm." "play_dm executes"
test_goal "$GLP_ROOT/AofGLP/multiagent/social_networks/play_feed.glp" "play_feed." "play_feed executes"
test_goal "$GLP_ROOT/AofGLP/multiagent/social_networks/play_group_manager.glp" "play_group_manager." "play_group_manager executes"
test_goal "$GLP_ROOT/AofGLP/multiagent/social_networks/play_group_interlaced.glp" "play_group_interlaced." "play_group_interlaced executes"
test_goal "$GLP_ROOT/AofGLP/multiagent/social_networks/play_child_safe.glp" "play_child_safe." "play_child_safe executes"

print_summary "Multiagent Examples"
exit $?
