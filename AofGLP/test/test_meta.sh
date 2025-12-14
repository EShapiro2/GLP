#!/bin/bash
# test_meta.sh - Test meta interpreters (plain, enhanced, debugging)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.sh"

echo "=========================================="
echo "Testing: Meta Interpreters"
echo "=========================================="

# Plain Meta Interpreters
echo ""
echo "--- Plain Meta Interpreters ---"
test_compile "$GLP_ROOT/AofGLP/meta/plain/plain_meta.glp" "plain_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/plain/certainty_meta.glp" "certainty_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/plain/failsafe_meta.glp" "failsafe_meta.glp compiles"

# Enhanced Meta Interpreters
echo ""
echo "--- Enhanced Meta Interpreters ---"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/abortable_meta.glp" "abortable_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/control_meta.glp" "control_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/snapshot_meta.glp" "snapshot_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/snapshot_meta_cp.glp" "snapshot_meta_cp.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/termination_detection_meta.glp" "termination_detection_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/termination_meta.glp" "termination_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/timestamped_tree_meta.glp" "timestamped_tree_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/meta/enhanced/tracing_meta.glp" "tracing_meta.glp compiles"

# Debugging Meta Interpreters
echo ""
echo "--- Debugging Meta Interpreters ---"
test_compile "$GLP_ROOT/AofGLP/meta/debugging/runtime_control_meta.glp" "runtime_control_meta.glp compiles"

# Concurrent Prolog Examples (meta-related)
echo ""
echo "--- Concurrent Prolog Examples ---"
test_compile "$GLP_ROOT/AofGLP/concurrent_prolog_examples/cp_meta.glp" "cp_meta.glp compiles"
test_compile "$GLP_ROOT/AofGLP/concurrent_prolog_examples/debugger_meta.glp" "debugger_meta.glp compiles"

print_summary "Meta Interpreters"
exit $?
