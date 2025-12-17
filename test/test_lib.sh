#!/bin/bash
# test_lib.sh - Test library utilities and constants

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/test_helper.sh"

echo "=========================================="
echo "Testing: Library Utilities"
echo "=========================================="

# Broadcast
echo ""
echo "--- Broadcast ---"
test_compile "$BOOK_DIR/lib/broadcast/broadcast.glp" "broadcast.glp compiles"

# Channels
echo ""
echo "--- Channels ---"
test_compile "$BOOK_DIR/lib/channels/channel_ops.glp" "channel_ops.glp compiles"
test_compile "$BOOK_DIR/lib/channels/relay.glp" "relay.glp compiles"

# Guards
echo ""
echo "--- Guards ---"
test_compile "$BOOK_DIR/lib/guards/guard_utils.glp" "guard_utils.glp compiles"

# Lookup
echo ""
echo "--- Lookup ---"
test_compile "$BOOK_DIR/lib/lookup/lookup.glp" "lookup.glp compiles"

# Streams
echo ""
echo "--- Streams ---"
test_compile "$BOOK_DIR/lib/streams/inject.glp" "inject.glp compiles"
test_compile "$BOOK_DIR/lib/streams/tag_stream.glp" "tag_stream.glp compiles"

# Time
echo ""
echo "--- Time ---"
test_compile "$BOOK_DIR/lib/time/time_utils.glp" "time_utils.glp compiles"

# Constants
echo ""
echo "--- Constants ---"
test_compile "$BOOK_DIR/constants/gates/gates.glp" "gates.glp compiles"

print_summary "Library Utilities"
exit $?
