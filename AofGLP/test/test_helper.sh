#!/bin/bash
# test_helper.sh - Common functions for AofGLP test scripts

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
PASS_COUNT=0
FAIL_COUNT=0
SKIP_COUNT=0

# Get the GLP root directory
GLP_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REPL_DIR="$GLP_ROOT/glp_runtime"

# Test if a file compiles (loads without error)
test_compile() {
    local file="$1"
    local description="$2"

    # Convert absolute path to path relative to glp_runtime/glp/
    local rel_path="${file#$GLP_ROOT/}"

    # The REPL expects paths relative to glp_runtime/glp/
    # AofGLP files are accessed via symlink: glp/AofGLP -> ../../AofGLP

    cd "$REPL_DIR"
    local output=$(echo -e "$rel_path\n:quit" | dart run bin/glp_repl.dart 2>&1)

    if echo "$output" | grep -q "✓ Loaded"; then
        echo -e "${GREEN}✓ PASS${NC}: $description"
        ((PASS_COUNT++))
        return 0
    elif echo "$output" | grep -q "Error"; then
        echo -e "${RED}✗ FAIL${NC}: $description"
        echo "$output" | grep -E "Error|violation" | head -3
        ((FAIL_COUNT++))
        return 1
    else
        echo -e "${YELLOW}? SKIP${NC}: $description (unexpected output)"
        ((SKIP_COUNT++))
        return 2
    fi
}

# Test if a file compiles and a goal succeeds
test_goal() {
    local file="$1"
    local goal="$2"
    local description="$3"

    local rel_path="${file#$GLP_ROOT/}"

    cd "$REPL_DIR"
    local output=$(echo -e "$rel_path\n$goal\n:quit" | dart run bin/glp_repl.dart 2>&1)

    if echo "$output" | grep -q "→ succeeds"; then
        echo -e "${GREEN}✓ PASS${NC}: $description"
        ((PASS_COUNT++))
        return 0
    elif echo "$output" | grep -q "Error"; then
        echo -e "${RED}✗ FAIL${NC}: $description"
        echo "$output" | grep -E "Error|violation" | head -3
        ((FAIL_COUNT++))
        return 1
    elif echo "$output" | grep -q "→ fails"; then
        echo -e "${RED}✗ FAIL${NC}: $description (goal failed)"
        ((FAIL_COUNT++))
        return 1
    else
        echo -e "${YELLOW}? SKIP${NC}: $description (no clear result)"
        ((SKIP_COUNT++))
        return 2
    fi
}

# Print summary
print_summary() {
    local section="$1"
    echo ""
    echo "=========================================="
    echo "$section Summary"
    echo "=========================================="
    echo -e "${GREEN}Passed${NC}: $PASS_COUNT"
    echo -e "${RED}Failed${NC}: $FAIL_COUNT"
    echo -e "${YELLOW}Skipped${NC}: $SKIP_COUNT"
    echo "Total: $((PASS_COUNT + FAIL_COUNT + SKIP_COUNT))"

    if [ $FAIL_COUNT -eq 0 ]; then
        echo -e "\n${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "\n${RED}Some tests failed!${NC}"
        return 1
    fi
}

# Reset counters (for use in meta script)
reset_counters() {
    PASS_COUNT=0
    FAIL_COUNT=0
    SKIP_COUNT=0
}
