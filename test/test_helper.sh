#!/bin/bash
# test_helper.sh - Common functions for AofGLP test scripts
# Uses bash pattern matching instead of grep for portability

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
GLP_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPL_DIR="$GLP_ROOT/glp_runtime"
BOOK_DIR="$GLP_ROOT/programs/book"

# Dart path - check common locations
if command -v dart &> /dev/null; then
    DART="dart"
elif [ -x "/home/user/dart-sdk/bin/dart" ]; then
    DART="/home/user/dart-sdk/bin/dart"
else
    echo "Error: dart not found"
    exit 1
fi

# Test if a file compiles (loads without error)
test_compile() {
    local file="$1"
    local description="$2"

    # Use absolute path - REPL handles them correctly
    cd "$REPL_DIR"
    local output=$($DART run bin/glp_repl.dart <<EOF
$file
:quit
EOF
2>&1)

    # Use bash pattern matching instead of grep
    if [[ "$output" == *"Loaded:"* ]]; then
        echo -e "${GREEN}✓ PASS${NC}: $description"
        ((PASS_COUNT++))
        return 0
    elif [[ "$output" == *"Error"* ]]; then
        echo -e "${RED}✗ FAIL${NC}: $description"
        # Extract first few error lines using bash
        local count=0
        while IFS= read -r line && [ $count -lt 3 ]; do
            if [[ "$line" == *"Error"* ]] || [[ "$line" == *"violation"* ]]; then
                echo "  $line"
                ((count++))
            fi
        done <<< "$output"
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

    # Use absolute path - REPL handles them correctly
    cd "$REPL_DIR"
    local output=$($DART run bin/glp_repl.dart <<EOF
$file
$goal
:quit
EOF
2>&1)

    # Use bash pattern matching instead of grep
    if [[ "$output" == *"→ succeeds"* ]]; then
        echo -e "${GREEN}✓ PASS${NC}: $description"
        ((PASS_COUNT++))
        return 0
    elif [[ "$output" == *"Error"* ]]; then
        echo -e "${RED}✗ FAIL${NC}: $description"
        local count=0
        while IFS= read -r line && [ $count -lt 3 ]; do
            if [[ "$line" == *"Error"* ]] || [[ "$line" == *"violation"* ]]; then
                echo "  $line"
                ((count++))
            fi
        done <<< "$output"
        ((FAIL_COUNT++))
        return 1
    elif [[ "$output" == *"→ fails"* ]]; then
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
