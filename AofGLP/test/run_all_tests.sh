#!/bin/bash
# run_all_tests.sh - Run all AofGLP book example tests

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   AofGLP Book Examples Test Suite      ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}"
echo ""

TOTAL_PASS=0
TOTAL_FAIL=0
TOTAL_SKIP=0
FAILED_SECTIONS=""

run_section() {
    local script="$1"
    local name="$2"

    echo -e "\n${BLUE}Running: $name${NC}"
    echo "----------------------------------------"

    if bash "$script"; then
        echo -e "${GREEN}✓ $name: All tests passed${NC}"
    else
        echo -e "${RED}✗ $name: Some tests failed${NC}"
        FAILED_SECTIONS="$FAILED_SECTIONS $name"
    fi
}

# Run each test section
run_section "$SCRIPT_DIR/test_recursive.sh" "Recursive Examples"
run_section "$SCRIPT_DIR/test_streams.sh" "Stream Examples"
run_section "$SCRIPT_DIR/test_multiagent.sh" "Multiagent Examples"
run_section "$SCRIPT_DIR/test_meta.sh" "Meta Interpreters"
run_section "$SCRIPT_DIR/test_lib.sh" "Library Utilities"

# Final summary
echo ""
echo -e "${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║            FINAL SUMMARY               ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}"

if [ -z "$FAILED_SECTIONS" ]; then
    echo -e "${GREEN}All test sections passed!${NC}"
    exit 0
else
    echo -e "${RED}Failed sections:$FAILED_SECTIONS${NC}"
    exit 1
fi
