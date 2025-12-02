#!/bin/bash
# run_glp_tests.sh - GLP Test Suite
# Runs .glp programs through REPL and verifies results

set -eo pipefail

# Configuration
REPL="dart glp_repl.dart"
SESSION_LOG="test_session_$(date +%Y%m%d_%H%M%S).log"
VERBOSE=false
SHOW_SESSIONS=false

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Test result tracking
declare -a FAILED_TESTS

# Parse command line args
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose) VERBOSE=true; shift ;;
        -s|--show-sessions) SHOW_SESSIONS=true; shift ;;
        -h|--help) show_help; exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Core test runner function
run_test() {
    local name="$1"
    local program="$2"
    local query="$3"
    local expect_var="$4"
    local expect_value="$5"
    local expect_status="${6:-success}"  # success|suspended|failed

    TOTAL=$((TOTAL + 1))

    echo "─────────────────────────────────────────"
    echo "Test $TOTAL: $name"
    echo "  Program: $program"
    echo "  Query: $query"

    # Run REPL with commands
    local session_file="/tmp/glp_test_$$_${TOTAL}.txt"

    # Execute REPL session
    cat > "$session_file" <<EOF
$program
$query
:quit
EOF

    local output
    output=$($REPL < "$session_file" 2>&1) || true

    # Log to session file if requested
    if [[ "$VERBOSE" == "true" ]] || [[ "$SHOW_SESSIONS" == "true" ]]; then
        echo "=== Test $TOTAL: $name ===" >> "$SESSION_LOG"
        echo "$output" >> "$SESSION_LOG"
        echo "" >> "$SESSION_LOG"
    fi

    # Parse output
    local actual_value=""
    local actual_status="success"
    local resolvent=""
    local goal_count=""

    # Extract variable binding (e.g., "  Xs = [1, a, 2]")
    if [[ "$expect_var" != "" ]]; then
        actual_value=$(echo "$output" | grep "  $expect_var = " | head -1 | sed -E "s/^.*  $expect_var = //" || true)
    fi

    # Check for suspension
    if echo "$output" | grep -q "suspended"; then
        actual_status="suspended"
        # Extract resolvent (suspended goals)
        resolvent=$(echo "$output" | grep -A 10 "Suspended goals:" | tail -n +2 || echo "")
    fi

    # Check for failure
    if echo "$output" | grep -q "failed"; then
        actual_status="failed"
    fi

    # Extract goal count
    goal_count=$(echo "$output" | grep "→" | tail -1 | sed 's/.*→ \([0-9]\+\) goals.*/\1/' || true)

    # Verify result
    local passed=false

    if [[ "$expect_status" != "$actual_status" ]]; then
        echo "  ❌ FAIL: Expected status '$expect_status', got '$actual_status'"
        if [[ "$actual_status" == "suspended" ]]; then
            echo "     Resolvent:"
            echo "$resolvent" | sed 's/^/       /'
        fi
    elif [[ "$expect_var" != "" ]] && [[ "$actual_value" != "$expect_value" ]]; then
        echo "  ❌ FAIL: Expected $expect_var = $expect_value"
        echo "     Got: $actual_value"
        if [[ "$actual_status" == "suspended" ]]; then
            echo "     Resolvent:"
            echo "$resolvent" | sed 's/^/       /'
        fi
    else
        echo "  ✅ PASS"
        if [[ "$goal_count" != "" ]]; then
            echo "     Goals executed: $goal_count"
        fi
        passed=true
    fi

    if [[ "$passed" == "true" ]]; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("Test $TOTAL: $name")

        # Show session output on failure if verbose
        if [[ "$VERBOSE" == "true" ]]; then
            echo "     Session output:"
            echo "$output" | sed 's/^/       /'
        fi
    fi

    # Cleanup
    rm -f "$session_file"
}

# Test suite definitions
merge_tests() {
    run_test "Merge [1,2,3] and [a,b]" \
        "merge.glp" \
        "merge([1,2,3], [a,b], Xs)." \
        "Xs" \
        "[1, a, 2, b, 3]"

    run_test "Merge [a] and []" \
        "merge.glp" \
        "merge([a], [], X)." \
        "X" \
        "[a]"

    run_test "Merge [] and [a]" \
        "merge.glp" \
        "merge([], [a], X)." \
        "X" \
        "[a]"

    run_test "Merge [a] and [b]" \
        "merge.glp" \
        "merge([a], [b], X)." \
        "X" \
        "[a, b]"

    run_test "Merge [1,2,4,5,6] and [a]" \
        "merge.glp" \
        "merge([1,2,4,5,6], [a], X)." \
        "X" \
        "[1, a, 2, 4, 5, 6]"

    run_test "Merge [1,2,3,4] and [a,b,c,d]" \
        "merge.glp" \
        "merge([1,2,3,4], [a,b,c,d], X)." \
        "X" \
        "[1, a, 2, b, 3, c, 4, d]"
}

# Insertion sort tests
isort_tests() {
    run_test "Insertion sort [1,2,3,4]" \
        "isort.glp" \
        "insertion_sort([1,2,3,4], X)." \
        "X" \
        "[1, 2, 3, 4]"
}

# Metainterpreter tests (run1.glp)
run1_tests() {
    run_test "Meta: run(merge([],[b],X))" \
        "run1.glp" \
        "run(merge([],[b],X))." \
        "X" \
        "[b]"

    run_test "Meta: run(merge([a,b],[1,2],X))" \
        "run1.glp" \
        "run(merge([a,b],[1,2],X))." \
        "X" \
        "[a, 1, b, 2]"
}

# Main execution
main() {
    echo "╔════════════════════════════════════════╗"
    echo "║   GLP Test Suite                       ║"
    echo "╚════════════════════════════════════════╝"
    echo ""

    # Run test suites
    echo "Running Merge Tests..."
    merge_tests

    echo ""
    echo "Running Insertion Sort Tests..."
    isort_tests

    echo ""
    echo "Running Metainterpreter Tests (run1.glp)..."
    run1_tests

    # Summary
    echo ""
    echo "════════════════════════════════════════"
    echo "SUMMARY"
    echo "════════════════════════════════════════"
    echo "Total:  $TOTAL tests"
    echo "Passed: $PASSED tests"
    echo "Failed: $FAILED tests"

    if [[ "$FAILED" -gt 0 ]]; then
        echo ""
        echo "Failed tests:"
        for test in "${FAILED_TESTS[@]}"; do
            echo "  - $test"
        done
    fi

    if [[ "$SHOW_SESSIONS" == "true" ]] || [[ "$VERBOSE" == "true" ]]; then
        echo ""
        echo "Session log saved to: $SESSION_LOG"
    fi

    echo ""

    if [[ "$FAILED" -eq 0 ]]; then
        echo "✅ ALL TESTS PASSED!"
        exit 0
    else
        echo "❌ SOME TESTS FAILED"
        exit 1
    fi
}

show_help() {
    cat <<EOF
Usage: ./run_glp_tests.sh [OPTIONS]

Run GLP test suite through REPL

OPTIONS:
    -v, --verbose        Show detailed output for all tests
    -s, --show-sessions  Save session logs to file
    -h, --help           Show this help message

EXAMPLES:
    ./run_glp_tests.sh              # Run all tests
    ./run_glp_tests.sh -v           # Run with verbose output
    ./run_glp_tests.sh -s           # Save session logs

OUTPUT:
    On success: Variable bindings and goal counts
    On failure: Expected vs actual, plus resolvent if suspended
    On suspension (bug): Shows suspended goals (resolvent)

EOF
}

# Run main
main
