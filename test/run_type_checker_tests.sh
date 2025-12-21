#!/bin/bash
# GLP Type Checker Test Suite
# Tests Yardeni-Shapiro structural type checking implementation

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_RUNTIME="$SCRIPT_DIR/../glp_runtime"
TEST_DIR="$SCRIPT_DIR/analysis/type_checker"
GLPC="bin/glpc.dart"

cd "$GLP_RUNTIME"

echo "=========================================="
echo "   GLP Type Checker Test Suite          "
echo "=========================================="
echo ""

PASS=0
FAIL=0

# Helper function to run type check and capture output
run_type_check() {
    local file="$1"
    local expect_pass="$2"  # "pass" or "fail"
    local test_name="$3"

    output=$($DART run "$GLPC" --type-check "$file" 2>&1 || true)

    if [ "$expect_pass" = "pass" ]; then
        # Should compile without TYPE ERRORs
        if echo "$output" | grep -q "\[TYPE ERROR\]"; then
            echo "FAIL: $test_name (unexpected type error)"
            echo "      File: $file"
            echo "$output" | grep "\[TYPE ERROR\]" | head -3
            FAIL=$((FAIL + 1))
        else
            echo "PASS: $test_name"
            PASS=$((PASS + 1))
        fi
    else
        # Should have TYPE ERROR
        if echo "$output" | grep -q "\[TYPE ERROR\]"; then
            echo "PASS: $test_name (correctly rejected)"
            PASS=$((PASS + 1))
        else
            echo "FAIL: $test_name (should have type error)"
            echo "      File: $file"
            FAIL=$((FAIL + 1))
        fi
    fi
}

echo "=== Positive Controls (should pass) ==="
echo ""

run_type_check "$TEST_DIR/valid/structural/head_only.glp" "pass" "Head-only clauses"
run_type_check "$TEST_DIR/valid/structural/recursive_call.glp" "pass" "Recursive append"
run_type_check "$TEST_DIR/valid/structural/multi_predicate.glp" "pass" "Multiple predicates with body calls"

echo ""
echo "=== Negative Controls (should fail) ==="
echo ""

run_type_check "$TEST_DIR/invalid/structural/body_wrong_type.glp" "fail" "Ground path type mismatch"
run_type_check "$TEST_DIR/invalid/structural/variable_inconsistent.glp" "fail" "Variable type inconsistency"
run_type_check "$TEST_DIR/invalid/structural/ground_path_invalid_body.glp" "fail" "Invalid constant in body"

echo ""
echo "=== Special Cases ==="
echo ""

# Undeclared predicates should pass (type checking skipped)
run_type_check "$TEST_DIR/invalid/structural/body_undeclared_predicate.glp" "pass" "Undeclared predicate (skip checking)"

echo ""
echo "=== Moded Types - Embedded Mode Errors (should fail) ==="
echo ""

# Test ListTerm mode checking with embedded modes
MODED_TEST_DIR="test/programs/moded_types"

run_type_check "$MODED_TEST_DIR/invalid/deep/response_slot_no_embedded.glp" "fail" "Response slot without embedded mode (counter bug)"
run_type_check "$MODED_TEST_DIR/invalid/deep/nested_struct_wrong_mode.glp" "fail" "Nested structure with wrong mode"
run_type_check "$MODED_TEST_DIR/invalid/deep/double_nesting_error.glp" "fail" "Triple nesting mode error"
run_type_check "$MODED_TEST_DIR/invalid/deep/pair_list_wrong_mode.glp" "fail" "List of pairs wrong mode"
run_type_check "$MODED_TEST_DIR/invalid/deep/channel_wrong_inversion.glp" "fail" "Channel involution wrong mode"
run_type_check "$MODED_TEST_DIR/invalid/deep/accumulator_wrong_mode.glp" "fail" "Accumulator output mode error"
run_type_check "$MODED_TEST_DIR/invalid/deep/correct_type_wrong_annotation.glp" "fail" "Correct type wrong variable annotation"
run_type_check "$MODED_TEST_DIR/invalid/deep/mixed_clauses.glp" "fail" "Mixed correct/incorrect clauses"
run_type_check "$MODED_TEST_DIR/invalid/deep/recursive_type_deep_error.glp" "fail" "Recursive type deep mode error"
run_type_check "$MODED_TEST_DIR/invalid/deep/list_tail_mode_error.glp" "fail" "List tail mode error"

echo ""
echo "=========================================="
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "=========================================="

if [ $FAIL -eq 0 ]; then
    echo "ALL TYPE CHECKER TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED!"
    exit 1
fi
