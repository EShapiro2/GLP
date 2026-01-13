#!/bin/bash
# GLP Type Checker Test Suite - v2.0
# Runs all tests in a single REPL session
# Updated: 2026-01-12 - Fixed test classifications and removed duplicates

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$SCRIPT_DIR/../glp_runtime"
TEST_DIR="$RUNTIME_DIR/test/programs/typechecker"
MODED_DIR="$RUNTIME_DIR/test/programs/moded_types"
REPL="bin/glp_repl_typed.dart"

cd "$RUNTIME_DIR"

echo "======================================"
echo "   GLP Type Checker Test Suite v2.0   "
echo "======================================"
echo ""

# =============================================================================
# POSITIVE TESTS - Should load successfully (well-typed programs)
# =============================================================================
POSITIVE_FILES=(
    # === typechecker/positive ===
    "$TEST_DIR/positive/merge_basic.glp"
    "$TEST_DIR/positive/append_list.glp"
    "$TEST_DIR/positive/copy_stream.glp"
    "$TEST_DIR/positive/dl_append.glp"
    "$TEST_DIR/positive/new_channel.glp"
    "$TEST_DIR/positive/monitor.glp"
    "$TEST_DIR/positive/int_list_sum.glp"
    "$TEST_DIR/positive/nat_operations.glp"
    "$TEST_DIR/positive/process_complete.glp"
    "$TEST_DIR/positive/paper/merge.glp"
    "$TEST_DIR/positive/disjoint_primitives.glp"
    "$TEST_DIR/positive/book/universal_accepts_structured.glp"
    
    # === moded_types/valid (no duplicates) ===
    "$MODED_DIR/valid/append.glp"
    "$MODED_DIR/valid/counter.glp"
    "$MODED_DIR/valid/simple_io.glp"
    # Note: moded_types/valid/merge.glp removed - duplicate of paper/merge.glp
    
    # === moded_types/valid/embedded ===
    "$MODED_DIR/valid/embedded/counter_show.glp"
    "$MODED_DIR/valid/embedded/double_involution.glp"
    "$MODED_DIR/valid/embedded/double_involution_error.glp"
    "$MODED_DIR/valid/embedded/input_with_input_embedded.glp"
    "$MODED_DIR/valid/embedded/input_with_output_embedded.glp"
    "$MODED_DIR/valid/embedded/output_with_input_embedded.glp"
    "$MODED_DIR/valid/embedded/output_with_output_embedded.glp"
    "$MODED_DIR/valid/embedded/reader_at_input_embedded.glp"
    "$MODED_DIR/valid/embedded/writer_at_output_embedded.glp"
    
    # === moded_types/valid/universal ===
    "$MODED_DIR/valid/universal/any_copy.glp"
    "$MODED_DIR/valid/universal/any_multi_clause.glp"
    "$MODED_DIR/valid/universal/any_with_body.glp"
    "$MODED_DIR/valid/universal/list_with_any_element.glp"
    "$MODED_DIR/valid/universal/any_constant_at_output.glp"
)

# =============================================================================
# NEGATIVE TESTS - Should be rejected (ill-typed programs)
# =============================================================================
NEGATIVE_FILES=(
    # === typechecker/negative/coverage (contravariance violations) ===
    "$TEST_DIR/negative/coverage/merge_missing_both_nil.glp"
    "$TEST_DIR/negative/coverage/merge_missing_first_nil.glp"
    "$TEST_DIR/negative/coverage/merge_missing_cons.glp"
    
    # === typechecker/negative/head (covariance - head type errors) ===
    # Note: merge_reader_at_input.glp and merge_writer_at_output.glp removed
    # They fail SRSW (parser), not type checking
    "$TEST_DIR/negative/head/merge_wrong_constant.glp"
    "$TEST_DIR/negative/head/merge_wrong_functor.glp"
    
    # === typechecker/negative/body (body type errors) ===
    "$TEST_DIR/negative/body/merge_undefined_proc.glp"
    "$TEST_DIR/negative/body/merge_wrong_mode.glp"
    
    # === typechecker/negative/complementarity (complementarity violations) ===
    "$TEST_DIR/negative/complementarity/merge_type_mismatch.glp"
    "$TEST_DIR/negative/complementarity/merge_swapped_vars.glp"
    
    # === typechecker/negative/type_def (type definition errors) ===
    "$TEST_DIR/negative/type_def/merge_undefined_type.glp"
    
    # === typechecker/negative (top level) ===
    "$TEST_DIR/negative/merge_incomplete.glp"
    "$TEST_DIR/negative/missing_coverage.glp"
    "$TEST_DIR/negative/non_complementary_types.glp"
    "$TEST_DIR/negative/append_bad_type.glp"
    "$TEST_DIR/negative/constant_at_wrong_type.glp"
    "$TEST_DIR/negative/functor_mismatch.glp"
    "$TEST_DIR/negative/channel_non_complementary.glp"
    
    # === moded_types/invalid (SRSW violations - caught by parser) ===
    "$MODED_DIR/invalid/reader_at_input.glp"
    "$MODED_DIR/invalid/writer_at_output.glp"
    "$MODED_DIR/invalid/call_mode_mismatch.glp"
    "$MODED_DIR/invalid/embedded_mode_error.glp"
    
    # === moded_types/invalid/embedded ===
    "$MODED_DIR/invalid/embedded/counter_wrong_mode.glp"
    
    # === moded_types/invalid/deep ===
    "$MODED_DIR/invalid/deep/accumulator_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/channel_wrong_inversion.glp"
    "$MODED_DIR/invalid/deep/correct_type_wrong_annotation.glp"
    "$MODED_DIR/invalid/deep/double_nesting_error.glp"
    "$MODED_DIR/invalid/deep/list_tail_mode_error.glp"
    "$MODED_DIR/invalid/deep/mixed_clauses.glp"
    "$MODED_DIR/invalid/deep/nested_struct_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/pair_list_wrong_mode.glp"
    "$MODED_DIR/invalid/deep/recursive_type_deep_error.glp"
    "$MODED_DIR/invalid/deep/response_slot_no_embedded.glp"
    
    # === moded_types/invalid/universal ===
    "$MODED_DIR/invalid/universal/any_constant_at_input.glp"
    "$MODED_DIR/invalid/universal/any_empty_list.glp"
    "$MODED_DIR/invalid/universal/any_list_cons.glp"
    "$MODED_DIR/invalid/universal/any_mixed_clauses.glp"
    "$MODED_DIR/invalid/universal/any_reduce_pattern.glp"
    "$MODED_DIR/invalid/universal/any_struct_at_input.glp"
    "$MODED_DIR/invalid/universal/any_struct_at_output.glp"
    # Note: any_constant_at_output.glp removed - it's in valid/, not invalid/
)

# =============================================================================
# SRSW TESTS - Rejected by parser for SRSW violations (separate category)
# =============================================================================
SRSW_FILES=(
    "$TEST_DIR/negative/head/merge_reader_at_input.glp"
    "$TEST_DIR/negative/head/merge_writer_at_output.glp"
)

# Build REPL input: load all files, then quit
REPL_INPUT=""
for f in "${POSITIVE_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
for f in "${NEGATIVE_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
for f in "${SRSW_FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
REPL_INPUT+=":quit"$'\n'

# Run single REPL session
TOTAL_POSITIVE=${#POSITIVE_FILES[@]}
TOTAL_NEGATIVE=${#NEGATIVE_FILES[@]}
TOTAL_SRSW=${#SRSW_FILES[@]}
echo "Running $TOTAL_POSITIVE positive + $TOTAL_NEGATIVE negative + $TOTAL_SRSW SRSW tests..."
echo ""

output=$(echo "$REPL_INPUT" | dart run "$REPL" 2>&1)

# Parse results
PASS=0
FAIL=0

echo "--- Positive Tests (should load successfully) ---"
for f in "${POSITIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    # Check if this file had type errors or SRSW violations
    # Use specific pattern "Type errors in $f" to avoid matching errors from other files
    if echo "$output" | grep -q "Type errors in $f"; then
        echo "FAIL: $name (unexpected type errors)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "SRSW violations in $f"; then
        echo "FAIL: $name (unexpected SRSW violations)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Loaded: $f"; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (failed to load)"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "--- Negative Tests (should be rejected by type checker) ---"
for f in "${NEGATIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    # Check if this file was rejected (type errors, SRSW violations, or loading error)
    if echo "$output" | grep -B1 -A5 "$f" | grep -q "Type errors"; then
        echo "PASS: $name (rejected: type errors)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "SRSW violations"; then
        echo "PASS: $name (rejected: SRSW violation)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Error loading"; then
        echo "PASS: $name (rejected: loading error)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (should have been rejected)"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "--- SRSW Tests (should be rejected by parser for SRSW violations) ---"
for f in "${SRSW_FILES[@]}"; do
    name=$(basename "$f" .glp)
    # These should specifically fail SRSW, not type errors
    if echo "$output" | grep -B1 -A5 "$f" | grep -q "SRSW violations"; then
        echo "PASS: $name (rejected: SRSW violation)"
        PASS=$((PASS + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Type errors"; then
        echo "WARN: $name (rejected for type errors, expected SRSW)"
        PASS=$((PASS + 1))  # Still counts as pass since rejected
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Error loading"; then
        echo "PASS: $name (rejected: loading error)"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (should have been rejected for SRSW)"
        FAIL=$((FAIL + 1))
    fi
done

TOTAL=$((PASS + FAIL))

echo ""
echo "======================================"
echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
echo "======================================"

if [ $FAIL -eq 0 ]; then
    echo "ALL TYPE CHECKER TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
