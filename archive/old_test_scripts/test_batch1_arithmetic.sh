#!/bin/bash
# Test Batch 1: Arithmetic Trees programs
# Runs all tests in a SINGLE REPL session
# Usage: bash test_batch1_arithmetic.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$SCRIPT_DIR/../glp_runtime"
BOOK_DIR="$SCRIPT_DIR/../programs/typed_book/recursive/arithmetic_trees"
REPL="bin/glp_repl_typed.dart"

cd "$RUNTIME_DIR"

echo "======================================"
echo "   Batch 1: Arithmetic Trees Test     "
echo "======================================"
echo ""

FILES=(
    "$BOOK_DIR/natural_numbers.glp"
    "$BOOK_DIR/plus.glp"
    "$BOOK_DIR/times.glp"
    "$BOOK_DIR/lesseq.glp"
    "$BOOK_DIR/min.glp"
    "$BOOK_DIR/exp.glp"
    "$BOOK_DIR/ackermann.glp"
    "$BOOK_DIR/factorial.glp"
    "$BOOK_DIR/fibonacci.glp"
    "$BOOK_DIR/gcd_integer.glp"
    "$BOOK_DIR/hanoi.glp"
    "$BOOK_DIR/primes.glp"
    "$BOOK_DIR/sum_list.glp"
)

# Build REPL input: load all files, then quit
REPL_INPUT=""
for f in "${FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
REPL_INPUT+=":quit"$'\n'

# Run single REPL session
echo "Running ${#FILES[@]} tests in single REPL session..."
echo ""

output=$(echo "$REPL_INPUT" | dart run "$REPL" 2>&1)

# Parse results
PASS=0
FAIL=0

for f in "${FILES[@]}"; do
    name=$(basename "$f" .glp)
    
    # Check if this file had type errors, SRSW violations, or loading error
    if echo "$output" | grep -B1 -A5 "$f" | grep -q "Type errors"; then
        echo "FAIL: $name (type errors)"
        # Show first error line
        echo "      $(echo "$output" | grep -A10 "$f" | grep -E "^\s+-" | head -1)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "SRSW violations"; then
        echo "FAIL: $name (SRSW violation)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -B1 -A5 "$f" | grep -q "Error loading"; then
        echo "FAIL: $name (loading error)"
        # Show error details
        echo "      $(echo "$output" | grep -A3 "Error loading" | grep -v "Error loading" | head -1)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Loaded: $f"; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (unknown)"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "======================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "======================================"

# Save full output for debugging
echo "$output" > "$SCRIPT_DIR/batch1_output.txt"
echo ""
echo "Full output saved to $SCRIPT_DIR/batch1_output.txt"
