#!/bin/bash
# Test Batch 2: List Processing programs
# Runs all tests in a SINGLE REPL session

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNTIME_DIR="$SCRIPT_DIR/../glp_runtime"
BOOK_DIR="$SCRIPT_DIR/../programs/typed_book/recursive/list_processing"
REPL="bin/glp_repl_typed.dart"

cd "$RUNTIME_DIR"

echo "======================================"
echo "   Batch 2: List Processing Test      "
echo "======================================"
echo ""

FILES=(
    "$BOOK_DIR/append.glp"
    "$BOOK_DIR/copy.glp"
    "$BOOK_DIR/delete.glp"
    "$BOOK_DIR/dl_append.glp"
    "$BOOK_DIR/filter_even.glp"
    "$BOOK_DIR/is_list.glp"
    "$BOOK_DIR/length.glp"
    "$BOOK_DIR/map_inc.glp"
    "$BOOK_DIR/maxlist.glp"
    "$BOOK_DIR/member.glp"
    "$BOOK_DIR/nth.glp"
    "$BOOK_DIR/prefix.glp"
    "$BOOK_DIR/reverse.glp"
    "$BOOK_DIR/reverse_naive.glp"
    "$BOOK_DIR/translate.glp"
)

# Build REPL input
REPL_INPUT=""
for f in "${FILES[@]}"; do
    REPL_INPUT+="$f"$'\n'
done
REPL_INPUT+=":quit"$'\n'

echo "Running ${#FILES[@]} tests in single REPL session..."
echo ""

output=$(echo "$REPL_INPUT" | dart run "$REPL" 2>&1)

# Parse results
PASS=0
FAIL=0

for f in "${FILES[@]}"; do
    name=$(basename "$f" .glp)
    
    if echo "$output" | grep -q "Type errors in.*$name.glp"; then
        echo "FAIL: $name (type errors)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "SRSW violations.*$name.glp"; then
        echo "FAIL: $name (SRSW violation)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Error loading.*$name.glp"; then
        # Extract error reason
        reason=$(echo "$output" | grep "Error loading.*$name.glp" | sed 's/.*: //')
        echo "FAIL: $name ($reason)"
        FAIL=$((FAIL + 1))
    elif echo "$output" | grep -q "Loaded:.*$name.glp"; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "????: $name"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "======================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "======================================"

# Save output
echo "$output" > "$SCRIPT_DIR/batch2_output.txt"
echo ""
echo "Full output saved to $SCRIPT_DIR/batch2_output.txt"
