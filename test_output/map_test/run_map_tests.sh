#!/bin/bash
# GLP Map Operations Test Suite
# Tests all map built-ins: map_new, map_put, _map_get, map_contains
# and the stdlib map_get wrapper.
# Includes O(1) lookup benchmark.

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/../.."
GLP_RUNTIME="$GLP_DIR/glp_runtime"
MAP_TEST="$SCRIPT_DIR"

cd "$GLP_RUNTIME"

# Compile REPL to kernel snapshot for faster startup
REPL_SNAPSHOT=".dart_tool/repl.dill"
NEEDS_RECOMPILE=false
if [ ! -f "$REPL_SNAPSHOT" ]; then
    NEEDS_RECOMPILE=true
elif [ -n "$(find lib bin -name '*.dart' -newer "$REPL_SNAPSHOT" 2>/dev/null | head -1)" ]; then
    NEEDS_RECOMPILE=true
fi
if [ "$NEEDS_RECOMPILE" = true ]; then
    echo "Compiling REPL snapshot..."
    mkdir -p .dart_tool
    $DART compile kernel -o "$REPL_SNAPSHOT" bin/glp_repl.dart 2>/dev/null || true
fi
if [ -f "$REPL_SNAPSHOT" ]; then
    REPL="$REPL_SNAPSHOT"
else
    REPL="bin/glp_repl.dart"
fi

echo "======================================"
echo "   GLP Map Operations Test Suite      "
echo "======================================"
echo ""

PASS=0
FAIL=0

check() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (expected: $pattern)"
        FAIL=$((FAIL + 1))
    fi
}

check_not() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  FAIL: $name (should NOT match: $pattern)"
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    fi
}

# =============================================================================
# Test 1: Basic map creation and put
# =============================================================================
echo "--- Test 1: map_new + map_put + map_contains ---"
t1=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_new.glp
test_new(X).
:quit
HEREDOC
2>&1)

check "map_new + map_put + map_contains" "X = yes" "$t1"
check "test_map_new loads" "Loaded:" "$t1"

# =============================================================================
# Test 2: _map_get retrieval
# =============================================================================
echo "--- Test 2: _map_get retrieval ---"
t2=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_get.glp
test_get(X).
:quit
HEREDOC
2>&1)

check "_map_get retrieves value" "X = 99" "$t2"
check "test_map_get succeeds" "succeeds" "$t2"

# =============================================================================
# Test 3: Key not found (map_contains fails)
# =============================================================================
echo "--- Test 3: Key not found → otherwise ---"
t3=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_missing.glp
test_missing(X).
:quit
HEREDOC
2>&1)

check "missing key returns not_found" "X = not_found" "$t3"

# =============================================================================
# Test 4: Multiple puts and gets
# =============================================================================
echo "--- Test 4: Multiple puts and gets ---"
t4=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_multi.glp
test_multi(X, Y).
:quit
HEREDOC
2>&1)

check "multi-get first key" "X = 1" "$t4"
check "multi-get second key" "Y = 2" "$t4"

# =============================================================================
# Test 5: Overwrite existing key
# =============================================================================
echo "--- Test 5: Overwrite existing key ---"
t5=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_overwrite.glp
test_overwrite(X).
:quit
HEREDOC
2>&1)

check "overwrite returns new value" "X = new_val" "$t5"

# =============================================================================
# Test 6: Stdlib map_get wrapper
# =============================================================================
echo "--- Test 6: Stdlib map_get/3 wrapper ---"
t6=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_stdlib_get.glp
test_stdlib(X).
:quit
HEREDOC
2>&1)

check "stdlib map_get retrieves value" "X = 42" "$t6"

# =============================================================================
# Test 7: map_get fallback with otherwise
# =============================================================================
echo "--- Test 7: map_get fallback with otherwise ---"
t7=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_get_fallback.glp
test_fallback(X).
:quit
HEREDOC
2>&1)

check "map_get fallback returns not_found" "X = not_found" "$t7"

# =============================================================================
# Test 8: Empty map contains nothing
# =============================================================================
echo "--- Test 8: Empty map contains nothing ---"
t8=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_empty_contains.glp
test_empty(X).
:quit
HEREDOC
2>&1)

check "empty map contains returns empty" "X = empty" "$t8"

# =============================================================================
# Test 9: Numeric and mixed keys
# =============================================================================
echo "--- Test 9: Numeric and mixed keys ---"
t9=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_numeric_keys.glp
test_int_key(X).
test_mixed_keys(Y, Z).
:quit
HEREDOC
2>&1)

check "integer key lookup" "X = hello" "$t9"
check "mixed key atom lookup" "Y = alice" "$t9"
check "mixed key int lookup" "Z = answer" "$t9"

# =============================================================================
# Test 10: SRSW-compliant map threading
# =============================================================================
echo "--- Test 10: SRSW-compliant threading ---"
t10=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_threading.glp
test_thread(X).
:quit
HEREDOC
2>&1)

check "threaded map final get" "X = 30" "$t10"

# =============================================================================
# Test 11: Map as agent state (integration)
# =============================================================================
echo "--- Test 11: Map as agent state ---"
t11=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_agent.glp
test_agent(X).
:quit
HEREDOC
2>&1)

check "agent state lookup" "X = 42" "$t11"

# =============================================================================
# Test 12: Maps with structured values
# =============================================================================
echo "--- Test 12: Structured values ---"
t12=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_struct_values.glp
test_struct_val(X).
:quit
HEREDOC
2>&1)

check "struct value stored and retrieved" "X = person(alice, 30)" "$t12"

# =============================================================================
# Test 13: Store 100 values, verify lookups at key 1, 50, 100
# =============================================================================
echo "--- Test 13: 100-entry map (correctness) ---"
t13=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_100.glp
test_100(A, B, C).
test_100_missing(X).
:quit
HEREDOC
2>&1)

check "100-entry map: key 1 → 10" "A = 10" "$t13"
check "100-entry map: key 50 → 500" "B = 500" "$t13"
check "100-entry map: key 100 → 1000" "C = 1000" "$t13"
check "100-entry map: key 101 → not_found" "X = not_found" "$t13"

# =============================================================================
# Test 16: Map with list-of-numbers values
# =============================================================================
echo "--- Test 16: List values (store, head, len, sum, multi, overwrite) ---"
t16=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_list_values.glp
test_list_val(A).
test_list_head(B).
test_list_len(C).
test_list_sum(D).
test_two_lists(E, F).
test_overwrite_list(G).
:quit
HEREDOC
2>&1)

check "list value stored and retrieved" "A = \[1, 2, 3\]" "$t16"
check "list head extracted" "B = 10" "$t16"
check "list length computed" "C = 5" "$t16"
check "list sum computed" "D = 100" "$t16"
check "two lists: alice head" "E = 1" "$t16"
check "two lists: bob head" "F = 10" "$t16"
check "overwrite list and sum" "G = 600" "$t16"

# =============================================================================
# Test 15: map_remove — basic removal
# =============================================================================
echo "--- Test 15: map_remove basic ---"
t15=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_remove.glp
test_remove(A, C, Gone).
test_remove_gone(X).
:quit
HEREDOC
2>&1)

check "remove: remaining key a" "A = 1" "$t15"
check "remove: remaining key c" "C = 3" "$t15"
check "remove: b is gone" "Gone = gone" "$t15"
check "remove: contains confirms absent" "X = not_found" "$t15"

# =============================================================================
# Test 17: map_keys — extract keys as list
# =============================================================================
echo "--- Test 17: map_keys ---"
t17=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_keys.glp
test_keys_len(Len).
test_keys_has_x(X).
test_keys_has_y(Y).
test_keys_has_z(Z).
test_keys_empty(Keys).
:quit
HEREDOC
2>&1)

check "keys: length is 3" "Len = 3" "$t17"
check "keys: contains x" "X = yes" "$t17"
check "keys: contains y" "Y = yes" "$t17"
check "keys: contains z" "Z = yes" "$t17"
check "keys: empty map → []" "Keys = \[\]" "$t17"

# =============================================================================
# Test 18: map_remove — edge cases
# =============================================================================
echo "--- Test 18: map_remove edge cases ---"
t18=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_remove_edge.glp
test_remove_missing(X).
test_remove_last(Y).
test_remove_readd(Z).
:quit
HEREDOC
2>&1)

check "remove: missing key no crash" "X = empty" "$t18"
check "remove: last key → empty" "Y = empty" "$t18"
check "remove: re-add after remove" "Z = new_value" "$t18"

# =============================================================================
# Test 19: map_keys + map_remove combined
# =============================================================================
echo "--- Test 19: map_keys + map_remove combined ---"
t19=$($DART run "$REPL" <<HEREDOC
$MAP_TEST/test_map_keys_remove.glp
test_kr_len(Len).
test_kr_no_b(HasB).
test_kr_all_removed(Len2).
:quit
HEREDOC
2>&1)

check "keys after remove: length 2" "Len = 2" "$t19"
check "keys after remove: b absent" "HasB = no" "$t19"
check "keys after remove all: length 0" "Len2 = 0" "$t19"

# =============================================================================
# Test 14: O(1) lookup benchmark
# =============================================================================
echo "--- Test 14: O(1) lookup benchmark ---"
echo ""
echo "  Strategy: Build maps of size 10, 100, 1000."
echo "  Do 50000 lookups on each. Measure total time."
echo "  Subtract build-only time to isolate lookup cost."
echo "  If lookup is O(1), lookup times should be similar"
echo "  regardless of map size."
echo ""

LOOKUPS=50000

# Use python3 for precise timing (avoids shell `time` parsing issues)
measure() {
    python3 -c "
import subprocess, time, sys
start = time.perf_counter()
proc = subprocess.run(
    ['$DART', 'run', '$REPL'],
    input=sys.stdin.read(),
    capture_output=True, text=True
)
elapsed = time.perf_counter() - start
print(f'{elapsed:.4f}')
" <<HEREDOC
$MAP_TEST/test_map_benchmark.glp
$1
:quit
HEREDOC
}

# Run each measurement 3 times and take the median for stability
measure_median() {
    local t1 t2 t3
    t1=$(measure "$1")
    t2=$(measure "$1")
    t3=$(measure "$1")
    python3 -c "print(f'{sorted([$t1, $t2, $t3])[1]:.4f}')"
}

echo "  Measuring (3 trials each, median selected)..."
echo ""

# Measure build-only time (baseline)
BUILD_10=$(measure_median "build_only(10, X).")
BUILD_100=$(measure_median "build_only(100, X).")
BUILD_1000=$(measure_median "build_only(1000, X).")

# Measure build + lookup time
BENCH_10=$(measure_median "bench(10, $LOOKUPS, X).")
BENCH_100=$(measure_median "bench(100, $LOOKUPS, X).")
BENCH_1000=$(measure_median "bench(1000, $LOOKUPS, X).")

# Calculate isolated lookup times
L10=$(python3 -c "print(f'{max($BENCH_10 - $BUILD_10, 0.001):.4f}')")
L100=$(python3 -c "print(f'{max($BENCH_100 - $BUILD_100, 0.001):.4f}')")
L1000=$(python3 -c "print(f'{max($BENCH_1000 - $BUILD_1000, 0.001):.4f}')")

echo "  Results ($LOOKUPS lookups per trial, median of 3 runs):"
echo "  ┌─────────────┬────────────┬─────────────┬──────────────┐"
echo "  │ Map Size    │ Build (s)  │ Total (s)   │ Lookup (s)   │"
echo "  ├─────────────┼────────────┼─────────────┼──────────────┤"
printf "  │ %-11s │ %-10s │ %-11s │ %-12s │\n" "10" "${BUILD_10}" "${BENCH_10}" "${L10}"
printf "  │ %-11s │ %-10s │ %-11s │ %-12s │\n" "100" "${BUILD_100}" "${BENCH_100}" "${L100}"
printf "  │ %-11s │ %-10s │ %-11s │ %-12s │\n" "1000" "${BUILD_1000}" "${BENCH_1000}" "${L1000}"
echo "  └─────────────┴────────────┴─────────────┴──────────────┘"
echo ""

# O(1) check: lookup time for size 1000 should be < 3x lookup time for size 10
# (100x size increase → if O(1), ratio should stay ~1.0)
RATIO=$(python3 -c "
l10 = max($BENCH_10 - $BUILD_10, 0.001)
l1000 = max($BENCH_1000 - $BUILD_1000, 0.001)
print(f'{l1000 / l10:.1f}')
")

echo "  Lookup time ratio (size 1000 / size 10): ${RATIO}x"
echo "  (100x map size increase → if O(1), ratio should be ~1.0)"
echo ""

IS_O1=$(python3 -c "print(1 if float('$RATIO') < 3.0 else 0)")
if [ "$IS_O1" = "1" ]; then
    echo "  PASS: O(1) lookup confirmed (ratio ${RATIO}x < 3.0x threshold)"
    PASS=$((PASS + 1))
else
    echo "  FAIL: Lookup appears non-O(1) (ratio ${RATIO}x >= 3.0x threshold)"
    FAIL=$((FAIL + 1))
fi

# =============================================================================
# Summary
# =============================================================================
echo ""
echo "======================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "======================================"

if [ "$FAIL" -gt 0 ]; then
    echo "SOME TESTS FAILED!"
    exit 1
else
    echo "ALL MAP TESTS PASSED!"
fi
