#!/bin/bash
# GLP Map Operations Test Suite
# Tests all map built-ins: map_new, map_put, _map_get, map_contains
# and the stdlib map_get wrapper.

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
