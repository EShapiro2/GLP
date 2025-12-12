#!/bin/bash
# Module System REPL Tests
# These tests verify actual REPL usage of modules

cd "$(dirname "$0")/../glp_runtime"

REPL="./glp_repl"
PASS=0
FAIL=0

run_test() {
    local name="$1"
    local input="$2"
    local expected="$3"

    echo -n "Test: $name ... "
    result=$(echo -e "$input" | $REPL 2>&1)

    if echo "$result" | grep -qE "$expected"; then
        echo "PASS"
        ((PASS++))
    else
        echo "FAIL"
        echo "  Expected: $expected"
        echo "  Got: $result"
        ((FAIL++))
    fi
}

# ===========================================
# TEST 1: Simple cross-module RPC
# ===========================================
run_test "simple RPC: math#double" \
    "test_modules/math.glp\ntest_modules/main.glp\ntest_double(R)." \
    "R = 10"

# ===========================================
# TEST 2: Recursive cross-module RPC
# ===========================================
run_test "recursive RPC: math#factorial" \
    "test_modules/math.glp\ntest_modules/main.glp\ntest_factorial(R)." \
    "R = 120"

# ===========================================
# TEST 3: Multi-module chain A → B → C
# ===========================================
run_test "chain RPC: A→B→C" \
    "test_modules/utils.glp\ntest_modules/chain_b.glp\ntest_modules/chain_a.glp\nrun(4, R)." \
    "R = 12"

# ===========================================
# TEST 4: Dynamic RPC (Transmit) - module as variable
# NOTE: Parser doesn't support M? # goal syntax in REPL goals yet
# ===========================================
# run_test "dynamic RPC: Module variable" \
#     "test_modules/math.glp\nM = math, M? # double(7, R)." \
#     "R = 14"

# ===========================================
# TEST 5: Meta-interpreter with math_rules using reduce/2
# NOTE: Requires dynamic RPC (Module? # goal) which isn't supported yet
# ===========================================
# run_test "meta-interpreter: math_rules double" \
#     "test_modules/math_rules.glp\ntest_modules/meta.glp\nrun(double(5, R), math_rules)." \
#     "R = 10"

# ===========================================
# TEST 6: Meta-interpreter with different goal
# NOTE: Requires dynamic RPC (Module? # goal) which isn't supported yet
# ===========================================
# run_test "meta-interpreter: math_rules triple" \
#     "test_modules/math_rules.glp\ntest_modules/meta.glp\nrun(triple(4, R), math_rules)." \
#     "R = 12"

# ===========================================
# TEST 7: Missing module - RPC suspends (module not loaded)
# ===========================================
run_test "missing module: RPC suspends" \
    "test_modules/main.glp\ntest_double(R)." \
    "suspended"

# ===========================================
# TEST 8: Error - unexported procedure
# ===========================================
run_test "error: unexported procedure" \
    "test_modules/math.glp\nmath # private_helper(X)." \
    "unknown|not exported|error|Error"

# ===========================================
# TEST 9: Backwards compatibility - no module declaration
# ===========================================
run_test "backwards compat: no module decl" \
    "factorial.glp\nfactorial(5, R)." \
    "R = 120"

# ===========================================
# SUMMARY
# ===========================================
echo ""
echo "================================"
echo "Results: $PASS passed, $FAIL failed"
echo "================================"

if [ $FAIL -eq 0 ]; then
    exit 0
else
    exit 1
fi
