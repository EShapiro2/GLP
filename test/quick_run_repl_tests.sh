#!/bin/bash
# GLP REPL Quick Test Suite
# Samples each feature with minimal tests for fast iteration

set -e

# Find dart executable
DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}

# Paths - run from glp_runtime directory, point to test files
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_RUNTIME="$SCRIPT_DIR/../glp_runtime"
GLP_DIR="$SCRIPT_DIR/../glp/test"
REPL="bin/glp_repl.dart"

cd "$GLP_RUNTIME"

PASS=0
FAIL=0
TOTAL=0

echo "======================================"
echo "   GLP REPL Quick Test Suite          "
echo "======================================"
echo ""

# Helper function to run a test
run_test() {
    local name="$1"
    local glp_file="$2"
    local query="$3"
    local expected_pattern="$4"

    TOTAL=$((TOTAL + 1))
    echo "--------------------------------------"
    echo "Test $TOTAL: $name"
    echo "  File: $glp_file"
    echo "  Query: $query"

    # Run REPL with the query
    local output=$($DART run "$REPL" <<EOF
$GLP_DIR/$glp_file
$query
:quit
EOF
2>&1)

    # Check if output contains expected pattern
    if echo "$output" | grep -q "$expected_pattern"; then
        echo "  PASS"
        PASS=$((PASS + 1))
    else
        echo "  FAIL"
        echo "  Expected pattern: $expected_pattern"
        echo "  Output:"
        echo "$output" | sed 's/^/    /'
        FAIL=$((FAIL + 1))
    fi
}

# Helper function to test SRSW violation detection
run_srsw_test() {
    local name="$1"
    local glp_file="$2"

    TOTAL=$((TOTAL + 1))
    echo "--------------------------------------"
    echo "Test $TOTAL: SRSW Check - $name"
    echo "  File: $glp_file"

    local output=$($DART run "$REPL" <<EOF
$GLP_DIR/$glp_file
:quit
EOF
2>&1)

    if echo "$output" | grep -q "SRSW violation"; then
        echo "  PASS (correctly rejected)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL (should have detected SRSW violation)"
        FAIL=$((FAIL + 1))
    fi
}

# ============================================
# BASIC (2 tests)
# ============================================

run_test "Hello World" \
    "hello.glp" \
    "hello." \
    "Hello from GLP!"

run_test "Simple Unification" \
    "p.glp" \
    "p(X)." \
    "X = a"

# ============================================
# STREAM PROCESSING (2 tests)
# ============================================

run_test "Merge small lists" \
    "merge.glp" \
    "merge([1], [a], Xs)." \
    "Xs = \[1, a\]"

run_srsw_test "SRSW violation detection" \
    "merge_with_reader.glp"

# ============================================
# METAINTERPRETER (2 tests)
# ============================================

run_test "Simple Run" \
    "run.glp" \
    "run(true)." \
    "→"

run_test "Merge via Meta (small)" \
    "run1.glp" \
    "run(merge([a],[b],X))." \
    "X = \[a, b\]"

# ============================================
# ARITHMETIC (3 tests)
# ============================================

run_test "Addition" \
    "arithmetic_fixed.glp" \
    "add(2, 3, X)." \
    "X = 5"

run_test "Multiplication" \
    "arithmetic_fixed.glp" \
    "multiply(3, 4, Y)." \
    "Y = 12"

run_test "Sum small list" \
    "sum_direct.glp" \
    "sumd([1,2,3], S)." \
    "S = 6"

# ============================================
# LIST OPERATIONS (3 tests)
# ============================================

run_test "Append" \
    "append.glp" \
    "append([a], [b], Zs)." \
    "Zs = \[a, b\]"

run_test "Reverse" \
    "reverse.glp" \
    "reverse([a,b], Ys)." \
    "Ys = \[b, a\]"

run_test "Copy" \
    "copy.glp" \
    "copy([x,y], Ys)." \
    "Ys = \[x, y\]"

# ============================================
# SORTING (3 tests - small inputs)
# ============================================

run_test "Quicksort [2,1]" \
    "qsort.glp" \
    "quicksort([2,1],X)." \
    "X = \[1, 2\]"

run_test "Insertion sort [3,1,2]" \
    "isort.glp" \
    "insertion_sort([3,1,2],X)." \
    "X = \[1, 2, 3\]"

run_test "Bubble sort [2,1]" \
    "bsort.glp" \
    "sort([2,1], Ys)." \
    "Ys = \[1, 2\]"

# ============================================
# LOGIC GATES (1 test)
# ============================================

run_test "AND gate" \
    "gates.glp" \
    "and([one,zero], [one,one], Out)." \
    "Out = \[one, zero\]"

# ============================================
# RECURSIVE ARITHMETIC (3 tests - small inputs)
# ============================================

run_test "Fibonacci fib(3)" \
    "fib.glp" \
    "fib(3, F)." \
    "F = 2"

run_test "Factorial factorial(3)" \
    "factorial.glp" \
    "factorial(3, F)." \
    "F = 6"

run_test "Hanoi(1)" \
    "hanoi.glp" \
    "hanoi(1, a, c, M)." \
    "→ 6 goals"

# ============================================
# META + ARITHMETIC (2 tests - small inputs)
# ============================================

run_test "Meta: factorial(3)" \
    "factorial.glp" \
    "run_fact(factorial(3, F))." \
    "F = 6"

run_test "Meta: sum small" \
    "sum_list.glp" \
    "run_sum(sum([1,2], S))." \
    "S = 3"

# ============================================
# STDLIB: UNIV (2 tests)
# ============================================

run_test "Univ: compose" \
    "hello.glp" \
    "T =.. [foo, a]." \
    "T = foo(a)"

run_test "Univ: decompose" \
    "hello.glp" \
    "bar(x) =.. L." \
    "L = \[bar, x\]"

# ============================================
# STDLIB: UNIFY (2 tests)
# ============================================

run_test "Unify: atom" \
    "hello.glp" \
    "X = foo." \
    "X = foo"

run_test "Unify: structure" \
    "hello.glp" \
    "X = f(a)." \
    "X = f(a)"

# ============================================
# STDLIB: ASSIGN (3 tests)
# ============================================

run_test "Assign: number" \
    "hello.glp" \
    "X := 5." \
    "X = 5"

run_test "Assign: expression" \
    "hello.glp" \
    "X := 2 + 3." \
    "X = 5"

run_test "Assign: precedence" \
    "hello.glp" \
    "X := 2 + 3 * 4." \
    "X = 14"

# ============================================
# PRIMES (1 test - small input)
# ============================================

run_test "Primes up to 10" \
    "primes.glp" \
    "primes(10, Ps)." \
    "Ps = \\[2, 3, 5, 7\\]"

# ============================================
# SUMMARY
# ============================================

echo ""
echo "======================================"
echo "SUMMARY"
echo "======================================"
echo "Total:  $TOTAL tests"
echo "Passed: $PASS tests ($(( PASS * 100 / TOTAL ))%)"
echo "Failed: $FAIL tests"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
