#!/bin/bash
# GLP REPL Test Suite
# Runs all working GLP programs through the REPL and verifies output

set -e

PASS=0
FAIL=0
TOTAL=0

echo "╔════════════════════════════════════════╗"
echo "║   GLP REPL Test Suite                  ║"
echo "╚════════════════════════════════════════╝"
echo ""
echo "Running all GLP programs through REPL..."
echo ""

# Helper function to run a test
run_test() {
    local name="$1"
    local glp_file="$2"
    local query="$3"
    local expected_pattern="$4"

    TOTAL=$((TOTAL + 1))
    echo "────────────────────────────────────────"
    echo "Test $TOTAL: $name"
    echo "  File: $glp_file"
    echo "  Query: $query"

    # Run REPL with the query
    local output=$(dart glp_repl.dart <<EOF
$glp_file
$query
:quit
EOF
2>&1)

    # Check if output contains expected pattern
    if echo "$output" | grep -q "$expected_pattern"; then
        echo "  ✅ PASS"
        PASS=$((PASS + 1))
    else
        echo "  ❌ FAIL"
        echo "  Expected pattern: $expected_pattern"
        echo "  Output:"
        echo "$output" | sed 's/^/    /'
        FAIL=$((FAIL + 1))
    fi
}

# ============================================
# BASIC TESTS
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
# STREAM PROCESSING TESTS
# ============================================

run_test "Merge [1,2,3] and [a,b]" \
    "merge.glp" \
    "merge([1,2,3], [a,b], Xs)." \
    "Xs = \[1, a, 2, b, 3\]"

run_test "Merge Standalone" \
    "merge_standalone.glp" \
    "merge([1,2], [a,b], Xs)." \
    "Xs = \[1, a, 2, b\]"

run_test "Merge with Reader" \
    "merge_with_reader.glp" \
    "test_merge." \
    "→"

# ============================================
# METAINTERPRETER TESTS
# ============================================

run_test "Clause Lookup" \
    "clause.glp" \
    "clause(p(a), B)." \
    "B = true"

run_test "Simple Run" \
    "run.glp" \
    "run(true)." \
    "→"

run_test "Merge via Metainterpreter (SRSW fix)" \
    "run1.glp" \
    "run(merge([a,b],[b],X))." \
    "X = \[a, b, b\]"

run_test "Insertion Sort via Metainterpreter" \
    "isort.glp" \
    "run(insertion_sort([3,4,2,3,6,1,2],Xs))." \
    "Xs = \[1, 2, 2, 3, 3, 4, 6\]"

# ============================================
# ARITHMETIC TESTS (Fixed Versions)
# ============================================

run_test "Addition 5+3" \
    "arithmetic_fixed.glp" \
    "add(5, 3, X)." \
    "X = 8"

run_test "Multiplication 4*7" \
    "arithmetic_fixed.glp" \
    "multiply(4, 7, Y)." \
    "Y = 28"

run_test "Compound (2*3)+4" \
    "arithmetic_fixed.glp" \
    "compute(Z)." \
    "Z = 10"

# ============================================
# STRUCTURE TESTS
# ============================================

run_test "Structure Demo" \
    "struct_demo.glp" \
    "build_person(P)." \
    "P =" || true  # May fail due to numeric parsing

# ============================================
# SORTING TESTS
# ============================================

run_test "Quicksort empty list" \
    "qsort.glp" \
    "quicksort([],X)." \
    "X = \[\]"

run_test "Quicksort single element" \
    "qsort.glp" \
    "quicksort([1],X)." \
    "X = \[1\]"

run_test "Insertion sort empty list" \
    "isort.glp" \
    "insertion_sort([],X)." \
    "X = \[\]"

run_test "Insertion sort single element" \
    "isort.glp" \
    "insertion_sort([3],X)." \
    "X = \[3\]"

run_test "Insertion sort two elements" \
    "isort.glp" \
    "insertion_sort([3,4],X)." \
    "X = \[3, 4\]"

# ============================================
# QUICKSORT ADDITIONAL TESTS
# ============================================

run_test "Quicksort two elements [1,2]" \
    "qsort.glp" \
    "quicksort([1,2],X)." \
    "X = \[1, 2\]"

run_test "Quicksort larger list [1,6,4,2,7,4,2,6]" \
    "qsort.glp" \
    "quicksort([1,6,4,2,7,4,2,6],X)." \
    "X = \[1, 2, 2, 4, 4, 6, 6, 7\]"

run_test "Quicksort five elements [1,3,4,2,5]" \
    "qsort.glp" \
    "quicksort([1,3,4,2,5],X)." \
    "X = \[1, 2, 3, 4, 5\]"

run_test "Quicksort with non-number fails" \
    "qsort.glp" \
    "quicksort([a],X)." \
    "X = <unbound>"

run_test "Quicksort with unbound tail suspends" \
    "qsort.glp" \
    "quicksort([1|X?],Y)." \
    "Y = <unbound>"

# ============================================
# METAINTERPRETER MERGE TESTS
# ============================================

run_test "Metainterpreter: merge([a],[b],X)" \
    "run1.glp" \
    "run(merge([a],[b],X))." \
    "X = \[a, b\]"

run_test "Metainterpreter: merge([a],[b,c,d],X)" \
    "run1.glp" \
    "run(merge([a],[b,c,d],X))." \
    "X = \[a, b, c, d\]"

run_test "Metainterpreter: run2(X) - shared variable test" \
    "run1.glp" \
    "run2(X)." \
    "X = \[\]"

run_test "Metainterpreter: merge chain with shared vars" \
    "run1.glp" \
    "run((merge([a],[b],X),merge(X?,[c],Y)))." \
    "Y = \[a, c, b\]"

# ============================================
# METAINTERPRETER QUICKSORT TESTS
# ============================================

run_test "Metainterpreter: quicksort([],X)" \
    "qsort.glp" \
    "run(quicksort([],X))." \
    "X = \[\]"

run_test "Metainterpreter: quicksort([1],X)" \
    "qsort.glp" \
    "run(quicksort([1],X))." \
    "X = \[1\]"

run_test "Nested metainterpreter: run1(run(quicksort([1],X)))" \
    "qsort.glp" \
    "run1(run(quicksort([1],X)))." \
    "X = \[1\]"

run_test "Nested metainterpreter: run1(run(quicksort([1,4,3,2,4,5],X)))" \
    "qsort.glp" \
    "run1(run(quicksort([1,4,3,2,4,5],X)))." \
    "X = \[1, 2, 3, 4, 4, 5\]"

# ============================================
# SUMMARY
# ============================================

echo ""
echo "════════════════════════════════════════"
echo "SUMMARY"
echo "════════════════════════════════════════"
echo "Total:  $TOTAL tests"
echo "Passed: $PASS tests ($(( PASS * 100 / TOTAL ))%)"
echo "Failed: $FAIL tests"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "✅ ALL TESTS PASSED!"
    exit 0
else
    echo "❌ SOME TESTS FAILED"
    exit 1
fi
