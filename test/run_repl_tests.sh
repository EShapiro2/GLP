#!/bin/bash
# GLP REPL Test Suite
# Runs all working GLP programs through the REPL and verifies output

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
echo "   GLP REPL Test Suite                  "
echo "======================================"
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

    # Run REPL - just try to load the file
    local output=$($DART run "$REPL" <<EOF
$GLP_DIR/$glp_file
:quit
EOF
2>&1)

    # Check if output contains SRSW violation message
    if echo "$output" | grep -q "SRSW violation"; then
        echo "  PASS (correctly rejected)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL (should have detected SRSW violation)"
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

run_srsw_test "merge_with_reader.glp has duplicate writer X" \
    "merge_with_reader.glp"

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
    "P = person"

# ============================================
# LIST OPERATIONS (From CP Book)
# ============================================

run_test "Append two lists" \
    "append.glp" \
    "append([a,b], [c,d], Zs)." \
    "Zs = \[a, b, c, d\]"

run_test "Append empty list" \
    "append.glp" \
    "append([], [x,y], Zs)." \
    "Zs = \[x, y\]"

run_test "Append to empty list" \
    "append.glp" \
    "append([a,b], [], Zs)." \
    "Zs = \[a, b\]"

run_test "Reverse list" \
    "reverse.glp" \
    "reverse([a,b,c], Ys)." \
    "Ys = \[c, b, a\]"

run_test "Reverse empty list" \
    "reverse.glp" \
    "reverse([], Ys)." \
    "Ys = \[\]"

run_test "Reverse single element" \
    "reverse.glp" \
    "reverse([x], Ys)." \
    "Ys = \[x\]"

run_test "Copy list" \
    "copy.glp" \
    "copy([a,b,c], Ys)." \
    "Ys = \[a, b, c\]"

run_test "Copy empty list" \
    "copy.glp" \
    "copy([], Ys)." \
    "Ys = \[\]"

run_test "Ordered merge" \
    "opmerge.glp" \
    "opmerge([1,3,5], [2,4,6], Zs)." \
    "Zs = \[1, 2, 3, 4, 5, 6\]"

run_test "Ordered merge with duplicates" \
    "opmerge.glp" \
    "opmerge([1,2,3], [2,3,4], Zs)." \
    "Zs = \[1, 2, 3, 4\]"

run_test "Ordered merge empty first" \
    "opmerge.glp" \
    "opmerge([], [1,2], Zs)." \
    "Zs = \[1, 2\]"

run_test "Fair stable merge" \
    "fsmerge.glp" \
    "fsmerge([a,b,c], [x,y,z], Zs)." \
    "Zs = \[a, x, b, y, c, z\]"

run_test "Fair stable merge unequal" \
    "fsmerge.glp" \
    "fsmerge([a,b], [x,y,z], Zs)." \
    "Zs = \[a, x, b, y, z\]"

# ============================================
# LOGIC GATES (Circuit Simulation)
# ============================================

run_test "AND gate" \
    "gates.glp" \
    "and([one,zero,one], [one,one,zero], Out)." \
    "Out = \[one, zero, zero\]"

run_test "OR gate" \
    "gates.glp" \
    "or([one,zero,one], [one,one,zero], Out)." \
    "Out = \[one, one, one\]"

run_test "AND gate all ones" \
    "gates.glp" \
    "and([one,one], [one,one], Out)." \
    "Out = \[one, one\]"

run_test "OR gate all zeros" \
    "gates.glp" \
    "or([zero,zero], [zero,zero], Out)." \
    "Out = \[zero, zero\]"

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

run_test "Bubble sort" \
    "bsort.glp" \
    "sort([3,1,4,1,5], Ys)." \
    "Ys = \[1, 1, 3, 4, 5\]"

run_test "Bubble sort empty" \
    "bsort.glp" \
    "sort([], Ys)." \
    "Ys = \[\]"

run_test "Bubble sort single" \
    "bsort.glp" \
    "sort([7], Ys)." \
    "Ys = \[7\]"

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
    "→ 4 goals"

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
# ARITHMETIC TESTS (Working with := operator)
# These use the _direct.glp files with accumulator style
# ============================================

run_test "Sum list (arithmetic)" \
    "sum_direct.glp" \
    "sum([1,2,3,4,5], S)." \
    "S = 15"

run_test "Sum empty list (arithmetic)" \
    "sum_direct.glp" \
    "sum([], S)." \
    "S = 0"

run_test "Inner product (arithmetic)" \
    "ip_direct.glp" \
    "ip([1,2,3], [4,5,6], S)." \
    "S = 32"

run_test "Inner product single (arithmetic)" \
    "ip_direct.glp" \
    "ip([2], [3], S)." \
    "S = 6"

run_test "Multiply stream (arithmetic)" \
    "multiply_direct.glp" \
    "multiply(3, [1,2,3,4], Ys)." \
    "Ys = \[3, 6, 9, 12\]"

run_test "Multiply stream empty (arithmetic)" \
    "multiply_direct.glp" \
    "multiply(5, [], Ys)." \
    "Ys = \[\]"

run_test "Fibonacci fib(0) (arithmetic)" \
    "fib_direct.glp" \
    "fib(0, F)." \
    "F = 0"

run_test "Fibonacci fib(1) (arithmetic)" \
    "fib_direct.glp" \
    "fib(1, F)." \
    "F = 1"

run_test "Fibonacci fib(2) (arithmetic)" \
    "fib_direct.glp" \
    "fib(2, F)." \
    "F = 1"

run_test "Hanoi base case hanoi(0)" \
    "hanoi.glp" \
    "hanoi(0, a, c, M)." \
    "→ 1 goals"

run_test "Hanoi one disk hanoi(1)" \
    "hanoi.glp" \
    "hanoi(1, a, c, M)." \
    "→ 6 goals"

run_test "Hanoi two disks hanoi(2)" \
    "hanoi.glp" \
    "hanoi(2, a, c, M)." \
    "→ 16 goals"

# Tests using full arithmetic files (tail-recursive with accumulator)
run_test "Sum list (full file)" \
    "sum_list.glp" \
    "sum([1,2,3,4,5], S)." \
    "S = 15"

run_test "Inner product (full file)" \
    "inner_product.glp" \
    "ip([1,2,3], [4,5,6], S)." \
    "S = 32"

run_test "Multiply stream (full file)" \
    "multiply.glp" \
    "multiply(3, [1,2,3,4], Ys)." \
    "Ys = \\[3, 6, 9, 12\\]"

# Base cases from fib.glp and factorial.glp
run_test "Fibonacci fib(0) full file" \
    "fib.glp" \
    "fib(0, F)." \
    "F = 0"

run_test "Fibonacci fib(1) full file" \
    "fib.glp" \
    "fib(1, F)." \
    "F = 1"

run_test "Factorial factorial(1)" \
    "factorial.glp" \
    "factorial(1, F)." \
    "F = 1"

run_test "Factorial factorial(2)" \
    "factorial.glp" \
    "factorial(2, F)." \
    "F = 2"

# ============================================
# NON-TAIL-RECURSIVE ARITHMETIC (Now Working!)
# Reader reactivation chain fix enabled these
# ============================================

run_test "Fibonacci fib(3) non-tail-recursive" \
    "fib.glp" \
    "fib(3, F)." \
    "F = 2"

run_test "Fibonacci fib(10) non-tail-recursive" \
    "fib.glp" \
    "fib(10, F)." \
    "F = 55"

run_test "Factorial factorial(3) non-tail-recursive" \
    "factorial.glp" \
    "factorial(3, F)." \
    "F = 6"

run_test "Factorial factorial(5) non-tail-recursive" \
    "factorial.glp" \
    "factorial(5, F)." \
    "F = 120"

# ============================================
# METAINTERPRETER + ARITHMETIC (Working!)
# reduce((X?:=T), true) :- X:=T?. enabled these
# ============================================

run_test "Meta: run(factorial(5,F))" \
    "factorial.glp" \
    "run(factorial(5, F))." \
    "F = 120"

run_test "Meta: run(multiply(3,[1,2,3,4],Ys))" \
    "multiply.glp" \
    "run(multiply(3, [1,2,3,4], Ys))." \
    "Ys = \\[3, 6, 9, 12\\]"

run_test "Meta: run(sum([1,2,3,4,5],S))" \
    "sum_list.glp" \
    "run(sum([1,2,3,4,5], S))." \
    "S = 15"

run_test "Meta: run(ip([1,2,3],[4,5,6],S))" \
    "inner_product.glp" \
    "run(ip([1,2,3], [4,5,6], S))." \
    "S = 32"

run_test "Meta: run(hanoi(2,a,c,M))" \
    "hanoi.glp" \
    "run(hanoi(2, a, c, M))." \
    "→ 54 goals"

# ============================================
# UNIV (=..) TESTS - Structure decomposition/composition
# Uses stdlib/univ.glp which is auto-loaded
# ============================================

run_test "Univ: compose tuple from list [foo]" \
    "hello.glp" \
    "T =.. [foo]." \
    "T = foo()"

run_test "Univ: compose tuple from list [bar, x, y]" \
    "hello.glp" \
    "T =.. [bar, x, y]." \
    "T = bar(x, y)"

run_test "Univ: decompose foo(a, b) to list" \
    "hello.glp" \
    "foo(a, b) =.. L." \
    "L = \[foo, a, b\]"

run_test "Univ: decompose bar(1, 2, 3) to list" \
    "hello.glp" \
    "bar(1, 2, 3) =.. L." \
    "L = \[bar, 1, 2, 3\]"

# ============================================
# UNIFICATION (=) TESTS
# Uses stdlib/unify.glp which is auto-loaded
# Definition: X? = X. (same var as reader then writer)
# ============================================

run_test "Unify: X = foo binds X to atom" \
    "hello.glp" \
    "X = foo." \
    "X = foo"

run_test "Unify: X = 42 binds X to number" \
    "hello.glp" \
    "X = 42." \
    "X = 42"

run_test "Unify: X = foo(a, b) binds X to structure" \
    "hello.glp" \
    "X = foo(a, b)." \
    "X = foo(a, b)"

run_test "Unify: X = [1, 2, 3] binds X to list" \
    "hello.glp" \
    "X = [1, 2, 3]." \
    "X = \[1, 2, 3\]"

run_test "Unify: X = foo(bar(a)) binds X to nested structure" \
    "hello.glp" \
    "X = foo(bar(a))." \
    "X = foo(bar(a))"

run_test "Unify: X = Y? suspends when Y unbound" \
    "hello.glp" \
    "X = Y?." \
    "suspended"

# ============================================
# ASSIGNMENT (:=) TESTS
# Uses stdlib/assign.glp which is auto-loaded
# Base case: Result := N :- number(N?) | Result = N?.
# ============================================

run_test "Assign: X := 3 binds X to plain number" \
    "hello.glp" \
    "X := 3." \
    "X = 3"

run_test "Assign: X := 5 + 3 evaluates addition" \
    "hello.glp" \
    "X := 5 + 3." \
    "X = 8"

run_test "Assign: X := 10 - 4 evaluates subtraction" \
    "hello.glp" \
    "X := 10 - 4." \
    "X = 6"

run_test "Assign: X := 4 * 7 evaluates multiplication" \
    "hello.glp" \
    "X := 4 * 7." \
    "X = 28"

run_test "Assign: X := 20 / 4 evaluates division" \
    "hello.glp" \
    "X := 20 / 4." \
    "X = 5"

run_test "Assign: X := 5 + 3 * 2 respects precedence" \
    "hello.glp" \
    "X := 5 + 3 * 2." \
    "X = 11"

run_test "Assign: X := (5 + 3) * 2 respects parentheses" \
    "hello.glp" \
    "X := (5 + 3) * 2." \
    "X = 16"

run_test "Assign: X := -5 evaluates unary minus" \
    "hello.glp" \
    "X := -5." \
    "X = -5"

# ============================================
# TIME PREDICATES TESTS
# ============================================

run_test "Time: now(T) returns Unix timestamp" \
    "test_time.glp" \
    "get_time(T)." \
    "T = 1"

run_test "Time: wait_until succeeds for past timestamp" \
    "test_time.glp" \
    "past_time(1000000000000, X)." \
    "X = yes"

run_test "Time: wait_until fails for future timestamp" \
    "test_time.glp" \
    "past_time(9999999999999, X)." \
    "failed"

run_test "Time: wait(100) suspends then succeeds" \
    "test_time.glp" \
    "wait_test(X)." \
    "X = done"

# ============================================
# PRIMES TESTS (Sieve of Eratosthenes)
# ============================================

run_test "Primes up to 20" \
    "primes.glp" \
    "primes(20, Ps)." \
    "Ps = \\[2, 3, 5, 7, 11, 13, 17, 19\\]"

run_test "Primes up to 10" \
    "primes.glp" \
    "primes(10, Ps)." \
    "Ps = \\[2, 3, 5, 7\\]"

# ============================================
# FUTURE TESTS (not yet implemented)
# ============================================

FUTURE=0

# Helper for future tests - shows them as skipped
run_future_test() {
    local name="$1"
    local file="$2"
    local query="$3"
    local expected="$4"
    local reason="$5"

    FUTURE=$((FUTURE + 1))
    echo "--------------------------------------"
    echo "Future Test $FUTURE: $name"
    echo "  File: $file"
    echo "  Query: $query"
    echo "  Expected: $expected"
    echo "  SKIPPED ($reason)"
}

run_future_test "Meta: run(primes(10,Ps))" \
    "primes.glp" \
    "run(primes(10, Ps))." \
    "Ps = [2, 3, 5, 7]" \
    "needs reduce clause for := in primes.glp"

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
echo "Future: $FUTURE tests (see reasons above)"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
