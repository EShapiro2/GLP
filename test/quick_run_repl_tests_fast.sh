#!/bin/bash
# GLP REPL Quick Test Suite - FAST VERSION (~2s)
# Tests basic features in single REPL session
# NOTE: Some recursive/arithmetic tests skipped - use full_run_repl_tests.sh for complete coverage

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_RUNTIME="$SCRIPT_DIR/../glp_runtime"
GLP_DIR="$SCRIPT_DIR/../glp/test"
REPL="bin/glp_repl.dart"

cd "$GLP_RUNTIME"

echo "======================================"
echo "   GLP REPL Quick Test Suite (FAST)   "
echo "======================================"
echo ""

# Load files and run queries in single session
output=$($DART run "$REPL" <<REPL_INPUT
$GLP_DIR/hello.glp
$GLP_DIR/p.glp
$GLP_DIR/merge.glp
$GLP_DIR/run.glp
$GLP_DIR/run1.glp
$GLP_DIR/append.glp
$GLP_DIR/reverse.glp
$GLP_DIR/copy.glp
$GLP_DIR/qsort.glp
$GLP_DIR/isort.glp
$GLP_DIR/bsort.glp
$GLP_DIR/gates.glp
$GLP_DIR/factorial.glp
$GLP_DIR/primes.glp
$GLP_DIR/fib.glp
$GLP_DIR/sum_list.glp
$GLP_DIR/hanoi.glp
hello.
p(X).
merge([1], [a], Xs).
run(true).
run(merge([a],[b],X)).
append([a], [b], Zs).
reverse([a,b], Ys).
copy([x,y], Ys).
quicksort([2,1],X).
insertion_sort([3,1,2],X).
sort([2,1], Ys).
and([one,zero], [one,one], Out).
factorial(3, F).
primes(10, Ps).
fib(3, F2).
sum([1,2,3], S).
hanoi(1, a, c, M).
run_fact(factorial(3, F3)).
run_sum(sum([1,2], S2)).
T =.. [foo, a].
bar(x) =.. L.
X = foo.
X = f(a).
X := 5.
X := 2 + 3.
X := 2 + 3 * 4.
:quit
REPL_INPUT
2>&1)

# Tests that work reliably in single-session mode
declare -a tests=(
    "Hello World:Hello from GLP!"
    "Simple Unification:X = a"
    "Merge:Xs = \[1, a\]"
    "Simple Run:run(true)"
    "Meta merge:X = \[a, b\]"
    "Append:Zs = \[a, b\]"
    "Reverse:Ys = \[b, a\]"
    "Copy:Ys = \[x, y\]"
    "Quicksort:X = \[1, 2\]"
    "Insertion sort:X = \[1, 2, 3\]"
    "Bubble sort:Ys = \[1, 2\]"
    "AND gate:Out = \[one, zero\]"
    "Factorial:F = 6"
    "Primes:Ps = \\[2, 3, 5, 7\\]"
    "Fibonacci:F2 = 2"
    "Sum list:S = 6"
    "Hanoi:→ 6 goals"
    "Meta factorial:F3 = 6"
    "Meta sum:S2 = 3"
    "Univ compose:T = foo(a)"
    "Univ decompose:L = \[bar, x\]"
    "Unify atom:X = foo"
    "Unify struct:X = f(a)"
    "Assign number:X = 5"
    "Assign expr:X = 5"
    "Assign precedence:X = 14"
)

PASS=0
FAIL=0

for test in "${tests[@]}"; do
    name="${test%%:*}"
    pattern="${test#*:}"
    if echo "$output" | grep -q "$pattern"; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (expected: $pattern)"
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "======================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "======================================"
echo "(Full test suite: full_run_repl_tests.sh)"

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
