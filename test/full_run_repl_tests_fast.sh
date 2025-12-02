#!/bin/bash
# GLP REPL Full Test Suite - FAST VERSION (~3-5s)
# Loads all files in single REPL session for speed
# NOTE: arithmetic_fixed.glp excluded (conflicts with primes.glp mod guards)
# NOTE: SRSW violation tests run separately at end

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_RUNTIME="$SCRIPT_DIR/../glp_runtime"
GLP_DIR="$SCRIPT_DIR/../glp/test"
REPL="bin/glp_repl.dart"

cd "$GLP_RUNTIME"

echo "======================================"
echo "   GLP REPL Full Test Suite (FAST)   "
echo "======================================"
echo ""

# Load all files and run all queries in single session
output=$($DART run "$REPL" <<REPL_INPUT
$GLP_DIR/hello.glp
$GLP_DIR/p.glp
$GLP_DIR/merge.glp
$GLP_DIR/merge_standalone.glp
$GLP_DIR/run1.glp
$GLP_DIR/append.glp
$GLP_DIR/reverse.glp
$GLP_DIR/copy.glp
$GLP_DIR/opmerge.glp
$GLP_DIR/fsmerge.glp
$GLP_DIR/gates.glp
$GLP_DIR/qsort.glp
$GLP_DIR/isort.glp
$GLP_DIR/bsort.glp
$GLP_DIR/struct_demo.glp
$GLP_DIR/sum_direct.glp
$GLP_DIR/ip_direct.glp
$GLP_DIR/multiply_direct.glp
$GLP_DIR/fib_direct.glp
$GLP_DIR/hanoi.glp
$GLP_DIR/sum_list.glp
$GLP_DIR/inner_product.glp
$GLP_DIR/multiply.glp
$GLP_DIR/fib.glp
$GLP_DIR/factorial.glp
$GLP_DIR/test_time.glp
$GLP_DIR/primes.glp
hello.
p(X).
merge([1,2,3], [a,b], Xs).
merge([1,2], [a,b], Xs2).
clause(p(a), B).
run(true).
run(merge([a,b],[b],X)).
run_isort(insertion_sort([3,4,2,3,6,1,2],Xsort)).
append([a,b], [c,d], Zs).
append([], [x,y], Zs2).
append([a,b], [], Zs3).
reverse([a,b,c], Ys).
reverse([], Ys2).
reverse([x], Ys3).
copy([a,b,c], Yc).
copy([], Yc2).
opmerge([1,3,5], [2,4,6], Zop).
opmerge([1,2,3], [2,3,4], Zop2).
opmerge([], [1,2], Zop3).
fsmerge([a,b,c], [x,y,z], Zfs).
fsmerge([a,b], [x,y,z], Zfs2).
and([one,zero,one], [one,one,zero], OutA).
or([one,zero,one], [one,one,zero], OutO).
and([one,one], [one,one], OutA2).
or([zero,zero], [zero,zero], OutO2).
quicksort([],Xq1).
quicksort([1],Xq2).
quicksort([1,2],Xq3).
quicksort([1,6,4,2,7,4,2,6],Xq4).
quicksort([1,3,4,2,5],Xq5).
quicksort([a],Xq6).
quicksort([1|X?],Xq7).
insertion_sort([],Xi1).
insertion_sort([3],Xi2).
insertion_sort([3,4],Xi3).
sort([3,1,4,1,5], Ybs).
sort([], Ybs2).
sort([7], Ybs3).
build_person(P).
run(merge([a],[b],Xm1)).
run(merge([a],[b,c,d],Xm2)).
run2(Xr2).
run((merge([a],[b],Xc),merge(Xc?,[c],Yc))).
run_qsort(quicksort([],Xrq1)).
run_qsort(quicksort([1],Xrq2)).
run1_qsort(run_qsort(quicksort([1],Xrq3))).
run1_qsort(run_qsort(quicksort([1,4,3,2,4,5],Xrq4))).
sumd([1,2,3,4,5], Sd1).
sumd([], Sd2).
ipd([1,2,3], [4,5,6], Sip1).
ipd([2], [3], Sip2).
multiplyd(3, [1,2,3,4], Ym1).
multiplyd(5, [], Ym2).
fibd(0, Fd0).
fibd(1, Fd1).
fibd(2, Fd2).
hanoi(0, a, c, Mh0).
hanoi(1, a, c, Mh1).
hanoi(2, a, c, Mh2).
sum([1,2,3,4,5], Ssl1).
ip([1,2,3], [4,5,6], Sipf).
multiply(3, [1,2,3,4], Ymf).
fib(0, Ff0).
fib(1, Ff1).
factorial(1, Fac1).
factorial(2, Fac2).
fib(3, Ff3).
fib(10, Ff10).
factorial(3, Fac3).
factorial(5, Fac5).
run_fact(factorial(5, Facm)).
run_mult(multiply(3, [1,2,3,4], Ymm)).
run_sum(sum([1,2,3,4,5], Ssm)).
run_ip(ip([1,2,3], [4,5,6], Sipm)).
run_hanoi(hanoi(2, a, c, Mhm)).
T1 =.. [foo].
T2 =.. [bar, x, y].
foo(a, b) =.. L1.
bar(1, 2, 3) =.. L2.
Xu1 = foo.
Xu2 = 42.
Xu3 = foo(a, b).
Xu4 = [1, 2, 3].
Xu5 = foo(bar(a)).
Xu6 = Y?.
Xa1 := 3.
Xa2 := 5 + 3.
Xa3 := 10 - 4.
Xa4 := 4 * 7.
Xa5 := 20 / 4.
Xa6 := 5 + 3 * 2.
Xa7 := (5 + 3) * 2.
Xa8 := -5.
get_time(Ttime).
past_time(1000000000000, Xpast).
past_time(9999999999999, Xfuture).
wait_test(Xwait).
primes(20, Ps20).
primes(10, Ps10).
:quit
REPL_INPUT
2>&1)

# Test definitions: "Name:pattern"
declare -a tests=(
    # Basic tests
    "Hello World:Hello from GLP!"
    "Simple Unification:X = a"

    # Merge tests
    "Merge [1,2,3] and [a,b]:Xs = \[1, a, 2, b, 3\]"
    "Merge standalone:Xs2 = \[1, a, 2, b\]"

    # Metainterpreter tests
    "Clause Lookup:B = true"
    "Simple Run:run(true)"
    "Merge via Metainterpreter:X = \[a, b, b\]"
    "Insertion Sort via Meta:Xsort = \[1, 2, 2, 3, 3, 4, 6\]"

    # List operations
    "Append two lists:Zs = \[a, b, c, d\]"
    "Append empty list:Zs2 = \[x, y\]"
    "Append to empty list:Zs3 = \[a, b\]"
    "Reverse list:Ys = \[c, b, a\]"
    "Reverse empty list:Ys2 = \[\]"
    "Reverse single element:Ys3 = \[x\]"
    "Copy list:Yc = \[a, b, c\]"
    "Copy empty list:Yc2 = \[\]"
    "Ordered merge:Zop = \[1, 2, 3, 4, 5, 6\]"
    "Ordered merge duplicates:Zop2 = \[1, 2, 3, 4\]"
    "Ordered merge empty:Zop3 = \[1, 2\]"
    "Fair stable merge:Zfs = \[a, x, b, y, c, z\]"
    "Fair stable merge unequal:Zfs2 = \[a, x, b, y, z\]"

    # Logic gates
    "AND gate:OutA = \[one, zero, zero\]"
    "OR gate:OutO = \[one, one, one\]"
    "AND gate all ones:OutA2 = \[one, one\]"
    "OR gate all zeros:OutO2 = \[zero, zero\]"

    # Quicksort
    "Quicksort empty:Xq1 = \[\]"
    "Quicksort single:Xq2 = \[1\]"
    "Quicksort two:Xq3 = \[1, 2\]"
    "Quicksort larger:Xq4 = \[1, 2, 2, 4, 4, 6, 6, 7\]"
    "Quicksort five:Xq5 = \[1, 2, 3, 4, 5\]"
    "Quicksort non-number:Xq6 = <unbound>"
    "Quicksort unbound tail:Xq7 = <unbound>"

    # Insertion sort
    "Insertion sort empty:Xi1 = \[\]"
    "Insertion sort single:Xi2 = \[3\]"
    "Insertion sort two:Xi3 = \[3, 4\]"

    # Bubble sort
    "Bubble sort:Ybs = \[1, 1, 3, 4, 5\]"
    "Bubble sort empty:Ybs2 = \[\]"
    "Bubble sort single:Ybs3 = \[7\]"

    # Structure demo
    "Structure Demo:P = person"

    # Meta merge tests
    "Meta merge [a],[b]:Xm1 = \[a, b\]"
    "Meta merge [a],[b,c,d]:Xm2 = \[a, b, c, d\]"
    "Meta run2:→ 4 goals"
    "Meta merge chain:Yc = \[a, c, b\]"

    # Meta quicksort
    "Meta quicksort empty:Xrq1 = \[\]"
    "Meta quicksort single:Xrq2 = \[1\]"
    "Nested meta quicksort 1:Xrq3 = \[1\]"
    "Nested meta quicksort 6:Xrq4 = \[1, 2, 3, 4, 4, 5\]"

    # Arithmetic direct
    "Sum list direct:Sd1 = 15"
    "Sum empty direct:Sd2 = 0"
    "Inner product direct:Sip1 = 32"
    "Inner product single:Sip2 = 6"
    "Multiply stream:Ym1 = \[3, 6, 9, 12\]"
    "Multiply stream empty:Ym2 = \[\]"
    "Fibonacci 0 direct:Fd0 = 0"
    "Fibonacci 1 direct:Fd1 = 1"
    "Fibonacci 2 direct:Fd2 = 1"

    # Hanoi
    "Hanoi 0:→ 1 goals"
    "Hanoi 1:→ 6 goals"
    "Hanoi 2:→ 16 goals"

    # Full arithmetic files
    "Sum list full:Ssl1 = 15"
    "Inner product full:Sipf = 32"
    "Multiply stream full:Ymf = \[3, 6, 9, 12\]"
    "Fibonacci 0 full:Ff0 = 0"
    "Fibonacci 1 full:Ff1 = 1"
    "Factorial 1:Fac1 = 1"
    "Factorial 2:Fac2 = 2"

    # Non-tail-recursive
    "Fibonacci 3:Ff3 = 2"
    "Fibonacci 10:Ff10 = 55"
    "Factorial 3:Fac3 = 6"
    "Factorial 5:Fac5 = 120"

    # Meta + arithmetic
    "Meta factorial 5:Facm = 120"
    "Meta multiply:Ymm = \[3, 6, 9, 12\]"
    "Meta sum:Ssm = 15"
    "Meta inner product:Sipm = 32"
    "Meta hanoi 2:→ 54 goals"

    # Univ
    "Univ compose [foo]:T1 = foo()"
    "Univ compose [bar,x,y]:T2 = bar(x, y)"
    "Univ decompose foo(a,b):L1 = \[foo, a, b\]"
    "Univ decompose bar(1,2,3):L2 = \[bar, 1, 2, 3\]"

    # Unification
    "Unify atom:Xu1 = foo"
    "Unify number:Xu2 = 42"
    "Unify structure:Xu3 = foo(a, b)"
    "Unify list:Xu4 = \[1, 2, 3\]"
    "Unify nested:Xu5 = foo(bar(a))"
    "Unify suspend:suspended"

    # Assignment
    "Assign plain:Xa1 = 3"
    "Assign add:Xa2 = 8"
    "Assign sub:Xa3 = 6"
    "Assign mul:Xa4 = 28"
    "Assign div:Xa5 = 5"
    "Assign precedence:Xa6 = 11"
    "Assign parens:Xa7 = 16"
    "Assign negative:Xa8 = -5"

    # Time predicates
    "Time now:Ttime = 1"
    "Time past:Xpast = yes"
    "Time future:failed"
    "Time wait:Xwait = done"

    # Primes
    "Primes 20:Ps20 = \[2, 3, 5, 7, 11, 13, 17, 19\]"
    "Primes 10:Ps10 = \[2, 3, 5, 7\]"
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

# Run SRSW violation test separately
echo ""
echo "--- SRSW Violation Tests ---"

srsw_output=$($DART run "$REPL" <<SRSW_INPUT
$GLP_DIR/merge_with_reader.glp
:quit
SRSW_INPUT
2>&1)

if echo "$srsw_output" | grep -q "SRSW violation"; then
    echo "PASS: SRSW Check - merge_with_reader.glp rejected"
    PASS=$((PASS + 1))
else
    echo "FAIL: SRSW Check - merge_with_reader.glp should be rejected"
    FAIL=$((FAIL + 1))
fi

# Run arithmetic_fixed.glp tests separately (conflicts with primes)
echo ""
echo "--- Arithmetic Fixed Tests (separate session) ---"

arith_output=$($DART run "$REPL" <<ARITH_INPUT
$GLP_DIR/arithmetic_fixed.glp
add(5, 3, Xadd).
multiply(4, 7, Ymul).
compute(Zcomp).
:quit
ARITH_INPUT
2>&1)

if echo "$arith_output" | grep -q "Xadd = 8"; then
    echo "PASS: Addition 5+3"
    PASS=$((PASS + 1))
else
    echo "FAIL: Addition 5+3 (expected: Xadd = 8)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_output" | grep -q "Ymul = 28"; then
    echo "PASS: Multiplication 4*7"
    PASS=$((PASS + 1))
else
    echo "FAIL: Multiplication 4*7 (expected: Ymul = 28)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_output" | grep -q "Zcomp = 10"; then
    echo "PASS: Compound (2*3)+4"
    PASS=$((PASS + 1))
else
    echo "FAIL: Compound (2*3)+4 (expected: Zcomp = 10)"
    FAIL=$((FAIL + 1))
fi

TOTAL=$((PASS + FAIL))

echo ""
echo "======================================"
echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
echo "======================================"

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
