#!/bin/bash
# GLP Unified Test Suite v1.0
# Replaces: full_run_repl_tests.sh + run_typechecker_repl_tests.sh
# All runtime test programs are well-typed.
#
# Sections:
#   A - Typed Runtime Tests (load + run queries + check output)
#   B - Type-Check-Only Positive Tests (load succeeds)
#   C - Negative Type Tests (load must be rejected)
#   D - SRSW Violation Tests (load must be rejected)
#   E - Invalid Guard Test (true in guard rejected)
#   F - CSSG Modules (modular play tests via project-directory loading)
#   G - Social Graph Simulated UI Modules (project-directory loading)
#   H - CSSN Modules (project-directory loading, plays 1-12)
#   I - self.glp Procedure Tests (shared procs, shadowing, local shadow, type error)
#   J - CSSG v2 Modules (child_agent with parent(X) output keys)
#   K - CSSN v2 Modules (child_agent with blocking consent)
#   L - Dynamic Module Dispatch Tests (activate + M # goal)
#   M - Multi-Isolate (madGLP) Tests (dart test, CSSN v2, one isolate per agent)
#   N - Currencies Modules (project-directory loading, plays 1-12)
#   O - Currencies Multi-Isolate Tests (dart test, one isolate per agent)
#   P - Module Boundary Enforcement Tests (exported vs private procedures)

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/.."
GLP_RUNTIME="$GLP_DIR/glp_runtime"
TYPED="$GLP_DIR/programs/tests/typed"

# The tree this run measures.  A concurrent session that commits mid-run leaves
# the suite testing a mixture: sources are re-read as each section runs, and the
# REPL binary is rebuilt from whatever is on disc at that moment.  The failures
# that produces are true statements about a tree that no longer exists, and they
# look like ordinary failures in unrelated sections — four Section A logic-gate
# failures on 2026-08-01 were of exactly this kind, and a baseline was lost the
# same way that morning.  Recorded here, re-checked at the end.
START_HEAD=$(cd "$GLP_DIR" && git rev-parse HEAD 2>/dev/null || echo "no-git")
START_DIRTY=$(cd "$GLP_DIR" && git status --porcelain 2>/dev/null | sort | shasum | cut -d' ' -f1)
BOOK="$GLP_DIR/programs/book"
TC_DIR="$GLP_RUNTIME/test/programs/typechecker"
MODED="$GLP_RUNTIME/test/programs/moded_types"

cd "$GLP_RUNTIME"

# Compile the REPL to a native AOT binary for fast startup (Issue 11/12).
# Each check then starts in ~0.01s instead of ~0.8s for `dart run`. The binary
# is rebuilt whenever any lib/ or bin/ Dart source is newer than it; self.glp is
# read at runtime, so .glp edits need no rebuild. A failed compile aborts (set -e)
# rather than silently running stale code. The slow `dart run` REPL path is retired.
REPL_EXE="bin/glp_repl_exe"
if [ ! -f "$REPL_EXE" ] || [ -n "$(find lib bin -name '*.dart' -newer "$REPL_EXE" 2>/dev/null | head -1)" ]; then
    echo "Compiling REPL AOT binary..."
    $DART compile exe -o "$REPL_EXE" bin/glp_repl.dart >/dev/null
fi
REPL_RUN="./$REPL_EXE"

echo "======================================"
echo "   GLP Unified Test Suite v1.0        "
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
# SECTION A: TYPED RUNTIME TESTS (load + type-check + run queries)
# =============================================================================
echo "=== Section A: Typed Runtime Tests ==="
echo ""

# --- A1: p, merge_simple, merge_standalone, metainterpreter ---
echo "--- A1: p, merge, metainterpreter ---"
a1=$("$REPL_RUN" <<HEREDOC
$TYPED/p.glp
$BOOK/streams/producers_consumers/merge_simple.glp
$TYPED/merge_standalone.glp
$TYPED/run1.glp
p(X).
merge([1,2,3], [a,b], Xs).
merge2([c,d], Out).
clause(p(a), B).
run(true).
run(merge([a,b],[b],X)).
runA(X2).
run2(Xr2).
:quit
HEREDOC
2>&1)

check "p(X) ill-typed: writer at input arg of p(Constant?)" "(p) is not well-typed" "$a1"
check "Merge [1,2,3]+[a,b]" "Xs = \[1, a, 2, b, 3\]" "$a1"
check "Clause lookup" "B = true" "$a1"
check "run(true)" "succeeds" "$a1"
check "Meta merge" "X = \[a, b, b\]" "$a1"
check "runA empty merge" "X2 = \[\]" "$a1"

# --- A2: Append, Reverse, Copy ---
echo "--- A2: Append, Reverse, Copy ---"
a2=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/append.glp
$BOOK/recursive/list_processing/reverse.glp
$BOOK/recursive/list_processing/copy.glp
append([a,b], [c,d], Zs).
append([], [x,y], Zs2).
append([a,b], [], Zs3).
reverse([a,b,c], Ys).
reverse([], Ys2).
reverse([x], Ys3).
copy([a,b,c], Yc).
copy([], Yc2).
:quit
HEREDOC
2>&1)

check "Append two lists" "Zs = \[a, b, c, d\]" "$a2"
check "Append empty+list" "Zs2 = \[x, y\]" "$a2"
check "Append list+empty" "Zs3 = \[a, b\]" "$a2"
check "Reverse list" "Ys = \[c, b, a\]" "$a2"
check "Reverse empty" "Ys2 = \[\]" "$a2"
check "Reverse single" "Ys3 = \[x\]" "$a2"
check "Copy list" "Yc = \[a, b, c\]" "$a2"
check "Copy empty" "Yc2 = \[\]" "$a2"

# --- A3: Quicksort ---
echo "--- A3: Quicksort ---"
a3=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/quicksort.glp
quicksort([],Xq1).
quicksort([1],Xq2).
quicksort([1,2],Xq3).
quicksort([1,6,4,2,7,4,2,6],Xq4).
quicksort([1,3,4,2,5],Xq5).
quicksort([a],Xq6).
quicksort([1|X?],Xq7).
:quit
HEREDOC
2>&1)

check "Quicksort empty" "Xq1 = \[\]" "$a3"
check "Quicksort single" "Xq2 = \[1\]" "$a3"
check "Quicksort two" "Xq3 = \[1, 2\]" "$a3"
check "Quicksort larger" "Xq4 = \[1, 2, 2, 4, 4, 6, 6, 7\]" "$a3"
check "Quicksort five" "Xq5 = \[1, 2, 3, 4, 5\]" "$a3"
check "Quicksort non-number ill-typed: [a] is not a NumList" "No alternative of Number? matches the constant a" "$a3"
check "Quicksort unbound tail" "Xq7 = <unbound>" "$a3"

# --- A4: Insertion Sort ---
echo "--- A4: Insertion Sort ---"
a4=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/insertion_sort.glp
insertion_sort([],Xi1).
insertion_sort([3],Xi2).
insertion_sort([3,4],Xi3).
insertion_sort([3,4,2,3,6,1,2],Xi4).
:quit
HEREDOC
2>&1)

check "Insertion sort empty" "Xi1 = \[\]" "$a4"
check "Insertion sort single" "Xi2 = \[3\]" "$a4"
check "Insertion sort two" "Xi3 = \[3, 4\]" "$a4"
check "Insertion sort larger" "Xi4 = \[1, 2, 2, 3, 3, 4, 6\]" "$a4"

# --- A5: Bubble Sort --- (REMOVED: bubble_sort.glp fails type checking at load time)

# --- A6: Ordered merge ---
echo "--- A6: Ordered merge ---"
a6=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/merge_ordered.glp
merge([1,3,5], [2,4,6], Zop).
merge([1,2,3], [2,3,4], Zop2).
merge([], [1,2], Zop3).
:quit
HEREDOC
2>&1)

check "Ordered merge" "Zop = \[1, 2, 3, 4, 5, 6\]" "$a6"
check "Ordered merge duplicates" "Zop2 = \[1, 2, 2, 3, 3, 4\]" "$a6"
check "Ordered merge empty" "Zop3 = \[1, 2\]" "$a6"

# --- A7: Fair merge ---
echo "--- A7: Fair merge ---"
a7=$("$REPL_RUN" <<HEREDOC
$BOOK/streams/producers_consumers/fair_merge.glp
merge([a,b,c], [x,y,z], Zfs).
merge([a,b], [x,y,z], Zfs2).
:quit
HEREDOC
2>&1)

check "Fair merge equal" "Zfs = \[a, x, b, y, c, z\]" "$a7"
check "Fair merge unequal" "Zfs2 = \[a, x, b, y, z\]" "$a7"

# --- A8: Gates ---
echo "--- A8: Logic gates ---"
a8=$("$REPL_RUN" <<HEREDOC
$BOOK/constants/gates.glp
and([one,zero,one], [one,one,zero], OutA).
or([one,zero,one], [one,one,zero], OutO).
and([one,one], [one,one], OutA2).
or([zero,zero], [zero,zero], OutO2).
:quit
HEREDOC
2>&1)

check "AND gate" "OutA = \[one, zero, zero\]" "$a8"
check "OR gate" "OutO = \[one, one, one\]" "$a8"
check "AND all ones" "OutA2 = \[one, one\]" "$a8"
check "OR all zeros" "OutO2 = \[zero, zero\]" "$a8"

# --- A9: Arithmetic (sum, fib, factorial, hanoi, primes, inner_product) ---
echo "--- A9: Arithmetic programs ---"
a9=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/inner_product.glp
$BOOK/recursive/arithmetic_trees/fibonacci.glp
$BOOK/recursive/arithmetic_trees/factorial.glp
$BOOK/recursive/arithmetic_trees/hanoi.glp
$BOOK/recursive/arithmetic_trees/primes.glp
inner_product([1,2,3], [4,5,6], Sipf).
fib(0, Ff0).
fib(1, Ff1).
fib(3, Ff3).
fib(10, Ff10).
factorial(1, Fac1).
factorial(2, Fac2).
factorial(3, Fac3).
factorial(5, Fac5).
hanoi(0, a, c, Mh0).
hanoi(1, a, c, Mh1).
hanoi(2, a, c, Mh2).
primes(20, Ps20).
primes(10, Ps10).
:quit
HEREDOC
2>&1)

check "Inner product" "Sipf = 32" "$a9"
check "Fibonacci 0" "Ff0 = 0" "$a9"
check "Fibonacci 1" "Ff1 = 1" "$a9"
check "Fibonacci 3" "Ff3 = 2" "$a9"
check "Fibonacci 10" "Ff10 = 55" "$a9"
check "Factorial 1" "Fac1 = 1" "$a9"
check "Factorial 2" "Fac2 = 2" "$a9"
check "Factorial 3" "Fac3 = 6" "$a9"
check "Factorial 5" "Fac5 = 120" "$a9"
check "Hanoi 0" "succeeds" "$a9"
check "Hanoi 1" "succeeds" "$a9"
check "Hanoi 2" "succeeds" "$a9"
check "Primes 20" "Ps20 = \[2, 3, 5, 7, 11, 13, 17, 19\]" "$a9"
check "Primes 10" "Ps10 = \[2, 3, 5, 7\]" "$a9"

# --- A10: Multiply ---
echo "--- A10: Multiply ---"
a10=$("$REPL_RUN" <<HEREDOC
$TYPED/multiply.glp
multiply(3, [1,2,3,4], Ym1).
multiply(5, [], Ym2).
:quit
HEREDOC
2>&1)

check "Multiply stream" "Ym1 = \[3, 6, 9, 12\]" "$a10"
check "Multiply empty" "Ym2 = \[\]" "$a10"

# --- A11: Struct demo, depth, paa, guards, misc ---
echo "--- A11: Structure and pattern tests ---"
a11=$("$REPL_RUN" <<HEREDOC
$TYPED/struct_demo.glp
$TYPED/depth_test.glp
$TYPED/paa.glp
$TYPED/no_guard.glp
$TYPED/with_guard.glp
$TYPED/two_struct_list.glp
$TYPED/nonground_list.glp
$TYPED/reader_output.glp
$TYPED/assign_reader_test.glp
build_person(P).
bin_nest(val, Xbn).
ter_all(a, b, c, Xta).
tree3(val, Xtr3).
multi_w(p, q, Xmw).
p(Xpaa1, Xpaa1?).
no_guard([5,x,y], Xng).
with_guard([5,x,y], Xwg).
test([foo(a), bar(b)]).
test_list_in_body([1,2,3,4], Xngl).
build_list(a, b, Xbld).
unwrap([hello,world], Xunw).
identity(foo, Xid).
assign_reader(hello, Xar).
:quit
HEREDOC
2>&1)

check "Build person" "P = person" "$a11"
check "Nested binary" "Xbn = outer(inner(val, b), c)" "$a11"
check "Ternary all vars" "Xta = triple(a, b, c)" "$a11"
check "Deep binary tree" "Xtr3 = node(node(leaf(val), leaf(a)), leaf(b))" "$a11"
check "Multiple writers" "Xmw = pair(wrap(p), wrap(q))" "$a11"
check "p(X,X?) succeeds" "Xpaa1 = a" "$a11"
check "No guard" "Xng = \[5, a, b" "$a11"
check "With guard" "Xwg = \[5, a, b" "$a11"
check "Two struct list" "succeeds" "$a11"
check "Non-ground list pass" "Xngl = \[1, 2, 3, 4\]" "$a11"
check "Build list" "Xbld = \[a, b\]" "$a11"
check "Unwrap" "Xunw = hello" "$a11"
check "Identity" "Xid = foo" "$a11"
check "Assign reader" "Xar = hello" "$a11"

# --- A12: Arithmetic guards, comparisons, otherwise, guard_reader ---
echo "--- A12: Arithmetic guards and otherwise ---"
a12=$("$REPL_RUN" <<HEREDOC
$TYPED/arith_guard_ground.glp
$TYPED/arith_comparison.glp
$TYPED/otherwise_guard.glp
$TYPED/guard_reader.glp
compare_and_use(3, 5, Rag1).
max(7, 4, M1).
in_range(5, 1, 10, Rir1).
in_range(15, 1, 10, Rir2).
compare_expr(1, 5, Rce1).
arith_eq(5, 5, Raeq1).
arith_eq(5, 3, Raeq2).
arith_neq(5, 3, Raneq1).
arith_neq(5, 5, Raneq2).
expr_eq(4, 6, Reeq1).
test_lt(3, 5, Rlt1).
test_gt(5, 3, Rgt1).
test_le(5, 5, Rle1).
test_ge(3, 5, Rge1).
classify(5, Rcl1).
classify(-3, Rcl2).
classify(0, Rcl3).
grade(95, G1).
grade(75, G2).
grade(55, G3).
type_of(42, T1).
type_of(hello, T2).
guard_ground(42).
guard_int(7).
guard_compare(3, 5).
guard_known_valid(hello, Ygr).
:quit
HEREDOC
2>&1)

check "compare_and_use" "Rag1 = pair(3, 5)" "$a12"
check "max" "M1 = 7" "$a12"
check "in_range yes" "Rir1 = yes" "$a12"
check "in_range no" "Rir2 = no" "$a12"
check "compare_expr" "Rce1 = pair(1, 5)" "$a12"
check "arith_eq equal" "Raeq1 = equal" "$a12"
check "arith_eq not equal" "Raeq2 = not_equal" "$a12"
check "arith_neq" "Raneq1 = not_equal" "$a12"
check "arith_neq equal" "Raneq2 = equal" "$a12"
check "expr_eq" "Reeq1 = equal" "$a12"
check "test_lt" "Rlt1 = yes" "$a12"
check "test_gt" "Rgt1 = yes" "$a12"
check "test_le" "Rle1 = yes" "$a12"
check "test_ge fails" "Rge1 = no" "$a12"
check "classify positive" "Rcl1 = positive" "$a12"
check "classify negative" "Rcl2 = negative" "$a12"
check "classify zero" "Rcl3 = zero" "$a12"
check "grade a" "G1 = a" "$a12"
check "grade c" "G2 = c" "$a12"
check "grade f" "G3 = f" "$a12"
check "type integer" "T1 = integer" "$a12"
check "type string" "T2 = string" "$a12"
check "guard_ground" "succeeds" "$a12"
check "guard_int" "succeeds" "$a12"
check "guard_compare" "succeeds" "$a12"
check "guard_known_valid" "Ygr = hello" "$a12"

# --- A13: Ground equal, guard negation ---
echo "--- A13: Ground equal and guard negation ---"
a13=$("$REPL_RUN" <<HEREDOC
$TYPED/test_ground_equal.glp
$TYPED/test_guard_negation.glp
test(a, a, R1).
test(a, b, R2).
test(foo(1,2), foo(1,2), R3).
test(foo(1,2), foo(1,3), R4).
test([1,2,3], [1,2,3], R5).
test([1,2], [1,3], R6).
test_neg_int(5, Rn1).
test_neg_int(hello, Rn2).
test_neg_number(3.14, Rn3).
test_neg_number(hello, Rn4).
test_neg_eq(5, 5, Rn5).
test_neg_eq(5, 3, Rn6).
:quit
HEREDOC
2>&1)

check "equal atoms" "R1 = equal" "$a13"
check "not equal atoms" "R2 = not_equal" "$a13"
check "equal structs" "R3 = equal" "$a13"
check "not equal structs" "R4 = not_equal" "$a13"
check "equal lists" "R5 = equal" "$a13"
check "not equal lists" "R6 = not_equal" "$a13"
check "neg int is_int" "Rn1 = is_int" "$a13"
check "neg int not_int" "Rn2 = not_int" "$a13"
check "neg number is_num" "Rn3 = is_num" "$a13"
check "neg number not_num" "Rn4 = not_num" "$a13"
check "neg eq equal" "Rn5 = eq" "$a13"
check "neg eq not equal" "Rn6 = neq" "$a13"

# --- A14: Circular terms ---
echo "--- A14: Circular term tests ---"
a14=$("$REPL_RUN" <<HEREDOC
$TYPED/circular_test.glp
is_ground(foo, Rc1).
is_ground(f(a,b), Rc2).
test_equal(foo, foo, Rc3).
test_equal(foo, bar, Rc4).
test_self_equal(f(a,b), Rc5).
show(hello, Xshow).
:quit
HEREDOC
2>&1)

check "ground foo" "Rc1 = yes" "$a14"
check "ground f(a,b)" "Rc2 = yes" "$a14"
check "equal foo foo" "Rc3 = yes" "$a14"
check "equal foo bar" "Rc4 = no" "$a14"
check "self equal" "Rc5 = yes" "$a14"
check "show" "Xshow = hello" "$a14"

# --- A15: Arithmetic fixed (uses :=) ---
echo "--- A15: Arithmetic with := ---"
a15=$("$REPL_RUN" <<HEREDOC
$TYPED/arithmetic_fixed.glp
add(5, 3, Xadd).
multiply(4, 7, Ymul).
compute(Zcomp).
subtract(10, 3, Xsub).
:quit
HEREDOC
2>&1)

check "add 5+3" "Xadd = 8" "$a15"
check "multiply 4*7" "Ymul = 28" "$a15"
check "compute (2*3)+4" "Zcomp = 10" "$a15"
check "subtract 10-3" "Xsub = 7" "$a15"

# --- A16: Arithmetic kernels ---
echo "--- A16: Arithmetic kernels ---"
a16=$("$REPL_RUN" <<HEREDOC
$TYPED/test_arithmetic_kernels.glp
test_idiv(10, 3, Rak1).
test_abs(-5, Rak2).
test_sqrt(16, Rak3).
test_pow(2, 10, Rak4).
test_floor(3.7, Rak5).
test_ceil(3.2, Rak6).
:quit
HEREDOC
2>&1)

check "idiv" "Rak1 = 3" "$a16"
check "abs" "Rak2 = 5" "$a16"
check "sqrt" "Rak3 = 4" "$a16"
check "pow" "Rak4 = 1024" "$a16"
check "floor" "Rak5 = 3" "$a16"
check "ceil" "Rak6 = 4" "$a16"

# --- A17: Guards comprehensive ---
echo "--- A17: Guards comprehensive ---"
a17=$("$REPL_RUN" <<HEREDOC
$TYPED/test_guards_comprehensive.glp
test_list_ok([1,2,3], Rgc1).
test_string_ok("hello", Rgc2).
test_constant_ok(foo, Rgc3).
:quit
HEREDOC
2>&1)

check "list guard" "Rgc1 = ok" "$a17"
check "string guard" "Rgc2 = ok" "$a17"
check "constant guard" "Rgc3 = ok" "$a17"

# --- A18: Constant ground, gethead ---
echo "--- A18: Constant ground, gethead ---"
a18=$("$REPL_RUN" <<HEREDOC
$TYPED/constant_ground_test.glp
$TYPED/gethead_test.glp
test_constant(foo, Rcgt1).
test_gethead(Rgh1).
:quit
HEREDOC
2>&1)

check "constant ground" "Rcgt1 = foo" "$a18"
check "gethead" "Rgh1 = a" "$a18"

# --- A18b: Parameterized proc decl with bare type var ---
echo "--- A18b: Param bare typevar ---"
a18b=$("$REPL_RUN" <<HEREDOC
$TYPED/param_bare_typevar.glp
test_gethead_param(Rpbt1).
:quit
HEREDOC
2>&1)

check "param bare typevar" "Rpbt1 = a" "$a18b"

# --- A19: Defined guards ---
echo "--- A19: Defined guards ---"
a19=$("$REPL_RUN" <<HEREDOC
$TYPED/test_defined_guards.glp
test(ch(Adg?, Bdg), Rdg1).
test(foo, Rdg2).
test(Xdg?, Rdg3).
:quit
HEREDOC
2>&1)

check "defined guard match" "Rdg1 = ok" "$a19"
check "defined guard fail" "Rdg2 = not_channel" "$a19"
check "defined guard suspend" "suspended" "$a19"

# --- A19b: Bounded-buffer back-pressure — guard must suspend, not fail (Issue 12) ---
# Regression for the guard-deref clause-index/heap-addr collision (runner.dart
# _dereferenceWithTracking): integer(X1?) on an unbound reader must suspend and
# let the producer fill, regardless of goal order. Before the fix, consumer-first
# failed instead of suspending; producer-first worked — order-sensitive.
echo "--- A19b: Bounded-buffer back-pressure (Issue 12) ---"
a19bb_cons=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/book/streams/buffered_communication/hollow_integers.glp
:limit 50
consumer([X1?, X2?, X3? | Xs]), producer(1, [X1, X2, X3 | Xs?]).
:quit
HEREDOC
2>&1)
a19bb_prod=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/book/streams/buffered_communication/hollow_integers.glp
:limit 50
producer(1, [X1, X2, X3 | Xs?]), consumer([X1?, X2?, X3? | Xs]).
:quit
HEREDOC
2>&1)

check "bounded buffer consumer-first suspends (not fails)" "suspended" "$a19bb_cons"
check "bounded buffer producer-first suspends" "suspended" "$a19bb_prod"

# --- A20: Channel guards ---
# new_channel/send/receive are prelude defined guards, unfolded by the PE.
echo "--- A20: Channel guards ---"
a20=$("$REPL_RUN" <<HEREDOC
$TYPED/test_channel_guards.glp
make_pair(MpC1, MpC2).
:quit
HEREDOC
2>&1)

check "channel make_pair succeeds" "succeeds" "$a20"

# --- A20b: no_readers guard (succeeds on writers, suspends on readers) ---
# Replaces the loose programs/tests/test_no_readers.glp, which posted its cases
# with the `?-` directive form the parser has no handling for, so nothing ran
# them; its three cases were all ground terms.  The guard never fails: it
# succeeds when the term carries no reader — writers included, which is what
# separates it from ground/1 — and otherwise suspends.
echo "--- A20b: no_readers guard ---"
a20b=$("$REPL_RUN" <<HEREDOC
$TYPED/test_no_readers.glp
test_no_readers(42, Rnr1).
test_no_readers(foo(1, bar(2)), Rnr2).
test_no_readers(f(Wnr), Rnr3).
test_no_readers([1,2|Tnr], Rnr4).
test_no_readers(g(Ynr?), Rnr5).
test_neg_no_readers(h(Znr?), Rnr6).
test_neg_no_readers(7, Rnr7).
:quit
HEREDOC
2>&1)

check "no_readers ground constant" "Rnr1 = ok" "$a20b"
check "no_readers ground compound" "Rnr2 = ok" "$a20b"
check "no_readers writer inside term" "Rnr3 = ok" "$a20b"
check "no_readers writer tail" "Rnr4 = ok" "$a20b"
check "no_readers reader suspends" "suspended" "$a20b"
check "~no_readers finds reader" "Rnr6 = has_readers" "$a20b"
check "~no_readers on ground falls through" "Rnr7 = none" "$a20b"

# --- A21: Comprehensive defined guards ---
echo "--- A21: Comprehensive defined guards ---"
a21=$("$REPL_RUN" <<HEREDOC
$TYPED/test_defined_guards_all.glp
make_pair(Call1, Call2).
bind_response(yes, RespYes, LocalYes).
bind_response(no, RespNo, LocalNo).
test_channel(ch(TchA?, TchB), TchR1).
test_channel(foo, TchR2).
test_channel(p(TpaA, TpaB), TchR3).
test_pair(p(TprA, TprB), TprR1).
test_pair(foo, TprR2).
test_wrapper(w(TwrX), TwrR1).
test_wrapper(foo, TwrR2).
test_nested(w(p(TnA, TnB)), TnR1).
test_nested(w(hello), TnR2).
test_nested(foo, TnR3).
test_wrap(hello, TwpR).
test_deep(foo, TdpR).
test_triple(1, 2, TtrR).
:quit
HEREDOC
2>&1)

check "DG make_pair succeeds" "succeeds" "$a21"
check "DG bind yes" 'RespYes = accept(ch(' "$a21"
check "DG bind yes local" 'LocalYes = ch(' "$a21"
check "DG bind no" "RespNo = no" "$a21"
check "DG bind no local" "LocalNo = none" "$a21"
check "DG channel ok" "TchR1 = ok" "$a21"
check "DG channel fail atom" "TchR2 = not_channel" "$a21"
check "DG channel fail pair" "TchR3 = not_channel" "$a21"
check "DG pair ok" "TprR1 = ok" "$a21"
check "DG pair fail" "TprR2 = not_pair" "$a21"
check "DG wrapper ok" "TwrR1 = ok" "$a21"
check "DG wrapper fail" "TwrR2 = not_wrapper" "$a21"
check "DG nested pair" "TnR1 = wrapper_with_pair" "$a21"
check "DG nested wrapper" "TnR2 = just_wrapper" "$a21"
check "DG nested neither" "TnR3 = neither" "$a21"
check "DG wrap binding" "TwpR = wrapped(hello)" "$a21"
check "DG deep binding" "TdpR = outer(inner(foo))" "$a21"
check "DG triple" "TtrR = pair(1, 2)" "$a21"

# --- A22: Wait test ---
echo "--- A22: Wait test ---"
a22=$("$REPL_RUN" <<HEREDOC
$TYPED/test_time.glp
wait_test(Xwait).
:quit
HEREDOC
2>&1)

check "wait test" "Xwait = done" "$a22"

# --- A23: DiffList ---
# dl_append/dl_to_list are prelude defined guards, unfolded by the PE.
echo "--- A23: Difference lists ---"
a23=$("$REPL_RUN" <<HEREDOC
$TYPED/diff_list.glp
$TYPED/bb_diff.glp
Xdl = foo\bar.
test_dl_to_list([1,2,3]\\[], Ldtl).
:quit
HEREDOC
2>&1)

check "DL bind via =" 'Xdl = \\(foo, bar)' "$a23"
check "DL dl_to_list" 'Ldtl = \[1, 2, 3\]' "$a23"

# --- A24: Suspension tests ---
echo "--- A24: Suspension tests ---"
a24=$("$REPL_RUN" <<HEREDOC
$TYPED/test_bob.glp
$TYPED/test_nested_suspend.glp
$TYPED/test_guard_suspend.glp
bob(Xbob?).
level1(Xlv1?).
level2([Xlv2?|Rlv2]).
level3([wrapper(Xlv3?)|Rlv3]).
wait_ground(Xwg?).
:quit
HEREDOC
2>&1)

check "bob suspend" "suspended" "$a24"
check "level1 suspend" "suspended" "$a24"
check "level2 suspend" "suspended" "$a24"
check "level3 suspend" "suspended" "$a24"
check "guard ground suspend" "suspended" "$a24"

# --- A25: Quoted functor and body ---
# quoted_functor_test.glp declared and defined '_test_kernel' in a non-system
# module and was loaded here until 2026-07-31, when Rule B was corrected to test
# the underscore prefix and call position rather than a list of reserved names.
# Its subject is now illegal, so the file is a NEGATIVE fixture (Section C) and
# its load and goal have moved out of this section.  The rest of A25 stands:
# quoted_body_test.glp is an ordinary module, and '_equator' in `X = ...` is a
# term in data position, which the corrected rule leaves unrestricted.
echo "--- A25: Quoted functor and body ---"
a25=$("$REPL_RUN" <<HEREDOC
$TYPED/quoted_body_test.glp
double(5, Rqb1).
X = '_equator'(E, stop).
:quit
HEREDOC
2>&1)

check "double 5" "Rqb1 = 10" "$a25"
check "struct bind via =" "X = _equator(" "$a25"

# --- A26: Univ, assignment, MWM (stdlib, no file needed) ---
echo "--- A26: Univ, assignment, MWM ---"
a26=$("$REPL_RUN" <<HEREDOC
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
mwm([], Xmwm1).
mwm([stream([1,2,3])], Xmwm2).
mwm([stream([a,b]), stream([1,2])], Xmwm3).
:quit
HEREDOC
2>&1)

check "Univ compose foo" "T1 = foo()" "$a26"
check "Univ compose bar" "T2 = bar(x, y)" "$a26"
check "Univ decompose foo(a,b)" "L1 = \[foo, a, b\]" "$a26"
check "Univ decompose bar(1,2,3)" "L2 = \[bar, 1, 2, 3\]" "$a26"
check "Unify atom" "Xu1 = foo" "$a26"
check "Unify number" "Xu2 = 42" "$a26"
check "Unify struct" "Xu3 = foo(a, b)" "$a26"
check "Unify list" 'Xu4 = \[1, 2, 3\]' "$a26"
check "Unify nested" "Xu5 = foo(bar(a))" "$a26"
check "Unify unbound reader aliases" "Xu6 = <unbound>" "$a26"
check "Assign 3" "Xa1 = 3" "$a26"
check "Assign add" "Xa2 = 8" "$a26"
check "Assign sub" "Xa3 = 6" "$a26"
check "Assign mul" "Xa4 = 28" "$a26"
check "Assign div" "Xa5 = 5" "$a26"
check "Assign precedence" "Xa6 = 11" "$a26"
check "Assign parens" "Xa7 = 16" "$a26"
check "Assign negative" "Xa8 = -5" "$a26"
check "MWM empty rejected: mwm/2 undeclared to type checker" "Undefined procedure: mwm/2" "$a26"
check "MWM single rejected: mwm/2 undeclared to type checker" "Undefined procedure: mwm/2" "$a26"
check "MWM two streams rejected: mwm/2 undeclared to type checker" "Undefined procedure: mwm/2" "$a26"

# --- A26c: Well-typed-goal check (initial goal type-checked as a body goal) ---
# The REPL type-checks every initial goal as a body goal (def:well-typed-clause)
# before execution (TGLP modules.tex sec:runtime-boundary, glp-semantics.tex).
# A well-typed goal runs; an ill-typed goal is rejected with a specific error
# and never runs.
echo "--- A26c: Well-typed-goal check ---"
a26c=$("$REPL_RUN" <<HEREDOC
$TYPED/p.glp
p(a).
merge([1,2],[3,4],Xgc1).
p(Xgc2).
:quit
HEREDOC
2>&1)

check "Goal check: well-typed p(a) runs" "succeeds" "$a26c"
check "Goal check: well-typed merge runs" "Xgc1 = \[1, 3, 2, 4\]" "$a26c"
check "Goal check: ill-typed p(Xgc2) rejected (writer at input arg)" "(Xgc2, 0, input)" "$a26c"

# --- A26d: Issue 19 — unresolved type is a locatable diagnostic, not a crash ---
# A type referenced but not in scope (here Response, as when an isolate loads an
# incomplete self.glp scope) must surface as a located type error, not an
# unhandled UnknownTypeError that escapes and kills the caller. Checker side of
# known-issues Issue 19; pairs with IGLP's isolate-side catch.
echo "--- A26d: Unresolved-type diagnostic (Issue 19) ---"
a26d=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/type_errors/unresolved_type_response.glp
:quit
HEREDOC
2>&1)

check "Issue 19: unresolved type is a located diagnostic" "Unresolved type: Response at line 6" "$a26d"
check "Issue 19: offending source is rejected" "Type checking failed" "$a26d"

# --- A26e: Hierarchical single-module load (nested self.glp levels) ---
# A self-contained module loaded standalone links in EVERY ancestor self.glp
# level with nearer-shadows-farther precedence, includes its own-directory
# self.glp, and its own definitions shadow all of them (modules.tex §Design;
# manual §19.6). See programs/tests/nested_probe/ (root self.glp + app/self.glp
# + app/sub/leaf.glp + app/worker.glp).
echo "--- A26e: Hierarchical single-module load ---"
NP="$GLP_DIR/programs/tests/nested_probe"
np_leaf=$("$REPL_RUN" <<HEREDOC
$NP/app/sub/leaf.glp
run_where(W).
run_deep(D).
run_mid(M).
:quit
HEREDOC
2>&1)
check "nested: nearer self.glp shadows farther (where=mid)" "W = mid" "$np_leaf"
check "nested: farther self.glp proc reached (deep_only=outer)" "D = outer" "$np_leaf"
check "nested: nearer self.glp proc reached (mid_only=mid)" "M = mid" "$np_leaf"

np_worker=$("$REPL_RUN" <<HEREDOC
$NP/app/worker.glp
run_local(W).
run_own(M).
:quit
HEREDOC
2>&1)
check "nested: own-dir self.glp linked (mid_only=mid)" "M = mid" "$np_worker"
check "nested: module def shadows all self.glp (where=leaf_tag)" "W = leaf_tag" "$np_worker"

# --- A26f: single-module scope renaming (no ancestor-call hijack; collision) ---
# Static linking renames every SCOPE module (ancestor/own-dir self.glp, exposed)
# to M:p and resolves each module's calls in its own scope, while the loaded
# module keeps its bare names as plain-name entry points (modules.tex §Static
# Linking step 3 + §3.4). So an ancestor self.glp's internal call resolves to
# its OWN renamed procedure, never to the loaded module's same-named bare proc.
# shadow_internal/: ancestor self.glp defines helper(anc_a) + anc(R):-helper(R);
# the loaded m.glp redefines helper(mod_b) and run(R):-anc(R). run must yield
# anc_a — anc keeps its own helper. (Regression for the noRename bare-name hijack.)
echo "--- A26f: single-module scope renaming ---"
a26f=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/shadow_internal/m.glp
run(R).
:quit
HEREDOC
2>&1)
check "single-module: ancestor keeps own helper (run=anc_a, not hijacked)" "R = anc_a" "$a26f"

# Two modules exposed at one level sharing an exported name/arity is an error,
# raised at link time on the single-module path too (modules.tex §-expose).
a26f_col=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/expose_collide/leaf.glp
:quit
HEREDOC
2>&1)
check "single-module: -expose collision rejected at link time" "expose collision" "$a26f_col"

# --- A27: Reader-to-reader bug (befriend_intro) ---
echo "--- A27: Reader-to-reader fail ---"
a27=$("$REPL_RUN" <<HEREDOC
$TYPED/test_befriend_intro_bug.glp
med(charlie, ch([msg(agent, _user, befriend_intro(bob, alice, X?)) | Xs], Y), ch(Us?, Vs), [], 2).
:quit
HEREDOC
2>&1)
check_not "reader-to-reader no reduction" "req(2)" "$a27"

# --- A28: Module guard ---
echo "--- A28: Module guard ---"
a28=$("$REPL_RUN" <<HEREDOC
$TYPED/module_guard.glp
test_not_module(42, Rm1).
:quit
HEREDOC
2>&1)
check "module guard ~module(42)" "Rm1 = not_module" "$a28"

# --- A29: Struct terms inside lists in goal arguments (Issue 0b regression) ---
echo "--- A29: Structs in list goal args ---"
a29=$("$REPL_RUN" <<HEREDOC
$BOOK/streams/producers_consumers/distribute_indexed.glp
distribute_indexed([send(1,a), send(2,b), send(1,c), send(2,d)], Y, Z).
:quit
HEREDOC
2>&1)
check "Struct-in-list goal arg: route Y" "Y = \[a, c\]" "$a29"
check "Struct-in-list goal arg: route Z" "Z = \[b, d\]" "$a29"

# --- A30: =.. (univ) as a goal in a clause body (Issue 0a regression) ---
echo "--- A30: =.. as a body goal ---"
a30=$("$REPL_RUN" <<HEREDOC
$TYPED/univ_body.glp
comp([foo, a, b], T).
comp([greet, hello, world], G).
:quit
HEREDOC
2>&1)
check "Body =.. compose foo" "T = foo(a, b)" "$a30"
check "Body =.. compose greet" "G = greet(hello, world)" "$a30"

# --- A31: Forwarded-writer reactivation (bug of 2026-07-04) ---
# A consumer suspended on a reader must wake when its writer, forwarded down a
# recursive lookup, is bound through a chain ending in an already-bound value.
echo "--- A31: Forwarded-writer reactivation ---"
a31=$("$REPL_RUN" <<HEREDOC
$TYPED/forwarded_writer_wake.glp
test_d1(O1).
test_d2(O2).
test_d4(O4).
:quit
HEREDOC
2>&1)
check "Forwarded-writer wake depth 1" "O1 = a" "$a31"
check "Forwarded-writer wake depth 2" "O2 = a" "$a31"
check "Forwarded-writer wake depth 4" "O4 = a" "$a31"

# --- A32: Guard suspension on expression readers; =/2 call convention ---
# Bug of 2026-07-22 (pair-disconnect report): (1) a comparison guard whose
# operand held an unbound reader inside an arithmetic/constant expression
# failed instead of suspending (runner.dart); (2) =/2 was declared with reversed
# modes (=(_?, _)) so the type-checker accepted the backwards call `R4? = Rest`
# (which deadlocks) and rejected the sanctioned `Rest = R4?`; declaration fixed
# to =(X, X?), clause X? = X. unchanged, and the P99 call sites corrected.
echo "--- A32: Guard suspension on expression readers; =/2 call convention ---"
a32=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_p99_probe4.glp
walkd(35, 4, R1).
oncec(35, 4, R2).
walk(35, 4, R3).
gst([a, '(', b, ',', c, ')'], T).
:quit
HEREDOC
2>&1)
check "Expression-guard suspend (walkd)" "R1 = 6" "$a32"
check "=/2 on unbound reader (oncec)" "R2 = 6" "$a32"
check "pf clause-4 shape (walk)" "R3 = 6" "$a32"
check "p67 pt1 chain (gst)" "T = gt(a, gt(b, gnil, gnil), gt(c, gnil, gnil))" "$a32"

# --- A33: Seeded random (random/4, GLP-ICLP appendix-guards) ---
# Value in [1,N]; NextSeed a function of Seed alone; threading reproduces
# exactly (rthree draws three, threading NextSeed); N < 1 fails at the guard.
echo "--- A33: Seeded random ---"
a33=$("$REPL_RUN" <<HEREDOC
$TYPED/random_test.glp
random(42, 6, V, NS).
rthree(1, R).
random(42, 0, VBad, NSBad).
:quit
HEREDOC
2>&1)
check "random single draw value" "V = 1" "$a33"
check "random single draw next seed" "NS = 705894" "$a33"
check "random threaded triple reproduces" "R = rt(808, 250, 74)" "$a33"
check "random N<1 fails at guard" "→ failed" "$a33"

SECTION_A_PASS=$PASS
SECTION_A_FAIL=$FAIL

echo ""
echo "Section A: $SECTION_A_PASS passed, $SECTION_A_FAIL failed"
echo ""

# =============================================================================
# SECTION B: TYPE-CHECK-ONLY POSITIVE TESTS
# (Load each file, verify "Loaded:" message, use :clear between files)
# =============================================================================
echo "=== Section B: Positive Type Check Tests ==="
echo ""

POSITIVE_FILES=(
    # --- typechecker/positive ---
    # Structural identity in the duality check (§20.3): a Stream<A> stored into a
    # field typed by the named alias AStream must type-check (false positive before
    # the alias-aware same-base fix).
    "$TC_DIR/positive/alias_dual_field.glp"
    "$TC_DIR/positive/merge_basic.glp"
    "$TC_DIR/positive/append_list.glp"
    "$TC_DIR/positive/copy_stream.glp"
    "$TC_DIR/positive/dl_append.glp"
    "$TC_DIR/positive/new_channel.glp"
    "$TC_DIR/positive/monitor.glp"
    "$TC_DIR/positive/int_list_sum.glp"
    "$TC_DIR/positive/nat_operations.glp"
    "$TC_DIR/positive/process_complete.glp"
    "$TC_DIR/positive/disjoint_primitives.glp"
    "$TC_DIR/positive/universal_structured_term.glp"
    "$TC_DIR/positive/guards_all.glp"
    "$TC_DIR/positive/merge_variable_coverage_base.glp"
    "$TC_DIR/positive/merge_variable_coverage_mixed.glp"
    "$TC_DIR/positive/merge_variable_coverage_recursive.glp"
    "$TC_DIR/positive/toplevel_type_param.glp"
    "$TC_DIR/positive/book/universal_accepts_structured.glp"

    # --- moded_types/valid ---
    "$MODED/valid/append.glp"
    "$MODED/valid/counter.glp"
    "$MODED/valid/simple_io.glp"
    "$MODED/valid/merge.glp"
    "$MODED/valid/union_alias_basic.glp"
    "$MODED/valid/union_alias_simple.glp"
    "$MODED/valid/union_alias_three.glp"
    # Two-clause channel consumer: receive (non-empty read stream) + close (closed
    # read stream). Unfolding the guards places ch([X|In],Out?) and ch([],[]) in
    # the two heads, which together cover the channel's read stream (cond. 2).
    "$MODED/valid/channel_consumer_closed.glp"

    # --- moded_types/valid/embedded ---
    "$MODED/valid/embedded/counter_show.glp"
    "$MODED/valid/embedded/input_with_input_embedded.glp"
    "$MODED/valid/embedded/output_with_input_embedded.glp"
    "$MODED/valid/embedded/output_with_output_embedded.glp"

    # --- moded_types/valid/universal ---
    "$MODED/valid/universal/any_copy.glp"
    "$MODED/valid/universal/any_multi_clause.glp"
    "$MODED/valid/universal/list_with_any_element.glp"
    "$MODED/valid/universal/any_constant_at_output.glp"
    "$MODED/valid/universal/any_constant_at_input.glp"
    "$MODED/valid/universal/any_empty_list.glp"

    # --- book/constants ---
    "$BOOK/constants/circuits.glp"
    "$BOOK/constants/gates.glp"
    "$BOOK/constants/gates_simple.glp"

    # --- book/recursive/arithmetic_trees ---
    "$BOOK/recursive/arithmetic_trees/ackermann.glp"
    "$BOOK/recursive/arithmetic_trees/exp.glp"
    "$BOOK/recursive/arithmetic_trees/factorial.glp"
    "$BOOK/recursive/arithmetic_trees/fibonacci.glp"
    "$BOOK/recursive/arithmetic_trees/gcd_integer.glp"
    "$BOOK/recursive/arithmetic_trees/lesseq.glp"
    "$BOOK/recursive/arithmetic_trees/min.glp"
    "$BOOK/recursive/arithmetic_trees/natural_numbers.glp"
    "$BOOK/recursive/arithmetic_trees/plus.glp"
    "$BOOK/recursive/arithmetic_trees/primes.glp"
    "$BOOK/recursive/arithmetic_trees/times.glp"

    # --- book/recursive/list_processing ---
    "$BOOK/recursive/list_processing/append.glp"
    "$BOOK/recursive/list_processing/copy.glp"
    "$BOOK/recursive/list_processing/delete.glp"
    "$BOOK/recursive/list_processing/filter_even.glp"
    "$BOOK/recursive/list_processing/inner_product.glp"
    "$BOOK/recursive/list_processing/inner_product_iter.glp"
    "$BOOK/recursive/list_processing/insertion_sort.glp"
    "$BOOK/recursive/list_processing/length.glp"
    "$BOOK/recursive/list_processing/map_inc.glp"
    "$BOOK/recursive/list_processing/maxlist.glp"
    "$BOOK/recursive/list_processing/member.glp"
    "$BOOK/recursive/list_processing/merge_ordered.glp"
    "$BOOK/recursive/list_processing/merge_sort.glp"
    "$BOOK/recursive/list_processing/nth.glp"
    "$BOOK/recursive/list_processing/polygon_area.glp"
    "$BOOK/recursive/list_processing/quicksort.glp"
    "$BOOK/recursive/list_processing/reverse.glp"
    "$BOOK/recursive/list_processing/reverse_naive.glp"
    "$BOOK/recursive/list_processing/translate.glp"
    "$BOOK/recursive/list_processing/variants/quicksort_original.glp"

    # --- book/recursive/structure_processing ---
    "$BOOK/recursive/structure_processing/binary_tree.glp"
    "$BOOK/recursive/structure_processing/list_to_bst.glp"
    "$BOOK/recursive/structure_processing/observe.glp"
    "$BOOK/recursive/structure_processing/substitute.glp"
    "$BOOK/recursive/structure_processing/traversals.glp"
    "$BOOK/recursive/structure_processing/tree_sum.glp"

    # --- book/social_networks ---
    "$BOOK/social_networks/broadcast.glp"
    "$BOOK/social_networks/replicate.glp"
    "$BOOK/social_networks/interlaced_streams.glp"

    # --- book/streams/buffered_communication ---
    "$BOOK/streams/buffered_communication/hollow_integers.glp"

    # --- book/streams/producers_consumers ---
    "$BOOK/streams/producers_consumers/biased_merge.glp"
    "$BOOK/streams/producers_consumers/coop_stream.glp"
    "$BOOK/streams/producers_consumers/cooperative_producers.glp"
    "$BOOK/streams/producers_consumers/distribute.glp"
    "$BOOK/streams/producers_consumers/distribute_binary.glp"
    "$BOOK/streams/producers_consumers/distribute_indexed.glp"
    "$BOOK/streams/producers_consumers/fair_merge.glp"
    "$BOOK/streams/producers_consumers/merge_simple.glp"
    "$BOOK/streams/producers_consumers/merge_tree.glp"
    "$BOOK/streams/producers_consumers/mwm.glp"
    "$BOOK/streams/producers_consumers/producer_consumer.glp"
    "$BOOK/streams/producers_consumers/producer_consumer_countdown.glp"
    "$BOOK/streams/producers_consumers/relay.glp"

    # --- book/streams/objects_monitors ---
    "$BOOK/streams/objects_monitors/counter_monitor.glp"
    "$BOOK/streams/objects_monitors/network_switch_3way.glp"

    # --- book/meta (GLP-ICLP paper metainterpreters) ---
    "$BOOK/meta/plain/plain_meta.glp"
    "$BOOK/meta/plain/failsafe_meta.glp"
    "$BOOK/meta/enhanced/control_meta.glp"
    "$BOOK/meta/enhanced/termination_meta.glp"
    "$BOOK/meta/enhanced/snapshot_meta.glp"
    "$BOOK/meta/enhanced/tracing_meta.glp"
    "$BOOK/meta/debugging/runtime_control_meta.glp"

    # --- lib (GLP-ICLP paper techniques) ---
    "$GLP_DIR/programs/lib/lookup/lookup.glp"
    "$GLP_DIR/programs/lib/streams/tag_stream.glp"

    # --- book/misc ---
    "$BOOK/test_bug.glp"
    "$BOOK/test_friend.glp"
    "$BOOK/test_lookup2.glp"

    # --- subtyping positive tests ---
    "$TC_DIR/positive/subtyping/basic_readop_fileop.glp"
    "$TC_DIR/positive/subtyping/constants_fewer_alternatives.glp"
    "$TC_DIR/positive/subtyping/contravariant_response_slot.glp"
    "$TC_DIR/positive/subtyping/direct_constant_subtype.glp"
    "$TC_DIR/positive/subtyping/struct_fewer_functors.glp"
    # Subtyping through the root self.glp unions Number ::= Integer ; Real. and
    # Constant ::= Number ; String ; Module.  Negative twins:
    # negative/subtyping/{constant_to_integer_reverse,integer_to_string_disjoint,
    # head_pair_integer_to_constant,compound_to_constant}.glp.
    "$TC_DIR/positive/subtyping/constant_accepts_all_literals.glp"
    "$TC_DIR/positive/subtyping/integer_below_number_and_constant.glp"
    "$TC_DIR/positive/subtyping/int_list_to_constant_list.glp"
    "$TC_DIR/positive/subtyping/constant_union_to_constant.glp"

    # --- module guard test ---
    "$TYPED/module_guard.glp"

    # --- parameterized types ---
    "$TYPED/param_stream_integer.glp"
    "$TYPED/param_channel.glp"
    "$TYPED/param_procedure_inference.glp"

    # --- Abstract-parameter routing matrix (sec:abstract-parameters) ---
    # Case (i): a clean parametric proc (no parameter inspection) that COVERS the
    # whole input takes the abstract route and is certified clean against its
    # abstract instance, even though it is never instantiated.
    "$GLP_DIR/programs/tests/param_abstract_covered.glp"

    # --- SRSW relaxations (glp.tex Remark "Guards and SRSW") ---
    # Both must be ACCEPTED; their negative counterpart is srsw/known_not_ground
    # in Section D. A guard occurrence counts toward SRSW, so X? in a guard plus
    # once in the body is legal; and a groundness-implying guard licenses
    # multiple occurrences of both halves.
    "$GLP_DIR/programs/tests/srsw/guard_occurrence_ok.glp"
    "$GLP_DIR/programs/tests/srsw/ground_guard_multi_ok.glp"
)

# Build REPL input: load each positive file with :clear between
B_INPUT=""
for f in "${POSITIVE_FILES[@]}"; do
    B_INPUT+="$f"$'\n'
    B_INPUT+=":clear"$'\n'
done
B_INPUT+=":quit"$'\n'

b_output=$(echo "$B_INPUT" | "$REPL_RUN" 2>&1)

B_PASS=0
B_FAIL=0
FAILED_POSITIVE=()
for f in "${POSITIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    # Check for errors first
    if echo "$b_output" | grep -q "Type errors in $f"; then
        echo "  FAIL: $name (unexpected type errors)"
        B_FAIL=$((B_FAIL + 1))
        FAIL=$((FAIL + 1))
        FAILED_POSITIVE+=("$f")
    elif echo "$b_output" | grep -q "SRSW violations in $f"; then
        echo "  FAIL: $name (unexpected SRSW violations)"
        B_FAIL=$((B_FAIL + 1))
        FAIL=$((FAIL + 1))
        FAILED_POSITIVE+=("$f")
    elif echo "$b_output" | grep -q "Error loading $f"; then
        echo "  FAIL: $name (loading error)"
        B_FAIL=$((B_FAIL + 1))
        FAIL=$((FAIL + 1))
        FAILED_POSITIVE+=("$f")
    elif echo "$b_output" | grep -q "Loaded: $f"; then
        echo "  PASS: $name"
        B_PASS=$((B_PASS + 1))
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (unknown failure)"
        B_FAIL=$((B_FAIL + 1))
        FAIL=$((FAIL + 1))
        FAILED_POSITIVE+=("$f")
    fi
done

echo ""
echo "Section B: $B_PASS passed, $B_FAIL failed"
if [ ${#FAILED_POSITIVE[@]} -gt 0 ]; then
    echo "Failed positive tests:"
    for f in "${FAILED_POSITIVE[@]}"; do
        echo "  - $f"
    done
fi
echo ""

# =============================================================================
# SECTION C: NEGATIVE TYPE TESTS (must be rejected)
# =============================================================================
echo "=== Section C: Negative Type Tests ==="
echo ""

NEGATIVE_FILES=(
    # --- typechecker/negative/coverage ---
    "$TC_DIR/negative/coverage/merge_missing_both_nil.glp"
    "$TC_DIR/negative/coverage/merge_missing_first_nil.glp"
    "$TC_DIR/negative/coverage/merge_missing_cons.glp"

    # --- typechecker/negative/head ---
    "$TC_DIR/negative/head/merge_wrong_constant.glp"
    "$TC_DIR/negative/head/merge_wrong_functor.glp"
    # Moved here from Section D on 2026-08-02. Both are rejected by the type
    # checker, not by the SRSW pass — measured — so Section D asserted nothing
    # about SRSW for them. Their headers announce a mode error and an SRSW
    # violation; the type checker reaches the mode error first.
    "$TC_DIR/negative/head/merge_reader_at_input.glp"
    "$TC_DIR/negative/head/merge_writer_at_output.glp"

    # --- typechecker/negative/body ---
    "$TC_DIR/negative/body/merge_undefined_proc.glp"
    "$TC_DIR/negative/body/merge_wrong_mode.glp"

    # --- typechecker/negative/complementarity ---
    "$TC_DIR/negative/complementarity/merge_type_mismatch.glp"
    "$TC_DIR/negative/complementarity/merge_swapped_vars.glp"

    # --- typechecker/negative (top level) ---
    "$TC_DIR/negative/merge_incomplete.glp"
    "$TC_DIR/negative/missing_coverage.glp"
    # Nested-union coverage gap at a list head: CounterMsg declares up/down but
    # no clause handles them (CounterStream -> [|] -> up/down uncovered). Requires
    # descending into the union nested under the list constructor. Positive twin:
    # moded_types/valid/embedded/counter_show.glp.
    "$TC_DIR/negative/counter_missing_updown.glp"
    # The same defect in the shape that escaped: vGLP's repro of 2026-07-09,
    # kept here (at vGLP's request of 2026-07-31) now that the report has been
    # retired to vGLP/OLD/, so the regression is argued from the suite. Color
    # declares red and blue; no clause handles blue.
    "$TC_DIR/negative/color_missing_blue.glp"
    # Channel consumer missing the close clause: unfolding `receive` leaves the
    # closed read stream ch([], _) uncovered. Positive twin:
    # moded_types/valid/channel_consumer_closed.glp.
    "$TC_DIR/negative/channel_missing_close.glp"
    "$TC_DIR/negative/non_complementary_types.glp"
    "$TC_DIR/negative/append_bad_type.glp"
    "$TC_DIR/negative/constant_at_wrong_type.glp"
    "$TC_DIR/negative/functor_mismatch.glp"
    "$TC_DIR/negative/channel_non_complementary.glp"

    # --- typed (primitive layer) ---
    # Rule B, definition position: a module that does not declare -mode(system)
    # may not declare or define a procedure whose name is a quoted
    # underscore-prefixed constant.  This file declares and defines
    # '_test_kernel'.  It was a POSITIVE fixture in Section A25 until the rule
    # was corrected on 2026-07-31 to test the prefix and the position rather
    # than a list of reserved names; the file is kept, reclassified, because its
    # subject is exactly what the rule now forbids.
    "$TYPED/quoted_functor_test.glp"

    # --- moded_types/invalid ---
    "$MODED/invalid/reader_at_input.glp"
    "$MODED/invalid/writer_at_output.glp"
    "$MODED/invalid/call_mode_mismatch.glp"
    "$MODED/invalid/embedded_mode_error.glp"
    "$MODED/invalid/union_alias_overlap.glp"
    "$MODED/invalid/union_alias_refs_alias.glp"

    # --- moded_types/invalid/embedded ---
    "$MODED/invalid/embedded/counter_wrong_mode.glp"

    # --- moded_types/invalid/deep ---
    "$MODED/invalid/deep/accumulator_wrong_mode.glp"
    "$MODED/invalid/deep/channel_wrong_inversion.glp"
    "$MODED/invalid/deep/correct_type_wrong_annotation.glp"
    "$MODED/invalid/deep/double_nesting_error.glp"
    "$MODED/invalid/deep/list_tail_mode_error.glp"
    "$MODED/invalid/deep/mixed_clauses.glp"
    "$MODED/invalid/deep/nested_struct_wrong_mode.glp"
    "$MODED/invalid/deep/pair_list_wrong_mode.glp"
    "$MODED/invalid/deep/recursive_type_deep_error.glp"
    "$MODED/invalid/deep/response_slot_no_embedded.glp"

    # --- moded_types/invalid/universal ---
    "$MODED/invalid/universal/any_list_cons.glp"
    "$MODED/invalid/universal/any_mixed_clauses.glp"
    "$MODED/invalid/universal/any_reduce_pattern.glp"
    "$MODED/invalid/universal/any_struct_at_input.glp"
    "$MODED/invalid/universal/any_struct_at_output.glp"

    # --- subtyping negative tests ---
    "$TC_DIR/negative/subtyping/wrong_direction_fileop_readop.glp"
    "$TC_DIR/negative/subtyping/contravariant_wrong_direction.glp"
    "$TC_DIR/negative/subtyping/disjoint_types.glp"
    "$TC_DIR/negative/subtyping/arg_type_mismatch.glp"
    # Direction and scope of the primitive subtype order: Constant is not below
    # Integer, Integer and String are unrelated, a compound is not a Constant,
    # and subtyping does not reach a head-head pair.
    "$TC_DIR/negative/subtyping/constant_to_integer_reverse.glp"
    "$TC_DIR/negative/subtyping/integer_to_string_disjoint.glp"
    "$TC_DIR/negative/subtyping/compound_to_constant.glp"
    "$TC_DIR/negative/subtyping/head_pair_integer_to_constant.glp"

    # --- CSSN Issue 16: control token merged into a narrower network-input reader ---
    # Stream(Ctl) (carries `canonical`) merged into Stream(NetLike); the (Both,Both?)
    # pair is not dual.  Guards against re-adding a control token to FriendMsg.
    "$TC_DIR/negative/subtyping/control_token_merge.glp"

    # --- Alias-aware parameter inference (§20.3 structural identity) ---
    # A named list alias (Outs ::= [] ; [Ent | Outs]) routed through a parameterized
    # procedure must instantiate it at X=Ent and be checked; the masked error surfaces
    # only when inference resolves the alias to its structural form Stream<Ent>.
    "$TC_DIR/negative/subtyping/alias_param_inference.glp"

    # --- parameterized types negative ---
    "$TYPED/param_arity_mismatch.glp"

    # --- parameterized proc decl negative: SRSW violation in a clause body ---
    # (Zs? reader in both head arg 3 and body, no writer). SRSW is checked at
    # load independent of type instantiation, so this is rejected even though the
    # parameterized proc is never instantiated.
    "$TC_DIR/negative/body/param_merge_wrong_mode.glp"

    # --- Issue 14: polymorphic-polarity re-check at instantiation (Case B) ---
    # pconsumer's Stream(X)? body matches a reader where the inferred element
    # type ProdMsg requires a writer; rejected only after the per-instantiation
    # check re-runs the clause under X := ProdMsg. Accepted before the fix.
    "$GLP_DIR/programs/tests/min_polarity_bug3.glp"

    # --- Parameterized instantiation closure (closed under calls) ---
    # inner's polarity clash is reachable only through go -> outer -> inner;
    # caught only when per-instantiation checking is closed under calls.
    "$GLP_DIR/programs/tests/min_polarity_closure.glp"

    # --- Finiteness rule: recursive parameterized type, param as proper subterm ---
    # Bad(X) ::= node(Bad(Box(X))) — rejected statically at the expansion stage.
    "$GLP_DIR/programs/tests/growing_type_recursion.glp"

    # --- Monomorphic recursion: a recursive call at a different instantiation ---
    # wrap threads Stream(X) -> Stream(Box(X)); the recursive ploop is checked at
    # the enclosing instantiation, so the type-changing recursion is rejected.
    "$GLP_DIR/programs/tests/monomorphic_recursion.glp"

    # --- Abstract-parameter routing matrix, case (ii): a clean parametric proc
    # (no parameter inspection) with a COVERAGE GAP takes the abstract route and is
    # rejected against its abstract instance pdrop(Stream<$abstract_X>?) — [] is
    # uncovered — even though it is never instantiated. (Paper Decision 1: coverage
    # is part of def:parametrically-well-typed.) Filename retained for continuity;
    # see the file header.
    "$GLP_DIR/programs/tests/param_free_not_checked.glp"

    # --- Abstract-parameter routing: same clean pdrop gap, also instantiated by
    # go/1. The abstract route catches the gap whether or not pdrop is instantiated;
    # the instantiation does not mask it.
    "$GLP_DIR/programs/tests/param_instantiated_coverage_gap.glp"

    # --- Abstract-parameter routing matrix, case (iii): a parametric proc that
    # INSPECTS its parameter (a constant/functor at a parameter position) takes the
    # per-instantiation route and is NOT certified by the abstract route. Loaded
    # standalone with no instantiation it has nothing to certify, so it is rejected
    # (typed-program.md "Modular Checking via Abstract Parameters",
    # sec:abstract-parameters). Within a program an instantiation supplies the verdict.
    "$GLP_DIR/programs/tests/param_inspect_uninstantiated.glp"
    # Condition 2 of def:well-typed-clause at a call to a parameterised
    # procedure whose parameter the call site cannot bind: the modes of the
    # top-level arguments are fixed by the template, so a transposed writer and
    # reader is rejected without waiting for the element type.
    "$GLP_DIR/programs/tests/param_call_mode_neg.glp"

    # --- book/ examples (owned by GLP-ICLP) mis-declared with a bare type
    # parameter where a concrete type belongs: a constant/functor sits at the
    # parameter position, so they inspect the parameter, take the per-instantiation
    # route, and loaded standalone with no instantiation have nothing to certify —
    # rejected (sec:abstract-parameters). Listed as expected rejections until
    # GLP-ICLP re-declares or prunes them.
    "$BOOK/social_graph/channel.glp"
    "$BOOK/social_graph/typed_social_agent.glp"
    "$BOOK/streams/producers_consumers/cooperative.glp"
    "$BOOK/streams/producers_consumers/merge_dynamic.glp"
)

# Build REPL input with :clear between each negative file
C_INPUT=""
for f in "${NEGATIVE_FILES[@]}"; do
    C_INPUT+="$f"$'\n'
    C_INPUT+=":clear"$'\n'
done
C_INPUT+=":quit"$'\n'

c_output=$(echo "$C_INPUT" | "$REPL_RUN" 2>&1)

C_PASS=0
C_FAIL=0
for f in "${NEGATIVE_FILES[@]}"; do
    name=$(basename "$f" .glp)
    if echo "$c_output" | grep -q "Loaded: $f"; then
        echo "  FAIL: $name (expected rejection, got loaded)"
        C_FAIL=$((C_FAIL + 1))
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $name (rejected)"
        C_PASS=$((C_PASS + 1))
        PASS=$((PASS + 1))
    fi
done

echo ""
echo "Section C: $C_PASS passed, $C_FAIL failed"
echo ""

# =============================================================================
# SECTION D: SRSW VIOLATION TESTS
# =============================================================================
echo "=== Section D: SRSW Violation Tests ==="
echo ""

# Every file here must be rejected by the SRSW pass and say so. The condition
# used to be `grep -qi "SRSW violation\|Error loading"`, which any rejection
# satisfied: two of the three entries were rejected by the type checker before
# the SRSW pass ran, so this section asserted nothing about SRSW for them. They
# have moved to NEGATIVE_FILES, where their actual verdict is what is checked.
SRSW_FILES=(
    # Every violation class of def:glp-program in one file, each with its own
    # diagnostic: a writer twice and a reader twice (SO), and a variable with no
    # reader and one with no writer (the pairing requirement).
    "$GLP_DIR/programs/tests/srsw/srsw_multi_error.glp"
    # known/1 does not imply groundness, so it licenses no multiple occurrence
    # (glp.tex Remark "Guards and SRSW").
    "$GLP_DIR/programs/tests/srsw/known_not_ground.glp"
)

for f in "${SRSW_FILES[@]}"; do
    name=$(basename "$f" .glp)
    srsw_out=$(echo -e "$f\n:quit" | "$REPL_RUN" 2>&1)
    if echo "$srsw_out" | grep -q "SRSW violations found"; then
        echo "  PASS: $name (SRSW rejected)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (should be rejected by the SRSW pass)"
        FAIL=$((FAIL + 1))
    fi
done

# The violation classes of def:glp-program, each named in the diagnostic rather
# than merely bundled into a rejection.
srsw_multi=$(echo -e "$GLP_DIR/programs/tests/srsw/srsw_multi_error.glp\n:quit" | "$REPL_RUN" 2>&1)
check "SO: a writer occurring twice" "Writer variable \"X\" occurs 2 times" "$srsw_multi"
check "SO: a reader occurring twice" "Reader variable \"Y?\" occurs 2 times" "$srsw_multi"
check "pairing: a variable with no reader" "Variable \"Z\" has no reader" "$srsw_multi"
check "pairing: a variable with no writer" "Variable \"Y\" has no writer" "$srsw_multi"

# merge_with_reader: the one entry of this section that is rejected by the SRSW
# pass rather than by the type checker, so it is the only test that speaks for
# the "SRSW-violation tests" of the paper's validation sentence. It was gated on
# `[ -f ... ]` and lived under programs/tests/archive/repl, a directory with an
# open instruction to delete it: deleting the file made the suite run one test
# fewer and fail nothing. Moved to programs/tests/srsw/ and wired unconditionally
# on 2026-08-02 — if the file goes missing now, this goes red.
SRSW_MWR="$GLP_DIR/programs/tests/srsw/merge_with_reader.glp"
srsw_mwr_out=$(echo -e "$SRSW_MWR\n:quit" | "$REPL_RUN" 2>&1)
if echo "$srsw_mwr_out" | grep -qi "SRSW violation"; then
    echo "  PASS: merge_with_reader (SRSW rejected)"
    PASS=$((PASS + 1))
else
    echo "  FAIL: merge_with_reader (should be rejected)"
    FAIL=$((FAIL + 1))
fi

echo ""

# =============================================================================
# SECTION E: INVALID GUARD TEST
# =============================================================================
echo "=== Section E: Invalid Guard Test ==="
echo ""

# The fixtures are on disk, in programs/tests/guards_invalid/, so a reader
# following the paper to the repository finds this category. It used to be one
# assertion on a file this script wrote to /tmp and deleted, which showed
# nothing to anybody reading the tree.
#
# Each entry pairs a fixture with the diagnostic it must produce, so a rejection
# for some other reason does not pass as an invalid-guard rejection.
GUARD_NEG_DIR="$GLP_DIR/programs/tests/guards_invalid"
guard_cases=(
    "true_in_guard.glp|\"true\" is not a guard"
    "false_in_guard.glp|\"false\" is not a guard"
    "fail_in_guard.glp|\"fail\" is not a guard"
    "negated_arithmetic.glp|Guard \"<\" cannot be negated"
    "negated_defined_guard.glp|Defined guard \"d\" cannot be negated"
)

for case in "${guard_cases[@]}"; do
    gfile="${case%%|*}"
    gmsg="${case#*|}"
    guard_out=$(echo -e "$GUARD_NEG_DIR/$gfile\n:quit" | "$REPL_RUN" 2>&1)
    if echo "$guard_out" | grep -qF "$gmsg"; then
        echo "  PASS: ${gfile%.glp} rejected ($gmsg)"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: ${gfile%.glp} should be rejected with: $gmsg"
        FAIL=$((FAIL + 1))
    fi
done

echo ""

# Section F (old-gen CSSG Modules) retired in A5 — CSSG v2 coverage is Section J.

# =============================================================================
# Section G: Social Graph Simulated UI Modules (project-directory loading)
# =============================================================================
echo "=== Section G: Social Graph Simulated UI Modules ==="
echo ""

SGSIM="$GLP_DIR/programs/social/graph"

# Loading
g_load=$("$REPL_RUN" <<HEREDOC
$SGSIM
:quit
HEREDOC
2>&1)

check "SG-SIM project loads" "Loaded program" "$g_load"
check_not "SG-SIM no type errors" "Type checking failed" "$g_load"
check_not "SG-SIM no load errors" "Error loading" "$g_load"

# Silent plays (play1-play3)
echo "--- Silent plays (play1-play3) ---"
for play_num in 1 2 3; do
    g_play=$("$REPL_RUN" <<HEREDOC
$SGSIM
play${play_num}.
:quit
HEREDOC
2>&1)
    check "SG play${play_num} succeeds" "succeeds\|suspended" "$g_play"
done

# Tagged plays (fplay1-fplay3) with output checks
echo "--- Tagged plays (fplay1-fplay3) ---"

g_fp1=$("$REPL_RUN" <<HEREDOC
$SGSIM
fplay1.
:quit
HEREDOC
2>&1)

check "SG fplay1 succeeds" "succeeds\|suspended" "$g_fp1"
check "SG fplay1 alice connected bob" "tagged(alice.*connected(bob)" "$g_fp1"
check "SG fplay1 charlie connected alice" "tagged(charlie.*connected(alice)" "$g_fp1"

g_fp2=$("$REPL_RUN" <<HEREDOC
$SGSIM
fplay2.
:quit
HEREDOC
2>&1)

check "SG fplay2 succeeds" "succeeds\|suspended" "$g_fp2"
check "SG fplay2 rejected" "tagged(alice.*rejected" "$g_fp2"

g_fp3=$("$REPL_RUN" <<HEREDOC
$SGSIM
fplay3.
:quit
HEREDOC
2>&1)

check "SG fplay3 succeeds" "succeeds\|suspended" "$g_fp3"

# Unfriend (paper §3a — guarded by either; one-way teardown, recipient complies)
echo "--- Unfriend (play_unfriend_send/recv/absent) ---"

g_uf_send=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_unfriend_send.
:quit
HEREDOC
2>&1)
check "SG unfriend_send succeeds" "succeeds\|suspended" "$g_uf_send"
check "SG unfriend_send teardown on friend channel" "msg(alice, bob, unfriend)" "$g_uf_send"

g_uf_recv=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_unfriend_recv.
:quit
HEREDOC
2>&1)
check "SG unfriend_recv succeeds" "succeeds\|suspended" "$g_uf_recv"
check "SG unfriend_recv notifies user" "unfriended(alice)" "$g_uf_recv"

g_uf_abs=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_unfriend_absent.
:quit
HEREDOC
2>&1)
check "SG unfriend_absent no-op succeeds" "succeeds\|suspended" "$g_uf_abs"

echo ""

# Section H (old-gen CSSN Modules) retired in A5 — CSSN v2 coverage is Section K.

# =============================================================================
# Section I: self.glp Procedure Tests
# =============================================================================
echo "=== Section I: self.glp Procedure Tests ==="
echo ""

SELFPROC_TESTS="$GLP_DIR/programs/tests"

# --- I1: self.glp shared procedure ---
echo "--- I1: self.glp shared procedure ---"
i1=$("$REPL_RUN" <<HEREDOC
$SELFPROC_TESTS/module_self_procs
test_self_proc(5, R).
:quit
HEREDOC
2>&1)

check "self.glp shared proc loads" "Loaded program" "$i1"
check "self.glp shared proc result" "R = 10" "$i1"

# --- I2: self.glp shadowing ---
echo "--- I2: self.glp shadowing ---"
i2=$("$REPL_RUN" <<HEREDOC
$SELFPROC_TESTS/module_self_shadow
test_shadow(X, Y).
:quit
HEREDOC
2>&1)

check "self.glp shadow loads" "Loaded program" "$i2"
check "self.glp shadow outer" "X = outer" "$i2"
check "self.glp shadow inner" "Y = inner" "$i2"

# --- I3: Local shadows self.glp ---
echo "--- I3: Local shadows self.glp ---"
i3=$("$REPL_RUN" <<HEREDOC
$SELFPROC_TESTS/module_self_local_shadow
test_local_shadow(R).
:quit
HEREDOC
2>&1)

check "local shadow loads" "Loaded program" "$i3"
check "local shadow result" "R = from_local" "$i3"

# --- I4: Type error in self.glp (negative) ---
echo "--- I4: Type error in self.glp (negative) ---"
i4=$("$REPL_RUN" <<HEREDOC
$SELFPROC_TESTS/module_self_type_error
:quit
HEREDOC
2>&1)

check "self.glp type error rejected" "Type checking failed\|type.*error\|Error" "$i4"
check_not "self.glp type error not loaded" "Loaded program" "$i4"

echo ""

# =============================================================================
# Section K: CSSN v2 Modules (child_agent with blocking consent)
#
# (Former Section J "CSSG v2 Modules" — programs/social/child_safe — was removed
# 2026-06-21: child_safe is retired in favour of cssn, and this section's
# child_agent fplay4-7 coverage is a strict subset of Section K below.)
# =============================================================================
echo "=== Section K: CSSN v2 Modules ==="
echo ""

CSSN_V2="$GLP_DIR/programs/cssn"

# Loading
k_load=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
:quit
HEREDOC
2>&1)

check "CSSN v2 project loads" "Loaded program" "$k_load"
check_not "CSSN v2 no type errors" "Type checking failed" "$k_load"

# fplay1-3: Basic social graph (adult-only, unchanged)
echo "--- CSSN v2 basic social graph (fplay1-fplay3) ---"

for play_num in 1 2 3; do
    k_fpN=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay${play_num}.
:quit
HEREDOC
2>&1)
    check "CSSN v2 fplay${play_num} succeeds" "succeeds\|suspended" "$k_fpN"
done

# fplay4-7: child_agent befriending
echo "--- CSSN v2 child_agent befriending (fplay4-fplay7) ---"

k_fp4=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay4.
:quit
HEREDOC
2>&1)

check "CSSN v2 fplay4 succeeds" "succeeds\|suspended" "$k_fp4"
check "CSSN v2 fplay4 carol connected dave" "tagged(carol.*connected(dave)" "$k_fp4"

for play_num in 5 6 7; do
    k_fpN=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay${play_num}.
:quit
HEREDOC
2>&1)
    check "CSSN v2 fplay${play_num} succeeds" "succeeds\|suspended" "$k_fpN"
done

# fplay14: Idempotent befriend commit — redundant child_introduce.
# Alice issues child_introduce(carol, bob, dave) TWICE.  Each child accepts both
# child_befriend notifications.  Idempotency suppresses the second commit, so
# `connected(dave)` is emitted exactly once on carol's side and `connected(carol)`
# exactly once on dave's side.
echo "--- CSSN v2 idempotent befriend commit (fplay14) ---"
k_fp14=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay14.
:quit
HEREDOC
2>&1)
check "CSSN v2 fplay14 succeeds" "succeeds\|suspended" "$k_fp14"
fp14_alice_introduces=$(echo "$k_fp14" | grep -c "tagged(alice, cmd(child_introduce(carol, bob, dave))")
fp14_carol_connected=$(echo "$k_fp14" | grep -c "tagged(carol, notify(connected(dave))")
fp14_dave_connected=$(echo "$k_fp14" | grep -c "tagged(dave, notify(connected(carol))")
if [ "$fp14_alice_introduces" = "2" ]; then
    echo "  PASS: CSSN v2 fplay14 alice issued two child_introduces"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay14 alice issued $fp14_alice_introduces child_introduces (expected 2)"
    FAIL=$((FAIL + 1))
fi
if [ "$fp14_carol_connected" = "1" ]; then
    echo "  PASS: CSSN v2 fplay14 carol connected(dave) emitted exactly once"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay14 carol connected(dave) emitted $fp14_carol_connected times (expected 1)"
    FAIL=$((FAIL + 1))
fi
if [ "$fp14_dave_connected" = "1" ]; then
    echo "  PASS: CSSN v2 fplay14 dave connected(carol) emitted exactly once"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay14 dave connected(carol) emitted $fp14_dave_connected times (expected 1)"
    FAIL=$((FAIL + 1))
fi

# fplay15: Idempotent befriend commit — simultaneous bilateral cold-call.
# Alice and Bob cold-call each other simultaneously, both accept.  The smaller-name
# tie-break converges both sides on a single canonical channel; the other commit
# is suppressed.  Each side emits exactly one connected/2 notification.
echo "--- CSSN v2 bilateral cold-call (fplay15) ---"
k_fp15=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay15.
:quit
HEREDOC
2>&1)
check "CSSN v2 fplay15 succeeds" "succeeds\|suspended" "$k_fp15"
fp15_alice_connect=$(echo "$k_fp15" | grep -c "tagged(alice, cmd(connect(bob))")
fp15_bob_connect=$(echo "$k_fp15" | grep -c "tagged(bob, cmd(connect(alice))")
fp15_alice_connected=$(echo "$k_fp15" | grep -c "tagged(alice, notify(connected(bob))")
fp15_bob_connected=$(echo "$k_fp15" | grep -c "tagged(bob, notify(connected(alice))")
if [ "$fp15_alice_connect" = "1" ] && [ "$fp15_bob_connect" = "1" ]; then
    echo "  PASS: CSSN v2 fplay15 both agents issued connect"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay15 connects: alice=$fp15_alice_connect bob=$fp15_bob_connect (expected 1 each)"
    FAIL=$((FAIL + 1))
fi
if [ "$fp15_alice_connected" = "1" ]; then
    echo "  PASS: CSSN v2 fplay15 alice connected(bob) emitted exactly once"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay15 alice connected(bob) emitted $fp15_alice_connected times (expected 1)"
    FAIL=$((FAIL + 1))
fi
if [ "$fp15_bob_connected" = "1" ]; then
    echo "  PASS: CSSN v2 fplay15 bob connected(alice) emitted exactly once"
    PASS=$((PASS + 1))
else
    echo "  FAIL: CSSN v2 fplay15 bob connected(alice) emitted $fp15_bob_connected times (expected 1)"
    FAIL=$((FAIL + 1))
fi

# fplay8-10: CSSN groups
echo "--- CSSN v2 group plays (fplay8-fplay10) ---"

k_fp8=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay8.
:quit
HEREDOC
2>&1)

check "CSSN v2 fplay8 succeeds" "succeeds\|suspended" "$k_fp8"
check "CSSN v2 fplay8 group_joined" "tagged(alice.*group_joined" "$k_fp8"

for play_num in 9 10; do
    k_fpN=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay${play_num}.
:quit
HEREDOC
2>&1)
    check "CSSN v2 fplay${play_num} succeeds" "succeeds\|suspended" "$k_fpN"
done

# fplay11: child-managed group with blocking consent
echo "--- CSSN v2 blocking consent play (fplay11) ---"

k_fp11=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay11.
:quit
HEREDOC
2>&1)

check "CSSN v2 fplay11 succeeds" "succeeds\|suspended" "$k_fp11"
check "CSSN v2 fplay11 tagged output" "tagged(" "$k_fp11"

# fplay12: adult-managed group with children
echo "--- CSSN v2 adult-managed group play (fplay12) ---"

k_fp12=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
fplay12.
:quit
HEREDOC
2>&1)

check "CSSN v2 fplay12 succeeds" "succeeds\|suspended" "$k_fp12"
check "CSSN v2 fplay12 tagged output" "tagged(" "$k_fp12"

echo ""

# =============================================================================
# Section L: Static Cross-Module Call Tests
# =============================================================================
echo "=== Section L: Static Cross-Module Call Tests ==="
echo ""

SCM="$GLP_DIR/programs/tests/static_cross_module/"

# --- L1: Static cross-module double (linked program; # resolved at link time) ---
echo "--- L1: Static cross-module double ---"
l1=$("$REPL_RUN" <<HEREDOC
$SCM
test_double(5, X).
:quit
HEREDOC
2>&1)

check "static cross-module program loaded" "Loaded program" "$l1"
check "test_double(5, X) = 10" "X = 10" "$l1"

# --- L2: Static cross-module triple ---
echo "--- L2: Static cross-module triple ---"
l2=$("$REPL_RUN" <<HEREDOC
$SCM
test_triple(4, X).
:quit
HEREDOC
2>&1)

check "test_triple(4, X) = 12" "X = 12" "$l2"

# --- L3: Static cross-module add_ten ---
echo "--- L3: Static cross-module add_ten ---"
l3=$("$REPL_RUN" <<HEREDOC
$SCM
test_add_ten(7, X).
:quit
HEREDOC
2>&1)

check "test_add_ten(7, X) = 17" "X = 17" "$l3"

echo ""

# =============================================================================
# Section M: Multi-Isolate (madGLP) Tests
# =============================================================================

echo "=== Section M: Multi-Isolate (madGLP) Tests ==="
echo ""
echo "  (folded into Section Q, which runs the whole Dart tree — running"
echo "   cssn_v2_isolate_test.dart here as well would only double-count it)"
echo ""

# =============================================================================
# Section N: Currencies Modules (project-directory loading, plays 1-12)
# =============================================================================
echo "=== Section N: Currencies Modules ==="
echo ""

BONDS_V2="$GLP_DIR/programs/currencies"

# Loading
n_load=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
:quit
HEREDOC
2>&1)

check "Currencies project loads" "Loaded program" "$n_load"
check_not "Currencies no type errors" "Type checking failed" "$n_load"

# fplay1: solo mint
echo "--- Currencies solo mint (fplay1) ---"

n_fp1=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay1.
:quit
HEREDOC
2>&1)

check "Currencies fplay1 succeeds" "succeeds" "$n_fp1"
check "Currencies fplay1 minted" "tagged(alice.*minted" "$n_fp1"

# fplay2: befriend + trade
echo "--- Currencies befriend + trade (fplay2) ---"

n_fp2=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay2.
:quit
HEREDOC
2>&1)

check "Currencies fplay2 succeeds" "succeeds" "$n_fp2"
check "Currencies fplay2 connected" "tagged(alice.*connected(bob)" "$n_fp2"
check "Currencies fplay2 trade_completed" "trade_completed" "$n_fp2"

# fplay3-6: trade variations
echo "--- Currencies trade plays (fplay3-fplay6) ---"

for play_num in 3 4 5 6; do
    n_fpN=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay${play_num}.
:quit
HEREDOC
2>&1)
    check "Currencies fplay${play_num} succeeds" "succeeds" "$n_fpN"
done

# fplay4b: time-dependent trade
echo "--- Currencies time-dependent trade (fplay4b) ---"

n_fp4b=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay4b.
:quit
HEREDOC
2>&1)

check "Currencies fplay4b succeeds" "succeeds" "$n_fp4b"

# fplay8-9: buyback + symmetric trade
echo "--- Currencies buyback + symmetric (fplay8-fplay9) ---"

for play_num in 8 9; do
    n_fpN=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay${play_num}.
:quit
HEREDOC
2>&1)
    check "Currencies fplay${play_num} succeeds" "succeeds" "$n_fpN"
done

# fplay10-11: escrow
echo "--- Currencies escrow plays (fplay10-fplay11) ---"

n_fp10=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay10.
:quit
HEREDOC
2>&1)

check "Currencies fplay10 succeeds" "succeeds" "$n_fp10"
check "Currencies fplay10 escrow" "escrow" "$n_fp10"

n_fp11=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
fplay11.
:quit
HEREDOC
2>&1)

check "Currencies fplay11 succeeds" "succeeds" "$n_fp11"
check "Currencies fplay11 escrow_cancelled" "escrow_cancelled" "$n_fp11"

# fplay12: village market (6 agents)
echo "--- Currencies village market (fplay12) ---"

n_fp12=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
:limit 5000000
fplay12.
:quit
HEREDOC
2>&1)

check "Currencies fplay12 succeeds" "succeeds" "$n_fp12"
check "Currencies fplay12 tagged output" "tagged(" "$n_fp12"

# fplay13: loan then issuer-dated redemption on per-agent local clocks
echo "--- Currencies loan then issuer-dated redemption (fplay13) ---"

n_fp13=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
:limit 5000000
fplay13.
:quit
HEREDOC
2>&1)

check "Currencies fplay13 succeeds" "succeeds" "$n_fp13"
check "Currencies fplay13 early presentation reaches the issuer as a normal offer (reclassified, not refused)" "trade_proposed(alice, \[lot(alice, 0, 3)" "$n_fp13"
check "Currencies fplay13 issuer advances its own date past maturity" "date_advanced(11)" "$n_fp13"
check "Currencies fplay13 redemption set-off returns lender's own coins" "bond(alice, 0, 1)" "$n_fp13"

# fplay_cl: credit line (paper §5 item 7), checked against Proposition prop:credit-line
echo "--- Currencies credit line (fplay_cl) ---"

n_clc=$("$REPL_RUN" <<HEREDOC
$BONDS_V2
:limit 5000000
fplay_cl.
:quit
HEREDOC
2>&1)

check "Currencies credit line succeeds" "succeeds" "$n_clc"
check "Currencies credit line: draw forwards the borrower's bonds to the lender" "tagged(alice, notify(trade_proposed(charlie" "$n_clc"
check "Currencies credit line: expiry judged by the escrow's own date d_e*" "date_advanced(26)" "$n_clc"
check "Currencies credit line: at expiry the escrow holds nothing (line fully returned)" "tagged(charlie, notify(balance_report(\[\])))" "$n_clc"
check "Currencies credit line: borrower has its principal bond back at T=25" "tagged(bob, notify(balance_report(\[bond(bob, 25, 1), bond(bob, 25, 2), bond(bob, 25, 3), bond(bob, 25, 4)\])))" "$n_clc"
check "Currencies credit line: at k_d=0 lender holds the full line plus interest, no bond at T" "tagged(alice, notify(balance_report(\[bond(bob, 20, 5), bond(bob, 20, 6), bond(alice, 0, 5)" "$n_clc"

echo ""

# =============================================================================
# Section O: Currencies Multi-Isolate Tests
# =============================================================================

echo "=== Section O: Currencies Multi-Isolate Tests ==="
echo ""
echo "  (folded into Section Q, which runs the whole Dart tree — running"
echo "   bonds_v2_isolate_test.dart here as well would only double-count it)"
echo ""

# =============================================================================
# SECTION P: MODULE BOUNDARY ENFORCEMENT TESTS
# =============================================================================
echo "=== Section P: Module Boundary Enforcement Tests ==="
echo ""

echo "--- Module boundary: exported vs private ---"
output=$("$REPL_RUN" <<HEREDOC
$TYPED/test_module_boundary.glp
public_proc(5, X).
private_proc(5, X).
:quit
HEREDOC
2>&1)
check "public_proc(5,X) returns X=6" "X = 6" "$output"
# A single-module program (loaded file, no self.glp) exports ALL its procedures,
# so every one is an entry point — modules.tex sec:static-linking. With -module
# removed there is no module-private REPL boundary on a single loaded file;
# private_proc is now callable. (Cross-module privacy still holds: a module
# resolves only what another's self.glp / exported declarations expose.)
check "private_proc(5,X) now callable (single module exports all): X=7" "X = 7" "$output"

echo ""

echo "--- Module-local private helper called from exported body ---"
output=$("$REPL_RUN" <<HEREDOC
$TYPED/module_local_private.glp
caller(5, Y).
:quit
HEREDOC
2>&1)
check "exported caller can reach private helper (Y=6)" "Y = 6" "$output"
check_not "no spawn-label error" "Spawn could not find" "$output"

echo ""

echo "--- Cross-module type error must not leak across the # seam ---"
# lib#relay (parametric, inserts constant `extra`) is instantiated by cons at
# Stream(Item), Item = a;b. `extra` is not an Item, so relay is ill-typed at that
# instantiation. The per-module check verifies only cons's call against the
# imported declaration; the linked-program check (project_linker:
# _seedCrossModuleInstantiations) must check lib's clauses at the importer's
# instantiation and reject. Guards the cross-module soundness hole.
output=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/cross_module_inspect_neg/
:quit
HEREDOC
2>&1)
check "cross-module param-inspect project rejected" "Head of lib:relay" "$output"
check_not "cross-module project not loaded green" "Loaded program: .*cross_module_inspect_neg" "$output"

echo ""

echo "--- A directory whose root self.glp exports nothing is not a program ---"
# modules.tex §Static Linking, "Entry and the absence of a boot module": a root
# self.glp that exports no procedure gives a program with no entry points, which
# step 5 restricts to the empty set, so no initial goal resolves against it and
# the loader rejects it rather than linking it and reporting success.
# co_load_neg's self.glp declares the type Coin and exports no procedure.
output=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/co_load_neg
:quit
HEREDOC
2>&1)
check "no-export directory rejected" "has no entry points" "$output"
check "rejection names the root self.glp" "self.glp exports no procedure" "$output"
check_not "no-export directory not loaded green" "Loaded program: .*co_load_neg" "$output"

# Control: the same load path over a directory whose root self.glp forwards an
# export. One entry point is enough; it need not be reached from another.
output=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/module_self_procs
test_self_proc(21, Y).
:quit
HEREDOC
2>&1)
check "forwarded root export is an entry point" "Y = 42" "$output"

echo ""

# =============================================================================
# Section J: SecureBonds (project-directory loading)
# =============================================================================
echo "=== Section J: SecureBonds ==="
echo ""

SECUREBONDS="$GLP_DIR/programs/currencies/secure"

# Loading
sb_load=$("$REPL_RUN" <<HEREDOC
$SECUREBONDS
:quit
HEREDOC
2>&1)

check "SecureBonds project loads" "Loaded program" "$sb_load"
check_not "SecureBonds no type errors" "Type checking failed" "$sb_load"
check_not "SecureBonds no load errors" "Error loading" "$sb_load"

# Play — sovereign finality with custodian acks
echo "--- SecureBonds play ---"
sb_play=$("$REPL_RUN" <<HEREDOC
$SECUREBONDS
play.
:quit
HEREDOC
2>&1)

check "SecureBonds play succeeds" "succeeds" "$sb_play"

# Play_recover — finality + recovery from log copy
echo "--- SecureBonds play_recover ---"
sb_recover=$("$REPL_RUN" <<HEREDOC
$SECUREBONDS
play_recover.
:quit
HEREDOC
2>&1)

check "SecureBonds play_recover succeeds" "succeeds" "$sb_recover"

echo ""

# =============================================================================
# Section S: Ancestor self.glp Scope Chain (A3 module-system amendment)
# =============================================================================
echo "=== Section S: Ancestor self.glp Scope Chain ==="
echo ""

SCOPE_CHAIN="$GLP_DIR/programs/tests/scope_chain"

# --- S1: file load resolves a multi-clause root self.glp procedure (:=/2) ---
echo "--- S1: file load resolves a multi-clause root self.glp procedure ---"
s1=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/s1_file/use_root_multiclause.glp
compute(R).
mtest(Z).
:quit
HEREDOC
2>&1)
check "S1 file loads" "Loaded:" "$s1"
check "S1 root multi-clause (:=) resolves" "R = 14" "$s1"
check "S1 root merge/3 resolves (file load)" "Z = \[1, 3, 2, 4\]" "$s1"

# --- S2: project load resolves a utility in an ancestor self.glp above the root ---
echo "--- S2: ancestor self.glp reach (intermediate ancestor above load point) ---"
s2=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/leaf
test_merge(Z).
:quit
HEREDOC
2>&1)
check "S2 leaf loads" "Loaded program" "$s2"
check "S2 ancestor pmerge resolves" "Z = \[1, 4, 2, 5, 3, 6\]" "$s2"

# --- S3: two distinct instantiations of the parameterised utility in one module ---
echo "--- S3: two instantiations of the parameterised utility ---"
s3=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/leaf
test_both(Zi, Zc).
:quit
HEREDOC
2>&1)
check "S3 integer instantiation" "Zi = \[1, 4, 2, 5, 3, 6\]" "$s3"
check "S3 constant instantiation" "Zc = \[\"a\", \"c\", \"b\", \"d\"\]" "$s3"

# --- S4: restated — currencies/play12 is not a program (Coordination, 2026-08-02 10:20) ---
# It asserted that play12 loads standalone. It does not and must not: play12/self.glp
# exports no procedure, so the program has no entry points (modules.tex §Static
# Linking, "Entry and the absence of a boot module"), and the play itself runs
# from programs/currencies. Coordination ruled the old assertion wrong rather
# than Grassroots Currencies' to repair. What the check was FOR — the I-1
# regression, an ancestor type failing to resolve through the directory load —
# is kept: an UnknownType would still be wrong, and now so would a load.
echo "--- S4: currencies/play12 is not a program (restated) ---"
s4=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/currencies/play12
:quit
HEREDOC
2>&1)
check "S4 play12 rejected: no entry points" "has no entry points" "$s4"
check_not "S4 no unknown type error" "UnknownType" "$s4"

# --- S5: opaque pass-through walker ---
echo "--- S5: opaque pass-through walker ---"
s5=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/opaque_walker
run(Result, S).
:quit
HEREDOC
2>&1)
check "S5 walker loads" "Loaded program" "$s5"
check "S5 opaque pass-through + append" "S = \[\"hello\"" "$s5"

# --- S6: walker parameterised over the entry type ---
echo "--- S6: parameterised walker (functor decomposition of type param) ---"
s6=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/param_walker
run(Result, S).
:quit
HEREDOC
2>&1)
check "S6 param walker loads" "Loaded program" "$s6"
check "S6 functor decomposition" "more(7)" "$s6"
check "S6 append via parameterised walker" "S = \[\"hello\"" "$s6"

# --- S7: shadowing — local definition wins over ancestor ---
echo "--- S7: shadowing (local pmerge shadows ancestor) ---"
s7=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/shadow
test_shadow(Z).
:quit
HEREDOC
2>&1)
check "S7 shadow loads" "Loaded program" "$s7"
check "S7 local pmerge resolves (not ancestor interleave)" "Z = \[1, 2, 3\]" "$s7"

# --- S8: goal-check env includes intermediate-ancestor types (regression) ---
# The REPL goal checker builds its environment per modules.tex §Scope
# construction: root scope, then every self.glp on the path from programs/
# down to the program root, then the program's modules. Before the fix,
# loadProgram skipped the intermediate ancestors, so a goal on a procedure
# whose declaration instantiates an ancestor-defined type (here Wrap(Integer)
# from scope_chain/self.glp) failed with UnknownTypeError. Original repro:
# programs/p99/lists, combinations(2, 4, Sols) — UnknownTypeError: Req.
echo "--- S8: goal-check env includes intermediate-ancestor types ---"
s8=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/leaf
wrap_all([1, 2, 3], W).
:quit
HEREDOC
2>&1)
check "S8 leaf loads" "Loaded program" "$s8"
check_not "S8 no unknown type error" "UnknownTypeError" "$s8"
check "S8 ancestor-typed goal runs" "W = \[wrap(1), wrap(2), wrap(3)\]" "$s8"

# --- S9: same regression on the single-FILE load path ---
# loadSource had the same gap as loadProgram (S8): the goal-check env merged
# only the loaded module, never the intermediate ancestor self.glp chain.
echo "--- S9: goal-check env on single-file load ---"
s9=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/leaf/client.glp
wrap_all([1, 2, 3], W).
:quit
HEREDOC
2>&1)
check "S9 file loads" "Loaded:" "$s9"
check_not "S9 no unknown type error" "UnknownTypeError" "$s9"
check "S9 ancestor-typed goal runs" "W = \[wrap(1), wrap(2), wrap(3)\]" "$s9"

# --- S10: hierarchy bound — file load outside programs/ has no ancestor scope ---
# The ancestor chain is anchored at the hierarchy root programs/ (modules.tex
# §Implicit ancestor scoping); a module file loaded from OUTSIDE the hierarchy
# gets no ancestor self.glp scope, even from its own directory. Foo is defined
# only in the fixture's sibling self.glp, so p(Foo) reads as a parametric
# declaration whose inspected parameter has no instantiation, and the load is
# rejected. Guards loadSource sharing the linker's programsDir bound (the
# legacy unbounded walk accepted this load).
echo "--- S10: no ancestor scope outside the hierarchy ---"
s10=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/test/fixtures/outside_hierarchy/m.glp
:quit
HEREDOC
2>&1)
check "S10 outside-hierarchy file load rejected" "no standalone well-typing" "$s10"

echo ""

# =============================================================================
# Section X: The -expose directive
# =============================================================================
echo "=== Section X: The -expose directive ==="
echo ""

EXPOSE="$GLP_DIR/programs/tests/expose"

# --- X1: a self.glp exposes a module; a leaf calls it unqualified ---
echo "--- X1: exposed procedure called unqualified ---"
x1=$("$REPL_RUN" <<HEREDOC
$EXPOSE/basic/leaf
run(R).
:quit
HEREDOC
2>&1)
check "X1 leaf loads" "Loaded program" "$x1"
check "X1 exposed twice resolves unqualified" "R = 42" "$x1"

# --- X2: the exposing directory's own subtree sees the exposed procedure ---
echo "--- X2: exposing dir subtree sees exposed ---"
x2=$("$REPL_RUN" <<HEREDOC
$EXPOSE/basic
use_exposed(R).
:quit
HEREDOC
2>&1)
check "X2 subtree sees exposed" "R = 100" "$x2"

# --- X3: shadowing — a local definition beats an exposed one ---
echo "--- X3: local definition shadows exposed ---"
x3=$("$REPL_RUN" <<HEREDOC
$EXPOSE/shadow
run(R).
:quit
HEREDOC
2>&1)
check "X3 loads" "Loaded program" "$x3"
check "X3 local twice (N+1) beats exposed (N*2)" "R = 11" "$x3"

# --- X4: collision — two exposed modules, same name/arity → error ---
echo "--- X4: collision error names both modules ---"
x4=$("$REPL_RUN" <<HEREDOC
$EXPOSE/collide
:quit
HEREDOC
2>&1)
check "X4 collision rejected" "collision" "$x4"
check_not "X4 not loaded" "Loaded program" "$x4"
check "X4 names module one" "\"one\"" "$x4"
check "X4 names module two" "\"two\"" "$x4"

# --- X5: exposed module lies outside the loaded subtree — still resolves ---
# basic/util/strutil.glp is a sibling of basic/leaf/ (outside leaf/'s subtree),
# yet X1's load resolved `twice`. Re-assert that resolution as X5.
echo "--- X5: exposed module outside the loaded subtree ---"
check "X5 strutil is outside leaf subtree" "outside" "$([ -e $EXPOSE/basic/leaf/strutil.glp ] && echo inside || echo outside)"
check "X5 resolves from outside subtree" "R = 42" "$x1"

# --- X6: a parameterised exposed utility instantiated at two types ---
echo "--- X6: parameterised exposed utility at two types ---"
x6=$("$REPL_RUN" <<HEREDOC
$EXPOSE/basic/leaf
two_inst(Zi, Zc).
:quit
HEREDOC
2>&1)
check "X6 integer instantiation" "Zi = \[1, 3, 2, 4\]" "$x6"
check "X6 constant instantiation" "Zc = \[\"a\", \"b\"\]" "$x6"

# --- X7: per-instantiation routing rejection (clause-template rule) ---
# A parameterised lib router (send_user) exposed from the root, called by a
# project whose entry union OMITS the destructured constructor (user_output),
# must be rejected at that instantiation — not merely under the wildcard
# self-check. Pins that exposed-parameterised resolution + per-instantiation
# defining-clause checking work together. See docs/glp-a5-stage-b-plan.md.
echo "--- X7: per-instantiation routing rejection ---"
x7=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/a5_routing_neg
:quit
HEREDOC
2>&1)
check_not "X7 bad-OutputEntry fixture not loaded" "Loaded program" "$x7"
check "X7 rejected at send_user instantiation" "send_user" "$x7"
check "X7 names the missing user_output constructor" "user_output" "$x7"

echo ""

# =============================================================================
# SECTION Y: GLP-ICLP PAPER PROGRAMS (techniques + metainterpreters)
# =============================================================================
# Runtime tests for the programs the GLP-ICLP-2026 paper presents
# (appendix-additional-techniques): each loads its canonical copy and runs
# the example goal from the file header.
echo "=== Section Y: GLP-ICLP Paper Programs ==="
echo ""

echo "--- Y1: techniques ---"
y1=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/lib/lookup/lookup.glp
lookup(b, [(a,1),(b,2),(c,3)], V).
:clear
$GLP_DIR/programs/lib/streams/tag_stream.glp
tag_stream(alice, [hi, bye], Tagged).
:clear
$BOOK/recursive/structure_processing/observe.glp
observe([m(1,[x]), m(2,[y])], Copy, Log).
:clear
$BOOK/streams/producers_consumers/cooperative_producers.glp
coop_play(S).
:clear
$BOOK/streams/objects_monitors/network_switch_3way.glp
switch_play(GotQ, GotR).
:clear
$BOOK/social_networks/interlaced_streams.glp
interlace_play(SA).
:clear
$BOOK/streams/producers_consumers/relay.glp
relay_play(Out).
:quit
HEREDOC
2>&1)

check "Y1 lookup finds value" "V = 2" "$y1"
check "Y1 tag_stream tags" "Tagged = \[msg(alice, hi), msg(alice, bye)\]" "$y1"
check "Y1 observe copies" "Copy = \[m(1, \[x\]), m(2, \[y\])\]" "$y1"
check "Y1 observe logs" "Log = \[m(1, \[x\]), m(2, \[y\])\]" "$y1"
check "Y1 cooperative producers hand over" "S = \[a, b, c, d, e, f\]" "$y1"
check "Y1 switch routes to q" "GotQ = \[hello\]" "$y1"
check "Y1 switch routes to r" "GotR = \[bye\]" "$y1"
check "Y1 interlace produces blocks" "SA = \[block(a1" "$y1"
check "Y1 relay bridges stream and channel" "Out = \[x, y\]" "$y1"

echo "--- Y2: metainterpreters ---"
y2=$("$REPL_RUN" <<HEREDOC
$BOOK/meta/plain/plain_meta.glp
run((merge([1,2,3],[4,5],Xs), merge([a,b],[c,d,e],Ys), merge(Xs?,Ys?,Zs))).
:clear
$BOOK/meta/plain/failsafe_meta.glp
run((merge([1],[],Xf), nogood), Fs).
:clear
$BOOK/meta/enhanced/control_meta.glp
run(merge([1,2],[3],Xc), [suspend,resume]).
:clear
$BOOK/meta/enhanced/termination_meta.glp
run((merge([1,2],[a],Xt), merge([],[],Yt)), done, R).
:clear
$BOOK/meta/enhanced/snapshot_meta.glp
run(merge([1,2],[3],Xa), [suspend,abort], [], Ra).
:clear
$BOOK/meta/enhanced/tracing_meta.glp
run(merge([1],[],Xr), T).
:clear
$BOOK/meta/debugging/runtime_control_meta.glp
run(merge([1,2],[3],Xd), [abort], [], Rd).
:quit
HEREDOC
2>&1)

check "Y2 plain meta merges" "Zs = \[1, 2, 3, 4, 5, a, b, c, d, e\]" "$y2"
check "Y2 failsafe meta reports failure" "Fs = \[failed(nogood)\]" "$y2"
check "Y2 control meta suspends and resumes" "Xc = \[1, 2, 3\]" "$y2"
check "Y2 termination meta detects termination" "R = done" "$y2"
check "Y2 snapshot meta dumps resolvent on abort" "Ra = \[merge(\[1, 2\], \[3\]" "$y2"
check "Y2 tracing meta builds trace" "T = t(1, " "$y2"
check "Y2 runtime-control meta dumps on abort" "Rd = \[merge(\[1, 2\], \[3\]" "$y2"

echo ""

# =============================================================================
# Section Z: Engine regression — high-arity clause dispatch
# =============================================================================
# A multi-clause committed-choice procedure of arity >= 11, whose selected clause
# builds a compound in its head output ([t(...)|Out?]), must commit. It used to
# fail (R left unbound) because the engine hard-coded 10 argument registers —
# operand slots >= 10 were read as temp/clause registers, so the 11th argument
# (the output at slot 10) was misread and the head never matched. Fixed by
# classifying an operand against the goal's actual argument set (runner.dart) and
# keeping temp registers above the arity (codegen.dart). The code-format spec
# makes argSlot an unbounded clen, so there is no 10-argument cap.
echo "=== Section Z: Engine regression — high-arity clause dispatch ==="

z_arity=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_arity_dispatch_bug.glp
hi(yes, 1, 2, 3, 4, 5, 6, 7, 8, 9, R).
:quit
HEREDOC
2>&1)
check "Z1 arity-11 head-constructing clause commits" "R = \[t(1, 2, 3, 4, 5)\]" "$z_arity"
check_not "Z2 arity-11 output not left unbound" "R = <unbound>" "$z_arity"

z_body=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_arity_dispatch_bug.glp
bp(5, R).
:quit
HEREDOC
2>&1)
check "Z3 arity>=11 body call places compound arg at slot 10" "R = r(5)" "$z_body"

z_trace=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_arity_dispatch_bug.glp
:trace
hi(yes, 1, 2, 3, 4, 5, 6, 7, 8, 9, R).
:quit
HEREDOC
2>&1)
check "Z4 trace prints all args of an arity-11 goal (11th not truncated)" "9, \[t(1, 2, 3, 4, 5)" "$z_trace"

echo ""

# =============================================================================
# Section ZG: Guard/type agreement (string/1, constant/1)
# =============================================================================
# The guards must admit exactly what the root self.glp types define:
#   Constant ::= Number ; String ; Module.
# [] is a constant that is neither a Number nor a Module, so it is a String —
# string([]) succeeds (it used to be excluded).  constant/1 admits a Number, a
# String (including []) and a Module term (it used to reject module terms).
# The Module alternative has no GLP-level coverage: nothing constructs a
# ModuleTerm today, so a module term cannot be produced from GLP source.
echo "=== Section ZG: Guard/type agreement ==="

zg1=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_guard_type_agreement.glp
isstr([], R).
:quit
HEREDOC
2>&1)
check "ZG1 string([]) succeeds — [] is a String" "R = yes" "$zg1"

zg2=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_guard_type_agreement.glp
isstr(abc, R).
:quit
HEREDOC
2>&1)
check "ZG2 string(abc) still succeeds" "R = yes" "$zg2"

zg3=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_guard_type_agreement.glp
iscon([], R).
:quit
HEREDOC
2>&1)
check "ZG3 constant([]) succeeds" "R = yes" "$zg3"

zg4=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/test_guard_type_agreement.glp
iscon(5, R).
:quit
HEREDOC
2>&1)
check "ZG4 constant(5) succeeds" "R = yes" "$zg4"

echo ""

# =============================================================================
# SECTION RM: Module-as-value kernels (self_module/1, run/2)
# =============================================================================
# GLP paper, appendix "Guards, Body Kernels, and System Predicates":
# self_module(M) binds M to the caller's own module value; run(Goal, M?)
# activates the module and posts Goal to it — Goal's predicate must be
# exported by the module, else an error.  The probe program's entry points
# (exports) are hello/1 and poke/1; secret/1 is compiled but not exported.
echo "=== Section RM: Module-as-value kernels (self_module/run) ==="

rm1=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), run(hello(Z), M?).
:quit
HEREDOC
2>&1)
check "RM1 probe program loads" "Loaded program" "$rm1"
check "RM1 self_module yields the module value" "M = Module(run_module_probe)" "$rm1"
check "RM1 run posts exported goal, binding flows back" "Z = \[done\]" "$rm1"

rm2=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), run(secret(Z), M?).
:quit
HEREDOC
2>&1)
check "RM2 run of non-exported predicate errs" "_run/2: secret/1 is not exported by module run_module_probe" "$rm2"

rm3=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), run(poke(Z), M?).
:quit
HEREDOC
2>&1)
check "RM3 exported entry may call non-exported internally" "Z = \[leaked\]" "$rm3"

echo ""

# =============================================================================
# Section Q: Dart unit tests (whole tree)
# =============================================================================
#
# Runs `dart test` over glp_runtime/test and `flutter test` over
# glp_multiagent/test, both in full.  Before this section existed the suite
# invoked `dart test` on exactly two named multiagent files, so "ALL TESTS
# PASSED!" said nothing about test/engine_v2/, test/compiler/, or anything else
# in the tree — four tests sat red there, unseen, for as long as anyone had been
# reading this suite as the gate.  Sections M and O are folded in here rather
# than run twice.
#
# glp_multiagent was added on 2026-08-02 and until then the section's title was
# false: it ran `dart test` in glp_runtime alone, so all fifteen files in
# glp_multiagent/test/ were outside the gate.  Seven of them were red from
# 2026-08-01 afternoon and nobody could see it, because every count anyone
# quoted — 1155, 1160, 1162, 1171 — was silent about them.  The two packages
# need different runners: glp_multiagent is a Flutter package, so `dart test`
# cannot run it at all.  This also matters for what the suite gates: the
# multi-isolate load path is covered by those fifteen files and by nothing in
# glp_runtime, so a change to it landed before this extension would have gone
# green here and proved nothing.
#
# The gate is the one in root claude.md: the green count plus the KNOWN RED set
# unchanged.  A test listed below may be red without failing the suite; any
# other red fails it.  A listed test that PASSES also fails the suite, so the
# list cannot rot into a permanent exception — it must be removed the moment its
# fix lands.

# Known-red tests.  One entry per line, matched against "<file>: <test name>".
# Each entry names the owning project and what blocks it.
# Empty: nothing is known-red.  An entry names a test another project owns and
# is expected to fail until they fix it; the guard below fails the suite when a
# listed test passes, so an entry is deleted in the same commit as its fix.
#
# glp_runtime entries carry the path `dart test` prints, relative to that
# package: `test/...`.  glp_multiagent entries carry the package name too —
# `glp_multiagent/test/...` — so the two trees cannot collide on a file of the
# same name.
#
# The seven below are vGLP's, all one cause, all red since d4afc77b on
# 2026-08-01: that commit added `imported procedure grassapp_agent#agent/7` to
# the play files so programs/book/grassapp would pass the per-module check as a
# directory program, and a source carrying an imported declaration is no longer
# a self-contained program, so the co-loaded path these seven still use is
# rejected.  vGLP claimed them at 2026-08-01 22:28 under the test-ownership
# ruling and is moving them onto a directory load; that needs config.programDir,
# which is IGLP's line and waits on send_to_net/1 reaching root self.glp.  vGLP
# named six of the seven — paper_screenshots_constructs was not on their list
# and fails on the same rejection of the same file.
KNOWN_RED=(
    "glp_multiagent/test/grassapp_scenario_test.dart: GrassApp scenario: four actors, chat replies, swap-then-redeem"
    "glp_multiagent/test/grassapp_unfriend_test.dart: charlie pays then unfriends Bob → unfriended(charlie) reaches Bob"
    "glp_multiagent/test/grassapp_loan_redeem_test.dart: GrassApp: loan then maturity-gated redemption on local clocks"
    "glp_multiagent/test/grassapp_escrow_test.dart: GrassApp: escrow time-release pays the beneficiary"
    "glp_multiagent/test/grassapp_village_test.dart: GrassApp village market: the seven operations of §8.2"
    "glp_multiagent/test/paper_screenshots_grassapp_test.dart: fig:grassapp — Friends, Currencies, Chats panels of one GrassApp"
    "glp_multiagent/test/paper_screenshots_constructs_test.dart: fig:constructs — card, form, and chat input in the running app"
)

echo "=== Section Q: Dart unit tests (whole tree) ==="
echo ""

# `dart test` exits non-zero whenever anything is red — including a known-red
# entry, which is not a suite failure — so every capture below is guarded
# against `set -e` (line 24) rather than letting it abort the run.
DART_TREE_RESULT=$(cd "$GLP_RUNTIME" && "$DART" test 2>&1) || true

# Strip ANSI colour and CR so the reporter's in-place updates become lines.
DART_TREE_CLEAN=$(printf '%s' "$DART_TREE_RESULT" | sed 's/\x1b\[[0-9;]*m//g' | tr '\r' '\n') || true

DART_TREE_PASSED=$(printf '%s' "$DART_TREE_CLEAN" | grep -oE '\+[0-9]+' | tail -1 | tr -d '+') || true
DART_TREE_PASSED=${DART_TREE_PASSED:-0}

# Failing tests, one "<file>: <test name>" per line.  grep exits 1 when the
# tree is fully green, which is the good case, so this is guarded too.
DART_TREE_FAILS=$(printf '%s' "$DART_TREE_CLEAN" \
    | grep -oE 'test/[A-Za-z0-9_/]+\.dart: .*\[E\]' \
    | sed 's/ \[E\]$//' \
    | sort -u) || true

# glp_multiagent, the other half of the tree.  It is a Flutter package, so it
# needs `flutter test`; `dart test` cannot run it.  A missing flutter is a hard
# failure and not a skip — silently omitting half the tree is the defect this
# extension exists to remove.
if ! command -v flutter >/dev/null 2>&1; then
    echo "  FAIL: flutter is not on PATH, so glp_multiagent/test/ cannot run"
    echo "        Half the Dart tree would go unmeasured; the suite reports no"
    echo "        result rather than a green one."
    FAIL=$((FAIL + 1))
    MA_PASSED=0
    MA_FAILS=""
else
    MA_RESULT=$(cd "$GLP_DIR/glp_multiagent" && flutter test 2>&1) || true
    MA_CLEAN=$(printf '%s' "$MA_RESULT" | sed 's/\x1b\[[0-9;]*m//g' | tr '\r' '\n') || true

    MA_PASSED=$(printf '%s' "$MA_CLEAN" | grep -oE '\+[0-9]+' | tail -1 | tr -d '+') || true
    MA_PASSED=${MA_PASSED:-0}

    # `flutter test` prints absolute paths where `dart test` prints package-
    # relative ones.  Normalise to `glp_multiagent/test/...`, the form KNOWN_RED
    # carries.
    MA_FAILS=$(printf '%s' "$MA_CLEAN" \
        | grep -oE '[A-Za-z0-9_/.-]+\.dart: .*\[E\]' \
        | sed 's/ \[E\]$//' \
        | sed 's|^.*/glp_multiagent/test/|glp_multiagent/test/|' \
        | sort -u) || true
fi

DART_TREE_PASSED=$((DART_TREE_PASSED + MA_PASSED))
if [ -n "$MA_FAILS" ]; then
    if [ -n "$DART_TREE_FAILS" ]; then
        DART_TREE_FAILS=$(printf '%s\n%s' "$DART_TREE_FAILS" "$MA_FAILS")
    else
        DART_TREE_FAILS="$MA_FAILS"
    fi
fi

Q_NEW_RED=0
Q_KNOWN_RED=0

if [ -n "$DART_TREE_FAILS" ]; then
    while IFS= read -r failure; do
        [ -z "$failure" ] && continue
        is_known=0
        for known in "${KNOWN_RED[@]}"; do
            if [ "$failure" = "$known" ]; then
                is_known=1
                break
            fi
        done
        if [ $is_known -eq 1 ]; then
            echo "  KNOWN RED (not a new failure): $failure"
            Q_KNOWN_RED=$((Q_KNOWN_RED + 1))
        else
            echo "  FAIL: $failure"
            Q_NEW_RED=$((Q_NEW_RED + 1))
        fi
    done <<< "$DART_TREE_FAILS"
fi

# Rot guard: a known-red entry that no longer fails must be removed from the
# list.  Leaving it would turn the list into a standing exception that silently
# covers a future regression in the same test.
Q_STALE=0
for known in "${KNOWN_RED[@]}"; do
    if ! printf '%s\n' "$DART_TREE_FAILS" | grep -qxF "$known"; then
        echo "  FAIL: known-red entry now PASSES — remove it from KNOWN_RED:"
        echo "        $known"
        Q_STALE=$((Q_STALE + 1))
    fi
done

PASS=$((PASS + DART_TREE_PASSED))
FAIL=$((FAIL + Q_NEW_RED + Q_STALE))

if [ $Q_NEW_RED -eq 0 ] && [ $Q_STALE -eq 0 ]; then
    if [ $Q_KNOWN_RED -gt 0 ]; then
        echo "  PASS: $DART_TREE_PASSED Dart tests passed ($Q_KNOWN_RED known red, see above)"
    else
        echo "  PASS: All $DART_TREE_PASSED Dart tests passed"
    fi
else
    echo "$DART_TREE_CLEAN" | tail -20
fi

echo ""

# =============================================================================
# SUMMARY
# =============================================================================
TOTAL=$((PASS + FAIL))

# Did the tree move under us?  If it did, this run's numbers describe no single
# tree and must not be read as a result — neither a green gate nor a red one.
END_HEAD=$(cd "$GLP_DIR" && git rev-parse HEAD 2>/dev/null || echo "no-git")
END_DIRTY=$(cd "$GLP_DIR" && git status --porcelain 2>/dev/null | sort | shasum | cut -d' ' -f1)
TREE_MOVED=0
if [ "$START_HEAD" != "$END_HEAD" ] || [ "$START_DIRTY" != "$END_DIRTY" ]; then
    TREE_MOVED=1
fi

echo "======================================"
echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
echo "======================================"

if [ $TREE_MOVED -eq 1 ]; then
    echo ""
    echo "RUN INVALID: the tree changed while the suite ran."
    if [ "$START_HEAD" != "$END_HEAD" ]; then
        echo "  HEAD at start: $START_HEAD"
        echo "  HEAD at end:   $END_HEAD"
    fi
    if [ "$START_DIRTY" != "$END_DIRTY" ]; then
        echo "  the working tree was edited during the run"
    fi
    echo "  The counts above describe no single tree and are not a result:"
    echo "  a pass is not a gate and a failure is not a defect.  Re-run when"
    echo "  the tree has settled."
    exit 2
fi

if [ $FAIL -eq 0 ]; then
    echo "ALL TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
