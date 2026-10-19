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
#   H - retired (old-generation CSSN modules); CSSN is Section K
#   I - self.glp Procedure Tests (shared procs, shadowing, local shadow, type error)
#   J - CSSG v2 Modules (child_agent with parent(X) output keys)
#   K - CSSN, the child-safe social network (programs/cssn), as ported on 2026-10-09
#   L - Dynamic Module Dispatch Tests (activate + M # goal)
#   M - Multi-Isolate (madGLP) Tests (dart test, one isolate per agent; run in Section Q)
#   N - Currencies Modules (project-directory loading, plays 1-12)
#   O - Currencies Multi-Isolate Tests (dart test, one isolate per agent)
#   P - Module Boundary Enforcement Tests (exported vs private procedures)
#   GF - Grassroots federation records, transactions and platform (GFWC)
#   APPS - The apps of the contracts (programs/apps), mini-apps compiled down the cascade: GSG's, GSN's and GSNP's
#
# Every section runs unless GLP_SECTIONS names a subset ("Section selection"
# below); test/run_fast.sh runs the fast tier.

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
# The agent cold-call round-trip fixture: the four multiagent Dart tests load
# it (single heap and two isolates). Moved out of book/ on 2026-08-02 and named
# for what it exercises, not for the platform it borrows.
AGENT_RT="$GLP_DIR/programs/tests/agent_roundtrip"
TC_DIR="$GLP_DIR/programs/tests/typechecker"
MODED="$GLP_DIR/programs/tests/moded_types"

# Section selection: the two tiers (Udi, 2026-10-14: a fast suite per merge on
# main, the full suite on main every 3-4 hours).  GLP_SECTIONS names the
# sections to run, space-separated, in any order; unset, empty or "all" runs
# every section, and only that run is the full suite.  "fast" is the fast tier,
# FAST_SECTIONS below, and test/run_fast.sh runs it; a merge adds the sections
# its branches touch, GLP_SECTIONS="fast VG N3".  "Q" is both halves of Section
# Q: QD, `dart test` over glp_runtime, and QF, the asset step and `flutter test`
# over glp_multiagent.  A name that is no section stops the run before it
# starts.  Sections run in the file's order whatever the order named.  A section
# not run calls none of its checks, so a KNOWN_RED_CHECKS entry of it is neither
# red nor a rot, and Section Q's rot guard reads only the KNOWN_RED entries of
# the half that ran.  Every section stands alone: the REPL build below is made
# for every run, and the asset step is QF's own.
ALL_SECTIONS="A B C D E G I K L M N2 N3 N4 VG O P J S X Y Z ZG RM SK MB SL MA RV SG APPS GF GSC CAS SGLP QD QF"
FAST_SECTIONS="A B C D E G I K L M N2 N3 N4 O P J S X Y Z ZG RM SK MB SL MA RV SG APPS GF GSC"
GLP_SECTIONS="${GLP_SECTIONS:-all}"
RUN_FAST=0
RUN_WANTED=" "
for s in $GLP_SECTIONS; do
    case "$s" in
        all)  RUN_WANTED+="$ALL_SECTIONS " ;;
        fast) RUN_WANTED+="$FAST_SECTIONS "; RUN_FAST=1 ;;
        Q)    RUN_WANTED+="QD QF " ;;
        *)    case " $ALL_SECTIONS " in
                  *" $s "*) RUN_WANTED+="$s " ;;
                  *) echo "GLP_SECTIONS: no section '$s'; the sections are: $ALL_SECTIONS (and fast, Q, all)"
                     exit 3 ;;
              esac ;;
    esac
done
RUN_SECTIONS=""
for s in $ALL_SECTIONS; do
    case "$RUN_WANTED" in *" $s "*) RUN_SECTIONS+="${RUN_SECTIONS:+ }$s" ;; esac
done
if [ -z "$RUN_SECTIONS" ]; then
    echo "GLP_SECTIONS=\"$GLP_SECTIONS\" names no section"
    exit 3
elif [ "$RUN_SECTIONS" = "$ALL_SECTIONS" ]; then
    RUN_TIER="FULL"
elif [ $RUN_FAST -eq 1 ]; then
    RUN_TIER="FAST"
else
    RUN_TIER="PARTIAL"
fi
_section() {
    case " $RUN_SECTIONS " in *" $1 "*) return 0 ;; esac
    return 1
}

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
if [ "$RUN_TIER" = "FULL" ]; then
    echo "Sections: all (the full suite)"
else
    echo "Sections ($RUN_TIER, GLP_SECTIONS=\"$GLP_SECTIONS\"): $RUN_SECTIONS"
fi
echo ""

PASS=0
FAIL=0

# Known-red REPL checks: the counterpart, for check/check_not, of Section Q's
# KNOWN_RED list of Dart tests.  An entry is a check's name exactly as it is
# passed to check or check_not (or to _check_pass and _check_fail, as Section
# Q's asset step does), with its owner in the comment above it.  A
# listed check that fails is reported KNOWN RED and does not fail the suite; a
# listed check that passes fails it (the rot guard), so the entry is deleted in
# the commit that turns it green.  Udi's rulings put a dormant project's
# failures here, since no session exists to repair them.
KNOWN_RED_CHECKS=(
# The super-app's hosting of CSSN's ported mini-app (CSSN's port of
# 2026-10-09): GSG's to port, as it hosts the currency's mini-app at its
# person channel; each entry comes off with that port.
    "SG two mini-apps at once: bob connected and greeted under it"
    "SG two mini-apps on two smartphones: and its message arrives"
    "SG super-app hosts the child-safe platform: alice connected to bob"
    "SG super-app hosts the child-safe platform: bob connected and greeted"
    "SG super-app hosts the child-safe platform: no failed play"
    "SG two mini-apps at once: alice connected under the child-safe platform"
    "SG two mini-apps on two smartphones: the child-safe friendship holds"
    "SG child-safe platform on two smartphones: alice is connected to bob"
    "SG child-safe platform on two smartphones: bob is connected to alice"
    "SG child-safe platform on two smartphones: alice's message reaches bob"
    "SG two mini-apps at once: no failed play"
)
KR_CHECKS=0

_known_red_check() {
    local name="$1" known
    for known in "${KNOWN_RED_CHECKS[@]}"; do
        [ "$name" = "$known" ] && return 0
    done
    return 1
}

_check_pass() {
    if _known_red_check "$1"; then
        echo "  FAIL: known-red check now PASSES — remove it from KNOWN_RED_CHECKS: $1"
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $1"
        PASS=$((PASS + 1))
    fi
}

_check_fail() {
    if _known_red_check "$1"; then
        echo "  KNOWN RED (not a new failure): $1 ($2)"
        KR_CHECKS=$((KR_CHECKS + 1))
    else
        echo "  FAIL: $1 ($2)"
        FAIL=$((FAIL + 1))
    fi
}

check() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        _check_pass "$name"
    else
        _check_fail "$name" "expected: $pattern"
    fi
}

check_not() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        _check_fail "$name" "should NOT match: $pattern"
    else
        _check_pass "$name"
    fi
}

if _section A; then
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

check "p(X) ill-typed: writer at input arg of p(String?)" "(p) is not well-typed" "$a1"
check "Merge [1,2,3]+[a,b]" "Xs = \[1, a, 2, b, 3\]" "$a1"
check "Clause lookup" "B = true" "$a1"
check "run(true)" "succeeds" "$a1"
check "Meta merge" "X = \[a, b, b\]" "$a1"
check "runA empty merge" "X2 = \[\]" "$a1"

# --- A2: Append, Reverse, Copy ---
echo "--- A2: Append, Reverse, Copy ---"
# Each program runs its goals and is cleared before the next loads: the three
# each define append/3 or reduce/2, and a later load of a name an earlier load
# defines is refused (GLP weeding round three, item 14).
a2=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/append.glp
append([a,b], [c,d], Zs).
append([], [x,y], Zs2).
append([a,b], [], Zs3).
:clear
$BOOK/recursive/list_processing/reverse.glp
reverse([a,b,c], Ys).
reverse([], Ys2).
reverse([x], Ys3).
:clear
$BOOK/recursive/list_processing/copy.glp
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
# Each program runs its goals and is cleared before the next loads, as in A2.
a9=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/list_processing/inner_product.glp
inner_product([1,2,3], [4,5,6], Sipf).
:clear
$BOOK/recursive/arithmetic_trees/fibonacci.glp
fib(0, Ff0).
fib(1, Ff1).
fib(3, Ff3).
fib(10, Ff10).
:clear
$BOOK/recursive/arithmetic_trees/factorial.glp
factorial(1, Fac1).
factorial(2, Fac2).
factorial(3, Fac3).
factorial(5, Fac5).
:clear
$BOOK/recursive/arithmetic_trees/hanoi.glp
hanoi(0, a, c, Mh0).
hanoi(1, a, c, Mh1).
hanoi(2, a, c, Mh2).
:clear
$BOOK/recursive/arithmetic_trees/primes.glp
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
check "Hanoi 0" "Mh0 = \[\]" "$a9"
check "Hanoi 1" "Mh1 = \[move(a, c)\]" "$a9"
check "Hanoi 2" "Mh2 = \[move(a, b), move(a, c), move(b, c)\]" "$a9"
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

# --- A11: Struct demo, depth, guards, misc ---
echo "--- A11: Structure and pattern tests ---"
a11=$("$REPL_RUN" <<HEREDOC
$TYPED/struct_demo.glp
$TYPED/depth_test.glp
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

# --- A12b: The guard real/1, beside integer/1's ---
# GLP-Spec appendix-guards.tex (12be29b), the guard table beside number:
# procedure real(Real?), Ground yes --- it succeeds if its argument is a Real,
# suspends on an unbound reader, fails otherwise (GLP #3 Cowork, 2026-10-02
# 22:19 UTC).  A Real is a floating-point number: 2.0 and the quotient 4 / 2
# are Reals and 2 is not, as integer(2.0) fails.  Its runtime cases are
# glp_runtime/test/engine/real_guard_test.dart's too.
echo "--- A12b: The guard real/1 ---"
a12b=$("$REPL_RUN" <<HEREDOC
$TYPED/real_guard.glp
real_or_other(2.5, Rr1).
real_or_other(2.0, Rr2).
real_or_other(2, Rr3).
real_or_other(hello, Rr4).
real_or_other("text", Rr5).
real_or_other(f(1.5), Rr6).
kind_of(3, Rk1).
kind_of(3.0, Rk2).
half(4, Rh1).
as_real(3, Ra1).
in_f(f(W1), Rw1).
real_or_other(Z1?, Rz1).
real_or_other(Z2?, Rz2), Z2 = 1.5.
twice(1.5, Rt1).
narrow(2.5, Rn1).
narrow(3, Rn2).
:quit
HEREDOC
2>&1)

check "real/1: a Real succeeds" "Rr1 = real" "$a12b"
check "real/1: 2.0 is a Real" "Rr2 = real" "$a12b"
check "real/1: 2, an Integer, fails" "Rr3 = other" "$a12b"
check "real/1: an atom fails" "Rr4 = other" "$a12b"
check "real/1: a string fails" "Rr5 = other" "$a12b"
check "real/1: a structure fails" "Rr6 = other" "$a12b"
check "real/1 beside integer/1: 3 is an Integer" "Rk1 = integer" "$a12b"
check "real/1 beside integer/1: 3.0 is a Real" "Rk2 = real" "$a12b"
check "real/1: the quotient 4 / 2 is a Real" "Rh1 = real" "$a12b"
check "real/1: '_real' gives a Real" "Ra1 = real" "$a12b"
check "real/1: an unbound writer fails" "Rw1 = no" "$a12b"
check "real/1: an unbound reader suspends" "suspended" "$(echo "$a12b" | grep -A1 'Rz1 = ')"
check "real/1: and resumes on a Real" "Rz2 = real" "$a12b"
check "real/1 grounds its argument: X? twice after it" "Rt1 = p(1.5, 1.5)" "$a12b"
check "real/1 narrows Number? to Real?" "Rn1 = ok" "$a12b"
check "real/1 narrows: an Integer takes otherwise" "Rn2 = no" "$a12b"

# --- A13: Ground equal, ground not-equal ---
# GLP-Spec appendix-guards.tex (bbff21d): "=?= succeeds if both arguments are
# ground and equal.  =?\= succeeds if no readers substitution makes them ground
# and equal."  Each suspends and fails by glp.tex, Guards: "A guard suspends if
# it does not succeed but some instance of it under a readers substitution
# would succeed.  A guard fails if no such instance exists."  test (both
# operands variables) takes the ground equality instruction; test_ab (a
# structure operand), test_neq and test_neq_stop take the generic guard call,
# the ground equality instruction having no negated operand (IGLP, 9b45225).
# So where the two clash, or an unbound writer stands in either, =?= fails and
# =?\= succeeds, whatever readers stand elsewhere --- f(a, Zq?) =?\= f(b, Wq?),
# f(Wq3) =?\= f(c), f(Xq4?) =?\= g(Yq4?), [a | Tq6?] =?\= [] --- and where a
# readers substitution makes them ground and equal but they are not both
# ground, each suspends: f(Xq7?) =?\= f(Yq7?) (GLP #3 Cowork, 2026-10-02 13:09
# UTC).
echo "--- A13: Ground equal and ground not-equal ---"
a13=$("$REPL_RUN" <<HEREDOC
$TYPED/test_ground_equal.glp
$TYPED/test_ground_not_equal.glp
test(a, a, R1).
test(a, b, R2).
test(foo(1,2), foo(1,2), R3).
test(foo(1,2), foo(1,3), R4).
test([1,2,3], [1,2,3], R5).
test([1,2], [1,3], R6).
test_neq(a, a, Rq1).
test_neq(a, b, Rq2).
test_neq(foo(1,2), foo(1,2), Rq3).
test_neq(foo(1,2), foo(1,3), Rq4).
test_neq([1,2,3], [1,2,3], Rq5).
test_neq([1,2], [1,3], Rq6).
test_neq_stop(go, Rq7).
test_neq_stop(stop, Rq8).
neq_only(f(a, Zq?), f(b, Wq?), Rq10).
neq_only(f(a, Zq2?), f(a, b), Rq11).
test(f(a, Ze1?), f(b, We1?), Re1).
test(f(a, Ze2?), f(a, b), Re2).
test_ab(f(a, b), Re3).
test_ab(f(Ze4?, c), Re4).
test_ab(f(Ze5?, b), Re5).
test(f(We6), f(c), Re6).
test_neq(f(Wq3), f(c), Rq12).
test(f(Xe7?), g(Ye7?), Re7).
test_neq(f(Xq4?), g(Yq4?), Rq13).
test([a|Te8?], [], Re8).
test_neq([a|Tq6?], [], Rq14).
neq_only(f(Xq7?), f(Yq7?), Rq15).
test_ab(f(a, We9), Re9).
:quit
HEREDOC
2>&1)

check "equal atoms" "R1 = equal" "$a13"
check "not equal atoms" "R2 = not_equal" "$a13"
check "equal structs" "R3 = equal" "$a13"
check "not equal structs" "R4 = not_equal" "$a13"
check "equal lists" "R5 = equal" "$a13"
check "not equal lists" "R6 = not_equal" "$a13"
check "=?\\= on equal atoms fails" "Rq1 = equal" "$a13"
check "=?\\= on differing atoms" "Rq2 = not_equal" "$a13"
check "=?\\= on equal structs fails" "Rq3 = equal" "$a13"
check "=?\\= on differing structs" "Rq4 = not_equal" "$a13"
check "=?\\= on equal lists fails" "Rq5 = equal" "$a13"
check "=?\\= on differing lists" "Rq6 = not_equal" "$a13"
check "=?\\= against a constant, differing" "Rq7 = go_on" "$a13"
check "=?\\= against a constant, equal, fails" "Rq8 = stopped" "$a13"
check "=?\\= succeeds at two constants that differ, readers beside them" "Rq10 = not_equal" "$a13"
check "=?\\= suspends where a readers substitution makes them ground and equal" "Rq11 = <unbound>" "$a13"
check "=?= fails at two constants that differ, readers beside them" "Re1 = not_equal" "$a13"
check "=?= suspends where a readers substitution makes them ground and equal" "Re2 = <unbound>" "$a13"
check "=?= against a structure, equal" "Re3 = equal" "$a13"
check "=?= against a structure fails at two constants that differ beside a reader" "Re4 = not_equal" "$a13"
check "=?= against a structure suspends on a nested reader" "Re5 = <unbound>" "$a13"
check "=?= fails on an unbound writer, which no readers substitution grounds" "Re6 = not_equal" "$a13"
check "=?\\= succeeds on an unbound writer" "Rq12 = not_equal" "$a13"
check "=?= fails at a clash of functor, neither side ground" "Re7 = not_equal" "$a13"
check "=?\\= succeeds at a clash of functor, neither side ground" "Rq13 = not_equal" "$a13"
check "=?= fails on a list cell with an unbound tail against the empty list" "Re8 = not_equal" "$a13"
check "=?\\= succeeds on a list cell with an unbound tail against the empty list" "Rq14 = not_equal" "$a13"
check "=?\\= suspends on two readers in structures that agree" "Rq15 = <unbound>" "$a13"
check "=?= against a structure fails on an unbound writer" "Re9 = not_equal" "$a13"

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
# A goal writer at the guard's `_` fails it (appendix-term-matching.tex, row
# "Writer X1", column "Writer X2"), so the channel holds readers or constants,
# ch(A?, B?) and ch(a, b), and not ch(A?, B) (GLP #3 Cowork, 2026-10-02 17:12
# UTC, 1(b)).
echo "--- A19: Defined guards ---"
a19=$("$REPL_RUN" <<HEREDOC
$TYPED/test_defined_guards.glp
test(ch(Adg?, Bdg?), Rdg1).
test(ch(a, b), Rdg4).
test(foo, Rdg2).
test(Xdg?, Rdg3).
:quit
HEREDOC
2>&1)

check "defined guard match" "Rdg1 = ok" "$a19"
check "defined guard match ground" "Rdg4 = ok" "$a19"
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

check "bounded buffer consumer-first runs to the cycle limit" "capped" "$a19bb_cons"
check_not "bounded buffer consumer-first does not fail" "→ failed" "$a19bb_cons"
check "bounded buffer producer-first runs to the cycle limit" "capped" "$a19bb_prod"
check_not "bounded buffer producer-first does not fail" "→ failed" "$a19bb_prod"

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
test_no_readers(f(_), Rnr8).
test_no_readers(foo(1, bar(_)), Rnr9).
:quit
HEREDOC
2>&1)

check "no_readers ground constant" "Rnr1 = ok" "$a20b"
check "no_readers ground compound" "Rnr2 = ok" "$a20b"
check "no_readers writer inside term" "Rnr3 = ok" "$a20b"
check "no_readers writer tail" "Rnr4 = ok" "$a20b"
check "no_readers reader suspends" "suspended" "$a20b"
# An anonymous writer inside a goal argument.  Until 2026-08-02 the REPL goal
# path had no UnderscoreTerm branch and threw "Unsupported struct argument
# type", while a NAMED writer in the same position (Rnr3 above) worked.
check "no_readers anonymous writer inside term" "Rnr8 = ok" "$a20b"
check "no_readers anonymous writer nested two deep" "Rnr9 = ok" "$a20b"

# --- A21: Comprehensive defined guards ---
# The goals that match a guard's `_` hold readers there: a goal writer against
# the head's `_` fails (GLP #3 Cowork, 2026-10-02 17:12 UTC, 1(b); see A19).
echo "--- A21: Comprehensive defined guards ---"
a21=$("$REPL_RUN" <<HEREDOC
$TYPED/test_defined_guards_all.glp
make_pair(Call1, Call2).
bind_response(yes, RespYes, LocalYes).
bind_response(no, RespNo, LocalNo).
test_channel(ch(TchA?, TchB?), TchR1).
test_channel(foo, TchR2).
test_channel(p(TpaA, TpaB), TchR3).
test_pair(p(TprA?, TprB?), TprR1).
test_pair(foo, TprR2).
test_wrapper(w(TwrX?), TwrR1).
test_wrapper(foo, TwrR2).
test_nested(w(p(TnA?, TnB?)), TnR1).
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
check "struct bind via =" "X = '_equator'(" "$a25"

# --- A26: Univ, assignment, MWM (stdlib, no file needed) ---
echo "--- A26: Univ, assignment, MWM ---"
a26=$("$REPL_RUN" <<HEREDOC
T1 =.. [foo].
T2 =.. [bar, x, y].
L1 ..= foo(a, b).
L2 ..= bar(1, 2, 3).
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
mwm([merge([1,2,3])], Xmwm2).
mwm([merge([a,b]), merge([1,2])], Xmwm3).
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
check "MWM empty" "Xmwm1 = \[\]" "$a26"
check "MWM single stream" "Xmwm2 = \[1, 2, 3\]" "$a26"
check "MWM two streams" "Xmwm3 = \[a, b, 1, 2\]" "$a26"

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

# The root self.glp exposes sglp#monitor, the root being the only common
# ancestor of programs/cssn and programs/sglp (modules.tex §-expose): a program
# outside programs/sglp calls monitor/4 by name, with the types it carries.
a26f_mon=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/expose_root_monitor/run.glp
clock(T).
released(K).
:quit
HEREDOC
2>&1)
check "root -expose(sglp#monitor): time request answered (T = 0)" "T = 0" "$a26f_mon"
check "root -expose(sglp#monitor): rated request released (K = go)" "K = go" "$a26f_mon"

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

# =.. waits for a list still being built (sGLP, 2026-10-03): the list is a
# reader (GLP-Spec appendix-guards, procedure =..(_, Stream(_)?)) and the
# '_list_to_tuple' kernel aborts on an incomplete one (appendix-guards, "Body
# kernel predicates"), so the root's =.. waits until the spine is assigned down
# to [] and the functor is assigned, and only then calls the kernel.  A list
# never completed leaves =.. suspended; nothing aborts.  The elements are not
# waited on.
a30w=$("$REPL_RUN" <<HEREDOC
$TYPED/univ_wait.glp
late_tail(Tw1).
late_spine(Tw2).
late_functor(Tw3).
Tw6 =.. [p, Yw6?].
never_tail(Gw4?, Tw4).
never_functor(Gw5?, Tw5).
:quit
HEREDOC
2>&1)
check "=.. waits: tail assigned after the call" "Tw1 = f(a, b)" "$a30w"
check "=.. waits: spine built after the call" "Tw2 = g(a, b, c, d)" "$a30w"
check "=.. waits: functor assigned after the call" "Tw3 = h(a)" "$a30w"
check "=.. does not wait on an element" "Tw6 = p(" "$a30w"
check "=.. waits: tail never assigned leaves it suspended" "Tw4 = <unbound>" "$a30w"
check "=.. waits: functor never assigned leaves it suspended" "Tw5 = <unbound>" "$a30w"
check "=.. waits: suspended, not failed" "→ suspended" "$a30w"
check_not "=.. waits: no abort" "ABORT" "$a30w"
check_not "=.. waits: no failure" "→ failed" "$a30w"

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

# --- A34: Ackermann on Peano numerals (book/recursive/arithmetic_trees) ---
# Clause 3's guard was compound(M?), ground(M?): compound refused M = 0, so
# ackermann(s(0), s(0), A) failed, and every goal that reaches it with it.
# ground(M?) alone licenses the second read of M? (GLP-Spec glp.tex, Remark
# "Guards and SRSW": compound does not imply groundness, ground does).
echo "--- A34: Ackermann ---"
a34=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/arithmetic_trees/ackermann.glp
ackermann(s(0), s(0), A11).
ackermann(s(s(0)), s(0), A21).
:quit
HEREDOC
2>&1)
check "Ackermann A(1,1) = 3" "A11 = s(s(s(0)))" "$a34"
check "Ackermann A(2,1) = 5" "A21 = s(s(s(s(s(0)))))" "$a34"

# --- A35: Minimum on Peano numerals (book/recursive/arithmetic_trees) ---
# The program committed to min_check's first clause and then failed in its body,
# the lesseq test sitting in the body where Concurrent Prolog had it in a deep
# guard GLP has not; minimum(s(s(0)), s(0), M) failed.  Now the flat standard
# form, which decides by the head alone.
echo "--- A35: Minimum ---"
a35=$("$REPL_RUN" <<HEREDOC
$BOOK/recursive/arithmetic_trees/min.glp
minimum(s(s(0)), s(0), M21).
minimum(0, s(0), M01).
:quit
HEREDOC
2>&1)
check "Minimum of 2 and 1 is 1" "M21 = s(0)" "$a35"
check "Minimum of 0 and 1 is 0" "M01 = 0" "$a35"

# --- A36: An operator name where a term is expected is a name ---
# GLP-Spec reserves no word (appendix-lp.tex, Definition "Logic Programs
# Syntax"): the reader takes an operator name or keyword in a term as the
# constant of that name, and as a functor before "(", as Prolog does (GLP #3
# Cowork, 2026-10-03 21:18 UTC).  Until 2026-10-03 an unquoted mod in a term
# was "Expected term, got TokenType.MOD" and procedure was read as the keyword.
# The display shows each name as a constant in single quotes, its unquoted text
# being an operator or a keyword, and a functor bare before "(" (GLP #3 Cowork,
# 2026-10-04 09:06 UTC, "23:49. Q1 and Q3"; A40).
echo "--- A36: Operator names as constants ---"
a36=$("$REPL_RUN" <<HEREDOC
$TYPED/operator_names.glp
names(N36).
words(W36).
functors(F36).
op(mod, R36a).
op(procedure, R36b).
op(+, R36c).
arith(A36, B36, C36).
even(4, E36).
X36 = mod.
Y36 = procedure.
:quit
HEREDOC
2>&1)
check "operator names: the fixture loads" "Loaded: .*operator_names.glp" "$a36"
check "operator names: every name a constant" 'N36 = \['\''mod'\'', '\''procedure'\'', '\''+'\'', '\''-'\'', '\''\*'\'', '\''/'\'', '\''//'\'', '\''<'\'', '\''>'\'', '\''=<'\'', '\''>='\'', '\''='\'', '\''=:='\'', '\''=\\\\='\'', '\''=?='\'', '\''=?\\\\='\'', '\''@<'\'', '\''=\.\.'\'', '\''\.\.='\'', '\'':-'\'', '\'':='\'', '\''::='\'', '\'';'\'', '\'':'\'', '\''~'\'', '\''#'\'', '\''\\\\'\'', '\''@'\''\]' "$a36"
check "operator names: mod and procedure in a structure" "W36 = w('mod', 'procedure', " "$a36"
check "operator names: procedure and = as functors" "F36 = f(procedure(a), =(b, c), mod(d, e))" "$a36"
check "operator names: mod a type alternative" "R36a = is_mod" "$a36"
check "operator names: procedure a type alternative" "R36b = is_procedure" "$a36"
check "operator names: + a type alternative" "R36c = is_plus" "$a36"
check "operator names: mod still infix" "A36 = 1" "$a36"
check "operator names: unary minus still neg" "B36 = -3" "$a36"
check "operator names: + and * by precedence" "C36 = 14" "$a36"
check "operator names: mod in a guard expression" "E36 = even" "$a36"
check "operator names: X = mod at the prompt" "X36 = 'mod'" "$a36"
check "operator names: Y = procedure at the prompt" "Y36 = 'procedure'" "$a36"
check_not "operator names: no syntax error" "Expected term" "$a36"

# --- A37: The answer display shows a list's tail ---
# A list whose tail is not [] is shown with the tail after "|", as send_to_user
# shows it.  Until 2026-10-03 the display dropped a constant or structure tail,
# [holding('G','H')|'W'] shown as [holding(G, H)] (GLP #3 Cowork, 2026-10-03
# 21:18 UTC: "a fault, fix it").  Since 2026-10-04 the display quotes the
# constants that need it, and send_to_user's output to the person stays as it
# is (GLP #3 Cowork, 2026-10-04 09:06 UTC, "23:49. Q1 and Q3").
echo "--- A37: List tails in the answer display ---"
a37=$("$REPL_RUN" <<HEREDOC
$TYPED/list_tail_display.glp
holding_tail(T37a).
two_and_constant(T37b).
structure_tail(T37c).
nested(T37d).
proper(T37e).
shown.
$TYPED/operator_names.glp
words(W37).
:quit
HEREDOC
2>&1)
check "list tail display: a constant tail" "T37a = \[holding('G', 'H') | 'W'\]" "$a37"
check "list tail display: two elements and a constant tail" "T37b = \[a, b | c\]" "$a37"
check "list tail display: a structure tail" "T37c = \[a | f(b)\]" "$a37"
check "list tail display: tails inside a structure and a list" "T37d = w(\[x | y\], \[\[p | q\] | r\])" "$a37"
check "list tail display: a proper list as before" "T37e = \[a, b, c\]" "$a37"
check "list tail display: send_to_user shows it unquoted, as it stays" "^GLP> \[holding(G, H) | W\]" "$a37"
check "list tail display: mod and procedure, the tail kept" "W37 = w('mod', 'procedure', \['mod' | 'procedure'\])" "$a37"

# --- A38: An undefined type name in a declaration is refused, named ---
# TGLP parameterized-types.tex, "Declaration parameters": "An undefined type
# name occurring in a declaration and not in its parameter list is an error".
# Until 2026-10-03 a declaration naming no parameters read such a name as a
# parameter; the fallback went once sGLP's Peers was defined (GLP #3 Cowork,
# 2026-10-03 21:18 UTC).  The refusal names the declaration, the file, the
# line and the type name, on the single-file path and in a directory program.
echo "--- A38: Undefined type names in declarations ---"
a38=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/decl_undefined_neg.glp
$GLP_DIR/programs/tests/decl_undefined_dir_neg
:quit
HEREDOC
2>&1)
check "undefined declaration type: single file refused, file and line named" "decl_undefined_neg.glp, line 16, column 16: undefined type \"M\" in the declaration of keep/2" "$a38"
check "undefined declaration type: directory program refused, module file and line named" "decl_undefined_dir_neg/worker.glp:4: undefined type \"Mesage\" in the declaration of relay/2" "$a38"
check_not "undefined declaration type: neither loads" "✓ Loaded" "$a38"

# --- A39: The standard library's channel operations declare what they define ---
# programs/lib/channels/channel_ops.glp defines send/3, receive/3 and
# new_channel/2, and a procedure a module defines is declared in it (TGLP
# modules.tex, Definition "Typed Procedure, Module"); it declares them as the
# root self.glp does (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:01. 3").  Until
# 2026-10-03 it declared none, and was refused once the root's declaration no
# longer stood for a module's own procedure (49b869f2, 80fcc1cb).
echo "--- A39: lib/channels/channel_ops.glp ---"
a39=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/lib/channels/channel_ops.glp
new_channel(C39a, C39b).
:quit
HEREDOC
2>&1)
check "channel_ops: loads" "Loaded: .*channel_ops.glp" "$a39"
check "channel_ops: new_channel pairs two channels" "C39a = ch(" "$a39"
check_not "channel_ops: no undeclared procedure" "declared only in an enclosing scope" "$a39"

# --- A40: The single-file path checks the object it compiles ---
# TGLP modules.tex, Compilation: "The flat program is the linked program of
# def:program, and it is the object checked."  A file loaded alone is linked
# as a one-module program, and its linked program is checked as a directory's
# is, each module of it first.  Until 2026-10-03 the single-file path checked
# the file's module alone and compiled the linked program unchecked (GLP #3
# Cowork, 2026-10-03 21:18 UTC, "16:01. 4"): a5_routing_neg/main.glp, whose
# call instantiates send_user/3 at an entry union with no user_output, loaded
# as a file while its directory was refused (X7).
echo "--- A40: Single-file loads check the linked program ---"
a40=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/a5_routing_neg/main.glp
$GLP_DIR/programs/tests/module_self_type_error/worker.glp
:quit
HEREDOC
2>&1)
check "single file: the linked program is checked" "a5_routing_neg/main.glp: Exception: Type checking failed for linked program" "$a40"
check "single file: refused at send_user's instantiation" "No transition for user_output" "$a40"
check "single file: its directory's self.glp checked as a module of it" "module_self_type_error/self.glp:3: Head of bad_proc is not well-typed" "$a40"
check_not "single file: neither loads" "✓ Loaded" "$a40"

# --- A41: The answer display reads back as the term ---
# GLP-Spec appendix-lp.tex, Definition "Logic Programs Syntax": the text
# denotes the term.  The display shows a constant in single quotes where
# unquoted it would read as a variable, an operator or a number, or as no one
# name, escaped as the reader reads a quoted name, and a functor bare before
# "(" where the reader takes it there; the shown text, typed back in a goal,
# is the term the guard compares it with (GLP #3 Cowork, 2026-10-04 09:06 UTC,
# "23:49. Q1 and Q3").  Until 2026-10-04 'G' was shown G, a variable.  A list
# with a constant tail is shown, not typed back; until 5a2b463c a goal's list
# tail that is no list and no variable was posted as [] (glp_engine.dart,
# _buildListTerm), and it is now built as written.  A name holding a quote is shown and not
# typed back: in a here-document inside $(...) bash 3.2 pairs single quotes
# regardless of the backslash; GlpPrinter's Dart test reads it back.  "str" is
# the string str, shown str: a quoted name and a double-quoted string are two
# spellings of one string, and the double-quoted form is read and never
# printed (GLP #3 Cowork, 2026-10-04 15:19 UTC, FAULT 2 (b)); until 2026-10-10
# it was shown "str".
echo "--- A41: The answer display reads back ---"
a41=$("$REPL_RUN" <<HEREDOC
$TYPED/display_read_back.glp
names(N41).
same_names(['G', '+', 'mod', '42', 'a b', '_x', 'procedure', [], "str"], R41).
same_names(['G'], Q41).
terms(T41).
same_terms(w('G'(a), f('=..', 'X'), +(1), =?=(a, b)), S41).
apostrophe(P41).
tails(L41).
:quit
HEREDOC
2>&1)
check "display: the fixture loads" "Loaded: .*display_read_back.glp" "$a41"
check "display: names quoted where they must be" "N41 = \['G', '+', 'mod', '42', 'a b', '_x', 'procedure', \[\], str\]" "$a41"
check "display: the shown names read back as the term" "R41 = yes" "$a41"
check "display: the comparison can fail" "Q41 = no" "$a41"
check "display: functors quoted where they must be" "T41 = w('G'(a), f('=\.\.', 'X'), +(1), =?=(a, b))" "$a41"
check "display: the shown structures read back as the term" "S41 = yes" "$a41"
check "display: a quote in a name escaped as the reader reads it" "P41 = 'it\\\\'s'" "$a41"
check "display: constant list tails shown" "L41 = \[\[a | b\], \[a, 'B' | 'C'\]\]" "$a41"

SECTION_A_PASS=$PASS
SECTION_A_FAIL=$FAIL

echo ""
echo "Section A: $SECTION_A_PASS passed, $SECTION_A_FAIL failed"
echo ""

fi  # Section A

if _section B; then
# =============================================================================
# SECTION B: TYPE-CHECK-ONLY POSITIVE TESTS
# (Load each file, verify "Loaded:" message, use :clear between files)
# =============================================================================
echo "=== Section B: Positive Type Check Tests ==="
echo ""

POSITIVE_FILES=(
    # --- book ---
    # Rejected until GLP's book task of 2026-10-12 21:18 UTC re-declared them:
    # cooperative.glp over Stream(Item), merge_dynamic.glp over Msg.
    "$BOOK/streams/producers_consumers/cooperative.glp"
    "$BOOK/streams/producers_consumers/merge_dynamic.glp"
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
    "$TC_DIR/positive/subtyping/head_pair_integer_to_constant.glp"
    "$TC_DIR/positive/subtyping/constants_fewer_alternatives.glp"
    "$TC_DIR/positive/subtyping/contravariant_response_slot.glp"
    "$TC_DIR/positive/subtyping/direct_constant_subtype.glp"
    "$TC_DIR/positive/subtyping/struct_fewer_functors.glp"
    # Subtyping through the root self.glp unions Number ::= Integer ; Real. and
    # Constant ::= Number ; String ; Module.  Negative twins:
    # negative/subtyping/{constant_to_integer_reverse,integer_to_string_disjoint,
    # head_pair_constant_to_integer,compound_to_constant}.glp.
    "$TC_DIR/positive/subtyping/constant_accepts_all_literals.glp"
    "$TC_DIR/positive/subtyping/integer_below_number_and_constant.glp"
    "$TC_DIR/positive/subtyping/int_list_to_constant_list.glp"
    "$TC_DIR/positive/subtyping/constant_union_to_constant.glp"

    # --- parameterized types ---
    "$TYPED/param_stream_integer.glp"
    "$TYPED/param_channel.glp"
    "$TYPED/param_procedure_inference.glp"

    # --- Abstract-parameter routing matrix (sec:abstract-parameters) ---
    # Case (i): a clean parametric proc (no parameter inspection) that COVERS the
    # whole input takes the abstract route and is certified clean against its
    # abstract instance, even though it is never instantiated.
    "$GLP_DIR/programs/tests/param_abstract_covered.glp"

    # --- Declaration parameters (parameterized-types.tex, "Declaration parameters") ---
    # A declaration names its type parameters in a list after the keyword; each
    # may occur bare or within a template instantiation. Negative counterpart:
    # param_decl_typo_neg.glp in NEGATIVE_FILES.
    "$GLP_DIR/programs/tests/param_decl_list.glp"

    # --- SRSW relaxations (glp.tex Remark "Guards and SRSW") ---
    # All must be ACCEPTED; their negative counterparts are srsw/known_not_ground
    # and srsw/writer_twice_no_ground in Section D. A guard occurrence counts
    # toward SRSW, so X? in a guard plus once in the body is legal; and a
    # groundness-implying guard licenses multiple occurrences of both halves,
    # the reader X? (ground_guard_multi_ok) and the writer X, in the head as in
    # the body (ground_writer_multi_ok; GLP-Spec 831f315).
    "$GLP_DIR/programs/tests/srsw/guard_occurrence_ok.glp"
    "$GLP_DIR/programs/tests/srsw/ground_guard_multi_ok.glp"
    "$GLP_DIR/programs/tests/srsw/ground_writer_multi_ok.glp"

    # Moved here from NEGATIVE_FILES on 2026-08-03, the day the fixture was
    # swept. It was rejected standalone because agent/4 and inject_msg/5 were
    # declared over a bare type parameter nothing instantiated; agent/4 is now
    # declared at ActorOut and NetInStream, its two streams separated, and the
    # outputs list carries one constructor per kind, so the file type-checks on
    # its own. Its sibling channel.glp still awaits the same sweep.
    "$AGENT_RT/typed_social_agent.glp"
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

fi  # Section B

if _section C; then
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

    # An untagged value at a tagged-union position, declared concretely: the
    # checker rejects it here, and the point of the test is the contrast with
    # programs/tests/param_unchecked/, where the same clause under a
    # parameterised declaration that nothing instantiates is checked by nothing.
    "$TYPED/tagged_union_untagged_neg.glp"

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
    "$TC_DIR/negative/subtyping/head_pair_constant_to_integer.glp"

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

    # --- Declaration parameters: a misspelt type name is not a parameter ---
    # procedure(X) pass(Strem?, Strem) — X is named, Strem is neither defined nor
    # named, so the declaration is rejected instead of reading Strem as a second
    # parameter (parameterized-types.tex, "Declaration parameters"). Positive
    # counterpart: param_decl_list.glp in POSITIVE_FILES.
    "$GLP_DIR/programs/tests/param_decl_typo_neg.glp"
    # procedure keep(M?, M) — no list, so M is an undefined type, not a
    # parameter (the fallback that read it as one went 2026-10-03, GLP #3
    # Cowork 21:18 UTC).  Positive counterpart: toplevel_type_param.glp.
    "$GLP_DIR/programs/tests/decl_undefined_neg.glp"

    # --- Monomorphic recursion: a recursive call at a different instantiation ---
    # wrap threads Stream(X) -> Stream(Box(X)); the recursive ploop is checked at
    # the enclosing instantiation, so the type-changing recursion is rejected.
    "$GLP_DIR/programs/tests/monomorphic_recursion.glp"

    # --- Abstract-parameter routing matrix, case (ii): a parametric proc (no
    # parameter inspection) with a COVERAGE GAP fails its abstract instance
    # pdrop(Stream<$abstract_X>?) — [] is uncovered — so it is not parametrically
    # well-typed, and no call instantiates it, so it is rejected
    # (sec:abstract-parameters). Filename retained for continuity; see the file
    # header.
    "$GLP_DIR/programs/tests/param_free_not_checked.glp"

    # --- Abstract-parameter routing: same pdrop gap, also instantiated by go/1.
    # Failing its abstract instance, pdrop is checked per instantiation, and at
    # X := Msg the gap is the same; the instantiation does not mask it.
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
    # TGLP parameterized-types.tex "Multiple parameters": X cannot be both
    # Integer and String.  The call fixes X = Integer and names
    # Channel<Integer,Integer>, a type no declaration names; the calling clause
    # is checked by that expansion (def:instantiation), which until 2026-09-27
    # it was not, and the file loaded.
    "$GLP_DIR/programs/tests/param_conflict_neg.glp"

    # Mis-declared with a bare type parameter where a concrete type belongs: a
    # constant/functor sits at the parameter position, so they inspect the
    # parameter, take the per-instantiation route, and loaded standalone with no
    # instantiation have nothing to certify — rejected (sec:abstract-parameters).
    #
    # channel.glp has now had the sweep too (2026-08-03): send/3, receive/3 and
    # new_channel/3 carry `procedure(X, Y)` parameter lists. It is STILL rejected
    # standalone, and that is the point of keeping it here — the rejection is now
    # the abstract-parameter rule doing its job rather than a missing list.
    # send/3 and receive/3 destructure the parameter's stream (`ch(In, [X?|Out?])`),
    # so they inspect it, take the per-instantiation route, and standalone have
    # no instantiation to be checked at (sec:abstract-parameters). Its sibling
    # typed_social_agent.glp was swept the same day — per-kind OutputEntry, agent/4
    # declared at ActorOut and NetInStream — and now type-checks standalone, so it
    # moved to POSITIVE_FILES. That sweep did NOT unskip ui_mediator_test, which
    # this comment used to predict; that test was removed on 2026-08-03 (Udi's
    # instruction) rather than repaired, and known-issues Issue 20 says why.
    "$AGENT_RT/channel.glp"

    # The recv2x2 probe directory reduced to one file, bodies and declarations
    # unchanged: boot_b_outfed and boot_d_outfed hand-build ch(S?, OutW?), a
    # reader at the Out position, and each is rejected with "Variable mode
    # mismatch: reader requires ↓ (consume), got ↑ (produce)" on the path
    # (ch/2, 0, input) → (OutW?, 2, output).
    "$GLP_DIR/programs/tests/recv2x2_neg.glp"
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

# NEG: one negative per check and load path (Integration 2026-10-11 15:53 UTC,
# item 5), so that no check passes by never running.  Types on the
# multi-isolate path with a program directory: ":boot <f>_boot.glp" loads <f>/
# in every agent's isolate (IsolateManager with programDir), and the fault is
# in a module of that directory, not in the boot source --- TGLP
# well-typing.tex, Definition "Well-Typed Clause", condition 2
# (programs/tests/neg_boot_programdir/types/self.glp).
NEG_BOOT="$GLP_DIR/programs/tests/neg_boot_programdir"
neg_boot_types=$("$REPL_RUN" <<HEREDOC
:boot $NEG_BOOT/types_boot.glp
:quit
HEREDOC
2>&1)
check "NEG types / :boot with a program directory: the agent's load is refused by the type check" "Agent alice failed to initialize: Exception: Type checking failed for module(s) of the program:" "$neg_boot_types"
check "NEG types / :boot with a program directory: the refusal names the module and the atom" "types/agents.glp:6: Body atom 0 (takes_integer) is not well-typed" "$neg_boot_types"
check_not "NEG types / :boot with a program directory: the boot does not settle" "Boot settled" "$neg_boot_types"

fi  # Section C

if _section D; then
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
    # Nor does =?\=, Ground "no" (GLP-Spec appendix-guards.tex, bbff21d): A13's
    # neq_pair, which read each argument twice after it, until 2026-10-02.
    "$GLP_DIR/programs/tests/srsw/neq_not_ground.glp"
    # And without a groundness-implying guard a writer occurs once: "if the
    # success of a guard implies that X? is bound to a ground term, then both X
    # and X? may occur multiple times in the clause" (glp.tex Remark "Guards
    # and SRSW", 831f315) --- here under known/1, and with no guard.
    "$GLP_DIR/programs/tests/srsw/writer_twice_no_ground.glp"
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

# =?\= grounds nothing (GLP-Spec appendix-guards.tex, bbff21d): a reader twice
# after it is refused, and the diagnostic names it.
srsw_neq=$(echo -e "$GLP_DIR/programs/tests/srsw/neq_not_ground.glp\n:quit" | "$REPL_RUN" 2>&1)
check "=?\\= grounds nothing: a reader twice after it is refused" "Reader variable \"X?\" occurs 2 times" "$srsw_neq"

# A groundness-implying guard licenses a repeated writer, in the head as in
# the body (glp.tex Remark "Guards and SRSW", 831f315): without one it is
# refused, and the diagnostic says so; with one the clause loads
# (ground_writer_multi_ok, Section B) and its goal reduces only where the
# occurrences meet one term, the writer mgu of the goal and the head
# (Definitions "Writer MGU", "GLP Goal/Clause Reduction").
srsw_wtwice=$(echo -e "$GLP_DIR/programs/tests/srsw/writer_twice_no_ground.glp\n:quit" | "$REPL_RUN" 2>&1)
check "a writer twice without a groundness-implying guard is refused" "Writer variable \"X\" occurs 2 times without a groundness-implying guard" "$srsw_wtwice"
check "a writer twice in the head without a guard is refused" "Writer variable \"R\" occurs 2 times in the head of the clause two(R, R) without a groundness-implying guard" "$srsw_wtwice"
srsw_wmulti=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/srsw/ground_writer_multi_ok.glp
w(3, Yw1).
same_ground(3, 3, Sg1).
same_ground(3, 4, Sg2).
same_ground(f(1), f(1), Sg3).
same_ground(f(1), f(2), Sg4).
:quit
HEREDOC
2>&1)
check "a writer in the head and the body under integer/1 runs" "Yw1 = s(3)" "$srsw_wmulti"
check "a writer twice in the head: one term reduces" "Sg1 = yes" "$srsw_wmulti"
check "a writer twice in the head: two terms do not" "Sg2 = no" "$srsw_wmulti"
check "a writer twice in the head: one structure reduces" "Sg3 = yes" "$srsw_wmulti"
check "a writer twice in the head: two structures do not" "Sg4 = no" "$srsw_wmulti"

# merge_with_reader: the one entry of this section that is rejected by the SRSW
# pass rather than by the type checker, so it is the only test that speaks for
# the "SRSW-violation tests" of the paper's validation sentence. It was gated on
# `[ -f ... ]` and lived under programs/tests/archive/repl, a directory with an
# open instruction to delete it: deleting the file made the suite run one test
# fewer and fail nothing. Moved to programs/tests/srsw/ and wired unconditionally
# on 2026-08-02 — if the file goes missing now, this goes red. That directory
# was deleted later the same day, this move being what made it safe to.
SRSW_MWR="$GLP_DIR/programs/tests/srsw/merge_with_reader.glp"
srsw_mwr_out=$(echo -e "$SRSW_MWR\n:quit" | "$REPL_RUN" 2>&1)
if echo "$srsw_mwr_out" | grep -qi "SRSW violation"; then
    echo "  PASS: merge_with_reader (SRSW rejected)"
    PASS=$((PASS + 1))
else
    echo "  FAIL: merge_with_reader (should be rejected)"
    FAIL=$((FAIL + 1))
fi

# NEG: one negative per check and load path (Integration 2026-10-11 15:53 UTC,
# item 5).  SRSW on the multi-isolate path: an agent program with Id? twice in
# its body (GLP-Spec glp.tex, Definitions "Single-Occurrence (SO) Invariant"
# and "GLP Program, Goals"), booted with no program directory --- the agent
# program follows the boot clause (programs/tests/neg_srsw_boot/) --- and as a
# module of the program directory the agent loads
# (programs/tests/neg_boot_programdir/srsw/, IsolateManager with programDir).
# The AgentRuntime path is glp_runtime/test/multiagent/neg_check_paths_test.dart.
neg_srsw_inline=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/neg_srsw_boot/agent_boot.glp
:quit
HEREDOC
2>&1)
check "NEG SRSW / :boot, no program directory: the agent's load is refused by the SRSW pass" "Agent alice failed to initialize: \[semantic\] SRSW violations found:" "$neg_srsw_inline"
check "NEG SRSW / :boot, no program directory: the refusal names the reader occurring twice" "agent_init/2: Line 18: Reader variable \"Id?\" occurs 2 times" "$neg_srsw_inline"
check_not "NEG SRSW / :boot, no program directory: the boot does not settle" "Boot settled" "$neg_srsw_inline"
neg_srsw_dir=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/neg_boot_programdir/srsw_boot.glp
:quit
HEREDOC
2>&1)
check "NEG SRSW / :boot with a program directory: the agent's load is refused by the SRSW pass" "Agent alice failed to initialize: \[semantic\] SRSW violations found:" "$neg_srsw_dir"
check "NEG SRSW / :boot with a program directory: the refusal names the module's reader occurring twice" "neg_boot_programdir/srsw/agents:agent_init/2: Line 5: Reader variable \"Id?\" occurs 2 times" "$neg_srsw_dir"
check_not "NEG SRSW / :boot with a program directory: the boot does not settle" "Boot settled" "$neg_srsw_dir"

# NEG: SRSW in a module of the program's tree that no entry point reaches
# (programs/tests/neg_srsw_unreached/): TGLP modules.tex, Compilation, second
# step, "each module is checked against its scope---for SRSW, its guards and
# its types---exactly as a single-module program is, whether or not an entry
# point reaches it" (GLP, 2026-10-13 08:02 UTC, item 2).  Until 2026-10-13
# SRSW ran on the linked program alone, after step 5 had dropped the module,
# and the program loaded.
neg_srsw_unreached=$(echo -e "$GLP_DIR/programs/tests/neg_srsw_unreached\n:quit" | "$REPL_RUN" 2>&1)
check "NEG SRSW / unreached module: the program is refused by the SRSW pass" "Error loading project $GLP_DIR/programs/tests/neg_srsw_unreached: \[semantic\] SRSW violations found:" "$neg_srsw_unreached"
check "NEG SRSW / unreached module: the refusal names the unreached module's reader occurring twice" "tests/neg_srsw_unreached/unreached:twice/2: Line 8: Reader variable \"X?\" occurs 2 times" "$neg_srsw_unreached"
check_not "NEG SRSW / unreached module: the program does not load" "Loaded program" "$neg_srsw_unreached"

echo ""

fi  # Section D

if _section E; then
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
    # false and fail are defined nowhere, as a guard or in a body: each is an
    # unknown guard predicate, refused at compile time, the type checker
    # finding no declaration of it (weeding round three, item 11).
    "false_in_guard.glp|Undefined procedure: false/0"
    "fail_in_guard.glp|Undefined procedure: fail/0"
    # GLP has no guard negation: the parser refuses ~d(X?) as a syntax error.
    "negated_defined_guard.glp|[syntax] \"~\" is not GLP syntax"
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

# NEG: one negative per check and load path (Integration 2026-10-11 15:53 UTC,
# item 5).  The invalid guard of true_in_guard.glp --- true in guard position,
# no guard predicate (GLP-Spec glp.tex, Definition "Guarded Clause") --- on
# every other load path: a program directory, the fault in its own self.glp
# (neg_guard_dir/); a linked program of two modules, the fault in the module
# the export reaches by a cross-module call (neg_guard_linked/); a boot with no
# program directory, the agent program following the boot clause
# (neg_guard_boot/); and a boot whose agents load a program directory with the
# fault in one of its modules (neg_boot_programdir/guard/, IsolateManager with
# programDir).  The AgentRuntime path is
# glp_runtime/test/multiagent/neg_check_paths_test.dart.
neg_guard_dir=$(echo -e "$GLP_DIR/programs/tests/neg_guard_dir\n:quit" | "$REPL_RUN" 2>&1)
check "NEG guards / directory: an invalid guard in the program's self.glp is refused" "Error loading project $GLP_DIR/programs/tests/neg_guard_dir: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 12" "$neg_guard_dir"
neg_guard_linked=$(echo -e "$GLP_DIR/programs/tests/neg_guard_linked\n:quit" | "$REPL_RUN" 2>&1)
check "NEG guards / linked: an invalid guard in the module a cross-module call reaches is refused" "Error loading project $GLP_DIR/programs/tests/neg_guard_linked: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 4" "$neg_guard_linked"
neg_guard_inline=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/neg_guard_boot/agent_boot.glp
:quit
HEREDOC
2>&1)
check "NEG guards / :boot, no program directory: the agent's load is refused for the guard" "Agent alice failed to initialize: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 16" "$neg_guard_inline"
check_not "NEG guards / :boot, no program directory: the boot does not settle" "Boot settled" "$neg_guard_inline"
neg_guard_bootdir=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/neg_boot_programdir/guard_boot.glp
:quit
HEREDOC
2>&1)
check "NEG guards / :boot with a program directory: the agent's load is refused for the module's guard" "Agent alice failed to initialize: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 5" "$neg_guard_bootdir"
check_not "NEG guards / :boot with a program directory: the boot does not settle" "Boot settled" "$neg_guard_bootdir"

# NEG: an invalid guard in a module of the program's tree that no entry point
# reaches (programs/tests/neg_guard_unreached/): TGLP modules.tex,
# Compilation, second step, "each module is checked against its scope---for
# SRSW, its guards and its types---exactly as a single-module program is,
# whether or not an entry point reaches it" (GLP, 2026-10-13 08:02 UTC, item
# 2).  Until 2026-10-13 the guard check ran on the linked program alone, after
# step 5 had dropped the module, and the program loaded.
neg_guard_unreached=$(echo -e "$GLP_DIR/programs/tests/neg_guard_unreached\n:quit" | "$REPL_RUN" 2>&1)
check "NEG guards / unreached module: an invalid guard in a module no entry point reaches is refused" "Error loading project $GLP_DIR/programs/tests/neg_guard_unreached: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 6" "$neg_guard_unreached"
check_not "NEG guards / unreached module: the program does not load" "Loaded program" "$neg_guard_unreached"

echo ""

fi  # Section E

if _section G; then
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

# Introduction via a mutual friend (paper Section 5.2): "The mutual friend
# vouches for the identities of both, having a friend channel to each", so the
# introduction proceeds only where both introduced parties have a friend output
# entry.  The test is a procedure over the outputs list and GLP admits no
# procedure in a guard (Section 6.3), so it is a body goal and the clause
# dispatches on its answers.  Both sites are covered: the person-posted
# introduce/2, and request_intro over the friend channel, which until
# 2026-09-18 looked up the anchor and not the requester.  Where either party is
# not a friend nothing is done beyond not introducing --- the paper says no
# more, so the code decides no more.
echo "--- Introduction: both parties must be friends (play_introduce_*) ---"

g_intro=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_introduce_both.
play_introduce_one.
play_introduce_other.
play_introduce_neither.
play_request_intro_both.
play_request_intro_stranger.
play_request_intro_no_anchor.
:quit
HEREDOC
2>&1)
check "SG introduce: both friends --- the intro reaches P" "intro_sent(to_bob, alice, bob, carol)" "$g_intro"
check "SG introduce: both friends --- the intro reaches Q" "intro_sent(to_carol, alice, carol, bob)" "$g_intro"
check "SG introduce: only P a friend --- nothing on P's channel" "no_intro(to_bob)" "$g_intro"
check "SG introduce: only Q a friend --- nothing on Q's channel" "no_intro(to_carol)" "$g_intro"
check "SG introduce: neither a friend --- nothing on any channel" "no_intro(to_dave)" "$g_intro"
check "SG request_intro: requester and anchor both friends --- introduced to the requester" "intro_sent(req_to_bob, alice, bob, carol)" "$g_intro"
check "SG request_intro: requester and anchor both friends --- introduced to the anchor" "intro_sent(req_to_carol, alice, carol, bob)" "$g_intro"
check "SG request_intro: the requester is no friend --- not introduced" "no_intro(req_to_carol)" "$g_intro"
check "SG request_intro: the anchor is no friend --- not introduced" "no_intro(req_to_bob)" "$g_intro"
check_not "SG introduce: no failed play" "→ failed" "$g_intro"

echo ""

fi  # Section G

if _section I; then
# Section H (old-generation CSSN modules) retired in A5; CSSN's checks are Section K.

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

fi  # Section I

if _section K; then
# =============================================================================
# Section K: the child-safe social network (programs/cssn), CSSN Section 6:
# the adult, parent and child agents, three vGLP modules compiled beside their
# sources, as a certified mini-app of the Grassroots Super-App, and its plays
# --- the village of Section 6.3 and one play per further act or condition ---
# each party a scripted person on the mini-app's person channel and a
# stand-in for the super-app.  Each act does what its schema in
# programs/cssn/CSSN.glp says: items are signed by their author and again by
# each forwarder, poster and delivering leader; a parent's state does not
# change by its child's acts, so a parent's step that waits on a child's act
# waits on the child's screen, the parent looking at the child's phone.
# =============================================================================
echo "=== Section K: CSSN, the child-safe social network ==="
echo ""

CSSN_V2="$GLP_DIR/programs/cssn"
CSSN_APP="$CSSN_V2/childsafe"

# The certified program: programs/cssn/childsafe is the three agents, the
# cards they share and the entries that activate the mini-app with its two
# channels; it holds no network entry and reaches neither the network nor the
# person, so it carries the compiler's certificate.
echo "--- CSSN mini-app (programs/cssn/childsafe) ---"

k_app=$("$REPL_RUN" <<HEREDOC
$CSSN_APP
self_module(M), decompose_module(M?, K, S, C).
:quit
HEREDOC
2>&1)

check "CSSN mini-app loads as a program" "Loaded program" "$k_app"
check_not "CSSN mini-app no type errors" "Type checking failed" "$k_app"
check_not "CSSN mini-app is certified" "CERTIFICATE REFUSED" "$k_app"
check "CSSN mini-app: its certificate carries the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$k_app"

# The committed adult.glp, parent.glp and child.glp must be the compiler's
# own output: :emit rewrites them from their .vglp sources, and the working
# tree must be unchanged.
k_emit=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
:emit $CSSN_V2
:quit
HEREDOC
2>&1)
check "CSSN: :emit re-emits adult.glp" "wrote $CSSN_APP/adult.glp" "$k_emit"
check "CSSN: :emit re-emits parent.glp" "wrote $CSSN_APP/parent.glp" "$k_emit"
check "CSSN: :emit re-emits child.glp" "wrote $CSSN_APP/child.glp" "$k_emit"
k_emit_diff=$(cd "$GLP_DIR" && git diff --stat -- programs/cssn/childsafe/adult.glp programs/cssn/childsafe/parent.glp programs/cssn/childsafe/child.glp)
check_not "CSSN: the committed agents are the compiler's output" "\.glp" "$k_emit_diff"

k_load=$("$REPL_RUN" <<HEREDOC
$CSSN_V2
:quit
HEREDOC
2>&1)
check "CSSN program loads" "Loaded program" "$k_load"
check_not "CSSN program no type errors" "Type checking failed" "$k_load"

# The contract's act schemas (CSSN Appendix B.2), programs/cssn/CSSN.glp,
# in the term form programs/gsc reads, each schema listing its variables in
# its head (GSC 0228e79): GSC's verdict on it --- an agent-named contract,
# its groups closed by the listed role of the members the leader lists
# (CSSN.glp at 4cb1ec11; GSC 8064cfb, Appendix D.4),
# with traceable provenance and volition, and an introduction with wards in
# place of an unobstructed introductory act: the verdict syntactically
# grassroots with wards (GSC, Appendix F, def:sg-wards, at GSC 503db66),
# befriend the act, child the mark, parent and parenting its guardianship,
# child_adult_befriend and child_befriend the relay acts, and retired the
# leaving mark (def:guardianship, def:leaving, def:unobstructed-marks,
# def:relay, def:introduction-wards; programs/gsc/test_gsc.sh, which checks
# it by hand).
echo "--- CSSN contract, GSC's verdict ---"
k_contract=$(bash "$CSSN_V2/compile.sh" "$(mktemp -d)" 2>&1 || true)
check "CSSN contract: GSC gives a verdict" "verdict: " "$k_contract"
check "CSSN contract: syntactically grassroots with wards, befriend the act, child the mark and retired the leaving mark" "verdict: syntactically_grassroots_with_wards(befriend, \[child\], \[guardianship(child, identity, parent, parenting)\], \[relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))\], \[retired\])" "$k_contract"
check_not "CSSN contract: its predicates have traceable provenance" "untraceable(" "$k_contract"
check_not "CSSN contract: it satisfies volition" "volition(" "$k_contract"
check_not "CSSN contract: it is not malformed" "malformed(" "$k_contract"

# One play: its log, tagged(Party, M), M a screen message or did(Step).
k_play() {
    "$REPL_RUN" <<HEREDOC
$CSSN_V2
:limit 50000000
$1.
:quit
HEREDOC
}

# The village (Section 6.3), act by act.  The parties' messages interleave in
# any order; every check is of a line, or of a count that only grows.
echo "--- CSSN village, seven acts ---"
k_v=$(k_play village 2>&1)
check_not "CSSN village: no failed goal" "→ failed\|ERROR" "$k_v"
check "CSSN village act 1: a child enters with its parent, at the parent" "tagged(alice, child(carol))" "$k_v"
check "CSSN village act 1: and at the child" "tagged(carol, parent(alice))" "$k_v"
check "CSSN village act 1: a parent of two" "tagged(bob, child(eve))" "$k_v"
check "CSSN village act 2: adult friendship" "tagged(alice, connected(frank))" "$k_v"
check "CSSN village act 2: a message Alice signed and forwarded, signed again by her" "tagged(bob, received(sgn(sgn(Hello Bob, alice), alice)))" "$k_v"
check "CSSN village act 3: child befriending" "tagged(carol, connected(dave))" "$k_v"
check "CSSN village act 3: the sibling case" "tagged(dave, connected(eve))" "$k_v"
check "CSSN village act 3: the third friendship" "tagged(eve, connected(carol))" "$k_v"
check "CSSN village act 4: a child leader's group" "tagged(carol, member_joined(group_id(carol, kids_chat), eve))" "$k_v"
check "CSSN village act 4: a member's post, delivered by the leader, signed by its author, the poster and the leader" "tagged(dave, group_received(group_id(carol, kids_chat), sgn(sgn(sgn(Hi from Eve, eve), eve), carol)))" "$k_v"
check "CSSN village act 4: Eve leaves" "tagged(eve, group_left(group_id(carol, kids_chat)))" "$k_v"
check "CSSN village act 5: an adult leader's group, a child joining through its parent" "tagged(frank, member_joined(group_id(frank, study), carol))" "$k_v"
check "CSSN village act 5: a member joining late is owed what was posted before" "tagged(carol, group_received(group_id(frank, study), sgn(sgn(sgn(Hi from Dave!, dave), dave), frank)))" "$k_v"
check "CSSN village act 6: parent-initiated removal" "tagged(dave, group_left(group_id(frank, study)))" "$k_v"
check "CSSN village act 6: child unfriending" "tagged(dave, unfriended(carol))" "$k_v"
check "CSSN village act 7: declaring of age, at the parent" "tagged(bob, child_of_age(eve))" "$k_v"
check "CSSN village act 7: and at the child, an adult" "tagged(eve, of_age)" "$k_v"
check "CSSN village act 7: befriending with no parent among the guards" "tagged(eve, connected(frank))" "$k_v"
check_not "CSSN village: a parent's state does not change by its children's acts, and no count is shown" "approvals(\|withdrawals(" "$k_v"
# The line number of the first log line holding a pattern, 0 if none.
k_line() { echo "$2" | grep -n -F "$1" | head -1 | cut -d: -f1 | grep . || echo 0; }
k_v_decl=$(k_line "tagged(bob, did(req(declare_of_age(eve))))" "$k_v")
for k_v_seen in "tagged(eve, group_left(group_id(carol, kids_chat)))" "tagged(dave, group_left(group_id(frank, study)))" "tagged(dave, unfriended(carol))"; do
    k_v_at=$(k_line "$k_v_seen" "$k_v")
    if [ "$k_v_at" -gt 0 ] && [ "$k_v_decl" -gt "$k_v_at" ]; then
        _check_pass "CSSN village act 7: Bob declares Eve of age once his child's phone shows $k_v_seen"
    else
        _check_fail "CSSN village act 7: Bob declares Eve of age once his child's phone shows $k_v_seen" "the declaration is at line $k_v_decl, the child's screen at line $k_v_at"
    fi
done
k_v_left=$(k_line "tagged(dave, group_left(group_id(frank, study)))" "$k_v")
k_v_after=$(echo "$k_v" | tail -n +"$((k_v_left + 1))" | grep -c -F "tagged(dave, group_received(group_id(frank, study)" || true)
if [ "$k_v_left" -gt 0 ] && [ "$k_v_after" = "0" ]; then
    _check_pass "CSSN village act 6: a member who has left holds no delivery of the group"
else
    _check_fail "CSSN village act 6: a member who has left holds no delivery of the group" "Dave left at line $k_v_left and holds $k_v_after deliveries after it"
fi
k_v_n=$(echo "$k_v" | grep -c "tagged(" || true)
if [ "$k_v_n" = "141" ]; then
    _check_pass "CSSN village: the log is 141 lines"
else
    _check_fail "CSSN village: the log is 141 lines" "the log is $k_v_n lines (expected 141)"
fi

echo "--- CSSN entering with two parents, declaring of age ---"
k_tp=$(k_play play_two_parents 2>&1)
check "CSSN two parents: a child enters with two" "tagged(carol, parent(bob))" "$k_tp"
check "CSSN two parents: at each parent" "tagged(bob, child(carol))" "$k_tp"
check "CSSN two parents: a child is no parent" "tagged(carol, not_my_child(carol))" "$k_tp"
check "CSSN two parents: a child refuses a conversation" "tagged(carol, refused(dave))" "$k_tp"
check "CSSN two parents: one parent declares the child of age" "tagged(alice, child_of_age(carol))" "$k_tp"
check "CSSN two parents: which ends the other's parenting too" "tagged(bob, child_of_age(carol))" "$k_tp"
check "CSSN two parents: the child is an adult" "tagged(carol, of_age)" "$k_tp"
check "CSSN two parents: its parents are adults again" "tagged(alice, not_my_child(carol))" "$k_tp"
check "CSSN two parents: both of them" "tagged(bob, not_my_child(carol))" "$k_tp"
check "CSSN two parents: and the child befriends directly" "tagged(carol, connected(dave))" "$k_tp"

echo "--- CSSN two introductions at once ---"
k_tw=$(k_play play_twice 2>&1)
k_tw_c=$(echo "$k_tw" | grep -c "tagged(carol, connected(dave))" || true)
k_tw_d=$(echo "$k_tw" | grep -c "tagged(dave, connected(carol))" || true)
if [ "$k_tw_c" = "1" ] && [ "$k_tw_d" = "1" ]; then
    _check_pass "CSSN twice: two introductions establish one friend channel"
else
    _check_fail "CSSN twice: two introductions establish one friend channel" "carol connected $k_tw_c times, dave $k_tw_d (expected 1 each)"
fi
k_tw_b=$(echo "$k_tw" | grep -c -F "tagged(bob, did(answer(intro(alice, carol, dave), yes)))" || true)
if [ "$k_tw_b" = "2" ]; then
    _check_pass "CSSN twice: Bob approves both introductions"
else
    _check_fail "CSSN twice: Bob approves both introductions" "Bob approved $k_tw_b (expected 2)"
fi
check_not "CSSN twice: and the parents' screens show no count" "approvals(\|withdrawals(" "$k_tw"

echo "--- CSSN two adults befriending each other at once ---"
k_bw=$(k_play play_both_ways 2>&1)
k_bw_a=$(echo "$k_bw" | grep -c "tagged(alice, connected(bob))" || true)
k_bw_b=$(echo "$k_bw" | grep -c "tagged(bob, connected(alice))" || true)
if [ "$k_bw_a" = "1" ] && [ "$k_bw_b" = "1" ]; then
    _check_pass "CSSN both ways: one friendship"
else
    _check_fail "CSSN both ways: one friendship" "alice connected $k_bw_a times, bob $k_bw_b (expected 1 each)"
fi

echo "--- CSSN a late joiner is owed the earlier posts ---"
k_lj=$(k_play play_late_join 2>&1)
check "CSSN late join: the member on the list at the post receives it, signed by the leader" "tagged(bob, group_received(group_id(alice, late_join), sgn(sgn(Posted before Carol joined, alice), alice)))" "$k_lj"
check "CSSN late join: the late joiner is owed it too" "tagged(carol, group_received(group_id(alice, late_join), sgn(sgn(Posted before Carol joined, alice), alice)))" "$k_lj"
check "CSSN late join: a member joining after two posts is owed the first" "tagged(dave, group_received(group_id(alice, late_join), sgn(sgn(Posted before Carol joined, alice), alice)))" "$k_lj"
check "CSSN late join: and the second" "tagged(dave, group_received(group_id(alice, late_join), sgn(sgn(Posted before Dave joined, alice), alice)))" "$k_lj"
k_lj_b=$(echo "$k_lj" | grep -c -F "tagged(bob, group_received(group_id(alice, late_join), sgn(sgn(Posted before Carol joined, alice), alice)))" || true)
if [ "$k_lj_b" = "1" ]; then
    _check_pass "CSSN late join: no member receives the post twice"
else
    _check_fail "CSSN late join: no member receives the post twice" "bob received it $k_lj_b times (expected 1)"
fi
k_lj_order=$(echo "$k_lj" | grep "tagged(dave, group_received(group_id(alice, late_join)" | head -2 | tr '\n' '|')
case "$k_lj_order" in
    *"Posted before Carol joined"*"Posted before Dave joined"*)
        _check_pass "CSSN late join: the catch-up is in the order the posts were made" ;;
    *)
        _check_fail "CSSN late join: the catch-up is in the order the posts were made" "order was: $k_lj_order" ;;
esac

echo "--- CSSN ending a child's membership of a child's group ---"
k_cl=$(k_play play_child_leave 2>&1)
check "CSSN child leave: the member's parent's role, at the member" "tagged(dave, group_left(group_id(carol, kids_chat)))" "$k_cl"
check "CSSN child leave: and at the leader" "tagged(carol, member_left(group_id(carol, kids_chat), dave))" "$k_cl"
check "CSSN child leave: the member's own leaving, at the member" "tagged(eve, group_left(group_id(carol, kids_chat)))" "$k_cl"
check "CSSN child leave: and at the leader too" "tagged(carol, member_left(group_id(carol, kids_chat), eve))" "$k_cl"
k_cl_req=$(echo "$k_cl" | grep -n -F "tagged(bob, did(req(leave_child(dave, group_id(carol, kids_chat)))))" | head -1 | cut -d: -f1)
k_cl_seen=$(echo "$k_cl" | grep -n -F "tagged(dave, group_joined(group_id(carol, kids_chat)))" | head -1 | cut -d: -f1)
if [ -n "$k_cl_req" ] && [ -n "$k_cl_seen" ] && [ "$k_cl_req" -gt "$k_cl_seen" ]; then
    _check_pass "CSSN child leave: Bob ends Dave's membership once Dave's phone shows him joined"
else
    _check_fail "CSSN child leave: Bob ends Dave's membership once Dave's phone shows him joined" "the request at line $k_cl_req, the joining at line $k_cl_seen"
fi

echo ""

fi  # Section K

if _section L; then
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

fi  # Section L

if _section M; then
# =============================================================================
# Section M: Multi-Isolate (madGLP) Tests
# =============================================================================

echo "=== Section M: Multi-Isolate (madGLP) Tests ==="
echo ""
echo "  (folded into Section Q, which runs the whole Dart tree — running"
echo "   the madGLP Dart tests here as well would only double-count them)"
echo ""

fi  # Section M

if _section N2; then
# =============================================================================
# Section N2: The coins program (programs/currencies/coins): the vGLP currency agent of
# the Grassroots Currencies paper as a mini-app of the Grassroots Super-App.
# programs/currencies/coins/currency is the certified program --- the agent
# coins_agent.vglp, its canonical compilation coins_agent.glp (emitted by glpc
# :emit; the .vglp beside it is skipped by the loader while the .glp stands),
# the mini-app entry coins/3 and the mini-app's own two plays --- and
# programs/currencies/coins adds the six-agent village market of the paper's Section 5,
# which stands in for the super-app (the conversations) and the persons.  The
# run needs the reduction limit raised.
# =============================================================================
echo "=== Section N2: Coins program (Grassroots Currencies) ==="
echo ""

COINS="$GLP_DIR/programs/currencies/coins"

# The certified program: loads, type-checks, and carries the compiler's
# certificate, since no entry point reaches the network or the person.
n2_core=$("$REPL_RUN" <<HEREDOC
$COINS/currency
self_module(M), decompose_module(M?, K, S, C).
:quit
HEREDOC
2>&1)

check "Coins mini-app loads as a program" "Loaded program" "$n2_core"
check_not "Coins mini-app no type errors" "Type checking failed" "$n2_core"
check_not "Coins mini-app is certified" "CERTIFICATE REFUSED" "$n2_core"
check "Coins mini-app: its certificate carries the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$n2_core"

n2_load=$("$REPL_RUN" <<HEREDOC
$COINS
:quit
HEREDOC
2>&1)

check "Coins program loads" "Loaded program" "$n2_load"
check_not "Coins program no type errors" "Type checking failed" "$n2_load"

# The committed coins_agent.glp must be the compiler's own output: :emit
# rewrites it from coins_agent.vglp, and the working tree must be unchanged.
n2_emit=$("$REPL_RUN" <<HEREDOC
$COINS
:emit $COINS
:quit
HEREDOC
2>&1)
check "Coins program: :emit re-emits coins_agent.glp" "wrote $COINS/currency/coins_agent.glp" "$n2_emit"
n2_emit_diff=$(cd "$GLP_DIR" && git diff --stat -- programs/currencies/coins/currency/coins_agent.glp)
check_not "Coins program: committed coins_agent.glp is the compiler's output" "coins_agent.glp" "$n2_emit_diff"

echo "--- Coins village market (village) ---"

n2_run=$("$REPL_RUN" <<HEREDOC
$COINS
:limit 50000000
village.
:quit
HEREDOC
2>&1)

check "Coins village: no runtime error" "→ suspended" "$n2_run"
check_not "Coins village: no failed goal" "ERROR" "$n2_run"
check "Coins village: the conversations open" "tagged(frank, opened(eve))" "$n2_run"
# Alice's holdings are read as a multiset: since the port to the dispatcher's
# person channel, bob's payment of 5 can reach her agent before her swap with
# charlie settles, and her lots are tallied in the order they arrive
# (Currencies #8 Cowork, 2026-10-11 13:06 UTC: "the coins holdings check reads
# the multiset").  Each holdings line of hers, its lots sorted:
n2_alice=$(echo "$n2_run" | grep -o 'tagged(alice, holdings(\[[^]]*\]))' | awk '{
  s = $0; sub(/^tagged\(alice, holdings\(\[/, "", s); sub(/\]\)\)$/, "", s)
  n = split(s, a, /\), /); for (i = 1; i < n; i++) a[i] = a[i] ")"
  for (i = 2; i <= n; i++) { v = a[i]; j = i - 1; while (j > 0 && a[j] > v) { a[j + 1] = a[j]; j-- } a[j + 1] = v }
  o = ""; for (i = 1; i <= n; i++) o = o (i > 1 ? ", " : "") a[i]
  print "alice holds {" o "}" }')
check "Coins village: Alice's holdings" "^alice holds {lot(alice, 8), lot(bob, 5), lot(charlie, 15), lot(frank, 4)}$" "$n2_alice"
check "Coins village: Bob's holdings" "tagged(bob, holdings(\[lot(alice, 10), lot(diana, 20)\]))" "$n2_run"
check "Coins village: Charlie's holdings" "tagged(charlie, holdings(\[lot(alice, 10), lot(eve, 10), lot(charlie, 6), lot(frank, 5)\]))" "$n2_run"
check "Coins village: Diana's holdings" "tagged(diana, holdings(\[lot(bob, 24), lot(frank, 13), lot(diana, 8)\]))" "$n2_run"
check "Coins village: Eve's holdings" "tagged(eve, holdings(\[lot(charlie, 4), lot(frank, 1), lot(alice, 2), lot(bob, 10)\]))" "$n2_run"
check "Coins village: Frank's holdings" "tagged(frank, holdings(\[lot(diana, 7), lot(eve, 10), lot(frank, 5)\]))" "$n2_run"
check "Coins village: five redemptions" "tagged(frank, redeemed(diana, frank))" "$n2_run"

echo "--- The coins mini-app's own plays (play_mint, play_swap) ---"

n2_pm=$("$REPL_RUN" <<HEREDOC
$COINS/currency
:limit 5000000
play_mint(S).
:quit
HEREDOC
2>&1)

check_not "Coins mini-app play_mint: no failed goal" "ERROR" "$n2_pm"
check "Coins mini-app play_mint: alice mints 3 of her own" "S = \[minted(3), holdings(\[lot(alice, 3)\])" "$n2_pm"

n2_ps=$("$REPL_RUN" <<HEREDOC
$COINS/currency
:limit 5000000
play_swap(A, B).
:quit
HEREDOC
2>&1)

check_not "Coins mini-app play_swap: no failed goal" "ERROR" "$n2_ps"
check "Coins mini-app play_swap: the proposer's screen, her conversation opened and the swap done" "A = \[opened(bob), minted(2), holdings(\[lot(alice, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 2)\])" "$n2_ps"
check "Coins mini-app play_swap: the counterparty's screen" "B = \[opened(alice), minted(2), holdings(\[lot(bob, 2)\]), swap_done(alice), holdings(\[lot(alice, 2)\])" "$n2_ps"
echo ""

fi  # Section N2

if _section N3; then
# =============================================================================
# Section N3: The bonds program (programs/currencies/bonds): the vGLP bond agent of the
# Grassroots Bonds paper as a mini-app of the Grassroots Super-App.
# programs/currencies/bonds/dated is the certified program --- the agent
# bonds_agent.vglp, its canonical compilation bonds_agent.glp (emitted by glpc
# :emit; the .vglp beside it is skipped by the loader while the .glp stands),
# the mini-app entry bonds/3, the scripted person with the escrow programs it
# answers by, and the mini-app's own two plays --- and programs/currencies/bonds
# adds the plays that stand in for the super-app, opening the conversations
# between the parties who transact: the seven-party village market of the
# paper's Section 7, the credit line of its Section 5, and the instruments of
# its Sections 4 and 5 and Appendix C.  As for N2 the runs need the reduction
# limit raised.
# =============================================================================
echo "=== Section N3: Bonds program (Grassroots Bonds) ==="
echo ""

BONDS="$GLP_DIR/programs/currencies/bonds"
BONDSD="$BONDS/dated"

# The certified program: loads, type-checks, and carries the compiler's
# certificate, since no entry point reaches the network or the person.
n3_core=$("$REPL_RUN" <<HEREDOC
$BONDSD
self_module(M), decompose_module(M?, K, S, C).
:quit
HEREDOC
2>&1)

check "Bonds mini-app loads as a program" "Loaded program" "$n3_core"
check_not "Bonds mini-app no type errors" "Type checking failed" "$n3_core"
check_not "Bonds mini-app is certified" "CERTIFICATE REFUSED" "$n3_core"
check "Bonds mini-app: its certificate carries the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$n3_core"

n3_load=$("$REPL_RUN" <<HEREDOC
$BONDS
:quit
HEREDOC
2>&1)

check "Bonds program loads" "Loaded program" "$n3_load"
check_not "Bonds program no type errors" "Type checking failed" "$n3_load"

n3_emit=$("$REPL_RUN" <<HEREDOC
$BONDS
:emit $BONDSD
:quit
HEREDOC
2>&1)
check "Bonds program: :emit re-emits bonds_agent.glp" "wrote $BONDSD/bonds_agent.glp" "$n3_emit"
n3_emit_diff=$(cd "$GLP_DIR" && git diff --stat -- programs/currencies/bonds/dated/bonds_agent.glp)
check_not "Bonds program: committed bonds_agent.glp is the compiler's output" "bonds_agent.glp" "$n3_emit_diff"

echo "--- Bonds village market (village) ---"

n3_run=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
village.
:quit
HEREDOC
2>&1)

check "Bonds village: no runtime error" "→ suspended" "$n3_run"
check_not "Bonds village: no failed goal" "ERROR" "$n3_run"
check "Bonds village: the conversations open" "tagged(frank, opened(escrow))" "$n3_run"
check "Bonds village: Alice's holdings" "tagged(alice, holdings(\[lot(bob, now, 5), lot(alice, now, 8), lot(charlie, now, 15), lot(frank, now, 4)\]))" "$n3_run"
check "Bonds village: Bob's holdings" "tagged(bob, holdings(\[lot(alice, now, 10), lot(diana, now, 20)\]))" "$n3_run"
check "Bonds village: Charlie's holdings" "tagged(charlie, holdings(\[lot(alice, now, 10), lot(eve, now, 10), lot(charlie, now, 6)\]))" "$n3_run"
check "Bonds village: Diana's holdings" "tagged(diana, holdings(\[lot(bob, 25, 24), lot(frank, 28, 12), lot(diana, now, 8), lot(frank, now, 1)\]))" "$n3_run"
check "Bonds village: Eve's holdings" "tagged(eve, holdings(\[lot(charlie, now, 4), lot(frank, now, 1), lot(alice, now, 2), lot(bob, now, 10)\]))" "$n3_run"
check "Bonds village: Frank's holdings" "tagged(frank, holdings(\[lot(diana, now, 7), lot(eve, now, 10), lot(frank, 28, 5), lot(frank, now, 5)\]))" "$n3_run"
check "Bonds village: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_run"
check "Bonds village: a bond presented before its issuer's date reaches its maturity is returned by the issuer" "tagged(frank, not_mature_for(diana, 28))" "$n3_run"
check "Bonds village: and the bearer holds it back" "tagged(diana, not_mature(frank, 28))" "$n3_run"
check "Bonds village: Bob's bonds mature by his own calendar" "tagged(bob, date_advanced(25))" "$n3_run"
check "Bonds village: Advance is the next day, so the day before is logged too" "tagged(bob, date_advanced(24))" "$n3_run"
check "Bonds village: the redemption takes back an immature bond" "tagged(frank, redeemed(diana, frank, 28))" "$n3_run"
check "Bonds village: once mature by its issuer's date, the bond is taken for a coin" "tagged(diana, matured(frank, 28))" "$n3_run"
check_not "Bonds village: Mature records no date warranty, so no warranty( line is logged" "warranty(" "$n3_run"
check "Bonds village: the escrow agent's date reaches the release date" "tagged(escrow, date_advanced(30))" "$n3_run"
check "Bonds village: the escrow releases to the beneficiary" "tagged(frank, received_transfer(release, escrow, \[lot(frank, now, 5)\]))" "$n3_run"

echo "--- Bonds two escrows open at once (two_escrows) ---"

# One escrow agent holding two date-based agreements at once, agreements([...])
# (Currencies 2026-10-13 08:01 UTC): Charlie deposits 2 frank-coins for Frank
# until day 30, then 2 more; both are held together, the escrow agent's date
# reaches 30, and each agreement releases its own deposit to Frank --- the
# case that failed on the retired bonds_v2 (tmp/gsgsend7-twoesc.txt).
n3_esc2=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
two_escrows.
:quit
HEREDOC
2>&1)

check "Bonds two escrows: no runtime error" "→ suspended" "$n3_esc2"
check_not "Bonds two escrows: no failed goal" "ERROR" "$n3_esc2"
check "Bonds two escrows: the escrow agent holds both deposits at once" "tagged(escrow, holdings(\[lot(frank, now, 4)\]))" "$n3_esc2"
check "Bonds two escrows: the escrow agent's date reaches the release date" "tagged(escrow, date_advanced(30))" "$n3_esc2"
check "Bonds two escrows: the first release reaches the beneficiary" "tagged(frank, holdings(\[lot(charlie, now, 4), lot(frank, now, 2)\]))" "$n3_esc2"
check "Bonds two escrows: the second release reaches the beneficiary" "tagged(frank, holdings(\[lot(charlie, now, 4), lot(frank, now, 4)\]))" "$n3_esc2"
check "Bonds two escrows: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_esc2"

echo "--- Bonds credit line (credit_line), Proposition prop:credit-line ---"

n3_cl=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
credit_line.
:quit
HEREDOC
2>&1)

check "Bonds credit line: no runtime error" "→ suspended" "$n3_cl"
check_not "Bonds credit line: no failed goal" "ERROR" "$n3_cl"
check "Bonds credit line: establishment, the escrow holds the limit" "tagged(escrow, holdings(\[lot(alice, now, 20)\]))" "$n3_cl"
check "Bonds credit line: draw, the escrow holds the undrawn coins" "tagged(escrow, holdings(\[lot(alice, now, 12)\]))" "$n3_cl"
check "Bonds credit line: draw, the lender holds the principal for the amount drawn" "tagged(alice, holdings(\[lot(bob, 25, 8), lot(bob, 20, 2)\]))" "$n3_cl"
check "Bonds credit line: partial repayment restores capacity" "tagged(escrow, holdings(\[lot(alice, now, 15)\]))" "$n3_cl"
check "Bonds credit line: expiry judged by the escrow agent's own date" "tagged(escrow, date_advanced(25))" "$n3_cl"
check "Bonds credit line: at expiry the undrawn coins return to the lender" "tagged(alice, received_transfer(return, escrow, \[lot(alice, now, 20)\]))" "$n3_cl"
check "Bonds credit line: at expiry with nothing drawn the lender holds its coins and no principal bond" "tagged(alice, holdings(\[lot(bob, 20, 2), lot(alice, now, 20)\]))" "$n3_cl"
check "Bonds credit line: the borrower holds its principal bonds back" "tagged(bob, holdings(\[lot(bob, 25, 8)\]))" "$n3_cl"
check "Bonds credit line: the escrow holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_cl"

echo "--- Bonds collateral (collateral) ---"

n3_col=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
collateral.
:quit
HEREDOC
2>&1)

check "Bonds collateral: no runtime error" "→ suspended" "$n3_col"
check_not "Bonds collateral: no failed goal" "ERROR" "$n3_col"
check "Bonds collateral: the borrower posts coins that are not its own" "tagged(escrow, received_transfer(deposit, bob, \[lot(carol, now, 3)\]))" "$n3_col"
check "Bonds collateral: on fulfilment the collateral is returned to the borrower" "tagged(bob, received_transfer(return, escrow, \[lot(carol, now, 3)\]))" "$n3_col"
check "Bonds collateral: on default it is released to the lender" "tagged(alice, received_transfer(release, escrow, \[lot(carol, now, 3)\]))" "$n3_col"
check "Bonds collateral: the repaid lender holds its coins back and the coupon bond" "tagged(alice, holdings(\[lot(bob, 20, 1), lot(alice, now, 5), lot(bob, 30, 6), lot(carol, now, 3)\]))" "$n3_col"
check "Bonds collateral: the borrower holds the bonds it redeemed back and the returned collateral" "tagged(bob, holdings(\[lot(bob, 20, 5), lot(carol, now, 3), lot(alice, now, 5)\]))" "$n3_col"
check "Bonds collateral: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_col"

echo "--- Bonds forward contract (forward) ---"

n3_fwd=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
forward.
:quit
HEREDOC
2>&1)

check_not "Bonds forward: no failed goal" "ERROR" "$n3_fwd"
check "Bonds forward: each party holds the other's bonds of the agreed face value, maturing together (alice)" "tagged(alice, holdings(\[lot(bob, 30, 12)\]))" "$n3_fwd"
check "Bonds forward: and bob" "tagged(bob, holdings(\[lot(alice, 30, 10)\]))" "$n3_fwd"

echo "--- Bonds guarantee (guarantee) ---"

n3_gtee=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
guarantee.
:quit
HEREDOC
2>&1)

check_not "Bonds guarantee: no failed goal" "ERROR" "$n3_gtee"
check "Bonds guarantee: the guarantor deposits its own bonds" "tagged(escrow, received_transfer(deposit, gary, \[lot(gary, now, 3)\]))" "$n3_gtee"
check "Bonds guarantee: on fulfilment they are returned to the guarantor" "tagged(gary, received_transfer(return, escrow, \[lot(gary, now, 3)\]))" "$n3_gtee"
check "Bonds guarantee: on default they are released to the lender" "tagged(alice, received_transfer(release, escrow, \[lot(gary, now, 3)\]))" "$n3_gtee"
check "Bonds guarantee: the lender holds the guarantor's bonds and the defaulted loan's" "tagged(alice, holdings(\[lot(bob, 20, 1), lot(alice, now, 5), lot(bob, 30, 6), lot(gary, now, 3)\]))" "$n3_gtee"
check "Bonds guarantee: the guarantor is left with what it did not forfeit" "tagged(gary, holdings(\[lot(gary, now, 3)\]))" "$n3_gtee"
check "Bonds guarantee: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_gtee"

echo "--- Bonds packaged exchange: balloon and fixed-payment loans ---"

n3_bal=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
balloon.
:quit
HEREDOC
2>&1)

check_not "Bonds balloon loan: no failed goal" "ERROR" "$n3_bal"
check "Bonds balloon loan: the lender holds the interest bonds and the principal" "tagged(alice, holdings(\[lot(bob, 10, 1), lot(bob, 20, 1), lot(bob, 30, 10)\]))" "$n3_bal"
check "Bonds balloon loan: the borrower holds the coins lent" "tagged(bob, holdings(\[lot(alice, now, 10)\]))" "$n3_bal"
check "Bonds balloon loan: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_bal"

n3_fix=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
fixed_payment.
:quit
HEREDOC
2>&1)

check_not "Bonds fixed-payment loan: no failed goal" "ERROR" "$n3_fix"
check "Bonds fixed-payment loan: the lender holds the three equal payments" "tagged(alice, holdings(\[lot(bob, 10, 4), lot(bob, 20, 4), lot(bob, 30, 4)\]))" "$n3_fix"
check "Bonds fixed-payment loan: the borrower holds the coins lent" "tagged(bob, holdings(\[lot(alice, now, 9)\]))" "$n3_fix"
check "Bonds fixed-payment loan: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_fix"

n3_irs=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
interest_rate_swap.
:quit
HEREDOC
2>&1)

check_not "Bonds interest rate swap: no failed goal" "ERROR" "$n3_irs"
check "Bonds interest rate swap: the fixed payer holds the floating leg" "tagged(alice, holdings(\[lot(bob, 10, 2), lot(bob, 20, 4), lot(bob, 30, 5)\]))" "$n3_irs"
check "Bonds interest rate swap: the floating payer holds the fixed leg" "tagged(bob, holdings(\[lot(alice, 10, 3), lot(alice, 20, 3), lot(alice, 30, 3)\]))" "$n3_irs"
check "Bonds interest rate swap: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_irs"

echo "--- Bonds option, exercised and expired (option_exercised, option_expired) ---"

n3_optx=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
option_exercised.
:quit
HEREDOC
2>&1)

check_not "Bonds option exercised: no failed goal" "ERROR" "$n3_optx"
check "Bonds option exercised: the holder takes the underlying" "tagged(alice, holdings(\[lot(bob, 40, 10)\]))" "$n3_optx"
check "Bonds option exercised: the writer takes the premium and the strike" "tagged(bob, holdings(\[lot(alice, now, 10)\]))" "$n3_optx"
check "Bonds option exercised: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_optx"

n3_opte=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
option_expired.
:quit
HEREDOC
2>&1)

check_not "Bonds option expired: no failed goal" "ERROR" "$n3_opte"
check "Bonds option expired: the escrow agent's own date passes the window" "tagged(escrow, date_advanced(41))" "$n3_opte"
check "Bonds option expired: the strike goes back to the holder" "tagged(alice, holdings(\[lot(alice, now, 8)\]))" "$n3_opte"
check "Bonds option expired: the writer keeps the underlying and takes the premium" "tagged(bob, holdings(\[lot(bob, 40, 10), lot(alice, now, 2)\]))" "$n3_opte"
check "Bonds option expired: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_opte"

echo "--- Bonds insurance, claimed and expired (insurance_claimed, insurance_expired) ---"

n3_insc=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
insurance_claimed.
:quit
HEREDOC
2>&1)

check_not "Bonds insurance claimed: no failed goal" "ERROR" "$n3_insc"
check "Bonds insurance claimed: the payout is the insurer's reserve" "tagged(alice, holdings(\[lot(bob, now, 20)\]))" "$n3_insc"
check "Bonds insurance claimed: the insurer keeps the premium" "tagged(bob, holdings(\[lot(alice, now, 2)\]))" "$n3_insc"

n3_inse=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
insurance_expired.
:quit
HEREDOC
2>&1)

check_not "Bonds insurance expired: no failed goal" "ERROR" "$n3_inse"
check "Bonds insurance expired: the insured holds nothing" "tagged(alice, holdings(\[\]))" "$n3_inse"
check "Bonds insurance expired: the insurer takes the premium and its reserve back" "tagged(bob, holdings(\[lot(alice, now, 2), lot(bob, now, 20)\]))" "$n3_inse"

echo "--- Bonds credit default swap, with and without the credit event ---"

n3_cdse=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
cds_event.
:quit
HEREDOC
2>&1)

check_not "Bonds CDS credit event: no failed goal" "ERROR" "$n3_cdse"
check "Bonds CDS credit event: the protection buyer takes the reserve" "tagged(alice, holdings(\[lot(bob, now, 20)\]))" "$n3_cdse"
check "Bonds CDS credit event: the seller keeps the premium bonds" "tagged(bob, holdings(\[lot(alice, 10, 1), lot(alice, 20, 1)\]))" "$n3_cdse"

n3_cdsn=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
cds_no_event.
:quit
HEREDOC
2>&1)

check_not "Bonds CDS no credit event: no failed goal" "ERROR" "$n3_cdsn"
check "Bonds CDS no credit event: the buyer holds nothing" "tagged(alice, holdings(\[\]))" "$n3_cdsn"
check "Bonds CDS no credit event: the seller takes the premiums and its reserve back at expiry" "tagged(bob, holdings(\[lot(alice, 10, 1), lot(alice, 20, 1), lot(bob, now, 20)\]))" "$n3_cdsn"

echo "--- Bonds letter of credit (letter_of_credit) ---"

n3_loc=$("$REPL_RUN" <<HEREDOC
$BONDS
:limit 50000000
letter_of_credit.
:quit
HEREDOC
2>&1)

check_not "Bonds letter of credit: no failed goal" "ERROR" "$n3_loc"
check "Bonds letter of credit: the seller is paid in the bank's coins" "tagged(alice, holdings(\[lot(bank, now, 10)\]))" "$n3_loc"
check "Bonds letter of credit: the bank holds the buyer's reimbursement bonds" "tagged(bank, holdings(\[lot(bob, 30, 10)\]))" "$n3_loc"
check "Bonds letter of credit: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_loc"

echo "--- The bonds mini-app's own plays (play_mint, play_swap) ---"

n3_pm=$("$REPL_RUN" <<HEREDOC
$BONDSD
:limit 5000000
play_mint(S).
:quit
HEREDOC
2>&1)

check_not "Bonds mini-app play_mint: no failed goal" "ERROR" "$n3_pm"
check "Bonds mini-app play_mint: alice mints 3 of her own" "S = \[minted(3, now), holdings(\[lot(alice, now, 3)\])" "$n3_pm"

n3_ps=$("$REPL_RUN" <<HEREDOC
$BONDSD
:limit 5000000
play_swap(A, B).
:quit
HEREDOC
2>&1)

check_not "Bonds mini-app play_swap: no failed goal" "ERROR" "$n3_ps"
check "Bonds mini-app play_swap: the proposer's screen, her conversation opened and her coins swapped for bonds" "A = \[opened(bob), minted(2, now), holdings(\[lot(alice, now, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 30, 2)\])" "$n3_ps"
check "Bonds mini-app play_swap: the counterparty's screen" "B = \[opened(alice), minted(2, 30), holdings(\[lot(bob, 30, 2)\]), swap_done(alice), holdings(\[lot(alice, now, 2)\])" "$n3_ps"
echo ""

fi  # Section N3

if _section N4; then
# =============================================================================
# Section N4: The denominated program (programs/currencies/sovereign): the bond agent of
# grassroots bonds with the denomination added, as the act schemas of
# "Sovereign Grassroots Currencies" are the bonds schemas with the denomination
# added --- cent(u, f, d) carrying f as its middle argument, every schema
# carrying the one denomination variable f --- as a mini-app of the Grassroots
# Super-App.  programs/currencies/sovereign/denominated is the certified program --- the
# agent sovereign_agent.vglp, its canonical compilation sovereign_agent.glp
# (emitted by glpc :emit; the .vglp beside it is skipped by the loader while
# the .glp stands), the mini-app entry sovereign/3, the scripted person with
# the escrow programs it answers by, and the mini-app's own two plays --- and
# programs/currencies/sovereign adds the plays that stand in for the super-app: the
# village market of the bonds paper denominated in one fiat currency, which
# must end in exactly the holdings of the undenominated run with usd in every
# lot; the same village with the central bank as an eighth party, exercising
# all twelve transactions of the contract --- mint in both forms, advance,
# swap, accept, pay, payout, spend, redeem, mature, deposit, release and
# return --- with the fiat unit for a sovereign coin paid to the central bank
# handed over by its person outside the contract; the term credit line of the
# bonds paper's Section 5; and the live-person harness sovereign_ui/3, the
# same market cut to the central bank, the community bank and the household,
# with the central bank on the app's screen.  As for N2 and N3 the runs need
# the reduction limit raised.
# =============================================================================
echo "=== Section N4: Denominated program (Sovereign Grassroots Currencies) ==="
echo ""

SOV="$GLP_DIR/programs/currencies/sovereign"
SOVD="$SOV/denominated"

# The certified program: loads, type-checks, and carries the compiler's
# certificate, since no entry point reaches the network or the person.
n4_core=$("$REPL_RUN" <<HEREDOC
$SOVD
self_module(M), decompose_module(M?, K, S, C).
:quit
HEREDOC
2>&1)

check "Denominated mini-app loads as a program" "Loaded program" "$n4_core"
check_not "Denominated mini-app no type errors" "Type checking failed" "$n4_core"
check_not "Denominated mini-app is certified" "CERTIFICATE REFUSED" "$n4_core"
check "Denominated mini-app: its certificate carries the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$n4_core"

n4_load=$("$REPL_RUN" <<HEREDOC
$SOV
:quit
HEREDOC
2>&1)

check "Denominated program loads" "Loaded program" "$n4_load"
check_not "Denominated program no type errors" "Type checking failed" "$n4_load"

n4_emit=$("$REPL_RUN" <<HEREDOC
$SOV
:emit $SOVD
:quit
HEREDOC
2>&1)
check "Denominated program: :emit re-emits sovereign_agent.glp" "wrote $SOVD/sovereign_agent.glp" "$n4_emit"
n4_emit_diff=$(cd "$GLP_DIR" && git diff --stat -- programs/currencies/sovereign/denominated/sovereign_agent.glp)
check_not "Denominated program: committed sovereign_agent.glp is the compiler's output" "sovereign_agent.glp" "$n4_emit_diff"

echo "--- The denominated village market (village) ---"

n4_run=$("$REPL_RUN" <<HEREDOC
$SOV
:limit 50000000
village.
:quit
HEREDOC
2>&1)

check "Denominated village: no runtime error" "→ suspended" "$n4_run"
check_not "Denominated village: no failed goal" "ERROR" "$n4_run"
check "Denominated village: the conversations open" "tagged(frank, opened(escrow))" "$n4_run"
check "Denominated village: Alice's holdings, denominated" "tagged(alice, holdings(\[lot(bob, usd, now, 5), lot(alice, usd, now, 8), lot(charlie, usd, now, 15), lot(frank, usd, now, 4)\]))" "$n4_run"
check "Denominated village: Bob's holdings, denominated" "tagged(bob, holdings(\[lot(alice, usd, now, 10), lot(diana, usd, now, 20)\]))" "$n4_run"
check "Denominated village: Charlie's holdings, denominated" "tagged(charlie, holdings(\[lot(alice, usd, now, 10), lot(eve, usd, now, 10), lot(charlie, usd, now, 6)\]))" "$n4_run"
check "Denominated village: Diana's holdings, denominated" "tagged(diana, holdings(\[lot(bob, usd, 25, 24), lot(frank, usd, 28, 12), lot(diana, usd, now, 8), lot(frank, usd, now, 1)\]))" "$n4_run"
check "Denominated village: Eve's holdings, denominated" "tagged(eve, holdings(\[lot(charlie, usd, now, 4), lot(frank, usd, now, 1), lot(alice, usd, now, 2), lot(bob, usd, now, 10)\]))" "$n4_run"
check "Denominated village: Frank's holdings, denominated" "tagged(frank, holdings(\[lot(diana, usd, now, 7), lot(eve, usd, now, 10), lot(frank, usd, 28, 5), lot(frank, usd, now, 5)\]))" "$n4_run"
check "Denominated village: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n4_run"
check "Denominated village: a bond presented before its issuer's date reaches its maturity is still returned" "tagged(diana, not_mature(frank, 28))" "$n4_run"
check "Denominated village: and taken for a coin once it has" "tagged(diana, matured(frank, 28))" "$n4_run"
check_not "Denominated village: Mature records no date warranty, so no warranty( line is logged" "warranty(" "$n4_run"
check "Denominated village: Advance is the next day, so the day before is logged too" "tagged(bob, date_advanced(24))" "$n4_run"

echo "--- The sovereign market (sovereign_village) ---"

n4_sov=$("$REPL_RUN" <<HEREDOC
$SOV
:limit 50000000
sovereign_village.
:quit
HEREDOC
2>&1)

check "Sovereign market: no runtime error" "→ suspended" "$n4_sov"
check_not "Sovereign market: no failed goal" "ERROR" "$n4_sov"
check "Sovereign market: the central bank mints its own coins" "tagged(cb, minted(30, now))" "$n4_sov"
check "Sovereign market: the mutual credit line is willed by the central bank" "tagged(cb, swap_done(diana))" "$n4_sov"
check "Sovereign market: and by the community bank" "tagged(diana, swap_done(cb))" "$n4_sov"
check "Sovereign market: the central bank holds the community bank's coins" "tagged(cb, holdings(\[lot(cb, usd, now, 20), lot(diana, usd, now, 10)\]))" "$n4_sov"
check "Sovereign market: chain redemption --- the household takes a sovereign coin for a community-bank coin" "tagged(frank, redeemed(diana, cb, now))" "$n4_sov"
check "Sovereign market: and the community bank gives it out of the credit line" "tagged(diana, presented(frank, cb, now))" "$n4_sov"
check "Sovereign market: fiat fulfilment --- the household pays the sovereign coin to its issuer" "tagged(frank, paid(cb, 1))" "$n4_sov"
check "Sovereign market: and the central bank takes it" "tagged(cb, received(frank, 1))" "$n4_sov"
check "Sovereign market: and the central bank's person hands over the fiat unit, outside the contract" "tagged(cb, fiat_handed(frank, 1))" "$n4_sov"
check "Sovereign market: the sovereign coin paid for fiat is back with its issuer" "tagged(cb, holdings(\[lot(cb, usd, now, 21), lot(diana, usd, now, 10)\]))" "$n4_sov"
check "Sovereign market: accept --- the farmer, holding a sovereign coin, accepts up to 5 in payment" "tagged(bob, accepted(cb, usd, 5))" "$n4_sov"
check "Sovereign market: and the household, holding one, accepts up to 5" "tagged(frank, accepted(cb, usd, 5))" "$n4_sov"
check "Sovereign market: payout --- the central bank pays the household 4 of its own coins" "tagged(cb, paid_out(frank, 4))" "$n4_sov"
check "Sovereign market: and the household takes them against 4 of its acceptances" "tagged(frank, received_from(cb, cb, 4))" "$n4_sov"
check "Sovereign market: leaving 1" "tagged(frank, acceptances(\[accepting(cb, usd, 1)\]))" "$n4_sov"
check "Sovereign market: spend --- the household pays 3 sovereign coins to the farmer, whom it holds no conversation with" "tagged(frank, spent_at(bob, cb, 3))" "$n4_sov"
check "Sovereign market: and the farmer takes them against 3 of its acceptances" "tagged(bob, received_from(frank, cb, 3))" "$n4_sov"
check "Sovereign market: leaving 2" "tagged(bob, acceptances(\[accepting(cb, usd, 2)\]))" "$n4_sov"
check "Sovereign market: and the issuer countersigns and records the spend" "tagged(cb, countersigned(frank, bob, 3))" "$n4_sov"
check "Sovereign market: mature --- a bond presented before its issuer's date reaches its maturity is returned" "tagged(diana, not_mature(frank, 28))" "$n4_sov"
check "Sovereign market: and taken for a coin once it has" "tagged(diana, matured(frank, 28))" "$n4_sov"
check_not "Sovereign market: Mature records no date warranty, so no warranty( line is logged" "warranty(" "$n4_sov"
check "Sovereign market: Advance is the next day, so the day before is logged too" "tagged(bob, date_advanced(24))" "$n4_sov"
check "Sovereign market: one escrow released" "tagged(frank, received_transfer(release, escrow, \[lot(frank, usd, now, 5)\]))" "$n4_sov"
check "Sovereign market: and another returned" "tagged(frank, received_transfer(return, escrow, \[lot(eve, usd, now, 2)\]))" "$n4_sov"
check "Sovereign market: the central bank's final holdings" "tagged(cb, holdings(\[lot(cb, usd, now, 17), lot(diana, usd, now, 10)\]))" "$n4_sov"
check "Sovereign market: the community bank's final holdings" "tagged(diana, holdings(\[lot(cb, usd, now, 7), lot(bob, usd, 25, 24), lot(diana, usd, now, 11), lot(frank, usd, 28, 12), lot(frank, usd, now, 1)\]))" "$n4_sov"
check "Sovereign market: the farmer's final holdings" "tagged(bob, holdings(\[lot(alice, usd, now, 10), lot(diana, usd, now, 18), lot(cb, usd, now, 5)\]))" "$n4_sov"
check "Sovereign market: the household's final holdings" "tagged(frank, holdings(\[lot(diana, usd, now, 6), lot(eve, usd, now, 10), lot(frank, usd, 28, 5), lot(frank, usd, now, 5), lot(cb, usd, now, 1)\]))" "$n4_sov"
check "Sovereign market: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n4_sov"
check "Sovereign market: the household's holdings, one community-bank coin spent up the chain and the sovereign coin paid back" "tagged(frank, holdings(\[lot(diana, usd, now, 6), lot(eve, usd, now, 10), lot(frank, usd, 28, 5), lot(frank, usd, now, 5)\]))" "$n4_sov"
check "Sovereign market: the four parties outside the sovereign layer are unmoved" "tagged(eve, holdings(\[lot(charlie, usd, now, 4), lot(frank, usd, now, 1), lot(alice, usd, now, 2), lot(bob, usd, now, 10)\]))" "$n4_sov"

echo "--- The live-person harness (sovereign_ui): the central bank on the app's screen ---"

# The entry the app starts and the Dart bridge runs, sovereign_ui/3
# (programs/currencies/sovereign/play_ui.glp), with no grant: its person
# stream is open and stays so, as a person channel never closes (vGLP,
# Definition "Person Channel, Person Writer, GLP with Persons, Grant";
# PersonIn ::= [_ | PersonIn]).  The central bank's request form --- the
# agent's question, one form per alternative of its twelve requests, Mint
# coins the first and Return the twelfth, each field labelled by the name of
# its declared type and a Key a peer field (vGLP, Definition "Widget
# Declaration, Default Widget") --- is drawn, and its two opened
# conversations reach its screen, by send_to_user/1, one term to a line, as
# the bridge reads them, while the peers' screens are tagged and nothing of
# theirs reaches the central bank's screen; with no grant the peers mint and
# wait for the line.
n4_ui=$("$REPL_RUN" <<HEREDOC
$SOV
:limit 50000000
sovereign_ui(cb, G?, []).
:quit
HEREDOC
2>&1)

check "Sovereign harness: no runtime error" "→ suspended" "$n4_ui"
check_not "Sovereign harness: no failed goal" "ERROR" "$n4_ui"
check "Sovereign harness: the central bank's Mint coins form stands" "draw([0-9][0-9]*, input_box(menu(\[form(mint, \[field(Denomination, text), field(Amount, number)\]), " "$n4_ui"
check "Sovereign harness: and its Return form, the twelfth" "input_box(menu(\[\(form([a-z]*, \[[A-Za-z(), ]*\]), \)\{11\}form(return, \[field(Depositor, peer), field(Denomination, text), field(Issuer, peer), field(Due, text), field(Amount, number)\])\])), stream_request_r(input(\[\])))" "$n4_ui"
check "Sovereign harness: its conversation with the community bank reaches its screen" "^draw(0, thread(.*), output(\\[.*opened(diana)" "$n4_ui"
check "Sovereign harness: and with the household" "^draw(0, thread(.*), output(\\[.*opened(frank)" "$n4_ui"
check "Sovereign harness: the peers' screens are tagged" "tagged(diana, minted(10, now))" "$n4_ui"
check_not "Sovereign harness: and nothing of theirs reaches the central bank's screen" "msg(agent, person, minted(10, now))" "$n4_ui"

# The same run with the central bank's taps scripted (play_sovereign_ui): its
# four acts --- the mint, the mutual credit line, the chain redemption on the
# peers' logs, and the payment of its coin back to it for a fiat unit handed
# over outside the contract --- reach its screen, and Diana's reverse swap
# arrives as the card of its swap responder and is accepted.
n4_uip=$("$REPL_RUN" <<HEREDOC
$SOV
:limit 50000000
play_sovereign_ui.
:quit
HEREDOC
2>&1)

check "Sovereign harness, scripted: no runtime error" "→ suspended" "$n4_uip"
check_not "Sovereign harness, scripted: no failed goal" "ERROR" "$n4_uip"
check "Sovereign harness, scripted: act 1, the central bank mints its own coins" "tagged(cb, minted(30, now))" "$n4_uip"
check "Sovereign harness, scripted: and holds them" "tagged(cb, holdings(\[lot(cb, usd, now, 30)\]))" "$n4_uip"
check "Sovereign harness, scripted: act 2, the mutual credit line willed by the central bank" "tagged(cb, swap_done(diana))" "$n4_uip"
check "Sovereign harness, scripted: and by the community bank" "tagged(diana, swap_done(cb))" "$n4_uip"
check "Sovereign harness, scripted: each holds coins of the other" "tagged(cb, holdings(\[lot(cb, usd, now, 20), lot(diana, usd, now, 10)\]))" "$n4_uip"
check "Sovereign harness, scripted: act 3, the chain redemption leaves the household holding a sovereign coin" "tagged(frank, redeemed(diana, cb, now))" "$n4_uip"
check_not "Sovereign harness, scripted: a redemption warrants no date" "tagged(frank, warranty(" "$n4_uip"
check "Sovereign harness, scripted: and gives the coin out of the line" "tagged(diana, presented(frank, cb, now))" "$n4_uip"
check "Sovereign harness, scripted: the household's holdings with the sovereign coin" "tagged(frank, holdings(\[lot(diana, usd, now, 6), lot(frank, usd, 28, 5), lot(cb, usd, now, 1)\]))" "$n4_uip"
check "Sovereign harness, scripted: act 4, the household pays the sovereign coin to the central bank" "tagged(frank, paid(cb, 1))" "$n4_uip"
check "Sovereign harness, scripted: and the central bank's person hands over the fiat unit, outside the contract" "tagged(cb, fiat_handed(frank, 1))" "$n4_uip"
check "Sovereign harness, scripted: the coin paid for fiat is back with its issuer" "tagged(cb, holdings(\[lot(cb, usd, now, 21), lot(diana, usd, now, 10)\]))" "$n4_uip"
check "Sovereign harness, scripted: the reverse swap, proposed by the community bank and accepted on the card" "tagged(cb, holdings(\[lot(cb, usd, now, 30), lot(diana, usd, now, 1)\]))" "$n4_uip"

echo "--- The denominated credit line (credit_line), Proposition prop:credit-line ---"

n4_cl=$("$REPL_RUN" <<HEREDOC
$SOV
:limit 50000000
credit_line.
:quit
HEREDOC
2>&1)

check "Denominated credit line: no runtime error" "→ suspended" "$n4_cl"
check_not "Denominated credit line: no failed goal" "ERROR" "$n4_cl"
check "Denominated credit line: establishment, the escrow agent holds the limit" "tagged(escrow, holdings(\[lot(alice, usd, now, 20)\]))" "$n4_cl"
check "Denominated credit line: the draw is a packaged exchange, both lots held at once" "tagged(escrow, holdings(\[lot(alice, usd, now, 20), lot(bob, usd, 25, 8), lot(bob, usd, 20, 2)\]))" "$n4_cl"
check "Denominated credit line: drawn 8, the escrow agent holds the undrawn coins" "tagged(escrow, holdings(\[lot(alice, usd, now, 12)\]))" "$n4_cl"
check "Denominated credit line: the borrower holds the coins lent" "tagged(bob, holdings(\[lot(alice, usd, now, 8)\]))" "$n4_cl"
check "Denominated credit line: at expiry the undrawn coins return to the lender" "tagged(alice, received_transfer(return, escrow, \[lot(alice, usd, now, 20)\]))" "$n4_cl"
check "Denominated credit line: at expiry with nothing drawn the lender holds its coins and no principal bond" "tagged(alice, holdings(\[lot(bob, usd, 20, 2), lot(alice, usd, now, 20)\]))" "$n4_cl"
check "Denominated credit line: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n4_cl"

echo "--- The denominated mini-app's own plays (play_mint, play_swap) ---"

n4_pm=$("$REPL_RUN" <<HEREDOC
$SOVD
:limit 5000000
play_mint(S).
:quit
HEREDOC
2>&1)

check_not "Denominated mini-app play_mint: no failed goal" "ERROR" "$n4_pm"
check "Denominated mini-app play_mint: alice mints 3 of her own" "S = \[minted(3, now), holdings(\[lot(alice, usd, now, 3)\])" "$n4_pm"

n4_ps=$("$REPL_RUN" <<HEREDOC
$SOVD
:limit 5000000
play_swap(A, B).
:quit
HEREDOC
2>&1)

check_not "Denominated mini-app play_swap: no failed goal" "ERROR" "$n4_ps"
check "Denominated mini-app play_swap: the proposer's screen, her conversation opened and the swap done" "A = \[opened(bob), minted(2, now), holdings(\[lot(alice, usd, now, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, usd, now, 2)\])" "$n4_ps"
check "Denominated mini-app play_swap: the counterparty's screen" "B = \[opened(alice), minted(2, now), holdings(\[lot(bob, usd, now, 2)\]), swap_done(alice), holdings(\[lot(alice, usd, now, 2)\])" "$n4_ps"
echo ""

fi  # Section N4

if _section VG; then
# =============================================================================
# Section VG: The canonical compilation on one-clause procedures
# (programs/tests/vglp/one_clause): every procedure of responder.vglp is
# volitional with one clause, its question in writer mode, the offering peer
# shown (vGLP, Definition "Volitional Procedure, Interactive Type, Interactive
# Term, Ordinary Clause, vGLP Program").  The source compiles on load (no .glp
# beside it), and each play runs it through the dispatcher and the construct
# processes with a scripted person, who sees each draw and grants input(Id,
# Q, R) at the position the view marks (vGLP, Section 6.3, "The canonical
# compilation"); a play gives the decisions D and the person's
# log L.  The person's answer reaches its clause; a refusal is an answer, no,
# which the clause takes; a grant that forms no term of the question's type
# answers nothing, and the next one answers it; greet's card is drawn before
# the person gives a name, its clause guarded only on what the program
# writes; pick's one clause takes the person's choice.  Until 2026-10-12 the
# fixture was in the old syntax and its plays spoke the old compilation's
# mediator, cards and slots, with an else-branch selected by a decline and a
# play in which an ordinary clause aborted pick's open ask, which the paper
# has no counterpart for (GLP, 2026-10-11 12:59 UTC, "03:24").
# =============================================================================
echo "=== Section VG: Canonical compilation, one-clause procedures ==="
echo ""

ONECLAUSE="$GLP_DIR/programs/tests/vglp/one_clause"
VG_CARD="drawn(1, form(card, \[field('Peer', shown), field('YesNo', buttons(\[yes, no\]))\]), card(bob, input(\[2\]))), withdrawn(1) |"

vg_load=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
:quit
HEREDOC
2>&1)
check "One-clause program loads, the .vglp compiled on load" "Loaded program" "$vg_load"
check_not "One-clause program no type errors" "Type checking failed" "$vg_load"

vg_answer=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_answer(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: answer, no failed goal" "ERROR" "$vg_answer"
check "One-clause: the answer reaches its clause" "D = \[decided(yes, bob)\]" "$vg_answer"
check "One-clause: the card is drawn, the offering peer shown and the person's position marked, and withdraws once answered" "L = \[drawn(0, thread(.*), \[\]), $VG_CARD" "$vg_answer"

vg_decline=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_decline(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: decline, no failed goal" "ERROR" "$vg_decline"
check "One-clause: the person's decline is an answer, no, which the clause takes" "D = \[decided(no, bob)\]" "$vg_decline"
check "One-clause: the declined card withdraws once answered" "L = \[drawn(0, thread(.*), \[\]), $VG_CARD" "$vg_decline"

vg_mismatch=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_mismatch(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: mismatch, no failed goal" "ERROR" "$vg_mismatch"
check "One-clause: a grant that forms no YesNo answers nothing, the next one reaches its clause" "D = \[decided(yes, bob)\]" "$vg_mismatch"
check "One-clause: the card stays through the refused grant and withdraws once answered" "L = \[drawn(0, thread(.*), \[\]), $VG_CARD" "$vg_mismatch"

vg_greet=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_greet(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: greet, no failed goal" "ERROR" "$vg_greet"
check "One-clause: greet's card is drawn before the person gives a name, its clause guarded on what the program writes" "L = \[drawn(0, thread(.*), \[\]), drawn(1, form(greeting, \[field('Peer', shown), field('Name', text)\]), greeting(bob, input(\[2\]))), withdrawn(1) |" "$vg_greet"
check "One-clause: the name the person gives reaches greet's clause" "D = \[greeted(alice, bob)\]" "$vg_greet"

vg_pick=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_pick(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: pick, no failed goal" "ERROR" "$vg_pick"
check "One-clause: pick's card is drawn, the offering peer shown" "L = \[drawn(0, thread(.*), \[\]), drawn(1, form(choice, \[field('Peer', shown), field('Constant', text)\]), choice(bob, input(\[2\]))), withdrawn(1) |" "$vg_pick"
check "One-clause: pick's one clause takes the person's choice" "D = \[picked(go)\]" "$vg_pick"
echo ""

# -----------------------------------------------------------------------------
# Section VG, continued: recursive interactive types with no mode inversion
# (programs/tests/vglp/recursive; vGLP 092c9fe, Definition "Volitional
# Procedure, Interactive Type": "a named type in it is recursive only if it
# has no mode inversion", and the paragraph after it: "With no mode
# inversion, a recursive type is written whole by the program or whole by the
# person"; vGLP #14 Cowork, 2026-10-18 11:50 UTC, Udi approved).  SpeechAct
# ::= speech(Utterance, Key) and Utterance ::= text(String) ; SpeechAct are
# recursive through each other: the person writes a speech act whole, formed
# by recursion on the type's definition (play_say; a grant that forms none
# answers nothing, play_say_refused), and the program writes one whole, drawn
# once written (play_forward).  The widget of a recursive type is finite by a
# reference back to itself, rec(T, W) binding T to W around the ref(T) in it.
# -----------------------------------------------------------------------------
echo "--- Recursive interactive types (recursive) ---"

RECURSIVE="$GLP_DIR/programs/tests/vglp/recursive"
VG_SAY="rec('SpeechAct', form(speech, \[field('Utterance', rec('Utterance', menu(\[form(text, \[field('String', text)\]), form(speech, \[field('Utterance', ref('Utterance')), field('Key', peer)\])\]))), field('Key', peer)\]))"
VG_FORWARD="form(forward, \[field('SpeechAct', rec('SpeechAct', form(speech, \[field('Utterance', rec('Utterance', menu(\[form(text, \[field('String', shown)\]), form(speech, \[field('Utterance', ref('Utterance')), field('Key', shown)\])\]))), field('Key', shown)\]))), field('YesNo', buttons(\[yes, no\]))\])"

vg_rec_load=$("$REPL_RUN" <<HEREDOC
$RECURSIVE
:quit
HEREDOC
2>&1)
check "Recursive: the program loads, the .vglp compiled on load" "Loaded program" "$vg_rec_load"
check_not "Recursive: no type errors" "Type checking failed" "$vg_rec_load"

vg_say=$("$REPL_RUN" <<HEREDOC
$RECURSIVE
play_say(L).
:quit
HEREDOC
2>&1)
check_not "Recursive: say, no failed goal" "ERROR" "$vg_say"
check "Recursive: the person's speech act, signed twice, is formed whole from one grant and reaches the program; its widget is finite by rec and ref, and the construct withdraws" "L = \[drawn(1, $VG_SAY, input(\[\])), said(speech(speech(text(hi), alice), bob)), withdrawn(1) |" "$vg_say"

vg_say_refused=$("$REPL_RUN" <<HEREDOC
$RECURSIVE
play_say_refused(L).
:quit
HEREDOC
2>&1)
check_not "Recursive: refused grant, no failed goal" "ERROR" "$vg_say_refused"
check "Recursive: a grant that forms no speech act answers nothing, and the next one answers" "L = \[drawn(1, $VG_SAY, input(\[\])), said(speech(text(hi), carol)), withdrawn(1) |" "$vg_say_refused"

vg_forward=$("$REPL_RUN" <<HEREDOC
$RECURSIVE
play_forward(L).
:quit
HEREDOC
2>&1)
check_not "Recursive: forward, no failed goal" "ERROR" "$vg_forward"
check "Recursive: the program's speech act, signed twice, is drawn whole beside the person's yes or no, and answered" "L = \[drawn(1, $VG_FORWARD, speech(speech(text(hi), alice), bob), input(\[2\])), forwarded(yes), withdrawn(1) |" "$vg_forward"
echo ""

# -----------------------------------------------------------------------------
# Section VG, continued: the compiler from an act-schema contract to its
# canonical CVA platform in vGLP (programs/vglp/cva; vGLP, Section 3.2,
# Definition "Canonical CVA Platform", two-phase commit coordinated by the
# initiating agent, and Section 5.4, "The compilation" and the paragraph on
# act schemas, a class the union of its parts, each with the people in the marked roles of its schema-bindings as its guard set, vGLP 099f876; vGLP's Task C, reworked to the five
# handlers of vGLP 8d56915 and the answers of 158ec9b; Answer recording the
# person's answer and Vote voting on it, vGLP c3b2191; wound-wait,
# Commit and Yield, attempts numbered and ranked, vGLP 49cd9f7; no clock and
# no tick, the agent taking its own transactions when it starts and after each
# trigger, one at a time until none holds, vGLP efe7bbc).  GSC's social graph
# and currency (their owners', programs/gsc/GSG.glp and
# programs/currencies/coins/GC.glp) and vGLP's nudge, introduce, gnudge, childsafe, link, friends, waves,
# parts, opens and groups (programs/vglp/cva/contracts.glp) compiled, the committed modules held to
# what the compiler prints; the contracts it refuses, not effective, and those it
# compiles whatever their guards; and the plays of each platform, two agents
# or three with scripted persons paced on their screens, each run on the
# committed module through the canonical compilation until it suspends, each
# agent having taken its own transactions until none holds and no trigger
# being left, the logs' tails unbound.  A play gives each person's log, the views its agent writes, view(S, K), the machine state and the persons it knows, each once, with menu(K, Ks) where the person looks and gave(K, A) where the person answers an approval question (vGLP b7e3517, Section 5.4, "The compilation"); and the network's log of the cargoes it carries, sent(From, To, Cargo), where the attempts, their numbers and ranks, the votes and the outcomes show.  A person whose attempt closes without committing sees its notice, refused(P, Q) or yielded(P), and makes the request again (vGLP c44769f, Section 5.4, "The compilation"; programs/vglp/cva/self.glp).
# -----------------------------------------------------------------------------
echo "--- The canonical CVA platform of an act-schema contract ---"
CVA="$GLP_DIR/programs/vglp/cva"

# Guarded: set -e would end the run without totals on a red compile.sh.
cva_print=$(bash "$CVA/compile.sh" --check 2>&1 || true)
check "CVA compiler: the social graph's platform is what the compiler prints" "social_graph/platform.vglp: as printed" "$cva_print"
check "CVA compiler: the currency's platform, each part guarded by the people its schema-binding marks, is what the compiler prints" "currency/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's nudge, certified by GSC's check/2, compiled to the platform it prints" "nudge/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's introduce, certified by GSC's check/2, compiled to the platform it prints" "introduce/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's gnudge, certified by GSC's check/2, compiled to the platform it prints" "gnudge/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's childsafe, certified by GSC's check/2, compiled to the platform it prints" "childsafe/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's link, certified by GSC's check/2, compiled to the platform it prints" "link/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's friends, its introduction predicate friend, compiled to the platform it prints" "friends/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's waves, two classes of the same participants and atoms, compiled to the platform it prints" "waves/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's parts, a class of two parts, compiled to the platform it prints" "parts/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's opens, a class of two unguarded parts with different openers, compiled to the platform it prints" "opens/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's groups, close taking the listed role a[listed(*, n)], certified by GSC's check/2, compiled to the platform it prints (vGLP f5f1644)" "groups/platform.vglp: as printed" "$cva_print"
check_not "CVA compiler: no contract refused or missing" "^fault\|^missing\|NOT as printed" "$cva_print"

cva_refuse=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(two_reqs, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(r1, [])], []), rs(guarded, [atom(y, [])], [], [atom(r2, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [atom(r3, [])], []), rs(guarded, [atom(y, [])], [], [atom(r4, [])], [])])], syntactically_grassroots).
compile(free_x, [schema(s, [pvar(x)], [rs(guarded, [atom(x1, [])], [], [atom(r, [pvar(x)])], []), rs(guarded, [atom(y1, [])], [], [atom(t, [pvar(x)])], [])])], syntactically_grassroots).
compile(uneff, [schema(u, [], [rs(unguarded, [atom(x, [role(2)])], [], [], []), rs(unguarded, [atom(y, [role(1)])], [], [], [])])], syntactically_grassroots).
compile(gossip, [], not_grassroots([untraceable([p])])).
compile(nointro, [], not_grassroots([no_introductory_act])).
compile(federated, [], conditions_met).
compile(one_req, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(r1, [])], []), rs(guarded, [atom(y, [])], [], [atom(r2, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [atom(r3, [])], []), rs(guarded, [atom(y, [])], [], [atom(r2, [])], [])])], syntactically_grassroots).
compile(two_gsets, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], []), rs(unguarded, [atom(z, [])], [], [], [])]), schema(s2, [], [rs(unguarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], []), rs(guarded, [atom(z, [])], [], [], [])])], syntactically_grassroots).
compile(unguarded_reqs, [schema(u1, [], [rs(unguarded, [atom(x, [role(2)])], [], [atom(r, [role(2)])], []), rs(unguarded, [atom(y, [role(1)])], [], [atom(w, [role(1)])], [])]), schema(u2, [], [rs(unguarded, [atom(y, [role(2)])], [], [atom(w, [role(2)])], []), rs(unguarded, [atom(x, [role(1)])], [], [], [])])], syntactically_grassroots).
compile(refl_own, reflexive([f], [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(f, [role(1)])], []), rs(guarded, [atom(y, [])], [], [atom(f, [role(2)])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], [])])]), syntactically_grassroots).
compile(refl_not, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(f, [role(1)])], []), rs(guarded, [atom(y, [])], [], [atom(f, [role(2)])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], [])])], syntactically_grassroots).
compile(contra, [schema(s1, [], [rs(guarded, [atom(x, []), atom(x2, [])], [], [atom(w, [])], []), rs(guarded, [atom(y, [])], [], [atom(w, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], [atom(k, [role(3)])]), rs(guarded, [atom(y, [])], [], [], []), rs(guarded, [atom(x2, [])], [], [atom(k, [role(1)])], [])])], syntactically_grassroots).
compile(contra_live, [schema(s1, [], [rs(guarded, [atom(x, []), atom(x2, [])], [], [atom(w, [])], []), rs(guarded, [atom(y, [])], [], [atom(w, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], [atom(k, [role(2)])]), rs(guarded, [atom(y, [])], [], [], []), rs(guarded, [atom(x2, [])], [], [atom(k, [role(1)])], [])])], syntactically_grassroots).
compile(marks, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(r1, [])], []), rs(unguarded, [atom(y, [])], [], [], [])]), schema(s2, [], [rs(unguarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], [])])], syntactically_grassroots).
compile(marks_same, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(r1, [])], []), rs(unguarded, [atom(y, [])], [], [], [])]), schema(s2, [], [rs(unguarded, [atom(x, [])], [], [atom(r1, [])], []), rs(guarded, [atom(y, [])], [], [], [])])], syntactically_grassroots).
compile(refl_own_marks, reflexive([f], [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(f, [role(1)])], []), rs(guarded, [atom(y, [])], [], [atom(f, [role(2)])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], []), rs(unguarded, [atom(y, [])], [], [], [])])]), syntactically_grassroots).
compile(refl_not_marks, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(f, [role(1)])], []), rs(guarded, [atom(y, [])], [], [atom(f, [role(2)])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], []), rs(unguarded, [atom(y, [])], [], [], [])])], syntactically_grassroots).
compile(contra_marks, [schema(s1, [], [rs(guarded, [atom(x, []), atom(x2, [])], [], [atom(w, [])], []), rs(guarded, [atom(y, [])], [], [atom(w, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], [atom(k, [role(3)])]), rs(unguarded, [atom(y, [])], [], [], []), rs(guarded, [atom(x2, [])], [], [atom(k, [role(1)])], [])])], syntactically_grassroots).
compile(contra_live_marks, [schema(s1, [], [rs(guarded, [atom(x, []), atom(x2, [])], [], [atom(w, [])], []), rs(guarded, [atom(y, [])], [], [atom(w, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [], [atom(k, [role(2)])]), rs(unguarded, [atom(y, [])], [], [], []), rs(guarded, [atom(x2, [])], [], [atom(k, [role(1)])], [])])], syntactically_grassroots).
compile(cover, [schema(u, [], [rs(guarded, [], [atom(f, [role(2)])], [], []), rs(unguarded, [], [atom(f, [role(1)])], [], [])]), schema(v, [], [rs(guarded, [], [atom(f, [role(2)])], [atom(a, [role(2)])], []), rs(unguarded, [], [atom(f, [role(1)])], [], [])])], syntactically_grassroots).
compile(sym, [schema(unf, [], [rs(guarded, [], [atom(f, [role(2)])], [], []), rs(unguarded, [], [atom(f, [role(1)])], [], [])])], syntactically_grassroots).
compile(asym, [schema(unf, [], [rs(guarded, [], [atom(f, [role(2)])], [], [atom(r, [])]), rs(unguarded, [], [atom(f, [role(1)])], [], [])])], syntactically_grassroots).
compile(sgnread, [schema(d, [svar(y)], [rs(unguarded, [atom(done, [svar(y), role(2)])], [], [atom(posted, [svar(y)]), atom(listed, [role(2)])], [atom(done, [svar(y), role(2)])]), rs(unguarded, [atom(item, [sgn(y, 1)])], [], [], [])])], syntactically_grassroots).
compile(sgnread_not, [schema(d, [svar(y)], [rs(unguarded, [atom(done, [svar(y), role(2)])], [], [atom(listed, [role(2)])], [atom(done, [svar(y), role(2)])]), rs(unguarded, [atom(item, [sgn(y, 1)])], [], [], [])])], syntactically_grassroots).
compile(inert, [schema(s, [], [rs(guarded, [atom(x, [])], [], [], []), rs(unguarded, [], [], [], [])])], malformed([changes_nothing(s, 2)])).
compile(headless, [], malformed([not_in_head(s, svar(x))])).
compile(listed_named, [cschema(cl, [svar(n)], [], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(wards_named, [cschema(cl, [svar(n)], [], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots_with_wards(enter, [m], [guardianship(m, i, g, w)], [relay(r, ward(1, m, 2), free(2))], [])).
compile(listed_two, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], []), crs(listed(1, invited, [svar(n)]), unguarded, [], [atom(guest, [role(1), svar(n)])], [], [])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(seated_role, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(seated(ext(own(1), n), member), unguarded, [], [atom(member, [nterm(ext(own(1), n))])], [], [])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(name_term, [schema(cr, [svar(n)], [rs(guarded, [atom(member, [nterm(ext(own(1), n))]), atom(led, [svar(n)])], [], [], [atom(member, [nterm(ext(own(1), n))])])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(name_var, [schema(cr, [svar(n)], [rs(guarded, [atom(member, [nterm(ext(nvar(z), n))]), atom(led, [svar(n)])], [], [], [])])], conditions_met).
compile(reached, [cschema(cl, [svar(n)], [reach(own(1), friend, own(2))], [rs(guarded, [atom(closed, [svar(n)])], [], [], []), rs(unguarded, [atom(seen, [svar(n)])], [], [], [])])], conditions_met).
compile(listed_guard, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(listed_second, [schema(cl, [svar(n)], [rs(unguarded, [atom(seen, [svar(n)])], [], [atom(friend, [role(2)])], []), rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(2), svar(n)])], [atom(friend, [role(1)])], []), crs(listed(2, listed, [svar(n)]), unguarded, [], [atom(member, [role(2), svar(n)])], [], [])])], syntactically_grassroots_with_seated_and_listed_roles).
compile(order, [schema(post, [svar(x), svar(n)], [rs(guarded, [], [], [atom(member, [role(2), svar(n)])], []), rs(unguarded, [atom(posted, [svar(x), svar(n)])], [], [atom(member, [role(2), svar(n)])], [atom(posted, [svar(x), svar(n)])])])], syntactically_grassroots).
:quit
HEREDOC
2>&1)
# None of the locality conditions stays (vGLP 099f876, Section 5.4, "Act
# schemas as local specifications": each schema-binding determines a part of
# its class, with the people in its marked roles as its guard set, and "Such a
# contract is a local specification"; vGLP #15, Code_inbox.md, 2026-10-19
# 10:50 UTC, item 2): a class is the union of its parts, and neither equal
# marked sets nor simple guards are asked.
check "CVA compiler: two schemas of one class requiring different atoms at both participants, marking the same people, compiled: the class has two parts" "%% the canonical CVA platform of the contract(two_reqs)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(two_reqs)" "$cva_refuse"
# Effective (vGLP c0bea08, Section 5.4): every variable of a schema that occurs
# in an atom it requires or forbids also occurs in an atom it deletes or adds,
# a variable inside a signed term included; and the atoms each unguarded schema
# requires at its first role name every other role and every variable of its
# deleted and added atoms, y read through sgn(y, 1) (vGLP #14, Code_inbox.md,
# 2026-10-18 21:47 UTC, answer 2).
check "CVA compiler: a variable of atoms required at two participants and of no deleted or added atom, refused as not effective" "%% not compiled(free_x)" "$cva_refuse"
check "CVA compiler: ... the schema and the variable named" "%% the schema and the variable(s, pvar(x))" "$cva_refuse"
check "CVA compiler: an unguarded schema whose first role names no other role, refused as not effective" "%% the schema(u)" "$cva_refuse"
check "CVA compiler: an unguarded schema whose first role requires an atom naming y, y in an added atom through the signed term sgn(y, 1), compiled (sgnread)" "%% the canonical CVA platform of the contract(sgnread)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(sgnread)" "$cva_refuse"
check "CVA compiler: ... its first role requiring no atom naming y, refused as not effective (sgnread_not)" "%% not compiled(sgnread_not)" "$cva_refuse"
check "CVA compiler: ... the schema named" "%% the schema(d)" "$cva_refuse"
check "CVA compiler: a contract without traceable provenance is not compiled" "%% not compiled(gossip)" "$cva_refuse"
check "CVA compiler: a contract whose one fault is that it has no unobstructed introductory act, traceable provenance and volition met, compiled (vGLP, Section 5.4, asks locality and effectiveness alone; CSSN, Appendix B.4)" "%% the canonical CVA platform of the contract(nointro)" "$cva_refuse"
check "CVA compiler: a contract with community roles is not compiled" "%% not compiled(federated)" "$cva_refuse"
# A verdict malformed, of a schema's form (GSC, Definition "Act schema", as programs/gsc/self.glp writes its faults), is refused whole, its faults of the compiler's Verdict type.
check "CVA compiler: a contract malformed, a role that is not guarding changing nothing, is not compiled" "%% not compiled(inert)" "$cva_refuse"
check "CVA compiler: ... the verdict named" "%% it is malformed; check_named in programs/gsc gives the faults." "$cva_refuse"
check "CVA compiler: a contract malformed, a variable not in its schema's head (GSC 0228e79), is not compiled, the fault of the compiler's Verdict type" "%% not compiled(headless)" "$cva_refuse"
# The verdicts on an agent-named contract with community roles (programs/gsc/self.glp at GLP 96908b2d), of the compiler's Verdict type; its listed roles taken and its seated and constituent roles refused (vGLP f5f1644, Section 5.4, "Act schemas as local specifications"; vGLP #15 Cowork to Code, 2026-10-19 14:24 UTC, item 1).
check "CVA compiler: the verdict syntactically_grassroots_with_seated_and_listed_roles, a schema of a listed role, compiled" "%% the canonical CVA platform of the contract(listed_named)" "$cva_refuse"
check "CVA compiler: the verdict syntactically_grassroots_with_wards, a schema of a listed role, compiled" "%% the canonical CVA platform of the contract(wards_named)" "$cva_refuse"
check "CVA compiler: a schema of two listed roles, refused" "%% it has a schema with more than one listed role, which this compiler does not take." "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(listed_two)" "$cva_refuse"
check "CVA compiler: a seated role, refused" "%% it has seated or constituent roles, which vGLP Section 5.4 does not take: it takes listed roles (vGLP f5f1644)." "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(seated_role)" "$cva_refuse"
# "a name term <u>.y_1...y_n is the community name of the person bound to u followed by the speech acts bound to y_1, ..., y_n" (vGLP c649c0d, Section 5.4, "Act schemas as local specifications"); a name variable, which a seated role would need, stays out with seated roles (vGLP #15 Cowork to Code, 2026-10-19 14:49 UTC).
check "CVA compiler: a name term built on a role, in an atom, compiled" "%% the canonical CVA platform of the contract(name_term)" "$cva_refuse"
check "CVA compiler: ... the atom holding a community name, one argument" "^member(CName)$" "$cva_refuse"
check "CVA compiler: a name term built on a name variable, refused" "%% it has name variables, which vGLP Section 5.4 does not take, as it does not take seated roles." "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(name_var)" "$cva_refuse"
check_not "CVA compiler: no contract refused for a name term built on a role" "it has name terms" "$cva_refuse"
check "CVA compiler: reach conditions, refused" "%% it has reach conditions, which vGLP Section 5.4 does not take." "$cva_refuse"
# "a schema with a listed role has the base of that role as its only guarding role or, if it has none, as its first role" (vGLP f5f1644, Section 5.4, "Act schemas as local specifications"; vGLP #15, item 3), as CSSN's child_close, guarded by the parent and listing at the child, does not.
check "CVA compiler: a schema with a listed role guarded by a role other than its base, refused as not effective" "%% listed role whose base is not its only guarding role, or, with no guarding role, its first role:" "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(listed_guard)" "$cva_refuse"
check "CVA compiler: an unguarded schema with a listed role whose base is not its first role, refused as not effective" "%% not compiled(listed_second)" "$cva_refuse"
# A class's arguments are its schema's roles and then its variables in the order of its head (GSC 0228e79, Definition "Act schema"; vGLP #15 Cowork to Code, 2026-10-19 13:34 UTC, item 2): post(p?, a ; x, n), whose first atom names n before x, is post(p, a, x, n).
check "CVA compiler: a class's arguments in the order of the head, post(p?, a ; x, n) as post(V1, V2, V3, V4), x the third" "^binding_of(post(V1, V2, V3, V4), post, \[b(role(1), p(V1?)), b(role(2), p(V2?)), b(svar(x), s(V3?)), b(svar(n), s(V4?))\])$" "$cva_refuse"
check "CVA compiler: requirements differing at one participant only, compiled" "%% the canonical CVA platform of the contract(one_req)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(one_req)" "$cva_refuse"
check "CVA compiler: a class whose parts are guarded by two sets not both of one person, compiled, simple guards not asked (vGLP 099f876)" "%% the canonical CVA platform of the contract(two_gsets)" "$cva_refuse"
check_not "CVA compiler: ... and no fault on its guards" "%% not compiled(two_gsets)\|does not have simple guards" "$cva_refuse"
check "CVA compiler: an unguarded class whose schema-bindings require different atoms at one participant, compiled, the old condition of equal requirements in an unguarded class gone" "%% the canonical CVA platform of the contract(unguarded_reqs)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(unguarded_reqs)" "$cva_refuse"
# Act schemas as local specifications (vGLP 099f876, Section 5.4): an atom of
# a reflexive predicate naming the participant holds there and requires
# nothing; a schema-binding whose two roles at one person require and forbid
# one atom there determines nothing and is of no class; and each schema-binding
# determines a part with the people in its marked roles as its guard set, so a
# class whose schema-bindings mark different people is compiled, each part
# with its own guard set.  The fixtures *_marks mark different people at their
# two schemas, which the condition of equal marked sets refused where a
# transaction of the class was determined by one alone, and which compile now.
check "CVA compiler: requirements differing at both participants only by the reflexive atom naming each, compiled" "%% the canonical CVA platform of the contract(refl_own)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(refl_own)" "$cva_refuse"
check "CVA compiler: ... the same, f not declared reflexive, marking the same people, compiled" "%% the canonical CVA platform of the contract(refl_not)" "$cva_refuse"
check "CVA compiler: ... the two schemas marking different people, the reflexive atoms naming each holding there, every transaction of the class determined by both, compiled (refl_own_marks)" "%% the canonical CVA platform of the contract(refl_own_marks)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(refl_own_marks)" "$cva_refuse"
check "CVA compiler: ... the same, f not declared reflexive, a transaction determined by one alone, compiled, its parts guarded apart (refl_not_marks)" "%% the canonical CVA platform of the contract(refl_not_marks)" "$cva_refuse"
check "CVA compiler: a schema-binding of two roles at one person requiring and forbidding one atom there is of no class, so the contract is local" "%% the canonical CVA platform of the contract(contra)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(contra)" "$cva_refuse"
check "CVA compiler: ... its two roles at one person satisfiable there, the schema-binding of the class, marking the same people, compiled (contra_live)" "%% the canonical CVA platform of the contract(contra_live)" "$cva_refuse"
check "CVA compiler: ... marking different people, the schema-binding of no class: compiled (contra_marks)" "%% the canonical CVA platform of the contract(contra_marks)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(contra_marks)" "$cva_refuse"
check "CVA compiler: ... marking different people, the schema-binding of the class, compiled, its part guarded apart (contra_live_marks)" "%% the canonical CVA platform of the contract(contra_live_marks)" "$cva_refuse"
check_not "CVA compiler: ... no schema-binding named against it" "%% and the schema-binding(s2(person(0), person(1), person(0)))" "$cva_refuse"
check "CVA compiler: two schema-bindings of one class marking different people, a transaction determined by one alone, compiled, a part each with its own guard set" "%% the canonical CVA platform of the contract(marks)" "$cva_refuse"
check_not "CVA compiler: ... the condition of equal marked sets gone" "its condition): the schema-bindings determining two transactions of one class mark" "$cva_refuse"
check "CVA compiler: ... marking different people and requiring the same atoms, compiled" "%% the canonical CVA platform of the contract(marks_same)" "$cva_refuse"
check "CVA compiler: a class of four schema-bindings, two marking p and two marking q, some requiring an atom the others do not, each transaction determined by one marking p and one marking q, compiled (cover)" "%% the canonical CVA platform of the contract(cover)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(cover)" "$cva_refuse"
check "CVA compiler: unfriending, one class guarded by either party, compiled (sym)" "%% the canonical CVA platform of the contract(sym)" "$cva_refuse"
check "CVA compiler: unfriending whose first role forbids an atom, a transaction determined by the binding of one party first alone, compiled, two parts guarded by one party each (asym)" "%% the canonical CVA platform of the contract(asym)" "$cva_refuse"
check_not "CVA compiler: ... no schema-binding named against it" "%% the schema-binding(unf(person(0), person(1)))" "$cva_refuse"

# The classes are the contract's equivalence, generated by relating two
# transactions when one schema-binding determines both (vGLP 9c819f5, Section
# 5.4; vGLP #14's task of 2026-10-18 12:24 UTC, C): two schema-bindings of the
# same participants and the same deleted and added atoms are linked when no
# atom one requires at a participant the other forbids there, and a class is
# the closure.  Twelve fixtures of the compiler's own: clash2 (two classes),
# chain3 (a third binding joins them, all marking the same people, compiled),
# clash2_marks and chain3_marks (the same, the two marking different people:
# two classes, and one, its parts guarded apart, both compiled),
# wards and wards_leaving (a verdict grassroots with wards compiles, A, with
# no leaving mark and with one, GSC's Appendix F), signed (a signed term in
# an added atom, the speech act bound to y signed by the person in role i,
# compiles, B), deadfree (a schema requiring and forbidding an atom of a
# variable of no deleted or added atom, not effective, vGLP c0bea08), sibling
# (CSSN's child role and adult role at one person, two classes),
# sibling_noclash (the adult role not forbidding child, one class, all marking
# the same people, compiled), and sibling_marks and sibling_noclash_marks (the
# same, the parent of the adult role not marked: two classes, and one, its
# parts guarded apart, both compiled).
cva_classes=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(clash2, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(guarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])])], syntactically_grassroots).
compile(chain3, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(guarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])]), schema(s3, [], [rs(guarded, [atom(z, [])], [], [], []), rs(guarded, [atom(w, [])], [], [], [])])], syntactically_grassroots).
compile(wards, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])])], grassroots_with_wards(enter, [m], [guardianship(m, i, g, w)], [relay(r, ward(1, m, 2), free(2))], [])).
compile(wards_leaving, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])])], grassroots_with_wards(enter, [m], [guardianship(m, i, g, w)], [relay(r, ward(1, m, 2), free(2))], [retired])).
compile(signed, [schema(fwd, [svar(x)], [rs(guarded, [atom(sent, [svar(x), role(2)])], [], [atom(item, [svar(x)])], []), rs(unguarded, [atom(item, [sgn(x, 1)])], [], [atom(friend, [role(1)])], [])])], syntactically_grassroots).
compile(deadfree, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [pvar(x)], [rs(guarded, [atom(z, [])], [], [atom(q, [pvar(x)])], [atom(q, [pvar(x)])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])])], syntactically_grassroots).
compile(sibling, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(child, []), atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(sibling_noclash, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(clash2_marks, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(unguarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(unguarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])])], syntactically_grassroots).
compile(chain3_marks, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(unguarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(unguarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])]), schema(s3, [], [rs(guarded, [atom(z, [])], [], [], []), rs(guarded, [atom(w, [])], [], [], [])])], syntactically_grassroots).
compile(sibling_marks, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(child, []), atom(friend, [role(1)])]), rs(unguarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(sibling_noclash_marks, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])]), rs(unguarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
:quit
HEREDOC
2>&1)
check "CVA compiler: two schema-bindings of the same participants and atoms, one requiring an atom the other forbids, are two classes (clash2)" "%% the canonical CVA platform of the contract(clash2)" "$cva_classes"
check "CVA compiler: a third schema-binding linking them joins one class, all marking the same people, compiled (chain3)" "%% the canonical CVA platform of the contract(chain3)" "$cva_classes"
check "CVA compiler: ... the two marking different people, two classes, compiled (clash2_marks)" "%% the canonical CVA platform of the contract(clash2_marks)" "$cva_classes"
check "CVA compiler: ... and the third joining them, one class, its parts guarded apart, compiled (chain3_marks)" "%% the canonical CVA platform of the contract(chain3_marks)" "$cva_classes"
check "CVA compiler: a contract grassroots with wards is compiled (wards)" "%% the canonical CVA platform of the contract(wards)" "$cva_classes"
check "CVA compiler: ... and one with a leaving mark, the verdict's fifth argument (GSC, Appendix F, Definition \"Leaving mark\"), compiled (wards_leaving)" "%% the canonical CVA platform of the contract(wards_leaving)" "$cva_classes"
check "CVA compiler: a signed term in an added atom, the speech act bound to y signed by the person in role i, compiles (signed)" "%% the canonical CVA platform of the contract(signed)" "$cva_classes"
check "CVA compiler: a schema requiring and forbidding an atom of a variable of no deleted or added atom, refused as not effective (deadfree)" "%% not compiled(deadfree)" "$cva_classes"
check "CVA compiler: ... the schema and the variable named" "%% the schema and the variable(s2, pvar(x))" "$cva_classes"
check "CVA compiler: CSSN's sibling case, a child role and an adult role at one person, two classes (sibling)" "%% the canonical CVA platform of the contract(sibling)" "$cva_classes"
check "CVA compiler: the adult role not forbidding child, one class, all marking the same people, compiled (sibling_noclash)" "%% the canonical CVA platform of the contract(sibling_noclash)" "$cva_classes"
check "CVA compiler: the parent of the adult role not marked, two classes, compiled (sibling_marks)" "%% the canonical CVA platform of the contract(sibling_marks)" "$cva_classes"
check "CVA compiler: ... and the adult role not forbidding child, one class, its parts guarded apart, compiled (sibling_noclash_marks)" "%% the canonical CVA platform of the contract(sibling_noclash_marks)" "$cva_classes"


# The compiled agent is no longer volitional: it asks for requests by request
# questions, goals of a volitional procedure whose question, Menu ::=
# menu(Key, List(Key), Request?), shows the agent's own key and the persons it
# knows, asked when the agent starts and again when the agent goal reads each
# answer (vGLP 321127a, Section 5.4; vGLP #14's task of 2026-10-18 21:22 UTC;
# vGLP ca78e11; vGLP 2026-10-16 13:53 UTC, C.1); and its agent goal writes on the program's output to its person, Output, construct 0, its view, view(S, K), the machine state S of its app state and the persons K it knows, when it starts and whenever either changes: "the compilation defines Output as the type of the views, and declares for the type Stream(Output) of the output a widget that draws the last view written" (vGLP b7e3517, Section 5.4, "The compilation"; vGLP #15, Code_inbox.md, 2026-10-19 10:55 UTC and 12:27 UTC).  A machine state is a list of the contract's atoms, Atom the union of the contract's predicates at their argument types, which the compiler prints from the contract, so that no Stream or List is under another (Definition "Volitional Procedure, Interactive Type"); the agent writes no transaction names, no outcome(At) and no discover(Q).  The plays' persons read the output from construct 0's draws, draw(0, W, output(T)), and from nowhere else.
for c in social_graph currency; do
    cva_agent=$(cat "$CVA/$c/platform.vglp")
    check "CVA $c: the agent is an ordinary procedure, no longer volitional" "^exported procedure agent(Peer?, Stream(Net)?, Stream(Net), Stream(Output))\.$" "$cva_agent"
    check "CVA $c: the request question, Menu, shows the agent's own key and the persons it knows and takes one request" "^Menu      ::= menu(Key, List(Key), Request?)\.$" "$cva_agent"
    check "CVA $c: ... asked by a volitional procedure" "^procedure (Menu)\*request(Key?, List(Key)?, Stream(Trigger))\.$" "$cva_agent"
    check "CVA $c: ... the key the agent's own identifier, when the agent starts" "^    request(Id?, \[\], Rq),$" "$cva_agent"
    check "CVA $c: ... and on reading each answer" "^    request(Id?, Ks?, R),$" "$cva_agent"
    check_not "CVA $c: no stream of requests as the agent's question" "(Stream(Request)?)\*agent" "$cva_agent"
    check "CVA $c: the program's output to its person is of the type of the views, a machine state, a list of the contract's atoms, and the persons the agent knows, and the notices (vGLP c44769f)" "^Output    ::= view(List(Atom), List(Key)) ; refused(Class, Key) ; yielded(Class)\.$" "$cva_agent"
    check "CVA $c: ... drawn by the widget last_state, which draws the last view written and the notices written after it" "^Stream(Output) =::= last_state\.$" "$cva_agent"
    check "CVA $c: a notice where Tally closes an attempt the person proposed on a vote no, refused(Part, Q)" "^    noticed(refused_by(F?), At?, Ap0?, St1?, Thr, Thr1?, St1n),$" "$cva_agent"
    check "CVA $c: ... and where Yield or the agent's own Wound closes it, yielded(Part)" "^    noticed(yielded_by, B?, Ap0?, St1?, Thr, Thr1?, St1n),$" "$cva_agent"
    check "CVA $c: ... only for the agent's own attempt of a guarded part, closed by the dispatch" "^    some_peer(G?, Gd),$" "$cva_agent"
    check "CVA $c: ... the view written when the agent starts: the empty machine state and no person known" "^agent(Id, NetIn, Out?, \[view(\[\], \[\])|Thr?\]) :- ground(Id?) |$" "$cva_agent"
    check "CVA $c: ... and the new view, where the machine state changes or a peer becomes known, its atoms as the contract writes them" "^show_view(yes, St, \[V?|Thr1?\], Thr1) :- view_of(St?, V)\.$" "$cva_agent"
    check "CVA $c: ... a peer becoming known at the second hello of an exchange" "^    show_known(Q?, St?, St1?, Thr, Thr1?, St2),$" "$cva_agent"
    check_not "CVA $c: ... no transaction names, no outcome(At) and no discover(Q) on the output" "^Txn \|discover_q\|\[discover(\|\[outcome(At" "$cva_agent"
    check_not "CVA $c: no session question" "Session" "$cva_agent"
done
cva_person=$(cat "$CVA/self.glp")
check "CVA plays: the person reads the views from construct 0" "^person(\[draw(0, _, output(T))|Ds\]" "$cva_person"
check_not "CVA plays: ... and no session" "session_w" "$cva_person"
check "CVA plays: the person reads the request questions, the agent's own key and the persons it knows" "^person(\[draw(Id, _, menu_w(menu(Me, Ks, input(P))))|Ds\]" "$cva_person"
check "CVA plays: the screen shows a notice written after the last view" "^shows(refused(P, Q), _, T, _, B?) :- ground(P?), ground(Q?), ground(T?) |$" "$cva_person"
check_not "CVA plays: no step makes a request again until the screen shows something" "again(" "$cva_person"
# A scripted person proposing a class that names its agent names it by the
# key the request question shows (vGLP 321127a, Section 5.4): its step own(Ks,
# R) holds a hole for each place R names the agent, and the person writes the
# key into each before it grants R.
check "CVA plays: a proposal naming the agent is granted once the person has written the key the question shows into its holes" "^advance(ps(sess(Id, P, Me, _), T, Cs, \[own(Ks, R)|Ss\])" "$cva_person"
check "CVA plays: ... each hole the agent's own key" "^fill(Me, \[Me?|Ks\]) :- ground(Me?) | fill(Me?, Ks?)\.$" "$cva_person"
check "CVA plays: alice establishes her identity by the key her request question shows" "pair(\[look, own(\[A1\], propose(establish(A1?)))" "$(cat "$CVA/social_graph/self.glp")"

# The request type has the alternative propose([t]_j) for each guarded part
# [t]_j and the alternative known(Q) (vGLP 099f876, Section 5.4, the
# compilation): Proposal, the alternatives of the schemas marking a role some
# binding of which determines a transaction (compiler/local.glp, proposable).
# nudge's nudge is unguarded and not proposed; gnudge's g is a guarded part of
# a class whose other part, nudge's, is unguarded, and is proposed (vGLP #15,
# Code_inbox.md, 2026-10-19 10:50 UTC, item 4).  A contract with no guarded
# part has the request type known(Peer) alone, and no clause on the request
# propose([t]_j).
cva_proposal() { # cva_proposal <platform.vglp>: the alternatives of Proposal, one line
    awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$1"
}
check "CVA request type: the social graph's, propose(Proposal) and known(Peer)" "^Request ::= propose(Proposal) ; known(Peer)\.$" "$(cat "$CVA/social_graph/platform.vglp")"
check "CVA request type: the social graph proposes establish, befriend and unfriend" "^establish(Peer) befriend(Peer, Peer) unfriend(Peer, Peer) $" "$(cva_proposal "$CVA/social_graph/platform.vglp")"
check "CVA request type: nudge proposes befriend, not the unguarded nudge" "^befriend(Peer, Peer) $" "$(cva_proposal "$CVA/nudge/platform.vglp")"
check "CVA request type: gnudge proposes befriend and g, a guarded part of one class with the unguarded part of nudge, and not nudge" "^befriend(Peer, Peer) g(Peer, Peer) $" "$(cva_proposal "$CVA/gnudge/platform.vglp")"
check "CVA request type: opens proposes befriend, mark and rmark, and not the unguarded u1 and u2" "^befriend(Peer, Peer) mark(Peer, Peer) rmark(Peer, Peer) $" "$(cva_proposal "$CVA/opens/platform.vglp")"
cva_ung=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(ung, [schema(u, [], [rs(unguarded, [atom(b, [])], [atom(a, [])], [], [])])], syntactically_grassroots).
:quit
HEREDOC
2>&1)
check "CVA request type: a contract with no guarded class, compiled" "%% the canonical CVA platform of the contract(ung)" "$cva_ung"
check "CVA request type: ... its request type known(Peer) alone" "^Request ::= known(Peer)\.$" "$cva_ung"
check_not "CVA request type: ... no Proposal, and no clause on the request propose([t])" "^Proposal ::=\|step(req(propose\|await_request(propose" "$cva_ung"

cva_sg_menu=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_menu(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA request question: the first, asked when the agent starts, shows the agent's own key and no person known" "A = \[view(\[\], \[\]), menu(alice, \[\]), view(\[identity\], \[\])" "$cva_sg_menu"
check "CVA request question: the one asked on reading the request to befriend bob, once bob is known, shows the key and bob" "view(\[friend(bob), identity\], \[bob\]), menu(alice, \[bob\])" "$cva_sg_menu"

cva_sg_load=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:quit
HEREDOC
2>&1)
check "CVA social graph: the platform and its plays load, compiled by the canonical compilation" "Loaded program: .*cva/social_graph" "$cva_sg_load"
check_not "CVA social graph: no type errors" "Type checking failed" "$cva_sg_load"

cva_sg_befriend=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_befriend(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA befriend: alice's screen: her identity, established at its Propose, bob, once known, and bob her friend, once her attempt commits" "A = \[view(\[\], \[\]), view(\[identity\], \[\]), view(\[identity\], \[bob\]), view(\[friend(bob), identity\], \[bob\]) |" "$cva_sg_befriend"
check "CVA befriend: bob, in its guard set, is asked on the start, answers yes on his approval question, and sees alice his friend once the outcome commits" "B = \[view(\[\], \[\]), view(\[identity\], \[\]), view(\[identity\], \[alice\]), gave(befriend(alice, bob), yes), view(\[friend(alice), identity\], \[alice\]) |" "$cva_sg_befriend"
check "CVA befriend: the start to bob, his yes to alice, and the commit she tells him" "N = \[sent(alice, bob, start(attempt(alice, 2, 2, befriend(alice, bob)))), sent(bob, alice, vote(attempt(alice, 2, 2, befriend(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob)), commit))" "$cva_sg_befriend"

cva_sg_unfriend=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_unfriend(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA unfriend: bob, his number the largest he has read, proposes unfriending alice, attempt 3, once he sees her his friend, and alice's machine votes yes" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob)), commit)), sent(bob, alice, start(attempt(bob, 3, 3, unfriend(bob, alice)))), sent(alice, bob, vote(attempt(bob, 3, 3, unfriend(bob, alice)), yes))" "$cva_sg_unfriend"
check "CVA unfriend: alice, unasked, sees bob end the friendship, befriends him again once she sees it ended, and sees her own unfriend" "A = \[.*view(\[friend(bob), identity\], \[bob\]), view(\[identity\], \[bob\]), view(\[friend(bob), identity\], \[bob\]), view(\[identity\], \[bob\]) |" "$cva_sg_unfriend"
check "CVA unfriend: ... its yes, and bob's unfriend committed" "sent(alice, bob, vote(attempt(bob, 3, 3, unfriend(bob, alice)), yes)), sent(bob, alice, outcome(attempt(bob, 3, 3, unfriend(bob, alice)), commit))" "$cva_sg_unfriend"
check "CVA unfriend: befriended again, alice proposes unfriending bob, and bob's machine votes on it, unasked" "sent(alice, bob, start(attempt(alice, 5, 5, unfriend(alice, bob)))), sent(bob, alice, vote(attempt(alice, 5, 5, unfriend(alice, bob)), yes))" "$cva_sg_unfriend"
check "CVA unfriend: ... bob's screen: the friendship ended by him, made again by alice, and ended by her" "B = \[.*gave(attempt(alice, 2, 2, befriend(alice, bob)), yes), view(\[friend(alice), identity\], \[alice\]), view(\[identity\], \[alice\]), gave(attempt(alice, 4, 4, befriend(alice, bob)), yes), view(\[friend(alice), identity\], \[alice\]), view(\[identity\], \[alice\]) |" "$cva_sg_unfriend"
check "CVA unfriend: ... and alice's unfriend committed" "sent(alice, bob, outcome(attempt(alice, 5, 5, unfriend(alice, bob)), commit))" "$cva_sg_unfriend"
check_not "CVA unfriend: no person answers for unfriending" "gave(attempt([a-z]*, [0-9]*, [0-9]*, unfriend\|gave(unfriend" "$cva_sg_unfriend"

cva_sg_refused=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_refused(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA refused: bob answers alice's first attempt no and her second yes, and sees her his friend once it commits" "B = \[.*view(\[identity\], \[alice\]), gave(attempt(alice, 2, 2, befriend(alice, bob)), no), gave(attempt(alice, 3, 3, befriend(alice, bob)), yes), view(\[friend(alice), identity\], \[alice\]) |" "$cva_sg_refused"
check "CVA refused: ... no, and alice tallies it and tells bob abort" "sent(bob, alice, vote(attempt(alice, 2, 2, befriend(alice, bob)), no)), sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob)), abort))" "$cva_sg_refused"
check "CVA refused: aborted, alice, seeing the notice, requests befriending bob again, attempt 3, on which bob is asked again and votes yes" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob)), abort)), sent(alice, bob, start(attempt(alice, 3, 3, befriend(alice, bob)))), sent(bob, alice, vote(attempt(alice, 3, 3, befriend(alice, bob)), yes))" "$cva_sg_refused"
check "CVA refused: ... and the second attempt commits" "sent(alice, bob, outcome(attempt(alice, 3, 3, befriend(alice, bob)), commit))" "$cva_sg_refused"
check "CVA refused: alice's screen: the notice refused(befriend(alice, bob), bob) once bob's no closes her attempt, and bob her friend once the second commits" "A = \[view(\[\], \[\]), view(\[identity\], \[\]), view(\[identity\], \[bob\]), refused(befriend(alice, bob), bob), view(\[friend(bob), identity\], \[bob\]) |" "$cva_sg_refused"
check_not "CVA refused: ... no third attempt" "attempt(alice, 4, " "$cva_sg_refused"

cva_cur_load=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:quit
HEREDOC
2>&1)
check "CVA currency: the platform and its plays load, compiled by the canonical compilation" "Loaded program: .*cva/currency" "$cva_cur_load"
check_not "CVA currency: no type errors" "Type checking failed" "$cva_cur_load"

cva_cur_payswap=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 600000
play_pay_swap(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA pay and swap: alice's mint, a class of one participant, applied at its Propose: her coin on her screen before bob is known" "A = \[view(\[\], \[\]), view(\[coin(alice)\], \[\]), view(\[coin(alice)\], \[bob\])" "$cva_cur_payswap"
check "CVA pay and swap: alice proposes the swap, and bob is asked, answers yes, and holds her coin once it commits" "B = \[.*gave(swap(alice, bob, bob, alice), yes), view(\[coin(alice)\], \[alice\])" "$cva_cur_payswap"
check "CVA pay and swap: ... and the swap commits" "sent(alice, bob, outcome(attempt(alice, 2, 2, swap(alice, bob, bob, alice)), commit))" "$cva_cur_payswap"
check "CVA pay and swap: alice pays bob, guarded by her alone, and bob's machine votes yes, unasked, and it commits" "sent(alice, bob, start(attempt(alice, 3, 3, pay(alice, bob)))), sent(bob, alice, vote(attempt(alice, 3, 3, pay(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 3, 3, pay(alice, bob)), commit))" "$cva_cur_payswap"
check "CVA pay and swap: bob, unasked, holds his coin again once the pay commits" "B = \[.*view(\[coin(alice)\], \[alice\]), view(\[coin(bob), coin(alice)\], \[alice\]) |" "$cva_cur_payswap"
check "CVA pay and swap: ... and alice holds none" "A = \[.*view(\[coin(bob)\], \[bob\]), view(\[\], \[bob\]) |" "$cva_cur_payswap"

# An attempt's guard set is its part's (vGLP 099f876, Definition "Attempt";
# vGLP #15, Code_inbox.md, 2026-10-19 10:50 UTC, item 5): the swap back's class
# has the swaps' part, guarded by alice and bob, and the redeems' parts, guarded
# by alice and by bob.  Requesting the swap back, alice proposes the part her
# request names, the swaps', which asks bob; requesting it again after a
# second swap, she proposes her redeem's part, the guarded part of the class
# she guards and is ready for and has proposed less recently (Section 5.4,
# "The compilation"), which asks nobody.
cva_cur_redeem=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 1500000
play_redeem(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA redeem: the swap back, alice proposing the swaps' part her request names, guarded by alice and bob, attempt 3: bob is asked and answers yes" "sent(alice, bob, start(attempt(alice, 3, 3, swap(alice, bob, alice, bob))))" "$cva_cur_redeem"
check "CVA redeem: ... his answer" "B = \[.*gave(swap(alice, bob, alice, bob), yes)" "$cva_cur_redeem"
check "CVA redeem: ... it commits" "sent(bob, alice, vote(attempt(alice, 3, 3, swap(alice, bob, alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 3, 3, swap(alice, bob, alice, bob)), commit))" "$cva_cur_redeem"
check "CVA redeem: after a second swap, attempt 4, alice requests the swap back again and proposes her redeem's part, proposed less recently, attempt 5" "sent(alice, bob, outcome(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), commit)), sent(alice, bob, start(attempt(alice, 5, 5, redeem(alice, bob, alice))))" "$cva_cur_redeem"
check "CVA redeem: ... alice's screen: her coin and bob's exchanged at each commit, her own back at the last" "A = \[.*view(\[coin(bob)\], \[bob\]), view(\[coin(alice)\], \[bob\]), view(\[coin(bob)\], \[bob\]), view(\[coin(alice)\], \[bob\]) | " "$cva_cur_redeem"
check "CVA redeem: ... guarded by alice alone: bob's machine votes yes, unasked, and it commits" "sent(bob, alice, vote(attempt(alice, 5, 5, redeem(alice, bob, alice)), yes)), sent(alice, bob, outcome(attempt(alice, 5, 5, redeem(alice, bob, alice)), commit)) | " "$cva_cur_redeem"
check_not "CVA redeem: bob answers nothing for the redeem" "gave(attempt(alice, 5, 5, redeem\|gave(redeem" "$cva_cur_redeem"

cva_cur_busy=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 600000
play_busy(A, B, N).
:quit
HEREDOC
2>&1)
# play_busy (vGLP 49cd9f7, wound-wait): no vote no for contention.  alice
# holds her swap prepared, bob's approval question on it open; bob proposes a pay, whose
# start waits at alice, blocked by her swap, which has priority over it, so
# no Wound; bob answers her approval question no, her swap aborts, and the pay's start,
# put up again, is taken and the pay commits at its first attempt.
check "CVA busy: alice proposes a swap, attempt 4; bob, asked, leaves his approval question open, proposes a pay, attempt 5, and answers no once the request question is drawn again" "sent(alice, bob, start(attempt(alice, 4, 4, swap(alice, bob, bob, alice)))), sent(bob, alice, start(attempt(bob, 5, 5, pay(bob, alice)))), sent(bob, alice, vote(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), no))" "$cva_cur_busy"
check "CVA busy: ... his answer" "B = \[.*gave(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), no)" "$cva_cur_busy"
check "CVA busy: alice holds the pay's start waiting while her swap holds her lock: her vote on it comes only after her swap's outcome" "sent(alice, bob, outcome(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), abort)), sent(alice, bob, vote(attempt(bob, 5, 5, pay(bob, alice)), yes))" "$cva_cur_busy"
check_not "CVA busy: ... no vote no on the pay, and no Wound: no yield asked" "vote(attempt(bob, 5, 5, pay(bob, alice)), no)\|yield(" "$cva_cur_busy"
check "CVA busy: bob answers alice's approval question no, and her swap aborts" "sent(bob, alice, vote(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), no)), sent(alice, bob, outcome(attempt(alice, 4, 4, swap(alice, bob, bob, alice)), abort))" "$cva_cur_busy"
check "CVA busy: the lock released, alice's machine votes yes on the waiting pay, and it commits" "sent(alice, bob, vote(attempt(bob, 5, 5, pay(bob, alice)), yes)), sent(bob, alice, outcome(attempt(bob, 5, 5, pay(bob, alice)), commit))" "$cva_cur_busy"

cva_cur_declined=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 600000
play_declined(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA declined: bob, not holding the coin, votes on the start without asking his person, his screen unchanged" "B = \[view(\[\], \[\]), view(\[\], \[alice\]) |" "$cva_cur_declined"
check "CVA declined: ... no, and it aborts at both" "sent(bob, alice, vote(attempt(alice, 2, 2, swap(alice, bob, carol, alice)), no)), sent(alice, bob, outcome(attempt(alice, 2, 2, swap(alice, bob, carol, alice)), abort))" "$cva_cur_declined"
check_not "CVA declined: no approval question answered" "gave(" "$cva_cur_declined"

# vGLP's contract nudge (programs/vglp/cva/contracts.glp): befriend and an
# unguarded nudge, the one contract here with an unguarded class.  Its opener,
# the person in its first role, proposes it as an own transaction, the attempt asking
# nobody and the other participant voting by its machine (vGLP's 2026-10-11
# 22:51 UTC, the nudge play redone on the five handlers).  Both openers propose
# at once, their attempts of one rank, alice's first in the order of
# identifiers; bob, holding his own attempt prepared, holds alice's start
# waiting and wounds his own attempt, which aborts; he then takes her start,
# her nudge commits, and his retried attempt, keeping the rank of the wounded
# one, commits (vGLP 49cd9f7, wound-wait).
cva_nudge_load=$("$REPL_RUN" <<HEREDOC
$CVA/nudge/
:quit
HEREDOC
2>&1)
check "CVA nudge: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/nudge" "$cva_nudge_load"
check_not "CVA nudge: no type errors" "Type checking failed" "$cva_nudge_load"

cva_nudge=$("$REPL_RUN" <<HEREDOC
$CVA/nudge/
:limit 600000
play_nudge(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA nudge: alice and bob befriended, committed at both" "sent(alice, bob, outcome(attempt(alice, 1, 1, befriend(alice, bob)), commit))" "$cva_nudge"
check "CVA nudge: alice, its opener, proposes nudge(alice, bob), an own transaction, once befriended" "sent(alice, bob, outcome(attempt(alice, 1, 1, befriend(alice, bob)), commit)), sent(alice, bob, start(attempt(alice, 2, 2, nudge(alice, bob))))" "$cva_nudge"
check "CVA nudge: ... alice's screen: bob her friend, then her nudge of him, then his of her, each once committed" "A = \[view(\[\], \[\]), view(\[\], \[bob\]), view(\[friend(bob)\], \[bob\]), view(\[nudged(bob), friend(bob)\], \[bob\]), view(\[nudged_by(bob), nudged(bob), friend(bob)\], \[bob\]) |" "$cva_nudge"
check "CVA nudge: bob, its opener, proposes nudge(bob, alice), an own transaction, once befriended, of the same rank" "sent(bob, alice, start(attempt(bob, 2, 2, nudge(bob, alice))))" "$cva_nudge"
check "CVA nudge: bob, holding his attempt prepared, wounds it in favour of alice's, which has priority: it aborts before he takes her start" "sent(bob, alice, start(attempt(bob, 2, 2, nudge(bob, alice)))), sent(bob, alice, outcome(attempt(bob, 2, 2, nudge(bob, alice)), abort))" "$cva_nudge"
check "CVA nudge: ... his abort told before his vote on her start, yes" "sent(bob, alice, outcome(attempt(bob, 2, 2, nudge(bob, alice)), abort)), sent(bob, alice, vote(attempt(alice, 2, 2, nudge(alice, bob)), yes))" "$cva_nudge"
check_not "CVA nudge: an opener's attempt, bob's nudge wounded by himself, gives no notice (vGLP c44769f)" "refused(\|yielded(" "$cva_nudge"
check_not "CVA nudge: no person answers for the unguarded class" "gave(attempt([a-z]*, [0-9]*, [0-9]*, nudge\|gave(nudge" "$cva_nudge"
check_not "CVA nudge: no vote no for contention" "vote(attempt([a-z]*, [0-9]*, [0-9]*, nudge([a-z]*, [a-z]*)), no)" "$cva_nudge"
check "CVA nudge: alice's nudge committed at both" "sent(alice, bob, outcome(attempt(alice, 2, 2, nudge(alice, bob)), commit))" "$cva_nudge"
check "CVA nudge: bob retries, his attempt keeping the wounded one's rank 2, and it commits at both" "sent(bob, alice, outcome(attempt(bob, 3, 2, nudge(bob, alice)), commit))" "$cva_nudge"
check_not "CVA nudge: nothing aborted for ever: no third attempt at either nudge" "attempt(alice, 3, [0-9]*, nudge\|attempt(bob, 4, [0-9]*, nudge" "$cva_nudge"

# vGLP's contract introduce (programs/vglp/cva/contracts.glp): befriend and a
# three-party introduce, guarded at every role (vGLP's 2026-10-11 22:59 UTC,
# Q3, Udi approved).  Three agents: the attempt asks two, the first answers
# no, and the other's approval question is closed by the program once its Outcome closes
# the attempt; asked again, both answer yes.
cva_intro_load=$("$REPL_RUN" <<HEREDOC
$CVA/introduce/
:quit
HEREDOC
2>&1)
check "CVA introduce: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/introduce" "$cva_intro_load"
check_not "CVA introduce: no type errors" "Type checking failed" "$cva_intro_load"

cva_intro=$("$REPL_RUN" <<HEREDOC
$CVA/introduce/
:limit 1500000
play_introduce(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA introduce: alice, befriended with both, proposes introducing bob and carol, its start told to both" "sent(alice, bob, start(attempt(alice, 3, 3, introduce(alice, bob, carol)))), sent(alice, carol, start(attempt(alice, 3, 3, introduce(alice, bob, carol))))" "$cva_intro"
check "CVA introduce: bob, asked, answers no, and alice tells both abort" "sent(bob, alice, vote(attempt(alice, 3, 3, introduce(alice, bob, carol)), no)), sent(alice, bob, outcome(attempt(alice, 3, 3, introduce(alice, bob, carol)), abort)), sent(alice, carol, outcome(attempt(alice, 3, 3, introduce(alice, bob, carol)), abort))" "$cva_intro"
check "CVA introduce: carol, asked, her approval question still open when Outcome closes the attempt, gives it yes once she is asked on attempt 4, and yes to that" "C = \[.*gave(attempt(alice, 3, 3, introduce(alice, bob, carol)), yes), gave(attempt(alice, 4, 4, introduce(alice, bob, carol)), yes)" "$cva_intro"
check_not "CVA introduce: ... the approval question closed by the program, her yes reaches no goal: no vote of hers on attempt 3" "sent(carol, alice, vote(attempt(alice, 3, 3, introduce" "$cva_intro"
check "CVA introduce: alice, seeing the notice of the abort, requests it again, attempt 4, and both answer yes" "sent(carol, alice, vote(attempt(alice, 4, 4, introduce(alice, bob, carol)), yes)), sent(bob, alice, vote(attempt(alice, 4, 4, introduce(alice, bob, carol)), yes))" "$cva_intro"
check "CVA introduce: ... alice's screen: the notice refused(introduce(alice, bob, carol), bob), then the introduction" "A = \[.*view(\[friend(carol), friend(bob)\], \[bob, carol\]), refused(introduce(alice, bob, carol), bob), view(\[introduced(bob, carol), friend(carol), friend(bob)\], \[bob, carol\]) |" "$cva_intro"
check "CVA introduce: ... and tells bob commit" "sent(alice, bob, outcome(attempt(alice, 4, 4, introduce(alice, bob, carol)), commit))" "$cva_intro"
check "CVA introduce: ... and carol" "sent(alice, carol, outcome(attempt(alice, 4, 4, introduce(alice, bob, carol)), commit))" "$cva_intro"
check_not "CVA introduce: no later attempt of hers" "attempt(alice, [5-9], [0-9]*, introduce(alice, bob, carol))" "$cva_intro"

# vGLP's contract gnudge (programs/vglp/cva/contracts.glp): nudge with a
# second schema g, nudge(a, b) and g(b?, a) determining the same transactions,
# g guarded at its first role, so that the class of nudge(a, b) has two parts,
# nudge(a, b)'s, unguarded, and g(b, a)'s, guarded by b (vGLP 099f876, Section
# 5.4).  The opener of an unguarded part is the least person in the first role
# of its schema-bindings: alice preceding bob, bob opens the part of
# nudge(bob, alice), which the least person in the first role of all the
# class's schema-bindings, g(alice, bob)'s alice, would leave unopened, alice's
# own transactions holding only the parts of the unguarded schemas with her in
# their first role.
cva_gnudge_load=$("$REPL_RUN" <<HEREDOC
$CVA/gnudge/
:quit
HEREDOC
2>&1)
check "CVA gnudge: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/gnudge" "$cva_gnudge_load"
check_not "CVA gnudge: no type errors" "Type checking failed" "$cva_gnudge_load"

cva_gnudge=$("$REPL_RUN" <<HEREDOC
$CVA/gnudge/
:limit 600000
play_gnudge(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA gnudge: alice and bob befriended, committed at both" "sent(alice, bob, outcome(attempt(alice, 1, 1, befriend(alice, bob)), commit))" "$cva_gnudge"
check "CVA gnudge: bob, the least person in the first role of the schema-bindings of the unguarded part of nudge(bob, alice), opens it, an own transaction, once befriended, though alice precedes him" "sent(alice, bob, outcome(attempt(alice, 1, 1, befriend(alice, bob)), commit)), .*sent(bob, alice, start(attempt(bob, 2, 2, nudge(bob, alice))))" "$cva_gnudge"
check "CVA gnudge: ... its start told to alice, whose machine votes on it, no person asked" "sent(alice, bob, vote(attempt(bob, 3, 2, nudge(bob, alice)), " "$cva_gnudge"
check_not "CVA gnudge: alice, preceding bob, proposes nothing of the class of nudge(bob, alice), her person requesting none" "start(attempt(alice, [0-9]*, [0-9]*, nudge(bob, alice)))\|start(attempt(alice, [0-9]*, [0-9]*, g(" "$cva_gnudge"
check_not "CVA gnudge: an opener's attempt gives no notice" "refused(\|yielded(" "$cva_gnudge"
check_not "CVA gnudge: no person answers for the unguarded class" "gave(attempt([a-z]*, [0-9]*, [0-9]*, nudge\|gave(nudge\|gave(attempt([a-z]*, [0-9]*, [0-9]*, g(\|gave(g(" "$cva_gnudge"
check "CVA gnudge: bob wounds his own attempt in favour of alice's: it aborts before he votes on her start" "sent(bob, alice, start(attempt(bob, 2, 2, nudge(bob, alice)))), sent(bob, alice, outcome(attempt(bob, 2, 2, nudge(bob, alice)), abort)), sent(bob, alice, vote(attempt(alice, 2, 2, nudge(alice, bob)), yes))" "$cva_gnudge"
check_not "CVA gnudge: no vote no for contention" "vote(attempt([a-z]*, [0-9]*, [0-9]*, nudge([a-z]*, [a-z]*)), no)" "$cva_gnudge"
check "CVA gnudge: alice's nudge committed at both" "sent(alice, bob, outcome(attempt(alice, 2, 2, nudge(alice, bob)), commit))" "$cva_gnudge"
check "CVA gnudge: bob's nudge, the part he opens, retried at the wounded rank, committed at both" "sent(bob, alice, outcome(attempt(bob, 3, 2, nudge(bob, alice)), commit))" "$cva_gnudge"
check_not "CVA gnudge: nothing aborted for ever: no third attempt at either nudge" "attempt(alice, 3, [0-9]*, nudge\|attempt(bob, 4, [0-9]*, nudge" "$cva_gnudge"

# vGLP's contract childsafe (programs/vglp/cva/contracts.glp): befriend, enrol
# and a four-party cbefriend guarded by all four (vGLP 2026-10-13 12:05 UTC,
# item 5).  Seek and Refer removed (vGLP ac065e0): alice and dave request each
# other known before alice proposes cbefriend(carol, dave, alice, bob), which
# commits at all four.
cva_cs_load=$("$REPL_RUN" <<HEREDOC
$CVA/childsafe/
:quit
HEREDOC
2>&1)
check "CVA childsafe: the platform and its plays load, compiled by the canonical compilation" "Loaded program: .*cva/childsafe" "$cva_cs_load"
check_not "CVA childsafe: no type errors" "Type checking failed" "$cva_cs_load"

cva_cs=$("$REPL_RUN" <<HEREDOC
$CVA/childsafe/
:limit 1500000
play_childsafe(A, B, C, D, N).
:quit
HEREDOC
2>&1)
check "CVA childsafe: alice, knowing carol and bob, comes to know dave by her request and his, proposes cbefriend, attempt 3, and sees it approved" "A = \[.*view(\[friend(bob), child(carol)\], \[bob, carol\]), view(\[friend(bob), child(carol)\], \[bob, carol, dave\]), view(\[approved(carol, dave), friend(bob), child(carol)\], \[bob, carol, dave\]) |" "$cva_cs"
check "CVA childsafe: ... its start told to carol" "sent(alice, carol, start(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob))))" "$cva_cs"
check "CVA childsafe: dave, knowing bob, comes to know alice, and is asked" "D = \[view(\[\], \[\]), view(\[\], \[bob\]), .*view(\[parent(bob)\], \[alice, bob\]), gave(cbefriend(carol, dave, alice, bob), yes)" "$cva_cs"
check_not "CVA childsafe: no seek and no introduction" "seek(\|refer(\|intro(" "$cva_cs"
check "CVA childsafe: the three yeses, carol's, dave's and bob's" "sent(carol, alice, vote(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), yes)), sent(dave, alice, vote(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), yes)), sent(bob, alice, vote(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), yes))" "$cva_cs"
check "CVA childsafe: ... and it commits at carol" "sent(alice, carol, outcome(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), commit))" "$cva_cs"
check "CVA childsafe: ... at dave" "sent(alice, dave, outcome(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), commit))" "$cva_cs"
check "CVA childsafe: ... and at bob" "sent(alice, bob, outcome(attempt(alice, 3, 3, cbefriend(carol, dave, alice, bob)), commit))" "$cva_cs"

# vGLP's contract link (programs/vglp/cva/contracts.glp): befriend and a
# three-party link guarded at its initiator alone (vGLP 2026-10-13 17:53 UTC,
# item 7; vGLP 49cd9f7, Definition "Canonical CVA Platform", Wound and Yield).
# Idit's deadlock: alice's and bob's links meet in carol and dave, outside
# both initiators; the network delivers alice's start to carol first and
# bob's to dave first, so each holds the other's start waiting.  alice's has
# priority, so dave wounds bob's, bob aborts it on the yield, alice's
# commits, and bob's retried attempt, keeping the wounded rank, commits.
cva_link_load=$("$REPL_RUN" <<HEREDOC
$CVA/link/
:quit
HEREDOC
2>&1)
check "CVA link: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/link" "$cva_link_load"
check_not "CVA link: no type errors" "Type checking failed" "$cva_link_load"

cva_link=$("$REPL_RUN" <<HEREDOC
$CVA/link/
:limit 3000000
play_deadlock(A, B, C, D, N).
:quit
HEREDOC
2>&1)
check "CVA deadlock: carol votes yes on alice's link and holds bob's start waiting" "sent(alice, carol, start(attempt(alice, 2, 2, link(alice, carol, dave)))), .*sent(carol, alice, vote(attempt(alice, 2, 2, link(alice, carol, dave)), yes)), .*sent(bob, carol, start(attempt(bob, 3, 3, link(bob, carol, dave))))" "$cva_link"
check "CVA deadlock: dave votes yes on bob's link and holds alice's start waiting, blocked by bob's" "sent(dave, bob, vote(attempt(bob, 3, 3, link(bob, carol, dave)), yes)), sent(dave, bob, yield(attempt(bob, 3, 3, link(bob, carol, dave))))" "$cva_link"
check "CVA deadlock: dave wounds bob's attempt in favour of alice's, of the smaller rank, asking bob to yield, and votes on her start only after bob's abort, bob's retried start, on the notice, between them" "sent(dave, bob, yield(attempt(bob, 3, 3, link(bob, carol, dave)))), sent(bob, carol, outcome(attempt(bob, 3, 3, link(bob, carol, dave)), abort)), sent(bob, dave, outcome(attempt(bob, 3, 3, link(bob, carol, dave)), abort)), .*sent(dave, alice, vote(attempt(alice, 2, 2, link(alice, carol, dave)), yes))" "$cva_link"
check "CVA deadlock: bob, not every yes recorded, yields, and, seeing the notice, requests his link again: proposed as attempt 4 at the wounded rank 3" "sent(bob, dave, outcome(attempt(bob, 3, 3, link(bob, carol, dave)), abort)), .*sent(bob, carol, start(attempt(bob, 4, 3, link(bob, carol, dave))))" "$cva_link"
check "CVA deadlock: ... abort told to both" "sent(bob, carol, outcome(attempt(bob, 3, 3, link(bob, carol, dave)), abort)), sent(bob, dave, outcome(attempt(bob, 3, 3, link(bob, carol, dave)), abort))" "$cva_link"
check "CVA deadlock: dave, his lock released, votes yes on alice's link" "sent(dave, alice, vote(attempt(alice, 2, 2, link(alice, carol, dave)), yes))" "$cva_link"
check "CVA deadlock: alice's link commits at carol and dave" "sent(alice, carol, outcome(attempt(alice, 2, 2, link(alice, carol, dave)), commit)), sent(alice, dave, outcome(attempt(alice, 2, 2, link(alice, carol, dave)), commit))" "$cva_link"
check "CVA deadlock: bob's retried attempt, keeping the wounded rank 3, commits at carol and dave" "sent(bob, carol, outcome(attempt(bob, 4, 3, link(bob, carol, dave)), commit)), sent(bob, dave, outcome(attempt(bob, 4, 3, link(bob, carol, dave)), commit))" "$cva_link"
check "CVA deadlock: ... bob's screen: the notice yielded(link(bob, carol, dave)), then his link committed" "B = \[.*view(\[friend(dave), friend(carol)\], \[carol, dave\]), yielded(link(bob, carol, dave)), view(\[linked(carol, dave), friend(dave), friend(carol)\], \[carol, dave\])" "$cva_link"
check_not "CVA deadlock: alice's attempt, which commits, gives no notice" "A = \[.*\(refused\|yielded\)(" "$cva_link"
check_not "CVA deadlock: no vote no and no person asked for a link" "vote(attempt([a-z]*, [0-9]*, [0-9]*, link([a-z, ]*)), no)\|gave(attempt([a-z]*, [0-9]*, [0-9]*, link\|gave(link" "$cva_link"

# vGLP's contract friends (programs/vglp/cva/contracts.glp): befriend, friend
# reflexive and the contract's introduction predicate, so its platform has an
# introduction rule: an agent introduces every two of its friends to each
# other by Introduce peers, and each, on reading the introduction, sends the
# other ihello, an exchange of which is CVA's Introduce (vGLP, Section 3.2,
# Definition "Introduction Rule, Introduce Peers"; Section 5.4, the
# compilation and the paragraph on act schemas; vGLP 2026-10-16 13:53 UTC,
# C.3).  The network logs an introduction as intro(From, To, Q).
cva_friends_text=$(cat "$CVA/friends/platform.vglp")
check "CVA friends: the contract's introduction predicate printed with its platform" "^introduction_preds(\[friend\])$" "$cva_friends_text"
cva_sg_text=$(cat "$CVA/social_graph/platform.vglp")
check "CVA social graph: a contract naming no introduction predicate has none" "^introduction_preds(\[\])$" "$cva_sg_text"
# The type of the machine states, printed from the contract: Atom, one alternative per predicate at its argument types, a person Peer for a role and a speech act Speech for a speech-act variable or a signed term, and atom_of, which writes the atom the agent holds, g(Name, Vals), as the contract writes it (vGLP 99c7dc8, Section 5.4, "The compilation").
cva_atom() { # cva_atom <platform.vglp>: the alternatives of Atom, one line
    awk '/^Atom ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$1"
}
check "CVA machine states: the social graph's atoms, identity and friend(Peer)" "^identity friend(Peer) $" "$(cva_atom "$CVA/social_graph/platform.vglp")"
check "CVA machine states: ... friend(bob) written as the contract writes it" "^atom_of(g(friend, \[p(V1)\]), shown(friend(V1?)))$" "$cva_sg_text"
check "CVA machine states: the currency's atom, coin(Peer)" "^coin(Peer) $" "$(cva_atom "$CVA/currency/platform.vglp")"
check "CVA machine states: GSN's atoms, a speech act at a speech-act variable's place and a community name at a name term's" "^identity friend(Peer) received(Speech, Peer) member(CName) listed(Peer, Speech) closed(Speech) posted(Speech, Speech) delivered(Speech, Peer, Speech) received_in(Speech, Peer, Speech) $" "$(cva_atom "$GLP_DIR/programs/apps/gsn/platform.vglp")"
check "CVA machine states: GSNP's, a signed term a speech act" "^identity friend(Peer) item(Speech) authored(Speech) member(CName) listed(Peer, Speech) closed(Speech) posted(Speech, Speech) delivered(Speech, Peer, Speech) $" "$(cva_atom "$GLP_DIR/programs/apps/gsnp/platform.vglp")"

cva_friends_load=$("$REPL_RUN" <<HEREDOC
$CVA/friends/
:quit
HEREDOC
2>&1)
check "CVA friends: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/friends" "$cva_friends_load"
check_not "CVA friends: no type errors" "Type checking failed" "$cva_friends_load"

cva_friends=$("$REPL_RUN" <<HEREDOC
$CVA/friends/
:limit 1500000
play_friends(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA introduce peers: alice, a friend of bob and of carol, takes Introduce peers {bob, carol}, an own transaction, once her befriending of carol commits" "sent(alice, carol, outcome(attempt(alice, 2, 2, befriend(alice, carol)), commit)), intro(alice, bob, carol)" "$cva_friends"
check "CVA introduce peers: ... sending bob intro(carol) and carol intro(bob)" "intro(alice, bob, carol), intro(alice, carol, bob)" "$cva_friends"
check "CVA introduce peers: bob comes to know carol by the ihellos, never having requested her known, his view showing her, and his request question shows her" "B = \[.*view(\[friend(alice)\], \[alice\]), view(\[friend(alice)\], \[alice, carol\]), menu(bob, \[alice, carol\])" "$cva_friends"
check "CVA introduce peers: ... and he proposes befriending her" "sent(bob, carol, start(attempt(bob, 2, 2, befriend(bob, carol))))" "$cva_friends"
check "CVA introduce peers: carol comes to know bob, is asked, answers yes, and the friends of a friend are friends" "C = \[.*view(\[friend(alice)\], \[alice, bob\]), gave(befriend(bob, carol), yes), view(\[friend(bob), friend(alice)\], \[alice, bob\])" "$cva_friends"
check "CVA introduce peers: ... it commits" "sent(bob, carol, outcome(attempt(bob, 2, 2, befriend(bob, carol)), commit))" "$cva_friends"
check_not "CVA introduce peers: no pair introduced twice by one agent" "intro(alice, bob, carol), .*intro(alice, bob, carol)" "$cva_friends"
check "CVA two roles at one person: alice's greet(alice, alice), friend(alice) holding at her, friend reflexive, a class of one participant applied at its Propose: greeted and greeting, twice" "A = \[.*view(\[friend(carol), friend(bob)\], \[bob, carol\]), view(\[greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\]), view(\[greeted, greeting, greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\])" "$cva_friends"
check_not "CVA two roles at one person: ... no start sent for it" "start(attempt(alice, [0-9]*, [0-9]*, greet(alice, alice)))" "$cva_friends"
check "CVA counts: two cheers, each deleting one of her two greeted" "view(\[cheered, greeting, greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\]), view(\[cheered, cheered, greeting, greeting, friend(carol), friend(bob)\], \[bob, carol\])" "$cva_friends"
check "CVA counts: her third cheer, no greeted left, dropped and taking no number: her greet of bob is attempt 7" "sent(alice, bob, start(attempt(alice, 7, 7, greet(alice, bob))))" "$cva_friends"
check_not "CVA counts: ... no third cheer proposed" "cheered, cheered, cheered" "$cva_friends"
check "CVA two roles at one person: greet(alice, bob), at two people, bob's machine voting on it, commits" "sent(alice, bob, outcome(attempt(alice, 7, 7, greet(alice, bob)), commit))" "$cva_friends"

# The printed program's class at run time is the paper's (vGLP 9c819f5, Section 5.4, "Act schemas as local specifications": "The classes are those of the contract's equivalence, generated by relating two transactions when one schema-binding determines both"; vGLP #14's task, Code_inbox.md, 2026-10-18 13:35 UTC): the closure of a schema-binding's links among the schema-bindings of its participants and atoms, the compiler's local.glp, part C, at run time, so that the domains of the effects, the guard sets and the opener are taken over one class.  vGLP's contract waves (programs/vglp/cva/contracts.glp): wave(alice, bob), guarded by alice, requires friend(bob) at alice and hail(alice, bob), guarded by both, forbids it, the two deleting and adding the same atoms, so they are two classes.  hail asks bob, though the class of wave is guarded by alice; and once befriended, alice's second hail is dropped, taking no number, friend(bob) being outside the domain of her effect in the class of hail, though in the domain in the class of wave.  A class taken as the schema-bindings of the same participants and atoms would ask bob nothing for hail and take the second.
cva_waves_load=$("$REPL_RUN" <<HEREDOC
$CVA/waves/
:quit
HEREDOC
2>&1)
check "CVA waves: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/waves" "$cva_waves_load"
check_not "CVA waves: no type errors" "Type checking failed" "$cva_waves_load"

cva_waves=$("$REPL_RUN" <<HEREDOC
$CVA/waves/
:limit 1500000
play_waves(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA run-time class: alice hails bob, attempt 1, befriends him, attempt 2, her second hail dropped, taking no number, and she waves, attempt 3, each committed: her screen" "A = \[view(\[\], \[\]), view(\[\], \[bob\]), view(\[waved(bob)\], \[bob\]), view(\[friend(bob), waved(bob)\], \[bob\]), view(\[waved(bob), friend(bob), waved(bob)\], \[bob\]) | " "$cva_waves"
check "CVA run-time class: bob, in the guard set of the class of hail, is asked on its start and answers yes; asked again for befriend; and his machine votes on the wave unasked" "B = \[view(\[\], \[\]), view(\[\], \[alice\]), gave(hail(alice, bob), yes), view(\[waved_at(alice)\], \[alice\]), gave(befriend(alice, bob), yes), view(\[friend(alice), waved_at(alice)\], \[alice\]), view(\[waved_at(alice), friend(alice), waved_at(alice)\], \[alice\]) | " "$cva_waves"
check "CVA run-time class: the three starts, bob's three yeses and the three commits" "N = \[sent(alice, bob, start(attempt(alice, 1, 1, hail(alice, bob)))), sent(bob, alice, vote(attempt(alice, 1, 1, hail(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 1, 1, hail(alice, bob)), commit)), sent(alice, bob, start(attempt(alice, 2, 2, befriend(alice, bob)))), sent(bob, alice, vote(attempt(alice, 2, 2, befriend(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob)), commit)), sent(alice, bob, start(attempt(alice, 3, 3, wave(alice, bob)))), sent(bob, alice, vote(attempt(alice, 3, 3, wave(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 3, 3, wave(alice, bob)), commit)) | " "$cva_waves"

# A class is the union of its parts (vGLP c0bea08, Definition "Local Specification, Part, Opener"; Section 5.4, "Act schemas as local specifications": "the parts of a class are the sets of transactions its schema-bindings determine"; vGLP #14 Cowork, Code_inbox.md, 2026-10-18 21:47 UTC): an attempt names its part, each participant checks and applies that part's function, and on the request propose([t]_j), naming the class by a part, the agent proposes, among the parts of [t] it is ready for, the one it has proposed least recently (Section 5.4, "The compilation").  vGLP's contract parts (programs/vglp/cva/contracts.glp): tap(alice, bob) and pat(alice, bob), one class guarded by alice, of two parts, pat's requiring a coin at both.  alice, holding no coin, requests pat and proposes tap's part; holding one, she requests tap and proposes pat's, never proposed, and bob, holding no coin, votes no, outside the domain of his effect in pat's part though inside it in tap's; she requests tap again and proposes tap's, the less recent; and pat again and proposes pat's, the less recent then.  A program taking the domain over the class would name the part the request names, attempt 2 pat(alice, bob), and bob's machine would vote yes on attempt 4.
cva_parts_load=$("$REPL_RUN" <<HEREDOC
$CVA/parts/
:quit
HEREDOC
2>&1)
check "CVA parts: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/parts" "$cva_parts_load"
check_not "CVA parts: no type errors" "Type checking failed" "$cva_parts_load"

cva_parts=$("$REPL_RUN" <<HEREDOC
$CVA/parts/
:limit 1500000
play_parts(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA parts: alice, holding no coin, requests pat and proposes tap's part, the one she is ready for, attempt 2, which commits" "sent(alice, bob, start(attempt(alice, 2, 2, tap(alice, bob)))), sent(bob, alice, vote(attempt(alice, 2, 2, tap(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 2, 2, tap(alice, bob)), commit))" "$cva_parts"
check "CVA parts: holding a coin, minted at attempt 3, she requests tap and proposes pat's part, never proposed, attempt 4; seeing the notice of its abort, she requests tap again and proposes tap's, the less recent, attempt 5; then pat's, attempt 6" "sent(alice, bob, start(attempt(alice, 4, 4, pat(alice, bob)))), .*sent(alice, bob, start(attempt(alice, 5, 5, tap(alice, bob)))), .*sent(alice, bob, start(attempt(alice, 6, 6, pat(alice, bob))))" "$cva_parts"
check "CVA parts: ... alice's screen, the notice naming the part, pat's, by its term, at each abort" "A = \[view(\[\], \[\]), view(\[\], \[bob\]), view(\[friend(bob)\], \[bob\]), view(\[tapped(bob), friend(bob)\], \[bob\]), view(\[coin, tapped(bob), friend(bob)\], \[bob\]), refused(pat(alice, bob), bob), view(\[tapped(bob), coin, tapped(bob), friend(bob)\], \[bob\]), refused(pat(alice, bob), bob) |" "$cva_parts"
check "CVA parts: bob, holding no coin, votes no on pat's part, his precondition in it failing, and yes on tap's" "sent(alice, bob, start(attempt(alice, 4, 4, pat(alice, bob)))), sent(bob, alice, vote(attempt(alice, 4, 4, pat(alice, bob)), no)), sent(alice, bob, outcome(attempt(alice, 4, 4, pat(alice, bob)), abort)), sent(alice, bob, start(attempt(alice, 5, 5, tap(alice, bob)))), sent(bob, alice, vote(attempt(alice, 5, 5, tap(alice, bob)), yes)), sent(alice, bob, outcome(attempt(alice, 5, 5, tap(alice, bob)), commit))" "$cva_parts"
check "CVA parts: ... and no on attempt 6, pat's" "sent(bob, alice, vote(attempt(alice, 6, 6, pat(alice, bob)), no)), sent(alice, bob, outcome(attempt(alice, 6, 6, pat(alice, bob)), abort)) | " "$cva_parts"
check_not "CVA parts: bob asked nothing for the class, guarded by alice" "gave(attempt([a-z]*, [0-9]*, [0-9]*, \(tap\|pat\)\|gave(\(tap\|pat\)" "$cva_parts"

# The opener per unguarded part (vGLP 099f876, Section 5.4, "Act schemas as local specifications": "The opener of an unguarded part is the least, in the order of identifiers, of the people in the first role of the schema-bindings that determine it"; vGLP #15, Code_inbox.md, 2026-10-19 10:50 UTC, item 3, answering Code #10's fault of 10:16 UTC).  vGLP's contract opens (programs/vglp/cva/contracts.glp): u1(alice, bob) and u2(bob, alice), one class of two unguarded parts, opened by alice and by bob.  bob, holding w(alice) after marking alice, opens u2(bob, alice)'s part, which holds, and proposes it as an own transaction; alice, holding no r(bob), proposes nothing of the class.  An opener taken over the class would be alice, whose agent finds the class only through u1's first role, requiring r(bob), and nobody would open it.
cva_opens_load=$("$REPL_RUN" <<HEREDOC
$CVA/opens/
:quit
HEREDOC
2>&1)
check "CVA opens: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/opens" "$cva_opens_load"
check_not "CVA opens: no type errors" "Type checking failed" "$cva_opens_load"

cva_opens=$("$REPL_RUN" <<HEREDOC
$CVA/opens/
:limit 1500000
play_opens(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA opens: bob marks alice, attempt 2, once he sees her his friend, and then opens the part of u2(bob, alice), an own transaction, attempt 3" "sent(bob, alice, start(attempt(bob, 2, 2, mark(bob, alice)))), .*sent(bob, alice, start(attempt(bob, 3, 3, u2(bob, alice))))" "$cva_opens"
check "CVA opens: ... bob's screen: each committed" "B = \[.*view(\[friend(alice)\], \[alice\]), view(\[w(alice), friend(alice)\], \[alice\]), view(\[y(alice), w(alice), friend(alice)\], \[alice\]) | " "$cva_opens"
check "CVA opens: alice's machine votes yes on its start, unasked, and takes the outcome" "sent(bob, alice, start(attempt(bob, 3, 3, u2(bob, alice)))), sent(alice, bob, vote(attempt(bob, 3, 3, u2(bob, alice)), yes)), sent(bob, alice, outcome(attempt(bob, 3, 3, u2(bob, alice)), commit)) | " "$cva_opens"
check_not "CVA opens: alice, preceding bob, proposes nothing of the class, the part she opens not holding" "start(attempt(alice, [0-9]*, [0-9]*, \(u1\|u2\)" "$cva_opens"
check_not "CVA opens: no person answers for the unguarded parts" "gave(attempt([a-z]*, [0-9]*, [0-9]*, \(u1\|u2\)\|gave(\(u1\|u2\)" "$cva_opens"

# Listed roles (vGLP f5f1644, Section 5.4, "Act schemas as local specifications": "each schema-binding determines a part [t]_j of a class for each list L of the people of its listed roles: the set of transactions it determines with L in those roles, with f^[t]_j_p defined at d_p iff ... for p the person in the base of a listed role, p lists in d_p exactly the people of that role in L ... its request name is the schema-binding"; vGLP #15 Cowork to Code, 2026-10-19 14:24 UTC).  vGLP's contract groups (programs/vglp/cva/contracts.glp): close(a?, a[listed(*, n)] ; n), its part's term carrying the list of the people of its listed role, its request name not.
cva_groups_text=$(cat "$CVA/groups/platform.vglp")
check "CVA groups: the class term of close carries the list of the people of its listed role" "^close(Peer, List(Peer), Speech)$" "$cva_groups_text"
check "CVA groups: ... its request name, the schema-binding, its head with no extent, does not" "^befriend(Peer, Peer) create(Peer, Speech) join(Peer, Peer, Speech) close(Peer, Speech) $" "$(cva_proposal "$CVA/groups/platform.vglp")"
check "CVA groups: the binding of a class term of close, its listed role's place and a role for each of its people (listed_binding)" "^listed_binding(close, \[b(role(1), p(V1?)), b(svar(n), s(V3?))\], V2?, B)$" "$cva_groups_text"
check "CVA groups: the request name completed with the people the agent lists (listed_class)" "^listed_class(close, \[b(role(1), p(V1?)), b(svar(n), s(V3?))\], St?, C)$" "$cva_groups_text"
check "CVA groups: the base person lists exactly the people of the listed role" "^    listing_ok(Nm?, Bn?, Id?, S?, B2),$" "$cva_groups_text"
cva_groups_load=$("$REPL_RUN" <<HEREDOC
$CVA/groups/
:quit
HEREDOC
2>&1)
check "CVA groups: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/groups" "$cva_groups_load"
check_not "CVA groups: no type errors" "Type checking failed" "$cva_groups_load"
cva_groups=$("$REPL_RUN" <<HEREDOC
$CVA/groups/
:limit 3000000
play_groups(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA groups: alice requests close(alice, n), and her agent proposes close(alice, [alice, bob, carol], n), the people she lists, its start told to bob and carol" "sent(alice, bob, start(attempt(alice, 6, 6, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice))))), sent(alice, carol, start(attempt(alice, 6, 6, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)))))" "$cva_groups"
check "CVA groups: ... bob's and carol's machines vote yes, unasked, and it commits at both" "sent(alice, bob, outcome(attempt(alice, 6, 6, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice))), commit)), sent(alice, carol, outcome(attempt(alice, 6, 6, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice))), commit))" "$cva_groups"
check "CVA groups: alice's screen: the group closed, her membership and her listing of herself ended" "A = \[.*view(\[closed(speech(utterance(friends), alice)), listed(carol, speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), friend(carol), friend(bob)\], \[bob, carol\]) |" "$cva_groups"
check "CVA groups: bob's membership ended in the one act" "B = \[.*view(\[member(alice, speech(utterance(friends), alice)), friend(alice)\], \[alice\]), view(\[friend(alice)\], \[alice\]) |" "$cva_groups"
check "CVA groups: ... and carol's" "C = \[.*view(\[member(alice, speech(utterance(friends), alice)), friend(alice)\], \[alice\]), view(\[friend(alice)\], \[alice\]) |" "$cva_groups"
check_not "CVA groups: bob's request to close alice's group, his agent listing no one, is dropped: no attempt of his" "attempt(bob, " "$cva_groups"
check_not "CVA groups: no person asked for close" "gave(attempt([a-z]*, [0-9]*, [0-9]*, close\|gave(close" "$cva_groups"

# No clock and no tick (vGLP efe7bbc, Section 5.4, "Triggers": the agent takes
# its own transactions "when it starts and after each trigger, one at a time
# until none holds"; Udi's approval of 2026-10-18, reported by Code #10): each
# play's run suspends once every agent has taken its own transactions until
# none holds and no trigger is left to read, where until vGLP efe7bbc it ran to
# the REPL's :limit, each agent's clock ticking without end.
for v in cva_sg_menu cva_sg_befriend cva_sg_unfriend cva_sg_refused cva_cur_payswap cva_cur_redeem cva_cur_busy cva_cur_declined cva_nudge cva_intro cva_gnudge cva_cs cva_link cva_friends cva_waves cva_parts cva_opens cva_groups; do
    check "CVA quiescence: the run of $v suspends, no agent left with an own transaction or a trigger" "→ suspended" "${!v}"
    check_not "CVA quiescence: ... the run of $v not stopped at the limit" "capped" "${!v}"
done
echo ""


fi  # Section VG

if _section O; then
# =============================================================================
# Section O: Currencies Multi-Isolate Tests
# =============================================================================

echo "=== Section O: Currencies Multi-Isolate Tests ==="
echo ""
echo "  (folded into Section Q, which runs the whole Dart tree)"
echo ""

fi  # Section O

if _section P; then
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
# lib.glp's procedures are renamed by its path from the root (TGLP modules.tex,
# Compilation, third step).
check "cross-module param-inspect project rejected" "Head of tests/cross_module_inspect_neg/lib:relay" "$output"
check_not "cross-module project not loaded green" "Loaded program: .*cross_module_inspect_neg" "$output"

echo ""

echo "--- A param-inspecting procedure no call instantiates is rejected at load ---"
# The complement of the case above: there, an instantiation arises across the
# seam and the callee's clauses are checked at it. Here run/1's declaration is
# `Channel(_, _)?`, so the call supplies no element type and no instantiation
# of tagger/1 arises; tagger/1 inspects its parameter, so it is not parametrically
# well-typed and has no well-typing, and the program is rejected
# (parameterized-types.tex sec:abstract-parameters, TGLP 4f7027b). From
# 2026-08-03 to 2026-09-18 the load succeeded and printed a `[TYPE] ...
# unchecked in this program` line instead: the silence that let typed_actors.glp
# carry an untagged value at a tagged-union position for months (GLP 54dd7020)
# had become a warning, and a warning is not a refusal. The same clause declared
# concretely is tagged_union_untagged_neg.glp in NEGATIVE_FILES.
output=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/param_unchecked/
:quit
HEREDOC
2>&1)
# The refusal's words since the instantiation of a call by TGLP cc4a891
# (GLP-inst, merged at b9e0bfb3): the call is refused, naming what no site
# supplies and the callee that is not parametrically well-typed.
check "uninstantiated param-inspect rejected at load" "no site of the call supplies a type for X, Y" "$output"
check "the rejection names the procedure" "tagger/1 is not parametrically well-typed" "$output"
check_not "program with an uninstantiated inspecting procedure does not load" "Loaded program: .*param_unchecked" "$output"

echo ""

echo "--- The fixture's two single-isolate REPL boots load ---"
# docs/ma/HOW-TO-RUN.md documents these as the dGLP routes a human runs by hand,
# and until 2026-08-03 nothing tested them: they were two of the 35 files in
# agent_roundtrip that no test named. Both load, so they are working routes and
# stayed when the other 32 — which cannot load at all under the current language
# — were retired to programs/archive/agent_roundtrip/ (deleted 2026-09-16).
# Tested here so that the
# next person asking "does anything need this?" gets an answer from the suite
# rather than from a filename search.
for dglp_boot in play_dglp_boot play_ui_dglp_boot; do
    output=$("$REPL_RUN" <<HEREDOC
$AGENT_RT/typed_social_agent.glp
$AGENT_RT/typed_actors.glp
$AGENT_RT/typed_ui_mediator.glp
$AGENT_RT/typed_ui_actors.glp
$AGENT_RT/$dglp_boot.glp
:quit
HEREDOC
2>&1)
    check "dGLP $dglp_boot loads" "Loaded: .*$dglp_boot.glp" "$output"
    check_not "dGLP $dglp_boot no type error" "Error loading.*$dglp_boot" "$output"
done

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

fi  # Section P

if _section J; then
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

fi  # Section J

if _section S; then
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
# S3, S5, S6 and X6 show their strings in the one canonical print, a, not "a"
# (GLP #3 Cowork, 2026-10-04 15:19 UTC, FAULT 2 (b); until 2026-10-10 "a").
echo "--- S3: two instantiations of the parameterised utility ---"
s3=$("$REPL_RUN" <<HEREDOC
$SCOPE_CHAIN/leaf
test_both(Zi, Zc).
:quit
HEREDOC
2>&1)
check "S3 integer instantiation" "Zi = \[1, 4, 2, 5, 3, 6\]" "$s3"
check "S3 constant instantiation" "Zc = \[a, c, b, d\]" "$s3"

# --- S4: sovereign/denominated is a program (TGLP Definition "Program") ---
# TGLP Definition "Program" (parameterized-types.tex:164, TGLP 9b29563): "a directory
# carrying a self.glp, or a self-contained module, with one or more concrete initial
# goals".  denominated is a directory carrying a self.glp that exports its procedures,
# each declared at concrete types, so it is a program, and it loads through its
# self.glp (Coordination #3's ruling, 2026-10-04 16:05 UTC).  Its types are its
# ancestor's, programs/currencies/sovereign/self.glp, so the I-1 regression, an
# ancestor type failing to resolve through the directory load, is kept.  Until
# 2026-10-13 S4 loaded the play12 directory of the retired bonds program.
echo "--- S4: sovereign/denominated is a program ---"
s4=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/currencies/sovereign/denominated
:quit
HEREDOC
2>&1)
check "S4 denominated is a program: it loads" "Loaded program" "$s4"
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
check "S5 opaque pass-through + append" "S = \[hello" "$s5"

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
check "S6 append via parameterised walker" "S = \[hello" "$s6"

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

# --- S10: a program outside the root is refused ---
# TGLP modules.tex, "Scope construction": "A program lies at or below the
# root, and the scope of each of its modules runs from the root down to that
# module."  The fixture's directory is outside programs/, the root, so its file
# and its directory are each refused as a program, naming the path and the
# sentence, before anything of them is checked (GLP #3 Cowork, 2026-10-04 09:06
# UTC, "23:49").  Until 2026-10-04 the file was compiled in a scope without its
# own directory's self.glp and the root's exposes, and refused only for the
# undefined type Foo that self.glp defines.
echo "--- S10: a program outside the root is refused ---"
s10=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/test/fixtures/outside_hierarchy/m.glp
:quit
HEREDOC
2>&1)
check "S10 file outside the root refused, naming it" 'outside_hierarchy/m.glp lies outside the root' "$s10"
check "S10 file refusal quotes the sentence" 'A program lies at or below the root' "$s10"
check_not "S10 file not checked in a partial scope" 'undefined type "Foo"' "$s10"
s10d=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/test/fixtures/outside_hierarchy
:quit
HEREDOC
2>&1)
check "S10 directory outside the root refused, naming it" 'test/fixtures/outside_hierarchy lies outside the root' "$s10d"
check_not "S10 directory not taken for a program" 'has no entry points' "$s10d"

echo ""

fi  # Section S

if _section X; then
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
# Each module is named by its path from the root (TGLP modules.tex,
# Compilation, third step): a/one.glp is tests/expose/collide/a/one.
check "X4 names module one" "\"tests/expose/collide/a/one\"" "$x4"
check "X4 names module two" "\"tests/expose/collide/b/two\"" "$x4"

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
check "X6 constant instantiation" "Zc = \[a, b\]" "$x6"

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

# --- X8: a directory self.glp's PARAMETERISED type reaches its other modules ---
# code.glp names Para(C) from the directory's own self.glp; Plain, monomorphic
# and from the same file, is the control.  Reported as broken by SGSG on
# 2026-08-03 and it never was: their measurement ran from a program root OUTSIDE
# programs/, where there was then no ancestor scope at all, so NEITHER type was
# in scope; such a program is refused since 2026-10-04 (S10).  Only the
# asymmetry looked like a defect, and that is the
# type-parameter rule: an unknown capitalised name in a declaration reads as a
# type parameter, which Plain can be and Para(Constant) cannot, so Plain went
# quiet and Para was reported unresolved.
echo "--- X8: parameterised type from a directory self.glp ---"
x8=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/param_ancestor_scope
plain([wrap(a), wrap(b)]).
:quit
HEREDOC
2>&1)
check "X8 fixture loaded" "Loaded program" "$x8"
check_not "X8 Para resolves" "Unresolved type: Para" "$x8"
check "X8 goal succeeds" "succeeds" "$x8"

# --- X9: removed 2026-10-04 ---
# One type name at two arities across an exposed module boundary.  The check
# could no longer fail with the linker's name/arity key backed out, the linker
# renaming each type by its module, and a check that cannot fail goes (GLP #3
# Cowork, 2026-10-04 09:06 UTC, "00:34 ... X9: a check that cannot fail goes");
# its fixture, programs/tests/type_name_collision, went with it.

# --- X10: the spm programs load and accept a goal ---
# They are SGSG's since 2026-08-03 and were unloadable until the linked
# program's type definitions were keyed by name and arity (docs/known-issues.md,
# Issue 21); the goal is what X8's goal-check ordering fix made possible (every
# goal used to fail with UnknownTypeError: UserEvent, a gsg/self.glp template
# named by a module one directory below it).  Since 2026-09-07 the program is
# programs/social/spm, spm/self.glp the root exporting the plays as the entry
# points.  Since 2026-10-12 (GSG's layout, its task of 2026-10-02 item 2): each
# platform's plays/ has a self.glp exporting its plays, called `plays # play_x`;
# each platform's self.glp forwards `agent` to its agent module; spm/self.glp
# forwards `network` to `cva # network`, and the plays call `agent(...)` and
# `network(...)` unqualified, resolved up the scope chain.  cva/ still loads on
# its own; gsg/ and secure_gsg/ do not, cva being a sibling outside them.
echo "--- X10: spm programs ---"
x10c=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/social/spm/cva
:quit
HEREDOC
2>&1)
check "X10 spm/cva loads" "Loaded program" "$x10c"
x10=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/social/spm
:quit
HEREDOC
2>&1)
check "X10 spm loads" "Loaded program" "$x10"
x10g=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/social/spm
play_befriend(A, B).
:quit
HEREDOC
2>&1)
check "X10 spm accepts a goal" "succeeds" "$x10g"
check_not "X10 spm goal check resolves UserEvent" "UnknownTypeError: UserEvent" "$x10g"

echo ""

# --- X11: a descendant directory's types stay in the descendant's subtree ---
# The linked program held ONE flat type namespace, so whichever `Lot` the
# directory walk reached first became every module's: `inner/self.glp`'s two-
# argument Lot was what `txn.glp` — a module of the program ABOVE it — was
# checked against, and the program did not load.  Reported by Currencies Code
# on 2026-09-03 against the bonds program of that day with its coins/ directory
# under it.  Steps 3 and 4 of modules.tex §Compilation now rename each module's
# types to `M:T` and resolve every reference to the nearest scope defining it,
# so this holds whichever order the walk takes.  The goal is the second half:
# a goal is posted in the program ROOT's scope, and the descendant's Lot used
# to shadow the root's there too.
echo "--- X11: a descendant self.glp does not redefine an ancestor's types ---"
x11=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/type_scope_descendant
go(lot(a, b, c), C).
:quit
HEREDOC
2>&1)
check "X11 fixture loaded" "Loaded program" "$x11"
check_not "X11 txn keeps its own directory's Lot" "uncovered alternative" "$x11"
check "X11 goal succeeds" "succeeds" "$x11"

# --- X12: a module's own type name is its own, whatever the walk order ---
# `a_agent.glp` sorts before `self.glp`, the order that stopped programs/currencies/bonds
# loading (Currencies Code, 2026-09-03): the two definitions of `Answer` were
# merged into one namespace and the first the filesystem listed won.
echo "--- X12: a module's own type is its own ---"
x12=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/type_scope_module_own
play(N).
:quit
HEREDOC
2>&1)
check "X12 fixture loaded" "Loaded program" "$x12"
check "X12 goal succeeds" "succeeds" "$x12"

# --- X13: an alternative that names a type inherits its transitions ---
# TGLP appendix sec:type-automaton: "If A_j references another type S or S?:
# transitions are inherited from S or S? respectively" — the type union of
# typed-glp.tex.  The automaton built no transition at all for such an
# alternative, so a term of the named type had none from the naming type's
# state: `T ::= A ; b.  A ::= a(Integer, Constant).` rejected a(N, _) at a T?
# position (Currencies Code, 2026-09-03).
echo "--- X13: a type-name alternative inherits transitions ---"
x13=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/type_alt_typename.glp
take(a(3, x), N).
take(b, M).
:quit
HEREDOC
2>&1)
check "X13 fixture loaded" "Loaded" "$x13"
check_not "X13 a/2 has a transition from T?" "No transition for a(2,1)" "$x13"
check "X13 both alternatives run" "N = 3" "$x13"
check "X13 constant alternative runs" "M = 0" "$x13"

# --- X14: a head reader occurrence of a variable a later argument binds ---
# `g12(_, r(Date?), Date, Date?)` — the goal's writer at argument 4 was bound to
# the clause variable's WRITER, which the commit refused as a WxW violation, so
# the call died with "Bad state: WxW violation in commit".  A reader occurrence
# denotes the variable's READER.  Reported by Currencies Code, 2026-09-03: in
# the bonds agent the issuer's date came out unbound after its first fulfilled
# presentation and every later one was refused.
echo "--- X14: a head reader of a later-bound variable ---"
x14=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/head_reader_later_arg.glp
t12(A, B).
:quit
HEREDOC
2>&1)
check_not "X14 no WxW violation" "WxW violation" "$x14"
check "X14 output term carries the value" "A = r(0)" "$x14"
check "X14 output argument carries the value" "B = 0" "$x14"

# --- X15: -expose reads a signature in the exposed module's own scope ---
# TGLP modules.tex, "The -expose directive": -expose(M) "lifts the exported
# procedures of module M (and the types their signatures carry) into that
# directory's scope, as if defined in its self.glp"; "Procedure declarations":
# "A declaration carries the transitive closure of the types its signature
# references".  rates.glp's rate/2 names Level, from lib/self.glp, which is on
# rates.glp's chain and not on the exposing self.glp's or on app/'s: until
# 2026-10-04 the lift read the signature in the receiving scope and refused
# "undefined type Level" --- the root's -expose(social#graph#routing#intro)
# and social/graph/self.glp's AcceptorConsent (Integration, 2026-10-04 11:28
# UTC).  app/self.glp also names Level in a declaration of its own.
echo "--- X15: a signature type from the exposed module's own chain ---"
x15=$("$REPL_RUN" <<HEREDOC
$EXPOSE/own_scope/app
run(N).
rate_of(low, M).
:quit
HEREDOC
2>&1)
check "X15 fixture loaded" "Loaded program" "$x15"
check_not "X15 Level resolves" "undefined type \"Level\"" "$x15"
check "X15 exposed rate/2 runs" "N = 2" "$x15"
check "X15 a declaration over the lifted Level runs" "M = 1" "$x15"

# --- X16: a type no exported signature carries is not lifted ---
# clock.glp's scale/2 carries Unit; Tick is clock.glp's own.  Until 2026-10-04
# every type of the exposed module was lifted (programs/sglp/self.glp's
# -expose(monitor) lifted monitor.glp's Queue and Clock), so neg/ loaded
# (GLP #3 Cowork, 2026-10-04 09:06 UTC, "21:20").
echo "--- X16: an unlifted type is not in the exposing scope ---"
x16p=$("$REPL_RUN" <<HEREDOC
$EXPOSE/unlifted/pos
run(min, N).
:quit
HEREDOC
2>&1)
check "X16 pos loads" "Loaded program" "$x16p"
check "X16 pos runs over the lifted Unit" "N = 60" "$x16p"
x16n=$("$REPL_RUN" <<HEREDOC
$EXPOSE/unlifted/neg
:quit
HEREDOC
2>&1)
check_not "X16 neg not loaded" "Loaded program" "$x16n"
check "X16 neg refused naming Tick" "undefined type \"Tick\" in the declaration of run/1" "$x16n"

# --- X17: a program-root self.glp names the type its -expose lifts ---
# The lifted types are in the exposing directory's scope "as if defined in its
# self.glp", so its own declarations name them.  Until 2026-10-04 the goal-
# check environment merged the program-root self.glp without its lift and the
# program was refused: undefined type "T" in the declaration of p/1 (GLP #3
# Cowork, 2026-10-04 09:06 UTC, "00:34", FAULT 1).
echo "--- X17: the exposing program-root self.glp names a lifted type ---"
x17=$("$REPL_RUN" <<HEREDOC
$EXPOSE/names_lifted
p(a).
p(c).
:quit
HEREDOC
2>&1)
check "X17 fixture loaded" "Loaded program" "$x17"
check_not "X17 T resolves" "undefined type \"T\"" "$x17"
check "X17 goal over T succeeds" "succeeds" "$x17"
check "X17 goal outside T refused" "not well-typed" "$x17"

echo ""

fi  # Section X

if _section Y; then
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

fi  # Section Y

if _section Z; then
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

fi  # Section Z

if _section ZG; then
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

fi  # Section ZG

if _section RM; then
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

# run/3 and find_type/2 (GLP-Spec appendix-guards, "Dynamic activation"; TGLP
# §Dynamic Activation).  find_type(P/N, Type) assigns the type identity of the
# declaration of P/N in the caller's scope, and errs where P/N is not declared
# there; run(Goal, Type, Module) activates Module and posts Goal where Type
# equals the identity Module's exports record for Goal's predicate, and errs
# otherwise.  The identity is a 64-hex SHA-256 (TGLP Implementation Notes).
rm4=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
find_type(hello/1, T).
:quit
HEREDOC
2>&1)
check "RM4 find_type yields the type identity of a declared entry point" "T = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm4"

rm5=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), find_type(hello/1, T), run(hello(Z), T?, M?).
:quit
HEREDOC
2>&1)
check "RM5 run/3 under the recorded identity posts the goal" "Z = \[done\]" "$rm5"

rm6=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), run(hello(Z), "0000", M?).
:quit
HEREDOC
2>&1)
check "RM6 run/3 under another identity is refused" "_run/3: type identity mismatch for hello/1 in module run_module_probe" "$rm6"

rm7=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
find_type(nosuch/1, T).
:quit
HEREDOC
2>&1)
check "RM7 find_type on an undeclared P/N is an error" "_find_type/2: nosuch/1 is not declared in the caller's scope" "$rm7"

# P/N is resolved in the calling module's scope, as a call would be: m.glp's
# probe/1 obtains the identity of its own local/1 (the key m:local/1), which at
# the root is declared nowhere.
rm8=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/find_type_scope
probe(T).
find_type(local/1, U).
:quit
HEREDOC
2>&1)
check "RM8 find_type resolves P/N in the calling module's scope" "T = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm8"
check "RM8 a module-local P/N is not declared at the root" "_find_type/2: local/1 is not declared in the caller's scope" "$rm8"

# An activated module's body call to a root-scope procedure (merge/3)
# (Currencies Code's report of 2026-09-08 12:14 UTC): the root self.glp is the
# first link of the program's chain, and the root procedures the program
# reaches are compiled into its artefact, renamed :p (GLP's round six, item 1);
# until 2026-10-04 they were not, and the goal ran on a root runner.
rm9=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/run_module_probe
self_module(M), run(merged(Z), M?).
:quit
HEREDOC
2>&1)
check "RM9 an activated module reaches a root-scope procedure in its body" "Z = \[a, b\]" "$rm9"

echo ""
fi  # Section RM

if _section SK; then
# =============================================================================
# SECTION SK: Identity, signature and the certificate (self_key/1, sign/3,
# signature/2, decompose_module/4; the OS-privileged refusal)
# =============================================================================
# GLP-Spec appendix-guards, "Identity and signature" and "Module as a value";
# Secure GLP core.tex §The Seam; IGLP code format §Program Artefact and
# "Signed content"; SGSG Section 3 "What the super-app grants a mini-app" and
# Section 6.1 G1.  Keys, hashes and signed terms are hex strings; a key and a
# hash are 64 characters.  The probe runs in madGLP mode (:mad), as SGSG's
# harness will.
echo "=== Section SK: Identity, signature, certificate ==="
# A key or an identity is 64 hex digits, shown in single quotes where it begins
# with a digit, which unquoted would read as a number (GLP #3 Cowork,
# 2026-10-04 09:06 UTC, "23:49. Q1 and Q3"; A41), and bare otherwise.

sk1=$("$REPL_RUN" <<HEREDOC
:mad alice
$GLP_DIR/programs/tests/sign_probe
self_key(K).
roundtrip(hello(world), R).
:quit
HEREDOC
2>&1)
check "SK1 madGLP mode enters for a directory load" "madGLP mode on: agent alice" "$sk1"
check "SK1 self_key assigns the person's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sk1"
check "SK1 sign then signature round-trips the term as signed(K, H, T) under the signer's key and module identity" "R = signed(\('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\), \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\), hello(world))" "$sk1"

# The key the signed term carries is self_key's answer, and the module identity
# is this program's source identity, which decompose_module also reports.
sk2=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/sign_probe
self_key(K), sign(a_term, K?, S), signer(S?, K1).
identities(K, S, C).
:quit
HEREDOC
2>&1)
sk2_keys=$(echo "$sk2" | grep "K1\{0,1\} = " | grep -o "[0-9a-f]\{64\}" | sort -u | wc -l | tr -d ' ')
check "SK2 signature/2 matched as signed(K, _, _) gives the signer, and it is self_key's answer" "1" "$sk2_keys"
check "SK2 decompose_module assigns the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sk2"
check "SK2 decompose_module assigns the source identity" "S = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sk2"
check "SK2 decompose_module assigns the compiled identity" "C = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sk2"

# sign/3 signs only under a key whose private half the runtime holds (G2).
sk3=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/sign_probe
sign(hello, "0000000000000000000000000000000000000000000000000000000000000000", S).
:quit
HEREDOC
2>&1)
check "SK3 sign under another person's key is an error" "_sign/3: the runtime holds no private key for" "$sk3"

# signature/2 on a term that is not a verified signed term answers unsigned: a
# value the caller matches and not a failure, so the goal succeeds and the
# caller takes its unsigned branch.
sk4=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/sign_probe
signer("00ff", K).
signer(foo(bar), K).
:quit
HEREDOC
2>&1)
sk4_unsigned=$(echo "$sk4" | grep -c "K = unsigned" | tr -d ' ' || true)
check "SK4 signature/2 on a hex string that is no signed term, and on a term that is no string, answers unsigned" "2" "$sk4_unsigned"
check_not "SK4 unsigned is a value, not a failure: neither goal fails" "→ failed" "$sk4"

# The certificate: a mini-app calling nothing that reaches the network or the
# person is certified; one that does is refused, naming the offending calls,
# and decompose_module has no compiler's key to give for it.
sk5=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/cert_ok
self_module(M), decompose_module(M?, K, S, C).
double(21, Y).
:quit
HEREDOC
2>&1)
check_not "SK5 a mini-app reaching neither the network nor the person is certified" "CERTIFICATE REFUSED" "$sk5"
check "SK5 its certificate carries the compiler's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sk5"
check "SK5 it runs" "Y = 42" "$sk5"

sk6=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/cert_refused
self_module(M), decompose_module(M?, K, S, C).
:quit
HEREDOC
2>&1)
# The calls are named by their procedures' renamed names, each module's path
# from the root (TGLP modules.tex, Compilation, third step).
check "SK6 a mini-app calling send_to_net/1 is refused a certificate naming the call" "CERTIFICATE REFUSED.*tests/cert_refused/app:leak/1 calls send_to_net/1" "$sk6"
check "SK6 a wrapper reaching send_to_user/1 does not pass" "app:wrapper/1 calls send_to_user/1" "$sk6"
check "SK6 the refused module has no compiler's key" "_decompose_module/4: module cert_refused carries no certificate" "$sk6"

echo ""
fi  # Section SK

if _section MB; then
# =============================================================================
# SECTION MB: multi-agent boot programs run from the REPL (:boot)
# =============================================================================
# The two-agent madGLP harness (IGLP Cowork 2026-09-08 08:08, item 1): `:boot
# <f>_boot.glp` spawns an isolate per agent of the boot clause, each under its
# own key pair, loads the program directory beside the boot file in each, and
# returns when the traffic has settled; what an agent sends to its person is
# printed as `[agent] term`.  MB1: a forwarded link is held at both ends, each
# end reports pending_link and authorises, and the value is delivered (IGLP
# madglp-spec \S Held Links).  MB2: a module value shipped from one agent to
# another is activated there by run/3 under the type identity find_type/2
# gives (GLP-Spec appendix-guards, "Dynamic activation"): a certified
# mini-app, hello/1 alone (programs/tests/mad_ship_hello), which :artefact
# writes beside alice's program and alice reads with load_file/2, since the
# machine activates no program whose certificate does not verify (GSG
# s6-security.tex, G1).
echo "=== Section MB: multi-agent boot programs (:boot) ==="

mb1=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/mad_held_links_boot.glp
:quit
HEREDOC
2>&1)
check "MB1 the boot settles with three agents" "Boot settled: 3 agents" "$mb1"
check "MB1 the holder's assignment is held and reported, from the forwarding friend" "\[carol\] held(carol, _w(alice, [0-9]*), bob)" "$mb1"
check "MB1 the anchor's incoming assignment is held and reported, from the holder" "\[alice\] held(alice, _w(alice, [0-9]*), carol)" "$mb1"
check "MB1 both ends authorised, the value is delivered at the anchor" "\[alice\] delivered(hello_from_carol)" "$mb1"

# MB2's artefact is a build output, ignored by git, written before the boot and
# removed after it, as Section SL's are.  Until 2026-10-02 alice shipped her
# own program, which calls send_to_net/1 and send_to_user/1 and is refused a
# certificate, and since B6 (e5da7ccb) bob's run/3 refused to activate it
# (GLP #3 Cowork, 2026-10-02 20:58 UTC, "20:31" item 2).
MB_SHIP="$GLP_DIR/programs/tests/mad_ship_module"
rm -f "${MB_SHIP:?}"/*.glpw
mb2=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/tests/mad_ship_hello $MB_SHIP
:boot $GLP_DIR/programs/tests/mad_ship_module_boot.glp
:quit
HEREDOC
2>&1)
rm -f "${MB_SHIP:?}"/*.glpw
check "MB2 :artefact writes the certified mini-app beside alice's program" "Wrote $MB_SHIP/mad_ship_hello.glpw --- certified under [0-9a-f]\{64\}" "$mb2"
check "MB2 the boot settles with two agents" "Boot settled: 2 agents" "$mb2"
check "MB2 a shipped module value is activated by run/3 under find_type's identity" "\[bob\] ran(\[done\])" "$mb2"

# MB3: a writer end that travels INSIDE A STREAM ELEMENT --- one level below
# the link itself --- and the holder's assignment coming back (SGSG Code's
# report of 2026-09-08 12:53 UTC; madglp-spec, Definition Globalize).  alice
# cold-calls bob with a stream, writes [hello, probe(W)] on it keeping W's
# reader, and bob assigns W.  This is the first step of the warm call.
mb3=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/mad_stream_writer_boot.glp
:quit
HEREDOC
2>&1)
check "MB3 the holder reads both elements of the stream" "\[bob\] saw(probe)" "$mb3"
check "MB3 a writer inside a stream element crosses, and the assignment comes back" "\[alice\] got(hi)" "$mb3"

# MB4: an end that arrived TWO CROSSINGS down, written into in a LATER
# reduction than the one that handed it over (SGSG Code, 2026-09-08 17:14).  A
# `global_send` goal watching such a tail was realised as a heap callback and
# never fired when the tail became known by the resolution of a variable chain,
# so the later element was produced and never sent; the warm call of SGSG's
# Section 5.2 stopped exactly there.  Without the repair bob sees saw(hello)
# and nothing after it.
mb4=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/mad_two_crossings_boot.glp
:quit
HEREDOC
2>&1)
check "MB4 the twice-crossed end delivers the hand-over reduction's element" "\[bob\] saw(hello)" "$mb4"
check "MB4 and the element written into its tail afterwards" "\[bob\] saw(probe)" "$mb4"
check "MB4 the holder's assignment comes back" "\[alice\] got(hi)" "$mb4"

# MB5: the link probes (programs/tests/linkprobes, and linkprobes.md beside
# them).  Eight `:boot` programs that fence in what crosses a link and what
# does not: each is a cold call between isolates, after which one side writes
# on a stream the other reads.  They differ in how many crossings the reader's
# end made before it was read, and whether the elements were written in one
# reduction or in two.  They were the evidence for the defect repaired on
# 2026-09-08 in lib/runtime/heap_fcp.dart (fireBoundCallbacks), where a value
# written into the tail of a stream whose reader had already crossed a link was
# produced and never sent, and until now nothing in the suite loaded them: MB4
# above covers the same repair on one program, and these cover the six shapes
# that isolate it.  linkprobe13 and linkprobe14 are the pair in which the
# channel is handed over by a third agent, so every link is held and neither
# probe calls authorise_link/2: nothing passing is the correct behaviour, not a
# defect (withdrawn by SGSG Code, 2026-09-09).
LP_DIR="$GLP_DIR/programs/tests/linkprobes"
rm -f "$LP_DIR"/runprobe/*.glpw

lp7=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe7_boot.glp $LP_DIR/linkprobe7
:quit
HEREDOC
2>&1)
check "MB5 linkprobe7 two crossings, both elements in one reduction: the boot settles" "Boot settled: 2 agents" "$lp7"
check "MB5 linkprobe7 the twice-crossed end is read" "\[bob\] saw(root)" "$lp7"
check "MB5 linkprobe7 both its elements arrive" "\[bob\] saw(probe)" "$lp7"
check "MB5 linkprobe7 the holder's assignment comes back" "\[alice\] got(hi)" "$lp7"

lp9=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe9_boot.glp $LP_DIR/linkprobe9
:quit
HEREDOC
2>&1)
check "MB5 linkprobe9 one crossing, the write split: the greeting arrives" "\[bob\] saw(hello)" "$lp9"
check "MB5 linkprobe9 and the element written into the tail later" "\[bob\] saw(probe)" "$lp9"
check "MB5 linkprobe9 the holder's assignment comes back" "\[alice\] got(hi)" "$lp9"

# linkprobe10 is the warm call's stop itself: linkprobe7's two crossings with
# linkprobe9's later write.  Before the repair bob saw the greeting and nothing
# after it.
lp10=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe10_boot.glp $LP_DIR/linkprobe10
:quit
HEREDOC
2>&1)
check "MB5 linkprobe10 two crossings and the write split: the greeting arrives" "\[bob\] saw(hello)" "$lp10"
check "MB5 linkprobe10 and the element written into the tail afterwards" "\[bob\] saw(probe)" "$lp10"
check "MB5 linkprobe10 the holder's assignment comes back" "\[alice\] got(hi)" "$lp10"

# linkprobe11: the later element is ground, a second greeting carrying no
# writer, so what linkprobe10 shows is the later write and not the writer in it.
lp11=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe11_boot.glp $LP_DIR/linkprobe11
:quit
HEREDOC
2>&1)
lp11_hellos=$(echo "$lp11" | grep -c "\[bob\] saw(hello)" || true)
check "MB5 linkprobe11 a ground later element crosses too: both greetings arrive" "^2$" "$lp11_hellos"

# linkprobe12: the stream is left open after the probe, as a live
# conversation's is, so the closing of the stream is not what carried it.
lp12=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe12_boot.glp $LP_DIR/linkprobe12
:quit
HEREDOC
2>&1)
check "MB5 linkprobe12 the later element crosses on a stream left open" "\[bob\] saw(probe)" "$lp12"
check "MB5 linkprobe12 the holder's assignment comes back" "\[alice\] got(hi)" "$lp12"

# runprobe: the writer is created by a mini-app that run/3 activated, from the
# certified artefact beside the program --- activation is not what stops it.
lpr=$("$REPL_RUN" <<HEREDOC
:artefact $LP_DIR/probeapp $LP_DIR/runprobe
:boot $LP_DIR/runprobe_boot.glp $LP_DIR/runprobe
:quit
HEREDOC
2>&1)
check "MB5 runprobe the mini-app's artefact is certified" "probeapp.glpw --- certified under" "$lpr"
check "MB5 runprobe an activated mini-app's writer crosses: the greeting arrives" "\[bob\] saw(hello)" "$lpr"
check "MB5 runprobe and the element it wrote into the tail" "\[bob\] saw(probe)" "$lpr"
check "MB5 runprobe the holder's assignment comes back" "\[alice\] got(hi)" "$lpr"

# linkprobe13 and linkprobe14: a channel handed over by a third agent.  Every
# link so made is held, and neither probe calls authorise_link/2, so neither
# greeting is delivered --- the held link doing its work, not a defect.
lp13=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe13_boot.glp $LP_DIR/linkprobe13
:quit
HEREDOC
2>&1)
check "MB5 linkprobe13 both ends of the channel cross: the boot settles" "Boot settled: 3 agents" "$lp13"
check_not "MB5 linkprobe13 nothing passes on a held link neither side authorises" "saw(hello_from" "$lp13"

lp14=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe14_boot.glp $LP_DIR/linkprobe14
:quit
HEREDOC
2>&1)
check "MB5 linkprobe14 one end forwarded by a third agent: the boot settles" "Boot settled: 3 agents" "$lp14"
check_not "MB5 linkprobe14 nothing passes on a held link neither side authorises" "saw(hello_from" "$lp14"

rm -f "$LP_DIR"/runprobe/*.glpw

echo ""
fi  # Section MB

if _section SL; then
# =============================================================================
# SECTION SL: load_file/2 and the artefact on disc (:artefact)
# =============================================================================
# GLP-Spec appendix-guards, "Compilation and file reading": load_file(Name,
# Content) reads the file Name names, within the calling module's own
# directory, and assigns Content the module it holds where it is a certified
# compiled program --- the certificate verified as the loader verifies it ---
# and its text otherwise.  The artefacts are written by the REPL's :artefact
# into the loading program's directory at test time (they are build outputs,
# ignored by git) and removed at the end of the section.
echo "=== Section SL: load_file/2 and :artefact ==="

SL_DIR="$GLP_DIR/programs/tests/load_probe"
SL_MAD="$GLP_DIR/programs/tests/mad_load_module"
rm -f "$SL_DIR"/*.glpw "$SL_MAD"/*.glpw

sl0=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/tests/run_module_probe $SL_DIR
:artefact $GLP_DIR/programs/tests/run_module_probe $SL_MAD
:quit
HEREDOC
2>&1)
check "SL0 :artefact writes a certified artefact into the loading program's directory" "Wrote $SL_DIR/run_module_probe.glpw --- certified under [0-9a-f]\{64\}" "$sl0"

# The forged artefact loads as its text, which is binary; the capture keeps
# printable ASCII only, so the check reads clean lines.
# A forged artefact: the certified one with a byte inside the signature ---
# the 40th from the end --- replaced by its complement, so the file always
# differs (a signature's last byte is often zero already, which setting it to
# zero would leave unchanged).
cp "$SL_DIR/run_module_probe.glpw" "$SL_DIR/forged.glpw"
sl_size=$(stat -f%z "$SL_DIR/forged.glpw")
sl_off=$((sl_size - 40))
sl_byte=$(xxd -s "$sl_off" -l 1 -p "$SL_DIR/forged.glpw")
sl_new=$(printf '%02x' $(( 0x$sl_byte ^ 0xff )))
printf "\\x$sl_new" | dd of="$SL_DIR/forged.glpw" bs=1 seek="$sl_off" conv=notrunc 2>/dev/null

sl1=$("$REPL_RUN" <<HEREDOC
$SL_DIR
load("run_module_probe.glpw", C).
activate("run_module_probe.glpw", Z).
load("note.txt", T).
load("forged.glpw", F).
load("../cert_ok/self.glp", E).
load("/etc/hosts", A).
:quit
HEREDOC
2>&1 | tr -cd '\11\12\15\40-\176')
check "SL1 a certified artefact loads as a Module" "C = Module(run_module_probe)" "$sl1"
check "SL1 run/3 activates the loaded module under find_type's identity" "Z = \[done\]" "$sl1"
check "SL1 a text file loads as its text" "T = 'hello world\\\\n'" "$sl1"
check "SL1 an artefact whose certificate does not verify loads as text, not a Module" "F = 'GLPW" "$sl1"
check "SL1 a name that leaves the caller's directory is an error" "_load_file/2: ../cert_ok/self.glp is not resolved within the calling module's directory" "$sl1"
check "SL1 an absolute name is an error" "_load_file/2: /etc/hosts is not resolved within the calling module's directory" "$sl1"

# Under the boot harness: an agent reads the artefact beside its program and
# activates it --- the super-app installing a mini-app it did not compile.
sl2=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/mad_load_module_boot.glp
:quit
HEREDOC
2>&1)
check "SL2 under :boot an agent loads the artefact from its program's directory and activates it" "\[alice\] ran(\[done\])" "$sl2"

rm -f "$SL_DIR"/*.glpw "$SL_MAD"/*.glpw
echo ""

fi  # Section SL

if _section MA; then
# =============================================================================
# SECTION MA: tests/multiagent program directories load under strict types
# =============================================================================
# The twelve madGLP boot programs, converted 2026-08-02 from single boot files
# into program directories: <name>_boot.glp keeps the boot clause alone and
# <name>/ holds self.glp (entry points exported and forwarded) and agents.glp
# (the code).  The boot clause stays OUT of the directory — BootLoader strips it
# on the boot path, but a directory load does not, and its @ atoms would then
# meet the type checker.  Their runtime behaviour is multiagent_glp_test.dart in
# Section Q, which loads each directory via BootConfig.programDir; this section
# gates the thing the conversion was for, that each loads clean under strict
# types.  A previous shape of this conversion had 8 of the 12 failing; all
# twelve load here, so that split was the shape's and not the language's.
echo "=== Section MA: tests/multiagent program directories ==="
echo ""

for ma_dir in "$GLP_DIR"/programs/tests/multiagent/*/; do
    ma_name=$(basename "$ma_dir")
    ma_out=$("$REPL_RUN" <<HEREDOC
$ma_dir
:quit
HEREDOC
2>&1)
    check "MA $ma_name loads as a program" "Loaded program" "$ma_out"
done

# The fixture's own converted boots.  Their code is not in the directories:
# agent/4, the three actor scripts, ui_mediator/5 and ui_actor/2 are reached
# through -expose in the parent programs/tests/agent_roundtrip/self.glp, because
# the ten alternative plays there cannot be one program (five boot/0, fifteen
# social_graph/3 over three message protocols).
for ma_dir in "$GLP_DIR"/programs/tests/agent_roundtrip/play_madglp/ \
              "$GLP_DIR"/programs/tests/agent_roundtrip/play_ui_madglp/; do
    ma_name=$(basename "$ma_dir")
    ma_out=$("$REPL_RUN" <<HEREDOC
$ma_dir
:quit
HEREDOC
2>&1)
    check "MA $ma_name loads as a program" "Loaded program" "$ma_out"
done
echo ""

fi  # Section MA

if _section RV; then
# =============================================================================
# Section RV: the rendezvous example (GNA's, programs/examples/rendezvous)
# =============================================================================
# GNA Section "Rendezvous Server": the rendezvous agent, its clients and
# friend-mediated rendezvous.  The directory is loaded as a program and each
# test goal of rv_agent_test.glp, rv_client_test.glp and
# friend_mediated_rendezvous_test.glp is posted through the directory's
# self.glp, which exports it; each check holds the goal to the answer the
# test's own comment states.  _rv_answer N is the REPL's answer to the Nth
# goal posted: the text from the (N+1)th prompt to the next, the first prompt
# answering the load.  Under the plain REPL no signature over a pair is made
# under the social graph, so every reconnect and available through the agent
# is rejected; what peer_address/2 reports, address(S) or none, is given to
# the agent's dispatch on it directly.  Test 22 alone has its test program
# stand in for the graph, so an available is admitted and reaches
# peer_address/2, which with no networking layer bound assigns none (IGLP,
# "One interface").  punch_udp/1 is the root's, which there returns having
# done nothing; rv_client.glp and fmrv.glp carried stubs of it until
# 2026-10-10.
echo "=== Section RV: Rendezvous example (GNA) ==="
echo ""

rv_out=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/examples/rendezvous
test1_needs_stored(P1).
test2_ready_stored(P2).
test3_match_find_ready(RA3, RB3, P3).
test4_match_find_needs(RA4, RB4, P4).
test5_find_ready_no_match(P5).
test6_find_ready_skip_then_match(RA6, RB6, P6).
test7_agent_single_reconnect(R7).
test8_available_then_reconnect_rejected(RA8, RB8).
test9_two_reconnects_rejected(R91, R92).
test10_reconnect_then_available(RA10, RB10).
test11_reconnect_rejected(R11).
test12_rejected_does_not_block_stream(R121, R122, RB12).
test13_dispatch_refuses(R131, R132, R133, R134).
test14_other_program_rejected(R14).
test15_match_on_none(R15, P15).
test16_match_on_address(RA16, RB16, P16).
test17_available_dispatch_refuses(R171, R172, R173, R174).
test18_unverified_available_not_matched(RB18, RA18).
test19_reconnect_pair_out_of_order(R19).
test20_match_available_on_none(RB20, P20).
test21_match_available_on_address(RA21, RB21, P21).
client1_make_reconnect(M1, CR1).
client2_make_available(M2, CR2).
client3_full_scenario_a_first(CA3, CB3).
client4_full_scenario_b_first(CA4, CB4).
client5_full_scenario_with_handlers.
client6_bad_signature(CA6, CB6).
client7_handle_reply_punch.
fmrv1_addr_update.
fmrv2_mediate(FA2, FC2).
fmrv3_punch_to.
fmrv4_empty.
fmrv5_mediate_unknown(FA5).
fmrv6_punch_to_not_found.
fmrv7_mediate_unknown_from(FA7).
:trace
test22_available_admitted_no_layer(S22, RB22, RA22).
:quit
HEREDOC
2>&1)
_rv_answer() {
    printf '%s\n' "$rv_out" | awk -v n="$1" '/^GLP> /{k++; sub(/^GLP> /, "")} k==n+1'
}
check "RV rendezvous loads as a program" "Loaded program" "$(_rv_answer 0)"

# rv_agent_test.glp
check "RV agent test1 succeeds" "^→ succeeds$" "$(_rv_answer 1)"
check "RV agent test1: A first, its needs entry is stored" "^P1 = \[needs(alice, bob, addr_a, X[0-9]*)\]$" "$(_rv_answer 1)"
check "RV agent test2 succeeds" "^→ succeeds$" "$(_rv_answer 2)"
check "RV agent test2: B first, its ready entry is stored with B's address" "^P2 = \[ready(bob, alice, addr_b, X[0-9]*)\]$" "$(_rv_answer 2)"
check "RV agent test3 succeeds" "^→ succeeds$" "$(_rv_answer 3)"
check "RV agent test3: A after B, A punches to B's address" "^RA3 = punch(addr_b)$" "$(_rv_answer 3)"
check "RV agent test3: A after B, B punches to A's address" "^RB3 = punch(addr_a)$" "$(_rv_answer 3)"
check "RV agent test3: A after B, nothing left pending" "^P3 = \[\]$" "$(_rv_answer 3)"
check "RV agent test4 succeeds" "^→ succeeds$" "$(_rv_answer 4)"
check "RV agent test4: B after A, A punches to B's address" "^RA4 = punch(addr_b)$" "$(_rv_answer 4)"
check "RV agent test4: B after A, B punches to A's address" "^RB4 = punch(addr_a)$" "$(_rv_answer 4)"
check "RV agent test4: B after A, nothing left pending" "^P4 = \[\]$" "$(_rv_answer 4)"
check "RV agent test5 succeeds" "^→ succeeds$" "$(_rv_answer 5)"
check "RV agent test5: another pair's entry passes, A's needs is stored after it" "^P5 = \[ready(carol, dave, addr_c, X[0-9]*), needs(alice, bob, addr_a, X[0-9]*)\]$" "$(_rv_answer 5)"
check "RV agent test6 succeeds" "^→ succeeds$" "$(_rv_answer 6)"
check "RV agent test6: past another pair's entry, A punches to B's address" "^RA6 = punch(addr_b)$" "$(_rv_answer 6)"
check "RV agent test6: past another pair's entry, B punches to A's address" "^RB6 = punch(addr_a)$" "$(_rv_answer 6)"
check "RV agent test6: the other pair's entry is left pending" "^P6 = \[ready(carol, dave, addr_c, X[0-9]*)\]$" "$(_rv_answer 6)"
check "RV agent test7: the agent terminates on the empty stream" "^→ succeeds$" "$(_rv_answer 7)"
check "RV agent test7: an unsigned reconnect is rejected" "^R7 = rejected$" "$(_rv_answer 7)"
check "RV agent test8 succeeds" "^→ succeeds$" "$(_rv_answer 8)"
check "RV agent test8: available then reconnect, the reconnect is rejected" "^RA8 = rejected$" "$(_rv_answer 8)"
check "RV agent test8: available then reconnect, the unsigned available is rejected" "^RB8 = rejected$" "$(_rv_answer 8)"
check "RV agent test9 succeeds" "^→ succeeds$" "$(_rv_answer 9)"
check "RV agent test9: two reconnects, the first is rejected" "^R91 = rejected$" "$(_rv_answer 9)"
check "RV agent test9: two reconnects, the second is rejected" "^R92 = rejected$" "$(_rv_answer 9)"
check "RV agent test10 succeeds" "^→ succeeds$" "$(_rv_answer 10)"
check "RV agent test10: reconnect then available, the reconnect is rejected" "^RA10 = rejected$" "$(_rv_answer 10)"
check "RV agent test10: reconnect then available, the unsigned available is rejected" "^RB10 = rejected$" "$(_rv_answer 10)"
check "RV agent test11 succeeds" "^→ succeeds$" "$(_rv_answer 11)"
check "RV agent test11: an invalid signature is rejected" "^R11 = rejected$" "$(_rv_answer 11)"
check "RV agent test12 succeeds" "^→ succeeds$" "$(_rv_answer 12)"
check "RV agent test12: the first reconnect is rejected" "^R121 = rejected$" "$(_rv_answer 12)"
check "RV agent test12: the reconnect behind the available is rejected" "^R122 = rejected$" "$(_rv_answer 12)"
check "RV agent test12: the available behind a rejection is consumed and rejected" "^RB12 = rejected$" "$(_rv_answer 12)"
check "RV agent test13 succeeds" "^→ succeeds$" "$(_rv_answer 13)"
check "RV agent test13: unsigned is refused" "^R131 = rejected$" "$(_rv_answer 13)"
check "RV agent test13: another signer is refused" "^R132 = rejected$" "$(_rv_answer 13)"
check "RV agent test13: another term is refused" "^R133 = rejected$" "$(_rv_answer 13)"
check "RV agent test13: another program's Hash is refused" "^R134 = rejected$" "$(_rv_answer 13)"
check "RV agent test14 succeeds" "^→ succeeds$" "$(_rv_answer 14)"
check "RV agent test14: a signature that verifies under another program is rejected" "^R14 = rejected$" "$(_rv_answer 14)"
check "RV agent test15 succeeds" "^→ succeeds$" "$(_rv_answer 15)"
check "RV agent test15: the layer observes no address for A, the reconnect is rejected" "^R15 = rejected$" "$(_rv_answer 15)"
check "RV agent test15: the layer observes no address for A, B's entry is left pending" "^P15 = \[ready(bob, alice, addr_b, X[0-9]*)\]$" "$(_rv_answer 15)"
check "RV agent test16 succeeds" "^→ succeeds$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, A punches to B's address" "^RA16 = punch(addr_b)$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, B punches to it" "^RB16 = punch(addr_a)$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, nothing left pending" "^P16 = \[\]$" "$(_rv_answer 16)"
check "RV agent test17 succeeds" "^→ succeeds$" "$(_rv_answer 17)"
check "RV agent test17: an unsigned available is refused" "^R171 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available signed by B itself is refused" "^R172 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available over the pair out of order is refused" "^R173 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available under another program's Hash is refused" "^R174 = rejected$" "$(_rv_answer 17)"
check "RV agent test18 succeeds" "^→ succeeds$" "$(_rv_answer 18)"
check "RV agent test18: an unverified available is rejected" "^RB18 = rejected$" "$(_rv_answer 18)"
check "RV agent test18: an unverified available leaves the waiting A untold" "^RA18 = <unbound>$" "$(_rv_answer 18)"
check "RV agent test19 succeeds" "^→ succeeds$" "$(_rv_answer 19)"
check "RV agent test19: a reconnect over the pair out of order is refused" "^R19 = rejected$" "$(_rv_answer 19)"
check "RV agent test20 succeeds" "^→ succeeds$" "$(_rv_answer 20)"
check "RV agent test20: the layer observes no address for B, the available is rejected" "^RB20 = rejected$" "$(_rv_answer 20)"
check "RV agent test20: the layer observes no address for B, A's entry is left pending" "^P20 = \[needs(alice, bob, addr_a, X[0-9]*)\]$" "$(_rv_answer 20)"
check "RV agent test21 succeeds" "^→ succeeds$" "$(_rv_answer 21)"
check "RV agent test21: the layer observes B's address, A punches to it" "^RA21 = punch(addr_b)$" "$(_rv_answer 21)"
check "RV agent test21: the layer observes B's address, B punches to A's address" "^RB21 = punch(addr_a)$" "$(_rv_answer 21)"
check "RV agent test21: the layer observes B's address, nothing left pending" "^P21 = \[\]$" "$(_rv_answer 21)"

# rv_client_test.glp
check "RV client test1 succeeds" "^→ succeeds$" "$(_rv_answer 22)"
check "RV client test1: make_reconnect builds reconnect(A, B, Sig, Reply)" "^M1 = reconnect(alice, bob, sig_ab, X[0-9]*)$" "$(_rv_answer 22)"
check "RV client test1: make_reconnect leaves the reply unbound" "^CR1 = <unbound>$" "$(_rv_answer 22)"
check "RV client test2 succeeds" "^→ succeeds$" "$(_rv_answer 23)"
check "RV client test2: make_available builds available(B, A, Sig, Reply)" "^M2 = available(bob, alice, sig_ba, X[0-9]*)$" "$(_rv_answer 23)"
check "RV client test2: make_available leaves the reply unbound" "^CR2 = <unbound>$" "$(_rv_answer 23)"
check "RV client test3 succeeds" "^→ succeeds$" "$(_rv_answer 24)"
check "RV client test3: A first, A's reconnect is rejected" "^CA3 = rejected$" "$(_rv_answer 24)"
check "RV client test3: A first, B's unsigned available is rejected" "^CB3 = rejected$" "$(_rv_answer 24)"
check "RV client test4 succeeds" "^→ succeeds$" "$(_rv_answer 25)"
check "RV client test4: B first, A's reconnect is rejected" "^CA4 = rejected$" "$(_rv_answer 25)"
check "RV client test4: B first, B's unsigned available is rejected" "^CB4 = rejected$" "$(_rv_answer 25)"
check "RV client test5: both rejected replies take handle_reply's rejected arm" "^→ succeeds$" "$(_rv_answer 26)"
check "RV client test6 succeeds" "^→ succeeds$" "$(_rv_answer 27)"
check "RV client test6: a bad signature is rejected" "^CA6 = rejected$" "$(_rv_answer 27)"
check "RV client test6: B's unsigned available is rejected" "^CB6 = rejected$" "$(_rv_answer 27)"
check "RV client test7: handle_reply dispatches punch(Addr) to punch_udp" "^→ succeeds$" "$(_rv_answer 28)"

# friend_mediated_rendezvous_test.glp
check "RV fmrv test1: an addr_update is consumed" "^→ succeeds$" "$(_rv_answer 29)"
check "RV fmrv test2 succeeds" "^→ succeeds$" "$(_rv_answer 30)"
check "RV fmrv test2: alice is sent carol's address" "^FA2 = \[msg(bob, alice, punch_to(found(addr_carol))) | X[0-9]*\]$" "$(_rv_answer 30)"
check "RV fmrv test2: carol is sent alice's address" "^FC2 = \[msg(bob, carol, punch_to(found(addr_alice))) | X[0-9]*\]$" "$(_rv_answer 30)"
check "RV fmrv test3: punch_to(found(Addr)) calls punch_udp" "^→ succeeds$" "$(_rv_answer 31)"
check "RV fmrv test4: the empty stream terminates" "^→ succeeds$" "$(_rv_answer 32)"
check "RV fmrv test5 succeeds" "^→ succeeds$" "$(_rv_answer 33)"
check "RV fmrv test5: mediation to an unknown peer sends alice not_found" "^FA5 = \[msg(bob, alice, punch_to(not_found)) | X[0-9]*\]$" "$(_rv_answer 33)"
check "RV fmrv test6: punch_to(not_found) is a no-op" "^→ succeeds$" "$(_rv_answer 34)"
check "RV fmrv test7 succeeds" "^→ succeeds$" "$(_rv_answer 35)"
check "RV fmrv test7: an unknown requester sends alice not_found" "^FA7 = \[msg(bob, alice, punch_to(not_found)) | X[0-9]*\]$" "$(_rv_answer 35)"

# rv_agent_test.glp test 22, posted last and under :trace (answer 36 is the
# :trace command's): the admitting path.  The test program signs the pair of
# self_key's K and bob under K and gives the agent as Graph the identity
# signature/2 reports, so bob's available for K verifies and is admitted; the
# trace shows the agent's peer_address/2 call, which only an admitted request
# makes, and with no layer bound it assigns none (IGLP, "One interface"), so
# bob is rejected and K, waiting, is told nothing.
rv_key="\('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)"
check "RV agent test22 succeeds" "^→ succeeds$" "$(_rv_answer 37)"
check "RV agent test22: the available's signature verifies, A's own over the pair in the fixed order" "^S22 = signed($rv_key, $rv_key, attest(\(\1, bob\|bob, \1\)))$" "$(_rv_answer 37)"
check "RV agent test22: the admitted available reaches the layer, which observes no address for B" "^:peer_address(bob, none) :- true$" "$(_rv_answer 37)"
check "RV agent test22: no address observed for B, the admitted available is rejected" "^RB22 = rejected$" "$(_rv_answer 37)"
check "RV agent test22: no address observed for B, the waiting A is told nothing" "^RA22 = <unbound>$" "$(_rv_answer 37)"

# rv_server/3, the entry the rendezvous server boots the agent on
# (networking/glue/bin/rendezvous_server.dart): the module loaded as the
# program, and a network input of one msg(Id, M) envelope, whose M reaches the
# agent; an unsigned reconnect is refused.  The server itself, over the layer,
# is networking/glue/test/rendezvous_server_test.dart's.
rv_server_out=$(printf 'load %s\nrv_server(rv, g, [msg(rv, reconnect(alice, bob, sig, RS))]).\n:quit\n' \
    "$GLP_DIR/programs/examples/rendezvous/rv_agent.glp" | "$REPL_RUN" 2>&1)
check "RV rv_server loads as the server's program" "Loaded: .*rv_agent.glp" "$rv_server_out"
check "RV rv_server: the envelope is taken off, and an unsigned reconnect refused" "^GLP> RS = rejected$" "$rv_server_out"
echo ""

fi  # Section RV

if _section SG; then
# =============================================================================
# Section SG: SGSG's programs --- the SPM programs (programs/social/spm: the CVA
# substrate, the social graph and its secure version, with Restore and
# Unfriend), the super-app (programs/social/graph/core) and its mini-apps, and
# the resource-bounded meta-interpreter of the SGSG paper's Section 6.3
# (programs/social/graph/budget).
# =============================================================================
echo "=== Section SG: SGSG programs (spm, super-app, budget) ==="
echo ""

sg_spm=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/social/spm
:limit 1000000
play_befriend(A1, B1).
play_befriend_simultaneous(A2, B2).
play_three_agents.
play_secure_befriend(A3, B3).
play_secure_unfriend(A5, B5, IR5).
play_secure_restore(A6, Z6, B6, IR6).
play_secure_replace(R7, A7, B7, C7, D7).
:quit
HEREDOC
2>&1)
check "SG spm loads" "Loaded program" "$sg_spm"
check "SG spm befriend: alice at epoch 1" "A1 = 1" "$sg_spm"
check "SG spm befriend: bob at epoch 1" "B1 = 1" "$sg_spm"
check "SG spm simultaneous offer resolves" "A2 = 1" "$sg_spm"
check "SG spm secure befriend: alice at epoch 1" "A3 = 1" "$sg_spm"
check "SG spm secure befriend: bob at epoch 1" "B3 = 1" "$sg_spm"
check "SG spm End friendship: epoch 2 at alice" "A5 = 2" "$sg_spm"
check "SG spm Integrate unfriend: epoch 2 at bob" "B5 = 2" "$sg_spm"
check "SG spm Unfriend preserves the identity record" "IR5 = ir(\[cust_a1, cust_a2\], 67)" "$sg_spm"
check "SG spm crash resets the state" "Z6 = 0" "$sg_spm"
check "SG spm Restore: epoch 1 healed from the checkpoint" "B6 = 1" "$sg_spm"
check "SG spm Restore: identity record from the checkpoint" "IR6 = ir(\[cust_a1, cust_a2\], 67)" "$sg_spm"
check "SG spm Replace: the announcement fires on a supermajority" "R7 = ok" "$sg_spm"
check "SG spm Replace: bob renamed alice to alice2 at epoch 1" "A7 = 1" "$sg_spm"
check "SG spm Replace: alice is gone from bob's map" "B7 = 0" "$sg_spm"
check "SG spm Replace: alice2 rebound to bob" "C7 = 1" "$sg_spm"
check "SG spm Replace: the cascade reached carol through bob's vouch" "D7 = 1" "$sg_spm"
check_not "SG spm no failed play" "→ failed" "$sg_spm"

# The social graph as the super-app (SGSG paper, Sections 3, 5.5 and 6.4):
# programs/social/graph/core is the agent and the plays; the certified
# mini-apps are programs of their own --- graph/pingapp, coins/currency ---
# whose artefacts :artefact writes into core, where the agent's load_file/2
# reads them; cssn's artefact carries no certificate (its boot glue reaches the
# person) and loads as text, which the super-app does not install.  Each
# mini-app is the instance of the root's entry template miniapp/4 at its own
# type, activated at the template's opaque instance, and each person of a
# mini-app is its own scripted person, the second template miniapp_person/4's
# instance, the plays holding every screen at Opaque (GSG 2026-10-13 10:29
# and 18:52 UTC).  The
# artefacts are written in the session that runs the plays: a certificate is
# accepted under a key the person trusts, and the REPL's key is that person's.
# The four forgery plays present, at each of the super-app's four sites that
# take a signed term apart, a term that is not a signed term, and show the
# refusal taken: nothing delivered, and the super-app carries on (Section 6.1).
SG_CORE="$GLP_DIR/programs/social/graph/core"
rm -f "$SG_CORE"/*.glpw
sg_core=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/social/graph/pingapp $SG_CORE
:artefact $GLP_DIR/programs/cssn $SG_CORE
:artefact $GLP_DIR/programs/currencies/coins/currency $SG_CORE
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
$SG_CORE
:limit 5000000
play_load("cssn.glpw", K1, Key1).
play_load("currency.glpw", K2, Key2).
play_install("cssn.glpw", cssn, I1).
play_install("currency.glpw", currency, I2).
play_install("pingapp.glpw", ping, I3).
play_forged_cert(I4).
play_forged_invite(F1).
play_forged_join(F2).
play_forged_root(F3).
play_attest(S, K, H, T).
play_invite(A, B).
play_invite_declined(A2, B2).
play_invite_timed(Ms).
play_coins(C1, C2).
:quit
HEREDOC
2>&1)
check "SG super-app: pingapp's artefact is certified" "pingapp.glpw --- certified under" "$sg_core"
check "SG super-app: cssn's artefact is refused a certificate" "cssn.glpw --- no certificate: refused" "$sg_core"
check "SG super-app: currency's artefact is certified" "currency.glpw --- certified under" "$sg_core"
check "SG super-app core loads" "Loaded program" "$sg_core"
check "SG super-app: load_file gives cssn's artefact as text" "K1 = text" "$sg_core"
check "SG super-app: load_file gives currency's artefact as a Module" "K2 = module" "$sg_core"
check "SG super-app: the super-app installs no text" "I1 = \[\]" "$sg_core"
check "SG super-app: the super-app installs the certified currency" "I2 = \[currency\]" "$sg_core"
check "SG super-app: the super-app installs the certified pingapp" "I3 = \[ping\]" "$sg_core"
check "SG super-app refuses a forgery: a certification that is not a signed term installs nothing" "I4 = \[\]" "$sg_core"
check "SG super-app refuses a forgery: an invitation that is not a signed term delivers nothing, and the next message arrives" "F1 = \[msg(agent, person, received(alice, hello))\]" "$sg_core"
check "SG super-app refuses a forgery: an undertaking that is not a signed term creates no root channel, and the next message arrives" "F2 = \[msg(agent, person, received(bob, hello))\]" "$sg_core"
check "SG super-app refuses a forgery: a root channel under an undertaking that is not a signed term is not accepted, and the next message arrives" "F3 = \[msg(agent, person, received(alice, hello))\]" "$sg_core"
check "SG super-app: the attestation minted on consent" "T = attest(alice, bob)" "$sg_core"
check "SG super-app: signed under the person's key" "K = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$sg_core"
check "SG super-app: invitation, handshake, activation: alice greeted" "A = \[opened(bob), greeted(bob)\]" "$sg_core"
check "SG super-app: invitation, handshake, activation: bob greeted" "B = \[opened(alice), greeted(alice)\]" "$sg_core"
check "SG super-app: a declined invitation opens nothing at alice" "A2 = <unbound>" "$sg_core"
check "SG super-app: a declined invitation opens nothing at bob" "B2 = <unbound>" "$sg_core"
check "SG super-app: the handshake is timed" "Ms = [0-9]*" "$sg_core"
check "SG super-app hosts the currency: alice's swap done" "C1 = \[opened(bob), minted(2), holdings(\[lot(alice, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 2)\]) | " "$sg_core"
check "SG super-app hosts the currency: bob's swap done" "C2 = \[opened(alice), minted(2), holdings(\[lot(bob, 2)\]), swap_done(alice), holdings(\[lot(alice, 2)\]) | " "$sg_core"
check_not "SG super-app no failed play" "→ failed" "$sg_core"

# A message the person addresses to someone who is not their friend is not
# sent, and the agent tells the person so (GSG paper, Section 5.1): alice's
# person sends to bob before either has called the other; bob's person calls
# alice and sends to her between his acceptance and the conveyance, alice's
# person consenting only once he was told.  Each is told not_a_friend; the
# befriending then completes and bob's next message reaches alice.
sg_naf=$("$REPL_RUN" <<HEREDOC
$SG_CORE
:limit 5000000
play_not_a_friend(A, B).
:quit
HEREDOC
2>&1)
check "SG super-app: a send to someone not a friend is not sent, and the person is told not_a_friend" "A = \[not_a_friend(bob), connected(bob), received(bob, hello) | " "$sg_naf"
check "SG super-app: a send between the acceptance and the conveyance is told not_a_friend" "B = \[not_a_friend(alice), connected(alice) | " "$sg_naf"

# The super-app's own person interface on the dispatcher (GSG paper, Section
# 3; vGLP, Definitions "Volitional Procedure, ..." and "Canonical
# Compilation"): home/3 asks the person once for the stream of their
# requests, and the dispatcher draws its construct on the person channel, the
# default widget of a stream of a union of tuples, a menu of the four forms
# kept open; a Connect granted on it reaches the agent, whose notice
# connected(bob) arrives on the same channel, merged with the draws
# (superapp_ui/3, alice with a scripted bob).  A friend's request to connect
# raises respond_friend's card, the friend shown, and the person's yes on it
# is their decision, with the pending value passed back as it came (home.glp
# with its dispatcher and a scripted person who grants on the card once it is
# drawn, play_home/5).  An introduction raises
# respond_intro's card, the introducer and the other party shown, and the
# person's yes or no on it is accept_intro or reject_intro of the other party,
# the pending value passed back as it came (GSG paper, Section 5.2; GSG,
# 2026-10-14 09:24 UTC, item 1).
sg_home=$("$REPL_RUN" <<HEREDOC
$SG_CORE
:limit 5000000
superapp_ui(alice, [input(1, [], connect(bob)) | G?], []).
play_home(alice, yes, [msg(agent, person, befriend(bob, error))], C, L).
play_home(alice, yes, [msg(agent, person, befriend_intro(carol, bob, error))], CI, LI).
play_home(alice, no, [msg(agent, person, befriend_intro(carol, bob, error))], CN, LN).
:quit
HEREDOC
2>&1)
check "SG super-app home: the request form, a menu of the four acts, drawn on the person channel" "draw(1, input_box(menu(\[form(trust, \[field(CompilerKey, peer)\]), form(install, \[field(AppName, text), field(Artefact, text)\]), form(invite, \[field(AppName, text), field(Friend, peer)\]), form(connect, \[field(Friend, peer)\])\])), stream_request_r(input(\[\])))" "$sg_home"
check "SG super-app home: a Connect granted reaches the agent, and connected(bob) is shown on the person channel" "^draw(0, thread(.*), output(\[msg(agent, person, connected(bob))\]))$" "$sg_home"
check "SG super-app home: a friend's request to connect raises respond_friend's card, the friend shown" "L = \[card(bob) | " "$sg_home"
check "SG super-app home: yes on the card is the person's decision, the pending value passed back" "C = \[msg(person, alice, decision(yes, bob, error))\]" "$sg_home"
check "SG super-app home: an introduction raises respond_intro's card, the introducer and the other party shown" "LI = \[card(carol, bob) | " "$sg_home"
check "SG super-app home: yes on the introduction's card is accept_intro, the pending value passed back" "CI = \[msg(person, alice, accept_intro(bob, error))\]" "$sg_home"
check "SG super-app home: no on the introduction's card is reject_intro, the pending value passed back" "CN = \[msg(person, alice, reject_intro(bob, error))\]" "$sg_home"
check_not "SG super-app home: no failed goal" "→ failed" "$sg_home"

# The child-safe platform hosted, in its own session: a play that has run the
# currency's mini-app leaves the session such that the next play's invitation
# opens nothing --- play_invite after play_coins returns [] as well --- so the
# plays are not run one after another in one REPL (SGSG Code, 2026-09-08).
sg_cs=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
$SG_CORE
:limit 5000000
play_cssn(N1, N2).
:quit
HEREDOC
2>&1)
check "SG super-app: the child-safe platform's artefact is certified" "childsafe.glpw --- certified under" "$sg_cs"
check "SG super-app hosts the child-safe platform: alice connected to bob" "N1 = \[connected(bob) | " "$sg_cs"
check "SG super-app hosts the child-safe platform: bob connected and greeted" "N2 = \[connected(alice), received(alice, hello) | " "$sg_cs"
check_not "SG super-app hosts the child-safe platform: no failed play" "→ failed" "$sg_cs"

# BOTH mini-apps under one super-app, in one heap (paper Section 7, Theorem
# 6.5): each person installs the currency and the child-safe platform, each
# with its own user-interface grant; the friend is invited to both, both
# handshakes open, both entries are activated by run/3 with their own
# conversations, and the two protocols proceed over the one friendship.
sg_two=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/coins/currency $SG_CORE
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
$SG_CORE
:limit 5000000
play_both(A, B, C, D).
:quit
HEREDOC
2>&1)
check "SG two mini-apps at once: alice's swap settles" "A = \[opened(bob), minted(2), holdings(\[lot(alice, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 2)\]) | " "$sg_two"
check "SG two mini-apps at once: bob's swap settles" "B = \[opened(alice), minted(2), holdings(\[lot(bob, 2)\]), swap_done(alice), holdings(\[lot(alice, 2)\]) | " "$sg_two"
check "SG two mini-apps at once: alice connected under the child-safe platform" "C = \[connected(bob) | " "$sg_two"
check "SG two mini-apps at once: bob connected and greeted under it" "D = \[connected(alice), received(alice, hello) | " "$sg_two"
check_not "SG two mini-apps at once: no failed play" "→ failed" "$sg_two"

# The same on two smartphones: one isolate per person, both mini-apps hosted by
# each super-app, both protocols over the one friendship.
sg_two_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/coins/currency $SG_CORE
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
:boot $GLP_DIR/programs/social/both_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG two mini-apps on two smartphones: the boot settles" "Boot settled: 2 agents" "$sg_two_boot"
check "SG two mini-apps on two smartphones: the currency's conversation opens" "\[alice\] opened(bob)" "$sg_two_boot"
check "SG two mini-apps on two smartphones: alice's swap settles" "\[alice\] swap_done(bob)" "$sg_two_boot"
check "SG two mini-apps on two smartphones: bob's swap settles" "\[bob\] swap_done(alice)" "$sg_two_boot"
check "SG two mini-apps on two smartphones: the child-safe friendship holds" "\[alice\] connected(bob)" "$sg_two_boot"
check "SG two mini-apps on two smartphones: and its message arrives" "\[bob\] received(alice, hello)" "$sg_two_boot"

# Under the boot harness: two isolates, each its own person with its own key,
# running the super-app of core/; each reads pingapp's artefact beside it and
# trusts the bundled compiler's key; the invitation crosses the friend channel
# between the isolates and the root channel opens across them.
sg_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/social/graph/pingapp $SG_CORE
:boot $GLP_DIR/programs/social/superapp_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG super-app on two smartphones: the boot settles" "Boot settled: 2 agents" "$sg_boot"
check "SG super-app on two smartphones: bob's execution opened" "\[bob\] opened(alice)" "$sg_boot"
check "SG super-app on two smartphones: alice's execution opened" "\[alice\] opened(bob)" "$sg_boot"
check "SG super-app on two smartphones: bob greeted" "\[bob\] greeted(alice)" "$sg_boot"
check "SG super-app on two smartphones: alice greeted" "\[alice\] greeted(bob)" "$sg_boot"

# The warm call on three smartphones (paper Section 5.2), END TO END: the three
# conversations open, alice's execution sends carol the end of a fresh
# variable, carol's forwards it to bob's, bob's assigns it, bob's runtime holds
# the assignment and reports the held link, bob's mini-app asks for a warm
# call, bob's social graph asks carol to introduce him to the anchor, carol's
# person consents, alice's and bob's persons each consent on home's card of
# the introduction (home.vglp, respond_intro), alice and bob befriend, each end authorises the links its
# own person's volition covers, and the value reaches alice's execution.
# A held link is authorised at BOTH of its ends, and the traffic one
# authorisation lets through is itself held at the other, so the reports come
# in rounds and every one must be answered (IGLP, 2026-09-09).  A global name
# does not cross a link as a term, which is why the request for an
# introduction names the anchor by the constant the person consented to;
# programs/tests/linkprobes/linkprobes.md holds the two-agent programs that fence
# the runtime behaviour in.
sg_warm=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/social/graph/pingapp $SG_CORE
:boot $GLP_DIR/programs/social/superapp3_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG warm call on three smartphones: the boot settles" "Boot settled: 3 agents" "$sg_warm"
check "SG warm call: alice and carol converse" "\[carol\] greeted(alice)" "$sg_warm"
check "SG warm call: carol and bob converse" "\[bob\] greeted(carol)" "$sg_warm"
check "SG warm call: alice's execution sends carol the probe's end" "\[alice\] probed(carol)" "$sg_warm"
check "SG warm call: carol's execution forwards it to bob" "\[carol\] forwarded(bob)" "$sg_warm"
check "SG warm call: bob's execution assigns it" "\[bob\] answered" "$sg_warm"
check "SG warm call: bob's runtime holds the link and reports it" "\[bob\] held_link(carol)" "$sg_warm"
check "SG warm call: carol introduces bob to alice" "\[bob\] sg(intro_from(carol, alice))" "$sg_warm"
check "SG warm call: and alice to bob" "\[alice\] sg(intro_from(carol, bob))" "$sg_warm"
check "SG warm call: alice and bob befriend" "\[alice\] sg(connected(bob))" "$sg_warm"
check "SG warm call: the held value reaches alice's execution" "\[alice\] got(hello_from_bob)" "$sg_warm"

# The registry's return from running to idle (paper Section 3, "Activation and
# joining"): the super-app records whether a mini-app is running, and an
# invitation is looked up in that record.  Until 2026-09-15 nothing ever put an
# entry back --- install wrote idle, the first invitation wrote running, and no
# clause wrote idle again --- so a mini-app that had ended was still recorded as
# running and the next invitation's root channel went to the execution that was
# gone.  social/graph/endapp is the mini-app that makes the case reachable:
# pingapp cannot, its loop ending only when the super-app closes the stream it
# reads, which the super-app does only as it closes itself.  endapp's execution
# AT ALICE ends once it has been greeted and at bob it does not, so alice's
# second invitation must reach bob's live execution, and the discriminating
# check is bob's SECOND greeted(alice): with the entry left at running it does
# not come, and his stream stops at the second opened(alice).  Its own session,
# a play that has run another mini-app leaving the session such that the next
# play's invitation opens nothing (see the child-safe platform below).
sg_reinvite=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/social/graph/endapp $SG_CORE
$SG_CORE
:limit 5000000
play_reinvite(A, B).
:quit
HEREDOC
2>&1)
check "SG registry: the self-ending mini-app's artefact is certified" "endapp.glpw --- certified under" "$sg_reinvite"
check "SG registry: alice's execution ends of its own accord" "A = \[opened(bob), greeted(bob)\]" "$sg_reinvite"
check "SG registry: the entry returns to idle and the second invitation is activated afresh" "B = \[opened(alice), greeted(alice), opened(alice), greeted(alice) | " "$sg_reinvite"
check_not "SG registry no failed play" "→ failed" "$sg_reinvite"

# The denominated currencies mini-app (programs/currencies/sovereign/denominated, Currencies'
# own) hosted by the super-app.  The super-app holds no declaration of a
# mini-app's types: it activates every mini-app alike, by run/3 at the type
# identity of the root's entry template miniapp/4 at the opaque instance of
# the type the mini-app's exports record (GSG paper, Sections 3 and 4; TGLP
# modules.tex, Dynamic Activation, "Opaque types"; GSG 2026-10-13 10:29 UTC).
# This play fences that activation rather than the acts, which are the
# mini-app's own and are exercised in Section N4: run/3 admits the mini-app
# only if the identities match, and the persons are the mini-app's own
# scripted persons, under the role watch.  Its own session, as the mini-app
# plays above are.
sg_sovereign=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/sovereign/denominated $SG_CORE
$SG_CORE
:limit 5000000
play_sovereign(A, B).
:quit
HEREDOC
2>&1)
check "SG denominated: the mini-app's artefact is certified" "denominated.glpw --- certified under" "$sg_sovereign"
check "SG denominated: run/3 activates it at alice and the conversation opens" "A = \[opened(bob) | " "$sg_sovereign"
check "SG denominated: and at bob" "B = \[opened(alice) | " "$sg_sovereign"
check_not "SG denominated no failed play" "→ failed" "$sg_sovereign"

# The currency on two smartphones (paper Section 7): the same super-app, two
# isolates with their own keys, hosting Currencies' mini-app from its certified
# artefact; the invitation and handshake cross the isolates and the swap of the
# currency's own contract runs over the conversation the social graph delivered.
sg_coins=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/coins/currency $SG_CORE
:boot $GLP_DIR/programs/social/coins_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG currency on two smartphones: the boot settles" "Boot settled: 2 agents" "$sg_coins"
check "SG currency on two smartphones: alice's execution opened" "\[alice\] opened(bob)" "$sg_coins"
check "SG currency on two smartphones: bob's execution opened" "\[bob\] opened(alice)" "$sg_coins"
check "SG currency on two smartphones: alice minted her own 2" "\[alice\] minted(2)" "$sg_coins"
check "SG currency on two smartphones: bob minted his own 2" "\[bob\] minted(2)" "$sg_coins"
check "SG currency on two smartphones: alice's swap with bob done" "\[alice\] swap_done(bob)" "$sg_coins"
check "SG currency on two smartphones: bob's swap with alice done" "\[bob\] swap_done(alice)" "$sg_coins"
check "SG currency on two smartphones: alice holds bob's coin" "\[alice\] holdings(\[lot(bob, 2)\])" "$sg_coins"
check "SG currency on two smartphones: bob holds alice's coin" "\[bob\] holdings(\[lot(alice, 2)\])" "$sg_coins"

# The child-safe platform on two smartphones (paper Section 7): the same
# super-app hosting CSSN's mini-app from its certified artefact; adopting the
# conversation the handshake opens is the friendship under that platform's
# contract, and one message of it crosses between the isolates.
sg_cssn=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
:boot $GLP_DIR/programs/social/cssn_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG child-safe platform on two smartphones: the boot settles" "Boot settled: 2 agents" "$sg_cssn"
check "SG child-safe platform on two smartphones: alice is connected to bob" "\[alice\] connected(bob)" "$sg_cssn"
check "SG child-safe platform on two smartphones: bob is connected to alice" "\[bob\] connected(alice)" "$sg_cssn"
check "SG child-safe platform on two smartphones: alice's message reaches bob" "\[bob\] received(alice, hello)" "$sg_cssn"
rm -f "$SG_CORE"/*.glpw

sg_budget=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/social/graph/budget
:limit 1000000
play_meta(Out).
play_residual(Out2).
bench(residual, 100, 7, L).
bench(meta, 100, 7, L2).
play_clean(O5, R5).
play_abort_waiting(O6, R6).
play_abort_interpreted(O7, R7, Witness).
:quit
HEREDOC
2>&1)
check "SG budget loads" "Loaded program" "$sg_budget"
check "SG budget control: clean termination closes the request stream" "R5 = \[\]" "$sg_budget"
check "SG budget control: a counter waiting on its stream is aborted" "O6 = \[1, 2, 3\]" "$sg_budget"
check "SG budget control: the aborted goal's request stream closes" "R6 = \[\]" "$sg_budget"
check "SG budget control interpreted: the abort closes execute/4's request stream" "R7 = \[\]" "$sg_budget"
check "SG budget control interpreted: the waiting goal's output is unwritten after the abort" "Witness = waiting" "$sg_budget"
check "SG budget control interpreted: the waiting goal ends when its input closes" "O7 = \[1, 2, 3\]" "$sg_budget"
check "SG budget: the counter under bounded/3" "Out = \[1, 2, 3, 4, 5\]" "$sg_budget"
check "SG budget: the residual program" "Out2 = \[1, 2, 3, 4, 5\]" "$sg_budget"
check "SG budget: residual counter of 100 under grants of 7" "L = 100" "$sg_budget"
check "SG budget: meta-interpreted counter of 100 under grants of 7" "L2 = 100" "$sg_budget"
check_not "SG budget no failed play" "→ failed" "$sg_budget"

echo ""

fi  # Section SG

if _section APPS; then
# =============================================================================
# Section APPS: the apps of the contracts (programs/apps), each a mini-app of the Grassroots Super-App compiled down the cascade from its contract's act schemas: the canonical CVA platform vGLP's compiler prints from them, compiled to GLP when the directory loads, under the root's entry template miniapp/4. GSG's app, programs/apps/gsg: its platform held to what the compiler prints from programs/gsc/GSG.glp (programs/apps/compile.sh --check); the program certified by :artefact; its router, which carries the platform's network over the conversations the social graph hands the mini-app, holding a message until the conversation with its recipient opens (play_held); and two executions over one conversation, each person first establishing their identity (GSG:1, Identity, establish(p?), GSC e03c56e0), alice offering bob friendship, bob asked and answering yes, and then bob ending it (play_friends), GSN's app, programs/apps/gsn, the same host on GSN's platform (play_network), and GSNP's app, programs/apps/gsnp, the same host on GSNP's platform, three executions over two conversations (play_provenance), each person's screen, the views the agent writes with the person's looks and answers, held exactly, an unbound tail written _: the run suspends, each agent having taken its own transactions until none holds and no trigger being left (vGLP, Section 5.4, "Triggers").  And the super-app hosting each app on smartphones, programs/apps/gsg_boot.glp and gsn_boot.glp on two and gsnp_boot.glp on three, under :boot: the boot harness reduces each event until the program is quiescent, as the device runtime does (IGLP, Implementation Notes, "On each event the agent binds what the event delivers, reduces until quiescent"), the platform's agent quiesces, and the boot settles, each person's lines held exactly, in order; on three, each person's super-app lines and mini-app screen each held exactly, in order, the two not being ordered against each other across three isolates.
# =============================================================================
echo "=== Section APPS: the apps (programs/apps) ==="
echo ""

# The line, exactly, among the REPL's lines, its prompt stripped and an unbound tail written _.
apps_line() {
    local name="$1" line="$2" out="$3"
    if echo "$out" | sed -e 's/^\(GLP> \)*//' -e 's/| X[0-9]*\]$/| _]/' | grep -qFx -- "$line"; then
        _check_pass "$name"
    else
        _check_fail "$name" "expected the line: $line"
    fi
}

apps_print=$(bash "$GLP_DIR/programs/apps/compile.sh" --check 2>&1; echo "exit $?")
check "APPS gsg: platform.vglp is what vGLP's compiler prints from GSG.glp" "^gsg/platform.vglp: as printed$" "$apps_print"
check "APPS compile.sh --check exits 0" "^exit 0$" "$apps_print"

APPS_ART=$(mktemp -d)
apps_gsg=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsg $APPS_ART
$GLP_DIR/programs/apps/gsg
play_held(In, ToBob, ToCarol).
:limit 600000
play_friends(A, B).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gsg: the mini-app's artefact is certified" "gsg.glpw --- certified under" "$apps_gsg"
check "APPS gsg loads" "Loaded program: .*programs/apps/gsg$" "$apps_gsg"
apps_line "APPS gsg router: bob's hello reaches the agent" "In = [msg(alice, hello(bob))]" "$apps_gsg"
apps_line "APPS gsg router: the hello held for bob goes first on his conversation, then the start" "ToBob = [msg(bob, hello(alice)), msg(bob, cargo(alice, start(attempt(alice, 1, 1, befriend(alice, bob)))))]" "$apps_gsg"
apps_line "APPS gsg router: the hello held for carol goes on hers" "ToCarol = [msg(carol, hello(alice))]" "$apps_gsg"
apps_line "APPS gsg friends: alice's screen" "A = [view([], []), menu(alice, []), view([identity], []), view([identity], [bob]), menu(alice, [bob]), view([friend(bob), identity], [bob]), view([identity], [bob]) | _]" "$apps_gsg"
apps_line "APPS gsg friends: bob's screen" "B = [view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), view([identity], [alice]) | _]" "$apps_gsg"
# The request question shows the agent's own key (vGLP 321127a, Section 5.4), and the scripted person, proposing a class that names their agent, writes the key it reads off the question into the holes of its step own(Ks, R) (person.glp).
check "APPS gsg: alice establishes her identity by the key her request question shows" "^script(alice, \[look, own(\[A1\], propose(establish(A1?)))" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check "APPS gsg: ... the key written into each hole" "^fill(Me, \[Me? | Ks\]) :- ground(Me?) | fill(Me?, Ks?)\.$" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check "APPS gsg: the types written again hold the notices of the platform's output (vGLP c44769f)" "^Output   ::= view(List(Atom), List(Key)) ; refused(Class, Key) ; yielded(Class)\.$" "$(cat "$GLP_DIR/programs/apps/gsg/self.glp")"
check "APPS gsg: the person's screen shows a notice written after the last view" "^shows(refused(P, Q), _, T, _, B?) :- ground(P?), ground(Q?), ground(T?) |$" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check_not "APPS gsg: no failed play" "→ failed" "$apps_gsg"
check_not "APPS gsg: no error" "Error" "$apps_gsg"
check "APPS gsg friends: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsg"
check_not "APPS gsg friends: ... and is not stopped at the limit" "capped" "$apps_gsg"

# GSN's app, programs/apps/gsn: the same host on GSN's platform, its wrapper the same file as GSG's; two executions over one conversation (play_network): alice and bob establish their identities and become friends, alice creates the group friends, adds bob to it and sends him the message 'Hi Bob', bob sends the group the message 'Hi all', which alice delivers to him, and bob leaves the group, each person acting on what their screen shows, the views the agent writes (vGLP b7e3517, Section 5.4, "The compilation"; person.glp).
check "APPS gsn: platform.vglp is what vGLP's compiler prints from GSN.glp" "^gsn/platform.vglp: as printed$" "$apps_print"
# The request type proposes the guarded classes only (vGLP, Section 5.4, the compilation): GSN's ten schemas that mark a role, and not drop or deliver, unguarded, each of which the leader's agent proposes as its opener (programs/gsc/GSN.glp at 120bf8bd).
apps_gsn_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gsn/platform.vglp")
check "APPS gsn: the request type proposes the guarded classes, not drop or deliver" "^establish(Peer) befriend(Peer, Peer) unfriend(Peer, Peer) message(Peer, Peer, Speech) create(Peer, Speech) join(Peer, Peer, Speech) leave(Peer, Peer, Speech) close(Peer, Speech) post_own(Peer, Speech, Speech) post(Peer, Peer, Speech, Speech) $" "$apps_gsn_proposal"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gsn/miniapp.glp"; then
    _check_pass "APPS gsn: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gsn: miniapp.glp is the same file as GSG's app's" "programs/apps/gsn/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gsn=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsn $APPS_ART
$GLP_DIR/programs/apps/gsn
:limit 2000000
play_network(A, B).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gsn: the mini-app's artefact is certified" "gsn.glpw --- certified under" "$apps_gsn"
check "APPS gsn loads" "Loaded program: .*programs/apps/gsn$" "$apps_gsn"
apps_line "APPS gsn network: alice's screen" "A = [view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]) | _]" "$apps_gsn"
apps_line "APPS gsn network: bob's screen" "B = [view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), gave(join(alice, bob, speech(utterance(friends), alice)), yes), view([member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), friend(alice), identity], [alice]) | _]" "$apps_gsn"
# GSN's contract (programs/gsc/GSN.glp at GLP 542ec8ca), compiled with its listed role and its name terms (vGLP f5f1644 and c649c0d, Section 5.4, "Act schemas as local specifications"): close's part carries the list of the people of its listed role, its request name not; a class's arguments in the order of the head, post(p?, a ; x, n) as post(p, a, x, n) (GSC 0228e79); member(<a>.n) holding the community name of the group, one argument.
apps_gsn_text=$(cat "$GLP_DIR/programs/apps/gsn/platform.vglp")
check "APPS gsn: post(p?, a ; x, n) compiles as post(p, a, x, n)" "^binding_of(post(V1, V2, V3, V4), post, \[b(role(1), p(V1?)), b(role(2), p(V2?)), b(svar(x), s(V3?)), b(svar(n), s(V4?))\])$" "$apps_gsn_text"
check "APPS gsn: close's class term carries the list of the people of its listed role, a[listed(*, n)]" "^close(Peer, List(Peer), Speech)$" "$apps_gsn_text"
check "APPS gsn: member(<a>.n) holds a community name, one argument" "^member(CName)$" "$apps_gsn_text"
check "APPS gsn: ... written in the view as the contract writes it" "^atom_of(g(member, \[c(V1)\]), shown(member(V1?)))$" "$apps_gsn_text"
check "APPS gsn: ... the community name of the person bound to u followed by the speech acts bound to the y's" "^CName     ::= community(Peer) ; dot(CName, Speech)\.$" "$apps_gsn_text"
check "APPS gsn: bob's group message in the order of post's head, its content before the group's name" "own(\[B2, B3\], propose(post(B2?, alice, speech(utterance('Hi all'), B3?), speech(utterance(friends), alice))))" "$(cat "$GLP_DIR/programs/apps/gsn/person.glp")"
check "APPS gsn: the types written again: close's list and the community name" "^CName     ::= community(Peer) ; dot(CName, Speech)\.$" "$(cat "$GLP_DIR/programs/apps/gsn/self.glp")"
check "APPS gsn: alice signs her message by the key her request question shows" "own(\[A7, A8\], propose(message(A7?, bob, speech(utterance('Hi Bob'), A8?))))" "$(cat "$GLP_DIR/programs/apps/gsn/person.glp")"
check_not "APPS gsn: no failed play" "→ failed" "$apps_gsn"
check_not "APPS gsn: no error" "Error" "$apps_gsn"
check "APPS gsn network: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsn"
check_not "APPS gsn network: ... and is not stopped at the limit" "capped" "$apps_gsn"

# GSNP's app, programs/apps/gsnp: the same host on GSNP's platform, its wrapper the same file as GSG's; three executions over two conversations, alice's with bob and bob's with carol (play_provenance): each establishes their identity; alice and bob become friends, bob asked and answering yes; alice signs the item 'Flood on Main Street' (GSNP:1, Authorship), which records her its author, and sends it to bob in a message, which carries it as she signed it (GSN:1, Message), nobody asked; bob and carol become friends, carol asked and answering yes; and bob forwards the item he holds, signed by alice, to carol (GSNP:2, Forwarding), nobody asked, who then holds it signed by alice and then by bob, the item of forward(bob, carol, y) being y signed by bob (programs/gsc/GSNP.glp at 120bf8bd, the schemas message and forward).
check "APPS gsnp: platform.vglp is what vGLP's compiler prints from GSNP.glp" "^gsnp/platform.vglp: as printed$" "$apps_print"
# The request type proposes the guarded classes only: GSNP's thirteen schemas that mark a role, and not drop, deliver or deliver_own, unguarded.
apps_gsnp_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gsnp/platform.vglp")
check "APPS gsnp: the request type proposes the guarded classes, not drop, deliver or deliver_own" "^establish(Peer) befriend(Peer, Peer) unfriend(Peer, Peer) sign(Peer, Speech) message(Peer, Peer, Speech) forward(Peer, Peer, Speech) create(Peer, Speech) join(Peer, Peer, Speech) leave(Peer, Peer, Speech) close(Peer, Speech) post_own(Peer, Speech, Speech) post(Peer, Peer, Speech, Speech) post_forward(Peer, Peer, Speech, Speech) $" "$apps_gsnp_proposal"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gsnp/miniapp.glp"; then
    _check_pass "APPS gsnp: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gsnp: miniapp.glp is the same file as GSG's app's" "programs/apps/gsnp/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gsnp=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsnp $APPS_ART
$GLP_DIR/programs/apps/gsnp
:limit 4000000
play_provenance(A, B, C).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gsnp: the mini-app's artefact is certified" "gsnp.glpw --- certified under" "$apps_gsnp"
check "APPS gsnp loads" "Loaded program: .*programs/apps/gsnp$" "$apps_gsnp"
apps_line "APPS gsnp provenance: alice's screen" "A = [view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([item(speech(utterance('Flood on Main Street'), alice)), authored(speech(utterance('Flood on Main Street'), alice)), friend(bob), identity], [bob]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: bob's screen" "B = [view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), view([item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice]), view([item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice, carol]), view([friend(carol), item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice, carol]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: carol's screen" "C = [view([], []), view([identity], []), view([identity], [bob]), gave(befriend(bob, carol), yes), view([friend(bob), identity], [bob]), view([item(speech(speech(utterance('Flood on Main Street'), alice), bob)), friend(bob), identity], [bob]) | _]" "$apps_gsnp"
check "APPS gsnp: alice signs the item by the key her request question shows" "own(\[A3, A4\], propose(sign(A3?, speech(utterance('Flood on Main Street'), A4?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check "APPS gsnp: alice sends the item she authored to bob in a message, as she signed it" "own(\[A5, A6\], propose(message(A5?, bob, speech(utterance('Flood on Main Street'), A6?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check_not "APPS gsnp: no failed play" "→ failed" "$apps_gsnp"
check_not "APPS gsnp: no error" "Error" "$apps_gsnp"
check "APPS gsnp provenance: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsnp"
check_not "APPS gsnp provenance: ... and is not stopped at the limit" "capped" "$apps_gsnp"

# Each app on two smartphones: the super-app of programs/social/graph/core on two isolates, each person with its own key pair, hosting the app from its certified artefact, gsg.glpw or gsn.glpw, which :artefact writes beside the super-app and load_file/2 reads there (programs/apps/gsg_boot.glp, gsn_boot.glp).  The two persons' lines interleave as the isolates run; each person's lines, their prefix stripped, are held exactly, in order: what the super-app told the person, sg(...), and the mini-app's screen, the steps of play_friends and play_network.
apps_person() { # apps_person <person> <output>: the person's lines in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_same() { # apps_same <name> <expected> <actual>
    if [ "$3" = "$2" ]; then _check_pass "$1"; else _check_fail "$1" "expected: $2; got: $3"; fi
}
APPS_CORE="$GLP_DIR/programs/social/graph/core"
apps_gsg_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsg $APPS_CORE
:boot $GLP_DIR/programs/apps/gsg_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gsg.glpw"
check "APPS gsg on two smartphones: the mini-app's artefact is certified" "gsg.glpw --- certified under" "$apps_gsg_boot"
check "APPS gsg on two smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$apps_gsg_boot"
check_not "APPS gsg on two smartphones: no event stopped at the cap" "did not quiesce" "$apps_gsg_boot"
apps_same "APPS gsg on two smartphones: alice's lines" "[sg(connected(bob)), view([], []), menu(alice, []), view([identity], []), view([identity], [bob]), menu(alice, [bob]), view([friend(bob), identity], [bob]), view([identity], [bob])]" "$(apps_person alice "$apps_gsg_boot")"
apps_same "APPS gsg on two smartphones: bob's lines" "[sg(befriend(alice)), sg(connected(alice)), view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), view([identity], [alice])]" "$(apps_person bob "$apps_gsg_boot")"

apps_gsn_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsn $APPS_CORE
:boot $GLP_DIR/programs/apps/gsn_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gsn.glpw"
check "APPS gsn on two smartphones: the mini-app's artefact is certified" "gsn.glpw --- certified under" "$apps_gsn_boot"
check "APPS gsn on two smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$apps_gsn_boot"
check_not "APPS gsn on two smartphones: no event stopped at the cap" "did not quiesce" "$apps_gsn_boot"
apps_same "APPS gsn on two smartphones: alice's lines" "[sg(connected(bob)), view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([posted(speech(utterance(Hi all), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([delivered(speech(utterance(Hi all), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance(Hi all), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([delivered(speech(utterance(Hi all), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance(Hi all), bob), speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob])]" "$(apps_person alice "$apps_gsn_boot")"
apps_same "APPS gsn on two smartphones: bob's lines" "[sg(befriend(alice)), sg(connected(alice)), view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), gave(join(alice, bob, speech(utterance(friends), alice)), yes), view([member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received(speech(utterance(Hi Bob), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received_in(speech(utterance(Hi all), bob), alice, speech(utterance(friends), alice)), received(speech(utterance(Hi Bob), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received_in(speech(utterance(Hi all), bob), alice, speech(utterance(friends), alice)), received(speech(utterance(Hi Bob), alice), alice), friend(alice), identity], [alice])]" "$(apps_person bob "$apps_gsn_boot")"

# GSNP's app on three smartphones (programs/apps/gsnp_boot.glp): alice connects to bob, and once they are connected bob connects to carol; each pair befriends in the super-app's social graph; alice invites bob to gsnp and bob invites carol; and the steps of play_provenance run over the two conversations.  Each person's super-app lines, sg(...), and their mini-app screen are each held exactly, in order: across three isolates a person's super-app events and their mini-app's are not ordered against each other, bob's connection to carol racing alice's invitation.
apps_person_sg() { # apps_person_sg <person> <output>: the person's super-app lines, sg(...), in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") == 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_person_screen() { # apps_person_screen <person> <output>: the person's mini-app screen, every other line of theirs, in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") != 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_gsnp_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsnp $APPS_CORE
:boot $GLP_DIR/programs/apps/gsnp_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gsnp.glpw"
check "APPS gsnp on three smartphones: the mini-app's artefact is certified" "gsnp.glpw --- certified under" "$apps_gsnp_boot"
check "APPS gsnp on three smartphones: the boot settles" "Boot settled: 3 agents (alice, bob, carol)" "$apps_gsnp_boot"
check_not "APPS gsnp on three smartphones: no event stopped at the cap" "did not quiesce" "$apps_gsnp_boot"
apps_same "APPS gsnp on three smartphones: alice's super-app lines" "[sg(connected(bob))]" "$(apps_person_sg alice "$apps_gsnp_boot")"
apps_same "APPS gsnp on three smartphones: alice's screen" "[view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([item(speech(utterance(Flood on Main Street), alice)), authored(speech(utterance(Flood on Main Street), alice)), friend(bob), identity], [bob])]" "$(apps_person_screen alice "$apps_gsnp_boot")"
apps_same "APPS gsnp on three smartphones: bob's super-app lines" "[sg(befriend(alice)), sg(connected(alice)), sg(connected(carol))]" "$(apps_person_sg bob "$apps_gsnp_boot")"
apps_same "APPS gsnp on three smartphones: bob's screen" "[view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), view([item(speech(utterance(Flood on Main Street), alice)), friend(alice), identity], [alice]), view([item(speech(utterance(Flood on Main Street), alice)), friend(alice), identity], [alice, carol]), view([friend(carol), item(speech(utterance(Flood on Main Street), alice)), friend(alice), identity], [alice, carol])]" "$(apps_person_screen bob "$apps_gsnp_boot")"
apps_same "APPS gsnp on three smartphones: carol's super-app lines" "[sg(befriend(bob)), sg(connected(bob))]" "$(apps_person_sg carol "$apps_gsnp_boot")"
apps_same "APPS gsnp on three smartphones: carol's screen" "[view([], []), view([identity], []), view([identity], [bob]), gave(befriend(bob, carol), yes), view([friend(bob), identity], [bob]), view([item(speech(speech(utterance(Flood on Main Street), alice), bob)), friend(bob), identity], [bob])]" "$(apps_person_screen carol "$apps_gsnp_boot")"

echo ""

fi  # Section APPS

if _section GF; then
# =============================================================================
# Section GF: Grassroots federation records, transactions and platform
# =============================================================================
# /Grassroots/GFWC/sections/federation.tex, Sections 2.5 to 2.7: the records
# of Definition "Record, Row" with the additions of Definition "Will Record,
# Origin Record", the state of Definition "State of a Row" read with decided
# acts in place of act records (Definition "State of a Row, Implied"), and the
# decision of a motion and of an act; the transactions of Definition
# "Grassroots Federation Implied Transactions"; and the federation platform of
# Definition "Federation Platform", one instance of the reconfigurable lattice
# agreement of Figure 2 of arXiv:1910.09264v2 per community over the rows of
# that community.  A community name is an agent followed by the decision
# identities of the Federates that formed it (Definition "Community Name"); a
# motion carries the descendants of its community and a report the descendants
# of the child it reports.  programs/federation is GFWC's (Coordination
# Appendix B, 2026-09-17).
echo "=== Section GF: Grassroots federation records and platform ==="

FED="$GLP_DIR/programs/federation"

# --- The Join of Figure 2 of sections/introduction.tex, at layer (5) --------
# Two communities f = name(alice, [y1, y2]) and g = name(bob, [y1]), a motion
# at each, and the wills.  Two of the three seats of f and both seats of g have
# willed, a supermajority of each electorate at theta = 1/2, so the act is
# decided and g is a child of f in the computed state; the report of g gives f
# its members, and its descendants are g with what g reports.
gf_join=$("$REPL_RUN" <<HEREDOC
$FED
decided_named(join_done, A).
state_named(join_done, S).
:quit
HEREDOC
2>&1)
check "GF federation loads" "Loaded program" "$gf_join"
check_not "GF no type error" "Error loading" "$gf_join"
check "GF join: the act is decided on the motion at each community" \
      "A = \[arec(join(name(alice, \[y1, y2\]), name(bob, \[y1\])), \[motion(join" "$gf_join"
check "GF join: the decided act carries the motion at g, with g's descendants" \
      "name(bob, \[y1\]), \[bob, erin\], \[name(bob, \[\])\], 0)\])\]" "$gf_join"
check "GF join: g is a child of f, and the report gives f its members" \
      "S = state(\[name(bob, \[y1\])\], \[\], \[bob, erin\]" "$gf_join"
check "GF join: the descendants of f are g and what g reports" \
      "\[bob, erin\], \[name(bob, \[y1\]), name(bob, \[\])\], \[st(alice" "$gf_join"
check "GF join: the assembly of f is its three seats" \
      "\[st(alice, uncoloured, 0), st(carol, uncoloured, 0), st(dave, uncoloured, 0)\], 0, \[\], \[\])" "$gf_join"

# --- The same, one will short at each community ----------------------------
# Nothing is decided, so there is no edge; the motion moved at f is open, and
# the motion relayed from g is not among f's open motions, its community being
# g (Definition "State of a Row"(6)).
gf_open=$("$REPL_RUN" <<HEREDOC
$FED
decided_named(join_open, A).
state_named(join_open, S).
:quit
HEREDOC
2>&1)
check "GF open: one will at each decides nothing" "A = \[\]" "$gf_open"
check "GF open: no edge" "S = state(\[\], \[\], \[\], \[\]" "$gf_open"
check "GF open: the motion moved at f is open" \
      "\[motion(join(name(alice, \[y1, y2\]), name(bob, \[y1\])), name(alice, \[y1, y2\]), \[alice, carol, dave\], \[\], 0)\])" "$gf_open"
check_not "GF open: the motion relayed from g is not f's open motion" \
      "name(bob, \[y1\]), \[bob, erin\], \[name(bob, \[\])\], 0)\])$" "$gf_open"

# --- A Leave of the next epoch ---------------------------------------------
# The Leave is moved at epoch 1, one act record standing on the pair, and is
# decided; it is then the sole head of the pair, so the edge is gone, while
# every record of the Join remains in the row.
gf_leave=$("$REPL_RUN" <<HEREDOC
$FED
decided_named(leave_done, A).
state_named(leave_done, S).
row_named(leave_done, R).
:quit
HEREDOC
2>&1)
check "GF leave: the Leave of epoch 1 is decided" \
      "A = \[arec(leave(name(alice, \[y1, y2\]), name(bob, \[y1\])), \[motion(leave" "$gf_leave"
check "GF leave: the Join of epoch 0 is decided still" \
      "arec(join(name(alice, \[y1, y2\]), name(bob, \[y1\]))" "$gf_leave"
check "GF leave: the edge is gone" "S = state(\[\], \[\], \[\], \[\]" "$gf_leave"
check "GF leave: the Join motion remains in the row" \
      "R = \[seat(plus, alice, uncoloured, 0, 0)" "$gf_leave"
check "GF leave: the wills on the Join remain in the row" \
      "will(carol, motion(join(" "$gf_leave"
check "GF leave: the report remains in the row" \
      "report(name(bob, \[y1\]), \[bob, erin\], \[name(bob, \[\])\], 12)" "$gf_leave"

# --- A seat record of an agent who already holds a seat is ignored ---------
# The seat records are taken in the order of (k, q); the third names alice,
# who holds the seat added by the first, so it adds nothing (Definition
# "State of a Row"(4)).
gf_seat=$("$REPL_RUN" <<HEREDOC
$FED
state_named(seat_dup, S).
:quit
HEREDOC
2>&1)
check "GF seat: a second seat for a seated agent is ignored" \
      "S = state(\[\], \[\], \[\], \[\], \[st(alice, uncoloured, 0), st(carol, uncoloured, 0)\]" "$gf_seat"

# --- The accounts accrue only from a raise whose first date is the date -----
# raise_gap holds raises (0, 3) and (5, 9): the first is taken and the second
# is not, its first date not being the date reached.  raise_chain holds
# (0, 3) and (3, 7), and both are taken.  The share-days are exact rationals.
gf_raise=$("$REPL_RUN" <<HEREDOC
$FED
state_named(raise_gap, S1).
state_named(raise_chain, S2).
:quit
HEREDOC
2>&1)
check "GF raise: the date is that of the raise taken" \
      "S1 = state(\[\], \[\], \[\], \[\], \[\], 3, " "$gf_raise"
check "GF raise: the accounts of the raise taken, and of no other" \
      "\[acct(ag(alice), 3, rat(3, 1)), acct(comm(name(bob, \[y1\])), 3, rat(3, 2))\]" "$gf_raise"
check "GF raise: a chain of raises accrues each of them" \
      "S2 = state(\[\], \[\], \[\], \[\], \[\], 7, \[acct(ag(alice), 7, rat(7, 2))\], \[\])" "$gf_raise"

# --- A Federate: the origin record and the community it forms --------------
# fed_child is the row of the community the Federate g by y2 forms, holding
# the origin record and the seats of the electorate, coloured g; g is its
# child.  fed_moved is the row of g itself once its Federate is decided, and
# the community formed, g's name extended by y2, is its parent.
gf_fed=$("$REPL_RUN" <<HEREDOC
$FED
state_named(fed_child, S1).
state_named(fed_moved, S2).
:quit
HEREDOC
2>&1)
check "GF federate: the origin record makes g a child of the community formed" \
      "S1 = state(\[name(bob, \[y1\])\], \[\], \[\], \[\], \[st(bob, col(name(bob, \[y1\])), 0), st(erin, col(name(bob, \[y1\])), 0)\]" "$gf_fed"
check "GF federate: the community formed is a parent of the one that moved it" \
      "S2 = state(\[\], \[name(bob, \[y1, y2\])\], \[\], \[\]" "$gf_fed"

# --- One row read by each of the two definitions ---------------------------
# rec_join is a row of the layer of Definition "Grassroots Federation over
# Records": it carries the act record of the Join and no wills.  Read as
# Definition "State of a Row" stands, the edge holds; read by Definition
# "State of a Row, Implied", nothing is decided and the motion is open.
gf_two=$("$REPL_RUN" <<HEREDOC
$FED
state_explicit_named(rec_join, S1).
state_named(rec_join, S2).
:quit
HEREDOC
2>&1)
check "GF readings: the act record gives the edge" \
      "S1 = state(\[name(bob, \[y1\])\], \[\], \[bob, erin\], \[name(bob, \[y1\]), name(bob, \[\])\]" "$gf_two"
check "GF readings: with no will the same row decides nothing" \
      "S2 = state(\[\], \[\], \[\], \[\]" "$gf_two"

# --- Dates, names and their order, and the supermajority -------------------
# Names, read as sequences alternating agents and identities, are compared
# lexicographically under a total order on agents and on identities: the
# agents decide first, then the identities, a proper prefix being the lesser.
# The name a Federate v by y forms is v extended by y.
gf_misc=$("$REPL_RUN" <<HEREDOC
$FED
adates_named(dates, D).
adate_named(dates, carol, T).
name_gt(name(bob, [y1]), name(alice, [y1, y2]), A1).
name_gt(name(alice, [y1, y2]), name(bob, [y1]), A2).
name_gt(name(alice, [y1, y2]), name(alice, [y1]), A3).
name_gt(name(alice, [y1]), name(alice, [y1, y2]), A4).
name_gt(name(alice, [y2]), name(alice, [y1, y2]), A5).
name_gt(name(alice, [y1]), name(alice, [y1]), A6).
name_extend(name(alice, [y1]), y2, N).
supermajority(rat(1, 2), 2, 3, A7).
supermajority(rat(1, 2), 1, 2, A8).
supermajority(rat(2, 3), 2, 3, A9).
:quit
HEREDOC
2>&1)
check "GF date: the date of an agent is the greatest its row records" \
      "D = \[adate(alice, 7), adate(bob, 5)\]" "$gf_misc"
check "GF date: an agent the row has no date of is at zero" "T = 0" "$gf_misc"
check "GF order: the agents decide it first, whatever the identities" "A1 = yes" "$gf_misc"
check "GF order: and the other way round" "A2 = no" "$gf_misc"
check "GF order: at one agent a name extended is the greater" "A3 = yes" "$gf_misc"
check "GF order: and its prefix the lesser" "A4 = no" "$gf_misc"
check "GF order: the identities are compared in turn" "A5 = yes" "$gf_misc"
check "GF order: a name is not greater than itself" "A6 = no" "$gf_misc"
check "GF names: the name a Federate forms is the deciding community's extended by the identity" \
      "N = name(alice, \[y1, y2\])" "$gf_misc"
check "GF supermajority: two of three at one half" "A7 = yes" "$gf_misc"
check "GF supermajority: one of two at one half is not" "A8 = no" "$gf_misc"
check "GF supermajority: two of three at two thirds is not" "A9 = no" "$gf_misc"

# --- Exact rationals -------------------------------------------------------
# A share and a ratio are rationals and their comparisons are exact, so they
# are carried as normalised integer pairs and compared without division.
gf_rat=$("$REPL_RUN" <<HEREDOC
$FED
rat_make(12, 18, R1).
rat_make(5, -10, R2).
rat_add(rat(1, 3), rat(1, 6), R3).
rat_mul(rat(2, 3), rat(3, 8), R4).
rat_cmp(rat(2, 3), rat(3, 4), O1).
rat_cmp(rat(3, 4), rat(3, 4), O2).
rat_cmp(rat(4, 5), rat(3, 4), O3).
rat_floor(rat(7, 2), F1).
rat_floor(rat(-7, 2), F2).
:quit
HEREDOC
2>&1)
check "GF rat: a rational is normalised" "R1 = rat(2, 3)" "$gf_rat"
check "GF rat: the denominator is kept positive" "R2 = rat(-1, 2)" "$gf_rat"
check "GF rat: addition" "R3 = rat(1, 2)" "$gf_rat"
check "GF rat: multiplication" "R4 = rat(1, 4)" "$gf_rat"
check "GF rat: less" "O1 = lt" "$gf_rat"
check "GF rat: equal" "O2 = eq" "$gf_rat"
check "GF rat: greater" "O3 = gt" "$gf_rat"
check "GF rat: the floor of a positive rational" "F1 = 3" "$gf_rat"
check "GF rat: the floor of a negative rational" "F2 = -4" "$gf_rat"

# --- The transactions of Definition "Grassroots Federation Implied
#     Transactions" ---------------------------------------------------------
# Each is a function from a world -- the rows of the names with the date of
# each agent -- to a world, its precondition read from the state the records of
# one community compute.  The runs are programs/federation/plays.glp.

# The Join of Figure 2 driven end to end: two Forms, the Move, Will and
# Establish of a Federate, the Move at g, its Relay to f, the Move at f --- which
# asks that g's motion be there, with descendants that do not hold f --- the
# Wills of a supermajority of each electorate, and the remaining Relays.
gf_run=$("$REPL_RUN" <<HEREDOC
$FED
play(join_fig2, R).
play_state(join_fig2, name(alice, [y1]), S1).
play_state(join_fig2, name(bob, []), S2).
:quit
HEREDOC
2>&1)
check "GF run: the Join of Figure 2 runs to the end" "R = done(" "$gf_run"
check "GF run: b is a child of f, beside the community f federated" \
      "S1 = state(\[name(alice, \[\]), name(bob, \[\])\], \[\]" "$gf_run"
check "GF run: f is the seat of the Federate's electorate" \
      "\[st(alice, col(name(alice, \[\])), 0)\]" "$gf_run"
check "GF run: and f is a parent of b in b's own row" \
      "S2 = state(\[\], \[name(alice, \[y1\])\], \[bob\]" "$gf_run"

# The Join stands decided at f alone; a Leave is moved there and willed, and
# only then is the Join relayed to b.  The Leave carries the greater epoch, so
# the edge is gone at both.
gf_leave2=$("$REPL_RUN" <<HEREDOC
$FED
play(leave_after, R).
play_state(leave_after, name(alice, [y1]), S1).
play_state(leave_after, name(bob, []), S2).
:quit
HEREDOC
2>&1)
check "GF run: the Leave after the Join runs to the end" "R = done(" "$gf_leave2"
check "GF run: b is no longer a child of f" \
      "S1 = state(\[name(alice, \[\])\], \[\]" "$gf_leave2"
check "GF run: and f is no longer a parent of b" "S2 = state(\[\], \[\], \[bob\]" "$gf_leave2"

# A Report carries the child's membership and descendants to its parent, and
# the Raise that follows accrues for the member and for the child, which it can
# do only because the report arrived.
gf_rep=$("$REPL_RUN" <<HEREDOC
$FED
play_row(report_raise, name(bob, [y1]), R).
play_state(report_raise, name(bob, [y1]), S).
:quit
HEREDOC
2>&1)
check "GF report: the child's membership and descendants reach the parent's row" \
      "report(name(bob, \[\]), \[bob\], \[\], 3)" "$gf_rep"
check "GF report: the Raise that follows accrues for the member and the child" \
      "raise(0, 5, \[sig(ag(bob), 5), sig(comm(name(bob, \[\])), 5)\], \[et(ag(bob), rat(5, 1)), et(comm(name(bob, \[\])), rat(5, 1))\])" "$gf_rep"
check "GF report: the parent's members and descendants follow the report" \
      "S = state(\[name(bob, \[\])\], \[\], \[bob\], \[name(bob, \[\])\]" "$gf_rep"
check "GF report: the parent's date and accounts follow the raise" \
      "5, \[acct(ag(bob), 5, rat(5, 1)), acct(comm(name(bob, \[\])), 5, rat(5, 1))\], \[\])" "$gf_rep"

# A Join whose parent is a descendant of the child is refused at the Move.
# alice's community a is a child of f = a.y1 and has reported to it, so f's
# descendants hold a.  f moves Join a -> f, the motion carries f's descendants,
# and it is relayed to a; the Move at a asks that they not hold a, and is
# refused (Definition "Grassroots Federation Implied Transactions", Move).
gf_cycle=$("$REPL_RUN" <<HEREDOC
$FED
play(join_cycle, R).
play_row(join_cycle_moved, name(alice, [y1]), RF).
play_row(join_cycle_moved, name(alice, []), RA).
:quit
HEREDOC
2>&1)
check "GF cycle: the Move of the Join at the parent, a descendant of the child, is refused" \
      "R = stuck(7, move(join(name(alice, \[\]), name(alice, \[y1\])), name(alice, \[\]), alice))" "$gf_cycle"
check "GF cycle: the child's motion carries the parent among its descendants" \
      "RF = \[moved(motion(join(name(alice, \[\]), name(alice, \[y1\])), name(alice, \[y1\]), \[alice\], \[name(alice, \[\])\], 0)), report(name(alice, \[\]), \[alice\], \[\], 3)" "$gf_cycle"
check "GF cycle: and that motion is in the parent's row when the Move is refused" \
      "RA = \[moved(motion(join(name(alice, \[\]), name(alice, \[y1\])), name(alice, \[y1\]), \[alice\], \[name(alice, \[\])\], 0)), will(alice" "$gf_cycle"

# The report of a grandchild reaches the grandparent: b reports to f1 = b.y1,
# then f1 reports to f2 = f1.y2 with the descendants it computes, and the
# descendants f2 computes hold both f1 and b.
gf_chain=$("$REPL_RUN" <<HEREDOC
$FED
play(report_chain, R).
play_row(report_chain, name(bob, [y1, y2]), RG).
play_state(report_chain, name(bob, [y1]), S1).
play_state(report_chain, name(bob, [y1, y2]), S2).
:quit
HEREDOC
2>&1)
check "GF chain: the run of two Federates and two Reports runs to the end" "R = done(" "$gf_chain"
check "GF chain: the parent reports its child among its descendants" \
      "RG = \[report(name(bob, \[y1\]), \[bob\], \[name(bob, \[\])\], 5)" "$gf_chain"
check "GF chain: the parent's descendants are its child" \
      "S1 = state(\[name(bob, \[\])\], \[name(bob, \[y1, y2\])\], \[bob\], \[name(bob, \[\])\]" "$gf_chain"
check "GF chain: the grandparent's descendants are the child and the grandchild" \
      "S2 = state(\[name(bob, \[y1\])\], \[\], \[bob\], \[name(bob, \[y1\]), name(bob, \[\])\]" "$gf_chain"

# Maintain (c): a child holds fewer seats than the floor of its share and has
# members unseated, so the one of least ratio is seated, coloured by the child,
# at the community's date.  erin's ratio is 2 and frank's is 1.
gf_mc=$("$REPL_RUN" <<HEREDOC
$FED
play_row(maintain_c, name(alice, [y1, y2]), R).
play_state(maintain_c, name(alice, [y1, y2]), S).
:quit
HEREDOC
2>&1)
check "GF maintain: the member of least ratio is seated" \
      "seat(plus, frank, col(name(bob, \[y1\])), 1, 2)" "$gf_mc"
check "GF maintain: and the assembly carries the new seat" \
      "st(frank, col(name(bob, \[y1\])), 1)\]" "$gf_mc"

# Maintain (g): no other rule holds and one seat has stood a full term, so it
# is removed.
gf_mg=$("$REPL_RUN" <<HEREDOC
$FED
play_row(maintain_g, name(alice, [y1, y2]), R).
play_state(maintain_g, name(alice, [y1, y2]), S).
:quit
HEREDOC
2>&1)
check "GF maintain: the seat whose term has passed is removed" \
      "seat(minus, bob, col(name(bob, \[y1\])), 0, 1)" "$gf_mg"
check "GF maintain: and the assembly is empty after it" \
      "S = state(\[name(bob, \[y1\])\], \[\], \[bob\], \[name(bob, \[y1\])\], \[\], 3" "$gf_mg"

# A transaction whose precondition fails answers blocked, and the run stops
# there naming it.
gf_block=$("$REPL_RUN" <<HEREDOC
$FED
play(join_twice, R1).
play(will_outsider, R2).
:quit
HEREDOC
2>&1)
check "GF blocked: a Join moved again where the edge holds" \
      "R1 = stuck(13, move(join(name(alice, \[y1\]), name(bob, \[\])), name(alice, \[y1\]), alice))" "$gf_block"
check "GF blocked: a Will by an agent outside the electorate" \
      "R2 = stuck(9, will_on(bob," "$gf_block"

# The guards: a Form, a Move and a Will by their agent, every other by nobody.
gf_guard=$("$REPL_RUN" <<HEREDOC
$FED
guard_of(form(alice), G1).
guard_of(move(federate(name(alice, []), y1), name(alice, []), alice), G2).
guard_of(will_on(alice, motion(federate(name(alice, []), y1), name(alice, []), [alice], [], 0), name(alice, [])), G3).
guard_of(raise_at(name(alice, [y1]), alice), G4).
guard_of(relay(date(alice, 0), name(alice, []), name(bob, []), alice, bob), G5).
guard_of(maintain_at(name(alice, [y1]), alice), G6).
guard_of(establish(name(alice, []), y1, alice), G7).
:quit
HEREDOC
2>&1)
check "GF guard: a Form is guarded by its agent" "G1 = \[alice\]" "$gf_guard"
check "GF guard: a Move is guarded by its agent" "G2 = \[alice\]" "$gf_guard"
check "GF guard: a Will is guarded by its agent" "G3 = \[alice\]" "$gf_guard"
check "GF guard: a Raise is guarded by nobody" "G4 = \[\]" "$gf_guard"
check "GF guard: a Relay is guarded by nobody" "G5 = \[\]" "$gf_guard"
check "GF guard: a Maintain is guarded by nobody" "G6 = \[\]" "$gf_guard"
check "GF guard: an Establish is guarded by nobody" "G7 = \[\]" "$gf_guard"

# --- The row lattice, its configurations and their quorums -----------------
# Definition "Row Lattice, Configuration, Quorum": the rows of a community
# under inclusion with union as join, the configuration of a row its set of
# seat records, its members the assembly those seat records compute, and its
# quorums the supermajorities of those members (Proposition "Intersection" is
# why two of them meet).  queried/3 is line 7 of Figure 2 of
# [kuznetsov2020reconfigurable]: the join of the commit estimate's
# configuration with every subset of the input configurations.
gf_lat=$("$REPL_RUN" <<HEREDOC
$FED
row_join([date(alice, 1)], [date(bob, 2), date(alice, 1)], R).
config([date(alice, 1), seat(plus, alice, uncoloured, 0, 0), seat(minus, bob, uncoloured, 0, 1)], C).
config_members([seat(plus, alice, uncoloured, 0, 0), seat(plus, bob, uncoloured, 0, 1)], name(carol, [y1]), M).
quorum_met(rat(1,2), [seat(plus, alice, uncoloured, 0, 0), seat(plus, bob, uncoloured, 0, 1)], name(carol, [y1]), [alice, bob], Q1).
quorum_met(rat(1,2), [seat(plus, alice, uncoloured, 0, 0), seat(plus, bob, uncoloured, 0, 1)], name(carol, [y1]), [alice], Q2).
quorum_met(rat(1,2), [seat(plus, alice, uncoloured, 0, 0), seat(plus, bob, uncoloured, 0, 1), seat(plus, carol, uncoloured, 0, 2)], name(carol, [y1]), [alice, carol], Q3).
queried([seat(plus, alice, uncoloured, 0, 0)], [[seat(plus, bob, uncoloured, 0, 1)]], V).
:quit
HEREDOC
2>&1)
check "GF lattice: the join of two rows is their union" \
      "R = \[date(alice, 1), date(bob, 2)\]" "$gf_lat"
check "GF lattice: the configuration of a row is its seat records" \
      "C = \[seat(plus, alice, uncoloured, 0, 0), seat(minus, bob, uncoloured, 0, 1)\]" "$gf_lat"
check "GF lattice: the members of a configuration are the assembly it computes" \
      "M = \[alice, bob\]" "$gf_lat"
check "GF lattice: both members of a two-member configuration are a quorum" "Q1 = yes" "$gf_lat"
check "GF lattice: one of two is not" "Q2 = no" "$gf_lat"
check "GF lattice: two of three are" "Q3 = yes" "$gf_lat"
check "GF lattice: line 7 queries every join of a subset of the inputs" \
      "V = \[\[seat(plus, alice, uncoloured, 0, 0)\], \[seat(plus, alice, uncoloured, 0, 0), seat(plus, bob, uncoloured, 0, 1)\]\]" "$gf_lat"

# --- The federation platform (Definition "Federation Platform") ------------
# One instance of the reconfigurable lattice agreement of Figure 2 of
# [kuznetsov2020reconfigurable] per community, over the rows of that community
# as its object, with the assembly a row computes as its configuration and the
# supermajorities of that assembly as its quorums, run by communicating
# volitional agents with known peers, an outbox, an inbox and a local date.  A
# record crosses from one community to the other by a disclosure, a cargo of
# its own sent by a member of the one assembly to a member of the other it
# knows, and the Relay is the proposal of that record by the member that
# received it.  Each run is taken once and read by prun_report/6: whether it
# ran to the end, the rows two agents learnt at two communities, the acts
# decided in them, the states they compute, and the messages left in flight.
#
# The Join of Figure 2 of /Grassroots/GFWC/sections/introduction.tex: two
# communities of two agents each, the Discovers by which the members of the
# two assemblies come to know one another, the Move at g, its disclosure to
# carol and carol's Relay of it at f, the Move at f, the Wills, and the
# disclosures and Relays that carry the remaining motion and the wills of each
# community to the other.  The same six records are learnt at both.
gf_plat=$("$REPL_RUN" <<HEREDOC
$FED
:limit 900000000
prun_report(join_platform, carol, name(carol, [y1, y2]), alice, name(alice, [y1, y2]), R).
:quit
HEREDOC
2>&1)
check "GF platform: the Join of Figure 2 runs to the end" "R = prep(yes, " "$gf_plat"
check "GF platform: f's learnt row holds g's motion, relayed to it first, then its own motion and the wills of carol and dave" \
      "moved(motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(alice, \[y1, y2\]), \[alice, bob\], \[\], 0)), moved(motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0)), will(carol, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0)), will(dave, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0))" "$gf_plat"
check "GF platform: and the wills of alice and bob, relayed to it" \
      "will(alice, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(alice, \[y1, y2\]), \[alice, bob\], \[\], 0)), will(bob, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(alice, \[y1, y2\]), \[alice, bob\], \[\], 0))\], \[origin(federate(name(alice, \[y1\]), y2)" "$gf_plat"
check "GF platform: g's learnt row holds f's motion and the wills of carol and dave, relayed" \
      "moved(motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0)), will(carol, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0)), will(dave, motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0))\], \[arec(join" "$gf_plat"
check "GF platform: the Join is decided at f, on the motion of each community" \
      "\[arec(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), \[motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(carol, \[y1, y2\]), \[carol, dave\], \[\], 0), motion(join(name(carol, \[y1, y2\]), name(alice, \[y1, y2\])), name(alice, \[y1, y2\]), \[alice, bob\], \[\], 0)\]), arec(federate(name(carol, \[y1\]), y2)" "$gf_plat"
check "GF platform: and at g, on the same two motions" \
      "arec(federate(name(alice, \[y1\]), y2), \[motion(federate(name(alice, \[y1\]), y2), name(alice, \[y1\]), \[alice, bob\], \[\], 0)\])\]" "$gf_plat"
check "GF platform: g is a child of f in the row learnt at f" \
      "state(\[name(carol, \[y1\]), name(alice, \[y1, y2\])\], \[\], \[\], \[\], \[st(carol, col(name(carol, \[y1\])), 0), st(dave, col(name(carol, \[y1\])), 0)\]" "$gf_plat"
check "GF platform: f is a parent of g in the row learnt at g" \
      "state(\[name(alice, \[y1\])\], \[name(carol, \[y1, y2\])\], \[\], \[\], \[st(alice, col(name(alice, \[y1\])), 0), st(bob, col(name(alice, \[y1\])), 0)\]" "$gf_plat"
check "GF platform: the run leaves no message in flight" "\[0, 0, 0, 0, 0, 0, 0, 0\])" "$gf_plat"

# Without the Discovers the disclosure is blocked: an agent discloses a record
# only to a peer it knows.  Without the disclosure the Relay is blocked:
# nothing was disclosed to the agent that would propose it.
gf_nodisc=$("$REPL_RUN" <<HEREDOC
$FED
:limit 900000000
prun(no_discover, N1).
prun(no_disclose, N2).
:quit
HEREDOC
2>&1)
check "GF platform: with no Discover the disclosure to the other community is blocked" \
      "N1 = pstuck(2, disclose(alice, carol, moved(motion(join" "$gf_nodisc"
check "GF platform: with no disclosure the Relay is blocked" \
      "N2 = pstuck(6, take(carol, relay(moved(motion(join" "$gf_nodisc"

# Two members of one community propose a record each, neither round having
# finished when the other opens: one learnt row holds both records, at each of
# them.
gf_conc=$("$REPL_RUN" <<HEREDOC
$FED
:limit 900000000
prun_report(concurrent_dates, carol, name(carol, [y1, y2]), dave, name(carol, [y1, y2]), R).
:quit
HEREDOC
2>&1)
check "GF platform: the concurrent proposals run to the end" "R = prep(yes, " "$gf_conc"
check "GF platform: carol's learnt row holds both records" \
      "date(carol, 5), date(dave, 7)\], \[origin(federate(name(carol, \[y1\]), y2)" "$gf_conc"
check "GF platform: dave's learnt row holds both records" \
      "date(carol, 5), date(dave, 7)\], \[arec(federate(name(carol, \[y1\]), y2)" "$gf_conc"
check "GF platform: the concurrent run leaves no message in flight" "\[0, 0, 0, 0\])" "$gf_conc"

# A member of an assembly of three crashes while the second will of a
# supermajority is still in flight: the quorum the other two form carries the
# proposal, and the act is decided all the same.
gf_crash=$("$REPL_RUN" <<HEREDOC
$FED
:limit 900000000
prun_report(crash_after_will, dave, name(carol, [y1, y2]), erin, name(carol, [y1, y2]), R).
:quit
HEREDOC
2>&1)
check "GF platform: the run with the crash runs to the end" "R = prep(yes, " "$gf_crash"
check "GF platform: the Federate is decided on the wills of two of the three" \
      "\[arec(federate(name(carol, \[y1, y2\]), y3), \[motion(federate(name(carol, \[y1, y2\]), y3), name(carol, \[y1, y2\]), \[carol, dave, erin\], \[\], 0)\]), arec(federate(name(carol, \[y1\]), y2)" "$gf_crash"
check "GF platform: the community the Federate forms is a parent in the learnt row" \
      "state(\[name(carol, \[y1\])\], \[name(carol, \[y1, y2, y3\])\], \[\], \[\]" "$gf_crash"
check "GF platform: the crashed member learnt the first will and not the second" \
      "will(carol, motion(federate(name(carol, \[y1, y2\]), y3), name(carol, \[y1, y2\]), \[carol, dave, erin\], \[\], 0))\], \[arec(federate(name(carol, \[y1, y2\]), y3)" "$gf_crash"
check "GF platform: so it has the Federate of its community undecided" \
      "\[arec(federate(name(carol, \[y1\]), y2), \[motion(federate(name(carol, \[y1\]), y2), name(carol, \[y1\]), \[carol, dave, erin\], \[\], 0)\])\], state(" "$gf_crash"
check "GF platform: so the motion is open still in the row it holds" \
      "0, \[\], \[motion(federate(name(carol, \[y1, y2\]), y3), name(carol, \[y1, y2\]), \[carol, dave, erin\], \[\], 0)\])" "$gf_crash"
check "GF platform: and the messages it never took are in its inbox" \
      "\[0, 0, 0, 0, 0, 2\])" "$gf_crash"

# Forming on the platform: an agent forms its own community, moves a Federate
# there, wills it --- being alone its own supermajority --- and establishes the
# community the Federate forms, whose instance it starts at the row the
# Establish forms, that row's configuration being the initial one.
gf_form=$("$REPL_RUN" <<HEREDOC
$FED
:limit 900000000
prun_report(form_federate, alice, name(alice, [y1]), alice, name(alice, []), R).
:quit
HEREDOC
2>&1)
check "GF platform: the forming run runs to the end" "R = prep(yes, " "$gf_form"
check "GF platform: the Establish starts the new community at its origin and seat" \
      "R = prep(yes, \[origin(federate(name(alice, \[\]), y1), motion(federate(name(alice, \[\]), y1), name(alice, \[\]), \[alice\], \[\], 0)), seat(plus, alice, col(name(alice, \[\])), 0, 0)\]" "$gf_form"
check "GF platform: the Form gives the community it forms the agent's date" \
      "\[date(alice, 0), moved(motion(federate(name(alice, \[\]), y1), name(alice, \[\]), \[alice\], \[\], 0)), will(alice, " "$gf_form"
check "GF platform: and the community that federated has the new one as a parent" \
      "state(\[\], \[name(alice, \[y1\])\], \[alice\], \[\], \[st(alice, uncoloured, 0)\], 0, \[\], \[\])" "$gf_form"

echo ""

fi  # Section GF

if _section GSC; then
# =============================================================================
# Section GSC: The GSC checker and compiler (programs/gsc)
# =============================================================================
# programs/gsc decides whether a contract is syntactically grassroots and
# compiles the schemas of one that is, against /Grassroots/GSC; a contract is
# read from its owner's file (programs/gsc/GSN.glp,
# programs/currencies/coins/GC.glp, programs/cssn/CSSN.glp,
# programs/federation/GFWC.glp), one copy each, and the fixtures that
# change one are programs/gsc/contracts.glp's variants of it.  Its checks
# were written as programs/gsc/test_gsc.sh, which runs the same REPL binary
# this suite does and prints one summary line.  The script stays the one place
# those checks are written --- the paper's owner reads and runs it on its own,
# and a copy here would go stale against it --- so this section runs it and
# folds its two counts into the suite's, adding nothing of its own.
echo "=== Section GSC: GSC checker and compiler ==="
echo ""

# `set -e` (line 25) and a non-zero exit from a red check would abort the run,
# so the capture is guarded, as Section Q's are.
GSC_RESULT=$(bash "$GLP_DIR/programs/gsc/test_gsc.sh" 2>&1) && GSC_STATUS=0 || GSC_STATUS=$?
GSC_SUMMARY=$(printf '%s' "$GSC_RESULT" | grep -oE '^=== [0-9]+ passed, [0-9]+ failed ===$' | tail -1) || true

# No summary line means the script did not finish its checks --- a missing
# file, a REPL that would not build, a syntax error --- and scoring it zero
# would report a green suite over checks that never ran.  That is a failure of
# the suite, not a skip.
if [ -z "$GSC_SUMMARY" ]; then
    echo "  FAIL: programs/gsc/test_gsc.sh exited $GSC_STATUS printing no summary, so its checks did not run"
    printf '%s\n' "$GSC_RESULT" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
else
    GSC_PASSED=$(printf '%s' "$GSC_SUMMARY" | awk '{print $2}')
    GSC_FAILED=$(printf '%s' "$GSC_SUMMARY" | awk '{print $4}')
    PASS=$((PASS + GSC_PASSED))
    FAIL=$((FAIL + GSC_FAILED))
    if [ "$GSC_FAILED" -eq 0 ]; then
        echo "  PASS: All $GSC_PASSED gsc checks passed"
    else
        printf '%s\n' "$GSC_RESULT" | grep -E '^  FAIL|^        ' | sed 's/^/  /'
        echo "  FAIL: $GSC_FAILED of $((GSC_PASSED + GSC_FAILED)) gsc checks failed"
    fi
fi

echo ""

fi  # Section GSC

if _section CAS; then
# =============================================================================
# Section CAS: The cascade, recorded whole (cascade.sh)
# =============================================================================
# A run of the cascade on a contract is recorded in cascade/<NAME>/, at the
# repository root, NAME the contract's name (Appendix C), as one tagged commit (claude.md "Code"; Coordination 5.7): the contract's two
# files with the checker's verdict, the VMAT form, the canonical platform as
# .vglp and as .glp, the platform's interface in sGLP's term form, and its run
# with vGLP's scripted persons, with a manifest.
# This section re-runs cascade.sh --check on every recorded contract and holds
# the committed artefacts to what the tools print now; a contract with no
# record is not run.
echo "=== Section CAS: The cascade, recorded whole ==="
echo ""

for cas_c in gsg gsn gsnp gc cssn gfwc gai; do
    cas_n=$(printf '%s' "$cas_c" | tr a-z A-Z)
    cas_o="cascade/$cas_n"
    [ -f "$GLP_DIR/$cas_o/manifest.md" ] || continue
    cas_out=$(bash "$GLP_DIR/cascade.sh" "$cas_c" --check 2>&1; echo "exit $?")
    for cas_f in "$cas_n.md" "$cas_n.glp" verdict.txt vmat.tex platform.vglp platform.glp interface.glp run.log; do
        [ -f "$GLP_DIR/$cas_o/$cas_f" ] || continue
        check "cascade $cas_c: $cas_f is what the tools print" "^$cas_f: as printed$" "$cas_out"
    done
    check "cascade $cas_c: cascade.sh --check exits 0" "^exit 0$" "$cas_out"
done

echo ""

fi  # Section CAS

if _section SGLP; then
# =============================================================================
# Section SGLP: sGLP in GLP (programs/sglp)
# =============================================================================
# programs/sglp/test_sglp.sh holds sGLP's checks of its programs --- the monitor
# over when_idle, the law of its releases, the social graph's runs, the
# circulation's replay on a fixed log, coins among friends' runs --- against
# sGLP's paper, and prints one summary line; this section runs it and folds its
# two counts into the suite's, as Section GSC does gsc's.  Its runs take some
# minutes.
echo "=== Section SGLP: sGLP in GLP ==="
echo ""

SGLP_RESULT=$(bash "$GLP_DIR/programs/sglp/test_sglp.sh" 2>&1) && SGLP_STATUS=0 || SGLP_STATUS=$?
SGLP_SUMMARY=$(printf '%s' "$SGLP_RESULT" | grep -oE '^=== [0-9]+ passed, [0-9]+ failed ===$' | tail -1) || true

if [ -z "$SGLP_SUMMARY" ]; then
    echo "  FAIL: programs/sglp/test_sglp.sh exited $SGLP_STATUS printing no summary, so its checks did not run"
    printf '%s\n' "$SGLP_RESULT" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
else
    SGLP_PASSED=$(printf '%s' "$SGLP_SUMMARY" | awk '{print $2}')
    SGLP_FAILED=$(printf '%s' "$SGLP_SUMMARY" | awk '{print $4}')
    PASS=$((PASS + SGLP_PASSED))
    FAIL=$((FAIL + SGLP_FAILED))
    if [ "$SGLP_FAILED" -eq 0 ]; then
        echo "  PASS: All $SGLP_PASSED sGLP checks passed"
    else
        printf '%s\n' "$SGLP_RESULT" | grep -E '^  FAIL|^        ' | sed 's/^/  /'
        echo "  FAIL: $SGLP_FAILED of $((SGLP_PASSED + SGLP_FAILED)) sGLP checks failed"
    fi
fi

# A simulation outside programs/sglp (Integration's ruling of 2026-10-16 10:52
# UTC, item 1; sGLP's answers of 11:25 and 12:32 UTC): programs/tests/sglp_outside
# is transformed by transform.sh --source, its printed modules as printed, and it
# loads and runs with the monitor and sglp.glp exposed from the root
# programs/self.glp (TGLP modules.tex, "The -expose directive").
sglp_out_tx=$(bash "$GLP_DIR/programs/sglp/transform.sh" --source "$GLP_DIR/programs/tests/sglp_outside" --check 2>&1; echo "exit $?")
check "sglp outside: transform.sh --source --check, profiles.glp as printed" "^profiles.glp: as printed$" "$sglp_out_tx"
check "sglp outside: transform.sh --source --check, population.glp as printed" "^population.glp: as printed$" "$sglp_out_tx"
check "sglp outside: transform.sh --source --check exits 0" "^exit 0$" "$sglp_out_tx"
sglp_out_run=$("$REPL_RUN" <<HEREDOC
:limit 1000000000000
$GLP_DIR/programs/tests/sglp_outside
poll_run(5, As).
:quit
HEREDOC
2>&1)
check "sglp outside: the program loads" "Loaded program" "$sglp_out_run"
check "sglp outside: the person answers each poll, logged with the monitor's clock" "entry(14916.00387843316, 1, Poll, poll(2, yes))" "$sglp_out_run"
check "sglp outside: the program reads the five answers" "As = \[yes, yes, yes, yes, yes\]" "$sglp_out_run"

# The simulations of canonical CVA platforms under sGLP, each a record of its
# own in simulation/<NAME>/<date>/ (claude.md, "Code"; sGLP's task of
# 2026-10-18 20:50 UTC, item 4): programs/sglp/cva/record.sh --check re-runs
# the transformation on the record's sGLP source, holding its printed modules
# to the record's, and each run's first simulated day with its seed, holding
# its log to the record's up to that time.  With no record, nothing is run.
for sim_m in "$GLP_DIR"/simulation/*/*/manifest.md; do
    [ -f "$sim_m" ] || continue
    sim_r=$(dirname "$sim_m"); sim_n=${sim_r#$GLP_DIR/}
    sim_out=$(bash "$GLP_DIR/programs/sglp/cva/record.sh" --check "$sim_r" 2>&1; echo "exit $?")
    check "$sim_n: profiles.glp as printed" "^profiles.glp: as printed$" "$sim_out"
    check "$sim_n: population.glp as printed" "^population.glp: as printed$" "$sim_out"
    for sim_s in $(sed -n 's/^- seeds: //p' "$sim_m" | tr ',' ' '); do
        check "$sim_n: seed $sim_s's first day as recorded" "^$sim_s.log: first day as recorded" "$sim_out"
    done
    check "$sim_n: record.sh --check exits 0" "^exit 0$" "$sim_out"
done

echo ""

fi  # Section SGLP

if _section QD || _section QF; then
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
# The four vGLP entries went on 2026-09-09: each named a parse test of a
# deployed .vglp by the path it had before SGSG, Currencies and CSSN moved the
# source with its .glp into the certified program of a mini-app, and vGLP
# changed the four paths in test/vglp/program_compilation_test.dart, which is
# what each of the three moving projects asked for --- CSSN's move carried two
# sources and the other two one each, which is why four entries answered three
# requests.  All eight deployed sources parse.
#
# GLP (IGLP layer), 2026-09-24: the sovereign screen.  Currencies' b2367233 gave
# the denominated mini-app twelve request clauses where it had seven, for SGC's
# contract, and lib/manifests/sovereign_ui.dart still describes seven forms and
# the old model.  The manifest is the image of the display declarations, and its
# twelve forms and their labels are GLP's to write.  SGC shows no screen, so this
# does not hold SGC.
KNOWN_RED=(
    "glp_multiagent/test/sovereign_screen_test.dart: the sovereign mini-app: the central bank boots, its seven forms stand, it mints and opens the credit line through its screen, and the play runs on to the chain redemption and the fiat payment"
)

echo "=== Section Q: Dart unit tests (whole tree) ==="
echo ""

# glp_multiagent/assets/glp/ is generated and gitignored (2026-09-16), so a
# fresh clone or a new worktree has none, and pubspec.yaml declares asset
# directories that are not there.  `flutter test` then dies at "Failed to build
# asset bundle" before running a single test, printing no count and no [E]
# lines --- which the parsing below would read as zero passed and nothing red.
# Generate them here rather than trusting every session to remember: the script
# is idempotent and the tree it writes is gitignored, so it cannot move the
# tree-change guard.  It is QF's: only flutter test reads the bundle.
if ! _section QF; then
    :
elif [ -f "$GLP_DIR/glp_multiagent/tool/sync_glp_assets.sh" ]; then
    SYNC_RESULT=$(cd "$GLP_DIR/glp_multiagent" && bash tool/sync_glp_assets.sh 2>&1) || true
    # Each certified mini-app the script builds is a check of its own, so that
    # an artefact its owner's source cannot yet give is known red, not the
    # whole step.  The script stops at the
    # first artefact missing, so each is looked for on disc; its list is the
    # script's own.
    SYNC_ARTEFACTS=$(sed -n 's/^for a in \(.*\); do$/\1/p' "$GLP_DIR/glp_multiagent/tool/sync_glp_assets.sh")
    SYNC_MISSING=0
    if [ -z "$SYNC_ARTEFACTS" ]; then
        echo "  FAIL: the artefact list of sync_glp_assets.sh cannot be read"
        FAIL=$((FAIL + 1))
    fi
    for a in $SYNC_ARTEFACTS; do
        if [ -s "$GLP_DIR/glp_multiagent/assets/glp/programs/social/graph/core/$a.glpw" ]; then
            _check_pass "Q asset step writes $a.glpw"
        else
            _check_fail "Q asset step writes $a.glpw" "not written"
            SYNC_MISSING=$((SYNC_MISSING + 1))
        fi
    done
    if printf '%s' "$SYNC_RESULT" | grep -q '^Synced GLP assets'; then
        echo "  (the bundled GLP assets are generated)"
    elif [ "$SYNC_MISSING" -gt 0 ]; then
        echo "  (the bundled GLP assets are generated but for the artefacts above)"
    else
        echo "  FAIL: sync_glp_assets.sh did not complete, so glp_multiagent's"
        echo "        assets are missing or stale and its tests cannot run:"
        printf '%s\n' "$SYNC_RESULT" | tail -5 | sed 's/^/        /'
        FAIL=$((FAIL + 1))
    fi
else
    echo "  FAIL: glp_multiagent/tool/sync_glp_assets.sh is missing"
    FAIL=$((FAIL + 1))
fi

# `dart test` exits non-zero whenever anything is red — including a known-red
# entry, which is not a suite failure — so every capture below is guarded
# against `set -e` (line 24) rather than letting it abort the run.
#
# Each runner's raw output is kept, overwritten on every run, so that a failure
# can be read in full after the suite ends: the section itself prints only the
# failing names and, when its gate fails, the last twenty lines of dart test's
# output and none of flutter test's (Integration, 2026-10-11).  Writing them
# reads nothing back and moves no count or gate.
Q_RAW_DIR="/Users/udi/Grassroots/tmp"
Q_RAW_DART="$Q_RAW_DIR/suite_q_dart.txt"
Q_RAW_FLUTTER="$Q_RAW_DIR/suite_q_flutter.txt"
mkdir -p "$Q_RAW_DIR" 2>/dev/null || true
if _section QD; then
DART_TREE_RESULT=$(cd "$GLP_RUNTIME" && "$DART" test 2>&1) && DART_TREE_STATUS=0 || DART_TREE_STATUS=$?
printf '%s\n' "$DART_TREE_RESULT" > "$Q_RAW_DART" 2>/dev/null \
    || echo "  (dart test's output could not be written to $Q_RAW_DART)"
else
    DART_TREE_RESULT=""
    DART_TREE_STATUS=0
fi

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
if ! _section QF; then
    MA_PASSED=0
    MA_FAILS=""
elif ! command -v flutter >/dev/null 2>&1; then
    echo "  FAIL: flutter is not on PATH, so glp_multiagent/test/ cannot run"
    echo "        Half the Dart tree would go unmeasured; the suite reports no"
    echo "        result rather than a green one."
    FAIL=$((FAIL + 1))
    MA_PASSED=0
    MA_FAILS=""
    echo "flutter is not on PATH, so glp_multiagent/test/ did not run" > "$Q_RAW_FLUTTER" 2>/dev/null \
        || echo "  (flutter test's output could not be written to $Q_RAW_FLUTTER)"
else
    MA_RESULT=$(cd "$GLP_DIR/glp_multiagent" && flutter test 2>&1) && MA_STATUS=0 || MA_STATUS=$?
    printf '%s\n' "$MA_RESULT" > "$Q_RAW_FLUTTER" 2>/dev/null \
        || echo "  (flutter test's output could not be written to $Q_RAW_FLUTTER)"
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

# A runner that exits non-zero while naming no failing test did not run its
# tests: it died before or during collection, so there are no [E] lines to
# count and the tree would be scored on whatever partial "+N" the reporter had
# printed.  That is a green suite over an unmeasured half --- the defect this
# section exists to remove --- so it is a hard failure, as a missing flutter
# is.  It is not enough to check the count: with the bundled assets absent
# `flutter test` still prints partial counts before dying at "Failed to build
# asset bundle", which scored the tree 744 instead of 804 and still reported
# ALL TESTS PASSED (worktree against main, 2026-09-16).
if [ "${DART_TREE_STATUS:-0}" -ne 0 ] && [ -z "$DART_TREE_FAILS" ]; then
    echo "  FAIL: dart test exited $DART_TREE_STATUS naming no failing test, so glp_runtime/test/ did not run"
    printf '%s\n' "$DART_TREE_CLEAN" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
fi
if [ "${MA_STATUS:-0}" -ne 0 ] && [ -z "$MA_FAILS" ]; then
    echo "  FAIL: flutter test exited $MA_STATUS naming no failing test, so glp_multiagent/test/ did not run"
    printf '%s\n' "$MA_CLEAN" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
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
# An entry of the half not run is neither red nor a rot: glp_multiagent's are
# QF's, the rest QD's.
Q_STALE=0
for known in "${KNOWN_RED[@]}"; do
    case "$known" in
        glp_multiagent/*) _section QF || continue ;;
        *)                _section QD || continue ;;
    esac
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

fi  # Section Q

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
if [ "$RUN_TIER" = "FULL" ]; then
    echo "Total: $TOTAL | Passed: $PASS | Failed: $FAIL"
else
    echo "$RUN_TIER Total: $TOTAL | Passed: $PASS | Failed: $FAIL | Sections: $RUN_SECTIONS"
fi
if [ $KR_CHECKS -gt 0 ]; then
    echo "Known red, REPL checks (KNOWN_RED_CHECKS, not counted above): $KR_CHECKS"
fi
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

if [ $FAIL -eq 0 ] && [ "$RUN_TIER" = "FULL" ]; then
    echo "ALL TESTS PASSED!"
    exit 0
elif [ $FAIL -eq 0 ]; then
    echo "$RUN_TIER: ALL TESTS OF THESE SECTIONS PASSED (not the full suite)"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
