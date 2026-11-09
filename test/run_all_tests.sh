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
#   K - CSSN, the child-safe social network (programs/cssn): the canonical platform of CSSN.glp and the village
#   L - Dynamic Module Dispatch Tests (activate + M # goal)
#   M - Multi-Isolate (madGLP) Tests (dart test, one isolate per agent; run in Section Q)
#   N - Currencies Modules (project-directory loading, plays 1-12)
#   O - Currencies Multi-Isolate Tests (dart test, one isolate per agent)
#   P - Module Boundary Enforcement Tests (exported vs private procedures)
#   GF - Grassroots federation records, transactions and platform (GFWC)
#   APPS - The apps of the contracts (programs/apps), mini-apps compiled down the cascade: GKK's, GSG's, GSN's, GSNP's, GAI's, GC's and GB's
#   TY - The tight typing discipline: no untyped procedure and no open type off test/OPEN_TYPES
#   NET - networking's own tests (GNA): flutter test in networking/ and networking/glue, dart test in networking/core, flutter test in servers/directory
#   DL - the debugging log service of the phones (servers/debuglog): dart test there
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
ALL_SECTIONS="TY A B C D E G I K L M N2 N3 N4 VG O P J S X Y Z ZG RM SK MB SL MA RV SG APPS GF GSC CAS SGLP QD QF NET DL"
FAST_SECTIONS="TY A B C D E G I K L M N2 N3 N4 O P J S X Y Z ZG RM SK MB SL MA RV SG APPS GF GSC"
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
# The super-app's checks of CSSN's hand-written program, programs/cssn as it
# stood (its artefact refused a certificate, given as text) and its childsafe/
# (its artefact certified; the two mini-apps at once and on two smartphones),
# which CSSN's compiled platform replaced and deleted (CSSN #5, Code_inbox
# 2026-10-18 21:52 UTC).  GSG's to port; GSG is dormant (Udi, 2026-10-19:
# "mark GSG's checks of CSSN's deleted hand-written program known red, owner
# GSG, as for the dormant Currencies and GFWC").  Each entry comes off with
# GSG's port.
    "SG super-app: cssn's artefact is refused a certificate"
    "SG super-app: load_file gives cssn's artefact as text"
    "SG super-app: the super-app installs no text"
    "SG super-app: the child-safe platform's artefact is certified"
    "SG two mini-apps at once: alice's swap settles"
    "SG two mini-apps at once: bob's swap settles"
    "SG two mini-apps on two smartphones: the currency's conversation opens"
    "SG two mini-apps on two smartphones: alice's swap settles"
    "SG two mini-apps on two smartphones: bob's swap settles"
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

# A boot's lines of one person, [<person>] <term>, the super-app's sg(...) lines left aside, in order, as a list; and sorted, where the isolates' order is not held (the boots of the currencies' plays, Sections N2, N3 and N4; the apps' boots have their own in Section APPS).
boot_person() { # boot_person <person> <output>
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") != 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
boot_person_sorted() { # boot_person_sorted <person> <output>
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") != 1 { print substr($0, length(p) + 1) }' | sort | awk '{ s = s (n++ ? ", " : "") $0 } END { print "[" s "]" }'
}
boot_same() { # boot_same <name> <expected> <actual>
    if [ "$3" = "$2" ]; then _check_pass "$1"; else _check_fail "$1" "expected: $2; got: $3"; fi
}

if _section TY; then
# =============================================================================
# Section TY: The tight typing discipline (test/check_typing.py, test/OPEN_TYPES)
# =============================================================================
# NO NEW GLP CODE IS WRITTEN UNTYPED, OR WITH OPEN TYPES (Udi, 2026-10-24;
# claude.md, "Code"; Coordination #9's task 1, Code_inbox 2026-10-25 05:28
# UTC).  check_typing.py prints every procedure with clauses and no
# declaration in its module, and every `_` or `_?` in a type definition or a
# procedure declaration, over every .glp and .vglp file git tracks, hand-written
# and printed alike, the root self.glp's clause-less declarations (the guard
# predicates and body kernels the runtime implements) excepted.  test/OPEN_TYPES
# lists those there on 2026-10-30.  A finding not on the list is a failure; so is
# an entry no longer found (the rot guard, as KNOWN_RED's): the list only
# shrinks, and an entry is deleted in the commit that types it.
echo "=== Section TY: The tight typing discipline ==="
echo ""

TY_OUT=$(python3 "$SCRIPT_DIR/check_typing.py" "$GLP_DIR" 2>&1) && TY_STATUS=0 || TY_STATUS=$?
if [ "$TY_STATUS" -ne 0 ]; then
    echo "  FAIL: TY: test/check_typing.py exited $TY_STATUS, so the discipline was not checked"
    printf '%s\n' "$TY_OUT" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
else
    TY_FOUND=$(printf '%s\n' "$TY_OUT" | grep -v '^$' | LC_ALL=C sort -u) || true
    TY_LIST=$(grep -v -e '^#' -e '^$' "$SCRIPT_DIR/OPEN_TYPES" | LC_ALL=C sort -u) || true
    TY_NEW=$(LC_ALL=C comm -23 <(printf '%s\n' "$TY_FOUND") <(printf '%s\n' "$TY_LIST") | grep -v '^$') || true
    TY_GONE=$(LC_ALL=C comm -13 <(printf '%s\n' "$TY_FOUND") <(printf '%s\n' "$TY_LIST") | grep -v '^$') || true
    if [ -z "$TY_NEW" ]; then
        _check_pass "TY: every untyped procedure and open type is on test/OPEN_TYPES"
    else
        while IFS= read -r ty_l; do
            echo "  FAIL: TY: not on test/OPEN_TYPES, so new: $ty_l"
            FAIL=$((FAIL + 1))
        done <<< "$TY_NEW"
    fi
    if [ -z "$TY_GONE" ]; then
        _check_pass "TY: every entry of test/OPEN_TYPES is still found"
    else
        while IFS= read -r ty_l; do
            echo "  FAIL: TY: no longer found, so delete it from test/OPEN_TYPES: $ty_l"
            FAIL=$((FAIL + 1))
        done <<< "$TY_GONE"
    fi
    TY_N=$(printf '%s\n' "$TY_LIST" | grep -c .) || true
    echo "  test/OPEN_TYPES: $TY_N entries ($(printf '%s\n' "$TY_LIST" | grep -c $'\tuntyped\t') untyped, $(printf '%s\n' "$TY_LIST" | grep -c $'\topen-decl\t') open-decl, $(printf '%s\n' "$TY_LIST" | grep -c $'\topen-type\t') open-type)"
fi

echo ""

fi  # Section TY

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
# SplitMix64 (GLP-ICLP-2026 2fc437f).  Value in [1,N]; NextSeed a function of
# Seed alone; threading reproduces exactly (rthree draws three, threading
# NextSeed); N < 1 fails at the guard.  cover is GLP #11's covering test
# (Code_inbox 2026-11-01 07:26 UTC): from 0, 2147483647 and -1, a hundred draws
# with bound 6 all in [1, 6], and the three streams differ.  The pinned values
# were computed from the paper's sentences in Python, independently of the code.
echo "--- A33: Seeded random ---"
a33=$("$REPL_RUN" <<HEREDOC
$TYPED/random_test.glp
random(42, 6, V, NS).
rthree(1, R).
cover(0, C0).
cover(2147483647, C1).
cover(-1, C2).
random(42, 0, VBad, NSBad).
:quit
HEREDOC
2>&1)
check "random single draw value" "V = 4" "$a33"
check "random single draw next seed" "NS = -7046029254386353089" "$a33"
check "random threaded triple reproduces" "R = rt(230, 507, 4)" "$a33"
check "random cover seed 0" "C0 = cover(100, \[3, 6, 4, 5, 4\])" "$a33"
check "random cover seed 2147483647" "C1 = cover(100, \[5, 6, 6, 5, 5\])" "$a33"
check "random cover seed -1" "C2 = cover(100, \[3, 5, 5, 3, 4\])" "$a33"
check "random N<1 fails at guard" "→ failed" "$a33"

# --- A33a: Splitting a seed (split_random_seed/3, GLP-ICLP appendix-guards) ---
# From one seed, the three streams of random/4 draws from Seed, S1 and S2 differ
# from one another and from any shift of one another over the first hundred
# draws (shifted counts the agreeing shifts over the six ordered pairs; control
# shows a shift is counted), and a second run from the same seed reproduces
# them (reproduces, and the split and first draws pinned twice).
echo "--- A33a: Splitting a seed ---"
a33a=$("$REPL_RUN" <<HEREDOC
$TYPED/split_random_test.glp
split_random_seed(42, S1, S2).
shifted(42, N).
shifted(7, N7).
control(42, C).
reproduces(42, R).
firsts(42, F).
split_random_seed(42, T1, T2).
firsts(42, G).
:quit
HEREDOC
2>&1)
check "split_random_seed S1" "S1 = 3250621881602310087" "$a33a"
check "split_random_seed S2" "S2 = 540350159304224772" "$a33a"
check "split streams unshifted (42)" "N = 0" "$a33a"
check "split streams unshifted (7)" "N7 = 0" "$a33a"
check "split shift control" "C = 1" "$a33a"
check "split second run reproduces" "R = yes" "$a33a"
check "split first draws" "F = firsts(745971022, 1039421601, 1816191086)" "$a33a"
check "split second split reproduces" "T1 = 3250621881602310087" "$a33a"
check "split second first draws reproduce" "G = firsts(745971022, 1039421601, 1816191086)" "$a33a"

# --- A33b: Simulated time (start_clock/1, rated/2, step_clock/2) ---
# GLP-ICLP appendix-guards, "Simulated time" (GLP-ICLP-2026 5fd9a15).  The
# loop is the monitor's: a clause guarded by when_idle whose body is step_clock
# (times/3).  repro: seed 42, rates 1, 2 and 0.5, run twice, the reference
# releases and stopped(C).  hz: the horizon, the tokens past it bound to stop,
# Step stopped(C).  tie: two tokens of one activation time released in the
# order made pending.  restart: start_clock binds a pending token to stop.
# rated_first / unrated_first: sGLP's "The claim, tested", one --- the unrated
# goal runs to rest before the rated one is released, whichever is spawned
# first.  claim: "The claim, tested", two --- rates 1, 2, 0.5 and 1 a second,
# 10,000 releases, mean and variance within 5% and 10% of 1/4.5 and 1/4.5^2,
# and the mean the reference.  The pinned values were computed from the
# paper's sentences in Python, independently of the code.  a33c: rated and
# step_clock abort before the first start_clock.
echo "--- A33b: Simulated time ---"
a33b=$("$REPL_RUN" <<HEREDOC
$TYPED/clock_test.glp
:limit 5000000
repro(42, R1).
repro(42, R2).
hz(0.5, H).
tie(T).
restart(A).
rated_first(RF).
unrated_first(UF).
claim(1, St).
:quit
HEREDOC
2>&1)
check "clock repro from seed 42" "R1 = run(\[1.522941750012381, 1.7360415557214341, 1.7652128941799705\], stopped(1.7652128941799705))" "$a33b"
check "clock repro second run" "R2 = run(\[1.522941750012381, 1.7360415557214341, 1.7652128941799705\], stopped(1.7652128941799705))" "$a33b"
check "clock horizon stops tokens past it" "H = hz(\[0.32910293578974764\], stopped(0.32910293578974764), toks(stop, stop, go(0.32910293578974764)))" "$a33b"
check "clock tie in order made pending" "T = tie(x_first, same)" "$a33b"
check "clock start_clock stops pending" "A = stop" "$a33b"
check "clock unrated runs to rest (rated first)" "RF = after" "$a33b"
check "clock unrated runs to rest (unrated first)" "UF = after" "$a33b"
check "clock four rates, 10,000 releases" "St = stats(0.22016502528052906, 0.0473870040391368, ok)" "$a33b"
check_not "clock no abort" "ABORT" "$a33b"
a33c=$("$REPL_RUN" <<HEREDOC
$TYPED/clock_test.glp
rated(1, X).
step_clock(5, S).
:quit
HEREDOC
2>&1)
check "clock rated aborts before start_clock" "\[ABORT\] rated/2: the simulated clock is not started" "$a33c"
check "clock step_clock aborts before start_clock" "\[ABORT\] step_clock/2: the simulated clock is not started" "$a33c"

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
check "operator names: 0 - 5 + 2 is -3, GLP having no unary minus" "B36 = -3" "$a36"
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


# --- A42: Text: string_concat/2 and term_to_string/2 ---
# GLP-Spec appendix-guards.tex, "Text (string_concat/2, term_to_string/2)"
# (GLP-ICLP-2026 ca3c8e5; GLP #12, Code_inbox 2026-11-06 07:18 UTC).
# string_concat/2: the characters of the strings one after another, [] as an
# element contributing the characters [].  term_to_string/2: "a text that the
# reader reads as the ground term T".  shown(14) prints, by send_to_user/1, a
# line for each sample, its number and its text, built by the two; a42b reads
# each printed line back as a goal same(I, Text, R), the reader's term
# compared with sample I by =?=.  The kernel's abort on a number is the Dart
# test's (glp_runtime/test/runtime/text_kernels_test.dart): a number is not a
# String, and a goal or a clause passing one does not type-check.
echo "--- A42: Text ---"
a42=$("$REPL_RUN" <<HEREDOC
$TYPED/text_test.glp
string_concat(['Menu', '?'], C1).
string_concat([], C2).
string_concat(['one'], C3).
string_concat(['X = ', []], C4).
text(1, X1).
shown(14).
:quit
HEREDOC
2>&1)
check "text: the fixture loads" "Loaded: .*text_test.glp" "$a42"
check "text: string_concat(['Menu', '?'], S)" "C1 = 'Menu?'" "$a42"
check "text: string_concat of []" "C2 = ''" "$a42"
check "text: string_concat of one string" "C3 = one" "$a42"
check "text: an element [] contributes []" "C4 = 'X = \[\]'" "$a42"
check "text: term_to_string('Carol', S)" "X1 = '\\\\'Carol\\\\''" "$a42"
a42_lines=$(echo "$a42" | sed -e 's/^\(GLP> \)*//' | grep -E '^[0-9]+ ')
check "text: Carol quoted" "^1 'Carol'$" "$a42_lines"
check "text: G quoted" "^2 'G'$" "$a42_lines"
check "text: + quoted" "^3 '+'$" "$a42_lines"
check "text: '42' quoted" "^4 '42'$" "$a42_lines"
check "text: a string with spaces quoted" "^5 'Flood on Main Street'$" "$a42_lines"
check "text: a structure" "^6 utterance('Flood on Main Street', f('G', +('Carol')))$" "$a42_lines"
check "text: a list" "^7 \['Carol', 'G', '+', '42'\]$" "$a42_lines"
check "text: a list with a tail" "^8 \[a, 'B' | 'C'\]$" "$a42_lines"
check "text: an integer" "^9 42$" "$a42_lines"
check "text: a negative integer" "^10 -7$" "$a42_lines"
check "text: a real" "^11 2\.5$" "$a42_lines"
check "text: a negative argument" "^12 neg(-7)$" "$a42_lines"
check "text: reals in digits, no exponent" "^13 f(0\.0000001, 1000000000000000000000\.0)$" "$a42_lines"
check "text: a list with a structure tail" "^14 \[1, -2\.5 | f(a, -3)\]$" "$a42_lines"
check "text: fourteen lines printed" "^14$" "$(echo "$a42_lines" | grep -c .)"
a42_goals=$(echo "$a42_lines" | sed -E 's/^([0-9]+) (.*)$/same(\1, \2, R\1)./')
a42b=$("$REPL_RUN" <<HEREDOC
$TYPED/text_test.glp
$a42_goals
same(1, 'G', Q).
:quit
HEREDOC
2>&1)
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do
    check "text: printed line $i reads back as sample $i" "R$i = yes" "$a42b"
done
check "text: the comparison can fail" "Q = no" "$a42b"
check_not "text: no abort" "ABORT" "$a42$a42b"

# --- A43: The tree library, programs/lib/tree ---
# A size-annotated balanced tree of constants (Code #24's task 2, Cloud_inbox
# 2026-11-06 17:50 UTC): each operation against a list model, the balance at
# every node after each insertion, in a drawn order and in ascending and
# descending order, and kth at every index.  tree_test.glp's header names the
# checks; each test gives ok when every check holds.
echo "--- A43: lib/tree ---"
a43=$("$REPL_RUN" <<HEREDOC
:limit 100000000
$GLP_DIR/programs/lib/tree/
test_insert(300, 7, R1).
test_insert(300, 500, R2).
test_ascending(200, R3).
test_descending(200, R4).
test_from_list(300, 11, R5).
test_member(300, 13, R6).
test_kth(300, 17, R7).
test_union(150, 19, 23, R8).
:quit
HEREDOC
2>&1)
check "tree: the library and its tests load" "Loaded program: .*lib/tree" "$a43"
check "tree: insertion in a drawn order, seed 7" "R1 = ok" "$a43"
check "tree: insertion in a drawn order, seed 500" "R2 = ok" "$a43"
check "tree: insertion in ascending order" "R3 = ok" "$a43"
check "tree: insertion in descending order" "R4 = ok" "$a43"
check "tree: from_list" "R5 = ok" "$a43"
check "tree: member" "R6 = ok" "$a43"
check "tree: kth at every index" "R7 = ok" "$a43"
check "tree: union" "R8 = ok" "$a43"

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
check "NEG SRSW / :boot, no program directory: the refusal names the reader occurring twice" "agent_init/2: Line 19: Reader variable \"Id?\" occurs 2 times" "$neg_srsw_inline"
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
check "NEG guards / :boot, no program directory: the agent's load is refused for the guard" "Agent alice failed to initialize: \[semantic\] \"true\" is not a guard - it can only appear in body position at Line 17" "$neg_guard_inline"
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
play_unfriend_send(alice, bob).
:quit
HEREDOC
2>&1)
check "SG unfriend_send succeeds" "succeeds\|suspended" "$g_uf_send"
check "SG unfriend_send teardown on friend channel" "msg(alice, bob, unfriend)" "$g_uf_send"

g_uf_recv=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_unfriend_recv(alice, bob).
:quit
HEREDOC
2>&1)
check "SG unfriend_recv succeeds" "succeeds\|suspended" "$g_uf_recv"
check "SG unfriend_recv notifies user" "unfriended(alice)" "$g_uf_recv"

g_uf_abs=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_unfriend_absent(alice, bob, carol).
:quit
HEREDOC
2>&1)
check "SG unfriend_absent no-op succeeds" "succeeds\|suspended" "$g_uf_abs"

# Introduction via a mutual friend (paper Section 5.2): "The mutual friend
# vouches for the identities of both, having a friend channel to each", so the
# introduction proceeds only where both introduced parties have a friend output
# entry.  The test is a procedure over the outputs list and GLP admits no
# procedure in a guard (Section 6.3), so it is a body goal and the clause
# dispatches on its answers.  The site is the person-posted introduce/2 (the
# warm call's request_intro is gone with the warm call, GLP #13, Code_inbox.md
# 2026-11-07 22:09 UTC).  Where either party is not a friend nothing is done
# beyond not introducing --- the paper says no more, so the code decides no
# more.
echo "--- Introduction: both parties must be friends (play_introduce_*) ---"

g_intro=$("$REPL_RUN" <<HEREDOC
$SGSIM
play_introduce_both(alice, bob, carol).
play_introduce_one(alice, bob, carol).
play_introduce_other(alice, bob, carol).
play_introduce_neither(alice, bob, carol, dave).
:quit
HEREDOC
2>&1)
check "SG introduce: both friends --- the intro reaches P" "intro_sent(to_bob, alice, bob, carol)" "$g_intro"
check "SG introduce: both friends --- the intro reaches Q" "intro_sent(to_carol, alice, carol, bob)" "$g_intro"
check "SG introduce: only P a friend --- nothing on P's channel" "no_intro(to_bob)" "$g_intro"
check "SG introduce: only Q a friend --- nothing on Q's channel" "no_intro(to_carol)" "$g_intro"
check "SG introduce: neither a friend --- nothing on any channel" "no_intro(to_dave)" "$g_intro"
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
# Section K: the child-safe social network (programs/cssn), CSSN Section 4:
# its program is the canonical CVA platform of its contract, programs/cssn/
# CSSN.glp (CSSN Appendix B.2), which vGLP's compiler prints, platform.vglp
# (programs/cssn/compile.sh), wrapped as a mini-app of the Grassroots
# Super-App by the entry miniapp/4 (miniapp.glp, the same file as the apps'),
# certified; and the village of CSSN Section 4.6, "Village Demonstration"
# (play_village, plays.glp):
# seven executions, each with a conversation with each of the six others, the
# persons scripted (person.glp), a parent looking at the phones of their
# children, since a parent's state does not change by the child's acts.  Each
# check of the village is of an act as that section states it, read off the
# screens the persons report: did(R) a request, gave(K, yes) an approval, and
# the views, view(S, K), the machine state S and the persons K the agent knows.
# =============================================================================
echo "=== Section K: CSSN, the child-safe social network ==="
echo ""

CSSN_DIR="$GLP_DIR/programs/cssn"

# The contract's act schemas (CSSN Appendix C.2), programs/cssn/CSSN.glp, in
# the term form programs/gsc reads (CSSN.glp at a261cb74, its comments at
# 89e11f96, CSSN #9 Cowork): GSC's verdict on it, by the conditions for a
# contract with wards (GAI ddf0a33, sections/wards.tex; CSSN, Appendix B.4):
# "The marks are child, its guardianship (child, identity, parent,
# parenting), its leaving mark retired, the introductory act acquaint, which
# adds knows, and the relay acts child_acquaint and child_acquaint_2, by which GSC's
# checker certifies the contract syntactically grassroots with wards"
# (CSSN.glp), with traceable provenance and volition.  The platform committed
# in programs/cssn is what the compiler prints from it (compile.sh --check)
# and the platform of the record of the cascade on CSSN.
echo "--- CSSN contract, GSC's verdict, the platform ---"
k_contract=$(bash "$CSSN_DIR/compile.sh" --check 2>&1; echo "exit $?")
check "CSSN contract: GSC gives a verdict" "verdict: " "$k_contract"
check "CSSN contract: syntactically grassroots with wards, acquaint the introductory act, child the mark, its guardianship" "verdict: syntactically_grassroots_with_wards(acquaint, \[child\], \[guardianship(child, identity, creator, created, parenting)\], " "$k_contract"
check "CSSN contract: ... child_acquaint a relay act" "verdict: syntactically_grassroots_with_wards(.*relay(child_acquaint, " "$k_contract"
check "CSSN contract: ... child_acquaint_2 a relay act" "verdict: syntactically_grassroots_with_wards(.*relay(child_acquaint_2, " "$k_contract"
check "CSSN contract: ... retired the leaving mark" "verdict: syntactically_grassroots_with_wards(.*, \[retired\])$" "$k_contract"
check_not "CSSN contract: its predicates have traceable provenance" "untraceable(" "$k_contract"
check_not "CSSN contract: it satisfies volition" "volition(" "$k_contract"
check_not "CSSN contract: it is not malformed" "malformed(" "$k_contract"
# The canonical CVA platform of the contract (vGLP a890465, Section 5.4, "Act schemas as local specifications": "the listed roles of a schema have one base, which is its only guarding role or, if it is not a guarding role, its first role"): child_close(r, p?, r[listed(*, n)] ; n), its listed role based at r, its first role, which does not guard, compiles, r the opener of the part p guards (vGLP #16 Cowork to Code, 2026-10-19 15:57 UTC, items 1 and 5).
check "CSSN contract: compiled to its canonical CVA platform, platform.vglp as the compiler prints it" "^platform.vglp: as printed$" "$k_contract"
check "CSSN contract: compile.sh --check exits 0" "^exit 0$" "$k_contract"
# CSSN, Section 4.6.1, "The program": compiled "with knows its introduction predicate" (CSSN.glp, introduction_named(cssn, introduction(knows)), after GKK).
check "CSSN contract: compiled with knows its introduction predicate" "^introduction: introduction(knows)$" "$k_contract"
if cmp -s "$CSSN_DIR/platform.vglp" "$GLP_DIR/cascade/CSSN/platform.vglp"; then
    _check_pass "CSSN: platform.vglp is the platform of the recorded run of the cascade on CSSN"
else
    _check_fail "CSSN: platform.vglp is the platform of the recorded run of the cascade on CSSN" "programs/cssn/platform.vglp differs from cascade/CSSN/platform.vglp"
fi
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$CSSN_DIR/miniapp.glp"; then
    _check_pass "CSSN: miniapp.glp is the same file as the apps'"
else
    _check_fail "CSSN: miniapp.glp is the same file as the apps'" "programs/cssn/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi

# The certified program: the platform, its entry and its plays reach neither the network nor the person.
echo "--- CSSN program ---"
K_ART=$(mktemp -d)
k_load=$("$REPL_RUN" <<HEREDOC
:artefact $CSSN_DIR $K_ART
$CSSN_DIR
:quit
HEREDOC
2>&1)
rm -rf "$K_ART"
check "CSSN program: the mini-app's artefact is certified" "cssn.glpw --- certified under" "$k_load"
check "CSSN program loads" "Loaded program: .*programs/cssn$" "$k_load"
check_not "CSSN program no type errors" "Type checking failed" "$k_load"
check_not "CSSN program is certified" "CERTIFICATE REFUSED" "$k_load"

# The village (CSSN, Section 4.6, "Village Demonstration"): one run, seven screens, each a line.
echo "--- CSSN village, eight acts, alice's closing of kids_chat among them ---"
k_v=$("$REPL_RUN" <<HEREDOC 2>&1
$CSSN_DIR
:limit 100000000
play_village(A, B, G, F, C, D, E).
:quit
HEREDOC
)
k_screen() { echo "$k_v" | sed -e 's/^\(GLP> \)*//' | grep "^$1 = " | head -1; }
k_last() { k_screen "$1" | sed -e 's/.*view(\[//' -e 's/\], \[.*//'; }
k_shows() {   # k_shows <name> <agent's variable> <text on the screen>
    if k_screen "$2" | grep -qF -- "$3"; then _check_pass "$1"; else _check_fail "$1" "$2's screen lacks: $3"; fi
}
k_holds() {   # k_holds <name> <agent's variable> <atom in the last view>
    if k_last "$2" | grep -qF -- "$3"; then _check_pass "$1"; else _check_fail "$1" "$2's last view lacks: $3"; fi
}
k_lacks() {   # k_lacks <name> <agent's variable> <atom an earlier view held and the last view does not>
    if k_screen "$2" | grep -qF -- "$3" && ! k_last "$2" | grep -qF -- "$3"; then _check_pass "$1"; else _check_fail "$1" "$2's screen never showed, or its last view still holds: $3"; fi
}
k_never() {   # k_never <name> <agent's variable> <text no line of the screen shows>
    if [ -n "$(k_screen "$2")" ] && ! k_screen "$2" | grep -qF -- "$3"; then _check_pass "$1"; else _check_fail "$1" "$2's screen shows: $3"; fi
}
K_K="speech(utterance(kids_chat), carol)"
K_S="speech(utterance(study), frank)"
check_not "CSSN village: no failed goal" "→ failed\|ERROR\|Error" "$k_v"
check "CSSN village: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$k_v"
check_not "CSSN village: ... and is not stopped at the limit" "capped" "$k_v"
check_not "CSSN village: no attempt refused, refused(P, Q, D) the notice (vGLP f0acbc1)" "refused(" "$k_v"
# CSSN extends GSN, not GSNP (CSSN.md; Udi, 2026-10-20): a message is content received from a friend, received(x, p), or delivered in a group by its leader, received_in(x, a, n), and nobody signs.
check_not "CSSN village: nobody signs, and no item or authored atom is written (CSSN extends GSN)" "propose(sign(\|item(\|authored(" "$k_v"
# Act 1, coming into the contract (GKK:1, GKK:2; GnW:1, CSSN:1): the adults establish their identities and come to know each other, Alice and Bob, Bob and Frank, and Bob and Grace; Bob enters Dave and Eve, each child's phone answering, and makes Grace a parent of each, Grace and the child approving (make_parent(p?, q?, r?)); Alice enters Carol (CSSN, Section 4.6.3, act 1; CSSN #10 Cowork, Code_inbox 2026-10-24 17:29 UTC, item 2).
k_shows "CSSN village act 1: alice, bob, grace and frank establish their identities" F "did(propose(establish(frank)))"
k_shows "CSSN village act 1: alice and bob come to know each other (GKK:2), alice requesting" A "did(propose(acquaint(alice, bob)))"
k_shows "CSSN village act 1: ... and bob approving" B "gave(acquaint(alice, bob), yes)"
k_shows "CSSN village act 1: bob and frank come to know each other, frank approving" F "gave(acquaint(bob, frank), yes)"
k_shows "CSSN village act 1: bob and grace come to know each other, grace approving" G "gave(acquaint(bob, grace), yes)"
k_shows "CSSN village act 1: carol enters with her creator alice, at alice" A "view([created(carol), parenting(carol), friend(carol), knows(carol), identity]"
k_shows "CSSN village act 1: ... and at carol, her phone answering yes" C "gave(enter(alice, carol), yes)"
k_shows "CSSN village act 1: ... carol a child, alice her creator and first friend" C "view([identity, child, creator(alice), friend(alice), knows(alice)]"
k_shows "CSSN village act 1: bob enters dave" B "did(propose(enter(bob, dave)))"
k_shows "CSSN village act 1: ... dave's phone answering yes, bob his creator" D "view([identity, child, creator(bob), friend(bob), knows(bob)]"
k_shows "CSSN village act 1: bob makes grace a parent of dave (CSSN:1)" B "did(propose(make_parent(bob, grace, dave)))"
k_shows "CSSN village act 1: ... grace approving" G "gave(make_parent(bob, grace, dave), yes)"
k_shows "CSSN village act 1: bob enters eve" E "gave(enter(bob, eve), yes)"
k_shows "CSSN village act 1: bob makes grace a parent of eve" B "did(propose(make_parent(bob, grace, eve)))"
k_shows "CSSN village act 1: ... grace approving" G "gave(make_parent(bob, grace, eve), yes)"
k_holds "CSSN village act 2: dave records grace a parent bob made" D "guarded_by(grace)"
k_holds "CSSN village act 1: grace is parenting dave" G "parenting(dave)"
k_holds "CSSN village act 1: dave knows grace" D "knows(grace)"
k_never "CSSN village act 1: dave records only his creator" D "creator(grace)"
# Act 2, adult friendships (GSG:1), a direct message (GSN:1) and the introductions (GKK:3): Bob introduces Frank to Alice, an act of Bob and Alice, and Alice to Frank, an act of Bob and Frank, and Alice befriends Frank (CSSN, Section 4.6.3, act 2).
k_holds "CSSN village act 2: alice and bob are friends" B "friend(alice)"
k_shows "CSSN village act 2: alice sends bob a message, which he holds as received from her" B "received(speech(utterance('Hello Bob'), alice), alice)"
k_holds "CSSN village act 2: bob and frank are friends" F "friend(bob)"
k_shows "CSSN village act 2: bob introduces frank to alice (GKK:3)" B "did(propose(introduce(bob, alice, frank)))"
k_shows "CSSN village act 2: ... an act of bob and alice, alice approving" A "gave(introduce(bob, alice, frank), yes)"
k_holds "CSSN village act 2: ... after which alice knows frank" A "knows(frank)"
k_shows "CSSN village act 2: bob introduces alice to frank" B "did(propose(introduce(bob, frank, alice)))"
k_shows "CSSN village act 2: ... an act of bob and frank, frank approving" F "gave(introduce(bob, frank, alice), yes)"
k_holds "CSSN village act 2: ... after which frank knows alice" F "knows(alice)"
k_shows "CSSN village act 2: alice befriends frank" A "did(propose(befriend(alice, frank)))"
k_holds "CSSN village act 2: ... and alice and frank are friends" F "friend(alice)"
# Act 3, child acquaintances (GnW:2, CSSN:2; child_acquaint_2(r?, s?, p?, q?)) and child friendships (GnW:2, CSSN:3; child_befriend(r?, s?, p?, q?)).
k_shows "CSSN village act 3: alice requests that carol and dave come to know each other, naming bob" A "did(propose(child_acquaint_2(carol, dave, alice, bob)))"
k_shows "CSSN village act 3: ... and bob approves" B "gave(child_acquaint_2(carol, dave, alice, bob), yes)"
k_shows "CSSN village act 3: ... and carol" C "gave(child_acquaint_2(carol, dave, alice, bob), yes)"
k_shows "CSSN village act 3: ... and dave" D "gave(child_acquaint_2(carol, dave, alice, bob), yes)"
k_holds "CSSN village act 3: dave knows carol by their child acquaintance" D "knows(carol)"
k_shows "CSSN village act 3: alice then proposes that carol and dave befriend, naming bob" A "did(propose(child_befriend(carol, dave, alice, bob)))"
k_shows "CSSN village act 3: ... and bob approves" B "gave(child_befriend(carol, dave, alice, bob), yes)"
k_shows "CSSN village act 3: carol and dave are friends" C "friend(dave)"
k_shows "CSSN village act 3: the sibling case, bob both parents named, dave and eve coming to know each other" D "gave(child_acquaint_2(dave, eve, bob, bob), yes)"
k_shows "CSSN village act 3: ... and then befriending" D "gave(child_befriend(dave, eve, bob, bob), yes)"
k_holds "CSSN village act 3: ... dave and eve are friends" E "friend(dave)"
k_shows "CSSN village act 3: alice requests that carol and eve come to know each other, naming bob" A "did(propose(child_acquaint_2(carol, eve, alice, bob)))"
k_shows "CSSN village act 3: ... then that they befriend" A "did(propose(child_befriend(carol, eve, alice, bob)))"
k_holds "CSSN village act 3: ... and carol and eve are friends" E "friend(carol)"
k_shows "CSSN village act 3: alice knows dave by their child acquaintance, and nothing more of him" A "knows(dave)"
# Act 4, child-adult acquaintances (CSSN:2; child_acquaint(r?, a?, p?)) and friendships (CSSN:3; child_adult_befriend(r?, a?, p?)).
k_shows "CSSN village act 4: bob requests that dave and frank come to know each other" B "did(propose(child_acquaint(dave, frank, bob)))"
k_shows "CSSN village act 4: ... frank approving" F "gave(child_acquaint(dave, frank, bob), yes)"
k_shows "CSSN village act 4: ... then that they befriend" B "did(propose(child_adult_befriend(dave, frank, bob)))"
k_shows "CSSN village act 4: alice requests that carol and frank come to know each other" A "did(propose(child_acquaint(carol, frank, alice)))"
k_shows "CSSN village act 4: ... carol approving" C "gave(child_acquaint(carol, frank, alice), yes)"
k_shows "CSSN village act 4: ... then that they befriend" A "did(propose(child_adult_befriend(carol, frank, alice)))"
k_holds "CSSN village act 4: frank and dave are friends" F "friend(dave)"
k_holds "CSSN village act 4: ... and frank and carol" F "friend(carol)"
# Act 5, a child leader's group (GSN:2 to GSN:5, GnW:2, CSSN:6).
k_shows "CSSN village act 5: carol creates kids_chat" C "member(dot(community(carol), $K_K))"
k_shows "CSSN village act 5: carol invites dave, alice, bob and dave approving" B "gave(child_join_2(carol, dave, alice, bob, $K_K), yes)"
k_shows "CSSN village act 5: ... and eve" C "listed(eve, $K_K)"
k_shows "CSSN village act 5: carol's own post, delivered to every member" E "received_in(speech(utterance('Welcome everyone!'), carol), carol, $K_K)"
k_shows "CSSN village act 5: dave's post, sent to carol and delivered by her" E "received_in(speech(utterance('Thanks Carol!'), dave), carol, $K_K)"
k_shows "CSSN village act 5: eve's post, delivered to dave" D "received_in(speech(utterance('Hi all!'), eve), carol, $K_K)"
k_lacks "CSSN village act 5: eve leaves kids_chat" E "member(dot(community(carol), $K_K))"
# Act 6, an adult leader's group, each child joining with its parent's approval.
k_shows "CSSN village act 6: frank invites dave, bob approving" B "gave(child_join(frank, dave, bob, $K_S), yes)"
k_shows "CSSN village act 6: ... and carol, alice approving" A "gave(child_join(frank, carol, alice, $K_S), yes)"
k_shows "CSSN village act 6: frank's post, delivered to dave" D "received_in(speech(utterance('Welcome to the study group'), frank), frank, $K_S)"
k_shows "CSSN village act 6: dave's post, delivered to carol by frank" C "received_in(speech(utterance('Hi from Dave!'), dave), frank, $K_S)"
k_shows "CSSN village act 6: carol's post, delivered to dave by frank" D "received_in(speech(utterance('Hi from Carol!'), carol), frank, $K_S)"
# Act 7, parents ending (GnW:3).
k_shows "CSSN village act 7: grace ends dave's friendship with carol" G "did(propose(child_unfriend(dave, carol, grace)))"
k_lacks "CSSN village act 7: ... at dave" D "friend(carol)"
k_lacks "CSSN village act 7: ... and at carol" C "friend(dave)"
k_lacks "CSSN village act 7: carol's agent drops dave from kids_chat, his leader no longer his friend (GSN:4)" D "member(dot(community(carol), $K_K))"
k_shows "CSSN village act 7: bob ends dave's membership of the study group" B "did(propose(child_leave(frank, dave, bob, $K_S)))"
k_lacks "CSSN village act 7: ... at dave" D "member(dot(community(frank), $K_S))"
k_lacks "CSSN village act 7: ... and at frank" F "listed(dave, $K_S)"
# Alice closes kids_chat (CSSN, Section 4.6, "Village Demonstration", act 7): child_close(r, p?, r[listed(*, n)] ; n), a guarded part whose opener is the child, r, and its guard the parent, p.  Alice's request is a Request of rho_N to carol's machine, which holds it, completes the list from its listed atoms and proposes the close, naming the request; alice's machine votes yes on it unasked, and her person is never asked (vGLP f0acbc1, Section 3.2, Definitions "Local Specification, Part, Guard Set, Opener, Request" to "Deciding"; Section 5.4, "Triggers" and "The compilation").
k_shows "CSSN village act 7: alice requests that kids_chat be closed, child_close(carol, alice, n), its parts opened by carol" A "did(propose(child_close(carol, alice, $K_K)))"
k_holds "CSSN village act 7: ... carol's machine proposes it on alice's request, and kids_chat is closed at carol" C "closed($K_K)"
k_lacks "CSSN village act 7: ... carol's membership and her listing of herself ended in the one act" C "listed(carol, $K_K)"
k_never "CSSN village act 7: ... alice's person is never asked on the close, her request her approval" A "gave(child_close"
# Act 8, coming of age (CSSN:4): Eve, an adult, "meets Frank and befriends him, with no parent among the guards" (CSSN, Section 4.6.3, act 8).
k_shows "CSSN village act 8: bob, eve's creator, declares eve of age" B "did(propose(of_age(eve, bob)))"
k_never "CSSN village act 8: ... and nobody is asked, grace not" G "gave(of_age"
k_lacks "CSSN village act 8: which ends bob's parenting of eve" B "parenting(eve)"
k_lacks "CSSN village act 8: ... and his creating" B "created(eve)"
k_lacks "CSSN village act 8: ... and grace's parenting of eve, the parent bob made" G "parenting(eve)"
k_holds "CSSN village act 8: ... grace's parenting of dave remaining" G "parenting(dave)"
k_lacks "CSSN village act 8: eve is an adult" E "child"
k_lacks "CSSN village act 8: ... with no creator" E "creator(bob)"
k_shows "CSSN village act 8: eve comes to know frank (GKK:2), with no parent among the guards" E "did(propose(acquaint(eve, frank)))"
k_shows "CSSN village act 8: ... frank approving" F "gave(acquaint(eve, frank), yes)"
k_holds "CSSN village act 8: ... after which eve knows frank" E "knows(frank)"
k_shows "CSSN village act 8: eve befriends frank" E "did(propose(befriend(eve, frank)))"
k_holds "CSSN village act 8: ... and eve and frank are friends, with no parent among the guards" E "friend(frank)"
k_holds "CSSN village act 8: eve and bob remain friends" E "friend(bob)"
k_never "CSSN village: a parent's state does not change by the child's acts" A "friend(dave)"

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
# the mini-app entry coins/3 and the mini-app's own three plays --- and
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

echo "--- The coins mini-app's own plays (play_mint, play_swap, play_same) ---"

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

# A request that changes no state is not a transaction (GC, Definition 3.1:
# Redeem with r != q, Swap with u != v or k != k'): a redemption of a bob-coin
# against a bob-coin, and a swap of a lot for itself, are refused at the
# bearer before anything is sent, the screen saying so; the holdings stay,
# and the counterparty's screen shows nothing of either.
n2_same=$("$REPL_RUN" <<HEREDOC
$COINS/currency
:limit 5000000
play_same(A, B).
:quit
HEREDOC
2>&1)

check_not "Coins mini-app play_same: no failed goal" "ERROR" "$n2_same"
check "Coins mini-app play_same: the bearer's screen refuses the redemption of a coin against a coin of its own issuer, redeem_same, and the swap of a lot for itself, swap_same, and her holdings stay" "A = \[opened(bob), minted(2), holdings(\[lot(alice, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 2)\]), redeem_same(bob), swap_same(bob, bob, 2) | X[0-9]*\]$" "$n2_same"
check "Coins mini-app play_same: nothing is sent, the counterparty's screen showing nothing of either" "B = \[opened(alice), minted(2), holdings(\[lot(bob, 2)\]), swap_done(alice), holdings(\[lot(alice, 2)\]) | X[0-9]*\]$" "$n2_same"
echo ""


# the coins program's play_swap on two smartphones (programs/currencies/coins_swap_boot.glp): the super-app of programs/social/graph/core on two isolates, each person with its own key pair, hosting the coins mini-app from its certified artefact currency.glpw, which :artefact writes beside the super-app and load_file/2 reads there; alice connects to bob, who consents, and once connected invites him to the mini-app, which he has consented to in advance; the handshake opens their conversation, and each person's script, waiting for it to open, then runs play_swap's steps over it.  Each person's lines, play_swap's screen, are held exactly, in order (Currencies #15, Code_inbox 2026-11-05 07:26 UTC, Udi approving: the plays' persons as mini-apps the super-app hosts).
n2_coins_swap=$("$REPL_RUN" <<HEREDOC
:artefact $COINS/currency $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/coins_swap_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/currency.glpw"
check "N2 coins swap on smartphones: the mini-app's artefact is certified" "currency.glpw --- certified under" "$n2_coins_swap"
check "N2 coins swap on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n2_coins_swap"
check_not "N2 coins swap on smartphones: no event stopped at the cap" "did not quiesce" "$n2_coins_swap"
boot_same "N2 coins swap on smartphones: alice's lines" "[opened(bob), minted(2), holdings([lot(alice, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, 2)])]" "$(boot_person alice "$n2_coins_swap")"
boot_same "N2 coins swap on smartphones: bob's lines" "[opened(alice), minted(2), holdings([lot(bob, 2)]), swap_done(alice), holdings([lot(alice, 2)])]" "$(boot_person bob "$n2_coins_swap")"

# the coins program's play_same on two smartphones (coins_same_boot.glp): play_swap, after which alice asks for the two acts that change no state, each refused at her before anything is sent, her screen saying so and bob's showing nothing of either.
n2_coins_same=$("$REPL_RUN" <<HEREDOC
:artefact $COINS/currency $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/coins_same_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/currency.glpw"
check "N2 coins same on smartphones: the mini-app's artefact is certified" "currency.glpw --- certified under" "$n2_coins_same"
check "N2 coins same on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n2_coins_same"
check_not "N2 coins same on smartphones: no event stopped at the cap" "did not quiesce" "$n2_coins_same"
boot_same "N2 coins same on smartphones: alice's lines" "[opened(bob), minted(2), holdings([lot(alice, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, 2)]), redeem_same(bob), swap_same(bob, bob, 2)]" "$(boot_person alice "$n2_coins_same")"
boot_same "N2 coins same on smartphones: bob's lines" "[opened(alice), minted(2), holdings([lot(bob, 2)]), swap_done(alice), holdings([lot(alice, 2)])]" "$(boot_person bob "$n2_coins_same")"

# the coins mini-app of coins/play_ui.glp on two smartphones (coins_ui_boot.glp), the live person scripted: alice mints 2 and proposes to bob the swap of her 2 for his 2; bob mints 2, accepts, and proposes the reverse swap, his 2 alice-coins for her 2 bob-coins, which alice accepts, as the harness describes her acts.
n2_coins_ui=$("$REPL_RUN" <<HEREDOC
:artefact $COINS/currency $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/coins_ui_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/currency.glpw"
check "N2 coins ui on smartphones: the mini-app's artefact is certified" "currency.glpw --- certified under" "$n2_coins_ui"
check "N2 coins ui on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n2_coins_ui"
check_not "N2 coins ui on smartphones: no event stopped at the cap" "did not quiesce" "$n2_coins_ui"
boot_same "N2 coins ui on smartphones: alice's lines" "[opened(bob), minted(2), holdings([lot(alice, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, 2)]), swap_done(bob), holdings([lot(alice, 2)])]" "$(boot_person alice "$n2_coins_ui")"
boot_same "N2 coins ui on smartphones: bob's lines" "[opened(alice), minted(2), holdings([lot(bob, 2)]), swap_done(alice), holdings([lot(alice, 2)]), holdings([]), swap_done(alice), holdings([lot(bob, 2)])]" "$(boot_person bob "$n2_coins_ui")"

fi  # Section N2

if _section N3; then
# =============================================================================
# Section N3: The bonds program (programs/currencies/bonds): the vGLP bond agent of the
# Grassroots Bonds paper as a mini-app of the Grassroots Super-App.
# programs/currencies/bonds/dated is the certified program --- the agent
# bonds_agent.vglp, its canonical compilation bonds_agent.glp (emitted by glpc
# :emit; the .vglp beside it is skipped by the loader while the .glp stands),
# the mini-app entry bonds/3, the scripted person with the escrow programs it
# answers by, and the mini-app's own three plays --- and programs/currencies/bonds
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

# The agent's initial goal, agent(ch(_?,_),ch(_?,_)) (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP, Implementation Notes, "Boot"), answered by dated's own agent/2, as coins and sovereign answer it: the person's first act names the entry the plays start, bonds(Me), and the mini-app's draws go out on the agent's person channel (Currencies #15 Cowork, Code_inbox.md, 2026-11-04 16:10 UTC, item 3).
n3_agent=$("$REPL_RUN" <<HEREDOC
$BONDSD
:limit 2000000
agent(ch([bonds(alice)], O), ch(N?, X)).
:quit
HEREDOC
2>&1)
check "Bonds mini-app agent/2: the first act bonds(alice) starts the mini-app, its screen drawn on the person channel" "O = \[draw(0, thread(menu(" "$n3_agent"
check "Bonds mini-app agent/2: ... and its request form drawn, minting first" "draw(1, input_box(menu(\[form(mint, " "$n3_agent"
check_not "Bonds mini-app agent/2: no failed goal" "→ failed\|Error" "$n3_agent"

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
check "Bonds village: Frank's holdings" "tagged(frank, holdings(\[lot(diana, now, 7), lot(eve, now, 10), lot(frank, 28, 6), lot(frank, now, 4)\]))" "$n3_run"
check "Bonds village: the escrow agent holds nothing at the end" "tagged(escrow, holdings(\[\]))" "$n3_run"
check "Bonds village: a bond presented before its issuer's date reaches its maturity is returned by the issuer" "tagged(frank, not_mature_for(diana, 28))" "$n3_run"
check "Bonds village: and the bearer holds it back" "tagged(diana, not_mature(frank, 28))" "$n3_run"
check "Bonds village: Bob's bonds mature by his own calendar" "tagged(bob, date_advanced(25))" "$n3_run"
check "Bonds village: Advance is the next day, so the day before is logged too" "tagged(bob, date_advanced(24))" "$n3_run"
check "Bonds village: the redemption takes back an immature bond, the bearer's screen naming the coin presented and the bond taken" "tagged(frank, redeemed(diana, now, frank, 28))" "$n3_run"
check "Bonds village: ... and the issuer's screen naming them too" "tagged(diana, presented(frank, now, frank, 28))" "$n3_run"
check "Bonds village: once mature by its issuer's date, the bond is redeemed as a coin is, against a coin its issuer holds (GB:4), the bearer's screen naming the bond presented, of day 28, and the coin taken" "tagged(diana, redeemed(frank, 28, frank, now))" "$n3_run"
check "Bonds village: ... the issuer taking the matured bond back and giving the coin, its screen naming both" "tagged(frank, presented(diana, 28, frank, now))" "$n3_run"
check_not "Bonds village: no redemption message without the due of the instrument presented" "redeemed([a-z]*, [a-z]*, [a-z0-9]*))\|presented([a-z]*, [a-z]*, [a-z0-9]*))" "$n3_run"
check_not "Bonds village: no Mature act, a matured bond being redeemed (GB:4), so no matured( line is logged" "matured(\|matured_for(" "$n3_run"
check_not "Bonds village: Redeem records no date warranty, so no warranty( line is logged" "warranty(" "$n3_run"
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

echo "--- The bonds mini-app's own plays (play_mint, play_swap, play_same) ---"

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

# A request that changes no state determines no transaction (GB, Definition
# def:gb-transactions and Appendix A): a redemption of a bond against a bond
# of its own issuer and due, and a swap of a lot for the same lot, are refused
# at the bearer before anything is sent, the screen saying so; the holdings
# stay, and the counterparty's screen shows nothing of either.
n3_same=$("$REPL_RUN" <<HEREDOC
$BONDSD
:limit 5000000
play_same(A, B).
:quit
HEREDOC
2>&1)

check_not "Bonds mini-app play_same: no failed goal" "ERROR" "$n3_same"
check "Bonds mini-app play_same: the bearer's screen refuses the redemption of a bond against a bond of its own issuer and due, redeem_same, and the swap of a lot for the same lot, swap_same, and her holdings stay" "A = \[opened(bob), minted(2, now), holdings(\[lot(alice, now, 2)\]), holdings(\[\]), swap_done(bob), holdings(\[lot(bob, 30, 2)\]), redeem_same(bob, 30), swap_same(bob, bob, 30, 2) | X[0-9]*\]$" "$n3_same"
check "Bonds mini-app play_same: nothing is sent, the counterparty's screen showing nothing of either" "B = \[opened(alice), minted(2, 30), holdings(\[lot(bob, 30, 2)\]), swap_done(alice), holdings(\[lot(alice, now, 2)\]) | X[0-9]*\]$" "$n3_same"
echo ""


# the bonds program's play_swap on two smartphones (programs/currencies/dated_swap_boot.glp), as the coins program's in Section N2: the super-app on two isolates hosting the bonds mini-app from its certified artefact dated.glpw; alice mints 2 coins and bob 2 bonds maturing day 30, alice offers her 2 coins for bob's 2 bonds and bob accepts, each person's lines play_swap's screen, held exactly, in order (Currencies #15, Code_inbox 2026-11-05 07:26 UTC).
n3_dated_swap=$("$REPL_RUN" <<HEREDOC
:artefact $BONDS/dated $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/dated_swap_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/dated.glpw"
check "N3 dated swap on smartphones: the mini-app's artefact is certified" "dated.glpw --- certified under" "$n3_dated_swap"
check "N3 dated swap on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n3_dated_swap"
check_not "N3 dated swap on smartphones: no event stopped at the cap" "did not quiesce" "$n3_dated_swap"
boot_same "N3 dated swap on smartphones: alice's lines" "[opened(bob), minted(2, now), holdings([lot(alice, now, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, 30, 2)])]" "$(boot_person alice "$n3_dated_swap")"
boot_same "N3 dated swap on smartphones: bob's lines" "[opened(alice), minted(2, 30), holdings([lot(bob, 30, 2)]), swap_done(alice), holdings([lot(alice, now, 2)])]" "$(boot_person bob "$n3_dated_swap")"

# the bonds program's play_same on two smartphones (dated_same_boot.glp): play_swap, after which alice asks for the two acts that change no state, each refused at her, her screen saying so.
n3_dated_same=$("$REPL_RUN" <<HEREDOC
:artefact $BONDS/dated $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/dated_same_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/dated.glpw"
check "N3 dated same on smartphones: the mini-app's artefact is certified" "dated.glpw --- certified under" "$n3_dated_same"
check "N3 dated same on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n3_dated_same"
check_not "N3 dated same on smartphones: no event stopped at the cap" "did not quiesce" "$n3_dated_same"
boot_same "N3 dated same on smartphones: alice's lines" "[opened(bob), minted(2, now), holdings([lot(alice, now, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, 30, 2)]), redeem_same(bob, 30), swap_same(bob, bob, 30, 2)]" "$(boot_person alice "$n3_dated_same")"
boot_same "N3 dated same on smartphones: bob's lines" "[opened(alice), minted(2, 30), holdings([lot(bob, 30, 2)]), swap_done(alice), holdings([lot(alice, now, 2)])]" "$(boot_person bob "$n3_dated_same")"

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

# The entry the agent's initial goal starts on the person's first act,
# sovereign_ui/4 (programs/currencies/sovereign/play_ui.glp), given the central
# bank's key and its two peers' by readable names (IGLP, Remark "Agent
# Names"), with no grant: its person
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
sovereign_ui(cb, [diana, frank], G?, []).
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


# the denominated program's play_swap on two smartphones (programs/currencies/sovereign_swap_boot.glp), as the coins program's in Section N2: the super-app on two isolates hosting the denominated mini-app from its certified artefact denominated.glpw; each mints 2 in usd, alice offers her 2 for bob's 2 and bob accepts, each person's lines play_swap's screen, held exactly, in order (Currencies #15, Code_inbox 2026-11-05 07:26 UTC).
n4_sovereign_swap=$("$REPL_RUN" <<HEREDOC
:artefact $SOV/denominated $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/sovereign_swap_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/denominated.glpw"
check "N4 sovereign swap on smartphones: the mini-app's artefact is certified" "denominated.glpw --- certified under" "$n4_sovereign_swap"
check "N4 sovereign swap on smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$n4_sovereign_swap"
check_not "N4 sovereign swap on smartphones: no event stopped at the cap" "did not quiesce" "$n4_sovereign_swap"
boot_same "N4 sovereign swap on smartphones: alice's lines" "[opened(bob), minted(2, now), holdings([lot(alice, usd, now, 2)]), holdings([]), swap_done(bob), holdings([lot(bob, usd, now, 2)])]" "$(boot_person alice "$n4_sovereign_swap")"
boot_same "N4 sovereign swap on smartphones: bob's lines" "[opened(alice), minted(2, now), holdings([lot(bob, usd, now, 2)]), swap_done(alice), holdings([lot(alice, usd, now, 2)])]" "$(boot_person bob "$n4_sovereign_swap")"

# the sovereign market of sovereign/play_ui.glp on three smartphones (sovereign_ui_boot.glp), the central bank scripted as play_sovereign_ui scripts it: cb, diana and frank over the three conversations their acts need, the central bank calling and inviting both and diana calling and inviting frank; each person's lines are play_sovereign_ui's for them, frank's without the date_advanced lines of the single-engine play, whose advance(28) stands in for the day, which on a phone the device gives (Currencies #15, Code_inbox 2026-11-05 07:26 UTC: under the boot every person's date advances alike with the day the device gives); each person's lines are held as a set, sorted, since IGLP's Contract makes delivery unordered and the program does not fix their order (Currencies #17, Code_inbox 2026-11-09 08:12 UTC, item 3; diana's so since her two conversations open in the order the isolates run).
n4_sovereign_ui=$("$REPL_RUN" <<HEREDOC
:artefact $SOV/denominated $GLP_DIR/programs/social/graph/core
:boot $GLP_DIR/programs/currencies/sovereign_ui_boot.glp $GLP_DIR/programs/social/graph/core
:quit
HEREDOC
2>&1)
rm -f "$GLP_DIR/programs/social/graph/core/denominated.glpw"
check "N4 sovereign ui on smartphones: the mini-app's artefact is certified" "denominated.glpw --- certified under" "$n4_sovereign_ui"
check "N4 sovereign ui on smartphones: the boot settles" "Boot settled: 3 agents (cb, diana, frank)" "$n4_sovereign_ui"
check_not "N4 sovereign ui on smartphones: no event stopped at the cap" "did not quiesce" "$n4_sovereign_ui"
boot_same "N4 sovereign ui on smartphones: cb's lines, sorted" "[fiat_handed(frank, 1), holdings([lot(cb, usd, now, 20), lot(diana, usd, now, 10)]), holdings([lot(cb, usd, now, 20)]), holdings([lot(cb, usd, now, 21), lot(diana, usd, now, 10)]), holdings([lot(cb, usd, now, 30), lot(diana, usd, now, 1)]), holdings([lot(cb, usd, now, 30)]), minted(30, now), opened(diana), opened(frank), received(frank, 1), swap_done(diana), swap_done(diana)]" "$(boot_person_sorted cb "$n4_sovereign_ui")"
boot_same "N4 sovereign ui on smartphones: frank's lines, sorted" "[holdings([lot(diana, usd, now, 10), lot(frank, usd, 28, 1)]), holdings([lot(diana, usd, now, 10), lot(frank, usd, 28, 2)]), holdings([lot(diana, usd, now, 11), lot(frank, usd, 28, 1)]), holdings([lot(diana, usd, now, 11)]), holdings([lot(diana, usd, now, 12)]), holdings([lot(diana, usd, now, 15)]), holdings([lot(diana, usd, now, 6), lot(frank, usd, 28, 5), lot(cb, usd, now, 1)]), holdings([lot(diana, usd, now, 6), lot(frank, usd, 28, 5)]), holdings([lot(diana, usd, now, 6), lot(frank, usd, 28, 5)]), holdings([lot(diana, usd, now, 7), lot(frank, usd, 28, 4)]), holdings([lot(diana, usd, now, 7), lot(frank, usd, 28, 5)]), holdings([lot(diana, usd, now, 8), lot(frank, usd, 28, 3)]), holdings([lot(diana, usd, now, 8), lot(frank, usd, 28, 4)]), holdings([lot(diana, usd, now, 9), lot(frank, usd, 28, 2)]), holdings([lot(diana, usd, now, 9), lot(frank, usd, 28, 3)]), holdings([lot(frank, usd, 28, 18)]), minted(18, 28), opened(cb), opened(diana), paid(cb, 1), paid(diana, 3), paying(cb, 1), paying(diana, 3), redeemed(diana, cb, now), redeemed(diana, frank, 28), redeemed(diana, frank, 28), redeemed(diana, frank, 28), redeemed(diana, frank, 28), redeemed(diana, frank, 28), swap_done(diana)]" "$(boot_person_sorted frank "$n4_sovereign_ui")"
boot_same "N4 sovereign ui on smartphones: diana's lines, sorted" "[holdings([lot(cb, usd, now, 10), lot(diana, usd, now, 15)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 13), lot(diana, usd, now, 8)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 14), lot(diana, usd, now, 7)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 15), lot(diana, usd, now, 6)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 16), lot(diana, usd, now, 5)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 17), lot(diana, usd, now, 4)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 18), lot(diana, usd, now, 3)]), holdings([lot(cb, usd, now, 10), lot(frank, usd, 28, 18)]), holdings([lot(cb, usd, now, 10)]), holdings([lot(cb, usd, now, 10)]), holdings([lot(cb, usd, now, 9), lot(frank, usd, 28, 13), lot(diana, usd, now, 9)]), holdings([lot(diana, usd, now, 10)]), holdings([lot(frank, usd, 28, 13), lot(diana, usd, now, 18)]), holdings([lot(frank, usd, 28, 13), lot(diana, usd, now, 9)]), minted(10, now), minted(15, now), opened(cb), opened(frank), presented(frank, cb, now), presented(frank, frank, 28), presented(frank, frank, 28), presented(frank, frank, 28), presented(frank, frank, 28), presented(frank, frank, 28), received(frank, 3), swap_done(cb), swap_done(cb), swap_done(frank)]" "$(boot_person_sorted diana "$n4_sovereign_ui")"

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

# A refresh, an Input addressed to no construct, on which the dispatcher has
# every construct, and construct 0, draw again what it last drew (vGLP
# d433f71, Section 6.3: "Input(T) ::= input(Integer, Path, T) ; refresh"; IGLP
# 7a25ff8, after Definition "Restart": the runtime grants it at a restart).
vg_refresh=$("$REPL_RUN" <<HEREDOC
$ONECLAUSE
play_refresh(D, L).
:quit
HEREDOC
2>&1)
check_not "One-clause: refresh, no failed goal" "ERROR" "$vg_refresh"
check "One-clause: refresh answers nothing, the answer after it reaching its clause" "D = \[decided(yes, bob)\]" "$vg_refresh"
check "One-clause: on refresh construct 0 and the card draw again what they last drew, and the card withdraws once answered" "L = \[drawn(0), drawn(1), drawn(0), drawn(1), withdrawn(1) |" "$vg_refresh"

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
# no tick, vGLP efe7bbc; the agent taking its own transactions one at a time, one on each own trigger it reads, which it writes on a stream of its own merged fairly with its other triggers, when it starts, after each other trigger it reads unless one it wrote is still unread, and after each own transaction it takes, vGLP 6c8c663).  GSC's social graph
# and currency (their owners', programs/gsc/GSG.glp and
# programs/currencies/coins/GC.glp) and vGLP's nudge, introduce, gnudge, childsafe, link, friends, waves,
# parts, opens, groups, counter, merges, coins, wardclose, numbers, bonds and intros (programs/vglp/cva/contracts.glp) compiled, the committed modules held to
# what the compiler prints; the contracts it refuses, not effective, and those it
# compiles whatever their guards; and the plays of each platform, two agents
# or three with scripted persons paced on their screens, each run on the
# committed module through the canonical compilation until it suspends, each
# agent having taken its own transactions until none holds and no trigger
# being left, the logs' tails unbound.  A play gives each person's log, the views its agent writes, view(S, K, A), the machine state, the persons it knows and the approvals its person made that are pending, the requests by their names and the answers yes by their attempts (vGLP 1423679, Section 5.4, "The compilation"), each once, with menu(K, Ks) where the person looks and gave(K, A) where the person answers an approval question (vGLP b7e3517, Section 5.4, "The compilation"); and the network's log of the cargoes it carries, sent(From, To, Cargo), where the attempts, their numbers and ranks, the votes and the outcomes show.  A person's request is its approval, which stands until the transaction is taken or refused: the agent proposes, and proposes again an attempt that yields, with no question to its person; a person whose approval ends by a refusal sees the notice, refused(P, Q, D), Q the refuser and D the reason, declined or unable, and may make the request again (vGLP f0acbc1, Section 3.2, "The protocol"; Section 5.4, "Triggers" and "The compilation"; programs/vglp/cva/self.glp).  A Request takes a number, as a Propose does, so each agent's attempts are numbered after its requests.
# -----------------------------------------------------------------------------
echo "--- The canonical CVA platform of an act-schema contract ---"
CVA="$GLP_DIR/programs/vglp/cva"

# Guarded: set -e would end the run without totals on a red compile.sh.
cva_print=$(bash "$CVA/compile.sh" --check 2>&1 || true)
check "CVA compiler: the social graph's platform is what the compiler prints" "social_graph/platform.vglp: as printed" "$cva_print"
check "CVA compiler: the social network's platform, GSC's GSN, close taking the listed role a[listed(*, n)], is what the compiler prints" "social_network/platform.vglp: as printed" "$cva_print"
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
check "CVA compiler: vGLP's counter, an unguarded count always open to its agent once begun, certified by GSC's check/2, compiled to the platform it prints (vGLP 6c8c663)" "counter/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's merges, merge taking two listed roles of one base, certified by GSC's check/2, compiled to the platform it prints (vGLP c649c0d)" "merges/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's coins, GC's mint, swap and pay with coins of one unit, certified by GSC's check/2, compiled to the platform it prints (vGLP f0acbc1)" "coins/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's wardclose, child_close taking a listed role whose base is not a guarding role, its guarded part opened by the child, certified by GSC's check/2, compiled to the platform it prints (vGLP f0acbc1)" "wardclose/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's numbers, act schemas with numbers, a multiplicity, a variable local to a role and a constraint checked by the role whose local variable it names, certified by GSC's check/2, compiled to the platform it prints (vGLP 613ee8d)" "numbers/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's bonds, GB's mint_bond, advance and redeem_bc with GKK's establish and acquaint, GB's mint and swap_cb, and dawn, certified by GSC's check/2, compiled to the platform it prints (vGLP 613ee8d)" "bonds/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's intros, GKK's acquaintance and introduction with no identity, its introduction at a commit, with peek's reading role and send's and credit's signing conditions (vGLP dd37d2a), certified by GSC's check/2, compiled to the platform it prints (vGLP 613ee8d)" "intros/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's days, an advance that requires the day, today(d), compiled to the platform it prints (vGLP 1423679)" "days/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's linear, constraints of a linear equality and an inequality in GSC's form, compiled to the platform it prints (GSC bcc1891)" "linear/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's forms, a safe formula in a schema's body local to its role, in GSC's term form qschema, compiled to the platform it prints (vGLP bcb2726)" "forms/platform.vglp: as printed" "$cva_print"
check "CVA compiler: vGLP's shares, a quantified conjunct whose instances the attempt names, certified by GSC's check/2, compiled to the platform it prints (vGLP 9ee6f87)" "shares/platform.vglp: as printed" "$cva_print"
check_not "CVA compiler: no contract refused or missing" "^fault\|^missing\|NOT as printed" "$cva_print"
# "The platform's own words are four ... Every other word the app shows is the contract's, so the compiler prints a platform only from a contract with pragmas, and refuses one with none; the checks that a contract is local, effective and grassroots are of its schemas and need none" (vGLP ffcb093, Section 5.4, "The contract's words"; vGLP #22 Cowork to Code, Code_inbox.md, 2026-10-31 12:55 UTC, item 3): each of vGLP's contracts carries a pragmas term beside it, pragmas_named/2, which compile.sh gives the compiler.
check "CVA compiler: each of vGLP's contracts carries pragmas, its tab and a button for each guarded schema, which its platform carries (nudge)" '^tabs(\[tab("Friends", friend)\])$' "$(cat "$CVA/nudge/platform.vglp")"
check "CVA compiler: ... and each button in its request widget, by its label (bonds, acquaint for GKK's meet, vGLP #22's task, item 2)" 'button("Acquaint", \[act(acquaint, ' "$(cat "$CVA/bonds/platform.vglp")"
check_not "CVA compiler: ... no schema meet left in vGLP's contracts (bonds, intros)" "schema(meet," "$(cat "$CVA/contracts.glp")"

# The fixtures below are checked, not printed: compile/3 gives the compiler no pragmas, so a fixture that passes the checks, its verdict, local and effective, is refused with the line saying so, "%% not compiled, having no pragmas(Name)", and one that fails a check is refused with the fault, "%% not compiled(Name)"; a fixture whose printed platform a check reads carries a minimal pragmas term, compile/5 (vGLP ffcb093, Section 5.4, "The contract's words"; vGLP #22 Cowork to Code, Code_inbox.md, 2026-10-31 12:55 UTC, item 3).
cva_refuse=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(two_reqs, [schema(s1, [], [rs(guarded, [atom(x, [])], [], [atom(r1, [])], []), rs(guarded, [atom(y, [])], [], [atom(r2, [])], [])]), schema(s2, [], [rs(guarded, [atom(x, [])], [], [atom(r3, [])], []), rs(guarded, [atom(y, [])], [], [atom(r4, [])], [])])], syntactically_grassroots).
compile(free_x, [schema(s, [pvar(x)], [rs(guarded, [atom(x1, [])], [], [atom(r, [pvar(x)])], []), rs(guarded, [atom(y1, [])], [], [atom(t, [pvar(x)])], [])])], syntactically_grassroots).
compile(uneff, [schema(u, [], [rs(unguarded, [atom(x, [role(2)])], [], [], []), rs(unguarded, [atom(y, [role(1)])], [], [], [])])], syntactically_grassroots).
compile(gossip, [], not_grassroots([untraceable([p])])).
compile(nointro, [], not_grassroots([no_introductory_act])).
compile(jointly, [], not_grassroots([obstructed_jointly(meet)])).
compile(nonlinear, [], malformed([constraint_not_linear(s, mul(num(x), num(y)))])).
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
compile(inert, [schema(s, [], [rs(guarded, [atom(x, [])], [], [], []), rs(unguarded, [], [], [], [])])], not_grassroots([no_introductory_act, volition(s, 1, 2), changes_nothing(s, 2)])).
compile(reading_ok, [schema(s, [], [rs(guarded, [atom(x, [])], [], [atom(k, [role(2)])], []), rs(unguarded, [], [], [atom(y, [])], [])])], not_grassroots([no_introductory_act])).
compile(unnamed_two, [schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])]), schema(link, [], [rs(guarded, [atom(linked, [role(2), role(3)])], [], [atom(friend, [role(2)]), atom(friend, [role(3)])], []), rs(unguarded, [atom(via, [role(1), role(3)])], [], [atom(friend, [role(1)])], []), rs(unguarded, [atom(via, [role(1), role(2)])], [], [atom(friend, [role(1)])], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Friends"', [friend])], [act(befriend, '"Befriend"', [], said([[], [person(1), text('" asks."')]]), '"Friends"', by([1])), act(link, '"Link"', [], none, '"Friends"', by([1]))])).
compile(unnamed_one, [schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])]), schema(link, [], [rs(guarded, [atom(linked, [role(2), role(3)])], [], [atom(friend, [role(2)]), atom(friend, [role(3)])], []), rs(unguarded, [atom(via, [role(1), role(3)])], [], [atom(friend, [role(1)])], []), rs(unguarded, [atom(via, [role(1), role(2)])], [], [atom(friend, [role(1)])], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Friends"', [friend])], [act(befriend, '"Befriend"', [], said([[], [person(1), text('" asks."')]]), '"Friends"', by([1])), act(link, '"Link"', ['""', '""', '"Person"'], none, '"Friends"', by([1]))])).
compile(group_row, [schema(create, [svar(n)], [rs(guarded, [atom(member, [nterm(ext(own(1), n))]), atom(listed, [role(1), svar(n)])], [], [], [atom(member, [nterm(ext(own(1), n))])])]), schema(leave, [svar(n)], [rs(unguarded, [], [atom(listed, [role(2), svar(n)])], [], []), rs(guarded, [], [atom(member, [nterm(ext(own(1), n))])], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Groups"', [member])], [act(create, '"Create"', ['""', '"Name"'], none, '"Groups"', by([1])), act(leave, '"Leave"', [], none, '"Groups"', by([2]))])).
compile(signing_con, [schema(acquaint, [], [rs(guarded, [atom(knows, [role(2)])], [], [], []), rs(guarded, [atom(knows, [role(1)])], [], [], [])]), schema(post, [svar(x)], [rs(guarded, [atom(item, [svar(x)])], [], [], [])]), nschema(send, [svar(x)], unsigned(role(2), svar(x)), [rs(unguarded, [atom(sent, [svar(x), role(2)])], [], [atom(item, [svar(x)]), atom(knows, [role(2)])], [atom(sent, [svar(x), role(2)])]), rs(unguarded, [atom(got, [svar(x)])], [], [], [])])], syntactically_grassroots).
compile(signing_bad, [], malformed([signing_not_agent(send, svar(x))])).
compile(headless, [], malformed([not_in_head(s, svar(x))])).
compile(listed_named, [cschema(cl, [svar(n)], [], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(wards_named, [cschema(cl, [svar(n)], [], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots_with_wards(enter, [m], [guardianship(m, i, g, w, w)], [relay(r, ward(1, m, guardian(2)), free(2))], [])).
compile(listed_two, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], []), crs(listed(1, invited, [svar(n)]), unguarded, [], [atom(guest, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_bases, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(friend, [role(2)])], []), rs(unguarded, [atom(seen, [svar(n)])], [], [atom(friend, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], []), crs(listed(2, invited, [svar(n)]), unguarded, [], [atom(guest, [role(2), svar(n)])], [], [])])], syntactically_grassroots).
compile(seated_role, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(seated(ext(own(1), n), member), unguarded, [], [atom(member, [nterm(ext(own(1), n))])], [], [])])], syntactically_grassroots).
compile(name_term, [schema(cr, [svar(n)], [rs(guarded, [atom(member, [nterm(ext(own(1), n))]), atom(led, [svar(n)])], [], [], [atom(member, [nterm(ext(own(1), n))])])])], syntactically_grassroots, no_introduction, pragmas([list('"Led"', [led])], [act(cr, '"Cr"', [], none, '"Led"', by([1]))])).
compile(name_var, [schema(cr, [svar(n)], [rs(guarded, [atom(member, [nterm(ext(nvar(z), n))]), atom(led, [svar(n)])], [], [], [])])], syntactically_grassroots).
compile(reached, [cschema(cl, [svar(n)], [reach(own(1), friend, own(2))], [rs(guarded, [atom(closed, [svar(n)])], [], [], []), rs(unguarded, [atom(seen, [svar(n)])], [], [], [])])], syntactically_grassroots).
compile(listed_guard, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_guard_second, [schema(cl, [svar(n)], [rs(guarded, [atom(seen, [svar(n)])], [], [atom(friend, [role(2)])], []), rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(2), svar(n)])], [atom(friend, [role(1)])], []), crs(listed(2, listed, [svar(n)]), unguarded, [], [atom(member, [role(2), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_two_guards, [schema(cl, [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_second, [schema(cl, [svar(n)], [rs(unguarded, [atom(seen, [svar(n)])], [], [atom(friend, [role(2)])], []), rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(2), svar(n)])], [atom(friend, [role(1)])], []), crs(listed(2, listed, [svar(n)]), unguarded, [], [atom(member, [role(2), svar(n)])], [], [])])], syntactically_grassroots).
compile(order, [schema(post, [svar(x), svar(n)], [rs(guarded, [], [], [atom(member, [role(2), svar(n)])], []), rs(unguarded, [atom(posted, [svar(x), svar(n)])], [], [atom(member, [role(2), svar(n)])], [atom(posted, [svar(x), svar(n)])])])], syntactically_grassroots, no_introduction, pragmas([list('"Posted"', [posted])], [act(post, '"Post"', ['""', '""', '"Text"', '"Group"'], none, '"Posted"', by([1]))])).
compile(listed_guard_two, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), rs(guarded, [], [], [atom(minding, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_unguarded, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_mixed, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])]), schema(cl2, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), rs(unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(listed_likewise, [schema(cl, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])]), schema(cl3, [svar(n)], [rs(unguarded, [atom(closed, [svar(n)])], [atom(listed, [role(1), svar(n)])], [atom(parent, [role(2)]), atom(ready, [svar(n)])], []), rs(guarded, [], [], [atom(parenting, [role(1)])], []), crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [role(1), svar(n)])], [], [])])], syntactically_grassroots).
compile(two_locals, [nschema(s, [num(a), num(b)], gt(num(a), num(b)), [rs(guarded, [atom(x, [])], [], [atom(m, [num(a)])], []), rs(guarded, [atom(y, [])], [], [atom(m, [num(b)])], [])])], syntactically_grassroots).
compile(one_local, [nschema(s, [num(d), num(t)], le(num(d), num(t)), [rs(guarded, [atom(x, [num(d)])], [], [], []), rs(unguarded, [atom(y, [num(d)])], [], [atom(m, [num(t)])], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Xs"', [x])], [act(s, '"S"', ['""', '""', '"Amount"'], none, '"Xs"', by([1]))])).
compile(gt_open, [nschema(u, [num(t), num(t1)], gt(num(t1), num(t)), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(succ_open, [nschema(u, [num(t), num(t1)], succ(num(t), num(t1)), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(or_open, [nschema(u, [num(t), num(t1)], or(succ(num(t), num(t1)), gt(num(t1), num(t))), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(eq_open, [nschema(u, [num(t), num(t1)], eq(num(t1), plus(num(t), num(1))), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(eq_scaled, [nschema(u, [num(t), num(t1)], eq(mul(num(2), num(t1)), num(t)), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(eq_zero, [nschema(u, [num(t), num(t1)], eq(minus(num(t1), num(t1)), num(t)), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(and_chain, [nschema(u, [num(t), num(t1), num(t2)], and(eq(num(t2), plus(num(t1), num(1))), eq(num(t1), plus(num(t), num(1)))), [rs(unguarded, [atom(level, [num(t2)]), atom(mid, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Levels"', [level])], [])).
compile(and_lt, [nschema(u, [num(t), num(t1)], and(lt(num(t), num(6)), gt(num(t1), num(t))), [rs(unguarded, [atom(level, [num(t1)])], [atom(level, [num(t)])], [], [])])], syntactically_grassroots).
compile(two_role_num, [schema(s, [num(n)], [rs(guarded, [atom(x, [])], [], [atom(m, [num(n)])], []), rs(guarded, [atom(y, [])], [], [atom(m, [num(n)])], [])])], syntactically_grassroots).
compile(mult, [schema(s, [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))], [], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Coins"', [coin])], [act(s, '"S"', [], none, '"Coins"', by([1]))])).
compile(nopr, [schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])])], syntactically_grassroots, no_introduction, no_pragmas).
:quit
HEREDOC
2>&1)
check "CVA compiler: a contract with no pragmas, local and effective, its verdict syntactically grassroots, refused, given no_pragmas by compile/5 as compile.sh gives it for a NAME.txt with none (nopr)" "%% not compiled, having no pragmas(nopr)" "$cva_refuse"
check "CVA compiler: ... the line saying why" '%% it passes the checks, its verdict, local and effective, and has no pragmas, the words its app shows: the compiler prints a platform only from a contract with pragmas, and refuses one with none (vGLP, Section 5.4, "The contract.s words").' "$cva_refuse"
check_not "CVA compiler: ... and no platform printed for it, nor for a fixture given no pragmas by compile/3" "%% the canonical CVA platform of the contract(nopr)\|%% the canonical CVA platform of the contract(two_reqs)" "$cva_refuse"
# None of the locality conditions stays (vGLP 099f876, Section 5.4, "Act
# schemas as local specifications": each schema-binding determines a part of
# its class, with the people in its marked roles as its guard set, and "Such a
# contract is a local specification"; vGLP #15, Code_inbox.md, 2026-10-19
# 10:50 UTC, item 2): a class is the union of its parts, and neither equal
# marked sets nor simple guards are asked.
check "CVA compiler: two schemas of one class requiring different atoms at both participants, marking the same people, passing the checks: the class has two parts" "%% not compiled, having no pragmas(two_reqs)" "$cva_refuse"
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
check "CVA compiler: an unguarded schema whose first role requires an atom naming y, y in an added atom through the signed term sgn(y, 1), passing the checks (sgnread)" "%% not compiled, having no pragmas(sgnread)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(sgnread)" "$cva_refuse"
check "CVA compiler: ... its first role requiring no atom naming y, refused as not effective (sgnread_not)" "%% not compiled(sgnread_not)" "$cva_refuse"
check "CVA compiler: ... the schema named" "%% the schema(d)" "$cva_refuse"
check "CVA compiler: a contract without traceable provenance is not compiled" "%% not compiled(gossip)" "$cva_refuse"
check "CVA compiler: a contract whose one fault is that it has no unobstructed introductory act, traceable provenance and volition met, passing the checks (vGLP, Section 5.4, asks locality and effectiveness alone; CSSN, Appendix B.4)" "%% not compiled, having no pragmas(nointro)" "$cva_refuse"
# The verdicts of programs/gsc at GSC-unob3 62fb0342, every value case decided (GSC bcc1891, Appendix "Values in Act Schemas"), of the compiler's Verdict type: obstructed_jointly, a fault of an introductory act, as obstructed is; and a constraint that is not linear, a fault of the schema's form in a verdict malformed.
check "CVA compiler: a contract whose one fault is an introductory act obstructed jointly, no choice of its values meeting every condition of def:unobstructed, passing the checks, as one with no unobstructed introductory act (jointly)" "%% not compiled, having no pragmas(jointly)" "$cva_refuse"
check "CVA compiler: a contract malformed, a product of two value variables in its constraint, is not compiled (nonlinear)" "%% not compiled(nonlinear)" "$cva_refuse"
# A verdict malformed, of a schema's form (GSC, Definition "Act schema", as programs/gsc/self.glp writes its faults), is refused whole, its faults of the compiler's Verdict type.
# A reading role, "a non-guarding role that adds and deletes nothing, admitted where a guarding role requires an atom naming it" (vGLP dd37d2a, Section 5.4, "Speech-act schemas as local specifications"; GSC 0380a8a): one with no such atom is a fault of volition, changes_nothing in a verdict not_grassroots, as programs/gsc gives it from GLP e2b5a802 on, and is not compiled; one with it passes the checks.
check "CVA compiler: a reading role that no guarding role's required atom names, the fault changes_nothing of volition in a verdict not_grassroots, is not compiled (inert)" "%% not compiled(inert)" "$cva_refuse"
check "CVA compiler: ... the verdict named" "%% it lacks traceable provenance or volition, or is otherwise faulty; check_named in programs/gsc gives the faults." "$cva_refuse"
check "CVA compiler: a reading role that a guarding role's required atom names, passing the checks (reading_ok)" "%% not compiled, having no pragmas(reading_ok)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(reading_ok)" "$cva_refuse"
# A constraint with a signing condition, unsigned(u, y) (vGLP dd37d2a, Section 5.4: "signed(u, y) and unsigned(u, y), that the person bound to u signed the speech act bound to y or did not"), passing the checks; a signing condition on no agent, the checker's fault signing_not_agent in a verdict malformed, refused.
check "CVA compiler: a constraint holding unsigned(role(2), svar(x)), passing the checks (signing_con)" "%% not compiled, having no pragmas(signing_con)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(signing_con)" "$cva_refuse"
check "CVA compiler: a signing condition on no agent, malformed, is not compiled (signing_bad)" "%% not compiled(signing_bad)" "$cva_refuse"
check "CVA compiler: ... the verdict named" "%% it is malformed; check_named in programs/gsc gives the faults." "$cva_refuse"
# A line's fields are the variables of its button's acts that the atoms each requires at the proposer's role do not bind, and pragmas.sh refuses a line whose fields are not these (vGLP 40f8444, Section 5.4, "The contract's words"; the pragmas.sh fixtures below): the compiler takes the pragmas pragmas.sh gives it, and no longer refuses a line by the places one entry fills.  link(p?, q, r), its proposer requiring friend(q) and friend(r), binds both, and its line with no field is compiled (unnamed_two), as is the same act given a field for r (unnamed_one), and GSN's leave(a, b? ; n), Bob's line with no field (group_row).
check "CVA compiler: a line with no field, its act's other roles bound by the atoms its proposer requires, compiled (unnamed_two)" "%% the canonical CVA platform of the contract(unnamed_two)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(unnamed_two)" "$cva_refuse"
check "CVA compiler: the same act with a field for r, compiled (unnamed_one)" "%% the canonical CVA platform of the contract(unnamed_one)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(unnamed_one)" "$cva_refuse"
check "CVA compiler: a line leaving a group's leader and its name unnamed, the places one entry, the group, fills, compiled (group_row)" "%% the canonical CVA platform of the contract(group_row)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(group_row)" "$cva_refuse"
check "CVA compiler: a contract malformed, a variable not in its schema's head (GSC 0228e79), is not compiled, the fault of the compiler's Verdict type" "%% not compiled(headless)" "$cva_refuse"
# The verdicts on an agent-named contract with community roles (programs/gsc/self.glp at GLP f5081cfa, GSC f3c4f43), of the compiler's Verdict type; its listed roles taken and its seated and constituent roles refused (vGLP f5f1644, Section 5.4, "Act schemas as local specifications"; vGLP #15 Cowork to Code, 2026-10-19 14:24 UTC, item 1).
check "CVA compiler: the verdict syntactically_grassroots, GSC's one definition for agent roles and listed roles (GSC ac7ad63), a schema of a listed role, passing the checks" "%% not compiled, having no pragmas(listed_named)" "$cva_refuse"
check "CVA compiler: the verdict syntactically_grassroots_with_wards, a schema of a listed role, passing the checks" "%% not compiled, having no pragmas(wards_named)" "$cva_refuse"
# Several listed roles of one base (vGLP c649c0d, Section 5.4, "Act schemas as local specifications": "each schema-binding determines a part [t]_j of a class for each list L of the people of its listed roles", and "the listed roles of a schema have one base"; vGLP #16 Cowork to Code, 2026-10-19 15:51 UTC: "refuse as not effective a schema whose listed roles have different bases").
check "CVA compiler: a schema of two listed roles of one base, its only guarding role, passing the checks" "%% not compiled, having no pragmas(listed_two)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(listed_two)\|more than one listed role" "$cva_refuse"
check "CVA compiler: a schema whose two listed roles have different bases, refused as not effective" "%% listed roles have different bases:" "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(listed_bases)" "$cva_refuse"
check "CVA compiler: a seated role, refused" "%% it has seated or constituent roles, which vGLP Section 5.4 does not take: it takes listed roles (vGLP f5f1644)." "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(seated_role)" "$cva_refuse"
# "a name term <u>.y_1...y_n is the community name of the person bound to u followed by the speech acts bound to y_1, ..., y_n" (vGLP c649c0d, Section 5.4, "Act schemas as local specifications"); a name variable, which a seated role would need, stays out with seated roles (vGLP #15 Cowork to Code, 2026-10-19 14:49 UTC).
check "CVA compiler: a name term built on a role, in an atom, compiled" "%% the canonical CVA platform of the contract(name_term)" "$cva_refuse"
check "CVA compiler: ... the atom holding a community name, one argument" "^member(CName)$" "$cva_refuse"
check "CVA compiler: a name term built on a name variable, refused" "%% it has name variables, which vGLP Section 5.4 does not take, as it does not take seated roles." "$cva_refuse"
check "CVA compiler: ... not compiled" "%% not compiled(name_var)" "$cva_refuse"
check_not "CVA compiler: no contract refused for a name term built on a role" "it has name terms" "$cva_refuse"
check "CVA compiler: reach conditions, refused" "%% it has reach conditions, which vGLP Section 5.4 does not take." "$cva_refuse"
# "the listed roles of a schema have one base, which is its only guarding role or, if it is not a guarding role, its first role" (vGLP a890465, Section 5.4, "Act schemas as local specifications"; vGLP #16 Cowork to Code, 2026-10-19 15:57 UTC, item 1: "child_close(r, p?, r[listed(*, n)] ; n) is such a schema: r opens it, p guards it"): listed_guard is child_close's form, its base the first role and not guarding, guarded by the second, and is compiled; a base that is not guarding and not the first role, guarded or not, and a base that guards with another guarding role, are refused.
check "CVA compiler: a schema whose listed role's base is its first role and not a guarding role, guarded by another role, as CSSN's child_close, passing the checks (listed_guard)" "%% not compiled, having no pragmas(listed_guard)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(listed_guard)" "$cva_refuse"
check "CVA compiler: a schema whose listed role's base is neither a guarding role nor, the schema having one guarding role or none, its first role, refused as not effective (vGLP 9ee6f87, Section 5.4)" "%% listed role whose base is neither a guarding role nor, the schema having one guarding role or none, its first role:" "$cva_refuse"
check "CVA compiler: ... guarded by its first role, not compiled (listed_guard_second)" "%% not compiled(listed_guard_second)" "$cva_refuse"
check "CVA compiler: ... unguarded, not compiled (listed_second)" "%% not compiled(listed_second)" "$cva_refuse"
check "CVA compiler: a schema whose listed role's base guards with another guarding role, passing the checks, the base a guarding role (vGLP 9ee6f87, Section 5.4: \"the base of a schema is a guarding role or, where the schema has one guarding role or none, its first role\") (listed_two_guards)" "%% not compiled, having no pragmas(listed_two_guards)" "$cva_refuse"
# The locality condition on a guarded part with an opener (vGLP f0acbc1, Section 5.4, "Act schemas as local specifications": "the contract is a local specification where every such schema has one guarding role, and every other guarded part of the class that a guard of such a part guards is determined likewise, with the same opener"; vGLP #18 Cowork to Code, 2026-10-20 10:44 UTC, item 5): a schema with a listed role whose base is not a guarding role, with two guarding roles or none, is refused; a contract one of whose such schemas determines a part of a class another part of which, guarded by its guard, is of a schema without a listed role, is refused, both schemas named; and one whose other part is of such a schema, with the same person in its first role, is compiled.
check "CVA compiler: a schema with a listed role whose base is not a guarding role, and two guarding roles, refused as not local (listed_guard_two)" "%% not compiled(listed_guard_two)" "$cva_refuse"
check "CVA compiler: ... the reason" "%% with a listed role whose base is not a guarding role has more than one guarding role, and it has one:" "$cva_refuse"
# "every such schema has one guarding role or none" (vGLP 613ee8d, Section 5.4, "Act schemas as local specifications"; vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:00 UTC): a schema with a listed role whose base is not a guarding role and no guarding role determines unguarded parts, its base its first role, which opens them.
check "CVA compiler: ... and no guarding role, passing the checks, its parts unguarded, opened by its first role (listed_unguarded)" "%% not compiled, having no pragmas(listed_unguarded)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(listed_unguarded)" "$cva_refuse"
check_not "CVA compiler: ... no reason of no guarding role given" "has no guarding role, and it has one:" "$cva_refuse"
check "CVA compiler: a guard of a guarded part opened by the child guarding another part of its class, of a schema with no listed role, refused as not local (listed_mixed)" "%% not compiled(listed_mixed)" "$cva_refuse"
check "CVA compiler: ... both schemas named" "%% the schema and the other(cl, cl2)" "$cva_refuse"
check "CVA compiler: ... the other part of a schema with a listed role whose base is not a guarding role, the same person in its first role, passing the checks (listed_likewise)" "%% not compiled, having no pragmas(listed_likewise)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(listed_likewise)" "$cva_refuse"
# Numbers (vGLP 613ee8d, Section 5.4, "Act schemas as local specifications"; vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:11 UTC, item 1): "A numeric variable that occurs only in atoms a schema requires or deletes at one role, and in the constraint, is local to that role ... A constraint is local to the role whose local variables it names, and to every role where it names none, which each checks.  Every other numeric variable is bound by the schema-binding, as a speech-act variable is, and the request names it, so the request form takes it, a number field.  A multiplicity k on an atom added or deleted adds or deletes k copies.  Locality: a constraint names the local variables of one role; refuse otherwise.  Effectiveness: a local variable need not occur in a deleted or added atom; a variable of an added atom of an unguarded schema that the required atoms do not name may be bound by the constraint to finitely many values (GB's advance, succ)."
check "CVA numbers: a constraint naming the local variables of two roles, refused as not local (two_locals)" "%% not compiled(two_locals)" "$cva_refuse"
check "CVA numbers: ... the reason" "%% names the local variables of two roles or more, and it names those of one:" "$cva_refuse"
check "CVA numbers: a constraint local to one role, its variable in no deleted or added atom, compiled (one_local)" "%% the canonical CVA platform of the contract(one_local)" "$cva_refuse"
check "CVA numbers: ... the constraint printed as checked by the role whose local variable it names, and the request naming d, a number field" "^schema_numeric(s, numeric(le(num(d), num(t)), \[num(t)\], at(2)))$" "$cva_refuse"
check "CVA numbers: ... the schema-binding binding d and not t, which stands for itself" "^binding_of(s(V1, V2, V3), s, \[b(role(1), p(V1?)), b(role(2), p(V2?)), b(num(d), n(V3?)), b(num(t), lv(s, t))\])$" "$cva_refuse"
check "CVA numbers: an unguarded schema adding a variable its constraint gives infinitely many values, gt, refused as not effective (gt_open)" "%% not compiled(gt_open)" "$cva_refuse"
check "CVA numbers: ... the same with succ, which gives it one value, passing the checks, as GB's advance (succ_open)" "%% not compiled, having no pragmas(succ_open)" "$cva_refuse"
check_not "CVA numbers: ... and not refused" "%% not compiled(succ_open)" "$cva_refuse"
check "CVA numbers: ... or(succ, gt), one branch giving infinitely many values, refused (or_open)" "%% not compiled(or_open)" "$cva_refuse"
# A constraint in GSC's form, "a conjunction of linear equalities and inequalities over its value variables and values" (GSC bcc1891, Definition "Schema with values"; programs/gsc/self.glp at GSC-unob3 62fb0342): an equality gives its one variable not bound one value where its coefficient is not 0, and a conjunction what either conjunct gives, taken again with each value given (vGLP, Section 5.4, "Act schemas as local specifications": "or the constraint gives the variable finitely many values once those are bound").
check "CVA numbers: an unguarded schema adding a variable a linear equality gives one value, t1 = t + 1, passing the checks (eq_open)" "%% not compiled, having no pragmas(eq_open)" "$cva_refuse"
check_not "CVA numbers: ... and not refused" "%% not compiled(eq_open)" "$cva_refuse"
check "CVA numbers: ... 2 t1 = t, its coefficient 2, passing the checks (eq_scaled)" "%% not compiled, having no pragmas(eq_scaled)" "$cva_refuse"
check "CVA numbers: ... t1 - t1 = t, its coefficient 0, giving t1 no value, refused as not effective (eq_zero)" "%% not compiled(eq_zero)" "$cva_refuse"
check "CVA numbers: a conjunction of two equalities, t2 = t1 + 1 and t1 = t + 1, t1 given by the second and t2 by the first once t1 is, compiled (and_chain)" "%% the canonical CVA platform of the contract(and_chain)" "$cva_refuse"
check "CVA numbers: ... its constraint printed as the contract writes it, t1 and t2, which it determines from t, local to the role as t is (vGLP 9ee6f87, Section 5.4)" "^schema_numeric(u, numeric(and(eq(num(t2), plus(num(t1), num(1))), eq(num(t1), plus(num(t), num(1)))), \[num(t2), num(t1), num(t)\], at(1)))$" "$cva_refuse"
check "CVA numbers: a conjunction of two inequalities, t < 6 and t1 > t, giving t1 infinitely many values, refused as not effective (and_lt)" "%% not compiled(and_lt)" "$cva_refuse"
check "CVA numbers: a numeric variable of atoms two roles require and of no deleted or added atom, local to neither, refused as not effective, the variable named (two_role_num)" "%% the schema and the variable(s, num(n))" "$cva_refuse"
check "CVA numbers: a multiplicity bound by the schema-binding, compiled (mult)" "%% the canonical CVA platform of the contract(mult)" "$cva_refuse"
check "CVA numbers: ... the request naming k, a number" "^binding_of(s(V1, V2), s, \[b(role(1), p(V1?)), b(num(k), n(V2?))\])$" "$cva_refuse"
# "The opener of an unguarded part is the least, in the order of identifiers, of the people in the first role of the schema-bindings that determine it, and so is the opener of a guarded part determined by a schema with a listed role whose base is not a guarding role, where that person is not a guard of the part" (vGLP a890465, Section 5.4, "Act schemas as local specifications"), and "A part with an opener is proposed by its opener, and a guarded part with none by one of its guards" (Section 3.2, "The protocol"): the shared text gives such a part its opener, and a guard proposes only a guarded part with no opener.
cva_groups_text=$(cat "$CVA/groups/platform.vglp" 2>&1 || true)
check "CVA openers: a guarded part opened by the person in the first role of its schema-bindings of a schema whose listed roles' base does not guard, not a guard of the part" "^    part_gfirsts(All?, mate(N?, B?, G?, F?), Q?, G?, Fo),$" "$cva_groups_text"
check "CVA openers: ... the schema's base its first role, read off the schema" "^opens_guarded1(lschema(_, _, Rs, \[listing(_, Bi, _, _, _, _, _)|_\]), B?) :- ground(Rs?), integer(Bi?) |$" "$cva_groups_text"
check "CVA openers: a guard proposes only a guarded part with no opener, of which it is a proposer (vGLP 9ee6f87, Definition \"Proposing\")" "^    proposes_op(Op?, Id?, B),$" "$cva_groups_text"
check "CVA openers: ... and an opener proposes as its own transaction only an unguarded part" "^opening_parts(\[pt(T, G, Op)|Ps\], Id, R?) :- ground(Id?) |$" "$cva_groups_text"
# A class's arguments are its schema's roles and then its variables in the order of its head (GSC 0228e79, Definition "Act schema"; vGLP #15 Cowork to Code, 2026-10-19 13:34 UTC, item 2): post(p?, a ; x, n), whose first atom names n before x, is post(p, a, x, n).
check "CVA compiler: a class's arguments in the order of the head, post(p?, a ; x, n) as post(V1, V2, V3, V4), x the third" "^binding_of(post(V1, V2, V3, V4), post, \[b(role(1), p(V1?)), b(role(2), p(V2?)), b(svar(x), s(V3?)), b(svar(n), s(V4?))\])$" "$cva_refuse"
check "CVA compiler: requirements differing at one participant only, passing the checks" "%% not compiled, having no pragmas(one_req)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(one_req)" "$cva_refuse"
check "CVA compiler: a class whose parts are guarded by two sets not both of one person, passing the checks, simple guards not asked (vGLP 099f876)" "%% not compiled, having no pragmas(two_gsets)" "$cva_refuse"
check_not "CVA compiler: ... and no fault on its guards" "%% not compiled(two_gsets)\|does not have simple guards" "$cva_refuse"
check "CVA compiler: an unguarded class whose schema-bindings require different atoms at one participant, passing the checks, the old condition of equal requirements in an unguarded class gone" "%% not compiled, having no pragmas(unguarded_reqs)" "$cva_refuse"
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
check "CVA compiler: requirements differing at both participants only by the reflexive atom naming each, passing the checks" "%% not compiled, having no pragmas(refl_own)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(refl_own)" "$cva_refuse"
check "CVA compiler: ... the same, f not declared reflexive, marking the same people, passing the checks" "%% not compiled, having no pragmas(refl_not)" "$cva_refuse"
check "CVA compiler: ... the two schemas marking different people, the reflexive atoms naming each holding there, every transaction of the class determined by both, passing the checks (refl_own_marks)" "%% not compiled, having no pragmas(refl_own_marks)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(refl_own_marks)" "$cva_refuse"
check "CVA compiler: ... the same, f not declared reflexive, a transaction determined by one alone, passing the checks, its parts guarded apart (refl_not_marks)" "%% not compiled, having no pragmas(refl_not_marks)" "$cva_refuse"
check "CVA compiler: a schema-binding of two roles at one person requiring and forbidding one atom there is of no class, so the contract is local" "%% not compiled, having no pragmas(contra)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(contra)" "$cva_refuse"
check "CVA compiler: ... its two roles at one person satisfiable there, the schema-binding of the class, marking the same people, passing the checks (contra_live)" "%% not compiled, having no pragmas(contra_live)" "$cva_refuse"
check "CVA compiler: ... marking different people, the schema-binding of no class: passing the checks (contra_marks)" "%% not compiled, having no pragmas(contra_marks)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(contra_marks)" "$cva_refuse"
check "CVA compiler: ... marking different people, the schema-binding of the class, passing the checks, its part guarded apart (contra_live_marks)" "%% not compiled, having no pragmas(contra_live_marks)" "$cva_refuse"
check_not "CVA compiler: ... no schema-binding named against it" "%% and the schema-binding(s2(person(0), person(1), person(0)))" "$cva_refuse"
check "CVA compiler: two schema-bindings of one class marking different people, a transaction determined by one alone, passing the checks, a part each with its own guard set" "%% not compiled, having no pragmas(marks)" "$cva_refuse"
check_not "CVA compiler: ... the condition of equal marked sets gone" "its condition): the schema-bindings determining two transactions of one class mark" "$cva_refuse"
check "CVA compiler: ... marking different people and requiring the same atoms, passing the checks" "%% not compiled, having no pragmas(marks_same)" "$cva_refuse"
check "CVA compiler: a class of four schema-bindings, two marking p and two marking q, some requiring an atom the others do not, each transaction determined by one marking p and one marking q, passing the checks (cover)" "%% not compiled, having no pragmas(cover)" "$cva_refuse"
check_not "CVA compiler: ... and not refused" "%% not compiled(cover)" "$cva_refuse"
check "CVA compiler: unfriending, one class guarded by either party, passing the checks (sym)" "%% not compiled, having no pragmas(sym)" "$cva_refuse"
check "CVA compiler: unfriending whose first role forbids an atom, a transaction determined by the binding of one party first alone, passing the checks, two parts guarded by one party each (asym)" "%% not compiled, having no pragmas(asym)" "$cva_refuse"
check_not "CVA compiler: ... no schema-binding named against it" "%% the schema-binding(unf(person(0), person(1)))" "$cva_refuse"

# The contract's words (vGLP 8aa82f0, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"): a question put to each guarding role other than the proposer's, the proposer, by([1]), named before the button, as pragmas.sh reads a line (trio); "the compiler requires a button for every guarded schema" (nobutton); and the widget declaration of the request type, a button for each button of the pragmas with the acts it names, each act(F, M, Pre, Binds, Fields, Same, Text), the atoms it requires and forbids at the proposer's role, the places they bind and its fields with their labels and types, in the schema's order (vGLP 4236349, Section 6.2: the request widget "draws one button for each way the viewer's state, in the last view written, satisfies the act at the proposer's role, with the values that way binds and the act's fields"; README.md of programs/vglp/cva/compiler, 6) (wdg).
cva_words=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 200000000
compile(trio, [schema(s, [], [rs(guarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], []), rs(guarded, [atom(z, [])], [], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Xs"', [x, xs])], [act(s, '"S"', ['""', '""', '"Person"'], said([[], [person(1), text('" asks."')], [person(1), text('" asks."')]]), '"Xs"', by([1]))])).
compile(nobutton, [schema(s, [], [rs(guarded, [atom(x, [])], [], [], []), rs(guarded, [atom(y, [])], [], [], []), rs(guarded, [atom(z, [])], [], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Xs"', [x, xs])], [])).
compile(wdg, [schema(sign, [svar(y)], [rs(guarded, [atom(item, [svar(y)])], [], [], [])]), schema(fwd, [svar(y)], [rs(guarded, [], [], [atom(item, [svar(y)])], []), rs(unguarded, [atom(item, [sgn(y, 1)])], [], [], [])]), schema(swap_a, [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))], [], [], [])]), schema(swap_b, [num(k)], [rs(guarded, [times(num(k), atom(bond, [role(1)]))], [], [], [])]), schema(link, [], [rs(guarded, [atom(linked, [role(2)])], [], [atom(knows, [role(2)])], [atom(linked, [role(2)])]), rs(guarded, [atom(linked, [role(1)])], [], [atom(knows, [role(1)])], [atom(linked, [role(1)])])])], syntactically_grassroots, no_introduction, pragmas([list('"Items"', [item, items])], [act(sign, '"Sign"', ['""', '"text"'], said([[person(1), text('" signs."')]]), '"Items"', by([1])), act(fwd, '"Forward"', ['""', '"Person"'], none, '"Items"', by([1])), act(swap_a, '"Swap"', ['""', '"amount"'], none, '"Items"', by([1])), act(swap_b, '"Swap"', ['""', '"amount"'], none, '"Items"', by([1])), act(link, '"Link"', [], said([[], [person(1), text('" asks."')]]), '"Items"', by([1]))])).
:quit
HEREDOC
2>&1)
check "CVA compiler, pragmas: the clause's one question put to each guarding role other than the proposer's, the second and the third (trio)" 'ask_of(.q(V2?, 1, .person(V1?), text(" asks.").), q(V3?, 1, .person(V1?), text(" asks.").)., Me?, Ws)' "$cva_words"
check "CVA compiler, pragmas: a contract with a guarded schema no button names, not compiled (nobutton)" "%% not compiled(nobutton)" "$cva_words"
check "CVA compiler, pragmas: ... the line saying which schema" "%% a guarded schema has no button in the contract's pragmas:(s)" "$cva_words"
check "CVA compiler, widget: the request type's declaration a menu of buttons, button(B, Acts), no tab, no row and no item in it (wdg)" '^Request? =::= social_menu(.button("Sign", ' "$cva_words"
check "CVA compiler, widget: a button of one act with a field, its type text, and the text of its request form, no atom required (wdg, sign)" 'button("Sign", .act(sign, 1, .1., .., .., .field(2, "text", text)., .., .at(1), say(" signs.").).)' "$cva_words"
check "CVA compiler, widget: an act whose required atom item(y) binds the way, Binds [3], and a field for a person, its type person (wdg, fwd)" 'button("Forward", .act(fwd, 1, .1, 2., .req(item, .at(3).)., .3., .field(2, "Person", person)., .., ..).)' "$cva_words"
check "CVA compiler, widget: a button of several acts, swap_a and swap_b in one button, each with its number field (wdg, Swap)" 'button("Swap", .act(swap_a, 1, .1., .., .., .field(2, "amount", number)., .., ..), act(swap_b, 1, .1., .., .., .field(2, "amount", number)., .., ..).)' "$cva_words"
check "CVA compiler, widget: an act with a required and a forbidden atom at the proposer's role, req(knows, [at(2)]) and forb(linked, [at(2)]), Bob bound by knows, and what the schemas prove, unless(linked, [at(2)]) (wdg, link)" 'button("Link", .act(link, 1, .1, 2., .req(knows, .at(2).), forb(linked, .at(2).), unless(linked, .at(2).)., .2., .., .., ..).)' "$cva_words"
check_not "CVA compiler, widget: ... no label, item, pick, group, meeting or tab entry left (wdg)" 'label(\|item([0-9]\|pick(\|group(\|meeting(\|act([a-z_]*, "' "$(printf '%s\n' "$cva_words" | grep '^Request? =::=')"

# Each constant from the contract is written through term_to_string/2, so that the reader reads it back as the same constant, and each line built by string_concat/2 (vGLP 4aa7e60, "Speech-act schemas"; GLP-Spec ca3c8e5, appendix-guards, "Text"; vGLP #23, Code_inbox.md, 2026-11-06 07:57 UTC): a contract with an agent variable 'Carol' and a predicate 'received_in' prints the platform of its renamed twin, but for those names.
cva_twin_print() {
"$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 200000000
compile(twin, [schema(pass, [pvar($1)], [rs(guarded, [atom($2, [pvar($1)])], [atom(held, [role(2)])], [atom(held, [role(2)])], []), rs(guarded, [atom(held, [role(1)])], [], [], [atom(held, [role(1)])])])], syntactically_grassroots, no_introduction, pragmas([list('"Got"', [$2])], [act(pass, '"Pass"', ['""', '""', '"Got"'], said([[], [person(1), text('" asks."')]]), '"Got"', by([1]))])).
:quit
HEREDOC
}
cva_twin_a=$(cva_twin_print "'Carol'" "'received_in'" 2>&1)
cva_twin_b=$(cva_twin_print dora taken_in 2>&1)
check "CVA compiler, text: an agent variable 'Carol' printed so that the reader reads it back, pvar('Carol') (twin)" "^schema_entry(pass, schema(pass, \[pvar('Carol')\], " "$cva_twin_a"
check "CVA compiler, text: ... a plain predicate bare, received_in (twin)" "atom(received_in, \[pvar('Carol')\])" "$cva_twin_a"
check "CVA compiler, text: the platform of the renamed twin but for those names (twin)" "^same$" "$([ "$(printf '%s\n' "$cva_twin_a" | sed -e "s/'Carol'/dora/g" -e 's/received_in/taken_in/g')" = "$cva_twin_b" ] && echo same || echo differs)"
check "CVA compiler, text: ... a platform printed (twin)" "the canonical CVA platform of the contract(twin)" "$cva_twin_b"

# The pragmas of a contract that extends another (vGLP 40f8444, Section 5.4, "The contract's words", its last sentence: "an inherited button, with its fields and its questions, stands while the contract has an act it names, an act the contract replaces being gone and its button with it; and where the contract writes a button of an inherited button's name, its line replaces the inherited one"): GAI's GSNP writes its own [Message] line, which replaces GSN's.
cva_gsnp_out=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/gai/GSNP.glp
contract_named(gsnp, C).
:quit
HEREDOC
2>&1)
cva_gsnp_term=$(echo "$cva_gsnp_out" | sed -e 's/^\(GLP> \)*//' | sed -n -e 's/^C = //p' | head -1)
cva_gsnp_pragmas=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$GLP_DIR/programs/gai/GSNP.txt" "$cva_gsnp_term" 2>&1 || true)
check "CVA pragmas: GSNP's [Message] line replaces GSN's, the lines matched by button (GSNP.txt)" "GSN: Messaging: Alice \[Message\] <text>: replaced by the line of GSNP.txt with a button of the same name" "$cva_gsnp_pragmas"
# The rule on fixtures of pragmas.sh's own (vGLP 40f8444, Section 5.4, "The contract's words"): XB, its Link line naming a field for Bob, whom link's friend(Bob) at Alice binds, refused, the field named (bound); XE, extending XB, whose Joint link line writes [Link] under another clause's title and replaces XB's Link line, whose term has no mint, so XB's [Mint] goes, and whose [Link] has no field, read (inherit); XC, its [Mint] with no field for k, a multiplicity naming no variable bound, refused, the variable named (unnamed).
cva_px=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XB' 'Alice and Bob are persons.' '1. ■ Friendship. Alice and Bob may jointly decide to become friends.' '2. ■ Link. Alice may link Bob.' '3. ■ Minting. Alice may mint coins.' 'Pragmas' 'Friends: the friends of Alice.' 'Friendship: Alice [Befriend]' '  Bob: Alice asks to be your friend.' 'Link: Alice [Link] <person>' 'Minting: Alice [Mint] <amount>' > "$cva_px/XB.txt"
printf '%s\n' '% Extends: XB, the base.' 'XE' 'Alice and Bob are persons.' '1. ■ Joint link. Alice may link Bob, a friend.' 'Pragmas' 'Linked: those Alice linked.' 'Joint link: Alice [Link]' > "$cva_px/XE.txt"
printf '%s\n' '% Extends: nothing.' 'XC' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint]' > "$cva_px/XC.txt"
cva_px_bef="schema(befriend, ['Alice', 'Bob'], [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(knows, [role(2)])], []), rs(guarded, [atom(friend, [role(1)])], [], [atom(knows, [role(1)])], [])])"
cva_px_link="schema(link, ['Alice', 'Bob'], [], [rs(guarded, [atom(linked, [role(2)])], [], [atom(friend, [role(2)])], []), rs(unguarded, [], [], [], [])])"
cva_px_mint="schema(mint, ['Alice'], [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))], [], [], [])])"
cva_px_bound=$(bash "$CVA/compiler/pragmas.sh" "$cva_px/XB.txt" "[$cva_px_bef, $cva_px_link, $cva_px_mint]" 2>&1; echo "exit $?")
cva_px_inherit=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_px/XE.txt" "[$cva_px_bef, $cva_px_link]" 2>&1; echo "exit $?")
cva_px_unnamed=$(bash "$CVA/compiler/pragmas.sh" "$cva_px/XC.txt" "[$cva_px_mint]" 2>&1; echo "exit $?")
rm -rf "$cva_px"
check "CVA pragmas: a field naming a variable the proposer's required atoms bind, refused (bound)" 'the pragma "Link" has the field <person> beyond the variables of link that the atoms its proposer’s role requires do not bind, which are none; those atoms bind Bob' "$cva_px_bound"
check "CVA pragmas: ... and pragmas.sh exits 1" "^exit 1$" "$cva_px_bound"
check_not "CVA pragmas: ... the Minting line not refused (bound)" 'the pragma "Minting"' "$cva_px_bound"
check "CVA pragmas: a line of an extending contract replaces the inherited line with a button of the same name, under another clause's title (inherit)" "XB: Link: Alice \[Link\] <person>: replaced by the line of XE.txt with a button of the same name" "$cva_px_inherit"
check "CVA pragmas: ... an inherited button whose contract has no act it names goes, with its field (inherit)" "XB: Minting: Alice \[Mint\]: gone, the contract having no act it names" "$cva_px_inherit"
check "CVA pragmas: ... the inherited button that stands, with its question, read (inherit)" "act('befriend', '\"Befriend\"', \[\], said(\[\[\], \[person(1), text('\" asks to be your friend.\"')\]\]), '\"Friends\"', by(\[1\]))" "$cva_px_inherit"
check "CVA pragmas: ... the extending contract's [Link], no field, its Bob bound by friend(Bob), on its own tab (inherit)" "act('link', '\"Link\"', \[\], none, '\"Linked\"', by(\[1\]))" "$cva_px_inherit"
check_not "CVA pragmas: ... no act of mint (inherit)" "act('mint'" "$cva_px_inherit"
check "CVA pragmas: a line leaving unnamed a variable no required atom binds, refused, the variable named (unnamed)" 'the pragma "Minting" leaves unnamed the variable k of mint' "$cva_px_unnamed"
# The line saying what a contract is written from, with several relations, each with the contracts after it (GSC, Section 8, Relations among Contracts: amends, extends, instantiates, implements and restricts; GAI.txt, "GAI extends GnW and amends GFP (...)."; GAI #12, Code_inbox.md, 2026-11-08 08:10 UTC): XR extends XB2 and amends XC2, and has the pragmas of both with its own (two); XS implements XC2 (implements); XQ's "copies" is no relation, and the name read is refused (norel).
cva_pr2=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XB2' 'Alice and Bob are persons.' '1. ■ Friendship. Alice and Bob may jointly decide to become friends.' 'Pragmas' 'Friends: the friends of Alice.' 'Friendship: Alice [Befriend]' '  Bob: Alice asks to be your friend.' > "$cva_pr2/XB2.txt"
printf '%s\n' '% Extends: nothing.' 'XC2' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' > "$cva_pr2/XC2.txt"
printf '%s\n' 'XR' 'XR extends XB2 and amends XC2 (GSC, From Grassroots Contract to Grassroots App).' 'Alice and Bob are persons.' '1. ■ Waving. Alice may wave.' 'Pragmas' 'Waves: the waves of Alice.' 'Waving: Alice [Wave]' > "$cva_pr2/XR.txt"
printf '%s\n' 'XS' 'XS implements XC2.' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Mints: the coins Alice minted.' > "$cva_pr2/XS.txt"
printf '%s\n' 'XQ' 'XQ extends XB2 and copies XC2.' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Coins: the coins Alice holds.' > "$cva_pr2/XQ.txt"
cva_pr2_wave="schema(wave, ['Alice'], [], [rs(guarded, [atom(wave, [])], [], [], [])])"
cva_pr2_two=$(bash "$CVA/compiler/pragmas.sh" "$cva_pr2/XR.txt" "[$cva_px_bef, $cva_px_mint, $cva_pr2_wave]" 2>&1; echo "exit $?")
cva_pr2_impl=$(bash "$CVA/compiler/pragmas.sh" "$cva_pr2/XS.txt" "[$cva_px_mint]" 2>&1; echo "exit $?")
cva_pr2_norel=$(bash "$CVA/compiler/pragmas.sh" "$cva_pr2/XQ.txt" "[$cva_px_mint]" 2>&1; echo "exit $?")
rm -rf "$cva_pr2"
check "CVA pragmas: two relations on one line, extends and amends, the pragmas of both contracts read: XB2's [Befriend] (two)" "act('befriend', '\"Befriend\"', \[\], said(" "$cva_pr2_two"
check "CVA pragmas: ... XC2's [Mint] (two)" "act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\]))" "$cva_pr2_two"
check "CVA pragmas: ... XR's own [Wave] (two)" "act('wave', '\"Wave\"', \[\], none, '\"Waves\"', by(\[1\]))" "$cva_pr2_two"
check "CVA pragmas: ... and pragmas.sh exits 0 (two)" "^exit 0$" "$cva_pr2_two"
check "CVA pragmas: implements read as a relation, XC2's [Mint] read (implements)" "act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\]))" "$cva_pr2_impl"
check "CVA pragmas: ... and pragmas.sh exits 0 (implements)" "^exit 0$" "$cva_pr2_impl"
check "CVA pragmas: a word that is no relation, refused, the name read named (norel)" 'names no contract at "copies XC2"' "$cva_pr2_norel"
check_not "CVA pragmas: ... and pragmas.sh does not exit 0 (norel)" "^exit 0$" "$cva_pr2_norel"
# Two kinds of pragmas (GSC, sections/02-contracts.tex:3; GSA, sections/s3-super-app.tex:69; GSC #13, Code_inbox.md, 2026-11-07 19:29 UTC, 2): "Interface pragmas" read as "Pragmas" are, and "Implementation pragmas", and every line after it to the next interface heading, skipped, no paper yet saying what form they take in the app's code (XI); and "Pragmas" alone the interface ones (XP).
cva_ph=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XI' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Interface pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' 'Implementation pragmas' 'Minting: before minting, the app checks the amount.' > "$cva_ph/XI.txt"
printf '%s\n' '% Extends: nothing.' 'XP' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' > "$cva_ph/XP.txt"
cva_ph_mint="schema(mint, ['Alice'], [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))], [], [], [])])"
cva_ph_i=$(bash "$CVA/compiler/pragmas.sh" "$cva_ph/XI.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
cva_ph_p=$(bash "$CVA/compiler/pragmas.sh" "$cva_ph/XP.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
rm -rf "$cva_ph"
check "CVA pragmas: the interface pragmas under their heading read, the implementation pragmas skipped (XI)" "^pragmas(\[list('\"Coins\"', \['coin'\])\], \[act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\]))\])$" "$cva_ph_i"
check "CVA pragmas: ... and pragmas.sh exits 0 (XI)" "^exit 0$" "$cva_ph_i"
check "CVA pragmas: ... the same as under the heading Pragmas alone (XP)" "^pragmas(\[list('\"Coins\"', \['coin'\])\], \[act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\]))\])$" "$cva_ph_p"
# The look the interface pragmas open with, "Look: <icon>, <family>" (GSC 34bc713, Section 2: "The interface pragmas may open with a line naming the app's look, its icon and its family"): read and no tab, under either heading (XL, XLI); not first, refused (XLX); not of that form, refused (XLF).
cva_pl=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XL' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Look: 🪙, money' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' > "$cva_pl/XL.txt"
printf '%s\n' '% Extends: nothing.' 'XLI' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Interface pragmas' 'Look: 🪙, money' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' > "$cva_pl/XLI.txt"
printf '%s\n' '% Extends: nothing.' 'XLX' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Coins: the coins Alice holds.' 'Look: 🪙, money' 'Minting: Alice [Mint] <amount>' > "$cva_pl/XLX.txt"
printf '%s\n' '% Extends: nothing.' 'XLF' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Look: money' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' > "$cva_pl/XLF.txt"
cva_pl_l=$(bash "$CVA/compiler/pragmas.sh" "$cva_pl/XL.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
cva_pl_i=$(bash "$CVA/compiler/pragmas.sh" "$cva_pl/XLI.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
cva_pl_x=$(bash "$CVA/compiler/pragmas.sh" "$cva_pl/XLX.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
cva_pl_f=$(bash "$CVA/compiler/pragmas.sh" "$cva_pl/XLF.txt" "[$cva_ph_mint]" 2>&1; echo "exit $?")
rm -rf "$cva_pl"
check "CVA pragmas: the Look line read and no tab, the pragmas as without it (XL)" "^pragmas(\[list('\"Coins\"', \['coin'\])\], \[act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\]))\])$" "$cva_pl_l"
check "CVA pragmas: ... and pragmas.sh exits 0 (XL)" "^exit 0$" "$cva_pl_l"
check "CVA pragmas: ... the same under the heading Interface pragmas (XLI)" "^pragmas(\[list('\"Coins\"', \['coin'\])\], \[act('mint'" "$cva_pl_i"
check "CVA pragmas: a Look line not the first of the interface pragmas, refused (XLX)" 'the look is the line the interface pragmas open with, "Look: <icon>, <family>"' "$cva_pl_x"
check_not "CVA pragmas: ... and pragmas.sh does not exit 0 (XLX)" "^exit 0$" "$cva_pl_x"
check "CVA pragmas: a Look line not of the form Look: <icon>, <family>, refused (XLF)" '"Look: money": the look is the line the interface pragmas open with' "$cva_pl_f"
check_not "CVA pragmas: ... and pragmas.sh does not exit 0 (XLF)" "^exit 0$" "$cva_pl_f"
# Several buttons on a line, and one button on two lines (vGLP 40f8444, Section 5.4, "The contract's words": "for each act they propose, its button, in brackets, bearing the act's name, and its fields, in angle brackets"; "A button is of the acts it names"; vGLP #23, Code_inbox.md, 2026-11-05 13:33 UTC, 1 and 2): the fields after a button, up to the next button, are that button's, so XM's "[Mint] <amount> [Burn]" is read, mint with k and burn with none (buttons), and XN's "[Mint] [Burn] <amount>" refused, mint leaving k unnamed and burn's <amount> beyond its variables (buttons_order); XD's [Mint] on two lines refused, the button and both lines named (twice); and XH extends XB-L, a contract's name with a hyphened part read whole, as GFF's GDO-L (hyphen).
cva_pb=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XM' 'Alice is a person.' '1. ■ Minting. Alice may mint coins and burn one.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount> [Burn]' > "$cva_pb/XM.txt"
printf '%s\n' '% Extends: nothing.' 'XN' 'Alice is a person.' '1. ■ Minting. Alice may mint coins and burn one.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] [Burn] <amount>' > "$cva_pb/XN.txt"
printf '%s\n' '% Extends: nothing.' 'XD' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' '2. ■ Burning. Alice may burn a coin.' 'Pragmas' 'Coins: the coins Alice holds.' 'Minting: Alice [Mint] <amount>' 'Burning: Alice [Mint] <amount> [Burn]' > "$cva_pb/XD.txt"
printf '%s\n' '% Extends: nothing.' 'XB-L' 'Alice is a person.' '1. ■ Burning. Alice may burn a coin.' 'Pragmas' 'Coins: the coins Alice holds.' 'Burning: Alice [Burn]' > "$cva_pb/XB-L.txt"
printf '%s\n' '% Extends: XB-L, the base (XB-L.txt beside this file).' 'XH' 'Alice is a person.' '1. ■ Minting. Alice may mint coins.' 'Pragmas' 'Minted: the coins Alice minted.' 'Minting: Alice [Mint] <amount>' > "$cva_pb/XH.txt"
cva_pb_burn="schema(burn, ['Alice'], [], [rs(guarded, [], [atom(coin, [role(1)])], [atom(coin, [role(1)])], [])])"
cva_pb_buttons=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pb/XM.txt" "[$cva_px_mint, $cva_pb_burn]" 2>&1; echo "exit $?")
cva_pb_order=$(bash "$CVA/compiler/pragmas.sh" "$cva_pb/XN.txt" "[$cva_px_mint, $cva_pb_burn]" 2>&1; echo "exit $?")
cva_pb_twice=$(bash "$CVA/compiler/pragmas.sh" "$cva_pb/XD.txt" "[$cva_px_mint, $cva_pb_burn]" 2>&1; echo "exit $?")
cva_pb_hyphen=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pb/XH.txt" "[$cva_px_mint, $cva_pb_burn]" 2>&1; echo "exit $?")
rm -rf "$cva_pb"
check "CVA pragmas: the fields after a button, up to the next, are its own: [Mint] <amount> names k (buttons)" "XM: Minting: Alice \[Mint\] <amount>: fields k" "$cva_pb_buttons"
check "CVA pragmas: ... and [Burn] after it has none (buttons)" "XM: Minting: Alice \[Burn\]: fields none" "$cva_pb_buttons"
check "CVA pragmas: ... both acts read (buttons)" "act('mint', '\"Mint\"', \['\"\"', '\"amount\"'\], none, '\"Coins\"', by(\[1\])), act('burn', '\"Burn\"', \[\], none, '\"Coins\"', by(\[1\]))" "$cva_pb_buttons"
check "CVA pragmas: [Mint] [Burn] <amount>: mint, with no field of its own, leaves k unnamed (buttons_order)" 'the pragma "Minting" leaves unnamed the variable k of mint' "$cva_pb_order"
check "CVA pragmas: ... and <amount> is beyond burn's variables (buttons_order)" 'the pragma "Minting" has the field <amount> beyond the variables of burn' "$cva_pb_order"
check "CVA pragmas: ... and pragmas.sh exits 1 (buttons_order)" "^exit 1$" "$cva_pb_order"
check "CVA pragmas: one button written by two lines, refused, the button and both lines named (twice)" 'the button \[Mint\] is written by two lines, "XD: Minting: Alice \[Mint\] <amount>" and "XD: Burning: Alice \[Mint\] <amount> \[Burn\]"' "$cva_pb_twice"
check "CVA pragmas: ... and pragmas.sh exits 1 (twice)" "^exit 1$" "$cva_pb_twice"
check "CVA pragmas: a contract extending one whose name has a hyphened part, XB-L, reads its lines (hyphen)" "XB-L: Burning: Alice \[Burn\]: fields none" "$cva_pb_hyphen"
check "CVA pragmas: ... and its tab (hyphen)" "list('\"Coins\"', \['coin'\]), list('\"Minted\"', \['coin'\])" "$cva_pb_hyphen"
# A button replaces a button (vGLP 4236349, Section 5.4, "The contract's words", its last sentence: "where the contract writes a button of an inherited button's name, its button, with its fields and its questions, replaces the inherited one, the other buttons of the inherited line standing"): XK extends XG, whose line writes [Create] <name> [Close], and writes only [Close]; XG's [Create], with its field, stands (replace_one).
cva_pr=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XG' 'Alice is a person.' '1. ■ Groups. Alice may create a group and close it.' 'Pragmas' 'Led: the groups Alice leads.' 'Groups: Alice [Create] <name> [Close]' > "$cva_pr/XG.txt"
printf '%s\n' '% Extends: XG, the base.' 'XK' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closed: the groups Alice closed.' 'Closing: Alice [Close]' > "$cva_pr/XK.txt"
cva_pr_create="schema(create, ['Alice'], [svar(n)], [rs(guarded, [atom(leads, [svar(n)])], [], [], [atom(leads, [svar(n)])])])"
cva_pr_close="schema(close, ['Alice'], [svar(n)], [rs(guarded, [atom(closed, [svar(n)])], [atom(leads, [svar(n)])], [atom(leads, [svar(n)])], [])])"
cva_pr_one=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pr/XK.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
rm -rf "$cva_pr"
check "CVA pragmas: a button of an inherited button's name replaces that button (replace_one)" "XG: Groups: Alice \[Close\]: replaced by the line of XK.txt with a button of the same name, the other buttons of the line standing" "$cva_pr_one"
check "CVA pragmas: ... the other button of the inherited line stands, with its field (replace_one)" "XG: Groups: Alice \[Create\] <name>: fields n" "$cva_pr_one"
check "CVA pragmas: ... both acts read, [Create] on its contract's tab and [Close] on the extending one's (replace_one)" "act('create', '\"Create\"', \['\"\"', '\"name\"'\], none, '\"Led\"', by(\[1\])), act('close', '\"Close\"', \[\], none, '\"Closed\"', by(\[1\]))" "$cva_pr_one"
check "CVA pragmas: ... and pragmas.sh exits 0 (replace_one)" "^exit 0$" "$cva_pr_one"
# A tab of an inherited tab's name replaces it (vGLP 14d9064, Section 5.4, "The contract's words", its last sentence: "has their tabs with its own, a tab of an inherited tab's name replacing it"): XT, its tabs Led and Closed, and XU and XV, each extending the one before, XU writing Led and XV writing Closed and Led; each tab printed once, in the place of the tab it replaces, the last written, with XU's own tab Kept after them (replace_tab); as GDO-L's Pointing replaces GDO's (replace_tab GDO-L).
cva_pt=$(mktemp -d)
printf '%s\n' 'XT' 'Alice is a person.' '1. ■ Groups. Alice may create a group and close it.' 'Pragmas' 'Led: the groups Alice leads.' 'Closed: the groups Alice leads.' 'Groups: Alice [Create] <name> [Close]' > "$cva_pt/XT.txt"
printf '%s\n' 'XU' 'XU extends XT.' 'Alice is a person.' '1. ■ Keeping. Alice may close a group.' 'Pragmas' 'Led: the groups Alice closed.' 'Kept: the groups Alice leads.' > "$cva_pt/XU.txt"
printf '%s\n' 'XV' 'XV extends XU.' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closed: the groups Alice closed.' 'Led: the groups Alice leads.' > "$cva_pt/XV.txt"
cva_pt_one=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pt/XV.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
rm -rf "$cva_pt"
check "CVA pragmas: a tab of an inherited tab's name replaces it, each tab once, in the place of the tab it replaces, the last written (replace_tab)" "^pragmas(\[list('\"Led\"', \['leads'\]), list('\"Closed\"', \['closed'\]), list('\"Kept\"', \['leads'\])\], " "$cva_pt_one"
check "CVA pragmas: ... the replaced tab named, with the last that replaces it (replace_tab)" "^XT: the tab Closed: replaced by the tab of XV of the same name$" "$cva_pt_one"
check "CVA pragmas: ... through a contract between (replace_tab)" "^XT: the tab Led: replaced by the tab of XV of the same name$" "$cva_pt_one"
check "CVA pragmas: ... and pragmas.sh exits 0 (replace_tab)" "^exit 0$" "$cva_pt_one"
# An inherited tab stands while the contract has the predicate it lists (vGLP e1d1b8a, Section 5.4, "The contract's words": "an inherited tab standing while the contract has the predicate it lists, as an inherited button stands while the contract has an act it names"), as GSN's Messages and Posts are gone in GSNP: XW amends XT, whose Closed lists closed; compiled with create alone, Closed is gone and Led stands, and with close too, both stand (drop_tab).
cva_pdt=$(mktemp -d)
printf '%s\n' 'XT' 'Alice is a person.' '1. ■ Groups. Alice may create a group and close it.' 'Pragmas' 'Led: the groups Alice leads.' 'Closed: the groups Alice closed.' 'Groups: Alice [Create] <name> [Close]' > "$cva_pdt/XT.txt"
printf '%s\n' 'XW' 'XW amends XT.' 'Alice is a person.' '1. ■ Keeping. Alice may keep a group.' 'Pragmas' 'Keeping: Alice [Create] <name>' > "$cva_pdt/XW.txt"
cva_pdt_one=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pdt/XW.txt" "[$cva_pr_create]" 2>&1; echo "exit $?")
cva_pdt_two=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pdt/XW.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
rm -rf "$cva_pdt"
check "CVA pragmas: an inherited tab whose predicate the contract lacks is gone, the tab whose predicate it has standing (drop_tab)" "^pragmas(\[list('\"Led\"', \['leads'\])\], " "$cva_pdt_one"
check "CVA pragmas: ... the tab gone named (drop_tab)" "^XT: the tab Closed: gone, the contract having no predicate it lists$" "$cva_pdt_one"
check "CVA pragmas: ... and pragmas.sh exits 0 (drop_tab)" "^exit 0$" "$cva_pdt_one"
check "CVA pragmas: ... with the predicate in the contract the tab stands (drop_tab)" "^pragmas(\[list('\"Led\"', \['leads'\]), list('\"Closed\"', \['closed'\])\], " "$cva_pdt_two"
check_not "CVA pragmas: ... and none is gone (drop_tab)" "gone, the contract having no predicate" "$cva_pdt_two"
# The line saying what a contract is written from, the first after its title and before its names declaration, "<NAME> <relation> <NAME'> (...)." or with no parentheses, the relation one of extends, amends, restricts and instantiates, several contracts joined by "and" (style guide 36; GSC, Section 8; vGLP #23, Code_inbox.md, 2026-11-06 07:17 UTC, 1): pragmas.sh takes the contracts it names as those whose pragmas the contract has, and reads the % Extends line where the line is absent.
cva_prel=$(mktemp -d)
mkdir "$cva_prel/old" "$cva_prel/new" "$cva_prel/hy"
for d in old new hy; do printf '%s\n' 'XG' 'Alice is a person.' '1. ■ Groups. Alice may create a group and close it.' 'Pragmas' 'Led: the groups Alice leads.' 'Groups: Alice [Create] <name> [Close]' > "$cva_prel/$d/XG.txt"; done
printf '%s\n' '% Extends: XG, the base.' 'XK' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closed: the groups Alice closed.' 'Closing: Alice [Close]' > "$cva_prel/old/XK.txt"
printf '%s\n' '% Extends: nothing.' 'XK' 'XK amends XG (GSC, From Grassroots Contract to Grassroots App, https://arxiv.org/abs/2601.00001).' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closed: the groups Alice closed.' 'Closing: Alice [Close]' > "$cva_prel/new/XK.txt"
printf '%s\n' 'XG-L' 'Alice is a person.' '1. ■ Labels. Alice may close a group.' 'Pragmas' 'Labelled: the groups Alice closed.' > "$cva_prel/hy/XG-L.txt"
printf '%s\n' 'XK: Closing' 'XK extends XG and XG-L.' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closed: the groups Alice closed.' 'Closing: Alice [Close]' > "$cva_prel/hy/XK.txt"
printf '%s\n' 'XK' 'XK extends XNONE.' 'Alice is a person.' '1. ■ Closing. Alice may close a group.' 'Pragmas' 'Closing: Alice [Close]' > "$cva_prel/hy/XN.txt"
cva_prel_old=$(bash "$CVA/compiler/pragmas.sh" "$cva_prel/old/XK.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
cva_prel_new=$(bash "$CVA/compiler/pragmas.sh" "$cva_prel/new/XK.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
cva_prel_hy=$(bash "$CVA/compiler/pragmas.sh" "$cva_prel/hy/XK.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
cva_prel_no=$(bash "$CVA/compiler/pragmas.sh" "$cva_prel/hy/XN.txt" "[$cva_pr_create, $cva_pr_close]" 2>&1; echo "exit $?")
rm -rf "$cva_prel"
check "CVA pragmas: a fixture with the relation line prints what one with the % Extends line prints (relation)" "^same$" "$([ "$cva_prel_old" = "$cva_prel_new" ] && echo same || echo differs)"
check "CVA pragmas: ... and it reads the contract named, XG's tab and XK's (relation)" "pragmas(\[list('\"Led\"', \['leads'\]), list('\"Closed\"', \['closed'\])\]" "$cva_prel_new"
check "CVA pragmas: several contracts joined by and, a hyphenated name read whole, XG-L's tab among the lists (relation)" "list('\"Labelled\"', \['closed'\])" "$cva_prel_hy"
check "CVA pragmas: ... and pragmas.sh exits 0 (relation)" "^exit 0$" "$cva_prel_hy"
check "CVA pragmas: a contract the line names with no NAME.txt, refused, named (relation)" "the contract XNONE, which its line saying what it is written from names, has no XNONE.txt" "$cva_prel_no"

# A variable local to a role is no place of the request and has no field (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications": "Every other variable is bound by the schema-binding, and a request names it"): XL's [Lpf] <label>, lpf(Alice, Bob ; l, d) its d local to Bob's role, names l and no field for d (local_field).
cva_pl=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XL' 'Alice and Bob are persons.' '1. ■ Labeled pointing. Alice may point to their friend Bob with a label Bob points with.' 'Pragmas' 'Pointing: the persons Alice points to.' 'Labeled pointing: Alice [Lpf] <label>' > "$cva_pl/XL.txt"
cva_pl_lpf="schema(lpf, ['Alice', 'Bob'], [pvar(l), pvar(d)], [rs(guarded, [atom(lp, [role(2), pvar(l)])], [], [atom(friend, [role(2)])], [atom(lp, [role(2), pvar(l)])]), rs(unguarded, [], [], [atom(lp, [pvar(d), pvar(l)])], [])])"
cva_pl_out=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_pl/XL.txt" "[$cva_pl_lpf]" 2>&1; echo "exit $?")
rm -rf "$cva_pl"
check "CVA pragmas: an agent variable local to another role has no field (local_field)" "XL: Labeled pointing: Alice \[Lpf\] <label>: fields l$" "$cva_pl_out"
check "CVA pragmas: ... the act's fields name l at its third place, d no place of the request (local_field)" "act('lpf', '\"Lpf\"', \['\"\"', '\"\"', '\"label\"'\], none, '\"Pointing\"', by(\[1\]))" "$cva_pl_out"

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
# compiles, B), itemarg (the item of a speech act, item_of(x), as an argument
# of an atom required, forbidden and added, passes the checks, vGLP e1d1b8a),
# deadfree (a schema requiring and forbidding an atom of a
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
compile(wards, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])])], grassroots_with_wards(enter, [m], [guardianship(m, i, g, w, w)], [relay(r, ward(1, m, guardian(2)), free(2))], [])).
compile(wards_leaving, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])])], grassroots_with_wards(enter, [m], [guardianship(m, i, g, w, w)], [relay(r, ward(1, m, guardian(2)), free(2))], [retired])).
compile(signed, [schema(fwd, [svar(x)], [rs(guarded, [atom(sent, [svar(x), role(2)])], [], [atom(item, [svar(x)])], []), rs(unguarded, [atom(item, [sgn(x, 1)])], [], [atom(friend, [role(1)])], [])])], syntactically_grassroots).
compile(itemarg, [schema(inst, [svar(x)], [rs(guarded, [atom(installed, [item_of(svar(x))])], [], [atom(item, [svar(x)])], [atom(installed, [item_of(svar(x))])])])], syntactically_grassroots).
compile(deadfree, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(guarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [pvar(x)], [rs(guarded, [atom(z, [])], [], [atom(q, [pvar(x)])], [atom(q, [pvar(x)])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])])], syntactically_grassroots).
compile(localagent, [schema(lpf, [pvar(l), pvar(d)], [rs(guarded, [atom(lp, [role(2), pvar(l)])], [], [atom(friend, [role(2)])], [atom(lp, [role(2), pvar(l)])]), rs(unguarded, [], [], [atom(lp, [pvar(d), pvar(l)])], [])])], syntactically_grassroots).
compile(sibling, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(child, []), atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(sibling_noclash, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(clash2_marks, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(unguarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(unguarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])])], syntactically_grassroots).
compile(chain3_marks, [schema(s1, [], [rs(guarded, [atom(z, [])], [], [atom(a, [])], []), rs(unguarded, [atom(w, [])], [], [atom(c, [])], [])]), schema(s2, [], [rs(unguarded, [atom(z, [])], [], [], [atom(a, [])]), rs(guarded, [atom(w, [])], [], [atom(d, [])], [])]), schema(s3, [], [rs(guarded, [atom(z, [])], [], [], []), rs(guarded, [atom(w, [])], [], [], [])])], syntactically_grassroots).
compile(sibling_marks, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(child, []), atom(friend, [role(1)])]), rs(unguarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
compile(sibling_noclash_marks, reflexive([friend], [schema(csib, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(1)])]), rs(guarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(parenting, [role(2)])], [atom(child, [])])]), schema(cab, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, []), atom(child, []), atom(parent, [role(3)])], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])]), rs(unguarded, [atom(approval, [])], [], [atom(identity, []), atom(parenting, [role(1)]), atom(friend, [role(2)])], [atom(child, [])])])]), syntactically_grassroots).
:quit
HEREDOC
2>&1)
check "CVA compiler: two schema-bindings of the same participants and atoms, one requiring an atom the other forbids, are two classes (clash2)" "%% not compiled, having no pragmas(clash2)" "$cva_classes"
check "CVA compiler: a third schema-binding linking them joins one class, all marking the same people, passing the checks (chain3)" "%% not compiled, having no pragmas(chain3)" "$cva_classes"
check "CVA compiler: ... the two marking different people, two classes, passing the checks (clash2_marks)" "%% not compiled, having no pragmas(clash2_marks)" "$cva_classes"
check "CVA compiler: ... and the third joining them, one class, its parts guarded apart, passing the checks (chain3_marks)" "%% not compiled, having no pragmas(chain3_marks)" "$cva_classes"
check "CVA compiler: a contract grassroots with wards passes the checks (wards)" "%% not compiled, having no pragmas(wards)" "$cva_classes"
check "CVA compiler: ... and one with a leaving mark, the verdict's fifth argument (GSC, Appendix F, Definition \"Leaving mark\"), passing the checks (wards_leaving)" "%% not compiled, having no pragmas(wards_leaving)" "$cva_classes"
check "CVA compiler: a signed term in an added atom, the speech act bound to y signed by the person in role i, passes the checks (signed)" "%% not compiled, having no pragmas(signed)" "$cva_classes"
check "CVA compiler: the item of a speech act as an argument of an atom, item_of(x), required, forbidden and added, its variable bound by an atom the role requires, passes the checks (itemarg; vGLP e1d1b8a, Section 5.4: an argument of an atom may be \"the signer, the author or the item of a speech act\")" "%% not compiled, having no pragmas(itemarg)" "$cva_classes"
check "CVA compiler: a schema requiring and forbidding an atom of a variable of no deleted or added atom, refused as not effective (deadfree)" "%% not compiled(deadfree)" "$cva_classes"
check "CVA compiler: ... the schema and the variable named" "%% the schema and the variable(s2, pvar(x))" "$cva_classes"
# Every variable local to a role (vGLP 8a3ae79, Section 5.4, "Speech-act schemas as local specifications": "A variable that occurs only in atoms the schema requires or deletes at one role, and in the constraint, is local to that role, as the person d where Bob's role requires that Bob point to someone with a label"; the effectiveness condition's "but a variable local to a role"): GDO-L's lpoint_friend, its agent variable d only in lp(d, l) required at Bob's role, passes the checks (localagent).
check "CVA compiler: an agent variable local to a role is excepted from effectiveness, passing the checks (localagent)" "%% not compiled, having no pragmas(localagent)" "$cva_classes"
check_not "CVA compiler: ... and not refused as not effective (localagent)" "%% the schema and the variable(lpf" "$cva_classes"

# Speech-act schemas with quantifiers, qschema in GSC's term form (programs/gsc/self.glp; vGLP 9ee6f87, Section 5.4, "Speech-act schemas as local specifications"): a safe formula of the body local to a role is taken, GFF-I's unfollow, not(and(friend(Alice, Bob), eq(Bob, Dave))) at Alice (q_local); one whose atoms two roles hold is local to no role and refused (q_nowhere); a quantified conjunct binding a listed guarding role is taken, the class term naming the people of the role and "its request name is the schema-binding" (q_conjunct); a count in a constraint is taken, local to the role holding its anchor (q_card); a conjunct binding no listed role with a conditional literal at a role other than the one holding its anchor is taken, its instances a list the base finds and the attempt names (q_inst; vGLP 9ee6f87, Section 5.4: "A conjunct with a listed role, or with conditional literals at a role other than the one holding its anchor, is local to its base; the base's machine finds its instances, the attempt names them"); one whose conditional literals are all at the role holding its anchor, local to that role, is refused, the compiler not carrying it yet (q_inst_local); one whose base is neither a guarding role nor, the schema having two guarding roles, its first role is refused as not effective (q_inst_base); and a pragma naming before its button a person other than the base, which guards, is refused (q_inst_by); a listed guarding role with the condition of its quantifier and a conditional literal at another role, the question put to each person of its list, ql(L, G, Ws), its listing carrying the condition and the literal, and the schema's base its first role (q_guard); and a listed reading role, the base guarding (q_read).
cva_q=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(q_local, [schema(establish, ['Alice'], [], [rs(guarded, [atom(identity, [])], [], [], [atom(identity, [])])]), qschema(unfollow, ['Alice', 'Bob'], [pvar('Dave')], [], true, [rs(guarded, [], [atom(lpoints_to, [role(2), pvar('Dave')])], [atom(identity, [])], []), rs(unguarded, [], [], [atom(identity, [])], [])], [], [fnot(fand(fa(role(1), atom(friend, [role(2)])), fcons(eq(role(2), pvar('Dave')))))])], syntactically_grassroots).
compile(q_nowhere, [qschema(s, ['Alice', 'Bob'], [], [], true, [rs(guarded, [atom(z, [])], [], [], []), rs(guarded, [atom(w, [])], [], [], [])], [], [fand(fa(role(1), atom(a, [])), fa(role(2), atom(b, [])))])], syntactically_grassroots).
compile(q_conjunct, [qschema(s, ['Alice', 'Bob', 'Alice'], [], [pvar('Carol')], true, [rs(guarded, [], [], [atom(member, [role(3)])], []), rs(guarded, [atom(in, [])], [], [], []), crs(qlisted(1, member, [role(3)], [role(3)]), guarded, [atom(joined, [role(2)])], [], [], [])], [conjunct([role(3)], 1, atom(member, [role(3)]), fcons(true), [], [], [fa(role(3), atom(joined, [role(2)]))])], [])], syntactically_grassroots).
compile(q_card, [qschema(s, ['Alice'], [num(d)], [pvar(f)], lt(cardinality([pvar(f)], role(1), atom(p, [pvar(f)]), fcons(true)), num(d)), [rs(guarded, [atom(z, [num(d)])], [], [], [])], [], [])], syntactically_grassroots).
compile(q_inst, [qschema(s, ['Alice', 'Bob'], [], [pvar('Dave')], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [])], [conjunct([pvar('Dave')], 1, atom(member, [pvar('Dave')]), fcons(true), [hm(2, atom(seen, [pvar('Dave')]))], [], [fa(role(2), atom(seen, [pvar('Dave')]))])], [])], syntactically_grassroots).
compile(q_guard, [qschema(s, ['Alice', 'Bob', 'Carol'], [], [pvar('Carol')], true, [rs(guarded, [atom(admitted, [role(2)])], [], [], [atom(admitted, [role(2)])]), rs(guarded, [atom(in, [])], [], [], [atom(in, [])]), crs(qlisted(1, admitted, [role(3)], [role(3)]), guarded, [atom(heard, [role(2)])], [], [atom(in, [])], [])], [conjunct([role(3)], 1, atom(admitted, [role(3)]), fcons(ne(role(3), role(2))), [hm(2, atom(seen, [role(3)]))], [], [fa(role(3), atom(heard, [role(2)])), fa(role(2), atom(seen, [role(3)]))])], [fforall([role(3)], role(1), atom(admitted, [role(3)]), fcons(ne(role(3), role(2))), [fa(role(3), atom(in, []))])])], syntactically_grassroots, no_introduction, pragmas([list('"Members"', [admitted])], [act(s, '"Admit"', ['""', '"person"'], said([[], [text('" asks you in."')], [person(1), text('" asks to admit "'), person(2), text('"."')]]), '"Members"', by([1]))])).
compile(q_inst_by, [qschema(s, ['Alice', 'Bob'], [], [pvar('Dave')], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [atom(in, [])])], [conjunct([pvar('Dave')], 1, atom(member, [pvar('Dave')]), fcons(true), [hm(2, atom(seen, [pvar('Dave')]))], [], [fa(role(2), atom(seen, [pvar('Dave')]))])], [])], syntactically_grassroots, no_introduction, pragmas([list('"Members"', [member])], [act(s, '"S"', [], said([[person(2), text('" asks."')], []]), '"Members"', by([2]))])).
compile(q_inst_local, [qschema(s, ['Alice', 'Bob'], [], [pvar('Dave')], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [])], [conjunct([pvar('Dave')], 1, atom(member, [pvar('Dave')]), fcons(true), [hm(1, atom(seen, [pvar('Dave')]))], [], [fa(role(1), atom(seen, [pvar('Dave')]))])], [])], syntactically_grassroots).
compile(q_inst_base, [qschema(s, ['Alice', 'Bob', 'Carol'], [], [pvar('Dave')], true, [rs(guarded, [atom(in, [])], [], [], [atom(in, [])]), rs(guarded, [atom(in, [])], [], [], [atom(in, [])]), rs(unguarded, [], [], [], [])], [conjunct([pvar('Dave')], 3, atom(member, [pvar('Dave')]), fcons(true), [hm(2, atom(seen, [pvar('Dave')]))], [], [fa(role(2), atom(seen, [pvar('Dave')]))])], [])], syntactically_grassroots).
compile(q_inst_sgn, [qschema(s, ['Alice', 'Bob'], [], [svar(y)], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [])], [conjunct([svar(y)], 1, atom(item, [svar(y)]), fcons(true), [hm(2, atom(got, [sgn(y, 1)]))], [], [fa(role(2), atom(got, [sgn(y, 1)]))])], [])], syntactically_grassroots).
compile(q_inst_none, [qschema(s, ['Alice', 'Bob'], [], [pvar('Dave')], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [])], [conjunct([pvar('Dave')], 1, atom(member, [pvar('Dave')]), fcons(true), [hm(2, atom(seen, []))], [], [fa(role(2), atom(seen, []))])], [])], syntactically_grassroots).
compile(q_inst_del, [qschema(s, ['Alice', 'Bob'], [], [pvar('Dave')], true, [rs(guarded, [], [], [], []), rs(guarded, [atom(in, [])], [], [], [])], [conjunct([pvar('Dave')], 1, atom(member, [pvar('Dave')]), fcons(true), [], [hm(2, atom(seen, [pvar('Dave')]))], [fnot(fa(role(2), atom(seen, [pvar('Dave')])))])], [])], syntactically_grassroots).
compile(q_real_det, [nschema(halve, [real(v), real(h)], eq(mul(num(2), num(h)), num(v)), [rs(guarded, [atom(half, [num(h)])], [atom(poured, [num(v)])], [], [])])], syntactically_grassroots).
compile(q_author, [nschema(sign, [svar(y)], eq(author_of(svar(y)), role(1)), [rs(guarded, [atom(item, [svar(y)])], [], [], [])])], syntactically_grassroots).
compile(q_item, [nschema(echo, [svar(y), svar(z)], eq(item_of(svar(y)), item_of(svar(z))), [rs(guarded, [atom(echo, [svar(y), svar(z)])], [], [atom(item, [svar(y)]), atom(item, [svar(z)])], [])])], syntactically_grassroots).
compile(q_program, [nschema(load, [svar(y)], program(item_of(svar(y))), [rs(guarded, [atom(loaded, [svar(y)])], [], [atom(item, [svar(y)])], [])])], syntactically_grassroots).
compile(q_real, [schema(pour, [real(v)], [rs(guarded, [atom(poured, [num(v)])], [], [], [])])], syntactically_grassroots).
compile(q_read, [qschema(s, ['Alice', 'Bob'], [], [pvar('Bob')], true, [rs(guarded, [atom(polled, [])], [], [], [atom(polled, [])]), crs(qlisted(1, admitted, [role(2)], [role(2)]), unguarded, [], [], [atom(in, [])], [])], [], [fforall([role(2)], role(1), atom(admitted, [role(2)]), fcons(true), [fa(role(2), atom(in, []))])])], syntactically_grassroots).
:quit
HEREDOC
2>&1)
check "CVA compiler: a schema with a safe formula of its body local to its first role, qschema, passes the checks (q_local)" "%% not compiled, having no pragmas(q_local)" "$cva_q"
check "CVA compiler: a safe formula whose atoms two roles hold, refused as local to no role (q_nowhere)" "formula of a schema's body is local to no role" "$cva_q"
check "CVA compiler: ... the schema named (q_nowhere)" "%% the schema(s)" "$cva_q"
check "CVA compiler: a quantified conjunct binding a listed guarding role, taken, passing the checks (q_conjunct)" "%% not compiled, having no pragmas(q_conjunct)" "$cva_q"
check "CVA compiler: a count in a constraint, local to the role holding its anchor, taken, passing the checks (q_card)" "%% not compiled, having no pragmas(q_card)" "$cva_q"
check "CVA compiler: a conjunct binding no listed role with a conditional literal at a role other than the one holding its anchor, its instances a list the attempt names, taken, passing the checks (q_inst; vGLP 9ee6f87, Section 5.4)" "%% not compiled, having no pragmas(q_inst)" "$cva_q"
check "CVA compiler: a pragma naming before the button a person other than the base of a schema with a conjunct whose instances the attempt names, the base guarding, refused (q_inst_by; vGLP 9ee6f87, Section 5.4, \"The contract's words\")" "names before a button a person other than the base of its schema, which guards" "$cva_q"
check "CVA compiler: a conjunct binding no listed role whose conditional literals are all at the role holding its anchor, local to that role, taken, passing the checks, the role finding its instances in its own state (q_inst_local; vGLP fe12932, Section 5.4; vGLP #24, Code_inbox.md, 2026-11-07 18:52 UTC, (ii))" "%% not compiled, having no pragmas(q_inst_local)" "$cva_q"
check "CVA compiler: a conditional literal holding a signed term of a speech-act variable its quantifier binds, refused until a contract needs it (q_inst_sgn; vGLP #24, Code_inbox.md, 2026-11-07 18:52 UTC, (iii))" "%% not compiled(q_inst_sgn)" "$cva_q"
check "CVA compiler: ... the reason (q_inst_sgn)" "a conditional literal holding a signed term of a speech-act variable its quantifier binds, refused until a contract needs it" "$cva_q"
check "CVA compiler: a conjunct whose literals at another role name no variable its quantifier binds, an instance restricted to none, refused (q_inst_none)" "%% not compiled(q_inst_none)" "$cva_q"
check "CVA compiler: a conjunct deleting a conditional literal at another role, taken, passing the checks (q_inst_del; vGLP fe12932, Section 5.4; vGLP #24, Code_inbox.md, 2026-11-07 18:52 UTC, (i))" "%% not compiled, having no pragmas(q_inst_del)" "$cva_q"
check "CVA compiler: a conjunct whose instances the attempt names, its base neither a guarding role nor, the schema having two guarding roles, its first role, refused as not effective (q_inst_base)" "%% not compiled(q_inst_base)" "$cva_q"
check "CVA compiler: ... the reason (q_inst_base)" "a schema with a conjunct whose instances the attempt names, whose base is neither" "$cva_q"
check "CVA compiler: a listed guarding role with a condition and a conditional literal, its listing carrying both (q_guard)" "^schema_entry(s, lschema(s, \[\], .*\[listing(3, 1, admitted, \[\], fcons(ne(role(3), role(2))), \[hm(2, atom(seen, \[role(3)\]))\], \[\])\]))$" "$cva_q"
check "CVA compiler: ... its question put to each person of its list (q_guard)" "^ask_of(\[q(V2?, 1, \[text(\" asks you in.\")\]), ql(V3?, 1, \[person(V1?), text(\" asks to admit \"), person(V2?), text(\".\")\])\], Me?, Ws)$" "$cva_q"
check "CVA compiler: ... its base, the base of its listed role, its first role (q_guard)" "^schema_base(s, 1)$" "$cva_q"
check "CVA compiler: ... the class term naming the people of the listed role, the request name not (q_guard)" "^s(Peer, Peer, List(Peer))$" "$cva_q"
check "CVA compiler: a real variable, real(v) among the schema's variables, taken, passing the checks (q_real; vGLP fb40cff, Section 5.4)" "%% not compiled, having no pragmas(q_real)" "$cva_q"
check "CVA compiler: a real variable the constraint determines, refused with the reason, GLP typing arithmetic on reals Number (q_real_det)" "%% not compiled(q_real_det)" "$cva_q"
check "CVA compiler: ... the reason (q_real_det)" "it has a real variable the constraint determines, which the compiler does not carry" "$cva_q"
check "CVA compiler: the author of a speech act in a constraint, GSC's author_of (GSC 65a26da), taken, passing the checks, vGLP's Section 5.4 naming the author and the item of a speech act (vGLP 27bae5d) (q_author)" "%% not compiled, having no pragmas(q_author)" "$cva_q"
check_not "CVA compiler: ... and not refused (q_author)" "%% not compiled(q_author)" "$cva_q"
check "CVA compiler: the item of a speech act in a constraint, eq(item_of(y), item_of(z)), taken, passing the checks (q_item)" "%% not compiled, having no pragmas(q_item)" "$cva_q"
check_not "CVA compiler: ... and not refused (q_item)" "%% not compiled(q_item)" "$cva_q"
check "CVA compiler: a program condition of a speech act in a constraint, refused with the reason, vGLP's Section 5.4 naming a program among the values and not how the runtime recognises one (q_program)" "%% not compiled(q_program)" "$cva_q"
check "CVA compiler: ... the reason (q_program)" "a program condition of a speech act among them, which vGLP Section 5.4 does not name" "$cva_q"
check "CVA compiler: a listed reading role, its base guarding, passing the checks (q_read)" "%% not compiled, having no pragmas(q_read)" "$cva_q"
check_not "CVA compiler: no qschema contract left unread (q_local, q_nowhere, q_conjunct, q_card, q_inst, q_inst_by, q_inst_local, q_inst_sgn, q_inst_none, q_inst_del, q_inst_base, q_guard, q_real_det, q_author, q_item, q_program, q_real, q_read)" "not well-typed\|→ failed" "$cva_q"

# Counts, sums, min and floor, and environment predicates (vGLP 9ee6f87, Section 5.4, "Speech-act schemas as local specifications"; vGLP #24, Code_inbox.md, 2026-11-07 13:11 UTC, 3 and 4): vGLP's counts, its agent run with no person and given at deployment cap(3) and the weights wt(4) and wt(2), which it holds from startup and its first view shows; fill, whose constraint counts the copies of tok its state holds, is taken until it holds three; and heavy, whose constraint sums the weights against min(2 floor(3), 100) = 6, is taken; with the weights wt(4) and wt(1), whose sum is 5, it is not.
cva_cnt_dir="$GLP_DIR/programs/tests/vg_counts_$$"
mkdir -p "$cva_cnt_dir" && cp "$CVA/counts/platform.vglp" "$cva_cnt_dir/platform.vglp"
cva_cnt_emit=$(printf ':emit %s\n:quit\n' "$cva_cnt_dir" | "$REPL_RUN" 2>&1)
cva_cnt=$("$REPL_RUN" <<HEREDOC
$cva_cnt_dir/platform.glp
:limit 2000000
agent(alice, [], N, O, [cap(3), wt(4), wt(2)], A).
agent(bob, [], N, O, [cap(3), wt(4), wt(1)], A).
:quit
HEREDOC
2>&1)
rm -rf "${cva_cnt_dir:?}"
check "CVA counts: the platform emitted to GLP, its agent taking the environment atoms, List(Atom)" "wrote .*vg_counts_.*platform.glp" "$cva_cnt_emit"
check "CVA counts: the first view shows the environment atoms given at deployment, held from startup" "O = \[view(alice, \[cap(3), wt(4), wt(2)\], " "$cva_cnt"
check "CVA counts: fill taken until the count of tok reaches cap's 3, each copy counted, and heavy taken, the sum of the weights 6 = min(2 floor(3), 100)" "view(alice, \[tok, tok, heavy, tok, cap(3), wt(4), wt(2)\]" "$cva_cnt"
check_not "CVA counts: ... and no fourth tok" "view(alice, \[tok, tok, tok, tok\|view(alice, \[tok, tok, heavy, tok, tok" "$cva_cnt"
check "CVA counts: with the weights 4 and 1, three tok and no heavy" "view(bob, \[tok, tok, tok, cap(3), wt(4), wt(1)\]" "$cva_cnt"
check_not "CVA counts: ... heavy not taken, the sum 5 below 6" "view(bob, \[[a-z(), 0-9]*heavy" "$cva_cnt"
# A conjunct whose instances the attempt names (vGLP 9ee6f87, Section 5.4, "Speech-act schemas as local specifications": "A conjunct with a listed role, or with conditional literals at a role other than the one holding its anchor, is local to its base; the base's machine finds its instances, the attempt names them, and every participant takes them as arguments of the transaction"; "each schema-binding determines a part [t]_j of a class for each list L of instances"): vGLP's shares, share(p?, q?) with "forall x (noted(x) at p -> seen(x) at q)", its printed procedures run on given states.  alice, who noted carol and dave, finds them at her state, the class term naming them after the schema's variables and the request name not; bob's role adds seen of each, inst(1, J) standing for the J-th; the class of the part, its schema-binding found among the class's changes with the list; alice's domain holds where her state has exactly the instances of the list, and not where it has others, her unable vote naming the anchors she lacks and those she has beyond the list; and bob's effect adds seen(carol) and seen(dave).
cva_sh_dir="$GLP_DIR/programs/tests/vg_shares_$$"
mkdir -p "$cva_sh_dir" && cp "$CVA/shares/platform.vglp" "$cva_sh_dir/platform.vglp"
cva_sh_emit=$(printf ':emit %s\n:quit\n' "$cva_sh_dir" | "$REPL_RUN" 2>&1)
cva_sh=$("$REPL_RUN" <<HEREDOC
$cva_sh_dir/platform.glp
:limit 50000000
name_class(share(alice, bob), [g(started, []), g(knows, [p(bob)]), g(knows, [p(carol)]), g(knows, [p(dave)]), g(noted, [p(dave)]), g(noted, [p(carol)])], C1).
name_class(share(alice, bob), [g(started, []), g(knows, [p(bob)])], C2).
request_name(share(alice, bob, [carol, dave]), N3).
term_spec(share(alice, bob, [carol, dave]), N4, R4, B4).
class_info(share(alice, bob, [carol, dave]), alice, [], R5, A5).
part_dom(share(alice, bob, [carol, dave]), alice, [g(started, []), g(knows, [p(bob)]), g(noted, [p(dave)]), g(noted, [p(carol)])], D6).
part_dom(share(alice, bob, [carol, dave]), alice, [g(started, []), g(knows, [p(bob)]), g(noted, [p(carol)]), g(noted, [p(erin)])], D7).
unable_reason(share(alice, bob, [carol, dave]), alice, [g(started, []), g(knows, [p(bob)]), g(noted, [p(carol)]), g(noted, [p(erin)])], R8).
apply_part(share(alice, bob, [carol, dave]), bob, [g(started, []), g(knows, [p(alice)])], S9).
name_class(pass(alice, bob), [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)]), g(owes, [p(carol), n(5)])], C10).
part_dom(pass(alice, bob, [i(carol, 2), i(carol, 5), i(dave, 1)]), alice, [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)]), g(owes, [p(carol), n(5)])], D11).
unable_reason(pass(alice, bob, [i(carol, 2), i(dave, 1), i(dave, 3)]), alice, [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)]), g(owes, [p(carol), n(5)])], R12).
apply_part(pass(alice, bob, [i(carol, 2), i(carol, 5), i(dave, 1)]), bob, [g(started, []), g(knows, [p(alice)])], S13).
class_info(pass(alice, bob, [i(carol, 2), i(carol, 5), i(dave, 1)]), alice, [], R14, A14).
iota_pairs(share(alice, bob, [carol, dave]), alice, I15).
name_class(tally(alice, bob), [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)]), g(owes, [p(carol), n(5)])], C16).
part_dom(tally(alice, bob, [carol, dave]), alice, [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)]), g(owes, [p(carol), n(5)])], D17).
unable_reason(tally(alice, bob, [carol, erin]), alice, [g(started, []), g(knows, [p(bob)]), g(owes, [p(dave), n(1)]), g(owes, [p(carol), n(2)])], R18).
apply_part(tally(alice, bob, [carol, dave]), bob, [g(started, []), g(knows, [p(alice)])], S19).
class_info(tally(alice, bob, [carol, dave]), alice, [], R20, A20).
part_dom(clear(alice, bob, [carol, dave]), bob, [g(started, []), g(knows, [p(alice)]), g(seen, [p(carol)])], D21).
apply_part(clear(alice, bob, [carol, dave]), bob, [g(started, []), g(knows, [p(alice)]), g(seen, [p(carol)]), g(seen, [p(erin)])], S22).
apply_part(settle(alice), alice, [g(started, []), g(noted, [p(carol)]), g(noted, [p(dave)])], S23).
apply_part(wipe(alice), alice, [g(settled, []), g(started, []), g(noted, [p(carol)]), g(noted, [p(dave)])], S24).
part_dom(wipe(alice), alice, [g(started, []), g(settled, [])], D25).
question_words(share(alice, bob, [carol, dave]), bob, W26).
part_dom(pour(alice, 2.5), alice, [g(started, [])], D27).
part_dom(pour(alice, -1.0), alice, [g(started, [])], D28).
apply_part(pour(alice, 2.5), alice, [g(started, [])], S29).
apply_part(drain(alice, 2.5), alice, [g(started, []), g(poured, [r(2.5)])], S30).
class_info(pour(alice, 2.5), alice, [], R31, A31).
:quit
HEREDOC
2>&1)
rm -rf "${cva_sh_dir:?}"
check "CVA shares: the platform emitted to GLP" "wrote .*vg_shares_.*platform.glp" "$cva_sh_emit"
check "CVA shares: the class term carries the list of instances after the schema's variables, List(Peer)" "^share(Peer, Peer, List(Peer))$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... and its listing, instances(K, Base, X, A, Phi, CAdd, CDel)" "instances(1, 1, \[pvar(x)\], atom(noted, \[pvar(x)\]), fcons(true), \[hm(2, atom(seen, \[pvar(x)\]))\], \[\])" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: the base finds the instances in its own state, the persons it noted, in the order of identifiers" "C1 = share(alice, bob, \[carol, dave\])" "$cva_sh"
check "CVA shares: ... none where it noted none" "C2 = share(alice, bob, \[\])" "$cva_sh"
check "CVA shares: the request name is the schema-binding, with no list" "N3 = pname(share(alice, bob))" "$cva_sh"
check "CVA shares: the conditional literal added at bob's role once for each instance, inst(1, J, x) standing for x at the J-th" "R4 = \[rs(guarded, \[\], \[\], \[atom(started, \[\]), atom(knows, \[role(2)\])\], \[\]), rs(guarded, \[atom(seen, \[inst(1, 2, pvar(x))\]), atom(seen, \[inst(1, 1, pvar(x))\]), atom(shared, \[role(1)\])\]" "$cva_sh"
check "CVA shares: ... the binding sending each to its instance's value" "B4 = \[b(role(1), p(alice)), b(role(2), p(bob)), b(inst(1, 1, pvar(x)), p(carol)), b(inst(1, 2, pvar(x)), p(dave))\]" "$cva_sh"
check "CVA shares: the class of the part: bob adds seen of each instance, and the schema-binding with the list is found among the class's changes" "R5 = cls(ci(share(alice, bob, \[carol, dave\]), \[delta(alice, \[\], \[\]), delta(bob, \[\], \[g(seen, \[p(dave)\]), g(seen, \[p(carol)\]), g(shared, \[p(alice)\])\])\], \[alice, bob\], \[pt(share(alice, bob, \[carol, dave\]), \[alice, bob\], proposed(\[alice\]))\], \[mate(share, .*b(inst(1, 1, pvar(x)), p(carol)), b(inst(1, 2, pvar(x)), p(dave))\]" "$cva_sh"
check "CVA shares: alice's domain holds where her state has exactly the instances of the list" "D6 = yes" "$cva_sh"
check "CVA shares: ... and not where it has others" "D7 = no" "$cva_sh"
check "CVA shares: her unable vote names the anchor of the instance she lacks and that of the one she has beyond the list" "R8 = unable(\[noted(dave)\], \[noted(erin)\])" "$cva_sh"
check "CVA shares: bob's effect adds seen of each instance" "S9 = \[g(seen, \[p(dave)\]), g(seen, \[p(carol)\]), g(shared, \[p(alice)\]), g(started, \[\]), g(knows, \[p(alice)\])\]" "$cva_sh"
check "CVA shares: a conjunct binding two variables, a person and a number, an instance an assignment of both, its type i(Peer, Integer) in the class term (vGLP 2f1dc92; vGLP #24, Code_inbox.md, 2026-11-07 17:19 UTC)" "^Inst_pass_1 ::= i(Peer, Integer)\.$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... pass's class term, List(Inst_pass_1) after its variables" "^pass(Peer, Peer, List(Inst_pass_1))$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... the base finds each assignment its anchor and its state give, carol owing 2 and 5 two instances" "C10 = pass(alice, bob, \[i(carol, 2), i(carol, 5), i(dave, 1)\])" "$cva_sh"
check "CVA shares: ... the base's domain holds at exactly those instances" "D11 = yes" "$cva_sh"
check "CVA shares: ... its unable vote names the anchor it lacks and the one it has beyond the list" "R12 = unable(\[owes(dave, 3)\], \[owes(carol, 5)\])" "$cva_sh"
check "CVA shares: ... bob's effect adds owed of each assignment" "S13 = \[g(owed, \[p(dave), n(1)\]), g(owed, \[p(carol), n(5)\]), g(owed, \[p(carol), n(2)\]), g(passed, \[p(alice)\])" "$cva_sh"
check "CVA shares: ... the class's schema-binding found among its changes, each variable at each instance an entry inst(1, J, X)" "R14 = cls(ci(pass(alice, bob, \[i(carol, 2), i(carol, 5), i(dave, 1)\]), .*b(inst(1, 1, pvar(x)), p(carol)), b(inst(1, 1, num(k)), n(2)), b(inst(1, 2, pvar(x)), p(carol)), b(inst(1, 2, num(k)), n(5)), b(inst(1, 3, pvar(x)), p(dave)), b(inst(1, 3, num(k)), n(1))\]" "$cva_sh"
# The introduction rule, the anchors and the instances restricted (vGLP fe12932, Section 5.4; vGLP #24, Code_inbox.md, 2026-11-07 18:52 UTC): "a schema-binding in which p's role requires an atom naming an agent c bound to a variable of the schema or of an instance of the part and the person q in another role gains an atom naming c" gives p the pair {q, c}, the anchor counting as an atom its holder requires, "so Alice introduces Bob and Dave at the commit"; tally's instances restricted to the variable its literal names, without repetition; clear's deleted conditional literal subtracted from a state that need not hold it; settle's and wipe's conjuncts local to alice's role, which finds the instances in its own state and applies the literals at each; and the approval question showing the anchors at the proposer where a tab lists their predicate (line 9).
check "CVA shares: the introduction rule gives alice, the base, the pairs of bob, who gains seen of each instance, and each person her instances bind" "I15 = \[pr(bob, carol), pr(bob, dave)\]" "$cva_sh"
check "CVA shares: tally's instances restricted to the variable its literal names, without repetition, carol owing twice one instance" "C16 = tally(alice, bob, \[carol, dave\])" "$cva_sh"
check "CVA shares: ... the class term List(Peer)" "^tally(Peer, Peer, List(Peer))$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... the base's domain holds at exactly those restricted instances" "D17 = yes" "$cva_sh"
check "CVA shares: ... its unable vote names the anchor it has beyond the list, and leaves out that of an instance it lacks, which binds no amount" "R18 = unable(\[\], \[owes(dave, 1)\])" "$cva_sh"
check "CVA shares: ... bob's effect adds owed_by of each instance once" "S19 = \[g(owed_by, \[p(dave)\]), g(owed_by, \[p(carol)\]), g(tallied, \[p(alice)\]), g(started, \[\]), g(knows, \[p(alice)\])\]" "$cva_sh"
check "CVA shares: ... the class's schema-binding found among its changes, one entry inst(1, J, x) for each instance" "R20 = cls(ci(tally(alice, bob, \[carol, dave\]), .*\[mate(tally, \[b(role(2), p(bob)), b(role(1), p(alice)), b(inst(1, 1, pvar(x)), p(carol)), b(inst(1, 2, pvar(x)), p(dave))\]" "$cva_sh"
check "CVA shares: clear's deleted conditional literal is not required: bob's domain holds where he holds seen of one instance and not the other" "D21 = yes" "$cva_sh"
check "CVA shares: ... and his effect subtracts seen of each instance, which his state need not hold, leaving the others" "S22 = \[g(cleared, \[p(alice)\]), g(started, \[\]), g(knows, \[p(alice)\]), g(seen, \[p(erin)\])\]" "$cva_sh"
check "CVA shares: settle's conjunct, local to alice's role, adds done of each instance she finds in her own state" "S23 = \[g(settled, \[\]), g(done, \[p(carol)\]), g(done, \[p(dave)\]), g(started, \[\]), g(noted, \[p(carol)\]), g(noted, \[p(dave)\])\]" "$cva_sh"
check "CVA shares: wipe's, local to her role, deletes noted at each instance" "S24 = \[g(wiped, \[\]), g(settled, \[\]), g(started, \[\])\]" "$cva_sh"
check "CVA shares: ... a local conjunct asks nothing of the domain: wipe holds where she noted no one" "D25 = yes" "$cva_sh"
check "CVA shares: the approval question shows bob the anchors alice requires, the tab Notes listing noted (line 9)" "W26 = \[person(alice), text(' shares the persons they noted.'), held(noted(carol), 1), held(noted(dave), 1)\]" "$cva_sh"
check "CVA shares: the local conjuncts printed with their roles, schema_locals" "^schema_locals(settle, \[local(1, \[pvar(x)\], atom(noted, \[pvar(x)\]), fcons(true), \[hm(1, atom(done, \[pvar(x)\]))\], \[\])\])$" "$(cat "$CVA/shares/platform.vglp")"
# Real variables (vGLP fb40cff, Section 5.4: "a value variable, whose type, a whole number or a real, the schema writes with it"; vGLP #24, Code_inbox.md, 2026-11-07, 5): pour's real v a field, drain's found in the state, the printed types Real, a real held r(X).
check "CVA shares: a real variable's place in the class term is of the type Real" "^pour(Peer, Real)$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... and its argument in the type Atom" "^poured(Real)$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: ... the binding sending num(v) to r(V)" "^binding_of(pour(V1, V2), pour, \[b(role(1), p(V1?)), b(num(v), r(V2?))\])$" "$(cat "$CVA/shares/platform.vglp")"
check "CVA shares: pour's constraint v > 0 holds at 2.5" "D27 = yes" "$cva_sh"
check "CVA shares: ... and not at -1.0" "D28 = no" "$cva_sh"
check "CVA shares: ... its effect adds poured(2.5)" "S29 = \[g(poured, \[r(2.5)\]), g(started, \[\])\]" "$cva_sh"
check "CVA shares: drain moves the real its state holds" "S30 = \[g(drained, \[r(2.5)\]), g(started, \[\])\]" "$cva_sh"
check "CVA shares: ... pour's class found among its changes, the real bound" "R31 = cls(ci(pour(alice, 2.5), \[delta(alice, \[\], \[g(poured, \[r(2.5)\])\])\]" "$cva_sh"
check_not "CVA shares: no goal left unread" "not well-typed\|→ failed" "$cva_sh"
# A variable the constraint determines from the variables local to a role is local to that role (vGLP 9ee6f87, Section 5.4: "and so is a variable the constraint determines from the variables local to that role and the variables the schema-binding binds"): vGLP's linear, climb's t1 = t + 2 and mark's 3 m = t + 3, t local to the first role, so t1 and m are local too, in no class term, the machine computing them.
cva_lin=$(cat "$CVA/linear/platform.vglp")
check "CVA numbers: a variable the constraint determines from a local variable is local, no place of the class term (linear, climb)" "^binding_of(climb(V1), climb, \[b(role(1), p(V1?)), b(num(t1), lv(climb, t1)), b(num(t), lv(climb, t))\])$" "$cva_lin"
check "CVA numbers: ... and mark's m (linear)" "^binding_of(mark(V1), mark, \[b(role(1), p(V1?)), b(num(m), lv(mark, m)), b(num(t), lv(mark, t))\])$" "$cva_lin"
# A guarded part with no opener has its proposers, the persons in the base of the schema-bindings that determine it, who alone request it and propose it (vGLP 9ee6f87, Definitions "Local Specification", "Attempt" and "Proposing"; Section 5.4).
check "CVA proposers: a guarded part with no opener is proposed(Ob), its proposers" "^Opener    ::= opener(Peer) ; proposed(Peers)\.$" "$cva_lin"
check "CVA proposers: ... the persons in the base of the schema-bindings of its class that guard it" "^    part_bases(All?, G?, Ob),$" "$cva_lin"
check "CVA proposers: ... and proposes it on an approval" "^    proposes_op(Op?, Id?, B),$" "$cva_lin"

# pragmas.sh reads qschema: a button names the schemas bearing its name with a suffix, qschema among them, GFF-I's [Follow] naming follow_friend and follow_transitive (q_button); and a variable the constraint determines from those the proposer's atoms bind and the other fields is no field: "A field is for a variable of the schema that those atoms do not bind and the constraint does not determine from the variables they bind and the other fields" (vGLP bcb2726, Section 5.4, "The contract's words"), GCC's cmint, i1 = i + k, its one field k (q_det).
cva_qp=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XQ' 'Alice, Bob and Carol are persons.' '1. ■ Following. Alice may follow.' '2. ■ Minting. Alice may mint.' 'Pragmas' 'Pointing: the lpoints to Alice has.' 'Following: Alice [Follow] <label>' 'Minting: Alice [Cmint] <amount>' > "$cva_qp/XQ.txt"
cva_qp_ff="schema(follow_friend, ['Alice', 'Bob'], [pvar('Dave')], [rs(guarded, [atom(lpoints_to, [role(2), pvar('Dave')])], [], [atom(friend, [role(2)])], [atom(lpoints_to, [role(2), pvar('Dave')])]), rs(unguarded, [], [], [atom(identity, [])], [])])"
cva_qp_ft="qschema(follow_transitive, ['Alice', 'Bob', 'Carol'], [pvar('Dave')], [], true, [rs(guarded, [atom(lpoints_to, [role(3), pvar('Dave')])], [], [atom(knows, [role(3)]), atom(lpoints_to, [role(2), pvar('Dave')])], [atom(lpoints_to, [role(3), pvar('Dave')])]), rs(unguarded, [], [], [atom(lpoints_to, [role(3), pvar('Dave')])], []), rs(unguarded, [], [], [atom(identity, [])], [])], [], [fnot(fand(fa(role(1), atom(friend, [role(3)])), fcons(eq(role(3), pvar('Dave')))))])"
cva_qp_cm="nschema(cmint, ['Alice'], [num(k), num(i), num(i1)], eq(num(i1), plus(num(i), num(k))), [rs(guarded, [times(num(k), atom(ccoin, [role(1)])), atom(issued, [num(i1)])], [atom(issued, [num(i)])], [atom(treasurer, [])], [])])"
cva_qp_out=$(PRAGMAS_LINES=1 bash "$CVA/compiler/pragmas.sh" "$cva_qp/XQ.txt" "[$cva_qp_ff, $cva_qp_ft, $cva_qp_cm]" 2>&1; echo "exit $?")
rm -rf "$cva_qp"
check "CVA pragmas: [Follow] names follow_transitive, a qschema, beside follow_friend (q_button)" "act('follow_transitive', '\"Follow\"', \[\], none, '\"Pointing\"', by(\[1\]))" "$cva_qp_out"
check "CVA pragmas: ... follow_friend's field the label, Dave (q_button)" "act('follow_friend', '\"Follow\"', \['\"\"', '\"\"', '\"label\"'\], none, '\"Pointing\"', by(\[1\]))" "$cva_qp_out"
check "CVA pragmas: cmint's one field, k, i1 determined by i1 = i + k (q_det)" "XQ: Minting: Alice \[Cmint\] <amount>: fields k$" "$cva_qp_out"
check "CVA pragmas: ... the act's field at k's place, none at i1's (q_det)" "act('cmint', '\"Cmint\"', \['\"\"', '\"amount\"'\], none, '\"Pointing\"', by(\[1\]))" "$cva_qp_out"
check "CVA pragmas: ... and pragmas.sh exits 0 (q_button, q_det)" "^exit 0$" "$cva_qp_out"
check "CVA compiler: CSSN's sibling case, a child role and an adult role at one person, two classes (sibling)" "%% not compiled, having no pragmas(sibling)" "$cva_classes"
check "CVA compiler: the adult role not forbidding child, one class, all marking the same people, passing the checks (sibling_noclash)" "%% not compiled, having no pragmas(sibling_noclash)" "$cva_classes"
check "CVA compiler: the parent of the adult role not marked, two classes, passing the checks (sibling_marks)" "%% not compiled, having no pragmas(sibling_marks)" "$cva_classes"
check "CVA compiler: ... and the adult role not forbidding child, one class, its parts guarded apart, passing the checks (sibling_noclash_marks)" "%% not compiled, having no pragmas(sibling_noclash_marks)" "$cva_classes"


# The compiled agent is no longer volitional: it asks for requests by request
# questions, goals of a volitional procedure whose question, of the request
# type Request?, takes one request, asked when the agent starts and again when
# the agent goal reads each answer (vGLP 12f629d, Section 5.4, "The
# compilation"; vGLP #23 Cowork, Code_inbox 2026-11-04 20:18 UTC); and its agent goal writes on the program's output to its person, Output, construct 0, its view, view(Id, S, K, A, Ts), its own key Id, the machine state S of its app state, the persons K it knows and the approvals A its person made that are pending, when it starts and whenever any changes (vGLP 1423679, Section 5.4, "The compilation"; at vGLP b7e3517, view(S, K)): "the compilation defines Output as the type of the views, and declares for the type Stream(Output) of the output a widget that draws the last view written" (vGLP b7e3517, Section 5.4, "The compilation"; vGLP #15, Code_inbox.md, 2026-10-19 10:55 UTC and 12:27 UTC).  A machine state is a list of the contract's atoms, Atom the union of the contract's predicates at their argument types, which the compiler prints from the contract, so that no Stream or List is under another (Definition "Volitional Procedure, Interactive Type"); the agent writes no transaction names, no outcome(At) and no discover(Q).  The plays' persons read the output from construct 0's draws, draw(0, W, output(T)), and from nowhere else.
# The standing approvals (vGLP f0acbc1, Section 3.2, "The protocol" and Definitions "Cargo, App State, Question, Approval, Free, Ready" to "Deciding"; Section 5.4, "Triggers" and "The compilation"; vGLP #18 Cowork to Code, 2026-10-20 10:44 UTC): an attempt (i, k, pi, [t]_j, r) names its request r or none; a vote no carries its reason, declined or unable(Lacks, Has); an abort its refuser and that vote's reason, and a yield is an outcome of its own; a request, request(Rho, K), is a cargo; and the notice, refused(Part, Q, D), is written when an approval the person made ends by a refusal, the notice yielded gone.  The social graph's platform, vGLP's coins and the currency's, each printed now, the currency's from GC.glp in GSC's current term form (Currencies 6499166c).
for c in social_graph coins currency; do
    cva_agent=$(cat "$CVA/$c/platform.vglp")
    # Every contract compiled carries pragmas, GSG and GC theirs and vGLP's coins the term beside it in contracts.glp: the view carries also the lists its app shows, view(S, K, A, Ts) (vGLP 1423679, Section 5.4, "The compilation" and "The contract's words"; vGLP ffcb093, ibid.: "the compiler prints a platform only from a contract with pragmas").
    cva_output='^Output    ::= view(Key, List(Atom), List(Key), List(Approved), List(Tab)) ; refused(Approved, Key, Reason)\.$'
    cva_start='^agent(Id, NetIn, Out?, \[view(Id?, \[\], \[\], \[\], Ts?)|Thr?\]) :- ground(Id?) |$'
    check "CVA approvals, $c: the agent is an ordinary procedure, no longer volitional" "^exported procedure agent(Peer?, Stream(Net)?, Stream(Net), Stream(Output))\.$" "$cva_agent"
    check_not "CVA approvals, $c: the request question is bare, of the request type Request? (vGLP 12f629d, Section 5.4, \"The compilation\"): no type Menu" "^Menu \|(menu(" "$cva_agent"
    check "CVA approvals, $c: ... asked by a volitional procedure whose question takes one request" "^procedure (Request?)\*request(Stream(Trigger))\.$" "$cva_agent"
    check "CVA approvals, $c: ... its one clause" "^(R)\*request(Out?) :-$" "$cva_agent"
    check "CVA approvals, $c: ... the widget declaration of the request question on its moded type, Request? (vGLP, Section 6.2, Definition \"Widget Declaration\": \"where \$T\$ is a moded type\")" "^Request? =::= social_menu(" "$cva_agent"
    check "CVA approvals, $c: an attempt names its request, or none" "^Attempt   ::= attempt(Peer, Integer, Integer, Class, Names)\.$" "$cva_agent"
    check "CVA approvals, $c: ... a request, the class of a guarded part with no opener the agent requests of itself, one(C), or rho_N, rho(N)" "^Rho       ::= one(Class) ; rho(Proposal)\.$" "$cva_agent"
    check "CVA approvals, $c: a vote no carries its reason, declined or unable, with the atoms the schema requires at the voter that its state lacks and those it forbids that its state has; and unable alone, the reason of the notice of a request dropped where the agent is ready for no part it names (vGLP, Section 5.4, \"The compilation\")" "^Reason    ::= declined ; unable ; unable(List(Atom), List(Atom))\.$" "$cva_agent"
    check "CVA approvals, $c: ... an abort its refuser and that vote's reason, and a yield is an outcome of its own" "^Decision  ::= commit ; abort(Peer, Reason) ; yield\.$" "$cva_agent"
    check "CVA approvals, $c: ... a request is a cargo, and so is a ready (vGLP 1d84a35)" "^            ; yield(Attempt) ; ready(Attempt) ; request(Rho, Integer)\.$" "$cva_agent"
    check "CVA approvals, $c: the record holds the requests made and the ends of approvals" "^            ; yielded(Attempt) ; requested(Rho, Integer) ; ended(Rho, Integer)$" "$cva_agent"
    check "CVA approvals, $c: the program's output to its person is of the type of the views, view(S, K, A), the approvals pending A beside the machine state and the persons known, each by its request name, Approved ::= Proposal, and, with pragmas, the lists, and the notices, refused(N, Q, D), N the request name of the refused attempt's part (vGLP e286276, Section 5.4, \"The compilation\")" "$cva_output" "$cva_agent"
    check "CVA approvals, $c: ... each approval by its request name, no attempt and no list in the view" "^Approved  ::= Proposal\.$" "$cva_agent"
    check "CVA approvals, $c: ... drawn by the widget last_state, which draws the last view written and the notices written after it" "^Stream(Output) =::= last_state\.$" "$cva_agent"
    check "CVA approvals, $c: on the answer propose(N) the dispatch takes a Request, holds the request, or drops it, a hello sent first where N names a meeting part with a peer neither known nor sent to" "^    dispatch_request(Cd?, Th?, Ap0?, Ls?, In?, Ans1, Thr1, Out1, Ow, Approvals?, H?, St1?)\.$" "$cva_agent"
    check "CVA approvals, $c: ... a request held put up again, again(N), and taken, held or dropped as on its reading" "^step(again(N), Th, Ls, In, Ans?, Thr?, Out?, Ow?, Approvals, H, St) :-$" "$cva_agent"
    check "CVA approvals, $c: ... the Request of {[t]} to the agent itself, one(C), C the term of the part the request names" "^    self_cand(B?, C?, Id?, Cs),$" "$cva_agent"
    check "CVA approvals, $c: ... the Request of rho_N to the opener of the guarded parts with an opener named N" "^    rho_cand(B?, N?, O?, Cs),$" "$cva_agent"
    check "CVA approvals, $c: ... the one taken least recently" "^    least_rcand(Xs?, Md?, X?, Y),$" "$cva_agent"
    check "CVA approvals, $c: Propose an own transaction, of a guarded part with no opener of the class of an approval pending at the agent" "^    guard_cands(St?, Csg, St1),$" "$cva_agent"
    check "CVA approvals, $c: ... and on a request the agent holds, naming it" "^    held_cands(St1?, Csh, St2),$" "$cva_agent"
    check "CVA approvals, $c: ... where its state is in the domain of no such part, of one of them, which it refuses at once" "^guard_ready(\[\], \[P|_\], I, \[cand(op(P?), prop(I?, P?, none))\]) :- ground(P?) | true\.$" "$cva_agent"
    check "CVA approvals, $c: a Propose that refuses at once tells abort with the agent as refuser" "^    tell(Q?, Id?, outcome(At?, abort(Id?, D?)), Ss),$" "$cva_agent"
    check "CVA approvals, $c: Tally of a no tells abort with the voter as refuser and its reason" "^    tell(Q?, Id?, outcome(At?, abort(F?, D?)), Ss),$" "$cva_agent"
    check "CVA approvals, $c: ... and the notice where an approval of the guard ends" "^    ended_notice(En?, C2?, F?, D?, Ns),$" "$cva_agent"
    check "CVA approvals, $c: a guard's vote no ends every approval of the class pending at it, with the notice, the agent its refuser" "^    ended_notice(En?, C2?, Id?, D?, Ns)[,.]$" "$cva_agent"
    check "CVA approvals, $c: an abort ends the request the attempt names, or the approvals of a guard that voted or answered yes, with the notice" "^    ended_notice(En?, C?, Q?, D?, Ns)[,.]$" "$cva_agent"
    check "CVA approvals, $c: Commit ends every approval of the class pending at the agent" "^    end_approvals(I?, Id?, \[closed(At?)|R?\], R1, _)\.$" "$cva_agent"
    check "CVA approvals, $c: ... and Commit and Outcome on commit delete every request of the class the agent holds" "^    drop_held_class(Hd?, I?, Id?, Hd1),$" "$cva_agent"
    check "CVA approvals, $c: Start: a guard whose request the attempt names votes yes if it is pending and no if not, unasked" "^    start_named(En?, I?, At?, Id?, Ap?, Ss, Ns, Ap1)\.$" "$cva_agent"
    check "CVA approvals, $c: ... a guard is asked only where no approval of the class is pending at it" "^start_asked(yes, no, At, app(S, K, L, R), \[\], approval_for(At?), \[\], app(S?, K?, L?, \[asked(At?)|R?\])) :-$" "$cva_agent"
    check "CVA approvals, $c: the agent's own Wound and Yield tell the outcome yield" "^    tell(Q?, Id?, outcome(At?, yield), Ss)\.$" "$cva_agent"
    check "CVA approvals, $c: ... the view written when the agent starts: the empty machine state, no person known and no approval pending" "$cva_start" "$cva_agent"
    check "CVA approvals, $c: ... and the new view, where the machine state changes or a peer becomes known, its atoms as the contract writes them" "^show_view(yes, St, \[V?|Thr1?\], Thr1) :- view_of(St?, V)\.$" "$cva_agent"
    check "CVA approvals, $c: ... the notices written after the view" "^    put_notices(Ns?, Thr1, Thr2?),$" "$cva_agent"
    check_not "CVA approvals, $c: no notice yielded, no noticed and no proposing on the request question" "yielded(Class)\|noticed(\|propose_cond(\|initiate_cond(" "$cva_agent"
    check_not "CVA approvals, $c: no session question" "Session" "$cva_agent"
done
# The widget of a question is the one the declaration in scope names on its moded type (vGLP, Section 6.2, Definitions "Widget Declaration" and "Default Widget"), so the compiled request question is drawn by social_menu and not by its default form: the platform emitted in a scratch directory beside the platforms, whose types it shares, and the directory removed.
cva_scratch="$CVA/moded_scratch"
rm -rf "$cva_scratch"; mkdir -p "$cva_scratch"
cp "$CVA/social_graph/platform.vglp" "$CVA/social_graph/self.glp" "$cva_scratch/"
"$REPL_RUN" > /dev/null 2>&1 <<HEREDOC
:emit $cva_scratch
:quit
HEREDOC
cva_emitted=$(cat "$cva_scratch/platform.glp" 2>/dev/null)
rm -rf "$cva_scratch"
check "CVA widget: the request question, of the moded type Request?, drawn by the widget its declaration names" "^construct(Id, request_r(X?), Gs, Ds?) :- .*named('social_menu(" "$cva_emitted"
check "CVA widget: ... and the approval question, of the moded type Approval, by approval_sentence" "^construct(Id, approval_w(X), Gs, Ds?) :- .*named(approval_sentence, " "$cva_emitted"
# The printed text run on its own: the emitted platform of the currency, GC, loaded, its procedures called with the terms they read.
# Held requests first (vGLP e22e2ad, Section 5.4, "The compilation": "puts up again every request of its person it holds, and then the starts it holds"; Code #24's trace of 2026-11-06, /Users/udi/Grassroots/tmp/trace-frank.txt: with two held requests the second fell behind a new recheck).
# The approval question (ibid., "The contract's words": "and the atoms the act requires of the proposer whose predicates the tabs list, as the tabs draw them --- in a swap, the coins the proposer gives").
cva_scratch="$CVA/words_scratch"
rm -rf "$cva_scratch"; mkdir -p "$cva_scratch"
cp "$CVA/currency/platform.vglp" "$CVA/currency/self.glp" "$cva_scratch/"
"$REPL_RUN" > /dev/null 2>&1 <<HEREDOC
:emit $cva_scratch
:quit
HEREDOC
cva_text=$("$REPL_RUN" 2>&1 <<HEREDOC
$cva_scratch/platform.glp
requests_first([again(establish(alice)), recheck, own, again(establish(bob)), recheck], R1).
requests_first([recheck, own], R2).
question_words(swap(alice, bob, alice, 2, bob, 3), bob, W1).
question_words(swap(alice, bob, carol, 2, bob, 3), bob, W2).
question_words(swap(bob, carol, alice, 2, bob, 3), bob, W3).
:quit
HEREDOC
)
rm -rf "$cva_scratch"
check "CVA held requests: every held request before the rest, unread requests before a new recheck" "R1 = \[again(establish(alice)), again(establish(bob)), recheck, own, recheck\]" "$cva_text"
check "CVA held requests: ... and the triggers left in their order where no request is held" "R2 = \[recheck, own\]" "$cva_text"
check "CVA next_n: each platform's own list, requests first" "^    requests_first(L0?, L1),$" "$(cat "$CVA/currency/platform.vglp")"
check "CVA approval: Bob's question for a swap shows after the values the coins Alice gives, with their count, held(coin(alice), 2), and the atoms of the other tabs her role requires" 'W1 = \[person(alice), text(.*valued(.amount wanted., 3), held(knows(bob), 1), held(coin(alice), 2)\]' "$cva_text"
check "CVA approval: ... under the attempt's binding, coins of carol" 'W2 = .*held(coin(carol), 2)\]' "$cva_text"
check_not "CVA approval: ... and no atom shown to a person the question is not put to" 'W3 = .*held(' "$cva_text"
check "CVA widget: a role's place in Roles and an agent variable's not, introduce's Carol at place 3 (vGLP e22e2ad, Section 5.4: \"distinct roles bound to distinct persons\"; vGLP #23, Code_inbox.md, 2026-11-06 12:00 UTC, 1)" 'button("Introduce", .act(introduce, 1, .1, 2., ' "$(cat "$CVA/social_graph/platform.vglp")"
cva_poss=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(poss, [schema(s, [], [rs(guarded, [atom(x, [])], [], [], []), rs(unguarded, [atom(y, [])], [], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Xs"', [x])], [act(s, '"S"', [], none, '"Xs"', by([1], [same(1, 2)]))])).
:quit
HEREDOC
2>&1)
check "CVA widget: two places possibly one person, in Same, both in Roles (poss)" 'button("S", .act(s, 1, .1, 2., .*.same(1, 2)., ..).)' "$cva_poss"
cva_sg_refresh=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_refresh(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA refresh: a person's refresh, a grant addressed to no construct, on which the dispatcher has every construct and construct 0 draw again what they last drew, the agent writing no view on it, its last view written once; the request question still open, she proposes acquaint (vGLP d433f71, Sections 5.4 and 6.3)" "A = \[.*view(alice, \[identity\], \[\], \[\], \[[^]]*\]), view(alice, \[identity\], \[\], \[acquaint(alice, bob)\], .*view(alice, \[knows(bob), identity\]" "$cva_sg_refresh"
check_not "CVA refresh: ... no failed goal" "ERROR" "$cva_sg_refresh"
check_not "CVA refresh: ... no message sent for it, a stutter" "sent(alice, bob, refresh\|attempt(alice, [0-9]*, [0-9]*, refresh" "$cva_sg_refresh"
# What the loop above does not check of the currency's platform: the request question asked with the agent's own key, when the agent starts and on reading each answer; a peer becoming known at the second hello of an exchange; and nothing on the output but the views and the notices.
cva_agent=$(cat "$CVA/currency/platform.vglp")
check "CVA currency: ... the request question asked bare when the agent starts, the key in the view" "^    request(Rq),$" "$cva_agent"
check "CVA currency: ... and on reading each answer" "^    request(R),$" "$cva_agent"
check "CVA currency: ... every view carries the agent's own key" "^view_of(St, view(Id?, A?, Ks?, Ps?, Ts?)) :- ground(St?) |$" "$cva_agent"
check_not "CVA currency: no stream of requests as the agent's question" "(Stream(Request)?)\*agent" "$cva_agent"
check "CVA currency: ... a peer becoming known at the second hello of an exchange" "^    show_known(Q?, St?, St1?, Thr, Thr1?, St2),$" "$cva_agent"
check_not "CVA currency: ... no transaction names, no outcome(At) and no discover(Q) on the output" "^Txn \|discover_q\|\[discover(\|\[outcome(At" "$cva_agent"
cva_person=$(cat "$CVA/self.glp")
check "CVA plays: the person reads the views from construct 0" "^person(\[draw(0, _, output(T))|Ds\]" "$cva_person"
check_not "CVA plays: ... and no session" "session_w" "$cva_person"
check "CVA plays: the person reads the request questions, bare, the agent's own key and the persons it knows off the views" "^person(\[draw(Id, _, request_r(input(P)))|Ds\]" "$cva_person"
check "CVA plays: the screen shows a notice written after the last view, refused(P, Q, D) (vGLP f0acbc1)" "^shows(refused(P, Q, D), _, T, _, B?) :- ground(P?), ground(Q?), ground(D?), ground(T?) |$" "$cva_person"
check_not "CVA plays: ... and no notice yielded (vGLP f0acbc1: the notice yielded goes)" "^shows(yielded(" "$cva_person"
check_not "CVA plays: no step makes a request again until the screen shows something" "again(" "$cva_person"
# A scripted person proposing a class that names its agent names it by the
# key the request question shows (vGLP 321127a, Section 5.4): its step own(Ks,
# R) holds a hole for each place R names the agent, and the person writes the
# key into each before it grants R.
check "CVA plays: a proposal naming the agent is granted once the person has written the key the view shows into its holes" "^advance(ps(sess(Id, P), \[view(Me, S, K, A, Ts)|Vs\], Cs, \[own(Ks, R)|Ss\])" "$cva_person"
check "CVA plays: ... each hole the agent's own key" "^fill(Me, \[Me?|Ks\]) :- ground(Me?) | fill(Me?, Ks?)\.$" "$cva_person"
check "CVA plays: alice establishes her identity by the key her request question shows" "pair(\[look, own(\[A1\], propose(establish(A1?)))" "$(cat "$CVA/social_graph/self.glp")"

# The request type has the alternative propose(N) for the request name N of each guarded part (vGLP 1423679, Section 5.4, "The compilation"; the request known(Q) gone, vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 1): Proposal, the alternatives of the schemas marking a role some
# binding of which determines a transaction (compiler/local.glp, proposable).
# nudge's nudge is unguarded and not proposed; gnudge's g is a guarded part of
# a class whose other part, nudge's, is unguarded, and is proposed (vGLP #15,
# Code_inbox.md, 2026-10-19 10:50 UTC, item 4).  A contract with no guarded
# part has no request: no request type, no request question and no clause on
# the request propose([t]_j) (vGLP d433f71, Section 5.4, "The compilation": "a
# type with the alternative propose(N) for the request name N of each guarded
# part"; the request type loses refresh, a refresh being a grant, Section 6.3).
cva_proposal() { # cva_proposal <platform.vglp>: the alternatives of Proposal, one line
    awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$1"
}
check "CVA request type: the social graph's, propose(Proposal) alone, no refresh (vGLP d433f71, Section 6.3): the request known(Q) goes, a meeting with a peer whose key the person has being the cold call (vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 1; vGLP e22e2ad, Section 5.4, \"The compilation\")" "^Request ::= propose(Proposal)\.$" "$(cat "$CVA/social_graph/platform.vglp")"
check_not "CVA request type: ... no request known(Q) in any platform" "^Request ::=.*known(" "$(cat "$CVA"/*/platform.vglp)"
check "CVA request type: the social graph proposes establish, acquaint, introduce, befriend and unfriend, GKK's and GSG's" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) befriend(Peer, Peer) unfriend(Peer, Peer) $" "$(cva_proposal "$CVA/social_graph/platform.vglp")"
check "CVA request type: nudge proposes befriend, not the unguarded nudge" "^befriend(Peer, Peer) $" "$(cva_proposal "$CVA/nudge/platform.vglp")"
check "CVA request type: gnudge proposes befriend and g, a guarded part of one class with the unguarded part of nudge, and not nudge" "^befriend(Peer, Peer) g(Peer, Peer) $" "$(cva_proposal "$CVA/gnudge/platform.vglp")"
check "CVA request type: opens proposes befriend, mark and rmark, and not the unguarded u1 and u2" "^befriend(Peer, Peer) mark(Peer, Peer) rmark(Peer, Peer) $" "$(cva_proposal "$CVA/opens/platform.vglp")"
cva_ung=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(ung, [schema(u, [], [rs(unguarded, [atom(b, [])], [atom(a, [])], [], [])])], syntactically_grassroots, no_introduction, pragmas([list('"Bs"', [b])], [])).
:quit
HEREDOC
2>&1)
check "CVA request type: a contract with no guarded class, compiled" "%% the canonical CVA platform of the contract(ung)" "$cva_ung"
check_not "CVA request type: ... no request type, no part being guarded" "^Request ::=" "$cva_ung"
check_not "CVA request type: ... and no widget declaration for a request question" "^Request? =::=" "$cva_ung"
check_not "CVA request type: ... and no request question asked" "request(Rq)\|ask_next" "$cva_ung"
check "CVA request type: ... its agent reading the arriving messages, its approvals' answers and its own triggers" "^    merge(\[\], Nt?, M1),$" "$cva_ung"
check_not "CVA request type: ... no request refresh in any platform" "^Request ::=.*refresh" "$(cat "$CVA"/*/platform.vglp)"
check_not "CVA request type: ... no Proposal, and no clause on the request propose([t])" "^Proposal ::=\|step(req(propose\|await_request(propose" "$cva_ung"

cva_sg_menu=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_menu(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA request question: the first, asked when the agent starts, shows the agent's own key and no person known" "A = \[view(alice, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), menu(alice, \[\]), view(alice, \[\], \[\], \[establish(alice)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\])" "$cva_sg_menu"
check "CVA request question: the one asked on reading the request to befriend bob, once bob is known and met, shows the key and bob" "view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), menu(alice, \[bob\])" "$cva_sg_menu"

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
check "CVA befriend: alice's screen: each request pending until it commits; her identity, established at its Propose; bob, once known, the cold call of her acquaint answered; knows(bob), once they come to know each other (GKK:2); and bob her friend, once her attempt commits" "A = \[view(alice, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[\], \[\], \[establish(alice)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[bob\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_befriend"
check "CVA befriend: bob is asked on alice's hello of acquaint, a meeting schema (vGLP, Section 5.4, \"The compilation\"), and answers yes, his request of it; then, in befriend's guard set, is asked on its start, answers yes, his answer pending, and sees alice his friend once the outcome commits" "B = \[view(bob, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[\], \[\], \[establish(bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), gave(acquaint(alice, bob), yes), view(bob, \[identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[identity\], \[alice\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), gave(befriend(alice, bob), yes), view(bob, \[knows(alice), identity\], \[alice\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_befriend"
check "CVA befriend: the acquaintance's two-phase commit with both approvals standing, bob's attempt wounded in favour of alice's, no second question; then the start of befriend to bob, his yes and the commit, her attempt numbered after her Requests, establish's, acquaint's and befriend's, and her acquaint's attempt (vGLP f0acbc1, Definition \"Proposing\", Request)" "N = \[sent(alice, bob, start(attempt(alice, 4, 4, acquaint(alice, bob), none))), sent(bob, alice, start(attempt(bob, 4, 4, acquaint(alice, bob), none))), sent(bob, alice, outcome(attempt(bob, 4, 4, acquaint(alice, bob), none), yield)), sent(bob, alice, vote(attempt(alice, 4, 4, acquaint(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 4, 4, acquaint(alice, bob), none), commit)), sent(alice, bob, start(attempt(alice, 6, 6, befriend(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 6, 6, befriend(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 6, 6, befriend(alice, bob), none), commit))" "$cva_sg_befriend"

cva_sg_unfriend=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_unfriend(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA unfriend: bob, his number the largest he has read, requests and proposes unfriending alice, attempt 8, once he sees her his friend, and alice's machine votes yes" "sent(alice, bob, outcome(attempt(alice, 6, 6, befriend(alice, bob), none), commit)), sent(bob, alice, start(attempt(bob, 8, 8, unfriend(bob, alice), none))), sent(alice, bob, vote(attempt(bob, 8, 8, unfriend(bob, alice), none), yes))" "$cva_sg_unfriend"
check "CVA unfriend: alice, unasked, sees bob end the friendship, befriends him again once she sees it ended, and sees her own unfriend, each request pending until it commits" "A = \[.*view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[unfriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_unfriend"
check "CVA unfriend: ... its yes, and bob's unfriend committed" "sent(alice, bob, vote(attempt(bob, 8, 8, unfriend(bob, alice), none), yes)), sent(bob, alice, outcome(attempt(bob, 8, 8, unfriend(bob, alice), none), commit))" "$cva_sg_unfriend"
check "CVA unfriend: befriended again, attempt 10, alice proposes unfriending bob, attempt 12, and bob's machine votes on it, unasked" "sent(alice, bob, start(attempt(alice, 12, 12, unfriend(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 12, 12, unfriend(alice, bob), none), yes))" "$cva_sg_unfriend"
check "CVA unfriend: ... bob's screen: the friendship ended by him, made again by alice, and ended by her" "B = \[.*gave(befriend(alice, bob), yes), view(bob, \[knows(alice), identity\], \[alice\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[unfriend(bob, alice)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), gave(befriend(alice, bob), yes), view(bob, \[knows(alice), identity\], \[alice\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_unfriend"
check "CVA unfriend: ... and alice's unfriend committed" "sent(alice, bob, outcome(attempt(alice, 12, 12, unfriend(alice, bob), none), commit))" "$cva_sg_unfriend"
check_not "CVA unfriend: no person answers for unfriending" "gave(attempt([a-z]*, [0-9]*, [0-9]*, unfriend\|gave(unfriend" "$cva_sg_unfriend"

cva_sg_refused=$("$REPL_RUN" <<HEREDOC
$CVA/social_graph/
:limit 600000
play_refused(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA refused: bob answers alice's first attempt no and her second yes, and sees her his friend once it commits" "B = \[.*view(bob, \[knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), gave(befriend(alice, bob), no), gave(befriend(alice, bob), yes), view(bob, \[knows(alice), identity\], \[alice\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_refused"
check "CVA refused: ... his vote no, declined, and alice tallies it and tells bob abort, bob its refuser, declined (vGLP f0acbc1, Definition \"Cargo, App State, Question, Approval, Free, Ready\")" "sent(bob, alice, vote(attempt(alice, 6, 6, befriend(alice, bob), none), no(declined))), sent(alice, bob, outcome(attempt(alice, 6, 6, befriend(alice, bob), none), abort(bob, declined)))" "$cva_sg_refused"
check "CVA refused: aborted, her approval ended by bob's refusal, alice, seeing the notice, requests befriending bob again, attempt 8, on which bob is asked again and votes yes" "sent(alice, bob, outcome(attempt(alice, 6, 6, befriend(alice, bob), none), abort(bob, declined))), sent(alice, bob, start(attempt(alice, 8, 8, befriend(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 8, 8, befriend(alice, bob), none), yes))" "$cva_sg_refused"
check "CVA refused: ... and the second attempt commits" "sent(alice, bob, outcome(attempt(alice, 8, 8, befriend(alice, bob), none), commit))" "$cva_sg_refused"
check "CVA refused: alice's screen: her request gone and the notice refused(befriend(alice, bob), bob, declined) once bob's refusal ends her approval, her request again pending, and bob her friend once the second commits" "A = \[view(alice, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[\], \[\], \[establish(alice)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[identity\], \[bob\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]), refused(befriend(alice, bob), bob, declined), view(alice, \[knows(bob), identity\], \[bob\], \[befriend(alice, bob)\], \[tab('Contacts', knows), tab('Friends', friend)\]), view(alice, \[friend(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Friends', friend)\]) |" "$cva_sg_refused"
check_not "CVA refused: ... no third attempt" "attempt(alice, 9, \|attempt(alice, 1[0-9], " "$cva_sg_refused"

cva_cur_load=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:quit
HEREDOC
2>&1)
check "CVA currency: the platform and its plays load, compiled by the canonical compilation" "Loaded program: .*cva/currency" "$cva_cur_load"
check_not "CVA currency: no type errors" "Type checking failed" "$cva_cur_load"

# The plays of GC, which extends GKK (Currencies 6499166c): each person
# establishes their identity, which every other role requires, and mints, and
# alice and bob come to know each other by acquaint before they swap, pay or redeem (GKK:1, GKK:2, GC:2 to
# GC:4).  GC's schemas carry amounts (Currencies 72cf826b): mint of k coins,
# swap of k for k1, payment of k, redemption of one coin; a numeric variable
# bound by the schema-binding is named by the request, and a multiplicity k
# adds or deletes k copies (vGLP 660834d, Section 5.4, "Act schemas as local
# specifications").  mint(P, K): P mints K coins; swap(P, Q, U, K, V, K1): P
# gives K coins of U and takes K1 of V; pay(P, Q, K): P pays Q K coins of Q.
check "CVA request type: the currency proposes GKK's three and GC's four, mint, swap and pay naming their amounts and redeem one coin" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) mint(Peer, Integer) swap(Peer, Peer, Peer, Integer, Peer, Integer) pay(Peer, Peer, Integer) redeem(Peer, Peer, Peer) $" "$(cva_proposal "$CVA/currency/platform.vglp")"
cva_cur_payswap=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 600000
play_pay_swap(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA pay and swap: alice's establish and her mint of 2 coins, each a class of one participant, applied at its Propose: her identity and her 2 coins on her screen before bob is known" "A = \[view(alice, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[\], \[\], \[establish(alice)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[identity\], \[\], \[mint(alice, 2)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), coin(alice), identity\], \[\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), coin(alice), identity\], \[\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), coin(alice), identity\], \[bob\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\])" "$cva_cur_payswap"
check "CVA pay and swap: alice requests acquaint(alice, bob), a cold call, the hello on which bob is asked and answers yes, his request of it; both propose it, attempt 6 each, bob's wounded in favour of alice's, and it commits, no second question asked" "sent(alice, bob, start(attempt(alice, 6, 6, acquaint(alice, bob), none))), sent(bob, alice, start(attempt(bob, 6, 6, acquaint(alice, bob), none))), sent(bob, alice, outcome(attempt(bob, 6, 6, acquaint(alice, bob), none), yield)), sent(bob, alice, vote(attempt(alice, 6, 6, acquaint(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 6, 6, acquaint(alice, bob), none), commit))" "$cva_cur_payswap"
check "CVA pay and swap: ... bob's screen: his answer to the hello, alice known once his hello goes back, his request pending, and knows(alice) once it commits" "B = \[.*gave(acquaint(alice, bob), yes), view(bob, \[coin(bob), coin(bob), coin(bob), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[coin(bob), coin(bob), coin(bob), identity\], \[alice\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[knows(alice), coin(bob), coin(bob), coin(bob), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\])" "$cva_cur_payswap"
check "CVA pay and swap: ... alice's screen: bob known once his hello comes back, and knows(bob) once it commits" "A = \[.*view(alice, \[coin(alice), coin(alice), identity\], \[bob\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[knows(bob), coin(alice), coin(alice), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\])" "$cva_cur_payswap"
check "CVA pay and swap: alice proposes swapping 2 of her coins for 3 of bob's, and bob is asked, answers yes, his answer pending, and holds her 2 coins and none of his own once it commits" "B = \[.*gave(swap(alice, bob, alice, 2, bob, 3), yes), view(bob, \[knows(alice), coin(bob), coin(bob), coin(bob), identity\], \[alice\], \[swap(alice, bob, alice, 2, bob, 3)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[coin(alice), coin(alice), knows(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\])" "$cva_cur_payswap"
check "CVA pay and swap: ... and the swap commits" "sent(alice, bob, outcome(attempt(alice, 8, 8, swap(alice, bob, alice, 2, bob, 3), none), commit))" "$cva_cur_payswap"
check "CVA pay and swap: ... alice's screen: her request pending, and bob's 3 coins and none of hers once it commits" "A = \[.*view(alice, \[knows(bob), coin(alice), coin(alice), identity\], \[bob\], \[swap(alice, bob, alice, 2, bob, 3)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\])" "$cva_cur_payswap"
check "CVA pay and swap: alice pays bob 2 of his coins, guarded by her, and bob's machine votes yes, unasked, and it commits" "sent(alice, bob, start(attempt(alice, 10, 10, pay(alice, bob, 2), none))), sent(bob, alice, vote(attempt(alice, 10, 10, pay(alice, bob, 2), none), yes)), sent(alice, bob, outcome(attempt(alice, 10, 10, pay(alice, bob, 2), none), commit))" "$cva_cur_payswap"
check "CVA pay and swap: bob, unasked, holds 2 of his coins again once the pay commits" "B = \[.*view(bob, \[coin(alice), coin(alice), knows(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[coin(bob), coin(bob), coin(alice), coin(alice), knows(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]) |" "$cva_cur_payswap"
check "CVA pay and swap: ... and alice holds the one coin of bob's she kept" "A = \[.*view(alice, \[coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[pay(alice, bob, 2)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]) |" "$cva_cur_payswap"

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
check "CVA redeem: the swap back, alice proposing the swaps' part her request names, guarded by alice and bob, attempt 10: bob is asked and answers yes" "sent(alice, bob, start(attempt(alice, 10, 10, swap(alice, bob, bob, 1, alice, 1), none)))" "$cva_cur_redeem"
check "CVA redeem: ... his answer" "B = \[.*gave(swap(alice, bob, bob, 1, alice, 1), yes)" "$cva_cur_redeem"
check "CVA redeem: ... it commits" "sent(bob, alice, vote(attempt(alice, 10, 10, swap(alice, bob, bob, 1, alice, 1), none), yes)), sent(alice, bob, outcome(attempt(alice, 10, 10, swap(alice, bob, bob, 1, alice, 1), none), commit))" "$cva_cur_redeem"
check "CVA redeem: after a second swap, attempt 12, alice requests the redemption, redeem(alice, bob, alice), and her machine proposes her redeem's part, the part her request names, attempt 14 (vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 4)" "sent(alice, bob, outcome(attempt(alice, 12, 12, swap(alice, bob, alice, 1, bob, 1), none), commit)), sent(alice, bob, start(attempt(alice, 14, 14, redeem(alice, bob, alice), none)))" "$cva_cur_redeem"
check "CVA redeem: ... alice's screen: her coin and bob's exchanged at each commit, each request pending until it commits, her own coin back at the last" "A = \[.*view(alice, \[coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), knows(bob), knows(bob), identity\], \[bob\], \[swap(alice, bob, bob, 1, alice, 1)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), knows(alice), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), knows(alice), knows(bob), knows(bob), identity\], \[bob\], \[swap(alice, bob, alice, 1, bob, 1)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), knows(bob), knows(alice), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(bob), knows(bob), knows(alice), knows(bob), knows(bob), identity\], \[bob\], \[redeem(alice, bob, alice)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[coin(alice), knows(alice), knows(bob), knows(alice), knows(bob), knows(bob), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]) | " "$cva_cur_redeem"
check "CVA redeem: ... guarded by alice: bob's machine votes yes, unasked, and it commits" "sent(bob, alice, vote(attempt(alice, 14, 14, redeem(alice, bob, alice), none), yes)), sent(alice, bob, outcome(attempt(alice, 14, 14, redeem(alice, bob, alice), none), commit)) | " "$cva_cur_redeem"
check_not "CVA redeem: bob answers nothing for the redeem" "gave(attempt(alice, 14, 14, redeem\|gave(redeem" "$cva_cur_redeem"

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
# no Wound; bob answers her approval question no, his vote no declined, her
# swap aborts, bob its refuser, and the pay's start, put up again, is taken
# and the pay commits at its first attempt (vGLP f0acbc1, Definitions "Cargo,
# App State, Question, Approval, Free, Ready" to "Deciding").
check "CVA busy: alice proposes a swap, attempt 10; bob, asked, leaves his approval question open, proposes a pay, attempt 12, and answers no once the request question is drawn again" "sent(alice, bob, start(attempt(alice, 10, 10, swap(alice, bob, alice, 1, bob, 1), none))), sent(bob, alice, start(attempt(bob, 12, 12, pay(bob, alice, 1), none))), sent(bob, alice, vote(attempt(alice, 10, 10, swap(alice, bob, alice, 1, bob, 1), none), no(declined)))" "$cva_cur_busy"
check "CVA busy: ... his answer" "B = \[.*gave(swap(alice, bob, alice, 1, bob, 1), no)" "$cva_cur_busy"
check "CVA busy: alice holds the pay's start waiting while her swap holds her lock: her vote on it comes only after her swap's outcome" "sent(alice, bob, outcome(attempt(alice, 10, 10, swap(alice, bob, alice, 1, bob, 1), none), abort(bob, declined))), sent(alice, bob, vote(attempt(bob, 12, 12, pay(bob, alice, 1), none), yes))" "$cva_cur_busy"
check_not "CVA busy: ... no vote no on the pay, and no Wound: no yield asked" "vote(attempt(bob, 12, 12, pay(bob, alice, 1), none), no\|yield(" "$cva_cur_busy"
check "CVA busy: bob answers alice's approval question no, his vote no declined, and her swap aborts, bob its refuser" "sent(bob, alice, vote(attempt(alice, 10, 10, swap(alice, bob, alice, 1, bob, 1), none), no(declined))), sent(alice, bob, outcome(attempt(alice, 10, 10, swap(alice, bob, alice, 1, bob, 1), none), abort(bob, declined)))" "$cva_cur_busy"
check "CVA busy: the lock released, alice's machine votes yes on the waiting pay, and it commits" "sent(alice, bob, vote(attempt(bob, 12, 12, pay(bob, alice, 1), none), yes)), sent(bob, alice, outcome(attempt(bob, 12, 12, pay(bob, alice, 1), none), commit))" "$cva_cur_busy"
check "CVA busy: alice's screen: the notice refused(swap(alice, bob, alice, 1, bob, 1), bob, declined) once bob's refusal ends her approval, and her coin paid back to her once the pay commits" "A = \[.*view(alice, \[coin(bob), knows(bob), knows(bob), coin(alice), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), refused(swap(alice, bob, alice, 1, bob, 1), bob, declined), view(alice, \[coin(alice), coin(bob), knows(bob), knows(bob), coin(alice), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]) |" "$cva_cur_busy"

cva_cur_declined=$("$REPL_RUN" <<HEREDOC
$CVA/currency/
:limit 600000
play_declined(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA declined: bob, not holding the coin, votes on the start without asking his person, his screen unchanged since he came to know alice" "B = \[view(bob, \[\], \[\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[\], \[\], \[establish(bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[identity\], \[\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), gave(acquaint(alice, bob), yes), view(bob, \[identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[identity\], \[alice\], \[acquaint(alice, bob)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(bob, \[knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]) |" "$cva_cur_declined"
check "CVA declined: ... no, unable, the coin his state lacks named, and it aborts at both, bob its refuser" "sent(bob, alice, vote(attempt(alice, 8, 8, swap(alice, bob, alice, 1, carol, 1), none), no(unable(\[coin(carol)\], \[\])))), sent(alice, bob, outcome(attempt(alice, 8, 8, swap(alice, bob, alice, 1, carol, 1), none), abort(bob, unable(\[coin(carol)\], \[\]))))" "$cva_cur_declined"
check "CVA declined: ... alice's screen: her request gone and the notice of her approval ended by bob's refusal" "A = \[.*view(alice, \[knows(bob), coin(alice), identity\], \[bob\], \[swap(alice, bob, alice, 1, carol, 1)\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), view(alice, \[knows(bob), coin(alice), identity\], \[bob\], \[\], \[tab('Contacts', knows), tab('Coins', coin, counted)\]), refused(swap(alice, bob, alice, 1, carol, 1), bob, unable(\[coin(carol)\], \[\])) |" "$cva_cur_declined"
check_not "CVA declined: no approval question answered for the swap" "gave(swap\|gave(attempt(alice, 8, " "$cva_cur_declined"

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
check "CVA nudge: alice and bob befriended, committed at both" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob), none), commit))" "$cva_nudge"
check "CVA nudge: bob is asked on alice's hello, befriend a meeting schema (vGLP, Section 5.4, \"The compilation\"), and his yes is his request of it, alice known once his hello goes back, the request pending until it commits" "B = \[view(bob, \[\], \[\], \[\], \[tab('Friends', friend)\]), gave(befriend(alice, bob), yes), view(bob, \[\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[\], \[alice\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(bob, \[friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\])" "$cva_nudge"
check "CVA nudge: ... the two-phase commit of the meeting runs with both approvals standing: each machine proposes it, bob's attempt wounded in favour of alice's, of the smaller rank, and bob votes yes unasked" "sent(alice, bob, start(attempt(alice, 2, 2, befriend(alice, bob), none))), sent(bob, alice, start(attempt(bob, 2, 2, befriend(alice, bob), none))), sent(bob, alice, outcome(attempt(bob, 2, 2, befriend(alice, bob), none), yield)), sent(bob, alice, vote(attempt(alice, 2, 2, befriend(alice, bob), none), yes))" "$cva_nudge"
check_not "CVA nudge: ... no second question: bob is not asked on alice's attempt" "gave(attempt(alice, 2, 2, befriend" "$cva_nudge"
check "CVA nudge: alice, its opener, proposes nudge(alice, bob), an own transaction, once befriended" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob), none), commit)), sent(alice, bob, start(attempt(alice, 3, 3, nudge(alice, bob), none)))" "$cva_nudge"
check "CVA nudge: ... alice's screen: her request of befriend pending, bob known once his hello comes back, bob her friend once it commits and her request no longer pending, then her nudge of him, then his of her, each once committed" "A = \[view(alice, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[\], \[\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[\], \[bob\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), view(alice, \[nudged(bob), friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), view(alice, \[nudged_by(bob), nudged(bob), friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]) |" "$cva_nudge"
check "CVA nudge: bob, its opener, proposes nudge(bob, alice), an own transaction, once befriended, of the same rank" "sent(bob, alice, start(attempt(bob, 3, 3, nudge(bob, alice), none)))" "$cva_nudge"
check "CVA nudge: bob, holding his attempt prepared, wounds it in favour of alice's, which has priority: it yields before he takes her start, the outcome yield (vGLP f0acbc1, Definition \"Starting and Wounding\", Wound)" "sent(bob, alice, start(attempt(bob, 3, 3, nudge(bob, alice), none))), sent(bob, alice, outcome(attempt(bob, 3, 3, nudge(bob, alice), none), yield))" "$cva_nudge"
check "CVA nudge: ... his yield told before his vote on her start, yes" "sent(bob, alice, outcome(attempt(bob, 3, 3, nudge(bob, alice), none), yield)), sent(bob, alice, vote(attempt(alice, 3, 3, nudge(alice, bob), none), yes))" "$cva_nudge"
check_not "CVA nudge: an opener's attempt, bob's nudge wounded by himself, gives no notice, a yield ending no approval (vGLP f0acbc1)" "refused(" "$cva_nudge"
check_not "CVA nudge: no person answers for the unguarded class" "gave(attempt([a-z]*, [0-9]*, [0-9]*, nudge\|gave(nudge" "$cva_nudge"
check_not "CVA nudge: no vote no for contention" "vote(attempt([a-z]*, [0-9]*, [0-9]*, nudge([a-z]*, [a-z]*), none), no)" "$cva_nudge"
check "CVA nudge: alice's nudge committed at both" "sent(alice, bob, outcome(attempt(alice, 3, 3, nudge(alice, bob), none), commit))" "$cva_nudge"
check "CVA nudge: bob retries, his attempt 4 keeping the wounded one's rank 3, and it commits at both" "sent(bob, alice, outcome(attempt(bob, 4, 3, nudge(bob, alice), none), commit))" "$cva_nudge"
check_not "CVA nudge: nothing aborted for ever: no third attempt at either nudge" "attempt(alice, [4-9], [0-9]*, nudge\|attempt(bob, [5-9], [0-9]*, nudge" "$cva_nudge"
check_not "CVA nudge: befriend and nudge, binary, introduce no one at their commits (vGLP e2bc4f4, Definition \"Deciding\", Commit)" "intro(" "$cva_nudge"

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
check "CVA introduce: alice, befriended with both, requests and proposes introducing bob and carol, attempt 6, its start told to both" "sent(alice, bob, start(attempt(alice, 6, 6, introduce(alice, bob, carol), none))), sent(alice, carol, start(attempt(alice, 6, 6, introduce(alice, bob, carol), none)))" "$cva_intro"
check "CVA introduce: bob, asked, answers no, his vote no declined, and alice tells both abort, bob its refuser, declined" "sent(bob, alice, vote(attempt(alice, 6, 6, introduce(alice, bob, carol), none), no(declined))), sent(alice, bob, outcome(attempt(alice, 6, 6, introduce(alice, bob, carol), none), abort(bob, declined))), sent(alice, carol, outcome(attempt(alice, 6, 6, introduce(alice, bob, carol), none), abort(bob, declined)))" "$cva_intro"
check "CVA introduce: carol, asked, her approval question still open when Outcome closes the attempt, gives it yes once she is asked on attempt 8, and yes to that" "C = \[.*gave(attempt(alice, 6, 6, introduce(alice, bob, carol), none), yes), gave(attempt(alice, 8, 8, introduce(alice, bob, carol), none), yes)" "$cva_intro"
check_not "CVA introduce: ... the approval question closed by the program, her yes reaches no goal: no vote of hers on attempt 6" "sent(carol, alice, vote(attempt(alice, 6, 6, introduce" "$cva_intro"
# The network log interleaves the messages of different agents in the order the scheduler runs them, which the paper leaves open (the runs of maGLP and vmaGLP interleave the agents' reductions, vGLP, Sections 4.2 and 5.2): the check holds the order of each agent's messages and not their interleaving with another agent's, which changes with what each reduction costs (Code #12, 2026-10-20, change 2 of the five).
check "CVA introduce: alice, seeing the notice of the refusal, which ended her approval, requests it again, attempt 8, and both answer yes" "\(sent(carol, alice, vote(attempt(alice, 8, 8, introduce(alice, bob, carol), none), yes)), sent(bob, alice, vote(attempt(alice, 8, 8, introduce(alice, bob, carol), none), yes))\|sent(bob, alice, vote(attempt(alice, 8, 8, introduce(alice, bob, carol), none), yes)), sent(carol, alice, vote(attempt(alice, 8, 8, introduce(alice, bob, carol), none), yes))\)" "$cva_intro"
check "CVA introduce: ... alice's screen: her request gone once bob's refusal ends it, the notice refused(introduce(alice, bob, carol), bob, declined), her request again pending, then the introduction" "A = \[.*view(alice, \[friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), refused(introduce(alice, bob, carol), bob, declined), view(alice, \[friend(carol), friend(bob)\], \[bob, carol\], \[introduce(alice, bob, carol)\], \[tab('Friends', friend)\]), view(alice, \[introduced(bob, carol), friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_intro"
check "CVA introduce: ... and tells bob commit" "sent(alice, bob, outcome(attempt(alice, 8, 8, introduce(alice, bob, carol), none), commit))" "$cva_intro"
check "CVA introduce: ... and carol" "sent(alice, carol, outcome(attempt(alice, 8, 8, introduce(alice, bob, carol), none), commit))" "$cva_intro"
check_not "CVA introduce: no later attempt of hers" "attempt(alice, 9, [0-9]*, introduce\|attempt(alice, 1[0-9], [0-9]*, introduce" "$cva_intro"
# Commit introduces the other participants of its attempt to each other, once
# per pair (vGLP e2bc4f4, Definition "Deciding", Commit): only alice, of the
# three guards, knows every participant (Definition "Holds, For, Connected,
# Unobstructed, Steady": "one of its guards, knows every other participant"),
# and her Commit introduces bob and carol, who come to know each other by the
# ihellos they exchange.
check "CVA introduce: ... alice's Commit introduces bob and carol to each other, after the outcomes: intro(carol) to bob and intro(bob) to carol" "sent(alice, carol, outcome(attempt(alice, 8, 8, introduce(alice, bob, carol), none), commit)), intro(alice, bob, carol), intro(alice, carol, bob) |" "$cva_intro"
check "CVA introduce: ... bob, who knew alice alone, comes to know carol" "B = \[.*view(bob, \[friend(carol), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[friend(carol), friend(alice)\], \[alice, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_intro"
check "CVA introduce: ... and carol, who knew alice alone, bob" "C = \[.*view(carol, \[friend(bob), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(carol, \[friend(bob), friend(alice)\], \[alice, bob\], \[\], \[tab('Friends', friend)\]) |" "$cva_intro"
check_not "CVA introduce: the befriendings, binary, introduce no one: no introduction before attempt 8 commits" "intro([a-z]*, [a-z]*, [a-z]*).*outcome(attempt(alice, 8, 8" "$cva_intro"
check_not "CVA introduce: ... and the pair is introduced once: no third introduction" "intro([a-z]*, [a-z]*, [a-z]*).*intro([a-z]*, [a-z]*, [a-z]*).*intro([a-z]*, [a-z]*, [a-z]*)" "$cva_intro"

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
check "CVA gnudge: alice and bob befriended, committed at both" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob), none), commit))" "$cva_gnudge"
check "CVA gnudge: bob, the least person in the first role of the schema-bindings of the unguarded part of nudge(bob, alice), opens it, an own transaction, once befriended, though alice precedes him" "sent(alice, bob, outcome(attempt(alice, 2, 2, befriend(alice, bob), none), commit)), .*sent(bob, alice, start(attempt(bob, 3, 3, nudge(bob, alice), none)))" "$cva_gnudge"
check "CVA gnudge: ... its start told to alice, whose machine votes on it, no person asked" "sent(alice, bob, vote(attempt(bob, 4, 3, nudge(bob, alice), none), " "$cva_gnudge"
check_not "CVA gnudge: alice, preceding bob, proposes nothing of the class of nudge(bob, alice), her person requesting none" "start(attempt(alice, [0-9]*, [0-9]*, nudge(bob, alice), none))\|start(attempt(alice, [0-9]*, [0-9]*, g(" "$cva_gnudge"
check_not "CVA gnudge: an opener's attempt gives no notice" "refused(" "$cva_gnudge"
check_not "CVA gnudge: no person answers for the unguarded class" "gave(attempt([a-z]*, [0-9]*, [0-9]*, nudge\|gave(nudge\|gave(attempt([a-z]*, [0-9]*, [0-9]*, g(\|gave(g(" "$cva_gnudge"
check "CVA gnudge: bob wounds his own attempt in favour of alice's: it yields before he votes on her start" "sent(bob, alice, start(attempt(bob, 3, 3, nudge(bob, alice), none))), sent(bob, alice, outcome(attempt(bob, 3, 3, nudge(bob, alice), none), yield)), sent(bob, alice, vote(attempt(alice, 3, 3, nudge(alice, bob), none), yes))" "$cva_gnudge"
check_not "CVA gnudge: no vote no for contention" "vote(attempt([a-z]*, [0-9]*, [0-9]*, nudge([a-z]*, [a-z]*), none), no)" "$cva_gnudge"
check "CVA gnudge: alice's nudge committed at both" "sent(alice, bob, outcome(attempt(alice, 3, 3, nudge(alice, bob), none), commit))" "$cva_gnudge"
check "CVA gnudge: bob's nudge, the part he opens, retried at the wounded rank, committed at both" "sent(bob, alice, outcome(attempt(bob, 4, 3, nudge(bob, alice), none), commit))" "$cva_gnudge"
check_not "CVA gnudge: nothing aborted for ever: no third attempt at either nudge" "attempt(alice, [4-9], [0-9]*, nudge\|attempt(bob, [5-9], [0-9]*, nudge" "$cva_gnudge"

# vGLP's contract childsafe (programs/vglp/cva/contracts.glp): befriend, acquaint
# (GKK's Acquaintance, adding no friend atom), enrol and a four-party cbefriend
# guarded by all four (vGLP 2026-10-13 12:05 UTC, item 5).  Its pragma names
# carol, the child in its first role, before its button, so carol is the base
# of the schema and the proposer of its part (vGLP 0b51a60, Section 5.4): she
# meets bob and dave by acquaint before she proposes cbefriend(carol, dave,
# alice, bob), which commits at all four (vGLP #24, Code_inbox.md, 2026-11-07
# 18:52 UTC, Red 1).
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
check "CVA childsafe: carol, knowing alice, meets bob and dave by acquaint, each asked by her hello, the Discover the initiator needs; she proposes cbefriend, her request pending until it commits, and sees it committed" "C = \[.*view(carol, \[knows(dave), knows(bob), parent(alice)\], \[alice, bob, dave\], \[\], \[tab('Friends', friend)\]), view(carol, \[knows(dave), knows(bob), parent(alice)\], \[alice, bob, dave\], \[cbefriend(carol, dave, alice, bob)\], \[tab('Friends', friend)\]), view(carol, \[friend(dave), knows(dave), knows(bob), parent(alice)\], \[alice, bob, dave\], \[\], \[tab('Friends', friend)\]) |" "$cva_cs"
check "CVA childsafe: ... its start told to alice" "sent(carol, alice, start(attempt(carol, [0-9]*, [0-9]*, cbefriend(carol, dave, alice, bob), none)))" "$cva_cs"
check "CVA childsafe: dave, knowing bob, is asked on carol's hello of acquaint and answers yes, comes to know carol, and is asked on cbefriend" "D = \[view(dave, \[\], \[\], \[\], \[tab('Friends', friend)\]), gave(enrol(bob, dave), yes), view(dave, \[\], \[bob\], \[\], \[tab('Friends', friend)\]), .*gave(acquaint(carol, dave), yes), view(dave, \[parent(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), .*gave(cbefriend(carol, dave, alice, bob), yes)" "$cva_cs"
check_not "CVA childsafe: no seek, no refer, and no befriending of a child by the other parent" "seek(\|refer(\|befriend(alice, dave)\|befriend(bob, carol)" "$cva_cs"
check "CVA childsafe: the three yeses, dave's, alice's and bob's" "sent(dave, carol, vote(attempt(carol, \([0-9]*\), \([0-9]*\), cbefriend(carol, dave, alice, bob), none), yes)), sent(alice, carol, vote(attempt(carol, \1, \2, cbefriend(carol, dave, alice, bob), none), yes)), sent(bob, carol, vote(attempt(carol, \1, \2, cbefriend(carol, dave, alice, bob), none), yes))" "$cva_cs"
check "CVA childsafe: ... and it commits at dave" "sent(carol, dave, outcome(attempt(carol, [0-9]*, [0-9]*, cbefriend(carol, dave, alice, bob), none), commit))" "$cva_cs"
check "CVA childsafe: ... at alice" "sent(carol, alice, outcome(attempt(carol, [0-9]*, [0-9]*, cbefriend(carol, dave, alice, bob), none), commit))" "$cva_cs"
check "CVA childsafe: ... and at bob" "sent(carol, bob, outcome(attempt(carol, [0-9]*, [0-9]*, cbefriend(carol, dave, alice, bob), none), commit))" "$cva_cs"
# Commit introduces the other participants of its attempt to each other, once
# per pair (vGLP e2bc4f4, Definition "Deciding", Commit): carol, the initiator,
# knows every participant (Definition "Holds, For, Connected, Unobstructed,
# Steady"), alice knowing bob and carol alone, and the attempt commits; carol's
# Commit introduces every two of alice, bob and dave, alice and bob, and bob
# and dave, knowing each other already, and alice and dave come to know each
# other (CSSN's child_befriend(carol, dave, alice, bob), CSSN to vGLP,
# 2026-10-19 11:21 UTC).
check "CVA childsafe: carol's Commit introduces every two of alice, bob and dave to each other, once per pair, after the outcomes" "sent(carol, bob, outcome(attempt(carol, [0-9]*, [0-9]*, cbefriend(carol, dave, alice, bob), none), commit)), intro(carol, alice, bob), intro(carol, bob, alice), intro(carol, alice, dave), intro(carol, dave, alice), intro(carol, bob, dave), intro(carol, dave, bob) |" "$cva_cs"
check_not "CVA childsafe: the enrolments, the befriending and the acquaintances, binary, introduce no one: no introduction before cbefriend commits" "intro([a-z]*, [a-z]*, [a-z]*).*outcome(attempt(carol, [0-9]*, [0-9]*, cbefriend" "$cva_cs"
check "CVA childsafe: alice, who knew bob and carol, comes to know dave" "A = \[.*view(alice, \[approved(carol, dave), friend(bob), child(carol)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), view(alice, \[approved(carol, dave), friend(bob), child(carol)\], \[bob, carol, dave\], \[\], \[tab('Friends', friend)\]) |" "$cva_cs"
check "CVA childsafe: ... dave comes to know alice" "D = \[.*view(dave, \[friend(carol), knows(carol), parent(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), view(dave, \[friend(carol), knows(carol), parent(bob)\], \[alice, bob, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_cs"
check "CVA childsafe: ... and bob, who knew all three, records the approval" "B = \[.*view(bob, \[approved(dave, carol), knows(carol), friend(alice), child(dave)\], \[alice, carol, dave\], \[\], \[tab('Friends', friend)\]) |" "$cva_cs"

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
check "CVA deadlock: carol votes yes on alice's link and holds bob's start waiting" "sent(alice, carol, start(attempt(alice, 5, 5, link(alice, carol, dave), none))), .*sent(carol, alice, vote(attempt(alice, 5, 5, link(alice, carol, dave), none), yes)), .*sent(bob, carol, start(attempt(bob, 8, 8, link(bob, carol, dave), none)))" "$cva_link"
check "CVA deadlock: dave votes yes on bob's link and holds alice's start waiting, blocked by bob's" "sent(dave, bob, vote(attempt(bob, 8, 8, link(bob, carol, dave), none), yes)), sent(dave, bob, yield(attempt(bob, 8, 8, link(bob, carol, dave), none)))" "$cva_link"
check "CVA deadlock: dave wounds bob's attempt in favour of alice's, of the smaller rank, asking bob to yield, and votes on her start only after bob's outcome yield, bob's retried start between them" "sent(dave, bob, yield(attempt(bob, 8, 8, link(bob, carol, dave), none))), sent(bob, carol, outcome(attempt(bob, 8, 8, link(bob, carol, dave), none), yield)), sent(bob, dave, outcome(attempt(bob, 8, 8, link(bob, carol, dave), none), yield)), .*sent(dave, alice, vote(attempt(alice, 5, 5, link(alice, carol, dave), none), yes))" "$cva_link"
check "CVA deadlock: bob, not every yes recorded, yields, the yield ending no approval, and his machine proposes his link again on his standing request, with no question to his person: attempt 9 at the wounded rank 8 (vGLP f0acbc1, Section 3.2, \"The protocol\")" "sent(bob, dave, outcome(attempt(bob, 8, 8, link(bob, carol, dave), none), yield)), sent(bob, carol, start(attempt(bob, 9, 8, link(bob, carol, dave), none)))" "$cva_link"
check "CVA deadlock: ... the outcome yield told to both" "sent(bob, carol, outcome(attempt(bob, 8, 8, link(bob, carol, dave), none), yield)), sent(bob, dave, outcome(attempt(bob, 8, 8, link(bob, carol, dave), none), yield))" "$cva_link"
check "CVA deadlock: dave, his lock released, votes yes on alice's link" "sent(dave, alice, vote(attempt(alice, 5, 5, link(alice, carol, dave), none), yes))" "$cva_link"
check "CVA deadlock: alice's link commits at carol and dave" "sent(alice, carol, outcome(attempt(alice, 5, 5, link(alice, carol, dave), none), commit)), sent(alice, dave, outcome(attempt(alice, 5, 5, link(alice, carol, dave), none), commit))" "$cva_link"
check "CVA deadlock: bob's retried attempt, keeping the wounded rank 8, commits at carol and dave" "sent(bob, carol, outcome(attempt(bob, 9, 8, link(bob, carol, dave), none), commit)), sent(bob, dave, outcome(attempt(bob, 9, 8, link(bob, carol, dave), none), commit))" "$cva_link"
check "CVA deadlock: ... bob's screen: no notice, his request of the link pending through his yield and his retry, and his link committed (vGLP f0acbc1: the notice yielded goes)" "B = \[.*view(bob, \[friend(dave), friend(carol)\], \[carol, dave\], \[link(bob, carol, dave)\], \[tab('Friends', friend)\]), view(bob, \[linked(carol, dave), friend(dave), friend(carol)\], \[carol, dave\], \[\], \[tab('Friends', friend)\])" "$cva_link"
check "CVA deadlock: bob's yes to dave's hello of befriend, read while his machine is prepared on his attempt with carol, held and put up again before dave's start, held waiting likewise, so that it is an approval again when that start is taken (vGLP, Section 5.4, \"The compilation\": \"it holds the request and puts it up again after every dispatch\")" "B = \[.*gave(befriend(dave, bob), yes), view(bob, \[\], \[carol, dave\], \[befriend(carol, bob)\], \[tab('Friends', friend)\]), view(bob, \[friend(carol)\], \[carol, dave\], \[\], \[tab('Friends', friend)\]), view(bob, \[friend(carol)\], \[carol, dave\], \[befriend(dave, bob)\], \[tab('Friends', friend)\]), view(bob, \[friend(dave), friend(carol)\], \[carol, dave\], \[\], \[tab('Friends', friend)\])" "$cva_link"
check "CVA deadlock: ... his machine votes yes on dave's attempt, unasked, no second question being asked (vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 1)" "sent(bob, dave, vote(attempt(dave, 4, 4, befriend(dave, bob), none), yes))" "$cva_link"
check_not "CVA deadlock: ... bob not asked on dave's attempt" "B = \[.*gave(attempt(dave, " "$cva_link"
check_not "CVA deadlock: no notice to anyone, no approval ending by a refusal" "refused(\|yielded(" "$cva_link"
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
check "CVA machine states: the social graph's atoms, identity, knows(Peer) and friend(Peer)" "^identity knows(Peer) friend(Peer) $" "$(cva_atom "$CVA/social_graph/platform.vglp")"
check "CVA machine states: ... friend(bob) written as the contract writes it" "^atom_of(g(friend, \[p(V1)\]), shown(friend(V1?)))$" "$cva_sg_text"
check "CVA machine states: the currency's atoms, GKK's identity and knows(Peer) and its own coin(Peer)" "^identity knows(Peer) coin(Peer) $" "$(cva_atom "$CVA/currency/platform.vglp")"
check "CVA machine states: GSN's atoms, a speech act at a speech-act variable's place and a community name at a name term's" "^identity knows(Peer) friend(Peer) received(Speech, Peer) member(CName) listed(Peer, Speech) closed(Speech) posted(Speech, Speech) delivered(Speech, Peer, Speech) received_in(Speech, Peer, Speech) $" "$(cva_atom "$GLP_DIR/programs/apps/gsn/platform.vglp")"
check "CVA machine states: GSNP's, on GKK, GnW and GAI, a signed term a speech act" "^identity retired knows(Peer) agent friend(Peer) principal(Peer) guarded_by(Peer) principal_of(Peer) guardian_of(Peer) agent_of(Peer, Speech) authored(Speech) attestation(Speech) retired_of(Peer) item(Speech) member(CName) listed(Peer, Speech) closed(Speech) posted(Speech, Speech) delivered(Speech, Peer, Speech) $" "$(cva_atom "$GLP_DIR/programs/apps/gsnp/platform.vglp")"

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
check "CVA introduce peers: alice, a friend of bob and of carol, takes Introduce peers {bob, carol}, an own transaction, once her befriending of carol commits, carol's attempt, alice's wounded in its favour" "sent(carol, alice, outcome(attempt(carol, 2, 2, befriend(alice, carol), none), commit)), intro(alice, bob, carol)" "$cva_friends"
check "CVA introduce peers: ... sending bob intro(carol) and carol intro(bob)" "intro(alice, bob, carol), intro(alice, carol, bob)" "$cva_friends"
check "CVA introduce peers: bob comes to know carol by the ihellos, never having asked to meet her, his view showing her, and his request question shows her" "B = \[.*view(bob, \[friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[friend(alice)\], \[alice, carol\], \[\], \[tab('Friends', friend)\]), menu(bob, \[alice, carol\])" "$cva_friends"
check "CVA introduce peers: ... and he proposes befriending her" "sent(bob, carol, start(attempt(bob, 4, 4, befriend(bob, carol), none)))" "$cva_friends"
check "CVA introduce peers: carol comes to know bob, is asked on his attempt, he being known so that no hello is sent, answers yes, her answer pending until it commits, and the friends of a friend are friends" "C = \[.*view(carol, \[friend(alice)\], \[alice, bob\], \[\], \[tab('Friends', friend)\]), gave(befriend(bob, carol), yes), view(carol, \[friend(alice)\], \[alice, bob\], \[befriend(bob, carol)\], \[tab('Friends', friend)\]), view(carol, \[friend(bob), friend(alice)\], \[alice, bob\], \[\], \[tab('Friends', friend)\])" "$cva_friends"
check "CVA introduce peers: ... it commits" "sent(bob, carol, outcome(attempt(bob, 4, 4, befriend(bob, carol), none), commit))" "$cva_friends"
check_not "CVA introduce peers: no pair introduced twice by one agent" "intro(alice, bob, carol), .*intro(alice, bob, carol)" "$cva_friends"
check "CVA two roles at one person: alice's greet(alice, alice), friend(alice) holding at her, friend reflexive, a class of one participant applied at its Propose: greeted and greeting, twice" "A = \[.*view(alice, \[friend(carol), friend(bob)\], \[bob, carol\], \[greet(alice, alice)\], \[tab('Friends', friend)\]), view(alice, \[greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), view(alice, \[greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[greet(alice, alice)\], \[tab('Friends', friend)\]), view(alice, \[greeted, greeting, greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\])" "$cva_friends"
check_not "CVA two roles at one person: ... no start sent for it" "start(attempt(alice, [0-9]*, [0-9]*, greet(alice, alice), none))" "$cva_friends"
check "CVA counts: two cheers, each deleting one of her two greeted" "view(alice, \[cheered, greeting, greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), view(alice, \[cheered, greeting, greeted, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[cheer(alice)\], \[tab('Friends', friend)\]), view(alice, \[cheered, cheered, greeting, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\])" "$cva_friends"
check "CVA counts: her third cheer, no greeted left, dropped and taking no number: her greet of bob is attempt 14, after her Requests and Proposes of two befriendings, two greets and two cheers, a dropped request taking no number (vGLP f0acbc1, Definition \"Proposing\", Request)" "sent(alice, bob, start(attempt(alice, 14, 14, greet(alice, bob), none)))" "$cva_friends"
check "CVA counts: ... the third cheer dropped with the notice refused(cheer(alice), alice, unable), her machine ready for no part named so (vGLP, Section 5.4, \"The compilation\": \"it drops the request where the agent is free and none is enabled, with a notice refused(N, p, unable) where the agent is ready for no part named N that it guards or opens\")" "A = \[.*view(alice, \[cheered, cheered, greeting, greeting, friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), refused(cheer(alice), alice, unable), view(" "$cva_friends"
check_not "CVA counts: ... no third cheer proposed" "cheered, cheered, cheered" "$cva_friends"
check "CVA two roles at one person: greet(alice, bob), at two people, bob's machine voting on it, commits" "sent(alice, bob, outcome(attempt(alice, 14, 14, greet(alice, bob), none), commit))" "$cva_friends"

# The printed program's class at run time is the paper's (vGLP 9c819f5, Section 5.4, "Act schemas as local specifications": "The classes are those of the contract's equivalence, generated by relating two transactions when one schema-binding determines both"; vGLP #14's task, Code_inbox.md, 2026-10-18 13:35 UTC): the closure of a schema-binding's links among the schema-bindings of its participants and atoms, the compiler's local.glp, part C, at run time, so that the domains of the effects, the guard sets and the opener are taken over one class.  vGLP's contract waves (programs/vglp/cva/contracts.glp): wave(alice, bob), guarded by alice, requires friend(bob) at alice and hail(alice, bob), guarded by both, forbids it, the two deleting and adding the same atoms, so they are two classes.  hail asks bob, though the class of wave is guarded by alice, hail a meeting schema, its request a cold call whose hello asks him; and once befriended, alice's second hail is dropped with the notice refused(hail(alice, bob), alice, unable), taking no number, friend(bob) being outside the domain of her effect in the class of hail, though in the domain in the class of wave.  A class taken as the schema-bindings of the same participants and atoms would ask bob nothing for hail and take the second.
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
check "CVA run-time class: alice hails bob, a meeting schema by which they come to know each other, attempt 2; befriends him, attempt 4; her second hail dropped with the notice refused(hail(alice, bob), alice, unable), her machine ready for no part named so, the friend(bob) hail forbids held, and taking no number; and she waves, attempt 6, each committed: her screen" "A = \[view(alice, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[\], \[\], \[hail(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[\], \[bob\], \[hail(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[waved(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), view(alice, \[waved(bob)\], \[bob\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[friend(bob), waved(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), refused(hail(alice, bob), alice, unable), view(alice, \[friend(bob), waved(bob)\], \[bob\], \[wave(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[waved(bob), friend(bob), waved(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]) | " "$cva_waves"
check "CVA run-time class: bob, in the guard set of the class of hail, is asked on alice's hello of it and answers yes, his request, and is not asked again on its start, both approvals standing; asked on befriend's start; and his machine votes on the wave unasked" "B = \[view(bob, \[\], \[\], \[\], \[tab('Friends', friend)\]), gave(hail(alice, bob), yes), view(bob, \[\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[\], \[alice\], \[hail(alice, bob)\], \[tab('Friends', friend)\]), view(bob, \[waved_at(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), gave(befriend(alice, bob), yes), view(bob, \[waved_at(alice)\], \[alice\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(bob, \[friend(alice), waved_at(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[waved_at(alice), friend(alice), waved_at(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]) | " "$cva_waves"
check "CVA run-time class: alice's three attempts, each with bob's yes and its commit; on hail, bob's yes to her hello his request, an approval that stands, so his machine votes yes on her attempt unasked, and proposes none, being no proposer of the part, alice in its base (vGLP 9ee6f87, Definitions \"Local Specification\" and \"Proposing\")" "N = \[sent(alice, bob, start(attempt(alice, 2, 2, hail(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 2, 2, hail(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 2, 2, hail(alice, bob), none), commit)), sent(alice, bob, start(attempt(alice, 4, 4, befriend(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 4, 4, befriend(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 4, 4, befriend(alice, bob), none), commit)), sent(alice, bob, start(attempt(alice, 6, 6, wave(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 6, 6, wave(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 6, 6, wave(alice, bob), none), commit)) | " "$cva_waves"

# A class is the union of its parts (vGLP c0bea08, Definition "Local Specification, Part, Opener"; Section 5.4, "Act schemas as local specifications": "the parts of a class are the sets of transactions its schema-bindings determine"): an attempt names its part, each participant checks and applies that part's function.  The request propose(N) is a Request of the class of the part N names, the person's approval, taken where the agent guards and is ready for that part, and dropped with the notice refused(N, p, unable) where it is ready for no part named N; and on own triggers the agent proposes, of the class of a pending request, only the parts that request names (vGLP 1423679, Section 5.4, "The compilation"; vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 4).  vGLP's contract parts (programs/vglp/cva/contracts.glp): tap(alice, bob) and pat(alice, bob), one class guarded by alice, of two parts, pat's requiring a coin at both.  alice, holding no coin, requests pat, and her machine drops it with the notice; she requests tap, and tap's part is proposed; holding a coin, she requests pat, and pat's part is proposed, and bob, holding no coin, votes no, unable, outside the domain of his effect in pat's part though inside it in tap's, and her approval ends with the notice; she requests tap and tap's is proposed.
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
check "CVA parts: alice, holding no coin, requests pat: ready for no part named so, pat's requiring the coin, her machine drops the request with the notice refused(pat(alice, bob), alice, unable), taking no number (vGLP, Section 5.4, \"The compilation\")" "A = \[view(alice, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[\], \[\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[\], \[bob\], \[befriend(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), refused(pat(alice, bob), alice, unable), view(alice, \[friend(bob)\], \[bob\], \[tap(alice, bob)\], \[tab('Friends', friend)\])" "$cva_parts"
check "CVA parts: ... she requests tap, and her machine proposes tap's part, the part her request names, attempt 4, which commits (vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 4)" "sent(alice, bob, start(attempt(alice, 4, 4, tap(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 4, 4, tap(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 4, 4, tap(alice, bob), none), commit))" "$cva_parts"
check "CVA parts: holding a coin, minted at attempt 6, she requests pat, and her machine proposes pat's part, attempt 8, which bob refuses; she requests tap, and it proposes tap's, attempt 10: of the class of a pending request, only the parts that request names (vGLP, Section 5.4, \"The compilation\")" "sent(alice, bob, start(attempt(alice, 8, 8, pat(alice, bob), none))), .*sent(alice, bob, start(attempt(alice, 10, 10, tap(alice, bob), none)))" "$cva_parts"
check "CVA parts: ... alice's screen: each request pending by its name until it ends; the notice naming the part, pat's, by its term, its refuser bob and his reason, unable, the coin the part requires at him that his state lacks (vGLP f0acbc1, Section 5.4, \"Act schemas\")" "A = \[.*view(alice, \[coin, tapped(bob), friend(bob)\], \[bob\], \[pat(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[coin, tapped(bob), friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]), refused(pat(alice, bob), bob, unable(\[coin\], \[\])), view(alice, \[coin, tapped(bob), friend(bob)\], \[bob\], \[tap(alice, bob)\], \[tab('Friends', friend)\]), view(alice, \[tapped(bob), coin, tapped(bob), friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]) |" "$cva_parts"
check "CVA parts: bob, holding no coin, votes no, unable, on pat's part, his precondition in it failing, the abort carrying him as refuser, and yes on tap's" "sent(alice, bob, start(attempt(alice, 8, 8, pat(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 8, 8, pat(alice, bob), none), no(unable(\[coin\], \[\])))), sent(alice, bob, outcome(attempt(alice, 8, 8, pat(alice, bob), none), abort(bob, unable(\[coin\], \[\])))), sent(alice, bob, start(attempt(alice, 10, 10, tap(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 10, 10, tap(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 10, 10, tap(alice, bob), none), commit))" "$cva_parts"
check "CVA parts: ... and yes on tap's, attempt 4, before" "sent(bob, alice, vote(attempt(alice, 4, 4, tap(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 4, 4, tap(alice, bob), none), commit))" "$cva_parts"
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
check "CVA opens: bob marks alice, attempt 4, once he sees her his friend, and then opens the part of u2(bob, alice), an own transaction, attempt 5, taking no Request" "sent(bob, alice, start(attempt(bob, 4, 4, mark(bob, alice), none))), .*sent(bob, alice, start(attempt(bob, 5, 5, u2(bob, alice), none)))" "$cva_opens"
check "CVA opens: ... bob's screen: his request of mark pending until it commits, then each committed" "B = \[.*view(bob, \[friend(alice)\], \[alice\], \[mark(bob, alice)\], \[tab('Friends', friend)\]), view(bob, \[w(alice), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[y(alice), w(alice), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]) | " "$cva_opens"
check "CVA opens: alice's machine votes yes on its start, unasked, and takes the outcome" "sent(bob, alice, start(attempt(bob, 5, 5, u2(bob, alice), none))), sent(alice, bob, vote(attempt(bob, 5, 5, u2(bob, alice), none), yes)), sent(bob, alice, outcome(attempt(bob, 5, 5, u2(bob, alice), none), commit)) | " "$cva_opens"
check_not "CVA opens: alice, preceding bob, proposes nothing of the class, the part she opens not holding" "start(attempt(alice, [0-9]*, [0-9]*, \(u1\|u2\)" "$cva_opens"
check_not "CVA opens: no person answers for the unguarded parts" "gave(attempt([a-z]*, [0-9]*, [0-9]*, \(u1\|u2\)\|gave(\(u1\|u2\)" "$cva_opens"

# Listed roles (vGLP f5f1644, Section 5.4, "Act schemas as local specifications": "each schema-binding determines a part [t]_j of a class for each list L of the people of its listed roles: the set of transactions it determines with L in those roles, with f^[t]_j_p defined at d_p iff ... for p the person in the base of a listed role, p lists in d_p exactly the people of that role in L ... its request name is the schema-binding"; vGLP #15 Cowork to Code, 2026-10-19 14:24 UTC).  vGLP's contract groups (programs/vglp/cva/contracts.glp): close(a?, a[listed(*, n)] ; n), its part's term carrying the list of the people of its listed role, its request name not.
cva_groups_text=$(cat "$CVA/groups/platform.vglp")
check "CVA groups: the class term of close carries the list of the people of its listed role" "^close(Peer, List(Peer), Speech)$" "$cva_groups_text"
check "CVA groups: ... its request name, the schema-binding, its head with no extent, does not" "^befriend(Peer, Peer) create(Peer, Speech) join(Peer, Peer, Speech) close(Peer, Speech) $" "$(cva_proposal "$CVA/groups/platform.vglp")"
check "CVA groups: the binding of a class term of close, its listed role's place and a role for each of its people (listed_binding; ppl(L) a listed role's list, ins(L) a conjunct's instances)" "^listed_binding(close, \[b(role(1), p(V1?)), b(svar(n), s(V3?))\], \[ppl(V2?)\], B)$" "$cva_groups_text"
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
check "CVA groups: alice requests close(alice, n), and her agent proposes close(alice, [alice, bob, carol], n), the people she lists, its start told to bob and carol" "sent(alice, bob, start(attempt(alice, 12, 12, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)), none))), sent(alice, carol, start(attempt(alice, 12, 12, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)), none)))" "$cva_groups"
check "CVA groups: ... bob's and carol's machines vote yes, unasked, and it commits at both" "sent(alice, bob, outcome(attempt(alice, 12, 12, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)), none), commit)), sent(alice, carol, outcome(attempt(alice, 12, 12, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)), none), commit))" "$cva_groups"
check "CVA groups: alice's screen: the group closed, her membership and her listing of herself ended" "A = \[.*view(alice, \[closed(speech(utterance(friends), alice)), listed(carol, speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_groups"
check "CVA groups: bob's membership ended in the one act, and bob, introduced to carol by alice's Commit, comes to know her (vGLP e2bc4f4, Definition \"Deciding\", Commit)" "B = \[.*view(bob, \[member(alice, speech(utterance(friends), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), refused(close(alice, speech(utterance(friends), alice)), bob, unable), view(bob, \[friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[friend(alice)\], \[alice, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_groups"
check "CVA groups: ... and carol's, who comes to know bob" "C = \[.*view(carol, \[member(alice, speech(utterance(friends), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(carol, \[friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(carol, \[friend(alice)\], \[alice, bob\], \[\], \[tab('Friends', friend)\]) |" "$cva_groups"
check "CVA groups: ... alice's Commit of the close introduces bob and carol to each other, after the outcomes" "sent(alice, carol, outcome(attempt(alice, 12, 12, close(alice, \[alice, bob, carol\], speech(utterance(friends), alice)), none), commit)), intro(alice, bob, carol), intro(alice, carol, bob)" "$cva_groups"
check_not "CVA groups: bob's request to close alice's group, his agent listing no one, is dropped: no attempt of his of close" "attempt(bob, [0-9]*, [0-9]*, close" "$cva_groups"
check "CVA groups: ... with the notice refused(N, bob, unable), N its request name, close(alice, n), with no list in it, his machine ready for no part named N (vGLP e286276, Section 5.4, \"The compilation\")" "B = \[.*refused(close(alice, speech(utterance(friends), alice)), bob, unable)" "$cva_groups"
check_not "CVA groups: no person asked for close" "gave(attempt([a-z]*, [0-9]*, [0-9]*, close\|gave(close" "$cva_groups"

# Several listed roles of one base (vGLP c649c0d, Section 5.4, "Act schemas as local specifications": "each schema-binding determines a part [t]_j of a class for each list L of the people of its listed roles ... for p the person in the base of a listed role, p lists in d_p exactly the people of that role in L", and "the listed roles of a schema have one base"; vGLP #16 Cowork to Code, 2026-10-19 15:51 UTC: "the base agent filling in each from its own listed atoms; the part's term carries the people of each, in the order of the roles").  vGLP's contract merges (programs/vglp/cva/contracts.glp): merge(a?, a[listed(*, n1)], a[listed(*, n2)] ; n1, n2).
# Guarded: set -e would end the run without totals where the platform is not printed.
cva_merges_text=$(cat "$CVA/merges/platform.vglp" 2>&1 || true)
check "CVA merges: the class term of merge carries the people of each listed role, in the order of the roles" "^merge(Peer, List(Peer), List(Peer), Speech, Speech)$" "$cva_merges_text"
check "CVA merges: ... its request name, the schema-binding, does not" "^befriend(Peer, Peer) create(Peer, Speech) join(Peer, Peer, Speech) merge(Peer, Speech, Speech) $" "$(cva_proposal "$CVA/merges/platform.vglp")"
check "CVA merges: the binding of a class term of merge, from the lists of the people of its two listed roles (listed_binding)" "^listed_binding(merge, \[b(role(1), p(V1?)), b(svar(n1), s(V4?)), b(svar(n2), s(V5?))\], \[ppl(V2?), ppl(V3?)\], B)$" "$cva_merges_text"
check "CVA merges: ... and the people of each listed role from a schema-binding, the first's (copies_of)" "^copies_of(B?, 2, V2)$" "$cva_merges_text"
check "CVA merges: ... and the second's" "^copies_of(B?, 3, V3)$" "$cva_merges_text"
check "CVA merges: the request name completed with the people the agent lists in each (listed_class)" "^listed_class(merge, \[b(role(1), p(V1?)), b(svar(n1), s(V4?)), b(svar(n2), s(V5?))\], St?, C)$" "$cva_merges_text"
cva_merges_load=$("$REPL_RUN" <<HEREDOC
$CVA/merges/
:quit
HEREDOC
2>&1)
check "CVA merges: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/merges" "$cva_merges_load"
check_not "CVA merges: no type errors" "Type checking failed" "$cva_merges_load"
cva_merges=$("$REPL_RUN" <<HEREDOC
$CVA/merges/
:limit 3000000
play_merges(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA merges: alice requests merge(alice, n1, n2), and her agent proposes merge(alice, [bob], [carol], n1, n2), the people she lists in each, its start told to bob and carol" "sent(alice, bob, start(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none))), sent(alice, carol, start(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none)))" "$cva_merges"
# The network log interleaves the messages of different agents in the order the scheduler runs them, which the paper leaves open (the runs of maGLP and vmaGLP interleave the agents' reductions, vGLP, Sections 4.2 and 5.2): the check holds the order of each agent's messages and not their interleaving with another agent's, which changes with what each reduction costs (Code #12, 2026-10-20, change 2 of the five).
check "CVA merges: ... bob's and carol's machines vote yes, unasked, and it commits at both" "\(sent(bob, alice, vote(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), yes)), sent(carol, alice, vote(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), yes))\|sent(carol, alice, vote(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), yes)), sent(bob, alice, vote(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), yes))\), sent(alice, bob, outcome(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), commit)), sent(alice, carol, outcome(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), commit))" "$cva_merges"
check "CVA merges: alice's screen: the merger recorded, n2 led no more" "A = \[.*view(alice, \[merged(speech(utterance(two), alice), speech(utterance(one), alice)), listed(carol, speech(utterance(two), alice)), listed(bob, speech(utterance(one), alice)), leads(speech(utterance(one), alice)), friend(carol), friend(bob)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_merges"
check "CVA merges: bob, a member of n1, records the merger, and, introduced to carol by alice's Commit, comes to know her (vGLP e2bc4f4, Definition \"Deciding\", Commit)" "B = \[.*view(bob, \[member(alice, speech(utterance(one), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[merged(speech(utterance(two), alice), speech(utterance(one), alice)), member(alice, speech(utterance(one), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(bob, \[merged(speech(utterance(two), alice), speech(utterance(one), alice)), member(alice, speech(utterance(one), alice)), friend(alice)\], \[alice, carol\], \[\], \[tab('Friends', friend)\]) |" "$cva_merges"
check "CVA merges: carol, a member of n2, becomes a member of n1 in the one act, and comes to know bob" "C = \[.*view(carol, \[member(alice, speech(utterance(two), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(carol, \[member(alice, speech(utterance(one), alice)), friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]), view(carol, \[member(alice, speech(utterance(one), alice)), friend(alice)\], \[alice, bob\], \[\], \[tab('Friends', friend)\]) |" "$cva_merges"
check "CVA merges: ... alice's Commit of the merger introduces bob and carol to each other, after the outcomes" "sent(alice, carol, outcome(attempt(alice, 14, 14, merge(alice, \[bob\], \[carol\], speech(utterance(one), alice), speech(utterance(two), alice)), none), commit)), intro(alice, bob, carol), intro(alice, carol, bob)" "$cva_merges"
check_not "CVA merges: no person asked for merge" "gave(attempt([a-z]*, [0-9]*, [0-9]*, merge\|gave(merge" "$cva_merges"

# The standing approvals (vGLP f0acbc1, Section 3.2, "The protocol": "A guard's person approves a transaction once, by a request or by answering yes when asked, and the approval stands until the transaction is taken or refused ... A machine that cannot carry out a transaction its person approved refuses it, so that the approval ends and the person is told"; Definitions "Proposing" to "Deciding"; Section 5.4, "Triggers" and "The compilation"; vGLP #18 Cowork to Code, 2026-10-20 10:44 UTC, item 6).  vGLP's contract coins (programs/vglp/cva/contracts.glp): GC's mint, swap and pay, coins of one unit.  swap is a meeting schema of the contract, two guarding roles requiring nothing naming the other, so a request of it with a peer not known is a cold call, the hello, and the peer's yes their request of it (vGLP 1423679, Section 5.4, "The compilation").  carol hands alice a Bob-coin, and alice, who has never known bob, requests a payment of it to bob and a swap of it with bob, two Requests to her own machine, her approvals; the swap's hello asks bob, who answers yes, and his machine proposes the swap, which has priority over alice's payment, commits and spends the coin; her machine then proposes the payment again and refuses it at once, the notice naming her machine, unable, the coin lacking; a swap bob declines gives her the notice naming bob, declined.
cva_coins_load=$("$REPL_RUN" <<HEREDOC
$CVA/coins/
:quit
HEREDOC
2>&1)
check "CVA coins: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/coins" "$cva_coins_load"
check_not "CVA coins: no type errors" "Type checking failed" "$cva_coins_load"
cva_coins=$("$REPL_RUN" <<HEREDOC
$CVA/coins/
:limit 3000000
play_coins(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA coins: carol swaps bob's coin for alice's, a cold call, swap a meeting schema of the contract: alice is asked on carol's hello and answers yes, her request; alice holds one Bob-coin, knowing carol alone" "A = \[view(alice, \[\], \[\], \[\], \[tab('Coins', coin)\]), view(alice, \[\], \[\], \[mint(alice)\], \[tab('Coins', coin)\]), view(alice, \[coin(alice)\], \[\], \[\], \[tab('Coins', coin)\]), gave(swap(carol, alice, alice, bob), yes), view(alice, \[coin(alice)\], \[carol\], \[\], \[tab('Coins', coin)\]), view(alice, \[coin(alice)\], \[carol\], \[swap(carol, alice, alice, bob)\], \[tab('Coins', coin)\]), view(alice, \[coin(bob)\], \[carol\], \[\], \[tab('Coins', coin)\])" "$cva_coins"
check "CVA coins: alice's requests of the payment and the swap, Requests to her own machine, take her numbers 7 and 8 while bob is unknown; the swap's hello asks bob, who answers yes, his request, and his machine proposes the swap, attempt 6, alice's machine voting yes unasked; her machine proposes the payment, attempt 9, and wounds it in favour of bob's swap, of the smaller rank, and the swap commits, spending the Bob-coin" "sent(bob, alice, start(attempt(bob, 6, 6, swap(alice, bob, carol, bob), none))), sent(alice, bob, start(attempt(alice, 9, 9, pay(alice, bob), none))), sent(alice, bob, outcome(attempt(alice, 9, 9, pay(alice, bob), none), yield)), sent(alice, bob, vote(attempt(bob, 6, 6, swap(alice, bob, carol, bob), none), yes)), sent(bob, alice, outcome(attempt(bob, 6, 6, swap(alice, bob, carol, bob), none), commit))" "$cva_coins"
check "CVA coins: ... bob's screen: his answer to alice's hello, his request pending, and the Bob-coin his once the swap commits" "B = \[.*gave(swap(alice, bob, carol, bob), yes), view(bob, \[coin(carol)\], \[alice, carol\], \[\], \[tab('Coins', coin)\]), view(bob, \[coin(carol)\], \[alice, carol\], \[swap(alice, bob, carol, bob)\], \[tab('Coins', coin)\]), view(bob, \[coin(bob)\], \[alice, carol\], \[\], \[tab('Coins', coin)\])" "$cva_coins"
check "CVA coins: the Bob-coin spent, her approval of the payment standing, her machine proposes the payment again, attempt 10 at the wounded rank 9, and refuses it at once: no start, the outcome abort told to bob, alice its refuser, unable, the Bob-coin the payment requires at her and her state lacks" "sent(bob, alice, outcome(attempt(bob, 6, 6, swap(alice, bob, carol, bob), none), commit)), .*sent(alice, bob, outcome(attempt(alice, 10, 9, pay(alice, bob), none), abort(alice, unable(\[coin(bob)\], \[\]))))" "$cva_coins"
check_not "CVA coins: ... no start of the swap refused at once" "start(attempt(alice, 10, " "$cva_coins"
check "CVA coins: ... her approval ended, her screen shows the notice naming her machine, unable" "view(alice, \[coin(carol)\], \[bob, carol\], \[\], \[tab('Coins', coin)\]), refused(pay(alice, bob), alice, unable(\[coin(bob)\], \[\]))" "$cva_coins"
check "CVA coins: alice mints again and requests a swap of her coin for bob's, attempt 14; bob, asked, answers no, his vote no declined, and alice's machine tells the outcome abort, bob its refuser, declined" "sent(bob, alice, vote(attempt(alice, 14, 14, swap(alice, bob, bob, alice), none), no(declined))), sent(alice, bob, outcome(attempt(alice, 14, 14, swap(alice, bob, bob, alice), none), abort(bob, declined))) | " "$cva_coins"
check "CVA coins: ... his answer" "B = \[.*gave(swap(alice, bob, bob, alice), no) | " "$cva_coins"
check "CVA coins: ... alice's screen: the notice naming bob, declined" "view(alice, \[coin(alice), coin(carol)\], \[bob, carol\], \[\], \[tab('Coins', coin)\]), refused(swap(alice, bob, bob, alice), bob, declined) | " "$cva_coins"
check_not "CVA coins: no question to alice's person on her own requests" "A = \[.*gave(pay\|A = \[.*gave(swap(alice, " "$cva_coins"
check_not "CVA coins: no notice to bob, whose answer was no, nor to carol" "B = \[.*refused(\|C = \[.*refused(" "$cva_coins"

# vGLP's contract wardclose (programs/vglp/cva/contracts.glp): CSSN's child_close, child_close(r, p?, r[listed(*, n)] ; n), whose guarded parts have the opener r, the child, and the guard p, its parent (vGLP f0acbc1, Definition "Local Specification, Part, Guard Set, Opener, Request"; Section 5.4, "Act schemas as local specifications").  alice, carol's parent, requests child_close(carol, alice, n): a Request of rho_N to carol's machine, which holds it, completes the list from its listed atoms and proposes the part, the attempt naming the request; alice's machine votes yes on it, unasked.  The close yields to bob's befriending of dave, which has priority, and carol's machine proposes it again on the request it still holds, keeping its rank, with no question to anyone; it commits.  The network holds alice's request until dave's join commits, bob's start to dave until dave's yes on the close, and alice's vote on the first close until dave's yield (wardclose/self.glp).
CVA_WC_N="speech(utterance(club), carol)"
CVA_WC_R="request(rho(child_close(carol, alice, $CVA_WC_N)), 3)"
CVA_WC_C="child_close(carol, alice, \[carol, dave\], $CVA_WC_N)"
cva_wardclose_load=$("$REPL_RUN" <<HEREDOC
$CVA/wardclose/
:quit
HEREDOC
2>&1)
check "CVA wardclose: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/wardclose" "$cva_wardclose_load"
check_not "CVA wardclose: no type errors" "Type checking failed" "$cva_wardclose_load"
cva_wardclose=$("$REPL_RUN" <<HEREDOC
$CVA/wardclose/
:limit 6000000
play_child_close(A, B, C, D, N).
:quit
HEREDOC
2>&1)
check "CVA wardclose: alice's request is a Request of rho_N to carol's machine, the opener of the parts named N, carrying N, her number 3" "sent(alice, carol, $CVA_WC_R)" "$cva_wardclose"
check "CVA wardclose: carol's machine, holding it, completes the list from its listed atoms and proposes child_close(carol, alice, [carol, dave], n), attempt 9, naming the request, its start told to alice and dave" "sent(alice, carol, $CVA_WC_R), .*sent(carol, alice, start(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R))), sent(carol, dave, start(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R)))" "$cva_wardclose"
check "CVA wardclose: dave votes yes and holds the close prepared; bob's start of befriend(bob, dave), rank 2, arrives, and dave asks carol to yield, Wound" "sent(dave, carol, vote(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R), yes)), sent(bob, dave, start(attempt(bob, 2, 2, befriend(bob, dave), none))), sent(dave, carol, yield(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R)))" "$cva_wardclose"
check "CVA wardclose: carol, alice's yes not yet tallied, yields, the outcome yield told to both" "sent(carol, alice, outcome(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R), yield)), sent(carol, dave, outcome(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R), yield))" "$cva_wardclose"
check "CVA wardclose: ... and her machine proposes the close again on the request it still holds, attempt 10 keeping the wounded rank 9, with no question to anyone" "sent(carol, dave, outcome(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R), yield)), .*sent(carol, alice, start(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R))), sent(carol, dave, start(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R)))" "$cva_wardclose"
check "CVA wardclose: alice's machine votes yes on each, unasked, the request it names pending at her" "sent(alice, carol, vote(attempt(carol, 9, 9, $CVA_WC_C, $CVA_WC_R), yes)), .*sent(alice, carol, vote(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R), yes))" "$cva_wardclose"
# The network log interleaves the messages of different agents in the order the scheduler runs them, which the paper leaves open (the runs of maGLP and vmaGLP interleave the agents' reductions, vGLP, Sections 4.2 and 5.2): the check holds the order of each agent's messages and not their interleaving with another agent's, which changes with what each reduction costs (Code #12, 2026-10-20, change 2 of the five).
check "CVA wardclose: dave, asked once on bob's befriend, answers yes, it commits, and dave's yes on the close follows" "sent(dave, bob, vote(attempt(bob, 2, 2, befriend(bob, dave), none), yes)), .*sent(bob, dave, outcome(attempt(bob, 2, 2, befriend(bob, dave), none), commit)), .*sent(dave, carol, vote(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R), yes))" "$cva_wardclose"
check "CVA wardclose: the close commits at alice and dave, and carol's Commit introduces alice and dave to each other" "sent(carol, alice, outcome(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R), commit)), sent(carol, dave, outcome(attempt(carol, 10, 9, $CVA_WC_C, $CVA_WC_R), commit)), intro(carol, alice, dave), intro(carol, dave, alice) | " "$cva_wardclose"
check "CVA wardclose: carol's screen: the group closed, her membership and her listing of herself ended" "C = \[.*view(carol, \[closed($CVA_WC_N), listed(dave, $CVA_WC_N), friend(dave), parent(alice)\], \[alice, dave\], \[\], \[tab('Friends', friend)\]) | " "$cva_wardclose"
check "CVA wardclose: dave's membership ended in the one act, and he comes to know alice" "D = \[.*view(dave, \[friend(bob), friend(carol)\], \[bob, carol\], \[\], \[tab('Friends', friend)\]), view(dave, \[friend(bob), friend(carol)\], \[alice, bob, carol\], \[\], \[tab('Friends', friend)\]) | " "$cva_wardclose"
check "CVA wardclose: alice's screen: she adopted carol and comes to know dave, her person acting once on the close, the request, pending by its request name, child_close(carol, alice, n), with no list in it, until the close commits (vGLP e286276, Section 5.4, \"The compilation\")" "A = \[view(alice, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[\], \[\], \[adopt(alice, carol)\], \[tab('Friends', friend)\]), view(alice, \[\], \[carol\], \[adopt(alice, carol)\], \[tab('Friends', friend)\]), view(alice, \[parenting(carol)\], \[carol\], \[\], \[tab('Friends', friend)\]), view(alice, \[parenting(carol)\], \[carol\], \[child_close(carol, alice, $CVA_WC_N)\], \[tab('Friends', friend)\]), view(alice, \[parenting(carol)\], \[carol\], \[\], \[tab('Friends', friend)\]), view(alice, \[parenting(carol)\], \[carol, dave\], \[\], \[tab('Friends', friend)\]) | " "$cva_wardclose"
check_not "CVA wardclose: alice's person is never asked on the close, and no notice is written: the yield ends no approval" "gave(attempt([a-z]*, [0-9]*, [0-9]*, child_close\|gave(child_close\|refused(\|yielded(" "$cva_wardclose"

# Numbers in act schemas (vGLP 613ee8d, Section 5.4, "Act schemas as local specifications": "an argument of an atom may be a number or a numeric variable, a schema may carry a constraint on its numeric variables, and an atom it adds or deletes may carry a multiplicity, a number or a numeric variable, for that many copies of it.  A numeric variable that occurs only in atoms the schema requires or deletes at one role, and in the constraint, is local to that role; a constraint is local to the role whose local variables it names, and to every role where it names none.  Every other numeric variable is bound by the schema-binding, as a speech-act variable is, and a request names it, so the request form takes it"; vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:11 UTC, items 1 and 3).  vGLP's contract numbers (programs/vglp/cva/contracts.glp): start, refuel(p? ; k) adding k fuel, step(p ; t, t1), unguarded, deleting a fuel and level(t) and adding level(t1), succ(t, t1), and claim(p?, q ; d, t), q's role keeping level(t), t local to it and le(d, t) checked by q's machine.  bob starts, asks for two units of fuel, and his machine steps his level once for each, an unguarded part it opens; alice claims 3 of bob, refused unable by bob's machine, his level 2, and then 2, which commits (numbers/self.glp).
cva_numbers_load=$("$REPL_RUN" <<HEREDOC
$CVA/numbers/
:quit
HEREDOC
2>&1)
check "CVA numbers: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/numbers" "$cva_numbers_load"
check_not "CVA numbers: no type errors" "Type checking failed" "$cva_numbers_load"
check "CVA numbers: refuel's request names k, a number field, and claim's names d; step, unguarded, is not proposed by a person" "^befriend(Peer, Peer) start(Peer) refuel(Peer, Integer) claim(Peer, Peer, Integer) mint(Peer, Integer) spend(Peer, Integer) burn(Peer, Integer, Integer) $" "$(cva_proposal "$CVA/numbers/platform.vglp")"
cva_numbers=$("$REPL_RUN" <<HEREDOC
$CVA/numbers/
:limit 20000000
play_numbers(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA numbers: bob starts at level 0, refuel(bob, 2) adds two copies of fuel, the multiplicity k bound by his request, and his machine takes step once for each, deleting a fuel and level(t), t bound by his state, and adding level(t1), t1 = t + 1, and no more once the fuel is spent" "B = \[view(bob, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[\], \[\], \[start(bob)\], \[tab('Friends', friend)\]), view(bob, \[started, level(0)\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[started, level(0)\], \[\], \[refuel(bob, 2)\], \[tab('Friends', friend)\]), view(bob, \[fuel, fuel, started, level(0)\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[level(1), fuel, started\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[level(2), started\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[level(2), started\], \[\], \[befriend(bob, alice)\], \[tab('Friends', friend)\]), view(bob, \[level(2), started\], \[alice\], \[befriend(bob, alice)\], \[tab('Friends', friend)\])" "$cva_numbers"
check_not "CVA numbers: ... step asks nobody and sends nothing, an attempt of one participant decided at its Propose" "gave(step\|gave(attempt([a-z]*, [0-9]*, [0-9]*, step\|start(attempt([a-z]*, [0-9]*, [0-9]*, step" "$cva_numbers"
check "CVA numbers: alice claims 3 of bob: bob's machine, his level 2, votes no, unable, the constraint le(d, t) local to his role, his state holding every atom the schema requires there, and alice's machine tells the abort, bob its refuser" "sent(alice, bob, start(attempt(alice, 10, 10, claim(alice, bob, 3), none))), sent(bob, alice, vote(attempt(alice, 10, 10, claim(alice, bob, 3), none), no(unable(\[\], \[\])))), sent(alice, bob, outcome(attempt(alice, 10, 10, claim(alice, bob, 3), none), abort(bob, unable(\[\], \[\]))))" "$cva_numbers"
check "CVA numbers: ... her screen shows the notice naming bob, unable" "A = \[.*refused(claim(alice, bob, 3), bob, unable(\[\], \[\]))" "$cva_numbers"
check "CVA numbers: alice claims 2, bob's machine votes yes, and it commits" "sent(alice, bob, start(attempt(alice, 12, 12, claim(alice, bob, 2), none))), sent(bob, alice, vote(attempt(alice, 12, 12, claim(alice, bob, 2), none), yes)), sent(alice, bob, outcome(attempt(alice, 12, 12, claim(alice, bob, 2), none), commit))" "$cva_numbers"
check "CVA numbers: ... alice holds claimed(bob, 2)" "A = \[.*view(alice, \[claimed(bob, 2), friend(bob)\], \[bob\], \[\], \[tab('Friends', friend)\]) | " "$cva_numbers"
check "CVA numbers: ... and bob honoured(alice, 2), his level unchanged" "B = \[.*view(bob, \[honoured(alice, 2), friend(alice), level(2), started\], \[alice\], \[\], \[tab('Friends', friend)\])" "$cva_numbers"
# A required atom with a multiplicity, deleted with none or with a multiplicity (GSC 1c13524, def:numbers; vGLP 4aa7e60, Section 5.4, "Act schemas as local specifications": "an atom required with a multiplicity k and deleted with none having k copies required and one deleted"; "a multiplicity k deleting or adding k copies"; vGLP #23, Code_inbox.md, 2026-11-06 07:17 UTC, 2): rest(K, J, A) among the atoms a role keeps, as GSC's reader writes it.
check "CVA numbers, rest: spend(bob, 3) refused, unable, where bob holds 2 tokens" "B = \[.*refused(spend(bob, 3), bob, unable)" "$cva_numbers"
check "CVA numbers, rest: spend(bob, 2), bob holding 2, commits, leaving one token and spent(2)" "B = \[.*view(bob, \[spent(2), [^]]*token, [^]]*\], \[alice\], \[\], \[tab('Friends', friend)\])" "$cva_numbers"
check "CVA numbers, rest: burn(bob, 5, 2) refused, unable, where bob holds 4 tokens, k required though j is deleted" "B = \[.*refused(burn(bob, 5, 2), bob, unable)" "$cva_numbers"
check "CVA numbers, rest: burn(bob, 4, 3), bob holding 4, commits, 3 deleted, one token left" "B = \[.*view(bob, \[burnt(4, 3), spent(2), [a-z(), 0-9]*token, [a-z(), 0-9]*started\], \[alice\], \[\], \[tab('Friends', friend)\])" "$cva_numbers"
check_not "CVA numbers: bob's person is never asked on a claim, his role not guarding" "gave(claim\|gave(attempt([a-z]*, [0-9]*, [0-9]*, claim" "$cva_numbers"
check_not "CVA numbers: no introduction: no role requires an atom naming an agent bound to a variable" "intro(" "$cva_numbers"

# The introduction at a commit (vGLP 613ee8d, Section 3.2, Definition "Introduction Rule, Introduce Peers": "Commit, and Outcome on a commit, of an attempt of [t]_j at p add the same messages and record for every {q, r} in iota_p([t]_j) with q, r in known_p for which p has not recorded introduced(q, r)"; Section 5.4, "Act schemas as local specifications": the introduction rule of a contract "gives each person p, for a part determined by a schema-binding in which p's role requires an atom naming an agent c bound to a variable and the person q in another role gains an atom naming c, the pair {q, c}: at the commit p, who knows both, introduces them to each other"; vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:11 UTC, item 2).  vGLP's contract intros (programs/vglp/cva/contracts.glp), GKK's acquaint and introduce with no identity: alice comes to know bob and carol, introduces carol to bob, and at the commit her machine introduces bob's and carol's to each other; they exchange ihellos and know each other, and bob meets carol (intros/self.glp).
cva_intros_text=$(cat "$CVA/intros/platform.vglp")
check "CVA introductions: Commit adds to the pairs of the attempt's other participants the pairs the introduction rule gives the agent for the part" "^    iota_known(P?, Id?, St?, Ps2),$" "$cva_intros_text"
check "CVA introductions: ... and Outcome on a commit those it gives a participant" "^outcome_intros(commit, At, Id, St, Ap, Ms?, Ap1?) :- ground(At?), ground(St?) |$" "$cva_intros_text"
cva_intros_load=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:quit
HEREDOC
2>&1)
check "CVA intros: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/intros" "$cva_intros_load"
check_not "CVA intros: no type errors" "Type checking failed" "$cva_intros_load"
cva_intros=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
play_intros(A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA intros: alice comes to know bob and then carol, each a cold call, each asked on her hello and answering yes, each acquaintance pending until it commits" "A = \[view(alice, \[\], \[\], \[\], \[tab('Contacts', knows)\]), view(alice, \[\], \[\], \[acquaint(alice, bob)\], \[tab('Contacts', knows)\]), view(alice, \[\], \[bob\], \[acquaint(alice, bob)\], \[tab('Contacts', knows)\]), view(alice, \[knows(bob)\], \[bob\], \[\], \[tab('Contacts', knows)\]), view(alice, \[knows(bob)\], \[bob\], \[acquaint(alice, carol)\], \[tab('Contacts', knows)\]), view(alice, \[knows(bob)\], \[bob, carol\], \[acquaint(alice, carol)\], \[tab('Contacts', knows)\]), view(alice, \[knows(carol), knows(bob)\], \[bob, carol\], \[\], \[tab('Contacts', knows)\])" "$cva_intros"
check "CVA intros: alice introduces carol to bob, bob's machine asks him and votes yes, and at the commit alice's machine sends intro(carol) to bob and intro(bob) to carol, which no person asked for" "sent(alice, bob, start(attempt(alice, 6, 6, introduce(alice, bob, carol), none))), sent(bob, alice, vote(attempt(alice, 6, 6, introduce(alice, bob, carol), none), yes)), sent(alice, bob, outcome(attempt(alice, 6, 6, introduce(alice, bob, carol), none), commit)), intro(alice, bob, carol), intro(alice, carol, bob)" "$cva_intros"
check "CVA intros: ... bob gains knows(carol), and his machine, introduced to carol's, comes to know her by the ihello exchange" "B = \[.*gave(introduce(alice, bob, carol), yes), view(bob, \[knows(alice)\], \[alice\], \[introduce(alice, bob, carol)\], \[tab('Contacts', knows)\]), view(bob, \[knows(carol), knows(alice)\], \[alice\], \[\], \[tab('Contacts', knows)\]), view(bob, \[knows(carol), knows(alice)\], \[alice, carol\], \[\], \[tab('Contacts', knows)\])" "$cva_intros"
check "CVA intros: ... carol, not a participant and not asked, comes to know bob likewise" "C = \[view(carol, \[\], \[\], \[\], \[tab('Contacts', knows)\]), gave(acquaint(alice, carol), yes), view(carol, \[\], \[alice\], \[\], \[tab('Contacts', knows)\]), view(carol, \[\], \[alice\], \[acquaint(alice, carol)\], \[tab('Contacts', knows)\]), view(carol, \[knows(alice)\], \[alice\], \[\], \[tab('Contacts', knows)\]), view(carol, \[knows(alice)\], \[alice, bob\], \[\], \[tab('Contacts', knows)\])" "$cva_intros"
check_not "CVA intros: ... carol's person is never asked on the introduction" "C = \[.*gave(introduce" "$cva_intros"
check "CVA intros: bob asks to come to know carol, whom his machine knows by the introduction, she is asked and answers yes, and it commits" "sent(bob, carol, start(attempt(bob, 8, 8, acquaint(bob, carol), none))), sent(carol, bob, vote(attempt(bob, 8, 8, acquaint(bob, carol), none), yes)), sent(bob, carol, outcome(attempt(bob, 8, 8, acquaint(bob, carol), none), commit))" "$cva_intros"
check "CVA intros: ... bob holds knows(carol) twice, the introduction's and the acquaintance's, and carol knows(bob)" "B = \[.*view(bob, \[knows(carol), knows(carol), knows(alice)\], \[alice, carol\], \[\], \[tab('Contacts', knows)\]) | " "$cva_intros"
check "CVA intros: ... carol, bob known to her, so that no hello is sent, is asked on his attempt; her answer, pending until it commits, and her state" "C = \[.*gave(acquaint(bob, carol), yes), view(carol, \[knows(alice)\], \[alice, bob\], \[acquaint(bob, carol)\], \[tab('Contacts', knows)\]), view(carol, \[knows(bob), knows(alice)\], \[alice, bob\], \[\], \[tab('Contacts', knows)\]) | " "$cva_intros"
check_not "CVA intros: an acquaintance introduces no one: its roles require no atom naming an agent bound to a variable" "intro(alice, bob, alice)\|intro(alice, carol, alice)\|intro(bob, carol, bob)" "$cva_intros"
# A reading role (vGLP dd37d2a, Section 5.4, "Speech-act schemas as local specifications": "its person is a participant of the transaction whose state does not change, as a stationary agent is, its machine votes on the atoms the schema requires and forbids at it, and it is asked nothing"; GSC 0380a8a): intros' peek(p?, q), q requiring open and adding and deleting nothing, run by trio/7 of programs/vglp/cva/intros/self.glp on its scripts, carol's empty.
cva_reading=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(peek(A2?, bob))), wait(holds(seen(bob)))], [own([B1], propose(open(B1?))), wait(holds(open)), approval(acquaint(alice, bob), yes)], [], A, B, C, N).
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(peek(A2?, bob))), wait(refused(peek(alice, bob), bob, unable([open], [])))], [approval(acquaint(alice, bob), yes)], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA reading role: alice peeks at bob, whose role is a reading role; bob's machine is told the start, votes yes, holding open, and is told the commit" "sent(alice, bob, start(attempt(alice, 4, 4, peek(alice, bob), none))), sent(bob, alice, vote(attempt(alice, 4, 4, peek(alice, bob), none), yes)), sent(alice, bob, outcome(attempt(alice, 4, 4, peek(alice, bob), none), commit))" "$cva_reading"
check "CVA reading role: ... alice gains seen(bob)" "A = \[.*view(alice, \[seen(bob), knows(bob)\], \[bob\], \[\], \[tab('Contacts', knows)\]) | " "$cva_reading"
check "CVA reading role: ... bob's machine applies no effect: his state after the commit is the state before it, knows(alice) and open" "B = \[view(bob, \[\], \[\], \[\], \[tab('Contacts', knows)\]), view(bob, \[\], \[\], \[open(bob)\], \[tab('Contacts', knows)\]), view(bob, \[open\], \[\], \[\], \[tab('Contacts', knows)\]), gave(acquaint(alice, bob), yes), view(bob, \[open\], \[alice\], \[\], \[tab('Contacts', knows)\]), view(bob, \[open\], \[alice\], \[acquaint(alice, bob)\], \[tab('Contacts', knows)\]), view(bob, \[knows(alice), open\], \[alice\], \[\], \[tab('Contacts', knows)\]) | " "$cva_reading"
check_not "CVA reading role: ... and bob is asked nothing on peek" "gave(attempt(alice, [0-9]*, [0-9]*, peek\|gave(peek" "$cva_reading"
check "CVA reading role: bob not open, his machine votes no, unable, the atom peek requires at his role lacking, and alice's machine aborts the attempt, bob its refuser" "sent(bob, alice, vote(attempt(alice, 4, 4, peek(alice, bob), none), no(unable(\[open\], \[\])))), sent(alice, bob, outcome(attempt(alice, 4, 4, peek(alice, bob), none), abort(bob, unable(\[open\], \[\]))))" "$cva_reading"
check "CVA reading role: ... alice is told the refusal" "refused(peek(alice, bob), bob, unable(\[open\], \[\])) | " "$cva_reading"
check "CVA reading role: ... and bob's state is knows(alice) alone" "B = \[view(bob, \[\], \[\], \[\], \[tab('Contacts', knows)\]), gave(acquaint(alice, bob), yes), view(bob, \[\], \[alice\], \[\], \[tab('Contacts', knows)\]), view(bob, \[\], \[alice\], \[acquaint(alice, bob)\], \[tab('Contacts', knows)\]), view(bob, \[knows(alice)\], \[alice\], \[\], \[tab('Contacts', knows)\]) | " "$cva_reading"
# Signing conditions evaluated at the binding (vGLP dd37d2a, Section 5.4: "signed(u, y) and unsigned(u, y), that the person bound to u signed the speech act bound to y or did not --- which is evaluated when the schema is bound"; GSC 10fcc92, def:numbers: "signed(u, y) holding when beta(u) is the signer of beta(y) or of a speech act inside it and unsigned(u, y) otherwise"): intros' send(p, q ; x), unsigned(q, x), and credit(p, q ; x), signed(q, x), unguarded, opened by alice on the items she posts, a signed by her and b signed by bob and again by her.
cva_signing_text=$(cat "$CVA/intros/platform.vglp")
check "CVA signing conditions: the printed constraint type takes signed and unsigned" "^Constraint ::= .* ; signed(Var, Var) ; unsigned(Var, Var).\$" "$cva_signing_text"
check "CVA signing conditions: send's constraint printed as the contract writes it, local to every role" "^schema_numeric(send, numeric(unsigned(role(2), svar(x)), \[\], every))\$" "$cva_signing_text"
check "CVA signing conditions: ... credit's" "^schema_numeric(credit, numeric(signed(role(2), svar(x)), \[\], every))\$" "$cva_signing_text"
cva_signing=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2, A3], propose(post(A2?, speech(utterance(a), A3?)))), own([A4, A5], propose(post(A4?, speech(speech(utterance(b), bob), A5?)))), wait(holds(sent(speech(utterance(a), alice), bob))), wait(holds(credited(speech(speech(utterance(b), bob), alice), bob)))], [approval(acquaint(alice, bob), yes), wait(holds(got(speech(utterance(a), alice)))), wait(holds(credit(speech(speech(utterance(b), bob), alice))))], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA signing conditions: alice's machine sends bob a, which bob did not sign, unsigned(bob, a) holding at the binding" "sent(alice, bob, outcome(attempt(alice, [0-9]*, [0-9]*, send(alice, bob, speech(utterance(a), alice)), none), commit))" "$cva_signing"
check "CVA signing conditions: ... and credits bob b, which bob signed inside it, signed(bob, b) holding at the binding" "sent(alice, bob, outcome(attempt(alice, [0-9]*, [0-9]*, credit(alice, bob, speech(speech(utterance(b), bob), alice)), none), commit))" "$cva_signing"
check_not "CVA signing conditions: ... and neither sends b, which bob signed, nor credits a, which he did not" "send(alice, bob, speech(speech(utterance(b), bob), alice))\|credit(alice, bob, speech(utterance(a), alice))" "$cva_signing"
check "CVA signing conditions: ... bob holds credit(b), cite of the same contract committing beside it" "B = \[.*view(bob, \[[^]]*credit(speech(speech(utterance(b), bob), alice))[^]]*knows(alice)\], \[alice\], \[\], \[tab('Contacts', knows)\]) | " "$cva_signing"
check "CVA signing conditions: ... and got(a)" "B = \[.*view(bob, \[[^]]*got(speech(utterance(a), alice))[^]]*knows(alice)\], \[alice\], \[\], \[tab('Contacts', knows)\]) | " "$cva_signing"
check "CVA signing conditions: ... the run suspends, none of alice's own transactions holding" "→ suspended" "$cva_signing"
# The author of a speech act in a constraint, evaluated at the binding (vGLP 27bae5d, Section 5.4, "Speech-act schemas as local specifications": "the signer, the author and the item of a speech act, its last signer, its first signer and its utterance"; GSC 65a26da, def:numbers, "the author author(y), the first signer of y"): intros' cite(p, q ; x), unguarded, author(x) = q, opened by alice on the items she posts, a signed by her, its author alice, and b signed by bob and again by her, its author bob.
check "CVA author: cite's constraint printed as the contract writes it, local to every role" "^schema_numeric(cite, numeric(eq(author_of(svar(x)), role(2)), \[\], every))\$" "$cva_signing_text"
check "CVA author: the printed expression type takes the author and the item of a speech act" "^Expr      ::= .* ; author_of(Var) ; item_of(Var).\$" "$cva_signing_text"
cva_author=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2, A3], propose(post(A2?, speech(utterance(a), A3?)))), own([A4, A5], propose(post(A4?, speech(speech(utterance(b), bob), A5?)))), wait(holds(cited(speech(speech(utterance(b), bob), alice), bob)))], [approval(acquaint(alice, bob), yes), wait(holds(cites(speech(speech(utterance(b), bob), alice))))], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA author: alice's machine cites to bob b, whose first signer, its author, is bob, author(b) = bob holding at the binding" "sent(alice, bob, outcome(attempt(alice, [0-9]*, [0-9]*, cite(alice, bob, speech(speech(utterance(b), bob), alice)), none), commit))" "$cva_author"
check_not "CVA author: ... and not a, whose author is alice" "cite(alice, bob, speech(utterance(a), alice))" "$cva_author"
check "CVA author: ... bob holds cites(b)" "B = \[.*view(bob, \[cites(speech(speech(utterance(b), bob), alice))" "$cva_author"
check "CVA author: ... the run suspends" "→ suspended" "$cva_author"
# Standing requests (vGLP 7c631b1, Section 5.4, "The contract's words": "An implementation pragma may make the acts of a clause standing, as `Encounter: Alice stands on [Open place] until [Close place]' does ...: the person's request of the act of the first button is also their standing request (Definition Local) of every class of the clause's acts that they propose, and their request of the act of the second withdraws it; a standing act has no button, every guarding role of it proposes it, and its persons are asked nothing, their machines voting yes on their standing approvals"; Definition "Proposing", Stand and Withdraw).  The declaration's term, standing(Schema, Start, Stop), is the compiler's, vGLP's own contracts giving it (contracts.glp, pragmas_named/2) and pragmas.sh reading it from a NAME.txt's standing line (GSC 1a8e31c, Section 2).  intros' greet(p, q), guarded by both, requiring here and knows of the other at each, stands on here: arrive makes the standing request and leave withdraws it.
check "CVA standing: the platform prints greet standing on arrive" "^schema_standing(greet, on(arrive))\$" "$cva_signing_text"
check "CVA standing: ... arrive making the standing request" "^stand_act(arrive, makes(greet))\$" "$cva_signing_text"
check "CVA standing: ... leave withdrawing it" "^stand_act(leave, withdraws(greet))\$" "$cva_signing_text"
cva_standing=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(arrive(A2?))), wait(holds(greeted(bob)))], [approval(acquaint(alice, bob), yes), wait(known(alice)), own([B1], propose(arrive(B1?))), wait(holds(greeted(alice)))], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA standing: alice and bob each arrive, a standing request of every greet they propose; a greet commits, its guards asked nothing" "outcome(attempt([a-z]*, [0-9]*, [0-9]*, greet([a-z]*, [a-z]*), none), commit)" "$cva_standing"
check_not "CVA standing: ... neither person asked about greet" "gave(greet\|gave(attempt([a-z]*, [0-9]*, [0-9]*, greet" "$cva_standing"
check "CVA standing: ... alice holds greeted(bob)" "A = \[.*view(alice, \[greeted(bob), here, knows(bob)\]" "$cva_standing"
check "CVA standing: ... bob holds greeted(alice)" "B = \[.*view(bob, \[greeted(alice), here, knows(alice)\]" "$cva_standing"
check "CVA standing: ... and the run suspends, the standing approval of the class committed having ceased" "→ suspended" "$cva_standing"
# Withdraw: alice arrives, a standing request, and leaves, which withdraws it; bob arrives after.  No greet commits, alice not here, and the run suspends.
cva_withdraw=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(arrive(A2?))), wait(holds(here)), own([A3], propose(leave(A3?)))], [approval(acquaint(alice, bob), yes), wait(known(alice)), own([B1], propose(arrive(B1?))), wait(holds(here))], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check_not "CVA standing: alice withdraws by leaving before bob arrives: no greet commits" "outcome(attempt([a-z]*, [0-9]*, [0-9]*, greet([a-z]*, [a-z]*), none), commit)" "$cva_withdraw"
check "CVA standing: ... alice holds knows(bob) alone" "A = \[.*view(alice, \[knows(bob)\], \[bob\], \[\], \[tab('Contacts', knows)\]) | " "$cva_withdraw"
check "CVA standing: ... and the run suspends" "→ suspended" "$cva_withdraw"
# Ready and Readied (vGLP 1d84a35, Definition "Proposing": "where every approval of [t] pending at p is standing, moreover s in dom(f), and, where another participant q refused p's last attempt beta of [t]_j as unable, p has recorded ready(beta, q)"; Ready, "by a participant p != i of an attempt alpha of [t]_j ... p has recorded voted(alpha, no) and neither answered(alpha, no) nor readied(alpha), and s in dom(f): p records readied(alpha) and adds message(p, i, ready(alpha))"; Readied, the initiator recording ready(alpha, q)).  The platform prints them, and two plays run them.  No show: alice arrives, a standing request of greet, bob, known to her, never arrives; her machine proposes greet(alice, bob) once, bob's refuses it as unable, lacking here, and under her standing approval alone she proposes it no more, waiting for his ready; the run suspends.  Ready: bob arrives only after refusing, his arrival waiting on carol's acquaintance, which alice starts once she has arrived; once here, his machine, having refused alice's attempt as unable, tells her ready(alpha), and a greet of the two commits, each person asked nothing.
check "CVA ready: the platform prints the cargo ready(Attempt)" "^            ; yield(Attempt) ; ready(Attempt) ; request(Rho, Integer).$" "$cva_signing_text"
check "CVA ready: ... the records readied, ready and unable" "^            ; readied(Attempt) ; ready(Attempt, Peer) ; unable(Attempt, Peer)$" "$cva_signing_text"
check "CVA ready: ... Ready as an own transaction, its effect recording readied(At) and telling the initiator ready(At)" "^ready_effect(At, St, \[to(P?, ready(At?))\], St1?) :- ground(At?), ground(St?) |$" "$cva_signing_text"
check "CVA ready: ... Readied on the message, recording ready(At, F)" "^    add_rec(ready(At?, F?), Ap?, Ap1),$" "$cva_signing_text"
check "CVA ready: ... and under standing approvals alone the parts proposed are those the agent is ready for whose last refusal as unable has its ready, none refused at once" "^    readied_parts(Rs?, I?, R?, Rs1),$" "$cva_signing_text"
cva_noshow=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 20000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(arrive(A2?))), wait(holds(here))], [approval(acquaint(alice, bob), yes), wait(known(alice))], [], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA ready, no show: alice's machine proposes greet(alice, bob) and bob's refuses it as unable, lacking here" "sent(bob, alice, vote(attempt(alice, [0-9]*, [0-9]*, greet(alice, bob), none), no(unable(\[here\], \[\]))))" "$cva_noshow"
check_not "CVA ready, no show: ... and, under her standing approval alone, she proposes it no more" "start(attempt(alice, [0-9]*, [0-9]*, greet(alice, bob), none)))[^|]*start(attempt(alice, [0-9]*, [0-9]*, greet(alice, bob), none)))" "$cva_noshow"
check_not "CVA ready, no show: ... no greet commits" "greet([a-z]*, [a-z]*), none), commit)" "$cva_noshow"
check "CVA ready, no show: ... and the run suspends, not stopped at the limit" "→ suspended" "$cva_noshow"
cva_ready=$("$REPL_RUN" <<HEREDOC
$CVA/intros/
:limit 30000000
trio([own([A1], propose(acquaint(A1?, bob))), wait(holds(knows(bob))), own([A2], propose(arrive(A2?))), wait(holds(here)), own([A3], propose(acquaint(A3?, carol))), wait(holds(greeted(bob)))], [approval(acquaint(alice, bob), yes), approval(acquaint(carol, bob), yes), wait(known(carol)), own([B1], propose(arrive(B1?))), wait(holds(greeted(alice)))], [approval(acquaint(alice, carol), yes), wait(known(alice)), own([C1], propose(acquaint(C1?, bob)))], A, B, C, N).
:quit
HEREDOC
2>&1)
check "CVA ready: bob's machine refuses alice's greet as unable, bob not here" "sent(alice, bob, outcome(attempt(alice, [0-9]*, [0-9]*, greet(alice, bob), none), abort(bob, unable(\[here\], \[\]))))" "$cva_ready"
check "CVA ready: ... and once bob is here tells her ready" "sent(bob, alice, ready(attempt(alice, [0-9]*, [0-9]*, greet(alice, bob), none)))" "$cva_ready"
check "CVA ready: ... a greet of alice and bob commits" "greet(alice, bob), none), commit)" "$cva_ready"
check_not "CVA ready: ... bob refuses alice's greet as unable once" "greet(alice, bob), none), abort(bob, unable[^|]*greet(alice, bob), none), abort(bob, unable" "$cva_ready"
check_not "CVA ready: ... neither person asked about greet" "gave(greet\|gave(attempt([a-z]*, [0-9]*, [0-9]*, greet" "$cva_ready"
check "CVA ready: ... alice holds greeted(bob)" "A = \[.*view(alice, \[greeted(bob), knows(carol), here, knows(bob)\]" "$cva_ready"
check "CVA ready: ... and the run suspends" "→ suspended" "$cva_ready"
# The compiler's checks of a standing declaration (cvac.glp, standing_faults): a schema of the contract (st_none), guarded (st_unguarded), its two acts schemas of the contract (st_noact), each of whose guarding roles requires atoms naming every other role and variable, so that "the guarded parts with no opener of classes of rho that p proposes with s in dom(f) are finitely many and computable from p, s and rho" (Definition "Effective Local Specification") (st_unnamed, GPE's Encounter as its schema stands, Alice's atoms naming no Bob); and a schema declared standing needs no button (st_ok).
cva_stfx="schema(open, [], [rs(guarded, [atom(open, [])], [], [], [atom(open, [])])]), schema(close, [], [rs(guarded, [], [atom(open, [])], [atom(open, [])], [])]), schema(greet, [], [rs(guarded, [atom(greeted, [role(2)])], [], [atom(open, []), atom(knows, [role(2)])], []), rs(guarded, [atom(greeted, [role(1)])], [], [atom(open, []), atom(knows, [role(1)])], [])])"
cva_stmeet="schema(open, [], [rs(guarded, [atom(open, [])], [], [], [atom(open, [])])]), schema(close, [], [rs(guarded, [], [atom(open, [])], [atom(open, [])], [])]), schema(meet, [], [rs(guarded, [atom(met, [role(2)])], [], [atom(open, [])], []), rs(guarded, [atom(met, [role(1)])], [], [atom(open, [])], [])])"
cva_stwave="schema(open, [], [rs(guarded, [atom(open, [])], [], [], [atom(open, [])])]), schema(close, [], [rs(guarded, [], [atom(open, [])], [atom(open, [])], [])]), schema(wave, [], [rs(unguarded, [atom(waved, [])], [], [atom(open, [])], [atom(waved, [])])])"
cva_stp="[list('\"Open\"', [open])], [act(open, '\"Open\"', [], none, '\"Open\"', by([1])), act(close, '\"Close\"', [], none, '\"Open\"', by([1]))]"
cva_standing_fx=$("$REPL_RUN" <<HEREDOC
$CVA/compiler/
:limit 100000000
compile(st_ok, [$cva_stfx], syntactically_grassroots, no_introduction, pragmas($cva_stp, [standing(greet, open, close)])).
compile(st_noact, [$cva_stfx], syntactically_grassroots, no_introduction, pragmas($cva_stp, [standing(greet, open, shut)])).
compile(st_none, [$cva_stfx], syntactically_grassroots, no_introduction, pragmas($cva_stp, [standing(nope, open, close)])).
compile(st_unnamed, [$cva_stmeet], syntactically_grassroots, no_introduction, pragmas($cva_stp, [standing(meet, open, close)])).
compile(st_unguarded, [$cva_stwave], syntactically_grassroots, no_introduction, pragmas($cva_stp, [standing(wave, open, close)])).
:quit
HEREDOC
2>&1)
check "CVA standing: a guarded schema declared standing, its roles naming each other, compiled with no button (st_ok)" "the canonical CVA platform of the contract(st_ok)" "$cva_standing_fx"
check_not "CVA standing: ... and not refused (st_ok)" "not compiled(st_ok)" "$cva_standing_fx"
check "CVA standing: a declaration naming no schema, refused (st_none)" "a standing declaration names no schema of the contract" "$cva_standing_fx"
check "CVA standing: a schema whose guarding roles name not each other, its standing parts not computable from a role's state, refused (st_unnamed)" "its standing parts are not computable from that role" "$cva_standing_fx"
check "CVA standing: a schema with no guarding role, refused (st_unguarded)" "a standing declaration names a schema with no guarding role" "$cva_standing_fx"
check "CVA standing: a declaration whose act withdrawing it is no schema of the contract, refused (st_noact)" "names, as the act that makes or withdraws the standing request, no schema of the contract.*shut" "$cva_standing_fx"
# The standing line of a contract's implementation pragmas (GSC 1a8e31c, Section 2: "``Encounter: Alice stands on [Open place] until [Close place]'' says that Alice's act Open place is her standing request of every act of Encounter she guards ... and that her act Close place withdraws it"; vGLP 7c631b1, Section 5.4), read by pragmas.sh as standing(S, A, W) for each schema S of the clause: XG's Encounter stands on [Open place] until [Close place]; XW's line is not of that form, refused.
cva_ps=$(mktemp -d)
printf '%s\n' '% Extends: nothing.' 'XG' 'Alice and Bob are persons.' '1. ■ Open Place.  Alice may declare the place open, and may close it.' '2. ■ Encounter.  Alice and Bob, who have both declared the place open, may know each other.' 'Interface pragmas' 'Contacts: the persons Alice knows.' 'Open Place: Alice [Open place] [Close place]' 'Implementation pragmas' 'Encounter: Alice stands on [Open place] until [Close place]; the two phones exchange keys.' > "$cva_ps/XG.txt"
printf '%s\n' '% Extends: nothing.' 'XW' 'Alice and Bob are persons.' '1. ■ Open Place.  Alice may declare the place open, and may close it.' '2. ■ Encounter.  Alice and Bob, who have both declared the place open, may know each other.' 'Interface pragmas' 'Contacts: the persons Alice knows.' 'Open Place: Alice [Open place] [Close place]' 'Implementation pragmas' 'Encounter: Alice stands on [Open place].' > "$cva_ps/XW.txt"
cva_ps_term="[schema(open_place, ['Alice'], [], [rs(guarded, [atom(open, [])], [], [], [atom(open, [])])]), schema(close_place, ['Alice'], [], [rs(guarded, [], [atom(open, [])], [atom(open, [])], [])]), schema(encounter, ['Alice', 'Bob'], [], [rs(guarded, [atom(knows, [role(2)])], [], [atom(open, [])], []), rs(guarded, [atom(knows, [role(1)])], [], [atom(open, [])], [])])]"
cva_ps_ok=$(bash "$CVA/compiler/pragmas.sh" "$cva_ps/XG.txt" "$cva_ps_term" 2>&1; echo "exit $?")
cva_ps_bad=$(bash "$CVA/compiler/pragmas.sh" "$cva_ps/XW.txt" "$cva_ps_term" 2>&1; echo "exit $?")
rm -rf "$cva_ps"
check "CVA standing: the standing line read, encounter standing on open_place until close_place (XG)" "\[standing('encounter', 'open_place', 'close_place')\])$" "$cva_ps_ok"
check "CVA standing: ... and pragmas.sh exits 0 (XG)" "^exit 0$" "$cva_ps_ok"
check "CVA standing: a standing line of another form, refused (XW)" "is not \"<Clause>: <Name> stands on \[<Start>\] until \[<Stop>\]\"" "$cva_ps_bad"
check "CVA standing: ... and pragmas.sh exits 1 (XW)" "^exit 1$" "$cva_ps_bad"

# GB's mint_bond, advance and redeem_bc (vGLP 613ee8d, Section 5.4, "Act schemas as local specifications"; vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:11 UTC, items 1 and 3: "GB: d, k and r by the request; t local to the issuer's role, its date; mint_bond's gt(d, t) checked by the minter, redeem_bc's le(d, t) by the issuer").  vGLP's contract bonds (programs/vglp/cva/contracts.glp): GB's three schemas as programs/currencies/bonds/GB.glp writes them, with GKK's establish and acquaint, GB's mint and swap_cb, and dawn, which gives a person their first date, date(0), GB naming no act that does (Code #12 to Currencies, 2026-10-20 22:23 UTC).  advance, unguarded, its t local to its role and t1 given by succ, is opened by a dated person's machine whenever it can, so the run does not suspend and is stopped at the REPL's :limit (bonds/self.glp).  A request of mint_bond whose guard is not ready is dropped with the notice refused(N, p, unable), and a request of redeem_bc proposes the redeem part, the part it names, guarded by the bearer, never the swap part its class holds (vGLP 1423679, Section 5.4, "The compilation"; vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, items 4 and 7): the bond maturing at 1000000 refused by the issuer's machine, le(d, t) failing, and the one maturing at 10 redeemed.
cva_bonds_load=$("$REPL_RUN" <<HEREDOC
$CVA/bonds/
:quit
HEREDOC
2>&1)
check "CVA bonds: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/bonds" "$cva_bonds_load"
check_not "CVA bonds: no type errors" "Type checking failed" "$cva_bonds_load"
check "CVA bonds: mint_bond's request names d and k, numbers, and not the minter's date t, local to her role; advance, unguarded, is not proposed by a person" "^establish(Peer) acquaint(Peer, Peer) dawn(Peer) mint(Peer, Integer) mint_bond(Peer, Integer, Integer) swap_cb(Peer, Peer, Peer, Integer, Peer, Integer, Integer) redeem_bc(Peer, Peer, Integer, Peer) $" "$(cva_proposal "$CVA/bonds/platform.vglp")"
cva_bonds=$("$REPL_RUN" <<HEREDOC
$CVA/bonds/
:limit 5000000
play_bonds(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA bonds: alice's machine advances her date, date(t) deleted and date(t1) added, t1 = t + 1, an unguarded part it opens" "A = \[.*view(alice, \[date([1-9][0-9]), " "$cva_bonds"
check_not "CVA bonds: ... advance asks nobody and sends nothing, an attempt of one participant decided at its Propose" "gave(advance\|gave(attempt([a-z]*, [0-9]*, [0-9]*, advance\|advance(alice" "$cva_bonds"
check "CVA bonds: ... without end: the run is stopped at the REPL's :limit" "capped" "$cva_bonds"
check "CVA bonds: alice mints a bond maturing at 10 at a date below it, gt(d, t) checked by her machine, t her date" "A = \[.*bond(alice, 10)" "$cva_bonds"
check "CVA bonds: ... and one maturing at 1000000" "A = \[.*bond(alice, 1000000)" "$cva_bonds"
check_not "CVA bonds: no bond maturing at 0 is minted, gt(0, t) failing at every date" "bond(alice, 0)" "$cva_bonds"
check "CVA bonds: ... the request of it dropped with the notice refused(mint_bond(alice, 0, 1), alice, unable), her machine ready for no part it names (vGLP, Section 5.4, \"The compilation\")" "A = \[.*refused(mint_bond(alice, 0, 1), alice, unable)" "$cva_bonds"
check "CVA bonds: alice mints two coins, mint(alice, 2), the multiplicity k bound by her request" "A = \[.*coin(alice), coin(alice), " "$cva_bonds"
check "CVA bonds: ... and bob likewise" "B = \[view(bob, \[\], \[\], \[\], \[tab('Contacts', knows)\]), view(bob, \[\], \[\], \[establish(bob)\], \[tab('Contacts', knows)\]), view(bob, \[identity\], \[\], \[\], \[tab('Contacts', knows)\]), view(bob, \[identity\], \[\], \[mint(bob, 2)\], \[tab('Contacts', knows)\]), view(bob, \[coin(bob), coin(bob), identity\], \[\], \[\], \[tab('Contacts', knows)\])" "$cva_bonds"
check "CVA bonds: bob swaps a coin of his for each of alice's bonds, she asked and answering yes" "sent(bob, alice, outcome(attempt(bob, 8, 8, swap_cb(bob, alice, bob, 1, alice, 1000000, 1), none), commit)), .*sent(bob, alice, outcome(attempt(bob, 10, 10, swap_cb(bob, alice, bob, 1, alice, 10, 1), none), commit))" "$cva_bonds"
check "CVA bonds: ... bob holds both bonds" "B = \[.*view(bob, \[bond(alice, 10), bond(alice, 1000000), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows)\])" "$cva_bonds"
check "CVA bonds: bob's request to redeem the bond maturing at 1000000 proposes the redeem part, the part his request names, guarded by him, alice not asked (vGLP #20 Cowork to Code, 2026-10-23 22:09 UTC, item 4); her machine votes no, unable, le(d, t) failing at her date" "sent(bob, alice, start(attempt(bob, 12, 12, redeem_bc(bob, alice, 1000000, alice), none))), sent(alice, bob, vote(attempt(bob, 12, 12, redeem_bc(bob, alice, 1000000, alice), none), no(unable(\[\], \[\])))), sent(bob, alice, outcome(attempt(bob, 12, 12, redeem_bc(bob, alice, 1000000, alice), none), abort(alice, unable(\[\], \[\]))))" "$cva_bonds"
check "CVA bonds: ... his screen shows the notice, alice its refuser, unable" "B = \[.*refused(redeem_bc(bob, alice, 1000000, alice), alice, unable(\[\], \[\]))" "$cva_bonds"
check "CVA bonds: ... the bond maturing at 10, her date past it: her machine votes yes, and it commits" "sent(bob, alice, start(attempt(bob, 14, 14, redeem_bc(bob, alice, 10, alice), none))), sent(alice, bob, vote(attempt(bob, 14, 14, redeem_bc(bob, alice, 10, alice), none), yes)), sent(bob, alice, outcome(attempt(bob, 14, 14, redeem_bc(bob, alice, 10, alice), none), commit))" "$cva_bonds"
check "CVA bonds: ... bob holds coin(alice) in its place" "B = \[.*view(bob, \[coin(alice), bond(alice, 1000000), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows)\])" "$cva_bonds"
check_not "CVA bonds: ... alice asked on no redemption and on no swap back" "A = \[.*gave(redeem_bc\|A = \[.*gave(swap_cb(alice\|A = \[.*gave(attempt(bob, 1[24]" "$cva_bonds"

# The day (vGLP 1423679, Section 5.4, "Act schemas as local specifications": "A contract may require the day: the compiled agent reads, with its triggers, a calendar, a stream of dates the device writes, one a day, and holds today(d), d the last date read, beside its app state, which no schema adds or deletes and which a schema may require at a role and constrain a numeric variable by ..., so that it is proposed once a day"; "The compilation": the agent goal reads "where the contract requires the day, the calendar, on each date of which it replaces today(d) and puts up again the starts and requests it holds"; vGLP #20 Cowork to Code, 2026-10-23 23:35 UTC, item 12).  vGLP's contract days (programs/vglp/cva/contracts.glp): dawn(p?), adding dated and date(0), and advance(p ; t, d), unguarded, requiring today(d) and date(t) with d > t; its play gives alice a calendar of the dates 1, 2 and 3, each written once her view shows the date before it (days/self.glp).
cva_days_load=$("$REPL_RUN" <<HEREDOC
$CVA/days/
:quit
HEREDOC
2>&1)
check "CVA days: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/days" "$cva_days_load"
check_not "CVA days: no type errors" "Type checking failed" "$cva_days_load"
cva_days=$("$REPL_RUN" <<HEREDOC
$CVA/days/
:limit 600000
play_days(A).
:quit
HEREDOC
2>&1)
check "CVA days: alice takes her first date, date(0), and on each date of her calendar her machine advances her date once, date(d) replacing date(t), an own transaction of an unguarded part it opens, proposed once a day" "A = \[view(alice, \[\], \[\], \[\], \[tab('Dates', date)\]), view(alice, \[\], \[\], \[dawn(alice)\], \[tab('Dates', date)\]), view(alice, \[dated, date(0)\], \[\], \[\], \[tab('Dates', date)\]), view(alice, \[date(1), dated\], \[\], \[\], \[tab('Dates', date)\]), view(alice, \[date(2), dated\], \[\], \[\], \[tab('Dates', date)\]), view(alice, \[date(3), dated\], \[\], \[\], \[tab('Dates', date)\]) | " "$cva_days"
check_not "CVA days: ... the view never shows today(d), no atom of the machine state the platform mapping reads" "A = .*today(" "$cva_days"
check "CVA days: ... and the run suspends, the calendar ended" "→ suspended" "$cva_days"

# A constraint in GSC's form, "a conjunction of linear equalities and inequalities over its value variables and values" (GSC bcc1891, Definition "Schema with values"; programs/gsc at GSC-unob3 62fb0342), its equality giving the variable of an added atom its value once the atoms its first role requires bind the others (vGLP, Section 5.4, "Act schemas as local specifications": "or the constraint gives the variable finitely many values once those are bound").  vGLP's contract linear (programs/vglp/cva/contracts.glp): start and refuel, as numbers has them; climb(p ; t, t1) | t1 = t + 2 and t < 6, and mark(p ; t, m) | 3m = t + 3, each unguarded and opened by p, t local to p's role (linear/self.glp).
cva_linear_load=$("$REPL_RUN" <<HEREDOC
$CVA/linear/
:quit
HEREDOC
2>&1)
check "CVA linear: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/linear" "$cva_linear_load"
check_not "CVA linear: no type errors" "Type checking failed" "$cva_linear_load"
check "CVA linear: climb's constraint printed as the contract writes it, checked by its role, t local to it" "^schema_numeric(climb, numeric(and(eq(num(t1), plus(num(t), num(2))), lt(num(t), num(6))), \[num(t1), num(t)\], at(1)))$" "$(cat "$CVA/linear/platform.vglp")"
cva_linear=$("$REPL_RUN" <<HEREDOC
$CVA/linear/
:limit 3000000
play_linear(A).
:quit
HEREDOC
2>&1)
check "CVA linear: alice starts at level 0 and refuels 5; her machine marks level 0, 3m = 0 + 3 giving m = 1, and climbs from 0 to 2, 4 and 6, t1 = t + 2, each climb deleting a fuel, and not from 6, where t < 6 fails, two fuel left; and it marks level 6, m = 3" "A = \[view(alice, \[\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[\], \[\], \[start(alice)\], \[tab('Levels', level)\]), view(alice, \[started, level(0)\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[marked(1), started, level(0)\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[marked(1), started, level(0)\], \[\], \[refuel(alice, 5)\], \[tab('Levels', level)\]), view(alice, \[fuel, fuel, fuel, fuel, fuel, marked(1), started, level(0)\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[level(2), fuel, fuel, fuel, fuel, marked(1), started\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[level(4), fuel, fuel, fuel, marked(1), started\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[level(6), fuel, fuel, marked(1), started\], \[\], \[\], \[tab('Levels', level)\]), view(alice, \[marked(3), level(6), fuel, fuel, marked(1), started\], \[\], \[\], \[tab('Levels', level)\]) | " "$cva_linear"
check_not "CVA linear: ... no mark at the levels 2 and 4, where 3m = t + 3 has no whole number m, and no level above 6" "marked(2)\|marked(4)\|level(8)" "$cva_linear"
check "CVA linear: ... and the run suspends, none of alice's own transactions holding" "→ suspended" "$cva_linear"

# A safe formula of a schema's body, local to a role, "the machine of the person in that role evaluates it in its own state" (vGLP bcb2726, Section 5.4, "Speech-act schemas as local specifications"): vGLP's forms (programs/vglp/cva/contracts.glp), tick(p) with not exists b (blocked(b) at p and b > 2), its platform emitted by the canonical compilation into a scratch directory and its procedures called on states; and eq comparing two persons, as GFF-I's not(and(friend(Alice, Carol), eq(Carol, Dave))) does ("eq and ne also compare two agent variables", GSC c8c5ead, def:numbers).  The play harness, programs/vglp/cva/self.glp, is on test/OPEN_TYPES, and a play over it would be new GLP with an open type, so the procedures are called directly (qschema_forms).
check "CVA forms: the safe formula printed with the role it is local to, role 1 (qschema_forms)" "^schema_forms(tick, \[sform(at(1), fnot(fexists(\[num(b)\], role(1), atom(blocked, \[num(b)\]), fcons(gt(num(b), num(2))))))\])$" "$(cat "$CVA/forms/platform.vglp")"
cva_forms_dir="$GLP_DIR/programs/tests/vg_forms_$$"
mkdir -p "$cva_forms_dir" && cp "$CVA/forms/platform.vglp" "$cva_forms_dir/"
cva_forms=$("$REPL_RUN" <<HEREDOC
:emit $cva_forms_dir
load $cva_forms_dir/platform.glp
:limit 10000000
forms_at(tick, [b(role(1), p(alice))], alice, F1), forms_hold(F1?, [b(role(1), p(alice))], [g(armed, []), g(blocked, [n(5)])], R1).
forms_at(tick, [b(role(1), p(alice))], alice, F2), forms_hold(F2?, [b(role(1), p(alice))], [g(armed, []), g(blocked, [n(1)])], R2).
contract_schemas(S3), opened(S3?, alice, [g(started, []), g(armed, []), g(blocked, [n(5)])], C3).
contract_schemas(S4), opened(S4?, alice, [g(started, []), g(armed, []), g(blocked, [n(1)])], C4).
schema_entry(tick, schema(_, _, R5)), dom_bindings(tick, R5?, [b(role(1), p(alice))], alice, [g(armed, []), g(blocked, [n(5)]), g(blocked, [n(1)])], B5).
form_holds(fnot(fand(fa(role(1), atom(friend, [role(2)])), fcons(eq(role(2), pvar('Dave'))))), [b(role(1), p(alice)), b(role(2), p(bob)), b(pvar('Dave'), p(bob))], [g(friend, [p(bob)])], R6).
form_holds(fnot(fand(fa(role(1), atom(friend, [role(2)])), fcons(eq(role(2), pvar('Dave'))))), [b(role(1), p(alice)), b(role(2), p(bob)), b(pvar('Dave'), p(carol))], [g(friend, [p(bob)])], R7).
:quit
HEREDOC
2>&1)
rm -rf "$cva_forms_dir"
check "CVA forms: the platform emitted and loaded, its types checked (qschema_forms)" "Loaded: .*vg_forms_.*platform.glp" "$cva_forms"
check "CVA forms: ... blocked(5) at alice, 5 > 2: the formula does not hold (qschema_forms)" "^R1 = no$" "$cva_forms"
check "CVA forms: ... blocked(1) alone: it holds (qschema_forms)" "^R2 = yes$" "$cva_forms"
check "CVA forms: ... alice's machine opens no tick where it fails (qschema_forms)" "^C3 = \[\]$" "$cva_forms"
check "CVA forms: ... and opens tick(alice) where it holds (qschema_forms)" "^C4 = \[tick(alice)\]$" "$cva_forms"
check "CVA forms: ... the precondition at alice, armed and the formula, held by no binding where some blocked(b) has b > 2 (qschema_forms)" "^B5 = \[\]$" "$cva_forms"
check "CVA forms: eq comparing two persons: not(friend(Bob) and Bob = Dave) fails where Dave is Bob (qschema_forms)" "R6 = no$" "$cva_forms"
check "CVA forms: ... and holds where Dave is Carol (qschema_forms)" "R7 = yes$" "$cva_forms"

# GSC's social network, GSN (programs/gsc/GSN.glp, the checker's gsn), compiled and run with scripted persons (vGLP #16 Cowork to Code, 2026-10-19 15:57 UTC, item 5; vGLP a890465, Section 7): four agents; create, join with its approval question, a message, a member's post and the leader's deliveries of it, leave, a second group, bob's, with alice a member, unfriend ending every membership between bob and alice in one act by its cleared atoms (vGLP f8127b1, Section 5.4, "Act schemas as local specifications": "A schema may mark an atom it deletes as cleared ... the act deletes every atom of the state that it matches, and requires none, as GSN's unfriend ends every membership between the two"; vGLP #22's task, Code_inbox 2026-10-31 07:33 UTC), and close by the leader taking the listed role a[listed(*, n)] of every member the leader lists (social_network/self.glp).
cva_net_load=$("$REPL_RUN" <<HEREDOC
$CVA/social_network/
:quit
HEREDOC
2>&1)
check "CVA social network: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/social_network" "$cva_net_load"
check_not "CVA social network: no type errors" "Type checking failed" "$cva_net_load"
cva_network=$("$REPL_RUN" <<HEREDOC
$CVA/social_network/
:limit 6000000
play_network(A, B, C, D, N).
:quit
HEREDOC
2>&1)
CVA_NET_N="speech(utterance(club), alice)"
CVA_NET_X="speech(utterance(hi), bob)"
check "CVA social network: alice creates the group, its first member, listing herself (GSN:2)" "A = \[.*view(alice, \[member(dot(community(alice), $CVA_NET_N)), listed(alice, $CVA_NET_N), friend(dave), friend(carol), friend(bob), knows(dave), knows(carol), knows(bob), identity\], \[bob, carol, dave\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\])" "$cva_network"
check "CVA social network: alice adds bob, who is asked on his approval question and answers yes (GSN:3)" "B = \[.*gave(join(alice, bob, $CVA_NET_N), yes), view(bob, \[friend(alice), knows(alice), identity\], \[alice\], \[join(alice, bob, $CVA_NET_N)\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\]), view(bob, \[member(dot(community(alice), $CVA_NET_N)), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\])" "$cva_network"
check "CVA social network: ... and carol and dave likewise" "sent(alice, carol, start(attempt(alice, 20, 20, join(alice, carol, $CVA_NET_N), none))), sent(carol, alice, vote(attempt(alice, 20, 20, join(alice, carol, $CVA_NET_N), none), yes)), sent(alice, carol, outcome(attempt(alice, 20, 20, join(alice, carol, $CVA_NET_N), none), commit)), sent(alice, dave, start(attempt(alice, 22, 22, join(alice, dave, $CVA_NET_N), none))), sent(dave, alice, vote(attempt(alice, 22, 22, join(alice, dave, $CVA_NET_N), none), yes)), sent(alice, dave, outcome(attempt(alice, 22, 22, join(alice, dave, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: alice's message to bob, bob's machine voting yes unasked (GSN:1)" "sent(alice, bob, start(attempt(alice, 24, 24, message(alice, bob, speech(utterance(post), alice)), none))), sent(bob, alice, vote(attempt(alice, 24, 24, message(alice, bob, speech(utterance(post), alice)), none), yes)), sent(alice, bob, outcome(attempt(alice, 24, 24, message(alice, bob, speech(utterance(post), alice)), none), commit))" "$cva_network"
check "CVA social network: bob's post to the group, alice's machine voting yes unasked (GSN:5)" "sent(bob, alice, start(attempt(bob, 26, 26, post(bob, alice, $CVA_NET_X, $CVA_NET_N), none))), sent(alice, bob, vote(attempt(bob, 26, 26, post(bob, alice, $CVA_NET_X, $CVA_NET_N), none), yes)), sent(bob, alice, outcome(attempt(bob, 26, 26, post(bob, alice, $CVA_NET_X, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: alice's machine delivers it to dave, an unguarded class it opens" "sent(alice, dave, start(attempt(alice, 27, 27, deliver(alice, dave, $CVA_NET_X, $CVA_NET_N), none))), sent(dave, alice, vote(attempt(alice, 27, 27, deliver(alice, dave, $CVA_NET_X, $CVA_NET_N), none), yes)), sent(alice, dave, outcome(attempt(alice, 27, 27, deliver(alice, dave, $CVA_NET_X, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: ... to carol" "sent(alice, carol, outcome(attempt(alice, 28, 28, deliver(alice, carol, $CVA_NET_X, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: ... and to bob" "sent(alice, bob, outcome(attempt(alice, 29, 29, deliver(alice, bob, $CVA_NET_X, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: each member holds it, received in the group from alice: bob" "B = \[.*view(bob, \[received_in($CVA_NET_X, alice, $CVA_NET_N), received(speech(utterance(post), alice), alice), member(dot(community(alice), $CVA_NET_N)), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\])" "$cva_network"
check "CVA social network: ... carol" "C = \[.*view(carol, \[received_in($CVA_NET_X, alice, $CVA_NET_N), member(dot(community(alice), $CVA_NET_N)), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\])" "$cva_network"
check "CVA social network: ... and dave" "D = \[.*view(dave, \[received_in($CVA_NET_X, alice, $CVA_NET_N), member(dot(community(alice), $CVA_NET_N)), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\])" "$cva_network"
# The network log interleaves the messages of different agents in the order the scheduler runs them, which the paper leaves open (the runs of maGLP and vmaGLP interleave the agents' reductions, vGLP, Sections 4.2 and 5.2): the check holds the order of each agent's messages and not their interleaving with another agent's, which changes with what each reduction costs (Code #12, 2026-10-20, change 2 of the five).
check "CVA social network: carol leaves, alice's machine voting yes (GSN:4)" "sent(carol, alice, start(attempt(carol, 30, 30, leave(alice, carol, $CVA_NET_N), none))), .*sent(alice, carol, vote(attempt(carol, 30, 30, leave(alice, carol, $CVA_NET_N), none), yes)), .*sent(carol, alice, outcome(attempt(carol, 30, 30, leave(alice, carol, $CVA_NET_N), none), commit))" "$cva_network"
check "CVA social network: ... and is a member no more" "C = \[.*view(carol, \[received_in($CVA_NET_X, alice, $CVA_NET_N), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\]) |" "$cva_network"
CVA_NET_M="speech(utterance(den), bob)"
CVA_NET_T="\[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\]"
check "CVA social network: bob creates a group of his own, m, and adds alice, who is asked and answers yes (GSN:2, GSN:3)" "A = \[.*gave(join(bob, alice, $CVA_NET_M), yes), .*view(alice, \[member(dot(community(bob), $CVA_NET_M)), " "$cva_network"
check "CVA social network: bob ends his friendship with alice, alice's machine voting yes, his request his will (GSN:4, unfriend)" "sent(bob, alice, start(attempt(bob, 35, 35, unfriend(bob, alice), none))), .*sent(alice, bob, vote(attempt(bob, 35, 35, unfriend(bob, alice), none), yes)), .*sent(bob, alice, outcome(attempt(bob, 35, 35, unfriend(bob, alice), none), commit))" "$cva_network"
# The covering test of cleared atoms (vGLP #22's task): unfriend clears listed(q, *) and member(<q>.*) at each role, q the other, and the one act ends every membership between the two, at both.
check "CVA social network: ... and the one act ends every membership between the two at bob: his membership of alice's group n and alice's listing in his group m, both cleared, with his friendship; his own membership and listing in m kept" "B = \[.*view(bob, \[listed(alice, $CVA_NET_M), member(dot(community(bob), $CVA_NET_M)), listed(bob, $CVA_NET_M), received_in($CVA_NET_X, alice, $CVA_NET_N), received(speech(utterance(post), alice), alice), member(dot(community(alice), $CVA_NET_N)), friend(alice), knows(alice), identity\], \[alice\], \[unfriend(bob, alice)\], $CVA_NET_T), view(bob, \[member(dot(community(bob), $CVA_NET_M)), listed(bob, $CVA_NET_M), received_in($CVA_NET_X, alice, $CVA_NET_N), received(speech(utterance(post), alice), alice), knows(alice), identity\], \[alice\], \[\], $CVA_NET_T) |" "$cva_network"
check "CVA social network: ... and at alice, in the same act: bob's listing in her group n and her membership of his group m, both cleared, with her friendship of bob; her own membership and listing in n and dave's listing kept" "A = \[.*view(alice, \[member(dot(community(bob), $CVA_NET_M)), delivered($CVA_NET_X, bob, $CVA_NET_N), delivered($CVA_NET_X, carol, $CVA_NET_N), delivered($CVA_NET_X, dave, $CVA_NET_N), posted($CVA_NET_X, $CVA_NET_N), listed(dave, $CVA_NET_N), listed(bob, $CVA_NET_N), member(dot(community(alice), $CVA_NET_N)), listed(alice, $CVA_NET_N), friend(dave), friend(carol), friend(bob), knows(dave), knows(carol), knows(bob), identity\], \[bob, carol, dave\], \[\], $CVA_NET_T), view(alice, \[delivered($CVA_NET_X, bob, $CVA_NET_N), delivered($CVA_NET_X, carol, $CVA_NET_N), delivered($CVA_NET_X, dave, $CVA_NET_N), posted($CVA_NET_X, $CVA_NET_N), listed(dave, $CVA_NET_N), member(dot(community(alice), $CVA_NET_N)), listed(alice, $CVA_NET_N), friend(dave), friend(carol), knows(dave), knows(carol), knows(bob), identity\], \[bob, carol, dave\], \[\], $CVA_NET_T)" "$cva_network"
# A cleared atom is required of no one (ibid.: "and requires none"): alice, at unfriend's unguarded role, is asked nothing, and her machine votes yes though she leads n, of which bob is a member, and is herself a member of m.
check_not "CVA social network: ... alice not asked on unfriend" "gave(attempt([a-z]*, [0-9]*, [0-9]*, unfriend\|gave(unfriend" "$cva_network"
check "CVA social network: alice closes the group, her agent completing the list from the people she lists, close(alice, [alice, dave], n), and dave's machine votes yes unasked (GSN:4)" "sent(alice, dave, start(attempt(alice, 37, 37, close(alice, \[alice, dave\], $CVA_NET_N), none))), sent(dave, alice, vote(attempt(alice, 37, 37, close(alice, \[alice, dave\], $CVA_NET_N), none), yes)), sent(alice, dave, outcome(attempt(alice, 37, 37, close(alice, \[alice, dave\], $CVA_NET_N), none), commit)) |" "$cva_network"
check "CVA social network: ... the group closed at alice, her membership ended" "A = \[.*view(alice, \[closed($CVA_NET_N), delivered($CVA_NET_X, bob, $CVA_NET_N), delivered($CVA_NET_X, carol, $CVA_NET_N), delivered($CVA_NET_X, dave, $CVA_NET_N), posted($CVA_NET_X, $CVA_NET_N), listed(dave, $CVA_NET_N), friend(dave), friend(carol), knows(dave), knows(carol), knows(bob), identity\], \[bob, carol, dave\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\]) |" "$cva_network"
check "CVA social network: ... and dave's membership ended in the same act" "D = \[.*view(dave, \[received_in($CVA_NET_X, alice, $CVA_NET_N), friend(alice), knows(alice), identity\], \[alice\], \[\], \[tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)\]) |" "$cva_network"
check_not "CVA social network: no person asked for post, deliver, leave or close but its guard" "gave(attempt([a-z]*, [0-9]*, [0-9]*, \(deliver\|close\|post\|leave\)\|gave(\(deliver\|close\|post\|leave\)" "$cva_network"

# Own triggers through the fair merge (vGLP 6c8c663, Section 5.4, "The compilation": "The agent goal takes its own transactions one at a time, one on each own trigger it reads.  It writes an own trigger when it starts, after each other trigger it reads unless an own trigger it has written is still unread, and after each own transaction it takes, so that it reads its other triggers between its own transactions and writes none once none holds"; the agent goal reads "the fair merge (Example ex:merge) of the answers to its request questions, the messages arriving on the network, the answers to its approval questions and its own triggers"; vGLP #15 Cowork to Code, 2026-10-19 14:20 UTC, Blocking: yes).  The agent writes own on a stream of its own, merged with the rest of its input, and not on the list of the triggers it puts up itself, which holds the recheck alone.
for c in social_graph counter; do
    cva_agent=$(cat "$CVA/$c/platform.vglp")
    check "CVA own triggers, $c: the agent's input is the fair merge of its request answers, the network, its approval answers and its own triggers, one written when it starts" "^    merge(M2?, \[own|Ow?\], In),$" "$cva_agent"
    check "CVA own triggers, $c: ... the agent goal holding the writer of its stream of own triggers and whether one it wrote is unread" "^    agent_goal(\[\], In?, yes, Ans, Thr, Out, Ow, \[\], \[\],$" "$cva_agent"
    check "CVA own triggers, $c: an own trigger written after an own transaction taken, or another trigger read with none unread" "^own_after(write_own, \[own|Ow1?\], Ow1, yes)\.$" "$cva_agent"
    check "CVA own triggers, $c: ... none after another trigger while one is unread" "^then_read(yes, own_unread)\.$" "$cva_agent"
    check "CVA own triggers, $c: ... and none after an own trigger on which none holds" "^    next(no_own, no, Ap0?, Ls?, In?, Ans, Thr, Out, Ow, Approvals?, H?, St?)\.$" "$cva_agent"
    check_not "CVA own triggers, $c: no own put on the agent goal's own list, read before its input" "agent_goal(\[own\]\|own_after(after, \[own\])" "$cva_agent"
done
# vGLP's contract counter (programs/vglp/cva/contracts.glp): befriend, begin(a?) and count(a), unguarded, adding a tick at a once a holds counting, so that alice's agent, counting, takes an own transaction on every own trigger it reads, without end.  It still reads its input between them: bob's hello carrying his request of befriend(bob, alice), a meeting schema, on which she is asked and answers yes, her request, and bob's start of it, on which her machine votes yes unasked, and the befriending commits at both while she counts on.  On the agent goal of main at ab4818e7, which took its own transactions in a loop before reading its input again, the same play leaves alice counting with no person known, bob's screen at its first view, and nothing on the network.
cva_counter_load=$("$REPL_RUN" <<HEREDOC
$CVA/counter/
:quit
HEREDOC
2>&1)
check "CVA counter: the platform and its play load, compiled by the canonical compilation" "Loaded program: .*cva/counter" "$cva_counter_load"
check_not "CVA counter: no type errors" "Type checking failed" "$cva_counter_load"
cva_counter=$("$REPL_RUN" <<HEREDOC
$CVA/counter/
:limit 600000
play_counter(A, B, N).
:quit
HEREDOC
2>&1)
check "CVA counter: alice begins counting, and her agent counts, an own transaction on each own trigger, before any person is known" "A = \[view(alice, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[\], \[\], \[begin(alice)\], \[tab('Friends', friend)\]), view(alice, \[counting\], \[\], \[\], \[tab('Friends', friend)\]), view(alice, \[tick, counting\], \[\], \[\], \[tab('Friends', friend)\]), .*view(alice, \[tick, tick, counting\], \[\], \[\], \[tab('Friends', friend)\])" "$cva_counter"
check "CVA counter: counting, her agent reads bob's hello and her yes, her request of befriend, sending her hello back: bob known" "view(alice, \[\(tick, \)*counting\], \[bob\], \[\], \[tab('Friends', friend)\])" "$cva_counter"
check "CVA counter: ... she is asked on bob's hello of befriend, a meeting schema, and answers yes; friend(bob) is added once his attempt commits, and her agent counts on after it" "gave(befriend(bob, alice), yes), .*view(alice, \[friend(bob), \(tick, \)*counting\], \[bob\], \[\], \[tab('Friends', friend)\]), view(alice, \[tick, friend(bob), \(tick, \)*counting\], \[bob\], \[\], \[tab('Friends', friend)\]), view(alice, \[tick, tick, friend(bob), \(tick, \)*counting\], \[bob\], \[\], \[tab('Friends', friend)\])" "$cva_counter"
check "CVA counter: bob's screen: his request of befriend pending, alice known once her hello comes back, and alice his friend once his attempt commits" "B = \[view(bob, \[\], \[\], \[\], \[tab('Friends', friend)\]), view(bob, \[\], \[\], \[befriend(bob, alice)\], \[tab('Friends', friend)\]), view(bob, \[\], \[alice\], \[befriend(bob, alice)\], \[tab('Friends', friend)\]), view(bob, \[friend(alice)\], \[alice\], \[\], \[tab('Friends', friend)\]) |" "$cva_counter"
check "CVA counter: bob's start to alice, her agent's vote yes and the commit, her counting asking nobody" "N = \[sent(bob, alice, start(attempt(bob, 2, 2, befriend(bob, alice), none))), sent(alice, bob, vote(attempt(bob, 2, 2, befriend(bob, alice), none), yes)), sent(bob, alice, outcome(attempt(bob, 2, 2, befriend(bob, alice), none), commit)) |" "$cva_counter"
check "CVA counter: the run does not suspend: alice's agent counts without end, stopped at the REPL's :limit" "capped" "$cva_counter"
check_not "CVA counter: no person asked for count" "gave(attempt([a-z]*, [0-9]*, [0-9]*, count\|gave(count" "$cva_counter"

# No clock and no tick (vGLP efe7bbc, Section 5.4, "Triggers": the agent takes its own transactions "one at a time, each on an own trigger that the agent writes on a stream merged fairly with its other triggers, when it starts, after another trigger and after each own transaction it takes, until none holds", vGLP 6c8c663; Udi's approval of 2026-10-18, reported by Code #10): each play's run but the counter's suspends once every agent has taken its own transactions until none holds and no trigger is left to read, where until vGLP efe7bbc it ran to the REPL's :limit, each agent's clock ticking without end.
for v in cva_sg_menu cva_sg_befriend cva_sg_unfriend cva_sg_refused cva_cur_payswap cva_cur_redeem cva_cur_busy cva_cur_declined cva_nudge cva_intro cva_gnudge cva_cs cva_link cva_friends cva_waves cva_parts cva_opens cva_groups cva_merges cva_network cva_coins cva_wardclose cva_numbers cva_intros cva_reading cva_signing cva_days cva_linear; do
    check "CVA quiescence: the run of $v suspends, no agent left with an own transaction or a trigger" "→ suspended" "${!v}"
    check_not "CVA quiescence: ... the run of $v not stopped at the limit" "capped" "${!v}"
done

# The composition of contracts (vGLP 7c631b1, Section 5.4, "The contract's words": "The compiler takes one contract or several: of several it prints the platform of their composition, the union of their schemas and of their pragmas, which extends each ...; what two of them inherit from one contract is taken once, and a schema, a tab or a button of one name that two of them write differently is refused"), by programs/vglp/cva/compiler/compose.sh, on fixtures of its own: XA and XB both inherit establish, written with different variable names, and the tab Contacts and the button Establish; XA adds mint, XB burn; XC writes establish differently, XD the tab Contacts, XE the button Establish, XF another introduction predicate.
cva_cm_est="schema(establish, ['Alice'], [pvar(c)], [rs(guarded, [atom(identity, [pvar(c)])], [], [], [atom(identity, [pvar(c)])])])"
cva_cm_est2="schema(establish, ['Alice'], [pvar('Carol')], [rs(guarded, [atom(identity, [pvar('Carol')])], [], [], [atom(identity, [pvar('Carol')])])])"
cva_cm_est3="schema(establish, ['Alice'], [pvar(c)], [rs(guarded, [atom(identity, [pvar(c)])], [], [], [])])"
cva_cm_mint="schema(mint, ['Alice'], [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))], [], [], [])])"
cva_cm_burn="schema(burn, ['Alice'], [], [rs(guarded, [], [atom(coin, [role(1)])], [atom(coin, [role(1)])], [])])"
cva_cm_tab="list('\"Contacts\"', ['identity'])"
cva_cm_act="act('establish', '\"Establish\"', ['\"\"'], none, '\"Contacts\"', by([1]))"
cva_cm_pa="pragmas([$cva_cm_tab, list('\"Coins\"', ['coin'])], [$cva_cm_act, act('mint', '\"Mint\"', ['\"\"', '\"amount\"'], none, '\"Coins\"', by([1]))])"
cva_cm_pb="pragmas([$cva_cm_tab, list('\"Burnt\"', ['coin'])], [$cva_cm_act, act('burn', '\"Burn\"', [], none, '\"Burnt\"', by([1]))])"
cva_cm_ok=$(bash "$CVA/compiler/compose.sh" XA "reflexive([knows], [$cva_cm_est, $cva_cm_mint])" "$cva_cm_pa" "introduction(knows)" XB "environment([today], [$cva_cm_est2, $cva_cm_burn])" "$cva_cm_pb" no_introduction 2>&1; echo "exit $?")
cva_cm_schema=$(bash "$CVA/compiler/compose.sh" XA "[$cva_cm_est, $cva_cm_mint]" "$cva_cm_pa" no_introduction XC "[$cva_cm_est3]" no_pragmas no_introduction 2>&1; echo "exit $?")
cva_cm_tabx=$(bash "$CVA/compiler/compose.sh" XA "[$cva_cm_est, $cva_cm_mint]" "$cva_cm_pa" no_introduction XD "[$cva_cm_est]" "pragmas([list('\"Contacts\"', ['knows'])], [])" no_introduction 2>&1; echo "exit $?")
cva_cm_button=$(bash "$CVA/compiler/compose.sh" XA "[$cva_cm_est, $cva_cm_mint]" "$cva_cm_pa" no_introduction XE "[$cva_cm_est]" "pragmas([], [act('establish', '\"Establish\"', [], none, '\"Contacts\"', by([1]))])" no_introduction 2>&1; echo "exit $?")
cva_cm_intro=$(bash "$CVA/compiler/compose.sh" XA "[$cva_cm_est]" no_pragmas "introduction(knows)" XF "[$cva_cm_est]" no_pragmas "introduction(friend)" 2>&1; echo "exit $?")
cva_cm_one=$(bash "$CVA/compiler/compose.sh" XA "[$cva_cm_est]" no_pragmas no_introduction 2>&1; echo "exit $?")
check "CVA composition: the union of the schemas, establish inherited by both taken once, the first's, mint and burn added, the reflexive and the environment predicates kept (ok)" "^environment(\[today\], reflexive(\[knows\], \[$(printf '%s' "$cva_cm_est" | sed -e 's/[][]/\\&/g'), $(printf '%s' "$cva_cm_mint" | sed -e 's/[][]/\\&/g'), $(printf '%s' "$cva_cm_burn" | sed -e 's/[][]/\\&/g')\]))$" "$cva_cm_ok"
check "CVA composition: ... the union of the pragmas, the tab Contacts and the button Establish both write taken once (ok)" "^pragmas(\[list('\"Contacts\"', \['identity'\]), list('\"Coins\"', \['coin'\]), list('\"Burnt\"', \['coin'\])\], \[act('establish', '\"Establish\"', \['\"\"'\], none, '\"Contacts\"', by(\[1\])), act('mint', .*, act('burn', '\"Burn\"', \[\], none, '\"Burnt\"', by(\[1\]))\])$" "$cva_cm_ok"
check "CVA composition: ... the introduction predicate the one contract names (ok)" "^introduction(knows)$" "$cva_cm_ok"
check "CVA composition: ... and compose.sh exits 0 (ok)" "^exit 0$" "$cva_cm_ok"
check "CVA composition: a schema of one name two write differently refused (schema)" "the schema establish is written differently by XA and XC" "$cva_cm_schema"
check "CVA composition: ... and compose.sh exits 1 (schema)" "^exit 1$" "$cva_cm_schema"
check "CVA composition: a tab of one name two write differently refused (tab)" "the tab '\"Contacts\"' is written differently by XA and XD" "$cva_cm_tabx"
check "CVA composition: ... and compose.sh exits 1 (tab)" "^exit 1$" "$cva_cm_tabx"
check "CVA composition: a button of one name two write differently refused (button)" "the button '\"Establish\"' is written differently by XA and XE" "$cva_cm_button"
check "CVA composition: ... and compose.sh exits 1 (button)" "^exit 1$" "$cva_cm_button"
check "CVA composition: two introduction predicates refused (intro)" "XA and XF name different introduction predicates" "$cva_cm_intro"
check "CVA composition: ... and compose.sh exits 1 (intro)" "^exit 1$" "$cva_cm_intro"
check "CVA composition: one contract is no composition, refused as a bad argument (one)" "^exit 2$" "$cva_cm_one"
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

# find_type(P/N, Hs, Type) (GLP-Spec appendix-guards, "Dynamic activation";
# TGLP modules.tex, Definition "Type Identity of a Type, Opaque Instance"): Hs
# is a list of type identities, one per type parameter of the caller's
# parameterised declaration of P/N in declaration order, and find_type/3 errs
# where the length of Hs is not the number of its type parameters.  The
# fixture calls it on the root's merge/3, of one type parameter, and
# miniapp/4, of two.
rm10=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/find_type_list
one(T).
two(T, U).
none(T).
short(T).
long(T).
:quit
HEREDOC
2>&1)
check "RM10 find_type/3 at one type parameter, a list of one" "T = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm10"
check "RM10 find_type/3 at two type parameters, a list of two" "U = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm10"
check "RM10 find_type/3 errs on an empty list for one type parameter" "_find_type/3: the length of the list of type identities, 0, is not the number of type parameters of the declaration of :merge/3, 1" "$rm10"
check "RM10 find_type/3 errs on one identity for two type parameters" "_find_type/3: the length of the list of type identities, 1, is not the number of type parameters of the declaration of :miniapp/4, 2" "$rm10"
check "RM10 find_type/3 errs on two identities for one type parameter" "_find_type/3: the length of the list of type identities, 2, is not the number of type parameters of the declaration of :merge/3, 1" "$rm10"

# At the prompt after a load, a posted goal's P/N names a declaration of the
# root as a clause of the program does: find_type "assigns the type identity
# of a declaration in the caller's scope" (TGLP modules.tex, "Dynamic
# Activation"), and the scope of a goal posted at the root holds the root's
# declarations.  Until 2026-10-24 find_type(now/1, V) and find_type(merge/3,
# [h1], U) erred there "not declared in the caller's scope".  U is the
# identity one/1, a clause of the program, obtains for the same goal.
rm11=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/tests/find_type_list
one(T).
find_type(merge/3, [h1], U).
find_type(now/1, V).
:quit
HEREDOC
2>&1)
check "RM11 find_type/3 at the prompt finds the root's merge/3" "U = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm11"
check "RM11 find_type/2 at the prompt finds the root's now/1" "V = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm11"
rm11_t=$(echo "$rm11" | sed -n "s/.*T = '\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\}$/\1/p" | head -1)
rm11_u=$(echo "$rm11" | sed -n "s/.*U = '\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\}$/\1/p" | head -1)
check "RM11 the prompt's identity of merge/3 at [h1] is the program clause's" "^same$" "$([ -n "$rm11_t" ] && [ "$rm11_t" = "$rm11_u" ] && echo same)"
check_not "RM11 no find_type errs at the prompt" "_find_type/[23]:" "$rm11"

# With no program loaded, a goal posted at the prompt runs no module, and its
# scope is the root's: find_type reads the root's declared table there.  Until
# 2026-10-24 it erred "the calling goal's module carries no declared table".
# The identities are those RM11 obtains with a program loaded.
rm12=$("$REPL_RUN" <<HEREDOC
find_type(now/1, V).
find_type(merge/3, [h1], U).
:quit
HEREDOC
2>&1)
check "RM12 with no program loaded, find_type/2 at the prompt finds the root's now/1" "V = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm12"
check "RM12 with no program loaded, find_type/3 at the prompt finds the root's merge/3" "U = \('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)$" "$rm12"
check_not "RM12 no find_type errs at the prompt with no program loaded" "_find_type/[23]:" "$rm12"
rm11_v=$(echo "$rm11" | sed -n "s/.*V = '\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\}$/\1/p" | head -1)
rm12_v=$(echo "$rm12" | sed -n "s/.*V = '\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\}$/\1/p" | head -1)
rm12_u=$(echo "$rm12" | sed -n "s/.*U = '\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\}$/\1/p" | head -1)
check "RM12 the identities are those obtained with a program loaded" "^same$" "$([ -n "$rm12_v" ] && [ "$rm12_v" = "$rm11_v" ] && [ "$rm12_u" = "$rm11_u" ] && echo same)"

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
check "MB1 the holder's assignment is held and reported, from the forwarding friend" "\[carol\] held(carol, '_w'(alice, [0-9]*), bob)" "$mb1"
check "MB1 the anchor's incoming assignment is held and reported, from the holder" "\[alice\] held(alice, '_w'(alice, [0-9]*), carol)" "$mb1"
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

# MB5: the link probes (programs/tests/linkprobes, and linkprobes.md beside them).  Eight `:boot` programs that fence in what crosses a link and what does not: each is a cold call between isolates, after which one side writes on a stream the other reads.  They differ in how many crossings the reader's end made before it was read, and whether the elements were written in one reduction or in two.  They were the evidence for the defect repaired on 2026-09-08 in lib/runtime/heap_fcp.dart (fireBoundCallbacks), where a value written into the tail of a stream whose reader had already crossed a link was produced and never sent, and until now nothing in the suite loaded them: MB4 above covers the same repair on one program, and these cover the six shapes that isolate it.  linkprobe13 and linkprobe14 are the pair in which the channel is handed over by a third agent, and neither probe calls authorise_link/2.  In linkprobe13 the third agent made the channel and anchors every link, so no link is held and both greetings pass through her (IGLP madglp-spec.tex, Held Links; Lemma "Both Ends Exported"); in linkprobe14 she forwards an end, the link is held, and nothing passes on it, while bob's greeting, once it has reached her, is hers to send.
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

# linkprobe13 and linkprobe14: a channel handed over by a third agent, and neither probe calls authorise_link/2.  In linkprobe13 carol made the channel and anchors every link, so none is held and each greeting passes through her; in linkprobe14 she forwards an end bob made, the link is held, and nothing passes on it.  linkprobe14's check holds in every order, delivery being unordered (GLP #11 Cowork, 2026-11-03 16:06 UTC, item 3): once bob's greeting has reached carol it is hers to send (IGLP, madGLP's Send Reader and Globalize), so alice may see it, from carol, and never on bob's held link: carol forwards the end to alice, and the router delivers no message from bob to alice (:deliveries), neither a value of bob's on the held link nor an answer to a request of alice's.
lp13=$("$REPL_RUN" <<HEREDOC
:boot $LP_DIR/linkprobe13_boot.glp $LP_DIR/linkprobe13
:quit
HEREDOC
2>&1)
check "MB5 linkprobe13 both ends of the channel cross: the boot settles" "Boot settled: 3 agents" "$lp13"
check "MB5 linkprobe13 no link is held: alice reads bob's greeting, through carol" "\[alice\] saw(hello_from(bob))" "$lp13"
check "MB5 linkprobe13 no link is held: bob reads alice's greeting, through carol" "\[bob\] saw(hello_from(alice))" "$lp13"

lp14=$("$REPL_RUN" <<HEREDOC
:deliveries
:boot $LP_DIR/linkprobe14_boot.glp $LP_DIR/linkprobe14
:quit
HEREDOC
2>&1)
check "MB5 linkprobe14 one end forwarded by a third agent: the boot settles" "Boot settled: 3 agents" "$lp14"
check "MB5 linkprobe14 carol forwards the end to alice" "^Delivered: carol > alice [1-9]" "$lp14"
check_not "MB5 linkprobe14 nothing of bob's reaches alice on the held link neither side authorises" "^Delivered: bob > alice" "$lp14"
check_not "MB5 linkprobe14 what alice sees is bob's greeting, from carol" "\[alice\] saw(hello_from(\(alice\|carol\))" "$lp14"

rm -f "$LP_DIR"/runprobe/*.glpw

# MB6: GKK's app as maGLP (GLP #11 Cowork, 2026-11-01 22:25 UTC; GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP, Implementation Notes, "Boot").  alice and bob, each in an isolate of its own, each started from agent(ch(_?,_),ch(_?,_)), each with the person scripted in GLP the boot clause names at it; alice's person names bob, her agent cold-calls bob with a conversation, msg(bob, conv(Ka, Kb, Ch)), and each agent runs programs/apps/gkk's mini-app over it.  Each agent's key pair is made at its spawn (GLP-Spec b4ecfaa, appendix-guards.tex, self_key/1; IGLP 8bec699, Boot) and self_key/1 assigns it; each person establishes their identity by it (GKK:1), alice asks to know bob by the key her agent was told and bob answers yes (GKK:2), and each then knows the other: alice's knows/1 names bob's key, and bob's names alice's.
mb6=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/multiagent/gkk_maglp_boot.glp $GLP_DIR/programs/apps/gkk
:quit
HEREDOC
2>&1)
# A key prints bare where it reads as an atom, and quoted where it begins with a digit; the first line an agent prints may follow the prompt.
mb6_ka=$(echo "$mb6" | sed -n "s/^.*\[alice\] key('\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\})$/\1/p" | head -1)
mb6_kb=$(echo "$mb6" | sed -n "s/^.*\[bob\] key('\{0,1\}\([0-9a-f]\{64\}\)'\{0,1\})$/\1/p" | head -1)
check "MB6 GKK as maGLP: the boot settles with two agents" "Boot settled: 2 agents" "$mb6"
check "MB6 alice establishes her identity by the key made at her spawn" "\[alice\] key('\{0,1\}[0-9a-f]\{64\}'\{0,1\})$" "$mb6"
check "MB6 bob's key likewise" "\[bob\] key('\{0,1\}[0-9a-f]\{64\}'\{0,1\})$" "$mb6"
check "MB6 alice knows bob, by his key" "\[alice\] knows('\{0,1\}${mb6_kb:-none}'\{0,1\})$" "$mb6"
check "MB6 bob knows alice, by her key" "\[bob\] knows('\{0,1\}${mb6_ka:-none}'\{0,1\})$" "$mb6"

# MB7: peer_app/2 under :boot, the app the harness names (IGLP 4a9ad28, Implementation Notes, "Simulation realisation": "The router reports each connection and disconnection to the agents, and reports every peer attested with the app the harness names, its developer, name and version"; Definition "Seam Predicates").  The REPL's :app names it; alice asks peer_app for bob, connected from the boot, and tells her person.  With no app named a connection is reported unattested.
mb7=$("$REPL_RUN" <<HEREDOC
:app TEAM app.grassroots 7
:boot $GLP_DIR/programs/tests/multiagent/peer_app_boot.glp
:app none
:boot $GLP_DIR/programs/tests/multiagent/peer_app_boot.glp
:quit
HEREDOC
2>&1)
check "MB7 :app names the app a :boot reports every peer attested with" "App named: app(TEAM, app.grassroots, 7)" "$mb7"
check "MB7 peer_app assigns the app the harness names for a connected peer" "\[alice\] app('TEAM', 'app.grassroots', 7)$" "$mb7"
check "MB7 with no app named, peer_app assigns unattested for a connected peer" "\[alice\] unattested$" "$mb7"
check_not "MB7 no failed goal" "→ failed" "$mb7"

# MB8: declare(nearby(pan, S)) under :boot (GLP-Spec 210cee1, appendix-guards, "Networking seam": nearby(U, S) assigns S a stream of the agents nearby over the proximity underlay U, seen(K) and gone(K), and remove(nearby(U)) ends the declaration and closes its stream; IGLP 45fabfc, Implementation Notes, "Simulation realisation": "A nearby declaration on the PAN is given seen for an adjacent agent the declaring agent has declared a peer, and for any other adjacent agent once the PAN trust levels of both are open"; "The harness sets no trust level at spawn").  The adjacency is total from the boot and alice declares bob a peer, so her declaration at her start is given seen(bob); she tells her person, removes the declaration, and tells her person the stream is closed.
mb8=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/tests/multiagent/nearby_boot.glp
:quit
HEREDOC
2>&1)
check "MB8 nearby: the boot settles with two agents" "Boot settled: 2 agents" "$mb8"
check "MB8 nearby: a declaration on the PAN is given seen for the adjacent agent declared a peer" "\[alice\] seen(bob)$" "$mb8"
check "MB8 nearby: remove(nearby(pan)) closes the stream" "\[alice\] closed$" "$mb8"
check_not "MB8 nearby: nothing is gone while the adjacency stands" "\[alice\] gone(" "$mb8"
check_not "MB8 no failed goal" "→ failed" "$mb8"

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
sl_size=$(stat -f%z "$SL_DIR/forged.glpw" 2>/dev/null || stat -c%s "$SL_DIR/forged.glpw")
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
# answering the load.  The agent admits by key: each request carries the
# other party's signature over the pair attest(PkA, PkB), checked by its
# signer and its term, and no source identity is read (GSA, "Distribution and
# Activation", "Reconnection"; GSA #2's task of 2026-11-07).  Under the plain
# REPL the tests' own signatures are the only ones that verify, so a
# reconnect or available carrying a made-up one is rejected; what
# peer_address/2 reports, address(S) or none, is given to the agent's
# dispatch on it directly.  Tests 22 and 25 have their test program sign the
# pair under its own key, so an available is admitted and reaches
# peer_address/2, which with no networking layer bound assigns none (IGLP,
# "One interface"); tests 23 and 24 give the dispatch on what signature/2
# reports its forms directly, the two of a pair made under different
# programs admitted and every other refused.  punch_udp/1 is the root's,
# which there returns having done nothing; rv_client.glp and fmrv.glp carried
# stubs of it until 2026-10-10.
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
test14_own_signature_rejected(R14).
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
test23_admitted_under_any_program(RA23, RB23).
test24_refused(R241, R242, R243, R244, R245, R246, R247, R248).
test25_server_admits_by_key(RA25, RR25).
test26_register(RI26, P26).
test27_register_refused(R271, R272, R273, R274, P27).
test28_register_once(R280, R281, R282, R283, P281, P282, P283).
test29_redeem(RI29, RR29, P29).
test30_redeem_no_address(RI30, R301, R302, P30).
test31_redeem_unregistered(R311, R312, RA31, RC31, P311, P312).
test32_redeem_used(R32, P32).
test33_redeem_twice(RI33, R331, R332, P33).
test34_redeem_refused(R341, R342, R343, R344, R345, P34).
test35_redeem_admitted_no_layer(RI35, RR35, P35).
test36_link_through_agent(S36, RI36, RR36).
test37_expired_link_through_agent(RI37, RR37).
test38_redeem_other_server(RI38, RR38, P38).
test39_server_own_key(RI3910, RR3910, RI3911, RR3911).
test40_prune_used(RI40, P401, P402).
test41_used_dropped_through_agent(RD41, RK41).
test42_redeem_not_signed_by_recipient(RI42, R421, R422).
test43_redeemer_refused(R431, R432, R433, R434, R435, RI43, P43).
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
check "RV agent test3: A after B, A punches to B's key and address" "^RA3 = punch(bob, addr_b)$" "$(_rv_answer 3)"
check "RV agent test3: A after B, B punches to A's key and address" "^RB3 = punch(alice, addr_a)$" "$(_rv_answer 3)"
check "RV agent test3: A after B, nothing left pending" "^P3 = \[\]$" "$(_rv_answer 3)"
check "RV agent test4 succeeds" "^→ succeeds$" "$(_rv_answer 4)"
check "RV agent test4: B after A, A punches to B's key and address" "^RA4 = punch(bob, addr_b)$" "$(_rv_answer 4)"
check "RV agent test4: B after A, B punches to A's key and address" "^RB4 = punch(alice, addr_a)$" "$(_rv_answer 4)"
check "RV agent test4: B after A, nothing left pending" "^P4 = \[\]$" "$(_rv_answer 4)"
check "RV agent test5 succeeds" "^→ succeeds$" "$(_rv_answer 5)"
check "RV agent test5: another pair's entry passes, A's needs is stored after it" "^P5 = \[ready(carol, dave, addr_c, X[0-9]*), needs(alice, bob, addr_a, X[0-9]*)\]$" "$(_rv_answer 5)"
check "RV agent test6 succeeds" "^→ succeeds$" "$(_rv_answer 6)"
check "RV agent test6: past another pair's entry, A punches to B's key and address" "^RA6 = punch(bob, addr_b)$" "$(_rv_answer 6)"
check "RV agent test6: past another pair's entry, B punches to A's key and address" "^RB6 = punch(alice, addr_a)$" "$(_rv_answer 6)"
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
check "RV agent test13: the requester's own signature is refused" "^R134 = rejected$" "$(_rv_answer 13)"
check "RV agent test14 succeeds" "^→ succeeds$" "$(_rv_answer 14)"
check "RV agent test14: a verified signature of the requester's own is rejected" "^R14 = rejected$" "$(_rv_answer 14)"
check "RV agent test15 succeeds" "^→ succeeds$" "$(_rv_answer 15)"
check "RV agent test15: the layer observes no address for A, the reconnect is rejected" "^R15 = rejected$" "$(_rv_answer 15)"
check "RV agent test15: the layer observes no address for A, B's entry is left pending" "^P15 = \[ready(bob, alice, addr_b, X[0-9]*)\]$" "$(_rv_answer 15)"
check "RV agent test16 succeeds" "^→ succeeds$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, A punches to B's key and address" "^RA16 = punch(bob, addr_b)$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, B punches to it" "^RB16 = punch(alice, addr_a)$" "$(_rv_answer 16)"
check "RV agent test16: the layer observes A's address, nothing left pending" "^P16 = \[\]$" "$(_rv_answer 16)"
check "RV agent test17 succeeds" "^→ succeeds$" "$(_rv_answer 17)"
check "RV agent test17: an unsigned available is refused" "^R171 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available signed by B itself is refused" "^R172 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available over the pair out of order is refused" "^R173 = rejected$" "$(_rv_answer 17)"
check "RV agent test17: an available signed by a third party is refused" "^R174 = rejected$" "$(_rv_answer 17)"
check "RV agent test18 succeeds" "^→ succeeds$" "$(_rv_answer 18)"
check "RV agent test18: an unverified available is rejected" "^RB18 = rejected$" "$(_rv_answer 18)"
check "RV agent test18: an unverified available leaves the waiting A untold" "^RA18 = <unbound>$" "$(_rv_answer 18)"
check "RV agent test19 succeeds" "^→ succeeds$" "$(_rv_answer 19)"
check "RV agent test19: a reconnect over the pair out of order is refused" "^R19 = rejected$" "$(_rv_answer 19)"
check "RV agent test20 succeeds" "^→ succeeds$" "$(_rv_answer 20)"
check "RV agent test20: the layer observes no address for B, the available is rejected" "^RB20 = rejected$" "$(_rv_answer 20)"
check "RV agent test20: the layer observes no address for B, A's entry is left pending" "^P20 = \[needs(alice, bob, addr_a, X[0-9]*)\]$" "$(_rv_answer 20)"
check "RV agent test21 succeeds" "^→ succeeds$" "$(_rv_answer 21)"
check "RV agent test21: the layer observes B's address, A punches to it" "^RA21 = punch(bob, addr_b)$" "$(_rv_answer 21)"
check "RV agent test21: the layer observes B's address, B punches to A's key and address" "^RB21 = punch(alice, addr_a)$" "$(_rv_answer 21)"
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
check "RV client test7: handle_reply dispatches punch(Key, Addr) to declare and punch_udp" "^→ succeeds$" "$(_rv_answer 28)"

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

# rv_agent_test.glp test 22, posted under :trace (answer 36 is the :trace
# command's): the admitting path.  The test program signs the pair of
# self_key's K and bob under K, so bob's available for K verifies and is
# admitted, under the test program, which is no social graph's; the trace
# shows the agent's peer_address/2 call, which only an admitted request
# makes, and with no layer bound it assigns none (IGLP, "One interface"), so
# bob is rejected and K, waiting, is told nothing.
rv_key="\('[0-9][0-9a-f]\{63\}'\|[a-f][0-9a-f]\{63\}\)"
check "RV agent test22 succeeds" "^→ succeeds$" "$(_rv_answer 37)"
check "RV agent test22: the available's signature verifies, A's own over the pair in the fixed order" "^S22 = signed($rv_key, $rv_key, attest(\(\1, bob\|bob, \1\)))$" "$(_rv_answer 37)"
check "RV agent test22: the admitted available reaches the layer, which observes no address for B" "^:peer_address(bob, none) :- true$" "$(_rv_answer 37)"
check "RV agent test22: no address observed for B, the admitted available is rejected" "^RB22 = rejected$" "$(_rv_answer 37)"
check "RV agent test22: no address observed for B, the waiting A is told nothing" "^RA22 = <unbound>$" "$(_rv_answer 37)"

# rv_agent_test.glp test 23, under :trace: alice's reconnect for bob carrying
# bob's signature over the pair made under one program, and bob's available
# for alice carrying alice's made under another; both admitted, each reading
# its requester's address, none with no layer bound, so both rejected there.
rv_23=$(_rv_answer 38)
check "RV agent test23 succeeds" "^→ succeeds$" "$rv_23"
check "RV agent test23: the reconnect, bob's signature made under one program, is admitted and reaches the layer" "^:peer_address(alice, none) :- true$" "$rv_23"
check "RV agent test23: the available, alice's signature made under another, is admitted and reaches the layer" "^:peer_address(bob, none) :- true$" "$rv_23"
check "RV agent test23: the admitted reconnect is rejected, the layer observing no address" "^RA23 = rejected$" "$rv_23"
check "RV agent test23: the admitted available is rejected, the layer observing no address" "^RB23 = rejected$" "$rv_23"

# rv_agent_test.glp test 24, under :trace: unsigned, another signer, another
# term and the pair out of order, each on a reconnect and on an available,
# refused before any address is read.
rv_24=$(_rv_answer 39)
check "RV agent test24 succeeds" "^→ succeeds$" "$rv_24"
check "RV agent test24: no refused request reaches the layer" "^0$" "$(printf '%s\n' "$rv_24" | grep -c '^:peer_address(')"
check "RV agent test24: an unsigned reconnect is refused" "^R241 = rejected$" "$rv_24"
check "RV agent test24: a reconnect signed by another key is refused" "^R242 = rejected$" "$rv_24"
check "RV agent test24: a reconnect signed over another term is refused" "^R243 = rejected$" "$rv_24"
check "RV agent test24: a reconnect over the pair out of order is refused" "^R244 = rejected$" "$rv_24"
check "RV agent test24: an unsigned available is refused" "^R245 = rejected$" "$rv_24"
check "RV agent test24: an available signed by another key is refused" "^R246 = rejected$" "$rv_24"
check "RV agent test24: an available signed over another term is refused" "^R247 = rejected$" "$rv_24"
check "RV agent test24: an available over the pair out of order is refused" "^R248 = rejected$" "$rv_24"

# rv_agent_test.glp test 25, under :trace: through rv_server/2, given no
# program identity, bob's available for K carrying K's verified signature is
# admitted and reads bob's address; K's reconnect for bob carrying the same,
# K's own, is refused without reading one.
rv_25=$(_rv_answer 40)
check "RV agent test25 succeeds" "^→ succeeds$" "$rv_25"
check "RV agent test25: only the available signed by the key it names reaches the layer" "^1$" "$(printf '%s\n' "$rv_25" | grep -c '^:peer_address(')"
check "RV agent test25: the available signed by the key it names reaches the layer" "^:peer_address(bob, none) :- true$" "$rv_25"
check "RV agent test25: the admitted available is rejected for the address none" "^RA25 = rejected$" "$rv_25"
check "RV agent test25: the reconnect carrying the requester's own signature is rejected" "^RR25 = rejected$" "$rv_25"

# rv_agent_test.glp tests 26 to 37, under :trace: invitation links (GNA
# Section 6, "Invitation links").  A link is invitation(I, Rv, RvAddr,
# Serial, Expiry) signed by the inviter I; register(I, Link, ReplyI)
# registers its serial as unused, and redeem(R, Link, ReplyR) is admitted
# where the link is signed by the inviter it names and now/1 is before its
# expiry, relayed where the layer observes both and the serial is registered
# for the inviter and unused, which marks it used until the link's expiry.
# The redemption is admitted only where the link names the agent, its Rv
# the agent's own key (rv in every test), and a used serial is dropped at
# the first request at or past its link's expiry.  Tests 26 to 35, 38 and 40
# give the dispatch its forms directly; 36, 37, 39 and 41 sign a link with
# sign/3 and give it to the agent's loop, where peer_address/2, with no
# layer bound, assigns none.
# A key as the trace prints it, unquoted whatever its first character.
rv_tkey="\('\?[0-9a-f]\{64\}'\?\)"
_rv_pa() { printf '%s\n' "$1" | grep -c '^:peer_address('; }
rv_26=$(_rv_answer 41)
check "RV agent test26 succeeds" "^→ succeeds$" "$rv_26"
check "RV agent test26: a registration signed by the inviter it names enters the serial as unused" "^P26 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_26"
check "RV agent test26: the registered inviter waits for the redemption" "^RI26 = <unbound>$" "$rv_26"
rv_27=$(_rv_answer 42)
check "RV agent test27 succeeds" "^→ succeeds$" "$rv_27"
check "RV agent test27: an unsigned registration is refused" "^R271 = rejected$" "$rv_27"
check "RV agent test27: a registration signed by another key is refused" "^R272 = rejected$" "$rv_27"
check "RV agent test27: a registration over another term is refused" "^R273 = rejected$" "$rv_27"
check "RV agent test27: a link naming another inviter than its signer is refused" "^R274 = rejected$" "$rv_27"
check "RV agent test27: nothing refused is registered" "^P27 = \[\]$" "$rv_27"
rv_28=$(_rv_answer 43)
check "RV agent test28 succeeds" "^→ succeeds$" "$rv_28"
check "RV agent test28: a serial registered and unused is not registered again" "^R281 = rejected$" "$rv_28"
check "RV agent test28: the registration standing still waits" "^R280 = <unbound>$" "$rv_28"
check "RV agent test28: the unused entry is left as it was" "^P281 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_28"
check "RV agent test28: a used serial is not registered again" "^R282 = rejected$" "$rv_28"
check "RV agent test28: the used entry is left as it was" "^P282 = \[used(alice, 7, 1000)\]$" "$rv_28"
check "RV agent test28: another serial of the inviter is registered" "^R283 = <unbound>$" "$rv_28"
check "RV agent test28: the other serial is entered after the used one" "^P283 = \[used(alice, 7, 1000), invited(alice, 8, X[0-9]*)\]$" "$rv_28"
rv_29=$(_rv_answer 44)
check "RV agent test29 succeeds" "^→ succeeds$" "$rv_29"
check "RV agent test29: a valid redemption, the inviter told the recipient's key and address" "^RI29 = punch(bob, addr_b)$" "$rv_29"
check "RV agent test29: a valid redemption, the recipient told the inviter's key and address" "^RR29 = punch(alice, addr_a)$" "$rv_29"
check "RV agent test29: the serial is marked used, the other pair's entry left pending" "^P29 = \[ready(carol, dave, addr_c, X[0-9]*), used(alice, 7, 1000)\]$" "$rv_29"
rv_30=$(_rv_answer 45)
check "RV agent test30 succeeds" "^→ succeeds$" "$rv_30"
check "RV agent test30: no address observed for the inviter, the redemption is refused" "^R301 = rejected$" "$rv_30"
check "RV agent test30: no address observed for the recipient, the redemption is refused" "^R302 = rejected$" "$rv_30"
check "RV agent test30: no address observed, the inviter is told nothing" "^RI30 = <unbound>$" "$rv_30"
check "RV agent test30: no address observed, the serial is left unused" "^P30 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_30"
rv_31=$(_rv_answer 46)
check "RV agent test31 succeeds" "^→ succeeds$" "$rv_31"
check "RV agent test31: a serial the inviter did not register is refused" "^R311 = rejected$" "$rv_31"
check "RV agent test31: a serial registered for another inviter is refused" "^R312 = rejected$" "$rv_31"
check "RV agent test31: the inviter's other serial is left waiting" "^RA31 = <unbound>$" "$rv_31"
check "RV agent test31: the other inviter's serial is left waiting" "^RC31 = <unbound>$" "$rv_31"
check "RV agent test31: the inviter's other entry is left as it was" "^P311 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_31"
check "RV agent test31: the other inviter's entry is left as it was" "^P312 = \[invited(carol, 7, X[0-9]*)\]$" "$rv_31"
rv_32=$(_rv_answer 47)
check "RV agent test32 succeeds" "^→ succeeds$" "$rv_32"
check "RV agent test32: a used serial is refused" "^R32 = rejected$" "$rv_32"
check "RV agent test32: the used entry is left as it was" "^P32 = \[used(alice, 7, 1000)\]$" "$rv_32"
rv_33=$(_rv_answer 48)
check "RV agent test33 succeeds" "^→ succeeds$" "$rv_33"
check "RV agent test33: the first redemption, the inviter told the recipient's key and address" "^RI33 = punch(bob, addr_b)$" "$rv_33"
check "RV agent test33: the first redemption, the recipient told the inviter's key and address" "^R331 = punch(alice, addr_a)$" "$rv_33"
check "RV agent test33: a second redemption of the link is refused" "^R332 = rejected$" "$rv_33"
check "RV agent test33: the serial stays used" "^P33 = \[used(alice, 7, 1000)\]$" "$rv_33"
rv_34=$(_rv_answer 49)
check "RV agent test34 succeeds" "^→ succeeds$" "$rv_34"
check "RV agent test34: no refused redemption reaches the layer" "^0$" "$(_rv_pa "$rv_34")"
check "RV agent test34: an unsigned redemption is refused" "^R341 = rejected$" "$rv_34"
check "RV agent test34: a link signed by another than the inviter it names is refused" "^R342 = rejected$" "$rv_34"
check "RV agent test34: a redemption over another term is refused" "^R343 = rejected$" "$rv_34"
check "RV agent test34: a link past its expiry is refused" "^R344 = rejected$" "$rv_34"
check "RV agent test34: a link at its expiry is refused" "^R345 = rejected$" "$rv_34"
check "RV agent test34: the serial is left unused" "^P34 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_34"
rv_35=$(_rv_answer 50)
check "RV agent test35 succeeds" "^→ succeeds$" "$rv_35"
check "RV agent test35: the admitted redemption reads the inviter's address" "^:peer_address(alice, none) :- true$" "$rv_35"
check "RV agent test35: the admitted redemption reads the recipient's address" "^:peer_address(bob, none) :- true$" "$rv_35"
check "RV agent test35: no address observed, the admitted redemption is refused" "^RR35 = rejected$" "$rv_35"
check "RV agent test35: the inviter is told nothing" "^RI35 = <unbound>$" "$rv_35"
check "RV agent test35: the serial is left unused" "^P35 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_35"
rv_36=$(_rv_answer 51)
check "RV agent test36 succeeds" "^→ succeeds$" "$rv_36"
check "RV agent test36: the link made by sign/3 verifies as the inviter's, carrying its key, the server, the serial and the expiry" "^S36 = signed($rv_key, $rv_key, invitation(\1, rv, rv_addr, 7, [0-9]*))$" "$rv_36"
check "RV agent test36: through the agent, the link's serial is registered as unused" "^examples/rendezvous/rv_agent:register_serial($rv_tkey, 7, RI36, \[\], \[invited(\1, 7, RI36)\]) :- true$" "$rv_36"
check "RV agent test36: through the agent, the redemption signed by its recipient reads both addresses, the REPL's one key being the inviter's and the recipient's" "^2$" "$(printf '%s\n' "$rv_36" | grep -c "^:peer_address($rv_tkey, none) :- true\$")"
check "RV agent test36: no address observed, the redemption is refused" "^RR36 = rejected$" "$rv_36"
check "RV agent test36: the inviter is told nothing" "^RI36 = <unbound>$" "$rv_36"
rv_37=$(_rv_answer 52)
check "RV agent test37 succeeds" "^→ succeeds$" "$rv_37"
check "RV agent test37: an expired link's redemption does not reach the layer" "^0$" "$(_rv_pa "$rv_37")"
check "RV agent test37: an expired link's redemption is refused" "^RR37 = rejected$" "$rv_37"
check "RV agent test37: the inviter is told nothing" "^RI37 = <unbound>$" "$rv_37"
rv_38=$(_rv_answer 53)
check "RV agent test38 succeeds" "^→ succeeds$" "$rv_38"
check "RV agent test38: a link naming another server does not reach the layer" "^0$" "$(_rv_pa "$rv_38")"
check "RV agent test38: a link naming another server is refused" "^RR38 = rejected$" "$rv_38"
check "RV agent test38: the inviter is told nothing" "^RI38 = <unbound>$" "$rv_38"
check "RV agent test38: the serial is left unused" "^P38 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_38"
rv_39=$(_rv_answer 54)
check "RV agent test39 succeeds" "^→ succeeds$" "$rv_39"
check "RV agent test39: through rv_server/2, only the link naming the server's own key reaches the layer" "^2$" "$(_rv_pa "$rv_39")"
check "RV agent test39: the redemption of the link naming the server reads the inviter's and the recipient's address, the REPL's one key" "^2$" "$(printf '%s\n' "$rv_39" | grep -c "^:peer_address($rv_tkey, none) :- true\$")"
check "RV agent test39: the link naming the server, no address observed, is refused" "^RR3910 = rejected$" "$rv_39"
check "RV agent test39: the link naming another server is refused" "^RR3911 = rejected$" "$rv_39"
check "RV agent test39: the inviter of the link naming the server is told nothing" "^RI3910 = <unbound>$" "$rv_39"
check "RV agent test39: the link naming another server is registered all the same" "^RI3911 = <unbound>$" "$rv_39"
rv_40=$(_rv_answer 55)
check "RV agent test40 succeeds" "^→ succeeds$" "$rv_40"
check "RV agent test40: used serials at or past their links' expiry are dropped, every other entry kept in its order" "^P401 = \[ready(carol, dave, addr_c, X[0-9]*), used(alice, 8, 1001), invited(alice, 9, X[0-9]*)\]$" "$rv_40"
check "RV agent test40: the registered inviter still waits" "^RI40 = <unbound>$" "$rv_40"
check "RV agent test40: a used serial before its link's expiry is kept" "^P402 = \[used(alice, 7, 1000)\]$" "$rv_40"
rv_41=$(_rv_answer 56)
check "RV agent test41 succeeds" "^→ succeeds$" "$rv_41"
check "RV agent test41: through the agent, a used serial past its link's expiry is dropped and the serial registered again" "^RD41 = <unbound>$" "$rv_41"
check "RV agent test41: through the agent, a used serial before its link's expiry is kept and the registration refused" "^RK41 = rejected$" "$rv_41"
# Tests 42 and 43: the redemption is the term redeem(R, Link) signed by R
# (GNA Section 6, "Invitation links", 0147658): one signed by another than
# the recipient it names, or the link presented bare, is refused before the
# layer is asked, and admit_redeemer/7 refuses every form but R's signature
# over redeem(R, Link).
rv_42=$(_rv_answer 57)
check "RV agent test42 succeeds" "^→ succeeds$" "$rv_42"
check "RV agent test42: a redemption not signed by its recipient, or the link presented bare, does not reach the layer" "^0$" "$(_rv_pa "$rv_42")"
check "RV agent test42: a redemption signed by another than its recipient is refused" "^R421 = rejected$" "$rv_42"
check "RV agent test42: the link presented bare, the form before GNA 0147658, is refused" "^R422 = rejected$" "$rv_42"
check "RV agent test42: the inviter is told nothing" "^RI42 = <unbound>$" "$rv_42"
rv_43=$(_rv_answer 58)
check "RV agent test43 succeeds" "^→ succeeds$" "$rv_43"
check "RV agent test43: no refused redemption reaches the layer" "^0$" "$(_rv_pa "$rv_43")"
check "RV agent test43: unsigned is refused" "^R431 = rejected$" "$rv_43"
check "RV agent test43: signed by another than the recipient is refused" "^R432 = rejected$" "$rv_43"
check "RV agent test43: naming another recipient is refused" "^R433 = rejected$" "$rv_43"
check "RV agent test43: another term is refused" "^R434 = rejected$" "$rv_43"
check "RV agent test43: a redemption whose link is no signed term is refused" "^R435 = rejected$" "$rv_43"
check "RV agent test43: the serial is left unused" "^P43 = \[invited(alice, 7, X[0-9]*)\]$" "$rv_43"

# rv_server/2, the entry the rendezvous server boots the agent on
# (networking/glue/bin/rendezvous_server.dart): the module loaded as the
# program, and a network input of one msg(Id, M) envelope, whose M reaches the
# agent; an unsigned reconnect is refused.  The server itself, over the layer,
# is networking/glue/test/rendezvous_server_test.dart's.
rv_server_out=$(printf 'load %s\nrv_server(rv, [msg(rv, reconnect(alice, bob, sig, RS))]).\n:quit\n' \
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
# mini-app is its own scripted person, the second template miniapp_person/5's
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
play_install(alice, "cssn.glpw", cssn, I1).
play_install(alice, "currency.glpw", currency, I2).
play_install(alice, "pingapp.glpw", ping, I3).
play_forged_cert(alice, I4).
play_verdict(alice, with_wards, V1).
play_verdict(alice, wards, V2).
play_verdict(alice, not_grassroots, V3).
play_verdict(alice, malformed, V4).
play_verdict(alice, word, V5).
play_forged_invite(alice, bob, F1).
play_forged_join(alice, bob, F2).
play_forged_root(alice, bob, F3).
play_attest(alice, bob, S, K, H, T).
play_invite(alice, bob, A, B).
play_invite_declined(alice, bob, A2, B2).
play_invite_timed(alice, bob, Ms).
play_coins(alice, bob, C1, C2).
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
# The checker's verdict a certification carries, as the checker prints it (GSA
# paper, Section 3, "A grassroots program is compiled from its contract"): a
# verdict that certifies the contract, with wards or without, installs; one
# that does not, and the word grassroots, which the checker never prints,
# install nothing (play_verdict/3; agent.glp, cert_check/4).
check "SG super-app: a certification with the verdict syntactically_grassroots_with_wards installs" "V1 = \[ping\]" "$sg_core"
check "SG super-app: a certification with the verdict grassroots_with_wards installs" "V2 = \[ping\]" "$sg_core"
check "SG super-app: a certification with the verdict not_grassroots installs nothing" "V3 = \[\]" "$sg_core"
check "SG super-app: a certification with the verdict malformed installs nothing" "V4 = \[\]" "$sg_core"
check "SG super-app: a certification with the word grassroots, no verdict of the checker, installs nothing" "V5 = \[\]" "$sg_core"
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
play_not_a_friend(alice, bob, A, B).
:quit
HEREDOC
2>&1)
check "SG super-app: a send to someone not a friend is not sent, and the person is told not_a_friend" "A = \[not_a_friend(bob), connected(bob), received(bob, hello) | " "$sg_naf"
check "SG super-app: a send between the acceptance and the conveyance is told not_a_friend" "B = \[not_a_friend(alice), connected(alice) | " "$sg_naf"

# The super-app's own person interface on the dispatcher (GSG paper, Section
# 3; vGLP, Definitions "Volitional Procedure, ..." and "Canonical
# Compilation"): home/3 asks the person once for the stream of their
# requests, and the dispatcher draws its construct on the person channel, the
# default widget of a stream of a union of tuples, a menu of the three forms,
# the buttons Open place, Close place and Invite by link, the form opening a
# link, and List, List unlisted and Unlist, kept open, no form taking a key
# typed (GSA #2, 2026-11-08 08:07 UTC, item 2); the persons known, seen
# nearby, are told the person, nearby([bob]), on the same channel, merged
# with the draws, and home draws a card for each with the button befriend,
# which the person picks to befriend them (superapp_ui/4, alice with a
# scripted peer for each key she knows, here bob, seen nearby; GSA paper,
# Appendix "The Operating System", "Nearby").  A friend's request to connect
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
superapp_ui(alice, [bob], G?, []).
play_home(alice, yes, [msg(agent, person, befriend(bob, error))], C, L).
play_home(alice, yes, [msg(agent, person, befriend_intro(carol, bob, error))], CI, LI).
play_home(alice, no, [msg(agent, person, befriend_intro(carol, bob, error))], CN, LN).
play_home(alice, befriend, [msg(agent, person, nearby([bob])), msg(agent, person, connected(carol)), msg(agent, person, nearby([bob, carol])), msg(agent, person, nearby([bob, carol, dave]))], CB, LB).
play_home(bob, introduce, [msg(agent, person, connected(alice)), msg(agent, person, connected(carol))], CX, LX).
:quit
HEREDOC
2>&1)
check "SG super-app home: the request form, a menu of three acts, the buttons Open place and Close place (GPE:1) and Invite by link (GRV:2), the form opening a link, and List, List unlisted and Unlist (GDS:2), drawn on the person channel, no form taking a key typed" "draw(1, input_box(menu(\[form(trust, \[field(CompilerKey, peer)\]), form(install, \[field(AppName, text), field(Artefact, text)\]), form(invite, \[field(AppName, text), field(Friend, peer)\]), button(open_place), button(close_place), button(invite_link), form(open_link, \[field(Link, text)\]), form(list, \[field(Email, text), field(Name, text)\]), form(list_unlisted, \[field(Email, text), field(Name, text)\]), button(unlist)\])), stream_request_r(input(\[\])))" "$sg_home"
check_not "SG super-app home: no connect form, a person befriended by picking them in Nearby" "form(connect," "$sg_home"
check "SG super-app home: the person known, seen nearby, is shown on the person channel, nearby([bob])" "^draw(0, thread(.*), output(\[msg(agent, person, nearby(\[bob\]))\]))$" "$sg_home"
check "SG super-app home: home draws bob's card in Nearby with the button befriend" "^draw(2, form(nearby, \[field(Friend, shown), field(Befriend, button(befriend))\]), nearbyPerson_w(nearby(bob, input(\[2\]))))$" "$sg_home"
check "SG super-app home: a friend's request to connect raises respond_friend's card, the friend shown" "L = \[card(bob) | " "$sg_home"
check "SG super-app home: yes on the card is the person's decision, the pending value passed back" "C = \[msg(person, alice, decision(yes, bob, error))\]" "$sg_home"
check "SG super-app home: an introduction raises respond_intro's card, the introducer and the other party shown" "LI = \[card(carol, bob) | " "$sg_home"
check "SG super-app home: yes on the introduction's card is accept_intro, the pending value passed back" "CI = \[msg(person, alice, accept_intro(bob, error))\]" "$sg_home"
check "SG super-app home: no on the introduction's card is reject_intro, the pending value passed back" "CN = \[msg(person, alice, reject_intro(bob, error))\]" "$sg_home"
check "SG super-app home: each person nearby whom the person knows gets a card with befriend, once, and a friend none (GSA paper, Appendix 'Nearby': the person befriends one by picking them there)" "LB = \[nearby(bob), nearby(dave) | " "$sg_home"
check "SG super-app home: picking befriend on a card is the person's request to befriend that person, no key typed" "CB = \[msg(person, alice, connect(bob)), msg(person, alice, connect(dave))\]" "$sg_home"
check "SG super-app home: two friends, home draws a card of the two with the button introduce (GKK's pragma Introduction, Alice [Introduce])" "LX = \[introduction(alice, carol) | " "$sg_home"
check "SG super-app home: picking introduce is the person's act of GKK:3, the later friend introduced to the earlier" "CX = \[msg(person, bob, introduce(carol, alice))\]" "$sg_home"
check_not "SG super-app home: no failed goal" "→ failed" "$sg_home"

# The operating system's invitation links (core/links.glp; GNA Section 6,
# "Invitation links", 2ce8e10; GSA paper, Section 4.2, "Legacy online
# connections", and Appendix "The Operating System", 963fcbf), under the
# plain REPL: one key the inviter and the recipient both, no layer bound, the
# seam doing nothing.  Invite by link with a rendezvous server named: the
# person is told the link as text, invitation(L), and it is registered with
# the server, register(K, L, Reply), the reply awaiting the redemption; with
# none named, refused and told the reason, rejected(no_rendezvous_server).  A text that is not a
# link, and a link whose expiry has passed: refused and told,
# rejected(open_link).  A link made and opened: redeemed at the server the
# link names, redeem(K, Signed, Reply), the reply awaiting the server.  The
# two phones over the WAN, the server a process, are NET's
# (networking/glue/test/rendezvous_server_test.dart).
sg_links=$("$REPL_RUN" <<HEREDOC
$SG_CORE
play_links([rendezvous(rv, rv_addr)], none, [invite_link], O1, N1, C1).
play_links([], none, [invite_link], O2, N2, C2).
play_links([], none, [open_link(nonsense)], O3, N3, C3).
play_link_open(O4, N4).
play_link_expired(O5).
:quit
HEREDOC
2>&1)
check "SG links: Invite by link tells the person the link, as text" "O1 = \[msg(agent, person, invitation('\{0,1\}[0-9a-f]*'\{0,1\}))" "$sg_links"
check "SG links: Invite by link registers the link with the server the first act names" "^N1 = \[msg(rv, register('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*))\]$" "$sg_links"
check "SG links: Invite by link with no server named is refused and told the reason" "O2 = \[msg(agent, person, rejected(no_rendezvous_server))\]$" "$sg_links"
check "SG links: a text that is not a link is refused and told" "O3 = \[msg(agent, person, rejected(open_link))\]$" "$sg_links"
check "SG links: a link made and opened is redeemed at the server it names" "^N4 = \[msg(rv, redeem('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*))\]$" "$sg_links"
check "SG links: a link whose expiry has passed is refused and told" "O5 = \[msg(agent, person, rejected(open_link))\]$" "$sg_links"
check_not "SG links: no failed goal" "→ failed" "$sg_links"

# The person's listing with a directory (core/links.glp; GNA Section 7, "The
# messages", 2ce8e10), under the plain REPL: List with a directory and a
# rendezvous server sends list(A, Signed, Supply, Reply) to the directory,
# Supply and Reply for it to write; with no directory, refused and told the
# reason, rejected(no_directory), and with no rendezvous server,
# rejected(no_rendezvous_server).  The directory's ask
# more(2) is answered by two links, each registered with the server, and
# then supply(A, [L1, L2]) to the directory; more(0) by supply(A, []).
# Unlist with a directory sends unlist(A, Signed, Reply); with none, refused
# and told the reason, rejected(no_directory).  The scripted directory over the WAN is NET's
# (networking/glue/test/rendezvous_server_test.dart).
sg_dir=$("$REPL_RUN" <<HEREDOC
$SG_CORE
play_links([rendezvous(rv, rv_addr)], directory(dir, dir_addr), [list("alice@example.org", "Alice", listed)], O1, N1, C1).
play_links([rendezvous(rv, rv_addr)], none, [list("alice@example.org", "", unlisted)], O2, N2, C2).
play_links([], directory(dir, dir_addr), [list("alice@example.org", "Alice", listed)], O6, N6, C6).
play_links([rendezvous(rv, rv_addr)], directory(dir, dir_addr), [more(2)], O3, N3, C3).
play_links([rendezvous(rv, rv_addr)], directory(dir, dir_addr), [more(0)], O7, N7, C7).
play_links([], directory(dir, dir_addr), [unlist], O4, N4, C4).
play_links([], none, [unlist], O5, N5, C5).
:quit
HEREDOC
2>&1)
check "SG directory: List sends the signed listing to the directory, its asks and answer for it to write" "^N1 = \[msg(dir, list('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*, X[0-9]*)) | X[0-9]*\]$" "$sg_dir"
check "SG directory: List with no directory is refused and told the reason" "O2 = \[msg(agent, person, rejected(no_directory))\]$" "$sg_dir"
check "SG directory: List with no rendezvous server is refused and told the reason" "O6 = \[msg(agent, person, rejected(no_rendezvous_server))\]$" "$sg_dir"
check "SG directory: more(2) registers two links with the server and then supplies them to the directory" "^N3 = \[msg(rv, register('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*)), msg(rv, register('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*)), msg(dir, supply('\{0,1\}[0-9a-f]*'\{0,1\}, \['\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}\]))\]$" "$sg_dir"
check "SG directory: more(0) supplies no link" "^N7 = \[msg(dir, supply('\{0,1\}[0-9a-f]*'\{0,1\}, \[\]))\]$" "$sg_dir"
check "SG directory: Unlist sends the signed unlisting to the directory" "^N4 = \[msg(dir, unlist('\{0,1\}[0-9a-f]*'\{0,1\}, '\{0,1\}[0-9a-f]*'\{0,1\}, X[0-9]*))\]$" "$sg_dir"
check "SG directory: Unlist with no directory is refused and told the reason" "O5 = \[msg(agent, person, rejected(no_directory))\]$" "$sg_dir"
check_not "SG directory: no failed goal" "→ failed" "$sg_dir"

# The child-safe platform hosted, in its own session: a play that has run the
# currency's mini-app leaves the session such that the next play's invitation
# opens nothing --- play_invite after play_coins returns [] as well --- so the
# plays are not run one after another in one REPL (SGSG Code, 2026-09-08).
sg_cs=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/cssn/childsafe $SG_CORE
$SG_CORE
:limit 5000000
play_cssn(alice, bob, N1, N2).
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
play_both(alice, bob, A, B, C, D).
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

# A held link on three smartphones (GSA paper, Section 5.4, "Whom the
# Grassroots App knows"): the three conversations open, alice's execution sends
# carol the end of a fresh variable, carol's forwards it to bob's, bob's
# assigns it, bob's runtime holds the assignment and reports the held link,
# and bob's mini-app reports it to bob.  The link stays held, nothing sent on
# it, for bob to be asked whether to know alice, which the rebuilt core asks;
# so the value does not reach alice's execution.  The warm call that released
# it is gone (GLP #13, Code_inbox.md 2026-11-07 22:09 UTC, Udi approving
# 22:08 UTC).  programs/tests/linkprobes/linkprobes.md holds the two-agent
# programs that fence the runtime behaviour in.
sg_warm=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/social/graph/pingapp $SG_CORE
:boot $GLP_DIR/programs/social/superapp3_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG held link on three smartphones: the boot settles" "Boot settled: 3 agents" "$sg_warm"
check "SG held link: alice and carol converse" "\[carol\] greeted(alice)" "$sg_warm"
check "SG held link: carol and bob converse" "\[bob\] greeted(carol)" "$sg_warm"
check "SG held link: alice's execution sends carol the probe's end" "\[alice\] probed(carol)" "$sg_warm"
check "SG held link: carol's execution forwards it to bob" "\[carol\] forwarded(bob)" "$sg_warm"
check "SG held link: bob's execution assigns it" "\[bob\] answered" "$sg_warm"
check "SG held link: bob's runtime holds the link and reports it" "\[bob\] held_link(carol)" "$sg_warm"
check_not "SG held link: the held value does not reach alice's execution" "\[alice\] got(" "$sg_warm"

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
play_reinvite(alice, bob, A, B).
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
play_sovereign(alice, bob, A, B).
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

# Activation without an invitation (GSA paper, Section 3, "A grassroots
# program instance": "An instance begins with one person, who installs the
# certified program and activates it; others join it on invitation"), and a
# later invitation to the running execution ("Activation and joining"):
# alice installs the currency and activates it with no peer, her scripted
# person mints 3 and her screen shows it before she knows anyone; she then
# connects to bob and invites him, and the conversation opens on the
# execution she activated alone, its screen showing it after her holdings
# (play_activate/4; agent.glp, activate_app/6 and deliver/9).  Its own
# session, as the mini-app plays above are.
sg_activate=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/coins/currency $SG_CORE
$SG_CORE
:limit 5000000
play_activate(alice, bob, A, B).
:quit
HEREDOC
2>&1)
check "SG activation alone: alice's execution, activated with no invitation and no peer, mints, then the later invitation's conversation opens on it" "A = \[minted(3), holdings(\[lot(alice, 3)\]), opened(bob) | " "$sg_activate"
check "SG activation alone: bob, invited, joins alice's running execution" "B = \[opened(alice) | " "$sg_activate"
check_not "SG activation alone: no failed play" "→ failed" "$sg_activate"

# Currencies' play_mint on one smartphone, one boot per program
# (programs/currencies/<program>_mint_boot.glp): alice alone, with her own key,
# installs the mini-app from its certified artefact and activates it with no
# invitation and no peer, and her scripted person, under the role mint, mints
# 3; her lines are play_mint's screen, in order.
for sg_mint in "coins coins/currency [minted(3), holdings([lot(alice, 3)])]" \
               "dated bonds/dated [minted(3, now), holdings([lot(alice, now, 3)])]" \
               "sovereign sovereign/denominated [minted(3, now), holdings([lot(alice, usd, now, 3)])]"; do
    sg_mint_name=${sg_mint%% *}; sg_mint_rest=${sg_mint#* }; sg_mint_dir=${sg_mint_rest%% *}; sg_mint_lines=${sg_mint_rest#* }
    sg_mint_out=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/currencies/$sg_mint_dir $SG_CORE
:boot $GLP_DIR/programs/currencies/${sg_mint_name}_mint_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
    rm -f "$SG_CORE"/*.glpw
    check "SG activation alone on a smartphone, $sg_mint_name play_mint: the boot settles" "Boot settled: 1 agents (alice)" "$sg_mint_out"
    check_not "SG activation alone on a smartphone, $sg_mint_name play_mint: no event stopped at the cap" "did not quiesce" "$sg_mint_out"
    boot_same "SG activation alone on a smartphone, $sg_mint_name play_mint: alice's lines are play_mint's screen" "$sg_mint_lines" "$(boot_person alice "$sg_mint_out")"
done

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

# Physical encounters (GSA paper, Section 4, the box GPE; Appendix "The
# Operating System", "Nearby"; programs/social/graph/GPE.txt): the social
# graph with the operating system's Nearby around it (graph/core/place.glp,
# os_phone/6), which declares to the layer a watch on the agents nearby over
# the PAN and the LAN, declare(nearby(U, S)).  Two smartphones that do not
# know each other both open the place (GPE:1); each is nearby the other, so
# they come to know each other by the encounter (GPE:2), each operating
# system giving its person's consent while the place is open, no question
# asked, the one with the smaller key putting it; home shows each the other
# as nearby; alice befriends bob (GSG:1) by picking him on his card in
# Nearby, no key typed, bob consents on his card, and they are friends.  Both
# trusts are declared closed at start, and every person known is declared to
# the layer a known peer.  Then three: alice and bob know each other at the
# start and befriend, alice picking bob in Nearby and never opening the
# place; bob and carol open it and come to know each other by the encounter;
# carol befriends bob by picking him, and bob introduces her to alice
# (GKK:3): each knows the other on her yes to the introduction, and the two
# are friends.  Each person's
# lines as a set, the isolates ordering them otherwise from run to run, and
# the order where one notice causes the next.
sg_nearby=$("$REPL_RUN" <<HEREDOC
:boot $GLP_DIR/programs/social/nearby_boot.glp $SG_CORE
:boot $GLP_DIR/programs/social/nearby3_boot.glp $SG_CORE
:quit
HEREDOC
2>&1)
check "SG nearby on two smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$sg_nearby"
check "SG nearby on three smartphones: the boot settles" "Boot settled: 3 agents (alice, bob, carol)" "$sg_nearby"
check_not "SG nearby: no event stopped at the cap" "did not quiesce" "$sg_nearby"
check_not "SG nearby: no failed play" "→ failed" "$sg_nearby"
sg_nearby2=$(echo "$sg_nearby" | awk '/Boot settled: 2 agents/ { exit } { print }')
sg_nearby3=$(echo "$sg_nearby" | awk 'f { print } /Boot settled: 2 agents/ { f = 1 }')
boot_same "SG nearby on two smartphones: alice knows bob by the encounter, home shows him nearby, and she befriends him by picking him" "[msg(agent, person, connected(bob)), msg(agent, person, nearby([bob]))]" "$(boot_person_sorted alice "$sg_nearby2")"
boot_same "SG nearby on two smartphones: bob knows alice by the encounter, home shows her nearby, and he consents on his card" "[msg(agent, person, befriend(alice)), msg(agent, person, connected(alice)), msg(agent, person, nearby([alice]))]" "$(boot_person_sorted bob "$sg_nearby2")"
check "SG nearby on two smartphones: alice befriends bob once home shows him nearby" "nearby(\[bob\])), msg(agent, person, connected(bob))" "$(boot_person alice "$sg_nearby2")"
boot_same "SG nearby on three smartphones: alice, who knew bob, befriends him and is introduced to carol" "[msg(agent, person, befriend_intro(bob, carol)), msg(agent, person, connected(bob)), msg(agent, person, connected(carol)), msg(agent, person, nearby([bob, carol])), msg(agent, person, nearby([bob]))]" "$(boot_person_sorted alice "$sg_nearby3")"
boot_same "SG nearby on three smartphones: bob knows carol by the encounter and befriends both" "[msg(agent, person, befriend(alice)), msg(agent, person, befriend(carol)), msg(agent, person, connected(alice)), msg(agent, person, connected(carol)), msg(agent, person, nearby([alice, carol])), msg(agent, person, nearby([alice]))]" "$(boot_person_sorted bob "$sg_nearby3")"
boot_same "SG nearby on three smartphones: carol knows bob by the encounter, befriends him, and is introduced to alice" "[msg(agent, person, befriend_intro(bob, alice)), msg(agent, person, connected(alice)), msg(agent, person, connected(bob)), msg(agent, person, nearby([alice, bob])), msg(agent, person, nearby([bob]))]" "$(boot_person_sorted carol "$sg_nearby3")"
check "SG nearby on three smartphones: carol knows alice by the introduction, not by an encounter" "nearby(\[bob\])), msg(agent, person, connected(bob)), msg(agent, person, befriend_intro(bob, alice)), .*nearby(\[alice, bob\]))" "$(boot_person carol "$sg_nearby3")"
check "SG nearby on three smartphones: alice knows carol by the introduction" "befriend_intro(bob, carol)), .*nearby(\[bob, carol\]))" "$(boot_person alice "$sg_nearby3")"

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
# Section APPS: the apps of the contracts (programs/apps), each a mini-app of the Grassroots Super-App compiled down the cascade from its contract's act schemas: the canonical CVA platform vGLP's compiler prints from them, compiled to GLP when the directory loads, under the root's entry template miniapp/4. GSG's app, programs/apps/gsg: its platform held to what the compiler prints from programs/gsc/GSG.glp (programs/apps/compile.sh --check); the program certified by :artefact; its router, which carries the platform's network over the conversations the social graph hands the mini-app, holding a message until the conversation with its recipient opens (play_held); and two executions over one conversation, each person first establishing their identity (GKK:1, Identity, establish(p?)), alice asking to come to know bob and bob answering yes (GKK:2, acquaint(p?, q?)), alice offering bob friendship, bob asked and answering yes (GSG:1), and then bob ending it (GSG:2) (play_friends), GKK's app, programs/apps/gkk, the base mini-app, the same host on GKK's platform, three executions over two conversations, acquaintance and an introduction (play_keys), GSN's app, programs/apps/gsn, the same host on GSN's platform (play_network), and GSNP's app, programs/apps/gsnp, the same host on GSNP's platform, five executions over four conversations, the play of GAI's GSNP figure (play_provenance), and GAI's app, programs/apps/gai, the same host on GAI's platform, three executions over three conversations, the example of GAI's paper, alice, bob and alf, alice's AI agent (play_agents), and GC's app, programs/apps/gc, the same host on GC's platform, two executions over one conversation, minting, a swap and a payment (play_coins), each person's screen, the views the agent writes with the person's looks and answers, held exactly, an unbound tail written _: the run suspends, each agent having taken its own transactions until none holds and no trigger being left (vGLP, Section 5.4, "Triggers").  And the super-app hosting each app on smartphones, programs/apps/gsg_boot.glp, gsn_boot.glp and gc_boot.glp on two, gsnp_boot.glp on five and gai_boot.glp on three, under :boot: the boot harness reduces each event until the program is quiescent, as the device runtime does (IGLP, Implementation Notes, "On each event the agent binds what the event delivers, reduces until quiescent"), the platform's agent quiesces, and the boot settles, each person's lines held exactly, in order; on five and on three, each person's super-app lines and mini-app screen each held exactly, in order, the two not being ordered against each other across the isolates.
# =============================================================================
echo "=== Section APPS: the apps (programs/apps) ==="
echo ""

# The line, exactly, among the REPL's lines, its prompt stripped and an unbound tail written _.
apps_line() {
    local name="$1" line="$2" out="$3"
    # The line is compared whole by awk: macOS's grep runs out of memory on a fixed-string line of some 5000 characters ("grep: out of memory", with the line as an argument and from a file alike), and the line of a play's screen is longer.
    if echo "$out" | sed -e 's/^\(GLP> \)*//' -e 's/| X[0-9]*\]$/| _]/' | APPS_LINE="$line" awk 'BEGIN { want = ENVIRON["APPS_LINE"] } $0 == want { found = 1 } END { exit !found }'; then
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
play_held(alice, bob, carol, In, ToBob, ToCarol).
:limit 600000
play_friends(A, B).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gsg: the mini-app's artefact is certified" "gsg.glpw --- certified under" "$apps_gsg"
check "APPS gsg loads" "Loaded program: .*programs/apps/gsg$" "$apps_gsg"
apps_line "APPS gsg router: bob's hello reaches the agent, carrying the acquaintance it asks for" "In = [msg(alice, hello(bob, acquaint(alice, bob)))]" "$apps_gsg"
apps_line "APPS gsg router: the hello held for bob goes first on his conversation, then the start" "ToBob = [msg(bob, hello(alice, acquaint(alice, bob))), msg(bob, cargo(alice, start(attempt(alice, 1, 1, befriend(alice, bob), none))))]" "$apps_gsg"
apps_line "APPS gsg router: the hello held for carol goes on hers" "ToCarol = [msg(carol, hello(alice, acquaint(alice, carol)))]" "$apps_gsg"
apps_line "APPS gsg friends: alice's screen, each request pending until it commits, bob known once his hello comes back" "A = [view(alice, [], [], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, [bob]), view(alice, [knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)]) | _]" "$apps_gsg"
apps_line "APPS gsg friends: bob's screen, asked on alice's hello of acquaint and answering yes, his request, and asked on her attempt of befriend" "B = [view(bob, [], [], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), gave(befriend(alice, bob), yes), view(bob, [knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [friend(alice), knows(alice), identity], [alice], [unfriend(bob, alice)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]) | _]" "$apps_gsg"
# The view shows the agent's own key, its first argument (vGLP 12f629d, Section 5.4, "The compilation"; Section 6.2), and the scripted person, proposing a class that names their agent, writes the key it reads off the last view drawn into the holes of its step own(Ks, R) (person.glp).
check "APPS gsg: alice establishes her identity by the key her view shows" "^script(alice, _, \[Bob\], \[look, own(\[A1\], propose(establish(A1?)))" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check "APPS gsg: ... the key written into each hole" "^fill(Me, \[Me? | Ks\]) :- ground(Me?) | fill(Me?, Ks?)\.$" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check "APPS gsg: the types written again hold the notices of the platform's output, refused(N, Q, D), and the approvals pending, each by its request name, N and the approvals of type Approved ::= Proposal (vGLP e286276, Section 5.4), the lists its app shows (vGLP 1423679, Section 5.4), and the agent's own key first (vGLP 12f629d, Section 5.4)" "^Output   ::= view(Key, List(Atom), List(Key), List(Approved), List(Tab)) ; refused(Approved, Key, Reason)\.$" "$(cat "$GLP_DIR/programs/apps/gsg/self.glp")"
check "APPS gsg: the person's screen shows a notice written after the last view, refused(P, Q, D)" "^shows(refused(P, Q, D), _, T, _, B?) :- ground(P?), ground(Q?), ground(D?), ground(T?) |$" "$(cat "$GLP_DIR/programs/apps/gsg/person.glp")"
check_not "APPS gsg: no failed play" "→ failed" "$apps_gsg"
check_not "APPS gsg: no error" "Error" "$apps_gsg"
check "APPS gsg friends: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsg"
check_not "APPS gsg friends: ... and is not stopped at the limit" "capped" "$apps_gsg"

# GKK's app, programs/apps/gkk: the base mini-app of the grassroots app, which every other mini-app extends (GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC), the same host on GKK's platform, its wrapper the same file as GSG's; three executions over three conversations, alice's with bob, alice's with carol and bob's with carol (play_keys): each establishes their identity (GKK:1); alice asks to come to know bob, bob is asked and answers yes, and each then knows the other (GKK:2, acquaint(p?, q?)); alice and carol come to know each other likewise; and alice, who knows bob and carol, introduces carol to bob, bob is asked and answers yes, and he then knows carol, carol not asked (GKK:3, introduce(p?, q? ; c)).  At the commit alice's agent introduces bob's and carol's to each other, the introduction rule giving her the pair {bob, carol}, her role requiring knows(c), c an agent variable, and bob's gaining knows(c) (vGLP 613ee8d, Definition "Introduction Rule, Introduce Peers"; Section 5.4, "Act schemas as local specifications": "under GKK:3 Alice introduces Carol and Bob"); their agents exchange ihellos and know each other, and bob asks to come to know carol, she answers yes, and each knows the other (GKK:2; GKK has no befriend, vGLP #18 Cowork to Code, Code_inbox.md, 2026-10-20 21:11 UTC, item 3, and Code #12's question 6 of 22:23 UTC to vGLP).
check "APPS gkk: platform.vglp is what vGLP's compiler prints from GKK.glp" "^gkk/platform.vglp: as printed$" "$apps_print"
apps_gkk_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gkk/platform.vglp")
check "APPS gkk: the request type proposes GKK's three classes" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) $" "$apps_gkk_proposal"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gkk/miniapp.glp"; then
    _check_pass "APPS gkk: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gkk: miniapp.glp is the same file as GSG's app's" "programs/apps/gkk/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gkk=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gkk $APPS_ART
$GLP_DIR/programs/apps/gkk
:limit 20000000
play_keys(A, B, C).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gkk: the mini-app's artefact is certified" "gkk.glpw --- certified under" "$apps_gkk"
check "APPS gkk loads" "Loaded program: .*programs/apps/gkk$" "$apps_gkk"
apps_line "APPS gkk keys: alice's screen, each acquaintance a cold call, each request pending until it commits" "A = [view(alice, [], [], [], [tab('Contacts', knows)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows)]), view(alice, [identity], [], [], [tab('Contacts', knows)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob, carol], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)]), menu(alice, [bob, carol]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)]) | _]" "$apps_gkk"
apps_line "APPS gkk keys: bob's screen: once he has answered yes to the introduction, his agent, introduced to carol's by alice's at the commit, comes to know her by the ihello exchange, and he comes to know her, so no hello is sent, holding knows(carol) twice" "B = [view(bob, [], [], [], [tab('Contacts', knows)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows)]), view(bob, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), gave(introduce(alice, bob, carol), yes), view(bob, [knows(alice), identity], [alice], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)]) | _]" "$apps_gkk"
apps_line "APPS gkk keys: carol's screen: asked on alice's hello of acquaint and answering yes; not asked on the introduction, her agent comes to know bob's by it, and she answers yes on his attempt to come to know her" "C = [view(carol, [], [], [], [tab('Contacts', knows)]), view(carol, [], [], [establish(carol)], [tab('Contacts', knows)]), view(carol, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, carol), yes), view(carol, [identity], [alice], [], [tab('Contacts', knows)]), view(carol, [identity], [alice], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)]), gave(acquaint(bob, carol), yes), view(carol, [knows(alice), identity], [alice, bob], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(carol, [knows(bob), knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)]) | _]" "$apps_gkk"
check "APPS gkk: alice introduces carol to bob, in the order of introduce's head, introduce(p?, q? ; c)" "own(\[A4\], propose(introduce(A4?, Bob?, Carol?)))" "$(cat "$GLP_DIR/programs/apps/gkk/person.glp")"
check "APPS gkk: the types written again hold knows(Peer)" "^Atom     ::= identity ; knows(Peer)\.$" "$(cat "$GLP_DIR/programs/apps/gkk/self.glp")"
check_not "APPS gkk: no failed play" "→ failed" "$apps_gkk"
check_not "APPS gkk: no error" "Error" "$apps_gkk"
check "APPS gkk keys: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gkk"
check_not "APPS gkk keys: ... and is not stopped at the limit" "capped" "$apps_gkk"

# GSN's app, programs/apps/gsn: the same host on GSN's platform, its wrapper the same file as GSG's; two executions over one conversation (play_network): alice and bob establish their identities, come to know each other and become friends, alice creates the group friends, adds bob to it and sends him the message 'Hi Bob', bob sends the group the message 'Hi all', which alice delivers to him, and bob leaves the group, each person acting on what their screen shows, the views the agent writes (vGLP b7e3517, Section 5.4, "The compilation"; person.glp).
check "APPS gsn: platform.vglp is what vGLP's compiler prints from GSN.glp" "^gsn/platform.vglp: as printed$" "$apps_print"
# The request type proposes the guarded classes only (vGLP, Section 5.4, the compilation): GSN's eleven schemas that mark a role, remove for the leader, and not deliver, unguarded, which the leader's agent proposes as its opener (programs/gsc/GSN.glp at 6069b119: GSN:3, "Alice, the leader of a group, may add their friend Bob to it ...; either Alice or Bob may then end Bob's membership", Alice [Remove], drop gone).
apps_gsn_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gsn/platform.vglp")
check "APPS gsn: the request type proposes the guarded classes, remove and not deliver" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) befriend(Peer, Peer) unfriend(Peer, Peer) message(Peer, Peer, Speech) create(Peer, Speech) join(Peer, Peer, Speech) leave(Peer, Peer, Speech) remove(Peer, Peer, Speech) close(Peer, Speech) post_own(Peer, Speech, Speech) post(Peer, Peer, Speech, Speech) $" "$apps_gsn_proposal"
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
apps_line "APPS gsn network: alice's screen, each request pending, by its request name, until its transaction is taken" "A = [view(alice, [], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [friend(bob), knows(bob), identity], [bob], [create(alice, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [join(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [message(alice, bob, speech(utterance('Hi Bob'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]) | _]" "$apps_gsn"
apps_line "APPS gsn network: bob's screen, each request pending, by its request name, until its transaction is taken" "B = [view(bob, [], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(befriend(alice, bob), yes), view(bob, [knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(join(alice, bob, speech(utterance(friends), alice)), yes), view(bob, [friend(alice), knows(alice), identity], [alice], [join(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [post(bob, alice, speech(utterance('Hi all'), bob), speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [leave(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]) | _]" "$apps_gsn"
# GSN's contract (programs/gsc/GSN.glp at GLP 542ec8ca), compiled with its listed role and its name terms (vGLP f5f1644 and c649c0d, Section 5.4, "Act schemas as local specifications"): close's part carries the list of the people of its listed role, its request name not; a class's arguments in the order of the head, post(p?, a ; x, n) as post(p, a, x, n) (GSC 0228e79); member(<a>.n) holding the community name of the group, one argument.
apps_gsn_text=$(cat "$GLP_DIR/programs/apps/gsn/platform.vglp")
check "APPS gsn: post(p?, a ; x, n) compiles as post(p, a, x, n)" "^binding_of(post(V1, V2, V3, V4), post, \[b(role(1), p(V1?)), b(role(2), p(V2?)), b(svar(x), s(V3?)), b(svar(n), s(V4?))\])$" "$apps_gsn_text"
check "APPS gsn: close's class term carries the list of the people of its listed role, a[listed(*, n)]" "^close(Peer, List(Peer), Speech)$" "$apps_gsn_text"
check "APPS gsn: member(<a>.n) holds a community name, one argument" "^member(CName)$" "$apps_gsn_text"
check "APPS gsn: ... written in the view as the contract writes it" "^atom_of(g(member, \[c(V1)\]), shown(member(V1?)))$" "$apps_gsn_text"
check "APPS gsn: ... the community name of the person bound to u followed by the speech acts bound to the y's, and dot_any(C), the image of N.* under a binding, of a cleared atom (vGLP f8127b1, Section 5.4; GSC 327e035, Definition Binding)" "^CName     ::= community(Peer) ; dot(CName, Speech) ; dot_any(CName)\.$" "$apps_gsn_text"
check "APPS gsn: bob's group message in the order of post's head, its content before the group's name" "own(\[B2, B3\], propose(post(B2?, Alice?, speech(utterance('Hi all'), B3?), speech(utterance(friends), Alice?))))" "$(cat "$GLP_DIR/programs/apps/gsn/person.glp")"
check "APPS gsn: the types written again: close's list and the community name, dot_any(C) with it" "^CName     ::= community(Peer) ; dot(CName, Speech) ; dot_any(CName)\.$" "$(cat "$GLP_DIR/programs/apps/gsn/self.glp")"
check "APPS gsn: alice signs her message by the key her view shows" "own(\[A7, A8\], propose(message(A7?, Bob?, speech(utterance('Hi Bob'), A8?))))" "$(cat "$GLP_DIR/programs/apps/gsn/person.glp")"
check_not "APPS gsn: no failed play" "→ failed" "$apps_gsn"
check_not "APPS gsn: no error" "Error" "$apps_gsn"
check "APPS gsn network: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsn"
check_not "APPS gsn network: ... and is not stopped at the limit" "capped" "$apps_gsn"

# GSNP's app, programs/apps/gsnp: the same host on GSNP's platform, its wrapper the same file as GSG's; the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4), five executions over four conversations, dan's with eve, eve's with carol, carol's with bob and bob's with fay (play_provenance), on GSNP's contract and pragmas (programs/gai/GSNP.glp and GSNP.txt), its views carrying the pending approvals and the tabs of the pragmas, Contacts, Friends, Groups, AI agents and Items: each establishes their identity; in dan and eve, eve and carol, carol and bob, and bob and fay, the first comes to know the second by their key, the second asked by its hello and answering yes, and then they become friends, the second asked and answering yes (GKK:2, GSG:1); dan signs the item 'Flood on Main Street' (GSNP:1, Authorship), which records him its author, and sends it to eve in a message, which carries it as he signed it (GSN:1, Message), nobody asked; eve forwards it to carol, and carol to bob (GSNP:2, Forwarding), nobody asked, the item of forward(p, q, y) being y signed by p, so that bob holds it signed by dan, then by eve, then by carol (programs/gai/GSNP.glp, the schemas sign, message and forward).
check "APPS gsnp: platform.vglp is what vGLP's compiler prints from GSNP.glp" "^gsnp/platform.vglp: as printed$" "$apps_print"
# The request type proposes the guarded classes only: GSNP's fifteen schemas that mark a role, and not drop, deliver or deliver_own, unguarded.
apps_gsnp_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gsnp/platform.vglp")
check "APPS gsnp: the request type proposes the guarded classes, GnW's and GAI's with GSN's, remove among them, not deliver or deliver_own" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) befriend(Peer, Peer) unfriend(Peer, Peer) enter(Peer, Peer, Speech) make_guardian(Peer, Peer, Peer, Speech) end_guardian(Peer, Peer, Peer) resign(Peer, Peer) guardian_unfriend(Peer, Peer) agent_acquaint(Peer, Peer, Peer, Speech) agent_befriend(Peer, Peer, Peer) agent_unfriend(Peer, Peer, Peer) agents_acquaint(Peer, Peer, Peer, Peer, Speech, Speech) agents_befriend(Peer, Peer, Peer, Peer) retire(Peer, Peer) sign(Peer, Speech) message(Peer, Peer, Speech) forward(Peer, Peer, Speech) create(Peer, Speech) join(Peer, Peer, Speech) leave(Peer, Peer, Speech) remove(Peer, Peer, Speech) close(Peer, Speech) post_own(Peer, Speech, Speech) post(Peer, Peer, Speech, Speech) post_forward(Peer, Peer, Speech, Speech) $" "$apps_gsnp_proposal"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gsnp/miniapp.glp"; then
    _check_pass "APPS gsnp: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gsnp: miniapp.glp is the same file as GSG's app's" "programs/apps/gsnp/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gsnp=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsnp $APPS_ART
$GLP_DIR/programs/apps/gsnp
:limit 6000000
play_provenance(D, E, C, B, F).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gsnp: the mini-app's artefact is certified" "gsnp.glpw --- certified under" "$apps_gsnp"
check "APPS gsnp loads" "Loaded program: .*programs/apps/gsnp$" "$apps_gsnp"
apps_line "APPS gsnp provenance: dan's screen, each request pending, by its request name, until its transaction is taken" "D = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(dan)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [eve], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [befriend(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [sign(dan, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [message(dan, eve, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: eve's screen, each request pending, by its request name, until its transaction is taken" "E = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(dan, eve), yes), view([identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [dan], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(dan, eve), yes), view([knows(dan), identity], [dan], [befriend(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(dan), knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [dan], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [befriend(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [forward(eve, carol, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: carol's screen, each request pending, by its request name, until its transaction is taken" "C = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(eve, carol), yes), view([identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [eve], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(eve, carol), yes), view([knows(eve), identity], [eve], [befriend(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [eve], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [befriend(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [forward(carol, bob, speech(speech(utterance('Flood on Main Street'), dan), eve))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: bob's screen, each request pending, by its request name, until its transaction is taken" "B = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(carol, bob), yes), view([identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [carol], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(carol, bob), yes), view([knows(carol), identity], [carol], [befriend(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [befriend(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [forward(bob, fay, speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp"
apps_line "APPS gsnp provenance: fay's screen, each request pending, by its request name, until its transaction is taken" "F = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(bob, fay), yes), view([identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [bob], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(bob, fay), yes), view([knows(bob), identity], [bob], [befriend(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol), bob)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp"
check "APPS gsnp: dan signs the item by the key his request question shows" "own(\[D3, D4\], propose(sign(D3?, speech(utterance('Flood on Main Street'), D4?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check "APPS gsnp: dan sends the item he authored to eve in a message, as he signed it" "own(\[D5, D6\], propose(message(D5?, Eve?, speech(utterance('Flood on Main Street'), D6?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
# The item's earlier signers, whom carol, bob and fay do not know, are holes their scripts fill by matching the item they hold, seek(Hs, Ks, A), its signers read off its nested speech acts, the author innermost (GAI's paper, Appendix "Contracts with Wards", "Atoms and states"; GAI #10 Cowork, Code_inbox.md, 2026-11-03 18:21 UTC): no script names a signer by an atom, since an agent is its key (IGLP, Remark "Agent Names").
check "APPS gsnp: carol finds the signer of the item before eve by matching it" "seek(\[CD\], \[CD1\], item(speech(speech(utterance('Flood on Main Street'), CD?), Eve?)))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check "APPS gsnp: carol forwards to bob the item signed by dan and then by eve" "own(\[C3\], propose(forward(C3?, Bob?, speech(speech(utterance('Flood on Main Street'), CD1?), Eve?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check "APPS gsnp: bob finds the signers of the item before carol by matching it" "seek(\[BD, BE\], \[BD1, BE1\], item(speech(speech(speech(utterance('Flood on Main Street'), BD?), BE?), Carol?)))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check "APPS gsnp: bob forwards to fay the item signed by dan, eve and carol" "own(\[B3\], propose(forward(B3?, Fay?, speech(speech(speech(utterance('Flood on Main Street'), BD1?), BE1?), Carol?))))" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check_not "APPS gsnp: no script names a signer by an atom" "), \(dan\|eve\|carol\|bob\|alf\|alice\|acme_ltd\))" "$(grep -v '^%%' "$GLP_DIR/programs/apps/gsnp/person.glp")"
check_not "APPS gsnp: no failed play" "→ failed" "$apps_gsnp"
check_not "APPS gsnp: no error" "Error" "$apps_gsnp"
check "APPS gsnp provenance: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsnp"
check_not "APPS gsnp provenance: ... and is not stopped at the limit" "capped" "$apps_gsnp"
# GSNP's app, play_agents: the play of GAI's Figure "The GAI app" and of the AI agent's item of Figure "The GSNP app" on the full platform, in the order of the example of GAI's paper (Section 3.2 and Table "The components that change in the example"; Code_inbox.md, GAI #10 Cowork, 2026-10-31 12:54 and 14:14 UTC): acme_ltd, the developer, establishes their identity, comes to know and befriends alice, signs alf's code and sends it to her by message (GAI:1, GSN:1); alice and bob establish their identities, come to know each other and befriend (GKK:1, GKK:2, GSG:1); alice signs her attestation y and enters alf with it (GAI:2, GnW:1); alice proposes alf's acquaintance with bob, alf and bob answering yes, bob recording agent_of(alf, y) (GnW:3, GAI:2), and then alf's befriending him, alf and bob answering yes (GnW:3); alf signs an item and sends it to bob (GSNP:1, GSN:1); and alice retires alf, which ends its friendships with her and with bob, nobody asked (GnW:5), the play holding her request until bob's screen shows the item (plays.glp, hold and told); no unfriend.
apps_gsnp_ag_art=$(mktemp -d)
apps_gsnp_ag=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsnp $apps_gsnp_ag_art
$GLP_DIR/programs/apps/gsnp
:limit 9000000
play_agents(K, A, R, B).
:quit
HEREDOC
2>&1)
rm -rf "$apps_gsnp_ag_art"
apps_line "APPS gsnp agents: acme_ltd's screen" "K = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(acme_ltd)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [acquaint(acme_ltd, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [alice], [acquaint(acme_ltd, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(alice), identity], [alice], [befriend(acme_ltd, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(alice), knows(alice), identity], [alice], [sign(acme_ltd, speech(utterance('The code of Alf'), acme_ltd))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), authored(speech(utterance('The code of Alf'), acme_ltd)), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), authored(speech(utterance('The code of Alf'), acme_ltd)), friend(alice), knows(alice), identity], [alice], [message(acme_ltd, alice, speech(utterance('The code of Alf'), acme_ltd))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), authored(speech(utterance('The code of Alf'), acme_ltd)), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp_ag"
apps_line "APPS gsnp agents: alice's screen" "A = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(acme_ltd, alice), yes), view([identity], [acme_ltd], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [acme_ltd], [acquaint(acme_ltd, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(acme_ltd), identity], [acme_ltd], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(acme_ltd, alice), yes), view([knows(acme_ltd), identity], [acme_ltd], [befriend(acme_ltd, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [sign(alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [retire(alf, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([retired_of(alf), principal_of(alf), guardian_of(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), item(speech(utterance('The code of Alf'), acme_ltd)), friend(acme_ltd), knows(acme_ltd), identity], [acme_ltd, alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp_ag"
apps_line "APPS gsnp agents: alf's screen" "R = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [alice], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [sign(alf, speech(utterance('Today\'s summary of the news'), alf))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [message(alf, bob, speech(utterance('Today\'s summary of the news'), alf))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([retired, item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), knows(bob), identity, agent, principal(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp_ag"
apps_line "APPS gsnp agents: bob's screen" "B = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(alice, bob), yes), view([identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(alice, bob), yes), view([knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([friend(alice), knows(alice), identity], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Today\'s summary of the news'), alf)), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Today\'s summary of the news'), alf)), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]) | _]" "$apps_gsnp_ag"
check "APPS gsnp agents: bob records alf with alice's attestation y, the speech act she signed" "agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))" "$apps_gsnp_ag"
check "APPS gsnp agents: bob holds the item alf signed" "item(speech(utterance('Today.'s summary of the news'), alf))" "$apps_gsnp_ag"
check "APPS gsnp agents: alice retires alf, alf retired, its friendships ended" "view(\[retired, item(speech(utterance('Today.'s summary of the news'), alf)), authored(speech(utterance('Today.'s summary of the news'), alf)), knows(bob), identity, agent, principal(alice), knows(alice), attestation" "$apps_gsnp_ag"
check_not "APPS gsnp agents: no unfriend in the play, retire ending alf's friendships" "propose(unfriend(" "$(cat "$GLP_DIR/programs/apps/gsnp/person.glp")"
check_not "APPS gsnp agents: no failed play" "→ failed" "$apps_gsnp_ag"
check_not "APPS gsnp agents: no error" "Error" "$apps_gsnp_ag"
check "APPS gsnp agents: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gsnp_ag"
check_not "APPS gsnp agents: ... and is not stopped at the limit" "capped" "$apps_gsnp_ag"

# GFF's app, programs/apps/gff: the same host on GFF's platform (programs/gff/GFF.glp and GFF.txt), its wrapper the same file as GSG's; GFF's play (Code_inbox.md, GFF #1 Cowork, 2026-10-27 16:34 UTC, with its revisions of 17:21, 18:39 and 19:2x and 20:25 UTC), five executions over four conversations, alice's with bob, bob's with carol, carol's with dave and dave's with eve (play_feeds): each two friends meet and befriend (GKK:2, GSG:1); alice posts two items (GFF:1); each two friends follow each other (GFF:2, friends_follow); once both items have reached bob he introduces alice to carol (GKK:3); carol decides to follow alice (GFF:2, follow), her machine following her through bob and asking her friends (through_friend, ask; GFF:3), and introduces her to dave; dave likewise through carol, and eve through dave; once eve follows alice, alice posts a third item, which reaches eve signed by alice, bob, carol and dave, once each (GFF:4); once it has, carol ends her following; once she has, alice posts a fourth, which reaches bob and not carol, dave or eve.  Each "once" across two persons is the play's, a watcher on one screen giving the other person the rest of their script (plays.glp).
check "APPS gff: platform.vglp is what vGLP's compiler prints from GFF.glp" "^gff/platform.vglp: as printed$" "$apps_print"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gff/miniapp.glp"; then
    _check_pass "APPS gff: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gff: miniapp.glp is the same file as GSG's app's" "programs/apps/gff/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
apps_gff_art=$(mktemp -d)
apps_gff=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gff $apps_gff_art
$GLP_DIR/programs/apps/gff
:limit 30000000
play_feeds(A, B, C, D, E).
:quit
HEREDOC
2>&1)
rm -rf "$apps_gff_art"
check "APPS gff: the mini-app's artefact is certified" "gff.glpw --- certified under" "$apps_gff"
check "APPS gff loads" "Loaded program: .*programs/apps/gff$" "$apps_gff"
apps_line "APPS gff feeds: alice's screen" "A = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [post(alice, speech(utterance('First item'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [post(alice, speech(utterance('Second item'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [post(alice, speech(utterance('Third item'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([feed(speech(utterance('Third item'), alice)), sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Third item'), alice), bob), feed(speech(utterance('Third item'), alice)), sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Third item'), alice), bob), feed(speech(utterance('Third item'), alice)), sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [post(alice, speech(utterance('Fourth item'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([feed(speech(utterance('Fourth item'), alice)), sent(speech(utterance('Third item'), alice), bob), feed(speech(utterance('Third item'), alice)), sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Fourth item'), alice), bob), feed(speech(utterance('Fourth item'), alice)), sent(speech(utterance('Third item'), alice), bob), feed(speech(utterance('Third item'), alice)), sent(speech(utterance('First item'), alice), bob), sent(speech(utterance('Second item'), alice), bob), follower(bob, alice), feed(speech(utterance('Second item'), alice)), feed(speech(utterance('First item'), alice)), follows(bob, bob), following(bob), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]) | _]" "$apps_gff"
apps_line "APPS gff feeds: bob's screen" "B = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(acquaint(alice, bob), yes), view([identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(befriend(alice, bob), yes), view([knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), identity], [alice], [befriend(alice, bob), acquaint(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), identity], [alice, carol], [befriend(alice, bob), acquaint(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(alice), knows(alice), identity], [alice, carol], [acquaint(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [befriend(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [befriend(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [introduce(bob, carol, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('First item'), alice), carol), follower(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Second item'), alice), carol), sent(speech(utterance('First item'), alice), carol), follower(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(utterance('Third item'), alice), alice), sent(speech(utterance('Second item'), alice), carol), sent(speech(utterance('First item'), alice), carol), follower(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Third item'), alice), carol), item(speech(utterance('Third item'), alice), alice), sent(speech(utterance('Second item'), alice), carol), sent(speech(utterance('First item'), alice), carol), follower(carol, alice), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(utterance('Third item'), alice), carol), item(speech(utterance('Third item'), alice), alice), sent(speech(utterance('Second item'), alice), carol), sent(speech(utterance('First item'), alice), carol), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(utterance('Fourth item'), alice), alice), sent(speech(utterance('Third item'), alice), carol), item(speech(utterance('Third item'), alice), alice), sent(speech(utterance('Second item'), alice), carol), sent(speech(utterance('First item'), alice), carol), follows(carol, carol), following(carol), item(speech(utterance('First item'), alice), alice), follower(carol, bob), item(speech(utterance('Second item'), alice), alice), friend(carol), follows(alice, alice), following(alice), follower(alice, bob), knows(carol), friend(alice), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]) | _]" "$apps_gff"
apps_line "APPS gff feeds: carol's screen" "C = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([], [], [establish(carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(acquaint(bob, carol), yes), view([identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [bob], [acquaint(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(befriend(bob, carol), yes), view([knows(bob), identity], [bob], [befriend(bob, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(bob), identity], [bob], [befriend(bob, carol), acquaint(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(bob), identity], [bob, dave], [befriend(bob, carol), acquaint(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(bob), knows(bob), identity], [bob, dave], [acquaint(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [befriend(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(introduce(bob, carol, alice), yes), view([follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [introduce(bob, carol, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [follow(carol, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [introduce(carol, dave, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked(dave, alice), sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(utterance('Third item'), alice), bob), alice), sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('Third item'), alice), bob), dave), item(speech(speech(utterance('Third item'), alice), bob), alice), sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('Third item'), alice), bob), dave), item(speech(speech(utterance('Third item'), alice), bob), alice), sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), follows(alice, bob), following(alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), decided(alice), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [unfollow(carol, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(utterance('Third item'), alice), bob), dave), item(speech(speech(utterance('Third item'), alice), bob), alice), sent(speech(speech(utterance('First item'), alice), bob), dave), sent(speech(speech(utterance('Second item'), alice), bob), dave), follower(dave, alice), item(speech(speech(utterance('Second item'), alice), bob), alice), item(speech(speech(utterance('First item'), alice), bob), alice), path(alice, bob, speech(utterance('First item'), alice)), does_not_follow(alice, dave), asked_of(bob, alice), asked_of(dave, alice), follower(dave, carol), follows(dave, dave), following(dave), knows(alice), follower(bob, carol), friend(dave), follows(bob, bob), following(bob), knows(dave), friend(bob), knows(bob), identity], [bob, dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]) | _]" "$apps_gff"
apps_line "APPS gff feeds: dave's screen" "D = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([], [], [establish(dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(acquaint(carol, dave), yes), view([identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [carol], [acquaint(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(befriend(carol, dave), yes), view([knows(carol), identity], [carol], [befriend(carol, dave)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(carol), identity], [carol], [befriend(carol, dave), acquaint(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(carol), identity], [carol, eve], [befriend(carol, dave), acquaint(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(carol), knows(carol), identity], [carol, eve], [acquaint(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(eve), friend(carol), knows(carol), identity], [carol, eve], [befriend(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked(carol, alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(introduce(carol, dave, alice), yes), view([follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [introduce(carol, dave, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [follow(dave, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [introduce(dave, eve, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked(eve, alice), sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(utterance('Third item'), alice), bob), carol), alice), asked(eve, alice), sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(speech(utterance('Second item'), alice), bob), carol), eve), item(speech(speech(speech(utterance('Third item'), alice), bob), carol), alice), asked(eve, alice), sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(speech(utterance('Second item'), alice), bob), carol), eve), item(speech(speech(speech(utterance('Third item'), alice), bob), carol), alice), sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([sent(speech(speech(speech(utterance('Third item'), alice), bob), carol), eve), sent(speech(speech(speech(utterance('Second item'), alice), bob), carol), eve), item(speech(speech(speech(utterance('Third item'), alice), bob), carol), alice), sent(speech(speech(speech(utterance('First item'), alice), bob), carol), eve), follower(eve, alice), path(alice, carol, speech(speech(utterance('Second item'), alice), bob)), asked_of(carol, alice), item(speech(speech(speech(utterance('First item'), alice), bob), carol), alice), does_not_follow(alice, eve), item(speech(speech(speech(utterance('Second item'), alice), bob), carol), alice), asked_of(eve, alice), follows(alice, carol), following(alice), decided(alice), knows(alice), follows(carol, carol), following(carol), follows(eve, eve), following(eve), follower(eve, dave), follower(carol, dave), friend(eve), knows(eve), friend(carol), knows(carol), identity], [carol, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]) | _]" "$apps_gff"
apps_line "APPS gff feeds: eve's screen" "E = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([], [], [establish(eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(acquaint(dave, eve), yes), view([identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([identity], [dave], [acquaint(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(befriend(dave, eve), yes), view([knows(dave), identity], [dave], [befriend(dave, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked(dave, alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), gave(introduce(dave, eve, alice), yes), view([follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [introduce(dave, eve, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [follow(eve, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(speech(utterance('First item'), alice), bob), carol), dave), alice), follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([asked_of(dave, alice), item(speech(speech(speech(speech(utterance('First item'), alice), bob), carol), dave), alice), follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(speech(utterance('Second item'), alice), bob), carol), dave), alice), asked_of(dave, alice), item(speech(speech(speech(speech(utterance('First item'), alice), bob), carol), dave), alice), follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([path(alice, dave, speech(speech(speech(utterance('Third item'), alice), bob), carol)), item(speech(speech(speech(speech(utterance('Second item'), alice), bob), carol), dave), alice), asked_of(dave, alice), item(speech(speech(speech(speech(utterance('First item'), alice), bob), carol), dave), alice), follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]), view([item(speech(speech(speech(speech(utterance('Third item'), alice), bob), carol), dave), alice), path(alice, dave, speech(speech(speech(utterance('Third item'), alice), bob), carol)), item(speech(speech(speech(speech(utterance('Second item'), alice), bob), carol), dave), alice), asked_of(dave, alice), item(speech(speech(speech(speech(utterance('First item'), alice), bob), carol), dave), alice), follows(alice, dave), following(alice), decided(alice), knows(alice), follower(dave, eve), follows(dave, dave), following(dave), friend(dave), knows(dave), identity], [dave], [], [tab('Contacts', knows), tab('Friends', friend), tab('Posts', item), tab('Feeds', follows)]) | _]" "$apps_gff"
check "APPS gff feeds: eve holds the third item, signed by alice, bob, carol and dave, once each" "E = .*item(speech(speech(speech(speech(utterance('Third item'), alice), bob), carol), dave), alice)" "$apps_gff"
check "APPS gff feeds: bob holds the fourth item" "B = .*item(speech(utterance('Fourth item'), alice), alice)" "$apps_gff"
check_not "APPS gff feeds: carol, dave and eve do not hold the fourth item" "[CDE] = .*Fourth item" "$apps_gff"
check_not "APPS gff: no failed play" "→ failed" "$apps_gff"
check_not "APPS gff: no error" "Error" "$apps_gff"
check "APPS gff feeds: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gff"
check_not "APPS gff feeds: ... and is not stopped at the limit" "capped" "$apps_gff"

# GAI's app, programs/apps/gai: the same host on GAI's platform, its wrapper the same file as GSG's; the example of GAI's paper (Section 3.2, the example after Example "An AI agent coming to know a person, and befriending them", and Table "The components that change in the example"; GAI at e4d935c), on GAI's contract (programs/gai/GAI.glp), three executions over three conversations, alice's with alf, alf's with bob and bob's with alice (play_agents), in the paper's order: alice and bob establish their identities (GKK:1), come to know each other (GKK:2; acquaint) and befriend (GSG:1; befriend), alice proposing and bob answering yes; alice signs her attestation y, 'Alf runs code signed by Acme Ltd' (GAI:2; sign(p? ; x)), and enters alf with it, which answers yes, she its principal, a guardian and its friend and holding the record agent_of(alf, y), alf holding attestation(y) (GnW:1, GAI:1, GAI:2; enter(p?, r? ; y)); alice proposes alf's acquaintance with bob, alf and bob answering yes, bob recording agent_of(alf, y) (GnW:3, GAI:2; agent_acquaint(r?, q?, p? ; y)), and once it is done, no longer among her pending approvals, alf's befriending bob, alf and bob answering yes (GnW:3; agent_befriend(r?, q?, p?)); and once that is done alice retires alf (GnW:5; retire(r, p?), the AI agent its first role), which ends alf's friendships with her and, by the listed role Carol[friend(*)], with bob, nobody asked, alice holding retired_of(alf) and alf retired.  The final states are those of the paper's table after Retire, less the item, which alf signs and sends to bob on GSNP's app, the full platform (GAI has no message); there is no unfriend.
check "APPS gai: platform.vglp is what vGLP's compiler prints from GAI.glp" "^gai/platform.vglp: as printed$" "$apps_print"
if cmp -s "$GLP_DIR/programs/apps/gai/platform.vglp" "$GLP_DIR/cascade/GAI/platform.vglp"; then
    _check_pass "APPS gai: platform.vglp is the platform of the recorded run of the cascade on GAI"
else
    _check_fail "APPS gai: platform.vglp is the platform of the recorded run of the cascade on GAI" "programs/apps/gai/platform.vglp differs from cascade/GAI/platform.vglp"
fi
# The request type proposes the guarded classes: every schema of GAI marks a role, seventeen, GnW:2's make_guardian, end_guardian, resign and guardian_unfriend among them (programs/gai/GAI.glp and GAI.txt, "seventeen over fifteen predicates").
apps_gai_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gai/platform.vglp")
check "APPS gai: the request type proposes the seventeen classes, every schema marking a role" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) befriend(Peer, Peer) unfriend(Peer, Peer) enter(Peer, Peer, Speech) make_guardian(Peer, Peer, Peer, Speech) end_guardian(Peer, Peer, Peer) resign(Peer, Peer) guardian_unfriend(Peer, Peer) agent_acquaint(Peer, Peer, Peer, Speech) agent_befriend(Peer, Peer, Peer) agent_unfriend(Peer, Peer, Peer) agents_acquaint(Peer, Peer, Peer, Peer, Speech, Speech) agents_befriend(Peer, Peer, Peer, Peer) retire(Peer, Peer) sign(Peer, Speech) $" "$apps_gai_proposal"
check "APPS gai: the view writes the record of an AI agent and its attestation, agent_of(r, y), at each guardian (GAI.glp, enter: the record agent_of(Carol, y) of its attestation y)" "^atom_of(g(agent_of, \[p(V1), s(V2)\]), shown(agent_of(V1?, V2?)))$" "$(cat "$GLP_DIR/programs/apps/gai/platform.vglp")"
check "APPS gai: the types written again hold GAI's atoms, as the platform prints them" "^agent_of(Peer, Speech)$" "$(cat "$GLP_DIR/programs/apps/gai/self.glp")"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gai/miniapp.glp"; then
    _check_pass "APPS gai: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gai: miniapp.glp is the same file as GSG's app's" "programs/apps/gai/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gai=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gai $APPS_ART
$GLP_DIR/programs/apps/gai
:limit 6000000
play_agents(A, R, B).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gai: the mini-app's artefact is certified" "gai.glpw --- certified under" "$apps_gai"
check "APPS gai loads" "Loaded program: .*programs/apps/gai$" "$apps_gai"
apps_line "APPS gai agents: alice's screen" "A = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity], [bob], [sign(alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [retire(alf, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([retired_of(alf), principal_of(alf), guardian_of(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]) | _]" "$apps_gai"
apps_line "APPS gai agents: alf's screen" "R = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [alice], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([retired, knows(bob), identity, agent, principal(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]) | _]" "$apps_gai"
apps_line "APPS gai agents: bob's screen" "B = [view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(acquaint(alice, bob), yes), view([identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(befriend(alice, bob), yes), view([knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([friend(alice), knows(alice), identity], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]) | _]" "$apps_gai"
check "APPS gai: alice asks to come to know bob by the key her request question shows (GKK:2)" "own(\[A2\], propose(acquaint(A2?, Bob?)))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: alice offers bob friendship by the key her request question shows (GSG:1)" "own(\[A3\], propose(befriend(A3?, Bob?)))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: alice signs her attestation y, by the key her request question shows, GAI's \"Alice [Sign] <text>\" (GAI:2)" "own(\[A4, A5\], propose(sign(A4?, speech(utterance('Alf runs code signed by Acme Ltd'), A5?))))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: alice enters her AI agent alf with y, by the key her request question shows (GnW:1)" "own(\[A6, A7\], propose(enter(A6?, Alf?, speech(utterance('Alf runs code signed by Acme Ltd'), A7?))))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: alice, alf's guardian, proposes its acquaintance with bob, presenting y, GAI's \"Alice [Agent acquaint] <person>\" (GnW:3, GAI:2)" "own(\[A8\], propose(agent_acquaint(Alf?, Bob?, A8?, speech(utterance('Alf runs code signed by Acme Ltd'), Me?))))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: ... and once it is done its befriending bob, GAI's \"Alice [Agent befriend] <person>\" (GnW:3)" "own(\[A9\], propose(agent_befriend(Alf?, Bob?, A9?)))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check "APPS gai: ... and once that is done retires alf, the AI agent retire's first role (GnW:5)" "own(\[A10\], propose(retire(Alf?, A10?)))" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check_not "APPS gai: nobody ends a friendship with alf by unfriend: retire ends its friendships (GnW:5)" "propose(unfriend(" "$(cat "$GLP_DIR/programs/apps/gai/person.glp")"
check_not "APPS gai: no failed play" "→ failed" "$apps_gai"
check_not "APPS gai: no error" "Error" "$apps_gai"
check "APPS gai agents: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gai"
check_not "APPS gai agents: ... and is not stopped at the limit" "capped" "$apps_gai"

# GC's app, programs/apps/gc: the same host on GC's platform, its wrapper the same file as GSG's; GC is Currencies' contract on GKK and not GSG (programs/currencies/coins/GC.glp), the app built for GSC's paper (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2); two executions over one conversation (play_coins): each establishes their identity (GKK:1) and bob mints a Bob-coin (GC:1, mint(p?)); alice asks to meet bob in person and he answers yes (GKK:2); alice mints an Alice-coin and asks bob to swap it for his Bob-coin, swap(alice, bob, alice, bob), and he answers yes (GC:2, swap(p?, q? ; u, v)); and alice pays the Bob-coin to bob, pay(alice, bob), which asks nobody (GC:3, pay(p?, q)).
check "APPS gc: platform.vglp is what vGLP's compiler prints from GC.glp" "^gc/platform.vglp: as printed$" "$apps_print"
if cmp -s "$GLP_DIR/programs/apps/gc/platform.vglp" "$GLP_DIR/cascade/GC/platform.vglp"; then
    _check_pass "APPS gc: platform.vglp is the platform of the recorded run of the cascade on GC"
else
    _check_fail "APPS gc: platform.vglp is the platform of the recorded run of the cascade on GC" "programs/apps/gc/platform.vglp differs from cascade/GC/platform.vglp"
fi
apps_gc_proposal=$(awk '/^Proposal ::=$/{on=1;next} on&&/^\.$/{exit} on&&!/^;$/{printf "%s ", $0}' "$GLP_DIR/programs/apps/gc/platform.vglp")
check "APPS gc: the request type proposes GKK's three classes and GC's four, mint, swap and pay naming their amounts (vGLP, Section 7.3)" "^establish(Peer) acquaint(Peer, Peer) introduce(Peer, Peer, Peer) mint(Peer, Integer) swap(Peer, Peer, Peer, Integer, Peer, Integer) pay(Peer, Peer, Integer) redeem(Peer, Peer, Peer) $" "$apps_gc_proposal"
check "APPS gc: the types written again hold coin(Peer)" "^Atom     ::= identity ; knows(Peer) ; coin(Peer)\.$" "$(cat "$GLP_DIR/programs/apps/gc/self.glp")"
if cmp -s "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gc/miniapp.glp"; then
    _check_pass "APPS gc: miniapp.glp is the same file as GSG's app's"
else
    _check_fail "APPS gc: miniapp.glp is the same file as GSG's app's" "programs/apps/gc/miniapp.glp differs from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gc=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gc $APPS_ART
$GLP_DIR/programs/apps/gc
:limit 600000
play_coins(A, B).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
check "APPS gc: the mini-app's artefact is certified" "gc.glpw --- certified under" "$apps_gc"
check "APPS gc loads" "Loaded program: .*programs/apps/gc$" "$apps_gc"
apps_line "APPS gc coins: alice's screen, each request pending until it commits, her amounts the fields of her requests" "A = [view(alice, [], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [knows(bob), identity], [bob], [mint(alice, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(alice), coin(alice), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(alice), coin(alice), knows(bob), identity], [bob], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, [bob]), view(alice, [coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [pay(alice, bob, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]) | _]" "$apps_gc"
apps_line "APPS gc coins: bob's screen, asked on alice's hello of acquaint and on her attempt of the swap" "B = [view(bob, [], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [identity], [], [mint(bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(bob), identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(acquaint(alice, bob), yes), view(bob, [coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(bob), identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(swap(alice, bob, alice, 2, bob, 3), yes), view(bob, [knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]) | _]" "$apps_gc"
check "APPS gc: alice asks bob to swap two Alice-coins for three Bob-coins, in the order of swap's head, swap(p?, q? ; u, k, v, k')" "own(\[A3, A4\], propose(swap(A3?, Bob?, A4?, 2, Bob?, 3)))" "$(cat "$GLP_DIR/programs/apps/gc/person.glp")"
check "APPS gc: alice pays two Bob-coins to bob by the key her request question shows, pay(p?, q ; k)" "own(\[A5\], propose(pay(A5?, Bob?, 2)))" "$(cat "$GLP_DIR/programs/apps/gc/person.glp")"
check_not "APPS gc: no failed play" "→ failed" "$apps_gc"
check_not "APPS gc: no error" "Error" "$apps_gc"
check "APPS gc coins: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gc"
check_not "APPS gc coins: ... and is not stopped at the limit" "capped" "$apps_gc"

# GB's app, programs/apps/gb: the same host on GB's platform, Currencies' contract on GKK (programs/currencies/bonds/GB.glp and GB.txt; Currencies #11 Cowork, Code_inbox.md, 2026-10-20 19:57 UTC, task 2; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 4), its wrapper GSG's but that it hands the agent the calendar, GB requiring the day (vGLP, Section 5.4, "Act schemas as local specifications"); three executions, alice's with a conversation with each of bob and carol (play_bonds): each gets day 1 as their date (GB:2, first_date); alice meets bob and carol (GKK:2); alice mints Alice-coins and Alice-bonds, bob Bob-bonds (GC:1, GB:1); alice asks bob for a loan, two Alice-coins for three Bob-bonds, and he answers yes (GB:3); bob's date advances to day 5 by his calendar (GB:2, advance); alice redeems a matured Bob-bond against an Alice-coin bob holds (GB:4); and alice deposits two Alice-coins with carol, who answers yes (GB:5).
check "APPS gb: platform.vglp is what vGLP's compiler prints from GB.glp and GB.txt" "^gb/platform.vglp: as printed$" "$apps_print"
check "APPS gb: the view names three tabs, one for each list line of the pragmas, GKK's Contacts and GB's Coins and Bonds, the two counted, GB adding and deleting coin and bond with multiplicities (GSC, Section 2; vGLP, Section 5.4)" '^tabs(\[tab("Contacts", knows), tab("Coins", coin, counted), tab("Bonds", bond, counted)\])$' "$(grep '^tabs(' "$GLP_DIR/programs/apps/gb/platform.vglp")"
check "APPS gb: Redeem's fields name the variables the atoms the proposer's role requires do not bind, the issuer's date t, local to the issuer's role, left out (vGLP, Section 5.4)" 'act(redeem_bc, 1, \[1, 2\], \[req(identity, \[\]), req(knows, \[at(2)\]), req(bond, \[at(2), at(3)\])\], \[2, 3\], \[field(4, "issuer wanted", agent)\], \[\], \[\]), act(redeem_bb, 1, \[1, 2\], \[req(identity, \[\]), req(knows, \[at(2)\]), req(bond, \[at(2), at(3)\])\], \[2, 3\], \[field(4, "issuer wanted", agent), field(5, "maturity wanted", number)\], \[\], \[\])' "$(grep '^Request? =::=' "$GLP_DIR/programs/apps/gb/platform.vglp")"
check "APPS gb: the types written again hold coin(Peer), bond(Peer, Integer) and the date" "^Atom     ::= identity ; knows(Peer) ; coin(Peer) ; bond(Peer, Integer) ; date(Integer) ; dated\.$" "$(cat "$GLP_DIR/programs/apps/gb/self.glp")"
apps_gb_wrap=$(diff "$GLP_DIR/programs/apps/gsg/miniapp.glp" "$GLP_DIR/programs/apps/gb/miniapp.glp" | grep -c '^[<>]')
if [ "$apps_gb_wrap" = 10 ]; then
    _check_pass "APPS gb: miniapp.glp is GSG's app's but for the calendar, five lines"
else
    _check_fail "APPS gb: miniapp.glp is GSG's app's but for the calendar, five lines" "$apps_gb_wrap lines differ from programs/apps/gsg/miniapp.glp"
fi
APPS_ART=$(mktemp -d)
apps_gb=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gb $APPS_ART
$GLP_DIR/programs/apps/gb
:limit 60000000
play_bonds(A, B, C).
:quit
HEREDOC
2>&1)
rm -rf "$APPS_ART"
apps_gb_flat=$(echo "$apps_gb" | sed -e "s/, \[tab('Contacts', knows), tab('Coins', coin, counted), tab('Bonds', bond, counted)\])/)/g")
check "APPS gb: the mini-app's artefact is certified" "gb.glpw --- certified under" "$apps_gb"
check "APPS gb loads" "Loaded program: .*programs/apps/gb$" "$apps_gb"
check_not "APPS gb: no failed play" "→ failed" "$apps_gb"
check_not "APPS gb: no error" "Error" "$apps_gb"
check "APPS gb bonds: the run suspends, no agent left with an own transaction or a trigger" "→ suspended" "$apps_gb"
check_not "APPS gb bonds: ... and is not stopped at the limit" "capped" "$apps_gb"
check_not "APPS gb bonds: no attempt refused" "refused(" "$apps_gb"
check "APPS gb bonds: each gets the first day of the calendar as their date, once (GB:2, first_date)" "C = \[view(carol, \[\], \[\], \[\]), view(carol, \[\], \[\], \[establish(carol)\]), view(carol, \[identity\], \[\], \[\]), view(carol, \[date(1), dated, identity\], \[\], \[\])" "$apps_gb_flat"
check "APPS gb bonds: alice mints three Alice-bonds maturing at day 10, later than her date (GB:1)" "view(alice, \[bond(alice, 10), bond(alice, 10), bond(alice, 10), coin(alice)" "$apps_gb_flat"
check "APPS gb bonds: bob answers yes to the loan, two Alice-coins for three Bob-bonds maturing at day 5 (GB:3)" "gave(swap_cb(alice, bob, alice, 2, bob, 5, 3), yes)" "$apps_gb_flat"
check "APPS gb bonds: bob's date advances a day at a time to day 5, the day his calendar gives (GB:2, advance), bob knowing alice twice over, by their meeting and by the Alice-coins he took in the loan, the swap adding at the role that receives coins knows of their issuer (GB.glp 7080bc69, GB:3: \"each comes to know the issuer of what they receive\")" "view(bob, \[date(4), coin(alice), coin(alice), knows(alice), knows(alice), bond(bob, 5), dated, identity\], \[alice\], \[\]), view(bob, \[date(5), coin(alice), coin(alice), knows(alice), knows(alice), bond(bob, 5), dated, identity\], \[alice\], \[\])" "$apps_gb_flat"
check "APPS gb bonds: alice redeems a matured Bob-bond against an Alice-coin bob holds, and nobody is asked, and comes to know the coin's issuer, herself (GB:4; GB.glp 7080bc69: \"p comes to know r\")" "\[redeem_bc(alice, bob, 5, alice)\]), view(alice, \[coin(alice), knows(alice), date(5), bond(bob, 5), bond(bob, 5)," "$apps_gb_flat"
check "APPS gb bonds: ... and bob holds the bond presented and one Alice-coin" "view(bob, \[bond(bob, 5), date(5), coin(alice), knows(alice), knows(alice), bond(bob, 5), dated, identity\], \[alice\], \[\])" "$apps_gb_flat"
check "APPS gb bonds: carol answers yes to alice's deposit of two Alice-coins, and holds them, and comes to know their issuer (GB:5; GB.glp 7080bc69: \"e comes to know u\")" "gave(deposit_c(alice, carol, alice, 2), yes), view(carol, \[knows(alice), date(1), dated, identity\], \[alice\], \[deposit_c(alice, carol, alice, 2)\]), view(carol, \[coin(alice), coin(alice), knows(alice), knows(alice), date(1), dated, identity\], \[alice\], \[\])" "$apps_gb_flat"

# Each app on two smartphones: the super-app of programs/social/graph/core on two isolates, each person with its own key pair, hosting the app from its certified artefact, gsg.glpw or gsn.glpw, which :artefact writes beside the super-app and load_file/2 reads there (programs/apps/gsg_boot.glp, gsn_boot.glp).  The two persons' lines interleave as the isolates run; each person's lines, their prefix stripped, are held exactly, in order: what the super-app told the person, sg(...), and the mini-app's screen, the steps of play_friends and play_network.
# A boot prints each line an agent sends its person in GLP syntax, as the REPL prints a binding, a quoted constant quoted, so that the line reads back as the term: utterance('Flood on Main Street') (GLP #9 Cowork, Code_inbox 2026-10-20 09:22 UTC, item 4.2).
apps_person() { # apps_person <person> <output>: the person's lines in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_same() { # apps_same <name> <expected> <actual>
    if [ "$3" = "$2" ]; then _check_pass "$1"; else _check_fail "$1" "expected: $2; got: $3"; fi
}
apps_same_either() { # apps_same_either <name> <expected> <other expected> <actual>: where the isolates may run in either order, both orders correct runs
    if [ "$4" = "$2" ] || [ "$4" = "$3" ]; then _check_pass "$1"; else _check_fail "$1" "expected: $2; or: $3; got: $4"; fi
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
apps_same "APPS gsg on two smartphones: alice's lines" "[sg(connected(bob)), view(alice, [], [], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, [bob]), view(alice, [knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend)])]" "$(apps_person alice "$apps_gsg_boot")"
apps_same "APPS gsg on two smartphones: bob's lines" "[sg(befriend(alice)), sg(connected(alice)), view(bob, [], [], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), gave(befriend(alice, bob), yes), view(bob, [knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [friend(alice), knows(alice), identity], [alice], [unfriend(bob, alice)], [tab('Contacts', knows), tab('Friends', friend)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend)])]" "$(apps_person bob "$apps_gsg_boot")"

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
apps_same "APPS gsn on two smartphones: alice's lines" "[sg(connected(bob)), view(alice, [], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [friend(bob), knows(bob), identity], [bob], [create(alice, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [join(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [message(alice, bob, speech(utterance('Hi Bob'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), listed(bob, speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(alice, [delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)])]" "$(apps_person alice "$apps_gsn_boot")"
apps_same "APPS gsn on two smartphones: bob's lines" "[sg(befriend(alice)), sg(connected(alice)), view(bob, [], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(befriend(alice, bob), yes), view(bob, [knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), gave(join(alice, bob, speech(utterance(friends), alice)), yes), view(bob, [friend(alice), knows(alice), identity], [alice], [join(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [post(bob, alice, speech(utterance('Hi all'), bob), speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), knows(alice), identity], [alice], [leave(alice, bob, speech(utterance(friends), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)]), view(bob, [received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(speech(utterance('Hi Bob'), alice), alice), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('Messages', received), tab('Posts', received_in)])]" "$(apps_person bob "$apps_gsn_boot")"

# GSNP's app on five smartphones (programs/apps/gsnp_boot.glp): dan connects to eve, and once connected eve connects to carol, carol to bob and bob to fay; each pair befriends in the super-app's social graph; once connected to the next, each invites them to gsnp; and the steps of play_provenance run over the four conversations.  Each person's super-app lines, sg(...), and their mini-app screen are each held exactly, in order: across the isolates a person's super-app events and their mini-app's are not ordered against each other.
apps_person_sg() { # apps_person_sg <person> <output>: the person's super-app lines, sg(...), in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") == 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_person_screen() { # apps_person_screen <person> <output>: the person's mini-app screen, every other line of theirs, in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") != 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_person_acts() { # apps_person_acts <person> <output>: the person's answers and looks, their lines that are neither sg(...) nor view(...), in order, as a list
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") != 1 && index(substr($0, length(p) + 1), "view(") != 1 { s = s (n++ ? ", " : "") substr($0, length(p) + 1) } END { print "[" s "]" }'
}
apps_person_last_view() { # apps_person_last_view <person> <output>: the person's last view
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "view(") == 1 { v = substr($0, length(p) + 1) } END { print v }'
}
apps_count() { # apps_count: the number of top-level items of a list on stdin, [a, b(c, d), e] counting 3
    awk '{ d = 0; n = 0; q = 0; for (i = 1; i <= length($0); i++) { c = substr($0, i, 1); if (c == "\x27") q = !q; if (q) continue; if (c == "(" || c == "[") d++; else if (c == ")" || c == "]") d--; else if (c == "," && d == 1) n++ } print (length($0) > 2 ? n + 1 : 0) }'
}
apps_person_sg_sorted() { # apps_person_sg_sorted <person> <output>: the person's super-app lines, sg(...), sorted, as a list, where the isolates' order is not held
    echo "$2" | sed -e 's/^\(GLP> \)*//' | awk -v p="[$1] " 'index($0, p) == 1 && index(substr($0, length(p) + 1), "sg(") == 1 { print substr($0, length(p) + 1) }' | sort | awk '{ s = s (n++ ? ", " : "") $0 } END { print "[" s "]" }'
}

# GKK's app on three smartphones (programs/apps/gkk_boot.glp): the super-app of programs/social/graph/core on three isolates, each person with its own key pair, hosting the base mini-app from its certified artefact gkk.glpw.  alice connects to bob and to carol, invites each once connected, and once both are her friends introduces them to each other in the social graph, the cards answered by the scripted home; carol invites bob once connected to him; and play_keys's steps run over the three conversations.  Each person's mini-app screen is held to the screen play_keys holds for them, exactly, in order; their super-app lines, sg(connected(...)), are held in either order, the isolates running in either.
apps_gkk_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gkk $APPS_CORE
:boot $GLP_DIR/programs/apps/gkk_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gkk.glpw"
check "APPS gkk on three smartphones: the mini-app's artefact is certified" "gkk.glpw --- certified under" "$apps_gkk_boot"
check "APPS gkk on three smartphones: the boot settles" "Boot settled: 3 agents (alice, bob, carol)" "$apps_gkk_boot"
check_not "APPS gkk on three smartphones: no event stopped at the cap" "did not quiesce" "$apps_gkk_boot"
apps_same_either "APPS gkk on three smartphones: alice's super-app lines" "[sg(connected(bob)), sg(connected(carol))]" "[sg(connected(carol)), sg(connected(bob))]" "$(apps_person_sg alice "$apps_gkk_boot")"
apps_same "APPS gkk on three smartphones: bob's super-app lines" "[sg(connected(alice)), sg(connected(carol))]" "$(apps_person_sg bob "$apps_gkk_boot")"
apps_same "APPS gkk on three smartphones: carol's super-app lines" "[sg(connected(alice)), sg(connected(bob))]" "$(apps_person_sg carol "$apps_gkk_boot")"
apps_same "APPS gkk on three smartphones: alice's screen is play_keys's" "[view(alice, [], [], [], [tab('Contacts', knows)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows)]), view(alice, [identity], [], [], [tab('Contacts', knows)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob, carol], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)]), menu(alice, [bob, carol]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)])]" "$(apps_person_screen alice "$apps_gkk_boot")"
apps_same "APPS gkk on three smartphones: bob's screen is play_keys's" "[view(bob, [], [], [], [tab('Contacts', knows)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows)]), view(bob, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), gave(introduce(alice, bob, carol), yes), view(bob, [knows(alice), identity], [alice], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)])]" "$(apps_person_screen bob "$apps_gkk_boot")"
apps_same "APPS gkk on three smartphones: carol's screen is play_keys's" "[view(carol, [], [], [], [tab('Contacts', knows)]), view(carol, [], [], [establish(carol)], [tab('Contacts', knows)]), view(carol, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, carol), yes), view(carol, [identity], [alice], [], [tab('Contacts', knows)]), view(carol, [identity], [alice], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)]), gave(acquaint(bob, carol), yes), view(carol, [knows(alice), identity], [alice, bob], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(carol, [knows(bob), knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)])]" "$(apps_person_screen carol "$apps_gkk_boot")"
apps_gsnp_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gsnp $APPS_CORE
:boot $GLP_DIR/programs/apps/gsnp_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gsnp.glpw"
check "APPS gsnp on five smartphones: the mini-app's artefact is certified" "gsnp.glpw --- certified under" "$apps_gsnp_boot"
check "APPS gsnp on five smartphones: the boot settles" "Boot settled: 5 agents (dan, eve, carol, bob, fay)" "$apps_gsnp_boot"
check_not "APPS gsnp on five smartphones: no event stopped at the cap" "did not quiesce" "$apps_gsnp_boot"
apps_same "APPS gsnp on five smartphones: dan's super-app lines" "[sg(connected(eve))]" "$(apps_person_sg dan "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: dan's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(dan)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [eve], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [befriend(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [sign(dan, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [message(dan, eve, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)])]" "$(apps_person_screen dan "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: eve's super-app lines" "[sg(befriend(dan)), sg(connected(dan)), sg(connected(carol))]" "$(apps_person_sg eve "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: eve's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(dan, eve), yes), view([identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [dan], [acquaint(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(dan, eve), yes), view([knows(dan), identity], [dan], [befriend(dan, eve)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(dan), knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [dan], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [befriend(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [forward(eve, carol, speech(utterance('Flood on Main Street'), dan))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), knows(dan), identity], [carol, dan], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)])]" "$(apps_person_screen eve "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: carol's super-app lines" "[sg(befriend(eve)), sg(connected(eve)), sg(connected(bob))]" "$(apps_person_sg carol "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: carol's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(eve, carol), yes), view([identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [eve], [acquaint(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(eve, carol), yes), view([knows(eve), identity], [eve], [befriend(eve, carol)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [eve], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [befriend(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [forward(carol, bob, speech(speech(utterance('Flood on Main Street'), dan), eve))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), knows(eve), identity], [bob, eve], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)])]" "$(apps_person_screen carol "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: bob's super-app lines" "[sg(befriend(carol)), sg(connected(carol)), sg(connected(fay))]" "$(apps_person_sg bob "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: bob's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(carol, bob), yes), view([identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [carol], [acquaint(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(carol, bob), yes), view([knows(carol), identity], [carol], [befriend(carol, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(carol), knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [befriend(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [forward(bob, fay, speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol))], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(fay), knows(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), knows(carol), identity], [carol, fay], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)])]" "$(apps_person_screen bob "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: fay's super-app lines" "[sg(befriend(bob)), sg(connected(bob))]" "$(apps_person_sg fay "$apps_gsnp_boot")"
apps_same "APPS gsnp on five smartphones: fay's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([], [], [establish(fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(acquaint(bob, fay), yes), view([identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([identity], [bob], [acquaint(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), gave(befriend(bob, fay), yes), view([knows(bob), identity], [bob], [befriend(bob, fay)], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)]), view([item(speech(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol), bob)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('Groups', member), tab('AI agents', guardian_of), tab('Items', item)])]" "$(apps_person_screen fay "$apps_gsnp_boot")"

# GAI's app on three smartphones (programs/apps/gai_boot.glp), alf, alice's AI agent, on a smartphone of its own: alice connects to alf, and once connected alf connects to bob and bob to alice; each pair befriends in the super-app's social graph; once connected to the next, each invites them to gai; and the steps of play_agents run over the three conversations, in the paper's order, to alice's retire of alf.  Each agent's super-app lines, sg(...), and their mini-app screen are each held exactly, in order; across the isolates alf's and bob's machines may write the persons they know a view later than in play_agents.
apps_gai_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gai $APPS_CORE
:boot $GLP_DIR/programs/apps/gai_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gai.glpw"
check "APPS gai on three smartphones: the mini-app's artefact is certified" "gai.glpw --- certified under" "$apps_gai_boot"
check "APPS gai on three smartphones: the boot settles" "Boot settled: 3 agents (alice, alf, bob)" "$apps_gai_boot"
check_not "APPS gai on three smartphones: no event stopped at the cap" "did not quiesce" "$apps_gai_boot"
apps_same "APPS gai on three smartphones: alice's super-app lines" "[sg(connected(alf)), sg(befriend(bob)), sg(connected(bob))]" "$(apps_person_sg alice "$apps_gai_boot")"
apps_same "APPS gai on three smartphones: alice's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity], [bob], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity], [bob], [sign(alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([principal_of(alf), guardian_of(alf), friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [retire(alf, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([retired_of(alf), principal_of(alf), guardian_of(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), item(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), authored(speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(bob), knows(bob), identity], [alf, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)])]" "$(apps_person_screen alice "$apps_gai_boot")"
apps_same "APPS gai on three smartphones: alf's super-app lines" "[sg(befriend(alice)), sg(connected(alice)), sg(connected(bob))]" "$(apps_person_sg alf "$apps_gai_boot")"
apps_same "APPS gai on three smartphones: alf's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [alice], [enter(alice, alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(bob), knows(bob), identity, agent, principal(alice), friend(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([retired, knows(bob), identity, agent, principal(alice), knows(alice), attestation(speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [alice, bob], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)])]" "$(apps_person_screen alf "$apps_gai_boot")"
apps_same "APPS gai on three smartphones: bob's super-app lines" "[sg(befriend(alf)), sg(connected(alf)), sg(connected(alice))]" "$(apps_person_sg bob "$apps_gai_boot")"
apps_same "APPS gai on three smartphones: bob's screen" "[view([], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(acquaint(alice, bob), yes), view([identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(befriend(alice, bob), yes), view([knows(alice), identity], [alice], [befriend(alice, bob)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), yes), view([friend(alice), knows(alice), identity], [alice], [agent_acquaint(alf, bob, alice, speech(utterance('Alf runs code signed by Acme Ltd'), alice))], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), gave(agent_befriend(alf, bob, alice), yes), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alice], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [agent_befriend(alf, bob, alice)], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([friend(alf), knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)]), view([knows(alf), agent_of(alf, speech(utterance('Alf runs code signed by Acme Ltd'), alice)), friend(alice), knows(alice), identity], [alf, alice], [], [tab('Contacts', knows), tab('Friends', friend), tab('AI agents', guardian_of)])]" "$(apps_person_screen bob "$apps_gai_boot")"

# GC's app on two smartphones (programs/apps/gc_boot.glp), as GSG's: the super-app of programs/social/graph/core on two isolates, alice and bob befriending in its social graph, over which it carries a mini-app's messages, and the steps of play_coins over their conversation.
apps_gc_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gc $APPS_CORE
:boot $GLP_DIR/programs/apps/gc_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gc.glpw"
check "APPS gc on two smartphones: the mini-app's artefact is certified" "gc.glpw --- certified under" "$apps_gc_boot"
check "APPS gc on two smartphones: the boot settles" "Boot settled: 2 agents (alice, bob)" "$apps_gc_boot"
check_not "APPS gc on two smartphones: no event stopped at the cap" "did not quiesce" "$apps_gc_boot"
apps_same "APPS gc on two smartphones: alice's lines" "[sg(connected(bob)), view(alice, [], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [knows(bob), identity], [bob], [mint(alice, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(alice), coin(alice), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(alice), coin(alice), knows(bob), identity], [bob], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, [bob]), view(alice, [coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [pay(alice, bob, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(alice, [coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)])]" "$(apps_person alice "$apps_gc_boot")"
apps_same "APPS gc on two smartphones: bob's lines" "[sg(befriend(alice)), sg(connected(alice)), view(bob, [], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [identity], [], [mint(bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(bob), identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(acquaint(alice, bob), yes), view(bob, [coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(bob), identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(swap(alice, bob, alice, 2, bob, 3), yes), view(bob, [knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view(bob, [coin(bob), coin(bob), coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)])]" "$(apps_person bob "$apps_gc_boot")"

# GC's village market on six smartphones (programs/apps/gc_village_boot.glp): the super-app of programs/social/graph/core on six isolates, the villagers of play_village under its roles, the earlier of each two who transact connecting to the later and inviting them once connected, the later having consented in advance, eight conversations; the six operations of GC's paper's village market run over them.  The isolates order a villager's views against their answers and against each other's attempts as the conversations open and the attempts arrive, which varies from run to run for a villager with three or four peers; so what is held of each villager is what the script and the end state fix: their answers and looks, gave(...) and menu(...), in order, the number of lines of their screen, and their last view, each play_village's; and the super-app's lines, one connection each, as a set.
apps_gc_village_boot=$("$REPL_RUN" <<HEREDOC
:artefact $GLP_DIR/programs/apps/gc $APPS_CORE
:boot $GLP_DIR/programs/apps/gc_village_boot.glp $APPS_CORE
:quit
HEREDOC
2>&1)
rm -f "$APPS_CORE/gc.glpw"
check "APPS gc village on six smartphones: the mini-app's artefact is certified" "gc.glpw --- certified under" "$apps_gc_village_boot"
check "APPS gc village on six smartphones: the boot settles" "Boot settled: 6 agents (alice, bob, charlie, diana, eve, frank)" "$apps_gc_village_boot"
check_not "APPS gc village on six smartphones: no event stopped at the cap" "did not quiesce" "$apps_gc_village_boot"
apps_same "APPS gc village on six smartphones: alice's super-app lines, one connection each" "[sg(connected(bob)), sg(connected(charlie)), sg(connected(eve)), sg(connected(frank))]" "$(apps_person_sg_sorted alice "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: alice's answers and looks, in order" "[menu(alice, [bob, charlie, eve, frank])]" "$(apps_person_acts alice "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: alice's screen has play_village's 26 lines" "26" "$(apps_person_screen alice "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: alice's last view is play_village's" "view(alice, [coin(frank), coin(frank), coin(frank), coin(frank), knows(frank), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), knows(charlie), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), knows(bob), knows(frank), knows(eve), knows(charlie), knows(bob), identity], [bob, charlie, eve, frank], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view alice "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: bob's super-app lines, one connection each" "[sg(connected(alice)), sg(connected(diana))]" "$(apps_person_sg_sorted bob "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: bob's answers and looks, in order" "[gave(acquaint(alice, bob), yes), gave(acquaint(diana, bob), yes), gave(swap(alice, bob, alice, 15, bob, 15), yes), gave(swap(diana, bob, diana, 20, bob, 24), yes), menu(bob, [alice, diana])]" "$(apps_person_acts bob "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: bob's screen has play_village's 22 lines" "22" "$(apps_person_screen bob "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: bob's last view is play_village's" "view(bob, [coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), knows(alice), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), knows(diana), knows(diana), knows(alice), identity], [alice, diana], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view bob "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: charlie's super-app lines, one connection each" "[sg(connected(alice)), sg(connected(eve))]" "$(apps_person_sg_sorted charlie "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: charlie's answers and looks, in order" "[gave(acquaint(alice, charlie), yes), gave(swap(alice, charlie, alice, 15, charlie, 15), yes), gave(swap(eve, charlie, frank, 5, alice, 5), yes), menu(charlie, [alice, eve])]" "$(apps_person_acts charlie "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: charlie's screen has play_village's 22 lines" "22" "$(apps_person_screen charlie "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: charlie's last view is play_village's" "view(charlie, [coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), knows(frank), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(charlie), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), knows(eve), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), coin(alice), knows(alice), knows(eve), knows(alice), identity], [alice, eve], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view charlie "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: diana's super-app lines, one connection each" "[sg(connected(bob)), sg(connected(frank))]" "$(apps_person_sg_sorted diana "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: diana's answers and looks, in order" "[menu(diana, [bob, frank])]" "$(apps_person_acts diana "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: diana's screen has play_village's 22 lines" "22" "$(apps_person_screen diana "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: diana's last view is play_village's" "view(diana, [coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), coin(frank), knows(frank), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), knows(bob), knows(frank), knows(bob), identity], [bob, frank], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view diana "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: eve's super-app lines, one connection each" "[sg(connected(alice)), sg(connected(charlie)), sg(connected(frank))]" "$(apps_person_sg_sorted eve "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: eve's answers and looks, in order" "[gave(acquaint(alice, eve), yes), gave(acquaint(charlie, eve), yes), gave(swap(charlie, eve, charlie, 10, eve, 10), yes), gave(swap(alice, eve, bob, 10, frank, 4), yes), menu(eve, [alice, charlie, frank])]" "$(apps_person_acts eve "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: eve's screen has play_village's 31 lines" "31" "$(apps_person_screen eve "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: eve's last view is play_village's" "view(eve, [coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), coin(bob), knows(bob), coin(alice), coin(alice), knows(alice), coin(frank), knows(frank), coin(charlie), coin(charlie), coin(charlie), coin(charlie), knows(charlie), knows(frank), knows(charlie), knows(alice), identity], [alice, charlie, frank], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view eve "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: frank's super-app lines, one connection each" "[sg(connected(alice)), sg(connected(diana)), sg(connected(eve))]" "$(apps_person_sg_sorted frank "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: frank's answers and looks, in order" "[gave(acquaint(diana, frank), yes), gave(acquaint(eve, frank), yes), gave(acquaint(alice, frank), yes), gave(swap(diana, frank, diana, 15, frank, 18), yes), gave(swap(eve, frank, eve, 10, frank, 10), yes), menu(frank, [alice, diana, eve])]" "$(apps_person_acts frank "$apps_gc_village_boot")"
apps_same "APPS gc village on six smartphones: frank's screen has play_village's 36 lines" "36" "$(apps_person_screen frank "$apps_gc_village_boot" | apps_count)"
apps_same "APPS gc village on six smartphones: frank's last view is play_village's" "view(frank, [coin(frank), knows(frank), coin(frank), knows(frank), coin(frank), knows(frank), coin(frank), knows(frank), coin(frank), knows(frank), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), coin(eve), knows(eve), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), coin(diana), knows(diana), knows(alice), knows(eve), knows(diana), identity], [alice, diana, eve], [], [tab('Contacts', knows), tab('Coins', coin, counted)])" "$(apps_person_last_view frank "$apps_gc_village_boot")"

# The village market of GB's paper, Section 7.1, on GB's app (play_village): six villagers and the escrow agent, ten conversations, alice's with frank among them (GKK:2), the seven operations at the paper's amounts; diana's premature presentation of a frank-bond is refused by frank's machine, the bond not mature by his date; once frank's date is day 28 he redeems five diana-coins, the escrow agent releases charlie's deposit to him (GB:5, release_c, one schema with deposit_c: a request of either proposes the part, vGLP, Section 5.4), and diana redeems a matured frank-bond against a frank-coin he holds (GB:4).  Each villager's last view held: Frank 7 diana-coins, 10 eve-coins, 6 frank-bonds maturing at day 28 and 4 frank-coins.
apps_gbv=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/apps/gb
:limit 400000000
play_village(A, B, C, D, E, F, G).
:quit
HEREDOC
2>&1)
check_not "APPS gb village: no failed play" "→ failed" "$apps_gbv"
check_not "APPS gb village: no error" "Error" "$apps_gbv"
check "APPS gb village: the run suspends, every operation taken" "→ suspended" "$apps_gbv"
apps_gbv_last() { # apps_gbv_last <variable>: the last view of that villager, its state as a sorted multiset
    echo "$apps_gbv" | sed -e 's/^\(GLP> \)*//' | grep "^$1 = " | grep -o 'view([a-z_]*, \[[^]]*\]' | tail -1 | sed -e 's/^view([a-z_]*, \[//' -e 's/\]$//' | tr -d ' ' | perl -ne 'chomp; my $d = 0; my $c = ""; for my $ch (split //) { if ($ch eq "," && $d == 0) { print "$c\n"; $c = ""; next } $d++ if $ch eq "("; $d-- if $ch eq ")"; $c .= $ch } print "$c\n" if $c ne ""' | LC_ALL=C sort | uniq -c | tr -s ' ' | tr '\n' ';'
}
check "APPS gb village: diana's premature presentation of a frank-bond is refused by frank, unable" "refused(redeem_bc(diana, frank, 28, frank), frank, unable(" "$apps_gbv"
check "APPS gb village: the escrow agent releases the deposit to frank, who answers yes (GB:5)" "gave(release_c(escrow, frank, frank, 5), yes)" "$apps_gbv"
apps_same "APPS gb village: frank's holdings, 7 diana-coins, 10 eve-coins, 6 frank-bonds maturing at day 28, 4 frank-coins; knows of the issuer added at the role that receives coins or bonds in each swap, redemption and release (GB.glp 7080bc69, GC:2, GB:3, GC:4, GB:4, GB:5: \"each comes to know the issuer of what they receive\", \"p comes to know r\", \"q comes to know u\"): alice by meeting, whom she comes to know before she asks eve for frank-coins (GKK:2), diana and eve by meeting and by a swap, and himself by five redemptions against frank-coins and the release of charlie's deposit of frank-coins" " 6 bond(frank,28); 7 coin(diana); 10 coin(eve); 4 coin(frank); 1 date(28); 1 dated; 1 identity; 1 knows(alice); 2 knows(diana); 1 knows(escrow); 2 knows(eve); 6 knows(frank);" "$(apps_gbv_last F)"
apps_same "APPS gb village: the escrow agent holds nothing once it has released, knowing frank by meeting and by charlie's deposit of frank-coins (GB.glp 7080bc69, GB:5: \"e comes to know u\")" " 1 date(30); 1 dated; 1 identity; 1 knows(charlie); 2 knows(frank);" "$(apps_gbv_last G)"
apps_same "APPS gb village: diana's holdings, knowing bob by meeting and by the bob-bonds of her swap, and frank by meeting, by the frank-bonds of her swap and by the frank-coin her redemption takes (GB.glp 7080bc69, GB:3, GB:4)" " 24 bond(bob,25); 12 bond(frank,28); 8 coin(diana); 1 coin(frank); 1 date(1); 1 dated; 1 identity; 2 knows(bob); 3 knows(frank);" "$(apps_gbv_last D)"

# The village market of GC's paper, Appendix "Village Market Demonstration", on GC's app (play_village): the six villagers, eight conversations, alice's with frank among them (GKK:2), the six operations at the paper's amounts, each villager's coins minted once; each villager's last view holds the holdings of the paper's Table 1 (Currencies #11 Cowork, Code_inbox.md, 2026-10-21 07:25 UTC, item 6).
apps_gcv=$("$REPL_RUN" <<HEREDOC
$GLP_DIR/programs/apps/gc
:limit 400000000
play_village(A, B, C, D, E, F).
:quit
HEREDOC
2>&1)
check_not "APPS gc village: no failed play" "→ failed" "$apps_gcv"
check_not "APPS gc village: no error" "Error" "$apps_gcv"
check "APPS gc village: the run suspends, every operation taken" "→ suspended" "$apps_gcv"
apps_gcv_last() { # apps_gcv_last <variable>: the last view of that villager, its coins as a sorted multiset
    echo "$apps_gcv" | sed -e 's/^\(GLP> \)*//' | grep "^$1 = " | grep -o 'view([a-z_]*, \[[^]]*\]' | tail -1 | sed -e 's/^view([a-z_]*, \[//' -e 's/\]$//' | tr -d ' ' | tr ',' '\n' | grep '^coin(' | LC_ALL=C sort | uniq -c | tr -s ' ' | tr '\n' ';'
}
apps_same "APPS gc village: alice's holdings, 5 bob-coins, 8 alice-coins, 15 charlie-coins, 4 frank-coins" " 8 coin(alice); 5 coin(bob); 15 coin(charlie); 4 coin(frank);" "$(apps_gcv_last A)"
apps_same "APPS gc village: bob's holdings, 10 alice-coins, 20 diana-coins" " 10 coin(alice); 20 coin(diana);" "$(apps_gcv_last B)"
apps_same "APPS gc village: charlie's holdings, 10 alice-coins, 10 eve-coins, 6 charlie-coins, 5 frank-coins" " 10 coin(alice); 6 coin(charlie); 10 coin(eve); 5 coin(frank);" "$(apps_gcv_last C)"
apps_same "APPS gc village: diana's holdings, 24 bob-coins, 13 frank-coins, 8 diana-coins" " 24 coin(bob); 8 coin(diana); 13 coin(frank);" "$(apps_gcv_last D)"
apps_same "APPS gc village: eve's holdings, 4 charlie-coins, 1 frank-coin, 2 alice-coins, 10 bob-coins" " 2 coin(alice); 10 coin(bob); 4 coin(charlie); 1 coin(frank);" "$(apps_gcv_last E)"
apps_same "APPS gc village: frank's holdings, 7 diana-coins, 10 eve-coins, 5 frank-coins" " 7 coin(diana); 10 coin(eve); 5 coin(frank);" "$(apps_gcv_last F)"

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
# compiles the schemas of one that is, against /Grassroots/GSC; GSC's
# contracts are read from their files (programs/gsc/GKK.glp, GSG.glp and
# GSN.glp), one copy each, and the fixtures that change one are
# programs/gsc/contracts.glp's variants of it.  Every other owner's contract
# is checked and recorded from its own .glp by cascade.sh (Section CAS; GSC
# #8, 2026-10-25 18:26 UTC).  Its checks
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
    # Each of the script's checks is reported under its own name, "GSC: <name>",
    # through _check_pass and _check_fail, so that a check another project must
    # fix can stand in KNOWN_RED_CHECKS with its owner (Code #11, 2026-10-19).
    # A failing check's detail lines are printed after it unless it is known red.
    GSC_SEEN_P=0; GSC_SEEN_F=0; GSC_SHOW=no
    while IFS= read -r gsc_line; do
        case "$gsc_line" in
            "  PASS  "*) GSC_SHOW=no; GSC_SEEN_P=$((GSC_SEEN_P + 1)); _check_pass "GSC: ${gsc_line#  PASS  }" ;;
            "  FAIL  "*) GSC_SEEN_F=$((GSC_SEEN_F + 1))
                         if _known_red_check "GSC: ${gsc_line#  FAIL  }"; then GSC_SHOW=no; else GSC_SHOW=yes; fi
                         _check_fail "GSC: ${gsc_line#  FAIL  }" "programs/gsc/test_gsc.sh" ;;
            "        "*) [ "$GSC_SHOW" = yes ] && printf '  %s\n' "$gsc_line" ;;
            *) GSC_SHOW=no ;;
        esac
    done < <(printf '%s\n' "$GSC_RESULT")
    # The lines counted must be the script's own totals, or a check went uncounted.
    if [ "$GSC_SEEN_P" -ne "$GSC_PASSED" ] || [ "$GSC_SEEN_F" -ne "$GSC_FAILED" ]; then
        echo "  FAIL: programs/gsc/test_gsc.sh reports $GSC_PASSED passed and $GSC_FAILED failed, but $GSC_SEEN_P PASS and $GSC_SEEN_F FAIL lines were read"
        FAIL=$((FAIL + 1))
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
# files, NAME.txt (NAME.md until its owner converts it) and NAME.glp, the
# schemas in GSC's notation printed from the term (schemas.txt), the checker's
# verdict, the VMAT form, the canonical platform as .vglp and as .glp, the
# platform's interface in sGLP's term form, and its run with vGLP's scripted
# persons, with a manifest.
# This section re-runs cascade.sh --check on every recorded contract and holds
# the committed artefacts to what the tools print now; a contract with no
# record is not run.
echo "=== Section CAS: The cascade, recorded whole ==="
echo ""

for cas_c in gkk gsg gsn gsnp gc gb cssn gfwc gai agora gdo gdc dgc gd gdo_l gra gff dao ldao fdao rec stay lib joint; do
    cas_n=$(printf '%s' "$cas_c" | tr a-z_ A-Z-)
    cas_o="cascade/$cas_n"
    [ -f "$GLP_DIR/$cas_o/manifest.md" ] || continue
    cas_out=$(bash "$GLP_DIR/cascade.sh" "$cas_c" --check 2>&1; echo "exit $?")
    for cas_f in "$cas_n.txt" "$cas_n.md" "$cas_n.glp" schemas.txt verdict.txt vmat.tex platform.vglp platform.glp interface.glp run.log; do
        [ -f "$GLP_DIR/$cas_o/$cas_f" ] || continue
        check "cascade $cas_c: $cas_f is what the tools print" "^$cas_f: as printed$" "$cas_out"
    done
    check "cascade $cas_c: cascade.sh --check exits 0" "^exit 0$" "$cas_out"
done
# A composition of contracts, recorded in cascade/<NAME>+<NAME>/ (vGLP 7c631b1, Section 5.4, "The contract's words": "The compiler takes one contract or several: of several it prints the platform of their composition"; cascade.sh, several contracts): GSN and GC, both extending GKK.
for cas_cs in "gsn gc"; do
    cas_n=$(printf '%s' "$cas_cs" | tr a-z_ A-Z- | tr ' ' +)
    cas_o="cascade/$cas_n"
    [ -f "$GLP_DIR/$cas_o/manifest.md" ] || continue
    cas_out=$(bash "$GLP_DIR/cascade.sh" $cas_cs --check 2>&1; echo "exit $?")
    for cas_f in $(cd "$GLP_DIR/$cas_o" && ls | grep -vx manifest.md); do
        check "cascade $cas_n: $cas_f is what the tools print" "^$cas_f: as printed$" "$cas_out"
    done
    check "cascade $cas_n: cascade.sh --check exits 0" "^exit 0$" "$cas_out"
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

# A simulation in a directory of its own (Integration's ruling of 2026-10-16
# 10:52 UTC, item 1; sGLP's answers of 11:25 and 12:32 UTC), under programs/sglp
# since 2026-11-08 (sGLP #11, Code_inbox 2026-11-07 21:21 UTC, Udi approving):
# programs/sglp/tests/outside is transformed by transform.sh --source, its
# printed modules as printed, and it loads and runs with the monitor and
# sglp.glp that programs/sglp/self.glp exposes (TGLP modules.tex, "The -expose
# directive").
sglp_out_tx=$(bash "$GLP_DIR/programs/sglp/transform.sh" --source "$GLP_DIR/programs/sglp/tests/outside" --check 2>&1; echo "exit $?")
check "sglp outside: transform.sh --source --check, profiles.glp as printed" "^profiles.glp: as printed$" "$sglp_out_tx"
check "sglp outside: transform.sh --source --check, population.glp as printed" "^population.glp: as printed$" "$sglp_out_tx"
check "sglp outside: transform.sh --source --check exits 0" "^exit 0$" "$sglp_out_tx"
sglp_out_run=$("$REPL_RUN" <<HEREDOC
:limit 1000000000000
$GLP_DIR/programs/sglp/tests/outside
poll_run(5, As).
:quit
HEREDOC
2>&1)
check "sglp outside: the program loads" "Loaded program" "$sglp_out_run"
check "sglp outside: the person answers each poll, logged with the monitor's clock" "entry(18909.90568894495, 1, Poll, poll(2, yes))" "$sglp_out_run"
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

# The run's own directory for the raw outputs and json reports of Sections Q
# and NET (Code #22, 2026-11-03, IGLP-rawpaths): under tmp/, named by the
# branch of the worktree the suite runs in and the PID of this run, printed at
# each section's head.  Until 2026-11-03 every run wrote the same files in
# tmp/, so two runs at once in two worktrees each wrote the other's lines into
# its file, and the json reader of one could count the other's tests.
SUITE_RUN_DIR="/Users/udi/Grassroots/tmp/suite-$(git -C "$GLP_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null | tr '/' '-')-$$"

# The watchdog (Code #22's authorisation, 2026-11-03; IGLP-coop), Section Q's
# and Section NET's.  A test's verdict does not depend on how loaded the
# machine is: each runner runs with
# --ignore-timeouts, so no test has a clock of its own, and the tests that run
# agents wait on events alone (IsolateManager.boot and settle,
# glp_multiagent/test/isolate_agents.dart).  Until 2026-11-03 every test had a
# clock, its own or the runner's thirty seconds, and under load tests failed
# that were merely slow: in the full suite on main 8ff18057, the night of
# 2026-11-02, mad_w_clean_test.dart and the GC village test of
# apps_screens_test.dart failed and each passed alone.  What a clock caught was
# a run that never ends; the watchdog catches it instead.  A runner still
# running after GLP_Q_BOUND seconds (three hours unless set) is killed, with
# every process it started, and each test it was in when killed is reported
# "did not finish", named by its file and its name as the json reporter
# recorded it (testStart with no testDone), and counted as a failure.
Q_BOUND="${GLP_Q_BOUND:-10800}"

# _q_names_in_progress <json> <prefix>: the tests the json reporter's file
# records as started and not done, one "<file>: <test name>" per line, the
# file as KNOWN_RED carries it, relative to its package root with <prefix>.
# _q_load_failures <json> <prefix>: the test files that did not load, which the
# reporter records as its hidden test "loading <file>" done with an error, and
# which the text reporter prints with no "<file>: " before its name, one
# "<file>: did not load" per line.
_q_names_in_progress() { _q_json "$1" "$2" progress; }
_q_load_failures() { _q_json "$1" "$2" load; }
_q_json() {
    python3 - "$1" "$2" "$3" <<'PYEOF'
import json, sys
path, prefix, mode = sys.argv[1], sys.argv[2], sys.argv[3]
suites, tests, done, failed = {}, {}, set(), set()
try:
    lines = open(path).read().splitlines()
except OSError:
    lines = []
for line in lines:
    try:
        e = json.loads(line)
    except ValueError:
        continue
    t = e.get('type')
    if t == 'suite':
        suites[e['suite']['id']] = e['suite'].get('path') or '?'
    elif t == 'testStart':
        tests[e['test']['id']] = (e['test']['suiteID'], e['test']['name'])
    elif t == 'testDone':
        done.add(e['testID'])
        if e.get('result') in ('error', 'failure'):
            failed.add(e['testID'])
for tid, (sid, name) in tests.items():
    if mode == 'progress' and tid in done:
        continue
    if mode == 'load' and not (tid in failed and name.startswith('loading ')):
        continue
    file = suites.get(sid, '?')
    i = file.find('/test/')
    if file.startswith('/') and i >= 0:
        file = file[i + 1:]
    print(prefix + file + ': ' + (name if mode == 'progress' else 'did not load'))
PYEOF
}

# _q_run <dir> <json> <out> <prefix> <runner...>: run the runner in <dir>, its
# output in <out>, with no test clock and with the json reporter writing
# <json>, as a process group of its own, killed whole once it outlives Q_BOUND.
# Sets Q_RUN_STATUS, its exit status; Q_RUN_DNF, 1 where it was killed; and
# Q_RUN_IN, the tests it was in, read before the kill, since a runner told to
# stop records each test it is in as done, flutter test's as an error "did not
# complete".
_q_run() {
    local dir="$1" json="$2" out="$3" prefix="$4"
    shift 4
    rm -f "$json"
    ( cd "$dir" && exec perl -e 'setpgrp(0, 0); exec @ARGV or die "exec: $!\n"' \
        "$@" --ignore-timeouts --file-reporter "json:$json" ) > "$out" 2>&1 &
    local pid=$! waited=0
    Q_RUN_DNF=0
    Q_RUN_IN=""
    while kill -0 "$pid" 2>/dev/null; do
        if [ "$waited" -ge "$Q_BOUND" ]; then
            Q_RUN_DNF=1
            Q_RUN_IN=$(_q_names_in_progress "$json" "$prefix") || true
            kill -TERM -- "-$pid" 2>/dev/null || true
            sleep 10
            kill -KILL -- "-$pid" 2>/dev/null || true
            break
        fi
        sleep 5
        waited=$((waited + 5))
    done
    wait "$pid" 2>/dev/null && Q_RUN_STATUS=0 || Q_RUN_STATUS=$?
}

# _q_report_dnf <runner>: the runner _q_run last ran, killed by the watchdog,
# each test it was in reported "did not finish"; Q_DNF_COUNT, their number,
# is the caller's to count as failures.
_q_report_dnf() {
    local names="$Q_RUN_IN"
    Q_DNF_COUNT=0
    if [ -z "$names" ]; then
        echo "  FAIL: $1 did not finish within $Q_BOUND s, in no test the json reporter recorded as started"
        Q_DNF_COUNT=1
        return
    fi
    while IFS= read -r t; do
        [ -z "$t" ] && continue
        echo "  FAIL: did not finish within $Q_BOUND s: $t"
        Q_DNF_COUNT=$((Q_DNF_COUNT + 1))
    done <<< "$names"
}

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
# requests.  The deployed sources parse: five since 2026-10-20, when CSSN's
# hand-written sources gave way to its compiled platform and IGLP's tests took
# their old tree as the fixture programs/tests/linker_tree (GLP #9, IGLP-fixture).
#
# The sovereign screen's entry, red from 2026-09-24, went on 2026-11-03, when
# sovereign_screen_test was rewritten on the bridge model as coins_screen_test
# is, its agent started by agent/2 on the person's first act (GLP #11,
# Code_inbox 2026-11-03 18:03 UTC, item 2).  The list is empty.
KNOWN_RED=()

echo "=== Section Q: Dart unit tests (whole tree) ==="
echo "  (raw output and json reports in $SUITE_RUN_DIR: q_dart.txt, q_dart.json, q_flutter.txt, q_flutter.json)"
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
# Each runner's raw output and json report are kept in the run's directory,
# SUITE_RUN_DIR, so that a failure can be read in full after the suite ends:
# the section itself prints only the failing names and, when its gate fails,
# the last twenty lines of dart test's output and none of flutter test's
# (Integration, 2026-10-11).  The json report is what the watchdog and the
# names of files that did not load are read from.
Q_RAW_DIR="$SUITE_RUN_DIR"
Q_RAW_DART="$Q_RAW_DIR/q_dart.txt"
Q_RAW_FLUTTER="$Q_RAW_DIR/q_flutter.txt"
mkdir -p "$Q_RAW_DIR" 2>/dev/null || true

Q_DNF_DART=0
Q_DNF_FLUTTER=0
if _section QD; then
_q_run "$GLP_RUNTIME" "$Q_RAW_DIR/q_dart.json" "$Q_RAW_DART" "" "$DART" test
DART_TREE_STATUS=$Q_RUN_STATUS
Q_DNF_DART=$Q_RUN_DNF
DART_TREE_RESULT=$(cat "$Q_RAW_DART" 2>/dev/null) || DART_TREE_RESULT=""
if [ "$Q_DNF_DART" = 1 ]; then
    _q_report_dnf "dart test"
    FAIL=$((FAIL + Q_DNF_COUNT))
fi
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
# A test the watchdog stopped is reported once, above, as one that did not
# finish, and not again as the runner's error "did not complete".
DART_TREE_FAILS=$(printf '%s' "$DART_TREE_CLEAN" \
    | grep -oE 'test/[A-Za-z0-9_/]+\.dart: .*\[E\]' \
    | sed 's/ \[E\]$//' \
    | { if [ "$Q_DNF_DART" = 1 ]; then grep -v ' - did not complete$'; else cat; fi; } \
    | sort -u) || true
# A file that did not load runs none of its tests, and its runner prints no
# "<file>: <test>" for it: it is named from the json report, as a failure of
# its file (Code #22, 2026-11-03: given_network_test.dart did not load on main
# 51e227c9, and the section said only that dart test named no failing test).
if _section QD; then
    Q_LOAD_DART=$(_q_load_failures "$Q_RAW_DIR/q_dart.json" "") || true
    if [ -n "$Q_LOAD_DART" ]; then
        DART_TREE_FAILS=$(printf '%s\n%s' "$DART_TREE_FAILS" "$Q_LOAD_DART" | sed '/^$/d' | sort -u)
    fi
fi

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
    _q_run "$GLP_DIR/glp_multiagent" "$Q_RAW_DIR/q_flutter.json" "$Q_RAW_FLUTTER" "glp_multiagent/" flutter test
    MA_STATUS=$Q_RUN_STATUS
    Q_DNF_FLUTTER=$Q_RUN_DNF
    MA_RESULT=$(cat "$Q_RAW_FLUTTER" 2>/dev/null) || MA_RESULT=""
    if [ "$Q_DNF_FLUTTER" = 1 ]; then
        _q_report_dnf "flutter test"
        FAIL=$((FAIL + Q_DNF_COUNT))
    fi
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
        | { if [ "$Q_DNF_FLUTTER" = 1 ]; then grep -v ' - did not complete$'; else cat; fi; } \
        | sort -u) || true
    Q_LOAD_FLUTTER=$(_q_load_failures "$Q_RAW_DIR/q_flutter.json" "glp_multiagent/") || true
    if [ -n "$Q_LOAD_FLUTTER" ]; then
        MA_FAILS=$(printf '%s\n%s' "$MA_FAILS" "$Q_LOAD_FLUTTER" | sed '/^$/d' | sort -u)
    fi
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
if [ "${DART_TREE_STATUS:-0}" -ne 0 ] && [ -z "$DART_TREE_FAILS" ] && [ "$Q_DNF_DART" = 0 ]; then
    echo "  FAIL: dart test exited $DART_TREE_STATUS naming no failing test, so glp_runtime/test/ did not run"
    printf '%s\n' "$DART_TREE_CLEAN" | tail -5 | sed 's/^/        /'
    FAIL=$((FAIL + 1))
fi
if [ "${MA_STATUS:-0}" -ne 0 ] && [ -z "$MA_FAILS" ] && [ "$Q_DNF_FLUTTER" = 0 ]; then
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
# QF's, the rest QD's; nor is an entry of a half the watchdog killed, whose
# tests after the kill did not run.
Q_STALE=0
for known in "${KNOWN_RED[@]}"; do
    case "$known" in
        glp_multiagent/*) _section QF && [ "$Q_DNF_FLUTTER" = 0 ] || continue ;;
        *)                _section QD && [ "$Q_DNF_DART" = 0 ] || continue ;;
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

if _section NET; then
# =============================================================================
# Section NET: networking's own tests (GNA)
# =============================================================================
#
# networking/ is GNA's code, three packages each with its own tests, which the
# suite did not run until 2026-11-03 (Code #22): the Flutter package in
# networking/ by `flutter test`; the glue in networking/glue by `flutter test`,
# since it depends on the Flutter package and has flutter_test and no
# package:test, so `dart test` refuses it; and the pure-Dart core in
# networking/core by `dart test`.  Each runs from its own directory, as
# networking/README.md says.  With them, since 2026-11-08 (Cloud #8), the
# directory of GNA's Section 7, servers/directory, by `flutter test`, since
# it depends on the glue and so on Flutter, which the Dart on the Mac's path
# cannot resolve: its agent over the headless layer against a scripted app
# over the loopback, and its store and page.  The conventions are Section Q's: each runner's
# raw output and json report kept in the run's directory, SUITE_RUN_DIR
# (net_flutter, net_glue, net_core and net_directory, each .txt and .json),
# each failing test printed as a FAIL
# line "<package directory>/test/<file>: <test name>" and counted, the passed
# count added to the totals, a missing runner a hard failure, and a runner that
# exits non-zero naming no failing test a hard failure, since it did not run its
# tests.  The section has no known-red list: a red test fails it.
echo "=== Section NET: networking's own tests (GNA) ==="
echo ""

NET_RAW_DIR="$SUITE_RUN_DIR"
mkdir -p "$NET_RAW_DIR" 2>/dev/null || true
echo "  (raw output and json reports in $NET_RAW_DIR: net_flutter, net_glue, net_core, net_directory, each .txt and .json)"
NET_PASSED=0
NET_FAILED=0

# _net_run <name> <directory from the repository root> <runner>: runs the
# runner's tests in that directory and adds to NET_PASSED and NET_FAILED.
_net_run() {
    local name="$1" rel="$2" runner="$3"
    local dir="$GLP_DIR/$rel"
    local raw="$NET_RAW_DIR/net_$name.txt"
    local result status clean passed fails dnf
    if ! command -v "$runner" >/dev/null 2>&1; then
        echo "  FAIL: $runner is not on PATH, so $rel/test/ cannot run"
        echo "$runner is not on PATH, so $rel/test/ did not run" > "$raw" 2>/dev/null \
            || echo "  ($runner test's output could not be written to $raw)"
        NET_FAILED=$((NET_FAILED + 1))
        return
    fi
    # Under the watchdog, as Section Q's runners are: no test clock, and a run
    # not finished within Q_BOUND reported by the tests it was in.
    _q_run "$dir" "$NET_RAW_DIR/net_$name.json" "$raw" "$rel/" "$runner" test
    status=$Q_RUN_STATUS
    dnf=$Q_RUN_DNF
    result=$(cat "$raw" 2>/dev/null) || result=""
    if [ "$dnf" = 1 ]; then
        _q_report_dnf "$runner test in $rel"
        NET_FAILED=$((NET_FAILED + Q_DNF_COUNT))
    fi
    clean=$(printf '%s' "$result" | sed 's/\x1b\[[0-9;]*m//g' | tr '\r' '\n') || true
    passed=$(printf '%s' "$clean" | grep -oE '\+[0-9]+' | tail -1 | tr -d '+') || true
    passed=${passed:-0}
    # `flutter test` prints absolute paths and `dart test` package-relative
    # ones; both are normalised to the path from the repository root.
    fails=$(printf '%s' "$clean" \
        | grep -oE '[A-Za-z0-9_/.-]+\.dart: .*\[E\]' \
        | sed 's/ \[E\]$//' \
        | sed "s|^.*/$rel/test/|$rel/test/|; s|^test/|$rel/test/|" \
        | { if [ "$dnf" = 1 ]; then grep -v ' - did not complete$'; else cat; fi; } \
        | sort -u) || true
    local loads
    loads=$(_q_load_failures "$NET_RAW_DIR/net_$name.json" "$rel/") || true
    if [ -n "$loads" ]; then
        fails=$(printf '%s\n%s' "$fails" "$loads" | sed '/^$/d' | sort -u)
    fi
    if [ "$status" -ne 0 ] && [ -z "$fails" ] && [ "$dnf" = 0 ]; then
        echo "  FAIL: $runner test exited $status in $rel naming no failing test, so $rel/test/ did not run"
        printf '%s\n' "$clean" | tail -5 | sed 's/^/        /'
        NET_FAILED=$((NET_FAILED + 1))
    fi
    if [ -n "$fails" ]; then
        while IFS= read -r failure; do
            [ -z "$failure" ] && continue
            echo "  FAIL: $failure"
            NET_FAILED=$((NET_FAILED + 1))
        done <<< "$fails"
    fi
    echo "  $rel: $passed passed ($runner test; output in $raw)"
    NET_PASSED=$((NET_PASSED + passed))
}

_net_run flutter networking flutter
_net_run glue networking/glue flutter
_net_run core networking/core dart
_net_run directory servers/directory flutter

PASS=$((PASS + NET_PASSED))
FAIL=$((FAIL + NET_FAILED))
if [ $NET_FAILED -eq 0 ]; then
    echo "  PASS: All $NET_PASSED networking tests passed"
fi

echo ""

fi  # Section NET

if _section DL; then
# =============================================================================
# Section DL: the debugging log service of the phones (servers/debuglog)
# =============================================================================
#
# GLP/phones/docs/debugging.md, "The log service": `dart test` in
# servers/debuglog, as Section NET runs networking/core: under the watchdog,
# the raw output and json report in the run's directory (dl.txt and dl.json),
# each failing test a FAIL line "servers/debuglog/test/<file>: <test name>",
# a missing dart or a run that exits non-zero naming no failing test a hard
# failure.  No known-red list: a red test fails it.
echo "=== Section DL: the debugging log service (servers/debuglog) ==="
echo ""
DL_RAW_DIR="$SUITE_RUN_DIR"
mkdir -p "$DL_RAW_DIR" 2>/dev/null || true
DL_REL="servers/debuglog"
DL_RAW="$DL_RAW_DIR/dl.txt"
DL_JSON="$DL_RAW_DIR/dl.json"
echo "  (raw output and json report in $DL_RAW_DIR: dl.txt and dl.json)"
DL_PASSED=0
DL_FAILED=0
if ! command -v dart >/dev/null 2>&1; then
    echo "  FAIL: dart is not on PATH, so $DL_REL/test/ cannot run"
    DL_FAILED=1
else
    _q_run "$GLP_DIR/$DL_REL" "$DL_JSON" "$DL_RAW" "$DL_REL/" dart test
    dl_status=$Q_RUN_STATUS
    if [ "$Q_RUN_DNF" = 1 ]; then
        _q_report_dnf "dart test in $DL_REL"
        DL_FAILED=$((DL_FAILED + Q_DNF_COUNT))
    fi
    dl_clean=$(sed 's/\x1b\[[0-9;]*m//g' "$DL_RAW" 2>/dev/null | tr '\r' '\n') || dl_clean=""
    DL_PASSED=$(printf '%s' "$dl_clean" | grep -oE '\+[0-9]+' | tail -1 | tr -d '+') || true
    DL_PASSED=${DL_PASSED:-0}
    dl_fails=$(printf '%s' "$dl_clean" \
        | grep -oE '[A-Za-z0-9_/.-]+\.dart: .*\[E\]' \
        | sed 's/ \[E\]$//' \
        | sed "s|^.*/$DL_REL/test/|$DL_REL/test/|; s|^test/|$DL_REL/test/|" \
        | sort -u) || true
    dl_loads=$(_q_load_failures "$DL_JSON" "$DL_REL/") || true
    if [ -n "$dl_loads" ]; then
        dl_fails=$(printf '%s\n%s' "$dl_fails" "$dl_loads" | sed '/^$/d' | sort -u)
    fi
    if [ "$dl_status" -ne 0 ] && [ -z "$dl_fails" ] && [ "$Q_RUN_DNF" = 0 ]; then
        echo "  FAIL: dart test exited $dl_status in $DL_REL naming no failing test, so $DL_REL/test/ did not run"
        printf '%s\n' "$dl_clean" | tail -5 | sed 's/^/        /'
        DL_FAILED=$((DL_FAILED + 1))
    fi
    if [ -n "$dl_fails" ]; then
        while IFS= read -r failure; do
            [ -z "$failure" ] && continue
            echo "  FAIL: $failure"
            DL_FAILED=$((DL_FAILED + 1))
        done <<< "$dl_fails"
    fi
    echo "  $DL_REL: $DL_PASSED passed (dart test; output in $DL_RAW)"
fi
PASS=$((PASS + DL_PASSED))
FAIL=$((FAIL + DL_FAILED))
if [ $DL_FAILED -eq 0 ]; then
    echo "  PASS: All $DL_PASSED debugging log service tests passed"
fi

echo ""

fi  # Section DL

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
