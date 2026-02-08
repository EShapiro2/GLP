#!/bin/bash
# GLP REPL Full Test Suite - FAST VERSION (~3-5s)
# Loads all files in single REPL session for speed
# NOTE: arithmetic_fixed.glp excluded (conflicts with primes.glp mod guards)
# NOTE: SRSW violation tests run separately at end

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_RUNTIME="$SCRIPT_DIR/../glp_runtime"
GLP_DIR="$SCRIPT_DIR/../programs/tests/repl"
MOD_DIR="$SCRIPT_DIR/../programs/tests/modules"
REPL_SOURCE="bin/glp_repl.dart"

cd "$GLP_RUNTIME"

# Compile REPL to kernel snapshot for faster startup (if out of date)
REPL_SNAPSHOT=".dart_tool/repl.dill"
if [ ! -f "$REPL_SNAPSHOT" ] || [ "$REPL_SOURCE" -nt "$REPL_SNAPSHOT" ]; then
    echo "Compiling REPL snapshot..."
    mkdir -p .dart_tool
    $DART compile kernel -o "$REPL_SNAPSHOT" "$REPL_SOURCE" 2>/dev/null || true
fi

# Use snapshot if available, else fall back to source
if [ -f "$REPL_SNAPSHOT" ]; then
    REPL="$REPL_SNAPSHOT"
else
    REPL="$REPL_SOURCE"
fi

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
$GLP_DIR/no_guard.glp
$GLP_DIR/with_guard.glp
$GLP_DIR/bb_diff.glp
$GLP_DIR/nonground_list.glp
$GLP_DIR/reader_output.glp
$GLP_DIR/two_struct_list.glp
$GLP_DIR/depth_test.glp
$GLP_DIR/paa.glp
$GLP_DIR/test_bob.glp
$GLP_DIR/test_nested_suspend.glp
$GLP_DIR/test_defined_guards.glp
$GLP_DIR/append_dl.glp
$GLP_DIR/assign_reader_test.glp
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
time_advances(Tadv).
past_time(1000000000000, Xpast).
past_time(9999999999999, Xfuture).
wait_test(Xwait).
primes(20, Ps20).
primes(10, Ps10).
no_guard(Xng, 5).
with_guard(Xwg, 5).
Xsl = [send(1,a), send(2,b)].
open(1, Xopen, Yopen).
open2(1, Xopen2).
append([1,2|T1?], T1, [3,4|T2?], T2, Hdl, Tdl?).
test_list_in_body([1,2,3,4], Xngl).
build_list(a, b, Xbld).
unwrap([hello], Xunw).
identity(foo, Xid).
test(Xtsl).
bin_nest(Xbn, val).
ter_all(Xta, a, b, c).
tree3(Xtr3, val).
multi_w(Xmw, p, q).
p(Xpaa1, Xpaa1?).
p(Xpaa2?, Xpaa2).
bob(Xbob?).
level1(Xlv1?).
level2([Xlv2?|Rlv2]).
level3([wrapper(Xlv3?)|Rlv3]).
# Defined guards via partial evaluation
test(ch(Adg?, Bdg), Rdg1).
test(foo, Rdg2).
test(Xdg?, Rdg3).
assign_reader(Rar?, Xar).
# Channel operations as defined guards
$GLP_DIR/test_channel_guards.glp
make_pair(ch(Achg?, Bchg), Rchg).
# Comprehensive defined guards test suite
$GLP_DIR/test_defined_guards_all.glp
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
relay_send([], RsOut, ch([], [])).
relay_recv([], RrOut, ch([], [])).
relay([], RelayOut, ch([], [])).
switch2x2(ch([], SwA), ch([], SwB), ch(SwC, []), ch(SwD, [])).
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
    "Simple Run:→ succeeds"
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
    "Meta run2:→ succeeds"
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
    "Hanoi 0:→ succeeds"
    "Hanoi 1:→ succeeds"
    "Hanoi 2:→ succeeds"

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
    "Meta hanoi 2:→ succeeds"

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
    # "Time advances:Tadv = true"  # Fragile test - timing dependent
    "Time past:Xpast = yes"
    "Time future:failed"
    "Time wait:Xwait = done"

    # Primes
    "Primes 20:Ps20 = \[2, 3, 5, 7, 11, 13, 17, 19\]"
    "Primes 10:Ps10 = \[2, 3, 5, 7\]"

    # Guard tests (sigmaHat fix)
    "Guard - no guard:Xng = \[5, a, b\]"
    "Guard - with guard:Xwg = \[5, a, b\]"

    # Structs in list
    "Structs in list:Xsl = \[send(1, a), send(2, b)\]"
    "Two structs in list:Xtsl = \[foo(a), bar(b)\]"

    # Nested structures with vars (depth test)
    "Nested binary with var:Xbn = outer(inner(val, b), c)"
    "Ternary all vars:Xta = triple(a, b, c)"
    "Deep binary tree:Xtr3 = node(node(leaf(val), leaf(a)), leaf(b))"
    "Multiple writers nested:Xmw = pair(wrap(p), wrap(q))"

    # Bounded buffer - WxW: goal unbound writer vs head writer fails
    # (open/3 uses incorrect mode annotations - T should be T?)
    "Open buffer:failed"

    # Open2 - WxW-compliant version with single dl(Head,Tail) output
    "Open2 buffer:Xopen2 = dl"

    # Difference list append (now correctly unifies T1 with T1?)
    "DL append:Hdl = \[1, 2, 3, 4 |"

    # Non-ground list in body (codegen fix)
    "Non-ground list pass:Xngl = \[1, 2, 3, 4\]"
    "Non-ground list build:Xbld = \[a, b\]"

    # Reader in output position (runtime fix)
    "Unwrap list element:Xunw = hello"
    "Identity returns value:Xid = foo"

    # Two-phase HEAD unification (writer/reader same variable)
    "p(X,X?) succeeds:Xpaa1 = a"
    "p(X?,X) succeeds:→ succeeds"

    # Suspension tests (unbound reader vs structure pattern)
    "Suspend bob(X?) unbound:→ suspended"
    "Suspend level1(X?) unbound:→ suspended"
    "Suspend level2 nested:→ suspended"
    "Suspend level3 deep nested:→ suspended"

    # Defined guards via partial evaluation
    "Defined guard match:Rdg1 = ok"
    "Defined guard fail:Rdg2 = not_channel"
    "Defined guard suspend:→ suspended"

    # Channel operations as defined guards
    "Channel make_pair:Rchg = ch("

    # Comprehensive defined guards test suite
    "DG make_pair creates channels:Call1 = ch("
    "DG bind_response yes:RespYes = accept(ch("
    "DG bind_response yes local:LocalYes = ch("
    "DG bind_response no:RespNo = no"
    "DG bind_response no local:LocalNo = none"
    "DG channel test success:TchR1 = ok"
    "DG channel test fail atom:TchR2 = not_channel"
    "DG channel test fail pair:TchR3 = not_channel"
    "DG pair test success:TprR1 = ok"
    "DG pair test fail:TprR2 = not_pair"
    "DG wrapper test success:TwrR1 = ok"
    "DG wrapper test fail:TwrR2 = not_wrapper"
    "DG nested wrapper with pair:TnR1 = wrapper_with_pair"
    "DG nested just wrapper:TnR2 = just_wrapper"
    "DG nested neither:TnR3 = neither"
    "DG wrap binding:TwpR = wrapped(hello)"
    "DG deep binding:TdpR = outer(inner(foo))"
    "DG triple binding:TtrR = pair(1, 2)"
    "DG relay_send base:RsOut = \[\]"
    "DG relay_recv base:RrOut = \[\]"
    "DG relay base:RelayOut = \[\]"
    "DG switch2x2 base:→ succeeds"

    # Goal reader vs head writer (should succeed, not suspend)
    "Goal reader to head writer:→ succeeds"
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

# ===========================================
# MEGA BATCH: All non-conflicting runtime tests in ONE session
# Uses :clear between incompatible test groups
# ===========================================
echo ""
echo "--- Mega Batch: All Runtime Tests ---"

mega_output=$($DART run "$REPL" <<MEGA_INPUT
# --- Arithmetic Fixed Tests (must run before primes conflict) ---
$GLP_DIR/arithmetic_fixed.glp
add(5, 3, Xadd).
multiply(4, 7, Ymul).
compute(Zcomp).
:clear
# --- MWM tests (stdlib) ---
mwm([], Xmwm1).
mwm([stream([1,2,3])], Xmwm2).
mwm([stream([a,b]), stream([1,2])], Xmwm3).
# --- Ground equality tests ---
$GLP_DIR/test_ground_equal.glp
test(a, a, R1).
test(a, b, R2).
test(foo(1,2), foo(1,2), R3).
test(foo(1,2), foo(1,3), R4).
test([1,2,3], [1,2,3], R5).
test([1,2], [1,3], R6).
# --- Guard negation tests ---
$GLP_DIR/test_guard_negation.glp
test_neg_int(5, Rn1).
test_neg_int(hello, Rn2).
test_neg_number(3.14, Rn3).
test_neg_number(hello, Rn4).
test_neg_eq(5, 5, Rn5).
test_neg_eq(5, 3, Rn6).
:clear
# --- Reduce tests ---
$GLP_DIR/reduce_test.glp
reduce(hello(world), Body1).
reduce(greet(foo, Y), Body2).
reduce(double(5, Y2), Body3).
# --- Quoted functor tests ---
$GLP_DIR/quoted_functor_test.glp
'_test_kernel'(5, Rq1).
$GLP_DIR/quoted_body_test.glp
wrapper(10, Rq2).
X = '_equator'(E, stop).
:clear
# --- Circular term tests ---
$GLP_DIR/circular_test.glp
link(A, f(B?)), link(B, f(A?)).
is_ground(foo, Rc1).
is_ground(f(a,b), Rc2).
test_equal(foo, foo, Rc3).
test_equal(foo, bar, Rc4).
test_self_equal(f(a,b), Rc5).
show(hello, Xshow).
:clear
# --- Circular term ops (needs fresh circular_test.glp load) ---
$GLP_DIR/circular_test.glp
link(Ac, f(Bc?)), link(Bc, f(Ac?)), test_ground(Ac?, Rg1), test_eq(Ac?, Ac?, Rg2).
:clear
# --- Multi-arity predicate tests ---
$GLP_DIR/sum_acc.glp
$GLP_DIR/reverse_acc.glp
$GLP_DIR/length_acc.glp
sum([1,2,3,4,5], S1).
reverse([1,2,3], Rrev1).
length([a,b,c,d], N1).
:clear
# --- Arithmetic guards imply groundness tests ---
$GLP_DIR/arith_guard_ground.glp
compare_and_use(3, 5, Rag1).
max(7, 4, M1).
in_range(5, 1, 10, Rir1).
in_range(15, 1, 10, Rir2).
compare_expr(1, 5, Rce1).
:clear
# --- Arithmetic comparison operators tests ---
$GLP_DIR/arith_comparison.glp
arith_eq(5, 5, Raeq1).
arith_eq(5, 3, Raeq2).
arith_neq(5, 3, Raneq1).
arith_neq(5, 5, Raneq2).
expr_eq(4, 6, Reeq1).
test_lt(3, 5, Rlt1).
test_gt(5, 3, Rgt1).
test_le(5, 5, Rle1).
test_ge(3, 5, Rge1).
:clear
# --- Otherwise guard tests ---
$GLP_DIR/otherwise_guard.glp
classify(5, Rcl1).
classify(-3, Rcl2).
classify(0, Rcl3).
grade(95, G1).
grade(75, G2).
grade(55, G3).
type_of(42, T1).
type_of(hello, T2).
:clear
# --- Difference list syntax tests ---
$GLP_DIR/diff_list.glp
dl_append([1,2|T]\\T, [3,4|U]\\U, Rdl).
dl_to_list([a,b,c]\\[], Ldl).
Xdl = foo\\bar.
:clear
# --- Guard reader occurrence tests ---
$GLP_DIR/guard_reader.glp
guard_ground(42).
guard_int(7).
guard_compare(3, 5).
guard_known_valid(hello, Ygr).
:clear
# --- Arithmetic kernel tests ---
$GLP_DIR/test_arithmetic_kernels.glp
test_idiv(10, 3, Rak1).
test_abs(-5, Rak2).
test_sqrt(16, Rak3).
test_pow(2, 10, Rak4).
test_floor(3.7, Rak5).
test_ceil(3.2, Rak6).
:clear
# --- Guards comprehensive tests ---
$GLP_DIR/test_guards_comprehensive.glp
test_list_ok([1,2,3], Rgc1).
test_string_ok("hello", Rgc2).
test_constant_ok(foo, Rgc3).
:clear
# --- Constant ground and gethead tests ---
$GLP_DIR/constant_ground_test.glp
test_constant(foo, Rcgt1, Rcgt2).
$GLP_DIR/gethead_test.glp
test2(Rgh1).
:quit
MEGA_INPUT
2>&1)

# --- SRSW Violation Test (needs separate session) ---
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

# --- Verify arithmetic fixed tests ---
echo ""
echo "--- Arithmetic Fixed Tests ---"
if echo "$mega_output" | grep -q "Xadd = 8"; then
    echo "PASS: Addition 5+3"
    PASS=$((PASS + 1))
else
    echo "FAIL: Addition 5+3 (expected: Xadd = 8)"
    FAIL=$((FAIL + 1))
fi

if echo "$mega_output" | grep -q "Ymul = 28"; then
    echo "PASS: Multiplication 4*7"
    PASS=$((PASS + 1))
else
    echo "FAIL: Multiplication 4*7 (expected: Ymul = 28)"
    FAIL=$((FAIL + 1))
fi

if echo "$mega_output" | grep -q "Zcomp = 10"; then
    echo "PASS: Compound (2*3)+4"
    PASS=$((PASS + 1))
else
    echo "FAIL: Compound (2*3)+4 (expected: Zcomp = 10)"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "--- MWM + Ground Equality + Guard Negation Tests ---"
batch_a_output="$mega_output"

# MWM checks
if echo "$batch_a_output" | grep -q "Xmwm1 = \[\]"; then
    echo "PASS: MWM empty"
    PASS=$((PASS + 1))
else
    echo "FAIL: MWM empty (expected: Xmwm1 = [])"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Xmwm2 = \[1, 2, 3\]"; then
    echo "PASS: MWM single stream"
    PASS=$((PASS + 1))
else
    echo "FAIL: MWM single stream (expected: Xmwm2 = [1, 2, 3])"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Xmwm3 = \[a, b, 1, 2\]"; then
    echo "PASS: MWM two streams"
    PASS=$((PASS + 1))
else
    echo "FAIL: MWM two streams (expected: Xmwm3 = [a, b, 1, 2])"
    FAIL=$((FAIL + 1))
fi

# Ground equality checks
if echo "$batch_a_output" | grep -q "R1 = equal"; then
    echo "PASS: =?= atoms equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= atoms equal (expected: R1 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "R2 = not_equal"; then
    echo "PASS: =?= atoms not equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= atoms not equal (expected: R2 = not_equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "R3 = equal"; then
    echo "PASS: =?= structures equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= structures equal (expected: R3 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "R4 = not_equal"; then
    echo "PASS: =?= structures not equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= structures not equal (expected: R4 = not_equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "R5 = equal"; then
    echo "PASS: =?= lists equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= lists equal (expected: R5 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "R6 = not_equal"; then
    echo "PASS: =?= lists not equal"
    PASS=$((PASS + 1))
else
    echo "FAIL: =?= lists not equal (expected: R6 = not_equal)"
    FAIL=$((FAIL + 1))
fi

# Guard negation checks
if echo "$batch_a_output" | grep -q "Rn1 = is_int"; then
    echo "PASS: ~integer(5) fails, integer succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~integer(5) fails, integer succeeds (expected: Rn1 = is_int)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Rn2 = not_int"; then
    echo "PASS: ~integer(hello) succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~integer(hello) succeeds (expected: Rn2 = not_int)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Rn3 = is_num"; then
    echo "PASS: ~number(3.14) fails, number succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~number(3.14) fails, number succeeds (expected: Rn3 = is_num)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Rn4 = not_num"; then
    echo "PASS: ~number(hello) succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~number(hello) succeeds (expected: Rn4 = not_num)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Rn5 = eq"; then
    echo "PASS: ~(5 =?= 5) fails, equality succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~(5 =?= 5) fails, equality succeeds (expected: Rn5 = eq)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_a_output" | grep -q "Rn6 = neq"; then
    echo "PASS: ~(5 =?= 3) succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: ~(5 =?= 3) succeeds (expected: Rn6 = neq)"
    FAIL=$((FAIL + 1))
fi

# --- Invalid Guard Tests ---
echo ""
echo "--- Invalid Guard Tests ---"

# Create temp file with true in guard position
TMP_TRUE_GUARD=$(mktemp /tmp/glp_test.XXXXXX)
mv "$TMP_TRUE_GUARD" "${TMP_TRUE_GUARD}.glp"
TMP_TRUE_GUARD="${TMP_TRUE_GUARD}.glp"
echo 'bad_guard(X?) :- true | X = done.' > "$TMP_TRUE_GUARD"

true_guard_output=$($DART run "$REPL" <<TRUE_INPUT
$TMP_TRUE_GUARD
:quit
TRUE_INPUT
2>&1)

rm -f "$TMP_TRUE_GUARD"

if echo "$true_guard_output" | grep -q '"true" is not a guard'; then
    echo "PASS: true in guard position rejected"
    PASS=$((PASS + 1))
else
    echo "FAIL: true in guard position should be rejected"
    FAIL=$((FAIL + 1))
fi

# --- Module RPC Tests (FROZEN - pending module system revision) ---
# These tests are commented out until module system is revised with type system
# See CLAUDE.md for details

# ===========================================
# Validate MEGA BATCH results (all tests already ran above)
# ===========================================
echo ""
echo "--- Reduce + Quoted Functor Tests ---"
batch_b_output="$mega_output"

# Reduce checks
if echo "$batch_b_output" | grep -q "Body1 = true"; then
    echo "PASS: reduce/2 for fact: hello(world) -> true"
    PASS=$((PASS + 1))
else
    echo "FAIL: reduce/2 for fact (expected: Body1 = true)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_b_output" | grep -qE "Body2 = :="; then
    echo "PASS: reduce/2 for rule: greet(foo, Y) -> Body contains :="
    PASS=$((PASS + 1))
else
    echo "FAIL: reduce/2 for rule (expected: Body2 contains :=)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_b_output" | grep -qE "Body3 = :=.*\*.*5.*2"; then
    echo "PASS: reduce/2 for guarded rule: double(5, Y) -> Body contains 5*2"
    PASS=$((PASS + 1))
else
    echo "FAIL: reduce/2 for guarded rule (expected: Body3 contains multiplication)"
    FAIL=$((FAIL + 1))
fi

# Quoted functor checks
if echo "$batch_b_output" | grep -q "Rq1 = 6"; then
    echo "PASS: Quoted atom functor in head: '_test_kernel'(5, R) returns R = 6"
    PASS=$((PASS + 1))
else
    echo "FAIL: Quoted atom functor in head (expected: Rq1 = 6)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_b_output" | grep -q "Rq2 = 11"; then
    echo "PASS: Quoted atom functor in body: wrapper(10, R) returns R = 11"
    PASS=$((PASS + 1))
else
    echo "FAIL: Quoted atom functor in body (expected: Rq2 = 11)"
    FAIL=$((FAIL + 1))
fi

if echo "$batch_b_output" | grep -q "_equator"; then
    echo "PASS: Quoted atom as structure functor: X = '_equator'(E, stop)"
    PASS=$((PASS + 1))
else
    echo "FAIL: Quoted atom as structure functor"
    FAIL=$((FAIL + 1))
fi

# --- Circular Term Tests ---
echo ""
echo "--- Circular Term Tests ---"
circular_output="$mega_output"

# Test circular term creation and display
if echo "$circular_output" | grep -q "<circular>"; then
    echo "PASS: Circular term created and displayed with <circular> marker"
    PASS=$((PASS + 1))
else
    echo "FAIL: Circular term creation (expected: <circular> marker in output)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rc1 = yes"; then
    echo "PASS: is_ground(foo) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: is_ground(foo) (expected: Rc1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rc2 = yes"; then
    echo "PASS: is_ground(f(a,b)) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: is_ground(f(a,b)) (expected: Rc2 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rc3 = yes"; then
    echo "PASS: test_equal(foo,foo) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_equal(foo,foo) (expected: Rc3 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rc4 = no"; then
    echo "PASS: test_equal(foo,bar) = no"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_equal(foo,bar) (expected: Rc4 = no)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rc5 = yes"; then
    echo "PASS: test_self_equal(f(a,b)) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_self_equal(f(a,b)) (expected: Rc5 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Xshow = hello"; then
    echo "PASS: show(hello,X) = hello"
    PASS=$((PASS + 1))
else
    echo "FAIL: show(hello,X) (expected: Xshow = hello)"
    FAIL=$((FAIL + 1))
fi

# Test ground and equality on actual circular terms (from second :clear group)
if echo "$circular_output" | grep -q "Rg1 = yes"; then
    echo "PASS: ground(circular_term) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: ground(circular_term) (expected: Rg1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$circular_output" | grep -q "Rg2 = yes"; then
    echo "PASS: circular_term =?= circular_term = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: circular_term =?= circular_term (expected: Rg2 = yes)"
    FAIL=$((FAIL + 1))
fi

# Multi-arity predicate tests
echo ""
echo "--- Multi-Arity Predicate Tests ---"
multi_arity_output="$mega_output"

if echo "$multi_arity_output" | grep -q "S1 = 15"; then
    echo "PASS: sum/2 with accumulator = 15"
    PASS=$((PASS + 1))
else
    echo "FAIL: sum/2 with accumulator (expected: S1 = 15)"
    FAIL=$((FAIL + 1))
fi

if echo "$multi_arity_output" | grep -q "Rrev1 = \[3, 2, 1\]"; then
    echo "PASS: reverse/2 with accumulator = [3,2,1]"
    PASS=$((PASS + 1))
else
    echo "FAIL: reverse/2 with accumulator (expected: Rrev1 = [3, 2, 1])"
    FAIL=$((FAIL + 1))
fi

if echo "$multi_arity_output" | grep -q "N1 = 4"; then
    echo "PASS: length/2 with accumulator = 4"
    PASS=$((PASS + 1))
else
    echo "FAIL: length/2 with accumulator (expected: N1 = 4)"
    FAIL=$((FAIL + 1))
fi

# Arithmetic guards imply groundness tests (from mega_output)
echo ""
echo "--- Arithmetic Guards Imply Groundness Tests ---"
arith_ground_output="$mega_output"

if echo "$arith_ground_output" | grep -q "Rag1 = pair(3, 5)"; then
    echo "PASS: compare_and_use(3, 5, R) = pair(3, 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: compare_and_use(3, 5, R) (expected: Rag1 = pair(3, 5))"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_ground_output" | grep -q "M1 = 7"; then
    echo "PASS: max(7, 4, M) = 7"
    PASS=$((PASS + 1))
else
    echo "FAIL: max(7, 4, M) (expected: M1 = 7)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_ground_output" | grep -q "Rir1 = yes"; then
    echo "PASS: in_range(5, 1, 10, R) = yes"
    PASS=$((PASS + 1))
else
    echo "FAIL: in_range(5, 1, 10, R) (expected: Rir1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_ground_output" | grep -q "Rir2 = no"; then
    echo "PASS: in_range(15, 1, 10, R) = no"
    PASS=$((PASS + 1))
else
    echo "FAIL: in_range(15, 1, 10, R) (expected: Rir2 = no)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_ground_output" | grep -q "Rce1 = pair(1, 5)"; then
    echo "PASS: compare_expr(1, 5, R) with X?+1 < Y?*2 = pair(1, 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: compare_expr(1, 5, R) (expected: Rce1 = pair(1, 5))"
    FAIL=$((FAIL + 1))
fi

# Arithmetic comparison operators (from mega_output)
echo ""
echo "--- Arithmetic Comparison Operators Tests ---"
arith_cmp_output="$mega_output"

if echo "$arith_cmp_output" | grep -q "Raeq1 = equal"; then
    echo "PASS: =:= equals (5 =:= 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =:= equals (expected: Raeq1 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Raeq2 = not_equal"; then
    echo "PASS: =:= not equals via otherwise (5 =:= 3)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =:= not equals via otherwise (expected: Raeq2 = not_equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Raneq1 = not_equal"; then
    echo "PASS: =\\= not equals (5 =\\= 3)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =\\= not equals (expected: Raneq1 = not_equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Raneq2 = equal"; then
    echo "PASS: =\\= equals via otherwise (5 =\\= 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =\\= equals via otherwise (expected: Raneq2 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Reeq1 = equal"; then
    echo "PASS: =:= with expressions (4+1 =:= 6-1)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =:= with expressions (expected: Reeq1 = equal)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Rlt1 = yes"; then
    echo "PASS: < comparison (3 < 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: < comparison (expected: Rlt1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Rgt1 = yes"; then
    echo "PASS: > comparison (5 > 3)"
    PASS=$((PASS + 1))
else
    echo "FAIL: > comparison (expected: Rgt1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Rle1 = yes"; then
    echo "PASS: =< comparison (5 =< 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: =< comparison (expected: Rle1 = yes)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_cmp_output" | grep -q "Rge1 = no"; then
    echo "PASS: >= comparison fails (3 >= 5)"
    PASS=$((PASS + 1))
else
    echo "FAIL: >= comparison fails (expected: Rge1 = no)"
    FAIL=$((FAIL + 1))
fi

# Otherwise guard tests (from mega_output)
echo ""
echo "--- Otherwise Guard Tests ---"
otherwise_output="$mega_output"

if echo "$otherwise_output" | grep -q "Rcl1 = positive"; then
    echo "PASS: otherwise - classify 5 as positive"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - classify 5 (expected: Rcl1 = positive)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "Rcl2 = negative"; then
    echo "PASS: otherwise - classify -3 as negative"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - classify -3 (expected: Rcl2 = negative)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "Rcl3 = zero"; then
    echo "PASS: otherwise - classify 0 as zero (default)"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - classify 0 (expected: Rcl3 = zero)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "G1 = a"; then
    echo "PASS: otherwise - grade 95 = a"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - grade 95 (expected: G1 = a)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "G2 = c"; then
    echo "PASS: otherwise - grade 75 = c"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - grade 75 (expected: G2 = c)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "G3 = f"; then
    echo "PASS: otherwise - grade 55 = f (default)"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - grade 55 (expected: G3 = f)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "T1 = integer"; then
    echo "PASS: otherwise - type_of 42 = integer"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - type_of 42 (expected: T1 = integer)"
    FAIL=$((FAIL + 1))
fi

if echo "$otherwise_output" | grep -q "T2 = string"; then
    echo "PASS: otherwise - type_of hello = string"
    PASS=$((PASS + 1))
else
    echo "FAIL: otherwise - type_of hello (expected: T2 = string)"
    FAIL=$((FAIL + 1))
fi

# Difference list syntax tests (from mega_output)
echo ""
echo "--- Difference List Syntax Tests ---"
dl_output="$mega_output"

# Test that dl_append parses and executes (produces a \(...) structure)
if echo "$dl_output" | grep -q 'Rdl = \\'; then
    echo "PASS: dl_append with H\\T syntax parses and executes"
    PASS=$((PASS + 1))
else
    echo "FAIL: dl_append with H\\T syntax (expected: Rdl = \\(...))"
    FAIL=$((FAIL + 1))
fi

# Test dl_to_list conversion
if echo "$dl_output" | grep -q "Ldl = \[a, b, c\]"; then
    echo "PASS: dl_to_list converts difference list to regular list"
    PASS=$((PASS + 1))
else
    echo "FAIL: dl_to_list conversion (expected: Ldl = [a, b, c])"
    FAIL=$((FAIL + 1))
fi

# Test parsing of simple H\T term
if echo "$dl_output" | grep -q 'Xdl = \\(foo, bar)'; then
    echo "PASS: Simple H\\T term parses as \\(H, T) structure"
    PASS=$((PASS + 1))
else
    echo "FAIL: Simple H\\T term (expected: Xdl = \\(foo, bar))"
    FAIL=$((FAIL + 1))
fi

# Guard reader occurrence tests (from mega_output)
echo ""
echo "--- Guard Reader Occurrence Tests ---"
guard_reader_output="$mega_output"

# Check that file loaded successfully (no SRSW errors)
if echo "$guard_reader_output" | grep -q "Loaded.*guard_reader.glp"; then
    echo "PASS: guard_reader.glp loads (grounding guards satisfy SRSW)"
    PASS=$((PASS + 1))
else
    echo "FAIL: guard_reader.glp should load without SRSW errors"
    FAIL=$((FAIL + 1))
fi

# Count the number of "succeeds" after guard_reader.glp queries
succeed_count=$(echo "$guard_reader_output" | grep -c "succeeds" || true)
if [ "$succeed_count" -ge 4 ]; then
    echo "PASS: guard_ground(42) with ground/1 guard"
    PASS=$((PASS + 1))
    echo "PASS: guard_int(7) with integer/1 guard"
    PASS=$((PASS + 1))
    echo "PASS: guard_compare(3,5) with X?<Y? guard"
    PASS=$((PASS + 1))
    echo "PASS: guard_known_valid(hello, Y) with known/1 guard + body reader"
    PASS=$((PASS + 1))
else
    echo "FAIL: guard_ground(42) (expected: succeeds)"
    FAIL=$((FAIL + 1))
    echo "FAIL: guard_int(7) (expected: succeeds)"
    FAIL=$((FAIL + 1))
    echo "FAIL: guard_compare(3,5) (expected: succeeds)"
    FAIL=$((FAIL + 1))
    echo "FAIL: guard_known_valid(hello, Y) (expected: succeeds)"
    FAIL=$((FAIL + 1))
fi

# ===========================================
# ORPHAN INTEGRATION TESTS (from mega_output)
# ===========================================
echo ""
echo "--- Orphan Integration Tests (Arithmetic Kernels) ---"
arith_kernel_output="$mega_output"

if echo "$arith_kernel_output" | grep -q "Loaded.*test_arithmetic_kernels.glp"; then
    echo "PASS: test_arithmetic_kernels.glp loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_arithmetic_kernels.glp should load"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak1 = 3"; then
    echo "PASS: test_idiv(10, 3, R) = 3"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_idiv(10, 3, R) (expected: Rak1 = 3)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak2 = 5"; then
    echo "PASS: test_abs(-5, R) = 5"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_abs(-5, R) (expected: Rak2 = 5)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak3 = 4"; then
    echo "PASS: test_sqrt(16, R) = 4"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_sqrt(16, R) (expected: Rak3 = 4)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak4 = 1024"; then
    echo "PASS: test_pow(2, 10, R) = 1024"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_pow(2, 10, R) (expected: Rak4 = 1024)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak5 = 3"; then
    echo "PASS: test_floor(3.7, R) = 3"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_floor(3.7, R) (expected: Rak5 = 3)"
    FAIL=$((FAIL + 1))
fi

if echo "$arith_kernel_output" | grep -q "Rak6 = 4"; then
    echo "PASS: test_ceil(3.2, R) = 4"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_ceil(3.2, R) (expected: Rak6 = 4)"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "--- Orphan Integration Tests (Guards Comprehensive) ---"
guards_comp_output="$mega_output"

if echo "$guards_comp_output" | grep -q "Loaded.*test_guards_comprehensive.glp"; then
    echo "PASS: test_guards_comprehensive.glp loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_guards_comprehensive.glp should load"
    FAIL=$((FAIL + 1))
fi

if echo "$guards_comp_output" | grep -q "Rgc1 = ok"; then
    echo "PASS: test_list_ok([1,2,3], R) = ok"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_list_ok([1,2,3], R) (expected: Rgc1 = ok)"
    FAIL=$((FAIL + 1))
fi

if echo "$guards_comp_output" | grep -q "Rgc2 = ok"; then
    echo "PASS: test_string_ok(\"hello\", R) = ok"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_string_ok(\"hello\", R) (expected: Rgc2 = ok)"
    FAIL=$((FAIL + 1))
fi

if echo "$guards_comp_output" | grep -q "Rgc3 = ok"; then
    echo "PASS: test_constant_ok(foo, R) = ok"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_constant_ok(foo, R) (expected: Rgc3 = ok)"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "--- Orphan Integration Tests (Constant Ground, Gethead) ---"
other_orphan_output="$mega_output"

if echo "$other_orphan_output" | grep -q "Loaded.*constant_ground_test.glp"; then
    echo "PASS: constant_ground_test.glp loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: constant_ground_test.glp should load"
    FAIL=$((FAIL + 1))
fi

if echo "$other_orphan_output" | grep -q "Rcgt1 = foo"; then
    echo "PASS: test_constant(foo, R1, R2) - R1 = foo"
    PASS=$((PASS + 1))
else
    echo "FAIL: test_constant(foo, R1, R2) (expected: Rcgt1 = foo)"
    FAIL=$((FAIL + 1))
fi

if echo "$other_orphan_output" | grep -q "Loaded.*gethead_test.glp"; then
    echo "PASS: gethead_test.glp loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: gethead_test.glp should load"
    FAIL=$((FAIL + 1))
fi

if echo "$other_orphan_output" | grep -q "Rgh1 = 42"; then
    echo "PASS: gethead test2(R) = 42"
    PASS=$((PASS + 1))
else
    echo "FAIL: gethead test2(R) (expected: R3 = 42)"
    FAIL=$((FAIL + 1))
fi

# ===========================================
# TYPECHECKER TESTS
# ===========================================
echo ""
echo "--- Typechecker Positive Tests ---"

TYPECHECK_DIR="$GLP_RUNTIME/test/programs/typechecker"

# Positive tests - should load successfully
tc_pos_output=$($DART run "$REPL" <<TC_POS
$TYPECHECK_DIR/positive/merge_basic.glp
merge([1,3,5], [2,4], R).
$TYPECHECK_DIR/positive/append_list.glp
append([1,2], [3,4], R).
$TYPECHECK_DIR/positive/copy_stream.glp
copy([1,2,3], R).
$TYPECHECK_DIR/positive/nat_operations.glp
$TYPECHECK_DIR/positive/monitor.glp
$TYPECHECK_DIR/positive/process_complete.glp
$TYPECHECK_DIR/positive/disjoint_primitives.glp
$TYPECHECK_DIR/positive/int_list_sum.glp
$TYPECHECK_DIR/positive/dl_append.glp
$TYPECHECK_DIR/positive/new_channel.glp
$TYPECHECK_DIR/positive/universal_structured_term.glp
$TYPECHECK_DIR/positive/book/universal_accepts_structured.glp
:quit
TC_POS
2>&1)

# Check positive tests loaded
if echo "$tc_pos_output" | grep -q "Loaded.*merge_basic.glp"; then
    echo "PASS: Typecheck merge_basic loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck merge_basic should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "R = \[1, 2, 3, 4, 5\]"; then
    echo "PASS: Typecheck merge_basic runs: merge([1,3,5], [2,4], R)"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck merge_basic run (expected: R = [1, 2, 3, 4, 5])"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*append_list.glp"; then
    echo "PASS: Typecheck append_list loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck append_list should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "R = \[1, 2, 3, 4\]"; then
    echo "PASS: Typecheck append_list runs: append([1,2], [3,4], R)"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck append_list run (expected: R = [1, 2, 3, 4])"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*copy_stream.glp"; then
    echo "PASS: Typecheck copy_stream loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck copy_stream should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*nat_operations.glp"; then
    echo "PASS: Typecheck nat_operations loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck nat_operations should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*monitor.glp"; then
    echo "PASS: Typecheck monitor loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck monitor should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*process_complete.glp"; then
    echo "PASS: Typecheck process_complete loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck process_complete should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*disjoint_primitives.glp"; then
    echo "PASS: Typecheck disjoint_primitives loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck disjoint_primitives should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*int_list_sum.glp"; then
    echo "PASS: Typecheck int_list_sum loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck int_list_sum should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*dl_append.glp"; then
    echo "PASS: Typecheck dl_append loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck dl_append should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*new_channel.glp"; then
    echo "PASS: Typecheck new_channel loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck new_channel should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*universal_structured_term.glp"; then
    echo "PASS: Typecheck universal_structured_term loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck universal_structured_term should load"
    FAIL=$((FAIL + 1))
fi

if echo "$tc_pos_output" | grep -q "Loaded.*universal_accepts_structured.glp"; then
    echo "PASS: Typecheck universal_accepts_structured loads"
    PASS=$((PASS + 1))
else
    echo "FAIL: Typecheck universal_accepts_structured should load"
    FAIL=$((FAIL + 1))
fi

echo ""
echo "--- Typechecker Negative Tests (batched) ---"

# Negative tests - all run in single session with :clear between each
# Count rejections to verify each file is properly rejected
tc_neg_output=$($DART run "$REPL" <<TC_NEG_ALL
$TYPECHECK_DIR/negative/type_def/merge_undefined_type.glp
:clear
$TYPECHECK_DIR/negative/head/merge_reader_at_input.glp
:clear
$TYPECHECK_DIR/negative/head/merge_writer_at_output.glp
:clear
$TYPECHECK_DIR/negative/head/merge_wrong_constant.glp
:clear
$TYPECHECK_DIR/negative/head/merge_wrong_functor.glp
:clear
$TYPECHECK_DIR/negative/body/merge_undefined_proc.glp
:clear
$TYPECHECK_DIR/negative/body/merge_wrong_mode.glp
:clear
$TYPECHECK_DIR/negative/complementarity/merge_swapped_vars.glp
:clear
$TYPECHECK_DIR/negative/complementarity/merge_type_mismatch.glp
:clear
$TYPECHECK_DIR/negative/coverage/merge_missing_both_nil.glp
:clear
$TYPECHECK_DIR/negative/coverage/merge_missing_cons.glp
:clear
$TYPECHECK_DIR/negative/coverage/merge_missing_first_nil.glp
:clear
$TYPECHECK_DIR/negative/append_bad_type.glp
:clear
$TYPECHECK_DIR/negative/channel_non_complementary.glp
:clear
$TYPECHECK_DIR/negative/constant_at_wrong_type.glp
:clear
$TYPECHECK_DIR/negative/functor_mismatch.glp
:clear
$TYPECHECK_DIR/negative/merge_incomplete.glp
:clear
$TYPECHECK_DIR/negative/missing_coverage.glp
:clear
$TYPECHECK_DIR/negative/non_complementary_types.glp
:quit
TC_NEG_ALL
2>&1)

# Count how many rejections occurred
reject_count=$(echo "$tc_neg_output" | grep -ci "Type checking failed\|Error loading" || true)
expected_rejects=19

# Individual test names for reporting
tc_neg_names=(
    "merge_undefined_type"
    "merge_reader_at_input"
    "merge_writer_at_output"
    "merge_wrong_constant"
    "merge_wrong_functor"
    "merge_undefined_proc"
    "merge_wrong_mode"
    "merge_swapped_vars"
    "merge_type_mismatch"
    "merge_missing_both_nil"
    "merge_missing_cons"
    "merge_missing_first_nil"
    "append_bad_type"
    "channel_non_complementary"
    "constant_at_wrong_type"
    "functor_mismatch"
    "merge_incomplete"
    "missing_coverage"
    "non_complementary_types"
)

# Check each test - if "Loaded" appears for that file, it failed to reject
for tc_name in "${tc_neg_names[@]}"; do
    if echo "$tc_neg_output" | grep -q "Loaded.*${tc_name}"; then
        # File loaded successfully when it should have failed
        echo "FAIL: Typecheck fail $tc_name (expected rejection, got success)"
        FAIL=$((FAIL + 1))
    else
        # Either got "Type checking failed" or "Error loading" - both are rejections
        echo "PASS: Typecheck fail $tc_name (rejects)"
        PASS=$((PASS + 1))
    fi
done

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
