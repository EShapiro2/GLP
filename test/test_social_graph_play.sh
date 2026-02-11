#!/bin/bash
# Clause-level unit tests for social_graph_play.glp
# Tests each procedure and clause individually
#
# Usage: bash test/test_social_graph_play.sh
# Run from GLP root directory

set -e

DART=${DART:-$(which dart 2>/dev/null || echo "/home/user/dart-sdk/bin/dart")}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/.."
GLP_RUNTIME="$GLP_DIR/glp_runtime"
FILE="$GLP_DIR/programs/tests/typed/social_graph_play.glp"

cd "$GLP_RUNTIME"

# Use snapshot if available
REPL_SNAPSHOT=".dart_tool/repl.dill"
NEEDS_RECOMPILE=false
if [ ! -f "$REPL_SNAPSHOT" ]; then
    NEEDS_RECOMPILE=true
elif [ -n "$(find lib bin -name '*.dart' -newer "$REPL_SNAPSHOT" 2>/dev/null | head -1)" ]; then
    NEEDS_RECOMPILE=true
fi
if [ "$NEEDS_RECOMPILE" = true ]; then
    echo "Compiling REPL snapshot..."
    mkdir -p .dart_tool
    $DART compile kernel -o "$REPL_SNAPSHOT" bin/glp_repl.dart 2>/dev/null || true
fi
if [ -f "$REPL_SNAPSHOT" ]; then
    REPL="$REPL_SNAPSHOT"
else
    REPL="bin/glp_repl.dart"
fi

echo "================================================"
echo "  Social Graph Play - Clause-Level Unit Tests"
echo "================================================"
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
# 1. FILE LOADING (type-check)
# =============================================================================
echo "--- 1. File loading and type-check ---"
t0=$($DART run "$REPL" <<HEREDOC
$FILE
:quit
HEREDOC
2>&1)

check "File loads successfully" "Loaded.*social_graph_play" "$t0"

# =============================================================================
# 2. MERGE (4 clauses)
# =============================================================================
echo "--- 2. merge ---"
t1=$($DART run "$REPL" <<HEREDOC
$FILE
merge([1,2,3], [a,b], M1).
merge([], [1,2], M2).
merge([1,2], [], M3).
merge([], [], M4).
:quit
HEREDOC
2>&1)

check "merge clause 1+2: interleave" "M1 = \[1, a, 2, b, 3\]" "$t1"
check "merge clause 4: first empty" "M2 = \[1, 2\]" "$t1"
check "merge clause 3: second empty" "M3 = \[1, 2\]" "$t1"
check "merge clause 3+4: both empty" "M4 = \[\]" "$t1"

# =============================================================================
# 3. LOOKUP_SEND_STEP (3 clauses)
# =============================================================================
echo "--- 3. lookup_send_step ---"
t2=$($DART run "$REPL" <<HEREDOC
$FILE
lookup_send_step(user, hello, [(user, U1), (net, N1)], LFs1).
lookup_send_step(net, hello, [(user, U2), (net, N2)], LFs2).
lookup_send_step(foo, hello, [(user, U3)], LFs3).
lookup_send_step(net, hello, [], LFs4).
:quit
HEREDOC
2>&1)

check "lookup_send_step clause 1: found first" "U1 = \[hello" "$t2"
check "lookup_send_step clause 2+1: skip then found" "N2 = \[hello" "$t2"
check "lookup_send_step clause 2+3: not found" "LFs3 = \[," "$t2"
check "lookup_send_step clause 3: empty list" "LFs4 = \[\]" "$t2"

# =============================================================================
# 4. LOOKUP_SEND (wrapper with ground guard)
# =============================================================================
echo "--- 4. lookup_send ---"
t3=$($DART run "$REPL" <<HEREDOC
$FILE
lookup_send(user, hello, [(user, LS1), (net, LS2)], LSFs).
:quit
HEREDOC
2>&1)

check "lookup_send: delegates to step, msg sent" "LS1 = \[hello" "$t3"
check "lookup_send: net entry unchanged" "LS2 = <unbound>" "$t3"

# =============================================================================
# 5. INJECT_MSG (3 clauses)
# =============================================================================
echo "--- 5. inject_msg ---"
t4=$($DART run "$REPL" <<HEREDOC
$FILE
inject_msg(no, bob, alice, [], IM1).
inject_msg(accept(ch(IX1?, IX2)), bob, alice, [], IM2).
inject_msg(no, bob, alice, [x, y], IM3).
:quit
HEREDOC
2>&1)

check "inject_msg clause 1: known resp=no" "IM1 = \[msg(bob, alice, response(no))\]" "$t4"
check "inject_msg clause 1: known resp=accept" "msg(bob, alice, response(accept" "$t4"
check "inject_msg clause 1: known resp with items" "IM3 = \[msg(bob, alice, response(no)), x, y\]" "$t4"

# =============================================================================
# 6. TAG_STREAM (2 clauses)
# =============================================================================
echo "--- 6. tag_stream ---"
t5=$($DART run "$REPL" <<HEREDOC
$FILE
tag_stream(alice, [m1, m2, m3], TS1).
tag_stream(bob, [], TS2).
:quit
HEREDOC
2>&1)

check "tag_stream clause 1: tags messages" "tagged(alice, m1)" "$t5"
check "tag_stream clause 1: tags all" "tagged(alice, m3)" "$t5"
check "tag_stream clause 2: empty" "TS2 = \[\]" "$t5"

# =============================================================================
# 7. BIND_RESPONSE (2 clauses)
# =============================================================================
echo "--- 7. bind_response ---"
t6=$($DART run "$REPL" <<HEREDOC
$FILE
bind_response(no, alice, BResp1, [(user, BU1), (net, BN1)], BFs1, [], BIn1).
bind_response(yes, alice, BResp2, [(user, BU2), (net, BN2)], BFs2, [], BIn2).
:quit
HEREDOC
2>&1)

check "bind_response clause 2: no - Resp=no" "BResp1 = no" "$t6"
check "bind_response clause 2: no - succeeds" "BIn1 = \[\]" "$t6"
check "bind_response clause 1: yes - Resp=accept" "BResp2 = accept(ch(" "$t6"
check "bind_response clause 1: yes - alice added" "alice" "$t6"

# =============================================================================
# 8. HANDLE_RESPONSE (2 clauses)
# =============================================================================
echo "--- 8. handle_response ---"
t7=$($DART run "$REPL" <<HEREDOC
$FILE
handle_response(no, alice, [(user, HU1), (net, HN1)], HFs1, [], HIn1).
handle_response(accept(ch(HFIn?, HFOut)), alice, [(user, HU2), (net, HN2)], HFs2, [], HIn2).
:quit
HEREDOC
2>&1)

check "handle_response clause 2: no - succeeds" "HIn1 = \[\]" "$t7"
check "handle_response clause 1: accept - alice in friends" "HFs2 = \[,.*alice" "$t7"
check "handle_response clause 1: accept - suspended" "suspended" "$t7"

# =============================================================================
# 9. SOCIAL_GRAPH - Base case (clause 6)
# =============================================================================
echo "--- 9. social_graph: base case ---"
t8=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [], [(user, SU1), (net, SN1)]).
:quit
HEREDOC
2>&1)

check "social_graph base case: succeeds" "succeeds" "$t8"

# =============================================================================
# 10. SOCIAL_GRAPH - Skip/otherwise (clause 5)
# =============================================================================
echo "--- 10. social_graph: skip unhandled ---"
t9=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [unknown_msg], [(user, SU2), (net, SN2)]).
:quit
HEREDOC
2>&1)

check "social_graph skip: succeeds" "succeeds" "$t9"

# =============================================================================
# 11. SOCIAL_GRAPH - response(no) (clause 4)
# =============================================================================
echo "--- 11. social_graph: response(no) ---"
t10=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(no))], [(user, SU3), (net, SN3)]).
:quit
HEREDOC
2>&1)

check "social_graph response(no): succeeds" "succeeds" "$t10"

# =============================================================================
# 12. SOCIAL_GRAPH - response(accept) (clause 4)
# =============================================================================
echo "--- 12. social_graph: response(accept) ---"
t11=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(accept(ch(SFIn?, SFOut))))], [(user, SU4), (net, SN4)]).
:quit
HEREDOC
2>&1)

check "social_graph response(accept): suspended" "suspended" "$t11"

# =============================================================================
# 13. SOCIAL_GRAPH - connect (clause 1)
# =============================================================================
echo "--- 13. social_graph: connect ---"
t12=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(alice, [msg(user, alice, connect(bob))], [(user, CU1), (net, CN1)]).
:quit
HEREDOC
2>&1)

check "social_graph connect: intro sent to net" "CN1 = \[msg(alice, bob, intro(alice, alice" "$t12"
check "social_graph connect: succeeds" "succeeds" "$t12"

# =============================================================================
# 14. SOCIAL_GRAPH - intro (clause 2)
# =============================================================================
echo "--- 14. social_graph: intro ---"
t13=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, intro(alice, alice, IRsp))], [(user, IU1), (net, IN1)]).
:quit
HEREDOC
2>&1)

check "social_graph intro: befriend sent to user" "IU1 = \[msg(agent, user, befriend(alice" "$t13"
check "social_graph intro: succeeds" "succeeds" "$t13"

# =============================================================================
# 15. SOCIAL_GRAPH - decision(no) (clause 3)
# =============================================================================
echo "--- 15. social_graph: decision(no) ---"
t14=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(user, bob, decision(no, alice, DResp1?))], [(user, DU1), (net, DN1)]).
:quit
HEREDOC
2>&1)

check "social_graph decision(no): succeeds" "succeeds" "$t14"

# =============================================================================
# 16. SOCIAL_GRAPH - decision(yes) (clause 3)
# =============================================================================
echo "--- 16. social_graph: decision(yes) ---"
t15=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(user, bob, decision(yes, alice, DResp2?))], [(user, DU2), (net, DN2)]).
:quit
HEREDOC
2>&1)

check "social_graph decision(yes): suspended (channel setup)" "suspended" "$t15"

# =============================================================================
# 17. AGENT_INIT
# =============================================================================
echo "--- 17. agent_init ---"
t16=$($DART run "$REPL" <<HEREDOC
$FILE
agent_init(alice, ch(AUIn?, AUOut), ch(ANIn?, ANOut)).
:quit
HEREDOC
2>&1)

check "agent_init: suspended (waiting for input)" "suspended" "$t16"

# =============================================================================
# 18. BOB_DONE (2 clauses)
# =============================================================================
echo "--- 18. bob_done ---"
t17=$($DART run "$REPL" <<HEREDOC
$FILE
bob_done([], BD1).
bob_done([x, y], BD2).
:quit
HEREDOC
2>&1)

check "bob_done clause 1: empty in" "BD1 = \[\]" "$t17"
check "bob_done clause 2: skip items" "BD2 = \[\]" "$t17"

# =============================================================================
# 19. ALICE_DONE
# =============================================================================
echo "--- 19. alice_done ---"
t18=$($DART run "$REPL" <<HEREDOC
$FILE
alice_done([]).
:quit
HEREDOC
2>&1)

check "alice_done: succeeds" "succeeds" "$t18"

# =============================================================================
# 20. PLAY (full integration)
# =============================================================================
echo "--- 20. play (full integration) ---"
t19=$($DART run "$REPL" <<HEREDOC
$FILE
play.
:quit
HEREDOC
2>&1)

check "play: suspended (concurrent processes running)" "suspended" "$t19"

# =============================================================================
# 21. SOCIAL_GRAPH - multiple messages sequence
# =============================================================================
echo "--- 21. social_graph: multiple messages ---"
t20=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(no)), msg(carol, bob, response(no))], [(user, MU1), (net, MN1)]).
:quit
HEREDOC
2>&1)

check "social_graph multi-msg: succeeds" "succeeds" "$t20"

# =============================================================================
# 22. INJECT_MSG - empty input with unknown resp
# =============================================================================
echo "--- 22. inject_msg: edge cases ---"
t21=$($DART run "$REPL" <<HEREDOC
$FILE
inject_msg(IMR, bob, alice, [], IM4).
:quit
HEREDOC
2>&1)

check "inject_msg clause 3: unknown resp + empty = empty" "IM4 = \[\]" "$t21"

# =============================================================================
# SUMMARY
# =============================================================================
echo ""
echo "================================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "================================================"

if [ $FAIL -gt 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
else
    echo "ALL TESTS PASSED!"
fi
