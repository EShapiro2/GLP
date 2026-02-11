#!/bin/bash
# project_test.sh - Clause-level unit tests for social_graph_play.glp
#
# Usage:  bash project_test.sh
# Run from: /Users/udi/Grassroots/GLP

DART=${DART:-$(which dart 2>/dev/null)}
GLP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GLP_RUNTIME="$GLP_DIR/glp_runtime"
FILE="$GLP_DIR/programs/tests/typed/social_graph_play.glp"
REPL="bin/glp_repl.dart"

cd "$GLP_RUNTIME"

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

echo "================================================"
echo "  Social Graph Play - Clause-Level Unit Tests"
echo "================================================"
echo ""

# --- 1. File loading ---
echo "--- 1. File loading ---"
t0=$($DART run "$REPL" <<HEREDOC
$FILE
:quit
HEREDOC
2>&1)
check "File loads and type-checks" "Loaded.*social_graph_play" "$t0"

# --- 2. merge (4 clauses) ---
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
check "merge interleave" "M1 = \[1, a, 2, b, 3\]" "$t1"
check "merge first empty" "M2 = \[1, 2\]" "$t1"
check "merge second empty" "M3 = \[1, 2\]" "$t1"
check "merge both empty" "M4 = \[\]" "$t1"

# --- 3. tag_stream (2 clauses) ---
echo "--- 3. tag_stream ---"
t2=$($DART run "$REPL" <<HEREDOC
$FILE
tag_stream(alice, [m1, m2, m3], TS1).
tag_stream(bob, [], TS2).
:quit
HEREDOC
2>&1)
check "tag_stream tags messages" "tagged(alice, m1)" "$t2"
check "tag_stream all tagged" "tagged(alice, m3)" "$t2"
check "tag_stream empty" "TS2 = \[\]" "$t2"

# --- 4. lookup_send_step (3 clauses) ---
echo "--- 4. lookup_send_step ---"
t3=$($DART run "$REPL" <<HEREDOC
$FILE
lookup_send_step(user, hello, [(user, U1), (net, N1)], LFs1).
lookup_send_step(net, hello, [(user, U2), (net, N2)], LFs2).
lookup_send_step(foo, hello, [(user, U3)], LFs3).
lookup_send_step(net, hello, [], LFs4).
:quit
HEREDOC
2>&1)
check "lookup_send_step found first" "U1 = \[hello" "$t3"
check "lookup_send_step skip then found" "N2 = \[hello" "$t3"
check "lookup_send_step not found" "LFs3 = \[," "$t3"
check "lookup_send_step empty" "LFs4 = \[\]" "$t3"

# --- 5. lookup_send (1 clause, ground guard) ---
echo "--- 5. lookup_send ---"
t4=$($DART run "$REPL" <<HEREDOC
$FILE
lookup_send(user, hello, [(user, LS1), (net, LS2)], LSFs).
:quit
HEREDOC
2>&1)
check "lookup_send delegates, msg sent" "LS1 = \[hello" "$t4"
check "lookup_send other entry unchanged" "LS2 = <unbound>" "$t4"

# --- 6. inject_msg (3 clauses) ---
echo "--- 6. inject_msg ---"
t5=$($DART run "$REPL" <<HEREDOC
$FILE
inject_msg(no, bob, alice, [], IM1).
inject_msg(accept(ch(IX1?, IX2)), bob, alice, [], IM2).
inject_msg(no, bob, alice, [x, y], IM3).
inject_msg(IMR, bob, alice, [], IM4).
:quit
HEREDOC
2>&1)
check "inject_msg known resp=no" "IM1 = \[msg(bob, alice, response(no))\]" "$t5"
check "inject_msg known resp=accept" "msg(bob, alice, response(accept" "$t5"
check "inject_msg known resp + items" "IM3 = \[msg(bob, alice, response(no)), x, y\]" "$t5"
check "inject_msg unknown resp + empty" "IM4 = \[\]" "$t5"

# --- 7. bind_response (2 clauses) ---
echo "--- 7. bind_response ---"
t6=$($DART run "$REPL" <<HEREDOC
$FILE
bind_response(no, alice, BResp1, [(user, BU1), (net, BN1)], BFs1, [], BIn1).
bind_response(yes, alice, BResp2, [(user, BU2), (net, BN2)], BFs2, [], BIn2).
:quit
HEREDOC
2>&1)
check "bind_response no: Resp=no" "BResp1 = no" "$t6"
check "bind_response no: input unchanged" "BIn1 = \[\]" "$t6"
check "bind_response yes: Resp=accept(ch(...))" "BResp2 = accept(ch(" "$t6"
check "bind_response yes: alice in friends" "alice" "$t6"

# --- 8. handle_response (2 clauses) ---
echo "--- 8. handle_response ---"
t7=$($DART run "$REPL" <<HEREDOC
$FILE
handle_response(no, alice, [(user, HU1), (net, HN1)], HFs1, [], HIn1).
handle_response(accept(ch(HFIn?, HFOut)), alice, [(user, HU2), (net, HN2)], HFs2, [], HIn2).
:quit
HEREDOC
2>&1)
check "handle_response no: succeeds" "HIn1 = \[\]" "$t7"
check "handle_response accept: alice in friends" "HFs2 = \[,.*alice" "$t7"
check "handle_response accept: suspended (streams)" "suspended" "$t7"

# --- 9. social_graph - base case (clause 6) ---
echo "--- 9. social_graph: base case ---"
t8=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [], [(user, SU1), (net, SN1)]).
:quit
HEREDOC
2>&1)
check "social_graph base case succeeds" "succeeds" "$t8"

# --- 10. social_graph - skip/otherwise (clause 5) ---
echo "--- 10. social_graph: skip ---"
t9=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [unknown_msg], [(user, SU2), (net, SN2)]).
:quit
HEREDOC
2>&1)
check "social_graph skip unhandled succeeds" "succeeds" "$t9"

# --- 11. social_graph - response(no) (clause 4) ---
echo "--- 11. social_graph: response(no) ---"
t10=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(no))], [(user, SU3), (net, SN3)]).
:quit
HEREDOC
2>&1)
check "social_graph response(no) succeeds" "succeeds" "$t10"

# --- 12. social_graph - response(accept) (clause 4) ---
echo "--- 12. social_graph: response(accept) ---"
t11=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(accept(ch(SFIn?, SFOut))))], [(user, SU4), (net, SN4)]).
:quit
HEREDOC
2>&1)
check "social_graph response(accept) suspended" "suspended" "$t11"

# --- 13. social_graph - connect (clause 1) ---
echo "--- 13. social_graph: connect ---"
t12=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(alice, [msg(user, alice, connect(bob))], [(user, CU1), (net, CN1)]).
:quit
HEREDOC
2>&1)
check "social_graph connect: intro sent to net" "CN1 = \[msg(alice, bob, intro(alice, alice" "$t12"
check "social_graph connect succeeds" "succeeds" "$t12"

# --- 14. social_graph - intro (clause 2) ---
echo "--- 14. social_graph: intro ---"
t13=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, intro(alice, alice, IRsp))], [(user, IU1), (net, IN1)]).
:quit
HEREDOC
2>&1)
check "social_graph intro: befriend sent to user" "IU1 = \[msg(agent, user, befriend(alice" "$t13"
check "social_graph intro succeeds" "succeeds" "$t13"

# --- 15. social_graph - decision(no) (clause 3) ---
echo "--- 15. social_graph: decision(no) ---"
t14=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(user, bob, decision(no, alice, DResp1?))], [(user, DU1), (net, DN1)]).
:quit
HEREDOC
2>&1)
check "social_graph decision(no) succeeds" "succeeds" "$t14"

# --- 16. social_graph - decision(yes) (clause 3) ---
echo "--- 16. social_graph: decision(yes) ---"
t15=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(user, bob, decision(yes, alice, DResp2?))], [(user, DU2), (net, DN2)]).
:quit
HEREDOC
2>&1)
check "social_graph decision(yes) suspended" "suspended" "$t15"

# --- 17. social_graph - multiple messages ---
echo "--- 17. social_graph: multi-message ---"
t16=$($DART run "$REPL" <<HEREDOC
$FILE
social_graph(bob, [msg(alice, bob, response(no)), msg(carol, bob, response(no))], [(user, MU1), (net, MN1)]).
:quit
HEREDOC
2>&1)
check "social_graph multi-message succeeds" "succeeds" "$t16"

# --- 18. agent_init ---
echo "--- 18. agent_init ---"
t17=$($DART run "$REPL" <<HEREDOC
$FILE
agent_init(alice, ch(AUIn?, AUOut), ch(ANIn?, ANOut)).
:quit
HEREDOC
2>&1)
check "agent_init suspended (waiting for input)" "suspended" "$t17"

# --- 19. bob_done (2 clauses) ---
echo "--- 19. bob_done ---"
t18=$($DART run "$REPL" <<HEREDOC
$FILE
bob_done([], BD1).
bob_done([x, y], BD2).
:quit
HEREDOC
2>&1)
check "bob_done empty" "BD1 = \[\]" "$t18"
check "bob_done skip items" "BD2 = \[\]" "$t18"

# --- 20. alice_done ---
echo "--- 20. alice_done ---"
t19=$($DART run "$REPL" <<HEREDOC
$FILE
alice_done([]).
:quit
HEREDOC
2>&1)
check "alice_done succeeds" "succeeds" "$t19"

# --- 21. play (full integration) ---
echo "--- 21. play (full integration) ---"
t20=$($DART run "$REPL" <<HEREDOC
$FILE
play.
:quit
HEREDOC
2>&1)
check "play runs (suspended = all processes active)" "suspended" "$t20"

# --- SUMMARY ---
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
