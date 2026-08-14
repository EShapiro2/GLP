#!/bin/bash
# Tests for the syntactically-grassroots checker (programs/jurix).
#
#   bash programs/jurix/test_jurix.sh
#
# The two contracts of /Grassroots/Jurix Sections 3.3 and 3.4, which Section 7
# certifies by hand, and four contracts broken in one place each.  Exits
# non-zero if any check fails.

set -u
GLP_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
JURIX="$GLP_DIR/programs/jurix/"
PASS=0
FAIL=0

check() {   # check <name> <expected substring> <output>
  if printf '%s' "$3" | grep -qF -- "$2"; then
    echo "  PASS  $1"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $1"
    echo "        expected: $2"
    FAIL=$((FAIL + 1))
  fi
}

check_not() {
  if printf '%s' "$3" | grep -qF -- "$2"; then
    echo "  FAIL  $1"
    echo "        unexpected: $2"
    FAIL=$((FAIL + 1))
  else
    echo "  PASS  $1"
    PASS=$((PASS + 1))
  fi
}

run() {     # run <goal> ... ; loads the program, then posts each goal
  local goals=""
  for g in "$@"; do goals="$goals$g\n"; done
  (cd "$GLP_DIR/glp_runtime" && printf "%b" "$JURIX\n$goals:quit\n" | bin/glpc 2>&1)
}

echo "=== jurix: the syntactically-grassroots checker ==="

# The two worked contracts, certified in Section 7.  One goal per run, so that
# each verdict is read off its own output.
out=$(run 'check_named(social_graph, V).' 'traceable_of(social_graph, E).')
check "the program loads" "Loaded program" "$out"
check_not "no type error" "Error loading" "$out"
check "social graph is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "friend, item and sent have traceable provenance" \
      "E = [friend, item, sent]" "$out"

out=$(run 'check_named(currency, V).' 'traceable_of(currency, E).')
check "currency is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "the coin has traceable provenance" \
      "E = [coin]" "$out"

# Befriend guarded in one role only: nothing is an introductory act, and
# befriend fails volition as well.
out=$(run 'check_named(sg_unguarded, V).')
check "one guard dropped: no introductory act" \
      "no_introductory_act" "$out"
check "one guard dropped: befriend fails volition" \
      "volition(befriend, 1, 2)" "$out"

# An act laying a friend atom without the friend's will obstructs befriend:
# clause 2 of def:unobstructed.
out=$(run 'check_named(sg_imposed, V).')
check "impose blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(impose, 1," "$out"
check "impose blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(impose, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"

# No mint: the swap requires at each role a coin no act of arity one supplies.
out=$(run 'check_named(cur_no_mint, V).')
check "no mint: the swap is unobtainable at role 1" \
      "obstructed(swap, 1, atom(coin, [pvar(u)]), unobtainable)" "$out"
check "no mint: the swap is unobtainable at role 2" \
      "obstructed(swap, 2, atom(coin, [pvar(v)]), unobtainable)" "$out"

# Minting a coin of another party's issue breaks provenance, and the acts
# guarded at one role that rest on it then fail volition.
out=$(run 'check_named(cur_loose_mint, V).' 'traceable_of(cur_loose_mint, E).')
check "loose mint: nothing has traceable provenance" "E = []" "$out"
check "loose mint: pay fails volition" "volition(pay, 1, 2)" "$out"
check "loose mint: redeem fails volition" "volition(redeem, 1, 2)" "$out"
check_not "loose mint: the swap is still unobstructed" "obstructed(swap" "$out"

# A contract with no schemas.
out=$(run 'check_named(nonesuch, V).')
check "the empty contract has no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
