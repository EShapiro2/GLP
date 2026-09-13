#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/jurix).
#
#   bash programs/jurix/test_jurix.sh
#
# The two contracts of /Grassroots/Jurix Sections 3.3 and 3.4, which Section 7
# certifies by hand, and four contracts broken in one place each; then the
# compilation of Section 5, against the two displays of Section 5.2.  Exits
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

check_eq() { # check_eq <name> <expected> <actual>
  if [ "$2" = "$3" ]; then
    echo "  PASS  $1"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $1"
    echo "        expected: $2"
    echo "        got:      $3"
    FAIL=$((FAIL + 1))
  fi
}

squash() { tr -d ' \t\n'; }

compiled() { # compiled <contract> <schema> ; the display, whitespace removed
  run "compile_schema($1, $2)." | sed -n '/begin{align/,/end{align/p' \
    | sed 's/GLP>//' | squash
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

# An act that adds the forbidden atom while giving the other party a role, and
# naming it in what it adds, is excused by clause 2 of def:unobstructed; what
# rejects it is volition.
out=$(run 'check_named(sg_imposed, V).')
check "impose is rejected by volition, not by clause 2" \
      "V = not_grassroots([volition(impose, 1, 2)])" "$out"
check_not "impose does not obstruct befriend" "obstructed" "$out"

# An act that adds it while sending no role to the other party is not excused:
# p is left holding friend(q) with q taking no part.
out=$(run 'check_named(sg_gossip, V).')
check "gossip blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1," "$out"
check "gossip blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"
check_not "gossip itself satisfies volition" "volition(gossip" "$out"

# Volition above arity two is connectedness of the role graph, not a test on
# every pair: a schema of arity four guarded at one role, whose role graph is
# the path 1-2-3-4, passes; cutting one edge of the path splits it.
out=$(run 'check_named(sg_chain, V).')
check "a path role graph at arity four certifies" \
      "V = syntactically_grassroots" "$out"

out=$(run 'check_named(sg_chain_cut, V).')
check "cutting an edge of the path fails volition" \
      "V = not_grassroots([volition(chain, 1, 3)])" "$out"

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

# CSSN's child-safe contract, eighteen schemas, transcribed from their entries
# of 2026-08-15 20:35 and 21:40 and 2026-08-16 12:37 UTC in
# Coordination/mail/Legal_inbox.md.  Its introductory act is parent_child;
# befriending is obstructed there by design.
out=$(run 'check_named(cssn, V).' 'traceable_of(cssn, E).')
check "CSSN's contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "CSSN's predicates of traceable provenance" \
      "E = [parenting, parent, child, friend, approval, withdrawal, member, listed, item, sent, posted, delivered]" \
      "$out"

# A contract with no schemas.
out=$(run 'check_named(nonesuch, V).')
check "the empty contract has no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

# --- the compilation (Section 5) -------------------------------------------
# The two displays of Section 5.2, transcribed from
# /Grassroots/Jurix/sections/05-compilation.tex, compared with the whitespace
# removed: the compiler emits a display one token to a line, GLP having no
# string concatenation, and a newline is whitespace to LaTeX.  The comma and
# the full stop that close the paper's two displays belong to the sentences
# around them, not to the compiled form, and are not expected here.

befriend_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{friend}(\Bob) \notin c_{\Alice} \text{ and } \mathit{friend}(\Alice) \notin c_{\Bob}, \qquad \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "befriend compiles to the display of Section 5.2" \
         "$befriend_paper" "$(compiled social_graph befriend)"

swap_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\text{\textcent}(u)\}) \uplus \{\text{\textcent}(v)\}, \qquad c'_{\Bob} := (c_{\Bob} \setminus \{\text{\textcent}(v)\}) \uplus \{\text{\textcent}(u)\},\\
& \text{provided } \text{\textcent}(u) \in c_{\Alice} \text{ and } \text{\textcent}(v) \in c_{\Bob}, \qquad \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "the swap compiles to the display of Section 5.2" \
         "$swap_paper" "$(compiled currency swap)"

# A contract compiles schema by schema, and only after it is checked.
out=$(run 'compile_named(social_graph).')
check_eq "the social graph compiles to four displays" "4" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"
out=$(run 'compile_named(currency).')
check_eq "the currency compiles to four displays" "4" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

out=$(run 'compile_named(sg_gossip).')
check "a contract that fails the conditions is not compiled" \
      "% not compiled: sg_gossip" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# A schema with no guarding role compiles to transactions with an empty guard
# (Section 5.1); CSSN's deliver is unguarded at both roles.
out=$(run 'compile_schema(cssn, deliver).')
check "an unguarded schema compiles to an empty guard" \
      "\\text{guarded by } \\emptyset" "$(printf '%s' "$out" | tr '\n' ' ')"

out=$(run 'compile_schema(cssn, nosuch).')
check "a schema the contract does not hold is reported" \
      "% no schema named" "$(printf '%s' "$out" | tr '\n' ' ')"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
