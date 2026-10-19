#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/gsc).
#
#   bash programs/gsc/test_gsc.sh
#
# GSC's contracts GSG, GSN and GSNP (programs/gsc/), GSNP being the social
# network with provenance of /Grassroots/GSC Section 3.3, which Section 7.1
# certifies by hand by definition:sg-agent-named (Appendix D.4), and the
# currency of Appendix B, which Appendix B.4 certifies
# (programs/currencies/coins/GC.glp), read from their owners' files, each
# schema listing its variables in its head (GSC 0228e79); and contracts of
# Section 3, over GSG, broken in one place each, one per way of failing the
# three conjuncts of def:syntactically-grassroots; then contracts declaring a
# predicate reflexive (the paragraph after def:binding); then schemas not
# formed as def:schema requires, refused, and a guarding role adding and
# deleting nothing, admitted; then the compilation
# of Section 5, against the display of befriend in Section 5.1 and that of the
# swap in Appendix B.3; then the contract of
# a grassroots federation, /Grassroots/GFWC sections/schemas.tex, read from
# programs/federation/GFWC.glp, against the three conditions of Appendix D
# of /Grassroots/GSC, and five contracts changing it in one place each, four
# against those and one against rootedness, which is none of them; then small
# contracts on the speech-act variables of traceable provenance and on
# volition by connected component, in the language of Appendix D; then the
# compilation of Appendix D, against its worked box, and the form of a
# schema there; then the signed
# speech-act term of Sections 3.1 to 3.3, on GSNP and on contracts refused
# for a signed term where def:schema forbids it; then
# contracts with wards, Appendix F, on a fixture of children and parents, on
# one of children and AI agents, on each broken in one place per clause, on
# one with a leaving mark and on it broken in one place per clause of
# def:leaving, on CSSN's and GAI's contracts, and on agent-named contracts
# with wards (def:sg-wards), with a seated role and with a listed role; then
# agent-named contracts with community roles (Appendix D.4), over GSNP,
# failing one clause of definition:sg-agent-named each, by a seated role and
# by a listed role, and one with a listed role guarded by a part of its
# extent; then listed roles formed otherwise than Appendix D's Definition
# Role allows, and schemas whose head is not their variables (def:schema).  A
# contract is its owner's and the checker
# reads its term (contracts.glp, the header): a fixture that changes one is a
# variant, given by name and the owner's term.  Exits non-zero if any check
# fails.

set -u
GLP_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
GSC_DIR="$GLP_DIR/programs/gsc/"
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

run() {     # run <goal> ... ; loads the program, then posts each goal
  local goals=""
  for g in "$@"; do goals="$goals$g\n"; done
  # The REPL's default reduction limit is 10000, which cs18's twenty-eight
  # schemas exceed; :limit is the REPL's knob for it.
  (cd "$GLP_DIR/glp_runtime" \
     && printf "%b" "$GSC_DIR\n:limit 1000000\n$goals:quit\n" | bin/glpc 2>&1)
}

term() {    # term <owner's file> <name> ; the contract's term, as it prints it
  (cd "$GLP_DIR/glp_runtime" \
     && printf '%s\ncontract_named(%s, C).\n:quit\n' "$1" "$2" | bin/glpc 2>&1) \
    | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //'
}

compiled() { # compiled <fixture> <schema> ; the display, whitespace removed
  run "compile_schema($1, $2)." | sed -n '/begin{align/,/end{align/p' \
    | sed 's/GLP>//' | squash
}

compiled_in() { # compiled_in <name> <term> <schema> ; the display of a contract given as a term
  run "compile_schema_in($1, $2, $3)." | sed -n '/begin{align/,/end{align/p' \
    | sed 's/GLP>//' | squash
}

echo "=== gsc: the syntactically-grassroots checker ==="

# The contracts, each read from its owner's file: one copy, which GSC's
# checker and vGLP's compiler load from there, each schema written
# schema(Name, Vars, Roles) or cschema(Name, Vars, Reaches, Roles), Vars its
# variables in the order of its head (GSC 0228e79, Definition Act schema).
# GSC's three, GSG, GSN and GSNP, each open with establish, GSG:1, which adds
# identity and requires and forbids nothing, and every other role of each
# requires identity.  SG is GSNP, Section 3.3's social network with
# provenance, an agent-named contract with the listed role of close
# (Appendix D.4, GSC 8064cfb); GSGT is GSG, a contract of Section 3, the
# contract the checks of Section 3 below are made on.
ESTABLISH="[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [])]), schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, [])], [atom(friend, [role(2)])])"
SG=$(term "$GLP_DIR/programs/gsc/GSNP.glp" gsnp)
check "GSNP's contract, the network with provenance, is read from programs/gsc/GSNP.glp" \
      "$ESTABLISH" "$SG"
GSGT=$(term "$GLP_DIR/programs/gsc/GSG.glp" gsg)
check "GSG's contract is read from programs/gsc/GSG.glp, establish first" "$ESTABLISH" "$GSGT"
GSNT=$(term "$GLP_DIR/programs/gsc/GSN.glp" gsn)
check "GSN's contract, the network without provenance, is read from programs/gsc/GSN.glp, establish first" \
      "$ESTABLISH" "$GSNT"
# GSG: befriend is unobstructed, identity being added by establish, of arity
# one, guarded and requiring and forbidding nothing (def:unobstructed, clause
# 1), and friend being added by befriend alone (clause 2); identity and friend
# have traceable provenance (def:grounded); unfriend's role Alice requires
# friend(Bob) (def:volition); so it is syntactically grassroots
# (def:syntactically-grassroots).  GSN (GSN.glp at 542ec8ca): a group is the
# community name <a>.n and close takes the listed role a[listed(*, n)],
# every member a lists (GSC 8064cfb, Appendix D's Definition Role), every
# name term based on the agent role a, so GSN is agent-named (Appendix D.4,
# definition:agent-named: every role an agent, a seated or a listed role) and
# is decided by definition:sg-agent-named: befriend, with agent roles and no
# name term, is unobstructed, identity from establish, of an agent role and
# no name term, and close's listed role adds no atom (clauses 1 and 2); its
# message, group and delivery schemas add atoms whose arguments are roles,
# the name <a>.n and speech-act variables no role requires or that an atom
# required at an agent role carries (clause 3); and every role graph is
# connected or has a guarding agent role in each component, close's two
# roles sharing the base a, the base of a listed role being its agent role
# (clause 4).  So it is syntactically grassroots by definition:sg-agent-named
# (Theorem "Grassroots realisation with seated and listed roles"), and not
# malformed: message's and post's guarding role adds and deletes nothing,
# which def:schema and Appendix D's Definition Act schema admit.
out=$(run "check($GSGT, V).")
check "GSG is syntactically grassroots" "V = syntactically_grassroots" "$out"
out=$(run "check($GSNT, V).")
check "GSN is syntactically grassroots by definition:sg-agent-named" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"
check_not "GSN's message and post, guarded at a role that adds and deletes nothing, are admitted" \
          "malformed(" "$out"
CUR=$(term "$GLP_DIR/programs/currencies/coins/GC.glp" gc)
check "the currency's contract, GC, is read from programs/currencies/coins/GC.glp" \
      "[schema(mint, [], [rs(guarded, [atom(coin, [role(1)])]" "$CUR"
CSSN=$(term "$GLP_DIR/programs/cssn/CSSN.glp" cssn)
check "CSSN's contract is read from programs/cssn/CSSN.glp" \
      "reflexive([friend], [schema(establish, []," "$CSSN"
check "and its close takes the listed role of the members the leader lists" \
      "crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [nterm(ext(own(1), n))])]" "$CSSN"
FED=$(term "$GLP_DIR/programs/federation/GFWC.glp" gfwc)
check "the federation's contract, GFWC, is read from programs/federation/GFWC.glp" \
      "[schema(form, [], [rs(guarded, [atom(seat, [nterm(own(1))])]" "$FED"
GAI=$(term "$GLP_DIR/programs/gai/GAI.glp" gai)
check "GAI's contract is read from programs/gai/GAI.glp" \
      "[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [atom(retired, [])])])" "$GAI"

# The two worked contracts, GSNP certified in Section 7.1 and the currency
# in Appendix B.4.  One goal per run, so that each verdict is read off its own
# output.  GSNP (GSNP.glp at 542ec8ca; GSC 8064cfb, Sections 3.3 and 7.1 and
# Appendix G): an agent-named contract, every name term <a>.n based on the
# agent role a, close taking the listed role a[listed(*, n)].  By
# definition:sg-agent-named: befriend, with agent roles and no name term, is
# unobstructed, identity from establish, friend added by befriend and no
# other schema, and close's listed role adds no atom (clauses 1 and 2);
# the set of all nine predicates has traceable provenance, each traced
# through an agent role (clause 3; Section 7.1, "So has the set of all nine
# predicates of the contract", and Appendix G, "each traced through an agent
# role"); and the role graphs meet clause 4, close's two roles having the
# base Alice.  So it is syntactically grassroots, as Section 7.1 says by
# Definition sg-agent-named, and its realisation volitionally grassroots by
# Theorem "Grassroots realisation with seated and listed roles".  Its signed
# terms, in
# forward, post_forward and deliver, are each of an agent role that requires
# an atom carrying the speech act (def:schema; Appendix D's Definition Act
# schema at GSC 6d4f147), deliver's of a role that does not guard; forward's
# and post's role Alice guards and adds and deletes nothing; so GSNP is not
# malformed.
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "the program loads" "Loaded program" "$out"
check_not "no type error" "Error loading" "$out"
check "GSNP is syntactically grassroots by definition:sg-agent-named" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"
check_not "GSNP's signed terms, deliver's among them, and its guarding roles that change nothing are admitted" "malformed(" "$out"
check "GSNP's nine predicates have traceable provenance" \
      "E = [identity, friend, item, authored, member, listed, closed, posted, delivered]" "$out"

out=$(run "check($CUR, V)." "traceable_preds($CUR, E).")
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

# The variants of Section 3 are made on GSG, a contract of Section 3.  An act
# that adds the forbidden atom while giving the other party a role, and
# naming it in what it adds, is excused by clause 2 of def:unobstructed; what
# rejects it is volition.
out=$(run "check_variant(sg_imposed, $GSGT, V).")
check "impose is rejected by volition, not by clause 2" \
      "V = not_grassroots([volition(impose, 1, 2)])" "$out"
check_not "impose does not obstruct befriend" "obstructed" "$out"

# An act that adds it while sending no role to the other party is not excused:
# p is left holding friend(q) with q taking no part.
out=$(run "check_variant(sg_gossip, $GSGT, V).")
check "gossip blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1," "$out"
check "gossip blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"
check_not "gossip itself satisfies volition" "volition(gossip" "$out"
# The same on GSNP, by clause 1 of definition:sg-agent-named, which asks
# def:unobstructed of the agent roles.
out=$(run "check_variant(sg_gossip, $SG, V).")
check "on GSNP gossip blocks befriend at both roles, by definition:sg-agent-named" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1, atom(friend, [pvar(u)]))), obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1, atom(friend, [pvar(u)])))])" \
      "$out"

# Volition above arity two is connectedness of the role graph, or a guarding
# role in every connected component of it, not a test on every pair: a schema
# of arity four guarded at one role, whose role graph is the path 1-2-3-4,
# passes; cutting one edge of the path splits it, and the half {3,4} holds no
# guarding role.
out=$(run "check_variant(sg_chain, $GSGT, V).")
check "a path role graph at arity four certifies" \
      "V = syntactically_grassroots" "$out"
out=$(run "check_variant(sg_chain, $SG, V).")
check "and on GSNP, by clause 4 of definition:sg-agent-named" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"

out=$(run "check_variant(sg_chain_cut, $GSGT, V).")
check "cutting an edge of the path fails volition" \
      "V = not_grassroots([volition(chain, 1, 3)])" "$out"

# Volition is one condition in Section 3 and Appendix D (GSC 7652554): tri(p?, q, r?)
# is guarded neither in all its roles nor connected, its role graph falling
# into {1,2} and {3}, each holding a guarding role, so it meets def:volition
# as it meets definition:volition as a cschema (sg_tri_cschema, below).
out=$(run "check_variant(sg_tri, $GSGT, V).")
check "a guarding role in every connected component meets volition in Section 3" \
      "V = syntactically_grassroots" "$out"

# No mint: the swap requires at each role a coin no act of arity one supplies.
out=$(run 'check_named(cur_no_mint, V).')
check "no mint: the swap is unobtainable at role 1" \
      "obstructed(swap, 1, atom(coin, [pvar(u)]), unobtainable)" "$out"
check "no mint: the swap is unobtainable at role 2" \
      "obstructed(swap, 2, atom(coin, [pvar(v)]), unobtainable)" "$out"

# Minting a coin of another party's issue breaks provenance, which the verdict
# names as the second conjunct of def:syntactically-grassroots, and the acts
# guarded at one role that rest on it then fail volition.
out=$(run 'check_named(cur_loose_mint, V).' 'traceable_of(cur_loose_mint, E).')
check "loose mint: nothing has traceable provenance" "E = []" "$out"
check "loose mint: the verdict names the coin as untraceable" \
      "untraceable([coin])" "$out"
check "loose mint: pay fails volition" "volition(pay, 1, 2)" "$out"
check "loose mint: redeem fails volition" "volition(redeem, 1, 2)" "$out"
check_not "loose mint: the swap is still unobstructed" "obstructed(swap" "$out"

# A speech act carried into an item from a required atom that has no
# traceable provenance, on GSNP, by clause 3 of definition:sg-agent-named
# (definition:provenance, each psi required at an agent role): the clause on
# speech-act variables drops tagged, its u named by no required atom, then
# item, tag's x being required only in tagged(x, u), then posted and
# delivered, post_own, post_forward and deliver adding a speech act their
# roles require only in an item or a posted atom; identity, friend,
# authored, sign adding authored(x) with x required nowhere, and the group's
# member, listed and closed keep it.  Befriend stays unobstructed and every
# schema meets clause 4, the edges resting on friend, member and listed.
# Clause 3 rejects the contract, and no other clause.
out=$(run "check_variant(sg_svar_loose, $SG, V)." "traceable_variant(sg_svar_loose, $SG, E).")
check "a speech act from an untraceable record: identity, friend, authored and the group's records keep provenance" \
      "E = [identity, friend, authored, member, listed, closed]" "$out"
check "and the verdict names clause 3 and no other" \
      "V = not_grassroots([untraceable([item, posted, delivered, tagged])])" "$out"

# cs18, the checker's fixture of eighteen schemas, twenty-eight expanded: a
# child-safe social contract as CSSN drafted it in Legal's inbox on 2026-08-15
# and 2026-08-16 and as it was brought to their paper on 2026-10-09, since
# revised by CSSN (its contract is programs/cssn/CSSN.glp, below).  Its
# introductory act is parent_child; befriending is obstructed there by design.
out=$(run 'check_named(cs18, V).' 'traceable_of(cs18, E).')
check "cs18 is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "cs18's predicates of traceable provenance" \
      "E = [parenting, parent, child, friend, approval, withdrawal, member, listed, item, sent, posted, delivered]" \
      "$out"

# send: the sender requires the item it sends and nothing else, the recipient
# that the sender is its friend.  Its display in the form of the swap's,
# Appendix B.3.
send_cs18=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{sent}(x,\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(x,a,\Alice)\},\\
& \text{provided } \mathit{item}(x,a,f) \in c_{\Alice} \text{ and } \mathit{friend}(\Alice) \in c_{\Bob},\\
& \text{guarded by } \{\Alice\}
\end{align*}
EOF
)
check_eq "cs18's send requires at the sender the item it sends and nothing else" \
         "$send_cs18" "$(compiled cs18 send)"

# friend is reflexive, and parent_child forbids parent(r) at the parent's role.
out=$(run 'contract_named(cs18, C).')
check "cs18 declares friend reflexive" \
      "C = reflexive([friend], [schema(parent_child," "$out"
parent_child_cs18=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{parenting}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{parent}(\Alice),\mathit{child}\},\\
& \text{provided } \mathit{parent}(\Bob) \notin c_{\Alice} \text{ and } \mathit{parent}(\Alice) \notin c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "cs18's parent_child forbids parent(r) at the parent's role" \
         "$parent_child_cs18" "$(compiled cs18 parent_child)"

# unfriend, child_unfriend, leave, child_leave and child_leave_2 each stand for
# one schema per role, guarded at that role, so the contract is twenty-eight
# schemas expanded, every one of them a willed act.  The one guarded at the
# role the table writes r is <schema>_r, and its display, in the form of
# the swap's display, Appendix B.3, is guarded by the person at that role alone; deliver is left the
# one schema no party guards.
out=$(run 'compile_named(cs18).')
check_eq "cs18 compiles to twenty-eight displays" "28" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"
check_eq "and deliver's is the one guarded by the empty set" "1" \
         "$(printf '%s' "$out" | grep -cF 'guarded by } \emptyset')"

IFS= read -r -d '' unfriend_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{friend}(\Bob) \in c_{\Alice} \text{ and } \mathit{friend}(\Alice) \in c_{\Bob},\\
& \text{guarded by }
EOF
IFS= read -r -d '' child_unfriend_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{friend}(\Alice)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\}, \qquad c'_{\mathit{Dave}} := c_{\mathit{Dave}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \{\mathit{parent}(\mathit{Carol}),\mathit{friend}(\Bob)\} \subseteq c_{\Alice} \text{ and } \{\mathit{parent}(\mathit{Dave}),\mathit{friend}(\Alice)\} \subseteq c_{\Bob} \text{ and } \mathit{parenting}(\Alice) \in c_{\mathit{Carol}} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Dave}},\\
& \text{guarded by }
EOF
IFS= read -r -d '' leave_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob},\\
& \text{guarded by }
EOF
IFS= read -r -d '' child_leave_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Carol}},\\
& \text{guarded by }
EOF
IFS= read -r -d '' child_leave_2_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\}, \qquad c'_{\mathit{Dave}} := c_{\mathit{Dave}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob} \text{ and } \mathit{parenting}(\Alice) \in c_{\mathit{Carol}} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Dave}},\\
& \text{guarded by }
EOF

expanded() { # expanded <schema> <role as the table writes it> <role's person> <body>
  check_eq "cs18's $1 is ${1%_*} guarded at $2 alone" \
           "$(printf '%s' "$4\\{$3\\}\\end{align*}" | squash)" "$(compiled cs18 "$1")"
}
expanded unfriend_p       p '\Alice'         "$unfriend_body"
expanded unfriend_q       q '\Bob'           "$unfriend_body"
expanded child_unfriend_r r '\Alice'         "$child_unfriend_body"
expanded child_unfriend_s s '\Bob'           "$child_unfriend_body"
expanded child_unfriend_p p '\mathit{Carol}' "$child_unfriend_body"
expanded child_unfriend_q q '\mathit{Dave}'  "$child_unfriend_body"
expanded leave_a          a '\Alice'         "$leave_body"
expanded leave_q          q '\Bob'           "$leave_body"
expanded child_leave_a    a '\Alice'         "$child_leave_body"
expanded child_leave_s    s '\Bob'           "$child_leave_body"
expanded child_leave_p    p '\mathit{Carol}' "$child_leave_body"
expanded child_leave_2_r  r '\Alice'         "$child_leave_2_body"
expanded child_leave_2_s  s '\Bob'           "$child_leave_2_body"
expanded child_leave_2_p  p '\mathit{Carol}' "$child_leave_2_body"
expanded child_leave_2_q  q '\mathit{Dave}'  "$child_leave_2_body"

# CSSN's contract, programs/cssn/CSSN.glp at 4cb1ec11 (CSSN, Appendix B.2):
# its groups are named <a>.n, and close and child_close take the listed role
# a[listed(*, n)], every member the leader lists, every name term based on
# the agent role a, so it is agent-named (GSC 8064cfb, Appendix D.4).  Its predicates have traceable provenance,
# each traced through an agent role, and it meets clause 4 of
# definition:sg-agent-named, and it has no unobstructed introductory act, so
# it is not syntactically grassroots by definition:sg-agent-named.  It has
# an introduction with wards (GSC, Appendix F, def:introduction-wards),
# checked by hand against GSC 429cfc8, and is syntactically grassroots with
# wards (def:sg-wards, GSC 503db66): befriend, its relay acts and establish
# have agent roles and no name term (clause 1); the listed roles of close
# and child_close delete member(<a>.n) and add nothing, member being no mark,
# leaving mark, friend, parent or parenting atom (clause 2); and it meets
# clauses 3 and 4 of definition:sg-agent-named (clause 3).  The introduction
# with wards:
# - the mark child, with the guardianship (child, identity, parent,
#   parenting) of def:guardianship: no schema deletes identity; enter and
#   enter_2, the schemas adding child, add identity and forbid it there; child
#   and parent atoms are added, and deleted (of_age, of_age_2,
#   parent_unfriend, parent_unfriend_2), as many times at every role; the
#   parent roles of enter and enter_2 require identity, forbid child and add
#   parenting of the child, and parent and parenting atoms are deleted in
#   pairs;
# - retired, a leaving mark for {child} (def:leaving): every guarding role of
#   the twenty-five schemas forbids it and none deletes it (clause 1);
#   parent_unfriend, the one schema adding it, deletes child at that role, so
#   requires a mark in M there (clause 2); enter and enter_2, the schemas
#   adding parent, forbid it at the parents' roles (clause 3);
# - befriend, unobstructed but for {child} (def:unobstructed-marks): identity
#   is added by establish, of arity one, guarded, requiring nothing and
#   forbidding nothing but retired, a leaving mark (clause 1); befriend
#   forbids child, a mark in M, retired, a leaving mark, and the friend atom
#   it adds (clause 2); every schema adds and deletes friend atoms in pairs
#   (clause 3);
# - child_adult_befriend, the relay act of a ward role of child and a free
#   role, and child_befriend, that of two ward roles of child, friend being
#   reflexive for the friend atom each parent requires of the other
#   (def:relay); each forbids nothing but child, retired and the friend atom
#   naming the other related role.
# So it is syntactically grassroots with wards and its verdict names the
# leaving mark retired: the protocol realising it is volitionally grassroots
# (Theorem "Grassroots realisation with wards and seated roles").  It is
# compiled, its realisation being taken by def:realisation (Appendix F,
# after def:introduction-wards).  The leaving marks for {child} are given
# with the guardianship of child taken, clause 3 of def:leaving reading its
# g (GSC 07d413c).
out=$(run "check($CSSN, V)." "leaving($CSSN, [child], L).")
check "CSSN's contract is syntactically grassroots with wards, retired its leaving mark" \
      "V = syntactically_grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
check "CSSN's leaving marks for {child} are [retired]" \
      "L = [leaving([guardianship(child, identity, parent, parenting)], [retired])]" "$out"
check_not "CSSN's predicates have traceable provenance" "untraceable(" "$out"
check_not "CSSN's contract satisfies volition" "volition(" "$out"
check_not "CSSN's contract is not malformed" "malformed(" "$out"
# One display per schema (Section 5.1, "A contract compiles schema by
# schema"): CSSN.glp's twenty-nine at 4cb1ec11, GSNP's sixteen and thirteen
# for children.
out=$(run "compile_contract(cssn, $CSSN).")
check_eq "and it compiles to twenty-nine displays" "29" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# --- reflexive predicates (the paragraph after def:binding) ----------------
# A contract may declare a predicate of arity one reflexive.  The social graph
# with friend declared so certifies as it does without.  These variants are
# made on GSG, a contract of Section 3.
out=$(run "check_variant(sg_reflexive, $GSGT, V).")
check "the social graph with friend reflexive is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"

# No schema adds or deletes, at a role, the atom of a reflexive predicate
# naming that role: a contract in which one does is refused as malformed,
# naming the schema, the role and the atom, and is not compiled.
out=$(run "check_variant(sg_refl_malformed, $GSGT, V).")
check "adding or deleting friend of its own role makes the contract malformed" \
      "V = malformed([reflexive(self_befriend, 1, atom(friend, [role(1)])), reflexive(self_unfriend, 1, atom(friend, [role(1)]))])" \
      "$out"
out=$(run "compile_variant(sg_refl_malformed, $GSGT).")
check "a malformed contract is not compiled" \
      "% not compiled: sg_refl_malformed is malformed" "$(printf '%s' "$out" | tr '\n' ' ')"
check "and the message names the fault, in the words of the paragraph after def:binding" \
      "self_befriend at role 1 adds or deletes the atom of a reflexive predicate naming that role; self_unfriend at role 1 adds or deletes the atom of a reflexive predicate naming that role; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# Clause 1 of def:unobstructed skips a required atom of a reflexive predicate
# naming the role: befriend requiring friend of each party at its own role is
# obstructed when friend is not reflexive and unobstructed when it is.
out=$(run "check_variant(sg_self_required, $GSGT, V).")
check "friend of its own role required, friend not reflexive: clause 1 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), unobtainable)])" \
      "$out"
out=$(run "check_variant(sg_refl_required, $GSGT, V).")
check "friend of its own role required, friend reflexive: clause 1 skips it" \
      "V = syntactically_grassroots" "$out"

# Clause 3 of def:unobstructed: the introductory act forbids at a role no atom
# of a reflexive predicate naming that role.
out=$(run "check_variant(sg_refl_forbidden, $GSGT, V).")
check "friend of its own role forbidden, friend reflexive: clause 3 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), reflexive), obstructed(befriend, 2, atom(friend, [role(2)]), reflexive)])" \
      "$out"

# A contract with no schemas.
out=$(run 'check_named(nonesuch, V).')
check "the empty contract has no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

# --- the form of a schema (def:schema) -------------------------------------
# def:schema at GSC bd0ebc4 requires +(i) and -(i) disjoint, and +(i) u -(i)
# nonempty at every role that is not a guarding role and at some role; a
# guarding role may add and delete nothing (the sentence after the example
# of def:schema).  GSNP's forward and post, and GSN's message and post, are
# guarded at a role that adds and deletes nothing and are admitted, above.  On
# GSG, a contract of Section 3, a role that does not guard and adds and
# deletes nothing, a schema no role of which adds or deletes an atom, and an
# atom added and deleted at one role are each refused, the contract
# malformed, and it is not compiled.  Appendix D's Definition Act schema
# requires the third and not the other two (the gf_ fixtures below).
out=$(run "check_variant(sg_inert_role, $GSGT, V).")
check "a role that does not guard and adds and deletes nothing is refused" \
      "V = malformed([changes_nothing(poke, 2)])" "$out"
out=$(run "check_variant(sg_inert_schema, $GSGT, V).")
check "a schema adding and deleting nothing at any role is refused, though its roles guard" \
      "V = malformed([no_role_changes(look)])" "$out"
out=$(run "check_variant(sg_added_deleted, $GSGT, V).")
check "an atom added and deleted at one role is refused" \
      "V = malformed([added_and_deleted(toggle, 1, atom(flag, []))])" "$out"
out=$(run "compile_variant(sg_inert_role, $GSGT)." "compile_variant(sg_inert_schema, $GSGT)." "compile_variant(sg_added_deleted, $GSGT).")
check "a contract refused for the form of a schema is not compiled, and the message says why, in the words of def:schema" \
      "% not compiled: sg_inert_role is malformed: poke at role 2 is not a guarding role and adds and deletes nothing; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
check "nor one whose schema changes nothing at any role" \
      "% not compiled: sg_inert_schema is malformed: look adds and deletes nothing at any role; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
check "nor one adding and deleting an atom at one role" \
      "% not compiled: sg_added_deleted is malformed: toggle at role 1 adds and deletes the same atom; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for any of them" "begin{align" "$out"

# --- the compilation (Section 5) -------------------------------------------
# The two displays of the paper, befriend's in Section 5.1, after
# def:compile, transcribed from /Grassroots/GSC/sections/05-compilation.tex
# at GSC aca046b, and the swap's in Appendix B.3, from
# sections/14-currency.tex at the same commit, compared with the whitespace
# removed: the compiler emits a display one token to a line, GLP having no
# string concatenation, and a newline is whitespace to LaTeX.  The comma and
# the full stop that close the paper's two displays belong to the sentences
# around them, not to the compiled form, and are not expected here.  The
# layout is Udi's ruling (GSC #3 to Code, 2026-10-18 21:49 UTC), which both
# displays print: the assignments, the required atoms on one proviso line and
# the forbidden atoms on the next, each in role order, a line omitted when it
# has no atom, and the guard on a line of its own.  Befriend is GSNP's, which
# requires identity at both roles (GSG:1), and GSG's, which is the same
# schema; the swap is GC's.
befriend_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{identity} \in c_{\Alice} \text{ and } \mathit{identity} \in c_{\Bob},\\
& \text{provided } \mathit{friend}(\Bob) \notin c_{\Alice} \text{ and } \mathit{friend}(\Alice) \notin c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "GSNP's befriend compiles to the display of Section 5.1" \
         "$befriend_paper" "$(compiled_in gsnp "$SG" befriend)"
check_eq "and so does GSG's" \
         "$befriend_paper" "$(compiled_in gsg "$GSGT" befriend)"

swap_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\text{\textcent}(u)\}) \uplus \{\text{\textcent}(v)\}, \qquad c'_{\Bob} := (c_{\Bob} \setminus \{\text{\textcent}(v)\}) \uplus \{\text{\textcent}(u)\},\\
& \text{provided } \text{\textcent}(u) \in c_{\Alice} \text{ and } \text{\textcent}(v) \in c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "the swap compiles to the display of Appendix B.3" \
         "$swap_paper" "$(compiled_in gc "$CUR" swap)"

# A contract compiles schema by schema (Section 5.1), and only after it is
# checked: GSNP's sixteen schemas, establish, befriend, unfriend, sign,
# message and forward of Section 3.3's table and the ten of a group of
# Appendix G, to sixteen displays.
out=$(run "compile_contract(gsnp, $SG).")
check_eq "GSNP compiles to sixteen displays" "16" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"
out=$(run "compile_contract(gc, $CUR).")
check_eq "the currency compiles to four displays" "4" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

out=$(run "compile_variant(sg_gossip, $GSGT).")
check "a contract that fails the conditions is not compiled" \
      "% not compiled: sg_gossip" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# A schema with no guarding role compiles to transactions with an empty guard
# (Section 5.1); cs18's deliver is unguarded at both roles.
out=$(run 'compile_schema(cs18, deliver).')
check "an unguarded schema compiles to an empty guard" \
      "\\text{guarded by } \\emptyset" "$(printf '%s' "$out" | tr '\n' ' ')"

out=$(run 'compile_schema(cs18, nosuch).')
check "a schema the contract does not hold is reported" \
      "% no schema named" "$(printf '%s' "$out" | tr '\n' ' ')"


# --- the contract of a grassroots federation (Appendix D) -------------------
# GFWC's five schemas over seat and child, /Grassroots/GFWC
# sections/schemas.tex, every role of them a party role or a seated role,
# certified in its Section 3.1 by hand against the conditions of Appendix D of
# /Grassroots/GSC; the checker returns the same.

out=$(run "check($FED, V)." "rooted_preds($FED, E).")
check "the federation contract meets the conditions of Appendix D" \
      "V = conditions_met" "$out"
check "seat is rooted" "E = [seat]" "$out"

out=$(run "traceable_preds($FED, E).")
check "seat and child have traceable provenance" "E = [seat, child]" "$out"

# Seating the assembly in the name of the community itself: the name argument
# is of least rank zero and at no party role.  Rootedness is no condition on
# the text.
out=$(run "check_variant(gf_unrooted, $FED, V)." "rooted_variant(gf_unrooted, $FED, E).")
check "the community named without its decision: seat is not rooted" \
      "E = []" "$out"
check "and the three conditions on the text are still met" \
      "V = conditions_met" "$out"

# A note of a community that is the name term of no role and that no required
# atom names: note loses traceable provenance, seat and child keep it, and the
# set of predicates occurring in the contract lacks it, the one condition of
# the three the contract fails.
out=$(run "traceable_variant(gf_untraceable, $FED, E).")
check "a note nothing traces: seat and child keep traceable provenance" \
      "E = [seat, child]" "$out"
check_not "and note does not have it" "note" "$out"
out=$(run "check_variant(gf_untraceable, $FED, V).")
check "and the contract fails traceable provenance, naming note" \
      "V = conditions_failed([untraceable([note])])" "$out"

# Seating the assembly in a community that is the name term of no role of the
# schema.  seat is the one role predicate of the contract, every role being a
# party role or a seated role, so cohesion asks its question of seat atoms
# (definition:cohesive).  The same atom costs seat its traceable provenance,
# so the verdict names seat as untraceable before the cohesion fault.
out=$(run "check_variant(gf_uncohesive, $FED, V).")
check "a seat atom of no role of the schema fails cohesion, and seat traceable provenance" \
      "V = conditions_failed([untraceable([seat]), cohesion(federate, 1, atom(seat, [nterm(nvar(eta))]))])" \
      "$out"

# Cohesion alone (Legal 2026-10-09 18:31 UTC): the federation with one schema
# more, seat_child, whose seated role requires child(zeta, eta) and seats its
# assembly in eta.  eta is the name term of no role of it, so cohesion fails;
# it is an argument of the required child atom, so seat keeps traceable
# provenance; the role graph is one role, so volition holds.  seat stops
# being rooted, which is no condition.
out=$(run "check_variant(gf_cohesion_only, $FED, V)." "traceable_variant(gf_cohesion_only, $FED, T)." \
          "rooted_variant(gf_cohesion_only, $FED, E).")
check "a seat atom in a child the role requires fails cohesion alone" \
      "V = conditions_failed([cohesion(seat_child, 1, atom(seat, [nterm(nvar(eta))]))])" \
      "$out"
check "and seat and child keep traceable provenance" "T = [seat, child]" "$out"
check "and seat is not rooted, which is no condition" "E = []" "$out"

# A join no assembly decides: its two roles are the assemblies of the two
# communities, nothing joins them, and neither guards.
out=$(run "check_variant(gf_novolition, $FED, V).")
check "a join with no guarding role fails volition" \
      "V = conditions_failed([volition(join, 1, 2)])" "$out"

# The speech-act variables of definition:provenance.  The first clause asks
# after the arguments of an added atom outside Y, the second after every
# speech-act variable that is an argument of it or occurs in a name term among
# its arguments.  sv_signed is the smallest case on which it matters that the
# first clause leaves Y out: a speech act signed and required nowhere.  The
# sv_ contracts are agent-named, of agent roles, and are decided by
# definition:sg-agent-named, whose clause 3 is definition:provenance with
# each psi required at an agent role; none has an introductory act.
out=$(run 'check_named(sv_signed, V).' 'traceable_of(sv_signed, E).')
check "a signed speech act required nowhere: item has traceable provenance" \
      "E = [item, got, mark]" "$out"
check "and the item take requires joins its role graph, no introductory act failing it" \
      "V = not_grassroots([no_introductory_act])" "$out"

# The case of sg_svar_loose in the language of Appendix D: the same speech act
# carried from an untraceable record.  The verdict names item and tagged as
# untraceable, as sg_svar_loose's names its own, before the volition fault.
out=$(run 'check_named(sv_loose, V).' 'traceable_of(sv_loose, E).')
check "a speech act from an untraceable record: item loses traceable provenance" \
      "E = [got, mark]" "$out"
check "and the verdict names item and tagged, and take, resting on item, fails volition" \
      "V = not_grassroots([no_introductory_act, untraceable([item, tagged]), volition(take, 1, 2)])" "$out"

# The name-term case of the second clause, on the side of the added atom and
# on the side of the required one.
out=$(run 'traceable_of(sv_in_name_loose, E).')
check "a speech act in a name term, from an untraceable record: club loses it" \
      "E = []" "$out"

out=$(run 'traceable_of(sv_in_name_carried, E).')
check "a speech act carried from a name term of a required atom: cited keeps it" \
      "E = [club, cited]" "$out"

# Volition by connected component (definition:volition): relay's role graph
# is the path 1-2-3 and its seated role alone, each holding a guarding role,
# and role 3 has no edge to one; relay_unguarded is relay with its seated role
# unguarded.  Over the social graph, whose predicates keep in the language of
# Appendix D the traceable provenance Section 3 gives them.
# The Appendix D variants take the social graph without the signed term, the
# fixture sg_plain, a contract of Section 3.  relay's seated role is of a
# name variable, so sg_relay is not agent-named and is decided by the three
# conditions of Appendix D.
SGP=$(run 'contract_named(sg_plain, C).' | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check "the social graph without the signed term, sg_plain, is read" \
      "[schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])]" "$SGP"
out=$(run "check_variant(sg_relay, $SGP, V)." "traceable_variant(sg_relay, $SGP, E).")
check_not "a guarding role in every connected component meets volition" \
      "volition(relay," "$out"
check "and a component holding none fails it" \
      "V = conditions_failed([volition(relay_unguarded, 1, 4)])" "$out"
check "the social graph keeps its traceable provenance in Appendix D" \
      "E = [friend, item, sent, chained, noted]" "$out"

# sg_tri with tri written as a cschema, in the language of Appendix D, of
# agent roles and no name term, so agent-named: clause 4 of
# definition:sg-agent-named holds of tri, each component holding a guarding
# agent role, as def:volition holds of it in Section 3.
out=$(run "check_variant(sg_tri_cschema, $SGP, V).")
check "tri as a cschema meets definition:sg-agent-named, as it meets def:syntactically-grassroots in Section 3" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"

# Rootedness is a notion of Appendix D; a contract of Section 3 is not asked
# about it, and keeps the verdict of def:syntactically-grassroots.
out=$(run "rooted_preds($GSGT, E)." "check($GSGT, V).")
check "a contract of Section 3 is not asked which predicates are rooted" \
      "E = []" "$out"
check "and keeps the verdict of Section 3" \
      "V = syntactically_grassroots" "$out"

# The compiled form of a schema with community roles is Definition Compilation
# of Appendix D, which the compiler prints for a contract that meets the
# conditions on the text and for no other.
out=$(run "compile_contract(gfwc, $FED).")
check "a contract with community roles is compiled" \
      "begin{align" "$out"
check_not "and not refused" "not compiled" "$out"
check_not "no role of the federation is a constituent role" '\ast}' "$out"

out=$(run "compile_variant(gf_novolition, $FED).")
check "a contract that fails the conditions of Appendix D is not compiled" \
      "% not compiled: gf_novolition" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# --- the compilation of Appendix D (definition:compile) ---------------------
# The worked box of Appendix D, transcribed from
# /Grassroots/GSC/sections/13-community-roles.tex, is the display for
# federate: the one assignment over the extent of its seated role, adding both
# atoms, the line on freshness and the guard.  Compared with the whitespace
# removed, as the displays of Section 5.1 and Appendix B.3 are, and without the full stop
# that closes the box's sentence.  The box is read by read, not by a here
# document inside $( ): its one c'_p is a lone quote, which bash 3.2, the
# system shell of macOS, takes to open a string there and never close.

IFS= read -r -d '' federate_tex <<'EOF'
\begin{align*}
& c'_p := c_p \uplus \{\mathit{child}(\zeta\cdot y,\zeta),\ \mathit{seat}(\zeta\cdot y)\} && (p\in\mathrm{ext}_c(\zeta^{\mathit{seat}})),\\
& \text{provided } \zeta\cdot y \text{ is an argument of no atom of } c,\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\zeta^{\mathit{seat}}) \text{ with } |G|>\theta|\mathrm{ext}_c(\zeta^{\mathit{seat}})|
\end{align*}
EOF
federate_box=$(printf '%s' "$federate_tex" | squash)
check_eq "federate compiles to the worked box of Appendix D" \
         "$federate_box" "$(compiled_in gfwc "$FED" federate)"

# The five schemas of the contract, in the one form of Appendix D.
out=$(run "compile_contract(gfwc, $FED).")
check_eq "the federation compiles to five displays" "5" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# form: a party role in the form of Appendix D, over its extent, with a proviso
# line for the seat it forbids, guarded by a part of it larger than the
# threshold 0; no name term sigma.y, so no line on freshness.
form=$(compiled_in gfwc "$FED" form)
check "form ranges over the extent of its party role" \
      "(p\\in\\mathrm{ext}_c(\\Alice))" "$form"
check "form forbids the seat it adds" \
      "\\text{provided}\\mathit{seat}(\\langle\\Alice\\rangle)\\notinc_p&&(p\\in\\mathrm{ext}_c(\\Alice))" \
      "$form"
check "form's guard is a part of that extent larger than 0 of it" \
      "\\text{guardedby}G,\\G\\subseteq\\mathrm{ext}_c(\\Alice)\\text{with}|G|>0|\\mathrm{ext}_c(\\Alice)|" \
      "$form"
check_not "form forms no name and prints no freshness line" \
      "noatomof" "$form"

# join: an assignment line and a proviso line per role, the reach condition,
# and a guard that is the union of two parts, each at its own threshold.
join=$(compiled_in gfwc "$FED" join)
check_eq "join prints an assignment line per role" "2" \
         "$(printf '%s' "$join" | grep -o "c'_p:=" | wc -l | tr -d ' ')"
check_eq "join prints a proviso line per role" "2" \
         "$(printf '%s' "$join" | grep -o '\\notinc_p' | wc -l | tr -d ' ')"
check "join prints its reach condition" \
      "\\text{provided}\\xi\\not\\rightsquigarrow_{\\mathit{child}}\\zeta" "$join"
check "join's guard is the union of two parts" \
      "\\text{guardedby}G_{1}\\cupG_{2},\\G_{1}\\subseteq\\mathrm{ext}_c(\\zeta^{\\mathit{seat}})\\text{with}|G_{1}|>\\theta|\\mathrm{ext}_c(\\zeta^{\\mathit{seat}})|\\text{and}G_{2}\\subseteq\\mathrm{ext}_c(\\xi^{\\mathit{seat}})\\text{with}|G_{2}|>\\theta|\\mathrm{ext}_c(\\xi^{\\mathit{seat}})|" \
      "$join"

# leave_1 and leave_2 differ only in which assembly guards.
check "leave_1 is guarded by a supermajority of the parent's assembly" \
      "\\text{guardedby}G,\\G\\subseteq\\mathrm{ext}_c(\\zeta^{\\mathit{seat}})" \
      "$(compiled_in gfwc "$FED" leave_1)"
check "leave_2 is guarded by a supermajority of the child's assembly" \
      "\\text{guardedby}G,\\G\\subseteq\\mathrm{ext}_c(\\xi^{\\mathit{seat}})" \
      "$(compiled_in gfwc "$FED" leave_2)"

# The form of a schema in Appendix D: its Definition Act schema requires
# +(i) and -(i) disjoint, and nothing of +(i) u -(i).  gf_inert_seated's
# confirm has a seated role, xi^seat, that adds and deletes nothing: the
# contract is formed, meets the three conditions on the text, and compiles,
# by definition:compile at GSC aca046b, to the assignment of each role over
# its extent; the required atoms of each role in role order, neither
# forbidding any; the proviso c'_p \neq c_p over the extent of xi^seat, which
# is no guarding agent role and whose state the other provisos do not change;
# and the guard.  An atom added and deleted at a seated role is refused,
# gf_added_deleted malformed.
out=$(run "check_variant(gf_inert_seated, $FED, V).")
check "a seated role adding and deleting nothing is admitted in Appendix D" \
      "V = conditions_met" "$out"
IFS= read -r -d '' confirm_tex <<'TEX'
\begin{align*}
& c'_p := c_p \uplus \{\mathit{confirmed}(\zeta,\xi)\} && (p\in\mathrm{ext}_c(\zeta^{\mathit{seat}})),\\
& c'_p := c_p && (p\in\mathrm{ext}_c(\xi^{\mathit{seat}})),\\
& \text{provided } \mathit{child}(\zeta,\xi) \in c_p && (p\in\mathrm{ext}_c(\zeta^{\mathit{seat}})),\\
& \text{provided } \mathit{child}(\zeta,\xi) \in c_p && (p\in\mathrm{ext}_c(\xi^{\mathit{seat}})),\\
& \text{provided } c'_p \neq c_p && (p\in\mathrm{ext}_c(\xi^{\mathit{seat}})),\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\zeta^{\mathit{seat}}) \text{ with } |G|>\theta|\mathrm{ext}_c(\zeta^{\mathit{seat}})|
\end{align*}
TEX
confirm_out=$(run "compile_schema_variant(gf_inert_seated, $FED, confirm)." \
  | sed -n '/begin{align/,/end{align/p' | sed 's/GLP>//' | squash)
check_eq "and it compiles with the proviso on the state of the seated role that changes nothing" \
         "$(printf '%s' "$confirm_tex" | squash)" "$confirm_out"
out=$(run "check_variant(gf_added_deleted, $FED, V).")
check "an atom added and deleted at a seated role is refused" \
      "V = malformed([added_and_deleted(reseat, 1, atom(seat, [nterm(nvar(zeta))]))])" "$out"

# --- the signed speech-act term (Sections 3.1 to 3.3) ----------------------
# GSNP, Section 3.3's network with provenance, item of arity one, forward
# adding item(sgn(x, Alice)) at Bob, post_forward adding
# posted(sgn(x, Bob), n) at the leader and deliver adding item(sgn(y, Alice))
# at the member.  Section 7.1 certifies it, each signed term counting as its
# two arguments, the speech act and the role (definition:provenance; GSC
# 6d4f147, Appendix D admitting the signed terms of Section 3).
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "GSNP, carrying signed terms in forward, post_forward and deliver, is syntactically grassroots by definition:sg-agent-named" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"
check "item, authored, posted and delivered have traceable provenance there with the rest" \
      "E = [identity, friend, item, authored, member, listed, closed, posted, delivered]" "$out"

# The form of a display (compile.glp): a schema of Section 3's language --
# agent roles, no reach condition, no name term -- in the form of Section
# 5.1, befriend's (above), as Section 5.1 prints befriend of Section 3.3;
# any other in the form of Appendix D's worked box, by
# definition:compile.  The compiler names role i by its index, role 1 \Alice
# and role 2 \Bob, so post's and post_forward's member, whom Section 3.3's
# table names Bob, is \Alice in their displays.
#
# forward(Alice?, Bob; x), Section 3.3's table at GSC 07d413c, of Section 3's
# language: at Alice, a guarding role adding and deleting nothing,
# c'_Alice := c_Alice; at Bob the signed term added; the required atoms,
# identity and item(x) at Alice and identity and friend(Alice) at Bob, on one
# proviso line, the atom Alice forbids, authored(x), on the next; and the
# guard {Alice} (def:compile).
forward_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(\mathrm{sgn}(x,\Alice))\},\\
& \text{provided } \{\mathit{identity},\mathit{item}(x)\} \subseteq c_{\Alice} \text{ and } \{\mathit{identity},\mathit{friend}(\Alice)\} \subseteq c_{\Bob},\\
& \text{provided } \mathit{authored}(x) \notin c_{\Alice},\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "forward compiles with the signed term and authored(x) forbidden at the forwarder" \
         "$forward_gsnp" "$(compiled_in gsnp "$SG" forward)"

# post(p?, a ; x, n), its role 1 the member posting their own item and role
# 2 the leader (Appendix G at GSC 6d4f147), carries the name term <a>.n and
# is printed by definition:compile: at the member, guarding and adding and
# deleting nothing, c'_p := c_p; at the leader posted(x, n) added, the item
# as its author signed it; the required atoms of each role over its extent;
# no freshness line, <a>.n occurring in member(<a>.n), which both roles
# require; and the guard, a part of the member's extent larger than 0 of it.
post_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \uplus \{\mathit{posted}(x,n)\} && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \{\mathit{identity},\ \mathit{authored}(x),\ \mathit{member}(\langle\Bob\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Bob\rangle\cdot n),\ \mathit{listed}(\Alice,n),\ \mathit{friend}(\Alice)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\Alice) \text{ with } |G|>0|\mathrm{ext}_c(\Alice)|
\end{align*}
TEX
)
check_eq "post compiles by definition:compile, its guarding role changing nothing" \
         "$post_gsnp" "$(compiled_in gsnp "$SG" post)"

# post_forward(p?, a ; x, n): an item the member received, posted with their
# signature added, sgn(x, Alice), and authored(x) forbidden at the member.
post_forward_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \uplus \{\mathit{posted}(\mathrm{sgn}(x,\Alice),n)\} && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \{\mathit{identity},\ \mathit{item}(x),\ \mathit{member}(\langle\Bob\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Bob\rangle\cdot n),\ \mathit{listed}(\Alice,n),\ \mathit{friend}(\Alice)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \mathit{authored}(x) \notin c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\Alice) \text{ with } |G|>0|\mathrm{ext}_c(\Alice)|
\end{align*}
TEX
)
check_eq "post_forward compiles with the member's signed term" \
         "$post_forward_gsnp" "$(compiled_in gsnp "$SG" post_forward)"

# deliver(a, q ; y, n): the leader Alice adds delivered(y, Bob, n), the
# member Bob the leader's signed term item(sgn(y, Alice)); the required
# atoms of each role, then what Alice forbids, authored(y) and
# delivered(y, Bob, n); and the empty guard, no role of deliver guarding.
# The signed term's role does not guard, and def:schema admits it, Alice
# requiring posted(y, n).
deliver_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := c_p \uplus \{\mathit{delivered}(y,\Bob,n)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \uplus \{\mathit{item}(\mathrm{sgn}(y,\Alice))\} && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n),\ \mathit{posted}(y,n),\ \mathit{listed}(\Bob,n),\ \mathit{friend}(\Bob)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \{\mathit{authored}(y),\ \mathit{delivered}(y,\Bob,n)\} \cap c_p = \emptyset && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{guarded by } \emptyset
\end{align*}
TEX
)
check_eq "deliver compiles with the leader's signed term and an empty guard" \
         "$deliver_gsnp" "$(compiled_in gsnp "$SG" deliver)"

# create(a? ; n) forms the name <a>.n, which occurs in no atom any role
# requires, so definition:compile at GSC 6d4f147 asks that it be an argument
# of no atom of c; join requires member(<a>.n) at the leader, so join asks
# it not.
# (read, not cat in a command substitution: its one apostrophe would open a
# quote to the shell's scan of the substitution)
IFS= read -r -d '' create_tex <<'TEX'
\begin{align*}
& c'_p := c_p \uplus \{\mathit{member}(\langle\Alice\rangle\cdot n),\ \mathit{listed}(\Alice,n)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \mathit{identity} \in c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{member}(\langle\Alice\rangle\cdot n),\ \mathit{closed}(n)\} \cap c_p = \emptyset && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \langle\Alice\rangle\cdot n \text{ is an argument of no atom of } c,\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\Alice) \text{ with } |G|>0|\mathrm{ext}_c(\Alice)|
\end{align*}
TEX
check_eq "create compiles with the freshness of the name it forms" \
         "$(printf '%s' "$create_tex" | squash)" "$(compiled_in gsnp "$SG" create)"
check_not "join, requiring member(<a>.n), asks no freshness of it" \
          "noatomof" "$(compiled_in gsnp "$SG" join)"

# close(a?, a[listed(*, n)] ; n): the leader adds closed(n) and deletes
# listed(a, n); every agent of the listed role's extent, the members a lists,
# deletes member(<a>.n) (Appendix G at GSC 8064cfb); the listed role written
# as Appendix D's Definition Role and Appendix G write it,
# \Alice[\mathit{listed}(\ast,n)]; no freshness line, member(<a>.n) being
# required at the listed role; and the guard, a part of the leader's extent.
# GSN's close is GSNP's.
close_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := (c_p \setminus \{\mathit{listed}(\Alice,n)\}) \uplus \{\mathit{closed}(n)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{provided } \{\mathit{identity},\ \mathit{listed}(\Alice,n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\Alice) \text{ with } |G|>0|\mathrm{ext}_c(\Alice)|
\end{align*}
TEX
)
check_eq "close compiles with the listed role of the members the leader lists" \
         "$close_gsnp" "$(compiled_in gsnp "$SG" close)"
check_eq "and GSN's close compiles to the same display" \
         "$close_gsnp" "$(compiled_in gsn "$GSNT" close)"

# An introduction through the signed term alone: each role of exchange adds
# an atom naming the other only as the role of its signed term (Section 3.1;
# Appendix D, "names"), so it is an introductory act; with plain speech-act
# variables in place of the signed terms nothing names the other party.
out=$(run 'check_named(sgn_intro, V).' 'traceable_of(sgn_intro, E).')
check "an act naming the other party by a signed term is an introductory act" \
      "V = syntactically_grassroots" "$out"
check "and the atom carrying the signed term has traceable provenance" \
      "E = [item, got]" "$out"
out=$(run 'check_named(sgn_intro_plain, V).')
check "with plain speech-act variables it names nobody and there is no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

# def:schema (GSC 9e42d53): a signed term occurs only in an atom marked +,
# and its speech-act variable occurs in an atom its role requires, whether or
# not that role guards; a schema violating either is refused, the contract
# malformed.  sgn_other_role signs x by Bob, who requires no atom with x among
# its arguments, Alice being the role that requires item(x); sgn_not_required
# signs x by the one role of sign, which requires nothing.  sgn_unguarded_signer
# signs x by Alice, who requires item(x) and does not guard, and is admitted:
# its verdict is the conditions', no introductory act, and not malformed.
out=$(run 'check_named(sgn_not_added, V).')
check "a signed term in an atom not marked + is refused" \
      "V = malformed([signed(forward, 2, atom(item, [sgn(x, 1)]), not_added)])" "$out"
out=$(run 'check_named(sgn_other_role, V).')
check "a signed term whose role requires no atom with its speech-act variable is refused" \
      "V = malformed([signed(forward, 2, atom(item, [sgn(x, 2)]), not_required)])" "$out"
out=$(run 'check_named(sgn_not_required, V).')
check "a signed term whose speech-act variable no role requires is refused" \
      "V = malformed([signed(sign, 1, atom(item, [sgn(x, 1)]), not_required)])" "$out"
out=$(run 'check_named(sgn_unguarded_signer, V).')
check "a signed term of a role that requires its speech act and does not guard is admitted" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'compile_named(sgn_not_required).')
check "and a contract refused for its signed term is not compiled" \
      "% not compiled: sgn_not_required is malformed" "$(printf '%s' "$out" | tr '\n' ' ')"
check "and the message names the fault, in the words of def:schema" \
      "sign at role 1 carries a signed term whose speech-act variable occurs in no atom its role requires; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"

# Appendix D's Definition Act schema admits the signed terms of Section 3,
# sgn(y, pi) for pi an agent role, with def:schema's requirements (GSC
# 6d4f147): sgn_community, sign written as a cschema, is admitted, and has no
# introductory act; sgn_seated signs at a seated role and is refused.
out=$(run 'check_named(sgn_community, V).')
check "a signed term of an agent role in a contract with community roles is admitted" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'check_named(sgn_seated, V).' 'compile_named(sgn_seated).')
check "a signed term of a seated role is refused" \
      "V = malformed([signed(note, 2, atom(got, [sgn(x, 2)]), not_agent_role)])" "$out"
check "and the message names the fault, in the words of Appendix D's Definition Act schema" \
      "note at role 2 carries a signed term whose role is not an agent role; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"

# The speech-act variable of a signed term is put to the clause of
# def:grounded on speech-act variables: carried from a required atom of no
# traceable provenance, the item loses it, and sent after it.
out=$(run 'check_named(sg_sgn_loose, V).' 'traceable_of(sg_sgn_loose, E).')
check "a signed speech act from an untraceable record: only friend keeps provenance" \
      "E = [friend]" "$out"
check "and the verdict is the second conjunct alone" \
      "V = not_grassroots([untraceable([item, sent, tagged])])" "$out"

# --- contracts with wards (Appendix F) -------------------------------------
# wd, children and their parents in the schemas Appendix F describes: not
# syntactically grassroots, and grassroots with wards (def:introduction-wards,
# thm:grassroots-wards) through befriend, the mark child, the guardianship
# (child, identity, parent, parenting), child_adult_befriend and
# child_befriend; and it has no leaving mark (def:leaving): no mark of it
# other than child is forbidden at every guarding role (clause 1).  wards_of
# gives what the search found,
# whatever the verdict, and guardianships the guardianships for a set of
# marks.
wd_found="introduction_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])"
out=$(run 'check_named(wd, V).' 'wards_of(wd, W).' 'contract_named(wd, C), guardianships(C?, [child], G).')
check "wd is grassroots with wards, with no leaving mark" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])" \
      "$out"
check "wards_of gives its introduction with wards" "W = $wd_found" "$out"
check "guardianships gives child's" \
      "G = [guardianship(child, identity, parent, parenting)]" "$out"
out=$(run 'compile_named(wd).')
check_eq "a contract grassroots with wards is compiled, wd to eight displays" "8" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# def:guardianship, one fixture per clause: each leaves child no
# guardianship, and the verdict is that of Section 3.
no_guardianship() { # no_guardianship <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), guardianships(C?, [child], G).")
  check "$1: $2, so child has no guardianship" "G = []" "$out"
  check "$1: and no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable)," "$out"
}
no_guardianship wd_iota_deleted "a schema deletes identity (clause 1)"
no_guardianship wd_no_iota      "adopt adds child without adding and forbidding identity (clause 2)"
no_guardianship wd_no_g         "enter adds child and no parent atom (clause 3)"
no_guardianship wd_no_w         "enter adds parent(p) and no parenting(r) (clause 4)"
no_guardianship wd_sigma_marked "enter's parent does not forbid child (clause 4)"
no_guardianship wd_del_half     "of_age deletes parent(p) and not parenting(r) (clause 4)"

# def:unobstructed-marks and def:relay, one fixture per clause: the
# guardianship stands and the introduction with wards fails.
no_wards() { # no_wards <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), guardianships(C?, [child], G).")
  check "$1: child keeps its guardianship" "G = [guardianship(child, identity, parent, parenting)]" "$out"
  check "$1: $2, so no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable)," "$out"
  check_not "$1: no fault but the introductory act's" "volition(" "$out"
}
no_wards wd_req_unsupplied    "befriend requires an atom no schema of arity one requiring nothing adds (def:unobstructed-marks, clause 1)"
no_wards wd_forb_other        "befriend forbids an atom neither a mark nor the one it adds (def:unobstructed-marks, clause 2)"
no_wards wd_asymmetric        "follow adds friend(q) at p and no friend(p) at q (def:unobstructed-marks, clause 3)"
no_wards wd_two_atoms         "befriend adds two atoms at a role (def:unobstructed-marks)"
no_wards wd_no_adult_relay    "no relay act with a ward role of child and a free role (def:introduction-wards)"
no_wards wd_no_child_relay    "no relay act with two ward roles of child (def:introduction-wards)"
no_wards wd_relay_extra_role  "a role of the relay act is neither a party nor a guardian role (def:relay, clause 1)"
no_wards wd_relay_guard_names "a guardian role adds an atom naming the other guardian role (def:relay, clause 1)"
no_wards wd_relay_req         "a free role requires parent(p) (def:relay, clause 2)"
no_wards wd_relay_forb        "a guardian role forbids friend(a) (def:relay, clause 3)"
no_wards wd_relay_unguarded   "the adult's role does not guard, so the two are no parties (def:relay)"

# wd2, children and AI agents: two marks, a guardianship for each, befriend
# unobstructed but for both, and the relay acts of every pair -- child with a
# free role, child with child, child with agent, agent with a free role,
# agent with agent -- in the order of the set.
out=$(run 'check_named(wd2, V).')
check "wd2 is grassroots with wards for the two marks child and agent" \
      "V = grassroots_with_wards(befriend, [child, agent], [guardianship(child, identity, parent, parenting), guardianship(agent, identity, principal, principal_of)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4)), relay(child_agent_befriend, ward(1, child, 3), ward(2, agent, 4)), relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [])" \
      "$out"

# wd_two_marks adds a schema adding child and agent at one role: no
# guardianship for {child, agent} (def:guardianship, clause 1), one for
# {child} alone, and no introduction with wards, befriend forbidding agent,
# which is then no mark in M (def:unobstructed-marks, clause 2).
out=$(run 'contract_named(wd_two_marks, C), guardianships(C?, [child, agent], G).' \
          'contract_named(wd_two_marks, C), guardianships(C?, [child], G).' \
          'wards_of(wd_two_marks, W).')
check "two marks added at one role: no guardianship for the pair" \
      "G = []" "$out"
check "and child alone keeps one" \
      "G = [guardianship(child, identity, parent, parenting)]" "$out"
check "and there is no introduction with wards" \
      "W = no_introduction_with_wards" "$out"

# def:leaving, on wd_leaving: wd with retired forbidden at every guarding
# role and retire(p?, r) ending the guardianship of p over r and adding
# retired at r, where child is required.  retired is a leaving mark for
# {child}: forbidden at every guarding role and deleted by none (clause 1),
# added only by retire, at a role requiring child (clause 2), and forbidden by
# enter, the one schema adding parent, at the parent's role (clause 3, its
# g the parent of the guardianship of child, GSC 07d413c).
# befriend forbids it, which clause 2 of def:unobstructed-marks admits of a
# leaving mark; establish forbids it, which clause 1 admits; and the relay
# acts forbid it, which clause 3 of def:relay admits.  So wd_leaving is
# grassroots with wards and the verdict names retired.
wl_found="introduction_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])"
out=$(run 'check_named(wd_leaving, V).' 'wards_of(wd_leaving, W).' 'leaving_of(wd_leaving, [child], L).')
check "wd_leaving is grassroots with wards, retired its leaving mark" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
check "wards_of names retired with its introduction with wards" "W = $wl_found" "$out"
check "leaving_of returns retired for {child}, with the guardianship of child" \
      "L = [leaving([guardianship(child, identity, parent, parenting)], [retired])]" "$out"
out=$(run 'compile_named(wd_leaving).')
check_eq "and it is compiled, to nine displays" "9" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# def:leaving, one fixture per clause: each leaves retired no leaving mark,
# child keeping its guardianship, so befriend forbids an atom that is neither
# a mark in M, nor a leaving mark, nor the atom it adds
# (def:unobstructed-marks, clause 2), and there is no introduction with wards.
no_leaving() { # no_leaving <fixture> <what it breaks>
  out=$(run "leaving_of($1, [child], L)." "wards_of($1, W)." "check_named($1, V)." \
            "contract_named($1, C), guardianships(C?, [child], G).")
  check "$1: $2, so retired is not a leaving mark" \
        "L = [leaving([guardianship(child, identity, parent, parenting)], [])]" "$out"
  check "$1: child keeps its guardianship" "G = [guardianship(child, identity, parent, parenting)]" "$out"
  check "$1: and there is no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable)," "$out"
}
no_leaving wd_leave_unforbidden "retire's guarding role does not forbid retired (clause 1)"
no_leaving wd_leave_deleted     "revive deletes retired (clause 1)"
no_leaving wd_leave_unmarked    "quit adds retired at a role requiring no mark in M (clause 2)"
no_leaving wd_leave_guardian    "adopt adds parent(p) at r and does not forbid retired at p (clause 3)"

# GAI's contract, programs/gai/GAI.glp at e6e13efe, checked by hand against
# GSC 429cfc8: agent has the guardianship (agent, identity, principal,
# principal_of) of def:guardianship, enter adding agent, identity and
# principal(p) at the agent, who had no identity, and principal_of(r) at p,
# who forbids agent, and no schema deleting principal or principal_of;
# retired is a leaving mark for {agent} (def:leaving): every guarding role
# forbids it and none deletes it (clause 1), retire, the one schema adding
# it, requires agent at that role (clause 2), and enter, the one schema
# adding principal, forbids it at p (clause 3); befriend is unobstructed but
# for {agent}, establish adding identity and forbidding nothing but retired,
# befriend forbidding agent, retired and the friend atom it adds, and every
# schema adding and deleting friend atoms in pairs (def:unobstructed-marks);
# agent_befriend is the relay act of a ward role of agent and a free role,
# and agents_befriend that of two ward roles of agent, each forbidding
# nothing but agent, retired and the friend atom naming the other related
# role (def:relay).  So it is grassroots with wards (def:introduction-wards,
# thm:grassroots-wards), the verdict naming retired, as GAI expected
# (GAI #3 to Code, Code_inbox 2026-10-19 10:45 UTC).
out=$(run "check($GAI, V)." "wards($GAI, W)." "guardianships($GAI, [agent], G)." "leaving($GAI, [agent], L).")
check "GAI's agent has a guardianship" \
      "G = [guardianship(agent, identity, principal, principal_of)]" "$out"
check "GAI's leaving marks for {agent} are [retired]" \
      "L = [leaving([guardianship(agent, identity, principal, principal_of)], [retired])]" "$out"
check "GAI's contract is grassroots with wards, retired its leaving mark" \
      "V = grassroots_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [retired])" \
      "$out"
check_not "GAI's predicates have traceable provenance" "untraceable(" "$out"
check_not "GAI's contract satisfies volition" "volition(" "$out"

# --- agent-named contracts with wards (Appendix F, def:sg-wards) -----------
# wd_leaving with gather(p?, (<p>.n)^club ; n), whose seated role requires
# child: an agent-named contract with wards (GSC 503db66).  wd_seated's
# seated role adds noted, no mark in M, leaving mark, friend, parent or
# parenting atom, so it meets clause 2 of def:sg-wards, and clauses 1 and 3
# as wd_leaving does: syntactically grassroots with wards.  wd_seated_leaving's
# adds retired, a leaving mark, and clause 2 fails: no introduction with
# wards meets def:sg-wards, and the verdict is definition:sg-agent-named's.
out=$(run 'check_named(wd_seated, V).')
check "a seated role adding no mark, leaving mark, friend or guardianship atom: syntactically grassroots with wards" \
      "V = syntactically_grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
out=$(run 'check_named(wd_seated_leaving, V).' 'wards_of(wd_seated_leaving, W).')
check "a seated role adding a leaving mark fails clause 2 of def:sg-wards" \
      "W = no_introduction_with_wards" "$out"
check "and the verdict is definition:sg-agent-named's, naming the seated role" \
      "obstructed(enter, 1, atom(retired, []), seated(gather, 2, atom(retired, [])))" "$out"
check_not "and it is not syntactically grassroots with wards" "syntactically_grassroots_with_wards" "$out"
# The same with gather(p?, p[club(*, n)] ; n), whose listed role is the
# agents p lists (GSC 8064cfb: clause 2 of def:sg-wards asks of seated and
# listed roles).
out=$(run 'check_named(wd_listed, V).')
check "a listed role adding no mark, leaving mark, friend or guardianship atom: syntactically grassroots with wards" \
      "V = syntactically_grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
out=$(run 'check_named(wd_listed_leaving, V).' 'wards_of(wd_listed_leaving, W).')
check "a listed role adding a leaving mark fails clause 2 of def:sg-wards" \
      "W = no_introduction_with_wards" "$out"
check "and the verdict is definition:sg-agent-named's, naming the listed role" \
      "obstructed(enter, 1, atom(retired, []), listed_role(gather, 2, atom(retired, [])))" "$out"
check_not "and it is not syntactically grassroots with wards" "syntactically_grassroots_with_wards" "$out"

# --- agent-named contracts (Appendix D.4, definition:sg-agent-named) -------
# GSNP with one schema more, each failing one clause, and two contracts that
# stand on their own, failing clause 1.
out=$(run "check_variant(sn_seated_forbidden, $SG, V).")
check "a seated role adding friend, which befriend forbids, fails clause 2 at both roles" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(2)]), seated(befriend_members, 2, atom(friend, [role(1)]))), obstructed(befriend, 2, atom(friend, [role(1)]), seated(befriend_members, 2, atom(friend, [role(1)])))])" \
      "$out"
out=$(run 'check_named(an_named_intro, V).')
check "an introductory act with a name term is none, by clause 1" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'check_named(an_named_unary, V).')
check "identity added only by a schema with a name term is unobtainable, by clause 1" \
      "V = not_grassroots([obstructed(befriend, 1, atom(identity, []), unobtainable), obstructed(befriend, 2, atom(identity, []), unobtainable)])" \
      "$out"
out=$(run "check_variant(sn_seated_witness, $SG, V)." "traceable_variant(sn_seated_witness, $SG, E).")
check "a speech act traced through a seated role only fails clause 3" \
      "V = not_grassroots([untraceable([noted])])" "$out"
check_not "and noted is not among the predicates traced through agent roles" "noted" \
          "$(printf '%s' "$out" | grep '^E = \|GLP> E = ')"
out=$(run "check_variant(sn_seated_guard, $SG, V).")
check "a component guarded by a seated role only fails clause 4" \
      "V = not_grassroots([volition(split, 1, 2)])" "$out"

# The same four over a listed role pi[rho(*, u_2, ..., u_r)] (Appendix D,
# Definitions Role and Extent, GSC 8064cfb): its base is the agent role pi,
# so it joins pi in the role graph (clause 4), it is no agent role (clauses 3
# and 4), and clause 2 asks of it what it asks of a seated role, the fault
# naming it listed_role.  sn_listed_vote's listed role carries the mark
# ?_theta, a part of the members a lists larger than theta of them guarding
# vote (Definition Act schema), and GSNP with it is syntactically grassroots.
out=$(run "check_variant(sn_listed_forbidden, $SG, V).")
check "a listed role adding friend, which befriend forbids, fails clause 2 at both roles" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(2)]), listed_role(befriend_listed, 2, atom(friend, [role(1)]))), obstructed(befriend, 2, atom(friend, [role(1)]), listed_role(befriend_listed, 2, atom(friend, [role(1)])))])" \
      "$out"
out=$(run "check_variant(sn_listed_witness, $SG, V)." "traceable_variant(sn_listed_witness, $SG, E).")
check "a speech act traced through a listed role only fails clause 3" \
      "V = not_grassroots([untraceable([noted])])" "$out"
check_not "and noted is not among the predicates traced through agent roles" "noted" \
          "$(printf '%s' "$out" | grep '^E = \|GLP> E = ')"
out=$(run "check_variant(sn_listed_guard, $SG, V).")
check "a component guarded by a listed role only fails clause 4" \
      "V = not_grassroots([volition(split, 1, 2)])" "$out"
out=$(run "check_variant(sn_listed_vote, $SG, V)." "traceable_variant(sn_listed_vote, $SG, E).")
check "a listed role guarded with ?_theta, joined to its agent role: GSNP with vote is syntactically grassroots" \
      "V = syntactically_grassroots_with_seated_and_listed_roles" "$out"
check "and voted and voter have traceable provenance, n traced through member(<a>.n) at the agent role" \
      "E = [identity, friend, item, authored, member, listed, closed, posted, delivered, voted, voter]" "$out"
# vote compiles (definition:compile) to an assignment over each role's
# extent, the listed role's written \Alice[\mathit{listed}(\ast,n)], a
# proviso line per role, and the guard, a part of the listed role's extent
# larger than theta of it.
vote_tex=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := c_p \uplus \{\mathit{voted}(n)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \uplus \{\mathit{voter}(n)\} && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{guarded by } G,\ G\subseteq\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)]) \text{ with } |G|>\theta|\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])|
\end{align*}
TEX
)
check_eq "vote compiles with the guard a part of the listed role's extent larger than theta of it" \
         "$vote_tex" \
         "$(run "compile_schema_variant(sn_listed_vote, $SG, vote)." | sed -n '/begin{align/,/end{align/p' | sed 's/GLP>//' | squash)"

# --- listed roles formed otherwise (Appendix D, Definition Role) -----------
# close_b's listed role is of itself, no agent role; note_l's carries a
# signed term among its arguments, which Definition Act schema admits only in
# an atom marked +; close_h's has an argument m its head does not list: the
# contract is malformed, each fault named, and it is not compiled.
out=$(run 'check_named(ls_faults, V).' 'compile_named(ls_faults).')
check "listed roles of no agent role, with a signed term, or with an unlisted variable are refused" \
      "V = malformed([not_in_head(close_h, svar(m)), listed_base(close_b, 2), listed_signed(note_l, 2)])" \
      "$out"
check "and it is not compiled, the message naming the faults" \
      "% not compiled: ls_faults is malformed: close_h has a variable its head does not list; close_b at role 2 has a listed role whose base is not an agent role of the schema; note_l at role 2 has a listed role with a signed term among its arguments; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"

# --- the head of a schema (def:schema at GSC 0228e79) ----------------------
# sign's head lists x twice and the role p, and leaves out y: the contract is
# malformed, each fault named, and it is not compiled.
out=$(run 'check_named(hd_faults, V).' 'compile_named(hd_faults).')
check "a head that repeats a variable, lists a role and leaves out a variable is refused" \
      "V = malformed([head_repeated(sign, svar(x)), head_not_variable(sign, role(1)), not_in_head(sign, svar(y))])" \
      "$out"
check "and it is not compiled, the message naming the faults" \
      "% not compiled: hd_faults is malformed: sign lists a variable twice in its head; sign lists in its head an entry that is not a variable; sign has a variable its head does not list; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
