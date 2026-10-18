#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/gsc).
#
#   bash programs/gsc/test_gsc.sh
#
# GSC's contracts GSG, GSN and GSNP (programs/gsc/), GSNP being the social
# network with provenance of /Grassroots/GSC Section 3.3, which Section 7.1
# certifies by hand, and the currency of Appendix B, which Appendix B.4
# certifies (programs/currencies/coins/GC.glp), read from their owners' files,
# and contracts broken in one place each, one per way of failing the three
# conjuncts of def:syntactically-grassroots; then contracts declaring a
# predicate reflexive (the paragraph after def:binding); then the compilation
# of Section 5, against the display of befriend in Section 5.1 and that of the
# swap in Appendix B.3; then the contract of
# a grassroots federation, /Grassroots/GFWC sections/schemas.tex, read from
# programs/federation/GFWC.glp, against the three conditions of Appendix D
# of /Grassroots/GSC, and five contracts changing it in one place each, four
# against those and one against rootedness, which is none of them; then small
# contracts on the speech-act variables of traceable provenance and on
# volition by connected component, in the language of Appendix D; then the
# compilation of Appendix D, against its worked box; then the signed
# speech-act term of Sections 3.1 to 3.3, on GSNP and on contracts refused
# for a signed term where def:schema forbids it; then
# contracts with wards, Appendix F, on a fixture of children and parents, on
# one of children and AI agents, on each broken in one place per clause, and
# on CSSN's and GAI's contracts.  A contract is its owner's and the checker
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
# checker and vGLP's compiler load from there.  GSC's three, GSG, GSN and
# GSNP, each open with establish, GSG:1, which adds identity and requires and
# forbids nothing, and every other role of each requires identity.  SG is
# GSNP, Section 3.3's social network with provenance, the contract the checks
# of Section 3 below are made on.
ESTABLISH="[schema(establish, [rs(guarded, [atom(identity, [])], [], [], [])]), schema(befriend, [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, [])], [atom(friend, [role(2)])])"
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
# friend(Bob) (def:volition).  GSN: its message, group and delivery schemas
# add atoms whose arguments are roles and speech-act variables no role requires
# or that a required atom of traceable provenance carries, and message's role
# Bob requires friend(Alice), post's role Bob member(Alice, n), leave's and
# deliver's role Alice listed(Bob, n), Alice being the leader of a group.  Both
# are syntactically grassroots (def:syntactically-grassroots).
out=$(run "check($GSGT, V).")
check "GSG is syntactically grassroots" "V = syntactically_grassroots" "$out"
out=$(run "check($GSNT, V).")
check "GSN is syntactically grassroots" "V = syntactically_grassroots" "$out"
CUR=$(term "$GLP_DIR/programs/currencies/coins/GC.glp" gc)
check "the currency's contract, GC, is read from programs/currencies/coins/GC.glp" \
      "[schema(mint, [rs(guarded, [atom(coin, [role(1)])]" "$CUR"
CSSN=$(term "$GLP_DIR/programs/cssn/CSSN.glp" cssn)
check "CSSN's contract is read from programs/cssn/CSSN.glp" \
      "reflexive([friend], [schema(establish," "$CSSN"
FED=$(term "$GLP_DIR/programs/federation/GFWC.glp" gfwc)
check "the federation's contract, GFWC, is read from programs/federation/GFWC.glp" \
      "[schema(form, [rs(guarded, [atom(seat, [nterm(own(1))])]" "$FED"
GAI=$(term "$GLP_DIR/programs/gai/GAI.glp" gai)
check "GAI's contract is read from programs/gai/GAI.glp" \
      "[schema(establish, [rs(guarded, [atom(identity, [])], [], [], [])])" "$GAI"

# The two worked contracts, GSNP certified in Section 7.1 and the currency
# in Appendix B.4.  One goal per run, so that each verdict is read off its own
# output.  GSNP: befriend is unobstructed (def:unobstructed: identity from
# establish, friend added by befriend alone), the set of all nine predicates
# has traceable provenance (def:grounded; Section 7.1, "So has the set of all
# nine predicates of the contract"), and volition holds (def:volition:
# unfriend's role Alice requires friend(Bob), forward's role Bob
# friend(Alice), post's role Bob member(Alice, n), leave's and deliver's role
# Alice listed(Bob, n)), so it is syntactically grassroots, as Section 7.1
# says.
# Its signed terms, in forward, post and deliver, are each signed by a role
# that requires an atom carrying the speech act (def:schema), deliver's by a
# role that does not guard, so GSNP is not malformed.
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "the program loads" "Loaded program" "$out"
check_not "no type error" "Error loading" "$out"
check "GSNP is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check_not "GSNP's signed terms, deliver's among them, are admitted" "malformed(" "$out"
check "GSNP's nine predicates have traceable provenance" \
      "E = [identity, friend, item, sent, member, listed, closed, posted, delivered]" "$out"

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

# An act that adds the forbidden atom while giving the other party a role, and
# naming it in what it adds, is excused by clause 2 of def:unobstructed; what
# rejects it is volition.
out=$(run "check_variant(sg_imposed, $SG, V).")
check "impose is rejected by volition, not by clause 2" \
      "V = not_grassroots([volition(impose, 1, 2)])" "$out"
check_not "impose does not obstruct befriend" "obstructed" "$out"

# An act that adds it while sending no role to the other party is not excused:
# p is left holding friend(q) with q taking no part.
out=$(run "check_variant(sg_gossip, $SG, V).")
check "gossip blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1," "$out"
check "gossip blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"
check_not "gossip itself satisfies volition" "volition(gossip" "$out"

# Volition above arity two is connectedness of the role graph, or a guarding
# role in every connected component of it, not a test on every pair: a schema
# of arity four guarded at one role, whose role graph is the path 1-2-3-4,
# passes; cutting one edge of the path splits it, and the half {3,4} holds no
# guarding role.
out=$(run "check_variant(sg_chain, $SG, V).")
check "a path role graph at arity four certifies" \
      "V = syntactically_grassroots" "$out"

out=$(run "check_variant(sg_chain_cut, $SG, V).")
check "cutting an edge of the path fails volition" \
      "V = not_grassroots([volition(chain, 1, 3)])" "$out"

# Volition is one condition in Section 3 and Appendix D (GSC 7652554): tri(p?, q, r?)
# is guarded neither in all its roles nor connected, its role graph falling
# into {1,2} and {3}, each holding a guarding role, so it meets def:volition
# as it meets definition:volition as a cschema (sg_tri_cschema, below).
out=$(run "check_variant(sg_tri, $SG, V).")
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
# traceable provenance: the clause of def:grounded on speech-act variables
# drops tagged, its u named by no required atom, then item, tag's x being
# required only in tagged(x, u), then sent, posted and delivered, forward,
# post_own, post and deliver adding a speech act their roles require only in
# an item or a posted atom; identity, friend and the group's member, listed
# and closed keep it.  Befriend stays unobstructed and every schema satisfies
# volition, the edges of def:volition resting on friend, member and listed.
# The second conjunct alone rejects the contract.
out=$(run "check_variant(sg_svar_loose, $SG, V)." "traceable_variant(sg_svar_loose, $SG, E).")
check "a speech act from an untraceable record: identity, friend and the group's records keep provenance" \
      "E = [identity, friend, member, listed, closed]" "$out"
check "and the verdict is the second conjunct alone" \
      "V = not_grassroots([untraceable([item, sent, posted, delivered, tagged])])" "$out"

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
& \text{provided } \mathit{item}(x,a,f) \in c_{\Alice} \text{ and } \mathit{friend}(\Alice) \in c_{\Bob}, \qquad \text{guarded by } \{\Alice\}
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
& \text{provided } \mathit{parent}(\Bob) \notin c_{\Alice} \text{ and } \mathit{parent}(\Alice) \notin c_{\Bob}, \qquad \text{guarded by } \{\Alice,\Bob\}
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
& \text{provided } \mathit{friend}(\Bob) \in c_{\Alice} \text{ and } \mathit{friend}(\Alice) \in c_{\Bob}, \qquad \text{guarded by }
EOF
IFS= read -r -d '' child_unfriend_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{friend}(\Alice)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\}, \qquad c'_{\mathit{Dave}} := c_{\mathit{Dave}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \{\mathit{parent}(\mathit{Carol}),\mathit{friend}(\Bob)\} \subseteq c_{\Alice} \text{ and } \{\mathit{parent}(\mathit{Dave}),\mathit{friend}(\Alice)\} \subseteq c_{\Bob} \text{ and } \mathit{parenting}(\Alice) \in c_{\mathit{Carol}} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Dave}}, \qquad \text{guarded by }
EOF
IFS= read -r -d '' leave_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob}, \qquad \text{guarded by }
EOF
IFS= read -r -d '' child_leave_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Carol}}, \qquad \text{guarded by }
EOF
IFS= read -r -d '' child_leave_2_body <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \setminus \{\mathit{listed}(\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\Alice,n)\}, \qquad c'_{\mathit{Carol}} := c_{\mathit{Carol}} \uplus \{\mathit{withdrawal}\}, \qquad c'_{\mathit{Dave}} := c_{\mathit{Dave}} \uplus \{\mathit{withdrawal}\},\\
& \text{provided } \mathit{listed}(\Bob,n) \in c_{\Alice} \text{ and } \mathit{member}(\Alice,n) \in c_{\Bob} \text{ and } \mathit{parenting}(\Alice) \in c_{\mathit{Carol}} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Dave}}, \qquad \text{guarded by }
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

# CSSN's contract, programs/cssn/CSSN.glp (CSSN, Appendix B.2): its
# predicates have traceable provenance and it satisfies volition, and it has
# no unobstructed introductory act (CSSN, Proposition "Provenance and
# volition"), so it is not syntactically grassroots.  It has an introduction
# with wards (GSC, Appendix F, def:introduction-wards): the mark child, with
# the guardianship (child, identity, parent, parenting) of def:guardianship;
# befriend, unobstructed but for {child} (def:unobstructed-marks);
# child_adult_befriend, the relay act of a ward role of child and a free role,
# and child_befriend, that of two ward roles of child (def:relay).  So it is
# grassroots with wards (thm:grassroots-wards), and it is compiled, its
# realisation being taken by def:realisation (Appendix F, after
# def:introduction-wards).
out=$(run "check($CSSN, V).")
check "CSSN's contract is grassroots with wards" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))])" \
      "$out"
check_not "CSSN's predicates have traceable provenance" "untraceable(" "$out"
check_not "CSSN's contract satisfies volition" "volition(" "$out"
check_not "CSSN's contract is not malformed" "malformed(" "$out"
out=$(run "compile_contract(cssn, $CSSN).")
check_eq "and it compiles to twenty-five displays" "25" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# --- reflexive predicates (the paragraph after def:binding) ----------------
# A contract may declare a predicate of arity one reflexive.  The social graph
# with friend declared so certifies as it does without.
out=$(run "check_variant(sg_reflexive, $SG, V).")
check "the social graph with friend reflexive is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"

# No schema adds or deletes, at a role, the atom of a reflexive predicate
# naming that role: a contract in which one does is refused as malformed,
# naming the schema, the role and the atom, and is not compiled.
out=$(run "check_variant(sg_refl_malformed, $SG, V).")
check "adding or deleting friend of its own role makes the contract malformed" \
      "V = malformed([reflexive(self_befriend, 1, atom(friend, [role(1)])), reflexive(self_unfriend, 1, atom(friend, [role(1)]))])" \
      "$out"
out=$(run "compile_variant(sg_refl_malformed, $SG).")
check "a malformed contract is not compiled" \
      "% not compiled: sg_refl_malformed is malformed" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# Clause 1 of def:unobstructed skips a required atom of a reflexive predicate
# naming the role: befriend requiring friend of each party at its own role is
# obstructed when friend is not reflexive and unobstructed when it is.
out=$(run "check_variant(sg_self_required, $SG, V).")
check "friend of its own role required, friend not reflexive: clause 1 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), unobtainable)])" \
      "$out"
out=$(run "check_variant(sg_refl_required, $SG, V).")
check "friend of its own role required, friend reflexive: clause 1 skips it" \
      "V = syntactically_grassroots" "$out"

# Clause 3 of def:unobstructed: the introductory act forbids at a role no atom
# of a reflexive predicate naming that role.
out=$(run "check_variant(sg_refl_forbidden, $SG, V).")
check "friend of its own role forbidden, friend reflexive: clause 3 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), reflexive), obstructed(befriend, 2, atom(friend, [role(2)]), reflexive)])" \
      "$out"

# A contract with no schemas.
out=$(run 'check_named(nonesuch, V).')
check "the empty contract has no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

# --- the compilation (Section 5) -------------------------------------------
# The two displays of the paper, befriend's in Section 5.1, after
# def:compile, transcribed from /Grassroots/GSC/sections/05-compilation.tex
# at GSC 16dfcd0, and the swap's in Appendix B.3, from
# sections/14-currency.tex, compared with the whitespace removed: the compiler
# emits a display one token to a line, GLP having no string concatenation, and
# a newline is whitespace to LaTeX.  The comma and the full stop that close
# the paper's two displays belong to the sentences around them, not to the
# compiled form, and are not expected here.  Befriend is GSNP's, which
# requires identity at both roles (GSG:1).

IFS= read -r -d '' befriend_tex <<'EOF'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{identity} \in c_{\Alice} \text{ and } \mathit{identity} \in c_{\Bob},\\
& \text{provided } \mathit{friend}(\Bob) \notin c_{\Alice} \text{ and } \mathit{friend}(\Alice) \notin c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
befriend_paper=$(printf '%s' "$befriend_tex" | squash)
check_eq "befriend compiles to the display of Section 5.1" \
         "$befriend_paper" "$(compiled_in gsnp "$SG" befriend)"

swap_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\text{\textcent}(u)\}) \uplus \{\text{\textcent}(v)\}, \qquad c'_{\Bob} := (c_{\Bob} \setminus \{\text{\textcent}(v)\}) \uplus \{\text{\textcent}(u)\},\\
& \text{provided } \text{\textcent}(u) \in c_{\Alice} \text{ and } \text{\textcent}(v) \in c_{\Bob}, \qquad \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "the swap compiles to the display of Appendix B.3" \
         "$swap_paper" "$(compiled_in gc "$CUR" swap)"

# A contract compiles schema by schema (Section 5.1), and only after it is
# checked: GSNP's twelve schemas, establish, befriend, unfriend, sign and
# forward of Section 3.3's table and the seven of a group, to twelve displays.
out=$(run "compile_contract(gsnp, $SG).")
check_eq "GSNP compiles to twelve displays" "12" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"
out=$(run "compile_contract(gc, $CUR).")
check_eq "the currency compiles to four displays" "4" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

out=$(run "compile_variant(sg_gossip, $SG).")
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
# first clause leaves Y out: a speech act signed and required nowhere.
out=$(run 'check_named(sv_signed, V).' 'traceable_of(sv_signed, E).')
check "a signed speech act required nowhere: item has traceable provenance" \
      "E = [item, got, mark]" "$out"
check "and the item take requires joins its role graph" \
      "V = conditions_met" "$out"

# The case of sg_svar_loose in the language of Appendix D: the same speech act
# carried from an untraceable record.  The verdict names item and tagged as
# untraceable, as sg_svar_loose's names its own, before the volition fault.
out=$(run 'check_named(sv_loose, V).' 'traceable_of(sv_loose, E).')
check "a speech act from an untraceable record: item loses traceable provenance" \
      "E = [got, mark]" "$out"
check "and the verdict names item and tagged, and take, resting on item, fails volition" \
      "V = conditions_failed([untraceable([item, tagged]), volition(take, 1, 2)])" "$out"

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
# fixture sg_plain, since Appendix D's Definition Act schema admits none and
# GSN carries one.
SGP=$(run 'contract_named(sg_plain, C).' | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check "the social graph without the signed term, sg_plain, is read" \
      "[schema(befriend, [rs(guarded, [atom(friend, [role(2)])]" "$SGP"
out=$(run "check_variant(sg_relay, $SGP, V)." "traceable_variant(sg_relay, $SGP, E).")
check_not "a guarding role in every connected component meets volition" \
      "volition(relay," "$out"
check "and a component holding none fails it" \
      "V = conditions_failed([volition(relay_unguarded, 1, 4)])" "$out"
check "the social graph keeps its traceable provenance in Appendix D" \
      "E = [friend, item, sent, chained, noted]" "$out"

# sg_tri with tri written as a cschema, in the language of Appendix D: the
# same verdict as in Section 3, volition being one condition in both.
out=$(run "check_variant(sg_tri_cschema, $SGP, V).")
check "tri as a cschema meets the conditions of Appendix D, as it does in Section 3" \
      "V = conditions_met" "$out"

# Rootedness is a notion of Appendix D; a contract of Section 3 is not asked
# about it, and keeps the verdict of def:syntactically-grassroots.
out=$(run "rooted_preds($SG, E)." "check($SG, V).")
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

# --- the signed speech-act term (Sections 3.1 to 3.3) ----------------------
# GSNP, Section 3.3's network with provenance, item of arity one, forward
# adding item(sgn(x, Alice)) at Bob, post adding posted(sgn(x, Bob), n) at the
# leader and deliver adding item(sgn(y, Alice)) at the member.  Section 7.1
# certifies it, each signed term counting as its two arguments, the speech
# act and the role (def:grounded).
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "GSNP, carrying signed terms in forward, post and deliver, is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "item, posted and delivered have traceable provenance there with the rest" \
      "E = [identity, friend, item, sent, member, listed, closed, posted, delivered]" "$out"

# forward compiles, by def:compile, to its assignment at each role, the
# proviso that each role's required atoms, identity and item(x) at Alice and
# identity and friend(Alice) at Bob, are in its state, and the guard {Alice},
# in the form of the swap's display (Appendix B.3), the signed term written
# \mathrm{sgn}(x,\Alice) as Section 3.3 writes it.
forward_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{sent}(x,\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(\mathrm{sgn}(x,\Alice))\},\\
& \text{provided } \{\mathit{identity},\mathit{item}(x)\} \subseteq c_{\Alice} \text{ and } \{\mathit{identity},\mathit{friend}(\Alice)\} \subseteq c_{\Bob}, \qquad \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "forward compiles with the signed term" \
         "$forward_gsnp" "$(compiled_in gsnp "$SG" forward)"

# deliver, by def:compile: the leader Alice's assignment adds
# delivered(y, Bob, n), the member Bob's the leader's signed term
# item(sgn(y, Alice)), the proviso is what each role requires and Alice
# forbids, and the guard is empty, no role of deliver guarding (Section 5.1,
# after def:compile).  The signed term's role does not guard, and def:schema
# admits it, Alice requiring posted(y, n).
deliver_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{delivered}(y,\Bob,n)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(\mathrm{sgn}(y,\Alice))\},\\
& \text{provided } \{\mathit{identity},\mathit{member}(\Alice,n),\mathit{posted}(y,n),\mathit{listed}(\Bob,n)\} \subseteq c_{\Alice} \text{ and } \mathit{delivered}(y,\Bob,n) \notin c_{\Alice} \text{ and } \{\mathit{identity},\mathit{member}(\Alice,n)\} \subseteq c_{\Bob}, \qquad \text{guarded by } \emptyset
\end{align*}
TEX
)
check_eq "deliver compiles with the leader's signed term and an empty guard" \
         "$deliver_gsnp" "$(compiled_in gsnp "$SG" deliver)"

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

# Appendix D's Definition Act schema admits no signed term: a contract with
# community roles carrying one is refused.
out=$(run 'check_named(sgn_community, V).')
check "a signed term in a contract with community roles is refused" \
      "V = malformed([signed(forward, 2, atom(item, [sgn(x, 1)]), community)])" "$out"

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
# child_befriend.  wards_of gives what the search found, whatever the
# verdict, and guardianships the guardianships for a set of marks.
wd_found="introduction_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))])"
out=$(run 'check_named(wd, V).' 'wards_of(wd, W).' 'contract_named(wd, C), guardianships(C?, [child], G).')
check "wd is grassroots with wards" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))])" \
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
      "V = grassroots_with_wards(befriend, [child, agent], [guardianship(child, identity, parent, parenting), guardianship(agent, identity, principal, principal_of)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4)), relay(child_agent_befriend, ward(1, child, 3), ward(2, agent, 4)), relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))])" \
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

# GAI's contract, programs/gai/GAI.glp: agent has the guardianship
# (agent, identity, principal, principal_of) and befriend is unobstructed but
# for {agent}, and agent_befriend forbids retired at the agent's role, a mark
# not in M, which clause 3 of def:relay does not allow: the contract is
# outside Appendix F (its last paragraph), and its verdict is Section 3's.
out=$(run "check($GAI, V)." "wards($GAI, W)." "guardianships($GAI, [agent], G).")
check "GAI's agent has a guardianship" \
      "G = [guardianship(agent, identity, principal, principal_of)]" "$out"
check "GAI's agent_befriend forbids retired, so there is no relay act and no introduction with wards" \
      "W = no_introduction_with_wards" "$out"
check "and GAI's verdict is Section 3's" \
      "V = not_grassroots([obstructed(befriend, 1, atom(agent, []), blocked_by(enter, 2, atom(agent, []))), obstructed(befriend, 2, atom(agent, []), blocked_by(enter, 2, atom(agent, [])))])" \
      "$out"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
