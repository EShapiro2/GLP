#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/gsc).
#
#   bash programs/gsc/test_gsc.sh
#
# GSC's contracts GKK, GSG and GSN (programs/gsc/), GKK the base contract every contract extends, its introductory act meet, knows of traceable provenance and introduce's roles joined by knows(q) at p (GSC, Sections 2.1 and 7.1; intro_acts/2 and variants of GKK), GSN being the social network of /Grassroots/GSC Section 3.3, which Section 7.1 certifies by hand by definition:sg-agent-named (Appendix D.4), and GSNP, the social network with provenance, GAI's, carrying the signed term of GAI's extension of the language; the currency of Appendix B, which Appendix B.4 certifies (programs/currencies/coins/GC.glp); CSSN's and GAI's contracts; each read from its owner's file, each schema listing its variables in its head (GSC 0228e79); and contracts of Section 3, over GSG, broken in one place each, one per way of failing the three conjuncts of def:syntactically-grassroots; then contracts declaring a predicate reflexive (GAI's appendix "Contracts with Wards"); then schemas not formed as def:schema requires, refused, and a guarding role adding and deleting nothing, admitted; then the compilation of Section 5, against the display of befriend in Section 5.1 and that of the swap in Appendix B.3; then small contracts on the speech-act variables of traceable provenance and on volition by connected component, in the language of Appendix D, and a contract with a name term based on an agent variable, put to the two conditions on the text of Appendix D (Appendix D.3) and compiled by definition:compile; then the signed speech-act term, on GSNP and on contracts refused for a signed term where it is forbidden; then contracts with wards (GAI's appendix "Contracts with Wards"), on a fixture of children and parents, on one of children and AI agents, on each broken in one place per clause, on one of children and AI agents whose relay acts require the AI agent's attestation, an atom of h of clause 2 of def:relay, and on it broken in one place per condition of that alternative, on one with a leaving mark and on it broken in one place per clause of def:leaving, on CSSN's and GAI's contracts, and on agent-named contracts with wards (def:sg-wards, CSSN's paper) with a listed role; then agent-named contracts with community roles (Appendix D.4), over GSNP, failing one clause of definition:sg-agent-named each by a listed role, and listed roles refused for the mark ? or for changing nothing; then listed roles formed otherwise than Appendix D's Definition Role allows, and schemas whose head is not their variables (def:schema); then the notation of Definition Act schema, printed from a contract's term (notation.glp); then the numbers act schemas allow (GSC, Section 3.2, the paragraph after the example of def:schema), which the conditions do not concern, a number naming no agent: GB, the bonds, Currencies' (programs/currencies/bonds/GB.glp), its verdict, its displays and its notation, a contract with numbers certified, a number required and supplied and one forbidden and obstructed, and a fixture of each malformation the passage forbids.  The federation extension of act schemas left GSC's paper (GSC f3c4f43) and its checks left with its code (Coordination #7, 2026-10-20); GFWC's contract is still read from its owner's file.  A contract is its owner's and the checker reads its term (contracts.glp, the header): a fixture that changes one is a variant, given by name and the owner's term.  Exits non-zero if any check fails.

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
# schema(Name, Vars, Roles) or cschema(Name, Vars, [], Roles), Vars its
# variables in the order of its head (GSC 0228e79, Definition Act schema).
# GSC's three, GSG, GSN and GSNP, each open with establish, GSG:1, which adds
# identity and requires and forbids nothing, and every other role of each
# requires identity.  SG is GSNP, Section 3.3's social network with
# provenance, an agent-named contract with the listed role of close
# (Appendix D.4, GSC 8064cfb); GSGT is GSG, a contract of Section 3, the
# contract the checks of Section 3 below are made on.
ESTABLISH="[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [])]), schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, [])], [atom(friend, [role(2)])])"
SG=$(term "$GLP_DIR/programs/gai/GSNP.glp" gsnp)
check "GSNP's contract, the network with provenance, is read from programs/gai/GSNP.glp" \
      "$ESTABLISH" "$SG"
# GKK, the base contract, and GSG and GSN, which extend it (Udi, 2026-10-20,
# to GSC #5): each opens with GKK's establish and meet.
ESTABLISH_MEET="[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [])]), schema(meet, [], [rs(guarded, [atom(knows, [role(2)])], [], [atom(identity, [])], []), rs(guarded, [atom(knows, [role(1)])], [], [atom(identity, [])], [])]), schema(introduce, [pvar(c)], "
GKKT=$(term "$GLP_DIR/programs/gsc/GKK.glp" gkk)
check "GKK's contract is read from programs/gsc/GKK.glp, establish, meet and introduce" "$ESTABLISH_MEET" "$GKKT"
GSGT=$(term "$GLP_DIR/programs/gsc/GSG.glp" gsg)
check "GSG's contract is read from programs/gsc/GSG.glp, GKK's schemas first" "$ESTABLISH_MEET" "$GSGT"
GSNT=$(term "$GLP_DIR/programs/gsc/GSN.glp" gsn)
check "GSN's contract, the network without provenance, is read from programs/gsc/GSN.glp, GKK's schemas first" \
      "$ESTABLISH_MEET" "$GSNT"
# GSG: befriend is unobstructed, identity being added by establish, of arity
# one, guarded and requiring and forbidding nothing (def:unobstructed, clause
# 1), and friend being added by befriend alone (clause 2); identity and friend
# have traceable provenance (def:grounded); unfriend's role Alice requires
# friend(Bob) (def:volition); so it is syntactically grassroots
# (def:syntactically-grassroots).  GSN (GSN.glp at 542ec8ca): a group is the
# community name <a>.n and close takes the listed role a[listed(*, n)],
# every member a lists (GSC 8064cfb, Appendix D's Definition Role), every
# name term based on the agent role a, so GSN is agent-named (Appendix D.4,
# definition:agent-named) and is decided by definition:sg-agent-named: befriend, with agent roles and no
# name term, is unobstructed, identity from establish, of an agent role and
# no name term, and close's listed role adds no atom (clauses 1 and 2); its
# message, group and delivery schemas add atoms whose arguments are roles,
# the name <a>.n and speech-act variables no role requires or that an atom
# required at an agent role carries (clause 3); and every role graph is
# connected or has a guarding agent role in each component, close's two
# roles sharing the base a, the base of a listed role being its agent role
# (clause 4).  So it is syntactically grassroots by definition:sg-agent-named
# (Theorem "Grassroots realisation with listed roles"), and not
# malformed: message's and post's guarding role adds and deletes nothing,
# which def:schema and Appendix D's Definition Act schema admit.
out=$(run "check($GSGT, V).")
check "GSG is syntactically grassroots" "V = syntactically_grassroots" "$out"
out=$(run "check($GSNT, V).")
check "GSN is syntactically grassroots by definition:sg-agent-named" \
      "V = syntactically_grassroots_with_listed_roles" "$out"
check_not "GSN's message and post, guarded at a role that adds and deletes nothing, are admitted" \
          "malformed(" "$out"

# GKK (programs/gsc/GKK.glp; GSC, Section 2.1, its schemas in Section 3.3's
# table, certified with GSN in Section 7.1).  Meet is its introductory act,
# unobstructed: guarded in both roles, it adds knows of the other at each,
# and it requires identity, which establish adds, of arity one, guarded and
# requiring and forbidding nothing, and forbids nothing (def:introduction,
# def:unobstructed); introduce is no introductory act, adding at Alice no
# atom naming Bob.  knows has traceable provenance: meet adds at each role an
# atom naming the other role, and introduce adds knows(c) at Bob, c an
# argument of knows(c), which Alice requires (def:grounded).  Volition holds:
# meet and introduce are guarded in both roles, and introduce's roles are
# joined besides by knows(Bob) at Alice, an atom of traceable provenance
# naming the other role (def:volition).  intro_acts/2 names each
# introductory act and whether it is unobstructed; the verdict says only that
# one is.
out=$(run "check($GKKT, V)." "intro_acts($GKKT, A)." "traceable_preds($GKKT, E).")
check "GKK is syntactically grassroots" "V = syntactically_grassroots" "$out"
check "GKK: meet is its one introductory act, and unobstructed" \
      "A = [intro_act(meet, unobstructed)]" "$out"
check "GKK: knows has traceable provenance, with identity" "E = [identity, knows]" "$out"
# In GSG befriend is an introductory act too, but obstructed: it requires
# knows of the other, which no schema of arity one adds (GSC, Section 3.6,
# the example after def:introduction); meet is unobstructed, so GSG is
# syntactically grassroots.  GSN likewise, by definition:sg-agent-named.
out=$(run "intro_acts($GSGT, A)." "traceable_preds($GSGT, E).")
check "GSG: meet unobstructed, befriend an introductory act obstructed by the knows of the other it requires at each role, which no schema of arity one adds" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" "$out"
check "GSG: identity, knows and friend have traceable provenance" "E = [identity, knows, friend]" "$out"
out=$(run "intro_acts($GSNT, A).")
check "GSN: meet unobstructed, befriend obstructed" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" "$out"
# Introduce's roles joined by knows(Bob) at Alice: with Bob not guarding and
# requiring no knows(Alice), the role graph still joins them, and volition
# holds (gkk_intro_joined); with Alice requiring no knows(Bob) besides,
# nothing joins them, Bob's component holds no guarding role, and volition
# fails at introduce (gkk_intro_unjoined).  And knows traced through the
# knows(c) Alice requires: with c known to neither role, knows loses
# traceable provenance (gkk_intro_loose).
out=$(run "check_variant(gkk_intro_joined, $GKKT, V).")
check "GKK, introduce guarded by Alice alone and Bob requiring no knows(Alice): knows(Bob) at Alice joins the two roles, volition holds" \
      "V = syntactically_grassroots" "$out"
out=$(run "check_variant(gkk_intro_unjoined, $GKKT, V).")
check "GKK, ... and Alice requiring no knows(Bob): nothing joins introduce's roles, volition fails" \
      "V = not_grassroots([volition(introduce, 1, 2)])" "$out"
out=$(run "check_variant(gkk_intro_loose, $GKKT, V)." "traceable_variant(gkk_intro_loose, $GKKT, E).")
check "GKK, knows(c) added at Bob and c required at neither role: knows is untraceable" \
      "V = not_grassroots([untraceable([knows])])" "$out"
check "GKK, ... and identity alone has traceable provenance" "E = [identity]" "$out"
CUR=$(term "$GLP_DIR/programs/currencies/coins/GC.glp" gc)
check "the currency's contract, GC, is read from programs/currencies/coins/GC.glp, GKK's schemas first" \
      "$ESTABLISH_MEET" "$CUR"
check "and its mint adds a coin of the minter's issue" \
      "schema(mint, [], [rs(guarded, [atom(coin, [role(1)])]" "$CUR"
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
# Theorem "Grassroots realisation with listed roles".  Its signed
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
      "V = syntactically_grassroots_with_listed_roles" "$out"
check_not "GSNP's signed terms, deliver's among them, and its guarding roles that change nothing are admitted" "malformed(" "$out"
check "GSNP's nine predicates have traceable provenance" \
      "E = [identity, friend, item, authored, member, listed, closed, posted, delivered]" "$out"

# GC extends GKK (Currencies, 6499166c; GSC 079cee0, Appendix B.4): meet is
# its introductory act, unobstructed, as in GKK; the swap is an introductory
# act too, but obstructed, requiring knows of the other at each role, which no
# schema of arity one adds, and a coin, which mint, requiring identity, does
# not add as clause 1 of def:unobstructed asks; and the coin has traceable
# provenance, as identity and knows have.
out=$(run "check($CUR, V)." "traceable_preds($CUR, E)." "intro_acts($CUR, A).")
check "currency is syntactically grassroots, meet its unobstructed introductory act" \
      "V = syntactically_grassroots" "$out"
check "the coin has traceable provenance, as identity and knows have" \
      "E = [identity, knows, coin]" "$out"
check "the currency: meet unobstructed, the swap an introductory act obstructed by the knows of the other it requires" \
      "A = [intro_act(meet, unobstructed), intro_act(swap, obstructed([obstructed(swap, 1, atom(knows, [role(2)]), unobtainable)," "$out"

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
# p is left holding friend(q) with q taking no part.  On GSG, which extends
# GKK, meet stays an unobstructed introductory act, so the contract is
# syntactically grassroots, and befriend's obstructions are read off
# intro_acts.
out=$(run "check_variant(sg_gossip, $GSGT, V)." "intro_acts_variant(sg_gossip, $GSGT, A).")
check "on GSG with gossip meet is unobstructed, and the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
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
      "V = syntactically_grassroots_with_listed_roles" "$out"

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
& \text{provided } \mathit{parent}(\mathit{Carol}),\mathit{friend}(\Bob) \in c_{\Alice} \text{ and } \mathit{parent}(\mathit{Dave}),\mathit{friend}(\Alice) \in c_{\Bob} \text{ and } \mathit{parenting}(\Alice) \in c_{\mathit{Carol}} \text{ and } \mathit{parenting}(\Bob) \in c_{\mathit{Dave}},\\
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
# (Theorem "Grassroots realisation with wards and seated roles" of CSSN's
# paper, def:sg-wards).  It is compiled, its realisation being taken as GSC's
# (GAI's appendix "Contracts with Wards").  The leaving marks for {child} are given
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
# schema"): CSSN.glp's twenty-five at GLP fdc27eb3, GSN's twelve with the
# child conditions and thirteen for children (CSSN #8 Cowork, 2026-10-20).
out=$(run "compile_contract(cssn, $CSSN).")
check_eq "and it compiles to twenty-five displays" "25" \
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
# obstructed by it when friend is not reflexive and not when it is.  On GSG
# befriend is obstructed besides by the knows of the other it requires and
# meet is unobstructed, so befriend's obstructions are read off intro_acts.
out=$(run "intro_acts_variant(sg_self_required, $GSGT, A).")
check "friend of its own role required, friend not reflexive: clause 1 fails" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(friend, [role(1)]), unobtainable), obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" \
      "$out"
out=$(run "check_variant(sg_refl_required, $GSGT, V)." "intro_acts_variant(sg_refl_required, $GSGT, A).")
check "friend of its own role required, friend reflexive: the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "friend of its own role required, friend reflexive: clause 1 skips it, befriend obstructed by knows alone" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" \
      "$out"

# Clause 3 of def:unobstructed: the introductory act forbids at a role no atom
# of a reflexive predicate naming that role.
out=$(run "intro_acts_variant(sg_refl_forbidden, $GSGT, A).")
check "friend of its own role forbidden, friend reflexive: clause 3 fails" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(1)]), reflexive), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), reflexive)]))]" \
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
# requires the same of a schema with listed roles (GSC 70afdc4;
# sn_listed_inert below).
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
# has no atom, and the guard on a line of its own.  Befriend is GSG's, which
# requires identity and knows of the other at both roles (GKK:1, GSG:1;
# GSC 21c1b28, Section 5.1, its two required atoms at a role listed before
# \in); GSNP's, GAI's, requires identity alone, a singleton written with
# \in; the swap is GC's, which extends GKK, transcribed from
# sections/gc-compiled.tex at GSC 115fc8e, identity and knows of the other
# required at both roles with its coin.
befriend_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{identity}, \mathit{knows}(\Bob) \in c_{\Alice} \text{ and } \mathit{identity}, \mathit{knows}(\Alice) \in c_{\Bob},\\
& \text{provided } \mathit{friend}(\Bob) \notin c_{\Alice} \text{ and } \mathit{friend}(\Alice) \notin c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "GSG's befriend compiles to the display of Section 5.1" \
         "$befriend_paper" "$(compiled_in gsg "$GSGT" befriend)"
befriend_gsnp=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{friend}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{friend}(\Alice)\},\\
& \text{provided } \mathit{identity} \in c_{\Alice} \text{ and } \mathit{identity} \in c_{\Bob},\\
& \text{provided } \mathit{friend}(\Bob) \notin c_{\Alice} \text{ and } \mathit{friend}(\Alice) \notin c_{\Bob},\\
& \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "GSNP's befriend, requiring identity alone, compiles to the same display with a singleton at each role" \
         "$befriend_gsnp" "$(compiled_in gsnp "$SG" befriend)"

swap_paper=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\text{\textcent}(u)\}) \uplus \{\text{\textcent}(v)\}, \qquad c'_{\Bob} := (c_{\Bob} \setminus \{\text{\textcent}(v)\}) \uplus \{\text{\textcent}(u)\},\\
& \text{provided } \mathit{identity}, \mathit{knows}(\Bob), \text{\textcent}(u) \in c_{\Alice} \text{ and } \mathit{identity}, \mathit{knows}(\Alice), \text{\textcent}(v) \in c_{\Bob},\\
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
check_eq "the currency compiles to seven displays, GKK's three and its own four" "7" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# sg_gossip on GSNP, which has no meet: on GSG meet stays an unobstructed
# introductory act and the variant certifies (above).
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

# Volition by connected component in the language of Appendix D
# (definition:volition): sg_tri_cschema, a variant of the social graph
# without the signed term, the fixture sg_plain, a contract of Section 3.
SGP=$(run 'contract_named(sg_plain, C).' | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check "the social graph without the signed term, sg_plain, is read" \
      "[schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])]" "$SGP"

# sg_tri with tri written as a cschema, in the language of Appendix D, of
# agent roles and no name term, so agent-named: clause 4 of
# definition:sg-agent-named, definition:volition, holds of tri, each
# component holding a guarding role, as def:volition holds of it in Section
# 3.
out=$(run "check_variant(sg_tri_cschema, $SGP, V).")
check "tri as a cschema meets definition:sg-agent-named, as it meets def:syntactically-grassroots in Section 3" \
      "V = syntactically_grassroots_with_listed_roles" "$out"

# A contract with a name term based on an agent variable (Appendix D.3):
# nn_tag's tag(p? ; q, y) adds tagged(<q>.y), <q>.y based on the agent
# variable q, so the contract is not agent-named (definition:agent-named) and
# is decided by the two conditions on the text of Appendix D, traceable
# provenance and volition; Appendix D proves no theorem of it.  q is an
# argument of friend(q), which tag requires, so tagged has traceable
# provenance, and nn_tag meets the two conditions.  nn_tag_loose's tag
# requires no friend(q), and tagged loses traceable provenance.
out=$(run 'check_named(nn_tag, V).' 'traceable_of(nn_tag, E).')
check "a contract with a name term based on an agent variable meets the conditions of Appendix D" \
      "V = conditions_met" "$out"
check "and its three predicates have traceable provenance" \
      "E = [identity, friend, tagged]" "$out"
out=$(run 'check_named(nn_tag_loose, V).')
check "with q traced through nothing, it fails traceable provenance, naming tagged" \
      "V = conditions_failed([untraceable([tagged])])" "$out"

# A contract meeting the conditions of Appendix D is compiled
# (definition:compile), tag in Appendix D's form: its assignment and its
# proviso over the extent of its role; the freshness of <q>.y, which occurs
# in no atom required at a role; and the guard, its one guarding role, as
# Section 5.1 writes a guard and Appendix D writes close's (GSC f3c4f43).
# One that fails them is not compiled.  (read, not cat in a command
# substitution: the display's one apostrophe would open a quote to the
# shell's scan of the substitution.)
IFS= read -r -d '' tag_tex <<'TEX'
\begin{align*}
& c'_p := c_p \uplus \{\mathit{tagged}(\langle q\rangle\cdot y)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{friend}(q)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \langle q\rangle\cdot y \text{ is an argument of no atom of } c,\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
check_eq "and tag compiles by definition:compile, guarded by its guarding role" \
         "$(printf '%s' "$tag_tex" | squash)" "$(compiled nn_tag tag)"
out=$(run 'compile_named(nn_tag_loose).')
check "a contract that fails the conditions of Appendix D is not compiled" \
      "% not compiled: nn_tag_loose" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# --- the signed speech-act term (Sections 3.1 to 3.3) ----------------------
# GSNP, Section 3.3's network with provenance, item of arity one, forward
# adding item(sgn(x, Alice)) at Bob, post_forward adding
# posted(sgn(x, Bob), n) at the leader and deliver adding item(sgn(y, Alice))
# at the member.  Section 7.1 certifies it, each signed term counting as its
# two arguments, the speech act and the role (definition:provenance; GSC
# 6d4f147, Appendix D admitting the signed terms of Section 3).
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "GSNP, carrying signed terms in forward, post_forward and deliver, is syntactically grassroots by definition:sg-agent-named" \
      "V = syntactically_grassroots_with_listed_roles" "$out"
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
& \text{provided } \mathit{identity},\mathit{item}(x) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{friend}(\Alice) \in c_{\Bob},\\
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
# require; and the guard, the member, its guarding role (definition:compile,
# guarded by the guarding roles of the schema, GSC f3c4f43).
post_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \uplus \{\mathit{posted}(x,n)\} && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{provided } \{\mathit{identity},\ \mathit{authored}(x),\ \mathit{member}(\langle\Bob\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Bob\rangle\cdot n),\ \mathit{listed}(\Alice,n),\ \mathit{friend}(\Alice)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Bob)),\\
& \text{guarded by } \{\Alice\}
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
& \text{guarded by } \{\Alice\}
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
& \text{guarded by } \{\Alice\}
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
# required at the listed role; and the guard, the leader, as Appendix D
# writes it, guarded by {\Alice}.  GSN's close is GSNP's.
close_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_p := (c_p \setminus \{\mathit{listed}(\Alice,n)\}) \uplus \{\mathit{closed}(n)\} && (p\in\mathrm{ext}_c(\Alice)),\\
& c'_p := c_p \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{provided } \{\mathit{identity},\ \mathit{listed}(\Alice,n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice)),\\
& \text{provided } \{\mathit{identity},\ \mathit{member}(\langle\Alice\rangle\cdot n)\} \subseteq c_p && (p\in\mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{guarded by } \{\Alice\}
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
# 6d4f147, until GSC 1a2a5f3 made the signed term GAI's): sgn_community, sign
# written as a cschema, is admitted, and has no introductory act; sgn_listed
# signs at a listed role and is refused.
out=$(run 'check_named(sgn_community, V).')
check "a signed term of an agent role in a contract with community roles is admitted" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'check_named(sgn_listed, V).' 'compile_named(sgn_listed).')
check "a signed term of a listed role is refused" \
      "V = malformed([signed(note, 2, atom(got, [sgn(x, 2)]), not_agent_role)])" "$out"
check "and the message names the fault: its role is not an agent role" \
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

# The atom of h of clause 2 of def:relay (GSC 410a440), on wd_attest: wd2 with the AI agent's attestation, enter_agent adding attestation(sgn(x, p)) at the agent, the one schema adding agent, and no schema deleting one, and every relay act with a ward role of agent requiring attestation(y) there, y in no other atom it requires.  Clause 2 admits each as an atom of h, h being attestation, so wd_attest is grassroots with wards as wd2 is, and relay_h names each atom of h at its relay act and ward role.
wa_verdict="V = grassroots_with_wards(befriend, [child, agent], [guardianship(child, identity, parent, parenting), guardianship(agent, identity, principal, principal_of)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4)), relay(child_agent_befriend, ward(1, child, 3), ward(2, agent, 4)), relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [])"
out=$(run 'check_named(wd_attest, V).' 'contract_named(wd_attest, C), relay_h(C?, H).')
check "wd_attest, its relay acts requiring the AI agent's attestation, is grassroots with wards" "$wa_verdict" "$out"
check "attestation is the h of clause 2 of def:relay at every ward role of agent" \
      "H = [h(child_agent_befriend, 2, attestation), h(agent_person_befriend, 1, attestation), h(agents_befriend, 1, attestation), h(agents_befriend, 2, attestation)]" "$out"
out=$(run 'check_named(wd2, V).' 'contract_named(wd2, C), relay_h(C?, H).')
check "wd2's relay acts require no atom of h" "H = []" "$out"

# One fixture per condition of that alternative: the guardianships stand, a relay act requires an attestation clause 2 does not admit, and there is no introduction with wards.
no_attest() { # no_attest <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), guardianships(C?, [child, agent], G)." "contract_named($1, C), relay_h(C?, H).")
  check "$1: child and agent keep their guardianships" \
        "G = [guardianship(child, identity, parent, parenting), guardianship(agent, identity, principal, principal_of)]" "$out"
  check "$1: $2, so no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and no atom of h" "H = []" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable)," "$out"
  check_not "$1: no fault but the introductory act's" "volition(" "$out"
  check_not "$1: and every predicate has traceable provenance" "untraceable(" "$out"
}
no_attest wd_attest_unadded "adopt_agent adds agent and no attestation (every schema that adds m adds an atom of h)"
no_attest wd_attest_deleted "revoke deletes an attestation (no schema deletes an atom of h)"
no_attest wd_attest_shared  "agents_befriend requires attestation(y) at both agents (a variable in no other atom it requires)"
no_attest wd_attest_role    "agent_person_befriend requires attestation(p) (arguments speech-act variables)"
no_attest wd_attest_child   "child_adult_befriend requires an attestation of the child, and enter adds none (a ward role of m)"
no_attest wd_attest_free    "agent_person_befriend requires an attestation of the person (a ward role)"

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

# GAI's contract, programs/gai/GAI.glp at 5744daec, checked by hand against GSC 410a440: agent has the guardianship (agent, identity, principal, principal_of) of def:guardianship, enter adding agent, identity and principal(p) at the agent, who had no identity, and principal_of(r) at p, who forbids agent, and no schema deleting principal or principal_of; retired is a leaving mark for {agent} (def:leaving): every guarding role forbids it and none deletes it (clause 1), retire, the one schema adding it, requires agent at that role (clause 2), and enter, the one schema adding principal, forbids it at p (clause 3); befriend is unobstructed but for {agent}, establish adding identity and forbidding nothing but retired, befriend forbidding agent, retired and the friend atom it adds, and every schema adding and deleting friend atoms in pairs (def:unobstructed-marks); agent_befriend is the relay act of a ward role of agent and a free role, and agents_befriend that of two ward roles of agent, each forbidding nothing but agent, retired and the friend atom naming the other related role, and each requiring at a ward role of agent the attestation attestation(y), y in no other atom it requires, which clause 2 of def:relay admits as an atom of h: enter, the one schema adding agent, adds attestation(sgn(x, p)) there, and no schema deletes an attestation.  So it is grassroots with wards (def:introduction-wards, thm:grassroots-wards), the verdict naming retired, as GAI's paper claims (GAI #4 to Code, Code_inbox 2026-10-19 16:57 UTC), and attestation is h.
out=$(run "check($GAI, V)." "wards($GAI, W)." "guardianships($GAI, [agent], G)." "leaving($GAI, [agent], L)." "relay_h($GAI, H).")
check "GAI's agent has a guardianship" \
      "G = [guardianship(agent, identity, principal, principal_of)]" "$out"
check "GAI's leaving marks for {agent} are [retired]" \
      "L = [leaving([guardianship(agent, identity, principal, principal_of)], [retired])]" "$out"
check "GAI's contract is grassroots with wards, retired its leaving mark" \
      "V = grassroots_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [retired])" \
      "$out"
check "attestation is the h of clause 2 of def:relay at the AI agents' ward roles" \
      "H = [h(agent_befriend, 1, attestation), h(agents_befriend, 1, attestation), h(agents_befriend, 2, attestation)]" "$out"
check_not "GAI's predicates have traceable provenance" "untraceable(" "$out"
check_not "GAI's contract satisfies volition" "volition(" "$out"

# --- agent-named contracts with wards (def:sg-wards, CSSN's paper) ---------
# wd_leaving with gather(p?, p[club(*, n)] ; n), whose listed role is the
# agents p lists and requires child: an agent-named contract with wards.
# wd_listed's listed role adds noted, no mark in M, leaving mark, friend,
# parent or parenting atom, so it meets clause 2 of def:sg-wards, and
# clauses 1 and 3 as wd_leaving does: syntactically grassroots with wards.
# wd_listed_leaving's adds retired, a leaving mark, and clause 2 fails: no
# introduction with wards meets def:sg-wards, and the verdict is
# definition:sg-agent-named's.
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
out=$(run 'check_named(an_named_intro, V).')
check "an introductory act with a name term is none, by clause 1" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'check_named(an_named_unary, V).')
check "identity added only by a schema with a name term is unobtainable, by clause 1" \
      "V = not_grassroots([obstructed(befriend, 1, atom(identity, []), unobtainable), obstructed(befriend, 2, atom(identity, []), unobtainable)])" \
      "$out"
# Over a listed role pi[rho(*, u_2, ..., u_r)] (Appendix D, Definitions Role
# and Extent, GSC 8064cfb): its base is the agent role pi, so it joins pi in
# the role graph (clause 4), it is no agent role (clause 3), and clause 2
# asks of it what clause 2 of def:unobstructed asks of an agent role, the
# fault naming it listed_role.  Only an agent role may carry the mark ?, and
# a role that does not guard adds or deletes an atom (Appendix D's
# Definition Act schema, GSC f3c4f43 and 70afdc4): sn_listed_guard's listed
# role carries ?, and sn_listed_inert's adds and deletes nothing, and each is
# refused.
out=$(run "check_variant(sn_listed_forbidden, $SG, V).")
check "a listed role adding friend, which befriend forbids, fails clause 2 at both roles" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(2)]), listed_role(befriend_listed, 2, atom(friend, [role(1)]))), obstructed(befriend, 2, atom(friend, [role(1)]), listed_role(befriend_listed, 2, atom(friend, [role(1)])))])" \
      "$out"
out=$(run "check_variant(sn_listed_witness, $SG, V)." "traceable_variant(sn_listed_witness, $SG, E).")
check "a speech act traced through a listed role only fails clause 3" \
      "V = not_grassroots([untraceable([noted])])" "$out"
check_not "and noted is not among the predicates traced through agent roles" "noted" \
          "$(printf '%s' "$out" | grep '^E = \|GLP> E = ')"
out=$(run "check_variant(sn_listed_guard, $SG, V)." "compile_variant(sn_listed_guard, $SG).")
check "a listed role carrying the mark ? is refused" \
      "V = malformed([listed_guarded(split, 3)])" "$out"
check "and it is not compiled, the message naming the fault" \
      "% not compiled: sn_listed_guard is malformed: split at role 3 has a listed role carrying the mark ?, which only an agent role may carry; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
out=$(run "check_variant(sn_listed_inert, $SG, V).")
check "a listed role adding and deleting nothing is refused" \
      "V = malformed([changes_nothing(idle, 2)])" "$out"

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

# --- the notation (def:schema, definition:schema) --------------------------
# notation_contract prints a contract's schemas from its term in the notation
# of Definition Act schema, def:schema of Section 3.2 and definition:schema of
# Appendix D, a token to a line, an empty line ending a line of the text
# (notation.glp; README, "The notation"); cascade.sh joins the tokens into a
# record's schemas.txt, as notation() does here.  GSG's befriend is the schema
# Section 3.2 displays, befriend(Alice?, Bob?) : identity, knows(Bob), not
# friend(Bob), +friend(Bob)  identity, knows(Alice), not friend(Alice),
# +friend(Alice), after GKK's establish, meet and introduce, each E_i on a line
# labelled by its role; GSN's close is Appendix D's example, with the identity
# the paper leaves out; GSNP's forward carries the signed term of GAI's
# Definition Act schema; and sg_reflexive, GSG's variant, declares friend
# reflexive.
notation() { # notation <term> ; the text notation_contract prints, its tokens joined
  run "notation_contract($1)." | sed -e 's/^\(GLP> \)*//' | perl -ne '
    chomp;
    if ($_ eq "%% notation begin") { $on = 1; next }
    if ($on && $_ eq "%% notation end") { $on = 0; next }
    next unless $on;
    if ($_ eq "") { print "$line\n"; $line = "" } else { $line .= $_ }'
}
one_schema() { # one_schema <name> ; from the notation on stdin, the lines of the schema <name>
  awk -v s="$1(" 'index($0, s) == 1 { on = 1 } on && /^$/ { exit } on'
}
gsg_notation=$(cat <<'EOF'
establish(Alice?) :
    at Alice: +identity

meet(Alice?, Bob?) :
    at Alice: identity, +knows(Bob)
    at Bob: identity, +knows(Alice)

introduce(Alice?, Bob?; c) :
    at Alice: identity, knows(Bob), knows(c)
    at Bob: identity, knows(Alice), +knows(c)

befriend(Alice?, Bob?) :
    at Alice: identity, knows(Bob), not friend(Bob), +friend(Bob)
    at Bob: identity, knows(Alice), not friend(Alice), +friend(Alice)

unfriend(Alice?, Bob) :
    at Alice: identity, -friend(Bob)
    at Bob: identity, -friend(Alice)
EOF
)
check_eq "GSG's schemas print in the notation of def:schema, befriend as Section 3.2 writes it" \
         "$gsg_notation" "$(notation "$GSGT")"
close_notation=$(cat <<'EOF'
close(Alice?, Alice[listed(*, n)]; n) :
    at Alice: identity, +closed(n), -listed(Alice, n)
    at Alice[listed(*, n)]: identity, -member(<Alice>.n)
EOF
)
check_eq "GSN's close prints as Appendix D writes it, its listed role and name term" \
         "$close_notation" "$(notation "$GSNT" | one_schema close)"
forward_notation=$(cat <<'EOF'
forward(Alice?, Bob; x) :
    at Alice: identity, item(x), not authored(x)
    at Bob: identity, friend(Alice), +item(sgn(x, Alice))
EOF
)
check_eq "GSNP's forward prints with the signed term of GAI's Definition Act schema" \
         "$forward_notation" "$(notation "$SG" | one_schema forward)"
REFL=$(run "variant(sg_reflexive, $GSGT, C)." | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check_eq "a contract declaring friend reflexive prints the declaration first" \
         "Reflexive predicates: friend" "$(notation "$REFL" | head -1)"

# --- Numbers (GSC, Section 3.2) -------------------------------------------
# "Act schemas allow numbers: an argument of an atom may be a number or a
# numeric variable, a schema may carry a constraint on its numeric
# variables, which every binding satisfies, and an atom added or deleted may
# carry a multiplicity ...  A number names no agent, so the conditions below
# do not concern them."  The term form is Currencies' GB.glp: num(x), the
# numeric variable x or the number x; times(num(k), A); nschema(Name, Vars,
# Constraint, Roles) over gt, le, succ and or (README, "Writing a
# contract").  GB, the bonds, is Currencies' (programs/currencies/bonds/
# GB.glp at 793a4ddf, GB extending GKK, twenty-one schemas): meet its
# introductory act, unobstructed; every predicate of traceable provenance,
# date naming no agent and coin and bond naming a role or an agent variable
# the other role requires; every role of a schema of arity two guarding, or
# the role graph joined by the knows(Bob) and the instrument of Bob that
# Alice requires.  So it is syntactically grassroots.
echo "--- numbers (GSC, Section 3.2) ---"
GBT=$(term "$GLP_DIR/programs/currencies/bonds/GB.glp" gb)
check "GB's contract is read from programs/currencies/bonds/GB.glp, with numbers, a multiplicity and a constraint" \
      "nschema(mint_bond, [num(d), num(k), num(t)], gt(num(d), num(t)), [rs(guarded, [times(num(k), atom(bond, [role(1), num(d)]))]" "$GBT"
out=$(run "check($GBT, V)." "traceable_preds($GBT, E)." "intro_acts($GBT, A).")
check "GB is syntactically grassroots, the conditions not concerning its numbers" \
      "V = syntactically_grassroots" "$out"
check "GB's predicates all have traceable provenance, date naming no agent" \
      "E = [identity, knows, coin, bond, date]" "$out"
check "GB's introductory act is meet, unobstructed" \
      "A = [intro_act(meet, unobstructed)," "$out"
# The displays of GB's schemas with numbers (GSC, Section 5, in the closest
# form its notation gives: the constraint on a proviso line of its own, and a
# multiplicity k\cdot A, as GB's paper writes it, a multiset with one
# included in c, as def:compile writes a multiset).
redeem_bc_display='\begin{align*}&c'"'"'_{\Alice}:=(c_{\Alice}\setminus\{\mathit{bond}(\Bob,d)\})\uplus\{\text{\textcent}(r)\},\qquadc'"'"'_{\Bob}:=(c_{\Bob}\setminus\{\text{\textcent}(r)\})\uplus\{\mathit{bond}(\Bob,d)\},\\&\text{provided}\mathit{identity},\mathit{knows}(\Bob),\mathit{bond}(\Bob,d)\inc_{\Alice}\text{and}\mathit{identity},\mathit{date}(t),\text{\textcent}(r)\inc_{\Bob},\\&\text{provided}d\let,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's redeem_bc compiles with its constraint on a proviso line of its own" \
         "$redeem_bc_display" "$(compiled_in gb "$GBT" redeem_bc)"
mint_bond_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\uplus\{k\cdot\mathit{bond}(\Alice,d)\},\\&\text{provided}\mathit{identity},\mathit{date}(t)\inc_{\Alice},\\&\text{provided}d>t,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's mint_bond compiles with its multiplicity and its constraint" \
         "$mint_bond_display" "$(compiled_in gb "$GBT" mint_bond)"
pay_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\setminus\{k\cdot\text{\textcent}(\Bob)\},\qquadc'"'"'_{\Bob}:=c_{\Bob}\uplus\{k\cdot\text{\textcent}(\Bob)\},\\&\text{provided}\{\mathit{identity},\mathit{knows}(\Bob),k\cdot\text{\textcent}(\Bob)\}\subseteqc_{\Alice}\text{and}\mathit{identity}\inc_{\Bob},\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's pay requires its k coins as def:compile writes a multiset, included in c" \
         "$pay_display" "$(compiled_in gb "$GBT" pay)"
gb_notation=$(notation "$GBT")
redeem_notation=$(cat <<'EOF'
redeem_bc(Alice?, Bob; d, r, t) | d <= t :
    at Alice: identity, knows(Bob), +coin(r), -bond(Bob, d)
    at Bob: identity, date(t), +bond(Bob, d), -coin(r)
EOF
)
check_eq "GB's redeem_bc prints its constraint after its head" \
         "$redeem_notation" "$(printf '%s\n' "$gb_notation" | one_schema redeem_bc)"
swap_notation=$(cat <<'EOF'
swap_cb(Alice?, Bob?; u, k, v, d1, k1) :
    at Alice: identity, knows(Bob), +k1.bond(v, d1), -k.coin(u)
    at Bob: identity, knows(Alice), +k.coin(u), -k1.bond(v, d1)
EOF
)
check_eq "GB's swap_cb prints its multiplicities, k.A" \
         "$swap_notation" "$(printf '%s\n' "$gb_notation" | one_schema swap_cb)"
advance_notation=$(cat <<'EOF'
advance(Alice; t, t1) | t1 = t + 1 :
    at Alice: identity, +date(t1), -date(t)
EOF
)
check_eq "GB's advance, guarded by no role, prints succ as t1 = t + 1" \
         "$advance_notation" "$(printf '%s\n' "$gb_notation" | one_schema advance)"
check_eq "GB's twenty-one schemas print" "21" "$(printf '%s\n' "$gb_notation" | grep -c ' :$')"

# The checker's fixtures with numbers (contracts.glp, "Numbers").  nm_bonds
# is syntactically grassroots; nm_supplied is too, the number 5 meet
# requires being added by score of arity one at its numeric variable k;
# nm_blocked is not, the date(0) meet forbids being added by advance at its
# numeric variable t1, which has Bob in no role: to the conditions every
# number is one (README, "One thing to know about the reading").
out=$(run "check_named(nm_bonds, V)." "traceable_of(nm_bonds, E)." \
          "check_named(nm_supplied, V)." "check_named(nm_blocked, V).")
check "a contract with numbers, multiplicities and constraints of each kind is syntactically grassroots" \
      "V = syntactically_grassroots" "$(printf '%s\n' "$out" | grep 'V = ' | sed -n 1p)"
check "its predicates all have traceable provenance, bond and date with numeric arguments" \
      "E = [identity, knows, bond, date, coin]" "$out"
check "a number required at the introductory act is supplied by a schema of arity one adding a numeric variable" \
      "V = syntactically_grassroots" "$(printf '%s\n' "$out" | grep 'V = ' | sed -n 2p)"
check "a number forbidden at the introductory act is obstructed by a schema adding a numeric variable" \
      "V = not_grassroots([obstructed(meet, 1, atom(date, [num(0)]), blocked_by(advance, 1, atom(date, [num(t1)])))])" "$out"
nm_mint_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\uplus\{k\cdot\mathit{bond}(\Alice,d)\},\\&\text{provided}\mathit{identity},\mathit{date}(t)\inc_{\Alice},\\&\text{provided}d>t\lord\le0,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "nm_bonds's mint compiles its constraint, or as \\lor" \
         "$nm_mint_display" "$(compiled nm_bonds mint)"
nm_mint_notation=$(cat <<'EOF'
mint(Alice?; d, k, t) | d > t or d <= 0 :
    at Alice: identity, date(t), +k.bond(Alice, d)
EOF
)
NMB=$(run "contract_named(nm_bonds, C)." | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check_eq "nm_bonds's mint prints its constraint after its head, or as or" \
         "$nm_mint_notation" "$(notation "$NMB" | one_schema mint)"

# The malformations the passage forbids, each refused (README, "The
# verdicts"): a role that does not guard and changes nothing, numbers or no
# (def:schema; GB's mature of b9369200, which redeem_bc and redeem_bb
# replace at 793a4ddf); a multiplicity on an atom required and not deleted,
# and on one forbidden, the passage admitting one on an atom added or
# deleted; a multiplicity that is not a number; a constraint on an agent,
# the passage admitting one on the numeric variables; an atom added with a
# multiplicity and deleted at one role (def:schema); and a head that lists a
# number and a numeric variable twice and leaves out the numeric variables
# the atoms and the constraint carry.
out=$(run "check_named(nm_reading_role, V)." "check_named(nm_mult_kept, V)." \
          "check_named(nm_mult_forbidden, V)." "check_named(nm_mult_agent, V)." \
          "check_named(nm_constraint_agent, V)." "check_named(nm_added_deleted, V)." \
          "check_named(nm_head, V)." "compile_named(nm_head).")
check "a role that reads a number and changes nothing is refused" \
      "V = malformed([changes_nothing(mature, 2)])" "$out"
check "a multiplicity on an atom required and not deleted is refused" \
      "V = malformed([multiplicity_not_added(spend, 1, atom(coin, [role(1)]))])" "$out"
check "a multiplicity on an atom forbidden is refused" \
      "V = malformed([multiplicity_not_added(hoard, 1, atom(coin, [role(1)]))])" "$out"
check "a multiplicity that is not a number is refused" \
      "V = malformed([multiplicity_not_numeric(mint, 1, atom(coin, [role(1)]))])" "$out"
check "a constraint on an agent variable is refused" \
      "V = malformed([constraint_not_numeric(mint, pvar(u))])" "$out"
check "an atom added with a multiplicity and deleted at one role is refused" \
      "V = malformed([added_and_deleted(churn, 1, atom(coin, [role(1)]))])" "$out"
check "a head listing a number and a numeric variable twice, and leaving out two, is refused" \
      "V = malformed([head_not_variable(mint, num(5)), head_repeated(mint, num(k)), not_in_head(mint, num(d)), not_in_head(mint, num(t))])" "$out"
check "and it is not compiled, the faults named" \
      "lists in its head an entry that is not a variable;" "$out"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
