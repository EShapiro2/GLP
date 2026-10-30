#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/gsc).
#
#   bash programs/gsc/test_gsc.sh
#
# GSC's contracts GKK, GSG and GSN (programs/gsc/), GKK the base contract every contract extends, its establish forbidding the identity it adds (GKK:1, GSC b2bb8be), its introductory act acquaint, knows of traceable provenance and introduce's roles joined by knows(q) at p (GSC, Sections 2.1 and 7.1; intro_acts/2 and variants of GKK), GSN being the social network of /Grassroots/GSC Section 3.3, which Section 7.1 certifies by hand by def:syntactically-grassroots, one definition for a contract of agent roles and listed roles (GSC ac7ad63); each read from its file, each schema listing its variables in its head (GSC 0228e79), each extending GKK holding GKK's schemas as GKK.glp writes them; and contracts over GSG, broken in one place each, one per way of failing the three conjuncts of def:syntactically-grassroots; then clause 1 of def:unobstructed on a schema of arity one that forbids the atom it adds; then contracts declaring a predicate reflexive (GAI's appendix "Contracts with Wards"); then schemas not formed as def:schema requires, refused, and a guarding role adding and deleting nothing, admitted; then the compilation of Section 5, against the displays of befriend and close in Section 5.1; then small contracts on the speech-act variables of traceable provenance and on volition by connected component, and a contract with a name term based on an agent variable, which def:role does not admit, refused; then the signed speech-act term, on contracts admitted with it and on contracts refused for a signed term where it is forbidden; then contracts with wards (GAI's appendix "Contracts with Wards"), on a fixture of children and parents, on one of children and AI agents, on each broken in one place per clause, on one of children and AI agents whose relay acts require the AI agent's attestation, an atom of h of clause 2 of def:relay, which adds no record of it and is refused, and on it broken in one place per condition of that alternative, on one with a leaving mark and on it broken in one place per clause of def:leaving, and on agent-named contracts with wards (def:sg-wards, CSSN's paper) with a listed role; then contracts with listed roles and name terms over GSN, failing a condition each by a listed role, and listed roles refused for the mark ? or for changing nothing; then listed roles formed otherwise than def:role allows, and schemas whose head is not their variables (def:schema); then the notation of Definition Speech-act schema, printed from a contract's term (notation.glp); then cleared atoms (GSC 327e035): GSN's unfriend in the notation and compiled with K_p, and fixtures on each rule --- a cleared atom puts no edge in the role graph, is no change a role makes, matches no atom its role adds, a wildcard only in an atom marked -, and a schema of arity one that clears supplies nothing under clause 1 of def:unobstructed; then the values speech-act schemas allow (GSC, Section 3.2 and its appendix "Values in Speech-Act Schemas"), which traceable provenance and volition do not concern, a value naming no agent: a contract with numbers certified, def:unobstructed on numbers --- a number required and supplied, or not, by a schema of arity one at a binding satisfying its constraint, a number matched by equality, a number forbidden and added, or not, at such a binding, a value the introduction chooses, a value below every number the contract writes, and a multiplicity a positive whole number at every binding --- and a fixture of each malformation the passage forbids.  The federation extension of speech-act schemas left GSC's paper (GSC f3c4f43) and its checks left with its code (Coordination #7, 2026-10-20).  A contract is its owner's and the checker reads its term (contracts.glp, the header): a fixture that changes one is a variant, given by name and the owner's term.  This script expects only GSC's contracts, GKK, GSG and GSN, and the checker's fixtures; every other owner's contract is checked and recorded by Code from its own .glp, by cascade.sh (GSC #8, 2026-10-25 18:26 UTC).  Exits non-zero if any check fails.

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
# checker and vGLP's compiler load from there, each schema written with its
# roles' names, schema(Name, Names, Vars, Roles) or cschema(Name, Names,
# Vars, [], Roles), Names the names of its roles in order and Vars its
# variables in the order of its head (GSC, Section 2 and Definition
# Speech-act schema; GKK.glp, GSG.glp and GSN.glp at GLP ec7c578c and 523bbe23).
# The variants of a contract below write the schemas they add or change the
# same way (contracts.glp, the header).
# GKK, the base contract: establish, GKK:1, adds identity, requires nothing
# and forbids the identity it adds (GKK:1, "act once", GSC b2bb8be; GKK.glp
# at GLP 79db2fae), and every other role requires identity.  GSG and GSN
# extend GKK (Udi, 2026-10-20, to GSC #5), so each opens with GKK's three
# schemas, as GKK.glp writes them: GKK_SCHEMAS is GKK's term, its closing
# bracket left off.  GSGT is GSG, the contract most variants below are made
# on, and GSNT is GSN, with the listed role of close (GSC 8064cfb).
GKKT=$(term "$GLP_DIR/programs/gsc/GKK.glp" gkk)
check "GKK's contract is read from programs/gsc/GKK.glp, establish forbidding the identity it adds, then acquaint and introduce, each naming its roles" \
      "[schema(establish, ['Alice'], [], [rs(guarded, [atom(identity, [])], [], [], [atom(identity, [])])]), schema(acquaint, ['Alice', 'Bob'], [], [rs(guarded, [atom(knows, [role(2)])], [], [atom(identity, [])], []), rs(guarded, [atom(knows, [role(1)])], [], [atom(identity, [])], [])]), schema(introduce, ['Alice', 'Bob'], [pvar(c)], " "$GKKT"
GKK_SCHEMAS="${GKKT%]}"
GSGT=$(term "$GLP_DIR/programs/gsc/GSG.glp" gsg)
check "GSG's contract is read from programs/gsc/GSG.glp, GKK's schemas first" "$GKK_SCHEMAS" "$GSGT"
GSNT=$(term "$GLP_DIR/programs/gsc/GSN.glp" gsn)
check "GSN's contract, the network without provenance, is read from programs/gsc/GSN.glp, GKK's schemas first" \
      "$GKK_SCHEMAS" "$GSNT"
# GSG: befriend is an introductory act and meet too; meet is unobstructed,
# identity being added by establish, of arity one, guarded, requiring
# nothing and forbidding only the identity it adds, which meet requires once
# at each role (def:unobstructed, clause 1), and meet forbidding nothing
# (clause 2); identity, knows and friend have traceable provenance
# (def:grounded); unfriend's role Alice requires friend(Bob) (def:volition);
# so it is syntactically grassroots (def:syntactically-grassroots).  GSN
# (GSN.glp at 542ec8ca): a group is the community name <a>.n and close takes
# the listed role a[listed(*, n)], every member a lists (GSC 8064cfb,
# def:role), every name term based on the agent role a.  One definition
# decides it (GSC ac7ad63): meet is unobstructed, and close's listed role
# adds no atom of a predicate meet forbids, meet forbidding none (clause 2);
# its message, group and delivery schemas add atoms whose arguments are
# roles, the name <a>.n and speech-act variables no role requires or that an
# atom required at an agent role carries (def:grounded); and every role
# graph is connected or has a guarding agent role in each component, close's
# two roles sharing the base a, the base of a listed role being its agent
# role (def:volition).  So it is syntactically grassroots (Theorem
# "Grassroots realisation"), and not malformed: message's and post's guarding
# role adds and deletes nothing, which def:schema admits.
out=$(run "check($GSGT, V).")
check "GSG is syntactically grassroots" "V = syntactically_grassroots" "$out"
out=$(run "check($GSNT, V).")
check "the program loads" "Loaded program" "$out"
check_not "no type error" "Error loading" "$out"
check "GSN, with the listed role of close, is syntactically grassroots by the one definition" \
      "V = syntactically_grassroots" "$out"
check_not "GSN's message and post, guarded at a role that adds and deletes nothing, are admitted" \
          "malformed(" "$out"

# GKK (programs/gsc/GKK.glp; GSC, Section 2.1, its schemas in Section 3.3's
# table, certified with GSN in Section 7.1).  Meet is its introductory act,
# unobstructed: guarded in both roles, it adds knows of the other at each,
# and it requires identity once at each role, which establish adds, of arity
# one, guarded, requiring nothing and forbidding only that atom, and forbids
# nothing (def:introduction, def:unobstructed, its clause 1 at GSC b2bb8be;
# Section 7.1); introduce is no introductory act, adding at Alice no
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
check "GKK: acquaint is its one introductory act, and unobstructed" \
      "A = [intro_act(acquaint, unobstructed)]" "$out"
check "GKK: knows has traceable provenance, with identity" "E = [identity, knows]" "$out"
# In GSG befriend is an introductory act too, but obstructed: it requires
# knows of the other, which no schema of arity one adds (GSC, Section 3.6,
# the example after def:introduction); meet is unobstructed, so GSG is
# syntactically grassroots.  GSN likewise.
out=$(run "intro_acts($GSGT, A)." "traceable_preds($GSGT, E).")
check "GSG: acquaint unobstructed, befriend an introductory act obstructed by the knows of the other it requires at each role, which no schema of arity one adds" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" "$out"
check "GSG: identity, knows and friend have traceable provenance" "E = [identity, knows, friend]" "$out"
out=$(run "intro_acts($GSNT, A).")
check "GSN: acquaint unobstructed, befriend obstructed" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" "$out"
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

# Clause 1 of def:unobstructed (GSC, Section 3.6, at GSC b2bb8be): the schema
# of arity one adding an atom the introductory act requires may require
# nothing and forbid nothing or only that atom, the act then requiring that
# atom once at the role.  GKK's establish forbids the identity it adds, and
# meet requires it once at each role (above).  uo_twice's meet requires
# identity twice at Alice, which establish, forbidding it, does not supply,
# a fault for each copy; uo_twice_free's establish forbids nothing and
# supplies it however many times it is required; uo_other's establish
# forbids an atom other than the one it adds and supplies nothing; nm_once's
# score forbids only the score it adds at the binding sending both its
# value variables to the number meet requires once at each role.
out=$(run 'check_named(uo_twice, V).' 'check_named(uo_twice_free, V).' \
          'check_named(uo_other, V).' 'check_named(nm_once, V).')
uov() { printf '%s\n' "$out" | sed -e 's/^\(GLP> \)*//' | grep '^V = ' | sed -n "$1p"; }
check_eq "an atom required twice is not supplied by a schema of arity one forbidding it" \
         "V = not_grassroots([obstructed(meet, 1, atom(identity, []), unobtainable), obstructed(meet, 1, atom(identity, []), unobtainable)])" "$(uov 1)"
check_eq "an atom required twice is supplied by a schema of arity one forbidding nothing" \
         "V = syntactically_grassroots" "$(uov 2)"
check_eq "a schema of arity one forbidding an atom other than the one it adds supplies nothing" \
         "V = not_grassroots([obstructed(meet, 1, atom(identity, []), unobtainable), obstructed(meet, 2, atom(identity, []), unobtainable)])" "$(uov 3)"
check_eq "a number required once is supplied by a schema of arity one forbidding only the atom it adds, at a binding the match extends" \
         "V = syntactically_grassroots" "$(uov 4)"

# Befriend guarded in one role only: nothing is an introductory act, and
# befriend fails volition as well.
out=$(run 'check_named(sg_unguarded, V).')
check "one guard dropped: no introductory act" \
      "no_introductory_act" "$out"
check "one guard dropped: befriend fails volition" \
      "volition(befriend, 1, 2)" "$out"

# The variants below are made on GSG, a contract of agent roles.  An act
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
check "on GSG with gossip acquaint is unobstructed, and the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "gossip blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1," "$out"
check "gossip blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"
check_not "gossip itself satisfies volition" "volition(gossip" "$out"
# The same on GSN, a contract with a listed role, the second part of
# clause 2 of def:unobstructed asked of the agent roles.  GSN extends GKK, so
# acquaint stays an unobstructed introductory act there too, and befriend's
# obstructions are read off intro_acts.
out=$(run "check_variant(sg_gossip, $GSNT, V)." "intro_acts_variant(sg_gossip, $GSNT, A).")
check "on GSN with gossip acquaint is unobstructed, and the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "on GSN gossip blocks befriend at both roles" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1, atom(friend, [pvar(u)]))), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1, atom(friend, [pvar(u)])))]))]" \
      "$out"

# Volition above arity two is connectedness of the role graph, or a guarding
# role in every connected component of it, not a test on every pair: a schema
# of arity four guarded at one role, whose role graph is the path 1-2-3-4,
# passes; cutting one edge of the path splits it, and the half {3,4} holds no
# guarding role.
out=$(run "check_variant(sg_chain, $GSGT, V).")
check "a path role graph at arity four certifies" \
      "V = syntactically_grassroots" "$out"
out=$(run "check_variant(sg_chain, $GSNT, V).")
check "and on GSN, a contract with a listed role" \
      "V = syntactically_grassroots" "$out"

out=$(run "check_variant(sg_chain_cut, $GSGT, V).")
check "cutting an edge of the path fails volition" \
      "V = not_grassroots([volition(chain, 1, 3)])" "$out"

# def:volition by connected component (GSC 7652554): tri(p?, q, r?) is
# guarded neither in all its roles nor connected, its role graph falling
# into {1,2} and {3}, each holding a guarding role, so it meets def:volition,
# as it does written as a cschema (sg_tri_cschema, below).
out=$(run "check_variant(sg_tri, $GSGT, V).")
check "a guarding role in every connected component meets volition" \
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

# cs18, the checker's fixture of eighteen schemas, twenty-eight expanded: a
# child-safe social contract as CSSN drafted it in Legal's inbox on 2026-08-15
# and 2026-08-16 and as it was brought to their paper on 2026-10-09, since
# revised by CSSN (its contract is programs/cssn/CSSN.glp, CSSN's).  Its
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

# --- reflexive predicates (GAI's appendix "Contracts with Wards") ---------
# A contract may declare a predicate of arity one reflexive.  The social graph
# with friend declared so certifies as it does without.  These variants are
# made on GSG.
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
check "and the message names the fault, in the words of GAI's appendix" \
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
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(friend, [role(1)]), unobtainable), obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" \
      "$out"
out=$(run "check_variant(sg_refl_required, $GSGT, V)." "intro_acts_variant(sg_refl_required, $GSGT, A).")
check "friend of its own role required, friend reflexive: the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "friend of its own role required, friend reflexive: clause 1 skips it, befriend obstructed by knows alone" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable)]))]" \
      "$out"

# Clause 3 of def:unobstructed: the introductory act forbids at a role no atom
# of a reflexive predicate naming that role.
out=$(run "intro_acts_variant(sg_refl_forbidden, $GSGT, A).")
check "friend of its own role forbidden, friend reflexive: clause 3 fails" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(1)]), reflexive), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), reflexive)]))]" \
      "$out"

# A contract with no schemas.
out=$(run 'check_named(nonesuch, V).')
check "the empty contract has no introductory act" \
      "V = not_grassroots([no_introductory_act])" "$out"

# --- the form of a schema (def:schema) -------------------------------------
# def:schema at GSC bd0ebc4 requires +(i) and -(i) disjoint, and +(i) u -(i)
# nonempty at every role that is not a guarding role and at some role; a
# guarding role may add and delete nothing (the sentence after the example
# of def:schema).  GSN's message and post are
# guarded at a role that adds and deletes nothing and are admitted, above.  On
# GSG a role that does not guard and adds and deletes nothing, a schema no
# role of which adds or deletes an atom, and an atom added and deleted at one
# role are each refused, the contract malformed, and it is not compiled.
# def:schema requires the same of a listed role (GSC 70afdc4;
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
# The displays of the paper, befriend's in Section 5.1, after def:compile,
# transcribed from /Grassroots/GSC/sections/05-compilation.tex at GSC
# aca046b, and close's below, compared with the whitespace removed: the compiler emits a display one token to a line, GLP having no
# string concatenation, and a newline is whitespace to LaTeX.  The comma and
# the full stop that close the paper's two displays belong to the sentences
# around them, not to the compiled form, and are not expected here.  The
# layout is Udi's ruling (GSC #3 to Code, 2026-10-18 21:49 UTC), which the
# displays print: the assignments, the required atoms on one proviso line and
# the forbidden atoms on the next, each in role order, a line omitted when it
# has no atom, and the guard on a line of its own.  Befriend is GSG's, which
# requires identity and knows of the other at both roles (GKK:1, GSG:1;
# GSC 21c1b28, Section 5.1, its two required atoms at a role listed before
# \in).
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

# A contract compiles schema by schema (Section 5.1), and only after it is
# checked: GSN's fourteen schemas, GKK's establish, acquaint and introduce,
# GSG's befriend and unfriend, message, and the eight of a group, to
# fourteen displays (GSN.glp at GLP 51b72a13).
out=$(run "compile_contract(gsn, $GSNT).")
check_eq "GSN compiles to fourteen displays" "14" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# sg_chain_cut on GSG, which fails volition (above): sg_gossip certifies on
# GSG and on GSN, both extending GKK, whose acquaint stays unobstructed.
out=$(run "compile_variant(sg_chain_cut, $GSGT).")
check "a contract that fails the conditions is not compiled" \
      "% not compiled: sg_chain_cut" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# A schema with no guarding role compiles to transactions with an empty guard
# (Section 5.1); cs18's deliver is unguarded at both roles.
out=$(run 'compile_schema(cs18, deliver).')
check "an unguarded schema compiles to an empty guard" \
      "\\text{guarded by } \\emptyset" "$(printf '%s' "$out" | tr '\n' ' ')"

out=$(run 'compile_schema(cs18, nosuch).')
check "a schema the contract does not hold is reported" \
      "% no schema named" "$(printf '%s' "$out" | tr '\n' ' ')"


# The speech-act variables of def:grounded.  The first requirement asks
# after the arguments of an added atom outside Y, the second after every
# speech-act variable that is an argument of it or occurs in a name term among
# its arguments.  sv_signed is the smallest case on which it matters that the
# first clause leaves Y out: a speech act signed and required nowhere.  The
# sv_ contracts are agent-named, of agent roles, and are decided by
# def:grounded, each psi required at an agent role; none has an
# introductory act.
out=$(run 'check_named(sv_signed, V).' 'traceable_of(sv_signed, E).')
check "a signed speech act required nowhere: item has traceable provenance" \
      "E = [item, got, mark]" "$out"
check "and the item take requires joins its role graph, no introductory act failing it" \
      "V = not_grassroots([no_introductory_act])" "$out"

# The case of sg_svar_loose written with a cschema: the same speech act
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

# Volition by connected component (def:volition) with a schema written as a
# cschema: sg_tri_cschema, a variant of the social graph without the signed
# term, the fixture sg_plain.
SGP=$(run 'contract_named(sg_plain, C).' | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check "the social graph without the signed term, sg_plain, is read" \
      "[schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])]" "$SGP"

# sg_tri with tri written as a cschema, of agent roles and no name term:
# def:volition holds of tri, each component holding a guarding role, as it
# holds of sg_tri.
out=$(run "check_variant(sg_tri_cschema, $SGP, V).")
check "tri as a cschema is syntactically grassroots, as tri is" \
      "V = syntactically_grassroots" "$out"

# A name term based on an agent variable, which def:role does not admit
# (GSC ac7ad63: a name term of a schema is <pi> for an agent role pi of it,
# or sigma.y): nn_tag's tag(p? ; q, y) adds tagged(<q>.y), <q>.y based on
# the agent variable q, so the contract is malformed, the name term named,
# and it is not compiled; nn_tag_loose, the same with tag requiring no
# friend(q), likewise.  traceable_of decides def:grounded alone, on the
# term as written.
out=$(run 'check_named(nn_tag, V).' 'traceable_of(nn_tag, E).')
check "a name term based on an agent variable is refused" \
      "V = malformed([name_term_base(tag, ext(own_var(q), y))])" "$out"
check "and traceable_of gives its three predicates" \
      "E = [identity, friend, tagged]" "$out"
out=$(run 'check_named(nn_tag_loose, V).')
check "with q traced through nothing, it is refused alike" \
      "V = malformed([name_term_base(tag, ext(own_var(q), y))])" "$out"
out=$(run 'compile_named(nn_tag).')
check "a contract refused for a name term is not compiled, the message naming the fault" \
      "% not compiled: nn_tag is malformed: tag has a name term whose base is not an agent role of the schema; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"
out=$(run 'compile_named(nn_tag_loose).')
check "nor is nn_tag_loose" \
      "% not compiled: nn_tag_loose" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# The form of a display (compile.glp): a schema of agent roles and no name
# term in the short form, befriend's in Section 5.1 (above), as Section 5.1
# prints befriend of Section 3.3; any other in the general form, a line per
# agent, as def:compile's display has it and Section 5.1 prints close's (Udi,
# 2026-10-23, to GSC #7, reported by GSC #7 Cowork): the agent of each agent
# role, its line joining what every role whose extent holds it does there,
# then the other agents q of a listed role's extent; the required atoms of
# every role on one proviso line, a listed role's in c_q over its extent;
# the forbidden atoms on the next; the freshness of a name; the guard.  The
# compiler names role i by its index, role 1 \Alice and role 2 \Bob.
#
# create(a? ; n) forms the name <a>.n, which occurs in no atom any role
# requires, so def:compile asks that it be an argument
# of no atom of c; join requires member(<a>.n) at the leader, so join asks
# it not.
# (read, not cat in a command substitution: its one apostrophe would open a
# quote to the shell's scan of the substitution)
IFS= read -r -d '' create_tex <<'TEX'
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{member}(\langle\Alice\rangle\cdot n),\mathit{listed}(\Alice,n)\},\\
& \text{provided } \mathit{identity} \in c_{\Alice},\\
& \text{provided } \{\mathit{member}(\langle\Alice\rangle\cdot n),\mathit{closed}(n)\} \cap c_{\Alice} = \emptyset,\\
& \text{provided } \langle\Alice\rangle\cdot n \text{ is an argument of no atom of } c,\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
check_eq "create compiles with the freshness of the name it forms" \
         "$(printf '%s' "$create_tex" | squash)" "$(compiled_in gsn "$GSNT" create)"
check_not "join, requiring member(<a>.n), asks no freshness of it" \
          "noatomof" "$(compiled_in gsn "$GSNT" join)"

# close(Alice?, Alice[listed(*, n)]; n), Section 5.1's example of
# def:compile (GSC f3e3f40), with the identity the paper leaves out and the
# extent the paper calls L written out: the leader Alice lists herself,
# requiring member(<Alice>.n) and listed(Alice, n) (GSN.glp at GLP
# 51b72a13), so she is in the extent of the listed role, and her line joins
# what both roles do, listed(Alice, n) and member(<Alice>.n) deleted and
# closed(n) added; every other member q she lists deletes
# member(<Alice>.n); the required atoms, the leader's and those of the
# listed role, over its extent; no freshness line, member(<Alice>.n) being
# required; and the guard, the leader.
close_gsn=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\mathit{listed}(\Alice,n),\mathit{member}(\langle\Alice\rangle\cdot n)\}) \uplus \{\mathit{closed}(n)\},\\
& c'_q := c_q \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)]),\ q \neq \Alice),\\
& \text{provided } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n),\mathit{listed}(\Alice,n) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n) \in c_q \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "GSN's close compiles a line per agent, the leader's joining what both her roles do, as Section 5.1 prints it" \
         "$close_gsn" "$(compiled_in gsn "$GSNT" close)"

# --- the signed speech-act term (GAI's extension, held here for GAI) -------
# An introduction through the signed term alone: each role of exchange adds
# an atom naming the other only as the role of its signed term (Section 3.1,
# what an atom names), so it is an introductory act; with plain speech-act
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

# GAI's Definition Speech-act schema admits a signed term sgn(y, pi) for pi an
# agent role, in a schema written as a cschema too: sgn_community, sign
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

# --- contracts with wards (GAI's appendix "Contracts with Wards", GAI ddf0a33)
# wd, children and their parents in the schemas the appendix describes: not
# syntactically grassroots, and grassroots with wards (def:introduction-wards,
# thm:grassroots-wards) through befriend, the mark child, the guardianship
# (child, identity, parent, parenting, parenting), its v its w, child_adult_befriend and
# child_befriend; and it has no leaving mark (def:leaving): no mark of it
# other than child is forbidden at every guarding role (clause 1).  wards_of
# gives what the search found,
# whatever the verdict, and guardianships the guardianships for a set of
# marks.
wd_found="introduction_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])"
out=$(run 'check_named(wd, V).' 'wards_of(wd, W).' 'contract_named(wd, C), guardianships(C?, [child], G).')
check "wd is grassroots with wards, with no leaving mark" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])" \
      "$out"
check "wards_of gives its introduction with wards" "W = $wd_found" "$out"
check "guardianships gives child's" \
      "G = [guardianship(child, identity, parent, parenting, parenting)]" "$out"
out=$(run 'compile_named(wd).')
check_eq "a contract grassroots with wards is compiled, wd to eight displays" "8" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# def:guardianship, one fixture per clause: each leaves child no
# guardianship, and the verdict is that of Section 3, the introductory act
# enter obstructed: establish, forbidding the identity it adds, supplies the
# identity enter requires at its first role (clause 1 of def:unobstructed,
# GSC b2bb8be), and enter adds the child it forbids there at a binding that
# gives the other party no role (clause 2).
no_guardianship() { # no_guardianship <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), guardianships(C?, [child], G).")
  check "$1: $2, so child has no guardianship" "G = []" "$out"
  check "$1: and no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, " "$out"
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
  check "$1: child keeps its guardianship" "G = [guardianship(child, identity, parent, parenting, parenting)]" "$out"
  check "$1: $2, so no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, " "$out"
  check_not "$1: no fault but the introductory act's" "volition(" "$out"
}
no_wards wd_req_unsupplied    "befriend requires an atom no schema of arity one requiring nothing adds (def:unobstructed-marks, clause 1)"
no_wards wd_forb_other        "befriend forbids an atom neither a mark in M nor a leaving mark (def:unobstructed-marks, clause 2)"
no_wards wd_forb_added        "befriend forbids the friend atom it adds, which clause 2 admits no longer (def:unobstructed-marks, clause 2, GAI ddf0a33)"
no_wards wd_two_atoms         "befriend adds two atoms at a role (def:unobstructed-marks)"
no_wards wd_no_adult_relay    "no relay act with a ward role of child and a free role (def:introduction-wards)"
no_wards wd_no_child_relay    "no relay act with two ward roles of child (def:introduction-wards)"
no_wards wd_relay_extra_role  "a role of the relay act is neither a party nor a guardian role (def:relay, clause 1)"
no_wards wd_relay_guard_names "a guardian role adds an atom naming the other guardian role (def:relay, clause 1)"
no_wards wd_relay_req         "a free role requires parent(p) (def:relay, clause 2)"
no_wards wd_relay_forb        "a guardian role forbids friend(a) (def:relay, clause 3)"
no_wards wd_relay_forb_ward   "a ward role forbids friend of the other related role, which clause 3 admits no longer (def:relay, clause 3, GAI ddf0a33)"
no_wards wd_relay_forb_free   "a free role forbids friend of the ward (def:relay, clause 3, GAI ddf0a33)"
no_wards wd_relay_unguarded   "the adult's role does not guard, so the two are no parties (def:relay)"

# The clause on the symmetry of e is gone from def:unobstructed-marks (GAI
# ddf0a33): a schema adding friend of one role only, and one adding friend of
# an agent variable at a role that forbids no mark, are admitted, and each
# contract is grassroots with wards as wd is.
admitted_wards() { # admitted_wards <fixture> <what it adds>
  out=$(run "check_named($1, V).")
  check "$1: $2, and it is grassroots with wards" \
        "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])" \
        "$out"
}
admitted_wards wd_asymmetric "follow adds friend(q) at p and no friend(p) at q"
admitted_wards wd_introduce  "introduce adds friend(c) at q, c an agent variable, at a role that forbids no mark"

# wd2, children and AI agents: two marks, a guardianship for each, befriend
# unobstructed but for both, and the relay acts of every pair -- child with a
# free role, child with child, child with agent, agent with a free role,
# agent with agent -- in the order of the set.
out=$(run 'check_named(wd2, V).')
check "wd2 is grassroots with wards for the two marks child and agent" \
      "V = grassroots_with_wards(befriend, [child, agent], [guardianship(child, identity, parent, parenting, parenting), guardianship(agent, identity, principal, principal_of, principal_of)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4)), relay(child_agent_befriend, ward(1, child, 3), ward(2, agent, 4)), relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [])" \
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
      "G = [guardianship(child, identity, parent, parenting, parenting)]" "$out"
check "and there is no introduction with wards" \
      "W = no_introduction_with_wards" "$out"

# The atom of h of clause 2 of def:relay, on wd_attest: wd2 with the AI agent's attestation, enter_agent adding attestation(sgn(x, p)) at the agent, the one schema adding agent, and no schema deleting one, and every relay act with a ward role of agent requiring attestation(y) there, y in no other atom it requires.  Clause 2 at GAI 11d7133 admits an atom of h only where every schema that adds v(rho) at a role, v principal_of here, adds there the record h'(rho, args), and adds the atom of h at rho or requires the record at a role requiring w(rho); enter_agent adds no record, so clause 2 admits no attestation, the relay acts with a ward role of agent fail, and wd_attest has no introduction with wards.  wr below adds the record at every guardian and is certified.
out=$(run 'check_named(wd_attest, V).' 'wards_of(wd_attest, W).' 'contract_named(wd_attest, C), relay_h(C?, H).')
check "wd_attest, enter_agent adding no record of the attestation at the principal, has no introduction with wards (def:relay, clause 2, GAI 11d7133)" \
      "W = no_introduction_with_wards" "$out"
check "and no atom of h" "H = []" "$out"
out=$(run 'check_named(wd2, V).' 'contract_named(wd2, C), relay_h(C?, H).')
check "wd2's relay acts require no atom of h" "H = []" "$out"

# One fixture per condition of that alternative: the guardianships stand, a relay act requires an attestation clause 2 does not admit, and there is no introduction with wards.
no_attest() { # no_attest <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), guardianships(C?, [child, agent], G)." "contract_named($1, C), relay_h(C?, H).")
  check "$1: child and agent keep their guardianships" \
        "G = [guardianship(child, identity, parent, parenting, parenting), guardianship(agent, identity, principal, principal_of, principal_of)]" "$out"
  check "$1: $2, so no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and no atom of h" "H = []" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, " "$out"
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
wl_found="introduction_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])"
out=$(run 'check_named(wd_leaving, V).' 'wards_of(wd_leaving, W).' 'leaving_of(wd_leaving, [child], L).')
check "wd_leaving is grassroots with wards, retired its leaving mark" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
check "wards_of names retired with its introduction with wards" "W = $wl_found" "$out"
check "leaving_of returns retired for {child}, with the guardianship of child" \
      "L = [leaving([guardianship(child, identity, parent, parenting, parenting)], [retired])]" "$out"
out=$(run 'compile_named(wd_leaving).')
check_eq "and it is compiled, to nine displays" "9" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# def:leaving, one fixture per clause: each leaves retired no leaving mark,
# child keeping its guardianship, so befriend forbids an atom that is neither
# a mark in M nor a leaving mark (def:unobstructed-marks, clause 2), and there
# is no introduction with wards.
no_leaving() { # no_leaving <fixture> <what it breaks>
  out=$(run "leaving_of($1, [child], L)." "wards_of($1, W)." "check_named($1, V)." \
            "contract_named($1, C), guardianships(C?, [child], G).")
  check "$1: $2, so retired is not a leaving mark" \
        "L = [leaving([guardianship(child, identity, parent, parenting, parenting)], [])]" "$out"
  check "$1: child keeps its guardianship" "G = [guardianship(child, identity, parent, parenting, parenting)]" "$out"
  check "$1: and there is no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable)," "$out"
}
no_leaving wd_leave_unforbidden "retire's guarding role does not forbid retired (clause 1)"
no_leaving wd_leave_deleted     "revive deletes retired (clause 1)"
no_leaving wd_leave_unmarked    "quit adds retired at a role requiring no mark in M (clause 2)"
no_leaving wd_leave_guardian    "adopt adds parent(p) at r and does not forbid retired at p (clause 3)"

# --- agent-named contracts with wards (def:sg-wards, CSSN's paper) ---------
# wd_leaving with gather(p?, p[club(*, n)] ; n), whose listed role is the
# agents p lists and requires child: an agent-named contract with wards.
# wd_listed's listed role adds noted, no mark in M, leaving mark, friend,
# parent or parenting atom, so it meets clause 2 of def:sg-wards, and
# clauses 1 and 3 as wd_leaving does: syntactically grassroots with wards.
# wd_listed_leaving's adds retired, a leaving mark, and clause 2 fails: no
# introduction with wards meets def:sg-wards, and the verdict is
# def:syntactically-grassroots', its first conjunct failing by the first
# part of clause 2 of def:unobstructed.
out=$(run 'check_named(wd_listed, V).')
check "a listed role adding no mark, leaving mark, friend or guardianship atom: syntactically grassroots with wards" \
      "V = syntactically_grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
out=$(run 'check_named(wd_listed_leaving, V).' 'wards_of(wd_listed_leaving, W).')
check "a listed role adding a leaving mark fails clause 2 of def:sg-wards" \
      "W = no_introduction_with_wards" "$out"
check "and the verdict names the listed role, by clause 2 of def:unobstructed" \
      "obstructed(enter, 1, atom(retired, []), listed_role(gather, 2, atom(retired, [])))" "$out"
check_not "and it is not syntactically grassroots with wards" "syntactically_grassroots_with_wards" "$out"

# --- contracts with wards at GAI 11d7133 (sections/wards.tex) ----------------
# A schema deletes an atom at a role if the atom is marked - there or matches
# a cleared atom there; clause 1 of def:guardianship asks that no schema add
# or delete a mark in M at more than one role; and clause 2 of def:relay
# admits, in a relay act with two ward roles, e of the other ward role at the
# guardian role of one (GnW:4), and an atom of h at a ward role only where
# every schema adding v(rho) at a role adds there the record h'(rho, args),
# and adds the atom of h at rho or requires the record at a role requiring
# w(rho).  A leaving mark, a mark, has no argument, and a cleared atom
# carries the wildcard as one, so no cleared atom deletes a leaving mark.
out=$(run 'check_named(wd_ward_e, V).')
check "wd_ward_e: each parent of child_befriend requires friend of the other child, e of the other ward role (def:relay, clause 2, GnW:4): grassroots with wards" \
      "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])" \
      "$out"
no_wards wd_ward_e_own         "a parent of child_befriend requires friend of their own child, e of no other ward role (def:relay, clause 2)"
no_wards wd_relay_guard_clears "a parent of child_befriend clears friend(*), deleting an atom naming the other guardian role (def:relay, clause 1)"
no_guardianship wd_twins       "twins adds child at two roles (def:guardianship, clause 1)"
no_guardianship wd_clear_g     "drop clears parent(*), deleting atoms of g and no child, and parent(r) at r and no parenting(r) (def:guardianship, clauses 3 and 4)"

# wr: AI agents, every guardian holding the record of the agent's
# attestation, added by enter_agent at the creator and by make_guardian at
# the guardian it makes, who copies it from the creator.
wr_found="introduction_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of, guardian_of)], [relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [])"
out=$(run 'check_named(wr, V).' 'wards_of(wr, W).' 'contract_named(wr, C), relay_h(C?, H).')
check "wr: the record at every guardian, the attestation an atom of h: grassroots with wards" \
      "V = grassroots_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of, guardian_of)], [relay(agent_person_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [])" \
      "$out"
check "wards_of gives its introduction with wards" "W = $wr_found" "$out"
check "relay_h names the attestation at each ward role" \
      "H = [h(agent_person_befriend, 1, attestation), h(agents_befriend, 1, attestation), h(agents_befriend, 2, attestation)]" "$out"
out=$(run 'contract_named(wr, C), guardianships(C?, [agent], G).')
check "wr: guardian_of a v of a guardianship for agent" \
      "guardianship(agent, identity, principal, principal_of, guardian_of)" "$out"

no_record() { # no_record <fixture> <what it breaks>
  out=$(run "check_named($1, V)." "wards_of($1, W)." "contract_named($1, C), relay_h(C?, H).")
  check "$1: $2, so no introduction with wards" "W = no_introduction_with_wards" "$out"
  check "$1: and no atom of h" "H = []" "$out"
  check "$1: and the verdict is Section 3's" "V = not_grassroots([obstructed(befriend, " "$out"
}
no_record wr_rec_creator    "make_guardian adds guardian_of(r) and no record at the guardian (def:relay, clause 2)"
no_record wr_rec_unrequired "make_guardian adds the record at the guardian, adding no attestation at r and requiring the record at no role requiring principal_of(r) (def:relay, clause 2)"
no_record wr_clear_h        "wipe clears attestation(*), deleting atoms of h (def:relay, clause 2)"
no_record wr_clear_v        "quit clears guardian_of(*) and deletes or forbids no principal_of (def:guardianship, clause 5)"
out=$(run 'contract_named(wr_clear_v, C), guardianships(C?, [agent], G).')
check "wr_clear_v: guardian_of is a v of no guardianship for agent" \
      "G = [guardianship(agent, identity, principal, principal_of, principal_of)]" "$out"

# --- listed roles and name terms (def:role, def:introduction, def:unobstructed)
# GSN with one schema more, each failing one condition, and two contracts
# that stand on their own, failing def:introduction and clause 1 of
# def:unobstructed by a name term.
out=$(run 'check_named(an_named_intro, V).')
check "an introductory act with a name term is none, by def:introduction" \
      "V = not_grassroots([no_introductory_act])" "$out"
out=$(run 'check_named(an_named_unary, V).')
check "identity added only by a schema with a name term is unobtainable, by clause 1 of def:unobstructed" \
      "V = not_grassroots([obstructed(befriend, 1, atom(identity, []), unobtainable), obstructed(befriend, 2, atom(identity, []), unobtainable)])" \
      "$out"
# Over a listed role pi[rho(*, u_2, ..., u_r)] (def:role, def:extent, GSC
# 8064cfb): its base is the agent role pi, so it joins pi in the role graph
# (def:volition), it is no agent role, so an atom it requires is no psi of
# def:grounded, and the first part of clause 2 of def:unobstructed asks of
# it what the second asks of an agent role, the fault naming it
# listed_role.  Only an agent role may carry the mark ?, and a role that
# does not guard adds or deletes an atom (def:schema, GSC f3c4f43 and
# 70afdc4): sn_listed_guard's listed
# role carries ?, and sn_listed_inert's adds and deletes nothing, and each is
# refused.
# GSN extends GKK: acquaint stays its unobstructed introductory act, and
# befriend's obstructions are read off intro_acts.
out=$(run "intro_acts_variant(sn_listed_forbidden, $GSNT, A).")
check "a listed role adding friend, which befriend forbids, fails clause 2 at both roles" \
      "A = [intro_act(acquaint, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(2)]), listed_role(befriend_listed, 2, atom(friend, [role(1)]))), obstructed(befriend, 2, atom(friend, [role(1)]), listed_role(befriend_listed, 2, atom(friend, [role(1)])))]))]" \
      "$out"
out=$(run "check_variant(sn_listed_witness, $GSNT, V)." "traceable_variant(sn_listed_witness, $GSNT, E).")
check "a speech act traced through a listed role only fails traceable provenance" \
      "V = not_grassroots([untraceable([noted])])" "$out"
check_not "and noted is not among the predicates traced through agent roles" "noted" \
          "$(printf '%s' "$out" | grep '^E = \|GLP> E = ')"
out=$(run "check_variant(sn_listed_guard, $GSNT, V)." "compile_variant(sn_listed_guard, $GSNT).")
check "a listed role carrying the mark ? is refused" \
      "V = malformed([listed_guarded(split, 3)])" "$out"
check "and it is not compiled, the message naming the fault" \
      "% not compiled: sn_listed_guard is malformed: split at role 3 has a listed role carrying the mark ?, which only an agent role may carry; check_named gives the faults." \
      "$(printf '%s' "$out" | tr '\n' ' ')"
out=$(run "check_variant(sn_listed_inert, $GSNT, V).")
check "a listed role adding and deleting nothing is refused" \
      "V = malformed([changes_nothing(idle, 2)])" "$out"

# --- listed roles formed otherwise (def:role) ------------------------------
# close_b's listed role is of itself, no agent role; note_l's carries a
# signed term among its arguments, which GAI's Definition Speech-act schema admits
# only in an atom marked +; close_h's has an argument m its head does not list: the
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

# --- the notation (def:schema, def:numbers) --------------------------------
# notation_contract prints a contract's schemas from its term in the notation
# of Definition Speech-act schema, def:schema of Section 3.2, a token to a line,
# an empty line ending a line of the text
# (notation.glp; README, "The notation"); cascade.sh joins the tokens into a
# record's schemas.txt, as notation() does here.  GSG's befriend is the schema
# Section 3.2 displays, befriend(Alice?, Bob?) : identity, knows(Bob), not
# friend(Bob), +friend(Bob)  identity, knows(Alice), not friend(Alice),
# +friend(Alice), after GKK's establish, forbidding the identity it adds,
# acquaint and introduce, each E_i on a line
# labelled by its role; GSN's close is Section 3.2's example of a listed
# role (def:role), requiring the leader's membership (GSC f3e3f40), with
# the identity the paper leaves out; and sg_reflexive, GSG's variant,
# declares friend reflexive.
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
    at Alice: not identity, +identity

acquaint(Alice?, Bob?) :
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
    at Alice: identity, member(<Alice>.n), +closed(n), -listed(Alice, n)
    at Alice[listed(*, n)]: identity, -member(<Alice>.n)
EOF
)
check_eq "GSN's close prints its listed role and name term as def:role writes them" \
         "$close_notation" "$(notation "$GSNT" | one_schema close)"
REFL=$(run "variant(sg_reflexive, $GSGT, C)." | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check_eq "a contract declaring friend reflexive prints the declaration first" \
         "Reflexive predicates: friend" "$(notation "$REFL" | head -1)"

# --- Values (GSC, Section 3.2 and the appendix "Values in Speech-Act Schemas") -----
# "Speech-act schemas allow values of any type: an argument of an atom may be a
# value or a value variable, a schema may carry a constraint on its value
# variables, which every binding satisfies, and an atom added or deleted may
# carry a multiplicity, a count of copies and so a positive whole number ...
# A value names no agent, so the conditions below do not concern it."  The
# term form is Currencies' GB.glp: num(x), the value variable x or the
# number x; times(num(k), A); nschema(Name, Vars,
# Constraint, Roles), the constraint a conjunction of linear equalities and
# inequalities (def:numbers; README, "Writing a contract").  A binding sends a
# value variable to a value and a multiplicity to a positive whole number,
# and an introduction chooses its own values (def:binding-numbers; the
# appendix "Values in Speech-Act Schemas"), and whether some choice meets the
# clauses is decided exactly, the constraints being linear (linear.glp).
echo "--- numbers (GSC, Section 3.2 and its appendix) ---"

# The checker's fixtures with values (contracts.glp, "Values").  nm_bonds
# is syntactically grassroots.  def:unobstructed asks that an atom be added
# by a schema at a binding, which satisfies that schema's constraint
# (def:binding-numbers), so a number is matched by equality there.  Clause 1:
# nm_supplied's meet requires score(5), which score adds at the binding
# sending k to 5, its constraint true; nm_admits's score adds it under
# k > 3, which 5 satisfies; nm_excludes's does not, under k <= 4, which 5
# fails; nm_other_number's score adds score(4), and 4 is not 5.  Clause 2:
# nm_forbid_admits's meet forbids date(0), which set_date adds at the
# binding sending t to 0, under t <= 3, set_date having Bob in no role;
# nm_forbid_excludes's set_date adds it at no binding, under t > 3.  A
# value of the introductory act is the introduction's to choose (the
# appendix "Values in Speech-Act Schemas"): in nm_intro_var score adds score(n)
# whatever the value, and in nm_intro_var_bounded where it is above 3, which
# the introduction chooses.  nm_blocked's advance adds date(0) where t is
# -1, a value, under t1 = t + 1, Bob in no role: obstructed.  A multiplicity
# is a positive whole number at every binding: nm_mult_unsat's raise adds
# flag, which meet forbids, under k <= 0, at no binding, and nm_mult_sat's,
# under k <= 1, at the binding sending k to 1; and nm_mult_zero writes a
# multiplicity 0, which is refused.
out=$(run "check_named(nm_bonds, V)." "traceable_of(nm_bonds, E)." \
          "check_named(nm_supplied, V)." "check_named(nm_blocked, V).")
check "a contract with numbers, multiplicities and constraints of each kind is syntactically grassroots" \
      "V = syntactically_grassroots" "$(printf '%s\n' "$out" | grep 'V = ' | sed -n 1p)"
check "its predicates all have traceable provenance, bond and date with numeric arguments" \
      "E = [identity, knows, bond, date, coin]" "$out"
check "a number required at the introductory act is supplied by a schema of arity one adding it at the binding sending its value variable to that number" \
      "V = syntactically_grassroots" "$(printf '%s\n' "$out" | grep 'V = ' | sed -n 2p)"
check "a value forbidden at the introductory act, added at the binding sending t to -1, is obstructed" \
      "V = not_grassroots([obstructed(meet, 1, atom(date, [num(0)]), blocked_by(advance, 1, atom(date, [num(t1)])))])" "$out"
out=$(run "check_named(nm_admits, V)." "check_named(nm_excludes, V)." \
          "check_named(nm_other_number, V)." "check_named(nm_forbid_admits, V)." \
          "check_named(nm_forbid_excludes, V)." "check_named(nm_intro_var, V)." \
          "check_named(nm_intro_var_bounded, V)." "compile_named(nm_blocked).")
nmv() { printf '%s\n' "$out" | sed -e 's/^\(GLP> \)*//' | grep '^V = ' | sed -n "$1p"; }
check_eq "a required score(5) is supplied by a schema of arity one adding score(k) under a constraint that admits 5, k > 3" \
         "V = syntactically_grassroots" "$(nmv 1)"
check_eq "a required score(5) is not supplied by a schema of arity one adding score(k) under a constraint that excludes 5, k <= 4" \
         "V = not_grassroots([obstructed(meet, 1, atom(score, [num(5)]), unobtainable), obstructed(meet, 2, atom(score, [num(5)]), unobtainable)])" "$(nmv 2)"
check_eq "a required score(5) is not supplied by a schema adding score(4), a number matched by equality" \
         "V = not_grassroots([obstructed(meet, 1, atom(score, [num(5)]), unobtainable), obstructed(meet, 2, atom(score, [num(5)]), unobtainable)])" "$(nmv 3)"
check_eq "a forbidden date(0) is added by a schema adding date(t) under a constraint that admits 0, t <= 3, the other party in no role: obstructed" \
         "V = not_grassroots([obstructed(meet, 1, atom(date, [num(0)]), blocked_by(set_date, 1, atom(date, [num(t)])))])" "$(nmv 4)"
check_eq "a forbidden date(0) is added at no binding of a schema adding date(t) under a constraint that excludes 0, t > 3: unobstructed" \
         "V = syntactically_grassroots" "$(nmv 5)"
check_eq "a number of the introductory act required, supplied whatever number its introduction chooses" \
         "V = syntactically_grassroots" "$(nmv 6)"
check_eq "a value of the introductory act required, supplied where its introduction chooses a value above 3, which it does" \
         "V = syntactically_grassroots" "$(nmv 7)"
check "a contract whose introductory act is obstructed by a value is not compiled" \
      "% not compiled: nm_blocked is not syntactically grassroots" "$(printf '%s\n' "$out" | tr '\n' ' ')"
NMBL=$(run "contract_named(nm_blocked, C)." | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check "the introductory acts of that contract name meet obstructed by advance" \
      "A = [intro_act(meet, obstructed([obstructed(meet, 1, atom(date, [num(0)]), blocked_by(advance, 1, atom(date, [num(t1)])))]))]" "$(run "intro_acts($NMBL, A).")"
out=$(run "check_named(nm_mult_unsat, V)." "check_named(nm_mult_sat, V)." "check_named(nm_mult_zero, V)." "compile_named(nm_mult_zero).")
mmv() { printf '%s\n' "$out" | sed -e 's/^\(GLP> \)*//' | grep '^V = ' | sed -n "$1p"; }
check_eq "a schema adding a forbidden atom k times under a constraint no positive whole number satisfies, k <= 0, adds it at no binding: unobstructed" \
         "V = syntactically_grassroots" "$(mmv 1)"
check_eq "under k <= 1 it adds it at the binding sending k to 1, the other party in no role: obstructed" \
         "V = not_grassroots([obstructed(meet, 1, atom(flag, []), blocked_by(raise, 1, atom(flag, [])))])" "$(mmv 2)"
check_eq "a multiplicity written as 0 is refused" \
         "V = malformed([multiplicity_not_whole(mint, 1, atom(coin, [role(1)]))])" "$(mmv 3)"
check "and the message names the fault" \
      "mint at role 1 carries a multiplicity that is not a positive whole number;" "$(printf '%s\n' "$out" | tr '\n' ' ')"
nm_mint_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\uplus\{k\cdot\mathit{bond}(\Alice,d)\},\\&\text{provided}\mathit{identity},\mathit{date}(t)\inc_{\Alice},\\&\text{provided}d>t\landd\let+30,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "nm_bonds's mint compiles its constraint, a conjunction, and as \\land and a sum as +" \
         "$nm_mint_display" "$(compiled nm_bonds mint)"
nm_mint_notation=$(cat <<'EOF'
mint(Alice?; d, k, t) | d > t, d <= t + 30 :
    at Alice: identity, date(t), +k.bond(Alice, d)
EOF
)
NMB=$(run "contract_named(nm_bonds, C)." | sed -e 's/^\(GLP> \)*//' | grep -E '^C = ' | head -1 | sed -e 's/^C = //')
check_eq "nm_bonds's mint prints its constraint after its head, and as a comma" \
         "$nm_mint_notation" "$(notation "$NMB" | one_schema mint)"

# Every case is decided exactly (linear.glp): whether some choice of the
# introduction's values meets the clauses is whether a formula of linear
# equalities and inequalities has a solution, the values rational and a
# multiplicity a positive whole number, the bindings of a schema that would
# add an atom the act forbids eliminated first.  nm_two_between's meet has
# introductions only where 0 < x < y < 1, which no two values within one of
# a number the contract writes, or halfway between two, satisfy.  nm_cover's
# meet forbids date(x), which low adds where x <= 5 and high where x >= 5,
# so at every introduction; nm_gap's high adds it where x >= 6, and an
# introduction with 5 < x < 6 is unobstructed.  nm_evens's count adds
# date(2j), j a multiplicity and so whole, and meet's multiplicity k forbids
# date(k), so the introduction with k = 1 is unobstructed; nm_counts's count
# adds date(j), every whole number, so none is.  nm_half's meet has the
# multiplicity k with 2k = 3, which no whole number satisfies: no
# introduction.  nm_joint's meet requires score(x), supplied where x >= 5,
# and forbids date(x), added where x >= 5: each clause is met at some x, and
# both at none.  A constraint that is not a conjunction of linear equalities
# and inequalities is refused: nm_or's disjunction and nm_nonlinear's
# product of two value variables.
out=$(run "check_named(nm_two_between, V)." "check_named(nm_cover, V)." \
          "check_named(nm_gap, V)." "check_named(nm_evens, V)." \
          "check_named(nm_counts, V)." "check_named(nm_half, V)." \
          "check_named(nm_joint, V)." "check_named(nm_or, V)." \
          "check_named(nm_nonlinear, V)." "compile_named(nm_nonlinear).")
lnv() { printf '%s\n' "$out" | sed -e 's/^\(GLP> \)*//' | grep '^V = ' | sed -n "$1p"; }
check_eq "two values of the introductory act, related, chosen strictly between two numbers the contract writes: syntactically grassroots" \
         "V = syntactically_grassroots" "$(lnv 1)"
check_eq "a value forbidden at the introductory act, added by one schema below 5 and by another above 5: obstructed at every introduction" \
         "V = not_grassroots([obstructed(meet, 1, atom(date, [num(x)]), blocked_by(low, 1, atom(date, [num(t)])))])" "$(lnv 2)"
check_eq "added below 5 and above 6, the introduction choosing a value between: syntactically grassroots" \
         "V = syntactically_grassroots" "$(lnv 3)"
check_eq "a multiplicity of the act forbidden as a date, added at the even whole numbers, the introduction choosing 1: syntactically grassroots" \
         "V = syntactically_grassroots" "$(lnv 4)"
check_eq "added at every whole number: obstructed at every introduction" \
         "V = not_grassroots([obstructed(meet, 1, atom(date, [num(k)]), blocked_by(count, 1, atom(date, [num(t)])))])" "$(lnv 5)"
check_eq "a multiplicity with 2k = 3, which no whole number satisfies: no introduction" \
         "V = not_grassroots([no_introductory_act])" "$(lnv 6)"
check_eq "each clause met at some value of the introduction, and both at none: obstructed jointly" \
         "V = not_grassroots([obstructed_jointly(meet)])" "$(lnv 7)"
check_eq "a constraint with a disjunction is refused" \
         "V = malformed([constraint_not_conjunction(mint, or(gt(num(d), num(t)), le(num(d), num(0))))])" "$(lnv 8)"
check_eq "a constraint with a product of two value variables is refused" \
         "V = malformed([constraint_not_linear(mint, mul(num(d), num(k)))])" "$(lnv 9)"
check "and it is not compiled, the fault named" \
      "mint carries a constraint that is not linear;" "$(printf '%s\n' "$out" | tr '\n' ' ')"

# The malformations the passage forbids, each refused (README, "The
# verdicts"): a role that does not guard and changes nothing, numbers or no
# (def:schema; GB's mature of b9369200, which GB's redemption replaces,
# redeem since 18f28661); a multiplicity on an atom required and not deleted,
# and on one forbidden, the passage admitting one on an atom added or
# deleted; a multiplicity that is not a number; a constraint on an agent,
# the passage admitting one on the value variables; an atom added with a
# multiplicity and deleted at one role (def:schema); and a head that lists a
# number and a value variable twice and leaves out the value variables
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
check "a head listing a number and a value variable twice, and leaving out two, is refused" \
      "V = malformed([head_not_variable(mint, num(5)), head_repeated(mint, num(k)), not_in_head(mint, num(d)), not_in_head(mint, num(t))])" "$out"
check "and it is not compiled, the faults named" \
      "lists in its head an entry that is not a variable;" "$out"

# Cleared atoms (GSC 327e035; Udi, 2026-10-30, to GSC #9, on EC6): an atom
# marked - with the wildcard among its arguments, or in place of the
# speech-act variable of a name term among them, deletes every atom of the
# agent's state that it matches and requires none (Definition Speech-act
# schema, Binding, Compilation's K_p).  GSN's unfriend clears listed(Bob, *)
# and member(<Bob>.*) at Alice and the converse at Bob, as Section 3.3's
# table writes it (GSN.glp: any, and ext_any(N) for <N>.*).
unfriend_notation=$(cat <<'EOF'
unfriend(Alice?, Bob) :
    at Alice: identity, -friend(Bob), -listed(Bob, *), -member(<Bob>.*)
    at Bob: identity, -friend(Alice), -listed(Alice, *), -member(<Alice>.*)
EOF
)
check_eq "GSN's unfriend prints its cleared atoms as Section 3.3's table writes them" \
         "$unfriend_notation" "$(notation "$GSNT" | one_schema unfriend)"
# Compiled (def:compile): c'_p := ((c_p \ -(i)) \ K_p) u +(i), K_p holding
# every atom of c_p, with its multiplicity, that matches a cleared atom; the
# cleared atoms require nothing, so the proviso holds friend of the other
# alone.  unfriend has a name term, <Bob>.*, so it is printed in the general
# form, a line per agent.
read -r -d '' unfriend_tex <<'EOF'
\begin{align*}
&c'_{\Alice} := (c_{\Alice} \setminus \{\mathit{friend}(\Bob)\}) \setminus K_{\Alice},\\
&\text{where } K_{\Alice} \text{ holds every atom of } c_{\Alice} \text{, with its multiplicity, that matches } \mathit{listed}(\Bob,\ast) \text{ or } \mathit{member}(\langle\Bob\rangle\cdot\ast),\\
&c'_{\Bob} := (c_{\Bob} \setminus \{\mathit{friend}(\Alice)\}) \setminus K_{\Bob},\\
&\text{where } K_{\Bob} \text{ holds every atom of } c_{\Bob} \text{, with its multiplicity, that matches } \mathit{listed}(\Alice,\ast) \text{ or } \mathit{member}(\langle\Alice\rangle\cdot\ast),\\
&\text{provided } \mathit{identity},\mathit{friend}(\Bob) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{friend}(\Alice) \in c_{\Bob},\\
&\text{guarded by } \{\Alice\}
\end{align*}
EOF
check_eq "GSN's unfriend compiles with K_p, the atoms its cleared atoms match (def:compile)" \
         "$(printf '%s' "$unfriend_tex" | squash)" "$(compiled_in gsn "$GSNT" unfriend)"
# A cleared atom requires none, so it puts no edge in the role graph
# (def:volition): shun's Alice clears listed(Bob, *) and its Bob
# member(<Alice>.*), each naming the other role, and Bob does not guard, so
# volition fails at shun (cl_no_edge); with Alice deleting listed(Bob, n),
# which she then requires, the roles are joined (cl_edge).
out=$(run "check_variant(cl_no_edge, $GSNT, V)." "check_variant(cl_edge, $GSNT, V).")
check "a cleared atom naming the other role puts no edge in the role graph: volition fails" \
      "V = not_grassroots([volition(shun, 1, 2)])" "$out"
check "the same atom deleted, and so required, joins the roles: syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
# A cleared atom is no change a role makes: +(i) u -(i) is nonempty at every
# role that is not a guarding role and at some role, -(i) the atoms marked -
# with no wildcard (def:schema).
out=$(run "check_variant(cl_inert, $GSNT, V).")
check "a role that only clears does not change, and a schema that only clears changes nothing" \
      "V = malformed([changes_nothing(purge, 2), no_role_changes(purge)])" "$out"
# No atom of +(i) is obtained from an atom of *(i) by replacing its wildcards
# (def:schema).
out=$(run "check_variant(cl_added, $GSNT, V)." "compile_variant(cl_added, $GSNT).")
check "an atom added that a cleared atom of its role matches is refused, each named" \
      "V = malformed([added_and_cleared(relist, 1, atom(listed, [role(1), svar(n)])), added_and_cleared(relist, 1, atom(member, [nterm(ext(own(1), n))]))])" "$out"
check "and it is not compiled, the fault named" \
      "relist at role 1 adds an atom that one of its cleared atoms matches;" "$(printf '%s' "$out" | tr '\n' ' ')"
# Only an atom marked - may have the wildcard (def:schema), and a listed
# role's arguments are arguments of its schema (def:role).
out=$(run "check_variant(cl_wild, $GSNT, V)." "compile_variant(cl_wild, $GSNT).")
check "the wildcard outside an atom marked - is refused, in a listed role's arguments and in an atom added, required or forbidden" \
      "V = malformed([listed_wildcard(sweep, 2), wildcard(peek, 1, atom(seen, [any])), wildcard(peek, 1, atom(listed, [role(1), any])), wildcard(peek, 1, atom(member, [nterm(ext_any(own(1)))]))])" "$out"
check "and it is not compiled, the faults named" \
      "peek at role 1 carries the wildcard in an atom not marked -;" "$(printf '%s' "$out" | tr '\n' ' ')"
# Clause 1 of def:unobstructed: the schema of arity one adding an atom the
# introductory act requires "requires and clears nothing".  GKK's establish
# clearing note(*) besides supplies no identity, so acquaint is obstructed.
out=$(run "check_variant(gkk_establish_clears, $GKKT, V)." "intro_acts_variant(gkk_establish_clears, $GKKT, A).")
check "a schema of arity one that clears supplies nothing under clause 1: acquaint obstructed at both roles" \
      "V = not_grassroots([obstructed(acquaint, 1, atom(identity, []), unobtainable), obstructed(acquaint, 2, atom(identity, []), unobtainable)])" "$out"
check "and acquaint is GKK's one introductory act, obstructed" \
      "A = [intro_act(acquaint, obstructed([obstructed(acquaint, 1, atom(identity, []), unobtainable), obstructed(acquaint, 2, atom(identity, []), unobtainable)]))]" "$out"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
