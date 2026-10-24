#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/gsc).
#
#   bash programs/gsc/test_gsc.sh
#
# GSC's contracts GKK, GSG and GSN (programs/gsc/), GKK the base contract every contract extends, its establish forbidding the identity it adds (GKK:1, GSC b2bb8be), its introductory act meet, knows of traceable provenance and introduce's roles joined by knows(q) at p (GSC, Sections 2.1 and 7.1; intro_acts/2 and variants of GKK), GSN being the social network of /Grassroots/GSC Section 3.3, which Section 7.1 certifies by hand by def:syntactically-grassroots, one definition for a contract of agent roles and listed roles (GSC ac7ad63), and GSNP, the social network with provenance, GAI's, carrying the signed term of GAI's extension of the language; the currency of Appendix B, which Appendix B.4 certifies (programs/currencies/coins/GC.glp); CSSN's and GAI's contracts; each read from its owner's file, each schema listing its variables in its head (GSC 0228e79), each extending GKK holding GKK's schemas as GKK.glp writes them; and contracts over GSG, broken in one place each, one per way of failing the three conjuncts of def:syntactically-grassroots; then clause 1 of def:unobstructed on a schema of arity one that forbids the atom it adds; then contracts declaring a predicate reflexive (GAI's appendix "Contracts with Wards"); then schemas not formed as def:schema requires, refused, and a guarding role adding and deleting nothing, admitted; then the compilation of Section 5, against the display of befriend in Section 5.1 and that of the swap in Appendix B.3; then small contracts on the speech-act variables of traceable provenance and on volition by connected component, and a contract with a name term based on an agent variable, which def:role does not admit, refused; then the signed speech-act term, on GSNP and on contracts refused for a signed term where it is forbidden; then contracts with wards (GAI's appendix "Contracts with Wards"), on a fixture of children and parents, on one of children and AI agents, on each broken in one place per clause, on one of children and AI agents whose relay acts require the AI agent's attestation, an atom of h of clause 2 of def:relay, and on it broken in one place per condition of that alternative, on one with a leaving mark and on it broken in one place per clause of def:leaving, on CSSN's and GAI's contracts, and on agent-named contracts with wards (def:sg-wards, CSSN's paper) with a listed role; then contracts with listed roles and name terms over GSNP, failing a condition each by a listed role, and listed roles refused for the mark ? or for changing nothing; then listed roles formed otherwise than def:role allows, and schemas whose head is not their variables (def:schema); then the notation of Definition Act schema, printed from a contract's term (notation.glp); then the values act schemas allow (GSC, Section 3.2 and its appendix "Values in Act Schemas"), which traceable provenance and volition do not concern, a value naming no agent: GB, the bonds, Currencies' (programs/currencies/bonds/GB.glp), its verdict, its displays and its notation, a contract with numbers certified, def:unobstructed on numbers --- a number required and supplied, or not, by a schema of arity one at a binding satisfying its constraint, a number matched by equality, a number forbidden and added, or not, at such a binding, a value the introduction chooses, a value below every number the contract writes, and a multiplicity a positive whole number at every binding --- and a fixture of each malformation the passage forbids.  The federation extension of act schemas left GSC's paper (GSC f3c4f43) and its checks left with its code (Coordination #7, 2026-10-20); GFWC's contract is still read from its owner's file.  A contract is its owner's and the checker reads its term (contracts.glp, the header): a fixture that changes one is a variant, given by name and the owner's term.  Exits non-zero if any check fails.

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
# GKK, the base contract: establish, GKK:1, adds identity, requires nothing
# and forbids the identity it adds (GKK:1, "act once", GSC b2bb8be; GKK.glp
# at GLP 79db2fae), and every other role requires identity.  GSG, GSN and
# GSNP extend GKK, and so do GC and GB (Udi, 2026-10-20, to GSC #5; GSNP on
# GKK at GLP 35c4211a, GAI #6 Cowork), so each opens with GKK's three
# schemas, as GKK.glp writes them: GKK_SCHEMAS is GKK's term, its closing
# bracket left off.  SG is GSNP, Section 3.3's social network with
# provenance, with the listed role of close (GSC 8064cfb); GSGT is GSG, the
# contract most variants below are made on.
GKKT=$(term "$GLP_DIR/programs/gsc/GKK.glp" gkk)
check "GKK's contract is read from programs/gsc/GKK.glp, establish forbidding the identity it adds, then meet and introduce" \
      "[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [atom(identity, [])])]), schema(meet, [], [rs(guarded, [atom(knows, [role(2)])], [], [atom(identity, [])], []), rs(guarded, [atom(knows, [role(1)])], [], [atom(identity, [])], [])]), schema(introduce, [pvar(c)], " "$GKKT"
GKK_SCHEMAS="${GKKT%]}"
SG=$(term "$GLP_DIR/programs/gai/GSNP.glp" gsnp)
check "GSNP's contract, the network with provenance, is read from programs/gai/GSNP.glp, GKK's schemas first" \
      "$GKK_SCHEMAS" "$SG"
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
check "GKK: meet is its one introductory act, and unobstructed" \
      "A = [intro_act(meet, unobstructed)]" "$out"
check "GKK: knows has traceable provenance, with identity" "E = [identity, knows]" "$out"
# In GSG befriend is an introductory act too, but obstructed: it requires
# knows of the other, which no schema of arity one adds (GSC, Section 3.6,
# the example after def:introduction); meet is unobstructed, so GSG is
# syntactically grassroots.  GSN likewise.
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
CUR=$(term "$GLP_DIR/programs/currencies/coins/GC.glp" gc)
check "the currency's contract, GC, is read from programs/currencies/coins/GC.glp, GKK's schemas first" \
      "$GKK_SCHEMAS" "$CUR"
check "and its mint adds k coins of the minter's issue (Currencies 72cf826b)" \
      "schema(mint, [num(k)], [rs(guarded, [times(num(k), atom(coin, [role(1)]))]" "$CUR"
CSSN=$(term "$GLP_DIR/programs/cssn/CSSN.glp" cssn)
check "CSSN's contract is read from programs/cssn/CSSN.glp, friend and knows reflexive" \
      "reflexive([friend, knows], [schema(establish, []," "$CSSN"
check "and its close takes the listed role of the members the leader lists" \
      "crs(listed(1, listed, [svar(n)]), unguarded, [], [atom(member, [nterm(ext(own(1), n))])]" "$CSSN"
FED=$(term "$GLP_DIR/programs/federation/GFWC.glp" gfwc)
check "the federation's contract, GFWC, is read from programs/federation/GFWC.glp" \
      "[schema(form, [], [rs(guarded, [atom(seat, [nterm(own(1))])]" "$FED"
GAI=$(term "$GLP_DIR/programs/gai/GAI.glp" gai)
check "GAI's contract is read from programs/gai/GAI.glp" \
      "[schema(establish, [], [rs(guarded, [atom(identity, [])], [], [], [atom(identity, []), atom(retired, [])])])" "$GAI"

# The two worked contracts, GSNP certified in Section 7.1 and the currency
# in Appendix B.4.  One goal per run, so that each verdict is read off its own
# output.  GSNP (GSNP.glp at 542ec8ca; GSC 8064cfb, Sections 3.3 and 7.1):
# every name term <a>.n based on the agent role a, close taking the listed
# role a[listed(*, n)].  By def:syntactically-grassroots, one definition for
# a contract of agent roles and listed roles (GSC ac7ad63): meet, with agent
# roles and no name term, is unobstructed, identity from establish, and
# close's listed role adds no atom of a predicate meet forbids, meet
# forbidding none (def:unobstructed); the set of all its predicates has
# traceable provenance, each traced through an agent role (def:grounded); and
# the role graphs meet def:volition, close's two roles having the base Alice.
# So it is syntactically grassroots, and its realisation volitionally
# grassroots by Theorem "Grassroots realisation".  Its signed terms, in
# forward, post_forward and deliver, are each of an agent role that requires
# an atom carrying the speech act (GAI's Definition Act schema), deliver's of
# a role that does not guard; forward's and post's role Alice guards and adds
# and deletes nothing; so GSNP is not malformed.
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "the program loads" "Loaded program" "$out"
check_not "no type error" "Error loading" "$out"
check "GSNP, with the listed role of close, is syntactically grassroots by the one definition" \
      "V = syntactically_grassroots" "$out"
check_not "GSNP's signed terms, deliver's among them, and its guarding roles that change nothing are admitted" "malformed(" "$out"
check "GSNP's ten predicates have traceable provenance" \
      "E = [identity, knows, friend, item, authored, member, listed, closed, posted, delivered]" "$out"

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
check "on GSG with gossip meet is unobstructed, and the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "gossip blocks befriend at role 1" \
      "obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1," "$out"
check "gossip blocks befriend at role 2" \
      "obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1," "$out"
check_not "befriend is not blocked by itself" \
      "blocked_by(befriend" "$out"
check_not "gossip itself satisfies volition" "volition(gossip" "$out"
# The same on GSNP, a contract with a listed role, the second part of
# clause 2 of def:unobstructed asked of the agent roles.  GSNP extends GKK
# (GLP 35c4211a), so meet stays an unobstructed introductory act there too,
# and befriend's obstructions are read off intro_acts.
out=$(run "check_variant(sg_gossip, $SG, V)." "intro_acts_variant(sg_gossip, $SG, A).")
check "on GSNP with gossip meet is unobstructed, and the contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "on GSNP gossip blocks befriend at both roles" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(2)]), blocked_by(gossip, 1, atom(friend, [pvar(u)]))), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(1)]), blocked_by(gossip, 1, atom(friend, [pvar(u)])))]))]" \
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
check "and on GSNP, a contract with a listed role" \
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

# A speech act carried into an item from a required atom that has no
# traceable provenance, on GSNP, by def:grounded (each psi required at an
# agent role): the requirement on
# speech-act variables drops tagged, its u named by no required atom, then
# item, tag's x being required only in tagged(x, u), then posted and
# delivered, post_own, post_forward and deliver adding a speech act their
# roles require only in an item or a posted atom; identity, friend,
# authored, sign adding authored(x) with x required nowhere, and the group's
# member, listed and closed keep it, and knows, GKK's.  Meet stays
# unobstructed and every schema meets def:volition, the edges resting on
# knows, friend, member and listed.  Traceable provenance rejects the
# contract, and no other condition.
out=$(run "check_variant(sg_svar_loose, $SG, V)." "traceable_variant(sg_svar_loose, $SG, E).")
check "a speech act from an untraceable record: identity, knows, friend, authored and the group's records keep provenance" \
      "E = [identity, knows, friend, authored, member, listed, closed]" "$out"
check "and the verdict names traceable provenance and no other condition" \
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

# CSSN's contract, programs/cssn/CSSN.glp at 6ff16d78 and a261cb74 (CSSN #9
# Cowork, Code_inbox 2026-10-20 19:44 UTC), on GKK, friend and knows
# reflexive: its groups are named <a>.n, and close and child_close take the
# listed role a[listed(*, n)], every member the leader lists, every name
# term based on the agent role a, so it is agent-named (GSC 8064cfb;
# CSSN's def:agent-named).  Its predicates have traceable provenance, each
# traced through an agent role, and it meets def:volition; meet forbids
# child, so it has no unobstructed introductory act and is not syntactically
# grassroots by def:syntactically-grassroots.  It has an
# introduction with wards (GAI's appendix "Contracts with Wards" at GAI
# ddf0a33, def:introduction-wards) and is syntactically grassroots with wards
# (def:sg-wards of CSSN's paper):
# - the mark child, with the guardianship (child, identity, parent,
#   parenting) of def:guardianship;
# - retired, a leaving mark for {child} (def:leaving);
# - meet, unobstructed but for {child} (def:unobstructed-marks, two clauses):
#   identity is added by establish, of arity one, guarded, requiring nothing
#   and forbidding nothing but retired, a leaving mark (clause 1); meet
#   forbids child, a mark in M, and retired, a leaving mark (clause 2);
# - child_meet, the relay act of a ward role of child and a free role, and
#   child_meet_2, that of two ward roles of child, knows being reflexive for
#   the knows atom each parent requires of the other (def:relay); each
#   forbids nothing but child and retired (clause 3).
# So its verdict names meet, the two relay acts and the leaving mark retired:
# the protocol realising it is volitionally grassroots (Theorem "Grassroots
# realisation with wards and seated roles" of CSSN's paper, def:sg-wards).
# It is compiled, its realisation being taken as GSC's.  The leaving marks
# for {child} are given with the guardianship of child taken, clause 3 of
# def:leaving reading its g (GSC 07d413c).
out=$(run "check($CSSN, V)." "leaving($CSSN, [child], L).")
check "CSSN's contract is syntactically grassroots with wards through meet, child_meet and child_meet_2, retired its leaving mark" \
      "V = syntactically_grassroots_with_wards(meet, [child], [guardianship(child, identity, parent, parenting)], [relay(child_meet, ward(1, child, 3), free(2)), relay(child_meet_2, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
check "CSSN's leaving marks for {child} are [retired]" \
      "L = [leaving([guardianship(child, identity, parent, parenting)], [retired])]" "$out"
check_not "CSSN's predicates have traceable provenance" "untraceable(" "$out"
check_not "CSSN's contract satisfies volition" "volition(" "$out"
check_not "CSSN's contract is not malformed" "malformed(" "$out"
# One display per schema (Section 5.1, "A contract compiles schema by
# schema"): CSSN.glp's twenty-nine at GLP 6ff16d78, after GKK, GSG and GSN
# (CSSN #9 Cowork, Code_inbox 2026-10-20 19:44 UTC).
out=$(run "compile_contract(cssn, $CSSN).")
check_eq "and it compiles to twenty-nine displays" "29" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

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
# \in); GSNP's, GAI's, on GKK at GLP 35c4211a, the same; the swap is GC's, which extends GKK, transcribed from
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
check_eq "GSNP's befriend, on GKK, compiles to the same display" \
         "$befriend_paper" "$(compiled_in gsnp "$SG" befriend)"

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
# checked: GSNP's eighteen schemas, GKK's establish, meet and introduce,
# befriend, unfriend, sign, message and forward, and the ten of a group, to
# eighteen displays (GSNP.glp at GLP 35c4211a).
out=$(run "compile_contract(gsnp, $SG).")
check_eq "GSNP compiles to eighteen displays" "18" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"
out=$(run "compile_contract(gc, $CUR).")
check_eq "the currency compiles to seven displays, GKK's three and its own four" "7" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# sg_chain_cut on GSG, which fails volition (above): sg_gossip certifies on
# GSG and on GSNP, both extending GKK, whose meet stays unobstructed.
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

# --- the signed speech-act term (Sections 3.1 to 3.3) ----------------------
# GSNP, Section 3.3's network with provenance, item of arity one, forward
# adding item(sgn(x, Alice)) at Bob, post_forward adding
# posted(sgn(x, Bob), n) at the leader and deliver adding item(sgn(y, Alice))
# at the member.  Section 7.1 certifies it, each signed term counting as its
# two arguments, the speech act and the role (def:grounded; GAI's
# Definition Act schema).
out=$(run "check($SG, V)." "traceable_preds($SG, E).")
check "GSNP, carrying signed terms in forward, post_forward and deliver, is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "item, authored, posted and delivered have traceable provenance there with the rest" \
      "E = [identity, knows, friend, item, authored, member, listed, closed, posted, delivered]" "$out"

# The form of a display (compile.glp): a schema of agent roles and no name
# term in the short form, befriend's in Section 5.1 (above), as Section 5.1
# prints befriend of Section 3.3; any other in the general form, a line per
# agent, as def:compile's display has it and Section 5.1 prints close's (Udi,
# 2026-10-23, to GSC #7, reported by GSC #7 Cowork): the agent of each agent
# role, its line joining what every role whose extent holds it does there,
# then the other agents q of a listed role's extent; the required atoms of
# every role on one proviso line, a listed role's in c_q over its extent;
# the forbidden atoms on the next; the freshness of a name; the guard.  The
# compiler names role i by its index, role 1 \Alice and role 2 \Bob, so
# post's and post_forward's member, whom Section 3.3's table names Bob, is
# \Alice in their displays.
#
# forward(Alice?, Bob; x), Section 3.3's table at GSC 07d413c, of agent
# roles and no name term: at Alice, a guarding role adding and deleting nothing,
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
# 2 the leader, carries the name term <a>.n and is printed in the general
# form of def:compile, a line per agent: the member, guarding and adding and
# deleting nothing, c'_{\Alice} := c_{\Alice}; the leader, posted(x, n)
# added, the item as its author signed it; the required atoms of each; no
# freshness line, <a>.n occurring in member(<a>.n), which both roles
# require; and the guard, the member, its guarding role (def:compile,
# guarded by the guarding roles of the schema, GSC f3c4f43).
post_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice},\\
& c'_{\Bob} := c_{\Bob} \uplus \{\mathit{posted}(x,n)\},\\
& \text{provided } \mathit{identity},\mathit{authored}(x),\mathit{member}(\langle\Bob\rangle\cdot n) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{member}(\langle\Bob\rangle\cdot n),\mathit{listed}(\Alice,n),\mathit{friend}(\Alice) \in c_{\Bob},\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "post compiles in the general form of def:compile, a line per agent, its guarding role changing nothing" \
         "$post_gsnp" "$(compiled_in gsnp "$SG" post)"

# post_forward(p?, a ; x, n): an item the member received, posted with their
# signature added, sgn(x, Alice), and authored(x) forbidden at the member.
post_forward_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice},\\
& c'_{\Bob} := c_{\Bob} \uplus \{\mathit{posted}(\mathrm{sgn}(x,\Alice),n)\},\\
& \text{provided } \mathit{identity},\mathit{item}(x),\mathit{member}(\langle\Bob\rangle\cdot n) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{member}(\langle\Bob\rangle\cdot n),\mathit{listed}(\Alice,n),\mathit{friend}(\Alice) \in c_{\Bob},\\
& \text{provided } \mathit{authored}(x) \notin c_{\Alice},\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "post_forward compiles with the member's signed term" \
         "$post_forward_gsnp" "$(compiled_in gsnp "$SG" post_forward)"

# deliver(a, q ; y, n): the leader Alice adds delivered(y, Bob, n), the
# member Bob the leader's signed term item(sgn(y, Alice)); the required
# atoms of each, then what Alice forbids, authored(y) and
# delivered(y, Bob, n); and the empty guard, no role of deliver guarding.
# The signed term's role does not guard, and def:schema admits it, Alice
# requiring posted(y, n).
deliver_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{delivered}(y,\Bob,n)\},\\
& c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(\mathrm{sgn}(y,\Alice))\},\\
& \text{provided } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n),\mathit{posted}(y,n),\mathit{listed}(\Bob,n),\mathit{friend}(\Bob) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n) \in c_{\Bob},\\
& \text{provided } \{\mathit{authored}(y),\mathit{delivered}(y,\Bob,n)\} \cap c_{\Alice} = \emptyset,\\
& \text{guarded by } \emptyset
\end{align*}
TEX
)
check_eq "deliver compiles with the leader's signed term and an empty guard" \
         "$deliver_gsnp" "$(compiled_in gsnp "$SG" deliver)"

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
         "$(printf '%s' "$create_tex" | squash)" "$(compiled_in gsnp "$SG" create)"
check_not "join, requiring member(<a>.n), asks no freshness of it" \
          "noatomof" "$(compiled_in gsnp "$SG" join)"

# close(a?, a[listed(*, n)] ; n), Section 5.1's example of def:compile (GSC
# ac7ad63), with the identity the paper leaves out and the extent the paper
# calls L written out: the leader Alice lists herself, requiring
# listed(Alice, n), so she is in the extent of the listed role, and her line
# joins what both roles do, listed(Alice, n) and member(<Alice>.n) deleted
# and closed(n) added; every other member q she lists deletes
# member(<Alice>.n); the required atoms, the leader's and those of the
# listed role, over its extent; no freshness line, member(<a>.n) being
# required at the listed role; and the guard, the leader.  GSN's close is
# GSNP's.
close_gsnp=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\mathit{listed}(\Alice,n),\mathit{member}(\langle\Alice\rangle\cdot n)\}) \uplus \{\mathit{closed}(n)\},\\
& c'_q := c_q \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)]),\ q \neq \Alice),\\
& \text{provided } \mathit{identity},\mathit{listed}(\Alice,n) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n) \in c_q \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{guarded by } \{\Alice\}
\end{align*}
TEX
)
check_eq "close compiles a line per agent, the leader's joining what both her roles do, as Section 5.1 prints it" \
         "$close_gsnp" "$(compiled_in gsnp "$SG" close)"
check_eq "and GSN's close compiles to the same display" \
         "$close_gsnp" "$(compiled_in gsn "$GSNT" close)"

# CSSN's child_close(r, p?, r[listed(*, n)] ; n): the child Alice, the
# leader, lists herself and is in the extent; whether the parent Bob is, the
# schema does not say, so Bob's line is written for each case, the case in
# parentheses, and q excludes both.
child_close_cssn=$(cat <<'TEX' | squash
\begin{align*}
& c'_{\Alice} := (c_{\Alice} \setminus \{\mathit{listed}(\Alice,n),\mathit{member}(\langle\Alice\rangle\cdot n)\}) \uplus \{\mathit{closed}(n)\},\\
& c'_{\Bob} := c_{\Bob} \qquad (\Bob \notin \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& c'_{\Bob} := c_{\Bob} \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} \qquad (\Bob \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& c'_q := c_q \setminus \{\mathit{member}(\langle\Alice\rangle\cdot n)\} \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)]),\ q \neq \Alice,\ q \neq \Bob),\\
& \text{provided } \mathit{identity},\mathit{child},\mathit{parent}(\Bob),\mathit{listed}(\Alice,n) \in c_{\Alice} \text{ and } \mathit{identity},\mathit{parenting}(\Alice) \in c_{\Bob} \text{ and } \mathit{identity},\mathit{member}(\langle\Alice\rangle\cdot n) \in c_q \qquad (q \in \mathrm{ext}_c(\Alice[\mathit{listed}(\ast,n)])),\\
& \text{provided } \{\mathit{child},\mathit{retired}\} \cap c_{\Bob} = \emptyset,\\
& \text{guarded by } \{\Bob\}
\end{align*}
TEX
)
check_eq "CSSN's child_close compiles a line per agent, the parent's for each case of the extent" \
         "$child_close_cssn" "$(compiled_in cssn "$CSSN" child_close)"

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

# GAI's Definition Act schema admits a signed term sgn(y, pi) for pi an
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
  check "$1: child keeps its guardianship" "G = [guardianship(child, identity, parent, parenting)]" "$out"
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
        "V = grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [])" \
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
# a mark in M nor a leaving mark (def:unobstructed-marks, clause 2), and there
# is no introduction with wards.
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

# GAI's contract, programs/gai/GAI.glp at 88d85368, on GKK (GAI #6 Cowork, Code_inbox 2026-10-20 20:12 and 20:15 UTC), against GAI's appendix "Contracts with Wards" at GAI ddf0a33: agent has the guardianship (agent, identity, principal, principal_of) of def:guardianship, enter adding agent, identity and principal(p) at the agent, who had no identity, and principal_of(r) at p, who forbids agent, and no schema deleting principal or principal_of; retired is a leaving mark for {agent} (def:leaving): every guarding role forbids it and none deletes it (clause 1), retire, the one schema adding it, requires agent at that role (clause 2), and enter, the one schema adding principal, forbids it at p (clause 3); meet is the introductory act, unobstructed but for {agent}: it requires identity, which establish adds, requiring nothing and forbidding nothing but retired, and it forbids agent and retired, a mark in M and a leaving mark (def:unobstructed-marks); agent_meet is the relay act of a ward role of agent and a free role, and agents_meet that of two ward roles of agent, each forbidding nothing but agent and retired, and each requiring at a ward role of agent the attestation attestation(y), y in no other atom it requires, which clause 2 of def:relay admits as an atom of h: enter, the one schema adding agent, adds attestation(sgn(x, p)) there, and no schema deletes an attestation.  So it is grassroots with wards (def:introduction-wards, thm:grassroots-wards), the verdict naming retired, as GAI's paper claims (Proposition Grassroots), and attestation is h.
out=$(run "check($GAI, V)." "wards($GAI, W)." "guardianships($GAI, [agent], G)." "leaving($GAI, [agent], L)." "relay_h($GAI, H).")
check "GAI's agent has two guardianships, enter adding knows(r) at the principal as it adds principal_of(r)" \
      "G = [guardianship(agent, identity, principal, knows), guardianship(agent, identity, principal, principal_of)]" "$out"
check "GAI's leaving marks for {agent} are [retired], with either guardianship" \
      "L = [leaving([guardianship(agent, identity, principal, knows)], [retired]), leaving([guardianship(agent, identity, principal, principal_of)], [retired])]" "$out"
check "GAI's contract is grassroots with wards through meet, agent_meet and agents_meet, retired its leaving mark" \
      "V = grassroots_with_wards(meet, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_meet, ward(1, agent, 3), free(2)), relay(agents_meet, ward(1, agent, 3), ward(2, agent, 4))], [retired])" \
      "$out"
check "attestation is the h of clause 2 of def:relay at the AI agents' ward roles" \
      "H = [h(agent_meet, 1, attestation), h(agents_meet, 1, attestation), h(agents_meet, 2, attestation)]" "$out"
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
# def:syntactically-grassroots', its first conjunct failing by the first
# part of clause 2 of def:unobstructed.
out=$(run 'check_named(wd_listed, V).')
check "a listed role adding no mark, leaving mark, friend or guardianship atom: syntactically grassroots with wards" \
      "V = syntactically_grassroots_with_wards(befriend, [child], [guardianship(child, identity, parent, parenting)], [relay(child_adult_befriend, ward(1, child, 3), free(2)), relay(child_befriend, ward(1, child, 3), ward(2, child, 4))], [retired])" \
      "$out"
out=$(run 'check_named(wd_listed_leaving, V).' 'wards_of(wd_listed_leaving, W).')
check "a listed role adding a leaving mark fails clause 2 of def:sg-wards" \
      "W = no_introduction_with_wards" "$out"
check "and the verdict names the listed role, by clause 2 of def:unobstructed" \
      "obstructed(enter, 1, atom(retired, []), listed_role(gather, 2, atom(retired, [])))" "$out"
check_not "and it is not syntactically grassroots with wards" "syntactically_grassroots_with_wards" "$out"

# --- listed roles and name terms (def:role, def:introduction, def:unobstructed)
# GSNP with one schema more, each failing one condition, and two contracts
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
# GSNP extends GKK (GLP 35c4211a): meet stays its unobstructed introductory
# act, and befriend's obstructions are read off intro_acts.
out=$(run "intro_acts_variant(sn_listed_forbidden, $SG, A).")
check "a listed role adding friend, which befriend forbids, fails clause 2 at both roles" \
      "A = [intro_act(meet, unobstructed), intro_act(befriend, obstructed([obstructed(befriend, 1, atom(knows, [role(2)]), unobtainable), obstructed(befriend, 2, atom(knows, [role(1)]), unobtainable), obstructed(befriend, 1, atom(friend, [role(2)]), listed_role(befriend_listed, 2, atom(friend, [role(1)]))), obstructed(befriend, 2, atom(friend, [role(1)]), listed_role(befriend_listed, 2, atom(friend, [role(1)])))]))]" \
      "$out"
out=$(run "check_variant(sn_listed_witness, $SG, V)." "traceable_variant(sn_listed_witness, $SG, E).")
check "a speech act traced through a listed role only fails traceable provenance" \
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

# --- listed roles formed otherwise (def:role) ------------------------------
# close_b's listed role is of itself, no agent role; note_l's carries a
# signed term among its arguments, which GAI's Definition Act schema admits
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
# of Definition Act schema, def:schema of Section 3.2, a token to a line,
# an empty line ending a line of the text
# (notation.glp; README, "The notation"); cascade.sh joins the tokens into a
# record's schemas.txt, as notation() does here.  GSG's befriend is the schema
# Section 3.2 displays, befriend(Alice?, Bob?) : identity, knows(Bob), not
# friend(Bob), +friend(Bob)  identity, knows(Alice), not friend(Alice),
# +friend(Alice), after GKK's establish, forbidding the identity it adds,
# meet and introduce, each E_i on a line
# labelled by its role; GSN's close is Section 3.2's example of a listed
# role (def:role), with the identity
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
    at Alice: not identity, +identity

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
check_eq "GSN's close prints its listed role and name term as def:role writes them" \
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

# --- Values (GSC, Section 3.2 and the appendix "Values in Act Schemas") -----
# "Act schemas allow values of any type: an argument of an atom may be a
# value or a value variable, a schema may carry a constraint on its value
# variables, which every binding satisfies, and an atom added or deleted may
# carry a multiplicity, a count of copies and so a positive whole number ...
# A value names no agent, so the conditions below do not concern it."  The
# term form is Currencies' GB.glp: num(x), the value variable x or the
# number x; times(num(k), A); nschema(Name, Vars,
# Constraint, Roles), the constraint a conjunction of linear equalities and
# inequalities (def:numbers; README, "Writing a contract").  GB, the bonds, is Currencies' (programs/currencies/bonds/
# GB.glp at 18f28661, GB extending GKK, twelve schemas: GKK's three, mint
# once for a coin and once for a bond, advance, swap, pay, redeem and the
# three escrow transfers, over cent(u, d), an instrument issued by u and due
# at d, a coin due at 0): meet its introductory act, unobstructed; every
# predicate of traceable provenance, date naming no agent and cent naming a
# role or an agent variable the other role requires; every role of a schema
# of arity two guarding, or the role graph joined by the knows(Bob) and the
# instrument of Bob that Alice requires.  So it is syntactically grassroots.  A binding sends a
# value variable to a value and a multiplicity to a positive whole number,
# and an introduction chooses its own values (def:binding-numbers; the
# appendix "Values in Act Schemas"), and whether some choice meets the
# clauses is decided exactly, the constraints being linear (linear.glp).
echo "--- numbers (GSC, Section 3.2 and its appendix) ---"
GBT=$(term "$GLP_DIR/programs/currencies/bonds/GB.glp" gb)
check "GB's contract is read from programs/currencies/bonds/GB.glp, with numbers, a multiplicity and a constraint" \
      "nschema(mint_bond, [num(d), num(k), num(t)], gt(num(d), num(t)), [rs(guarded, [times(num(k), atom(cent, [role(1), num(d)]))]" "$GBT"
out=$(run "check($GBT, V)." "traceable_preds($GBT, E)." "intro_acts($GBT, A).")
check "GB is syntactically grassroots, the conditions not concerning its numbers" \
      "V = syntactically_grassroots" "$out"
check "GB's predicates all have traceable provenance, date naming no agent" \
      "E = [identity, knows, cent, date]" "$out"
check "GB's introductory act is meet, unobstructed" \
      "A = [intro_act(meet, unobstructed)," "$out"
check "GB's swap, obstructed by the knows it requires, deletes k instruments, k a positive whole number its introduction chooses, and no schema of arity one requiring nothing adds an instrument" \
      "intro_act(swap, obstructed([obstructed(swap, 1, atom(knows, [role(2)]), unobtainable), obstructed(swap, 1, atom(cent, [pvar(u), num(d)]), unobtainable)," "$out"
# The displays of GB's schemas with numbers (GSC, Section 5, in the closest
# form its notation gives: the constraint on a proviso line of its own, and a
# multiplicity k\cdot A, as GB's paper writes it, a multiset with one
# included in c, as def:compile writes a multiset).
redeem_display='\begin{align*}&c'"'"'_{\Alice}:=(c_{\Alice}\setminus\{\text{\textcent}(\Bob,d)\})\uplus\{\text{\textcent}(r,d1)\},\qquadc'"'"'_{\Bob}:=(c_{\Bob}\setminus\{\text{\textcent}(r,d1)\})\uplus\{\text{\textcent}(\Bob,d)\},\\&\text{provided}\mathit{identity},\mathit{knows}(\Bob),\text{\textcent}(\Bob,d)\inc_{\Alice}\text{and}\mathit{identity},\mathit{date}(t),\text{\textcent}(r,d1)\inc_{\Bob},\\&\text{provided}d\let,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's redeem compiles with its constraint on a proviso line of its own" \
         "$redeem_display" "$(compiled_in gb "$GBT" redeem)"
mint_bond_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\uplus\{k\cdot\text{\textcent}(\Alice,d)\},\\&\text{provided}\mathit{identity},\mathit{date}(t)\inc_{\Alice},\\&\text{provided}d>t,\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's mint_bond compiles with its multiplicity and its constraint" \
         "$mint_bond_display" "$(compiled_in gb "$GBT" mint_bond)"
pay_display='\begin{align*}&c'"'"'_{\Alice}:=c_{\Alice}\setminus\{k\cdot\text{\textcent}(\Bob,0)\},\qquadc'"'"'_{\Bob}:=c_{\Bob}\uplus\{k\cdot\text{\textcent}(\Bob,0)\},\\&\text{provided}\{\mathit{identity},\mathit{knows}(\Bob),k\cdot\text{\textcent}(\Bob,0)\}\subseteqc_{\Alice}\text{and}\mathit{identity}\inc_{\Bob},\\&\text{guardedby}\{\Alice\}\end{align*}'
check_eq "GB's pay requires its k coins as def:compile writes a multiset, included in c" \
         "$pay_display" "$(compiled_in gb "$GBT" pay)"
gb_notation=$(notation "$GBT")
redeem_notation=$(cat <<'EOF'
redeem(Alice?, Bob; d, r, d1, t) | d <= t :
    at Alice: identity, knows(Bob), +cent(r, d1), -cent(Bob, d)
    at Bob: identity, date(t), +cent(Bob, d), -cent(r, d1)
EOF
)
check_eq "GB's redeem prints its constraint after its head" \
         "$redeem_notation" "$(printf '%s\n' "$gb_notation" | one_schema redeem)"
swap_notation=$(cat <<'EOF'
swap(Alice?, Bob?; u, d, k, v, d1, k1) :
    at Alice: identity, knows(Bob), +k1.cent(v, d1), -k.cent(u, d)
    at Bob: identity, knows(Alice), +k.cent(u, d), -k1.cent(v, d1)
EOF
)
check_eq "GB's swap prints its multiplicities, k.A" \
         "$swap_notation" "$(printf '%s\n' "$gb_notation" | one_schema swap)"
advance_notation=$(cat <<'EOF'
advance(Alice; t, t1) | t1 = t + 1 :
    at Alice: identity, +date(t1), -date(t)
EOF
)
check_eq "GB's advance, guarded by no role, prints succ as t1 = t + 1" \
         "$advance_notation" "$(printf '%s\n' "$gb_notation" | one_schema advance)"
check_eq "GB's twelve schemas print" "12" "$(printf '%s\n' "$gb_notation" | grep -c ' :$')"

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
# appendix "Values in Act Schemas"): in nm_intro_var score adds score(n)
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

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
