#!/bin/bash
# Tests for the syntactically-grassroots checker and the compiler
# (programs/jurix).
#
#   bash programs/jurix/test_jurix.sh
#
# The two contracts of /Grassroots/Jurix Sections 3.3 and 3.4, which Section 7
# certifies by hand, and contracts broken in one place each, one per way of
# failing the three conjuncts of def:syntactically-grassroots; then the
# compilation of Section 5, against the two displays of Section 5.2; then the
# contract of a grassroots federation, /Grassroots/GFWC sections/schemas.tex,
# against the three conditions of Section 8 of /Grassroots/Jurix, and four
# contracts broken in one place each, three against those and one against
# rootedness, which is none of them; then small contracts on the
# speech-act variables of traceable provenance and on volition by connected
# component, in the language of Section 8; then the compilation of Section 8,
# against its worked box.  Exits non-zero if any check fails.

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
  # The REPL's default reduction limit is 10000, which CSSN's eighteen
  # schemas exceed; :limit is the REPL's knob for it.
  (cd "$GLP_DIR/glp_runtime" \
     && printf "%b" "$JURIX\n:limit 1000000\n$goals:quit\n" | bin/glpc 2>&1)
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

# Volition above arity two is connectedness of the role graph, or a guarding
# role in every connected component of it, not a test on every pair: a schema
# of arity four guarded at one role, whose role graph is the path 1-2-3-4,
# passes; cutting one edge of the path splits it, and the half {3,4} holds no
# guarding role.
out=$(run 'check_named(sg_chain, V).')
check "a path role graph at arity four certifies" \
      "V = syntactically_grassroots" "$out"

out=$(run 'check_named(sg_chain_cut, V).')
check "cutting an edge of the path fails volition" \
      "V = not_grassroots([volition(chain, 1, 3)])" "$out"

# Volition is one condition in Sections 3 and 8 (Jurix 7652554): tri(p?, q, r?)
# is guarded neither in all its roles nor connected, its role graph falling
# into {1,2} and {3}, each holding a guarding role, so it meets def:volition
# as it meets definition:volition as a cschema (sg_tri_cschema, below).
out=$(run 'check_named(sg_tri, V).')
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
# drops item, and sent after it, while befriend stays unobstructed and every
# schema satisfies volition.  The second conjunct alone rejects the contract.
out=$(run 'check_named(sg_svar_loose, V).' 'traceable_of(sg_svar_loose, E).')
check "a speech act from an untraceable record: only friend keeps provenance" \
      "E = [friend]" "$out"
check "and the verdict is the second conjunct alone" \
      "V = not_grassroots([untraceable([item, sent, tagged])])" "$out"

# CSSN's child-safe contract, eighteen schemas, transcribed from their entries
# of 2026-08-15 20:35 and 21:40 and 2026-08-16 12:37 UTC in
# Coordination/mail/Legal_inbox.md, with send, parent_child and friend brought
# to CSSN's paper on 2026-10-09.  Its introductory act is parent_child;
# befriending is obstructed there by design.
out=$(run 'check_named(cssn, V).' 'traceable_of(cssn, E).')
check "CSSN's contract is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"
check "CSSN's predicates of traceable provenance" \
      "E = [parenting, parent, child, friend, approval, withdrawal, member, listed, item, sent, posted, delivered]" \
      "$out"

# send is CSSN's paper's, sections/artefact-schemas.tex:46--47: the sender
# requires the item it sends and nothing else, the recipient that the sender
# is its friend (Legal 2026-10-09 18:46 UTC).  Its display in the form of
# Section 5.2.
send_cssn=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{sent}(x,\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{item}(x,a,\Alice)\},\\
& \text{provided } \mathit{item}(x,a,f) \in c_{\Alice} \text{ and } \mathit{friend}(\Alice) \in c_{\Bob}, \qquad \text{guarded by } \{\Alice\}
\end{align*}
EOF
)
check_eq "CSSN's send requires at the sender the item it sends and nothing else" \
         "$send_cssn" "$(compiled cssn send)"

# friend is reflexive, as CSSN's sections/schemas.tex declares it, and
# parent_child forbids parent(r) at the parent's role, as its
# sections/artefact-schemas.tex:8 writes it (Legal 2026-10-09 18:43 UTC).
out=$(run 'contract_named(cssn, C).')
check "CSSN's contract declares friend reflexive" \
      "C = reflexive([friend], [schema(parent_child," "$out"
parent_child_cssn=$(cat <<'EOF' | squash
\begin{align*}
& c'_{\Alice} := c_{\Alice} \uplus \{\mathit{parenting}(\Bob)\}, \qquad c'_{\Bob} := c_{\Bob} \uplus \{\mathit{parent}(\Alice),\mathit{child}\},\\
& \text{provided } \mathit{parent}(\Bob) \notin c_{\Alice} \text{ and } \mathit{parent}(\Alice) \notin c_{\Bob}, \qquad \text{guarded by } \{\Alice,\Bob\}
\end{align*}
EOF
)
check_eq "CSSN's parent_child forbids parent(r) at the parent's role" \
         "$parent_child_cssn" "$(compiled cssn parent_child)"

# --- reflexive predicates (the paragraph after def:binding) ----------------
# A contract may declare a predicate of arity one reflexive.  The social graph
# with friend declared so certifies as it does without.
out=$(run 'check_named(sg_reflexive, V).')
check "the social graph with friend reflexive is syntactically grassroots" \
      "V = syntactically_grassroots" "$out"

# No schema adds or deletes, at a role, the atom of a reflexive predicate
# naming that role: a contract in which one does is refused as malformed,
# naming the schema, the role and the atom, and is not compiled.
out=$(run 'check_named(sg_refl_malformed, V).')
check "adding or deleting friend of its own role makes the contract malformed" \
      "V = malformed([reflexive(self_befriend, 1, atom(friend, [role(1)])), reflexive(self_unfriend, 1, atom(friend, [role(1)]))])" \
      "$out"
out=$(run 'compile_named(sg_refl_malformed).')
check "a malformed contract is not compiled" \
      "% not compiled: sg_refl_malformed is malformed" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# Clause 1 of def:unobstructed skips a required atom of a reflexive predicate
# naming the role: befriend requiring friend of each party at its own role is
# obstructed when friend is not reflexive and unobstructed when it is.
out=$(run 'check_named(sg_self_required, V).')
check "friend of its own role required, friend not reflexive: clause 1 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), unobtainable), obstructed(befriend, 2, atom(friend, [role(2)]), unobtainable)])" \
      "$out"
out=$(run 'check_named(sg_refl_required, V).')
check "friend of its own role required, friend reflexive: clause 1 skips it" \
      "V = syntactically_grassroots" "$out"

# Clause 3 of def:unobstructed: the introductory act forbids at a role no atom
# of a reflexive predicate naming that role.
out=$(run 'check_named(sg_refl_forbidden, V).')
check "friend of its own role forbidden, friend reflexive: clause 3 fails" \
      "V = not_grassroots([obstructed(befriend, 1, atom(friend, [role(1)]), reflexive), obstructed(befriend, 2, atom(friend, [role(2)]), reflexive)])" \
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


# --- the contract of a grassroots federation (Section 8) -------------------
# GFWC's five schemas over seat and child, /Grassroots/GFWC
# sections/schemas.tex, every role of them a party role or a seated role,
# certified in its Section 3.1 by hand against the conditions of Section 8 of
# /Grassroots/Jurix; the checker returns the same.

out=$(run 'check_named(federation, V).' 'rooted_of(federation, E).')
check "the federation contract meets the conditions of Section 8" \
      "V = conditions_met" "$out"
check "seat is rooted" "E = [seat]" "$out"

out=$(run 'traceable_of(federation, E).')
check "seat and child have traceable provenance" "E = [seat, child]" "$out"

# Seating the assembly in the name of the community itself: the name argument
# is of least rank zero and at no party role.  Rootedness is no condition on
# the text.
out=$(run 'check_named(gf_unrooted, V).' 'rooted_of(gf_unrooted, E).')
check "the community named without its decision: seat is not rooted" \
      "E = []" "$out"
check "and the three conditions on the text are still met" \
      "V = conditions_met" "$out"

# A note of a community that is the name term of no role and that no required
# atom names: note loses traceable provenance, seat and child keep it, and the
# set of predicates occurring in the contract lacks it, the one condition of
# the three the contract fails.
out=$(run 'traceable_of(gf_untraceable, E).')
check "a note nothing traces: seat and child keep traceable provenance" \
      "E = [seat, child]" "$out"
check_not "and note does not have it" "note" "$out"
out=$(run 'check_named(gf_untraceable, V).')
check "and the contract fails traceable provenance, naming note" \
      "V = conditions_failed([untraceable([note])])" "$out"

# Seating the assembly in a community that is the name term of no role of the
# schema.  seat is the one role predicate of the contract, every role being a
# party role or a seated role, so cohesion asks its question of seat atoms
# (definition:cohesive).  The same atom costs seat its traceable provenance,
# so the verdict names seat as untraceable before the cohesion fault.
out=$(run 'check_named(gf_uncohesive, V).')
check "a seat atom of no role of the schema fails cohesion, and seat traceable provenance" \
      "V = conditions_failed([untraceable([seat]), cohesion(federate, 1, atom(seat, [nterm(nvar(eta))]))])" \
      "$out"

# A join no assembly decides: its two roles are the assemblies of the two
# communities, nothing joins them, and neither guards.
out=$(run 'check_named(gf_novolition, V).')
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

# The case of sg_svar_loose in the language of Section 8: the same speech act
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
# Section 8 the traceable provenance Section 3 gives them.
out=$(run 'check_named(sg_relay, V).' 'traceable_of(sg_relay, E).')
check_not "a guarding role in every connected component meets volition" \
      "volition(relay," "$out"
check "and a component holding none fails it" \
      "V = conditions_failed([volition(relay_unguarded, 1, 4)])" "$out"
check "the social graph keeps its traceable provenance in Section 8" \
      "E = [friend, item, sent, chained, noted]" "$out"

# sg_tri with tri written as a cschema, in the language of Section 8: the
# same verdict as in Section 3, volition being one condition in both.
out=$(run 'check_named(sg_tri_cschema, V).')
check "tri as a cschema meets the conditions of Section 8, as it does in Section 3" \
      "V = conditions_met" "$out"

# Rootedness is a notion of Section 8; a contract of Section 3 is not asked
# about it, and keeps the verdict of def:syntactically-grassroots.
out=$(run 'rooted_of(social_graph, E).' 'check_named(social_graph, V).')
check "a contract of Section 3 is not asked which predicates are rooted" \
      "E = []" "$out"
check "and keeps the verdict of Section 3" \
      "V = syntactically_grassroots" "$out"

# The compiled form of a schema with community roles is Definition Compilation
# of Section 8, which the compiler prints for a contract that meets the
# conditions on the text and for no other.
out=$(run 'compile_named(federation).')
check "a contract with community roles is compiled" \
      "begin{align" "$out"
check_not "and not refused" "not compiled" "$out"
check_not "no role of the federation is a constituent role" '\ast}' "$out"

out=$(run 'compile_named(gf_novolition).')
check "a contract that fails the conditions of Section 8 is not compiled" \
      "% not compiled: gf_novolition" "$(printf '%s' "$out" | tr '\n' ' ')"
check_not "and no display is printed for it" "begin{align" "$out"

# --- the compilation of Section 8 (definition:compile) ---------------------
# The worked box of Section 8, transcribed from
# /Grassroots/Jurix/sections/13-community-roles.tex, is the display for
# federate: the one assignment over the extent of its seated role, adding both
# atoms, the line on freshness and the guard.  Compared with the whitespace
# removed, as the two displays of Section 5.2 are, and without the full stop
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
check_eq "federate compiles to the worked box of Section 8" \
         "$federate_box" "$(compiled federation federate)"

# The five schemas of the contract, in the one form of Section 8.
out=$(run 'compile_named(federation).')
check_eq "the federation compiles to five displays" "5" \
         "$(printf '%s' "$out" | grep -c 'begin{align')"

# form: a party role in the form of Section 8, over its extent, with a proviso
# line for the seat it forbids, guarded by a part of it larger than the
# threshold 0; no name term sigma.y, so no line on freshness.
form=$(compiled federation form)
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
join=$(compiled federation join)
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
      "$(compiled federation leave_1)"
check "leave_2 is guarded by a supermajority of the child's assembly" \
      "\\text{guardedby}G,\\G\\subseteq\\mathrm{ext}_c(\\xi^{\\mathit{seat}})" \
      "$(compiled federation leave_2)"

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
