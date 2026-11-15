#!/bin/bash
# gen12.sh: prints GSG12_sglp.glp, the sGLP source of the larger GSG simulation: twelve persons,
# two circles of six joined by one acquaintance, Frank's and Grace's.  The script is computed:
# each person meets its two neighbours in its circle, introduces them to each other, and
# befriends its neighbours and those it was introduced to; four persons end a friendship at times.
S=$(mktemp)
awk 'function k(i) { return sprintf("p%02d", i) }
function line(p, w, a) { print k(p), w, a }
function pair(x, y) { line(x, 2, "befriend(" k(x) ", " k(y) ")"); line(y, 2, "befriend(" k(y) ", " k(x) ")") }
function meet(x, y) { line(x, 3, "acquaint(" k(x) ", " k(y) ")"); line(y, 3, "acquaint(" k(y) ", " k(x) ")") }
function intro(a, b, c) { line(a, 1, "introduce(" k(a) ", " k(b) ", " k(c) ")"); line(a, 1, "introduce(" k(a) ", " k(c) ", " k(b) ")") }
BEGIN {
  for (i = 1; i <= 12; i++) line(i, 3, "establish(" k(i) ")")
  for (c = 0; c < 2; c++) for (j = 1; j <= 6; j++) { i = 6 * c + j; r = 6 * c + j % 6 + 1; l = 6 * c + (j + 4) % 6 + 1
    meet(i, r); pair(i, r); intro(i, l, r); if (l < r) pair(l, r); else pair(r, l) }
  meet(6, 7); pair(6, 7)
  intro(6, 7, 5); intro(6, 7, 1); intro(7, 6, 8); intro(7, 6, 12)
  pair(5, 7); pair(1, 7); pair(6, 8); pair(6, 12)
  line(1, 1, "unfriend(" k(1) ", " k(3) ")"); line(4, 1, "unfriend(" k(4) ", " k(5) ")")
  line(9, 1, "unfriend(" k(9) ", " k(10) ")"); line(12, 1, "unfriend(" k(12) ", " k(7) ")") }' | sort -s -k1,1 | awk '!seen[$0]++' > $S
cat <<'H'
%% GSG12_sglp.glp --- the sGLP source of the larger simulation of GSG (programs/sglp/gsg), in the
%% term form the transformation reads, for the canonical platform of GSG as recorded,
%% cascade/GSG/platform.glp, and twelve agents, keyed p01 to p12.  Printed by gen12.sh.
%%
%% Two circles of six, p01 Alice to p06 Frank and p07 Grace to p12 Leo, joined by one
%% acquaintance, Frank's and Grace's.  Each person meets its two neighbours in its circle
%% (GKK:2), introduces them to each other (GKK:3), and befriends its neighbours and those it
%% was introduced to (GSG:1).  Frank introduces Grace to Eve and to Alice, and Grace introduces
%% Frank to Heidi and to Leo, so friendships cross between the circles.  Alice, Dave, Ivan and
%% Leo each end one friendship at times (GSG:2).
%%
%% Two approval profiles: the agreeable person approves whatever it is asked; the choosy person
%% declines one request in three.  A friendship is made only by the act of both (GSG:1): the
%% proposal of one and the approval of the other.
%%
%%   person scripted.     Request =::= script_request.   one proposal drawn from the list, 2/hour
%%   person agreeable.    Approval =::= agree.           yes, 6/hour
%%   person choosy.       Approval =::= choose.          yes, yes or no, drawn, 6/hour
%%   run 12 agents [requests ~ (scripted : 1.0), approvals ~ (agreeable : 0.67, choosy : 0.33)]
%%       until 4 days seed 1.

exported procedure source(SglpTerms).
source([
   person(scripted),
   binds('Request', script_request),
   decl(script_request('Request', dual('Person'), dual('Integer'))),
H
awk '{ n[$1] += $2 } END { for (i = 1; i <= 12; i++) { p = sprintf("p%02d", i); printf "   clause(script_request(var('"'"'R?'"'"'), person(%s, var('"'"'_'"'"'), var('"'"'_'"'"')), var('"'"'S'"'"')), [],\n          [random(var('"'"'S?'"'"'), %d, var('"'"'K'"'"'), var('"'"'_'"'"')), rated(act(var('"'"'K?'"'"'), %s, slot(var('"'"'R'"'"'))), rate(2, hour))]),\n", p, n[p], p } }' $S
echo "   decl(act(dual('Integer'), dual('Key'), dual('Slot'('Request')))),"
awk '{ p = $1; w = $2; a = $0; sub(/^[^ ]+ [^ ]+ /, "", a); for (i = 1; i <= w; i++) printf "   clause(act(%d, %s, slot(propose(%s))), [], []),\n", ++k[p], p, a }' $S
cat <<'T'

   person(agreeable),
   binds('Approval', agree),
   decl(agree(dual('Approval'), dual('Person'), dual('Integer'))),
   clause(agree(approval(var('_'), var('_'), var('A?')), var('_'), var('_')), [], [rated(say_yes(slot(var('A'))), rate(6, hour))]),
   decl(say_yes(dual('Slot'('YesNo')))),
   clause(say_yes(slot(yes)), [], []),

   person(choosy),
   binds('Approval', choose),
   decl(choose(dual('Approval'), dual('Person'), dual('Integer'))),
   clause(choose(approval(var('_'), var('_'), var('A?')), var('_'), var('S')), [],
          [random(var('S?'), 3, var('K'), var('_')), rated(answer(var('K?'), slot(var('A'))), rate(6, hour))]),
   decl(answer(dual('Integer'), dual('Slot'('YesNo')))),
   clause(answer(1, slot(yes)), [], []),
   clause(answer(2, slot(yes)), [], []),
   clause(answer(3, slot(no)), [], []),

   run(12, [mix(requests, [share(scripted, 1.0)]), mix(approvals, [share(agreeable, 0.67), share(choosy, 0.33)])], 4, day, 1)]).
T
rm -f $S
