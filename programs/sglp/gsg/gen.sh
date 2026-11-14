#!/bin/bash
# gen.sh: prints GSG_sglp.glp, the sGLP source of the GSG simulation, from the scripts below.
# A script line: person, weight, proposal; the proposal is drawn weight times as often.
S=$(mktemp)
cat > $S <<'L'
p1 3 establish(p1)
p1 3 acquaint(p1, p2)
p1 3 acquaint(p1, p3)
p1 2 introduce(p1, p2, p3)
p1 2 introduce(p1, p3, p2)
p1 3 befriend(p1, p2)
p1 3 befriend(p1, p3)
p1 2 befriend(p1, p4)
p1 1 unfriend(p1, p3)
p2 3 establish(p2)
p2 3 acquaint(p2, p1)
p2 3 acquaint(p2, p4)
p2 2 introduce(p2, p1, p4)
p2 2 introduce(p2, p4, p1)
p2 3 befriend(p2, p1)
p2 3 befriend(p2, p3)
p2 3 befriend(p2, p4)
p3 3 establish(p3)
p3 3 acquaint(p3, p1)
p3 3 acquaint(p3, p5)
p3 3 befriend(p3, p2)
p3 3 befriend(p3, p5)
p3 2 befriend(p3, p6)
p4 3 establish(p4)
p4 3 acquaint(p4, p2)
p4 3 acquaint(p4, p6)
p4 3 befriend(p4, p1)
p4 3 befriend(p4, p6)
p4 1 unfriend(p4, p6)
p5 3 establish(p5)
p5 3 acquaint(p5, p3)
p5 3 acquaint(p5, p6)
p5 2 introduce(p5, p3, p6)
p5 2 introduce(p5, p6, p3)
p5 3 befriend(p5, p3)
p5 3 befriend(p5, p6)
p6 3 establish(p6)
p6 3 acquaint(p6, p4)
p6 3 acquaint(p6, p5)
p6 3 befriend(p6, p5)
p6 3 befriend(p6, p3)
L
cat <<'H'
%% GSG_sglp.glp --- the sGLP source of a simulation of GSG, the Grassroots Social Graph
%% (programs/sglp/gsg), in the term form the transformation reads (programs/sglp/README.md,
%% "The term form"), the profiles over the canonical platform's questions and nothing of its
%% interface, which is the platform's as the cascade holds it (cascade/GSG/interface.glp).
%% It is written for the canonical platform of GSG as recorded, cascade/GSG/platform.glp, and
%% six agents, keyed p1 to p6 (cva/run.sh).  Printed by gen.sh beside this file.
%%
%%   p1  Alice   meets Bob and Carol; introduces them to each other (GKK:3); befriends Bob and
%%               Carol, and Dave once Bob has introduced them; ends her friendship with Carol
%%               at times (GSG:2), and Carol may be her friend again
%%   p2  Bob     meets Alice and Dave; introduces them to each other; befriends Alice, Dave, and
%%               Carol once introduced
%%   p3  Carol   meets Alice and Eve; befriends Bob, Eve, and Frank once Eve has introduced them
%%   p4  Dave    meets Bob and Frank; befriends Alice and Frank; ends his friendship with Frank
%%               at times
%%   p5  Eve     meets Carol and Frank; introduces them to each other; befriends both
%%   p6  Frank   meets Dave and Eve; befriends Eve and Carol
%%
%% Its surface form, one person's script and the others' alike:
%%
%%   person scripted.
%%   Request =::= script_request.
%%   procedure script_request(Request, Person?, Integer?).
%%   script_request(R?, person(p6, _, _), S) :-
%%       random(S?, 15, K, _), act(K?, p6, slot(R)) @ 2/hour.
%%   procedure act(Integer?, Key?, Slot(Request)?).
%%   act(1, p6, slot(propose(establish(p6)))).           % to 3
%%   act(4, p6, slot(propose(acquaint(p6, p4)))).        % to 6
%%   act(7, p6, slot(propose(acquaint(p6, p5)))).        % to 9
%%   act(10, p6, slot(propose(befriend(p6, p5)))).       % to 12
%%   act(13, p6, slot(propose(befriend(p6, p3)))).       % to 15
%%
%%   person agreeable.
%%   Approval =::= agree.
%%   procedure agree(Approval?, Person?, Integer?).
%%   agree(approval(_, _, A?), _, _) :- say_yes(slot(A)) @ 6/hour.
%%   procedure say_yes(Slot(YesNo)?).
%%   say_yes(slot(yes)).
%%
%%   run 6 agents [requests ~ (scripted : 1.0), approvals ~ (agreeable : 1.0)]
%%       until 3 days seed 1.
%%
%% The scripted person, at each request question, draws one proposal from its list, a proposal
%% listed k times drawn k times as often; a proposal the state does not allow is refused by the
%% platform, so the order of a run is the order in which the drawn proposals become possible.
%% The agreeable person approves every attempt and every hello it is asked about.

exported procedure source(SglpTerms).
source([
   person(scripted),
   binds('Request', script_request),
   decl(script_request('Request', dual('Person'), dual('Integer'))),
H
awk '{ n[$1] += $2 } END { for (i = 1; i <= 6; i++) { p = "p" i; printf "   clause(script_request(var('"'"'R?'"'"'), person(%s, var('"'"'_'"'"'), var('"'"'_'"'"')), var('"'"'S'"'"')), [],\n          [random(var('"'"'S?'"'"'), %d, var('"'"'K'"'"'), var('"'"'_'"'"')), rated(act(var('"'"'K?'"'"'), %s, slot(var('"'"'R'"'"'))), rate(2, hour))]),\n", p, n[p], p } }' $S
echo "   decl(act(dual('Integer'), dual('Key'), dual('Slot'('Request')))),"
awk '{ p = $1; w = $2; a = $0; sub(/^[^ ]+ [^ ]+ /, "", a); for (i = 1; i <= w; i++) printf "   clause(act(%d, %s, slot(propose(%s))), [], []),\n", ++k[p], p, a }' $S
cat <<'T'

   person(agreeable),
   binds('Approval', agree),
   decl(agree(dual('Approval'), dual('Person'), dual('Integer'))),
   clause(agree(approval(var('_'), var('_'), var('A?')), var('_'), var('_')), [], [rated(say_yes(slot(var('A'))), rate(6, hour))]),
   decl(say_yes(dual('Slot'('YesNo')))),
   clause(say_yes(slot(yes)), [], []),

   run(6, [mix(requests, [share(scripted, 1.0)]), mix(approvals, [share(agreeable, 1.0)])], 3, day, 1)]).
T
rm -f $S
