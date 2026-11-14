#!/bin/bash
# genV.sh <villages> <days> <seed> [local share] [keen share]: prints the sGLP source of GSG's
# village simulation, CSSN's simulation of the social graph (programs/sglp/cssn/CSSN_sglp.glp)
# without the children: its census, its parents' profiles and its offers, introductions and
# endings of friendship, over GSG's acts alone.  The helpers are copied from CSSN's source as
# they stand, renamed cs_ to gs_; the census's households, the menu and the policies are new.
V=$1; DAYS=$2; SEED=$3; LOC=${4:-0.5}; KEEN=${5:-0.5}
G=$(cd "$(dirname "$0")/../../.." && pwd); CS=$G/programs/sglp/cssn/CSSN_sglp.glp
KEEP=" cs_census cs_persons_all cs_persons_pairs cs_acq cs_acq_parents cs_pairs_of cs_app_pairs cs_acq_of cs_parents cs_next cs_app_persons cs_village_parents cs_other_parents cs_app cs_two cs_two1 cs_two2 cs_pick cs_length cs_nth cs_without cs_minus cs_inter cs_inter1 cs_mem cs_attr_knows cs_attr_village cs_attr_families cs_friends cs_all_parents cs_first_missing cs_first_missing1 cs_pick_or_nothing cs_has_identity cs_knows_atoms cs_request cs_offer cs_cands cs_offer_from cs_offer_to cs_introduce cs_introduce1 cs_end_friendship1 cs_say cs_yes_if "
# The offer with no one to offer to: CSSN proposes meeting the first person known again, which adds a
# copy of knows each time; here it proposes establish, which the platform refuses and which changes nothing.
FB=$(cat <<'X'
s/clause(gs_offer_to(\[\], \[\], var('Me'), \[var('Q0') | var('_')\], var('_'), slot(propose(acquaint(var('Me?'), var('Q0?'))))), \[\], \[\]),/clause(gs_offer_to([], [], var('Me'), var('_'), var('_'), slot(propose(establish(var('Me?'))))), [], []),/
X
)
cat <<H
%% ---------------------------------------------------------------------------
%% The sGLP source of GSG's village simulation (programs/sglp/gsg), printed by
%% genV.sh $V $DAYS $SEED $LOC $KEEN, in the term form the transformation reads, over the
%% canonical platform of GSG as recorded, cascade/GSG/platform.glp and interface.glp.
%%
%% It is CSSN's simulation of the social graph (programs/sglp/cssn/CSSN_sglp.glp; CSSN,
%% Section 5) without the children.  The census: villages of ten households, a household of
%% two adults 70 times in 100 and one otherwise; an adult knows at the start the adults of
%% their own village and two adults of other villages, their acquaintances, acquaintance
%% being mutual.  The menu, the approach of an adult, local or open: an adult first
%% establishes their identity, then comes to know each adult they know from outside the app,
%% one per menu (acquaint, GKK:2); each of these after seven or eight minutes on average.  Then
%% the adult answers the menu twice an hour on average: 45 in 52 an offer of friendship
%% (befriend, GSG:1) to an adult they know who is not yet a friend, of their own village 95
%% times in 100 for a local adult and 60 for an open one, uniformly from the list; 6 in 52
%% introducing a friend to another friend (introduce, GKK:3), the two drawn uniformly; and 1
%% in 52 ending a friendship (unfriend, GSG:2), the friend drawn uniformly; each falling back
%% to an offer where it has no one to act on, and an offer to a proposal the platform refuses,
%% establish, where there is no one to offer to.  The policy of an adult, keen or choosy: a meeting
%% and an introduction approved at once; an offer of friendship approved 90 times in 100 by a
%% keen adult and 50 by a choosy one, after half an hour on average; anything else approved.
%% CSSN's rates are a year's; these are a day's, so that a run of a day or two shows what
%% CSSN's year shows.
%%
%%   run gs_villages$V [ approach of adult ~ (local : $LOC ; open : $(awk "BEGIN{print 1-$LOC}")),
%%                     policy of adult ~ (keen : $KEEN ; choosy : $(awk "BEGIN{print 1-$KEEN}")) ]
%%       until $DAYS days seed $SEED.
%% ---------------------------------------------------------------------------

exported procedure source(SglpTerms).
source([
   type('GsFam', [fam('Integer', 'List'('Key'), 'List'('Key'))]),
   type('GsAttr', [village('Integer'), household('List'('Key')), knows('List'('Key')), families('List'('GsFam'))]),
   type('Person', [person('Key', 'Constant', 'List'('GsAttr'))]),
   type('GsKeys', [ks('Key', 'Key')]),
   type('GsPick', [pick('Key'), nothing]),
   type('GsKeys1', [['Key' | 'List'('Key')]]),
   type('GsTodo', [establish, acquaint('Key'), steady]),
   type('GsPair', [acq('Key', 'Key')]),

   %% THE CENSUS.
   decl(gs_villages$V(dual('Integer'), 'List'('Person'))),
   clause(gs_villages$V(var('S'), var('Ps?')), [], [gs_keys(var('Ks')), gs_census(var('Ks?'), var('S?'), var('Ps'))]),
   %% gs_draw(Ks?, V?, F?, S?, Fs, S1): the households, from the key table, ten to a village; a household has no children.
   decl(gs_draw(dual('List'('GsKeys')), dual('Integer'), dual('Integer'), dual('Integer'), 'List'('GsFam'), 'Integer')),
   clause(gs_draw([], var('_'), var('_'), var('S'), [], var('S?')), [], []),
   clause(gs_draw([ks(var('P1'), var('P2')) | var('Ks')], var('V'), var('F'), var('S'), [fam(var('V?'), var('Pa?'), []) | var('Fs?')], var('S2?')), [ground(var('V?'))], [random(var('S?'), 100, var('K1'), var('S1')), gs_parents(var('K1?'), var('P1?'), var('P2?'), var('Pa')), gs_next(var('V?'), var('F?'), var('V1'), var('F1')), gs_draw(var('Ks?'), var('V1?'), var('F1?'), var('S1?'), var('Fs'), var('S2'))]),
   %% gs_persons(Fs?, All?, Pairs?, Ps): the adults of the households Fs.
   decl(gs_persons(dual('List'('GsFam')), dual('List'('GsFam')), dual('List'('GsPair')), 'List'('Person'))),
   clause(gs_persons([], var('_'), var('_'), []), [], []),
   clause(gs_persons([fam(var('V'), var('Pa'), var('_')) | var('Fs')], var('All'), var('Pairs'), var('Ps?')), [ground(var('V?')), ground(var('Pa?')), ground(var('All?')), ground(var('Pairs?'))], [gs_adult_persons(var('Pa?'), var('V?'), var('Pa?'), var('All?'), var('Pairs?'), var('Ps1')), gs_persons(var('Fs?'), var('All?'), var('Pairs?'), var('Ps2')), gs_app_persons(var('Ps1?'), var('Ps2?'), var('Ps'))]),
   decl(gs_adult_persons(dual('List'('Key')), dual('Integer'), dual('List'('Key')), dual('List'('GsFam')), dual('List'('GsPair')), 'List'('Person'))),
   clause(gs_adult_persons([], var('_'), var('_'), var('_'), var('_'), []), [], []),
   clause(gs_adult_persons([var('P') | var('Pa')], var('V'), var('Pa0'), var('All'), var('Pairs'), [person(var('P?'), adult, [village(var('V?')), household(var('Pa0?')), knows(var('Kn?')), families(var('All?'))]) | var('Ps?')]), [ground(var('P?')), ground(var('V?')), ground(var('Pa0?')), ground(var('All?')), ground(var('Pairs?'))], [gs_village_parents(var('All?'), var('V?'), var('P?'), var('Vp')), gs_acq_of(var('P?'), var('Pairs?'), var('Ac')), gs_app(var('Vp?'), var('Ac?'), var('Kn')), gs_adult_persons(var('Pa?'), var('V?'), var('Pa0?'), var('All?'), var('Pairs?'), var('Ps'))]),
   %% gs_last_view(L?, Atoms): the atoms of the last view seen, [] where none was shown; GSG's view names its key first.
   decl(gs_last_view(dual('List'('Output')), 'List'('Atom'))),
   clause(gs_last_view([], []), [], []),
   clause(gs_last_view([view(var('_'), var('As'), var('_'), var('_'), var('_')) | var('_')], var('As?')), [], []),
   clause(gs_last_view([var('_') | var('L')], var('As?')), [otherwise], [gs_last_view(var('L?'), var('As'))]),

   %% CSSN's helpers, as they stand.
H
awk -v keep="$KEEP" '{ if (match($0, /^   (decl|clause)\(cs_[a-z0-9_]+\(/)) { n = substr($0, RSTART, RLENGTH); sub(/^   (decl|clause)\(/, "", n); sub(/\($/, "", n); if (index(keep, " " n " ")) print } }' $CS | sed "s/cs_/gs_/g; s/'Cs\([A-Z]\)/'Gs\1/g" | sed -e "$FB"
cat <<'M'

   %% THE MENU.  The approach: local or open, the share of offers made in the own village.
   person(local),
   binds('Request', local_request),
   decl(local_request('Request', dual('Person'), dual('Integer'))),
   clause(local_request(var('R?'), person(var('Me'), var('_'), var('As')), var('S')), [], [seen(var('L')), gs_menu(95, var('Me?'), var('As?'), var('L?'), var('S?'), slot(var('R')))]),
   person(open),
   binds('Request', open_request),
   decl(open_request('Request', dual('Person'), dual('Integer'))),
   clause(open_request(var('R?'), person(var('Me'), var('_'), var('As')), var('S')), [], [seen(var('L')), gs_menu(60, var('Me?'), var('As?'), var('L?'), var('S?'), slot(var('R')))]),
   decl(gs_menu(dual('Integer'), dual('Key'), dual('List'('GsAttr')), dual('List'('Output')), dual('Integer'), dual('Slot'('Request')))),
   clause(gs_menu(var('Local'), var('Me'), var('As'), var('L'), var('S'), slot(var('W?'))), [], [gs_last_view(var('L?'), var('Atoms')), gs_menu1(var('Atoms?'), var('Local?'), var('Me?'), var('As?'), var('S?'), slot(var('W')))]),
   decl(gs_menu1(dual('List'('Atom')), dual('Integer'), dual('Key'), dual('List'('GsAttr')), dual('Integer'), dual('Slot'('Request')))),
   clause(gs_menu1(var('Atoms'), var('Local'), var('Me'), var('As'), var('S'), slot(var('W?'))), [ground(var('Atoms?')), ground(var('As?')), ground(var('Me?'))], [gs_has_identity(var('Atoms?'), var('Id')), gs_attr_knows(var('As?'), var('Kn')), gs_knows_atoms(var('Atoms?'), var('Kw')), gs_knows_atoms(var('Atoms?'), var('Ks')), gs_first_missing(var('Kn?'), var('Kw?'), var('Q')), gs_todo(var('Id?'), var('Q?'), var('T')), gs_do(var('T?'), var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S?'), slot(var('W')))]),
   decl(gs_todo(dual('YesNo'), dual('GsPick'), 'GsTodo')),
   clause(gs_todo(no, var('_'), establish), [], []),
   clause(gs_todo(yes, pick(var('Q')), acquaint(var('Q?'))), [], []),
   clause(gs_todo(yes, nothing, steady), [], []),
   decl(gs_do(dual('GsTodo'), dual('Integer'), dual('Key'), dual('List'('Key')), dual('List'('GsAttr')), dual('List'('Atom')), dual('Integer'), dual('Slot'('Request')))),
   clause(gs_do(establish, var('_'), var('Me'), var('_'), var('_'), var('_'), var('_'), slot(var('W?'))), [], [rated(gs_request(slot(var('W')), propose(establish(var('Me?')))), rate(8, hour))]),
   clause(gs_do(acquaint(var('Q')), var('_'), var('Me'), var('_'), var('_'), var('_'), var('_'), slot(var('W?'))), [], [rated(gs_request(slot(var('W')), propose(acquaint(var('Me?'), var('Q?')))), rate(8, hour))]),
   clause(gs_do(steady, var('Local'), var('Me'), var('Ks'), var('As'), var('Atoms'), var('S'), slot(var('W?'))), [], [random(var('S?'), 52, var('K'), var('S1')), rated(gs_act(var('K?'), var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S1?'), slot(var('W'))), rate(2, hour))]),
   %% gs_act(K?, ...): the act drawn, K in 1..52: 45 an offer, 6 an introduction, 1 an ending.
   decl(gs_act(dual('Integer'), dual('Integer'), dual('Key'), dual('List'('Key')), dual('List'('GsAttr')), dual('List'('Atom')), dual('Integer'), dual('Slot'('Request')))),
   clause(gs_act(var('K'), var('Local'), var('Me'), var('Ks'), var('As'), var('Atoms'), var('S'), slot(var('W?'))), ['=<'(var('K?'), 45)], [gs_offer(var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S?'), slot(var('W')))]),
   clause(gs_act(var('K'), var('Local'), var('Me'), var('Ks'), var('As'), var('Atoms'), var('S'), slot(var('W?'))), ['>'(var('K?'), 45), '=<'(var('K?'), 51)], [gs_introduce(var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S?'), slot(var('W')))]),
   clause(gs_act(52, var('Local'), var('Me'), var('Ks'), var('As'), var('Atoms'), var('S'), slot(var('W?'))), [], [gs_end_friendship(var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S?'), slot(var('W')))]),
   %% gs_end_friendship: propose(unfriend(Me, Q)), Q a friend drawn uniformly; an offer where there is none.
   decl(gs_end_friendship(dual('Integer'), dual('Key'), dual('List'('Key')), dual('List'('GsAttr')), dual('List'('Atom')), dual('Integer'), dual('Slot'('Request')))),
   clause(gs_end_friendship(var('Local'), var('Me'), var('Ks'), var('As'), var('Atoms'), var('S'), slot(var('W?'))), [ground(var('Me?')), ground(var('Atoms?'))], [gs_friends(var('Atoms?'), var('Fr')), random(var('S?'), 2147483646, var('S1'), var('S2')), gs_pick_or_nothing(var('Fr?'), var('S1?'), var('Q')), gs_end_friendship1(var('Q?'), var('Local?'), var('Me?'), var('Ks?'), var('As?'), var('Atoms?'), var('S2?'), slot(var('W')))]),

   %% THE POLICY.  Keen or choosy: an offer of friendship approved 90 or 50 times in 100.
   person(keen),
   binds('Approval', keen_approval),
   decl(keen_approval(dual('Approval'), dual('Person'), dual('Integer'))),
   clause(keen_approval(approval(hello(var('_'), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(keen_approval(approval(attempt(var('_'), var('_'), var('_'), acquaint(var('_'), var('_')), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(keen_approval(approval(attempt(var('_'), var('_'), var('_'), introduce(var('_'), var('_'), var('_')), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(keen_approval(approval(attempt(var('_'), var('_'), var('_'), var('C'), var('_')), var('_'), var('A?')), var('_'), var('S')), [otherwise], [rated(gs_answer(90, var('C?'), var('S?'), slot(var('A'))), rate(2, hour))]),
   person(choosy),
   binds('Approval', choosy_approval),
   decl(choosy_approval(dual('Approval'), dual('Person'), dual('Integer'))),
   clause(choosy_approval(approval(hello(var('_'), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(choosy_approval(approval(attempt(var('_'), var('_'), var('_'), acquaint(var('_'), var('_')), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(choosy_approval(approval(attempt(var('_'), var('_'), var('_'), introduce(var('_'), var('_'), var('_')), var('_')), var('_'), var('A?')), var('_'), var('_')), [], [gs_say(slot(var('A')), yes)]),
   clause(choosy_approval(approval(attempt(var('_'), var('_'), var('_'), var('C'), var('_')), var('_'), var('A?')), var('_'), var('S')), [otherwise], [rated(gs_answer(50, var('C?'), var('S?'), slot(var('A'))), rate(2, hour))]),
   decl(gs_answer(dual('Integer'), dual('Class'), dual('Integer'), dual('Slot'('YesNo')))),
   clause(gs_answer(var('P'), befriend(var('_'), var('_')), var('S'), slot(var('A?'))), [], [random(var('S?'), 100, var('K'), var('_')), gs_yes_if(var('K?'), var('P?'), var('A'))]),
   clause(gs_answer(var('_'), var('_'), var('_'), slot(yes)), [otherwise], [true]),

M
echo "   %% THE RUN."
echo "   run(gs_villages$V, [mix(approach, adult, [share(local, $LOC), share(open, $(awk "BEGIN{print 1-$LOC}"))]), mix(policy, adult, [share(keen, $KEEN), share(choosy, $(awk "BEGIN{print 1-$KEEN}"))])], $DAYS, day, $SEED),"
echo ""
echo "   %% THE KEY TABLE: two keys a household, v<village>h<household>a and b."
printf "   decl(gs_keys('List'('GsKeys'))),\n   clause(gs_keys(["
awk -v V=$V 'BEGIN { for (v = 1; v <= V; v++) for (h = 1; h <= 10; h++) printf "%sks(v%dh%02da, v%dh%02db)", (v + h > 2 ? ", " : ""), v, h, v, h }'
printf "]), [], [])]).\n"
