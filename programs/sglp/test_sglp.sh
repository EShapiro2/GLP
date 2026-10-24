#!/bin/bash
# Tests of sGLP in GLP (programs/sglp) against sGLP's paper (the repository
# svGLP-Stochastic-Volitional-GLP at d2f64b6) and its code tasks of 2026-10-02
# 00:06 UTC, item 6, 15:23 UTC, items 1 and 3, 15:24 UTC, items 1 to 4, and
# 15:44 UTC, and of 2026-10-03 08:44 UTC, item 2, 08:58 UTC, 09:22 UTC and
# 09:45 UTC, the transformation of 2026-10-03 09:27 UTC, sGLP's tasks 5 to 8 of
# 2026-10-09 22:01 UTC and 2026-10-10 07:59, 08:14 and 09:33 UTC, and sGLP's
# task of 2026-10-20 08:00 UTC, at sGLP 0270ac6: the census, roles, the person,
# the screen and the log.
#
#   bash programs/sglp/test_sglp.sh
#
# (i)   tests/busy: a request's token is bound only after when_idle has
#       succeeded --- a rated goal and an unrated busy goal, the busy goal runs
#       to rest first, whichever is spawned first (Section 4, "The monitor").
# (ii)  tests/law: with four pending requests of rates 1/second, 2/second,
#       30/minute and 3600/hour and nothing else, the first 10000 times between
#       releases have the mean and the variance of an exponential with rate
#       4.5 per second, each within 10% (Section 2, Proposition "Time to the
#       Next Release").
# (iii) the social graph (social_graph/run.sh), 100 agents for 30 days: two runs
#       from seed 20260927 write byte-identical logs, and seeds 20260927 and 1
#       write different ones (Section 4, "The run": every draw is from the
#       run's seed).
# (iv)  the social graph, 100 agents for one year from seed 20260927: the run
#       ends with no error and a non-empty log of menus, approvals and their
#       answers, and its monitor's last line.
# (v)   the mix as arguments, 100 agents for 30 days from seed 20260927:
#       social_graph/6 at 60 and 70 writes the log social_graph/4 writes; at 0
#       and 0 every agent is indifferent and sociable, so over 0.4 of its menu
#       answers pick the other sex and over 0.8 of its approvals are answered yes
#       (the profiles' 0.5 and 0.9); at 100 and 100 every agent is homophile
#       and wary, so under 0.1 pick the other sex and under 0.3 are answered
#       yes (0.02 and 0.2).
# (vi)  friendship.awk's months, on the log of (iv): twelve lines, month m at
#       m * 2629800 s; the edges do not decrease and are those within a sex
#       and across; month 1's are the pairs with a yes before 2629800 s,
#       counted apart; month 12's graph is the year's.
# (vii) the population's draw (the task of 15:44 UTC, and of 2026-10-03
#       08:58 UTC for each agent's line): the draw run.sh prints for (iv) is
#       the profiles the log of (iv) shows, each agent's read from its menus'
#       other-sex share and its approvals' yes share; the .draw file beside the
#       log of (iv) is that counts line, then agents 1..100 in order, the
#       counts line being their counts; each agent's line is its profiles as
#       the log of (iv) shows them; at 0 and 0 and at 100 and 100, the draws
#       of (v) are none and all, in their counts and in every agent's line.
# (viii) circulation.awk (the task of 15:24 UTC, item 4, and of 2026-10-03
#       08:44 UTC, item 2, and 09:22 UTC), checked by hand: on
#       tests/circulation/four.log, a fixed log of four agents into its fifth
#       simulated month --- nine swaps proposed, seven accepted and two
#       declined; nine pays, taking part of a holding, more than a holding, all
#       of one and of coins not held; an answer at a month's end exactly; a pay
#       answered while a proposal waits; coins arriving while a proposal waits; the
#       clock --- it prints tests/circulation/four.expected, the twelve
#       months and the totals computed by hand from the log.  The pay answered
#       while a proposal waits: agent 4 pays 3 the ten of 3's coins it holds
#       (6500000 s) and, its wallet empty, proposes 3 a swap of ten (6600000
#       s), whose proposal waits at 3 showing Held 7, the seven of 4's coins 3
#       holds; 3's person answers pay(4, 9) while it waits (7800000 s, month 3)
#       and the proposal yes in month 4 (8000000 s).  The pay is made when the proposal
#       is answered, after the proposal's move: 3 holds 7 + 10 = 17 of 4's coins
#       and pays 9, keeping 8, and 4 holds 10 of 3's; so month 3 ends at
#       circulation 47, holdings 5, wallets 3, the pay not yet made, and
#       8000000 s at 58, 6 and 4.  A pay made at its answer would take min(9,
#       7) = 7 in month 3, leaving 40, 4 and 3 there, and 60 at 8000000
#       s.  Coins arriving while a proposal waits (the task of 2026-10-03 09:22
#       UTC): 2 pays 1 the ten of 1's coins it holds (8200000 s) and, its
#       wallet empty, proposes 1 a swap of ten (8300000 s), whose proposal waits
#       at 1; 3 proposes 2 a swap of ten (8400000 s), whose proposal waits at 2; 1
#       answers yes (10000000 s, month 4), holding 20 of 2's coins, and sends
#       2 ten of its own, which arrive while 2's proposal waits; 2 answers yes in
#       month 5 (11000000 s), holding 10 of 3's and 3 10 of 2's, and then
#       takes the ten of 1's coins.  So month 4 ends at circulation 58,
#       holdings 5, wallets 3, 2's ten of 1's coins not yet held, and months 5
#       to 12 and the log at 88, 8 and 4; coins held at the yes would give 68,
#       6 and 4 in month 4.
# (ix)  coins among friends (coins/run.sh; the task of 15:24 UTC, items 1 to
#       3), four agents for a week on the hand-made graph
#       tests/circulation/four.graph at the paper's mix, 50 and 50: two runs
#       from seed 20260927 write byte-identical logs and seed 1 a different
#       one; the run ends with no error at its clock, within the week;
#       circulation.awk reads its log whole, menus and offers and their
#       answers in the order of time, each offer answered being the proposal
#       waiting at its agent; and every menu and offer is as the agent's
#       clauses and request/2 give it.  The population's draw (the task of
#       2026-10-03 09:45 UTC): the .draw file beside each log is the counts
#       line run.sh prints, then agents 1..4 in order, whose counts it is; and,
#       four agents for a year from seed 1 on the same graph, each agent's line
#       is its profiles where the log shows them: its spending always, a
#       spender's menus coming daily and a saver's weekly, so that an agent
#       answering more menus than one every sqrt(7) days of the run, the rate
#       half-way between the two on a log scale, is shown a spender and one
#       answering fewer a saver; and its credit where it is shown an offer with
#       Held above 20, which the cautious answer no and the generous yes, every
#       such answer agreeing with its line.
# (x)   the transformation (transform.glp, transform.sh; the task of 2026-10-03
#       09:27 UTC): the printed modules in place, social_graph/profiles.glp and
#       population.glp and coins/profiles.glp and population.glp, are what the
#       transformation prints from the sGLP sources, graph_sglp.glp and
#       coins_sglp.glp, byte for byte (transform.sh --check); a source with a
#       dimension whose probabilities do not sum to one is refused with that
#       fault and nothing printed, its profile in no dimension being no fault
#       (sGLP's paper at 8813a7f, Section 3, Definitions "Stochastic Person,
#       Population" and "Well-Formed"; sGLP's task of 2026-10-24 08:32 UTC); a
#       mix with a share of 0.0 is translated, its draw selecting that profile
#       for no draw of 1..100; the same source mended is translated, the rated goal's procedure
#       taking the token, the person procedure the monitor's reference, the
#       asks Ask ::= ask(Constant, Question) and Question the union of t(T),
#       the person process a clause for its person declaration, which reads an
#       ask ask(T, t(X)) of the ask stream (vGLP, Definition "Canonical
#       Compilation"; sGLP's conformance of 2026-10-09 21:21 UTC), draws a
#       value of 1..2147483646 from its seed, hands it as the person goal's
#       seed and goes on with the rest of the asks and its next seed (sGLP
#       9f57a43, Definition "Person Process, Simulation Program, Stochastic
#       Agent"; sGLP's task 4 of 2026-10-09 20:59 UTC), and the run
#       declaration its thresholds.  The person clause by the interactive
#       type's mode (sGLP's task 5 of 2026-10-09 22:01 UTC; sGLP 8b0f1dd,
#       Definition "Person Process, ...": E the end the ask carries, the writer
#       where T is in reader mode; Definition "Dual, ...": the person
#       procedure's first argument of the dual type): a source whose one
#       interactive type, Rating ::= rating(Integer), is in reader mode, and
#       whose profile fair answers it with a rating drawn of 1..5 after a rated
#       goal, is translated: the person procedure imported with its first
#       argument Rating, the writer; the person process's clause takes the ask
#       ask(Type, rating(X?)), the writer X it carries, with no guard, spawns
#       the person goal on a fresh writer Y and the answer procedure on Y? and
#       X, which, once Y? is ground, assigns X with it and logs it.  The
#       handover by the moded type (sGLP's task 6 of 2026-10-10 07:59 UTC, (a);
#       sGLP's paper, Section 4, "The person process", and Appendix B: the
#       answer handed over once the positions the person writes are ground,
#       with fresh writers at the positions the program writes, whose readers
#       the person goal gets): a reader-mode type Bid ::= bid(Integer, Reply?)
#       and a writer-mode type Post ::= post(Integer, Line?), Line ::=
#       line(String, Status?), each handed over once the String or Integer the
#       person writes is ground, a fresh writer at the position the program
#       writes, '_' in the log, and, by the relay of sGLP's task 8 of 09:33
#       UTC, the person goal's writer there bound to a reader the person
#       process holds, the program's term shown to the person goal by the
#       present procedure of its type once ground (Section 4, "The person
#       process": "Every term crossing between the program and the person goal,
#       in either direction and at every depth, crosses the person process");
#       a writer-mode type Panel ::= panel(List(Card)), Card ::= card(Integer,
#       YesNo?), with positions the person writes inside an argument the
#       program writes, shown whole once the term the program writes has no
#       reader, the list of cards built and shown once its last cell is
#       written, each card's answer a question of its own, logged in the panel
#       as shown whole (sGLP's Q1 of 09:33 UTC, where task 6 refused it; sGLP's
#       task of 2026-10-18 20:52 UTC, item 1, at sGLP e8367a3, Section 3,
#       Definition "Relay" (ii), where the cards were shown an element at a
#       time); a rated goal's procedure given a stop clause
#       dropping its arguments, and a rated goal whose procedure has an output
#       argument refused (Section 4, "The transformation"; sGLP's task of
#       2026-10-14 09:54 UTC, item 2, where the single-answer check of task 6,
#       (b), is removed with Definition "Single-Answer"); and a profile whose
#       predicate is the compiled program's refused (Section 3, Definition
#       "Well-Formed").  The census, roles, the person and the screen (sGLP's
#       task of 2026-10-20 08:00 UTC, items 1 to 4; Section 3, Definitions
#       "Person, Census", "Dual, Person Procedure, Person Declaration",
#       "Profile, Dimension", "Well-Formed" and "Screen, Seen"): every person
#       procedure takes its person second and the monitor's and the screen's
#       references last, and the person process, its guard the person ground,
#       hands each person goal its person; a census of two roles is
#       translated, the census exported from the profiles module and called by
#       the population's persons/5, which refuses a person of a role of no
#       dimension, the stochastic person of each role its functor the role and
#       one profile per dimension of the role, a person procedure shared by two
#       profiles; and refused: a role whose dimensions do not cover the
#       interactive types, a census undeclared, a person procedure without the
#       person, a platform defining Person or seen, a profile defining seen, a
#       Person not of the terms person(K, Role, V); and seen(L), in the printed
#       profiles of (xix), the request seen(L) on the screen's reference.
# (xi)  a run with an interactive type in reader mode (tests/reader): five
#       ratings of the person, each logged once in the order of time and read
#       by the program; the population printed by transform.sh before the
#       run, the program holding no call of the transformation.
# (xii) a run with vGLP's delivery status (tests/delivery; sGLP's task 6, (c)):
#       the person's Stream(Msg)?, Msg ::= msg(String, Status?), the person
#       writing three messages, each once the last one's status is written,
#       and the program writing sent, delivered or read into each; the printed
#       relay, an element at a time, each status shown to the person goal by
#       the person process (task 8), and the log, each message with its status
#       as a variable, then the stream's end.
# (xiii) a run with a question the program puts inside an answer
#       (tests/receipt; sGLP's task 8 of 2026-10-10 09:33 UTC): (xii)'s
#       Stream(Msg)? with Status ::= sent ; read(Ack?), the person
#       acknowledging each read; the printed relay in both directions, and the
#       log, the acknowledgement among the answers, read(ack), the program
#       reading it.
# (xiv) a run with positions the person writes inside an argument the program
#       writes (tests/panel; sGLP's Q1 of 2026-10-10 09:33 UTC): Panel ::=
#       panel(List(Card)), Card ::= card(Integer, YesNo?), in writer mode, a panel
#       of three cards; the log each card's answer as a question of its own,
#       in the panel as shown whole, the program reading them.
# (xv)  a run with a stream the person writes whose element type holds no
#       position the program writes (tests/numbers; sGLP's task of 2026-10-14
#       09:46 UTC, items 2 and 3): Stream(Integer)? in reader mode, the person
#       writing 1, 2 and 3; the relay and the log one element at a time, then
#       the stream's end, the program reading their sum.
# (xvi) the social graph's people meet (sGLP's task of 2026-10-16 12:35 UTC;
#       Section 5 at sGLP eac5b3b; Appendix B, "Seeds" and "The agents are
#       numbered"): a run of fewer than 6 agents is refused; on the log of
#       (iii), seed 20260927, each menu shows the people its agent knows,
#       each list in increasing order, without repetition, without the agent,
#       the first of its sex and the second of the other; each agent's first
#       menu shows two and two; the people an agent knows never decrease; and
#       a menu asked after a yes between a and b --- the menu answered after
#       the agent's next menu answer --- shows b and each friend b had before
#       the yes, a friend's friends coming to know each other, some of them
#       met only so.
# (xvii) a list the program writes inside a question in two steps, shown once,
#       whole (tests/steps; sGLP's task of 2026-10-18 20:52 UTC, item 1, at sGLP
#       e8367a3, Section 3, Definition "Relay" (ii), and Appendix B, the person
#       process): the panel's first card written at once and its second once
#       the person has answered a question of another type, Ready ::=
#       ready(YesNo?), which the person answers about a day after asked, each
#       card about a second after shown; the log the readiness first, then
#       both cards, each in the panel as shown whole, the program reading the
#       three answers.
# (xviii) the harness of every canonical CVA platform (programs/sglp/cva/;
#       sGLP's task of 2026-10-18 20:50 UTC), on the canonical platform of the
#       social graph as programs/vglp/cva/compile.sh prints it
#       (programs/vglp/cva/social_graph/platform.vglp), compiled to GLP by the
#       REPL's :emit, with the scripted profiles of tests/cva/scripted_sglp.glp:
#       the platform's interface printed in sGLP's term form (cva/interface.sh),
#       its types, its volitional declarations, Menu's and Approval's, and its
#       compiled predicates; the profiles transformed against it
#       (transform.sh --interface); two agents for a day (cva/run.sh), two runs
#       from seed 20260927 writing one log and seed 1 another, each menu showing
#       its agent's own key and the people it knows, the scripted person asking
#       to meet the other, then proposing, and an attempt to befriend approved
#       at the other, and each element of the program's output each agent's
#       screen shows, output, a view or a notice (sGLP's task of 2026-10-20
#       08:00 UTC, items 4 and 5); the population's draw beside the log; a
#       simulation record in a scratch root (cva/record.sh), its check passing
#       on it and failing on its log or its printed population changed; and a
#       census over the platform, tests/cva/census_sglp.glp, two persons keyed
#       by the platform's keys, a host of two dimensions and a guest of one,
#       run with census for its agents (cva/run.sh), its draw, its menus showing
#       each agent's person's key, and a record of it and its check; and the
#       network with a census whose keys are not in @<'s order (tests/route;
#       sGLP #9, Code_inbox 2026-10-24 14:07 UTC): three agents keyed p2, p1
#       and p3, each sending a message to each key, every message reaching
#       the agent of its key (GLP-Spec, the transaction Cold-call).
# (xix) the screen (tests/screen; sGLP's task of 2026-10-20 08:00 UTC, items 4,
#       5 and 7): a program writing note(N1), N1 not ground until the person's
#       first count, and note(9) after it; the person answering each count with
#       the number of notes seen, seen(L): 0 before the notes are shown, the
#       first holding back the second, and 2 after; the log's output lines,
#       each note as the screen shows it, in order.
# (xx)  a census (tests/census; sGLP's task of 2026-10-20 08:00 UTC, items 1 to
#       4 and 7): the census village drawing a parent and one to three
#       children, two roles with attributes; a parent of two dimensions and a
#       child of one; each agent the census's person in its order, greeting
#       with the key and the attribute its person gives it, the parent with the
#       number of elements seen, none, the program defining no Output; the
#       draw each agent's stochastic person of its role; and the run refused
#       for a census drawing nobody and one drawing a person of a role of no
#       dimension.
#
# Prints one line per check and a summary line, "=== P passed, F failed ===";
# exits non-zero if any check fails.  The runs of (iii) and (iv) take some
# minutes.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
RUN="$HERE/social_graph/run.sh"
PASS=0
FAIL=0

check() {   # check <name> <condition result: 0 pass>
    if [ "$2" -eq 0 ]; then
        echo "  PASS  $1"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $1"
        FAIL=$((FAIL + 1))
    fi
}

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# repl <out> <line>...: the REPL on the lines given, its output in <out>.
repl() {
    local out=$1; shift
    (cd "$RT" && { printf '%s\n' "$@"; echo ':quit'; } | bin/glpc > "$out" 2>&1)
}

echo "--- (i) a token is bound only when the machine rests"
repl "$WORK/busy.out" ':limit 1000000000000' "$HERE/tests/busy" \
    'busy(100000).' 'busy_late(100000).'
grep -q '✓ Loaded program' "$WORK/busy.out"
check "tests/busy loads" $?
[ "$(grep -c '^\(GLP> \)*after$' "$WORK/busy.out")" -eq 2 ] && ! grep -q '^\(GLP> \)*before$' "$WORK/busy.out"
check "the rated goal, spawned before and after count(100000, Z), is released after it" $?

echo "--- (ii) the times between releases are exponential with the rates' sum"
repl "$WORK/law.out" ':limit 1000000000000' "$HERE/tests/law" 'law(10000, S).'
grep -q '✓ Loaded program' "$WORK/law.out"
check "tests/law loads" $?
STATS=$(grep -oE 'S = stats\([^)]*\)' "$WORK/law.out" | head -1)
echo "        $STATS; expected mean $(awk 'BEGIN { printf "%.6f", 1/4.5 }'), variance $(awk 'BEGIN { printf "%.6f", 1/4.5^2 }')"
echo "$STATS" | awk -F'[(,) ]+' '{ exit !($4 == 10000) }'
check "10000 times between releases" $?
echo "$STATS" | awk -F'[(,) ]+' '{ m = 1/4.5; d = ($5 - m) / m; if (d < 0) d = -d; exit !(d < 0.1) }'
check "their mean is within 10% of 1/4.5" $?
echo "$STATS" | awk -F'[(,) ]+' '{ v = 1/4.5^2; d = ($6 - v) / v; if (d < 0) d = -d; exit !(d < 0.1) }'
check "their variance is within 10% of 1/4.5^2" $?

echo "--- (iii) the social graph's runs are reproducible from the seed"
for r in a b s1; do
    seed=20260927; [ "$r" = s1 ] && seed=1
    bash "$RUN" 100 '30 days' "$seed" "$WORK/$r.log" "$WORK/$r.graph" > "$WORK/$r.out" 2>&1
    st=$?
    check "100 agents, 30 days, seed $seed ($r): the run ends with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/$r.out"
    [ -s "$WORK/$r.log" ]; check "100 agents, 30 days, seed $seed ($r): the log is not empty" $?
done
[ -s "$WORK/a.log" ] && cmp -s "$WORK/a.log" "$WORK/b.log"
check "two runs from seed 20260927 write byte-identical logs" $?
[ -s "$WORK/a.log" ] && [ -s "$WORK/s1.log" ] && ! cmp -s "$WORK/a.log" "$WORK/s1.log"
check "seeds 20260927 and 1 write different logs" $?

echo "--- (iv) the social graph, 100 agents for one year"
bash "$RUN" 100 '1 year' 20260927 "$WORK/y.log" "$WORK/y.graph" > "$WORK/y.out" 2>&1
st=$?
check "100 agents, 1 year, seed 20260927: the run ends with no error" $st
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/y.out"
[ -s "$WORK/y.log" ]; check "the log is not empty" $?
grep -q '^unread 0$' "$WORK/y.out" && grep -q '^menu_answers [1-9]' "$WORK/y.out" &&
    grep -q '^approval_answers [1-9]' "$WORK/y.out" && ! grep -q '^clock none$' "$WORK/y.out"
check "the log is menus, approvals and their answers, and the monitor's last line" $?
sed -n '/^wall-clock/p;/^answers/,/^unread/p' "$WORK/y.out" | sed 's/^/        /'

echo "--- (v) the mix as arguments"
# mix_shares <log>: "<menus> <other> <approvals> <yes>", the numbers of menu
# and approval answers, the share of the menus picking the other sex and the
# share of the approvals answered yes.
mix_shares() {
    awk -F'\t' '
        $3 == "Menu" { m++; if ($4 ~ /other\([^()]*\)\)$/) o++ }
        $3 == "Approval" { c++; if ($4 ~ /, yes\)$/) y++ }
        END { printf "%d %.4f %d %.4f\n", m, m ? o / m : 0, c, c ? y / c : 0 }' "$1"
}
for mix in "60 70" "0 0" "100 100"; do
    read -r hom wary <<< "$mix"
    bash "$RUN" 100 '30 days' 20260927 "$WORK/m$hom-$wary.log" '' "$hom" "$wary" \
        > "$WORK/m$hom-$wary.out" 2>&1
    st=$?
    check "100 agents, 30 days, seed 20260927, mix $hom $wary: the run ends with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/m$hom-$wary.out"
done
[ -s "$WORK/a.log" ] && cmp -s "$WORK/a.log" "$WORK/m60-70.log"
check "social_graph/6 at 60 and 70 writes the log social_graph/4 writes" $?
read -r M O C Y <<< "$(mix_shares "$WORK/m0-0.log")"
echo "        mix 0 0: $M menus, other-sex share $O; $C approvals, yes share $Y"
[ "$M" -gt 0 ] && [ "$C" -gt 0 ] && awk -v o="$O" -v y="$Y" 'BEGIN { exit !(o > 0.4 && y > 0.8) }'
check "at 0 and 0, indifferent and sociable: over 0.4 of the menus pick the other sex, over 0.8 of the approvals are answered yes" $?
read -r M O C Y <<< "$(mix_shares "$WORK/m100-100.log")"
echo "        mix 100 100: $M menus, other-sex share $O; $C approvals, yes share $Y"
[ "$M" -gt 0 ] && [ "$C" -gt 0 ] && awk -v o="$O" -v y="$Y" 'BEGIN { exit !(o < 0.1 && y < 0.3) }'
check "at 100 and 100, homophile and wary: under 0.1 of the menus pick the other sex, under 0.3 of the approvals are answered yes" $?

echo "--- (vi) friendship.awk's months"
awk -v n=100 -v months=12 -f "$HERE/social_graph/friendship.awk" "$WORK/y.log" > "$WORK/y.months" 2>&1
[ "$(grep -c '^month ' "$WORK/y.months")" -eq 12 ]
check "twelve month lines" $?
awk '
    BEGIN { m = 0; e = 0 }
    /^edges / { E = $2 } /^components / { C = $2 } /^isolated / { I = $2 } /^largest / { L = $2 }
    /^month / {
        if ($2 != ++m || $4 != m * 2629800 || $6 < e || $14 + $16 != $6) bad = 1
        e = $6; c = $8; i = $10; l = $12
    }
    END { exit !(m == 12 && !bad && e == E && c == C && i == I && l == L) }' "$WORK/y.months"
check "the edges do not decrease, are those within a sex and across, and month 12's graph is the year's" $?
M1=$(awk -F'\t' '
    $3 == "Approval" && $4 ~ /, yes\)$/ && $1 + 0 < 2629800 {
        p = substr($4, 10, index($4, ", ") - 10) + 0; a = $2 + 0
        if (a != p) print (a < p ? a " " p : p " " a)
    }' "$WORK/y.log" | sort -u | wc -l | tr -d ' ')
[ "$M1" -gt 0 ] && grep -q "^month 1 at 2629800 edges $M1 " "$WORK/y.months"
check "month 1's edges are the $M1 pairs with a yes before 2629800 s" $?
sed -n '/^month /p' "$WORK/y.months" | sed 's/^/        /'

echo "--- (vii) the population's draw"
# The draw run.sh printed for (iv), against the profiles the log of (iv) shows:
# an agent is homophile if under 0.26 of its menus pick the other sex
# (homophile 0.02, indifferent 0.5), and wary if under 0.55 of its approvals
# are answered yes (wary 0.2, sociable 0.9).
D=$(sed -n 's/^draw \(homophile [0-9]* indifferent [0-9]* wary [0-9]* sociable [0-9]*\)$/\1/p' "$WORK/y.out")
L=$(awk -F'\t' '
    NF == 4 && $3 == "Menu" { m[$2]++; if ($4 ~ /other\([^()]*\)\)$/) o[$2]++ }
    NF == 4 && $3 == "Approval" { c[$2]++; if ($4 ~ /, yes\)$/) y[$2]++ }
    END {
        for (a = 1; a <= 100; a++) {
            if (m[a] && o[a] / m[a] < 0.26) h++; else i++
            if (c[a] && y[a] / c[a] < 0.55) w++; else s++
        }
        printf "homophile %d indifferent %d wary %d sociable %d\n", h, i, w, s
    }' "$WORK/y.log")
echo "        run.sh: $D; the log: $L"
[ -n "$D" ] && [ "$D" = "$L" ]
check "the draw run.sh prints for (iv) is the profiles its log shows" $?
# draw_file_errors <draw> <n>: the number of ways the .draw file breaks its
# form: its first line not a counts line, an agent line out of 1..n's order or
# of another form, not n agent lines, the counts line not their counts.
draw_file_errors() {
    awk -v n="$2" '
        NR == 1 { if ($0 !~ /^draw homophile [0-9]+ indifferent [0-9]+ wary [0-9]+ sociable [0-9]+$/) bad++
                  ch = $3; ci = $5; cw = $7; cs = $9; next }
        $0 ~ /^agent [0-9]+ (homophile|indifferent) (wary|sociable)$/ && $2 + 0 == k + 1 {
            k++; if ($3 == "homophile") h++; else i++; if ($4 == "wary") w++; else s++; next }
        { bad++ }
        END { if (k != n || ch != h + 0 || ci != i + 0 || cw != w + 0 || cs != s + 0) bad++
              print bad + 0 }' "$1"
}
[ -s "$WORK/y.draw" ] && [ "$(draw_file_errors "$WORK/y.draw" 100)" -eq 0 ] &&
    [ "$(head -n 1 "$WORK/y.draw")" = "draw $D" ]
check "the .draw file of (iv) is the counts line run.sh prints, then agents 1..100 in order, whose counts it is" $?
# Each agent's line against its profiles as the log of (iv) shows them, by the
# shares above; an agent with no menu or no approval in the log is not shown.
A=$(awk 'FNR == NR { if ($1 == "agent") line[$2] = $3 " " $4; next }
    { split($0, f, "\t") }
    f[3] == "Menu" { m[f[2]]++; if (f[4] ~ /other\([^()]*\)\)$/) o[f[2]]++ }
    f[3] == "Approval" { c[f[2]]++; if (f[4] ~ /, yes\)$/) y[f[2]]++ }
    END {
        for (a = 1; a <= 100; a++) {
            if (!m[a] || !c[a]) { bad++; continue }
            l = (o[a] / m[a] < 0.26 ? "homophile" : "indifferent") " " (y[a] / c[a] < 0.55 ? "wary" : "sociable")
            if (l != line[a]) bad++
        }
        print bad + 0
    }' "$WORK/y.draw" "$WORK/y.log")
echo "        the .draw file of (iv): $A of 100 agents' lines not the profiles the log shows"
[ "$A" -eq 0 ]
check "each agent's line in the .draw file of (iv) is its profiles as the log of (iv) shows them" $?
grep -q '^draw homophile 0 indifferent 100 wary 0 sociable 100$' "$WORK/m0-0.out" &&
    [ "$(draw_file_errors "$WORK/m0-0.draw" 100)" -eq 0 ] &&
    [ "$(grep -c '^agent [0-9]* indifferent sociable$' "$WORK/m0-0.draw")" -eq 100 ]
check "at 0 and 0 the draw is no homophile and no wary agent, in its counts and in every agent's line" $?
grep -q '^draw homophile 100 indifferent 0 wary 100 sociable 0$' "$WORK/m100-100.out" &&
    [ "$(draw_file_errors "$WORK/m100-100.draw" 100)" -eq 0 ] &&
    [ "$(grep -c '^agent [0-9]* homophile wary$' "$WORK/m100-100.draw")" -eq 100 ]
check "at 100 and 100 the draw is every agent homophile and wary, in its counts and in every agent's line" $?

echo "--- (viii) circulation.awk on a fixed four-agent log"
awk -f "$HERE/coins/circulation.awk" "$HERE/tests/circulation/four.log" > "$WORK/four.out" 2>&1
cmp -s "$WORK/four.out" "$HERE/tests/circulation/four.expected"
st=$?
check "circulation.awk on tests/circulation/four.log prints the months and totals computed by hand" $st
[ "$st" -eq 0 ] || diff "$WORK/four.out" "$HERE/tests/circulation/four.expected" | sed 's/^/        /'

echo "--- (ix) coins among friends, four agents for a week, and the draw over a year"
# The hand-made graph tests/circulation/four.graph: 1-2, 1-3, 2-3, 3-4, so the
# friends are 1: [2, 3], 2: [1, 3], 3: [1, 2, 4], 4: [3].  coins_log_errors
# <log>: the number of the log's answers that break what the program's clauses
# and request/2 say: a menu shows the agent's friends and a wallet of positive
# holdings of friends' coins, one per issuer, and is answered pay(F, K) with K
# of 1..5 and F an issuer the wallet shows, or swap(F, 10) with F a friend and
# the wallet empty; an offer is of 10 from a friend.
coins_log_errors() {
    awk -F'\t' '
        BEGIN {
            F[1] = "[2, 3]"; F[2] = "[1, 3]"; F[3] = "[1, 2, 4]"; F[4] = "[3]"
            split("1 2 1 3 2 3 3 4", e, " ")
            for (i = 1; i < 8; i += 2) { isf[e[i], e[i + 1]] = 1; isf[e[i + 1], e[i]] = 1 }
        }
        $3 == "Menu" {
            s = $4
            if (substr(s, 1, 5) != "menu(") { bad++; next }
            s = substr(s, 6)
            i = index(s, "]"); if (substr(s, 1, i) != F[$2]) bad++
            s = substr(s, i + 3)
            i = index(s, "]"); w = substr(s, 2, i - 2); req = substr(s, i + 3)
            req = substr(req, 1, length(req) - 1)
            delete held; n = 0
            while (match(w, /holding\([0-9]+, -?[0-9]+\)/)) {
                split(substr(w, RSTART + 8, RLENGTH - 9), h, ", ")
                if (h[2] + 0 <= 0 || !((($2 + 0), (h[1] + 0)) in isf) || ((h[1] + 0) in held)) bad++
                held[h[1] + 0] = 1; n++
                w = substr(w, RSTART + RLENGTH)
            }
            if (req ~ /^pay\([0-9]+, [0-9]+\)$/) {
                split(substr(req, 5, length(req) - 5), r, ", ")
                if (!((r[1] + 0) in held) || r[2] + 0 < 1 || r[2] + 0 > 5) bad++
            } else if (req ~ /^swap\([0-9]+, [0-9]+\)$/) {
                split(substr(req, 6, length(req) - 6), r, ", ")
                if (n != 0 || !((($2 + 0), (r[1] + 0)) in isf) || r[2] + 0 != 10) bad++
            } else bad++
            next
        }
        $3 == "Offer" {
            split(substr($4, 7, length($4) - 7), o, ", ")
            if (!((($2 + 0), (o[1] + 0)) in isf) || o[2] + 0 != 10) bad++
        }
        END { print bad + 0 }' "$1"
}
COINS="$HERE/coins/run.sh"
G4="$HERE/tests/circulation/four.graph"
for r in a b s1; do
    seed=20260927; [ "$r" = s1 ] && seed=1
    bash "$COINS" 4 "$G4" '1 week' "$seed" "$WORK/c$r.log" > "$WORK/c$r.out" 2>&1
    st=$?
    check "4 agents, 1 week, seed $seed ($r): the run ends with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/c$r.out"
done
[ -s "$WORK/ca.log" ] && cmp -s "$WORK/ca.log" "$WORK/cb.log"
check "two runs from seed 20260927 write byte-identical logs" $?
[ -s "$WORK/ca.log" ] && [ -s "$WORK/cs1.log" ] && ! cmp -s "$WORK/ca.log" "$WORK/cs1.log"
check "seeds 20260927 and 1 write different logs" $?
awk '$1 == "clock" && $2 != "none" && $2 + 0 <= 604800 { ok = 1 } END { exit !ok }' "$WORK/ca.out"
check "the run ends at its clock, within the week" $?
grep -q '^unread 0$' "$WORK/ca.out" && grep -q '^unordered 0$' "$WORK/ca.out" &&
    grep -q '^unmatched 0$' "$WORK/ca.out" &&
    grep -q '^menu_answers [1-9]' "$WORK/ca.out" && grep -q '^offer_answers [1-9]' "$WORK/ca.out"
check "the log is menus and offers and their answers, in the order of time, each offer the proposal waiting at its agent, read whole by circulation.awk" $?
E=$(coins_log_errors "$WORK/ca.log")
[ "$E" -eq 0 ]
check "every menu and offer of the log is as the agent's clauses and request/2 give it ($E not)" $?
sed -n '/^wall-clock/p;/^month 1 /p;/^answers/,/^unmatched/p' "$WORK/ca.out" | sed 's/^/        /'
# The population's draw.  coins_draw_errors <draw> <n>: the number of ways the
# .draw file breaks its form: its first line not a counts line, an agent line
# out of 1..n's order or of another form, not n agent lines, the counts line
# not their counts.
coins_draw_errors() {
    awk -v n="$2" '
        NR == 1 { if ($0 !~ /^draw spender [0-9]+ saver [0-9]+ generous [0-9]+ cautious [0-9]+$/) bad++
                  csp = $3; csv = $5; cg = $7; cc = $9; next }
        $0 ~ /^agent [0-9]+ (spender|saver) (generous|cautious)$/ && $2 + 0 == k + 1 {
            k++; if ($3 == "spender") sp++; else sv++; if ($4 == "generous") g++; else c++; next }
        { bad++ }
        END { if (k != n || csp != sp + 0 || csv != sv + 0 || cg != g + 0 || cc != c + 0) bad++
              print bad + 0 }' "$1"
}
bash "$COINS" 4 "$G4" '1 year' 1 "$WORK/cy.log" > "$WORK/cy.out" 2>&1
st=$?
check "4 agents, 1 year, seed 1 (cy): the run ends with no error" $st
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cy.out"
DF=0
for r in a s1 y; do
    [ -s "$WORK/c$r.draw" ] && [ "$(coins_draw_errors "$WORK/c$r.draw" 4)" -eq 0 ] &&
        [ "$(head -n 1 "$WORK/c$r.draw")" = "$(grep '^draw spender ' "$WORK/c$r.out")" ] || DF=1
done
check "the .draw files of (a), (s1) and (cy) are the counts line run.sh prints, then agents 1..4 in order, whose counts it is" $DF
# Each agent's line of (cy) against its profiles where the log of (cy) shows
# them: its spending by its menus' rate, a spender's daily and a saver's
# weekly, against one menu every sqrt(7) days of the run's clock; its credit by
# its answers to offers with Held above 20, no cautious and yes generous.
read -r A C <<< "$(awk 'FNR == NR { if ($1 == "agent") { sp[$2] = $3; cr[$2] = $4 } next }
    { n = split($0, f, "\t") }
    n == 1 { clock = f[1] + 0 }
    n == 4 && f[3] == "Menu" { m[f[2] + 0]++ }
    n == 4 && f[3] == "Offer" {
        split(substr(f[4], 7, length(f[4]) - 7), o, ", ")
        if (o[3] + 0 > 20) { shown[f[2] + 0] = 1; if ((o[4] == "yes") != (cr[f[2] + 0] == "generous")) badcr[f[2] + 0] = 1 }
    }
    END {
        lim = clock / 86400 / sqrt(7)
        for (a = 1; a <= 4; a++) {
            if ((m[a] > lim ? "spender" : "saver") != sp[a] || (a in badcr)) bad++
            if (a in shown) c++
        }
        if (clock <= 0) bad++
        print bad + 0, c + 0
    }' "$WORK/cy.draw" "$WORK/cy.log")"
echo "        the .draw file of (cy): $A of 4 agents' lines not the profiles the log shows; the log shows the credit of $C"
[ "$A" -eq 0 ]
check "each agent's line in the .draw file of (cy) is its profiles where the log of (cy) shows them" $?
sed 's/^/        /' "$WORK/cy.draw"

echo "--- (x) the transformation"
bash "$HERE/transform.sh" --check > "$WORK/tx.out" 2>&1
st=$?
check "the printed modules are what the transformation prints from the sGLP sources" $st
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/tx.out"
# A small source: one interactive type Q ::= q(A?), the person procedure p_q
# of profile p answering it after a rated goal of ans; BAD has p's dimension at
# 0.5 and a profile r in no dimension, GOOD neither; ZERO has a dimension of p
# at 1.0 and r, sharing p's person procedure, at 0.0, and ZERO1 the same with r
# first.
V="[type('A', [a]), type('Q', [q(dual('A'))]), volitional('Q', '\'Q\'', q_w, ask(dual('A')))]"
P="person(p), binds('Q', p_q), decl(p_q(dual('Q'), dual('Person'), dual('Integer'))), clause(p_q(var('X'), var('_'), var('_')), [], [rated(ans(var('X?')), rate(1, day))]), decl(ans(dual('Q'))), clause(ans(q(a)), [], [])"
repl "$WORK/txbad.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$P, person(r), run(2, [mix(d, [share(p, 0.5)])], 1, day, 1)], profiles)."
repl "$WORK/txgood.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$P, run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)], profiles)." \
    "transform_terms($V, [$P, run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)], population)."
repl "$WORK/txzero.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$P, person(r), binds('Q', p_q), run(2, [mix(d, [share(p, 1.0), share(r, 0.0)])], 1, day, 1)], population)." \
    "transform_terms($V, [$P, person(r), binds('Q', p_q), run(2, [mix(d, [share(r, 0.0), share(p, 1.0)])], 1, day, 1)], population)."
sed 's/^\(GLP> \)*//' "$WORK/txbad.out" > "$WORK/txbad.lines"
sed 's/^\(GLP> \)*//' "$WORK/txgood.out" > "$WORK/txgood.lines"
grep -q '^%% not transformed: the probabilities of a dimension (one to nine profiles, whole percentages, summing to one)(d)$' "$WORK/txbad.lines" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txbad.lines")" -eq 1 ] &&
    ! grep -q '^p_q(' "$WORK/txbad.lines"
check "a source with a dimension not summing to one is refused with that fault alone, nothing printed, its profile in no dimension being no fault (Definitions \"Stochastic Person, Population\", \"Well-Formed\")" $?
sed 's/^\(GLP> \)*//' "$WORK/txzero.out" > "$WORK/txzero.lines"
for k in 1 2; do
    awk -v k="$k" '$0 == "%% transform begin" { n++; next } $0 == "%% transform end" { next } n == k' "$WORK/txzero.lines" | tr '\n' ' ' > "$WORK/txzero.$k.flat"
done
! grep -q '^%% not transformed' "$WORK/txzero.lines" &&
    grep -q "declaration(2, 1, day, 1, 100) \. " "$WORK/txzero.1.flat" &&
    grep -q "d(K, T1, p) :- '=<'(K?, T1?) | true \. d(K, T1, r) :- '>'(K?, T1?) | true \. " "$WORK/txzero.1.flat" &&
    grep -q "declaration(2, 1, day, 1, 0) \. " "$WORK/txzero.2.flat" &&
    grep -q "d(K, T1, r) :- '=<'(K?, T1?) | true \. d(K, T1, p) :- '>'(K?, T1?) | true \. " "$WORK/txzero.2.flat" &&
    grep -q "random(B?, 100, K1, S) , d(K1?, T1?, P1)" "$WORK/txzero.1.flat" &&
    grep -q "random(B?, 100, K1, S) , d(K1?, T1?, P1)" "$WORK/txzero.2.flat"
check "a mix with a share of 0.0 is translated, last or first, and its draw of 1..100 against the threshold 100 or 0 selects that profile for no agent (Definitions \"Stochastic Person, Population\", \"Well-Formed\")" $?
! grep -q '^%% not transformed' "$WORK/txgood.lines" &&
    grep -q '^ans(go, q(a))$' "$WORK/txgood.lines" &&
    grep -A 1 '^ans(stop, _)$' "$WORK/txgood.lines" | tr '\n' ' ' | grep -q '^ans(stop, _) \. $' &&
    grep -q '^p_q(X, _, _, Mon, _)$' "$WORK/txgood.lines" &&
    grep -q "^stream_append(rated('/'(1, day), Tok), Mon?, _)$" "$WORK/txgood.lines" &&
    grep -q '^ans(Tok?, X?)$' "$WORK/txgood.lines" &&
    grep -q '^Ask ::= ask(Constant, Question).$' "$WORK/txgood.lines" &&
    grep -A 2 '^Question$' "$WORK/txgood.lines" | tr '\n' ' ' | grep -q '^Question ::= q_w(Q) $' &&
    grep -q "^person(A, agent(p), H, Mon, Scr, Seed, Log, \\[ask('Q', q_w(E)) | As\\])$" "$WORK/txgood.lines" &&
    grep -q "^relay_1(A?, 'Q', E?, Y, Mon?, Log?)$" "$WORK/txgood.lines" &&
    grep -q '^relay_1(A, Type, q(X1?), q(Y1), Mon, Log)$' "$WORK/txgood.lines" &&
    grep -q '^random(Seed?, 2147483646, K, S)$' "$WORK/txgood.lines" &&
    grep -q '^p_q(Y?, H?, K?, Mon?, Scr?)$' "$WORK/txgood.lines" &&
    grep -q '^person(A?, agent(p), H?, Mon?, Scr?, S?, Log?, As?)$' "$WORK/txgood.lines" &&
    ! grep -q '^random(Seed?, 1, _, S)$' "$WORK/txgood.lines" &&
    grep -q '^declaration(2, 1, day, 1)$' "$WORK/txgood.lines" &&
    grep -q '^d(_, p)$' "$WORK/txgood.lines"
check "the mended source is translated: the token, its stop clause dropping the arguments, the monitor's and the screen's references, the asks, the person process's clause on an ask, its person and its seeding, the declaration and the draw" $?
# Reader mode: one interactive type Rating in reader mode, (Rating?)*rate, the
# person procedure fair_rating of profile fair writing the whole term, a
# rating of 1..5, after a rated goal of give, which receives the writer in a
# slot it reads (sGLP's paper, Section 4, at 3007ad5); and BV, a reader-mode type Bid
# with an argument the program writes, Reply?.
RV="[type('Rating', [rating('Integer')]), volitional(dual('Rating'), '\'Rating?\'', rating_r, rate('Integer'))]"
RP="person(fair), binds('Rating', fair_rating), decl(fair_rating('Rating', dual('Person'), dual('Integer'))), clause(fair_rating(var('R?'), var('_'), var('S')), [], [random(var('S?'), 5, var('K'), var('_')), rated(give(var('K?'), slot(var('R'))), rate(1, day))]), decl(give(dual('Integer'), dual('Slot'('Rating')))), clause(give(var('K'), slot(rating(var('K?')))), [], []), run(1, [mix(rater, [share(fair, 1.0)])], 30, days, 20260927)"
BV="[type('Reply', [ok, no]), type('Bid', [bid('Integer', dual('Reply'))]), volitional(dual('Bid'), '\'Bid?\'', bid_r, offer('Integer'))]"
BP="person(fair), binds('Bid', fair_bid), decl(fair_bid('Bid', dual('Person'), dual('Integer'))), clause(fair_bid(bid(1, var('_')), var('_'), var('_')), [], []), run(1, [mix(bidder, [share(fair, 1.0)])], 30, days, 1)"
repl "$WORK/txreader.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($RV, [$RP], profiles)." \
    "transform_terms($RV, [$RP], population)." \
    "transform_terms($BV, [$BP], population)."
sed 's/^\(GLP> \)*//' "$WORK/txreader.out" > "$WORK/txreader.lines"
# The lines of each part, between its begin and end lines.
awk '$0 == "%% transform begin" { k++; next } $0 == "%% transform end" { next } k == 2' "$WORK/txreader.lines" > "$WORK/txreader.pop"
awk '$0 == "%% transform begin" { k++; next } $0 == "%% transform end" { next } k == 3' "$WORK/txreader.lines" > "$WORK/txreader.bid"
! grep -q '^%% not transformed' "$WORK/txreader.pop" &&
    grep -q '^fair_rating(R?, _, S, Mon, _)$' "$WORK/txreader.lines" &&
    grep -A 8 '^fair_rating$' "$WORK/txreader.pop" | tr '\n' ' ' | grep -q '^fair_rating ( Rating , Person ? , Integer? , ' &&
    grep -A 5 "^person(A, agent(fair), H, Mon, Scr, Seed, Log, \\[ask('Rating?', rating_r(E?)) | As\\])$" "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q ' :- ground(H?) | random(Seed?, 2147483646, K, S) , $' &&
    grep -q '^fair_rating(Y, H?, K?, Mon?, Scr?)$' "$WORK/txreader.pop" &&
    grep -q "^relay_1(A?, 'Rating?', E, Y?, Mon?, Log?)$" "$WORK/txreader.pop" &&
    grep -A 19 '^relay_1$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q '^relay_1 ( Integer? , Constant? , Rating , Rating ? , MutualRef? , MutualRef? ) \. relay_1(A, Type, X?, Y, Mon, Log) :- answer_1(A?, Type?, Y?, X, Mon?, Log?) \. ' &&
    grep -q '^person(A?, agent(fair), H?, Mon?, Scr?, S?, Log?, As?)$' "$WORK/txreader.pop" &&
    grep -A 16 '^answer_1$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q '^answer_1 ( Integer? , Constant? , Rating ? , Rating , MutualRef? , MutualRef? ) . ' &&
    grep -A 5 '^answer_1(A, Type, Y, Y?, Mon, Log)$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q ' :- ground(Y?) | stream_append(time(Time), Mon?, _) , $' &&
    grep -q '^stream_append(entry(Time?, A?, Type?, Y?), Log?, _)$' "$WORK/txreader.pop" &&
    ! grep -q 'rating(rating(' "$WORK/txreader.pop"
check "in reader mode the person goal receives a writer and the program the reader: the person procedure of the dual type, its person second, the ask chosen by its type constant and taken apart to its functor only, rating_r(E?), the person ground, and the relay over its own type, relay_1(Rating, Rating?), the answer assigned from the copy and logged (Definitions \"Copy, Relay\" and \"Person Process\", sGLP a5aa59b and 0270ac6; sGLP's task of 2026-10-14 09:46 UTC, items 1 and 3)" $?
! grep -q '^%% not transformed' "$WORK/txreader.bid" &&
    grep -q "^person(A, agent(fair), H, Mon, Scr, Seed, Log, \\[ask('Bid?', bid_r(E?)) | As\\])$" "$WORK/txreader.bid" &&
    grep -q '^fair_bid(Y, H?, K?, Mon?, Scr?)$' "$WORK/txreader.bid" &&
    grep -q "^relay_1(A?, 'Bid?', E, Y?, Mon?, Log?)$" "$WORK/txreader.bid" &&
    grep -q '^answer_1(A?, Type?, Y?, X, Mon?, Log?)$' "$WORK/txreader.bid" &&
    grep -A 9 '^answer_1(A, Type, bid(Z1, Z2?), bid(Z1?, Z3), Mon, Log)$' "$WORK/txreader.bid" | tr '\n' ' ' |
        grep -q " :- ground(Z1?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, bid(Z1?, '_')), Log?, _) , present_1(A?, Type?, Z3?, Z2, Mon?, Log?) \. " &&
    [ "$(grep -c '^answer_1(A, Type, ' "$WORK/txreader.bid")" -eq 1 ] &&
    grep -A 5 '^present_1(_, _, Y, Y?, _, _)$' "$WORK/txreader.bid" | tr '\n' ' ' | grep -q ' :- ground(Y?) | true \. ' &&
    [ "$(grep -c '^present_1(' "$WORK/txreader.bid")" -eq 2 ]
check "a reader-mode type with an argument the program writes is translated: the answer handed over once the argument the person writes is ground, a fresh writer at the one the program writes, logged as _, the person goal's writer there bound to a reader the person process holds, and the program's term shown to the person goal by the present procedure of its type once ground" $?
# The relay by the moded type in writer mode and the checks' refusals (sGLP's
# task 6 of 2026-10-10 07:59 UTC, (a) and (b), Q1 and task 8 of 09:33 UTC):
# PV, a writer-mode type Post whose answer Line? holds a position the program
# writes, Status?; NV, a writer-mode type Panel whose argument the program
# writes, List(Card), holds positions the person writes, each card's YesNo?; OP,
# RP with give writing the rating as an output argument, a rated goal's
# procedure taking inputs only (sGLP's paper, Section 4, "The transformation";
# sGLP's task of 2026-10-14 09:54 UTC, item 2); CV, the mended source's
# interface naming ans/1 a predicate of the compiled program.
PV="[type('Status', [sent, read]), type('Line', [line('String', dual('Status'))]), type('Post', [post('Integer', dual('Line'))]), volitional('Post', '\'Post\'', post_w, poster('Integer'))]"
PP="person(p), binds('Post', p_post), decl(p_post(dual('Post'), dual('Person'), dual('Integer'))), clause(p_post(post(var('_'), line(hi, var('_'))), var('_'), var('_')), [], []), run(1, [mix(d, [share(p, 1.0)])], 1, day, 1)"
NV="[type('YesNo', [yes, no]), type('Card', [card('Integer', dual('YesNo'))]), type('Panel', [panel('List'('Card'))]), type('List'('X'), [[], ['X' | 'List'('X')]]), volitional('Panel', '\'Panel\'', panel_w, show('Integer'))]"
NP="person(p), binds('Panel', p_panel), decl(p_panel(dual('Panel'), dual('Person'), dual('Integer'))), clause(p_panel(var('_'), var('_'), var('_')), [], []), run(1, [mix(d, [share(p, 1.0)])], 1, day, 1)"
OP="person(fair), binds('Rating', fair_rating), decl(fair_rating('Rating', dual('Person'), dual('Integer'))), clause(fair_rating(var('R?'), var('_'), var('S')), [], [random(var('S?'), 5, var('K'), var('_')), rated(give(var('K?'), var('R')), rate(1, day))]), decl(give(dual('Integer'), 'Rating')), clause(give(var('K'), rating(var('K?'))), [], []), run(1, [mix(rater, [share(fair, 1.0)])], 30, days, 20260927)"
CV="[type('A', [a]), type('Q', [q(dual('A'))]), volitional('Q', '\'Q\'', q_w, ask(dual('A'))), compiled(ask, 2), compiled(ask1, 3), compiled(ans, 1)]"
repl "$WORK/txmoded.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($PV, [$PP], population)." \
    "transform_terms($NV, [$NP], population)." \
    "transform_terms($RV, [$OP], population)." \
    "transform_terms($CV, [$P, run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)], population)."
sed 's/^\(GLP> \)*//' "$WORK/txmoded.out" > "$WORK/txmoded.lines"
for k in 1 2 3 4; do
    awk -v k="$k" '$0 == "%% transform begin" { n++; next } $0 == "%% transform end" { next } n == k' "$WORK/txmoded.lines" > "$WORK/txmoded.$k"
done
! grep -q '^%% not transformed' "$WORK/txmoded.1" &&
    grep -q "^person(A, agent(p), H, Mon, Scr, Seed, Log, \\[ask('Post', post_w(E)) | As\\])$" "$WORK/txmoded.1" &&
    grep -q '^p_post(Y?, H?, K?, Mon?, Scr?)$' "$WORK/txmoded.1" &&
    grep -q '^relay_1(A, Type, post(X1, X2?), post(X1?, Y2), Mon, Log)$' "$WORK/txmoded.1" &&
    grep -q '^answer_1(A?, Type?, X1?, Y2?, X2, Mon?, Log?)$' "$WORK/txmoded.1" &&
    grep -A 9 '^answer_1(A, Type, X1, line(Z1, Z2?), line(Z1?, Z3), Mon, Log)$' "$WORK/txmoded.1" | tr '\n' ' ' |
        grep -q " :- ground(Z1?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, post(X1?, line(Z1?, '_'))), Log?, _) , present_1(A?, Type?, Z3?, Z2, Mon?, Log?) \. " &&
    grep -A 5 '^present_1(_, _, Y, Y?, _, _)$' "$WORK/txmoded.1" | tr '\n' ' ' | grep -q ' :- ground(Y?) | true \. '
check "in writer mode an answer holding a position the program writes is handed over once the positions the person writes are ground, a fresh writer there, and the program's term shown to the person goal by the present procedure of its type" $?
# Q1: the Panel is shown whole once the term the program writes has no reader
# (sGLP's task of 2026-10-18 20:52 UTC, item 1): the relay's one clause, its
# build of the panel and of the list of cards, each card's YesNo? answered as a
# question of its own and logged in the panel as shown whole.
tr '\n' ' ' < "$WORK/txmoded.2" > "$WORK/txmoded.2.flat"
! grep -q '^%% not transformed' "$WORK/txmoded.2" &&
    grep -q "person(A, agent(p), H, Mon, Scr, Seed, Log, \[ask('Panel', panel_w(E)) | As\]) :- ground(H?) | random(Seed?, 2147483646, K, S) , profiles # p_panel(Y?, H?, K?, Mon?, Scr?) , relay_1(A?, 'Panel', E?, Y, Mon?, Log?) , person(A?, agent(p), H?, Mon?, Scr?, S?, Log?, As?) \. " "$WORK/txmoded.2.flat" &&
    grep -q "relay_1(A, Type, X, Y?, Mon, Log) :- no_readers(X?) | present_1(A?, Type?, X?, B, S, 1, _, Fs, \[\], Mon?, Log?) , show_whole(B?, S?, Fs?, Y) \. " "$WORK/txmoded.2.flat" &&
    [ "$(grep -c '^relay_1(A, Type, ' "$WORK/txmoded.2")" -eq 1 ] && ! grep -q '^relay_1(_, ' "$WORK/txmoded.2" &&
    grep -q "present_1(_, _, panel(\[\]), panel(\[\]), panel(\[\]), C0, C0?, Fs0?, Fs0, _, _) \. " "$WORK/txmoded.2.flat" &&
    grep -q "present_1(A, Type, panel(\[card(Z1, Z2?) | Z4\]), panel(\[card(Z1?, Z3) | Z5?\]), panel(\[card(Z1?, '[$]h'(C0?, 0)) | S1?\]), C0, N2?, \[slot(F1) | G1?\], Fs0, Mon, Log) :- ground(Z1?) , integer(C0?) | ':='(N1, '+'(C0?, 1)) , answer_1(A?, Type?, C0?, 0, F1?, Z3?, Z2, Mon?, Log?) , present_2(A?, Type?, Z4?, Z5, S1, N1?, N2, G1, Fs0?, Mon?, Log?) \. " "$WORK/txmoded.2.flat" &&
    grep -q "answer_1 ( Integer? , Constant? , Integer? , Integer? , _? , YesNo ? , YesNo , MutualRef? , MutualRef? ) \. answer_1(A, Type, C, J, F, Y, Y?, Mon, Log) :- ground(Y?) , ground(F?) | stream_append(time(Time), Mon?, _) , whole_frame(F?, C?, J?, Y?, E) , stream_append(entry(Time?, A?, Type?, E?), Log?, _) \. " "$WORK/txmoded.2.flat" &&
    grep -q "present_2 ( Integer? , Constant? , List(Card) ? , List(Card) , _ , Integer? , Integer , Stream(Slot(_)) , Stream(Slot(_)) ? , MutualRef? , MutualRef? ) \. present_2(_, _, \[\], \[\], \[\], C0, C0?, Fs0?, Fs0, _, _) \. present_2(A, Type, \[card(Z1, Z2?) | Z4\], \[card(Z1?, Z3) | Z5?\], \[card(Z1?, '[$]h'(C0?, 0)) | S1?\], C0, N2?, \[slot(F1) | G1?\], Fs0, Mon, Log) :- ground(Z1?) , integer(C0?) | ':='(N1, '+'(C0?, 1)) , answer_1(A?, Type?, C0?, 0, F1?, Z3?, Z2, Mon?, Log?) , present_2(A?, Type?, Z4?, Z5, S1, N1?, N2, G1, Fs0?, Mon?, Log?) \. " "$WORK/txmoded.2.flat" &&
    ! grep -q '^present_3\|^answer_2' "$WORK/txmoded.2"
check "a writer-mode type with positions the person writes inside an argument the program writes is translated: shown whole once the term the program writes has no reader, the panel and its list of cards built and shown once complete, each card's answer a question of its own, logged in the panel as shown whole" $?
grep -q '^%% not transformed: a rated goal of a procedure with an output argument(key(give, 2))$' "$WORK/txmoded.3" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txmoded.3")" -eq 1 ] && ! grep -q '^person(' "$WORK/txmoded.3"
check "a rated goal whose procedure has an output argument is refused, nothing printed" $?
grep -q "^%% not transformed: a predicate of a profile's program that is the compiled program's(key(ans, 1))$" "$WORK/txmoded.4" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txmoded.4")" -eq 1 ] && ! grep -q '^person(' "$WORK/txmoded.4"
check "a profile whose predicate is the compiled program's is refused with that fault, nothing printed" $?

# The census, roles, the person and the screen (sGLP's task of 2026-10-20
# 08:00 UTC, items 1 to 4; sGLP's paper at 0270ac6, Section 3, Definitions
# "Person, Census", "Dual, Person Procedure, Person Declaration", "Profile,
# Dimension", "Well-Formed" and "Screen, Seen"): CW, a census w drawing a
# person of the role r1 and one of the role r2, with its Person; P1 and P2,
# profiles of a dimension of r1 and of r2, P2 sharing P1's person procedure;
# and the refusals: a dimension of a role not covering the interactive types,
# a census undeclared, a person procedure without the person, a platform
# defining Person or seen, a profile defining seen, and a Person not of the
# terms person(K, Role, V).
CW="type('Person', [person('Constant', 'Constant', 'List'('Constant'))]), decl(w(dual('Integer'), 'List'('Person'))), clause(w(var('_'), [person(k1, r1, []), person(k2, r2, [])]), [], [])"
P1="person(p1), binds('Q', p1_q), decl(p1_q(dual('Q'), dual('Person'), dual('Integer'))), clause(p1_q(var('X'), var('_'), var('_')), [], [rated(ans(var('X?')), rate(1, day))]), decl(ans(dual('Q'))), clause(ans(q(a)), [], [])"
P2="person(p2), binds('Q', p1_q)"
RUN2="run(w, [mix(d1, r1, [share(p1, 1.0)]), mix(d2, r2, [share(p2, 1.0)])], 1, day, 1)"
OLDP="person(p), binds('Q', p_q), decl(p_q(dual('Q'), dual('Integer'))), clause(p_q(var('X'), var('_')), [], [rated(ans(var('X?')), rate(1, day))]), decl(ans(dual('Q'))), clause(ans(q(a)), [], [])"
VP="[type('A', [a]), type('Q', [q(dual('A'))]), type('Person', [person('Constant', 'Constant', 'List'('Constant'))]), volitional('Q', '\'Q\'', q_w, ask(dual('A')))]"
VS="[type('A', [a]), type('Q', [q(dual('A'))]), volitional('Q', '\'Q\'', q_w, ask(dual('A'))), compiled(seen, 1)]"
RUN1="run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)"
repl "$WORK/txcensus.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$CW, $P1, $P2, $RUN2], profiles)." \
    "transform_terms($V, [$CW, $P1, $P2, $RUN2], population)." \
    "transform_terms($V, [$CW, $P1, person(p3), run(w, [mix(d1, r1, [share(p1, 1.0)]), mix(d2, r2, [share(p3, 1.0)])], 1, day, 1)], population)." \
    "transform_terms($V, [$P1, run(v, [mix(d1, [share(p1, 1.0)])], 1, day, 1)], population)." \
    "transform_terms($V, [$OLDP, $RUN1], population)." \
    "transform_terms($VP, [$P, $RUN1], population)." \
    "transform_terms($VS, [$P, $RUN1], population)." \
    "transform_terms($V, [$P, decl(seen(dual('Integer'))), clause(seen(1), [], []), $RUN1], population)." \
    "transform_terms($V, [type('Person', [someone('Constant')]), $P, $RUN1], population)."
sed 's/^\(GLP> \)*//' "$WORK/txcensus.out" > "$WORK/txcensus.lines"
for k in 1 2 3 4 5 6 7 8 9; do
    awk -v k="$k" '$0 == "%% transform begin" { n++; next } $0 == "%% transform end" { next } n == k' "$WORK/txcensus.lines" > "$WORK/txcensus.$k"
done
tr '\n' ' ' < "$WORK/txcensus.1" > "$WORK/txcensus.1.flat"
tr '\n' ' ' < "$WORK/txcensus.2" > "$WORK/txcensus.2.flat"
! grep -q '^%% not transformed' "$WORK/txcensus.1" "$WORK/txcensus.2" &&
    grep -q "Person ::= person(Constant, Constant, List(Constant)) \. exported procedure w ( Integer? , List(Person) ) \. w(_, \[person(k1, r1, \[\]), person(k2, r2, \[\])\]) \. " "$WORK/txcensus.1.flat" &&
    ! grep -q "the source defines no Person" "$WORK/txcensus.1" &&
    grep -q "exported procedure p1_q ( Q ? , Person ? , Integer? , MutualRef? , MutualRef? ) \. p1_q(X, _, _, Mon, _) :- stream_append(rated('/'(1, day), Tok), Mon?, _) , ans(Tok?, X?) \. " "$WORK/txcensus.1.flat" &&
    grep -q "Profiles ::= r1(Dimension1) ; r2(Dimension2) \. " "$WORK/txcensus.2.flat" &&
    grep -q "imported procedure profiles # w ( Integer? , List(Person) ) \. " "$WORK/txcensus.2.flat" &&
    grep -q "person(A, r1(p1), H, Mon, Scr, Seed, Log, \[ask('Q', q_w(E)) | As\]) :- ground(H?) | random(Seed?, 2147483646, K, S) , profiles # p1_q(Y?, H?, K?, Mon?, Scr?) , " "$WORK/txcensus.2.flat" &&
    grep -q "person(A, r2(p2), H, Mon, Scr, Seed, Log, \[ask('Q', q_w(E)) | As\]) :- ground(H?) | random(Seed?, 2147483646, K, S) , profiles # p1_q(Y?, H?, K?, Mon?, Scr?) , " "$WORK/txcensus.2.flat" &&
    grep -q "declaration(w, 1, day, 1) \. " "$WORK/txcensus.2.flat" &&
    grep -q "persons(B, _, Ok?, Ps?, B1?) :- integer(B?) | profiles # w(B?, Ps0) , checked(Ps0?, Ok, Ps) , skip(4096, B?, B1) \. " "$WORK/txcensus.2.flat" &&
    grep -q "roles_ok(\[person(_, r1, _) | Ps\], Ok?) :- roles_ok(Ps?, Ok) \. roles_ok(\[person(_, r2, _) | Ps\], Ok?) :- roles_ok(Ps?, Ok) \. roles_ok(\[_|_\], no) :- otherwise | true\." "$WORK/txcensus.2.flat" &&
    grep -q "stochastic_person(B, person(_, r1, _), r1(P1?), S?) :- random(B?, 100, K1, S) , d1(K1?, P1) \. stochastic_person(B, person(_, r2, _), r2(P1?), S?) :- random(B?, 100, K1, S) , d2(K1?, P1) \. " "$WORK/txcensus.2.flat" &&
    grep -q "screen(A, \[E|O\], Os, Rs, Mon, Log) :- ground(E?) | stream_append(time(Time), Mon?, _), stream_append(entry(Time?, A?, output, E?), Log?, _), screen(A?, O?, \[E?|Os?\], Rs?, Mon?, Log?)\. screen(A, O, Os, \[seen(Os?)|Rs\], Mon, Log) :- ground(Os?) | screen(A?, O?, Os?, Rs?, Mon?, Log?)\. screen(_, \[\], _, \[\], _, _)\." "$WORK/txcensus.2.flat"
check "a census of two roles is translated: the census exported from the profiles module and called by the persons of the run, which the run refuses where a person is of a role of no dimension; the stochastic person of each role, its functor the role, one profile per dimension of its role; a person procedure shared by two profiles; the person process giving each person goal its person; and the screen (Definitions \"Person, Census\", \"Stochastic Person, Population\", \"Screen, Seen\")" $?
grep -q "^%% not transformed: the types of the dimensions of a role not disjoint or not covering the interactive types(r2)$" "$WORK/txcensus.3" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.3")" -eq 1 ] && ! grep -q '^person(' "$WORK/txcensus.3"
check "a role whose dimensions do not cover the interactive types is refused, naming the role, nothing printed (Definition \"Profile, Dimension\": disjoint and covering for each role)" $?
grep -q "^%% not transformed: a census undeclared, not declared procedure W(Integer?, List(Person)), or holding a rated goal or reaching the screen(v)$" "$WORK/txcensus.4" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.4")" -eq 1 ]
check "a run declaration naming a census the source does not declare is refused (Definition \"Person, Census\")" $?
grep -q "^%% not transformed: a person procedure undeclared or not of the type its declaration requires(p_q)$" "$WORK/txcensus.5" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.5")" -eq 1 ]
check "a person procedure declared without the person, p(T°, Integer?), is refused (Definition \"Dual, Person Procedure, Person Declaration\": p(T°, Person?, Integer?))" $?
grep -q "^%% not transformed: a platform that defines screen, seen, Seen or Person$" "$WORK/txcensus.6" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.6")" -eq 1 ] &&
    grep -q "^%% not transformed: a platform that defines screen, seen, Seen or Person$" "$WORK/txcensus.7" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.7")" -eq 1 ] &&
    grep -q "^%% not transformed: a profile or the census that defines screen, seen or Seen$" "$WORK/txcensus.8" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.8")" -eq 1 ]
check "the reserved names: a platform defining the type Person or the procedure seen, and a profile defining seen, are refused (Definition \"Well-Formed\")" $?
grep -q "^%% not transformed: a Person of the source not of the terms person(Key, Role, Attributes)$" "$WORK/txcensus.9" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txcensus.9")" -eq 1 ]
check "a Person the source defines not of the terms person(K, Role, V) is refused (Definition \"Person, Census\")" $?
# The screen in the printed profiles of tests/screen (test (xix)): seen(L) a
# request on the screen's reference, which the person procedure and the
# procedure calling seen take.
tr '\n' ' ' < "$HERE/tests/screen/profiles.glp" > "$WORK/screenprof.flat"
grep -q "exported procedure look_count ( Count ? , Person ? , Integer? , MutualRef? , MutualRef? ) \. look_count(C, _, _, Mon, Scr) :- stream_append(rated('/'(1, second), Tok), Mon?, _) , look(Tok?, C?, Scr?) \. " "$WORK/screenprof.flat" &&
    grep -q "procedure look ( Token? , Count ? , MutualRef? ) \. look(stop, _, _) \. look(go, count(N?), Scr) :- stream_append(seen(L), Scr?, _) , length(L?, N) \. " "$WORK/screenprof.flat"
check "a goal seen(L) becomes the request seen(L) on the screen's reference, which the person procedure and each procedure reaching the screen take, after the monitor's (Section 4, \"The transformation\")" $?

echo "--- (xi) a run with an interactive type in reader mode"
# tests/reader: the run declaration's run of one agent asking five ratings
# (rates/3), its person process handing over each rating the person goal
# writes; the population printed before the run by transform.sh, which (x)
# holds it to, the program holding no call of the transformation.
repl "$WORK/reader.out" ':limit 1000000000000' "$HERE/tests/reader" 'reader_run(5, Ns).'
sed 's/^\(GLP> \)*//' "$WORK/reader.out" > "$WORK/reader.lines"
grep -q '^✓ Loaded program' "$WORK/reader.out"
check "tests/reader loads, its population printed before the run" $?
awk '
    /^entry\(/ {
        n++
        if ($0 !~ /^entry\([0-9.]+, 1, Rating\?, rating\([1-5]\)\)$/) bad++
        t = substr($0, 7, index($0, ",") - 7) + 0; if (t < last) bad++; last = t
        r = substr($0, index($0, "rating(") + 7, 1); rs[r]++
    }
    /^Ns = \[/ { s = $0; gsub(/[^0-9,]/, "", s); m = split(s, a, ","); for (i = 1; i <= m; i++) ns[a[i]]++ }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        for (r in rs) if (rs[r] != ns[r]) bad++
        for (r in ns) if (rs[r] != ns[r]) bad++
        exit !(n == 5 && m == 5 && seen && c == last && !bad)
    }' "$WORK/reader.lines"
check "five answers logged in the order of time, each a rating of 1..5 of type Rating?, the program reading those five, and the clock last" $?
grep '^entry(\|^clock(\|^Ns = ' "$WORK/reader.lines" | sed 's/^/        /'

echo "--- (xii) a run with the delivery status of vGLP's chat"
# tests/delivery: the person's Stream(Msg)?, Msg ::= msg(String, Status?), the
# person writing three messages' texts, each once the last one's status is
# written, and the program writing each status, sent, delivered or read; each
# message handed over once its text is ground, a fresh writer at its status
# whose reader the person process keeps and shows the person goal once the
# program has written it (sGLP's task 8 of 2026-10-10 09:33 UTC), and the
# stream's tail left open, the next answer; the log showing each message with
# its status as a variable.
D="$HERE/tests/delivery/population.glp"
tr '\n' ' ' < "$D" | grep -q "answer_1(A, Type, \[\], \[\], Mon, Log) :- stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, \[\]), Log?, _) \." &&
    tr '\n' ' ' < "$D" | grep -q "answer_1(A, Type, \[msg(Z1, Z2?) | Z4\], \[msg(Z1?, Z3) | Z5?\], Mon, Log) :- ground(Z1?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, \[msg(Z1?, '_') | '_'\]), Log?, _) , present_1(A?, Type?, Z3?, Z2, Mon?, Log?) , answer_1(A?, Type?, Z4?, Z5, Mon?, Log?) \." &&
    tr '\n' ' ' < "$D" | grep -q "present_1(_, _, Y, Y?, _, _) :- ground(Y?) | true \." &&
    tr '\n' ' ' < "$D" | grep -q "person(A, agent(talker), H, Mon, Scr, Seed, Log, \[ask('Stream(Msg)?', stream_msg_r(E?)) | As\]) :- ground(H?) | random(Seed?, 2147483646, K, S) , profiles # talk(Y, H?, K?, Mon?, Scr?) , relay_1(A?, 'Stream(Msg)?', E, Y?, Mon?, Log?) ," &&
    tr '\n' ' ' < "$D" | grep -q "relay_1(A, Type, X?, Y, Mon, Log) :- answer_1(A?, Type?, Y?, X, Mon?, Log?) \."
check "the printed relay: each element once its text is ground, a fresh writer at its status, whose reader the person process keeps and shows the person goal once ground, the tail the next answer, and the stream's end" $?
repl "$WORK/delivery.out" ':limit 1000000000000' "$HERE/tests/delivery" 'delivery_run(Ts).'
sed 's/^\(GLP> \)*//' "$WORK/delivery.out" > "$WORK/delivery.lines"
grep -q '^✓ Loaded program' "$WORK/delivery.out"
check "tests/delivery loads, its population printed before the run" $?
awk '
    /^entry\(/ { e[++n] = $0; t = substr($0, 7, index($0, ",") - 7) + 0; if (t < last) bad++; last = t }
    /^Ts = \[hello, there, bye\]$/ { ts = 1 }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        if (e[1] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(hello, _\) \| _\]\)$/) bad++
        if (e[2] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(there, _\) \| _\]\)$/) bad++
        if (e[3] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(bye, _\) \| _\]\)$/) bad++
        if (e[4] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[\]\)$/) bad++
        exit !(n == 4 && ts && seen && c == last && !bad)
    }' "$WORK/delivery.lines"
check "the log: each message with its status as a variable, then the stream's end, in the order of time; the program reads the three texts; the person, waiting on each status, writes all three; the clock last" $?
grep '^entry(\|^clock(\|^Ts = ' "$WORK/delivery.lines" | sed 's/^/        /'

echo "--- (xiii) a run with a question the program puts inside an answer"
# tests/receipt: test (xii)'s delivery status with Status ::= sent ;
# read(Ack?), the program putting inside the person's answer, where it writes
# read, a question of the person's, the acknowledgement of a read receipt
# (sGLP's task 8 of 2026-10-10 09:33 UTC; Section 4, "The person process":
# "Every term crossing between the program and the person goal, in either
# direction and at every depth, crosses the person process so, a question the
# program puts inside an answer included"): the printed relay, the status
# shown to the person goal by the present procedure of Status, the question
# inside it a fresh writer whose reader the person process keeps, and the
# acknowledgement handed to the program and logged in the status as shown;
# and the run, the log showing the acknowledgement and the program reading it.
R="$HERE/tests/receipt/population.glp"
tr '\n' ' ' < "$R" > "$WORK/receipt.flat"
grep -q "answer_1(A, Type, \[msg(Z1, Z2?) | Z4\], \[msg(Z1?, Z3) | Z5?\], Mon, Log) :- ground(Z1?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, \[msg(Z1?, '_') | '_'\]), Log?, _) , present_1(A?, Type?, Z3?, Z2, Mon?, Log?) , answer_1(A?, Type?, Z4?, Z5, Mon?, Log?) \." "$WORK/receipt.flat" &&
    grep -q "present_1(_, _, sent, sent, _, _) \. present_1(A, Type, read(Z1?), read(Z2), Mon, Log) :- answer_2(A?, Type?, Z2?, Z1, Mon?, Log?) \." "$WORK/receipt.flat" &&
    grep -q "answer_2(A, Type, Y, Y?, Mon, Log) :- ground(Y?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, read(Y?)), Log?, _) \." "$WORK/receipt.flat"
check "the printed relay: the status shown to the person goal by the present procedure of Status, the question inside it a fresh writer whose reader the person process keeps, the acknowledgement handed over and logged in the status as shown" $?
repl "$WORK/receipt.out" ':limit 1000000000000' "$HERE/tests/receipt" 'receipt_run(Rs).'
sed 's/^\(GLP> \)*//' "$WORK/receipt.out" > "$WORK/receipt.lines"
grep -q '^✓ Loaded program' "$WORK/receipt.out"
check "tests/receipt loads, its population printed before the run" $?
awk '
    /^entry\(/ { e[++n] = $0; t = substr($0, 7, index($0, ",") - 7) + 0; if (t < last) bad++; last = t }
    /^Rs = \[plain\(hello\), acked\(there, ack\), plain\(bye\)\]$/ { rs = 1 }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        if (e[1] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(hello, _\) \| _\]\)$/) bad++
        if (e[2] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(there, _\) \| _\]\)$/) bad++
        if (e[3] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, read\(ack\)\)$/) bad++
        if (e[4] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[msg\(bye, _\) \| _\]\)$/) bad++
        if (e[5] !~ /^entry\([0-9.]+, 1, Stream\(Msg\)\?, \[\]\)$/) bad++
        exit !(n == 5 && rs && seen && c == last && !bad)
    }' "$WORK/receipt.lines"
check "the log: the three messages, the acknowledgement of the one read, read(ack), after it and before the next, then the stream's end, in the order of time; the program reads the acknowledgement into its receipt; the clock last" $?
grep '^entry(\|^clock(\|^Rs = ' "$WORK/receipt.lines" | sed 's/^/        /'

echo "--- (xiv) a run with positions the person writes inside an argument the program writes"
# tests/panel: Panel ::= panel(List(Card)), Card ::= card(Integer, YesNo?), in
# writer mode (sGLP's Q1 of 2026-10-10 09:33 UTC; Section 4, "The person
# process": the person process "shows the person goal the question once the
# positions the program writes are ground, the positions the person writes
# inside them left open, each answered as a question of its own"; Section 3,
# Definition "Relay" (ii), at sGLP e8367a3): a panel of three cards, the person
# answering each; the log one line per card, each card's answer in the panel
# as shown whole, '_' at the other cards' answers; the program reading the
# three answers.
repl "$WORK/panel.out" ':limit 1000000000000' "$HERE/tests/panel" 'panel_run(3, As).'
sed 's/^\(GLP> \)*//' "$WORK/panel.out" > "$WORK/panel.lines"
grep -q '^✓ Loaded program' "$WORK/panel.out"
check "tests/panel loads, its population printed before the run" $?
awk '
    /^entry\(/ {
        n++; t = substr($0, 7, index($0, ",") - 7) + 0; if (t < last) bad++; last = t
        if ($0 !~ /^entry\([0-9.]+, 1, Panel, panel\(\[card\(1, [a-z_]+\), card\(2, [a-z_]+\), card\(3, [a-z_]+\)\]\)\)$/) { bad++; next }
        k = 0
        for (i = 1; i <= 3; i++) {
            p = "card\\(" i ", (yes|no)\\)"
            if (match($0, p)) { if (k) bad++; k = i; v[i] = substr($0, RSTART + 8, RLENGTH - 9) }
        }
        if (!k) bad++; else got[k]++
    }
    /^As = \[/ { s = $0; sub(/^As = \[/, "", s); sub(/\]$/, "", s); m = split(s, as, ", ") }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        for (i = 1; i <= 3; i++) if (got[i] != 1 || v[i] != as[i]) bad++
        exit !(n == 3 && m == 3 && seen && c == last && !bad)
    }' "$WORK/panel.lines"
check "the log: each card answered as a question of its own, in the panel as shown whole, the other cards' answers _, in the order of time; the program reads the three answers logged; the clock last" $?
grep '^entry(\|^clock(\|^As = ' "$WORK/panel.lines" | sed 's/^/        /'

echo "--- (xv) a run with a stream the person writes, its element type holding no position the program writes"
# tests/numbers: the person's Stream(Integer)? in reader mode, the person
# writing 1, 2 and 3 and closing the stream; the relay hands it over one
# element at a time, [E | Z?], Z the next question, though Integer holds no
# position the program writes (Definition "Copy, Relay"; sGLP's task of
# 2026-10-14 09:46 UTC, items 2 and 3), where it was waited on with ground
# and handed over whole.
D="$HERE/tests/numbers/population.glp"
tr '\n' ' ' < "$D" | grep -q "answer_1(A, Type, \[\], \[\], Mon, Log) :- stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, \[\]), Log?, _) \." &&
    tr '\n' ' ' < "$D" | grep -q "answer_1(A, Type, \[Z1 | Z2\], \[Z1? | Z3?\], Mon, Log) :- ground(Z1?) | stream_append(time(Time), Mon?, _) , stream_append(entry(Time?, A?, Type?, \[Z1? | '_'\]), Log?, _) , answer_1(A?, Type?, Z2?, Z3, Mon?, Log?) \." &&
    ! tr '\n' ' ' < "$D" | grep -q "answer_1(A, Type, Y, Y?, Mon, Log)"
check "the printed relay: a Stream(Integer) the person writes handed over one element at a time, the tail the next answer, and the stream's end" $?
repl "$WORK/numbers.out" ':limit 1000000000000' "$HERE/tests/numbers" 'numbers_run(S).'
sed 's/^\(GLP> \)*//' "$WORK/numbers.out" > "$WORK/numbers.lines"
grep -q '^✓ Loaded program' "$WORK/numbers.out"
check "tests/numbers loads, its population printed before the run" $?
awk '
    /^entry\(/ { e[++n] = $0; t = substr($0, 7, index($0, ",") - 7) + 0; if (t < last) bad++; last = t }
    /^S = 6$/ { sum = 1 }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        for (i = 1; i <= 3; i++) if (e[i] !~ ("^entry\\([0-9.]+, 1, Stream\\(Integer\\)\\?, \\[" i " \\| _\\]\\)$")) bad++
        if (e[4] !~ /^entry\([0-9.]+, 1, Stream\(Integer\)\?, \[\]\)$/) bad++
        exit !(n == 4 && sum && seen && c == last && !bad)
    }' "$WORK/numbers.lines"
check "the log: each integer as an element of its own, then the stream's end, in the order of time; the program reads their sum, 6; the clock last" $?
grep '^entry(\|^clock(\|^S = ' "$WORK/numbers.lines" | sed 's/^/        /'

echo "--- (xvi) the social graph's people meet through their friends"
bash "$RUN" 4 '1 day' 1 "$WORK/four.log" > "$WORK/four.out" 2>&1
st=$?
[ "$st" -ne 0 ] && ! grep -q '^entry\|Menu' "$WORK/four.log" 2>/dev/null
check "a run of 4 agents is refused, the harness requiring at least 6 (Appendix B)" $?
awk -F'\t' -v n=100 '
    function sex(x) { return x <= n / 2 ? 1 : 2 }
    function list(str, arr, a, want,   k, p, i, prev) {
        k = (str == "") ? 0 : split(str, p, ", ")
        prev = 0
        for (i = 1; i <= k; i++) {
            x = p[i] + 0; arr[x] = 1
            if (x <= prev || x == a || sex(x) != want) bad++
            prev = x
        }
        return k
    }
    NF == 4 && $3 == "Menu" {
        a = $2 + 0; t = $1 + 0
        match($4, /^menu\(\[[^]]*\]/); s1 = substr($4, 7, RLENGTH - 7)
        rest = substr($4, RLENGTH + 3); match(rest, /^\[[^]]*\]/); s2 = substr(rest, 2, RLENGTH - 2)
        split("", K)
        k1 = list(s1, K, a, sex(a)); k2 = list(s2, K, a, 3 - sex(a))
        m[a]++
        if (m[a] == 1) {
            if (k1 != 2 || k2 != 2) bad++
            for (x in K) first[a, x] = 1
            askt = 0
        } else askt = last[a]
        last[a] = t
        for (key in prevk) { split(key, q, SUBSEP); if (q[1] == a) { if (!(q[2] in K)) shrink++; delete prevk[key] } }
        for (x in K) prevk[a, x] = 1
        for (j = 1; j <= nr[a]; j++) if (rt[a, j] < askt) {
            nq = split(rs[a, j], q, " ")
            for (i = 1; i <= nq; i++) {
                if (!(q[i] in K)) miss++
                else if (i > 1 && !((a, q[i]) in first) && !((a, q[i]) in counted)) { met++; counted[a, q[i]] = 1 }
            }
            checked++
        }
    }
    NF == 4 && $3 == "Approval" && $4 ~ /, yes\)$/ {
        a = $2 + 0; t = $1 + 0
        b = substr($4, 10, index($4, ",") - 10) + 0
        ra = b; for (x in F) { split(x, q, SUBSEP); if (q[1] == b && q[2] != a) ra = ra " " q[2] }
        rb = a; for (x in F) { split(x, q, SUBSEP); if (q[1] == a && q[2] != b) rb = rb " " q[2] }
        nr[a]++; rt[a, nr[a]] = t; rs[a, nr[a]] = ra
        nr[b]++; rt[b, nr[b]] = t; rs[b, nr[b]] = rb
        F[a, b] = 1; F[b, a] = 1
    }
    END {
        printf "        menus checked against a yes before them %d, people missing %d, met through a friend %d, lists out of order or of the wrong sex %d, lists shrunk %d\n", checked, miss, met, bad, shrink
        exit !(checked > 0 && miss == 0 && met > 0 && bad == 0 && shrink == 0)
    }' "$WORK/a.log"
check "each menu shows the people its agent knows, in increasing order and by sex, two and two at the start, never fewer later; a menu asked after a yes shows the friend and the friend's friends, some met only so (Section 5)" $?

echo "--- (xvii) a list the program writes in two steps, shown once, whole"
# tests/steps: the panel's first card written at once and its second once the
# person has answered Ready, which the profile does about a day after asked,
# each card about a second after shown (sGLP's task of 2026-10-18 20:52 UTC,
# item 1): the printed relays, Ready's flat and the Panel's whole; and the run,
# the readiness answered first and each card after it, in the panel as shown
# whole, the program reading the three answers.  Shown a card at a time, the
# first card would be answered seconds after the start and a day before the
# readiness, in the panel as [card(1, yes) | _].
D="$HERE/tests/steps/population.glp"
tr '\n' ' ' < "$D" > "$WORK/steps.flat"
grep -q "relay_1(A, Type, ready(X1?), ready(Y1), Mon, Log) :- answer_1(A?, Type?, Y1?, X1, Mon?, Log?) \." "$WORK/steps.flat" &&
    grep -q "relay_2(A, Type, X, Y?, Mon, Log) :- no_readers(X?) | present_1(A?, Type?, X?, B, S, 1, _, Fs, \[\], Mon?, Log?) , show_whole(B?, S?, Fs?, Y) \." "$WORK/steps.flat" &&
    grep -q "answer_2(A, Type, C, J, F, Y, Y?, Mon, Log) :- ground(Y?) , ground(F?) | stream_append(time(Time), Mon?, _) , whole_frame(F?, C?, J?, Y?, E) , stream_append(entry(Time?, A?, Type?, E?), Log?, _) \." "$WORK/steps.flat"
check "the printed relays: Ready's shown at once, the Panel's shown whole once the term the program writes has no reader, each card's answer logged in the panel as shown whole" $?
repl "$WORK/steps.out" ':limit 1000000000000' "$HERE/tests/steps" 'steps_run(As).'
sed 's/^\(GLP> \)*//' "$WORK/steps.out" > "$WORK/steps.lines"
grep -q '^✓ Loaded program' "$WORK/steps.out"
check "tests/steps loads, its population printed before the run" $?
awk '
    /^entry\(/ { e[++n] = $0; t[n] = substr($0, 7, index($0, ",") - 7) + 0 }
    /^As = \[yes, yes, yes\]$/ { as = 1 }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        if (e[1] !~ /^entry\([0-9.]+, 1, Ready, ready\(yes\)\)$/) bad++
        if (e[2] !~ /^entry\([0-9.]+, 1, Panel, panel\(\[card\(1, yes\), card\(2, _\)\]\)\)$/) bad++
        if (e[3] !~ /^entry\([0-9.]+, 1, Panel, panel\(\[card\(1, _\), card\(2, yes\)\]\)\)$/) bad++
        if (!(t[1] > 3600 && t[1] <= t[2] && t[2] <= t[3] && t[3] - t[1] < 3600)) bad++
        exit !(n == 3 && as && seen && c == t[3] && !bad)
    }' "$WORK/steps.lines"
check "the log: the readiness, then the first card, shown with the second only once it was written, after the readiness, then the second, each in the panel as shown whole; the program reads the three answers; the clock last" $?
grep '^entry(\|^clock(\|^As = ' "$WORK/steps.lines" | sed 's/^/        /'

echo "--- (xviii) the harness of every canonical CVA platform"
# The horizon's unit: one of the paper's six, singular or plural (sGLP's
# paper, Appendix B); run.sh and record.sh refuse any other, month among them,
# with exit 2 and the six named, before they read a file or run anything.
bash "$HERE/cva/run.sh" "$WORK/none.glp" "$WORK/none" 2 "1 month" 1 "$WORK/none.log" > "$WORK/unit_run.out" 2>&1
st=$?
bash "$HERE/cva/record.sh" CSSN "$HERE/cva/run.sh" --cascade "$WORK/none" --until "1 month" --root "$WORK/unit_root" > "$WORK/unit_rec.out" 2>&1
sr=$?
[ "$st" -eq 2 ] && grep -q '^run.sh: the unit of <until> is second, minute, hour, day, week or year, singular or plural, not month$' "$WORK/unit_run.out" &&
    [ "$sr" -eq 2 ] && grep -q '^record.sh: the unit of --until is second, minute, hour, day, week or year, singular or plural, not month$' "$WORK/unit_rec.out" &&
    [ ! -e "$WORK/unit_root" ]
check "a horizon in months is refused by run.sh and by record.sh with exit 2, naming the six units, and nothing made (sGLP's paper, Appendix B)" $?
# The canonical platform of the social graph, as programs/vglp/cva/compile.sh
# prints it, compiled to GLP by :emit in a scratch directory under
# programs/tests, a container with no self.glp, as cascade.sh compiles it.
CVA="$WORK/cva"; mkdir -p "$CVA/printed"
EMITD="$GLP_DIR/programs/tests/sglp_cva_emit_$$"
rm -rf "$EMITD"; mkdir -p "$EMITD"
cp "$GLP_DIR/programs/vglp/cva/social_graph/platform.vglp" "$EMITD/platform.vglp"
repl "$WORK/cva_emit.out" ":emit $EMITD"
cp "$EMITD/platform.vglp" "$CVA/platform.vglp"; cp "$EMITD/platform.glp" "$CVA/platform.glp" 2>/dev/null
rm -rf "$EMITD"
[ -s "$CVA/platform.glp" ] && grep -q '^exported procedure agent(Peer?, Stream(Net)?, Stream(Net), Stream(Output), Stream(Ask(Question)))\.$' "$CVA/platform.glp"
check "the canonical platform of the social graph compiled to GLP, its agent/5 the compilation's, with its ask stream" $?
bash "$HERE/cva/interface.sh" "$CVA/platform.vglp" "$CVA/platform.glp" GSG > "$CVA/interface.glp" 2> "$WORK/cva_iface.err"
st=$?
[ "$st" -eq 0 ] &&
    grep -q "^  type('Menu', \[menu('Key', 'List'('Key'), dual('Request'))\]),$" "$CVA/interface.glp" &&
    grep -q "^  type('Approval', \[approval('Attempt', dual('YesNo'))\]),$" "$CVA/interface.glp" &&
    grep -q "^  type('Key', \['String'\]),$" "$CVA/interface.glp" &&
    grep -q "^  type('List'('X'), \[\[\], \['X' | 'List'('X')\]\]),$" "$CVA/interface.glp" &&
    grep -q "^  volitional('Menu', '\\\\'Menu\\\\'', menu_w, request(dual('Key'), dual('List'('Key')), 'Stream'('Trigger'))),$" "$CVA/interface.glp" &&
    grep -q "^  volitional('Approval', '\\\\'Approval\\\\'', approval_w, respond(" "$CVA/interface.glp" &&
    grep -q "^  compiled(agent, 5),$" "$CVA/interface.glp" && grep -q "^  compiled(request1, 5),$" "$CVA/interface.glp" &&
    [ "$(grep -c '^  type(' "$CVA/interface.glp")" -ge "$(grep -cE '^[A-Z][A-Za-z0-9_]* *::=' "$CVA/platform.vglp")" ]
check "the platform's interface in sGLP's term form: its types, Menu and Approval among them, and the root's it names, Key and List, its volitional declarations with the constants and functors of their asks, and its compiled predicates" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_iface.err"
bash "$HERE/transform.sh" --interface "$CVA/interface.glp" --source "$HERE/tests/cva/scripted_sglp.glp" --out "$CVA/printed" > "$WORK/cva_tx.out" 2>&1
st=$?
tr '\n' ' ' < "$CVA/printed/population.glp" > "$WORK/cva_pop.flat"
[ "$st" -eq 0 ] && ! grep -q '^%% not transformed' "$CVA/printed/population.glp" "$CVA/printed/profiles.glp" &&
    grep -q "relay_1(A, Type, menu(X1, X2, X3?), menu(X1?, X2?, Y3), Mon, Log) :- ground(X1?) , ground(X2?) | answer_1(" "$WORK/cva_pop.flat" &&
    grep -q "stochastic_agent(A, B, H, Mon, Log, Asks, O) :- ground(H?) | stochastic_person(B?, H?, P, S) , random(S?, 2147483646, Seed, _) , start_screen(A?, O?, Mon?, Log?, Scr) , person(A?, P?, H?, Mon?, Scr?, Seed?, Log?, Asks?) \." "$WORK/cva_pop.flat" &&
    grep -q "run_declaration(2, 1, day, 20260927) \." "$WORK/cva_pop.flat" &&
    grep -q "Person ::= person(Key, Constant, List(Constant))" "$WORK/cva_pop.flat" &&
    grep -q "Person ::= person(Key, Constant, List(Constant))" "$CVA/printed/profiles.glp" &&
    grep -q "cva_agents(B, Ks, Mon, Log, Keys?, Seats?) :- persons(B?, Ks?, _, Ps, B1), seats(1, Ps?, B1?, Mon?, Log?, Keys, Seats)\." "$WORK/cva_pop.flat" &&
    grep -q "Seat ( X ) ::= seat ( Key , Stream(Ask) ? , Stream(X) ? ) \." "$WORK/cva_pop.flat"
check "the scripted profiles transformed against the platform's interface (transform.sh --interface): the relay of Menu, the person of run n agents keyed by the platform's Key, the run's entries of one arity, each agent's seat, its person process and its screen" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_tx.out"
for r in a b s1; do
    seed=20260927; [ "$r" = s1 ] && seed=1
    bash "$HERE/cva/run.sh" "$CVA/platform.glp" "$CVA/printed" 2 '1 day' "$seed" "$WORK/cva_$r.log" > "$WORK/cva_$r.out" 2>&1
    st=$?
    check "two agents of the canonical platform, one day, seed $seed ($r): the run ends with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_$r.out"
done
[ -s "$WORK/cva_a.log" ] && cmp -s "$WORK/cva_a.log" "$WORK/cva_b.log" && ! cmp -s "$WORK/cva_a.log" "$WORK/cva_s1.log"
check "two runs from seed 20260927 write one log and seed 1 another" $?
awk -F'\t' '
    NF == 4 {
        n++; t = $1 + 0; if (t < last || t > 86400) bad++; last = t
        if ($2 != 1 && $2 != 2) { bad++; next }
        me = "p" $2; other = ($2 == 1) ? "p2" : "p1"
        if ($3 == "Menu") {
            if (index($4, "menu(" me ", [") != 1) bad++
            if (!seen[$2]++) { if ($4 != "menu(" me ", [], known(" other "))") bad++ }
            if (index($4, "propose(befriend(" me ", " other "))")) bef++
            if (index($4, "propose(establish(" me "))")) est++
        } else if ($3 == "Approval") {
            if ($4 ~ /^approval\(attempt\(p[12], [0-9]+, [0-9]+, befriend\(p[12], p[12]\), none\), yes\)$/ && index($4, "attempt(" other ", ")) app++
            else bad++
        } else if ($3 == "output") {
            out[$2]++
            if ($4 !~ /^(view|refused)\(/) bad++
            if (out[$2] == 1 && ($1 + 0 != 0 || $4 != "view([], [])")) bad++
            if (index($4, "view([friend(" other ")")) fr++
        } else bad++
    }
    NF == 1 { c = $1 + 0; clocked = 1 }
    END { exit !(n > 0 && seen[1] && seen[2] && bef > 0 && est > 0 && app > 0 && out[1] > 0 && out[2] > 0 && fr > 0 && clocked && c == last && !bad) }' "$WORK/cva_a.log"
check "the log: each menu showing its agent's own key and the people it knows, the first answered by asking to meet the other, proposals to establish and to befriend, an attempt of the other's approved yes; and each element of the program's output each agent's screen shows, output, a view or a notice, the first the empty view at 0, a friendship in a view after the approval; in the order of time; the clock last (sGLP's task of 2026-10-20 08:00 UTC, item 5)" $?
printf 'draw scripted 2 agreeable 2\nagent 1 scripted agreeable\nagent 2 scripted agreeable\n' | cmp -s - "$WORK/cva_a.draw"
check "the population's draw beside the log: its counts, then agents 1 and 2 with their profiles" $?
grep -E '^(draw|wall-clock)' "$WORK/cva_a.out" | sed 's/^/        /'
head -6 "$WORK/cva_a.log" | sed 's/^/        /'
# A simulation record, in a scratch root, from the scratch platform's
# directory in place of a cascade tag, and its check.
mkdir -p "$WORK/sim"
bash "$HERE/cva/record.sh" GSG "$HERE/tests/cva/scripted_sglp.glp" --cascade "$CVA" --seeds 20260927,1 --root "$WORK/sim" > "$WORK/cva_rec.out" 2>&1
st=$?
REC=$(ls -d "$WORK"/sim/GSG/* 2>/dev/null | head -1)
all_there() { local d=$1; shift; for f in "$@"; do [ -s "$d/$f" ] || return 1; done; }
[ "$st" -eq 0 ] && [ -n "$REC" ] &&
    all_there "$REC" scripted_sglp.glp profiles.glp population.glp 20260927.log 20260927.draw 1.log 1.draw manifest.md &&
    cmp -s "$REC/20260927.log" "$WORK/cva_a.log" && cmp -s "$REC/1.log" "$WORK/cva_s1.log" &&
    cmp -s "$REC/scripted_sglp.glp" "$HERE/tests/cva/scripted_sglp.glp" &&
    grep -q '^- name: GSG$' "$REC/manifest.md" && grep -q '^- seeds: 20260927,1$' "$REC/manifest.md" &&
    grep -q '^- agents: 2$' "$REC/manifest.md" && grep -q '^- until: 1 day$' "$REC/manifest.md" &&
    grep -q '^- programs/sglp commit: ' "$REC/manifest.md" && grep -q '^- cascade commit: directory$' "$REC/manifest.md"
check "a simulation record (cva/record.sh): the sGLP source as it stood, the printed modules, each seed's log and draw, the runs' logs, and the manifest naming the cascade, the commits and the seeds" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_rec.out"
bash "$HERE/cva/record.sh" --check "$REC" > "$WORK/cva_chk.out" 2>&1
st=$?
[ "$st" -eq 0 ] && grep -q '^profiles.glp: as printed$' "$WORK/cva_chk.out" && grep -q '^population.glp: as printed$' "$WORK/cva_chk.out" &&
    grep -q '^20260927.log: first day as recorded' "$WORK/cva_chk.out" && grep -q '^1.log: first day as recorded' "$WORK/cva_chk.out"
check "the record's check (record.sh --check): its printed modules as printed and each seed's first day as recorded" $?
sed 's/^/        /' "$WORK/cva_chk.out"
cp -R "$REC" "$WORK/sim_bad"
awk -F'\t' 'NF == 4 && !done { $4 = $4 " "; done = 1 } { print }' OFS='\t' "$REC/1.log" > "$WORK/sim_bad/1.log"
echo '%% changed' >> "$WORK/sim_bad/population.glp"
bash "$HERE/cva/record.sh" --check "$WORK/sim_bad" > "$WORK/cva_bad.out" 2>&1
st=$?
[ "$st" -ne 0 ] && grep -q '^1.log: first day NOT as recorded$' "$WORK/cva_bad.out" &&
    grep -q '^population.glp: NOT as printed$' "$WORK/cva_bad.out" && grep -q '^20260927.log: first day as recorded' "$WORK/cva_bad.out"
check "the check fails on a record whose log or printed population was changed, naming each" $?
# A census on the canonical platform (tests/cva/census_sglp.glp; sGLP's task
# of 2026-10-20 08:00 UTC, items 1, 2 and 7): the census pair draws p1, a
# host, and p2, a guest, keyed by the platform's keys; a host has two
# dimensions and a guest one; the run is cva/run.sh's with census for its
# agents, the agents the persons in the census's order, from the blocks after
# the census's; and a record of it and its check.
mkdir -p "$CVA/census"
bash "$HERE/transform.sh" --interface "$CVA/interface.glp" --source "$HERE/tests/cva/census_sglp.glp" --out "$CVA/census" > "$WORK/cva_ctx.out" 2>&1
st=$?
tr '\n' ' ' < "$CVA/census/population.glp" > "$WORK/cva_cpop.flat"
[ "$st" -eq 0 ] && ! grep -q '^%% not transformed' "$CVA/census/population.glp" "$CVA/census/profiles.glp" &&
    grep -q "Profiles ::= host(Dimension1, Dimension2) ; guest(Dimension3) \. " "$WORK/cva_cpop.flat" &&
    grep -q "run_declaration(pair, 1, day, 20260927) \. " "$WORK/cva_cpop.flat" &&
    grep -q "Seat ( X ) ::= seat ( Key , Stream(Ask) ? , Stream(X) ? ) \. " "$WORK/cva_cpop.flat" &&
    grep -q "person(A, guest(guest), H, Mon, Scr, Seed, Log, \[ask('Menu', menu_w(E)) | As\]) :- ground(H?) | random(Seed?, 2147483646, K, S) , profiles # script_menu(Y?, H?, K?, Mon?, Scr?) , " "$WORK/cva_cpop.flat"
check "a census over the canonical platform transformed against its interface: two roles, the guest's one dimension declaring both questions by the host's person procedures, the seats keyed by the platform's Key" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_ctx.out"
bash "$HERE/cva/run.sh" "$CVA/platform.glp" "$CVA/census" census '1 day' 20260927 "$WORK/cva_c.log" > "$WORK/cva_c.out" 2>&1
st=$?
check "the census's two agents on the canonical platform, one day, seed 20260927: the run ends with no error" $st
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_c.out"
printf 'draw scripted 1 agreeable 1 guest 1\nagent 1 scripted agreeable\nagent 2 guest\n' | cmp -s - "$WORK/cva_c.draw" &&
    awk -F'\t' '
        NF == 4 && $3 == "Menu" { if (index($4, "menu(p" $2 ", [") != 1) bad++; m[$2]++ }
        NF == 4 && $3 == "output" { o[$2]++ }
        NF == 1 { clocked = 1 }
        END { exit !(m[1] > 0 && m[2] > 0 && o[1] > 0 && o[2] > 0 && clocked && !bad) }' "$WORK/cva_c.log"
check "the draw beside the log the host's two profiles and the guest's one, agents 1 and 2 the census's persons in its order; each agent's menus show its person's key, p1 and p2, and its screen's output lines are logged" $?
mkdir -p "$WORK/simc"
bash "$HERE/cva/record.sh" GSG "$HERE/tests/cva/census_sglp.glp" --cascade "$CVA" --root "$WORK/simc" > "$WORK/cva_crec.out" 2>&1
st=$?
CREC=$(ls -d "$WORK"/simc/GSG/* 2>/dev/null | head -1)
[ "$st" -eq 0 ] && [ -n "$CREC" ] && grep -q '^- agents: census$' "$CREC/manifest.md" &&
    cmp -s "$CREC/20260927.log" "$WORK/cva_c.log" &&
    bash "$HERE/cva/record.sh" --check "$CREC" > "$WORK/cva_cchk.out" 2>&1 &&
    grep -q '^20260927.log: first day as recorded' "$WORK/cva_cchk.out"
check "a record of the census's run (cva/record.sh), its agents census, its log the run's, and its check passing on it" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/cva_crec.out"

# The network with a census whose keys are not in the order @< takes
# (tests/route; sGLP #9, Code_inbox 2026-10-24 14:07 UTC; GLP-Spec, the
# transaction Cold-call, which adds msg(q, X) to the input stream of the agent
# q): a stub platform whose agents each send hi(K), K its key, to p2, p1 and
# p3, and show got(To, From) for each message reaching their input stream; the
# census p2, p1, p3, agents 1, 2 and 3 in its order.  Each agent shows the
# three messages to its own key and no other.
bash "$HERE/cva/run.sh" "$HERE/tests/route/platform.glp" "$HERE/tests/route" census '1 day' 1 "$WORK/route.log" > "$WORK/route.out" 2>&1
st=$?
awk -F'\t' 'NF == 4 && $3 == "output" { print $2, $4 }' "$WORK/route.log" | sort > "$WORK/route.got"
[ "$st" -eq 0 ] &&
    printf '%s\n' '1 got(p2, p1)' '1 got(p2, p2)' '1 got(p2, p3)' '2 got(p1, p1)' '2 got(p1, p2)' '2 got(p1, p3)' \
        '3 got(p3, p1)' '3 got(p3, p2)' '3 got(p3, p3)' | sort | cmp -s - "$WORK/route.got"
check "a census keyed p2, p1, p3, not in the order @< takes: every message reaches the agent of its key, agent 1 p2's three, agent 2 p1's and agent 3 p3's (GLP-Spec, Cold-call)" $?
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/route.out"

echo "--- (xix) the screen: seen before and after an element arrives, and the log's output lines"
# tests/screen: the program writes note(N1), N1 its first count, not ground
# until the person answers it, and note(9) after it; the person answers each
# count a second after asked with the number of notes seen (sGLP's task of
# 2026-10-20 08:00 UTC, items 4, 5 and 7; Section 3, Definition "Screen,
# Seen"; Section 4, "The screen" and "The log").
repl "$WORK/screen.out" ':limit 1000000000000' "$HERE/tests/screen" 'screen_run(Ns).'
sed 's/^\(GLP> \)*//' "$WORK/screen.out" > "$WORK/screen.lines"
grep -q '^✓ Loaded program' "$WORK/screen.out"
check "tests/screen loads, its population printed before the run" $?
awk '
    /^entry\(/ { e[++n] = $0; t[n] = substr($0, 7, index($0, ",") - 7) + 0 }
    /^Ns = \[0, 2\]$/ { ns = 1 }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        if (e[1] !~ /^entry\([0-9.]+, 1, Count, count\(0\)\)$/) bad++
        if (e[2] !~ /^entry\([0-9.]+, 1, output, note\(0\)\)$/) bad++
        if (e[3] !~ /^entry\([0-9.]+, 1, output, note\(9\)\)$/) bad++
        if (e[4] !~ /^entry\([0-9.]+, 1, Count, count\(2\)\)$/) bad++
        if (!(t[1] > 0 && t[2] == t[1] && t[3] == t[1] && t[4] > t[3])) bad++
        exit !(n == 4 && ns && seen && c == t[4] && !bad)
    }' "$WORK/screen.lines"
check "the log: the first count answered 0, seen before the notes were shown, note(N1) not yet ground holding back note(9); each note then shown in order, output, at the time of the answer that grounds the first; the second count answered 2, seen after both; the program reads [0, 2]; the clock last" $?
grep '^entry(\|^clock(\|^Ns = ' "$WORK/screen.lines" | sed 's/^/        /'

echo "--- (xx) a census of two roles with attributes, a dimension of one role, and seen with no Output"
# tests/census: the census village draws a parent, ann, with the number of
# its children, and one to three children, each with its age; a parent has
# two dimensions and a child one; the formal parent greets with its key and
# the number of elements it has seen, the program writing nothing to its
# person, and a child with its key and its age, both read off its person
# (sGLP's task of 2026-10-20 08:00 UTC, items 1 to 4 and 7).
repl "$WORK/census.out" ':limit 1000000000000' "$HERE/tests/census" 'census_run(Ps).' 'census_draw(Ds).'
sed 's/^\(GLP> \)*//' "$WORK/census.out" > "$WORK/census.lines"
grep -q '^✓ Loaded program' "$WORK/census.out"
check "tests/census loads, its population printed before the run" $?
awk '
    /^Ps = \[/ {
        s = $0
        while (match(s, /person\([a-z]+, [a-z]+, \[[a-z]+\([0-9]+\)\]\)/)) {
            q = substr(s, RSTART + 7, RLENGTH - 8); s = substr(s, RSTART + RLENGTH)
            split(q, f, ", "); np++; key[np] = f[1]; role[np] = f[2]
            sub(/^\[[a-z]+\(/, "", f[3]); sub(/\)\]$/, "", f[3]); val[np] = f[3]
        }
    }
    /^Ds = \[/ {
        s = $0
        while (match(s, /agent\([0-9]+, [a-z]+\([a-z, ]+\)\)/)) {
            q = substr(s, RSTART + 6, RLENGTH - 7); s = substr(s, RSTART + RLENGTH)
            a = substr(q, 1, index(q, ",") - 1) + 0; r = substr(q, index(q, ", ") + 2)
            dr[a] = substr(r, 1, index(r, "(") - 1); dp[a] = substr(r, index(r, "(") + 1, length(r) - index(r, "(") - 1); nd++
        }
    }
    /^entry\(/ {
        n++; tm = substr($0, 7, index($0, ",") - 7) + 0; if (tm < last) bad++; last = tm
        x = substr($0, 7, length($0) - 7); split(x, f, ", ")
        a = f[2] + 0; ans[a] = ans[a] " " substr(x, index(x, f[3] ", ") + length(f[3]) + 2)
    }
    /^clock\(/ { c = substr($0, 7, length($0) - 7) + 0; seen = 1 }
    END {
        if (np < 2 || role[1] != "parent" || nd != np) bad++
        for (a = 1; a <= np; a++) {
            if (dr[a] != role[a]) bad++
            want_n = (role[a] == "parent") ? 0 : val[a]
            mood = (role[a] == "parent") ? substr(dp[a], index(dp[a], ", ") + 2) : "happy"
            if (index(ans[a], " hello(" key[a] ", _)") == 0) bad++
            if (index(ans[a], " hello(_, " want_n ")") == 0) bad++
            if (index(ans[a], " mood(" mood ")") == 0) bad++
        }
        printf "        %d persons, %d entries, %d not as their persons and draws say\n", np, n, bad
        exit !(n == 3 * np && seen && c == last && !bad)
    }' "$WORK/census.lines"
check "the agents are the persons the census draws, in its order, a parent and its children, each with the stochastic person of its role, a parent two profiles and a child one; each greets with the key and the attribute its person gives it, the parent with the number of elements seen, none, the program writing nothing to its person; the moods as drawn; in the order of time; the clock last" $?
grep '^entry(\|^clock(\|^Ps = \|^Ds = ' "$WORK/census.lines" | sed 's/^/        /'
# A census drawing nobody (seed 3) and one drawing a person of a role of no
# dimension, a stranger (seed 4): the run is refused, taking no goal but the
# line refused, no entry and no clock.
repl "$WORK/census_refused.out" ':limit 1000000000000' "$HERE/tests/census" 'census_persons(3, Ok, Ps).' 'census_persons(4, Ok, Ps).' 'census_seeded(3, Ps).' 'census_seeded(4, Ps).'
sed 's/^\(GLP> \)*//' "$WORK/census_refused.out" > "$WORK/census_refused.lines"
grep -q '^Ok = no$' "$WORK/census_refused.lines" && [ "$(grep -c '^Ok = no$' "$WORK/census_refused.lines")" -eq 2 ] &&
    grep -q '^Ps = \[person(eve, stranger, \[\])\]$' "$WORK/census_refused.lines" &&
    [ "$(grep -c '^refused$' "$WORK/census_refused.lines")" -eq 2 ] &&
    ! grep -q '^entry(\|^clock(' "$WORK/census_refused.lines"
check "a census drawing no person, and one drawing a person of a role of no dimension, are refused by the run, which takes no goal but the line refused (Section 3, after Definition \"Well-Formed\")" $?

echo ""
echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
