#!/bin/bash
# Tests of sGLP in GLP (programs/sglp) against sGLP's paper (the repository
# svGLP-Stochastic-Volitional-GLP at d2f64b6) and its code tasks of 2026-10-02
# 00:06 UTC, item 6, 15:23 UTC, items 1 and 3, 15:24 UTC, items 1 to 4, and
# 15:44 UTC, and of 2026-10-03 08:44 UTC, item 2, 08:58 UTC, 09:22 UTC and
# 09:45 UTC, and the transformation of 2026-10-03 09:27 UTC.
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
#       ends with no error and a non-empty log of menus, cards and their
#       answers, and its monitor's last line.
# (v)   the mix as arguments, 100 agents for 30 days from seed 20260927:
#       social_graph/6 at 60 and 70 writes the log social_graph/4 writes; at 0
#       and 0 every agent is indifferent and sociable, so over 0.4 of its menu
#       answers pick the other sex and over 0.8 of its cards are answered yes
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
#       other-sex share and its cards' yes share; the .draw file beside the
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
#       answered while a card waits; coins arriving while a card waits; the
#       clock --- it prints tests/circulation/four.expected, the twelve
#       months and the totals computed by hand from the log.  The pay answered
#       while a card waits: agent 4 pays 3 the ten of 3's coins it holds
#       (6500000 s) and, its wallet empty, proposes 3 a swap of ten (6600000
#       s), whose card waits at 3 showing Held 7, the seven of 4's coins 3
#       holds; 3's person answers pay(4, 9) while it waits (7800000 s, month 3)
#       and the card yes in month 4 (8000000 s).  The pay is made when the card
#       is answered, after the card's move: 3 holds 7 + 10 = 17 of 4's coins
#       and pays 9, keeping 8, and 4 holds 10 of 3's; so month 3 ends at
#       circulation 47, holdings 5, wallets 3, the pay not yet made, and
#       8000000 s at 58, 6 and 4.  A pay made at its answer would take min(9,
#       7) = 7 in month 3, leaving 40, 4 and 3 there, and 60 at 8000000
#       s.  Coins arriving while a card waits (the task of 2026-10-03 09:22
#       UTC): 2 pays 1 the ten of 1's coins it holds (8200000 s) and, its
#       wallet empty, proposes 1 a swap of ten (8300000 s), whose card waits
#       at 1; 3 proposes 2 a swap of ten (8400000 s), whose card waits at 2; 1
#       answers yes (10000000 s, month 4), holding 20 of 2's coins, and sends
#       2 ten of its own, which arrive while 2's card waits; 2 answers yes in
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
#       answers in the order of time, each offer answered being the card
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
#       dimension whose probabilities do not sum to one and a profile in no
#       dimension is refused with those two faults and nothing printed; the
#       same source with them mended is translated, the rated goal's procedure
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
#       X, which, once Y? is ground, assigns X with it and logs it; and a
#       source whose reader-mode type has an argument the program writes is
#       refused with that fault, the transformation printing nothing for a
#       case the paper does not decide.
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
    'busy(100000, O).' 'busy_late(100000, O).'
grep -q '✓ Loaded program' "$WORK/busy.out"
check "tests/busy loads" $?
[ "$(grep -c '^\(GLP> \)*O = after$' "$WORK/busy.out")" -eq 2 ]
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
    grep -q '^card_answers [1-9]' "$WORK/y.out" && ! grep -q '^clock none$' "$WORK/y.out"
check "the log is menus, cards and their answers, and the monitor's last line" $?
sed -n '/^wall-clock/p;/^answers/,/^unread/p' "$WORK/y.out" | sed 's/^/        /'

echo "--- (v) the mix as arguments"
# mix_shares <log>: "<menus> <other> <cards> <yes>", the numbers of menu and
# card answers, the share of the menus picking the other sex and the share of
# the cards answered yes.
mix_shares() {
    awk -F'\t' '
        $3 == "Menu" { m++; if ($4 ~ /other\([^()]*\)\)$/) o++ }
        $3 == "Card" { c++; if ($4 ~ /, yes\)$/) y++ }
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
echo "        mix 0 0: $M menus, other-sex share $O; $C cards, yes share $Y"
[ "$M" -gt 0 ] && [ "$C" -gt 0 ] && awk -v o="$O" -v y="$Y" 'BEGIN { exit !(o > 0.4 && y > 0.8) }'
check "at 0 and 0, indifferent and sociable: over 0.4 of the menus pick the other sex, over 0.8 of the cards are answered yes" $?
read -r M O C Y <<< "$(mix_shares "$WORK/m100-100.log")"
echo "        mix 100 100: $M menus, other-sex share $O; $C cards, yes share $Y"
[ "$M" -gt 0 ] && [ "$C" -gt 0 ] && awk -v o="$O" -v y="$Y" 'BEGIN { exit !(o < 0.1 && y < 0.3) }'
check "at 100 and 100, homophile and wary: under 0.1 of the menus pick the other sex, under 0.3 of the cards are answered yes" $?

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
    $3 == "Card" && $4 ~ /, yes\)$/ && $1 + 0 < 2629800 {
        p = substr($4, 6, index($4, ", ") - 6) + 0; a = $2 + 0
        if (a != p) print (a < p ? a " " p : p " " a)
    }' "$WORK/y.log" | sort -u | wc -l | tr -d ' ')
[ "$M1" -gt 0 ] && grep -q "^month 1 at 2629800 edges $M1 " "$WORK/y.months"
check "month 1's edges are the $M1 pairs with a yes before 2629800 s" $?
sed -n '/^month /p' "$WORK/y.months" | sed 's/^/        /'

echo "--- (vii) the population's draw"
# The draw run.sh printed for (iv), against the profiles the log of (iv) shows:
# an agent is homophile if under 0.26 of its menus pick the other sex
# (homophile 0.02, indifferent 0.5), and wary if under 0.55 of its cards are
# answered yes (wary 0.2, sociable 0.9).
D=$(sed -n 's/^draw \(homophile [0-9]* indifferent [0-9]* wary [0-9]* sociable [0-9]*\)$/\1/p' "$WORK/y.out")
L=$(awk -F'\t' '
    NF == 4 && $3 == "Menu" { m[$2]++; if ($4 ~ /other\([^()]*\)\)$/) o[$2]++ }
    NF == 4 && $3 == "Card" { c[$2]++; if ($4 ~ /, yes\)$/) y[$2]++ }
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
# shares above; an agent with no menu or no card in the log is not shown.
A=$(awk 'FNR == NR { if ($1 == "agent") line[$2] = $3 " " $4; next }
    { split($0, f, "\t") }
    f[3] == "Menu" { m[f[2]]++; if (f[4] ~ /other\([^()]*\)\)$/) o[f[2]]++ }
    f[3] == "Card" { c[f[2]]++; if (f[4] ~ /, yes\)$/) y[f[2]]++ }
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
check "the log is menus and offers and their answers, in the order of time, each offer the card waiting at its agent, read whole by circulation.awk" $?
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
# 0.5 and a profile r in no dimension, GOOD neither.
V="[type('A', [a]), type('Q', [q(dual('A'))]), volitional('Q', q_w, ask(dual('A')))]"
P="person(p), binds('Q', p_q), decl(p_q(dual('Q'), dual('Integer'))), clause(p_q(var('X'), var('_')), [], [rated(ans(var('X?')), rate(1, day))]), decl(ans(dual('Q'))), clause(ans(q(a)), [], [])"
repl "$WORK/txbad.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$P, person(r), run(2, [mix(d, [share(p, 0.5)])], 1, day, 1)], profiles)."
repl "$WORK/txgood.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($V, [$P, run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)], profiles)." \
    "transform_terms($V, [$P, run(2, [mix(d, [share(p, 1.0)])], 1, day, 1)], population)."
sed 's/^\(GLP> \)*//' "$WORK/txbad.out" > "$WORK/txbad.lines"
sed 's/^\(GLP> \)*//' "$WORK/txgood.out" > "$WORK/txgood.lines"
grep -q '^%% not transformed: the probabilities of a dimension (one to nine profiles, whole percentages, summing to one)(d)$' "$WORK/txbad.lines" &&
    grep -q '^%% not transformed: a profile not in exactly one dimension(r)$' "$WORK/txbad.lines" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txbad.lines")" -eq 2 ] &&
    ! grep -q '^p_q(' "$WORK/txbad.lines"
check "a source with a dimension not summing to one and a profile in no dimension is refused with those faults, nothing printed" $?
! grep -q '^%% not transformed' "$WORK/txgood.lines" &&
    grep -q '^ans(go, q(a))$' "$WORK/txgood.lines" &&
    grep -q '^p_q(X, _, Mon)$' "$WORK/txgood.lines" &&
    grep -q "^stream_append(rated('/'(1, day), Tok), Mon?, _)$" "$WORK/txgood.lines" &&
    grep -q '^ans(Tok?, X?)$' "$WORK/txgood.lines" &&
    grep -q '^Ask ::= ask(Constant, Question).$' "$WORK/txgood.lines" &&
    grep -A 2 '^Question$' "$WORK/txgood.lines" | tr '\n' ' ' | grep -q '^Question ::= q_w(Q) $' &&
    grep -q '^person(A, profiles(p), Mon, Seed, Log, \[ask(Type, q_w(q(X1?))) | As\])$' "$WORK/txgood.lines" &&
    grep -q '^random(Seed?, 2147483646, K, S)$' "$WORK/txgood.lines" &&
    grep -q '^p_q(q(Y1), K?, Mon?)$' "$WORK/txgood.lines" &&
    grep -q '^person(A?, profiles(p), Mon?, S?, Log?, As?)$' "$WORK/txgood.lines" &&
    ! grep -q '^random(Seed?, 1, _, S)$' "$WORK/txgood.lines" &&
    grep -q '^declaration(2, 1, day, 1)$' "$WORK/txgood.lines" &&
    grep -q '^d(_, p)$' "$WORK/txgood.lines"
check "the mended source is translated: the token, the monitor's reference, the asks, the person process's clause on an ask and its seeding, the declaration and the draw" $?
# Reader mode: one interactive type Rating in reader mode, (Rating?)*rate, the
# person procedure fair_rating of profile fair writing the whole term, a
# rating of 1..5, after a rated goal of give; and RV, a reader-mode type Bid
# with an argument the program writes, Reply?.
RV="[type('Rating', [rating('Integer')]), volitional(dual('Rating'), rating_r, rate('Integer'))]"
RP="person(fair), binds('Rating', fair_rating), decl(fair_rating('Rating', dual('Integer'))), clause(fair_rating(var('R?'), var('S')), [], [random(var('S?'), 5, var('K'), var('_')), rated(give(var('K?'), var('R')), rate(1, day))]), decl(give(dual('Integer'), 'Rating')), clause(give(var('K'), rating(var('K?'))), [], []), run(1, [mix(rater, [share(fair, 1.0)])], 30, days, 20260927)"
BV="[type('Reply', [ok, no]), type('Bid', [bid('Integer', dual('Reply'))]), volitional(dual('Bid'), bid_r, offer('Integer'))]"
BP="person(fair), binds('Bid', fair_bid), decl(fair_bid('Bid', dual('Integer'))), clause(fair_bid(bid(1, var('_')), var('_')), [], []), run(1, [mix(bidder, [share(fair, 1.0)])], 30, days, 1)"
repl "$WORK/txreader.out" ':limit 1000000000000' "$HERE" \
    "transform_terms($RV, [$RP], profiles)." \
    "transform_terms($RV, [$RP], population)." \
    "transform_terms($BV, [$BP], population)."
sed 's/^\(GLP> \)*//' "$WORK/txreader.out" > "$WORK/txreader.lines"
# The lines of each part, between its begin and end lines.
awk '$0 == "%% transform begin" { k++; next } $0 == "%% transform end" { next } k == 2' "$WORK/txreader.lines" > "$WORK/txreader.pop"
awk '$0 == "%% transform begin" { k++; next } $0 == "%% transform end" { next } k == 3' "$WORK/txreader.lines" > "$WORK/txreader.bid"
! grep -q '^%% not transformed' "$WORK/txreader.pop" &&
    grep -q '^fair_rating(R?, S, Mon)$' "$WORK/txreader.lines" &&
    grep -A 5 '^fair_rating$' "$WORK/txreader.pop" | tr '\n' ' ' | grep -q '^fair_rating ( Rating , Integer? , ' &&
    grep -A 3 '^person(A, profiles(fair), Mon, Seed, Log, \[ask(Type, rating_r(X?)) | As\])$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q ' :- random(Seed?, 2147483646, K, S) , $' &&
    grep -q '^fair_rating(Y, K?, Mon?)$' "$WORK/txreader.pop" &&
    grep -q '^answer_1(A?, Type?, Y?, X, Mon?, Log?)$' "$WORK/txreader.pop" &&
    grep -q '^person(A?, profiles(fair), Mon?, S?, Log?, As?)$' "$WORK/txreader.pop" &&
    grep -A 16 '^answer_1$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q '^answer_1 ( Integer? , Constant? , Rating ? , Rating , MutualRef? , MutualRef? ) . ' &&
    grep -A 5 '^answer_1(A, Type, Y, Y?, Mon, Log)$' "$WORK/txreader.pop" | tr '\n' ' ' |
        grep -q ' :- ground(Y?) | stream_append(time(Time), Mon?, _) , $' &&
    grep -q '^stream_append(entry(Time?, A?, Type?, Y?), Log?, _)$' "$WORK/txreader.pop" &&
    ! grep -q 'rating(rating(' "$WORK/txreader.pop"
check "in reader mode the person goal receives a writer and the program the reader: the person procedure of the dual type, the clause on the ask's writer, the answer assigned from the copy and logged" $?
grep -q '^%% not transformed: an interactive type in reader mode of one to nine arguments, none written by the program(Bid)$' "$WORK/txreader.bid" &&
    [ "$(grep -c '^%% not transformed' "$WORK/txreader.bid")" -eq 1 ] &&
    ! grep -q '^person(' "$WORK/txreader.bid"
check "a reader-mode type with an argument the program writes is refused with that fault, nothing printed" $?

echo ""
echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
