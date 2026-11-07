#!/bin/bash
# sglp-sweep.sh [tables]: sGLP's task 1 of 2026-10-09 20:15 UTC, the seed sweep
# for Tables 1 and 2 as mean and spread (Integration Code's subagent;
# measurement only, no file of the repository changed).  GLP branch sGLP-gap
# (= gap) as it stands, /Users/udi/Grassroots/GLP-worktrees/sGLP-gap.
#
# The runs are made as the grid's were (sglp-conformance-remake.sh, which
# made sglp-grid-* and sglp-coins-grid-*), one at a time, each after no
# glp_repl_exe is alive:
#   the social graph, the nine mixes homophile H of 0, 60, 100 by wary W of 0,
#   70, 100, seeds 1 to 20, 100 agents, one year:
#     /usr/bin/time -p bash social_graph/run.sh 100 '1 year' <seed> <log> <graph> H W
#     -> sglp-sweep-hH-wW-<seed>.out (run.sh's output), .log, .graph, .draw,
#        and .txt (the draw line and friendship.awk n=100 months=12 on the log);
#   coins among friends, the nine mixes spenders S of 0, 50, 100 by cautious C
#   of 0, 50, 100, the same seeds, each over sglp-sweep-h60-w70-<seed>.graph:
#     bash coins/run.sh 100 <graph> '1 year' <seed> <log> S C
#     -> sglp-coins-grid-sS-cC-<seed>.out, .log, .draw, and .txt
#        (circulation.awk on the log).
# FIRST the gate: the seed-1 runs at 60/70 and at 50/50 byte-identical to
# sglp-grid-h60-w70-1.log and sglp-coins-grid-s50-c50-1.log, or STOP.  A run is
# done when its .txt is in place, so the script resumes from the runs made.  A
# coins run the harness refuses because an agent is on no edge of the graph
# (Appendix B) is left out and recorded; any other failure or refusal is a
# STOP.  Last, the summary sglp-sweep-tables.txt (also made alone by
# "sglp-sweep.sh tables").  Progress in sglp-sweep-progress.txt.
set -u
WT=/home/user/GLP-worktrees/sglp-e92
T=/home/user/sglp-main
SG=$WT/programs/sglp/social_graph/run.sh
CO=$WT/programs/sglp/coins/run.sh
FA=$WT/programs/sglp/social_graph/friendship.awk
CA=$WT/programs/sglp/coins/circulation.awk
P=/home/user/sglp-main/progress.txt
SEEDS=$(seq 1 20)

say() { echo "$(date -u '+%F %T') UTC $*" >> "$P"; }
stop() { say "STOP: $*"; exit 1; }
load() { uptime | sed 's/.*load/load/'; }
wait_glp() {
    local n=0
    while ps -axo command | grep -v grep | grep -q glp_repl_exe; do
        [ $((n % 60)) -eq 0 ] && say "waiting: another GLP is alive: $(ps -axo pid,rss,etime,command | grep glp_repl_exe | grep -v grep | tr '\n' ';')"
        n=$((n + 1)); sleep 5
    done
}

sg_run() {   # sg_run <H> <W> <seed>
    local H=$1 W=$2 seed=$3
    local b=sglp-sweep-h$H-w$W-$seed
    [ -f "$T/$b.txt" ] && return 0
    rm -f "$T/$b.log" "$T/$b.graph" "$T/$b.draw" "$T/$b.out" "$T/$b.out.part" "$T/$b.txt.part"
    wait_glp; say "start $b"
    { echo "start $(date -u '+%F %T') UTC; $(load)"
      /usr/bin/time -p bash "$SG" 100 '1 year' "$seed" "$T/$b.log" "$T/$b.graph" "$H" "$W" 2>&1
      echo "exit $?"
      echo "end $(date -u '+%F %T') UTC; $(load)"; } > "$T/$b.out.part" 2>&1
    mv "$T/$b.out.part" "$T/$b.out"
    grep -q '^exit 0$' "$T/$b.out" || stop "$b failed: $T/$b.out"
    { grep '^draw homophile' "$T/$b.out"; awk -v n=100 -v months=12 -f "$FA" "$T/$b.log"; } > "$T/$b.txt.part"
    mv "$T/$b.txt.part" "$T/$b.txt"
    say "done $b: $(grep '^wall-clock' "$T/$b.out" | cut -d';' -f1); $(grep '^draw homophile' "$T/$b.out"); $(grep '^edges' "$T/$b.txt"); $(grep '^isolated' "$T/$b.txt")"
}

co_run() {   # co_run <S> <C> <seed>
    local S=$1 C=$2 seed=$3
    local b=sglp-coins-grid-s$S-c$C-$seed
    local g=$T/sglp-sweep-h60-w70-$seed.graph
    [ -f "$T/$b.txt" ] && return 0
    if [ -f "$T/$b.out" ] && grep -q '^exit 2$' "$T/$b.out" && grep -q 'is on no edge of the graph' "$T/$b.out"; then
        return 0
    fi
    [ -f "$g" ] || stop "$b: the graph $g is missing"
    rm -f "$T/$b.log" "$T/$b.draw" "$T/$b.out" "$T/$b.out.part" "$T/$b.txt.part"
    wait_glp; say "start $b"
    { bash "$CO" 100 "$g" '1 year' "$seed" "$T/$b.log" "$S" "$C"; echo "exit $?"; } > "$T/$b.out.part" 2>&1
    mv "$T/$b.out.part" "$T/$b.out"
    if grep -q '^exit 2$' "$T/$b.out"; then
        if grep -q 'is on no edge of the graph, so it has no friends' "$T/$b.out" &&
           [ "$(grep '^run\.sh: ' "$T/$b.out" | grep -vc 'is on no edge of the graph, so it has no friends')" -eq 0 ]; then
            say "LEFT OUT $b: the harness refuses the graph: $(grep '^run\.sh: ' "$T/$b.out" | tr '\n' ' ')"
            return 0
        fi
        stop "$b refused by the harness, not for an agent on no edge: $T/$b.out"
    fi
    grep -q '^exit 0$' "$T/$b.out" || stop "$b failed: $T/$b.out"
    awk -f "$CA" "$T/$b.log" > "$T/$b.txt.part"
    mv "$T/$b.txt.part" "$T/$b.txt"
    say "done $b: $(grep '^wall-clock' "$T/$b.out" | cut -d';' -f1); $(grep '^draw spender' "$T/$b.out"); $(grep '^circulation' "$T/$b.txt")"
}

# ---------------------------------------------------------------- the tables
# stats <label> <values...>: "<label>: mean m, sd s, min a, max b" over the
# numeric values (sample standard deviation, n - 1), then the values in seed
# order on the next line; "none" and "-" are listed and not counted.
stats() {
    local label=$1; shift
    echo "$*" | awk -v label="$label" '{
        n = 0; s = 0
        for (i = 1; i <= NF; i++) if ($i ~ /^-?[0-9.]+$/) {
            v = $i + 0; n++; s += v; x[n] = v
            if (n == 1 || v < lo) lo = v; if (n == 1 || v > hi) hi = v }
        if (n == 0) { printf "  %-34s no values\n", label; next }
        m = s / n; ss = 0
        for (i = 1; i <= n; i++) ss += (x[i] - m) ^ 2
        sd = (n > 1) ? sqrt(ss / (n - 1)) : 0
        printf "  %-34s mean %10.2f   sd %9.2f   min %8s   max %8s   (n %d)\n", label, m, sd, lo, hi, n
        printf "  %-34s", ""
        for (i = 1; i <= NF; i++) printf " %s", $i
        printf "\n"
    }'
}
# meansd <values...>: "m ± s" for the compact table.
meansd() {
    echo "$*" | awk '{ n = 0; s = 0
        for (i = 1; i <= NF; i++) if ($i ~ /^-?[0-9.]+$/) { n++; s += $i; x[n] = $i }
        if (n == 0) { print "-"; exit }
        m = s / n; ss = 0; for (i = 1; i <= n; i++) ss += (x[i] - m) ^ 2
        printf "%.1f±%.1f", m, (n > 1 ? sqrt(ss / (n - 1)) : 0) }'
}
key() { awk -v k="$2" '$1 == k { v = $2 } END { print (v == "" ? "-" : v) }' "$1"; }
month12() { awk -v f="$2" '$1 == "month" && $2 == 12 { for (i = 3; i < NF; i += 2) if ($i == f) v = $(i + 1) } END { print (v == "" ? "-" : v) }' "$1"; }
onemonth() { awk '$1 == "month" && $8 == 1 && !d { d = $2 } END { print (d == "" ? "none" : d) }' "$1"; }
sgvals() {   # sgvals <H> <W> <field>
    local s f v
    for s in $SEEDS; do
        f=$T/sglp-sweep-h$1-w$2-$s.txt
        if [ ! -f "$f" ]; then v=-
        else case $3 in
            edges) v=$(month12 "$f" edges) ;;
            across) v=$(month12 "$f" across) ;;
            one) v=$(onemonth "$f") ;;
            menus) v=$(key "$f" menu_answers) ;;
            cards) v=$(key "$f" approval_answers) ;;
            yes) v=$(key "$f" yes) ;;
        esac; fi
        printf '%s ' "$v"
    done
}
covals() {   # covals <S> <C> <field>
    local s f v
    for s in $SEEDS; do
        f=$T/sglp-coins-grid-s$1-c$2-$s.txt
        if [ ! -f "$f" ]; then v=-
        else v=$(key "$f" "$3"); fi
        printf '%s ' "$v"
    done
}
tables() {
    local H W S C s
    {
    echo "sGLP task 1 (2026-10-09 20:15 UTC): the seed sweep, seeds 1 to 20, 100 agents, one year;"
    echo "GLP worktree sGLP-runs at $(git -C "$WT" rev-parse --short HEAD) (main); made $(date -u "+%F %H:%M") UTC by /Users/udi/Grassroots/tmp/sglp-splitmix/tables.sh (sglp-meet-tables.sh adapted: coins over seeds 1-20 named sglp-coins-grid-, cards = approval_answers)."
    echo "Each figure is read from the run's .txt in $T: sglp-sweep-h<H>-w<W>-<seed>.txt"
    echo "(friendship.awk n=100 months=12) and sglp-coins-grid-s<S>-c<C>-<seed>.txt (circulation.awk)."
    echo "Table 1's columns: edges and across at one year = the month-12 line (simulated time 31557600 s, one year,"
    echo "past every run's clock, so the year's graph); one by month = the first month m of 1..12 whose line has"
    echo "components 1 (\"none\" if none).  Table 2's: circulation, proposals, declined = the totals at the log's end"
    echo "(each run's clock is within the year, so the year's).  Statistics: mean, sample standard deviation (n - 1),"
    echo "min, max over the seeds with a value; under them the values in seed order 1..20 (\"-\" = no run)."
    echo ""
    echo "GATES: $(grep -h '^.* GATE' "$P" | sed 's/^[0-9-]* [0-9:]* UTC //' | tr '\n' ' ')"
    echo "LEFT OUT: $(grep -h 'LEFT OUT' "$P" | sed 's/^[0-9-]* [0-9:]* UTC LEFT OUT //' | tr '\n' ' ' ) "
    echo ""
    echo "TABLE 1 (mean ± sd over the seeds): the social graph at the end of the year"
    printf "%-9s %-5s %16s %16s %14s %16s %16s %16s\n" homophile wary edges across "one by month" menus cards yes
    for H in 0 60 100; do for W in 0 70 100; do
        printf "%-9s %-5s %16s %16s %14s %16s %16s %16s\n" "$H" "$W" \
            "$(meansd $(sgvals $H $W edges))" "$(meansd $(sgvals $H $W across))" "$(meansd $(sgvals $H $W one))" \
            "$(meansd $(sgvals $H $W menus))" "$(meansd $(sgvals $H $W cards))" "$(meansd $(sgvals $H $W yes))"
    done; done
    echo ""
    echo "TABLE 2 (mean ± sd over the seeds): coins among friends at the end of the year, over sglp-sweep-h60-w70-<seed>.graph"
    printf "%-8s %-8s %16s %14s %12s %16s %14s %14s %12s\n" spenders cautious circulation proposals declined pays accepted holdings wallets
    for S in 0 50 100; do for C in 0 50 100; do
        printf "%-8s %-8s %16s %14s %12s %16s %14s %14s %12s\n" "$S" "$C" \
            "$(meansd $(covals $S $C circulation))" "$(meansd $(covals $S $C proposals))" "$(meansd $(covals $S $C declined))" \
            "$(meansd $(covals $S $C pays))" "$(meansd $(covals $S $C accepted))" "$(meansd $(covals $S $C holdings))" "$(meansd $(covals $S $C wallets))"
    done; done
    echo ""
    echo "THE SOCIAL GRAPH BY MIX, 100 agents, one year, homophile for K =< H and wary for K =< W"
    for H in 0 60 100; do for W in 0 70 100; do
        echo ""
        echo "== homophile $H, wary $W"
        stats "edges at one year" $(sgvals $H $W edges)
        stats "edges across the sexes" $(sgvals $H $W across)
        stats "one component by the end of month" $(sgvals $H $W one)
        stats "menus (menu_answers)" $(sgvals $H $W menus)
        stats "cards (card_answers)" $(sgvals $H $W cards)
        stats "yeses (yes)" $(sgvals $H $W yes)
        echo "  checks: unread $(for s in $SEEDS; do f=$T/sglp-sweep-h$H-w$W-$s.txt; [ -f "$f" ] && key "$f" unread; done | sort -u | tr '\n' ' ')(all 0 wanted); clock none in $(for s in $SEEDS; do f=$T/sglp-sweep-h$H-w$W-$s.txt; [ -f "$f" ] && key "$f" clock; done | grep -c '^none$') runs"
    done; done
    echo ""
    echo "COINS AMONG FRIENDS BY MIX, 100 agents, one year, spender for K =< S and cautious for K =< C, over sglp-sweep-h60-w70-<seed>.graph"
    for S in 0 50 100; do for C in 0 50 100; do
        echo ""
        echo "== spenders $S, cautious $C"
        stats "circulation at one year" $(covals $S $C circulation)
        stats "proposals" $(covals $S $C proposals)
        stats "declined" $(covals $S $C declined)
        stats "pays" $(covals $S $C pays)
        stats "accepted" $(covals $S $C accepted)
        stats "holdings" $(covals $S $C holdings)
        stats "wallets" $(covals $S $C wallets)
        echo "  checks: unread/unordered/unmatched $(for s in $SEEDS; do f=$T/sglp-coins-grid-s$S-c$C-$s.txt; [ -f "$f" ] && echo "$(key "$f" unread)/$(key "$f" unordered)/$(key "$f" unmatched)"; done | sort -u | tr '\n' ' ')(0/0/0 wanted); clock none in $(for s in $SEEDS; do f=$T/sglp-coins-grid-s$S-c$C-$s.txt; [ -f "$f" ] && key "$f" clock; done | grep -c '^none$') runs"
    done; done
    } > "$T/sglp-sweep-tables.txt.part" && mv "$T/sglp-sweep-tables.txt.part" "$T/sglp-sweep-tables.txt"
}

if [ "${1:-}" = tables ]; then tables; exit 0; fi

C=$(git -C "$WT" rev-parse --short HEAD)
[ -z "$(git -C "$WT" status --porcelain)" ] || stop "the worktree has uncommitted changes"
say "begin at $C"

# The gate, first.
sg_run 60 70 1
if cmp -s "$T/sglp-sweep-h60-w70-1.log" "$T/sglp-grid-h60-w70-1.log"; then
    say "GATE social graph: sglp-sweep-h60-w70-1.log is sglp-grid-h60-w70-1.log byte for byte"
else
    stop "GATE social graph: sglp-sweep-h60-w70-1.log differs from sglp-grid-h60-w70-1.log: $(cmp "$T/sglp-sweep-h60-w70-1.log" "$T/sglp-grid-h60-w70-1.log" 2>&1)"
fi
co_run 50 50 1
if cmp -s "$T/sglp-coins-grid-s50-c50-1.log" "$T/sglp-coins-grid-s50-c50-1.log"; then
    say "GATE coins: sglp-coins-grid-s50-c50-1.log is sglp-coins-grid-s50-c50-1.log byte for byte"
else
    stop "GATE coins: sglp-coins-grid-s50-c50-1.log differs from sglp-coins-grid-s50-c50-1.log: $(cmp "$T/sglp-coins-grid-s50-c50-1.log" "$T/sglp-coins-grid-s50-c50-1.log" 2>&1)"
fi

# The social graph's nine mixes by twenty seeds.
for seed in $SEEDS; do for H in 0 60 100; do for W in 0 70 100; do
    sg_run $H $W $seed
done; done; done
say "SOCIAL-DONE"

# The coins' nine mixes by twenty seeds, over the 60/70 graph of the seed.
for seed in $SEEDS; do for S in 0 50 100; do for C in 0 50 100; do
    co_run $S $C $seed
done; done; done
say "COINS-DONE"

tables
say "tables $T/sglp-sweep-tables.txt"
say "SWEEP-DONE"
