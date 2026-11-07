#!/bin/bash
# sglp-splitmix/runs.sh <job>: one run of sGLP #10's item 2(c) (2026-11-04 16:22 UTC),
# made as tmp/sglp-meet-runs.sh and sglp-meet-cost.sh made October's, on GLP
# worktree sGLP-runs (main f336d119), outputs in tmp/sglp-splitmix/.  A job is
# done when its .txt (or .out for coins refused) is in place.
#   sg <seed>            sglp-r-100y-<seed>          run.sh 100 '1 year' <seed> log graph, under /usr/bin/time -l
#   mix <seed> <H> <W>   sglp-sweep-hH-wW-<seed>     run.sh ... H W, under /usr/bin/time -p
#   co <seed>            sglp-coins-100y-<seed>      coins/run.sh 100 sglp-r-100y-<seed>.graph '1 year' <seed> log 50 50
#   cg <seed> <S> <C>    sglp-coins-grid-sS-cC-<seed> coins/run.sh 100 sglp-sweep-h60-w70-<seed>.graph '1 year' <seed> log S C
set -u
WT=/Users/udi/Grassroots/GLP-worktrees/sGLP-runs
T=/Users/udi/Grassroots/tmp/sglp-splitmix
LOCK=/Users/udi/Grassroots/tmp/glp-run.sh
SG=$WT/programs/sglp/social_graph/run.sh
CO=$WT/programs/sglp/coins/run.sh
FA=$WT/programs/sglp/social_graph/friendship.awk
CA=$WT/programs/sglp/coins/circulation.awk
P=$T/progress.txt
say() { echo "$(date -u '+%F %T') UTC $*" >> "$P"; }
cd "$WT" || exit 1
case $1 in
sg) seed=$2; b=sglp-r-100y-$seed
    [ -f "$T/$b.txt" ] && exit 0
    rm -f "$T/$b.log" "$T/$b.graph" "$T/$b.draw" "$T/$b.out"
    say "start $b"
    bash "$LOCK" splitmix-$b bash -c "{ uptime; /usr/bin/time -l bash '$SG' 100 '1 year' '$seed' '$T/$b.log' '$T/$b.graph'; echo \"exit \$?\"; uptime; } > '$T/$b.out' 2>&1"
    grep -q '^exit 0$' "$T/$b.out" || { say "FAILED $b"; exit 1; }
    { grep '^draw homophile' "$T/$b.out"; awk -v n=100 -v months=12 -f "$FA" "$T/$b.log"; } > "$T/$b.txt"
    say "done $b: $(grep '^wall-clock' "$T/$b.out" | cut -d';' -f1); $(grep '^edges' "$T/$b.txt" | head -1)" ;;
mix) seed=$2 H=$3 W=$4; b=sglp-sweep-h$H-w$W-$seed
    [ -f "$T/$b.txt" ] && exit 0
    rm -f "$T/$b.log" "$T/$b.graph" "$T/$b.draw" "$T/$b.out"
    say "start $b"
    bash "$LOCK" splitmix-$b bash -c "{ echo \"start \$(date -u '+%F %T') UTC\"; /usr/bin/time -p bash '$SG' 100 '1 year' '$seed' '$T/$b.log' '$T/$b.graph' '$H' '$W' 2>&1; echo \"exit \$?\"; echo \"end \$(date -u '+%F %T') UTC\"; } > '$T/$b.out' 2>&1"
    grep -q '^exit 0$' "$T/$b.out" || { say "FAILED $b"; exit 1; }
    { grep '^draw homophile' "$T/$b.out"; awk -v n=100 -v months=12 -f "$FA" "$T/$b.log"; } > "$T/$b.txt"
    say "done $b: $(grep '^wall-clock' "$T/$b.out" | cut -d';' -f1); $(grep '^edges' "$T/$b.txt" | head -1)" ;;
co|cg)
    if [ "$1" = co ]; then seed=$2 S=50 C=50; b=sglp-coins-100y-$seed; g=$T/sglp-r-100y-$seed.graph
    else seed=$2 S=$3 C=$4; b=sglp-coins-grid-s$S-c$C-$seed; g=$T/sglp-sweep-h60-w70-$seed.graph; fi
    [ -f "$T/$b.txt" ] && exit 0
    [ -f "$g" ] || { say "FAILED $b: no graph $g"; exit 1; }
    rm -f "$T/$b.log" "$T/$b.draw" "$T/$b.out"
    say "start $b"
    bash "$LOCK" splitmix-$b bash -c "{ bash '$CO' 100 '$g' '1 year' '$seed' '$T/$b.log' '$S' '$C'; echo \"exit \$?\"; } > '$T/$b.out' 2>&1"
    if grep -q '^exit 2$' "$T/$b.out" && grep -q 'is on no edge of the graph' "$T/$b.out"; then
        say "LEFT OUT $b: $(grep '^run\.sh: ' "$T/$b.out" | tr '\n' ' ')"; exit 0; fi
    grep -q '^exit 0$' "$T/$b.out" || { say "FAILED $b"; exit 1; }
    awk -f "$CA" "$T/$b.log" > "$T/$b.txt"
    say "done $b: $(grep '^wall-clock' "$T/$b.out" | cut -d';' -f1); $(grep '^circulation' "$T/$b.txt" | tail -1)" ;;
esac
