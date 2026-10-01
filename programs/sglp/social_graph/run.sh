#!/bin/bash
# A run of the social graph (graph.vglp) through the REPL, and its friendship
# graph.
#
#   bash programs/sglp/social_graph/run.sh <agents> <until> <seed> <log> [<graph>]
#
#   bash programs/sglp/social_graph/run.sh 1000 '5 years' 20260927 \
#       /Users/udi/Grassroots/tmp/sg.log /Users/udi/Grassroots/tmp/sg.graph
#
# runs the population of graph.vglp's run declaration with <agents> agents
# until <until> from seed <seed>: graph.vglp itself where these are its run
# declaration's, 1000 agents until 5 years seed 20260927, and otherwise a copy
# of it in a temporary directory with the run declaration's three values
# replaced.  The initial goal is goal.awk's.  The run's log (sGLP 8ddff2e,
# Definition "Interface Variable, Log") goes to <log>.  With <graph>,
# friendship.awk reads the log and the friendship graph at the end of the run
# goes to <graph>: comment lines starting #, then one edge per line, "p q",
# agents p < q by their numbers in the run, sorted.
#
# Prints the run's wall-clock time, the REPL's status lines and the log's
# size, then friendship.awk's counts; exits 0 if the run reached its horizon
# with no error, 1 otherwise.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

if [ $# -lt 4 ]; then
    echo "usage: run.sh <agents> <until> <seed> <log> [<graph>]" >&2
    exit 2
fi
N=$1; UNTIL=$2; SEED=$3; LOG=$4; GRAPH=${5:-}

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# The program: graph.vglp, or a copy with the run declaration's values replaced.
if [ "$N" = 1000 ] && [ "$UNTIL" = "5 years" ] && [ "$SEED" = 20260927 ]; then
    PROG="$HERE"
else
    PROG="$WORK/social_graph"
    mkdir "$PROG"
    sed -e "s/^run 1000 agents /run $N agents /" \
        -e "s/until 5 years seed 20260927\./until $UNTIL seed $SEED./" \
        "$HERE/graph.vglp" > "$PROG/graph.vglp"
    if ! grep -q "^run $N agents " "$PROG/graph.vglp" ||
       ! grep -q "until $UNTIL seed $SEED\.\$" "$PROG/graph.vglp"; then
        echo "run.sh: graph.vglp's run declaration is not the one this script replaces" >&2
        exit 2
    fi
fi

# The REPL's input: no bound on reductions, the log, the program, the goal.
{
    echo ":limit 1000000000000000"
    echo ":log $LOG"
    echo "$PROG"
    awk -v n="$N" -f "$HERE/goal.awk" || exit 2
    echo ":quit"
} > "$WORK/input" || exit 2

# The REPL prints every binding of the goal, the agents' streams among them;
# only its status lines are kept.
START=$(date +%s)
(cd "$RT" && bin/glpc < "$WORK/input" 2>&1) |
    grep -E '^(GLP> )?(✓ Loaded|Error|→ |Simulated time: )' > "$WORK/status"
END=$(date +%s)

echo "program $PROG"
echo "agents $N until $UNTIL seed $SEED"
echo "wall-clock $((END - START)) s"
sed 's/^GLP> //' "$WORK/status"
echo "log $LOG: $(wc -l < "$LOG" | tr -d ' ') entries, $(wc -c < "$LOG" | tr -d ' ') bytes"

OK=1
grep -q '✓ Loaded program' "$WORK/status" || OK=0
grep -q '^Simulated time: ' "$WORK/status" || OK=0
grep -q 'Error' "$WORK/status" && OK=0

if [ -n "$GRAPH" ]; then
    awk -v n="$N" -v edges="$WORK/edges" -f "$HERE/friendship.awk" "$LOG" || OK=0
    touch "$WORK/edges"
    {
        echo "# The friendship graph at the end of a run of the sGLP social graph"
        echo "# (sGLP 8ddff2e, Definition \"Friendship Graph of a Run\"),"
        echo "# programs/sglp/social_graph: $N agents until $UNTIL seed $SEED."
        echo "# Agents are numbered 1..$N as in the run, 1..$((N / 2)) of one sex and the rest"
        echo "# of the other.  One edge per line, \"p q\" with p < q, sorted."
        sort -n -k1,1 -k2,2 "$WORK/edges"
    } > "$GRAPH"
    echo "graph $GRAPH"
fi

[ "$OK" = 1 ] && exit 0 || exit 1
