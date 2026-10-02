#!/bin/bash
# A run of the Grassroots Social Graph in sGLP (programs/sglp, the translated
# form; sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP at d2f64b6,
# Section 4 and Section 5) through the REPL, its log and its friendship graph.
#
#   bash programs/sglp/social_graph/run.sh <agents> <until> <seed> <log> [<graph>]
#
#   bash programs/sglp/social_graph/run.sh 100 '1 year' 20260927 \
#       /Users/udi/Grassroots/tmp/sg.log /Users/udi/Grassroots/tmp/sg.graph
#
# loads programs/sglp and calls social_graph(<agents>, <count>, <unit>, <seed>),
# <until> being "<count> <unit>", the unit singular or plural: the run
# declaration's population (harness.glp) with <agents> agents, an even number,
# until <until> of simulated time from seed <seed>.  The REPL's goal limit is
# set out of reach.
#
# The log (Section 4, "The log"; Appendix B) is the sink this script names: the
# lines the run prints, one per answer as the person processes write them,
# "<time>\t<agent>\t<type>\t<term>", the simulated time in seconds and the
# question's term with the answer in place; and, when the monitor stops, its
# last line, the clock alone.  They are written to <log> as the run goes.  With
# <graph>, friendship.awk reads the log and writes the friendship graph at the
# end of the run (Definition "Friendship Graph of a Run") to <graph>: comment
# lines starting #, then one edge per line, "p q", p < q, sorted.
#
# Prints the run's wall-clock time, the REPL's status lines, the log's size and
# friendship.awk's counts; exits 0 if the run loaded, ran with no error and its
# monitor stopped, 1 otherwise.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SGLP="$(cd "$HERE/.." && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

if [ $# -lt 4 ]; then
    echo "usage: run.sh <agents> <until> <seed> <log> [<graph>]" >&2
    exit 2
fi
N=$1; UNTIL=$2; SEED=$3; LOG=$4; GRAPH=${5:-}
read -r COUNT UNIT EXTRA <<< "$UNTIL"
if ! [[ "$N" =~ ^[0-9]+$ ]] || ! [[ "$SEED" =~ ^[0-9]+$ ]] ||
   ! [[ "$COUNT" =~ ^[0-9]+(\.[0-9]+)?$ ]] || [ -z "${UNIT:-}" ] || [ -n "${EXTRA:-}" ]; then
    echo "run.sh: <agents> and <seed> are integers, <until> is \"<count> <unit>\"" >&2
    exit 2
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# The REPL's input: no bound on reductions, the program, the goal.
printf ':limit 1000000000000000\n%s\nsocial_graph(%s, %s, %s, %s).\n:quit\n' \
    "$SGLP" "$N" "$COUNT" "$UNIT" "$SEED" > "$WORK/input"

# The REPL's output: the log's lines go to <log>, tab-separated, as they come;
# the status lines to $WORK/status.  A line may carry the prompt "GLP> ".
: > "$LOG" || exit 2
START=$(date +%s)
(cd "$RT" && bin/glpc < "$WORK/input" 2>&1) | awk -v logf="$LOG" -v status="$WORK/status" '
    { sub(/^(GLP> )+/, "") }
    /^entry\(/ {
        # entry(T, A, Type, Term): T, A and Type hold no comma.
        s = substr($0, 7, length($0) - 7)
        i = index(s, ", "); t = substr(s, 1, i - 1); s = substr(s, i + 2)
        i = index(s, ", "); a = substr(s, 1, i - 1); s = substr(s, i + 2)
        i = index(s, ", "); ty = substr(s, 1, i - 1); s = substr(s, i + 2)
        printf "%s\t%s\t%s\t%s\n", t, a, ty, s > logf; fflush(logf)
        next
    }
    /^clock\(/ {
        printf "%s\n", substr($0, 7, length($0) - 7) > logf; fflush(logf)
        clocked = 1
        next
    }
    /^(✓ Loaded|Error|→ |\[CERTIFICATE)/ { print > status }
    END { if (!clocked) print "no clock: the monitor did not stop" > status }
'
END=$(date +%s)
touch "$LOG" "$WORK/status"

echo "program $SGLP: social_graph($N, $COUNT, $UNIT, $SEED)"
echo "wall-clock $((END - START)) s; $(uptime | sed 's/.*load/load/')"
cat "$WORK/status"
echo "log $LOG: $(wc -l < "$LOG" | tr -d ' ') lines, $(wc -c < "$LOG" | tr -d ' ') bytes"

OK=1
grep -q '✓ Loaded program' "$WORK/status" || OK=0
grep -q '^Error' "$WORK/status" && OK=0
grep -q '^no clock' "$WORK/status" && OK=0

if [ -n "$GRAPH" ]; then
    awk -v n="$N" -v edges="$WORK/edges" -f "$HERE/friendship.awk" "$LOG" || OK=0
    touch "$WORK/edges"
    LAST=$(tail -n 1 "$LOG" | cut -f1)
    if grep -q '^no clock' "$WORK/status"; then
        AT="the run did not end; the graph is at its log's last line, simulated time $LAST s"
    else
        AT="the run ended at simulated time $LAST s"
    fi
    {
        echo "# The friendship graph of a run of the sGLP social graph"
        echo "# (sGLP d2f64b6, Definition \"Friendship Graph of a Run\"),"
        echo "# programs/sglp/social_graph: $N agents until $UNTIL seed $SEED;"
        echo "# $AT."
        echo "# Agents are numbered 1..$N as in the run, 1..$((N / 2)) of one sex and the rest"
        echo "# of the other.  One edge per line, \"p q\" with p < q, sorted."
        sort -n -k1,1 -k2,2 "$WORK/edges"
    } > "$GRAPH"
    echo "graph $GRAPH"
fi

[ "$OK" = 1 ] && exit 0 || exit 1
