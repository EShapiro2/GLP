#!/bin/bash
# A run of the Grassroots Social Graph in sGLP (programs/sglp, the translated
# form; sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP at d2f64b6,
# Section 4 and Section 5) through the REPL, its log and its friendship graph.
#
#   bash programs/sglp/social_graph/run.sh <agents> <until> <seed> <log>
#       [<graph> [<hom> [<wary>]]]
#
#   bash programs/sglp/social_graph/run.sh 100 '1 year' 20260927 \
#       /Users/udi/Grassroots/tmp/sg.log /Users/udi/Grassroots/tmp/sg.graph
#   bash programs/sglp/social_graph/run.sh 100 '1 year' 20260927 \
#       /Users/udi/Grassroots/tmp/sg.log /Users/udi/Grassroots/tmp/sg.graph 0 100
#
# loads programs/sglp and calls social_graph(<agents>, <count>, <unit>, <seed>),
# <until> being "<count> <unit>", the unit singular or plural: the run
# declaration's population (harness.glp) with <agents> agents, an even number,
# until <until> of simulated time from seed <seed>.  Given <hom>, and <wary>
# after it, integers 0..100, it calls social_graph(<agents>, <count>, <unit>,
# <seed>, <hom>, <wary>) instead: the population with that mix, an agent being
# homophile for its first draw K =< <hom> and wary for its second K =< <wary>,
# K of 1..100.  They default to 60 and 70, the run declaration's mix and
# social_graph/4's: with neither, the call is social_graph/4; with <hom> alone,
# <wary> is 70.  <graph> may be '' for none.  The REPL's goal limit is set out
# of reach.
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
# Prints the goal; the population's draw, "draw homophile <h> indifferent <i>
# wary <w> sociable <s>", the number of agents of each profile in each
# dimension, which the log does not show (sGLP's code task of 2026-10-02 15:44
# UTC), computed by social_graph_draw/5 in a REPL session of its own before the
# run, so that the log is the same with it as without; the run's wall-clock
# time; the REPL's status lines; the log's size; and friendship.awk's counts.
# Writes the draw itself beside the log, to <log> with its .log replaced by
# .draw, or with .draw added where it has none (sGLP's code task of 2026-10-03
# 08:58 UTC): the line "draw homophile <h> indifferent <i> wary <w> sociable
# <s>", then one line "agent <a> <approach> <response>" per agent, a of 1..N in
# order, all from the one answer of social_graph_draw/5, whose counts are those
# of its agents.  Exits 0 if the draw was computed, every agent of 1..N in its
# list, and the run loaded, ran with no error and its monitor stopped, 1
# otherwise.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SGLP="$(cd "$HERE/.." && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

if [ $# -lt 4 ] || [ $# -gt 7 ]; then
    echo "usage: run.sh <agents> <until> <seed> <log> [<graph> [<hom> [<wary>]]]" >&2
    exit 2
fi
N=$1; UNTIL=$2; SEED=$3; LOG=$4; GRAPH=${5:-}; HOM=${6:-60}; WARY=${7:-70}
read -r COUNT UNIT EXTRA <<< "$UNTIL"
if ! [[ "$N" =~ ^[0-9]+$ ]] || ! [[ "$SEED" =~ ^[0-9]+$ ]] ||
   ! [[ "$COUNT" =~ ^[0-9]+(\.[0-9]+)?$ ]] || [ -z "${UNIT:-}" ] || [ -n "${EXTRA:-}" ]; then
    echo "run.sh: <agents> and <seed> are integers, <until> is \"<count> <unit>\"" >&2
    exit 2
fi
if ! [[ "$HOM" =~ ^[0-9]+$ ]] || ! [[ "$WARY" =~ ^[0-9]+$ ]] ||
   [ "$HOM" -gt 100 ] || [ "$WARY" -gt 100 ]; then
    echo "run.sh: <hom> and <wary> are integers 0..100" >&2
    exit 2
fi
if [ $# -ge 6 ]; then
    GOAL="social_graph($N, $COUNT, $UNIT, $SEED, $HOM, $WARY)"
else
    GOAL="social_graph($N, $COUNT, $UNIT, $SEED)"
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# The population's draw, in a REPL session of its own before the run, so that
# the run's machine and its log are the same with it as without: the program,
# social_graph_draw/5 with the run's agents, seed and mix, its answer read,
# D = draw(H, I, W, S, [agent(1, Ap, Re), ...]).  The counts are DRAW, and the
# counts line and the agents' lines go to the .draw file beside the log; with
# an answer that does not list agents 1..N in order, DRAW is empty and the file
# is not written.
case "$LOG" in
    *.log) DRAWF="${LOG%.log}.draw" ;;
    *) DRAWF="$LOG.draw" ;;
esac
rm -f "$DRAWF"
printf ':limit 1000000000000000\n%s\nsocial_graph_draw(%s, %s, %s, %s, D).\n:quit\n' \
    "$SGLP" "$N" "$SEED" "$HOM" "$WARY" > "$WORK/draw"
DRAW=$( (cd "$RT" && bin/glpc < "$WORK/draw" 2>&1) | awk -v n="$N" -v drawf="$DRAWF" '
    { sub(/^(GLP> )+/, "") }
    /^D = draw\(/ && !done {
        done = 1
        s = substr($0, 10)
        if (!match(s, /^[0-9]+, [0-9]+, [0-9]+, [0-9]+, \[/)) next
        split(substr(s, 1, RLENGTH - 3), c, ", ")
        s = substr(s, RLENGTH + 1)
        k = 0
        while (match(s, /^agent\([0-9]+, (homophile|indifferent), (wary|sociable)\)/)) {
            split(substr(s, 7, RLENGTH - 7), f, ", ")
            if (f[1] + 0 != k + 1) next
            line[++k] = "agent " f[1] " " f[2] " " f[3]
            s = substr(s, RLENGTH + 1)
            sub(/^, /, "", s)
        }
        if (s != "])" || k != n + 0) next
        counts = "homophile " c[1] " indifferent " c[2] " wary " c[3] " sociable " c[4]
        print "draw " counts > drawf
        for (i = 1; i <= k; i++) print line[i] > drawf
        close(drawf)
        print counts
    }')

# The REPL's input: no bound on reductions, the program, the goal.
printf ':limit 1000000000000000\n%s\n%s.\n:quit\n' "$SGLP" "$GOAL" > "$WORK/input"

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

echo "program $SGLP: $GOAL"
echo "draw ${DRAW:-none: social_graph_draw/5 gave no answer listing agents 1..$N}"
[ -n "$DRAW" ] && echo "draw $DRAWF: the counts and $N agents' profiles"
echo "wall-clock $((END - START)) s; $(uptime | sed 's/.*load/load/')"
cat "$WORK/status"
echo "log $LOG: $(wc -l < "$LOG" | tr -d ' ') lines, $(wc -c < "$LOG" | tr -d ' ') bytes"

OK=1
[ -n "$DRAW" ] || OK=0
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
        echo "# programs/sglp/social_graph: $N agents until $UNTIL seed $SEED, the mix"
        echo "# homophile for K =< $HOM and wary for K =< $WARY, K of 1..100;"
        echo "# $AT."
        echo "# Agents are numbered 1..$N as in the run, 1..$((N / 2)) of one sex and the rest"
        echo "# of the other.  One edge per line, \"p q\" with p < q, sorted."
        sort -n -k1,1 -k2,2 "$WORK/edges"
    } > "$GRAPH"
    echo "graph $GRAPH"
fi

[ "$OK" = 1 ] && exit 0 || exit 1
