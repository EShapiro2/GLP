#!/bin/bash
# A run of coins among friends in sGLP (programs/sglp, the translated form;
# sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP at c7d0b2f,
# Section 4 and Section 6; sGLP's code task of 2026-10-02 15:24 UTC, item 3,
# and of 2026-10-03 09:45 UTC) through the REPL, on a friendship graph: its
# log, its draw and its circulation.
#
#   bash programs/sglp/coins/run.sh <agents> <graph> <until> <seed> <log>
#       [<spender%> [<cautious%>]]
#
#   bash programs/sglp/coins/run.sh 100 \
#       /Users/udi/Grassroots/tmp/sglp-r-100y-20260927.graph '1 year' \
#       20260927 /Users/udi/Grassroots/tmp/coins.log
#
# reads <graph>, a friendship graph as the social graph's run.sh writes it:
# comment lines starting #, then one edge per line, "p q".  It refuses a graph
# with an edge outside 1..<agents>, or with an agent of 1..<agents> on no
# edge, whose friends would be the empty list on which one_of is never called
# (Section 5; Appendix B).  Agent a's friends are the agents it shares an edge
# with, in increasing order.  It loads programs/sglp and calls coins(<agents>,
# <friends>, <count>, <unit>, <seed>, <spender%>, <cautious%>), <until> being
# "<count> <unit>", the unit singular or plural, and <friends> the agents'
# friends lists in order: the run declaration's population (harness.glp) with
# <agents> agents on that graph until <until> of simulated time from seed
# <seed>, an agent being a spender for its first draw K =< <spender%> and
# cautious for its second K =< <cautious%>, K of 1..100.  They default to 50
# and 50, the run declaration's mix.  The REPL's goal limit is set out of
# reach.
#
# The log (Section 4, "The log"; Appendix B) is the sink this script names:
# the lines the run prints, one per answer as the person processes write them,
# "<time>\t<agent>\t<type>\t<term>", the simulated time in seconds, the type
# Menu or Offer, and the question's term with the answer in place ---
# menu(Fs, W, pay(F, K)), menu(Fs, W, swap(F, K)), offer(From, K, Held, yes)
# or offer(From, K, Held, no); and, when the monitor stops, its last line, the
# clock alone.  They are written to <log> as the run goes.
#
# Prints the goal, its friends abbreviated; the graph's edges; the
# population's draw, "draw spender <s> saver <v> generous <g> cautious <c>",
# the number of agents of each profile in each dimension, which the log does
# not show, computed by coins_draw/5 in a REPL session of its own before the
# run, so that the log is the same with it as without (sGLP's code task of
# 2026-10-03 09:45 UTC); the run's wall-clock and CPU time and the machine's
# load as it ends; the REPL process's instructions retired and peak memory
# footprint (/usr/bin/time -l); the REPL's status lines; the log's size; and
# circulation.awk's months and totals on the log.  Writes the draw itself
# beside the log, to <log> with its .log replaced by .draw, or with .draw added
# where it has none, as the social graph's run.sh does: the line "draw spender
# <s> saver <v> generous <g> cautious <c>", then one line "agent <a>
# <spending> <credit>" per agent, a of 1..N in order, all from the one answer
# of coins_draw/5, whose counts are those of its agents.
# Exits 0 if the draw was computed, every agent of 1..N in its list, and the
# run loaded, ran with no error and its monitor stopped, 1 otherwise, and 2 on
# a bad argument or graph, before running.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SGLP="$(cd "$HERE/.." && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

if [ $# -lt 5 ] || [ $# -gt 7 ]; then
    echo "usage: run.sh <agents> <graph> <until> <seed> <log> [<spender%> [<cautious%>]]" >&2
    exit 2
fi
N=$1; GRAPH=$2; UNTIL=$3; SEED=$4; LOG=$5; SPEND=${6:-50}; CRED=${7:-50}
read -r COUNT UNIT EXTRA <<< "$UNTIL"
if ! [[ "$N" =~ ^[0-9]+$ ]] || [ "$N" -lt 1 ] || ! [[ "$SEED" =~ ^[0-9]+$ ]] ||
   ! [[ "$COUNT" =~ ^[0-9]+(\.[0-9]+)?$ ]] || [ -z "${UNIT:-}" ] || [ -n "${EXTRA:-}" ]; then
    echo "run.sh: <agents> is a positive integer, <seed> an integer, <until> \"<count> <unit>\"" >&2
    exit 2
fi
if ! [[ "$SPEND" =~ ^[0-9]+$ ]] || ! [[ "$CRED" =~ ^[0-9]+$ ]] ||
   [ "$SPEND" -gt 100 ] || [ "$CRED" -gt 100 ]; then
    echo "run.sh: <spender%> and <cautious%> are integers 0..100" >&2
    exit 2
fi
if [ ! -r "$GRAPH" ]; then
    echo "run.sh: cannot read the graph $GRAPH" >&2
    exit 2
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# The friends lists from the graph: "[[f, ...], ...]" on one line, agent a's
# the a-th, in increasing order; or the refusal, on stderr.
awk -v n="$N" -v friends="$WORK/friends" '
    /^#/ || NF == 0 { next }
    NF != 2 || $1 !~ /^[0-9]+$/ || $2 !~ /^[0-9]+$/ {
        printf "run.sh: line %d of the graph is not an edge \"p q\": %s\n", NR, $0 > "/dev/stderr"
        bad = 1; next
    }
    $1 < 1 || $1 > n || $2 < 1 || $2 > n {
        printf "run.sh: the edge %s %s is outside 1..%d\n", $1, $2, n > "/dev/stderr"
        bad = 1; next
    }
    {
        p = $1 + 0; q = $2 + 0; edges++
        if (!((p, q) in f)) { f[p, q] = 1; d[p]++; l[p, d[p]] = q }
        if (!((q, p) in f)) { f[q, p] = 1; d[q]++; l[q, d[q]] = p }
    }
    END {
        for (a = 1; a <= n; a++)
            if (!(a in d)) {
                printf "run.sh: agent %d is on no edge of the graph, so it has no friends\n", a > "/dev/stderr"
                bad = 1
            }
        if (bad) exit 1
        s = "["
        for (a = 1; a <= n; a++) {
            # the a-th list, sorted increasing (insertion sort; degrees are small)
            k = d[a]
            for (i = 1; i <= k; i++) x[i] = l[a, i]
            for (i = 2; i <= k; i++) {
                v = x[i]; j = i - 1
                while (j >= 1 && x[j] > v) { x[j + 1] = x[j]; j-- }
                x[j + 1] = v
            }
            s = s (a > 1 ? ", " : "") "["
            for (i = 1; i <= k; i++) s = s (i > 1 ? ", " : "") x[i]
            s = s "]"
        }
        print s "]" > friends
        print edges
    }
' "$GRAPH" > "$WORK/edges" || exit 2
EDGES=$(cat "$WORK/edges")
FRIENDS=$(cat "$WORK/friends")
GOAL="coins($N, $FRIENDS, $COUNT, $UNIT, $SEED, $SPEND, $CRED)"

# The population's draw, in a REPL session of its own before the run, so that
# the run's machine and its log are the same with it as without: the program,
# coins_draw/5 with the run's agents, seed and mix, its answer read,
# D = draw(Sp, Sv, G, C, [agent(1, Spending, Credit), ...]).  The counts are
# DRAW, and the counts line and the agents' lines go to the .draw file beside
# the log; with an answer that does not list agents 1..N in order, DRAW is
# empty and the file is not written.
case "$LOG" in
    *.log) DRAWF="${LOG%.log}.draw" ;;
    *) DRAWF="$LOG.draw" ;;
esac
rm -f "$DRAWF"
printf ':limit 1000000000000000\n%s\ncoins_draw(%s, %s, %s, %s, D).\n:quit\n' \
    "$SGLP" "$N" "$SEED" "$SPEND" "$CRED" > "$WORK/draw"
DRAW=$( (cd "$RT" && bin/glpc < "$WORK/draw" 2>&1) | awk -v n="$N" -v drawf="$DRAWF" '
    { sub(/^(GLP> )+/, "") }
    /^D = draw\(/ && !done {
        done = 1
        s = substr($0, 10)
        if (!match(s, /^[0-9]+, [0-9]+, [0-9]+, [0-9]+, \[/)) next
        split(substr(s, 1, RLENGTH - 3), c, ", ")
        s = substr(s, RLENGTH + 1)
        k = 0
        while (match(s, /^agent\([0-9]+, (spender|saver), (generous|cautious)\)/)) {
            split(substr(s, 7, RLENGTH - 7), f, ", ")
            if (f[1] + 0 != k + 1) next
            line[++k] = "agent " f[1] " " f[2] " " f[3]
            s = substr(s, RLENGTH + 1)
            sub(/^, /, "", s)
        }
        if (s != "])" || k != n + 0) next
        counts = "spender " c[1] " saver " c[2] " generous " c[3] " cautious " c[4]
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
(cd "$RT" && /usr/bin/time -l -p -o "$WORK/time" bin/glpc < "$WORK/input" 2>&1) |
awk -v logf="$LOG" -v status="$WORK/status" '
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
LOAD=$(uptime | sed 's/.*load/load/')
touch "$LOG" "$WORK/status" "$WORK/time"

echo "program $SGLP: coins($N, <the friends of $GRAPH>, $COUNT, $UNIT, $SEED, $SPEND, $CRED)"
echo "graph $GRAPH: $EDGES edges, agents 1..$N each on one at least"
echo "draw ${DRAW:-none: coins_draw/5 gave no answer listing agents 1..$N}"
[ -n "$DRAW" ] && echo "draw $DRAWF: the counts and $N agents' profiles"
echo "wall-clock $((END - START)) s; cpu $(awk '$1 == "user" { u = $2 } $1 == "sys" { s = $2 } END { printf "user %s s, sys %s s", u, s }' "$WORK/time"); $LOAD"
echo "$(awk '$2 == "instructions" && $3 == "retired" { i = $1 } $2 == "peak" { p = $1 } END { printf "instructions retired %s; peak memory footprint %s bytes", i, p }' "$WORK/time")"
cat "$WORK/status"
echo "log $LOG: $(wc -l < "$LOG" | tr -d ' ') lines, $(wc -c < "$LOG" | tr -d ' ') bytes"

OK=1
[ -n "$DRAW" ] || OK=0
grep -q '✓ Loaded program' "$WORK/status" || OK=0
grep -q '^Error' "$WORK/status" && OK=0
grep -q '^no clock' "$WORK/status" && OK=0

awk -f "$HERE/circulation.awk" "$LOG" || OK=0

[ "$OK" = 1 ] && exit 0 || exit 1
