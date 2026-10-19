#!/bin/bash
# A run of a canonical CVA platform under sGLP (sGLP's task of 2026-10-18
# 20:50 UTC, item 2; sGLP's paper, Section 4, "The run" and "The log";
# Appendix B) through the REPL: the harness of every canonical platform,
# self.glp.in and harness.glp.in beside this script, with the platform's
# canonical compilation and an owner's printed profiles and population.
#
#   bash programs/sglp/cva/run.sh <platform.glp> <printed> <agents> <until> <seed> <log>
#
#   <platform.glp>  the platform's canonical compilation, as its cascade record
#                   holds it (cascade/<NAME>/platform.glp);
#   <printed>       a directory holding profiles.glp and population.glp, printed by
#                   programs/sglp/transform.sh --interface from the owner's sGLP
#                   source over the platform's interface;
#   <agents>        the number of agents, N >= 1, keyed p1 ... pN, each number
#                   zero-padded to the width of N (harness.glp.in);
#   <until>         "<count> <unit>", the horizon, the unit singular or plural;
#   <seed>          the run's seed;
#   <log>           the log, written as the run goes.
#
# It assembles the run's directory, programs/tests/sglp_cva_run_<pid>/, a
# directory of its own whose scope is the root's (the root exposes the monitor
# and programs/sglp/sglp.glp): self.glp and harness.glp from the .in files,
# platform.glp, profiles.glp and population.glp; and removes it at the end.
# The population's draw is computed first, in a REPL session of its own, so
# that the run's machine and its log are the same with it as without, by
# draw(Keys, Seed, D) (harness.glp.in), and written beside the log, to <log>
# with its .log replaced by .draw, or with .draw added where it has none: the
# line "draw <profile> <count> ...", each profile of each dimension in the
# order population.glp declares them, then one line "agent <a> <profile> ..."
# per agent, a of 1..N in order, its profile in each dimension.  The run is
# run(Keys, <count>, <unit>, <seed>), and the log is, as the examples' run
# scripts write it, one line per answer, "<time>\t<agent>\t<type>\t<term>",
# and the monitor's last line, the clock alone.
#
# Prints the goal, the draw's counts, the wall-clock time, the REPL's status
# lines and the log's size.  Exits 0 if the draw listed agents 1..N in order
# and the run loaded, ran with no error and its monitor stopped; 1 otherwise;
# 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

if [ $# -ne 6 ]; then
    echo "usage: run.sh <platform.glp> <printed> <agents> <until> <seed> <log>" >&2
    exit 2
fi
PLATFORM=$1; PRINTED=$2; N=$3; UNTIL=$4; SEED=$5; LOG=$6
read -r COUNT UNIT EXTRA <<< "$UNTIL"
if ! [[ "$N" =~ ^[0-9]+$ ]] || [ "$N" -lt 1 ] || ! [[ "$SEED" =~ ^[0-9]+$ ]] ||
   ! [[ "$COUNT" =~ ^[0-9]+(\.[0-9]+)?$ ]] || [ -z "${UNIT:-}" ] || [ -n "${EXTRA:-}" ]; then
    echo "run.sh: <agents> is a positive integer, <seed> an integer, <until> \"<count> <unit>\"" >&2
    exit 2
fi
for f in "$PLATFORM" "$PRINTED/profiles.glp" "$PRINTED/population.glp" "$HERE/self.glp.in" "$HERE/harness.glp.in"; do
    [ -f "$f" ] || { echo "run.sh: no file $f" >&2; exit 2; }
done

# The keys: p and the agent's number, zero-padded to the width of N.
W=${#N}
KEYS=$(awk -v n="$N" -v w="$W" 'BEGIN { for (a = 1; a <= n; a++) printf "%sp%0*d", (a > 1 ? ", " : "["), w, a; print "]" }')

WORK=$(mktemp -d)
RUNDIR="$GLP_DIR/programs/tests/sglp_cva_run_$$"
trap 'rm -rf "$WORK" "$RUNDIR"' EXIT
mkdir -p "$RUNDIR" || exit 2
cp "$HERE/self.glp.in" "$RUNDIR/self.glp" && cp "$HERE/harness.glp.in" "$RUNDIR/harness.glp" &&
    cp "$PLATFORM" "$RUNDIR/platform.glp" && cp "$PRINTED/profiles.glp" "$RUNDIR/profiles.glp" &&
    cp "$PRINTED/population.glp" "$RUNDIR/population.glp" || exit 2

# The dimensions' profiles, in the order population.glp declares them, the
# lines of each type DimensionK ::= p1 ; p2 ; ... ., a dimension's profiles
# separated by spaces and the dimensions by |.
PROFS=$(awk '
    /^Dimension[0-9]+$/ { d = 1; next }
    d == 1 && $0 == "::=" { d = 2; next }
    d == 2 && $0 == "." { d = 0; printf "|"; next }
    d == 2 && $0 != ";" { printf "%s ", $0; next }
    { d = 0 }' "$RUNDIR/population.glp")

case "$LOG" in
    *.log) DRAWF="${LOG%.log}.draw" ;;
    *) DRAWF="$LOG.draw" ;;
esac
rm -f "$DRAWF"
printf ':limit 1000000000000000\n%s\ndraw(%s, %s, D).\n:quit\n' "$RUNDIR" "$KEYS" "$SEED" > "$WORK/draw"
DRAW=$( (cd "$RT" && bin/glpc < "$WORK/draw" 2>&1) | awk -v n="$N" -v drawf="$DRAWF" -v profs="$PROFS" '
    { sub(/^(GLP> )+/, "") }
    /^D = \[/ && !done {
        done = 1
        s = substr($0, 6)
        k = 0
        while (match(s, /^agent\([0-9]+, profiles\([a-z0-9_, ]*\)\)/)) {
            t = substr(s, 7, RLENGTH - 8)
            i = index(t, ", "); a = substr(t, 1, i - 1); t = substr(t, i + 2)
            if (a + 0 != k + 1) next
            t = substr(t, 10, length(t) - 9)
            m = split(t, p, ", ")
            line[++k] = "agent " a
            for (j = 1; j <= m; j++) { line[k] = line[k] " " p[j]; cnt[p[j]]++ }
            s = substr(s, RLENGTH + 1)
            sub(/^, /, "", s)
        }
        if (s != "]" || k != n + 0) next
        nd = split(profs, ds, "|"); counts = ""
        for (j = 1; j <= nd; j++) {
            np = split(ds[j], ps, " ")
            for (q = 1; q <= np; q++) counts = counts (counts == "" ? "" : " ") ps[q] " " (cnt[ps[q]] + 0)
        }
        print "draw " counts > drawf
        for (i = 1; i <= k; i++) print line[i] > drawf
        close(drawf)
        print counts
    }')

GOAL="run($KEYS, $COUNT, $UNIT, $SEED)"
printf ':limit 1000000000000000\n%s\n%s.\n:quit\n' "$RUNDIR" "$GOAL" > "$WORK/input"
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

echo "program $PLATFORM with $PRINTED: run of $N agents until $UNTIL seed $SEED"
echo "draw ${DRAW:-none: draw/3 gave no answer listing agents 1..$N}"
[ -n "$DRAW" ] && echo "draw $DRAWF: the counts and $N agents' profiles"
echo "wall-clock $((END - START)) s"
cat "$WORK/status"
echo "log $LOG: $(wc -l < "$LOG" | tr -d ' ') lines, $(wc -c < "$LOG" | tr -d ' ') bytes"

OK=1
[ -n "$DRAW" ] || OK=0
grep -q '✓ Loaded program' "$WORK/status" || OK=0
grep -q '^Error' "$WORK/status" && OK=0
grep -q '^no clock' "$WORK/status" && OK=0
[ "$OK" = 1 ] && exit 0 || exit 1
