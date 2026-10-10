#!/bin/bash
# The transformation of the sGLP sources of the two examples and of the two
# run tests into GLP (programs/sglp, transform.glp; sGLP's paper, the
# repository svGLP-Stochastic-Volitional-GLP, Section 4, "The transformation"),
# through the REPL.
#
#   bash programs/sglp/transform.sh            print into the programs' directories
#   bash programs/sglp/transform.sh <dir>      print into <dir>/social_graph/, <dir>/coins/,
#                                              <dir>/tests/reader/ and <dir>/tests/delivery/
#   bash programs/sglp/transform.sh --check    print into a scratch directory and compare
#
# In one REPL session it loads programs/sglp and calls transform(P, Part) for
# P social_graph, coins, reader and delivery and Part profiles and population,
# and writes what each prints between its lines "%% transform begin" and
# "%% transform end" to its directory's Part.glp: social_graph/profiles.glp and
# social_graph/population.glp, from social_graph/graph_sglp.glp;
# coins/profiles.glp and coins/population.glp, from coins/coins_sglp.glp; and
# the run tests' (xi) and (xii), tests/reader/ from rating_sglp.glp and
# tests/delivery/ from chat_sglp.glp, the run tests' programs holding no call
# of the transformation (sGLP's task 6 of 2026-10-10 07:59 UTC).  The program
# it loads includes the printed files, which the harnesses call, so they are
# printed by the program that holds them, the files it prints being those it
# loaded where the sources are unchanged; --check says whether they are.
#
# Prints one line per file.  Exits 0 if all eight were printed, with no fault
# of the checks (a line "%% not transformed: ..."), and, with --check, each is
# byte for byte the file in place; 1 otherwise, the faults or the differing
# files named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

CHECK=0
if [ $# -gt 1 ]; then
    echo "usage: transform.sh [<dir> | --check]" >&2
    exit 2
fi
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
if [ $# -eq 0 ]; then
    OUT="$HERE"
elif [ "$1" = "--check" ]; then
    CHECK=1
    OUT="$WORK/out"
else
    OUT="$1"
fi
mkdir -p "$OUT/social_graph" "$OUT/coins" "$OUT/tests/reader" "$OUT/tests/delivery" || exit 2

PARTS="social_graph/profiles social_graph/population coins/profiles coins/population tests/reader/profiles tests/reader/population tests/delivery/profiles tests/delivery/population"
{
    echo ':limit 1000000000000000'
    echo "$HERE"
    for p in $PARTS; do
        prog=${p%/*}
        echo "transform(${prog##*/}, ${p##*/})."
    done
    echo ':quit'
} > "$WORK/input"

(cd "$RT" && bin/glpc < "$WORK/input" > "$WORK/repl.out" 2>&1)

# The REPL's output: the lines between each "%% transform begin" and its
# "%% transform end", in the order of the goals, to the parts' files.  A line
# may carry the prompt "GLP> ".
awk -v out="$OUT" -v parts="$PARTS" '
    BEGIN { n = split(parts, p, " "); k = 0 }
    { sub(/^(GLP> )+/, "") }
    $0 == "%% transform begin" { k++; f = out "/" p[k] ".glp"; printf "" > f; on = 1; next }
    $0 == "%% transform end" && on { close(f); on = 0; done[k] = 1; next }
    on { print > f; if ($0 ~ /^%% not transformed/) print "fault " p[k] ": " $0 }
    END {
        for (i = 1; i <= n; i++) if (!(i in done)) print "missing " p[i]
    }
' "$WORK/repl.out" > "$WORK/status"

OK=1
if [ -s "$WORK/status" ]; then
    cat "$WORK/status"
    grep -q '^✓ Loaded program' "$WORK/repl.out" || grep -E '^(GLP> )*Error' "$WORK/repl.out" | head -20
    OK=0
fi
for p in $PARTS; do
    f="$OUT/$p.glp"
    if [ "$CHECK" = 1 ]; then
        if [ -f "$f" ] && cmp -s "$f" "$HERE/$p.glp"; then
            echo "$p.glp: as printed"
        else
            echo "$p.glp: NOT as printed"
            [ -f "$f" ] && diff "$HERE/$p.glp" "$f" | head -20
            OK=0
        fi
    elif [ -f "$f" ]; then
        echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
