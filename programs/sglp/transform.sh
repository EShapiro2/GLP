#!/bin/bash
# The transformation of the sGLP sources of the two examples and of the eight
# run tests into GLP (programs/sglp, transform.glp; sGLP's paper, the
# repository svGLP-Stochastic-Volitional-GLP, Section 4, "The transformation"),
# through the REPL.
#
#   bash programs/sglp/transform.sh            print into the programs' directories
#   bash programs/sglp/transform.sh <dir>      print into <dir>/social_graph/, <dir>/coins/,
#                                              <dir>/tests/reader/, <dir>/tests/delivery/,
#                                              <dir>/tests/receipt/, <dir>/tests/panel/,
#                                              <dir>/tests/numbers/, <dir>/tests/steps/,
#                                              <dir>/tests/screen/ and <dir>/tests/census/
#   bash programs/sglp/transform.sh --check    print into a scratch directory and compare
#   bash programs/sglp/transform.sh --source <src> [--check]
#                                              print <src>/profiles.glp and <src>/population.glp
#                                              from the sGLP source in <src>, a directory
#                                              outside programs/sglp, or with --check compare
#   bash programs/sglp/transform.sh --interface <iface> --source <file> --out <dir> [--check]
#                                              print <dir>/profiles.glp and <dir>/population.glp
#                                              from an owner's sGLP source <file> over a
#                                              canonical CVA platform and the platform's
#                                              interface <iface>, or with --check compare
#
# In one REPL session it loads programs/sglp and calls transform(P, Part) for
# P social_graph, coins, reader, delivery, receipt, panel, numbers, steps, screen and census and Part profiles
# and population, and writes what each prints between its lines "%% transform
# begin" and "%% transform end" to its directory's Part.glp:
# social_graph/profiles.glp and social_graph/population.glp, from
# social_graph/graph_sglp.glp; coins/profiles.glp and coins/population.glp,
# from coins/coins_sglp.glp; and the run tests' (xi) to (xiv), tests/reader/
# from rating_sglp.glp, tests/delivery/ from chat_sglp.glp, tests/receipt/ from
# receipt_sglp.glp, tests/panel/ from show_sglp.glp, tests/numbers/ from
# total_sglp.glp (sGLP's task of 2026-10-14 09:46 UTC, item 3), tests/steps/ from
# steps_sglp.glp (sGLP's task of 2026-10-18 20:52 UTC, item 1), and tests/screen/ from
# notes_sglp.glp and tests/census/ from village_sglp.glp (sGLP's task of 2026-10-20
# 08:00 UTC, item 7), the run tests' programs
# holding no call of the transformation (sGLP's tasks 6 and 8 of 2026-10-10
# 07:59 and 09:33 UTC).  The program it loads includes the printed files,
# which the harnesses call, so they are printed by the program that holds
# them, the files it prints being those it loaded where the sources are
# unchanged; --check says whether they are.
#
# Prints one line per file.  Exits 0 if all twenty were printed, with no fault
# of the checks (a line "%% not transformed: ..."), and, with --check, each is
# byte for byte the file in place; 1 otherwise, the faults or the differing
# files named; 2 on a bad argument.
#
# With --source <src> it transforms a program whose directory is outside
# programs/sglp, as CSSN's simulation (Integration's ruling of 2026-10-16 10:52
# UTC, item 1, with sGLP's agreement of 2026-10-15 21:28 UTC): the sGLP source
# is <src>'s one file *_sglp.glp, in the term form (README.md, "The term
# form"), holding the declaration exported procedure source(SglpTerms,
# SglpTerms) and the unit clause source(V, S) and nothing else but comments; in
# the same REPL session it calls transform_terms(V, S, Part) for Part profiles
# and population, V and S the clause's arguments as written, and writes the two
# parts to <src>, or with --check compares them with those in <src>.  The
# printed modules call the monitor and name sglp.glp's Span and Slot, which the
# root programs/self.glp exposes, as it exposes sglp.glp's span/2 to the harness.
#
# With --interface <iface> --source <file> --out <dir> it transforms an owner's
# sGLP source over a canonical CVA platform (sGLP's task of 2026-10-18 20:50
# UTC, item 1; claude.md, "Code", the simulation of a platform under sGLP):
# <file>, the owner's <NAME>_sglp.glp, holds the declaration exported procedure
# source(SglpTerms) and the unit clause source(S), S the sGLP program in the
# term form and nothing of the platform's interface, which is <iface>, the
# interface the cascade prints beside the platform's .vglp
# (programs/sglp/cva/interface.sh), holding the declaration exported procedure
# interface(SglpTerms) and the unit clause interface(V); in one REPL session it
# calls transform_terms(V, S, Part) for Part cva_profiles and cva_population,
# the profiles and the population of a canonical platform, whose keys are its
# Key, the population with the run's entries of one arity that the harness of
# every canonical platform calls (programs/sglp/cva/), and writes the two parts to
# <dir> as profiles.glp and population.glp, or with --check compares them with
# those in <dir>.
# The parts are written to <dir> and not beside the source, since a .glp in an
# owner's directory is a module of the owner's program.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

usage() {
    echo "usage: transform.sh [<dir> | --check | --source <src> [--check] | --interface <iface> --source <file> --out <dir> [--check]]" >&2
    exit 2
}
CHECK=0
SRC=
IFACE=
# term_of <file> <name> <arity>: the arguments of the one unit clause
# <name>(...) of <file>, its comments dropped and its whitespace one space,
# quoted atoms and strings kept as written, where what is left is its
# declaration exported procedure <name>(SglpTerms, ...) and the clause.
term_of() {
    TERM_NAME="$2" TERM_ARITY="$3" perl -0777 -ne '
        my ($name, $n) = ($ENV{TERM_NAME}, $ENV{TERM_ARITY});
        my $q = qr/\x27(?:[^\x27\\]|\\.)*\x27|"(?:[^"\\]|\\.)*"/;
        s/($q)|%[^\n]*|(\s+)/defined $1 ? $1 : defined $2 ? " " : ""/ge;
        my $ts = join("\\s*,\\s*", ("SglpTerms") x $n);
        s/^\s*(?:exported\s+)?procedure\s+$name\s*\(\s*$ts\s*\)\s*\.//;
        print $1 if /^\s*$name\s*\((.*)\)\s*\.\s*$/s;
    ' "$1"
}
if [ $# -ge 1 ] && [ "$1" = "--interface" ]; then
    { { [ $# -eq 6 ] || { [ $# -eq 7 ] && [ "$7" = "--check" ]; }; } &&
      [ "$3" = "--source" ] && [ "$5" = "--out" ]; } || usage
    [ -f "$2" ] || { echo "transform.sh: no interface $2" >&2; exit 2; }
    [ -f "$4" ] || { echo "transform.sh: no sGLP source $4" >&2; exit 2; }
    [ -d "$6" ] || { echo "transform.sh: no directory $6" >&2; exit 2; }
    IFACE="$2"; SRCF="$4"; DEST="$(cd "$6" && pwd)"
    [ $# -eq 7 ] && CHECK=1
fi
if [ -n "$IFACE" ]; then
    V=$(term_of "$IFACE" interface 1)
    S=$(term_of "$SRCF" source 1)
    if [ -z "$V" ]; then
        echo "transform.sh: $IFACE is not the declaration and the clause interface(V)" >&2
        exit 2
    fi
    if [ -z "$S" ]; then
        echo "transform.sh: $SRCF is not the declaration and the clause source(S)" >&2
        exit 2
    fi
fi
if [ -n "$IFACE" ]; then
    :
elif [ $# -ge 1 ] && [ "$1" = "--source" ]; then
    { [ $# -eq 2 ] || { [ $# -eq 3 ] && [ "$3" = "--check" ]; }; } || usage
    [ -d "$2" ] || { echo "transform.sh: no directory $2" >&2; exit 2; }
    SRC="$(cd "$2" && pwd)"
    [ $# -eq 3 ] && CHECK=1
elif [ $# -gt 1 ]; then
    usage
fi
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

if [ -n "$IFACE" ]; then
    if [ "$CHECK" = 1 ]; then OUT="$WORK/out"; else OUT="$DEST"; fi
    BASE="$DEST"
    mkdir -p "$OUT" || exit 2
    PARTS="profiles population"
    {
        echo ':limit 1000000000000000'
        echo "$HERE"
        echo "transform_terms($V, $S, cva_profiles)."
        echo "transform_terms($V, $S, cva_population)."
        echo ':quit'
    } > "$WORK/input"
elif [ -n "$SRC" ]; then
    # The source: <src>'s one *_sglp.glp, its comments dropped and its
    # whitespace one space, quoted atoms and strings kept as written; what is
    # left must be the declaration and the clause source(V, S), whose arguments
    # are the goal's.
    set -- "$SRC"/*_sglp.glp
    if [ $# -ne 1 ] || [ ! -f "$1" ]; then
        echo "transform.sh: $SRC holds no single *_sglp.glp" >&2
        exit 2
    fi
    VS=$(perl -0777 -ne '
        my $q = qr/\x27(?:[^\x27\\]|\\.)*\x27|"(?:[^"\\]|\\.)*"/;
        s/($q)|%[^\n]*|(\s+)/defined $1 ? $1 : defined $2 ? " " : ""/ge;
        s/^\s*(?:exported\s+)?procedure\s+source\s*\(\s*SglpTerms\s*,\s*SglpTerms\s*\)\s*\.//;
        print $1 if /^\s*source\s*\((.*)\)\s*\.\s*$/s;
    ' "$1")
    if [ -z "$VS" ]; then
        echo "transform.sh: $1 is not the declaration and the clause source(V, S)" >&2
        exit 2
    fi
    if [ "$CHECK" = 1 ]; then OUT="$WORK/out"; else OUT="$SRC"; fi
    BASE="$SRC"
    mkdir -p "$OUT" || exit 2
    PARTS="profiles population"
    {
        echo ':limit 1000000000000000'
        echo "$HERE"
        for p in $PARTS; do
            echo "transform_terms($VS, $p)."
        done
        echo ':quit'
    } > "$WORK/input"
else
    if [ $# -eq 0 ]; then
        OUT="$HERE"
    elif [ "$1" = "--check" ]; then
        CHECK=1
        OUT="$WORK/out"
    else
        OUT="$1"
    fi
    BASE="$HERE"
    mkdir -p "$OUT/social_graph" "$OUT/coins" "$OUT/tests/reader" "$OUT/tests/delivery" "$OUT/tests/receipt" "$OUT/tests/panel" "$OUT/tests/numbers" "$OUT/tests/steps" "$OUT/tests/screen" "$OUT/tests/census" || exit 2

    PARTS="social_graph/profiles social_graph/population coins/profiles coins/population tests/reader/profiles tests/reader/population tests/delivery/profiles tests/delivery/population tests/receipt/profiles tests/receipt/population tests/panel/profiles tests/panel/population tests/numbers/profiles tests/numbers/population tests/steps/profiles tests/steps/population tests/screen/profiles tests/screen/population tests/census/profiles tests/census/population"
    {
        echo ':limit 1000000000000000'
        echo "$HERE"
        for p in $PARTS; do
            prog=${p%/*}
            echo "transform(${prog##*/}, ${p##*/})."
        done
        echo ':quit'
    } > "$WORK/input"
fi

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
        if [ -f "$f" ] && cmp -s "$f" "$BASE/$p.glp"; then
            echo "$p.glp: as printed"
        else
            echo "$p.glp: NOT as printed"
            [ -f "$f" ] && diff "$BASE/$p.glp" "$f" | head -20
            OK=0
        fi
    elif [ -f "$f" ]; then
        echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
