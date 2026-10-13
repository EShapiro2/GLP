#!/bin/bash
# The compiler from an act-schema contract to its canonical CVA platform in
# vGLP (programs/vglp/cva/compiler; vGLP, Section 3.2, the paragraph on act
# schemas, and Section 5.4, Definition "Compilation of the Canonical
# Platform"), run on
# the contracts of programs/jurix and of contracts.glp through the REPL.
#
#   bash programs/vglp/cva/compile.sh            print into programs/vglp/cva/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh <dir>      print into <dir>/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh --check    print into a scratch directory and compare
#
# for <contract> social_graph and currency, Jurix's two (Jurix, Sections 3.3
# and 3.4), and nudge, introduce, gnudge and childsafe, vGLP's, which
# contracts.glp beside the compiler holds.
# A contract is checked before it is compiled (Jurix, Section 5), so a REPL
# session on contracts.glp prints each of vGLP's contracts, contract_named/2;
# one on programs/jurix prints each of Jurix's, contract_named/2, and the
# checker's verdict on each contract, check_named/2 for Jurix's and check/2 for
# vGLP's; and one on the compiler calls compile(Name, Contract, Verdict) for
# each and writes what it prints between its lines "%% compile begin" and
# "%% compile end" to the contract's platform.vglp.  The compiled modules are
# committed beside their plays, which load them; --check says whether each is
# what the compiler prints.
#
# Prints one line per contract.  Exits 0 if each was compiled and, with
# --check, each is byte for byte the file in place; 1 otherwise, the contract
# not compiled or the differing file named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
JURIX="social_graph currency"
OURS="nudge introduce gnudge childsafe"
CONTRACTS="$JURIX $OURS"

CHECK=0
if [ $# -gt 1 ]; then
    echo "usage: compile.sh [<dir> | --check]" >&2
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
for c in $CONTRACTS; do mkdir -p "$OUT/$c" || exit 2; done

# vGLP's contracts, from contracts.glp.
{
    echo "$HERE/contracts.glp"
    for c in $OURS; do echo "contract_named($c, C)."; done
    echo ':quit'
} > "$WORK/ours.in"
(cd "$RT" && bin/glpc < "$WORK/ours.in" > "$WORK/ours.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/ours.out" | grep -E '^C = ' > "$WORK/ours"

# Jurix's contracts, and the verdicts on all of them, from programs/jurix.
# The REPL stops a goal at 10000 reductions unless told otherwise; the
# checker takes more.
{
    echo "$GLP_DIR/programs/jurix/"
    echo ':limit 10000000'
    for c in $JURIX; do
        echo "contract_named($c, C)."
        echo "check_named($c, V)."
    done
    n=0
    for c in $OURS; do
        n=$((n + 1))
        echo "check($(sed -n "${n}p" "$WORK/ours" | sed -e 's/^C = //'), V)."
    done
    echo ':quit'
} > "$WORK/jurix.in"
(cd "$RT" && bin/glpc < "$WORK/jurix.in" > "$WORK/jurix.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/jurix.out" | grep -E '^(C|V) = ' > "$WORK/jv"

# Each contract's two lines, its C and its V, in the order of CONTRACTS.
nj=$(echo $JURIX | wc -w)
{
    head -n $((2 * nj)) "$WORK/jv"
    n=0
    for c in $OURS; do
        n=$((n + 1))
        sed -n "${n}p" "$WORK/ours"
        sed -n "$((2 * nj + n))p" "$WORK/jv"
    done
} > "$WORK/terms"

# The compiler, on each contract and its verdict.
{
    echo "$HERE/compiler/"
    echo ':limit 1000000000'
    n=0
    for c in $CONTRACTS; do
        n=$((n + 1))
        ct=$(sed -n "$((2 * n - 1))p" "$WORK/terms" | sed -e 's/^C = //')
        vt=$(sed -n "$((2 * n))p" "$WORK/terms" | sed -e 's/^V = //')
        echo "compile($c, $ct, $vt)."
    done
    echo ':quit'
} > "$WORK/compile.in"
(cd "$RT" && bin/glpc < "$WORK/compile.in" > "$WORK/compile.out" 2>&1)

awk -v out="$OUT" -v names="$CONTRACTS" '
    BEGIN { n = split(names, p, " "); k = 0 }
    { sub(/^(GLP> )+/, "") }
    $0 == "%% compile begin" { k++; f = out "/" p[k] "/platform.vglp"; printf "" > f; on = 1; next }
    $0 == "%% compile end" && on { close(f); on = 0; done[k] = 1; next }
    on { print > f; if ($0 ~ /^%% not compiled/) print "fault " p[k] ": " $0 }
    END { for (i = 1; i <= n; i++) if (!(i in done)) print "missing " p[i] }
' "$WORK/compile.out" > "$WORK/status"

OK=1
if [ -s "$WORK/status" ]; then
    cat "$WORK/status"
    grep -E '^(GLP> )*(Error|✗)' "$WORK/ours.out" "$WORK/jurix.out" "$WORK/compile.out" | head -20
    OK=0
fi
for c in $CONTRACTS; do
    f="$OUT/$c/platform.vglp"
    if [ "$CHECK" = 1 ]; then
        if [ -f "$f" ] && cmp -s "$f" "$HERE/$c/platform.vglp"; then
            echo "$c/platform.vglp: as printed"
        else
            echo "$c/platform.vglp: NOT as printed"
            [ -f "$f" ] && diff "$HERE/$c/platform.vglp" "$f" | head -20
            OK=0
        fi
    elif [ -f "$f" ]; then
        echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
