#!/bin/bash
# The compiler from an act-schema contract to its canonical CVA platform in
# vGLP (programs/vglp/cva/compiler; vGLP, Section 3.2, the paragraph on act
# schemas, and Section 5.4, Definition "Compilation of the Canonical
# Platform"), run on the social graph's and the currency's contracts, read
# from their owners' files, and on vGLP's own, contracts.glp, through the REPL.
#
#   bash programs/vglp/cva/compile.sh            print into programs/vglp/cva/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh <dir>      print into <dir>/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh --check    print into a scratch directory and compare
#
# for <contract> social_graph and currency, the directories of GSN (GSC's,
# programs/gsc/GSN.glp, the checker's gsn; GSC, Section 3.3) and GC
# (Currencies', programs/currencies/coins/GC.glp, the checker's gc; Section
# 3.4), each the one copy in its owner's file (claude.md "Sessions", the
# cascade rule; Coordination Appendix C), and nudge, introduce, gnudge,
# childsafe, link and friends, vGLP's, which contracts.glp beside the compiler
# holds, with the introduction predicate each names, introduction_named/2, or
# none (GSC's two name none).  A contract is checked before it is compiled
# (GSC, Section 5), so a REPL session on each owner's file and one on
# contracts.glp print the contracts, contract_named/2; one on programs/gsc
# prints the checker's verdict on each, check/2; and one on the compiler calls
# compile(Name, Contract, Verdict, Intro) for each and writes what it prints
# between its lines "%% compile begin" and "%% compile end" to the contract's
# platform.vglp.  The compiled modules are committed beside their plays, which
# load them; --check says whether each is what the compiler prints.
#
# Prints one line per contract.  Exits 0 if each was compiled and, with
# --check, each is byte for byte the file in place; 1 otherwise, the contract
# not compiled or the differing file named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
THEIRS="gsn gc"
OURS="nudge introduce gnudge childsafe link friends"
CONTRACTS="$THEIRS $OURS"

contract_file() { # contract_file <contract> ; the file that holds it, its owner's
    case "$1" in
        gsn) echo "$GLP_DIR/programs/gsc/GSN.glp" ;;
        gc)  echo "$GLP_DIR/programs/currencies/coins/GC.glp" ;;
        *)   echo "$HERE/contracts.glp" ;;
    esac
}
dir_of() { # dir_of <contract> ; the directory of its platform and plays here
    case "$1" in
        gsn) echo social_graph ;;
        gc)  echo currency ;;
        *)   echo "$1" ;;
    esac
}
DIRS=$(for c in $CONTRACTS; do dir_of "$c"; done | tr '\n' ' ')

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
for d in $DIRS; do mkdir -p "$OUT/$d" || exit 2; done

# Each contract's term, one line per contract in the order of CONTRACTS:
# GSN and GC from their owners' files, one session each, and vGLP's from
# contracts.glp, one session, with their introduction predicates.
: > "$WORK/terms_c"
for c in $THEIRS; do
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$(contract_file "$c")" "$c" > "$WORK/$c.in"
    (cd "$RT" && bin/glpc < "$WORK/$c.in" > "$WORK/$c.out" 2>&1)
    sed -e 's/^\(GLP> \)*//' "$WORK/$c.out" | grep -E '^C = ' | head -1 >> "$WORK/terms_c"
done
{
    echo "$HERE/contracts.glp"
    for c in $OURS; do echo "contract_named($c, C)."; done
    for c in $OURS; do echo "introduction_named($c, I)."; done
    echo ':quit'
} > "$WORK/ours.in"
(cd "$RT" && bin/glpc < "$WORK/ours.in" > "$WORK/ours.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/ours.out" | grep -E '^C = ' >> "$WORK/terms_c"
sed -e 's/^\(GLP> \)*//' "$WORK/ours.out" | grep -E '^I = ' | sed -e 's/^I = //' > "$WORK/intros"

# The checker's verdict on each, from programs/gsc.  The REPL stops a goal
# at 10000 reductions unless told otherwise; the checker takes more.
{
    echo "$GLP_DIR/programs/gsc/"
    echo ':limit 10000000'
    n=0
    for c in $CONTRACTS; do
        n=$((n + 1))
        echo "check($(sed -n "${n}p" "$WORK/terms_c" | sed -e 's/^C = //'), V)."
    done
    echo ':quit'
} > "$WORK/gsc.in"
(cd "$RT" && bin/glpc < "$WORK/gsc.in" > "$WORK/gsc.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/gsc.out" | grep -E '^V = ' > "$WORK/terms_v"

# Each contract's two lines, its C and its V, in the order of CONTRACTS, and
# its introduction predicate.
: > "$WORK/terms"
n=0
for c in $CONTRACTS; do
    n=$((n + 1))
    sed -n "${n}p" "$WORK/terms_c" >> "$WORK/terms"
    sed -n "${n}p" "$WORK/terms_v" >> "$WORK/terms"
done
{
    for c in $THEIRS; do echo no_introduction; done
    cat "$WORK/intros"
} > "$WORK/introductions"

# The compiler, on each contract and its verdict.
{
    echo "$HERE/compiler/"
    echo ':limit 1000000000'
    n=0
    for c in $CONTRACTS; do
        n=$((n + 1))
        ct=$(sed -n "$((2 * n - 1))p" "$WORK/terms" | sed -e 's/^C = //')
        vt=$(sed -n "$((2 * n))p" "$WORK/terms" | sed -e 's/^V = //')
        it=$(sed -n "${n}p" "$WORK/introductions")
        echo "compile($c, $ct, $vt, $it)."
    done
    echo ':quit'
} > "$WORK/compile.in"
(cd "$RT" && bin/glpc < "$WORK/compile.in" > "$WORK/compile.out" 2>&1)

awk -v out="$OUT" -v names="$DIRS" '
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
    grep -E '^(GLP> )*(Error|✗)' "$WORK"/*.out | head -20
    OK=0
fi
for d in $DIRS; do
    f="$OUT/$d/platform.vglp"
    if [ "$CHECK" = 1 ]; then
        if [ -f "$f" ] && cmp -s "$f" "$HERE/$d/platform.vglp"; then
            echo "$d/platform.vglp: as printed"
        else
            echo "$d/platform.vglp: NOT as printed"
            [ -f "$f" ] && diff "$HERE/$d/platform.vglp" "$f" | head -20
            OK=0
        fi
    elif [ -f "$f" ]; then
        echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
