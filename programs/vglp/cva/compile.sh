#!/bin/bash
# The compiler from an act-schema contract to its canonical CVA platform in
# vGLP (programs/vglp/cva/compiler; vGLP, Section 3.2, the paragraph on act
# schemas, and Section 5.4, "The compilation"), run on the social graph's and the currency's contracts, read
# from their owners' files, and on vGLP's own, contracts.glp, through the REPL.
#
#   bash programs/vglp/cva/compile.sh            print into programs/vglp/cva/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh <dir>      print into <dir>/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh --check    print into a scratch directory and compare
#
# for <contract> social_graph and currency, the directories of GSG (GSC's,
# programs/gsc/GSG.glp, the checker's gsg, the social graph) and GC
# (Currencies', programs/currencies/coins/GC.glp, the checker's gc; Section
# 3.4), each the one copy in its owner's file (claude.md "Sessions", the
# cascade rule; Coordination Appendix C), and nudge, introduce, gnudge,
# childsafe, link, friends, waves, parts, opens, groups, counter and merges, vGLP's, which contracts.glp beside the compiler
# holds, with the introduction predicate each names, introduction_named/2, or
# none (GSC's two name none).  A contract is checked before it is compiled
# (GSC, Section 5): for each contract, a REPL session on its owner's file, or
# on contracts.glp, prints it, contract_named/2; one on programs/gsc prints the
# checker's verdict on it, check/2; and one on the compiler calls
# compile(Name, Contract, Verdict, Intro) and writes what it prints between
# its lines "%% compile begin" and "%% compile end" to the contract's
# platform.vglp.  A contract a step fails on is named, and its platform.vglp
# is left as it is.  The compiled modules are committed beside their plays,
# which load them; --check says whether each is what the compiler prints.
#
# Prints one line per contract.  Exits 0 if each was compiled and, with
# --check, each is byte for byte the file in place; 1 otherwise, the contract
# not compiled or the differing file named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
THEIRS="gsg gc"
OURS="nudge introduce gnudge childsafe link friends waves parts opens groups counter merges"
CONTRACTS="$THEIRS $OURS"

contract_file() { # contract_file <contract> ; the file that holds it, its owner's
    case "$1" in
        gsg) echo "$GLP_DIR/programs/gsc/GSG.glp" ;;
        gc)  echo "$GLP_DIR/programs/currencies/coins/GC.glp" ;;
        *)   echo "$HERE/contracts.glp" ;;
    esac
}
dir_of() { # dir_of <contract> ; the directory of its platform and plays here
    case "$1" in
        gsg) echo social_graph ;;
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

# Each contract on its own: its term, from its owner's file or from
# contracts.glp, with its introduction predicate; the checker's verdict on it,
# from programs/gsc; and the compiler, compile(Name, Contract, Verdict, Intro),
# whose lines between "%% compile begin" and "%% compile end" are its
# platform.vglp.  One REPL session to a step and a contract, so that a contract
# a step fails on is named and leaves the others as they are.
repl() {   # repl <input file> <output file>; the REPL's lines, prompts stripped
    (cd "$RT" && bin/glpc < "$1" 2>&1) | sed -e 's/^\(GLP> \)*//' > "$2"
}
: > "$WORK/status"
for c in $CONTRACTS; do
    d=$(dir_of "$c")
    src=$(contract_file "$c")
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$src" "$c" > "$WORK/c.in"
    repl "$WORK/c.in" "$WORK/$c.c.out"
    ct=$(grep -E '^C = ' "$WORK/$c.c.out" | head -1 | sed -e 's/^C = //')
    it=no_introduction
    if [ "$src" = "$HERE/contracts.glp" ]; then
        printf '%s\nintroduction_named(%s, I).\n:quit\n' "$src" "$c" > "$WORK/i.in"
        repl "$WORK/i.in" "$WORK/$c.i.out"
        it=$(grep -E '^I = ' "$WORK/$c.i.out" | head -1 | sed -e 's/^I = //')
    fi
    vt=""
    if [ -n "$ct" ]; then
        # The REPL stops a goal at 10000 reductions unless told otherwise; the
        # checker takes more.
        printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/v.in"
        repl "$WORK/v.in" "$WORK/$c.v.out"
        vt=$(grep -E '^V = ' "$WORK/$c.v.out" | head -1 | sed -e 's/^V = //')
    fi
    if [ -z "$ct" ] || [ -z "$vt" ] || [ -z "$it" ]; then
        echo "missing $d: no contract, no verdict or no introduction predicate from $src" >> "$WORK/status"
        grep -hE '^(Error|✗)' "$WORK/$c".*.out | head -3 >> "$WORK/status"
        continue
    fi
    printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s).\n:quit\n' "$HERE/compiler/" "$c" "$ct" "$vt" "$it" > "$WORK/p.in"
    repl "$WORK/p.in" "$WORK/$c.p.out"
    f="$OUT/$d/platform.vglp"
    if ! awk '
        $0 == "%% compile begin" { on = 1; next }
        $0 == "%% compile end" && on { on = 0; done = 1; next }
        on { print }
        END { if (!done) exit 1 }
    ' "$WORK/$c.p.out" > "$WORK/$c.platform"; then
        echo "missing $d: the compiler printed no module" >> "$WORK/status"
        grep -hE '^(Error|✗)' "$WORK/$c.p.out" | head -3 >> "$WORK/status"
        continue
    fi
    if grep -q '^%% not compiled' "$WORK/$c.platform"; then
        echo "fault $d: $(grep '^%% not compiled' "$WORK/$c.platform" | head -1)" >> "$WORK/status"
        continue
    fi
    cp "$WORK/$c.platform" "$f"
done

OK=1
if [ -s "$WORK/status" ]; then
    cat "$WORK/status"
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
