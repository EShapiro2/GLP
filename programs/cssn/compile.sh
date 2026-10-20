#!/bin/bash
# The child-safe social network's program: the canonical CVA platform of its contract (CSSN, Section 5.1, "The program"; Appendix C.5), which vGLP's compiler prints from the contract's act schemas, CSSN.glp, in the term form programs/gsc reads (vGLP, Section 5.4, "The compilation" and "Act schemas as local specifications").  The platform is committed in this directory, which loads it, and this script holds it to what the compiler prints, as programs/apps/compile.sh does for the apps and cascade.sh for the recorded runs of the cascade.
#
#   bash programs/cssn/compile.sh            print into programs/cssn/platform.vglp
#   bash programs/cssn/compile.sh <dir>      print into <dir>/platform.vglp
#   bash programs/cssn/compile.sh --check    print into a scratch directory and compare
#
# The steps are cascade.sh's 1, 2 and 4: the contract, contract_named(cssn, C), from CSSN.glp; the checker's verdict on it, programs/gsc's check/2; and the compiler, programs/vglp/cva/compiler's compile/4, on the contract, its verdict and its introduction predicate, introduction_named(cssn, I) where CSSN.glp names one and no_introduction otherwise.  What the compiler prints between its lines "%% compile begin" and "%% compile end" is the platform.
#
# Prints the verdict, the introduction predicate and, where it compiled, the file.  Exits 0 if the contract was compiled and, with --check, the file in place is what the compiler prints; 1 otherwise, the compiler's lines saying why; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
SRC="$HERE/CSSN.glp"

CHECK=0
case "$#:${1:-}" in
    0:) OUT="$HERE" ;;
    1:--check) CHECK=1 ;;
    1:*) OUT="$1" ;;
    *) echo "usage: compile.sh [<dir> | --check]" >&2; exit 2 ;;
esac
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
[ "$CHECK" = 1 ] && OUT="$WORK/out"
mkdir -p "$OUT" || exit 2

repl() {   # repl <input file> <output file>; the REPL's lines, prompts stripped
    (cd "$RT" && bin/glpc < "$1" 2>&1) | sed -e 's/^\(GLP> \)*//' > "$2"
}

# The contract.
printf '%s\ncontract_named(cssn, C).\n:quit\n' "$SRC" > "$WORK/c.in"
repl "$WORK/c.in" "$WORK/c.out"
CT=$(grep -E '^C = ' "$WORK/c.out" | head -1 | sed -e 's/^C = //')
if [ -z "$CT" ]; then
    echo "contract: not printed"
    grep -E '(Error|error|✗)' "$WORK/c.out" | head -20
    exit 1
fi

# Its introduction predicate, where CSSN.glp names one.
INTRO=no_introduction
if grep -q '^introduction_named(cssn,' "$SRC"; then
    printf '%s\nintroduction_named(cssn, I).\n:quit\n' "$SRC" > "$WORK/i.in"
    repl "$WORK/i.in" "$WORK/i.out"
    IT=$(grep -E '^I = ' "$WORK/i.out" | head -1 | sed -e 's/^I = //')
    [ -n "$IT" ] && INTRO="$IT"
fi
echo "introduction: $INTRO"

# Its verdict.  The REPL stops a goal at 10000 reductions unless told otherwise; the checker takes more.
printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$CT" > "$WORK/v.in"
repl "$WORK/v.in" "$WORK/v.out"
VT=$(grep -E '^V = ' "$WORK/v.out" | head -1 | sed -e 's/^V = //')
if [ -z "$VT" ]; then
    echo "verdict: not given"
    grep -E '(Error|error|✗)' "$WORK/v.out" | head -20
    exit 1
fi
echo "verdict: $VT"

# The compiler, on the contract, its verdict and its introduction predicate.
printf '%s\n:limit 1000000000\ncompile(cssn, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$CT" "$VT" "$INTRO" > "$WORK/p.in"
repl "$WORK/p.in" "$WORK/p.out"
if ! awk '
    $0 == "%% compile begin" { on = 1; next }
    $0 == "%% compile end" && on { on = 0; done = 1; next }
    on { print }
    END { if (!done) exit 1 }
' "$WORK/p.out" > "$WORK/platform.vglp"; then
    echo "compiler: printed no module"
    grep -E '^(Error|✗)' "$WORK/p.out" | head -20
    exit 1
fi
if grep -q '^%% not compiled' "$WORK/platform.vglp"; then
    echo "compiler: not compiled"
    cat "$WORK/platform.vglp"
    exit 1
fi

f="$OUT/platform.vglp"
cp "$WORK/platform.vglp" "$f"
if [ "$CHECK" = 1 ]; then
    if cmp -s "$f" "$HERE/platform.vglp"; then
        echo "platform.vglp: as printed"
    else
        echo "platform.vglp: NOT as printed"
        diff "$HERE/platform.vglp" "$f" | head -20
        exit 1
    fi
else
    echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
fi
exit 0
