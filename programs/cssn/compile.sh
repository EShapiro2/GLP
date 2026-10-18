#!/bin/bash
# The child-safe social contract compiled into its canonical CVA platform
# (CSSN, Section 6.1, "The program"; Appendix C.5): the contract's act schemas,
# CSSN.glp, in the term form programs/gsc reads; GSC's checker,
# check/2, gives its verdict; and programs/vglp/cva/compiler, compile/3, prints
# the platform from the two --- the steps programs/vglp/cva/compile.sh takes
# for the contracts it names, which do not include this one.
#
#   bash programs/cssn/compile.sh            print into programs/cssn/platform/platform.vglp
#   bash programs/cssn/compile.sh <dir>      print into <dir>/platform.vglp
#   bash programs/cssn/compile.sh --check    print into a scratch directory and compare
#
# Prints the verdict, the compiler's status and, where it compiled, the file.
# Exits 0 if the contract was compiled and, with --check, the file in place is
# what the compiler prints; 1 otherwise, the compiler's lines saying why; 2 on
# a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"

CHECK=0
if [ $# -gt 1 ]; then
    echo "usage: compile.sh [<dir> | --check]" >&2
    exit 2
fi
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
if [ $# -eq 0 ]; then
    OUT="$HERE/platform"
elif [ "$1" = "--check" ]; then
    CHECK=1
    OUT="$WORK/out"
else
    OUT="$1"
fi
mkdir -p "$OUT" || exit 2

# The contract.
printf '%s\ncontract_named(cssn, C).\n:quit\n' "$HERE/CSSN.glp" > "$WORK/c.in"
(cd "$RT" && bin/glpc < "$WORK/c.in" > "$WORK/c.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/c.out" | grep -E '^C = ' | head -1 | sed -e 's/^C = //' > "$WORK/c"
if [ ! -s "$WORK/c" ]; then
    echo "contract: not printed"
    grep -E '(Error|error|✗)' "$WORK/c.out" | head -20
    exit 1
fi
CT=$(cat "$WORK/c")

# Its verdict.  The REPL stops a goal at 10000 reductions unless told
# otherwise; the checker takes more.
printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$CT" > "$WORK/v.in"
(cd "$RT" && bin/glpc < "$WORK/v.in" > "$WORK/v.out" 2>&1)
sed -e 's/^\(GLP> \)*//' "$WORK/v.out" | grep -E '^V = ' | head -1 | sed -e 's/^V = //' > "$WORK/v"
if [ ! -s "$WORK/v" ]; then
    echo "verdict: not given"
    grep -E '(Error|error|✗)' "$WORK/v.out" | head -20
    exit 1
fi
VT=$(cat "$WORK/v")
echo "verdict: $VT"

# The compiler, on the contract and its verdict.
printf '%s\n:limit 1000000000\ncompile(cssn, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$CT" "$VT" > "$WORK/p.in"
(cd "$RT" && bin/glpc < "$WORK/p.in" > "$WORK/p.out" 2>&1)
awk '
    { sub(/^(GLP> )+/, "") }
    $0 == "%% compile begin" { on = 1; next }
    $0 == "%% compile end" && on { on = 0; done = 1; next }
    on { print }
    END { if (!done) exit 1 }
' "$WORK/p.out" > "$WORK/platform.vglp"
DONE=$?
if [ $DONE != 0 ]; then
    echo "compiler: printed no module"
    grep -E '^(GLP> )*(Error|✗)' "$WORK/p.out" | head -20
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
    if cmp -s "$f" "$HERE/platform/platform.vglp"; then
        echo "platform/platform.vglp: as printed"
    else
        echo "platform/platform.vglp: NOT as printed"
        diff "$HERE/platform/platform.vglp" "$f" | head -20
        exit 1
    fi
else
    echo "$f: $(wc -l < "$f" | tr -d ' ') lines"
fi
exit 0
