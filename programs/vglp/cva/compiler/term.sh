#!/bin/bash
# The term of a contract, as GSC's reader and checker take it (programs/gsc): from its owner's NAME.glp, the term contract_named(<name>, C) prints; or, where the file prints instead instance_named(<name>, Base, Bindings, Additions), only what an instance of a parametric contract adds or replaces (GSC, Definition Parametric contract), the instance formed from the base's term by programs/gsc's instantiate/4, the bindings first put to instance_faults/3, as cascade.sh's read_contract forms it.
#
#   bash programs/vglp/cva/compiler/term.sh <NAME.glp> <name>
#
# Prints the term on one line; or, where there is none, the line saying why on the standard error, and exits 1.  The base's term is the one contract_named(<base>, C) prints in the file under programs/ that defines it.  Exits 2 on a bad argument.
set -u
[ $# -eq 2 ] && [ -f "$1" ] || { echo "usage: term.sh <NAME.glp> <name>" >&2; exit 2; }
SRC="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"; N="$2"
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
RT="$PROGRAMS/../glp_runtime"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
repl() {   # repl <input file> <output file>; the REPL's lines, prompts stripped
    (cd "$RT" && bin/glpc < "$1" 2>&1) | sed -e 's/^\(GLP> \)*//' > "$2"
}
answer() { grep -E "^$1 = " "$2" | head -1 | sed -e "s/^$1 = //"; }
if grep -q "^instance_named($N," "$SRC"; then
    printf '%s\ninstance_named(%s, B, Bs, A).\n:quit\n' "$SRC" "$N" > "$WORK/c.in"
    repl "$WORK/c.in" "$WORK/c.out"
    B=$(answer B "$WORK/c.out"); BS=$(answer Bs "$WORK/c.out"); A=$(answer A "$WORK/c.out")
    [ -n "$B" ] && [ -n "$BS" ] && [ -n "$A" ] || { echo "term.sh: $SRC prints no instance_named($N, Base, Bindings, Additions)" >&2; exit 1; }
    BF=$(grep -rl --include='*.glp' "^contract_named($B," "$PROGRAMS" | grep -v '/tests/' | head -1)
    [ -n "$BF" ] || { echo "term.sh: no file under programs/ defines contract_named($B, C), the base of $N" >&2; exit 1; }
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$BF" "$B" > "$WORK/b.in"
    repl "$WORK/b.in" "$WORK/b.out"
    BT=$(answer C "$WORK/b.out")
    [ -n "$BT" ] || { echo "term.sh: $BF prints no contract_named($B, C)" >&2; exit 1; }
    printf '%s\n:limit 10000000\ninstance_faults(%s, %s, F).\ninstantiate(%s, %s, %s, D).\n:quit\n' "$PROGRAMS/gsc/" "$BT" "$BS" "$BT" "$BS" "$A" > "$WORK/x.in"
    repl "$WORK/x.in" "$WORK/x.out"
    F=$(answer F "$WORK/x.out")
    [ "$F" = "[]" ] || { echo "term.sh: the bindings of $N to $B are refused: ${F:-no answer}" >&2; exit 1; }
    D=$(answer D "$WORK/x.out")
    [ -n "$D" ] || { echo "term.sh: instantiate/4 formed no instance of $B for $N" >&2; exit 1; }
    printf '%s\n' "$D"
else
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$SRC" "$N" > "$WORK/c.in"
    repl "$WORK/c.in" "$WORK/c.out"
    C=$(answer C "$WORK/c.out")
    [ -n "$C" ] || { echo "term.sh: $SRC prints no contract_named($N, C)" >&2; exit 1; }
    printf '%s\n' "$C"
fi
