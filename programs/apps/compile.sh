#!/bin/bash
# The platforms of the apps: for each app, the canonical CVA platform vGLP's compiler prints from the act schemas of its contract (vGLP, Section 3.2, Definition "Canonical CVA Platform"; Section 5.4, "The compilation"), read from the contract's owner's file, the one copy (claude.md "Sessions", the cascade rule; Coordination Appendix C).  The platform is committed in the app's directory, which loads it, and this script holds it to what the compiler prints, as programs/vglp/cva/compile.sh does for vGLP's plays and cascade.sh for the recorded runs of the cascade.
#
#   bash programs/apps/compile.sh            print into programs/apps/<app>/platform.vglp
#   bash programs/apps/compile.sh --check    print into a scratch directory and compare
#
#   <app>  contract  owner's file
#   gsg    gsg       programs/gsc/GSG.glp
#   gsn    gsn       programs/gsc/GSN.glp
#   gsnp   gsnp      programs/gsc/GSNP.glp
#
# The steps are cascade.sh's 1, 2 and 4: the contract, contract_named/2, from the owner's file; the checker's verdict on it, programs/gsc's check/2; and the compiler, programs/vglp/cva/compiler's compile/4, on the contract, its verdict and its introduction predicate, introduction_named/2 where the owner's file names one and no_introduction otherwise.  What the compiler prints between its lines "%% compile begin" and "%% compile end" is the platform.
#
# Prints one line per app.  Exits 0 if each was compiled and, with --check, each is byte for byte the file in place; 1 otherwise, the app not compiled or the differing file named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
APPS="gsg gsn gsnp"

contract_of() { # contract_of <app> ; the checker's name of its contract
    case "$1" in
        gsg) echo gsg ;;
        gsn) echo gsn ;;
        gsnp) echo gsnp ;;
    esac
}
file_of() { # file_of <app> ; the owner's file that holds its contract
    case "$1" in
        gsg) echo "$GLP_DIR/programs/gsc/GSG.glp" ;;
        gsn) echo "$GLP_DIR/programs/gsc/GSN.glp" ;;
        gsnp) echo "$GLP_DIR/programs/gsc/GSNP.glp" ;;
    esac
}

CHECK=0
case "$#:${1:-}" in
    0:) ;;
    1:--check) CHECK=1 ;;
    *) echo "usage: compile.sh [--check]" >&2; exit 2 ;;
esac
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

repl() {   # repl <input file> <output file>; the REPL's lines, prompts stripped
    (cd "$RT" && bin/glpc < "$1" 2>&1) | sed -e 's/^\(GLP> \)*//' > "$2"
}

OK=1
for app in $APPS; do
    name=$(contract_of "$app")
    src=$(file_of "$app")
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$src" "$name" > "$WORK/c.in"
    repl "$WORK/c.in" "$WORK/c.out"
    ct=$(grep -E '^C = ' "$WORK/c.out" | head -1 | sed -e 's/^C = //')
    intro=no_introduction
    if grep -q "^introduction_named($name," "$src"; then
        printf '%s\nintroduction_named(%s, I).\n:quit\n' "$src" "$name" > "$WORK/i.in"
        repl "$WORK/i.in" "$WORK/i.out"
        it=$(grep -E '^I = ' "$WORK/i.out" | head -1 | sed -e 's/^I = //')
        [ -n "$it" ] && intro="$it"
    fi
    vt=""
    if [ -n "$ct" ]; then
        printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/v.in"
        repl "$WORK/v.in" "$WORK/v.out"
        vt=$(grep -E '^V = ' "$WORK/v.out" | head -1 | sed -e 's/^V = //')
    fi
    if [ -z "$ct" ] || [ -z "$vt" ]; then
        echo "$app: no contract or no verdict from $src"
        OK=0
        continue
    fi
    printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$name" "$ct" "$vt" "$intro" > "$WORK/p.in"
    repl "$WORK/p.in" "$WORK/p.out"
    mkdir -p "$WORK/out/$app"
    out="$WORK/out/$app/platform.vglp"
    if ! awk '
        $0 == "%% compile begin" { on = 1; next }
        $0 == "%% compile end" && on { on = 0; done = 1; next }
        on { print }
        END { if (!done) exit 1 }
    ' "$WORK/p.out" > "$out"; then
        echo "$app: the compiler printed no module"
        grep -E '(Error|error|✗)' "$WORK/p.out" | head -10
        OK=0
        continue
    fi
    if grep -q '^%% not compiled' "$out"; then
        echo "$app: $(grep '^%% not compiled' "$out" | head -1)"
        OK=0
        continue
    fi
    if [ "$CHECK" = 1 ]; then
        if cmp -s "$out" "$HERE/$app/platform.vglp"; then
            echo "$app/platform.vglp: as printed"
        else
            echo "$app/platform.vglp: NOT as printed"
            [ -f "$HERE/$app/platform.vglp" ] && diff "$HERE/$app/platform.vglp" "$out" | head -20
            OK=0
        fi
    else
        cp "$out" "$HERE/$app/platform.vglp"
        echo "$HERE/$app/platform.vglp: $(wc -l < "$out" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
