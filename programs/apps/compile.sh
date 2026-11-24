#!/bin/bash
# The platforms of the apps: for each app, the canonical CVA platform vGLP's compiler prints from the act schemas of its contract (vGLP, Section 3.2, Definition "Canonical CVA Platform"; Section 5.4, "The compilation"), read from the contract's owner's file, the one copy (claude.md "Sessions", the cascade rule; Coordination Appendix C).  The platform is committed in the app's directory, which loads it, and this script holds it to what the compiler prints, as programs/vglp/cva/compile.sh does for vGLP's plays and cascade.sh for the recorded runs of the cascade.
#
#   bash programs/apps/compile.sh            print into programs/apps/<app>/platform.vglp
#   bash programs/apps/compile.sh --check    print into a scratch directory and compare
#
#   <app>  contract  owner's file
#   gkk    knowledge       programs/gsc/Knowledge.glp
#   gsg    friendship       programs/gsc/Friendship.glp
#   gsn    groups       programs/gsc/Groups.glp
#   gsnp   provenance_groups      programs/gai/Provenance-Groups.glp
#   gai    ai_agents    programs/gai/AI-Agents.glp
#   gc     gc        programs/currencies/coins/Coins.glp
#   gb     gb        programs/currencies/bonds/Bonds.glp
#   dgc    denominated_currencies  programs/currencies/sovereign/Denominated-Currencies.glp
#   gff    feeds       programs/gff/Feeds.glp     (GFF, adopted from the Calculus, which has no GFF-I: Udi, 2026-11-15, to GFF #6)
#
# The steps are cascade.sh's 1, 2 and 4: the contract, contract_named/2, from the owner's file; the checker's verdict on it, programs/gsc's check/2; and the compiler, programs/vglp/cva/compiler's compile/5, on the contract, its verdict, its introduction predicate, introduction_named/2 where the owner's file names one and no_introduction otherwise, and its pragmas, the words its app shows, read from the NAME.txt beside the owner's file and from those of the contracts it extends by programs/vglp/cva/compiler/pragmas.sh (vGLP, Section 5.4, "The contract's words"), each act the schema its button names (GSC, Section 2).  What the compiler prints between its lines "%% compile begin" and "%% compile end" is the platform.
#
# Prints one line per app.  Exits 0 if each was compiled and, with --check, each is byte for byte the file in place; 1 otherwise, the app not compiled or the differing file named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
APPS="gkk gsg gsn gsnp gai gc gb dgc gff"

contract_of() { # contract_of <app> ; the checker's name of its contract
    case "$1" in
        gkk) echo knowledge ;;
        gsg) echo friendship ;;
        gsn) echo groups ;;
        gsnp) echo provenance_groups ;;
        gai) echo ai_agents ;;
        gc) echo coins ;;
        gb) echo bonds ;;
        dgc) echo denominated_currencies ;;
        gff) echo feeds ;;
    esac
}
file_of() { # file_of <app> ; the owner's file that holds its contract
    case "$1" in
        gkk) echo "$GLP_DIR/programs/gsc/Knowledge.glp" ;;
        gsg) echo "$GLP_DIR/programs/gsc/Friendship.glp" ;;
        gsn) echo "$GLP_DIR/programs/gsc/Groups.glp" ;;
        gsnp) echo "$GLP_DIR/programs/gai/Provenance-Groups.glp" ;;
        gai) echo "$GLP_DIR/programs/gai/AI-Agents.glp" ;;
        gc) echo "$GLP_DIR/programs/currencies/coins/Coins.glp" ;;
        gb) echo "$GLP_DIR/programs/currencies/bonds/Bonds.glp" ;;
        dgc) echo "$GLP_DIR/programs/currencies/sovereign/Denominated-Currencies.glp" ;;
        gff) echo "$GLP_DIR/programs/gff/Feeds.glp" ;;
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
    # The contract's term, contract_named/2's, or an instance's, formed from its base by programs/gsc's instantiate/4 where the file prints instance_named/4 (GSC, Definition Parametric contract), by programs/vglp/cva/compiler/term.sh, as cascade.sh's read_contract forms it.
    ct=$(bash "$GLP_DIR/programs/vglp/cva/compiler/term.sh" "$src" "$name" 2> "$WORK/t.err")
    [ -n "$ct" ] || echo "$app: $(head -1 "$WORK/t.err")"
    # The contract as GSC's reader reads it, as_read/2 of programs/gsc, each schema of the clause syntax, sas/7, in the checker's form with its roles' names, and a schema in that form as it is: the compiler and pragmas.sh take this and keep no copy of the reader (vGLP, Section 5.4, "Speech-act schemas": "A compiler written in GLP takes such a contract ... written as speech-act schemas [GSC]"; vGLP #23, Code_inbox.md, 2026-11-05 14:20 UTC, 1).
    rd=""
    if [ -n "$ct" ]; then
        printf '%s\n:limit 10000000\nas_read(%s, R).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/a.in"
        repl "$WORK/a.in" "$WORK/a.out"
        rd=$(grep -E '^R = ' "$WORK/a.out" | head -1 | sed -e 's/^R = //')
    fi
    intro=no_introduction
    if grep -q "^introduction_named($name," "$src"; then
        printf '%s\nintroduction_named(%s, I).\n:quit\n' "$src" "$name" > "$WORK/i.in"
        repl "$WORK/i.in" "$WORK/i.out"
        it=$(grep -E '^I = ' "$WORK/i.out" | head -1 | sed -e 's/^I = //')
        [ -n "$it" ] && intro="$it"
    fi
    # The contract's words: its pragmas, from its NAME.txt and those of the contracts it extends.
    pr=no_pragmas
    if [ -f "${src%.glp}.txt" ] && ! pr=$(printf '%s\n' "$rd" > "$WORK/pr.rd" && bash "$GLP_DIR/programs/vglp/cva/compiler/pragmas.sh" "${src%.glp}.txt" "@$WORK/pr.rd" 2> "$WORK/pr.err"); then
        echo "$app: its pragmas were not read: $(head -1 "$WORK/pr.err")"
        OK=0
        continue
    fi
    vt=""
    if [ -n "$ct" ]; then
        printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/v.in"
        repl "$WORK/v.in" "$WORK/v.out"
        vt=$(grep -E '^V = ' "$WORK/v.out" | head -1 | sed -e 's/^V = //')
    fi
    if [ -z "$ct" ] || [ -z "$rd" ] || [ -z "$vt" ]; then
        echo "$app: no contract or no verdict from $src"
        OK=0
        continue
    fi
    # The developer's values, NAME.values beside the contract's file, its owner's (vGLP 922ac53, Section 5.4), as cascade.sh passes them; with none, compile/5.
    vf="${src%.glp}.values"
    if [ -f "$vf" ]; then
        dv="dev(\"[$(grep -v '^[[:space:]]*$' "$vf" | paste -s -d, - | sed -e 's/,/, /g')]\")"
        printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$name" "$rd" "$vt" "$intro" "$pr" "$dv" > "$WORK/p.in"
    else
    printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$name" "$rd" "$vt" "$intro" "$pr" > "$WORK/p.in"
    fi
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
        mkdir -p "$HERE/$app"
        cp "$out" "$HERE/$app/platform.vglp"
        echo "$HERE/$app/platform.vglp: $(wc -l < "$out" | tr -d ' ') lines"
    fi
done

[ "$OK" = 1 ] && exit 0 || exit 1
