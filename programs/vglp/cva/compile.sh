#!/bin/bash
# The compiler from an act-schema contract to its canonical CVA platform in
# vGLP (programs/vglp/cva/compiler; vGLP, Section 3.2, the paragraph on act
# schemas, and Section 5.4, "The compilation"), run on the social graph's, the social network's and the currency's contracts, read
# from their owners' files, and on vGLP's own, contracts.glp, through the REPL.
#
#   bash programs/vglp/cva/compile.sh            print into programs/vglp/cva/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh <dir>      print into <dir>/<contract>/platform.vglp
#   bash programs/vglp/cva/compile.sh --check    print into a scratch directory and compare
#
# for <contract> social_graph, social_network and currency, the directories of Friendship (GSC's,
# programs/gsc/Friendship.glp, the checker's friendship, the social graph), Groups (GSC's,
# programs/gsc/Groups.glp, the checker's groups, the social network) and Coins
# (Currencies', programs/currencies/coins/Coins.glp, the checker's coins; Section
# 3.4), each the one copy in its owner's file (claude.md "Sessions", the
# cascade rule; Coordination Appendix C), its file and the name its file gives
# it found by the table of the contracts' names, calculus/NAMES.txt, and nudge, introduce, gnudge,
# childsafe, link, friends, waves, parts, opens, groups, counter, pokes, merges, coins, wardclose, numbers, bonds, intros, days, linear, forms, counts, shares and roots, vGLP's, which contracts.glp beside the compiler
# holds, with the introduction predicate each names, introduction_named/2, or
# none (GSC's three name none).  A contract is checked before it is compiled
# (GSC, Section 5): for each contract, a REPL session on its owner's file, or
# on contracts.glp, prints it, contract_named/2; one on programs/gsc prints the
# checker's verdict on it, check/2; and one on the compiler calls
# compile(Name, Contract, Verdict, Intro, Pragmas), the contract's pragmas,
# the words its app shows, read from the NAME.txt beside its owner's file and
# those of the contracts it extends by compiler/pragmas.sh, or, for
# contracts.glp, the term beside each, pragmas_named/2, each act the schema
# its button names (vGLP, Section 5.4, "The contract's words"; GSC, Section
# 2); a contract with no pragmas, no_pragmas, the compiler refuses ("the
# compiler prints a platform only from a contract with pragmas, and refuses
# one with none", vGLP ffcb093, ibid.).  It writes what it prints between
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
THEIRS="social_graph social_network currency"
OURS="nudge introduce gnudge childsafe link friends waves parts opens groups counter pokes merges coins wardclose numbers bonds intros days linear forms counts shares roots published leaves readings backed consts roles wild wards feeds"
CONTRACTS="$THEIRS $OURS"

# The contract of each of GSC's and Currencies' rows, by its name in the table
# of the contracts' names, calculus/NAMES.txt ("name on main | name | file name
# | row and atom | row and atom on main"), the one source of the names: the
# social graph is Friendship, the social network Groups and the currency Coins
# (Udi, 2026-11-16; Code #33, vGLP_inbox.md, 23:53 UTC).  A row is the directory
# of its platform and plays here.
contract_name() {
    case "$1" in
        social_graph)   echo Friendship ;;
        social_network) echo Groups ;;
        currency)       echo Coins ;;
    esac
}
# The table, calculus/NAMES.txt, or the file CONTRACT_NAMES names, a table of
# the same form, as pragmas.sh reads it: lines of four columns, or of five.
NAMES="${CONTRACT_NAMES:-$GLP_DIR/calculus/NAMES.txt}"
table_row() { # table_row <name> ; its row of the table, its first four columns joined by "|"
    awk -F'|' -v n="$1" '
        f && (NF == 4 || NF == 5) { for (i = 1; i <= NF; i++) gsub(/^[ \t]+|[ \t]+$/, "", $i); if ($2 == n) { print $1 "|" $2 "|" $3 "|" $4; exit } }
        { x = $1; gsub(/^[ \t]+|[ \t]+$/, "", x); if ((NF == 4 || NF == 5) && x == "name on main") f = 1 }
    ' "$NAMES"
}
# The owner's file of the contract of a row, by the table: the NAME.glp of its
# file name, "the name with a hyphen for each space and no apostrophe: NAME.txt,
# NAME.glp" (calculus/NAMES.txt), under programs/; and the name its file gives
# it, contract_named/2, "the atom of contract_named", the file name in lower
# case, a hyphen as an underscore (ibid.), the table's fourth column.
row_file() { # row_file <row> ; "<file>|<atom>"
    local r f g
    r=$(table_row "$(contract_name "$1")")
    [ -n "$r" ] || return 1
    f=$(printf '%s' "$r" | cut -d'|' -f3)
    for g in "$GLP_DIR"/programs/*/"$f.glp" "$GLP_DIR"/programs/*/*/"$f.glp"; do
        [ -f "$g" ] && { printf '%s|%s\n' "$g" "$(printf '%s' "$r" | cut -d'|' -f4)"; return 0; }
    done
    return 1
}
contract_file() { # contract_file <contract> ; the file that holds it, its owner's
    case "$1" in
        social_graph|social_network|currency) row_file "$1" | cut -d'|' -f1 ;;
        *)   echo "$HERE/contracts.glp" ;;
    esac
}
name_in() { # name_in <contract> ; the name its file gives it, contract_named/2
    case "$1" in
        social_graph|social_network|currency) row_file "$1" | cut -d'|' -f2 ;;
        *)  echo "$1" ;;
    esac
}
dir_of() { # dir_of <contract> ; the directory of its platform and plays here
    echo "$1"
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
    printf '%s\ncontract_named(%s, C).\n:quit\n' "$src" "$(name_in "$c")" > "$WORK/c.in"
    repl "$WORK/c.in" "$WORK/$c.c.out"
    ct=$(grep -E '^C = ' "$WORK/$c.c.out" | head -1 | sed -e 's/^C = //')
    # The contract as GSC's reader reads it, as_read/2 of programs/gsc, each schema of the clause syntax, sas/7, in the checker's form with its roles' names, and a schema in that form as it is: the compiler and pragmas.sh take this and keep no copy of the reader (vGLP, Section 5.4, "Speech-act schemas": "A compiler written in GLP takes such a contract ... written as speech-act schemas [GSC]"; vGLP #23, Code_inbox.md, 2026-11-05 14:20 UTC, 1).
    rd=""
    if [ -n "$ct" ]; then
        printf '%s\n:limit 10000000\nas_read(%s, R).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/a.in"
        repl "$WORK/a.in" "$WORK/$c.a.out"
        rd=$(grep -E '^R = ' "$WORK/$c.a.out" | head -1 | sed -e 's/^R = //')
    fi
    it=no_introduction
    if [ "$src" = "$HERE/contracts.glp" ]; then
        printf '%s\nintroduction_named(%s, I).\n:quit\n' "$src" "$c" > "$WORK/i.in"
        repl "$WORK/i.in" "$WORK/$c.i.out"
        it=$(grep -E '^I = ' "$WORK/$c.i.out" | head -1 | sed -e 's/^I = //')
    fi
    pr=no_pragmas
    if [ "$src" = "$HERE/contracts.glp" ]; then
        # vGLP's contracts carry their pragmas as a term beside them,
        # pragmas_named/2, the form pragmas.sh prints from a NAME.txt.
        printf '%s\npragmas_named(%s, P).\n:quit\n' "$src" "$c" > "$WORK/pn.in"
        repl "$WORK/pn.in" "$WORK/$c.pn.out"
        pr=$(grep -E '^P = ' "$WORK/$c.pn.out" | head -1 | sed -e 's/^P = //')
        [ -n "$pr" ] || pr=no_pragmas
    elif [ -f "${src%.glp}.txt" ] \
        && ! pr=$(printf '%s\n' "$rd" > "$WORK/$c.rd" && bash "$HERE/compiler/pragmas.sh" "${src%.glp}.txt" "@$WORK/$c.rd" 2> "$WORK/$c.pr.err"); then
        echo "missing $d: its pragmas were not read: $(head -1 "$WORK/$c.pr.err")" >> "$WORK/status"
        continue
    fi
    vt=""
    if [ -n "$ct" ]; then
        # The REPL stops a goal at 10000 reductions unless told otherwise; the
        # checker takes more.
        printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$ct" > "$WORK/v.in"
        repl "$WORK/v.in" "$WORK/$c.v.out"
        vt=$(grep -E '^V = ' "$WORK/$c.v.out" | head -1 | sed -e 's/^V = //')
    fi
    if [ -z "$ct" ] || [ -z "$rd" ] || [ -z "$vt" ] || [ -z "$it" ]; then
        echo "missing $d: no contract, no verdict or no introduction predicate from $src" >> "$WORK/status"
        grep -hE '^(Error|✗)' "$WORK/$c".*.out | head -3 >> "$WORK/status"
        continue
    fi
    # The values the developer gives, one atom a line in <dir>/values beside the platform, as the contract's Vocabulary writes them (vGLP 922ac53, Section 5.4: "A platform is deployed when its developer compiles it, giving the compiler the values"), passed as dev(Text), Text their list; nodev where there is no such file.
    if [ -f "$HERE/$d/values" ]; then
        dv="dev(\"[$(grep -v '^[[:space:]]*$' "$HERE/$d/values" | paste -s -d, - | sed -e 's/,/, /g')]\")"
    else
        dv=nodev
    fi
    printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s, %s, %s).\n:quit\n' "$HERE/compiler/" "$(name_in "$c")" "$rd" "$vt" "$it" "$pr" "$dv" > "$WORK/p.in"
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
