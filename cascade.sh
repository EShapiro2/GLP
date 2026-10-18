#!/bin/bash
# A run of the cascade on a contract, recorded whole (claude.md "Code";
# Coordination 5.7, "The cascade"; Udi, 2026-10-17): from the contract's
# clauses and act schemas in its owner's directory through every tool of the
# cascade, each in this repository, to the canonical platform and its run.
#
#   bash cascade.sh <contract>            run, and write cascade/<contract>/
#   bash cascade.sh <contract> --check    run into a scratch directory and compare
#                                         with cascade/<contract>/
#   bash cascade.sh <contract> --record   run, write, and commit the record as one
#                                         commit tagged cascade/<contract>/<date>
#                                         (a later run the same day: <date>-2, -3, ...)
#
# The contracts, each its owner's, two files in the owner's directory:
# contract.md, the clauses, and contract.glp, the act schemas in the term form
# programs/jurix reads, each in Jurix's notation above its term, printed by
# contract_named(<contract>, C):
#
#   social_graph   GSG         programs/social/graph
#   currency       Currencies  programs/currencies/coins
#   cssn           CSSN        programs/cssn
#   federation     GFWC        programs/federation
#   gai            GAI         programs/gai
#
# The steps, and the six artefacts written to cascade/<contract>/ at the
# repository root --- outside programs/, since every .glp and .vglp under a
# program's directory is a module of that program to the linker and to :emit,
# and a record is not a program:
#   1. contract.md and contract.glp, copied as they stand at the run.
#   2. verdict.txt: the checker's verdict on the schemas, programs/jurix's
#      check/2 (Jurix, def:syntactically-grassroots; Section 8's conditions
#      for a contract with community roles).
#   3. vmat.tex: the VMAT form, the volition-guarded multiagent atomic
#      transactions each schema compiles to, as programs/jurix's
#      compile_contract/2 prints them (Jurix, Section 5.2; Section 8's worked
#      box), or its line saying why the contract is not compiled.
#   4. platform.vglp: the canonical CVA platform, as programs/vglp/cva/compiler's
#      compile/4 prints it (vGLP, Section 5.4, Definition "Compilation of the
#      Canonical Platform"), or its lines saying why it is not compiled.
#   5. platform.glp: the platform compiled to GLP by the canonical compilation,
#      as the REPL's :emit writes it (vGLP, Definition "Canonical Compilation").
#   6. run.log: the run of the platform with vGLP's scripted persons, the plays
#      of programs/vglp/cva/<contract>/, each play's logs as the REPL prints
#      them; the plays run the platform.vglp committed there, which the run
#      requires to be the one this script printed.
# and manifest.md, naming the contract, its owner, the paper's repository and
# commit, the GLP commit the tools were at, and what each step gave.
#
# A step that gives nothing --- a contract the checker refuses is not compiled,
# an uncompiled platform has no .glp and no run, a platform with no plays has no
# run --- writes no artefact for it, and the manifest says so; the script
# records what the tools print and bypasses nothing.  Exits 0 when every step
# ran and, with --check, every artefact in place is what the tools print now;
# 1 otherwise, the differing file or the failed step named; 2 on a bad
# argument or a contract with no contract.md or contract.glp.
#
# The suite's Section CAS runs --check on every recorded contract.

set -u
GLP_DIR="$(cd "$(dirname "$0")" && pwd)"
RT="$GLP_DIR/glp_runtime"

usage() { echo "usage: cascade.sh <contract> [--check | --record]" >&2; exit 2; }
[ $# -ge 1 ] && [ $# -le 2 ] || usage
NAME="$1"
MODE="${2:-write}"
case "$MODE" in write|--check|--record) ;; *) usage ;; esac

case "$NAME" in
    social_graph) OWNER="programs/social/graph";    PAPER="/Users/udi/Grassroots/GSG" ;;
    currency)     OWNER="programs/currencies/coins"; PAPER="/Users/udi/Grassroots/GC" ;;
    cssn)         OWNER="programs/cssn";             PAPER="/Users/udi/Grassroots/CSSN" ;;
    federation)   OWNER="programs/federation";       PAPER="/Users/udi/Grassroots/GFWC" ;;
    gai)          OWNER="programs/gai";              PAPER="/Users/udi/Grassroots/GAI" ;;
    *) echo "cascade.sh: no contract named $NAME" >&2; exit 2 ;;
esac
SRC="$GLP_DIR/$OWNER"
REC="$GLP_DIR/cascade/$NAME"
PLAYS="$GLP_DIR/programs/vglp/cva/$NAME"
for f in contract.md contract.glp; do
    [ -f "$SRC/$f" ] || { echo "cascade.sh: $OWNER/$f is missing; the contract is its owner's two files" >&2; exit 2; }
done

WORK=$(mktemp -d)
EMIT="$GLP_DIR/programs/tests/cascade_emit/$NAME"
trap 'rm -rf "$WORK" "$EMIT"' EXIT
# Every mode writes into the scratch directory; a run that gave every artefact
# replaces the record whole at the end, so a run that fails leaves the record
# as it was.
OUT="$WORK/out"
mkdir -p "$OUT" || exit 2

repl() {   # repl <input file> <output file>; the REPL's lines, prompts stripped
    (cd "$RT" && bin/glpc < "$1" 2>&1) | sed -e 's/^\(GLP> \)*//' > "$2"
}
say() { echo "$NAME: $*"; }
STATUS=0
NOTE=()

# 1. The contract, as it stands.
cp "$SRC/contract.md" "$OUT/contract.md"
cp "$SRC/contract.glp" "$OUT/contract.glp"
printf '%s\ncontract_named(%s, C).\n:quit\n' "$SRC/contract.glp" "$NAME" > "$WORK/c.in"
repl "$WORK/c.in" "$WORK/c.out"
CT=$(grep -E '^C = ' "$WORK/c.out" | head -1 | sed -e 's/^C = //')
if [ -z "$CT" ]; then
    say "contract.glp prints no contract_named($NAME, C)"
    grep -E '(Error|error|✗)' "$WORK/c.out" | head -10
    exit 1
fi
INTRO=no_introduction
if grep -q "^introduction_named($NAME," "$SRC/contract.glp"; then
    printf '%s\nintroduction_named(%s, I).\n:quit\n' "$SRC/contract.glp" "$NAME" > "$WORK/i.in"
    repl "$WORK/i.in" "$WORK/i.out"
    IT=$(grep -E '^I = ' "$WORK/i.out" | head -1 | sed -e 's/^I = //')
    [ -n "$IT" ] && INTRO="$IT"
fi

# 2. The verdict, and 3. the VMAT form, from programs/jurix.
printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/jurix/" "$CT" > "$WORK/v.in"
repl "$WORK/v.in" "$WORK/v.out"
VT=$(grep -E '^V = ' "$WORK/v.out" | head -1 | sed -e 's/^V = //')
if [ -z "$VT" ]; then
    say "the checker gave no verdict"
    grep -E '(Error|error|✗)' "$WORK/v.out" | head -10
    exit 1
fi
printf '%s\n' "$VT" > "$OUT/verdict.txt"
say "verdict: $VT"

printf '%s\n:limit 10000000\ncompile_contract(%s, %s).\n:quit\n' "$GLP_DIR/programs/jurix/" "$NAME" "$CT" > "$WORK/m.in"
repl "$WORK/m.in" "$WORK/m.out"
# What the compiler prints between the goal and its status line: the displays,
# a token to a line, or the lines of its refusal.
awk '/^Goal reduction limit set to/ { on = 1; next } /^→ / { on = 0 } /^$/ { next } on' "$WORK/m.out" > "$OUT/vmat.tex"
if [ ! -s "$OUT/vmat.tex" ]; then
    say "the compiler to VMAT printed nothing"
    grep -E '(Error|error|✗)' "$WORK/m.out" | head -10
    exit 1
fi
if grep -q '^% not compiled' "$OUT/vmat.tex"; then
    NOTE+=("VMAT: $(grep '^% not compiled' "$OUT/vmat.tex" | head -1)")
    say "VMAT form: not compiled"
else
    say "VMAT form: $(grep -c '^\\begin{align\*}$' "$OUT/vmat.tex") displays"
fi

# 4. The canonical platform, from programs/vglp/cva/compiler.
printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$NAME" "$CT" "$VT" "$INTRO" > "$WORK/p.in"
repl "$WORK/p.in" "$WORK/p.out"
awk '
    $0 == "%% compile begin" { on = 1; next }
    $0 == "%% compile end" && on { on = 0; done = 1; next }
    on { print }
    END { if (!done) exit 1 }
' "$WORK/p.out" > "$OUT/platform.vglp"
if [ $? -ne 0 ]; then
    say "the CVA compiler printed no module"
    grep -E '(Error|error|✗)' "$WORK/p.out" | head -10
    exit 1
fi
COMPILED=1
if grep -q '^%% not compiled' "$OUT/platform.vglp"; then
    COMPILED=0
    NOTE+=("platform: $(grep '^%% not compiled' "$OUT/platform.vglp" | head -1)")
    say "platform.vglp: not compiled"
else
    say "platform.vglp: $(wc -l < "$OUT/platform.vglp" | tr -d ' ') lines"
fi

# 5. The platform compiled to GLP, by the REPL's :emit in a scratch directory
# under programs/tests, a container with no self.glp, so the scope is the root's.
RUN=0
if [ "$COMPILED" = 1 ]; then
    mkdir -p "$EMIT" && cp "$OUT/platform.vglp" "$EMIT/platform.vglp"
    printf ':emit %s\n:quit\n' "$EMIT" > "$WORK/e.in"
    repl "$WORK/e.in" "$WORK/e.out"
    if [ -f "$EMIT/platform.glp" ]; then
        cp "$EMIT/platform.glp" "$OUT/platform.glp"
        say "platform.glp: $(wc -l < "$OUT/platform.glp" | tr -d ' ') lines"
    else
        say "the canonical compilation wrote no platform.glp"
        grep -E '(Error|error|✗|skipped|failed)' "$WORK/e.out" | head -10
        exit 1
    fi
    rm -rf "$EMIT" "$GLP_DIR/programs/tests/cascade_emit"

    # 6. The run with scripted persons: the plays of programs/vglp/cva/<contract>.
    if [ -d "$PLAYS" ] && [ -f "$PLAYS/self.glp" ]; then
        if ! cmp -s "$OUT/platform.vglp" "$PLAYS/platform.vglp"; then
            say "the plays' platform.vglp in $PLAYS is not the one printed now; run programs/vglp/cva/compile.sh first"
            exit 1
        fi
        # One goal per play the plays export, a fresh variable per argument.
        GOALS=$(perl -ne '
            next unless /^exported procedure (play_[a-z0-9_]*)\((.*)\)\.\s*$/;
            my ($p, $args) = ($1, $2); my $d = 0; my $n = 1;
            for my $ch (split //, $args) { $d++ if $ch eq "("; $d-- if $ch eq ")"; $n++ if $ch eq "," && $d == 0 }
            print "$p(", join(", ", map { "V$_" } 1 .. $n), ").\n";
        ' "$PLAYS/self.glp")
        if [ -z "$GOALS" ]; then
            NOTE+=("run: $PLAYS/self.glp exports no play")
            say "run: no play exported by the plays"
        else
            { echo "$PLAYS/"; echo ':limit 600000'; printf '%s\n' "$GOALS"; echo ':quit'; } > "$WORK/r.in"
            repl "$WORK/r.in" "$WORK/r.out"
            # The plays' logs as the REPL prints them, less the limit's acknowledgement;
            # an unbound tail is printed as the heap's variable name, X<n>, which is
            # the printer's and not the run's, so it is written as _.
            awk '/^✓ Loaded program/ { on = 1; next } /^Goodbye!/ { on = 0 } /^Goal reduction limit set to/ { next } on' "$WORK/r.out" \
                | perl -pe 's/\bX[0-9]+\b/_/g' > "$OUT/run.log"
            RUN=1
            say "run.log: $(printf '%s\n' "$GOALS" | wc -l | tr -d ' ') plays, $(wc -l < "$OUT/run.log" | tr -d ' ') lines"
            if grep -qE '^(Error|✗)' "$OUT/run.log"; then say "run.log holds an error"; STATUS=1; fi
        fi
    else
        NOTE+=("run: no plays in programs/vglp/cva/$NAME")
        say "run: no plays for $NAME in programs/vglp/cva"
    fi
fi

# The manifest.
GLP_COMMIT=$(git -C "$GLP_DIR" rev-parse --short=8 HEAD 2>/dev/null || echo unknown)
if [ -d "$PAPER/.git" ]; then
    PAPER_URL=$(git -C "$PAPER" config --get remote.origin.url 2>/dev/null || echo unknown)
    PAPER_COMMIT=$(git -C "$PAPER" rev-parse --short=8 HEAD 2>/dev/null || echo unknown)
else
    PAPER_URL="$PAPER (not on this machine)"; PAPER_COMMIT=unknown
fi
{
    echo "# A run of the cascade on $NAME"
    echo ""
    echo "- Contract: \`$NAME\`, its owner's two files \`$OWNER/contract.md\` and \`$OWNER/contract.glp\`, copied here as they stood."
    echo "- The paper: \`$PAPER_URL\` at \`$PAPER_COMMIT\`, for the paper's own record."
    echo "- The tools: this repository at \`$GLP_COMMIT\` --- programs/jurix (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/$NAME (the plays with scripted persons)."
    echo "- Run on $(date -u '+%Y-%m-%d %H:%M UTC') by \`cascade.sh $NAME\`; the suite's Section CAS re-runs it and holds these files to what the tools print."
    echo ""
    echo "| Artefact | What it holds |"
    echo "|---|---|"
    echo "| \`contract.md\` | the clauses |"
    echo "| \`contract.glp\` | the act schemas, each in Jurix's notation above its term |"
    echo "| \`verdict.txt\` | the checker's verdict: \`$VT\` |"
    if grep -q '^% not compiled' "$OUT/vmat.tex"; then
        echo "| \`vmat.tex\` | the compiler to VMAT's refusal |"
    else
        echo "| \`vmat.tex\` | the VMAT form, $(grep -c '^\\begin{align\*}$' "$OUT/vmat.tex") displays, one per schema |"
    fi
    if [ "$COMPILED" = 1 ]; then
        echo "| \`platform.vglp\` | the canonical CVA platform, as printed |"
        echo "| \`platform.glp\` | the platform compiled to GLP, as emitted |"
        if [ "$RUN" = 1 ]; then
            echo "| \`run.log\` | the plays' logs: $(printf '%s\n' "$GOALS" | sed -e 's/(.*//' | tr '\n' ' ')|"
        else
            echo "| (no run) | ${NOTE[${#NOTE[@]}-1]#run: } |"
        fi
    else
        echo "| \`platform.vglp\` | the CVA compiler's refusal, and no platform.glp and no run |"
    fi
    if [ ${#NOTE[@]} -gt 0 ]; then
        echo ""
        echo "What the tools refused or lacked:"
        for n in "${NOTE[@]}"; do echo "- $n"; done
    fi
} > "$OUT/manifest.md"

# --check: each artefact in place is what the tools print now.
if [ "$MODE" = "--check" ]; then
    [ -d "$REC" ] || { say "no record in cascade/$NAME"; exit 1; }
    for f in contract.md contract.glp verdict.txt vmat.tex platform.vglp platform.glp run.log; do
        if [ -f "$OUT/$f" ] || [ -f "$REC/$f" ]; then
            if [ -f "$OUT/$f" ] && [ -f "$REC/$f" ] && cmp -s "$OUT/$f" "$REC/$f"; then
                echo "$f: as printed"
            else
                echo "$f: NOT as printed"
                [ -f "$OUT/$f" ] && [ -f "$REC/$f" ] && diff "$REC/$f" "$OUT/$f" | head -10
                STATUS=1
            fi
        fi
    done
    exit $STATUS
fi

[ "$STATUS" = 0 ] || exit 1
rm -rf "$REC" && mkdir -p "$REC" && cp "$OUT"/* "$REC"/ || { say "the record could not be written"; exit 1; }
say "written to cascade/$NAME"

# --record: one commit, tagged.
if [ "$MODE" = "--record" ]; then
    # A second run on one day is tagged cascade/<contract>/<date>-2, and so on.
    TAG="cascade/$NAME/$(date -u '+%Y-%m-%d')"
    n=1; T="$TAG"
    while git -C "$GLP_DIR" rev-parse -q --verify "refs/tags/$T" > /dev/null; do
        n=$((n + 1)); T="$TAG-$n"
    done
    TAG="$T"
    git -C "$GLP_DIR" add "cascade/$NAME" \
        && git -C "$GLP_DIR" commit -q -m "A run of the cascade on $NAME, recorded whole ($TAG): the contract, the verdict $VT, the VMAT form, the platform and its run, by cascade.sh at $GLP_COMMIT" -- "cascade/$NAME" \
        && git -C "$GLP_DIR" tag "$TAG" \
        && say "recorded as $(git -C "$GLP_DIR" rev-parse --short=8 HEAD), tagged $TAG" \
        || { say "the record was not committed"; exit 1; }
fi
exit 0
