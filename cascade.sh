#!/bin/bash
# A run of the cascade on a contract, recorded whole (claude.md "Code";
# Coordination 5.7, "The cascade"; Udi, 2026-10-17): from the contract's
# clauses and act schemas in its owner's directory through every tool of the
# cascade, each in this repository, to the canonical platform and its run.
#
#   bash cascade.sh <contract>            run, and write cascade/<NAME>/
#   bash cascade.sh <contract> --check    run into a scratch directory and compare
#                                         with cascade/<NAME>/
#   bash cascade.sh <contract> --record   run, write, and commit the record as one
#                                         commit tagged cascade/<NAME>/<date>
#                                         (a later run the same day: <date>-2, -3, ...)
#   bash cascade.sh <contract> <contract> ... [--check | --record]
#                                         the same on the composition of the
#                                         contracts, recorded in
#                                         cascade/<NAME>+<NAME>/ (vGLP 7c631b1,
#                                         Section 5.4, "The contract's words":
#                                         "The compiler takes one contract or
#                                         several"; compose.sh beside pragmas.sh)
#
# A contract's name and its owner are Udi's rulings, Coordination Appendix C,
# never read off a directory's name (Udi, 2026-10-18): a letter acronym NAME,
# which the checker takes in lower case, <contract>.  Each is two files the
# owner writes in its directory (claude.md "Code", the artefacts' discipline:
# GAI #5's task, Code_inbox.md 2026-10-20 12:38 UTC, Udi approving it, reported
# by GAI): NAME.txt, the contract, its lines beginning with % the meta-information
# and the rest the contract as the paper's box prints it; and NAME.glp, the act
# schemas in the term form programs/gsc reads, printed by
# contract_named(<contract>, C), the one source of the schemas.  vGLP keeps the
# plays with scripted persons for some, in directories under programs/vglp/cva/
# that keep their names, and an owner keeps them for its own, beside its
# program:
#
#   <contract>  NAME  owner       files in                  plays in
#   gkk         GKK   GSC         programs/gsc              programs/apps/gkk (GSC's, the app's plays)
#   gfp         GFP   GSC         programs/gsc              ---
#   gpd         GPD   GSA         programs/social/graph     ---
#   gas         GAS   GSA         programs/social/graph     ---
#   grv         GRV   GSA         programs/examples/rendezvous ---
#   gsv         GSV   GSA         programs/examples/rendezvous ---
#   gds         GDS   GSA         programs/examples/rendezvous ---
#   gpe         GPE   GSA         programs/social/graph     ---
#   gsg         GSG   GSC         programs/gsc              social_graph
#   gsn         GSN   GSC         programs/gsc              social_network
#   gnw         GnW   GSC         programs/gsc              ---
#   gsnp        GSNP  GAI         programs/gai              ---
#   gc          GC    Currencies  programs/currencies/coins currency
#   gb          GB    Currencies  programs/currencies/bonds ---
#   cssn        CSSN  CSSN        programs/cssn             programs/cssn (CSSN's, the village)
#   gfwc        GFWC  GFWC        programs/federation       ---
#   gai         GAI   GAI         programs/gai              ---
#   gff         GFF   GFF         programs/gff              programs/apps/gff (GFF's, the app's plays)
#   agora       AGORA Currencies  programs/currencies/agora ---
#   gdo         GDO   GRH         programs/lbm              ---
#   dgc         DGC   Currencies  programs/currencies/sovereign ---
#   gcc         GCC   Currencies  programs/currencies/gcc   ---
#   gdc         GDC   Currencies  programs/currencies/gdc   ---
#   bcc         BCC   Currencies  programs/currencies/bcc   ---
#   gra         GRA   GRH         programs/lbm              ---
#   gff_i       GFF-I GFF         programs/gff              ---
#   gff_ps      GFF-PS GFF        programs/gff              ---
#   gff_fr      GFF-FR GFF        programs/gff              ---
#   gff_fr_bc   GFF-FR-BC GFF     programs/gff              ---
#   gd          GD    GGGA        programs/ggga             ---
#   gd_s        GD-S  GGGA        programs/ggga             ---
#   gdo_l       GDO-L GFF         programs/gff              ---
#   dao         DAO   GCE         programs/gce/dao          ---
#   ldao        LDAO  GCE         programs/gce/ldao         ---
#   fdao        FDAO  GCE         programs/gce/fdao         ---
#   rec         REC   GCE         programs/gce/rec          ---
#   stay        STAY  GCE         programs/gce/stay         ---
#   lib         LIB   GCE         programs/gce/lib          ---
#   joint       JOINT GCE         programs/gce/joint        ---
#
# The steps, and the artefacts written to cascade/<NAME>/ at the repository
# root --- outside programs/, since every .glp and .vglp under a program's
# directory is a module of that program to the linker and to :emit, and a
# record is not a program:
#   1. NAME.txt and NAME.glp, copied as they stand at the run.
#   2. schemas.txt: the act schemas in GSC's notation (GSC, Definition "Act
#      schema", def:schema and Appendix D's definition:schema), printed from
#      the term by programs/gsc's notation_contract/1.
#   3. verdict.txt: the checker's verdict on the schemas, programs/gsc's
#      check/2 (GSC, def:syntactically-grassroots; Section 8's conditions
#      for a contract with community roles).
#   4. vmat.tex: the VMAT form, the volition-guarded multiagent atomic
#      transactions each schema compiles to, as programs/gsc's
#      compile_contract/2 prints them (GSC, Section 5.2; Section 8's worked
#      box), or its line saying why the contract is not compiled.
#   5. platform.vglp: the canonical CVA platform, as programs/vglp/cva/compiler's
#      compile/5 prints it (vGLP, Section 5.4, Definition "Compilation of the
#      Canonical Platform"), with the contract's pragmas, the words its app
#      shows, read from NAME.txt and from the NAME.txt of the contracts it
#      extends by programs/vglp/cva/compiler/pragmas.sh, each act the schema
#      its button names (vGLP, Section 5.4, "The contract's words"; GSC,
#      Section 2), or its lines saying why it is not compiled.
#   6. platform.glp: the platform compiled to GLP by the canonical compilation,
#      as the REPL's :emit writes it (vGLP, Definition "Canonical Compilation");
#      and interface.glp beside it, the platform's interface in sGLP's term
#      form, its types and volitional declarations as platform.vglp has them
#      and compiled(P, N) of each procedure of platform.glp, as
#      programs/sglp/cva/interface.sh prints them, the interface an owner's
#      simulation of the platform under sGLP is transformed against
#      (programs/sglp/transform.sh --interface; sGLP's task of 2026-10-18
#      20:50 UTC, item 1).
#   7. run.log: the run of the platform with scripted persons, the plays vGLP
#      or the owner keeps for it, each play's logs as the REPL prints
#      them; the plays run the platform.vglp committed there, which the run
#      requires to be the one this script printed.
# and manifest.md, the index of the directory, naming the contract, its owner,
# the paper's repository and commit, the GLP commit the tools were at, and
# what each step gave.
#
# A step that gives nothing --- a contract the checker refuses is not compiled,
# an uncompiled platform has no .glp and no run, a platform with no plays has no
# run --- writes no artefact for it, and the manifest says so; the script
# records what the tools print and bypasses nothing.  Exits 0 when every step
# ran and, with --check, every artefact in place is what the tools print now;
# 1 otherwise, the differing file or the failed step named; 2 on a bad
# argument or a contract with no NAME.txt or NAME.glp.  With --check, an
# artefact in the record that the run does not make, or one the run makes that
# the record lacks, is not as printed.
#
# TRANSITION (GAI #5's task, Code_inbox.md 2026-10-20 12:38 UTC, "Order"): an
# owner converts NAME.md to NAME.txt once this script reads the .txt; until
# then the script reads NAME.md where there is no NAME.txt and copies it into
# the record as NAME.md.  The fallback goes when the last owner has converted:
# the lines marked TRANSITION below.
#
# The suite's Section CAS runs --check on every recorded contract.

set -u
GLP_DIR="$(cd "$(dirname "$0")" && pwd)"
RT="$GLP_DIR/glp_runtime"

usage() { echo "usage: cascade.sh <contract> [<contract> ...] [--check | --record]" >&2; exit 2; }
[ $# -ge 1 ] || usage
MODE=write
case "${!#}" in --check|--record) MODE="${!#}"; set -- "${@:1:$#-1}" ;; esac
[ $# -ge 1 ] || usage
NAMES=("$@")

# lookup <contract>: its NAME.txt and NAME.glp's name, FILE, its owner's
# directory, OWNER, its paper, PAPER, and its plays, PLAYDIR, PLAYAT and
# PLAYLIMIT.
lookup() {
PLAYAT=""          # the owner's plays, a directory of the repository, where it keeps them
PLAYLIMIT=600000   # the reductions the plays' REPL session may take
case "$1" in
    gfp)  FILE=GFP;  OWNER="programs/gsc";              PAPER="/Users/udi/Grassroots/GSC";  PLAYDIR="" ;;   # GSC's, extends GSG (GSC #13, 2026-11-07)
    gpd)  FILE=GPD;  OWNER="programs/social/graph";      PAPER="/Users/udi/Grassroots/GSA";  PLAYDIR="" ;;   # GSA's, extends GFP (GSA #2, 2026-11-07)
    gas)  FILE=GAS;  OWNER="programs/social/graph";      PAPER="/Users/udi/Grassroots/GSA";  PLAYDIR="" ;;   # GSA's, extends GPD (GSA #2, 2026-11-07)
    grv)  FILE=GRV;  OWNER="programs/examples/rendezvous"; PAPER="/Users/udi/Grassroots/GNA";  PLAYDIR="" ;;   # GSA's (GNA), extends GSV (GSA #2, 2026-11-07; on GSV, GSA #3, 2026-11-10)
    gsv)  FILE=GSV;  OWNER="programs/examples/rendezvous"; PAPER="/Users/udi/Grassroots/GNA";  PLAYDIR="" ;;   # GSA's (GNA), instantiates GnW (GSA #3, 2026-11-10)
    gds)  FILE=GDS;  OWNER="programs/examples/rendezvous"; PAPER="/Users/udi/Grassroots/GNA";  PLAYDIR="" ;;   # GSA's (GNA), extends GSV (GSA #2, 2026-11-08; on GSV, GSA #3, 2026-11-10)
    gpe)  FILE=GPE;  OWNER="programs/social/graph";      PAPER="/Users/udi/Grassroots/GSA";  PLAYDIR="" ;;   # GSA's, extends GKK (GSA #2, 2026-11-07)
    gkk)  FILE=GKK;  OWNER="programs/gsc";              PAPER="/Users/udi/Grassroots/GSC";  PLAYDIR=""
          PLAYAT="programs/apps/gkk" ;;
    gsg)  FILE=GSG;  OWNER="programs/gsc";              PAPER="/Users/udi/Grassroots/GSC";  PLAYDIR="social_graph" ;;
    gsn)  FILE=GSN;  OWNER="programs/gsc";              PAPER="/Users/udi/Grassroots/GSC";  PLAYDIR="social_network"
          PLAYLIMIT=6000000 ;;   # the standing approvals' Requests take GSN's play past 600000 (vGLP f0acbc1; Code #12, 2026-10-20)
    gnw)  FILE=GnW;  OWNER="programs/gsc";              PAPER="/Users/udi/Grassroots/GSC";  PLAYDIR="" ;;   # GSC's, amends GSG (Udi, 2026-11-10, to GSC #14; GAI's programs/gai until 6415bff0)
    gsnp) FILE=GSNP; OWNER="programs/gai";              PAPER="/Users/udi/Grassroots/GAI";  PLAYDIR="" ;;
    gc)   FILE=GC;   OWNER="programs/currencies/coins"; PAPER="/Users/udi/Grassroots/GC";   PLAYDIR="currency" ;;
    gb)   FILE=GB;   OWNER="programs/currencies/bonds"; PAPER="/Users/udi/Grassroots/GB";   PLAYDIR="" ;;
    cssn) FILE=CSSN; OWNER="programs/cssn";             PAPER="/Users/udi/Grassroots/CSSN"; PLAYDIR=""
          PLAYAT="programs/cssn"; PLAYLIMIT=100000000 ;;
    gfwc) FILE=GFWC; OWNER="programs/federation";       PAPER="/Users/udi/Grassroots/GFWC"; PLAYDIR="" ;;
    gai)  FILE=GAI;  OWNER="programs/gai";              PAPER="/Users/udi/Grassroots/GAI";  PLAYDIR="" ;;
    gff)  FILE=GFF;  OWNER="programs/gff";              PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR=""
          PLAYAT="programs/apps/gff"; PLAYLIMIT=30000000 ;;   # the five persons' play, the same limit as Section APPS's
    agora) FILE=AGORA; OWNER="programs/currencies/agora"; PAPER="";                           PLAYDIR="" ;;
    gdo)  FILE=GDO;  OWNER="programs/lbm";              PAPER="/Users/udi/Grassroots/GRH";  PLAYDIR="" ;;
    dgc)  FILE=DGC;  OWNER="programs/currencies/sovereign"; PAPER="/Users/udi/Grassroots/SGC"; PLAYDIR="" ;;
    gcc)  FILE=GCC;  OWNER="programs/currencies/gcc";   PAPER="/Users/udi/Grassroots/GCC";  PLAYDIR="" ;;
    gdc)  FILE=GDC;  OWNER="programs/currencies/gdc";   PAPER="/Users/udi/Grassroots/GCC";  PLAYDIR="" ;;
    bcc)  FILE=BCC;  OWNER="programs/currencies/bcc";   PAPER="/Users/udi/Grassroots/GCC";  PLAYDIR="" ;;
    gra)  FILE=GRA;  OWNER="programs/lbm";              PAPER="/Users/udi/Grassroots/GRH";  PLAYDIR="" ;;
    gff_i) FILE=GFF-I; OWNER="programs/gff";               PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR="" ;;
    gff_ps) FILE=GFF-PS; OWNER="programs/gff";             PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR="" ;;
    gff_fr) FILE=GFF-FR; OWNER="programs/gff";             PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR="" ;;
    gff_fr_bc) FILE=GFF-FR-BC; OWNER="programs/gff";       PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR="" ;;   # GFF's, extends GFF-FR (Udi, 2026-11-07; GFF #3)
    gd)   FILE=GD;   OWNER="programs/ggga";             PAPER="/Users/udi/Grassroots/SRC";  PLAYDIR="" ;;   # SRC, a paper of the GGGA project, prints GD (Udi, 2026-11-05; GGGA #3)
    gd_s) FILE=GD-S; OWNER="programs/ggga";             PAPER="/Users/udi/Grassroots/SRC";  PLAYDIR="" ;;
    gdo_l) FILE=GDO-L; OWNER="programs/gff";            PAPER="/Users/udi/Grassroots/GFF";  PLAYDIR="" ;;   # GFF's, extends GDO (Udi, 2026-11-05; GFF #3)
    dao) FILE=DAO;  OWNER="programs/gce/dao";         PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends GKK (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    ldao) FILE=LDAO; OWNER="programs/gce/ldao";        PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends DAO (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    fdao) FILE=FDAO; OWNER="programs/gce/fdao";        PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends DAO and GB (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    rec) FILE=REC;  OWNER="programs/gce/rec";         PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends GKK (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    stay) FILE=STAY; OWNER="programs/gce/stay";        PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends GB (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    lib) FILE=LIB;  OWNER="programs/gce/lib";         PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends GB (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    joint) FILE=JOINT; OWNER="programs/gce/joint";       PAPER="";                            PLAYDIR="" ;;   # Nimrod Talmon's example contract, extends GC (pull request 9; GCE, Grassroots Contract Examples, Nimrod Talmon's: Udi, 2026-11-09)
    *) echo "cascade.sh: no contract named $1; the names are gkk gfp gpd gas grv gsv gds gpe gsg gsn gnw gsnp gc gb cssn gfwc gai gff agora gdo dgc gcc gdc bcc gra gd gd_s gdo_l gff_i gff_ps gff_fr gff_fr_bc dao ldao fdao rec stay lib joint" >&2; exit 2 ;;
esac
}
# Each contract named: its files, NAME.txt (NAME.md where the owner has not
# yet converted it) and NAME.glp.
FILES=(); OWNERS=(); PAPERS=(); SRCS=(); CTXTS=()
for n in "${NAMES[@]}"; do
    lookup "$n"
    SRC="$GLP_DIR/$OWNER"
    CTXT="$FILE.txt"
    [ -f "$SRC/$CTXT" ] || CTXT="$FILE.md"   # TRANSITION: NAME.md until its owner converts it
    for f in "$CTXT" "$FILE.glp"; do
        [ -f "$SRC/$f" ] || { echo "cascade.sh: $OWNER/$f is missing; the contract is its owner's two files, $FILE.txt and $FILE.glp" >&2; exit 2; }
    done
    FILES+=("$FILE"); OWNERS+=("$OWNER"); PAPERS+=("$PAPER"); SRCS+=("$SRC"); CTXTS+=("$CTXT")
done
# Several contracts are their composition (vGLP 7c631b1, Section 5.4, "The
# contract's words": "The compiler takes one contract or several: of several it
# prints the platform of their composition, the union of their schemas and of
# their pragmas, which extends each"), recorded under the names joined by +,
# as cascade/GSN+GC, and named to the checker and the compiler by theirs joined
# by _, as gsn_gc; vGLP keeps no plays for a composition.
if [ ${#NAMES[@]} -eq 1 ]; then
    NAME="${NAMES[0]}"
    PLAYS=""; [ -n "$PLAYDIR" ] && PLAYS="$GLP_DIR/programs/vglp/cva/$PLAYDIR"
    [ -n "$PLAYAT" ] && PLAYS="$GLP_DIR/$PLAYAT"
else
    NAME=$(IFS=_; echo "${NAMES[*]}")
    FILE=$(IFS=+; echo "${FILES[*]}")
    PLAYS=""
fi
REC="$GLP_DIR/cascade/$FILE"

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
# read_contract <name> <directory> <NAME>: the contract's term, CT, its
# reading, RD, and its introduction predicate, INTRO.
read_contract() {
local n="$1" src="$2" f="$3"
printf '%s\ncontract_named(%s, C).\n:quit\n' "$src/$f.glp" "$n" > "$WORK/c.in"
repl "$WORK/c.in" "$WORK/c.out"
CT=$(grep -E '^C = ' "$WORK/c.out" | head -1 | sed -e 's/^C = //')
if [ -z "$CT" ]; then
    say "$f.glp prints no contract_named($n, C)"
    grep -E '(Error|error|✗)' "$WORK/c.out" | head -10
    exit 1
fi
# The contract as GSC's reader reads it, as_read/2 of programs/gsc, each schema of the clause syntax, sas/7, in the checker's form with its roles' names, and a schema in that form as it is: the compiler and pragmas.sh take this and keep no copy of the reader (vGLP, Section 5.4, "Speech-act schemas": "A compiler written in GLP takes such a contract ... written as speech-act schemas [GSC]"; vGLP #23, Code_inbox.md, 2026-11-05 14:20 UTC, 1).
printf '%s\n:limit 10000000\nas_read(%s, R).\n:quit\n' "$GLP_DIR/programs/gsc/" "$CT" > "$WORK/a.in"
repl "$WORK/a.in" "$WORK/a.out"
RD=$(grep -E '^R = ' "$WORK/a.out" | head -1 | sed -e 's/^R = //')
if [ -z "$RD" ]; then
    say "GSC's reader, as_read/2, gave no reading of the contract"
    grep -E '(Error|error|✗)' "$WORK/a.out" | head -10
    exit 1
fi
INTRO=no_introduction
if grep -q "^introduction_named($n," "$src/$f.glp"; then
    printf '%s\nintroduction_named(%s, I).\n:quit\n' "$src/$f.glp" "$n" > "$WORK/i.in"
    repl "$WORK/i.in" "$WORK/i.out"
    IT=$(grep -E '^I = ' "$WORK/i.out" | head -1 | sed -e 's/^I = //')
    [ -n "$IT" ] && INTRO="$IT"
fi
}
if [ ${#NAMES[@]} -eq 1 ]; then
    cp "$SRC/$CTXT" "$OUT/$CTXT"
    cp "$SRC/$FILE.glp" "$OUT/$FILE.glp"
    read_contract "$NAME" "$SRC" "$FILE"
else
    # The composition: each contract read, with its pragmas, as when it is
    # compiled alone, and composed by programs/vglp/cva/compiler/compose.sh.
    COMP=()
    for i in "${!NAMES[@]}"; do
        cp "${SRCS[i]}/${CTXTS[i]}" "$OUT/${CTXTS[i]}"
        cp "${SRCS[i]}/${FILES[i]}.glp" "$OUT/${FILES[i]}.glp"
        read_contract "${NAMES[i]}" "${SRCS[i]}" "${FILES[i]}"
        P=no_pragmas
        if [ "${CTXTS[i]}" = "${FILES[i]}.txt" ] && ! P=$(bash "$GLP_DIR/programs/vglp/cva/compiler/pragmas.sh" "${SRCS[i]}/${CTXTS[i]}" "$RD" 2> "$WORK/pr.err"); then
            say "the pragmas of ${FILES[i]} were not read: $(head -1 "$WORK/pr.err")"
            exit 1
        fi
        COMP+=("${FILES[i]}" "$RD" "$P" "$INTRO")
    done
    if ! bash "$GLP_DIR/programs/vglp/cva/compiler/compose.sh" "${COMP[@]}" > "$WORK/compose.out" 2> "$WORK/compose.err"; then
        say "the composition was refused: $(head -1 "$WORK/compose.err")"
        exit 1
    fi
    RD=$(sed -n 1p "$WORK/compose.out"); PR=$(sed -n 2p "$WORK/compose.out"); INTRO=$(sed -n 3p "$WORK/compose.out")
    CT="$RD"
    say "composition of ${FILES[*]}: $(printf '%s' "$RD" | grep -oE '\b[cnq]?schema\(' | wc -l | tr -d ' ') schemas"
fi

# 2. The schemas in GSC's notation, printed from the term by programs/gsc: a
# token to a line between its lines "%% notation begin" and "%% notation end",
# an empty line ending a line of the text (programs/gsc/notation.glp).
printf '%s\n:limit 10000000\nnotation_contract(%s).\n:quit\n' "$GLP_DIR/programs/gsc/" "$CT" > "$WORK/n.in"
repl "$WORK/n.in" "$WORK/n.out"
perl -ne '
    chomp;
    if ($_ eq "%% notation begin") { $on = 1; next }
    if ($on && $_ eq "%% notation end") { $on = 0; $done = 1; next }
    next unless $on;
    if ($_ eq "") { print "$line\n"; $line = "" } else { $line .= $_ }
    END { exit($done ? 0 : 1) }
' "$WORK/n.out" > "$OUT/schemas.txt"
if [ $? -ne 0 ] || [ ! -s "$OUT/schemas.txt" ]; then
    say "the notation of the schemas was not printed"
    grep -E '(Error|error|✗)' "$WORK/n.out" | head -10
    exit 1
fi
# A schema is the clause tau: H <- B, gamma guarded by G, its first line
# "tau: H <-", H empty in a schema that adds and deletes nothing, and its body
# on the next line, indented (GSC, Section 3.2; programs/gsc/notation.glp).
say "schemas.txt: $(grep -cE '^[a-z_0-9]*: .* <-$' "$OUT/schemas.txt") schemas"

# 3. The verdict, and 4. the VMAT form, from programs/gsc.
printf '%s\n:limit 10000000\ncheck(%s, V).\n:quit\n' "$GLP_DIR/programs/gsc/" "$CT" > "$WORK/v.in"
repl "$WORK/v.in" "$WORK/v.out"
VT=$(grep -E '^V = ' "$WORK/v.out" | head -1 | sed -e 's/^V = //')
if [ -z "$VT" ]; then
    say "the checker gave no verdict"
    grep -E '(Error|error|✗)' "$WORK/v.out" | head -10
    exit 1
fi
printf '%s\n' "$VT" > "$OUT/verdict.txt"
say "verdict: $VT"

printf '%s\n:limit 10000000\ncompile_contract(%s, %s).\n:quit\n' "$GLP_DIR/programs/gsc/" "$NAME" "$CT" > "$WORK/m.in"
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

# 5. The canonical platform, from programs/vglp/cva/compiler, with the
# contract's words, its pragmas.
if [ ${#NAMES[@]} -eq 1 ]; then
    PR=no_pragmas
    if [ "$CTXT" = "$FILE.txt" ] && ! PR=$(bash "$GLP_DIR/programs/vglp/cva/compiler/pragmas.sh" "$SRC/$CTXT" "$RD" 2> "$WORK/pr.err"); then
        say "its pragmas were not read: $(head -1 "$WORK/pr.err")"
        exit 1
    fi
fi
printf '%s\n:limit 1000000000\ncompile(%s, %s, %s, %s, %s).\n:quit\n' "$GLP_DIR/programs/vglp/cva/compiler/" "$NAME" "$RD" "$VT" "$INTRO" "$PR" > "$WORK/p.in"
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

# 6. The platform compiled to GLP, by the REPL's :emit in a scratch directory
# under programs/tests, a container with no self.glp, so the scope is the root's.
RUN=0
if [ "$COMPILED" = 1 ]; then
    mkdir -p "$EMIT" && cp "$OUT/platform.vglp" "$EMIT/platform.vglp"
    printf ':emit %s\n:quit\n' "$EMIT" > "$WORK/e.in"
    repl "$WORK/e.in" "$WORK/e.out"
    if [ -f "$EMIT/platform.glp" ]; then
        cp "$EMIT/platform.glp" "$OUT/platform.glp"
        say "platform.glp: $(wc -l < "$OUT/platform.glp" | tr -d ' ') lines"
        if bash "$GLP_DIR/programs/sglp/cva/interface.sh" "$OUT/platform.vglp" "$OUT/platform.glp" "$FILE" \
                > "$OUT/interface.glp" 2> "$WORK/iface.err"; then
            say "interface.glp: $(grep -c '^  ' "$OUT/interface.glp") items"
        else
            say "the platform's interface in sGLP's term form was not printed"
            head -5 "$WORK/iface.err"
            exit 1
        fi
    else
        say "the canonical compilation wrote no platform.glp"
        grep -E '(Error|error|✗|skipped|failed)' "$WORK/e.out" | head -10
        exit 1
    fi
    rm -rf "$EMIT" "$GLP_DIR/programs/tests/cascade_emit"

    # 7. The run with scripted persons: the plays vGLP keeps for the contract.
    if [ -n "$PLAYS" ] && [ -d "$PLAYS" ] && [ -f "$PLAYS/self.glp" ]; then
        if ! cmp -s "$OUT/platform.vglp" "$PLAYS/platform.vglp"; then
            say "the plays' platform.vglp in $PLAYS is not the one printed now; run its compile.sh first (programs/vglp/cva/compile.sh, or the owner's)"
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
            { echo "$PLAYS/"; echo ":limit $PLAYLIMIT"; printf '%s\n' "$GOALS"; echo ':quit'; } > "$WORK/r.in"
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
        NOTE+=("run: vGLP keeps no plays for $NAME")
        say "run: vGLP keeps no plays for $NAME"
    fi
fi

# The manifest.
GLP_COMMIT=$(git -C "$GLP_DIR" rev-parse --short=8 HEAD 2>/dev/null || echo unknown)
paper_line() {   # paper_line <paper directory>: its remote and its commit
    if [ -d "$1/.git" ]; then
        echo "\`$(git -C "$1" config --get remote.origin.url 2>/dev/null || echo unknown)\` at \`$(git -C "$1" rev-parse --short=8 HEAD 2>/dev/null || echo unknown)\`"
    else
        echo "\`$1 (not on this machine)\` at \`unknown\`"
    fi
}
{
    echo "# A run of the cascade on $FILE"
    echo ""
    if [ ${#NAMES[@]} -eq 1 ]; then
        echo "- Contract: \`$FILE\` (\`$NAME\` to the checker), its owner's two files \`$OWNER/$CTXT\` and \`$OWNER/$FILE.glp\`, copied here as they stood at \`$GLP_COMMIT\`."
        echo "- The paper: $(paper_line "$PAPER"), for the paper's own record."
    else
        echo "- Contract: the composition of ${FILES[*]} (\`$NAME\` to the checker), the union of their schemas and of their pragmas (vGLP, Section 5.4, \"The contract's words\"), by programs/vglp/cva/compiler/compose.sh; each its owner's two files, copied here as they stood at \`$GLP_COMMIT\`:"
        for i in "${!NAMES[@]}"; do
            echo "  - \`${FILES[i]}\`: \`${OWNERS[i]}/${CTXTS[i]}\` and \`${OWNERS[i]}/${FILES[i]}.glp\`; the paper $(paper_line "${PAPERS[i]}")."
        done
    fi
    echo "- The tools: this repository at \`$GLP_COMMIT\` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP)${PLAYS:+, ${PLAYS#$GLP_DIR/} (the plays with scripted persons)}."
    echo "- Run on $(date -u '+%Y-%m-%d %H:%M UTC') by \`cascade.sh ${NAMES[*]}\`; the suite's Section CAS re-runs it and holds these files to what the tools print."
    echo ""
    echo "| Artefact | What it holds |"
    echo "|---|---|"
    for i in "${!NAMES[@]}"; do
        echo "| \`${CTXTS[i]}\` | the contract${FILES[1]:+ ${FILES[i]}} |"
        echo "| \`${FILES[i]}.glp\` | the act schemas${FILES[1]:+ of ${FILES[i]}}, as terms |"
    done
    echo "| \`schemas.txt\` | the act schemas in GSC's notation, printed from the terms |"
    echo "| \`verdict.txt\` | the checker's verdict: \`$VT\` |"
    if grep -q '^% not compiled' "$OUT/vmat.tex"; then
        echo "| \`vmat.tex\` | the compiler to VMAT's refusal |"
    else
        echo "| \`vmat.tex\` | the VMAT form, $(grep -c '^\\begin{align\*}$' "$OUT/vmat.tex") displays, one per schema |"
    fi
    if [ "$COMPILED" = 1 ]; then
        echo "| \`platform.vglp\` | the canonical CVA platform, as printed |"
        echo "| \`platform.glp\` | the platform compiled to GLP, as emitted |"
        echo "| \`interface.glp\` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |"
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
    [ -d "$REC" ] || { say "no record in cascade/$FILE"; exit 1; }
    # Every artefact the run made and every one in the record, the manifest
    # aside, which names the run.
    for f in $( (ls "$OUT"; ls "$REC") | grep -vx manifest.md | sort -u); do
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
say "written to cascade/$FILE"

# --record: one commit, tagged.
if [ "$MODE" = "--record" ]; then
    # A second run on one day is tagged cascade/<NAME>/<date>-2, and so on.
    # The refs are packed first: on a disc that ignores case a loose tag
    # directory of another spelling, cascade/gai beside cascade/GAI, takes the
    # new tag in under its own name.
    git -C "$GLP_DIR" pack-refs --all
    TAG="cascade/$FILE/$(date -u '+%Y-%m-%d')"
    n=1; T="$TAG"
    while git -C "$GLP_DIR" rev-parse -q --verify "refs/tags/$T" > /dev/null; do
        n=$((n + 1)); T="$TAG-$n"
    done
    TAG="$T"
    git -C "$GLP_DIR" add "cascade/$FILE" \
        && git -C "$GLP_DIR" commit -q -m "A run of the cascade on $FILE, recorded whole ($TAG): the contract, the verdict $VT, the VMAT form, the platform and its run, by cascade.sh at $GLP_COMMIT" -- "cascade/$FILE" \
        && git -C "$GLP_DIR" tag "$TAG" \
        && say "recorded as $(git -C "$GLP_DIR" rev-parse --short=8 HEAD), tagged $TAG" \
        || { say "the record was not committed"; exit 1; }
fi
exit 0
