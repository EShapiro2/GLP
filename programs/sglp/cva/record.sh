#!/bin/bash
# A simulation of a canonical CVA platform under sGLP, recorded whole (sGLP's
# task of 2026-10-18 20:50 UTC, item 3, and its point 3 of 20:27 UTC;
# claude.md, "Code", the simulation of a platform under sGLP): the owner's sGLP
# source run on the platform its cascade record compiled, by the harness of
# every canonical platform (run.sh, self.glp.in and harness.glp.in beside this
# script).
#
#   bash programs/sglp/cva/record.sh <NAME> <sglp> --cascade <tag|dir>
#        [--agents <n>] [--until '<count> <unit>'] [--seeds <s1>,<s2>,...]
#        [--root <dir>] [--record]
#   bash programs/sglp/cva/record.sh --check <record>
#
# <NAME> is the contract's name and <sglp> the owner's <NAME>_sglp.glp,
# programs/<owner>/<NAME>_sglp.glp, in the form transform.sh --interface
# reads.  --cascade names the platform: a tag of the cascade's record,
# cascade/<NAME>/<date>, whose platform.glp and interface.glp git holds; or a
# directory holding the two, which a test names.  The run is the run
# declaration's, its agents (census where it names a census), horizon and
# seed, or as --agents, --until and
# --seeds give them, one run per seed.  The record is written into
# <root>/<NAME>/<date>/, <root> being simulation/ at the repository root, beside
# cascade/, or --root's directory; a later record the same day goes to
# <date>-2, -3, ...  It holds:
#
#   <NAME>_sglp.glp   the owner's sGLP source as it stood;
#   profiles.glp, population.glp
#                     printed by transform.sh --interface from it over the
#                     platform's interface;
#   <seed>.log, <seed>.draw
#                     each run's log and the population's draw (run.sh);
#   manifest.md       the record's lines, each "- <key>: <value>": the name, the
#                     cascade tag or directory whose .glp it ran and its commit,
#                     the commits of the owner's sGLP source and of
#                     programs/sglp, the repository's, the agents, the
#                     horizon, the seeds; then each run's size and clock.
#
# With --record, under simulation/ only, the record is one commit, path-limited,
# tagged simulation/<NAME>/<date> (-2, -3, ... for a later one the same day).
#
# --check <record> re-runs the transformation on the record's sGLP source,
# holding its printed modules to the record's (transform.sh --interface
# --check), and each seed's first simulated day, holding the log's entries up
# to 86400 s, or up to the record's horizon where it is shorter, to the
# record's.  The suite's Section SGLP runs it on every record under simulation/.
#
# Exits 0 when every step ran and, with --check, everything is as recorded; 1
# otherwise, the differing file or the failed step named; 2 on a bad argument.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
SGLP="$(cd "$HERE/.." && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"

usage() {
    echo "usage: record.sh <NAME> <sglp> --cascade <tag|dir> [--agents <n>] [--until '<count> <unit>'] [--seeds <s>,...] [--root <dir>] [--record]" >&2
    echo "       record.sh --check <record>" >&2
    exit 2
}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# cascade_files <tag|dir> <NAME> <dest>: platform.glp and interface.glp of the
# cascade's record into <dest>, and its commit on stdout.
cascade_files() {
    local src=$1 name=$2 dest=$3
    if [ -d "$src" ]; then
        cp "$src/platform.glp" "$dest/platform.glp" 2>/dev/null &&
            cp "$src/interface.glp" "$dest/interface.glp" 2>/dev/null || return 1
        echo "directory"
    else
        git -C "$GLP_DIR" rev-parse -q --verify "refs/tags/$src" > /dev/null || return 1
        git -C "$GLP_DIR" show "$src:cascade/$name/platform.glp" > "$dest/platform.glp" 2>/dev/null &&
            git -C "$GLP_DIR" show "$src:cascade/$name/interface.glp" > "$dest/interface.glp" 2>/dev/null || return 1
        git -C "$GLP_DIR" rev-parse --short=8 "$src^{commit}"
    fi
}

# field <manifest> <key>: the value of the manifest's line "- <key>: <value>".
field() { sed -n "s/^- $2: //p" "$1" | head -1; }

# first_day <log> <secs>: the log's entries of time up to <secs>.
first_day() { awk -F'\t' -v h="$2" 'NF == 4 && $1 + 0 <= h + 0' "$1"; }

if [ $# -ge 1 ] && [ "$1" = "--check" ]; then
    [ $# -eq 2 ] && [ -d "$2" ] && [ -f "$2/manifest.md" ] || usage
    REC="$(cd "$2" && pwd)"; M="$REC/manifest.md"
    NAME=$(field "$M" name); CAS=$(field "$M" cascade); SRCB=$(field "$M" source)
    N=$(field "$M" agents); UNTIL=$(field "$M" until); SEEDS=$(field "$M" seeds)
    [ -n "$NAME" ] && [ -n "$CAS" ] && [ -n "$SRCB" ] && [ -n "$N" ] && [ -n "$UNTIL" ] && [ -n "$SEEDS" ] ||
        { echo "record.sh: $M lacks a line of the record"; exit 1; }
    STATUS=0
    cascade_files "$CAS" "$NAME" "$WORK" > /dev/null ||
        { echo "record.sh: the cascade $CAS gives no platform.glp and interface.glp of $NAME"; exit 1; }
    TX=$(bash "$SGLP/transform.sh" --interface "$WORK/interface.glp" --source "$REC/$SRCB" --out "$REC" --check 2>&1)
    for f in profiles.glp population.glp; do
        if printf '%s\n' "$TX" | grep -q "^$f: as printed$"; then echo "$f: as printed"; else echo "$f: NOT as printed"; STATUS=1; fi
    done
    [ "$STATUS" = 0 ] || printf '%s\n' "$TX" | grep -v ': as printed$' | head -10
    read -r COUNT UNIT <<< "$UNTIL"
    SECS=$(awk -v c="$COUNT" -v u="$UNIT" 'BEGIN {
        s["second"] = s["seconds"] = 1; s["minute"] = s["minutes"] = 60; s["hour"] = s["hours"] = 3600
        s["day"] = s["days"] = 86400; s["week"] = s["weeks"] = 604800; s["year"] = s["years"] = 31557600
        print (u in s) ? c * s[u] : -1 }')
    [ "$SECS" != "-1" ] || { echo "record.sh: the horizon $UNTIL has no unit of time"; exit 1; }
    if awk -v x="$SECS" 'BEGIN { exit !(x + 0 > 86400) }'; then H=86400; HU="1 day"; else H=$SECS; HU="$UNTIL"; fi
    mkdir -p "$WORK/printed" && cp "$REC/profiles.glp" "$REC/population.glp" "$WORK/printed/" || exit 1
    for s in $(printf '%s' "$SEEDS" | tr ',' ' '); do
        if ! bash "$HERE/run.sh" "$WORK/platform.glp" "$WORK/printed" "$N" "$HU" "$s" "$WORK/$s.log" > "$WORK/$s.out" 2>&1; then
            echo "$s.log: the run of the first day failed"; sed 's/^/    /' "$WORK/$s.out" | tail -5; STATUS=1; continue
        fi
        first_day "$REC/$s.log" "$H" > "$WORK/$s.rec"
        first_day "$WORK/$s.log" "$H" > "$WORK/$s.now"
        if [ -f "$REC/$s.log" ] && cmp -s "$WORK/$s.rec" "$WORK/$s.now"; then
            echo "$s.log: first day as recorded, $(wc -l < "$WORK/$s.now" | tr -d ' ') entries"
        else
            echo "$s.log: first day NOT as recorded"
            diff "$WORK/$s.rec" "$WORK/$s.now" | head -4 | sed 's/^/    /'
            STATUS=1
        fi
    done
    exit $STATUS
fi

[ $# -ge 4 ] || usage
NAME=$1; SRCF=$2; shift 2
CAS=; N=; UNTIL=; SEEDS=; ROOT="$GLP_DIR/simulation"; RECORD=0
while [ $# -gt 0 ]; do
    case "$1" in
        --cascade) [ $# -ge 2 ] || usage; CAS=$2; shift 2 ;;
        --agents) [ $# -ge 2 ] || usage; N=$2; shift 2 ;;
        --until) [ $# -ge 2 ] || usage; UNTIL=$2; shift 2 ;;
        --seeds) [ $# -ge 2 ] || usage; SEEDS=$2; shift 2 ;;
        --root) [ $# -ge 2 ] || usage; ROOT=$2; shift 2 ;;
        --record) RECORD=1; shift ;;
        *) usage ;;
    esac
done
[ -n "$CAS" ] && [ -f "$SRCF" ] || usage
# The unit of --until: one of the paper's six, singular or plural (sGLP's
# paper, Appendix B); run.sh refuses any other as well.
if [ -n "$UNTIL" ]; then
    read -r UC UU UX <<< "$UNTIL"
    case "${UU:-}" in
        second|seconds|minute|minutes|hour|hours|day|days|week|weeks|year|years) ;;
        *) echo "record.sh: the unit of --until is second, minute, hour, day, week or year, singular or plural, not ${UU:-none}" >&2
           exit 2 ;;
    esac
    [[ "$UC" =~ ^[0-9]+(\.[0-9]+)?$ ]] && [ -z "${UX:-}" ] ||
        { echo "record.sh: --until is '<count> <unit>'" >&2; exit 2; }
fi
[[ "$NAME" =~ ^[A-Z][A-Z0-9]*$ ]] || { echo "record.sh: a contract's name is a letter acronym, as GSG" >&2; exit 2; }
if [ "$RECORD" = 1 ] && [ "$ROOT" != "$GLP_DIR/simulation" ]; then
    echo "record.sh: --record commits a record under simulation/ only" >&2; exit 2
fi
CASC=$(cascade_files "$CAS" "$NAME" "$WORK") ||
    { echo "record.sh: the cascade $CAS gives no platform.glp and interface.glp of $NAME"; exit 1; }

DATE=$(date -u '+%Y-%m-%d')
mkdir -p "$ROOT/$NAME" || exit 2
D="$DATE"; k=1
while [ -e "$ROOT/$NAME/$D" ] || git -C "$GLP_DIR" rev-parse -q --verify "refs/tags/simulation/$NAME/$D" > /dev/null; do
    k=$((k + 1)); D="$DATE-$k"
done
REC="$ROOT/$NAME/$D"
mkdir -p "$REC" || exit 2
SRCB=$(basename "$SRCF")
cp "$SRCF" "$REC/$SRCB" || exit 1

if ! bash "$SGLP/transform.sh" --interface "$WORK/interface.glp" --source "$REC/$SRCB" --out "$REC" > "$WORK/tx" 2>&1 ||
   grep -q '^%% not transformed' "$REC/population.glp" "$REC/profiles.glp" 2>/dev/null; then
    echo "record.sh: the transformation failed"; cat "$WORK/tx"; grep -h '^%% not transformed' "$REC"/*.glp 2>/dev/null
    rm -rf "$REC"; exit 1
fi
# The run declaration's persons, horizon and seed, run_declaration/4 of the
# printed population: a number of agents, or the name of a census, for which
# run.sh is given census.
RD=$(tr '\n' ' ' < "$REC/population.glp" | grep -oE 'run_declaration\(([0-9]+|[a-z][a-z0-9_]*), [0-9.]+, [a-z]+, [0-9]+\) \.' | head -1)
read -r DN DC DU DS <<< "$(printf '%s' "$RD" | sed -E 's/run_declaration\(([0-9]+|[a-z][a-z0-9_]*), ([0-9.]+), ([a-z]+), ([0-9]+)\) \./\1 \2 \3 \4/')"
[[ "$DN" =~ ^[0-9]+$ ]] || [ -z "$DN" ] || DN=census
[ -n "$N" ] || N=$DN
[ -n "$UNTIL" ] || UNTIL="$DC $DU"
[ -n "$SEEDS" ] || SEEDS=$DS
[ -n "$N" ] && [ -n "$SEEDS" ] && [ -n "${UNTIL// /}" ] || { echo "record.sh: no run declaration in the printed population"; rm -rf "$REC"; exit 1; }

STATUS=0
for s in $(printf '%s' "$SEEDS" | tr ',' ' '); do
    if bash "$HERE/run.sh" "$WORK/platform.glp" "$REC" "$N" "$UNTIL" "$s" "$REC/$s.log" > "$WORK/$s.out" 2>&1; then
        echo "$s.log: $(wc -l < "$REC/$s.log" | tr -d ' ') lines"
    else
        echo "record.sh: the run of seed $s failed"; sed 's/^/    /' "$WORK/$s.out" | tail -8; STATUS=1
    fi
done
[ "$STATUS" = 0 ] || { rm -rf "$REC"; exit 1; }

# The commits: of the cascade's record, of the owner's source, of programs/sglp.
src_commit() {
    local rel
    case "$1" in "$GLP_DIR"/*) rel=${1#$GLP_DIR/} ;; *) echo "outside the repository"; return ;; esac
    if [ -n "$(git -C "$GLP_DIR" status --porcelain -- "$rel" 2>/dev/null)" ]; then echo "uncommitted"; return; fi
    git -C "$GLP_DIR" log -1 --format=%h --abbrev=8 -- "$rel" 2>/dev/null || echo unknown
}
SRCA="$(cd "$(dirname "$SRCF")" && pwd)/$(basename "$SRCF")"
{
    echo "# A simulation of $NAME under sGLP"
    echo ""
    echo "The owner's sGLP source run on the canonical platform its cascade record compiled, by the harness of every canonical CVA platform (programs/sglp/cva/), as programs/sglp/cva/record.sh recorded it on $(date -u '+%Y-%m-%d %H:%M UTC').  The suite's Section SGLP re-runs the transformation and each run's first simulated day and holds them to this record (record.sh --check)."
    echo ""
    echo "- name: $NAME"
    echo "- cascade: $CAS"
    echo "- cascade commit: $CASC"
    echo "- source: $SRCB"
    echo "- source path: ${SRCA#$GLP_DIR/}"
    echo "- source commit: $(src_commit "$SRCA")"
    echo "- programs/sglp commit: $(src_commit "$SGLP")"
    echo "- repository commit: $(git -C "$GLP_DIR" rev-parse --short=8 HEAD)"
    echo "- agents: $N"
    echo "- until: $UNTIL"
    echo "- seeds: $SEEDS"
    echo ""
    echo "| File | What it holds |"
    echo "|---|---|"
    echo "| \`$SRCB\` | the owner's sGLP source, as it stood |"
    echo "| \`profiles.glp\`, \`population.glp\` | printed by programs/sglp/transform.sh --interface from it over the platform's interface |"
    for s in $(printf '%s' "$SEEDS" | tr ',' ' '); do
        echo "| \`$s.log\`, \`$s.draw\` | seed $s: $(awk -F'\t' 'NF == 4' "$REC/$s.log" | wc -l | tr -d ' ') answers, the clock $(tail -n 1 "$REC/$s.log" | cut -f1); the population's draw, $(head -n 1 "$REC/$s.draw" 2>/dev/null) |"
    done
} > "$REC/manifest.md"
echo "written to ${REC#$GLP_DIR/}"

if [ "$RECORD" = 1 ]; then
    TAG="simulation/$NAME/$D"
    git -C "$GLP_DIR" pack-refs --all
    git -C "$GLP_DIR" add "simulation/$NAME/$D" &&
        git -C "$GLP_DIR" commit -q -m "A simulation of $NAME under sGLP, recorded whole ($TAG): $SRCB on the platform of $CAS, $N agents until $UNTIL, seeds $SEEDS, by programs/sglp/cva/record.sh" -- "simulation/$NAME/$D" &&
        git -C "$GLP_DIR" tag "$TAG" &&
        echo "recorded as $(git -C "$GLP_DIR" rev-parse --short=8 HEAD), tagged $TAG" ||
        { echo "record.sh: the record was not committed"; exit 1; }
fi
exit 0
