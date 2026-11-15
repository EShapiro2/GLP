#!/bin/bash
# A record of GDO's simulation with a configuration of its own, made as CSSN's
# mixes are (simulation/CSSN/<date>/<mix>/): the source with the configuration
# and the run line in place, recorded whole by programs/sglp/cva/record.sh over
# a cascade record of GDO, and beside it the map and the measures.
#
#   bash programs/sglp/lbm/sim/record.sh <dest> '<cfg>' <census> '<count> <unit>' [<cascade tag|dir>]
#
#   <dest>     the record's directory, simulation/<series>/<date>/<name>, which
#              must not exist;
#   <cfg>      cfg(Geometry, Metric, Nearest, Random, D, E, MapSeed, Step,
#              Movers, Meet, Part), put in place of the source's gd_config line
#              (GDO_sglp.glp, "Configuration");
#   <census>   gd_town40, gd_town100 or gd_town200, the run declaration's census;
#   <until>    the horizon, the run declaration's, "6 days";
#   <cascade>  the cascade record of GDO whose platform is run: a tag
#              cascade/GDO/<date>, or a directory holding platform.glp and
#              interface.glp; the newest tag cascade/GDO/* where none is given.
#
# The record holds what cva/record.sh writes --- GDO_sglp.glp as run, the
# printed profiles.glp and population.glp, 1.log, 1.draw and manifest.md, on
# which cva/record.sh --check passes --- and 1.census (census.sh, the map) and
# measures.txt (overlay.awk, days the horizon's where its unit is the day).
# summary.md, what we see in the run, is the owner's to write.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../../.." && pwd)"
[ $# -ge 4 ] && [ $# -le 5 ] || { echo "usage: record.sh <dest> '<cfg>' <census> '<count> <unit>' [<cascade tag|dir>]" >&2; exit 2; }
DEST=$1; CFG=$2; CENSUS=$3; UNTIL=$4
CAS=${5:-$(git -C "$GLP_DIR" tag -l 'cascade/GDO/*' --sort=-creatordate | head -1)}
[ -n "$CAS" ] || { echo "record.sh: no cascade record of GDO" >&2; exit 2; }
[ ! -e "$DEST" ] || { echo "record.sh: $DEST exists" >&2; exit 2; }
N=${UNTIL%% *}; U=${UNTIL##* }; U1=${U%s}
WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
sed -e "s/clause(gd_config(cfg([^)]*)), \[\], \[\])/clause(gd_config($CFG), [], [])/" \
    -e "s/^   run(gd_town[0-9]*, \(.*\), [0-9]*, [a-z]*, 1)/   run($CENSUS, \1, $N, $U1, 1)/" "$HERE/../GDO_sglp.glp" > "$WORK/GDO_sglp.glp"
grep -q "gd_config($CFG)" "$WORK/GDO_sglp.glp" && grep -q "run($CENSUS, .*, $N, $U1, 1)" "$WORK/GDO_sglp.glp" ||
    { echo "record.sh: the configuration or the run line was not put in place" >&2; exit 2; }
cd "$GLP_DIR" || exit 2
bash programs/sglp/cva/record.sh GDO "$WORK/GDO_sglp.glp" --cascade "$CAS" --root "$WORK/root" > "$WORK/rec.out" 2>&1 ||
    { cat "$WORK/rec.out" >&2; exit 1; }
REC=$(ls -d "$WORK"/root/GDO/*/ | head -1)
[ -f "$REC/manifest.md" ] || { cat "$WORK/rec.out" >&2; exit 1; }
mkdir -p "$(dirname "$DEST")" && mv "$REC" "$DEST" || exit 1
# the map, over the platform the record ran
mkdir -p "$WORK/cas"
case "$CAS" in
    cascade/GDO/*) git show "$CAS:cascade/GDO/platform.glp" > "$WORK/cas/platform.glp" || exit 1 ;;
    *) cp "$CAS/platform.glp" "$WORK/cas/platform.glp" || exit 1 ;;
esac
bash "$HERE/census.sh" "$WORK/cas/platform.glp" "$DEST" "$CENSUS" "$DEST/1.census" || exit 1
case "$U1" in day) DAYS=$N ;; week) DAYS=$((N * 7)) ;; year) DAYS=$((N * 365)) ;; *) DAYS=1 ;; esac
awk -v census="$DEST/1.census" -v days="$DAYS" -f "$HERE/overlay.awk" "$DEST/1.log" > "$DEST/measures.txt" || exit 1
echo "recorded in $DEST over $CAS: $(wc -l < "$DEST/1.log" | tr -d ' ') log lines"
