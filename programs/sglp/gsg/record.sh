#!/bin/bash
# A record of GSG's simulation, made as GDO's are (programs/sglp/lbm/sim/record.sh):
# one of the sources beside this script, recorded whole by programs/sglp/cva/record.sh
# over a cascade record of GSG.
#
#   bash programs/sglp/gsg/record.sh <dest> <source> [<cascade tag|dir>] [-- <record.sh options>]
#
#   <dest>     the record's directory, simulation/GSG/<date>/<name>, which must not exist;
#   <source>   GSG_sglp.glp, GSG12_sglp.glp, GSGV1_sglp.glp or GSGV3_sglp.glp, beside this
#              script; it is recorded as GSG_sglp.glp, the name cva/record.sh gives the
#              owner's source;
#   <cascade>  a tag cascade/GSG/<date>, or a directory holding platform.glp and
#              interface.glp; the newest tag cascade/GSG/* where none is given;
#   after --, options passed to cva/record.sh, such as --seeds 1,7.
#
# The record holds what cva/record.sh writes: GSG_sglp.glp as run, the printed
# profiles.glp and population.glp, each seed's log and draw, and manifest.md, on which
# cva/record.sh --check passes.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
[ $# -ge 2 ] || { echo "usage: record.sh <dest> <source> [<cascade tag|dir>] [-- <record.sh options>]" >&2; exit 2; }
DEST=$1; SRC=$2; shift 2
CAS=""
if [ $# -gt 0 ] && [ "$1" != "--" ]; then CAS=$1; shift; fi
[ $# -gt 0 ] && [ "$1" = "--" ] && shift
CAS=${CAS:-$(git -C "$GLP_DIR" tag -l 'cascade/GSG/*' --sort=-creatordate | head -1)}
[ -n "$CAS" ] || { echo "record.sh: no cascade record of GSG" >&2; exit 2; }
[ -f "$HERE/$SRC" ] || { echo "record.sh: no source $HERE/$SRC" >&2; exit 2; }
[ ! -e "$DEST" ] || { echo "record.sh: $DEST exists" >&2; exit 2; }
WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
cp "$HERE/$SRC" "$WORK/GSG_sglp.glp" || exit 2
cd "$GLP_DIR" || exit 2
bash programs/sglp/cva/record.sh GSG "$WORK/GSG_sglp.glp" --cascade "$CAS" --root "$WORK/root" "$@" > "$WORK/rec.out" 2>&1 ||
    { cat "$WORK/rec.out" >&2; exit 1; }
REC=$(ls -d "$WORK"/root/GSG/*/ | head -1)
[ -f "$REC/manifest.md" ] || { cat "$WORK/rec.out" >&2; exit 1; }
mkdir -p "$(dirname "$DEST")" && mv "$REC" "$DEST" || exit 1
echo "recorded $SRC in $DEST over $CAS: $(cat "$DEST"/*.log | wc -l | tr -d ' ') log lines"
