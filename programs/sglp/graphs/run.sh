#!/bin/bash
# A run of a canonical CVA platform under sGLP on a social graph of one's
# choosing (programs/sglp/graphs/README.md): the source composed with the
# setup library and the graph's scripts (compose.py), transformed against the
# platform's interface (transform.sh --interface), run (cva/run.sh) and its
# log checked against the graph (check.py).
#
#   bash programs/sglp/graphs/run.sh <platform> <graph> <until> <seed> <log> [<source>]
#
#   <platform>  a record under cascade/ (Friendship, Overlay, Feeds, ...) or a
#               directory holding platform.glp and interface.glp;
#   <graph>     a graph gen.py prints;
#   <until>     "<count> <unit>", the horizon, as cva/run.sh takes it;
#   <seed>      the run's seed;
#   <log>       the log, as cva/run.sh writes it;
#   <source>    an owner's sGLP source whose profiles call graph_next/3 and
#               graph_asked/3 (example_sglp.glp); with none, the persons only
#               build the graph, graph_builder and graph_approver.
#
# Beside the log it keeps what makes the run again: the graph, <log> with its
# .log replaced by .graph, and the composed source, by _sglp.glp; cva/run.sh
# writes the population's draw by .draw.  Prints cva/run.sh's lines and
# check.py's.  Exits with cva/run.sh's status where it is not 0, else with
# check.py's: 0 if the graph is there at the end, 1 if not; 2 on a bad
# argument.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
if [ $# -ne 5 ] && [ $# -ne 6 ]; then
    sed -n 8,19p "$0" >&2
    exit 2
fi
P=$1; GRAPH=$2; UNTIL=$3; SEED=$4; LOG=$5; SRC=${6:-}
if [ -d "$P" ]; then PD="$(cd "$P" && pwd)"; else PD="$GLP_DIR/cascade/$P"; fi
for f in "$PD/platform.glp" "$PD/interface.glp" "$GRAPH" ${SRC:+"$SRC"}; do
    [ -f "$f" ] || { echo "run.sh: no file $f" >&2; exit 2; }
done
N=$(awk '$1 == "n" { print $2; exit }' "$GRAPH")
[[ "${N:-}" =~ ^[0-9]+$ ]] || { echo "run.sh: $GRAPH declares no number of agents, \"n N\"" >&2; exit 2; }
case "$LOG" in *.log) BASE="${LOG%.log}" ;; *) BASE="$LOG" ;; esac

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
if [ -n "$SRC" ]; then
    python3 -I "$HERE/compose.py" --graph "$GRAPH" --interface "$PD/interface.glp" --source "$SRC" > "$WORK/run_sglp.glp"
else
    python3 -I "$HERE/compose.py" --graph "$GRAPH" --interface "$PD/interface.glp" --until "$UNTIL" --seed "$SEED" > "$WORK/run_sglp.glp"
fi || exit 2
[ "$GRAPH" -ef "$BASE.graph" ] || cp "$GRAPH" "$BASE.graph"
cp "$WORK/run_sglp.glp" "${BASE}_sglp.glp"
mkdir -p "$WORK/printed"
bash "$GLP_DIR/programs/sglp/transform.sh" --interface "$PD/interface.glp" --source "$WORK/run_sglp.glp" \
    --out "$WORK/printed" > "$WORK/transform.out" 2>&1 || { cat "$WORK/transform.out"; exit 1; }
bash "$GLP_DIR/programs/sglp/cva/run.sh" "$PD/platform.glp" "$WORK/printed" "$N" "$UNTIL" "$SEED" "$LOG"
st=$?
[ "$st" -eq 0 ] || exit "$st"
python3 -I "$HERE/check.py" "$GRAPH" "$LOG"
