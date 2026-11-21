#!/bin/bash
# Test of Feeds_sglp.glp (programs/sglp/gff/README.md): Feeds on a ring of 8
# agents, each with the 2 nearest on each side, for four simulated days, seed 1:
#
# 1. the run ends with no error and the graph is built, each of its steps
#    proposed once (programs/sglp/graphs/run.sh exits 0);
# 2. every agent but the author follows the author's feed, and each of the
#    author's 3 items is held by all 8 (sim/spread.py);
# 3. no knows atom is held twice, and an agent at distance 1 from the author,
#    which follows by befriending it, proposes no labeled_point_friend.
#
# Prints one line per check and "=== P passed, F failed ==="; exits non-zero
# if any fails.  Takes about a minute and a half.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
PASS=0
FAIL=0
check() {
    if [ "$2" -eq 0 ]; then echo "  PASS  $1"; PASS=$((PASS + 1)); else echo "  FAIL  $1"; FAIL=$((FAIL + 1)); fi
}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

python3 -I "$GLP_DIR/programs/sglp/graphs/gen.py" ring --n 8 --k 2 --seed 1 > "$WORK/ring8.graph" 2> /dev/null
bash "$GLP_DIR/programs/sglp/graphs/run.sh" Feeds "$WORK/ring8.graph" '4 days' 1 "$WORK/ring8.log" "$HERE/Feeds_sglp.glp" > "$WORK/run.out" 2>&1; st=$?
[ "$st" = 0 ] && grep -q '^friendships: 16 of 16 at the end' "$WORK/run.out" && grep -q ', 0 of those repeated;' "$WORK/run.out"
check "Feeds on a ring of 8 for four days: the run ends with no error, its 16 friendships built, each step proposed once" $?
[ "$st" = 0 ] || sed 's/^/        /' "$WORK/run.out"
python3 -I "$HERE/sim/spread.py" "$WORK/ring8.graph" "$WORK/ring8.log" > "$WORK/spread.out" 2>&1
grep -q '^following the author.s feed at the end: 7 of 7$' "$WORK/spread.out" &&
    [ "$(grep -c '^item[123]: posted at hour [0-9.]*, held by 8 of 8$' "$WORK/spread.out")" = 3 ]
check "every agent but the author follows its feed, and each of its 3 items is held by all 8" $?
grep -q 'most copies of one knows atom 1$' "$WORK/spread.out" &&
    awk '$1 == 1 && $2 ~ /^[0-9]+$/ { exit $6 != 0 }' "$WORK/spread.out" && grep -q '^       1 ' "$WORK/spread.out"
check "no knows atom held twice, and the author's friends, who follow by befriending it, propose no labeled_point_friend" $?
sed 's/^/        /' "$WORK/spread.out"
echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
