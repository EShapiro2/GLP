#!/bin/bash
# Tests of sGLP's programs (programs/sglp), against sGLP's paper (the
# repository svGLP-Stochastic-Volitional-GLP at 8ddff2e).
#
#   bash programs/sglp/test_sglp.sh
#
# (iii) Reproducibility of the social graph (social_graph/graph.vglp): two
# runs of a hundred agents for a simulated year from one seed write
# byte-identical logs, and seeds 20260927 and 1 write different ones (Section
# 3: the draws of a run are reproducible from its seed).  Each run reaches its
# horizon with no error and logs, and its log reads as the program's traffic:
# menus, cards and their answers (social_graph/friendship.awk).
#
# Prints one line per check and a summary line, "=== P passed, F failed ===";
# exits non-zero if any check fails.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
RUN="$HERE/social_graph/run.sh"
PASS=0
FAIL=0

check() {   # check <name> <condition result: 0 pass>
    if [ "$2" -eq 0 ]; then
        echo "  PASS  $1"
        PASS=$((PASS + 1))
    else
        echo "  FAIL  $1"
        FAIL=$((FAIL + 1))
    fi
}

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

echo "--- (iii) the social graph's runs are reproducible from the seed"
for r in a b s1; do
    seed=20260927; [ "$r" = s1 ] && seed=1
    bash "$RUN" 100 '1 year' "$seed" "$WORK/$r.log" "$WORK/$r.graph" > "$WORK/$r.out" 2>&1
    st=$?
    check "100 agents, 1 year, seed $seed ($r): the run reaches its horizon with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/$r.out"
    [ -s "$WORK/$r.log" ]; check "100 agents, 1 year, seed $seed ($r): the log is not empty" $?
    grep -q '^unread 0$' "$WORK/$r.out" && grep -q '^menu_answers [1-9]' "$WORK/$r.out" &&
        grep -q '^card_answers [1-9]' "$WORK/$r.out"
    check "100 agents, 1 year, seed $seed ($r): the log is menus, cards and their answers" $?
done
cmp -s "$WORK/a.log" "$WORK/b.log"
check "two runs from seed 20260927 write byte-identical logs" $?
! cmp -s "$WORK/a.log" "$WORK/s1.log"
check "seeds 20260927 and 1 write different logs" $?

echo ""
echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
