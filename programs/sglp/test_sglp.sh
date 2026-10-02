#!/bin/bash
# Tests of sGLP in GLP (programs/sglp) against sGLP's paper (the repository
# svGLP-Stochastic-Volitional-GLP at d2f64b6) and its code task of 2026-10-02
# 00:06 UTC, item 6.
#
#   bash programs/sglp/test_sglp.sh
#
# (i)   tests/busy: a request's token is bound only after when_idle has
#       succeeded --- a rated goal and an unrated busy goal, the busy goal runs
#       to rest first, whichever is spawned first (Section 4, "The monitor").
# (ii)  tests/law: with four pending requests of rates 1/second, 2/second,
#       30/minute and 3600/hour and nothing else, the first 10000 times between
#       releases have the mean and the variance of an exponential with rate
#       4.5 per second, each within 10% (Section 2, Proposition "Time to the
#       Next Release").
# (iii) the social graph (social_graph/run.sh), 100 agents for 30 days: two runs
#       from seed 20260927 write byte-identical logs, and seeds 20260927 and 1
#       write different ones (Section 4, "The run": every draw is from the
#       run's seed).
# (iv)  the social graph, 100 agents for one year from seed 20260927: the run
#       ends with no error and a non-empty log of menus, cards and their
#       answers, and its monitor's last line.
#
# Prints one line per check and a summary line, "=== P passed, F failed ===";
# exits non-zero if any check fails.  The runs of (iii) and (iv) take some
# minutes.

set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
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

# repl <out> <line>...: the REPL on the lines given, its output in <out>.
repl() {
    local out=$1; shift
    (cd "$RT" && { printf '%s\n' "$@"; echo ':quit'; } | bin/glpc > "$out" 2>&1)
}

echo "--- (i) a token is bound only when the machine rests"
repl "$WORK/busy.out" ':limit 1000000000000' "$HERE/tests/busy" \
    'busy(100000, O).' 'busy_late(100000, O).'
grep -q '✓ Loaded program' "$WORK/busy.out"
check "tests/busy loads" $?
[ "$(grep -c '^\(GLP> \)*O = after$' "$WORK/busy.out")" -eq 2 ]
check "the rated goal, spawned before and after count(100000, Z), is released after it" $?

echo "--- (ii) the times between releases are exponential with the rates' sum"
repl "$WORK/law.out" ':limit 1000000000000' "$HERE/tests/law" 'law(10000, S).'
grep -q '✓ Loaded program' "$WORK/law.out"
check "tests/law loads" $?
STATS=$(grep -oE 'S = stats\([^)]*\)' "$WORK/law.out" | head -1)
echo "        $STATS; expected mean $(awk 'BEGIN { printf "%.6f", 1/4.5 }'), variance $(awk 'BEGIN { printf "%.6f", 1/4.5^2 }')"
echo "$STATS" | awk -F'[(,) ]+' '{ exit !($4 == 10000) }'
check "10000 times between releases" $?
echo "$STATS" | awk -F'[(,) ]+' '{ m = 1/4.5; d = ($5 - m) / m; if (d < 0) d = -d; exit !(d < 0.1) }'
check "their mean is within 10% of 1/4.5" $?
echo "$STATS" | awk -F'[(,) ]+' '{ v = 1/4.5^2; d = ($6 - v) / v; if (d < 0) d = -d; exit !(d < 0.1) }'
check "their variance is within 10% of 1/4.5^2" $?

echo "--- (iii) the social graph's runs are reproducible from the seed"
for r in a b s1; do
    seed=20260927; [ "$r" = s1 ] && seed=1
    bash "$RUN" 100 '30 days' "$seed" "$WORK/$r.log" "$WORK/$r.graph" > "$WORK/$r.out" 2>&1
    st=$?
    check "100 agents, 30 days, seed $seed ($r): the run ends with no error" $st
    [ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/$r.out"
    [ -s "$WORK/$r.log" ]; check "100 agents, 30 days, seed $seed ($r): the log is not empty" $?
done
cmp -s "$WORK/a.log" "$WORK/b.log"
check "two runs from seed 20260927 write byte-identical logs" $?
! cmp -s "$WORK/a.log" "$WORK/s1.log"
check "seeds 20260927 and 1 write different logs" $?

echo "--- (iv) the social graph, 100 agents for one year"
bash "$RUN" 100 '1 year' 20260927 "$WORK/y.log" "$WORK/y.graph" > "$WORK/y.out" 2>&1
st=$?
check "100 agents, 1 year, seed 20260927: the run ends with no error" $st
[ "$st" -eq 0 ] || sed 's/^/        /' "$WORK/y.out"
[ -s "$WORK/y.log" ]; check "the log is not empty" $?
grep -q '^unread 0$' "$WORK/y.out" && grep -q '^menu_answers [1-9]' "$WORK/y.out" &&
    grep -q '^card_answers [1-9]' "$WORK/y.out" && ! grep -q '^clock none$' "$WORK/y.out"
check "the log is menus, cards and their answers, and the monitor's last line" $?
sed -n '/^wall-clock/p;/^answers/,/^unread/p' "$WORK/y.out" | sed 's/^/        /'

echo ""
echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
