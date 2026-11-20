#!/bin/bash
# Tests of sGLP runs on a social graph of one's choosing (programs/sglp/graphs/):
#
# (a) gen.py: each family's graph, its edges and degrees as the family has
#     them; a graph a function of its arguments and seed; the overlay along
#     friendships; a regular graph of odd n * d and a pointer off a
#     friendship refused.
# (b) compose.py: each agent's script, the lower of a pair acquainting and
#     befriending the higher; a graph of pointers on a platform with no
#     point_friend refused, naming what it lacks, and a source whose run
#     declaration's agents are not the graph's; the library and the example
#     loading as GLP.
# (c) runs on the canonical platforms of the cascade's records Friendship and
#     Overlay (graphs/run.sh): a ring of 4 on Friendship built by
#     graph_builder and graph_approver, each friendship there, no step
#     proposed twice, none refused, nothing beyond the graph, the log
#     reproducible from the seed; the complete graph of 6 on Friendship, no
#     step proposed while an attempt is in flight; the ring with a pointer each on Overlay,
#     each pointer there; and example_sglp.glp on a ring of 6, the graph
#     built, then the owner's introductions, each after its person's last
#     step of the graph.
#
# Prints one line per check and "=== P passed, F failed ==="; exits non-zero
# if any check fails.  Takes about a minute.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
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
gen() { python3 -I "$HERE/gen.py" "$@" 2> /dev/null; }
# edges <graph>: its number of friendships; degrees <graph>: its sorted degrees, one line.
edges() { grep -c '^friend ' "$1"; }
degrees() { awk '$1 == "n" { n = $2 } $1 == "friend" { d[$2]++; d[$3]++ } END { for (i = 1; i <= n; i++) printf "%d\n", d[i] + 0 }' "$1" | sort -n | uniq -c | awk '{ printf "%s:%s ", $2, $1 }'; }

echo "--- (a) gen.py"
gen complete --n 5 --seed 1 > "$WORK/k5"; gen ring --n 6 --seed 1 > "$WORK/c6"; gen ring --n 10 --k 2 --seed 1 > "$WORK/c10"
gen grid --rows 3 --cols 4 --seed 1 > "$WORK/g34"; gen grid --rows 3 --cols 3 --torus --seed 1 > "$WORK/t33"
[ "$(edges "$WORK/k5")" = 10 ] && [ "$(degrees "$WORK/k5")" = "4:5 " ] &&
    [ "$(edges "$WORK/c6")" = 6 ] && [ "$(degrees "$WORK/c6")" = "2:6 " ] &&
    [ "$(edges "$WORK/c10")" = 20 ] && [ "$(degrees "$WORK/c10")" = "4:10 " ] &&
    [ "$(edges "$WORK/g34")" = 17 ] && [ "$(edges "$WORK/t33")" = 18 ] && [ "$(degrees "$WORK/t33")" = "4:9 " ]
check "complete, ring and grid: the complete graph of 5, rings of 6 and of 10 with 2 each side, a 3 x 4 grid of 17 edges and a 3 x 3 torus 4-regular" $?
gen regular --n 20 --d 3 --seed 7 > "$WORK/r20"; gen ba --n 30 --m 2 --seed 7 > "$WORK/ba30"
gen ws --n 20 --k 2 --beta 0 --seed 7 > "$WORK/ws0"; gen ring --n 20 --k 2 --seed 9 > "$WORK/c20"
gen ws --n 20 --k 2 --beta 0.3 --seed 7 > "$WORK/ws3"
[ "$(edges "$WORK/r20")" = 30 ] && [ "$(degrees "$WORK/r20")" = "3:20 " ] &&
    [ "$(edges "$WORK/ba30")" = 57 ] && [ "$(degrees "$WORK/ba30" | cut -d' ' -f1 | cut -d: -f1)" = 2 ] &&
    diff <(grep -v '^#' "$WORK/ws0") <(grep -v '^#' "$WORK/c20") > /dev/null &&
    [ "$(edges "$WORK/ws3")" = 40 ] && ! diff <(grep -v '^#' "$WORK/ws3") <(grep -v '^#' "$WORK/c20") > /dev/null
check "regular, ba and ws: a 3-regular graph of 20, Barabasi-Albert of 30 with m 2 (57 edges, degree at least 2), Watts-Strogatz at beta 0 the ring and at 0.3 rewired with its 40 edges" $?
gen er --n 12 --p 0 --seed 1 > "$WORK/er0"; gen er --n 12 --p 1 --seed 1 > "$WORK/er1"
gen sbm --sizes 3,4 --p-in 1 --p-out 0 --seed 1 > "$WORK/sbm"
[ "$(edges "$WORK/er0")" = 0 ] && [ "$(edges "$WORK/er1")" = 66 ] && [ "$(edges "$WORK/sbm")" = 9 ] &&
    ! grep -qE '^friend (1|2|3) (4|5|6|7)$' "$WORK/sbm"
check "er at p 0 and 1 and sbm of blocks 3 and 4 at 1 and 0: no edge, all 66, and the two cliques alone" $?
gen er --n 30 --p 0.2 --seed 5 > "$WORK/e1"; gen er --n 30 --p 0.2 --seed 5 > "$WORK/e2"; gen er --n 30 --p 0.2 --seed 6 > "$WORK/e3"
cmp -s "$WORK/e1" "$WORK/e2" && ! cmp -s "$WORK/e1" "$WORK/e3"
check "a graph is a function of its arguments and seed: seed 5 twice one graph, seed 6 another" $?
gen ring --n 6 --seed 1 --points all > "$WORK/pa"; gen ws --n 20 --k 2 --beta 0.3 --seed 7 --points out:1 > "$WORK/p1"
printf '1 2\n2 1\n' > "$WORK/ok.ptr"; gen ring --n 6 --seed 1 --points "file:$WORK/ok.ptr" > "$WORK/pf"
[ "$(grep -c '^points ' "$WORK/pa")" = 12 ] && [ "$(grep -c '^points ' "$WORK/p1")" = 20 ] &&
    [ "$(awk '$1 == "points" { print $2 }' "$WORK/p1" | sort -u | wc -l | tr -d ' ')" = 20 ] &&
    awk '$1 == "friend" { f[$2 " " $3] = f[$3 " " $2] = 1 } $1 == "points" && !f[$2 " " $3] { bad = 1 } END { exit bad }' "$WORK/p1" &&
    [ "$(grep '^points ' "$WORK/pf" | tr '\n' ' ')" = "points 1 2 points 2 1 " ]
check "the overlay: all, both ways along each of a ring's 6 friendships; out:1, one pointer of each agent, each along a friendship; file:, the pointers given" $?
python3 -I "$HERE/gen.py" regular --n 5 --d 3 --seed 1 > /dev/null 2> "$WORK/odd.err"; s1=$?
printf '1 3\n' > "$WORK/bad.ptr"; python3 -I "$HERE/gen.py" ring --n 6 --seed 1 --points "file:$WORK/bad.ptr" > /dev/null 2> "$WORK/off.err"; s2=$?
[ "$s1" = 2 ] && grep -q 'n \* d even' "$WORK/odd.err" && [ "$s2" = 2 ] && grep -q 'not along a friendship' "$WORK/off.err"
check "refused with exit 2: a 3-regular graph of 5 agents, and a pointer from 1 to 3 on a ring of 6" $?

echo "--- (b) compose.py"
gen ring --n 4 --seed 1 > "$WORK/ring4"; gen ring --n 4 --seed 1 --points out:1 > "$WORK/ring4p"
python3 -I "$HERE/compose.py" --graph "$WORK/ring4" --interface "$GLP_DIR/cascade/Friendship/interface.glp" --until '1 day' --seed 1 > "$WORK/fr_sglp.glp"; st=$?
[ "$st" = 0 ] &&
    grep -qF "clause(graph_script(p1, [step(establish(p1), identity, []), step(acquaint(p1, p2), knows(p2), [identity]), step(acquaint(p1, p4), knows(p4), [identity]), step(befriend(p1, p2), friend(p2), [knows(p2)]), step(befriend(p1, p4), friend(p4), [knows(p4)])]), [], [])," "$WORK/fr_sglp.glp" &&
    grep -qF "clause(graph_script(p4, [step(establish(p4), identity, []), await(friend(p1)), await(friend(p3))]), [], [])," "$WORK/fr_sglp.glp" &&
    grep -qF "clause(graph_agents([p1, p2, p3, p4]), [], [])" "$WORK/fr_sglp.glp" &&
    grep -qF "run(4, [mix(requests, [share(graph_builder, 1.0)]), mix(approvals, [share(graph_approver, 1.0)])], 1, day, 1)" "$WORK/fr_sglp.glp"
check "the scripts of a ring of 4: p1 establishes, acquaints p2 and p4 and befriends them; p4, the highest, only establishes and awaits its friendships with p1 and p3; the agents' keys in order; the run declaration of graph_builder and graph_approver" $?
python3 -I "$HERE/compose.py" --graph "$WORK/ring4p" --interface "$GLP_DIR/cascade/Friendship/interface.glp" --until '1 day' --seed 1 > "$WORK/no.glp" 2> "$WORK/no.err"; s1=$?
python3 -I "$HERE/compose.py" --graph "$WORK/ring4" --interface "$GLP_DIR/cascade/Friendship/interface.glp" --source "$HERE/example_sglp.glp" > "$WORK/no2.glp" 2> "$WORK/no2.err"; s2=$?
[ "$s1" = 2 ] && [ ! -s "$WORK/no.glp" ] && grep -q 'Proposal has no point_friend/2; Atom has no points_to/1' "$WORK/no.err" &&
    [ "$s2" = 2 ] && [ ! -s "$WORK/no2.glp" ] && grep -q 'run declaration is of 6 agents, the graph of 4' "$WORK/no2.err"
check "refused with exit 2 and nothing printed: pointers on Friendship, naming point_friend and points_to, and the example's 6 agents on a graph of 4" $?
sed 's/run(6, /run(graph, /' "$HERE/example_sglp.glp" > "$WORK/any_sglp.glp"
python3 -I "$HERE/compose.py" --graph "$WORK/ring4" --interface "$GLP_DIR/cascade/Friendship/interface.glp" --source "$WORK/any_sglp.glp" > "$WORK/any.glp"; st=$?
[ "$st" = 0 ] && grep -qF "run(4, [mix(behaviour, [share(introducer, 1.0)])], 3, day, 1)" "$WORK/any.glp" && ! grep -q 'run(graph' "$WORK/any.glp"
check "a run declaration of graph agents, run(graph, ...), printed with the graph's number, 4" $?
(cd "$RT" && printf 'load %s\nload %s\n:quit\n' "$HERE/library.glp" "$HERE/example_sglp.glp" | bin/glpc > "$WORK/load.out" 2>&1)
[ "$(grep -c '✓ Loaded: ' "$WORK/load.out")" = 2 ] && ! grep -q 'Error' "$WORK/load.out"
check "library.glp and example_sglp.glp load as GLP, their terms of type SglpTerms" $?

echo "--- (c) runs on the cascade's platforms"
bash "$HERE/run.sh" Friendship "$WORK/ring4" '1 day' 1 "$WORK/fr.log" > "$WORK/fr.out" 2>&1; st=$?
check "a ring of 4 on Friendship, graph_builder and graph_approver, one day: the run ends with no error and the graph is there (run.sh exits 0)" $st
[ "$st" = 0 ] || sed 's/^/        /' "$WORK/fr.out"
grep -q '^friendships: 4 of 4 at the end, first all at ' "$WORK/fr.out" &&
    grep -q '^pointers: none in the graph$' "$WORK/fr.out" &&
    grep -q '^beyond the graph: 0 friendships, 0 pointers$' "$WORK/fr.out" &&
    grep -q '^proposals: 12, 12 of them the graph.s steps, 0 of those repeated; 0 refused$' "$WORK/fr.out" &&
    [ -s "$WORK/fr.graph" ] && [ -s "$WORK/fr_sglp.glp" ] && [ -s "$WORK/fr.draw" ]
check "each of its 4 friendships at both ends, 12 steps proposed once each (4 establish, 4 acquaint, 4 befriend), none refused, nothing beyond the graph; the graph, the source and the draw beside the log" $?
awk -F'\t' '$3 == "Approval" && $4 !~ /, yes\)$/ { bad = 1 } $3 == "Approval" { n++ } END { exit bad || n != 8 }' "$WORK/fr.log"
check "graph_approver approves the 8 questions of the graph, a hello for each acquaintance and an attempt for each friendship, at the higher of each pair" $?
bash "$HERE/run.sh" Friendship "$WORK/ring4" '1 day' 1 "$WORK/fr2.log" > /dev/null 2>&1
bash "$HERE/run.sh" Friendship "$WORK/ring4" '1 day' 2 "$WORK/fr3.log" > /dev/null 2>&1
cmp -s "$WORK/fr.log" "$WORK/fr2.log" && ! cmp -s "$WORK/fr.log" "$WORK/fr3.log"
check "the run is reproducible: seed 1 twice one log, seed 2 another" $?
gen complete --n 6 --seed 1 > "$WORK/k6"
bash "$HERE/run.sh" Friendship "$WORK/k6" '2 days' 1 "$WORK/k6.log" > "$WORK/k6.out" 2>&1; st=$?
[ "$st" = 0 ] && grep -q '^friendships: 15 of 15 at the end, first all at ' "$WORK/k6.out" &&
    grep -q '^proposals: 36, 36 of them the graph.s steps, 0 of those repeated; 0 refused$' "$WORK/k6.out"
check "the complete graph of 6 on Friendship, two days: its 15 friendships, 36 steps proposed once each, an agent proposing none while one of its attempts is in flight (which the platform queues unseen; proposing then, seed 1 proposed 17 again)" $?
[ "$st" = 0 ] || sed 's/^/        /' "$WORK/k6.out"
bash "$HERE/run.sh" Overlay "$WORK/ring4p" '1 day' 1 "$WORK/ov.log" > "$WORK/ov.out" 2>&1; st=$?
[ "$st" = 0 ] && grep -q '^pointers: 4 of 4 at the end, first all at ' "$WORK/ov.out" &&
    grep -q '^proposals: 16, 16 of them the graph.s steps, 0 of those repeated; 0 refused$' "$WORK/ov.out"
check "the ring of 4 with a pointer each on Overlay: its 4 friendships and 4 pointers there, 16 steps proposed once each, none refused" $?
[ "$st" = 0 ] || sed 's/^/        /' "$WORK/ov.out"
gen ring --n 6 --seed 1 > "$WORK/ring6"
bash "$HERE/run.sh" Friendship "$WORK/ring6" '3 days' 1 "$WORK/ex.log" "$HERE/example_sglp.glp" > "$WORK/ex.out" 2>&1; st=$?
[ "$st" = 0 ] && grep -q '^friendships: 6 of 6 at the end, first all at ' "$WORK/ex.out" &&
    grep -q '^beyond the graph: 0 friendships, 0 pointers$' "$WORK/ex.out" &&
    grep -q ' of those repeated; 0 refused$' "$WORK/ex.out" && grep -q ', 0 of those repeated' "$WORK/ex.out"
check "example_sglp.glp on a ring of 6, three days: the graph built, its steps proposed once each, nothing refused and no friendship beyond the graph" $?
[ "$st" = 0 ] || sed 's/^/        /' "$WORK/ex.out"
awk -F'\t' '
    $3 == "Request?" && $4 ~ /^propose\((establish|acquaint|befriend)\(/ { last[$2] = $1 + 0; if (intro[$2]) bad = 1 }
    $3 == "Request?" && $4 ~ /^propose\(introduce\(/ { n++; intro[$2] = 1; if (!($2 in last)) bad = 1 }
    END { exit bad || n < 6 }' "$WORK/ex.log"
check "then the owner's behaviour: at least 6 introductions, each person's after its own last step of the graph" $?

echo "=== $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
