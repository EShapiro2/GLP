#!/bin/bash
# gate.sh <tag>: the gate of an uncommitted change to glp_runtime.  Stashes the change, runs the
# before side on the committed tree, pops it, runs the after side, compares, then dart test and the
# benchmark on both sides.  Everything to $S/gate_<tag>_*.txt; the summary on stdout.
export PATH=/home/user/sdk/flutter/bin/cache/dart-sdk/bin:/home/user/sdk/flutter/bin:$PATH
S=/tmp/claude-0/-home-user/7fc92850-8d68-543b-ad64-b32a0ef20ff6/scratchpad
B=$S/bench
G=/home/user/glp
TAG=$1
SMALL="startup fib_23 fib_linear_1e5 primes_5000 load_gsc load_graph load_sglp load_app_gsn check_gsn check_gsn_x10 fib_20_x10 sglp_sg_100_1d sglp_sg_100_90d boot_held_links"
BIG="play_gkk play_sg play_sn play_gsn play_gai play_cssn cascade_gkk cascade_gsg cascade_gsn cascade_gai cascade_cssn"

side() {  # side <before|after>
  local X=$1
  cd $G/glp_runtime && printf ':quit\n' | bin/glpc > /dev/null 2>&1
  cd $G && bash test/run_fast.sh > $S/gate_${TAG}_fast_$X.txt 2>&1
  cd $G && GLP_SECTIONS="VG" bash test/run_all_tests.sh > $S/gate_${TAG}_vg_$X.txt 2>&1
  bash $S/loaddirs.sh $S/gate_${TAG}_dirs_$X.txt
  bash $S/loadall.sh $S/gate_${TAG}_load_$X.txt
  bash $S/emit_from.sh $G $X > $S/gate_${TAG}_emit_$X.txt 2>&1
  cd $G/glp_runtime && python3 -I $B/bench.py 5 $SMALL > $S/gate_${TAG}_bench_small_$X.txt 2>&1
  cd $G/glp_runtime && python3 -I $B/bench.py 3 $BIG > $S/gate_${TAG}_bench_big_$X.txt 2>&1
}

cd $G
n=$(git stash list | wc -l)
git stash -q
echo "== before at $(git rev-parse --short HEAD), clean: $(git status --short | wc -l)"
side before
git stash pop -q
echo "== after, edits back: $(git status --short | wc -l) files"
side after

cd $G/glp_runtime && dart test > $S/gate_${TAG}_darttest.txt 2>&1
echo "== fast: $(grep -c '^ *PASS' $S/gate_${TAG}_fast_before.txt)/$(grep -c '^ *FAIL' $S/gate_${TAG}_fast_before.txt) before, $(grep -c '^ *PASS' $S/gate_${TAG}_fast_after.txt)/$(grep -c '^ *FAIL' $S/gate_${TAG}_fast_after.txt) after; diff lines: $(diff <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_fast_before.txt) <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_fast_after.txt) | wc -l)"
echo "== VG: $(grep -E 'Total:' $S/gate_${TAG}_vg_before.txt | tail -1) | $(grep -E 'Total:' $S/gate_${TAG}_vg_after.txt | tail -1); diff lines: $(diff <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_vg_before.txt) <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_vg_after.txt) | wc -l)"
echo "== dirs diff: $(diff $S/gate_${TAG}_dirs_before.txt $S/gate_${TAG}_dirs_after.txt | wc -l); files diff: $(diff $S/gate_${TAG}_load_before.txt $S/gate_${TAG}_load_after.txt | wc -l)"
bash $S/compare.sh
echo "== dart test: $(tail -c 300 $S/gate_${TAG}_darttest.txt | tr '\r' '\n' | grep -E 'All tests passed|Some tests failed' | tail -1)"
echo "== benchmark before"; cat $S/gate_${TAG}_bench_small_before.txt $S/gate_${TAG}_bench_big_before.txt
echo "== benchmark after"; cat $S/gate_${TAG}_bench_small_after.txt $S/gate_${TAG}_bench_big_after.txt
echo done
