#!/bin/bash
# gate3.sh <tag> <before-tree> <after-tree>: the gate of a branch's tree against a base's tree, each side run in
# its own tree (no checkout): the fast tier, Section VG, every program directory loaded (loaddirs), every program
# and a sample of files loaded (loadall), every .vglp directory compiled and emitted (emit), and the small
# benchmark; then the two sides compared.  bench.py is bench/bench.py beside this file, copied into $S.
export PATH=/home/user/sdk/flutter/bin/cache/dart-sdk/bin:/home/user/sdk/flutter/bin:$PATH
S=${S:?set S to a scratch directory}
TAG=$1; BEFORE=$2; AFTER=$3
SMALL="startup fib_23 fib_linear_1e5 primes_5000 load_gsc load_graph load_sglp load_app_gsn check_gsn check_gsn_x10 fib_20_x10 boot_held_links"
norm() { sed -E -e 's/[0-9.]+ ?ms/Tms/g' -e 's/[0-9.]+ s\b/Ts/g' -e 's#'"$1"'#TREE#g' "$2"; }
loaddirs() { # loaddirs <tree> <out>
  local G=$1 out=$2; : > $out
  cd $G/glp_runtime
  for d in $(cd ../programs && find . -name self.glp -not -path '*/tests/*' | sed 's#/self.glp##; s#^\./##' | sort); do
    echo "##### DIR $d" >> $out
    printf "../programs/$d\n:quit\n" | timeout 120 bin/glpc 2>&1 >> $out
  done
}
loadall() { # loadall <tree> <out>
  local G=$1 out=$2; : > $out
  cd $G/glp_runtime
  for d in $(cd ../programs && find . -name self.glp -not -path '*/tests/*' | sed 's#/self.glp##; s#^\./##' | sort); do
    echo "##### DIR $d" >> $out
    printf "load ../programs/$d\n:quit\n" | timeout 60 bin/glpc 2>&1 >> $out
  done
  for f in $(cd ../programs && find book -name '*.glp' -not -name self.glp | sort | head -80); do
    echo "##### FILE $f" >> $out
    printf "load ../programs/$f\n:quit\n" | timeout 60 bin/glpc 2>&1 >> $out
  done
}
emit() { # emit <tree> <side>: a copy of the programs tree, every .vglp directory compiled and emitted into it
  local G=$1 X=$2 T=$S/tree_$X
  rm -rf $T $S/out_$X; mkdir -p $T $S/out_$X
  cp -r $G/programs $T/programs
  cd $G/glp_runtime
  for D in $(cd $T/programs && find . -name '*.vglp' | xargs -n1 dirname | sed 's|^\./||' | sort -u); do
    key=$(echo "$D" | tr '/' '_')
    printf "%s\n:emit %s\n:quit\n" "$T/programs/$D" "$T/programs/$D" | timeout 120 bin/glpc > $S/out_$X/$key.txt 2>&1
    echo "exit=$?" >> $S/out_$X/$key.txt
  done
}
side() { # side <before|after> <tree>
  local X=$1 G=$2
  cd $G/glp_runtime && dart pub get > /dev/null 2>&1; printf ':quit\n' | bin/glpc > /dev/null 2>&1
  cd $G && bash test/run_fast.sh > $S/gate_${TAG}_fast_$X.txt 2>&1
  cd $G && GLP_SECTIONS="VG" bash test/run_all_tests.sh > $S/gate_${TAG}_vg_$X.txt 2>&1
  loaddirs $G $S/gate_${TAG}_dirs_$X.txt
  loadall $G $S/gate_${TAG}_load_$X.txt
  emit $G $X > $S/gate_${TAG}_emit_$X.txt 2>&1
  cd $G/glp_runtime && GLP_RT=$G/glp_runtime python3 -I $S/bench.py 5 $SMALL > $S/gate_${TAG}_bench_small_$X.txt 2>&1
}
echo "== before: $BEFORE at $(git -C $BEFORE rev-parse --short HEAD)"; side before $BEFORE
echo "== after: $AFTER at $(git -C $AFTER rev-parse --short HEAD)"; side after $AFTER
echo "== fast: $(grep -c '^ *PASS' $S/gate_${TAG}_fast_before.txt)/$(grep -c '^ *FAIL' $S/gate_${TAG}_fast_before.txt) before, $(grep -c '^ *PASS' $S/gate_${TAG}_fast_after.txt)/$(grep -c '^ *FAIL' $S/gate_${TAG}_fast_after.txt) after"
diff <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_fast_before.txt) <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_fast_after.txt) | head -20
echo "== VG: $(grep -E 'Total:' $S/gate_${TAG}_vg_before.txt | tail -1) | $(grep -E 'Total:' $S/gate_${TAG}_vg_after.txt | tail -1)"
diff <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_vg_before.txt) <(grep -E '^ *(PASS|FAIL)' $S/gate_${TAG}_vg_after.txt) | head -20
echo "== dirs diff lines: $(diff <(norm $BEFORE $S/gate_${TAG}_dirs_before.txt) <(norm $AFTER $S/gate_${TAG}_dirs_after.txt) | wc -l)"
diff <(norm $BEFORE $S/gate_${TAG}_dirs_before.txt) <(norm $AFTER $S/gate_${TAG}_dirs_after.txt) | head -30
echo "== load diff lines: $(diff <(norm $BEFORE $S/gate_${TAG}_load_before.txt) <(norm $AFTER $S/gate_${TAG}_load_after.txt) | wc -l)"
diff <(norm $BEFORE $S/gate_${TAG}_load_before.txt) <(norm $AFTER $S/gate_${TAG}_load_after.txt) | head -30
echo "== emitted trees"; diff -r $S/tree_before/programs $S/tree_after/programs > $S/gate_${TAG}_treediff.txt && echo TREES IDENTICAL || echo "trees differ: $(wc -l < $S/gate_${TAG}_treediff.txt) lines"
n=0; bad=0; for f in $S/out_before/*.txt; do b=$(basename $f); n=$((n+1)); if ! diff <(norm tree_before $f | sed 's/tree_after/TREE/g') <(norm tree_after $S/out_after/$b | sed 's/tree_before/TREE/g') > /dev/null; then bad=$((bad+1)); echo "DIFFERS: $b"; fi; done; echo "emit outputs compared: $n, differing: $bad"
echo "== benchmark before"; cat $S/gate_${TAG}_bench_small_before.txt; echo "== benchmark after"; cat $S/gate_${TAG}_bench_small_after.txt
echo done
