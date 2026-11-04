#!/bin/bash
# bench_pair.sh <before-tree> <after-tree> <out>: every benchmark case on both trees, alternating case by case,
# so that drift in the machine's load falls on both columns alike.
export PATH=/home/user/sdk/flutter/bin/cache/dart-sdk/bin:/home/user/sdk/flutter/bin:$PATH
S=/tmp/claude-0/-home-user/7fc92850-8d68-543b-ad64-b32a0ef20ff6/scratchpad; B=$S/bench
BT=$1; AT=$2; OUT=$3; : > $OUT.before; : > $OUT.after
for T in $BT $AT; do (cd $T/glp_runtime && dart pub get >/dev/null 2>&1; printf ':quit\n' | bin/glpc >/dev/null 2>&1); done
SMALL="startup fib_23 fib_linear_1e5 primes_5000 load_gsc load_graph load_sglp load_app_gsn check_gsn check_gsn_x10 fib_20_x10 sglp_sg_100_1d sglp_sg_100_90d boot_held_links"
BIG="play_gkk play_sg play_sn play_gsn play_gai play_cssn cascade_gkk cascade_gsg cascade_gsn cascade_gai cascade_cssn"
run() { (cd $1/glp_runtime && GLP_RT=$1/glp_runtime python3 -I $B/bench.py $2 $3 2>&1 | tail -1); }
for c in $SMALL; do run $BT 5 $c >> $OUT.before; run $AT 5 $c >> $OUT.after; done
for c in $BIG; do run $BT 3 $c >> $OUT.before; run $AT 3 $c >> $OUT.after; done
paste -d'|' <(cut -c1-26 $OUT.before) <(cut -c19-26 $OUT.after)
echo done
