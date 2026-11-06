#!/bin/bash
# usage: emit_from.sh <source root with glp_runtime and programs> <tag>: build the REPL from that source,
# snapshot it with a copy of its programs tree, compile and emit every directory holding a .vglp.
export PATH=/home/user/sdk/flutter/bin:$PATH
S=/tmp/claude-0/-home-user/7fc92850-8d68-543b-ad64-b32a0ef20ff6/scratchpad
SRC=$1; TAG=$2; T=$S/tree_$TAG
rm -rf $T $S/out_$TAG; mkdir -p $T/glp_runtime/bin $S/out_$TAG
( cd $SRC/glp_runtime && dart pub get >/dev/null 2>&1; dart compile exe bin/glp_repl.dart -o $T/glp_runtime/bin/glp_repl_exe >/dev/null 2>&1 ) || { echo "BUILD FAILED"; exit 1; }
cp -r $SRC/programs $T/programs
cd $T/glp_runtime
n=0
for D in $(cd $T/programs && find . -name '*.vglp' | xargs -n1 dirname | sed 's|^\./||' | sort -u); do
  key=$(echo "$D" | tr '/' '_'); n=$((n+1))
  printf "%s\n:emit %s\n:quit\n" "$T/programs/$D" "$T/programs/$D" | timeout 120 bin/glp_repl_exe > $S/out_$TAG/$key.txt 2>&1
  echo "exit=$?" >> $S/out_$TAG/$key.txt
done
echo "compiled $n directories"
