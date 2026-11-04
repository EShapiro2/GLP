#!/bin/bash
S=/tmp/claude-0/-home-user/7fc92850-8d68-543b-ad64-b32a0ef20ff6/scratchpad
echo "== emitted trees (diff -r of the two programs copies, emitted .glp included)"
diff -r $S/tree_before/programs $S/tree_after/programs && echo "TREES IDENTICAL"
echo "== captured outputs, normalised"
norm() { sed -E -e '/^Build: /d' -e '/^\[glpc\] rebuilding/d' -e '/Woah!|superuser|📎|^[╔║╚]/d' -e 's/[0-9.]*ms//g' -e "s#tree_(before|after)#tree_X#g" "$1"; }
n=0; bad=0
for f in $S/out_before/*.txt; do
  b=$(basename $f); n=$((n+1))
  if ! diff <(norm $f) <(norm $S/out_after/$b) > /dev/null; then bad=$((bad+1)); echo "DIFFERS: $b"; diff <(norm $f) <(norm $S/out_after/$b) | head -20; fi
done
echo "outputs compared: $n, differing: $bad"
