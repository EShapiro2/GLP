#!/bin/bash
# loadall.sh <outfile>: load every program directory and single .glp of a sample through glpc, output to <outfile>
export PATH=/home/user/sdk/flutter/bin:$PATH
cd /home/user/glp/glp_runtime
out=$1; : > $out
dirs=$(cd ../programs && find . -name self.glp -not -path '*/tests/*' | sed 's#/self.glp##; s#^\./##' | sort)
for d in $dirs; do
  echo "##### DIR $d" >> $out
  printf "load ../programs/$d\n:quit\n" | timeout 60 bin/glpc 2>&1 | sed 's/[0-9.]*ms/Tms/g; s/[0-9.]* s\b/Ts/g' >> $out
done
for f in $(cd ../programs && find book -name '*.glp' -not -name self.glp | sort | head -80); do
  echo "##### FILE $f" >> $out
  printf "load ../programs/$f\n:quit\n" | timeout 60 bin/glpc 2>&1 | sed 's/[0-9.]*ms/Tms/g' >> $out
done
echo "DIRS $(echo "$dirs" | wc -l)" >> $out
