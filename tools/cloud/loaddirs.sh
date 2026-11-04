#!/bin/bash
export PATH=/home/user/sdk/flutter/bin:$PATH
cd /home/user/glp/glp_runtime
out=$1; : > $out
dirs=$(cd ../programs && find . -name self.glp -not -path '*/tests/*' | sed 's#/self.glp##; s#^\./##' | sort)
for d in $dirs; do
  echo "##### DIR $d" >> $out
  printf "../programs/$d\n:quit\n" | timeout 120 bin/glpc 2>&1 | sed 's/[0-9.]*ms/Tms/g; s/[0-9.]* s\b/Ts/g' >> $out
done
