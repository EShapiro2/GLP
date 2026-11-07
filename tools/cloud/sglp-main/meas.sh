#!/bin/bash
# Section 4's two measurements, as test_sglp.sh (i)-(ii) and sglp-meet-cost.sh made them, on sGLP-runs (f336d119).
set -u
WT=/Users/udi/Grassroots/GLP-worktrees/sGLP-runs
T=/Users/udi/Grassroots/tmp/sglp-splitmix
LOCK=/Users/udi/Grassroots/tmp/glp-run.sh
SG=$WT/programs/sglp/social_graph/run.sh
H=$WT/programs/sglp
cd "$WT" || exit 1
echo "$(date -u '+%F %T') UTC start claim" >> $T/progress.txt
bash "$LOCK" splitmix-claim bash -c "cd '$WT/glp_runtime' && { printf '%s\n' ':limit 1000000000000' '$H/tests/busy' 'busy(100000).' 'busy_late(100000).' ':quit'; } | bin/glpc > '$T/sglp-claim-busy.out' 2>&1; cd '$WT/glp_runtime' && { printf '%s\n' ':limit 1000000000000' '$H/tests/law' 'law(10000, S).' ':quit'; } | bin/glpc > '$T/sglp-claim-law.out' 2>&1"
for n in 100 200 500; do
  b=sglp-cost-$n-30d
  rm -f "$T/$b.rss"
  echo "$(date -u '+%F %T') UTC start $b" >> $T/progress.txt
  bash "$LOCK" splitmix-$b bash -c "
    { uptime; /usr/bin/time -l bash '$SG' $n '30 days' 20260927 '$T/$b.log' ''; echo \"exit \$?\"; uptime; } > '$T/$b.out' 2>&1"
  echo "$(date -u '+%F %T') UTC done $b: $(grep '^wall-clock' $T/$b.out)" >> $T/progress.txt
done
echo "MEAS-DONE" >> $T/progress.txt
