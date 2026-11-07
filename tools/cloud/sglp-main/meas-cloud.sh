#!/bin/bash
# Section 4's two measurements, as test_sglp.sh (i)-(ii) and sglp-meet-cost.sh made them, on sGLP-runs (f336d119).
set -u
WT=/home/user/GLP-worktrees/sglp-e92
T=/home/user/sglp-main
LOCK=/home/user/GLP-worktrees/Cloud-sglp-main/tools/cloud/sglp-main/nolock.sh
SG=$WT/programs/sglp/social_graph/run.sh
H=$WT/programs/sglp
cd "$WT" || exit 1
echo "$(date -u '+%F %T') UTC start claim" >> $T/progress.txt
bash "$LOCK" splitmix-claim bash -c "cd '$WT/glp_runtime' && { printf '%s\n' ':limit 1000000000000' '$H/tests/busy' 'busy(100000).' 'busy_late(100000).' ':quit'; } | bin/glpc > '$T/sglp-claim-busy.out' 2>&1; cd '$WT/glp_runtime' && { printf '%s\n' ':limit 1000000000000' '$H/tests/law' 'law(10000, S).' ':quit'; } | bin/glpc > '$T/sglp-claim-law.out' 2>&1"
echo "MEAS-DONE" >> $T/progress.txt
