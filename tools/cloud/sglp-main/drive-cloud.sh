#!/bin/bash
# The container's driver of runs-cloud.sh and meas-cloud.sh (Cloud #3, 2026-11-07): phase 1 the social graph's years and the sweep, phase 2 the coins years and grid, which read phase 1's graphs, and the claim's two tests; three jobs at a time, each stopped at ten minutes.
set -u
D="$(cd "$(dirname "$0")" && pwd)"
T=/home/user/sglp-main
export PATH=/home/user/sdk/dart-sdk/bin:$PATH
job() { timeout 600 bash "$D/runs-cloud.sh" "$@" || echo "$(date -u '+%F %T') UTC JOB-FAILED $*" >> "$T/progress.txt"; }
export -f job; export D T
{ echo sg 1; echo sg 20260927; echo mix 20260927 60 70
  for s in $(seq 1 20); do for h in 0 60 100; do for w in 0 70 100; do echo mix $s $h $w; done; done; done; } | xargs -P 3 -L 1 bash -c 'job "$@"' _
echo "$(date -u '+%F %T') UTC PHASE1-DONE" >> "$T/progress.txt"
{ echo co 1; echo co 20260927
  for s in 0 50 100; do echo cg 20260927 $s 50; done
  for sd in $(seq 1 20); do for s in 0 50 100; do for c in 0 50 100; do echo cg $sd $s $c; done; done; done; } | xargs -P 3 -L 1 bash -c 'job "$@"' _
timeout 600 bash "$D/meas-cloud.sh"
echo "$(date -u '+%F %T') UTC ALL-DONE" >> "$T/progress.txt"
