#!/bin/bash
# The 1,000-agent year in the container (Cloud #3, Code #25's task 5): run.sh 1000 '1 year' 20260927, 60/70, on main e92fb71d, under October's name sglp-k5y-20260927-year1; a watcher every 10 s writes the UTC time, the seconds since the start, the log's last time and the REPL's resident set, current (VmRSS) and peak (VmHWM), from /proc/<pid>/status, since the container has no /usr/bin/time.
set -u
WT=/home/user/GLP-worktrees/sglp-e92
T=/home/user/sglp-main
b=sglp-k5y-20260927-year1
export PATH=/home/user/sdk/dart-sdk/bin:$PATH
cd "$WT" || exit 1
rm -f "$T/$b".{log,graph,draw,out,watch,txt}
S=$(date +%s)
( bash programs/sglp/social_graph/run.sh 1000 '1 year' 20260927 "$T/$b.log" "$T/$b.graph"; echo "exit $?" ) > "$T/$b.out" 2>&1 &
R=$!
while kill -0 $R 2>/dev/null; do
  for p in $(pgrep -f glp_repl_exe); do
    st=/proc/$p/status
    [ -r $st ] && echo "$(date -u '+%F %T') $(( $(date +%s) - S )) s pid $p last $(tail -n 1 "$T/$b.log" 2>/dev/null | cut -f1) $(grep -E '^Vm(RSS|HWM)' $st | tr -s ' \t' ' ' | tr '\n' ' ')" >> "$T/$b.watch"
  done
  sleep 10
done
{ grep '^draw homophile' "$T/$b.out"; awk -v n=1000 -v months=12 -f "$WT/programs/sglp/social_graph/friendship.awk" "$T/$b.log"; } > "$T/$b.txt"
echo "DONE $(date -u '+%F %T') UTC after $(( $(date +%s) - S )) s" >> "$T/$b.watch"
