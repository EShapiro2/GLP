#!/bin/bash
# The full suite on main, once a night at 02:00 UTC (Udi, 2026-10-24, to Code #15),
# through the run slots, taking all four, its output in
# /Users/udi/Grassroots/tmp/nightly-<date>.txt.  Run by the user crontab entry
# "0 * * * *", hourly, the script running only in the hour 02 UTC, so it holds
# to 02:00 UTC whatever the Mac's local time.
[ "$(date -u +%H)" = 02 ] || exit 0
cd /Users/udi/Grassroots/GLP || exit 1
D=$(date -u +%Y-%m-%d)
bash /Users/udi/Grassroots/tmp/glp-run.sh nightly bash -c "cd /Users/udi/Grassroots/GLP && echo \"main \$(git log --format=%h -1)\" > /Users/udi/Grassroots/tmp/nightly-$D.txt && bash test/run_all_tests.sh >> /Users/udi/Grassroots/tmp/nightly-$D.txt 2>&1"
