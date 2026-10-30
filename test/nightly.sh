#!/bin/bash
# The full suite on main, once a night at 02:00 UTC (Udi, 2026-10-24, to Code #15),
# through the run slots, taking all four, its output in
# /Users/udi/Grassroots/tmp/nightly-<date>.txt.  Run by the user crontab entry
# "0 * * * *", hourly, the script running only in the hour 02 UTC, so it holds
# to 02:00 UTC whatever the Mac's local time.
[ "$(date -u +%H)" = 02 ] || exit 0
export PATH=/opt/homebrew/bin:/usr/local/bin:$PATH   # cron's PATH has neither dart nor flutter, so Section Q did not run on 2026-10-25 (Code #16)
cd /Users/udi/Grassroots/GLP || exit 1
D=$(date -u +%Y-%m-%d)
# The Mac sleeps at night, so it missed 2026-10-29 and 2026-10-30: pmset wakes it at 01:58 UTC
# (Udi, 2026-10-30, "wakepoweron at 3:58AM every day", Israel time), and caffeinate -i holds it awake until the run ends (Code #17).
caffeinate -i bash /Users/udi/Grassroots/tmp/glp-run.sh nightly bash -c "cd /Users/udi/Grassroots/GLP && echo \"main \$(git log --format=%h -1)\" > /Users/udi/Grassroots/tmp/nightly-$D.txt && bash test/run_all_tests.sh >> /Users/udi/Grassroots/tmp/nightly-$D.txt 2>&1"
