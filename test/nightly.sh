#!/bin/bash
# The Mac's part of the nightly, once a night at 02:00 UTC: what the Cloud cannot run.
# The whole suite runs on the helpers, its sections spread over them, started by a
# scheduled task of Code's at 02:00 UTC (Udi, 2026-11-16, to Code #33: the nightly
# moves to the helpers and the Mac's is cut down to pulling main, its seven tests and
# the screens test; until then it was the full suite here, Udi, 2026-10-24, to Code #15).
# The Mac's part: pull main, since the nightly of 2026-11-16 ran on a clone not pulled;
# glp_multiagent's three screenshot tests, which read a macOS font; networking's two
# tests of IPv6 on the loopback; and the apps' screens test.  Its output is in
# /Users/udi/Grassroots/tmp/nightly-<date>.txt.  Run by the user crontab entry
# "0 * * * *", hourly, the script running only in the hour 02 UTC, so it holds
# to 02:00 UTC whatever the Mac's local time.
[ "$(date -u +%H)" = 02 ] || exit 0
export PATH=/opt/homebrew/bin:/usr/local/bin:$PATH   # cron's PATH has neither dart nor flutter, so Section Q did not run on 2026-10-25 (Code #16)
cd /Users/udi/Grassroots/GLP || exit 1
D=$(date -u +%Y-%m-%d)
OUT=/Users/udi/Grassroots/tmp/nightly-$D.txt
# The clone is Code's and stays on main; a pull that is not a fast-forward is said and nothing runs.
# main alone is fetched: a pull of every branch met a fetch in a worktree of the same clone, the worktrees sharing its refs, and stopped (Mac #1, 2026-11-16).
# cron has no key agent, and the key's passphrase is in the login keychain, so the pull of 2026-11-17 was refused, "Permission denied (publickey)", and nothing ran: the login session's agent, launchd's socket, holds the key (Mac #1, 2026-11-17).
[ -n "$SSH_AUTH_SOCK" ] || export SSH_AUTH_SOCK="$(ls /private/tmp/com.apple.launchd.*/Listeners 2>/dev/null | head -1)"
if ! git pull --ff-only origin main > "$OUT" 2>&1; then echo "nightly: git pull --ff-only origin main failed; nothing run" >> "$OUT"; exit 1; fi
echo "main $(git log --format=%h -1)" >> "$OUT"
# The Mac sleeps at night, so it missed 2026-10-29 and 2026-10-30: pmset wakes it at 01:58 UTC
# (Udi, 2026-10-30, "wakepoweron at 3:58AM every day", Israel time), and caffeinate -i holds it awake until the run ends (Code #17).
caffeinate -i bash /Users/udi/Grassroots/tmp/glp-run.sh nightly bash -c "cd /Users/udi/Grassroots/GLP/glp_multiagent && bash tool/sync_glp_assets.sh >> $OUT 2>&1 && flutter test test/coins_screen_test.dart test/superapp_screen_test.dart test/village_market_screens_test.dart test/apps_screens_test.dart >> $OUT 2>&1; echo \"glp_multiagent exit \$?\" >> $OUT; cd /Users/udi/Grassroots/GLP/networking && flutter test test/transport/hole_punch_service_test.dart test/grassroots_network_underlay_sessions_test.dart >> $OUT 2>&1; echo \"networking exit \$?\" >> $OUT"
