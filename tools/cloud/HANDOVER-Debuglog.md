# Handover of Cloud-Debuglog (Cloud #5, 2026-11-08)

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The task is Code #26's in `Cloud-Debuglog_inbox.md`, 2026-11-08 15:27 UTC; the specification `phones/docs/debugging.md`; the report Cloud #5's in `Code_inbox.md` the same day.  This file points to them and copies neither.

## Done, on branch Cloud-debuglog

- `servers/debuglog`: the service, its tests and its README, which says what it answers and how it is deployed; Section DL of `test/run_all_tests.sh`.
- `servers/peoplesweb/deploy/Caddyfile` and `sweep.sh`: the route `/log*` and the thirty days' retention.
- `phones/tools/`: `build.sh`, `devices.sh`, `follow.sh`, each with its use in its head comment.

## Open

- Code deploys the service on the VPS from main once merged (the README's "The server"), and puts the password on the Mac.
- `devices.sh` and `build.sh` ran only against stand-ins for `xcrun`, `flutter`, `security` and `ideviceinstaller`: the first run on the Mac is their test.  `debugging.md`'s question 3 for Code, whether a debug build installs through `ideviceinstaller`, is answered by that run.
- The Reporter (Cloud-Reporter) posts to the interface the README gives.

## Environment

Flutter with Dart 3.13.7 at `/home/user/sdk/flutter`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  `dart test` in `servers/debuglog` needs nothing else.
