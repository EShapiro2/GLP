# Handover of Cloud-Debuglog (Cloud #5, 2026-11-08, brought up to date 2026-11-09)

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #26's in `Cloud-Debuglog_inbox.md`, 2026-11-08 15:27 UTC and 21:08 UTC; the specification `phones/docs/debugging.md`; the reports Cloud #5's in `Code_inbox.md`.  This file points to them and copies neither.

## Done

On branch Cloud-debuglog, merged on main 710f7c17:

- `servers/debuglog`: the service, its tests and its README, which says what it answers and how it is deployed; Section DL of `test/run_all_tests.sh`.
- `servers/peoplesweb/deploy/Caddyfile` and `sweep.sh`: the route `/log*` and the thirty days' retention.
- `phones/tools/`: `build.sh`, `devices.sh`, `follow.sh`, each with its use in its head comment.

On branch Cloud-buildsh, on Phones' report of a build iOS refused and on contributors building with their own team: `build.sh` cleans before every build, checks the profile's team, the app's identifier and every framework's platform and team before it installs, and installs under a time limit; `build_sim.sh` builds for the simulator in a clone of its own; `common.sh` holds what they share, with GLP_TEAM and GLP_BUNDLE_ID; the Runner target's identifier is the build setting GLP_BUNDLE_ID; the tools read the repository's password.

## Open

- Code deploys the service on the VPS from main, as the README's "The server" says, once Udi permits it.
- `devices.sh`, `build.sh` and `build_sim.sh` ran here only against stand-ins for `xcrun`, `flutter`, `security`, `codesign` and `ideviceinstaller`; the Mac's runs are their test.
- The Reporter (Cloud-Reporter) posts to the interface the README gives.

## Environment

Flutter with Dart 3.13.7 at `/home/user/sdk/flutter`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  `dart test` in `servers/debuglog` needs nothing else.
