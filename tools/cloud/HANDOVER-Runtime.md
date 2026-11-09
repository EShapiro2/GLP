# Cloud #10 handover, Runtime, 2026-11-09

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The task is Code #26's, `Cloud-Runtime_inbox.md`, 2026-11-08 19:40 UTC; the report is `Code_inbox.md`, To: Code, from Cloud #10.  This file points to the commits and the mail and copies neither.

## Environment

- Flutter with Dart 3.13.7 is at `/home/user/sdk/flutter`; put `/home/user/sdk/flutter/bin` and `/home/user/sdk/flutter/bin/cache/dart-sdk/bin` on PATH in every shell, `bin/glpc` included.
- Repositories added by `add_repo`: `Grassroots-Project-Overview` (the mail), `CGLP` (IGLP).  The GLP paper is `GLP-ICLP-2026`, public, cloned without adding.  The attached `Grassroots-Overview` is a paper, not the mail.
- `flutter pub get` and `flutter test` rewrite `analysis_options.yaml` in `glp_multiagent` and `networking/glue`; restore them with `git checkout --` before a commit.
- A baseline of main runs in a worktree of its own (`git worktree add /home/user/GLP-base origin/main`), never in the branch's clone while lib/ is being edited.

## Done, branch Cloud-runtime

1. Task 1, the restart makes every standing declaration to the new layer: 791599e, and the super-app tests' expectations 8436048.
2. Task 2, reachable: 6f2f407.
3. Task 3, load_file/2 after a refused update: not built; the store does not keep the program's files, and the report says what the fix needs.

On main at 7283bc77.

## Done, branch Cloud-runtime2

Code #26's task of 2026-11-08 22:09 UTC, Cloud #6's two findings: `AgentRuntime.dispose` ends the launch, and a failed save is reported once and leaves the event loop free: 850892c6.

## Open

1. Task 3 of Cloud-runtime, load_file/2 after a refused update, waits on IGLP's answer, which Code sent.
2. The points of the reports in Code_inbox.md, Cloud #10, 2026-11-08 21:09 UTC and 2026-11-09: a place made again at a restart being centred where the device is at the restart, and IGLP's Persistence saying what the runtime does when a save fails.
3. `apps_screens_test` hangs in this container on main as on the branches; leave it out of a package run, and say so.
