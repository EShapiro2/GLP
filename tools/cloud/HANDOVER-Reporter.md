# Cloud-Reporter handover, Cloud #6, 2026-11-08

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The task is Code #26's, `Cloud-Reporter_inbox.md`, 2026-11-08 15:27 UTC; the specification is `phones/docs/debugging.md`, "The phone side", items 1 to 5.  The report to Code, with the commit and the totals, is in `Code_inbox.md`, From: Cloud #6.

## Where the work is

Two tasks of Code #26, both on branch `Cloud-reporter`: the reporter (`Cloud-Reporter_inbox.md`, 15:27 UTC) and the scripted build (16:38 UTC, `phones/docs/debugging.md`, "The scripted build").

The scripted build is `glp_multiagent/lib/main_scripted.dart` and `lib/scripted_person.dart`, the host's optional first act in `superapp_host.dart`, the app's optional scripted person in `main_superapp.dart`, and `phones/tools/build.sh <udid> [<mode> [<known>]]`; its test is `glp_multiagent/test/scripted_build_test.dart`, over the simulated layer that `test/helpers/air.dart` now holds for it and for `superapp_nearby_test.dart`.

The reporter: branch `Cloud-reporter`, off main 05462f3.  The reporter is `glp_multiagent/lib/debuglog/`, wired in `glp_multiagent/lib/main_superapp.dart`; its tests are `glp_multiagent/test/debuglog_reporter_test.dart` and `debuglog_screen_test.dart`.  The other files the branch touches are `superapp_host.dart` (two hooks), `phone_network.dart` (the store's middleware), `pubspec.yaml` and `tool/sync_glp_assets.sh` (the build's commit in the assets).

## Environment

- Flutter 3.x with Dart 3.13.7 is at `/home/user/sdk/flutter`; put its `bin` and `bin/cache/dart-sdk/bin` on `PATH`.  It warns that it runs as root, and runs.
- `flutter test` in `glp_multiagent` needs `bash tool/sync_glp_assets.sh` first, or the asset bundle fails to build.
- `flutter pub get` rewrites `glp_multiagent/analysis_options.yaml`; that change is the tool's and is not committed.
- `Grassroots-Overview` attached to a session is a paper repository, not the mail; the mail is `Grassroots-Project-Overview`, added by `add_repo`.

## Open

The points Code is to decide are in the report; nothing is queued for the role after them.
