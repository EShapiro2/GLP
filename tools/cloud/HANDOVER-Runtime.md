# Cloud #16 handover, Runtime, 2026-11-09

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #27's, in `Cloud-Runtime_inbox.md`; the reports are Cloud #16's, in `Code_inbox.md`.  This file points to the commits and the mail and copies neither.  Cloud #10's handover of the same day is in git, at Cloud-runtime7 c9cdafbb.

## Environment

- The container starts with no Dart and no Flutter.  Flutter stable 3.47.9, with Dart 3.13.7, is the one `glp_multiagent`'s dependency tree needs: `curl` the release `stable/linux/flutter_linux_3.47.9-stable.tar.xz` from `storage.googleapis.com/flutter_infra_release/releases/` into `/home/user/sdk/` and unpack it; then `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH` and `export FLUTTER_ROOT=/home/user/sdk/flutter` in every shell.
- `networking`'s own tests open `libsodium.so`, and the container has only `libsodium.so.23`: `ln -s /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so`.  The glue's tests run without it.
- `flutter pub get` and `flutter test` rewrite `analysis_options.yaml` in `glp_multiagent`, `networking` and `networking/glue`, and `networking/pubspec.lock`; restore them with `git checkout --` before a commit.
- A package run of a branch goes in a worktree of its own (`git worktree add --detach /home/user/GLP-worktrees/<name> <branch>`), a baseline of `main` in another, never in the clone while its `lib/` is being edited.
- `apps_screens_test` hangs in this container, on `main` as on the branches; a package run of `glp_multiagent` leaves it out and says so.  `headless_boot_test`'s 13 tests fail here because the child process writes the container's "running flutter as root" warning to stderr, which the harness reads as a fault; on `main` alike.
- Repositories added by `add_repo`: `Grassroots-Project-Overview` (the mail, push), `Grassroots-docs`, `CGLP` (IGLP's paper), `GLP-Networking-API` (GNA's paper), each cloned at depth 1 under `/home/user/`.
- The native bindings, `networking/ios/Runner/PlaceGeofencePlugin.swift` and `networking/android/.../PlaceGeofencePlugin.kt`, cannot be compiled in this container: a change to them is checked by reading, and on a phone by Code.

## Done

| Work | Branch, commit | Report |
|---|---|---|
| Cloud-runtime5, 7 and 6 joined for merge, run whole against main | Cloud-runtime8 183cc49, off main 94622eaa | Code_inbox 2026-11-09 17:01 UTC |
| Each place's centre: declarePlace(place, centre, radius) through the layer, the platform bindings and the glue; the runtime keeps the centre and declares the place about it at a restart | Cloud-places 38eeefb, off Cloud-runtime8 | the last report |

## Queued

Nothing.

## Waiting

1. widowed/1, Cloud-runtime3 c13857cb, not for merge: waits on GLP's wording (Code_inbox 2026-11-09 13:22 UTC).
2. A point for GNA's and IGLP's Implementation Notes, in the last report: a place's centre crosses the seam as a latitude and a longitude in degrees, as the platforms' geofencing takes it; neither paper says what a centre is.
