# Cloud #16 handover, Runtime, 2026-11-10

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #27's, in `Cloud-Runtime_inbox.md`; the reports are Cloud #16's, in `Code_inbox.md`.  This file points to the commits and the mail and copies neither.  Cloud #10's handover of the same day is in git, at Cloud-runtime7 c9cdafbb.

## Environment

- The container starts with no Dart and no Flutter.  Flutter stable 3.47.9, with Dart 3.13.7, is the one `glp_multiagent`'s dependency tree needs: `curl` the release `stable/linux/flutter_linux_3.47.9-stable.tar.xz` from `storage.googleapis.com/flutter_infra_release/releases/` into `/home/user/sdk/` and unpack it; then `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH` and `export FLUTTER_ROOT=/home/user/sdk/flutter` in every shell.
- `networking`'s own tests open `libsodium.so`, and the container has only `libsodium.so.23`: `ln -s /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so`.  The glue's tests run without it.
- `flutter pub get` and `flutter test` rewrite `analysis_options.yaml` in `glp_multiagent`, `networking` and `networking/glue`, and `networking/pubspec.lock`; restore them with `git checkout --` before a commit.
- A package run of a branch goes in a worktree of its own (`git worktree add --detach /home/user/GLP-worktrees/<name> <branch>`), a baseline of `main` in another, never in the clone while its `lib/` is being edited.
- `apps_screens_test` hangs in this container, on `main` as on the branches; a package run of `glp_multiagent` leaves it out and says so.  `headless_boot_test`'s 13 tests fail here because the child process writes the container's "running flutter as root" warning to stderr, which the harness reads as a fault; on `main` alike.
- The container has no IPv6: `networking`'s four hole-punch tests (`test/transport/hole_punch_service_test.dart`, three; `test/grassroots_network_underlay_sessions_test.dart`, one) fail here on `Address family not supported`, on `main` alike.
- Repositories added by `add_repo`: `Grassroots-Project-Overview` (the mail, push), `Grassroots-docs`, `CGLP` (IGLP's paper), `GLP-Networking-API` (GNA's paper), `VEUI-Volition-Elicitation-...` (vGLP's paper), each cloned at depth 1 under `/home/user/`; the GLP paper is the public `GLP-ICLP-2026`, cloned without `add_repo`.  The GLP clone is shallow; `git fetch --deepen=3000` brings the history a `git log -S` needs.
- The native bindings, `networking/ios/Runner/PlaceGeofencePlugin.swift` and `networking/android/.../PlaceGeofencePlugin.kt`, cannot be compiled in this container: a change to them is checked by reading, and on a phone by Code.
- A whole `dart test` of `glp_runtime` takes about two hours here, under `-j 4`, so it runs in the background with its output in a file; the package's tests of two agents write their programs in temporary directories under `programs/tests/`, and a run killed before its end leaves them, to be removed by hand.  `test/vglp/canonical_test.dart`'s group (iii) fails on `main` 62e8bb5a in this container, two tests, on a widget declaration of a vGLP source, GSG's, before any change of the Runtime branch.
- At a restart from a saved state the runtime grants `refresh` on the person's input stream: a test program that reads that stream must pass it over, or its goal fails there.

## Done

| Work | Branch, commit | Report |
|---|---|---|
| Cloud-runtime5, 7 and 6 joined for merge, run whole against main | Cloud-runtime8 183cc49, off main 94622eaa | Code_inbox 2026-11-09 17:01 UTC |
| Each place's centre: declarePlace(place, centre, radius) through the layer, the platform bindings and the glue; the runtime keeps the centre and declares the place about it at a restart | Cloud-places 38eeefb, off Cloud-runtime8; the platform binding's test brought to the new register, main bce97bbb merged, in the commit after f8b09e3 | Code_inbox 2026-11-09 17:07 UTC, and the last report |
| The Grassroots App's iOS runner registers PlaceGeofencePlugin | Cloud-iosplaces a336f1c, on main 81a0de76 | Code_inbox 2026-11-09 23:07 UTC |
| The state saved when the app goes to the background and at no other time: AgentRuntime.save, the hosts' calls, the lost state started afresh (IGLP d70ec2e) | Runtime-save-on-background 6733fb9, off main 47907f48 | Code_inbox 2026-11-10 09:00 UTC |
| The doc comments' citations brought to IGLP db0a117 | Runtime-cite-db0a117 b3f9dd6, on main | Code_inbox 2026-11-10 09:15 UTC |
| Epochs and widowed/1 (IGLP db0a117; the GLP paper f577e20): names `_w(p, e, i)`, messages with the sender's epoch, `gone(G)`, code format 4, saved-state format 7, the running mark and the restart after a crash, `AgentRuntime.resume`, widowed/1 in both modes with its checker rule | Runtime-epochs 383c296, on main 01253c9c | Code_inbox 2026-11-10 10:33 UTC |
| GLP #14's points (a), (b) and (c) on epochs (the GLP paper 07835e1; IGLP cec4682): widowed_writer/1 and widowed_reader/1 in place of widowed/1, each in its one mode, the checker's admission of both modes removed; the program's link name 3-ary, `'_r'(Key, Epoch, Index)` and `'_w'(Key, Epoch, Index)`, in GlobalName, `'_send'`, `'_authorise_link'`, the reports and `send_to_net`; the holder-epoch citations at IGLP cec4682 | Runtime-epochs2 3138bba, off main 01253c9c | Code_inbox 2026-11-10 11:18 UTC |
| GLP #14's three small points (Code_inbox 11:44 UTC): the self_app kernel's doc comment cites the GLP paper b840c47; the bridge's rendering of pending(P) stands, the canonical compilation producing it (constructs.dart, _pend) and vGLP naming it (sections/parts/dispatcher.tex, the construct process); graph_source_identity_test names no flag since main 6b2c06ea | Runtime-small b6c8f723, 7f502aca, off main 75ac1709 | Code_inbox 2026-11-10 11:50 UTC |
| Real arithmetic (TGLP 1962166, "Type checking of :="; the GLP paper 0ec95cd, the kernel table): X := E typed Real on a real expression, the real kernels declared Real in the root, the := test's Real cases | Runtime-real a4c3eea6, off main fea427d5 | Code_inbox 2026-11-10, the last report |
| GLP #14's two questions (Code_inbox 11:49 UTC), answered in the report: the linker's three mechanisms, each with the smallest program refused without it, by a scratch branch with switches; GB's village under :boot does not reproduce on main, the first passing commit 3e2eea07 | no branch; scratch-linker thrown away | Code_inbox 2026-11-10, the last report |
| The linker's three mechanisms cite TGLP d993305, "Sources compiled to GLP", each naming the cases the paragraph leaves to M#T; M#T found unresolved by the checker | Runtime-cite-d993305 0e446ff1, off main ed899348 | Code_inbox 2026-11-10 13:15 UTC |
| Boot files (TGLP 6ce148a, Entry and the absence of a boot module; GLP #14, Code_inbox 13:08 UTC): the loader refuses a boot file defining a procedure no person of its clause names or calls; neg_guard_boot, neg_srsw_boot, boot_scope's three, and programs/tests/multiagent's nearby_boot and peer_app_boot on program directories; the Dart tests' inline boots over a program directory (test/multiagent/boot_program.dart); Section NEG's checks and a new negative | Runtime-boots, off main ed899348; the commit is in the report | Code_inbox 2026-11-10, the last report |

## Queued

Nothing.

## Waiting

1. A point for GNA's and IGLP's Implementation Notes, in the report of Cloud-places: a place's centre crosses the seam as a latitude and a longitude in degrees, as the platforms' geofencing takes it; neither paper says what a centre is.
2. M#T (Runtime-cite-d993305's report): the checker refuses a type written M#T, 'Unresolved type: a#Colour'; TGLP d993305 sends every module above a sources' directory to it, and nine modules of the tree name the formed types plainly instead, the linker writing them; GLP's ruling and IGLP's fix wait.
3. The village boot on main takes 20 seconds here, `bin/glpc` rebuilt and all: `git bisect run` judged every step by a check that passed by hand, so the bisect was driven by a loop of `git bisect fixed|broken` over the same script (scratchpad, bisect_village.sh); a worktree of the clone served it, the clone deepened first.
4. A boot's agents' program is a program directory (TGLP 6ce148a): a test that boots agents from an inline source gives the clause and its persons to the loader and the program to `bootWithProgram` (glp_runtime/test/multiagent/boot_program.dart), which writes it as a temporary directory's self.glp under programs/tests.
