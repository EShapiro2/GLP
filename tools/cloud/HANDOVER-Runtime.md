# Cloud #10 handover, Runtime, 2026-11-09

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #26's, in `Cloud-Runtime_inbox.md`; the reports are Cloud #10's, in `Code_inbox.md`.  This file points to the commits and the mail and copies neither.

## Environment

- Flutter with Dart 3.13.7 is at `/home/user/sdk/flutter`; put `/home/user/sdk/flutter/bin` and `/home/user/sdk/flutter/bin/cache/dart-sdk/bin` on PATH in every shell, `bin/glpc` included.
- Repositories added by `add_repo`: `Grassroots-Project-Overview` (the mail), `CGLP` (IGLP), `VEUI-...` (vGLP).  The GLP paper is `GLP-ICLP-2026`, public, cloned without adding.  The attached `Grassroots-Overview` is a paper, not the mail.
- `flutter pub get` and `flutter test` rewrite `analysis_options.yaml` in `glp_multiagent` and `networking/glue`; restore them with `git checkout --` before a commit.
- A baseline of main runs in a worktree of its own, never in the branch's clone while lib/ is being edited.
- `apps_screens_test` hangs in this container on main as on the branches; leave it out of a package run, and say so.
- The worker restarts often: commit and push after each step.

## Done

| Work | Branch, commit | Report |
|---|---|---|
| Declarations made again at a restart; reachable | Cloud-runtime, on main | Code_inbox 2026-11-08 21:09 UTC |
| dispose; the failed save | Cloud-runtime2 929dbb57 | 2026-11-09 13:14 UTC |
| self_app, the runtime's side | Cloud-runtime4, on main | 13:43 UTC |
| The store keeps the program's files | Cloud-runtime5 f9b7db74 | 14:39 UTC |
| self_app, the phone's side (a and b) | Cloud-runtime6 cf70052b, off main | the last report |
| The update check by type identity | Cloud-runtime7 613bbc5f, off Cloud-runtime5 | the last report |

## Queued

1. Each place's centre (Cloud-Runtime_inbox 14:47 UTC; IGLP 96cd44a; GNA a1cb804): GNA's declarePlace(place, centre, radius) returns the centre it registered; the runtime keeps it with the place's standing declaration (runtime/seam_declarations.dart, where a place's radius is kept) and makes each place again about it at a restart (MadContext.declareStandingToLayer).  GlpNetwork.declarePlace and its implementations, the glue's and the simulation's, take the centre.

## Waiting

1. widowed/1, Cloud-runtime3 c13857cb, not for merge: stopped on a point of the GLP paper, Code_inbox 2026-11-09 13:22 UTC.  Left: exclude the goal being reduced and follow the head's bindings, the checker's rule (analyzer.dart holds the Remark as it stands), tests.
2. Points for GLP and IGLP in the reports: what self_app assigns where the host gives no app; IGLP's Persistence on a failed save.
