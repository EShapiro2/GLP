# Handover of Cloud-Debuglog (Cloud #5, 2026-11-08, brought up to date 2026-11-09 15:40 UTC, stopping as Udi switches accounts)

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #26's in `Cloud-Debuglog_inbox.md`, of 2026-11-08 and 2026-11-09; the phones' specification `phones/docs/debugging.md`; the reports Cloud #5's in `Code_inbox.md`.  This file points to them and copies neither.

## Done

On branch Cloud-debuglog, merged on main 710f7c17:

- `servers/debuglog`: the service, its tests and its README, which says what it answers and how it is deployed; Section DL of `test/run_all_tests.sh`.
- `servers/peoplesweb/deploy/Caddyfile` and `sweep.sh`: the route `/log*` and the thirty days' retention.
- `phones/tools/`: `build.sh`, `devices.sh`, `follow.sh`, each with its use in its head comment.

On branch Cloud-buildsh, merged on main, on Phones' report of a build iOS refused and on contributors building with their own team: `build.sh` cleans before every build, checks the profile's team, the app's identifier and every framework's platform and team before it installs, and installs under a time limit; `build_sim.sh` builds for the simulator in a clone of its own; `common.sh` holds what they share, with GLP_TEAM and GLP_BUNDLE_ID; the Runner target's identifier is the build setting GLP_BUNDLE_ID; the tools read the repository's password.

On 2026-11-09, three tasks beside the role's, each reported in `Code_inbox.md`:

- GSC's real values: no change was needed in programs/gsc; BCC lacked `real(Constant)` in its own copy of the term types, which Currencies added.
- The look, branch Cloud-look: the site and the app read `programs/apps/look.json`, each contract's Look line and `about.json` until then; the app's green is gone (`glp_multiagent/lib/ui_runtime/look.dart`).
- GSC's term types declared once, branch Cloud-gsctypes2 (Cloud-gsctypes superseded): `SSchema`, `SContract` and `SDeclared` in `programs/gsc/self.glp`, GSC's four contracts declaring no copy, and their records re-made.  The other owners' 26 copies stay until a contract outside programs/gsc can see those types; the recommendation, the types in root `programs/self.glp`, is with Code for GLP.

In flight, branch Cloud-lookline (Code #26, `Cloud-Debuglog_inbox.md`, 2026-11-09 14:47 and 14:54 UTC):

- Done, 1b19307c: `pragmas.sh` reads the Look line the interface pragmas open with, and no tab; GPE compiles; Section VG's cases XL, XLI, XLX and XLF, VG 944 of 944 at that commit.
- Done but not finished, 51442764: `item_of` as an argument of an atom in vGLP's compiler (cvac.glp, local.glp, self.glp, text.glp) and the VG case itemarg, which passes; a platform printed with it emits to GLP.  Left: the printed runtime grew, so every committed platform the suite holds to what the compiler prints must be printed again (VG, 25 failed of the 811 run before it was stopped: social_graph, social_network, currency, nudge, introduce and the rest), and the cascade records re-made, then VG and CAS run whole.
- GPD still does not compile, on two points that are vGLP's: `program(item_of(x))`, a condition Section 5.4 does not name, and `leave`'s quantified conjunct, of a shape the compiler does not carry.
- Not started: the compiler carrying the look into the platform.  vGLP e1d1b8a says it does and not in what form; the reader left is a compiled mini-app on a phone, which learns its tabs from the view its agent writes.  Recommended: the view carries the look as it carries the tabs; every expected view in the suite changes with it.

## Open

- Code deploys the service on the VPS from main, as the README's "The server" says, once Udi permits it.
- `devices.sh`, `build.sh` and `build_sim.sh` ran here only against stand-ins for `xcrun`, `flutter`, `security`, `codesign` and `ideviceinstaller`; the Mac's runs are their test.
- The Reporter (Cloud-Reporter) posts to the interface the README gives.
- The tags of the four records on Cloud-gsctypes2, which the proxy refuses from here, are Code's to push.
- In this container every Flutter run rewrites `glp_multiagent/analysis_options.yaml`, which voids a suite run; restore it with `git checkout` before a run and leave it out of commits.  The screen tests need the Mac's fonts and cannot pass here.

## Environment

Flutter with Dart 3.13.7 at `/home/user/sdk/flutter`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  `dart test` in `servers/debuglog` needs nothing else.
