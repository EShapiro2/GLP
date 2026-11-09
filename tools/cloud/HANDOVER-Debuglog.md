# Handover of Cloud-Debuglog (Cloud #12, 2026-11-09, after Cloud #5's of 15:40 UTC)

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  The tasks are Code #26's and Code #27's in `Cloud-Debuglog_inbox.md`, of 2026-11-08 and 2026-11-09; the phones' specification `phones/docs/debugging.md`; the reports Cloud #5's and Cloud #12's in `Code_inbox.md`.  This file points to them and copies neither.

## Done

Cloud #5's, merged on main: `servers/debuglog` and Section DL (Cloud-debuglog, 710f7c17); `phones/tools/` (Cloud-buildsh); the look read by the site and the app (Cloud-look); GSC's clause term types declared once in `programs/gsc/self.glp` (Cloud-gsctypes2, 1bde61f1); `pragmas.sh` reading the Look line (Cloud-lookline 1b19307c, on main 74a133c0).

Cloud #12's, branch Cloud-itemof, off Cloud-lookline 8ef3a75e with main 94622eaa and then 74a133c0 merged in (Code #27's task of 2026-11-09 16:09 UTC, item 1): `item_of(y)` as an argument of an atom, Cloud #5's 51442764, finished.  The 25 platforms of `programs/vglp/cva` printed again (6acab862); GKK's, GSG's, GSN's, GC's and GB's in `programs/apps` (ec7b781d); every cascade record whose artefacts were not what the tools print re-made by `cascade.sh --record`, one tagged commit each, GKK, GSG, GSN, GC, GB, AGORA, GDO, GDC, DGC, GD, GDO-L, GFF, GFP, GPE, BCC, GCC, GFF-I, GFF-PS, GFF-FR, GFF-FR-BC and GSN+GC, GPE last on the merged tree (97a34909, `cascade/GPE/2026-11-09-2`).  The tags are local to the container, the proxy refusing a tag push; Code pushes them.  Sections VG, APPS and CAS at ec7b781d: `PARTIAL Total: 1345 | Passed: 1256 | Failed: 89`, VG 0 failed, the 89 of APPS and CAS each a contract fault that is on main as it is here, below (`programs/apps/compile.sh --check` and `cascade.sh gai|gsnp|cssn|gff --check` run on a worktree of main 74a133c0 give the same).  Section CAS again on the merged tree, 97a34909: `PARTIAL Total: 154 | Passed: 128 | Failed: 26`, cssn 10, gai 9, gsnp 7.

## Not mine, and whose

- GAI's `GAI.txt`: the pragma "Ward and person" leaves unnamed the variable y of `agent_acquaint`, since GnW:3 and GnW:4 lost their guardian role (2e8797b8, 2026-11-08); `pragmas.sh` refuses it, so GAI's and GSNP's platforms and records cannot be printed; APPS gai and gsnp, CAS gai and gsnp fail; and the checker's verdict on both is not_grassroots, acquaint obstructed at role 1 by identity, agent and retired (cascade.sh gai --check, on main 74a133c0 as here).  GAI's.
- CSSN's `CSSN.txt`: the button [Child acquaint] is written by two lines, "Ward and person" and "Ward and ward"; `pragmas.sh` refuses it (vGLP, Section 5.4, "The contract's words": one name one button with one line); CSSN's platform and record cannot be printed; CAS cssn fails.  CSSN's.
- GFF's `GFF.glp`: the compiler refuses the contract as not effective, the schema `deliver` (vGLP, Section 5.4, the paragraph on act schemas); its record now says so (c43de533), `programs/apps/gff/platform.vglp` stays at its last print; APPS gff fails.  GFF's, or vGLP's where the paragraph is wrong.
- GRA's `GRA.glp`: refused for a relation or function in a constraint, `point_friend`, `point_transitive`, `unpoint`; its record says so, unchanged.  GRH's.
- GRV's `GRV.txt`: the pragma "Registration" names `invite_by_link`, which the contract term has not; its record cannot be re-made.  GNA's, of the GSA project.
- GAS's `GAS.txt`: the pragma "Installation from a Store" leaves unnamed the variable z of `install` and `install_listed`.  GSA's.
- GPD's `GPD.glp`: `publish`, `offer` and `install` carry `program(item_of(x))`, a condition Section 5.4 does not name; `leave` carries a quantified conjunct the compiler does not carry.  vGLP's, by Code's task.

## Open

- The home compiled by composition from GPE, GSG, GPD and GAS (task 2): `cascade.sh gpe gsg gpd gas` stops at GAS's pragmas, and would then stop at GPD's two points above.
- GSC's term types declared once (task 3): 18 `.glp` outside `programs/gsc` still declare a copy (`grep -rln "::= sas(" programs`), Currencies' BCC, GCC and GDC, GNA's GRV, GAI's GAI and GSNP, GFF's six, GRH's GDO-LB, GRA and GRA-SS, and GSA's GAS, GPD and GPE; a contract outside `programs/gsc` cannot see `programs/gsc/self.glp`'s types, so they stay until the types are in root `programs/self.glp`, which is with Code for GLP.
- The compiler carrying the look into the platform (Cloud #5's point 4), with vGLP.
- Code deploys the debuglog service on the VPS from main once Udi permits it; `devices.sh`, `build.sh` and `build_sim.sh` ran only against stand-ins here; the Reporter posts to the README's interface.

## Environment

Dart SDK 3.9.4 at `/home/user/sdk/dart-sdk` (unpacked from storage.googleapis.com; the container had none); `export PATH=/home/user/sdk/dart-sdk/bin:$PATH`; `glp_runtime/bin/glpc` built with it.  No Flutter here, so `glp_multiagent` and the screen tests did not run.  `cascade.sh` writes `programs/tests/cascade_emit/` and removes it whole, so two runs of it at once break each other; one at a time, and never while the suite runs.
