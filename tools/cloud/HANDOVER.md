# Cloud #2 handover, 2026-11-05

This file is on `main`, `tools/cloud/`, with Cloud #1's scripts and reports beside it (its handover of 2026-11-04 is in git at 95960851).  Cloud #1's scripts name its scratchpad and `/home/user/glp`; `gate3.sh`, this session's gate, takes the two trees as arguments and runs each side in its own tree.

## Standing arrangement

- Cloud is Code's proxy for cloud missions (Udi, 2026-10-31).  Tasked by Code alone, in `Cloud_inbox.md`; reports in `Code_inbox.md`, `To: Code`; signs `Cloud #<n>`; writes to no owner.  GLP's `CLAUDE.md`, section Cloud, is the startup: the three repositories, the reading, the mail, the handover.  CGLP, IGLP's paper, Udi enabled for Cloud #2 on 2026-11-05; ask for it with the other three.
- Mail: clone `EShapiro2/Grassroots-Project-Overview` (read access sufficed: the proxy pushed the backup); `git pull -q --no-rebase --no-edit origin main` first; `bash bin/mail-read Cloud "Cloud #<n>"`; `bash bin/mail-send Code "Cloud #<n>" "Code" -` with the message on stdin, then `bash bin/mail-backup "$PWD/mail/Code_inbox.md" "<summary>"`.
- Two Cloud sessions read the inbox since 2026-11-04 19:28 UTC: Cloud #3's mission is the sGLP simulator alone, and the other Cloud touches nothing under `programs/sglp`.
- Environment: the Flutter SDK 3.47.9 (Dart 3.13.7), unpacked under `/home/user/sdk/flutter` from storage.googleapis.com (`git config --global --add safe.directory /home/user/sdk/flutter` first, the tarball being another user's); export `/home/user/sdk/flutter/bin/cache/dart-sdk/bin`.  The REPL's root is the tree it is built in: run a worktree's `glp_runtime/bin/glpc` on that worktree's programs.  Pushing GLP needs `add_repo` with push access.  A `cd` in one of several parallel shell calls moves the shell of the others: absolute paths everywhere.  The fast tier runs through on Linux since main 7de72772, Section SL taking a file's size by `stat -c%s` where `stat -f%z` fails.

## Done

| What | Where | Merged |
|---|---|---|
| Task 4, GC's village market on six smartphones: gc_village_boot.glp, gc_phone's market roles, Section APPS's checks of what the script and the end state fix | Currencies-rung2 774479f7 | GLP main 976944b2 (Code #23, 2026-11-05 07:16 UTC) |
| Task 4, GB's boots, play_bonds on three and the village on seven, core/gb_phone.glp: both settle at the first day | Currencies-rung2 895dfbe3 | with the above |
| Task 4, the seven plays of programs/currencies on smartphones: coins play_swap, play_same and play_ui's two, dated play_swap and play_same, denominated play_swap and play_ui's sovereign market; three phones in core, seven boots in programs/currencies, the roles in each program's miniapp_person/5, every role waiting for its conversations; Sections N2, N3 and N4 hold them, 270 of 270; play_mint has no boot, the super-app activating a mini-app at an invitation only (GSG's question, told Code) | Currencies-rung2 93f8ad77 | GLP main ce1059ca (Code #23, 2026-11-05 10:25 UTC) |
| The ten points of GLP's rulings (Code_inbox 2026-11-03 16:40 UTC), point 4 vGLP's: nine commits, 3ee73bb9 (5) 1732d32b (10) 0d51d5d0 (3) 3ab23863 (2) 23be14a2 (8) 53464201 (9) f18b751b and c65349e6 (6 and 7) b9137624 (1); dart test 2571 passed and three new tests; the gate against main 976944b2 clean but for the error texts the rulings change | IGLP-cloud1 c65349e6 | GLP main 69f123b1 (Code #23, 2026-11-05 10:25 UTC) |

## Blocked, with Code (its 2026-11-05 07:16 UTC: the questions are with the owners, the answers come through Code)

- GB's two plays across isolates (Code_inbox 2026-11-05 06:59 UTC): 1. a defect, alice's acquaintance with frank in gb_village_boot never starting after frank answers her hello on a conversation not yet open, where ten other such answers complete; reversing who connects lets the village run on (a diagnosis; the branch keeps the rule); with GSG and GLP.  2. the days: the plays write a person's day off another person's screen; a phone sees its own screen at Opaque, a person cannot advance past today, the harness waits on no clock; Cloud recommended the harness give the day under :boot, stepped when every agent is quiescent; with GLP, GSG and Currencies.
- Task 5, CSSN's village (07:01 UTC): the parents' 25 waits on their children's phones have no source on a phone; Cloud recommended the scripts wait on the parent's own screen; with CSSN.
- secure/play_issuer.glp stays one agent (Currencies, 2026-11-04 16:10 UTC): no boot.
- Cloud #1's: Stage B of the engine (the trail) waits on Udi; Stage C (the decision-graph compiler) on IGLP's Code Format; see `fcp-vs-glp-engine-2026-11-02.md`.

## Next, in Code's order

The answers on GB and CSSN, each a boot to write as the GC village was written (`programs/apps/gc_village_boot.glp`, `core/gc_phone.glp`'s market roles and `core/gb_phone.glp` the forms); then whatever Code queues.  Point 2 of the ten has no test of its own, `_substParam` being private.
