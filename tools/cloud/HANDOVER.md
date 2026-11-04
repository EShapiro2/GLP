# Cloud #1 handover, 2026-11-04

On `main` since 2026-11-04 (Code #23), so that a fresh cloud clone carries it; the branch `Cloud-tools` (0286ce8d) holds nothing else current.  A Cloud session starts from `CLAUDE.md`, section "Cloud".

Branch `Cloud-tools` of GLP, not for merging: this file, the comparison of the FCP machine with the engine, the
reading survey of glp_runtime, and the scripts the optimisation gates and benchmarks ran on.  The scripts name the
first session's scratchpad (`S=/tmp/claude-0/.../scratchpad`) and `/home/user/glp`; set `S` and the paths to the
new session's before running them.

## Standing arrangement

- Cloud is Code's proxy for cloud missions (Udi, 2026-10-31).  Tasked by Code alone, in `Cloud_inbox.md`; reports
  in `Code_inbox.md`, `To: Code`; signs `Cloud #<n>`; writes to no owner.  Optimisation steps are discussed with Udi
  in the chat, reported to Code; Udi's word on a merge is worded as his recommendation, not his approval.
- Mail: clone `EShapiro2/Grassroots-Project-Overview`; `git pull -q --no-rebase --no-edit origin main` first;
  `bash bin/mail-read Cloud "Cloud #<n>"`; `bash bin/mail-send Code "Cloud #<n>" "Code" "<msg>"`, then
  `bash bin/mail-backup "$PWD/mail/Code_inbox.md" "<summary>"` (its exec from mail-send fails in a clone).
- Udi's rule for the engine (2026-11-02): follow the FCP abstract machine and compiler (EShapiro2/FCP, the EFCP
  emulator in `EFCP/Efcp-release/Logix/EMULATOR`, Kliger's compiler in `CheckedOut/Logix/system/ndg`); every
  deviation is discussed with him first.
- Environment: the container's Flutter was upgraded to Dart 3.13.7 (`flutter upgrade --force`), which
  glp_multiagent's dependency tree needs; export the SDK's `bin` and `cache/dart-sdk/bin` in background shells.
  The GLP clone may be at `/home/user/glp`; worktrees per task beside it.

## Done, all on GLP main

| What | Commit | Merged |
|---|---|---|
| Cleanup of glp_runtime (six commits) | a637a19c | yes |
| Code image built once per load set | 3f76cdf5 | yes |
| GoalRecord, one record per goal | cf6e71b7 | yes |
| Program DFA once per environment, layered | 15e1117 | cf0678cd |
| Stage A: registers as arrays, one runner context | 485ef1e | cf0678cd |
| Rung 3: N agents over UDP loopback, both forms, ten probes | 064578f | 8e4e185a |
| GSC rung 2: gsg, gsn boots, play_held, gkk_boot | c1d3f55 | 56d6911f |
| GAI rung 2: gai, gsnp boots | b3a12f5 | 56d6911f |

Gains measured fairly at the end (main b4919312 against IGLP-cloud1 9acee0fd, the two trees alternating case by
case): the loads and plays 30 to 50% faster, the cascades 15 to 35%, the sGLP social graph 17 to 31%.  One anomaly
not explained: fib_23 did not move while fib_linear and fib_20_x10 gained 26 to 32%.

## Waiting on someone else

- **Stage B**, the trail in place of the tentative substitution map (FCP's `asgn`/`asgn_trl_undo`): waits on
  Udi's word, since it changes IGLP's "Clause try" sentence "The tentative substitution reaches the heap only at
  commit".  **Stage C**, the decision-graph compiler (FCP's ndg), needs switch instructions in IGLP's Code Format
  and a compiler phase written to GLP's matching table.  See `fcp-vs-glp-engine-2026-11-02.md`.
- **Task 4, Currencies rung 2** (Code #22, 2026-11-03 16:26 UTC; may start on GLP main b4fb7f13, Code 20:27 UTC):
  boots for gc/plays.glp, gb/plays.glp, coins/currency/plays.glp, bonds/dated/plays.glp,
  sovereign/denominated/plays.glp, coins/play_ui.glp, sovereign/play_ui.glp, secure/play_issuer.glp.  Main now
  has the deployed path: graph/core's agent/2 answers every boot by its first act, the phones in
  `graph/core/<app>_phone.glp`, the phone's first act `superapp_ui(Me, [])`; coins' and sovereign's agent/2.
  Two questions to Currencies, unanswered: where the dated bonds' agent/2 goes, and how the secure issuer receives
  bob's request across isolates (secure has no network entry).  The worktree `glp-cur-rung2` was made at b4919312;
  remake it on b4fb7f13.
- **Task 5, CSSN rung 2** (Code #22, 2026-11-03 16:46 UTC, after task 4): a boot of the village play,
  programs/cssn/plays.glp, under the new Boot; cascade record, village figure and simulation stay stopped.
- **The ten points** (GLP #11, 2026-11-03 16:40 UTC, via Code 16:45 UTC; Blocking: no, lowest): the rulings on
  the reading survey's points needing a ruling (`glp_runtime-reading-2026-10-31.md`): one partial evaluator;
  `_substParam` flips the mode; 1 and 1.0 distinct in `sameTypeExpr` and `shadowedBy`; point 4 to vGLP; delete
  `message_vocabulary.dart`; one reader refusing `-module(`; one goal reader; one reactivation path through
  `enqueueReactivatedGoal`; remove `MessageType.agentMessage`, write the cell once, raise the store's version;
  give every phase a category.  Each is a gate of its own (gate2.sh).
- **linkprobe14** over the real layer: with GLP; main has it now (Code 19:11 UTC).

## Order Code gave

GNA first (not Cloud's), then task 4, then task 5; the ten points lowest.
