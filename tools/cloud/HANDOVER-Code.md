# Code #37, 2026-11-25 ~19:40 UTC, retiring after its 4th compaction

Code #37 retires (Udi, 2026-11-25 10:34 UTC: after a handover a session retires).  The next Code session continues from here.

- **Integration branch `code-int2-sets-2026-11-25` at 677a5279b, pushed, not on main.**  It carries: the form 4f632c0 (named_widgets.dart: `size(S, F)` sets, `SocialAsk.propose` grants each set element then `listEnd`, vGLP 6.3; canonical.dart `_condition` takes `size(S, F)`); vGLP 9fcd36f, b19dedb, 13661eb, 7d8b2db, efa2f9d; Currencies 958a8ed, 5397ca5 (Coins and Bonds tab lines dropped); Calculus e65a66f, 2d5d069, 9adf185, 1bfb30e, 852f80f; GCE ab9002a, bbf4f7e, e4aacb8 (Disputes and Warnings, adopt.py --check ok); GM b17ca87 (Tennis Courts); all 89 adopted files printed again; the cssn and cva platforms printed again; main's retire rule.  Udi's yes for GCE's and GM's contracts: 2026-11-25 18:50 UTC, "1. yes 2. yes 3. done 4. ok 5. merge".  At b8a1fb3: APPS+VG 1663/1663, GSC 772/772.  QF at 4f632c0: 237/243 (3 Arial, 3 the GC, GB, village market widget tests Currencies is to mend).
- **Records, in progress at 677a527.**  90 rows (each manifest's "by `cascade.sh X`").  11 done at dde03f9 (they differ from 677a527 only in Warnings, so re-record warnings).  Helpers #1-4 tasked 2026-11-25 ~19:30 UTC, branches `helper-N/code37-records`, each a row list (Helper-1: labelled_overlay, liquid_cooperative, lodging, meeting_rooms, notes, partners, promoted_gossip, recommendations, rides, separating_distrust; 2-4 the rest of the helper lists); their reports come to Code_inbox.md.  The remaining 39 rows were queued for Code's own worktrees and are NOT run: give them to helpers 5-12.  Re-record warnings, disputes, tennis_courts at 677a527.  Rows failing on GO pragma faults at 976c32b (central_bank, commercial_bank, community_network, cooperative_dao, ads, backed_community_bank, bank, community_market): GO #15 holds the faults; 6 GO platforms do not compile (Calculus).
- **Then:** collect the records into the integration branch, commit, run APPS, VG and CAS (`cascade.sh --check`); site check (peoplesweb worlds, Friendship first, Audit #128); push to main; send main's commit to vGLP #33 (Audit #128), Calculus #11, GCE #1, GM, Currencies #23.
- **Audit #151 (GLP issue #167):** sets mended by 4f632c0 and vGLP's localsets and size.  The determined numbers (DAO pass/reject, Organisation act/amend, Double = Votes*2) asked of vGLP #33; Code #37 recommended `calc(P, E)` in the act's seventh list, the form computing it.  Merge that branch, code the form, then answer through Coordination to "Answer audit findings, Audit #5", and tell GCE.
- **Villagers:** carry Villagers main 20d8b67's contracts into GLP main and its paper into the papers tree when Villagers #1 sends the files and where each goes (Udi approved); then tell Audit #4's thread the commits, through Coordination.
- **Retrospective list for Udi:** constructs.dart (03bc123); old files deleted; Website-site held for LWG; Promoted-Community-Market; Organisation:1; Disputes' ruling text; GCE Interface mends; gb's known red, cleared; Coins and Bonds tab lines dropped (5397ca5); Foundation, Nonprofit, Partnership parametric (changed by Calculus e65a66f).
- Main at handover: 154d5006e (Website #166), with 9d0952c (retire rule).

# Code #37, 2026-11-25 08:55 UTC

- Main 852ed07: compound values (vGLP 2ad8dde, GSC 95d299c, Calculus b924830, Currencies 0eb5365 and d85e1df gb, GO d233191 and 5c02ddb); 91 adopted, platforms compile, 66 records printed, APPS and GSC 1021/1021, site check clean.  gb's known-red entries gone.  Also on main: Calculus derive.py b42a475 and 4e68cca (Audit PR 123, 125).
- Owed: Flutter QF as non-root.

# Code #37, 2026-11-25 02:35 UTC

- Main 8d14cc0: the sets change.  The Calculus's set terms (35dafec, d7b7a8f, 08f4b51 Found by an organisation and a single note a set of one, 5e6186d Disputes' ruling, a31ed7e Provenance Groups' messages), GSC 4a9175a (checker for sets), vGLP 881c449 (compiler and pragmas.sh on sets, Organisation's plays), GO da8ae14, GAI 4bef766 (gsnp screens), Currencies eac7ad2 (gc).  All 91 adopted (adopted-check 91/0, --check on all); every platform compiles (apps, CSSN, vGLP --check); the 66 records printed at 2d33962 plus Provenance Groups and Secure Friendship (its plays' platform printed afresh); gfwc dormant.  Section APPS 227/227 with gb's 22 checks known red: Bonds' term names a bond's type val('bond(Day)'), sent by Currencies to Calculus #11 and vGLP #32; Currencies mends gb, then the entries come off.
- cascade.sh: Organisation plays from cva/organisation; a play's declaration may run over several lines.
- Code mended GCE's Interface lines (GCE has no thread): Stays' Cancel without <sum>; Disputes' Decide <ruling>.
- Site check before every push to main: tools/cloud/site-check.sh.
- Owed: Flutter QF as non-root; the full suite on main (helpers' nightly).
- Retrospective list: constructs.dart (03bc123); old files deleted; Website-site held for LWG; Promoted-Community-Market; Organisation:1 (Calculus item 15); Disputes' ruling text; gb known red.

# Code #37, 2026-11-24 16:10 UTC, after its 2nd compaction

- Main 55c8532: the 91 contracts adopted and printed afresh (d966834, adopt.py --check on all 91); renames done and old Agents, Community, Backed-Community programs and records deleted; Audit PR 83 fixes; GSC 0daa1b8; CSSN Children; Website e7ec8fc (relation words; the 13:40 UTC outage); GO 55c8532.
- Udi's rule, 13:47 UTC: before every push to main run `bash tools/cloud/site-check.sh` (builds the site, loads it in Chromium; must say N contracts, >=50 svg, 0 errors).
- Held: printing all 66 records waits for the sets change.  When Calculus #11 sends GLP branch claude/project-thread-23okte (calculus/ only, set terms plus Audit 102 Findings 18, 19, #58), merge it together with GSC claude/project-thread-pgtsao 4a9175a; adopt.py on all 91 (three passes, then --check); programs/apps/compile.sh and programs/vglp/cva/compile.sh --check; print the 66 records in three worktrees (/home/user/wt-sglp, wt-gsc, wt-vg); site-check; push; tell the thread "Answer audit findings on contracts" the commit (Audit PR 106 Findings 18, 19, 20).
- Owed: Flutter QF run as non-root (Flutter 3.47.9 / Dart 3.13.7, /private/tmp, libsodium); vGLP's 19 reds were a root run.
- Not merged: Website-site 749372f4 (carries LWG's listing; no LWG on main without Udi's yes).
- Retrospective-approval list: constructs.dart one-line change (03bc123) for OSPM; deletion of the old files; Website-site held for LWG; Promoted-Community-Market on Udi's question list.
- In hand: answer Calculus #8's worlds proposal (Calculus docs/design-decisions.md 63, 64) to Coordination_inbox.md.

# Code #37, 2026-11-24 10:45 UTC, after its 1st compaction

- Done: the 91 Calculus contracts adopted on main f26b0da (adopt.py --adopted-check 91/0, --split-check 91/0); renames AI Agents, Community Bank (gcc), Backed Community Bank; go's Community Bank deleted; build.py reads calculus/contracts (local build: 92 contracts, the 91 plus Promoted Community Market, on Udi's list); apps/compile.sh and Section APPS read AI-Agents.  Merged with it: GSC 0898e2a, Currencies e772ed5, GO c41cd4a, vGLP a5a59e2, Calculus 70330f3, sGLP ae2a75c.
- Open, with owners: the CVA compiler refuses 11 that compiled before (App, Auction, Children, Local Overlay, Second-Score Auction, Separating Distrust, Provenance Groups, Denominated Coins, Denominated Currencies: pragmas.sh does not inherit buttons through weakens, unites, renames, lifts; vGLP) and Coins, Bonds (list pragmas name no predicate; Currencies).  When fixed: merge, run compile.sh of vglp/cva and apps, cascade.sh on those rows.  secure_friendship's play platform (programs/vglp/cva/secure_friendship) is printed by no compile.sh: vGLP's.
- 20 GO contracts are parametric (Calculus #11 certifies it): no platform until instantiated.
- Old programs/gai/Agents.glp, gcc/Community.glp, go/Backed-Community.glp and cascade/Agents, Community, Backed-Community still in the tree; vglp/cva and tests may read them; remove once nothing does.
- The full suite was not run on f26b0da.

# Code #37, 2026-11-24 00:58 UTC (a thread of the project "Grassroots Oversight")

- Merged to main Udi's two rules (8b5a3fa: claude.md, Contracts; claudese.md, 32) and the text check's present-tense construction with its pair in tests.tsv (29bdf37): 18 tests, 0 wrong; 214 of 285 rejected texts caught.  Mailed to_all to reread them.
- Udi, 2026-11-24 00:53 UTC: "fault resilience is not at this level of abstraction".  GLP #144 and #145 (Ohad Eitan) answered on GitHub, not merged, on SPM's verdict; the Calculus's 37f2045 and 0588310 withdrawn, not merged; 25b4215 (stamp for #144's term) waits on SPM's revision of #144.
- Not checked by Code #37: Calculus #9's Calculus-next17 490a4afde of 2026-11-19 is not an ancestor of main (Code-next17 and Code-gce may hold it).

# Handover of Code #36, 2026-11-23 13:00 UTC

Code #36 shut down at Udi's word (2026-11-23 12:49 UTC: "yes, we need to shut down and restart with a new account, so shut down all threads and write everything clearly in the handover").  This file is for the next Code session, on a new account with none of the old project's chat, threads or project files.  Earlier handovers are in git history of this file.

## Who Code is

Code is the one session that merges to GLP main, prints the cascade records, runs the suite and answers the pull requests on EShapiro2/GLP.  Its rules: `organisation/claude.md`, `organisation/writing-style-guide.md`, `organisation/claudese.md` (read all three in full at the start), `organisation/projects.md` (each project's repo and record), `CLAUDE.md` of this repo, and `EShapiro2/Integration`'s `CLAUDE.md`.  Udi answers questions only in the project chat; a thread sends each question to the coordinator as the full text ready for him.

## State of main

- Head 22bb801ce (Coordination's rule, admit a mistake at once).  Everything Code did is on main; no worktree holds unpushed work.
- Records: all 89 in `cascade/` printed again on 2026-11-22 (dd7f57027).  Section CAS (`GLP_SECTIONS="CAS" bash test/run_all_tests.sh`) checks every row of `cascade.sh` and every record with no row.  It passes all but: Groups+Coins (stale, held); Tennis-Courts and Rehearsal-Rooms (no `.glp`, not certified by the Calculus; their records stay because the live site's pages read them, and removing them needs Udi's yes); gfwc (dormant, skipped).
- Fast tier with TY and VG at dd7f57027's tree: 4220 passed, 0 failed.
- The text check, `tools/myenglish/check.py` (`--udi`, `--paper FILE`, `--clause`, `--test`): 16 tests, 0 wrong; 214 of 285 texts of `rejected-coordination.md` caught.  Standing duty: each text Udi calls Claudese goes into the check and into `tools/myenglish/tests.tsv` (a `wrong` line, and a `right` line only for a text Udi accepted), is merged, and the commit is reported; Coordination enters the pair in style guide 41 and claudese.md.

## Branches waiting for a merge

- `claude/project-thread-podf7o` b5c0e90e2, the sweep: NOT to be merged before Udi approves it.
- `claude/project-thread-7mpm39` c43fec393 (LWG): never merge; LWG goes neither on main nor on the live site.
- Finding 1 set (Audit Finding 1), to be merged together once GLP-Spec's exports guard is approved by Udi and in:
  1. GSC's checker, `claude/project-thread-qgc92q` (now f4ef10dbf).
  2. vGLP's, `claude/project-thread-gxjvnp` (now 2e88bbc62).
  3. GSA's, `claude/project-thread-xztgmf` (now 6c18841c2), which merges `code/type-identity-scope` (ef88e174c, Code's, interface text spells types by their defining scope).
  4. CSSN's `claude/project-thread-s9ompe` 66fa03f75 (rests on all of them); then print `cascade/Children` again.
  5. Currencies' `claude/project-thread-d567ul` acce035ef (dgc app; N2-N4 on compiled apps).
  6. GLP-Spec's `claude/project-thread-97txt2` 277d1a4e2 (encounters fixture).
  At that merge: remove the 20 entries of `tools/cloud/finding1-krc-remove.txt` from KNOWN_RED_CHECKS; reprint dgc by compile.sh; add `:artefact $GLP_DIR/programs/gsc/checker <core dir>` before every install through the super-app in `test/run_all_tests.sh` (Section APPS's super-app boots and Currencies' N2, N3, N4; GSA also wants it in Section K's install script).  Check each branch's head again before merging: the commits above are what was last seen.
- GO's `GO-spc-play` a1d69fab1, Single-Person Company's play: waits on vGLP.  Found is never proposed on a compiled platform; vGLP confirmed this is a gap of the OSPM paper (the old vGLP paper), whose revision waits on Udi, then the cvac change on gxjvnp and a reprint of `programs/go`'s platforms.  Then add a `cascade.sh` row `single_person_company` (PLAYDIR="single_person_company", PLAYLIMIT 200000000), record it, and tell GO for Audit finding 4.
- Website: branch `Website-site` 84a665887 builds /next; Website deploys main.  A visible change on the live site needs Udi's yes.

## Open pull requests on EShapiro2/GLP

Every pull request gets an answer: taken, or declined with the reason.
- #61 (Nimrod, LWG prototype): never on main; waits on the Calculus.
- #123, #124 (Nimrod, vGLP library: root, distance): wait on Nimrod.
- #125 (Nimrod, a count's condition with a function), #131 hohfeld.py, #132 attested.py, #133 PRESERVES.txt: wait on the Calculus.
- #128 (Dial, Plane, Budget, Committee Governance), #130 (Charter), #129 (DAO clauses 7 and 8): the Calculus's verdict of 2026-11-22 17:23 UTC: #130 and #128's Dial and Plane hold; Budget and Committee need #125.  None merges until Udi approves the five names, GCE as their owner, and DAO's clauses 6 to 8.
- #94 (Yuval, GFF path study): waits on Yuval.
- #142 (Yuval, `programs/sglp/graphs`) and #143 (Yuval, draft, Feeds under sGLP, on #142): not yet reviewed by Code; owners sGLP/vGLP and GFF.

## Approved by Udi and not yet done

None that Code knows of.

## Open questions for Udi (each to go through the coordinator)

1. Shall the sweep, branch `claude/project-thread-podf7o` b5c0e90e2, be merged to main?  (Its owner sends the question; Code only merges.)
2. GCE: do you approve the names Dial Governance, Plane Governance, Budget Governance, Committee Governance and Charter, GCE as their owner, and DAO's clauses 6 to 8 as marked?  (The Calculus's text; until then #128, #129, #130 wait.)
3. GLP-Spec's exports guard (Finding 1): GLP-Spec's question, not Code's.
4. The OSPM paper's revision for Found: vGLP's question, not Code's.

## Owed

- Audit: Finding 1 (the set above); Finding 4 (GO's Single-Person Company play, above).  Audit PR 27 on udi-shapiro/Grassroots-Audit: once it is merged, Code opens a PR there moving the report of every GLP file it renames or moves, and moves the reports that `python3 -I bin/stale.py <GLP main>` lists.  Its state was not checked at shutdown.
- Calculus: say when Tennis Courts and Rehearsal Rooms are certified; then record both again.
- Code's own nightly (when Udi sleeps, no helper threads): print every record again on the Calculus's tools, run Sections VG and CAS, merge to main.

## How things are run

- Dart 3.13.7 at /home/claude/dart-sdk; PATH needs its `bin`.
- `bash cascade.sh <ident> [--check]`, ident the lower-case name with `-` as `_`; never two at once in one checkout, and never edit cascade.sh while it runs.
- `python3 calculus/tools/adopt.py NAME --out .` prints an owner's `.glp` and the record files; `--check NAME` holds them; delete `calculus/PATHS.txt` after.
- The fast tier, `bash test/run_fast.sh TY VG`, takes over 30 minutes; a background task is capped at 2 hours, so Section CAS whole is split over several worktrees.
- Commits path-limited, never `git add -A` in GLP, no model id in a commit; nothing private (venues, submissions, referee reports, deadlines, tokens) in GLP.
- An owner's file changes only at the owner's request; printing by adopt.py is accepted.

## 2026-11-25 09:50 UTC (Code #37, after its 3rd compaction)

- GLP main 2e669ad: GSC's GSN app test on constant group names; GSN passes (non-root Flutter, /home/fl/glp).
- Waiting: vGLP #32 on how the request form fills a set place no atom binds (issue's new names, a payment's notes, a swap's TotalSet); then Code codes `_request` in glp_multiagent named_widgets.dart, and Currencies #23 mends the GC, GB and village market tests.
- Currencies #23 told yes: drop the Coins and Bonds tab lines (three tabs on note shared one key); on Udi's retrospective list.
- QF at f762939: 7 red, 3 the Mac's font, GSN now green, GC/GB/village waiting on the sets.
