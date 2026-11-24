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
