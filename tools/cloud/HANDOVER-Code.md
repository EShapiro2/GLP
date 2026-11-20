# Handover of Code #34, 2026-11-20

Code #34 compacted twice and stops here, by the rule of organisation/claude.md (Udi, 2026-11-20).  Its successor reads organisation/claude.md, writing-style-guide.md and claudese.md in full, then GLP/CLAUDE.md and this file, before any work.

## In flight

1. **The text check, tools/myenglish/check.py** (Coordination's request; Udi, 2026-11-20, "yes").  On this branch, not on main.  It passes a first test (a hedge and "final" fail; exit 1).  To do:
   - merge Coordination's a0d3ac16d (section 41, "Wrong and Right", of writing-style-guide.md; it replaces 060169c93);
   - read its four-line blocks, Wrong, Right, Why and Source, and fail the Wrong text or a text close to it (fuzzy match);
   - test it on real failures from the threads; merge it to main;
   - tell the coordinator the path and the command, `python3 -I tools/myenglish/check.py --udi < message.txt`.
2. **Release of Code-gce** (Udi approved, 2026-11-20 11:14 UTC, "yes").  Held while `adopt.py --check` was compared on main against Code-gce: Code-gce gives 63 of 88 records differing, because the records are the Calculus's printing and its const tools differ.  The run on main was not finished.  Finish it; if main gives the same, merge Code-gce to main, close #109 to #113 and #118, and report the commit and the site's sweep.  Keep LWG out, and GM's superseded branch claude/project-thread-7mpm39 (c43fec393).

## Open

- Pull requests: #61 (LWG) waits on Nimrod to merge main and put the three Shareholding Corporation additions into LWGM, as GO said.  #68 waits on Nimrod to merge main and make two mends; close it if he does not answer within a day of 11-19 12:20 UTC.  #94 waits on Yuval's tables, which paths.py does not reproduce.
- Step two of Matching (Promoted Matching, Friends Market): waits on GM's and the Calculus's commits; put them on a branch for /next, and main waits for Udi's yes.
- The nightly reprint of all records on const, then the gate, then const merged to main.
- The hourly check of pull requests (a cron in this session); re-create it in a new session.
- The live site's curl numbers wait on Mac.
- The branch coordination/compaction-rule cannot be deleted from here (403); its commit is on main.
- LWG's lines in servers/peoplesweb/tools/directories.py are Website's to bring to main at Udi's yes.

## Environment

Dart in /home/user/dart-sdk/bin, Flutter 3.47.9 in /home/user/flutter/bin.  Records are printed by rec14.sh's copy, `bash rec-merge.sh <row> <File> <ownerdir>`, the row from calculus/NAMES.txt.  Section NET in a container fails 25 tests of the environment, as on main.
