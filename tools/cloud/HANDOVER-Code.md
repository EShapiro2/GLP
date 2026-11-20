# Handover of Code #35, 2026-11-20

Code #35 compacted once and stops here, by the rule of organisation/claude.md, "Sessions" (Udi, 2026-11-20).  Its successor first reads, in full, organisation/claude.md, writing-style-guide.md and claudese.md, then GLP/CLAUDE.md and this file.  Only after that does it start work.

## Done

- The text check is on main (5a90a437c): `python3 -I tools/myenglish/check.py --udi < message.txt`, or `--clause` or `--paper FILE`.  It exits 1 on a failure, and it reads section 41 of the style guide with a fuzzy match.  The coordinator has the command.
- The five GCE contracts (Nonprofit, Partnership, Foundation, Disputes and Warnings) are on main (ba7dc9279).  They were printed with main's calculus/tools, and pull requests #109 to #113 are closed.
- organisation/project-instructions.md on main has three of Coordination's commits: 2f4382e3c (the text check), d88188457 (a thread's own helpers) and 0e43410ec (Fable for deep discussion).  GLP/CLAUDE.md follows them (b0c220c6d).  Four other commits wait for Udi: 76ac4d949, 2e0ead985, 0462ce6af and 5b1d43b29.

## In flight, on branch claude/project-thread-llohtv (this branch, not main)

- 50282582f: main plus GO's 49b9e3658, which revises Networked Organisation, Rides, Ads, Lodging, Marketplace and Gigs with Cur.
- 7419f2655: the renames GCC to Community and GBCC to Backed Community, which Udi approved.  The commit covers the files, NAMES, the citations and the records.  It does not change the lower-case words "community currency" in the clauses, because Udi has not yet approved that wording.  The platform.vglp, platform.glp and interface.glp files of Community and Backed-Community still carry the old names; Dart prints them.
- 3732581b0: Calculus #10's nameslines.py line, so that a kind opening with a possessive takes no article ("an its" is gone).  The six full contracts were printed again, and `adopt.py --check` passes for all six.
- Main waits for Udi's yes on all of this, and for his approval of GO's clause wording.  Udi told GO: "show me again proposed correction and do nothing till i answer".

## Waiting

- Calculus #10's checked commit: Ads and Rides mended (982b264d), the renames in the Calculus's own files (0480ae74), and the verdict stamped with the hash of its term, which adopt.py checks.  When it arrives: merge it on this branch, print the records with fresh verdicts, run the check, and give Website the commit for /next.  Today Ads and Rides on /next show no verdict, and the six stop at "checked".
- GO #13: its new text goes over 7419f2655, but only after Udi approves the clauses.
- Udi's rule: when he wants to review something on /next, Website runs the sweep at once and tells him.
- The nightly, when Udi sleeps, with no helper threads:
  1. reprint every record on the Calculus's tools (Code-gce / Calculus-next17, with Calculus 0857cb81);
  2. run the gate `GLP_SECTIONS="VG CAS" bash test/run_all_tests.sh`;
  3. merge to main;
  4. close #118;
  5. print the platforms of the renamed and revised contracts.
  Code-gce differs from main in 63 of 88 records; main itself differs in 22.

## Pull requests (answered on GitHub)

- Waiting on the Calculus: #61, #125, #128, #129 (and Nimrod), #130, #131, #132 and #133.
- #123 and #124: take them, with root and distance moved to programs/lib as typed procedures.
- #127: waits on GLP-Spec and Udi.
- #118: closes after the nightly.
- #68: close it if Nimrod has not answered by 11-20 12:20 UTC.
- #94: waits on Yuval's tables.
- The hourly PR check was a cron of this session; a new session creates it again.

## Never

- Merge claude/project-thread-7mpm39 (c43fec393).
- Put LWG on main or on the live site.
- Merge what rests on clauses Udi has not approved.
- `git add -A` in GLP.
- Put a model id in a commit.

## Environment

- Dart 3.13.7 is at /home/claude/dart-sdk.
- Worktrees in /home/claude: GLP (main), wt-gce (Code-gce), wt-go, wt-rel, wt-ren (this branch).
- The site builds with `cd servers/peoplesweb && python3 -I tools/build.py`.
