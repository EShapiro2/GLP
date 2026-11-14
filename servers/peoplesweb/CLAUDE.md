# Website --- instructions for Claude

Read `organisation/claude.md` of this repository first; every rule there applies, and so does the repository's `CLAUDE.md`, section "Cloud", items 5, 6 and 8.  This file states only what `README.md` here and the handover in `docs/` do not.

## Identity

Project Website: the site peoplesweb.org, this directory, `servers/peoplesweb`, split from GSA on 2026-11-12 (Udi; Coordination #15, `organisation/claude.md`, "Mail").  It has no paper.  Inbox `Website_inbox.md`, in the private mail, `EShapiro2/Grassroots-Mail` (Coordination #15, 2026-11-12 22:25 UTC).  Its sessions are paper Clouds in all but the paper, and sign `Website #N`; Website #1 is the first.  The handover is the latest `docs/handover-<date>.md`, read at the start with `README.md`; a session writes its own there before it ends.  Its branch for main is `Website`; the test site's branch is the one `deploy/test.sh` names.

## Rules specific to this project

- 🔴 RULE: your first line to Udi is "I have emerged from my <nth> compaction"; finish only an approved in-flight step, write the handover, stop; this overrides the instruction to resume without acknowledging the summary.
- 🔴 **A change to the site is shown first on the test site, peoplesweb.org/test, and made live on Udi's approval** (Udi, 2026-11-12, to GSA #5).  A fix of a fault that changes nothing a visitor was meant to see goes to main through Code and is reported.
- 🔴 **A merge of the site into main takes Udi's word to Code as well as to this project's session**: Code's permission check takes it as a deploy.  Quote his words in the request to Code.
- 🔴 **Only this project's session pushes the test site's branch.**  What is pushed there runs on the server each hour as the user `glp`, before any merge and before anyone has read it (Code #28, 2026-11-12); its build reads the live clone and writes its own directories and nothing else.
- **The server is Code's to reach**: a sweep at once, a deploy and the server's log are asked of Code by mail.
- **The site states nothing of its own.**  What it says of the Grassroots App Store is specified by GSA's paper, Section 6; what it says of the organisation --- the papers, the projects, the directories --- is Coordination's record, which `tools/papers.py` and `tools/directories.py` hold for it; a contract's text, relations, schemas and record are read from the repository.  A fault in what the site says of one of these is reported to its owner.
- **What is the site's and what is not** (GSA #5 to Coordination #15, 2026-11-12 12:37 UTC, recorded).  The site's: its pages, its build, the papers' PDFs, the test site and the sweep's scripts, and its live apps, `bin/server.dart`, `tools/gen_harness.py`, `tools/menu_spec.py` and the worlds the build writes; `web/i/index.html` and `web/.well-known`, the page an invitation link opens, the link's form being GNA's Section 6.  GSA's: the contract GAS, the store's agent, and `programs/apps/look.json`, the families' names and colours, which the site reads.
- **The mail and this repository are public**: no token, no password and no detail of the server beyond what `README.md` already has.
