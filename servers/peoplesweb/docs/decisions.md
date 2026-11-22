# Website: record

The record of the project Website, named in GLP `organisation/projects.md` (Coordination #17, 2026-11-20).  A thread reads, at its start and after a compaction, the three rules files of `organisation/`, this file, `README.md` here and the latest `docs/handover-<date>.md`.

## Identity

Project Website: the site peoplesweb.org, this directory, `servers/peoplesweb` of `EShapiro2/GLP`, split from GSA on 2026-11-12 (reported by Coordination #15, 2026-11-12).  It has no paper.  Its threads sign `Website #N`.  The live site is built from GLP main; the test site, peoplesweb.org/next, from the branch Website-site, the one `deploy/test.sh` names.

## Moved from CLAUDE.md, 2026-11-20

- 🔴 A change to the site is shown first on the test site, peoplesweb.org/next, and made live on Udi's approval (reported by GSA #5, 2026-11-12; under /test until 2026-11-17, when Udi said "next is good", reported by Website #3).  A fix of a fault that changes nothing a visitor was meant to see goes to main through Code and is reported.
- 🔴 A merge of the site into main takes Udi's word to Code as well as to this project's thread: Code's permission check takes it as a deploy.  Quote his words in the request to Code.
- 🔴 Only this project's thread pushes Website-site.  What is pushed there is built on the server within a minute, as the user `glp`, before any merge and before anyone has read it (`deploy/next.sh`, Udi's yes of 2026-11-20 20:26 UTC); its build reads the live clone and writes its own directories and nothing else.
- When Udi waits for /next, push, check that it is built, and tell the coordinator, who tells him.
- Udi answers every question in the project chat, not in the threads (Udi, 2026-11-22 14:15 UTC, relayed by the coordinator): put no question to Udi here; send each to the coordinator as the full text ready for him, and go on with the work that does not depend on the answer.
- The server is reached through Code, or through the Remote Control session on Udi's Mac when it is online.
- The site states nothing of its own.  What it says of the Grassroots App Store is specified by GSA's paper, Section 6; what it says of the organisation, the papers, the projects and the directories, is Coordination's record, which `tools/papers.py` and `tools/directories.py` hold for it; a contract's text, relations, schemas and record are read from the repository.  A fault in what the site says of one of these is reported to its owner.
- A platform or a verdict is shown only from a record printed from the contract's current text.
- What is the site's and what is not (GSA #5 to Coordination #15, 2026-11-12 12:37 UTC).  The site's: its pages, its build, the papers' PDFs, the test site and the sweep's scripts, and its live apps, `bin/server.dart`, `tools/gen_harness.py`, `tools/menu_spec.py` and the worlds the build writes; `web/i/index.html` and `web/.well-known`, the page an invitation link opens, the link's form being GNA's Section 6.  GSA's: the contract GAS, the store's agent, and `programs/apps/look.json`, the families' names and colours, which the site reads.
- This repository is public: no token, no password and no detail of the server beyond what `README.md` already has.
