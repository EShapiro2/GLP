# Handover of Cloud-GAS (Cloud #4, 2026-11-08 00:30 UTC)

Cloud #4 wrote this on emerging from its first compaction, and stopped.  The next session of the role reads it after its mail (GLP `CLAUDE.md`, "Cloud", 3).

## The role

GAS, the site at https://peoplesweb.org, and the GSA project's large tasks.  Inbox `Cloud-GAS_inbox.md`, read with `bash bin/mail-read Cloud-GAS "Cloud #N"`; report to Code in `Code_inbox.md`.  The site's code is `servers/peoplesweb`; its `README.md` says what is where.

## The server

InterServer VPS 162.35.126.126.  It follows GLP `main`: an hourly sweep (`deploy/sweep.sh`, `peoplesweb-sweep.timer`) pulls, rebuilds and republishes, so a fix reaches the site only once Code has merged it.  Udi sweeps at once with

    ssh root@162.35.126.126 systemctl start peoplesweb-sweep.service

## State

- On `main` (6df3a19a and before): the site, the globe, the live apps (19 of 31 contracts have a compiled platform), the Network and Operating System families.
- Branch `Cloud-graph`, commit 25e89ee5, off `main` bfc9d1d3, sent to Code to merge (Code_inbox, 2026-11-08 00:26 UTC), on Udi's "yout graph is full ot BS and references are missing" and his two follow-ups:
  - relations read from the first line of each contract's box (`GnW amends GSG (...)`), or that sentence in a `%` line where the box has none (DGC); amends drawn barred, implements dashed, instantiates dotted;
  - each contract's paper taken from its `% Owner` line (`tools/papers.py`, keyed by the paper's directory); GSA.pdf and GNA.pdf added to `web/papers/`;
  - GKK lies flat on the pole (Udi: it stood "almost perpendicular");
  - each ring spread, measured as seen from the front, and a ring too full for one row staggered, so no names overlap;
  - the shelves show every contract by family, those with no compiled platform marked Soon (Udi: "make the tables with categories whole as well").
- After the merge: Udi sweeps and checks the site.

## Open

- CSSN, GFF, GAI and GSNP: their worlds and artefacts fail on vGLP's `social_menu` reader fault, on `main`; vGLP's, not this role's.
- The PDFs in `web/papers/` are built from the paper repositories and go stale; rebuild one with `latexmk -pdf` from its repository and commit it here.  GFF's was built from a local copy with `\affiliation{\country{}}`, which Udi said not to fix in GFF's repository.
- Dated relation text in DGC's and GFF-PS's `%` lines: mailed to Currencies and GFF.
- Not this role's: the checker's verdict on GPD, GAS and GRV; sGLP's runs (Cloud-sGLP); G2, the new core (suspended).

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  TeX Live and latexmk installed.  The GLP environment's setup script installs them; peoplesweb.org is an allowed host.  Test the site with `python3 -I servers/peoplesweb/tools/build.py`, serve `build/www`, and render with Playwright.
