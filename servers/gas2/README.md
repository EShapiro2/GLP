# GAS2

A site that follows this repository's main branch and shows every grassroots contract in it: how the contracts relate, and each contract taken down the cascade to a certified mini-app of the Grassroots Super-App (`programs/social/graph/core`), with a live run of the mini-app on two super-apps. Live at https://talmonlab.bgu.ac.il/gas2/.

## Run

With Python 3, Git, Dart, and bash with perl (Git for Windows' on Windows):

    python3 -I servers/gas2/server.py

then open http://127.0.0.1:5101/. The server clones the repository's main branch into `DATA` (default `servers/gas2/data`, gitignored), compiles the REPL there, and builds. The first build takes a few minutes. After that it checks the branch every hour, and when the head has moved it builds the new head in a second working copy, switching to it once the build is complete.

Environment: `PORT` (5101), `HOST` (127.0.0.1), `DATA`, `UPSTREAM` (https://github.com/EShapiro2/GLP.git), `BRANCH` (main), `INTERVAL` (3600 seconds), `DART` (dart), `BASH` (found), `GAS2_WORKERS` (2, the REPLs run at once in a build).

## What is where

- `tools/glp.py`: the REPL and the steps of `programs/apps/compile.sh`, which are the contract (`contract_named/2`), GSC's reader (`as_read/2`), the checker (`check/2`), the pragmas (`pragmas.sh`) and the compiler (`compile/5`); then the REPL's `:artefact` and `:boot`.
- `tools/miniapp.py`: a contract's mini-app made from its platform, as the apps of `programs/apps` are made by hand. It holds `platform.vglp`, `miniapp.glp` (GSG's app's file, or GB's for a platform that reads the calendar), `self.glp` (the platform's types written again, then GSG's app's own) and `person.glp` (GSG's app's scripted person with a short script). It also writes a phone module in `programs/social/graph/core` and a boot file, as `gsg_phone.glp` and `gsg_boot.glp` are written, and the three lines `core/self.glp` needs to start each phone. All of these go into the server's working copy only.
- `tools/build.py`: every `programs/**/NAME.txt`, its cascade, its mini-app, one run of it, and the relations among the contracts. A relation's kind is the one the contract's text states (`X restricts Y`). Otherwise it is computed from the two contracts' schemas as GSC's reader reads them, with variables renamed in order: `extends` if every schema of the base is kept, `amends` if some are deleted or replaced (GSC, Definition "Amendment, extension, instantiation"). Its output is `data.json`, `artefacts/*.glpw` and `platforms/*.vglp`.
- `server.py`: the site, `data.json`, the files, `GET api/status`, and `POST api/run/<ID>?which=gas2|udi`, a live run in the serving working copy (GAS2's run, or the hand-written play of `programs/apps/<id>_boot.glp`).
- `web/`: the pages: `index.html` (the map and the list), `contract.html?c=<ID>` and `how.html`.
