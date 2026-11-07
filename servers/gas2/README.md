# GAS2

Two parts. The person's app (`app/`, over `device/`) is the Grassroots Super-App of `programs/social/graph/core` on a phone or a computer, with mini-apps made from the contracts; see `device/README.md`. The site, below, follows this repository's main branch and shows every grassroots contract in it: how the contracts relate, and each contract taken down the cascade to a certified mini-app of the Grassroots Super-App (`programs/social/graph/core`), with a live run of the mini-app on two super-apps. Live at https://talmonlab.bgu.ac.il/gas2/.

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

## The app

- `glp/gas2_device.glp`: the device harness, the super-app for a live person (`device/README.md`).
- `tools/device_tree.py`: the part of a built working copy a device runs, with GAS2's mini-apps and the harness.
- `device/`: the person's super-app and its page (Dart; also a program for computers, `bin/gas2.dart`).
- `app/`: the Flutter shell for Android, Windows and macOS (`tool/sync_assets.sh <tree>` before a build).
- `.github/workflows/gas2-apps.yml` (at the repository's root): builds the tree and the apps for Android, Windows, macOS and Linux on every push to this branch, onto the release `gas2-latest`, which `web/get.html` links to.

## The app maker

`web/maker.html`: a person writes a contract, its text (NAME.txt) and its act schemas
(NAME.glp), starting from one of GLP's.  `POST /api/make` queues it; `tools/make.py`
takes it down build.py's cascade in the live working copy (one job at a time, under the
lock a build takes to reset its slot) and certifies the mini-app; `GET /api/made/<key>`
says how far it got and why it stopped, and `/made/<key>.json` is the mini-app's
directory, which a device writes under `programs/apps/` and certifies under its
person's key (`device/lib/host.dart`, `addApp`).  The link to share is
`maker.html?k=<key>`; the device's page takes it under Mini-apps.

## On the web

`device/bin/web.dart`, talmonlab's app gas2web (port 5201): the device's page for
those with no app (iPhones), each person's super-app run on the server over the relay,
from the device tree of the site's live build (each build writes it, `out/device`).  The
server holds the person's key, and the page says so.
