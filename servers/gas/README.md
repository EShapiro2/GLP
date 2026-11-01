# GAS, the GLP app store

A site that shows the grassroots contracts of this repository, how the cascade turns each into code, and a live app for each compiled contract: four persons, Alice, Bob, Carol and Dave, each running the contract's canonical platform, in one heap. The owner is GAS; the paper responsible is GAS (https://github.com/nimrodtalmon/grassroots-contracts/blob/main/paper/main.pdf).

## Build and run

From this directory, with Python 3 and Dart:

    python3 -I tools/build.py      # the worlds and the site, from cascade/ and programs/
    dart pub get
    PORT=8080 dart run bin/server.dart

Then open http://localhost:8080/. A contract's world starts on its first visit, which takes about half a minute; at most `MAX_WORLDS` (default 3) run at once, and a world unused for an hour is dropped.

## What is where

- `tools/build.py` reads every contract with a recorded platform, `cascade/<NAME>/platform.vglp`, and writes:
  - `programs/vglp/cva/gas_worlds/live_<name>/`, its live harness (`tools/gen_harness.py`), in the scope of the canonical platforms as vGLP's plays are;
  - `build/www/specs/<name>.json`, the menu the app page draws, read off the platform (`tools/menu_spec.py`);
  - `build/www/stack/<NAME>/`, the cascade's layers, and `build/www/data.js`, every contract's text, owner, the contracts it extends, its schemas and verdict.
  
  Both directories are generated and gitignored, and the script rebuilds them whole. A contract recorded by `cascade.sh` appears on its next run.
- `bin/server.dart` serves `build/www` and the API the app page calls, `/api/<name>/{state,act,day,reset}`. The server is every person: it keeps what each person's dispatcher draws and injects the person's acts.
- `web/` holds the pages: `index.html` (the contracts, their text and the cascade's layers), `app.html` (the live app of one contract, `app.html?c=<name>`), `how.html` (an explainer for general readers), and `about.json` (the site's one line, icon and family for each contract).

## Environment

`PORT` (default 8080), `MAX_WORLDS` (default 3), and, to run from elsewhere, `GLP_ROOT` (the repository), `WORLDS_DIR` and `WEB_DIR`.
