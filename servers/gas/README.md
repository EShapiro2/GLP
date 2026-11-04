# GAS, the GLP app store

A site that shows the grassroots contracts of this repository, how the cascade turns each into code, and a live app for each compiled contract: four persons, Alice, Bob, Carol and Dave, each running the contract's canonical platform as an agent of madGLP, in an isolate of its own, the persons' messages carried between the isolates as madGLP's. The owner is GAS; the paper responsible is GAS (https://github.com/nimrodtalmon/grassroots-contracts/blob/main/paper/main.pdf).

## Build and run

With Python 3 and Dart, from any directory:

    servers/gas/run.sh

which is, from this directory:

    python3 -I tools/build.py      # the worlds and the site, from cascade/ and programs/
    dart pub get
    PORT=8080 dart run bin/server.dart

Then open http://localhost:8080/. A contract's world starts on its first visit, which takes about half a minute; at most `MAX_WORLDS` (default 3) run at once, and a world unused for an hour is dropped.

## What is where

- `tools/build.py` reads every contract with a recorded platform, `cascade/<NAME>/platform.vglp`, and writes:
  - `programs/vglp/cva/gas_worlds/live_<name>/`, its live harness (`tools/gen_harness.py`), in the scope of the canonical platforms as vGLP's plays are: its `agent/2` runs `live_person/3`, one person alone, when the host's first act is `me(Person)`, and `live_ui/1`, the four in one heap, when it is `world`;
  - `build/www/specs/<name>.json`, the menu the app page draws, read off the platform (`tools/menu_spec.py`);
  - `build/www/stack/<NAME>/`, the cascade's layers, and `build/www/data.js`, every contract's text, owner, the contracts it extends, its schemas and verdict.
  
  Both directories are generated and gitignored, and the script rebuilds them whole. A contract recorded by `cascade.sh` appears on its next run.
- `bin/server.dart` serves `build/www` and the API the app page calls, `/api/<name>/{state,act,day,reset}`. The server is every person: it keeps what each person's dispatcher draws and injects the person's acts. Each person of a world is an agent of madGLP in its own Dart isolate, told `me(Person)` first and so running `live_person/3` of the harness, with its own heap and key pair; a message the person's platform sends goes out by `send_to_net`, a cold call to the person it names, and the server carries it to that person's isolate, as the simulation's network carries messages between agents. A world is ready when every event sent to its persons, and every message they sent, has been taken. With `GAS_WORLDS=heap` a world is instead the four persons in one heap (`live_ui/1`, the agent told `world` first), their networks joined in GLP.
- `web/` holds the pages: `index.html` (the contracts, their text and the cascade's layers), `app.html` (the live app of one contract, `app.html?c=<name>`), `how.html` (an explainer for general readers), `make.html` (the app maker), `download.html` (the phone and desktop apps), `get.html` and `add.html` (a contract to get, an invite, in the phone app), and `about.json` (the site's one line, icon and family for each contract).
- The app maker: `tools/make.py` takes a contract written as its owner writes it --- its `NAME.txt`, then a line `Schemas` and its speech-act schemas in GSC's notation --- reads the schemas into the term form (`tools/notation.py`; a variable's kind, person, speech act or value, from its use, and a line `% kinds: x act` where the use does not say), and runs the cascade's checker, the pragmas reader and the canonical compiler on it (`cascade.sh`'s steps 1, 3 and 5), then the harness and the menu, into a package of the phone app named by its hash (`phone/tool/packages.py`'s form). The server runs it for `POST /api/make`, one at a time, keeps each package in `MADE_DIR`, serves it at `/made/pkg-<hash>.json`, and runs it live as `<id>_<first 8 hex>`, the name the phone gives it; `GET /api/makebase/<NAME>` gives a recorded contract's text to start from. `notation.py` reads back what `notation_contract/1` prints for every recorded contract, to the same term.

## Environment

`PORT` (default 8080), `HOST` (default 0.0.0.0; 127.0.0.1 behind a proxy), `MAX_WORLDS` (default 3), `GAS_WORLDS` (`mad`, the default, or `heap`), and, to run from elsewhere, `GLP_ROOT` (the repository), `WORLDS_DIR` and `WEB_DIR`.  For the app maker: `MADE_DIR` (default `made/`), `PYTHON` or `PYTHON_PATH_FILE` (a file whose first line names it; default `python3`), `GLPC` (the GLP REPL, default `glp_runtime/bin/glpc`; on Windows a `glp_repl.exe` compiled into `glp_runtime/bin`, which gives the REPL its root) and `BASH` (for `pragmas.sh`; on Windows Git's own is found).

## A server of its own

`deploy/` holds what a Linux server needs to run GAS from a clone of this repository: `gas.service`, the systemd unit, and `Caddyfile`, Caddy in front for TLS and the public name. On a fresh server, with Git, Python 3, Dart (https://dart.dev/get-dart) and Caddy (https://caddyserver.com/docs/install) installed:

    sudo useradd --system --home-dir /var/lib/gas --create-home gas
    sudo git clone --depth 1 https://github.com/EShapiro2/GLP /srv/glp
    sudo chown -R gas:gas /srv/glp
    sudo cp /srv/glp/servers/gas/deploy/gas.service /etc/systemd/system/
    sudo systemctl daemon-reload && sudo systemctl enable --now gas
    sudo cp /srv/glp/servers/gas/deploy/Caddyfile /etc/caddy/Caddyfile   # then put the server's name in it
    sudo systemctl reload caddy

To take this repository's latest contracts and code:

    sudo -u gas git -C /srv/glp pull && sudo systemctl restart gas
