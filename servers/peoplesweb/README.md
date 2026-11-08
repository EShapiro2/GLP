# GAS, the GLP app store, at peoplesweb.org

A site that shows the grassroots contracts of this repository, how the cascade turns each into code, and a live app for each compiled contract: four persons, Alice, Bob, Carol and Dave, each running the contract's canonical platform as an agent of madGLP in an isolate of its own, their messages carried between the isolates as the simulation's network carries them.  There are no GAS phones: a person runs a contract on their phone through the Grassroots Super-App (the GSG paper), and this site is the store's window and its live demonstration.

## Build and run

With Python 3 and Dart, from any directory in a clone of this repository:

    servers/peoplesweb/run.sh

which is, from this directory:

    python3 -I tools/build.py      # the worlds and the site, from cascade/ and programs/
    dart pub get
    PORT=8080 dart run bin/server.dart

Then open http://localhost:8080/.  A contract's world starts on its first visit, which takes a minute or two; at most `MAX_WORLDS` (default 3) run at once, and a world unused for an hour is dropped.

## What is where

- `tools/build.py` reads every contract's `NAME.txt` under `programs/` and, for every contract with a recorded platform, `cascade/<NAME>/platform.vglp`, writes:
  - `programs/vglp/cva/web_worlds/live_<name>/`, its live harness (`tools/gen_harness.py`), in the scope of the canonical platforms as vGLP's plays are: the platform's agent and its dispatcher per person, as the apps' `miniapp.glp` runs them; its `agent/2` runs `live_person/3`, one person alone, when the host's first act is `me(Person)`, and `live_ui/1`, the four in one heap, when it is `world`;
  - `build/www/specs/<name>.json`, the menu the app page draws, read off the platform (`tools/menu_spec.py`);
  - `build/www/stack/<NAME>/`, the cascade's layers, and `build/www/data.js`, every contract's text, owner, relation to the contracts it builds on, schemas, verdict and paper (`tools/papers.py`: an arXiv link where the paper is on arXiv, a served PDF or its state where not).

  Both directories are generated and gitignored, and the script rebuilds them whole.  A contract recorded by `cascade.sh` appears on its next run; the hourly sweep (`deploy/sweep.sh`) pulls, rebuilds and republishes.
- `bin/server.dart` serves `build/www` and the API the app page calls, `/api/<name>/{state,act,day,reset}`.  The server is every person: it keeps what each person's dispatcher draws and injects the person's grants.  Each person of a world is an agent of madGLP in its own Dart isolate, told `me(Person)` first, with its own heap and key pair; a message the person's platform sends goes out by `send_to_net`, a cold call to the person it names, and the server carries it to that person's isolate.  With `WORLDS=heap` a world is instead the four persons in one heap (`live_ui/1`, the agent told `world` first), their networks joined in GLP.
- `web/` holds the pages: `index.html` (the contracts on a navigable globe --- GKK at the pole, a ring per depth, a widening arrow extends, a narrowing one restricts, a barred one of one width amends, a dashed one implements and a dotted one instantiates, each relation read from the first line of the contract's box; a balloon per contract with its paper --- and the shelves, every contract by family, and the contract pages), `app.html` (the live app of one contract, `app.html?c=<name>`), `how.html` (an explainer for general readers), and `about.json` (the site's one line, icon and family for each contract).
- `deploy/` holds what the server at peoplesweb.org runs: `peoplesweb.service` (the live server), `Caddyfile` (Caddy serves the static site from `/var/www/peoplesweb.org` and proxies `/api`), and `sweep.sh` with `peoplesweb-sweep.{service,timer}`, the hourly pull-rebuild-republish.

## Environment

`PORT` (default 8080), `HOST` (default 0.0.0.0; 127.0.0.1 behind a proxy), `MAX_WORLDS` (default 3), `WORLDS` (`mad`, the default, or `heap`), and, to run from elsewhere, `GLP_ROOT` (the repository), `WORLDS_DIR` and `WEB_DIR`.

## The server

On the peoplesweb VPS, as root, with Git, Python 3, Dart (https://dart.dev/get-dart) and Caddy installed:

    useradd --system --home-dir /var/lib/glp --create-home glp
    git clone --depth 1 https://github.com/EShapiro2/GLP /srv/glp
    chown -R glp:glp /srv/glp
    cp /srv/glp/servers/peoplesweb/deploy/peoplesweb.service /etc/systemd/system/
    cp /srv/glp/servers/peoplesweb/deploy/peoplesweb-sweep.service /etc/systemd/system/
    cp /srv/glp/servers/peoplesweb/deploy/peoplesweb-sweep.timer /etc/systemd/system/
    mkdir -p /var/www/peoplesweb.org
    systemctl daemon-reload && systemctl enable --now peoplesweb peoplesweb-sweep.timer
    cp /srv/glp/servers/peoplesweb/deploy/Caddyfile /etc/caddy/Caddyfile
    systemctl reload caddy

The sweep runs hourly from then on; to take the repository's latest at once, `systemctl start peoplesweb-sweep.service`.
