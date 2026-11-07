# GAS2 on a device

The person's Grassroots Super-App: one agent of GLP's runtime running `programs/social/graph/core` with GAS2's device harness (`../glp/gas2_device.glp`), over talmonlab's relay, and the page the person uses, served on the loopback. The Flutter app (`../app`) wraps this package in a web view for Android, Windows and macOS. On a computer it also runs as a plain program (`bin/gas2.dart`).

## What is where

- `lib/host.dart`, `SuperHost`: certifies the mini-apps the tree carries under the person's key (the REPL's `:artefact`, done by the engine here), starts the agent, sends the first act `gas2_device(Key)`, and turns the person's acts into the harness's grants. It keeps what the harness shows, in the shape the page reads.
- `lib/page_server.dart`: `GET api/state` (long poll with `?since=<version>`), `POST api/act`, `POST api/add` (a person met from afar, by their key), and the page's files.
- `lib/relay_network.dart`, `lib/offline_network.dart`: GAS's relay network, copied from `servers/gas/core`. It is used relay-only: no Bluetooth or local network yet.
- `web/index.html`: the page, which has the person's key, people, mini-apps, and a mini-app's state, its acts and the questions it asks.
- `bin/gas2.dart`: the program for a computer.
- `test/two_devices.py`: two devices driven through their API, from trust to a mini-app's acts.

## The harness and the live person

The super-app's own entry for a person, `superapp/3`, installs a mini-app with its person channel held (`home.vglp`, `install_act/6`, `hold_person/1`), so the person never sees it. GAS2's harness shows it instead. The super-app holds that channel at `Opaque`, and typed GLP lets nothing outside the mini-app write the person's input into it. So each of GAS2's mini-apps exports a live person of its own: the role `live` of the root's second template, `miniapp_person/5`, which the super-app runs at the mini-app's own types (run/3 at the type identity, as the plays run their scripted persons). It shows the person every draw, `drawn(D)`. It reads the person's grants from the template's list of keys, as strings the device writes: `"g"`, the construct's id, its path, `"/"`, then the value by constructor name, with integers as digits and `"."` (`../tools/miniapp.py`, `live_glp`, which also writes each mini-app's `shapes.json`, the shapes of its acts for the page's forms).

The harness reads the person's acts as grants of the super-app's person channel, `Input(HomeDrawn)`, as `core/self.glp` types them; its header lists them. It answers a friend request answered in advance, and holds an invitation to someone not yet a friend until they are one.

## Run two on one computer

    cd talmonlab/server/gasrelay && DATA=/tmp/relay dart run bin/relay.dart 4611
    python3 -I ../tools/build.py <GLP clone> /tmp/out
    python3 -I ../tools/device_tree.py <GLP clone> /tmp/tree /tmp/out/data.json
    dart run bin/gas2.dart --tree /tmp/tree/programs --data /tmp/a --port 8301 --relay http://127.0.0.1:4611
    dart run bin/gas2.dart --tree /tmp/tree/programs --data /tmp/b --port 8302 --relay http://127.0.0.1:4611
    python3 test/two_devices.py gas2_gsn

Each device runs from its own copy of the tree (`<data>/tree`), made afresh at each start. The agent keeps no state across restarts yet.
