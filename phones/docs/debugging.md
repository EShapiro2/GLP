# Debugging the Grassroots App on phones: the log service and the harness

Phones #1 Cowork, 2026-11-08.  A proposal, approved in outline by Udi in chat (2026-11-08 15:08--15:12 UTC: a debugging log service at peoplesweb that every debuggable phone streams its log to, and a harness on top); the detail below is for Code to build and for Udi to approve where it decides something.  The record of what runs on the phones is `STATE.md` beside this file; the plays are `../plays/`; the tools that read the logs are `../tools/`.

## Why

The code of the Grassroots App was written to work, and tested in simulations: every play runs under `:boot` with scripted persons, and the suite judges it by its end state.  On a phone there is no scripted person, no end state to read and no terminal: what we have had so far is the device's syslog over a cable (`idevicesyslog`, 16 MB of it in `tmp/phones/syslog11.txt`, most of it the phone's own), Xcode's console, and what the person sees on the screen and relays in chat.  Nothing ran on two phones on 2026-11-08 because the session could not tell what each phone was doing.

So a debuggable build of the app reports everything it does, all the time, to one place that every session can read, and a play on phones is judged from those reports, as a play under `:boot` is judged from its end state.

## The log service

The service is debugging infrastructure, not a grassroots platform and not GLP: it is written in Dart as the other servers are, runs on the peoplesweb VPS beside the store, the rendezvous server and the directory, and the app that is debugged must not depend on it; a release build carries none of it.

1. Where: `servers/debuglog`, a systemd unit on the VPS, behind Caddy at `https://peoplesweb.org/log`; the files under `/var/lib/glp/debuglog/`.

2. What it receives: `POST /log/<phone>/<run>`, a batch of events as JSON lines, with the password the debug build holds and the VPS keeps in `/etc/glp/debuglog.env`; `<phone>` is the agent's public key in hex and `<run>` the launch's identifier (below).  The service stamps each batch with its arrival time, appends it to `<phone>/<run>.jsonl` and answers 204; it never reads the events.  Every event carries the phone's own time in Unix milliseconds, so the arrival stamp shows the skew between the phones' clocks and the server's.

3. What it serves: `GET /log/` lists the phones and their runs with their first and last event; `GET /log/<phone>/<run>` returns the file; `GET /log/<phone>/<run>?follow` streams it as it grows.  Reading takes the same password.  A Mac session reads a run with `curl`, and `tools/follow.sh <phone> <run>` writes it under `tmp/phones/logs/` as it arrives, which is where a Cowork session reads it.

4. Retention: runs are kept thirty days and then dropped by the hourly sweep; the files are never in git.

5. Deployment: `servers/debuglog/deploy/` holds the unit, the Caddy route and the sweep, as `servers/peoplesweb/deploy/` does; the Server session deploys it from main.

## The phone side

1. The debug build: a Dart define, `GLP_DEBUGLOG=<password>`, turns the reporting on at build time; a build without it has no reporter, no password and no traffic to the service.  `tools/build.sh` (below) passes it.

2. The run: at each launch the app draws a run identifier, the launch time in Unix milliseconds, and reports first the build (GLP main's hash, the app's version number from the bundle), the device (its model and operating-system version) and the agent's key.

3. The events, one JSON object a line, each with its time in Unix milliseconds, a source and a kind:
   - `runtime`: every line of `AgentRuntime.onOutput`, `onLog` and `onUserOutput` (`glp_runtime/lib/multiagent/agent_runtime.dart`), which already carry the messages sent and received, the `[INIT]` and `[ERROR]` lines and the person's channel;
   - `layer`: every action the networking layer's store reduces (`AppState`, GNA Implementation Notes, "State") and every callback of the glue, `onPeerDiscovered`, `onPeerLost`, `onPeerConnected` with the attested application or `null`, `onPeerDisconnected`, `onMessageReceived` with the message's size and underlay, `onConnectivityStatusChanged`, `onNetworkChanged` and `onPlaceEvent`, the trust levels as set, and the layer's own diagnostics, every `debugPrint` line of `networking/core`, of the Flutter package `networking/lib` (`[lan]`, `[ble]`, `[local-host]`) and of the glue `networking/glue`;
   - `screen`: each screen the bridge draws, as the text of its thread and the forms and cards it shows (`ui_runtime`), whenever it changes, and each act the person takes on it, the button and the fields as filled;
   - `app`: the app's own state line and every exception it catches (`main_superapp.dart`, `_status`).
   A key in an event is the full hex key; the service's reader shortens it.

4. Buffer and flush: the reporter keeps the events in a file in the app's support directory and posts a batch every two seconds, or at once on an `[ERROR]`, over whatever path the phone has, cellular included; so a Bluetooth test with Wi-Fi off still streams, and a phone with no path at all posts the whole buffer when it has one.  The buffer is kept across the termination of the app and posted at the next launch under the run it belongs to.  The reporter posts over plain HTTPS with `dart:io`, not over the networking layer, which is what is debugged.

5. Cost: the reporter adds no reduction to the agent and no message to the layer; it reads what the callbacks already give.  Its own failures are written to the device's syslog and never to the agent.

## The Mac

A Claude Code session on the Mac, in `/Users/udi/Grassroots/GLP/phones`, does the device work: it builds, installs, starts the follower and runs the play with Udi.  Its tools are in `tools/`:

- `build.sh <udid>`: a debug build signed for the phone, with `GLP_DEBUGLOG` from `/Users/udi/Grassroots/.debuglog-password`, as an `.ipa`; installs it with `ideviceinstaller`, the path that worked on 2026-11-08 where Xcode's and `devicectl`'s installs hung (`tmp/phones/STATE.md`).
- `devices.sh`: `xcrun devicectl list devices` and `devicectl device info apps` for each, so the session checks the phones itself before telling Udi anything.
- `follow.sh <phone> <run>`: the follower, into `tmp/phones/logs/`.
- `check.py <play> <log>...`: the checker, below.

## The harness: plays on phones

A play is a file in `plays/`, `<name>.md`, with two parts.

1. The script: what each person does on each phone, one step a line, numbered, in My English, as `1. Alice: Open place.  2. Bob: Open place.  3. Alice: Befriend Bob in Nearby.  4. Bob: yes.`, and the network the play runs on: Bluetooth with Wi-Fi off, the LAN with Bluetooth off, or the Internet through the rendezvous server.

2. The expectations: for each step, the events that must appear in each phone's log, in order, within a bound: after step 1, Alice's layer sets `pan` and `lan` open; after step 2, each layer discovers the other and `onPeerConnected` fires on both, Alice's agent puts GPE:2 and both know each other, and each screen shows the other in Nearby; after step 3, Bob's screen asks him; after step 4, both agents report `connected`, and the friend channel's handshake on both.

`check.py` reads the logs of a run and prints, step by step, met, failed or not reached, with the first event that failed and the time it took; a play is green when every expectation is met on every phone.  The first plays are Udi's priorities (GSA, `docs/integration.md`, 2026-11-07 22:54 UTC): `p1-two-phones-ble`, `p1-two-phones-lan`, `p1-three-phones`, then `p2-install-invite-gsn`, then `p3-rendezvous-wan`.

A play's expectations are what the papers say happens, GSA's Appendix "The Operating System" and GNA's layer: a failed expectation is a fault either in the code, reported to Code, or in the paper, reported to GSA; the play is never changed to pass.

## The scripted build

Taken up by Phones from Code's proposal to GSA (Code #26, GSA_inbox.md 2026-11-08 15:44 UTC), Udi approving (16:33 UTC).  A second debug build of the app whose person is a scripted person, the one the simulation runs, so that a play on phones runs with no tap and is judged from the logs alone.

1. The person: the scripted persons of `programs/social/graph/core/superapp_plays.glp`, as `encounter_befriend` and `befriend_known`, run by the runtime at the person's channel, as under `:boot` (IGLP, Implementation Notes, "Boot": a scripted person is a program the runtime runs at the person's channel); the agent, core and the layer are the release build's, unchanged.
2. The build: a Dart define, `GLP_PERSON=<mode>`, chooses the scripted person at build time, with the reporter on; a build without it has the person on the screen.  The entry is IGLP's Dart, beside `main_superapp.dart`; `tools/build.sh <udid> <mode>` builds it.
3. The plays: each play of `plays/` names the mode that runs it on each device; a play run with scripted persons is judged by the same expectations as one run by hand, the `screen` events then recording what the scripted person saw and did.
4. Udi's part is to open the app on each device and give it a network; a play run by hand stays for what a script cannot test: the permission prompts, the screens as a person sees them, and the app in the background.

## Who does what

- Code builds `servers/debuglog`, the reporter in the app and `tools/build.sh`, on Phones' tasks in `Code_inbox.md`, `To: Code, for Phones`; the reporter's events are the ones listed above, and the glue's and the runtime's callbacks are where it reads them, with no change to the agent or the layer.
- The Server session deploys the service, the route and the password.
- Phones writes the plays and `check.py`'s expectations, runs the plays with Udi and the Mac session, keeps `STATE.md`, and reports faults.
- Udi: the two phones, Developer Mode on both, a network without client isolation, and the Apple developer account when Apple answers; until then the free team signs one phone per build.

## Order

1. The service and the reporter, with `p1-two-phones-lan` as the first play, on the Mac app and the 11 Pro, which has the app and needs no signing beyond what it has.
2. The 16 Pro's build and install, then `p1-two-phones-ble` and `p1-three-phones` with the Mac as the third.
3. `p2-install-invite-gsn`, then `p3-rendezvous-wan`.

## For Udi's approval

1. The debug build holds a password, and the service accepts a log only with it.  If the password leaks, a stranger can write logs to the service, and the sweep deletes them after thirty days.
2. Thirty days' retention.
3. The Mac session reads the logs with `curl`; no page is built for them.

## For Code, where this proposal does not decide

1. Where the reporter hooks the layer's actions: the store's middleware, or the glue's callbacks, or both.
2. The screen as text: whether the bridge already has a textual form of what it draws (the test harness renders screens for the papers' figures) that the reporter can take.
3. Whether `flutter build ios --debug` with a define, signed by the free team, installs through `ideviceinstaller` as the release build did.
