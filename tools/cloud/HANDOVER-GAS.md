# Handover of Cloud-GAS (Cloud #7, 2026-11-08)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).  Cloud #4's handover, the site and its server, stands below it under "The site".

## The role

GAS: the site at https://peoplesweb.org, its server, and the operating system's side of GRV and GDS (GLP `CLAUDE.md`, "Cloud", 1).  Inbox `Cloud-GAS_inbox.md`, read with `bash bin/mail-read Cloud-GAS "Cloud #N"`; report to Code in `Code_inbox.md`.

## What Cloud #7 did, on branch Cloud-p3

All pushed; Code merges.  Reported to Code at 2026-11-08 17:36 UTC and after.

1. 369e7013: rv_agent answers punch(Key, Address) in reconnection and redemption (GNA 2ce8e10, Section 6).
2. 87135e88: `programs/social/graph/core/links.glp`, the operating system's Invite by link and the opening of a link; the super-app's first act `superapp(Key, Known, Servers)` (GSA 963fcbf, Appendix A, Boot); `link_phone/6`, the play the glue's test runs.
3. 9cb3ca31: `glp_multiagent/lib/superapp_host.dart` sends the servers in the first act.  IGLP's Dart, for Code to route.
4. d3fdf43b, d80a4447: GDS:2 on the app's side (GNA 5040fb6, Section 7): List, List unlisted, Unlist, the supply of day-long links.
5. 999e82ba: home's Introduce card (GKK's pragma Introduction) and Bob's card in GKK's words; the nearby3 play introduces by the button.
6. 68c86849: play_ui_boot.glp's notice type brought to core's (graph_scenario_test).
7. 7217962c: the site's favicon.ico, linked from each page and copied by build.py.
8. c4f4d744: the services in the first act, superapp(Key, Known, Services) (GSA 1d430b6); `programs/social/graph/core/services.json`, the live rendezvous server and no directory yet, synced into the assets and read by SuperAppHost; refusals with their reasons, rejected(no_directory) and rejected(no_rendezvous_server).

Tests: `networking/glue/test/rendezvous_server_test.dart` (5, two phones running core over loopback with the server as a process, and a scripted directory); Section SG's link, directory and Introduce checks; RV.

## Open

- The directory's key goes into `services.json` when Code deploys its agent (servers/directory, 162.35.126.126:9518); until then List is refused, rejected(no_directory).
- A Nearby card stays on screen when its person becomes a friend from the other side, and refresh redraws it: vGLP gives a program no way to withdraw a question.  Reported to Code 2026-11-08 as a vGLP gap, with a recommendation.
- The refused update told the person (GSA a11bdcd): neither GSA nor IGLP says how the refusal reaches the GLP program, or what the app's own version is to it.  Reported to Code 2026-11-08 as a gap, with a recommendation.
- The re-declaration at restart is the runtime's (IGLP 0c77e47), Cloud #10's, not this role's.
- Reconnection (GRV:4) waits on GLP's `reachable(Events)` stream; nothing is built for it.
- Home offers Introduce for every two friends: GKK:3 conditions on Alice knowing both, and Alice cannot know whether her two friends know each other.
- `glp_multiagent/test/superapp_screen_test.dart` does not run in a Linux container: it loads fonts from `/System/Library`.

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH` and `export FLUTTER_ROOT=/home/user/sdk/flutter`.  libsodium is installed, so the glue's tests run.  The papers: GNA is `EShapiro2/GLP-Networking-API`, GSA is `EShapiro2/GSG-Grassroots-Social-Graph`, each added with `add_repo` and cloned at depth 1.

## The site (Cloud #4, 2026-11-08 00:30 UTC)

The site's code is `servers/peoplesweb`; its `README.md` says what is where.  InterServer VPS 162.35.126.126 follows GLP `main` by an hourly sweep (`deploy/sweep.sh`); Udi sweeps at once with `ssh root@162.35.126.126 systemctl start peoplesweb-sweep.service`.  The PDFs in `web/papers/` are built from the paper repositories and go stale; rebuild one with `latexmk -pdf` from its repository.  CSSN, GFF, GAI and GSNP's worlds fail on vGLP's `social_menu` reader fault, vGLP's.  Test the site with `python3 -I servers/peoplesweb/tools/build.py`, serve `build/www`, and render with Playwright.
