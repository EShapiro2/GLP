# Handover of Cloud-GAS (Cloud #7, 2026-11-09 13:20 UTC, at its 1st compaction)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).  Cloud #4's handover, the site and its server, stands below it under "The site".

## The queue (Code #26, Cloud-GAS_inbox.md, 2026-11-09 13:02 UTC)

On branch `Cloud-phonehome` off main 32bc81fb (main merged in at 096303c7, nothing of its own yet), each task its own commit, each reported as pushed.  The open-types work, Cloud-vglp2, is set aside: "phones come first" (Udi).

1. **Closing a question**: done on `Cloud-close` at dcb82d7d (pushed): `_answer` in `glp_runtime/lib/vglp/constructs.dart` adds `$name(_, X, Gs, Gs?[, []], done) :- widowed(X) | true.` after the otherwise clause (vGLP 7c631b1, 6.1 and 6.3); home's Nearby cards in `nearby_cards`, `close_card`, `CardClose`.  The checker refuses the clause ("Variable mode mismatch") until Cloud #10's `widowed/1` (GLP-Spec aeed242) lands in the engine and the checker; Code says when.  Then merge main, test (VG, SG) and report.
2. **Composition, not begun in code.**  vGLP's compiler takes one contract or several and of several prints the platform of their composition, the union of their schemas and pragmas, which extends each (vGLP act-schemas-local 1a02877; GSC Section 8); what two inherit from one contract is taken once; a schema, tab or button of one name two write differently is refused (vGLP 5.4); `cascade.sh` takes several contracts for one record.  The plan: `cascade.sh` takes several names and forms one record (its directory name to be chosen and reported, such as `cascade/GSN+GC`); a GLP procedure in `programs/vglp/cva/compiler` composes the `contract_named` terms and the introductions, the union, an identical schema taken once, one name with different text refused; the pragmas through `pragmas.sh` (find_txt at 223, `relation_names` at 245, `visit` building `@order`) as of a contract whose relation line is "extends A and B", with the refusal of a tab or button of one name written differently; tested on GSN and GC, which both extend GKK, since GPE, GPD and GAS do not yet compile alone.
3. **Home printed by the composition** of GPE, GSG, GPD and GAS, run by core's OS on the phones in place of the hand-written home.  Waits on task 2 and on the faults reported to Code at 13:17 UTC: GAS's verdict malformed, `not_in_vars(distrust_store, trusts_store)`, and its Store pragma naming no person in guarding role 2 of open_store (GSA's; GSA #2's d107fb83 on main may have answered it, check); GPD's: the compiler's Arg lacks `item_of(Arg)`, which GSC's reader writes (`programs/gsc/self.glp:69` against `programs/vglp/cva/compiler/self.glp:29`; vGLP's).
4. **GSC's standing line**: `pragmas.sh` reads "Encounter: Alice stands on [Open place] until [Close place]" into `standing(Schema, Predicate)`; GPE.txt has it on main 2ae63194.  Not begun.

Set aside: Cloud-vglp2 (dd91e783, 27044d39, 30dbe7e5, pushed): the compiled types read into every scope, 33 OPEN_TYPES entries cleared, play_refresh typed, 17 cascade records re-made; left are GAI, CSSN, GFF and GSNP's records, the apps' compiled modules re-printed, and APPS.  Cloud-introfix 415f7b3c: the program-compilation test's lexer reads `=::=`.

## The role

GAS: the site at https://peoplesweb.org, its server, and the operating system's side of GRV and GDS (GLP `CLAUDE.md`, "Cloud", 1).  Inbox `Cloud-GAS_inbox.md`, read with `bash bin/mail-read Cloud-GAS "Cloud #N"`; report to Code in `Code_inbox.md`.

## What Cloud #7 did before, on branch Cloud-p3 (merged)

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
