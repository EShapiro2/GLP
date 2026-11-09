# Handover of Cloud-GDS (Cloud #8, 2026-11-09)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).

## The role

GDS, the Grassroots Directory Service: the directory's agent and its server, `servers/directory`, from GNA Section 7 and the contract `programs/examples/rendezvous/GDS.txt`.  Inbox `Cloud-GDS_inbox.md`, read with `bash bin/mail-read Cloud-GDS "Cloud #N"`; report to Code in `Code_inbox.md`.  `servers/directory/README.md` says what is where, how it is built and tested, and how it is deployed.  The app's side of the messages is Cloud-GAS's.

## Done

The branches and their commits are in Cloud #8's reports in `Code_inbox.md`: `Cloud-gds2`, `Cloud-lanreplay`, `Cloud-rvstate`, `Cloud-livegds`, and `Cloud-netroam` (1e757d6b, aa4eb7e3: UDP and LAN restarted on a new local network; the stale UDP sweep resets the Noise session; reported 2026-11-09 13:05 UTC, Code's merge pending).  The live run against the VPS is Phones Mac's.

## Cloud-reds (Code #26, 2026-11-09 09:09 UTC): done, reported

Branch `Cloud-reds`, off main 32bc81fb.  `GLP_SECTIONS="MB SG"`: 192, 191 passed, 1 failed (SG's "super-app no failed play").

- **MB6**, the code's, fixed in `programs/tests/multiagent/gkk_maglp_boot.glp`: the person used the platform's stale forms (now `draw`/`sess`, the five-argument view, `establish` and `acquaint` with the own key); and, since 63661479 (2026-11-08) the harness sets no trust level at spawn, bob, Closed, held alice's cold call unanswered (IGLP, "Simulation realisation"), so each person now opens the agent's PAN at its start, as the other test boots do.
- **SG "super-app no failed play"**, CSSN's: `:artefact programs/cssn` fails, since CSSN's printed `platform.vglp` has the six-argument `act` the reader no longer takes, and `compile.sh` cannot re-print it, `pragmas.sh` refusing CSSN.txt's `[Child acquaint]` written by two lines (vGLP, Section 5.4).  So `cssn.glpw` is not written and `play_load` and `play_install` of it fail.  The same cause turns Section K red; it is in CSSN's inbox since 2026-11-05.

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`: `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  The package depends on the glue and so on Flutter: `flutter test` in `servers/directory`, or `dart test` with the Flutter SDK's Dart.  libsodium is installed in the container; networking's own tests load it as `libsodium.so`, which needs `ln -sf /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so && ldconfig`.  `flutter analyze` and `flutter test` rewrite `networking/analysis_options.yaml`: restore it with `git checkout --` before a commit.  The REPL is `glp_runtime/bin/glpc`.
