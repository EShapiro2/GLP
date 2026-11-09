# Handover of Cloud-GDS (Cloud #8, 2026-11-09)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).

## The role

GDS, the Grassroots Directory Service: the directory's agent and its server, `servers/directory`, from GNA Section 7 and the contract `programs/examples/rendezvous/GDS.txt`.  Inbox `Cloud-GDS_inbox.md`, read with `bash bin/mail-read Cloud-GDS "Cloud #N"`; report to Code in `Code_inbox.md`.  `servers/directory/README.md` says what is where, how it is built and tested, and how it is deployed.  The app's side of the messages is Cloud-GAS's.

## Done

The branches and their commits are in Cloud #8's reports in `Code_inbox.md`: `Cloud-gds2`, `Cloud-lanreplay`, `Cloud-rvstate`, `Cloud-livegds`, and `Cloud-netroam` (1e757d6b, aa4eb7e3: UDP and LAN restarted on a new local network; the stale UDP sweep resets the Noise session; reported 2026-11-09 13:05 UTC, Code's merge pending).  The live run against the VPS is Phones Mac's.

## In flight: Cloud-reds (Code #26, 2026-11-09 09:09 UTC)

The long-standing reds on main, Sections MB and SG, each to its cause and owner.  Branch `Cloud-reds`, off main 32bc81fb.  At 32bc81fb, `GLP_SECTIONS="MB SG"`: 192, 186 passed, 6 failed (MB6's five checks, SG's "super-app no failed play").

**MB6, `programs/tests/multiagent/gkk_maglp_boot.glp`.**

- Cause 1, the code's, fixed on the branch: the boot's person used the platform's stale forms (`menu_w`, the four-argument view).  It now reads `draw(Id, _, request_r(input(P)))`, keeps the session as `sess(Id, P)`, takes the agent's own key from `view(Me, S, K, _, _)`, and proposes `establish(Me)` and `acquaint(Me, Q)`.  With it the boot settles with both agents and their keys.
- Cause 2, open: no `knows` line is reported; alice never receives `called(bob, Kq)`.  The app run alone in the REPL with `call(bob)` sends `msg(bob, conv(K, Kq, ch(...)))`, so the app is sound.  The suspicion was that the boot clause names bob nested, `ma_person(caller(bob), ...)@alice`, where the boot does not resolve a name to its key.  Against it: `MadContext.send` (`glp_runtime/lib/multiagent/mad_context.dart`, about line 1968) resolves the destination by `resolveAgent`, and `SimulationNetwork.agentNamed` returns a registered id as is, so `bob` should reach bob.  Next: trace the send (`:trace`, or the `[MAD ...] send:` lines) to see whether the message leaves alice, with what destination, and whether bob's router takes it; then IGLP's Remark "Agent Names" decides whether the runtime or the boot is at fault.

Run it from `glp_runtime`:

```bash
printf ':boot /home/user/GLP/programs/tests/multiagent/gkk_maglp_boot.glp /home/user/GLP/programs/apps/gkk\n:quit\n' | bin/glpc
```

Expected: "Boot settled: 2 agents", and `[alice] knows(<bob's key>)`, `[bob] knows(<alice's key>)`.

**SG, "super-app no failed play"**: not yet looked at.

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`: `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  The package depends on the glue and so on Flutter: `flutter test` in `servers/directory`, or `dart test` with the Flutter SDK's Dart.  libsodium is installed in the container; networking's own tests load it as `libsodium.so`, which needs `ln -sf /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so && ldconfig`.  `flutter analyze` and `flutter test` rewrite `networking/analysis_options.yaml`: restore it with `git checkout --` before a commit.  The REPL is `glp_runtime/bin/glpc`.
