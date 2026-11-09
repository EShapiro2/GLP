# Handover of Cloud-GDS (Cloud #13, 2026-11-09)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).  Cloud #8's handover, which this replaces, is in git at 94622eaa.

## The role

GDS, the Grassroots Directory Service: the directory's agent and its server, `servers/directory`, from GNA Section 7 and the contract `programs/examples/rendezvous/GDS.txt`; and, since Code #27's task of 2026-11-09 16:09 UTC, GNA's network work, which the phones wait on.  Inbox `Cloud-GDS_inbox.md`, read with `bash bin/mail-read Cloud-GDS "Cloud #N"`; report to Code in `Code_inbox.md`.  `servers/directory/README.md` says what is where, how it is built and tested, and how it is deployed.  The app's side of the messages is Cloud-GAS's.  The paper is `EShapiro2/GLP-Networking-API` (GNA), added with `add_repo` and cloned at depth 1; its decisions are `docs/decisions.md`, its Implementation Notes `sections/implementation-notes.tex`.

## Done by Cloud #13 (Code #27's task of 16:09 and 16:19 UTC)

Dan Bachar's #43 (a620a2ac6, F17, decision 40), #44 (71991cb21, F18, decision 41) and #45 (7a9508f8b, F19, decision 42), rebased by him as one stack of 18 commits on main 15eb55a3, each read whole and checked against GNA ffe263d: take all three as they stand.  The report, with the totals of networking's three test sets at each head and what moves with protocol 9, is in `Code_inbox.md`, Cloud #13, 2026-11-09.

Branch `Cloud-net9` is main (e630989, 2026-11-09 17:0x UTC, holding #28 to #42 and Cloud-loopback) with #45's head merged in, no conflict, and this handover, and nothing else: the three leave nothing of decision 42 undone that I found.  Every `GlpNetwork` implementation (the glue's two, the simulation client, `glp_multiagent/lib/debuglog/reporting_network.dart`) and every caller compile with `send` returning the id and `onMessageReceived` taking `senderMessageId`; the kept message records it and reports it again after a restart on both profiles; `dart analyze` reports no error in `glp_runtime`, `networking`, `networking/glue`, `networking/core`, `servers/directory`, `servers/gpw/name_service`, `servers/debuglog` and `tools/live`, and `flutter analyze` none in `glp_multiagent`.

## Open for the role

- Cloud #8's task 3, the home by composition, waiting on GSA's GAS faults, vGLP's `item_of` and GPE's Nearby tab (Cloud #8's reports of 2026-11-09 13:53 and 14:24 UTC).
- `Cloud-close`, waiting on `widowed/1` (GLP's, Cloud #10's 13:22 UTC).
- Protocol 9 is merged only when both servers and both phones move together; the list of what moves is in the report.

## Environment

The container starts with no Flutter: `/home/user/sdk/flutter` is Flutter 3.47.9 with Dart 3.13.7, from `https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.47.9-stable.tar.xz`, unpacked into `/home/user/sdk`; `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH` and `export FLUTTER_ROOT=/home/user/sdk/flutter` in every shell.  libsodium is installed as `libsodium.so.23`; networking's tests load `libsodium.so`, so `ln -sf /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so && ldconfig`.  The container has no IPv6: in `networking`'s `flutter test` the three `hole_punch_service_test` tests and `grassroots_network_underlay_sessions_test`'s 'a dial ... sends no hole punch' fail binding `::1`, and seven tests needing both loopbacks skip themselves (`networking/test/README.md`); Dan reports all pass on his machine.  `flutter analyze` and `flutter test` rewrite `networking/analysis_options.yaml`: restore it with `git checkout --` before a commit.  The package `servers/directory` depends on the glue and so on Flutter: `flutter test` there.  The REPL is `glp_runtime/bin/glpc`.
