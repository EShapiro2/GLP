# Handover of Cloud-GDS (Cloud #8, 2026-11-08)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).

## The role

GDS, the Grassroots Directory Service: the directory's agent and its server, `servers/directory`, from GNA Section 7 and the contract `programs/examples/rendezvous/GDS.txt`.  Inbox `Cloud-GDS_inbox.md`, read with `bash bin/mail-read Cloud-GDS "Cloud #N"`; report to Code in `Code_inbox.md`.  Branch `Cloud-gds`.  `servers/directory/README.md` says what is where, how it is built and tested, and how it is deployed.  The app's side of the messages is Cloud-GAS's.

## State

The branch's commits are in Cloud #8's reports in `Code_inbox.md`, which name them.  Waiting on GNA, through Code (Cloud #8 to Code, 2026-11-08 15:50 UTC):

1. The answer to `unlist(A, Signed, Reply)`.  Until GNA names it, `gds_agent.glp` holds the Reply unassigned in its pending list, as `held(Reply)`, and `DirUnlistReply` stands as a placeholder type.  Once answered: assign it in `admit_unlist`, drop `held`, and test the answer in `servers/directory/test/agent_test.dart`.
2. A list from A for an address the directory already holds verified for A, which is how an app reconnects after a restart of the directory.  Until GNA decides, such a list is answered as any other, by a mail, and the buffer is asked for again once that mail's link is opened.

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`: `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  The package depends on the glue and so on Flutter: `flutter test` in `servers/directory`, or `dart test` with the Flutter SDK's Dart.  libsodium is installed in the container.  The REPL is `glp_runtime/bin/glpc`.
