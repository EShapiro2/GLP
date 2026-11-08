# Handover of Cloud-GDS (Cloud #8, 2026-11-08)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).

## The role

GDS, the Grassroots Directory Service: the directory's agent and its server, `servers/directory`, from GNA Section 7 and the contract `programs/examples/rendezvous/GDS.txt`.  Inbox `Cloud-GDS_inbox.md`, read with `bash bin/mail-read Cloud-GDS "Cloud #N"`; report to Code in `Code_inbox.md`.  `servers/directory/README.md` says what is where, how it is built and tested, and how it is deployed.  The app's side of the messages is Cloud-GAS's.

## State

The branches and their commits are in Cloud #8's reports in `Code_inbox.md`.  Every message of GNA Section 7 is built, the answers to unlist and to a re-listing among them (GNA main 5040fb6).  The agent keeps its local state across a restart in the host's state file (IGLP, Definition Restart), and takes the runtime's `refresh` at a restart.  Nothing is open.

## Environment

Flutter 3.47.9 with Dart 3.13.7 at `/home/user/sdk/flutter`: `export PATH=/home/user/sdk/flutter/bin:/home/user/sdk/flutter/bin/cache/dart-sdk/bin:$PATH`.  The package depends on the glue and so on Flutter: `flutter test` in `servers/directory`, or `dart test` with the Flutter SDK's Dart.  libsodium is installed in the container; networking's own tests load it as `libsodium.so`, which needs `ln -sf /usr/lib/x86_64-linux-gnu/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so && ldconfig`.  The REPL is `glp_runtime/bin/glpc`.
