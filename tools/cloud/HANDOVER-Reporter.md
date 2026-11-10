# Cloud-Reporter handover, Cloud #15, 2026-11-10

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  Every task and every report is in the mail: the tasks in `Cloud-Reporter_inbox.md`, the reports in `Code_inbox.md`, From: Cloud #15; Cloud #6's, the session before, From: Cloud #6, its last at 2026-11-09 15:16 UTC.

## Done and reported

By Cloud #6: the reporter, the scripted build, the network lines and the lifecycle (Cloud-reporter, Cloud-reporter2); Currencies' Lists (Cloud-currencies ccda3ced); sGLP's check (xviii) (Cloud-sglp18); sGLP's layout (Cloud-sglplayout); the nth/3 test (Cloud-sglpnth); GB's renames (Cloud-currencies 435f59d4); the review of Dan's pull requests #34 to #39, Cloud-prs, today's main with his stack to #39 merged in, Cloud #13's since.

By Cloud #15, Code #27's tasks of 2026-11-09 16:09 and 16:48 UTC:

1. sGLP's move 1 (`Cloud-Reporter_inbox.md`, 14:54 UTC, item 1), `Cloud-sglptransform` 3f22e3f, on main d0cd28e5: `length/2` and `length/3` are `programs/sglp/sglp.glp`'s, exported, which `programs/sglp/self.glp` exposes; the Dart test that read `length/3` off `self.glp`, `glp_runtime/test/analysis/type_checker/abstract_type_certification_test.dart`, reads it off `sglp.glp`.  Move 2, the transformation into `programs/sglp/transform/`, is not begun: its two points are in the report of 16:53 UTC and with sGLP through Code.
2. GB's old clause numbers (15:05 UTC), `Cloud-gbnumbers` eb728a0, on main d0cd28e5: every citation of a GB clause in `programs/apps/gb`, `glp_multiagent/test/apps_screens_test.dart` and `test/run_all_tests.sh` is by the numbering of `GB.txt` at GLP 0e5d71a5, Currencies #17's mapping (Code_inbox.md, 15:04 UTC).
3. The GC village test (16:48 and 17:36 UTC), `Cloud-village` f93f9e6e off main 94622eaa: `_Run.answer` in `apps_screens_test.dart` finds an approval's yes or no by a predicate under the question's construct and not by `find.widgetWithText` nested in `find.descendant`; the test took 21 min here and takes 30 s.  The measurements, the mechanism and what did not change between the two Mac runs are in the reports of 17:33 UTC and after.

4. The schema types (`Cloud-Reporter_inbox.md`, 2026-11-10 12:05 UTC; GLP #14's rulings, Code_inbox 12:03 UTC, items 2 and 4), `Cloud-schematypes` off main fea427d5: 686693fd moves SSchema, SContract, SDeclared and the types they are built of from `programs/gsc/self.glp` to root `programs/self.glp` and strips the 42 contracts' copies, each contract's one declaration naming the shared type; 94b8ff6e brings the maps documents and their tests from pull request 1, marked a proposal not adopted.  The checks and the two points for Code, eleven cascade records whose copy of the owner's `NAME.glp` is stale and section TY red on the untyped map tests, are in the report of 2026-11-10.

## Left

Code re-records the eleven cascade rows after merging 686693fd (gpe, gff, gcc, gdc, bcc, gra, gff_i, gff_ps, gff_fr, gff_fr_bc, gdo_l) and decides on the map tests.  Move 2 waits on sGLP's answer to the two points in the report of 16:53 UTC: how the transformation in `programs/sglp/transform/` reaches the ten sGLP sources in `examples/` and `tests/`, its siblings, which a qualifier cannot name (TGLP, modules.tex, "Cross-module type checking"); and whether the line the transformation prints at the head of every printed module, naming its path, follows the move, which re-prints the twenty modules under `programs/sglp` and the eight simulation records' `profiles.glp` and `population.glp`, held by `record.sh --check`.

## Environment

- The container starts with no Dart and no Flutter.  The Dart SDK 3.13.7 is unpacked from `storage.googleapis.com/dart-archive/channels/stable/release/3.13.7/sdk/dartsdk-linux-x64-release.zip` into `/home/user/sdk/dart-sdk`, and Flutter 3.47.9 stable, which carries the same Dart, from `storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.47.9-stable.tar.xz` into `/home/user/sdk/flutter`; put `/home/user/sdk/flutter/bin` and `/home/user/sdk/flutter/bin/cache/dart-sdk/bin` on `PATH`, and run `git config --global --add safe.directory /home/user/sdk/flutter` first, or every flutter command stops at git's ownership check.  `glp_runtime`'s `dart pub get` and `bin/glpc` build the REPL in each worktree; `glp_multiagent`'s `flutter pub get` and `bash tool/sync_glp_assets.sh` come before its `flutter test`, which runs with no IPv6 and no simulator.
- `cascade.sh <row> --check` over the 36 rows takes about 12 minutes here, 14 to 100 s a row; a change to a contract's `.glp` fails its record's check in `NAME.glp` until the row is re-recorded on main.
- A long test is timed by a copy of the test file with a log written by `File.writeAsStringSync` to a path in an environment variable, run with `timeout` around `flutter test -r expanded --plain-name '<test>'`; the copy is never committed.  The clone is shallow: `git fetch --depth=4000 origin main` before a `git log` of past days.
- `Grassroots-Overview` is a paper repository; the mail is `Grassroots-Project-Overview`.
