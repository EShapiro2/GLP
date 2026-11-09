# Cloud-Reporter handover, Cloud #6, 2026-11-09

The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud.  Every task and every report is in the mail: Cloud #6's tasks in `Cloud-Reporter_inbox.md`, its reports in `Code_inbox.md`, From: Cloud #6.

## Done and reported

The reporter, the scripted build, the network lines and the lifecycle (Cloud-reporter, Cloud-reporter2); Currencies' Lists (Cloud-currencies ccda3ced); sGLP's check (xviii) (Cloud-sglp18); sGLP's layout (Cloud-sglplayout); the nth/3 test (Cloud-sglpnth); GB's renames (Cloud-currencies 435f59d4); the review of Dan's pull requests #34 to #39, this branch, Cloud-prs, today's main with his stack to #39 merged in.

## Left, in Code's order

1. sGLP's two moves (Cloud-Reporter_inbox.md, 14:54 UTC), branch Cloud-sglptransform off main.  Move 1, length/2 and length/3 into sglp.glp, exported, is plain.  Move 2, the transformation into programs/sglp/transform/, meets two points to raise with sGLP before coding: transform.glp reaches the examples' and the tests' sources by the qualifiers examples# and tests#, and from programs/sglp/transform/ those are siblings, which a qualifier cannot name; and every printed module and every simulation record holds the line "Printed by programs/sglp/transform.glp", which the move changes, so each record's check would fail.
2. GB's old clause numbers (Cloud-Reporter_inbox.md, 15:05 UTC), branch Cloud-gbnumbers off main: the mapping and the places are in that mail.

## Environment

- Flutter with Dart 3.13.7 is at `/home/user/sdk/flutter`; put its `bin` and `bin/cache/dart-sdk/bin` on `PATH`.
- networking's Flutter tests need libsodium: `apt-get install -y libsodium-dev`.  The container has no IPv6: the hole-punch tests, #38's new test, and six tests of #37 that skip themselves need it.
- `flutter test` in `glp_multiagent` needs `bash tool/sync_glp_assets.sh` first; `flutter pub get` rewrites `analysis_options.yaml`, which is not committed.
- `Grassroots-Overview` is a paper repository; the mail is `Grassroots-Project-Overview`.  GNA's paper is `GLP-Networking-API`.
