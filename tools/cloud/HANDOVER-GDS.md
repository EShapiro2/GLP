# Handover of Cloud-GDS (Cloud #11, 2026-11-09)

The next session of the role reads this after its mail (GLP `CLAUDE.md`, "Cloud", 3).  Cloud #8's handover of the role, before this one, is in git (302da5a8); its Cloud-reds is done by Cloud #8 (b08ef49e).  My reports are in `Code_inbox.md`, `From: Cloud #11`, 2026-11-09 13:17, 13:24, 13:54 and the last of the day.

## The role

GDS: the directory's agent and server, `servers/directory` (GNA Section 7; `programs/examples/rendezvous/GDS.txt`).  Inbox `Cloud-GDS_inbox.md`, shared with Cloud #8: read only the entries `For Cloud #N` names you.

## In flight: Cloud-vglp2, the open types (Code #26, 13:19 and 14:55 UTC)

Branch `Cloud-vglp2`, main merged at 4b2f241f.  Done: gb's platform re-printed (ea4d4e72); the scripted person's harness of `programs/vglp/cva/self.glp` typed, every cva play and runner with it, 77 entries off `test/OPEN_TYPES` (7605488b); 17 cascade records re-made with their papers' commits (the commit after 7605488b).  At that commit: VG 920 of 921 passed; TY red only on `cascade/GAI/platform.glp` open Drawn, red on main too, its record stopped by GAI.txt's pragma 'Ward and person'; CAS red only on gsnp, cssn, gai, gff, each stopped by its contract; APPS red throughout.

Left: vGLP e1d1b8a, 6.3, answered APPS's cause: the compilation writes the formed types into every module that imports a compiled procedure, the dispatcher, or a procedure whose declaration names a formed type.  Build that in `program_linker.dart` (`_writeCompiledTypes`), delete the hand Drawn and its siblings from `programs/apps/*/self.glp` and the other importers (the apps' owners' files: say so to Code), re-run APPS, VG, TY, CAS, and report.

## Done for Code: Dan's pull requests #40 to #44

Reported in `Code_inbox.md`, the last entry of Cloud #11.

## Environment

Flutter 3.47.9, Dart 3.13.7 at `/home/user/sdk/flutter`.  The papers cascade.sh names are cloned at `/Users/udi/Grassroots/<dir>` (GSC GB GC GCC SGC GRH GFF SRC) so a manifest names its paper's repository and commit; clone them again in a new container (`add_repo` each, read).  libsodium is linked as `libsodium.so`.  networking's flutter tests fail four in a container with no IPv6 (three hole-punch tests and one underlay test binding `::1`).
