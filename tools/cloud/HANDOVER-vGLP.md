# Cloud, role vGLP: handover

Written by Cloud #9, 2026-11-08.  The arrangement, the repositories, the reading and the mail are GLP's `CLAUDE.md`, section Cloud; the role's inbox is `mail/Cloud-vGLP_inbox.md`, its tasks Code #26's entry of 2026-11-08 15:27 UTC there.  This file points to the commits and the mail and copies neither.

## Environment

- The container had Flutter with Dart 3.13.7 at `/home/user/sdk/flutter`; export `/home/user/sdk/flutter/bin` and `/home/user/sdk/flutter/bin/cache/dart-sdk/bin` in every shell.  `bin/glpc` rebuilds the REPL on the first run after a Dart change.
- Repositories added by `add_repo`: `Grassroots-Project-Overview` (push), `VEUI-Volition-Elicitation-Rationalizing-the-GLP-Smartphone-User-Interface` (vGLP's paper), `Jurix-Formal-Grassroots-Social-Contracts` (GSC's paper).
- Never edit `test/run_all_tests.sh` while it runs: bash reads it as it goes.  Run a copy beside it, `test/.run_copy.sh`, and delete it after.
- A Flutter run rewrites `glp_multiagent/analysis_options.yaml`; restore it and never commit it.
- A program under test whose play must import a compiled module writes the drawn type with `f(_)`, so it is written by the Dart test under `programs/tests/vglp/` for the run and removed after (elicitation_test and bridge_test do so), keeping the tree clear of Section TY.

## The three tasks, branch Cloud-vglp

The commits and the report are in `Code_inbox.md`, `From: Cloud #9`.

1. pragmas.sh and `amends`, and the author and item of a speech act: done.
2. Program positions inside the person's answers, and a Stream only where the person writes: done.
3. Standing requests: see the report; what is built and what waits on a paper is named there.

## Task 4, branch Cloud-vglp2: in flight, does not load

Code #26's entry of 2026-11-08 20:20 UTC in `Cloud-vGLP_inbox.md`: the open types the compilation prints (Section TY's 12 reds), vGLP ac8c281, 6.3.  Branch Cloud-vglp2 off Cloud-vglp, `origin/main` merged (1efba6e).  The work-in-progress commit after it is not for merge: every directory it edits fails to load.

What is in it:

- `program_linker.dart`, `_writeCompiledTypes`, called after `_addVglpModules`: into each module that imports a compiled procedure or the dispatcher it writes the types the imported declarations name, closed over the compiled module's type definitions, replacing a copy of the same name.
- `constructs.dart`: Drawn written precisely, `f(View_f)` in place of `f(_)`.
- The hand copies of those types deleted from the `self.glp` of `programs/tests/vglp/*` and `programs/vglp/cva/*` (vGLP's own).  Other owners' importers (`programs/apps/*`, social/graph/core, currencies, cssn, linker_tree) are untouched, to be listed for Code to route.

The blocker: an importer `self.glp` is also an ancestor of the `.vglp` it imports from, and its scope is checked at discovery (`mergeSelfGlpFileIntoScope`, `module_hierarchy.dart`; the chain loop in `program_linker.dart` building `_vglpScope`), before the compiled module exists, so `undefined type "Ask" in the declaration of inputs/2`.  Three ways out, the first the right one: compile the `.vglp` and write its types before the importer's scope is checked; defer the declaration check of an importer; or delete only the types the compilation forms (the asks, the views, Drawn, Questioned) and keep the source types (6.3: "the types it forms for them").

Also open: the precise Drawn refuses the stream fixture's play granting `stream_string_r(7)`; a play granting a malformed answer needs a term of the type that forms none.  Then: re-print the platforms (`programs/vglp/cva/compile.sh`, `programs/apps/compile.sh`), re-make the cascade records `cascade.sh` can, type `play_refresh/3` in `programs/vglp/cva/social_graph/self.glp`, delete the cleared entries of `test/OPEN_TYPES`, run VG, TY, APPS, CAS and `dart test` (vglp), report.
