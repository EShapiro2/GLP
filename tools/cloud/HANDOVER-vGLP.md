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
