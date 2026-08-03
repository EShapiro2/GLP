# Retired test scripts

Nothing here is run, and nothing live points at anything here.

## run_book_tests.sh — retired 2026-08-03

It is the standing case of the rule added to `GLP/CLAUDE.md` the same day: live
code never points to archived code.  The script's whole purpose was to compile
the book corpus under `programs/archive/book`, so it could not be repointed when
that became `programs/old-archive/book` — repointing a live reference at an
archived path is the violation, not the remedy.  Either the corpus comes back
live or the script goes; the corpus is a deliberate snapshot of 2026-03-07 kept
in its unparameterised forms (`docs/known-issues.md`), so the script goes.

It was already dead in fact.  Line 8 aside, its 141 file paths are
`/home/user/GLP/...` absolutes from another machine and have never resolved
here, so the "141 files" it claimed to compile was a figure nobody had seen the
script produce.  The book examples that are live are compiled by
`test/run_all_tests.sh`, which loads them in sections A2, A3, A4, A6, A9 and Y1.

Its references in `README.md`, `CLAUDE.md`, `test/README.md` and
`docs/DISCIPLINE.md` went in the same commit, as the rule requires.
