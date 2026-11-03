# Grassroots Networking --- instructions for Claude

Read `/Grassroots/docs/claude.md` and `/Grassroots/GLP/CLAUDE.md` first; every rule there applies.  GNA owns this directory (Coordination Appendix B), and GNA's paper is its specification: `/Users/udi/Grassroots/GNA`, `main.tex` and `sections/`.

## The paper's appendix holds the rules of the code

The paper's appendix, Implementation Notes (`sections/implementation-notes.tex`), says where the code is and how it is organised (A.1), the rules it is written by (A.2: no legacy or compatibility code, local queueing and no relaying, no social graph in the layer, dual-role BLE, one address per peer, transport independence, the state), what it realizes (A.3), what it decides below the paper (A.4), what deviates or is not yet implemented (A.5), and how it is tested on two iPhones (A.6).  Read it before changing the code.  A rule of the appendix that the code or the paper contradicts is a defect in one of them, reported to GNA through Code and not resolved silently.

## Rules Code keeps here

1. Every Dart or Flutter run goes through the run slots, from the worktree: `bash /Users/udi/Grassroots/tmp/glp-run.sh <label> <command>`, its output to `/Users/udi/Grassroots/tmp/<name>.txt 2>&1`.
2. Commits are path-limited, `git add <files> && git commit -m "<one line>" -- <files>`, never `git add -A`.
3. The suite's Section NET runs this directory's tests: `flutter test` in `networking/` and in `networking/glue`, and `dart test` in `networking/core`; `GLP_SECTIONS="NET" bash test/run_all_tests.sh` from the worktree root.  A new test file goes in one of those three trees.
4. A test waits on the event it tests, a `Completer` the callback completes, with `timeout: Timeout.none`; no clock, no poll and no deadline decides a test.
5. GLP written in a test or a tool here is typed: every procedure declared, no `_` or `_?` in a type or a declaration (`/Grassroots/GLP/docs/typed-glp-manual.md` §18).
