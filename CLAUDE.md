# Instructions for Claude Code (GLP)

Read `/Grassroots/docs/claude.md` first; every rule there applies.  This file is Grassroots Integration's and carries what everyone who writes code in this repository needs: the startup sequence and every GLP and Dart rule.  Two read it: Integration Code, whose working directory is `/Users/udi/Grassroots/GLP`, and the subagent it spawns for a task, whose working directory is the `/Users/udi/Grassroots/GLP-worktrees/<project>` its brief names.

## Read at the start, and nothing more

Integration Code, at session start:

1. `/Grassroots/docs/claude.md`.
2. `/Grassroots/Integration/CLAUDE.md`.
3. This file.
4. `to_all_inbox.md`, then `Integration-Code_inbox.md` from its last receipt forward --- and again between tasks, being the one worker that does not wait to be told (`claude.md`, "Mail").

A subagent, at the start of its task: the same 1 to 3 with the owning project's `CLAUDE.md` at 2, then the sections of the owning paper its task names.  It reads no inbox and leaves no receipt, having none.

On demand, named by the task: `docs/DISCIPLINE.md` (development discipline), `docs/typed-glp-manual.md` (typed GLP), `docs/glp-cheat-sheet.md` ("GLP is NOT Prolog"), Coordination Appendix B (ownership), the reference specifications listed at the end.

## Ownership

Ownership is Coordination Appendix B, `/Grassroots/Coordination/sections/B-code-map.tex`, the only copy: one owner per directory under `programs/` and per Dart subsystem, ownership following the layer — GLP-Spec the language, IGLP all Dart, GSG the operating system, an application its own GLP.  Confirm you own a unit before editing it.  A change to code you do not own is a request to Integration.  Implementation decisions go in the owning paper's Implementation Notes appendix, not in a separate spec document.

## Branches, worktrees and merges

🔴 **A task runs in its owner's worktree, on its owner's branch, carried out by a subagent of Integration Code** (Udi, 2026-09-17).  `claude.md` "Sessions" and "Code" define the arrangement; this section is how it is worked in this repository.  You are that subagent if your brief named a worktree; you are Integration Code if it did not.

- The clone at `/Users/udi/Grassroots/GLP` stays on `main` and is Integration Code's alone.  No subagent edits, commits or runs in it.
- One worktree per project that owns code, made once by Integration Code: `git -C /Users/udi/Grassroots/GLP worktree add /Users/udi/Grassroots/GLP-worktrees/<project> -b <project>`.  A subagent works only in the one its brief names: `cd /Users/udi/Grassroots/GLP-worktrees/<project>`.
- 🔴 **The subagent reads the owning paper named in its task and codes from it.**  Integration Code does not summarise the paper for it and does not specify: the owner's Cowork wrote the task from the paper, and the paper is the specification.
- 🔴 **It edits only what its owner owns** (Coordination Appendix B), commits path-limited on the branch --- `git add <files> && git commit -m "<message>" -- <files>` --- and never `git add -A`.  Single-line commit messages.  `git merge main` first, so the branch carries the current gate.
- 🔴 **It runs the tests its task names, and not the full suite.**  The full suite is run only by Integration Code, on `main`, one run at a time: two suites at once contend on the Dart build lock and neither is a gate.
- 🔴 **A question or a paper fault ends the subagent.**  It reports and stops; Integration Code posts the question in the owner's inbox and spawns again when the answer is there.  A workaround in code is never the answer to a fault in the paper.
- When the branch is ready, Integration Code merges it into `main`, runs the full suite there, pushes, and answers the owner in its inbox.  A merge that fails the suite is not pushed.  On a merge conflict, STOP and report.
- An owner may add its own test block to `test/run_all_tests.sh` on its branch (Udi, 2026-09-16).  The harness machinery is Integration's and changes only by request to it: the gates, Section Q, `KNOWN_RED`, the runner guards and the asset step.  Two owners adding blocks to that file will conflict, so merge `main` into your branch before you write one.
- Never `git reset`, `git revert`, `git restore` or `git checkout -- <file>` on another session's work; never rewrite history on `main`.
- Generated files are gitignored and rebuilt by their script, never committed.

## The tree

Reorganised 2026-09-16 (Udi).  `README.md` carries the directory map; what moved, and where a Code session must now look:

| Was | Is |
|---|---|
| `programs/spm/` | `programs/social/spm/` |
| `programs/currencies/` (the program) | `programs/currencies/bonds_v2/` |
| `programs/coins/`, `programs/bonds/`, `programs/sovereign/` | `programs/currencies/coins/`, `.../bonds/`, `.../sovereign/` |
| `programs/multiagent_tests/`, `programs/vglp_tests/` | `programs/tests/multiagent/`, `programs/tests/vglp/` |
| `programs/social/linkprobe*/`, `probeapp/`, `runprobe/` and their boot files | `programs/tests/linkprobes/` |
| `glp_runtime/test/programs/*` | `programs/tests/*` |
| `glp_runtime/test/module/files/*.glp` | `programs/tests/module/` |

`programs/social/`, `programs/currencies/` and `programs/tests/` are containers with no `self.glp`, so they add nothing to the ancestor scope chain.  `programs/archive/`, `programs/old-archive/`, `archive/`, `test/archive/`, `programs/plays/` and `programs/exercises/` were deleted in the same series: there is no archive in this repository, and nothing is to be archived into one.

## Worktrees

| Project | Worktree | Branch |
|---|---|---|
| IGLP | `/Users/udi/Grassroots/GLP-worktrees/IGLP` | `IGLP` |
| GSG | `/Users/udi/Grassroots/GLP-worktrees/GSG` | `GSG` |
| Currencies | `/Users/udi/Grassroots/GLP-worktrees/Currencies` | `Currencies` |
| vGLP | `/Users/udi/Grassroots/GLP-worktrees/vGLP` | `vGLP` |
| GLP-Networking-API | `/Users/udi/Grassroots/GLP-worktrees/GLP-Networking-API` | `GLP-Networking-API` |
| GFWC | `/Users/udi/Grassroots/GLP-worktrees/GFWC` | `GFWC` |
| Legal | `/Users/udi/Grassroots/GLP-worktrees/Legal` | `Legal` |
| GLP-Spec | `/Users/udi/Grassroots/GLP-worktrees/GLP-Spec` | `GLP-Spec` |
| IGLP, the harness path fix alone (Udi, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-harness` | `IGLP-harness` |
| IGLP, the default-display rule alone (Udi, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-display` | `IGLP-display` |
| Currencies, the twelve `bonds_v2/mad_boot` repairs alone (Udi, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/Currencies-bonds-boot` | `Currencies-bonds-boot` |
| IGLP, the two checker faults alone (IGLP, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-checker` | `IGLP-checker` |
| IGLP, `main_sovereign.dart` boots the harness' person (IGLP, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-sovereign-boot` | `IGLP-sovereign-boot` |
| IGLP, the `signature/2` kernel and probe (IGLP, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-signature` | `IGLP-signature` |
| IGLP, its part of the `Key` mirror sweep (IGLP, 2026-09-18) | `/Users/udi/Grassroots/GLP-worktrees/IGLP-key` | `IGLP-key` |

A project not listed has no worktree yet and asks Integration for one.

## Operating GLP

### REPL

The REPL is the only tool: loading a `.glp` file runs the full pipeline (SRSW → PE → type-check → compile → execute).  **Always invoke** `bin/glpc` from `glp_runtime/` in your worktree: it runs the AOT-compiled REPL (~0.3 s startup) and rebuilds it whenever any `lib/` or `bin/` Dart source changes.  🔴 Do not use `dart run bin/glp_repl.dart` for routine checks; it is the slow path, for debugging `glpc` itself.

Non-interactive use, no approval prompt needed (no heredoc, which needs approval per invocation):

```bash
cd <worktree>/glp_runtime && printf 'load ../programs/path/to/file.glp\ngoal.\n:quit\n' | bin/glpc
```

Loading a project: enter the directory path at the prompt; the project linker resolves all `M # goal(...)` cross-module calls at compile time.  REPL commands: `:quit`, `:trace`, `:debug`, `:limit N`, `:activate <module>`, `:emit <dir>`.

### Test suites

| Suite | Command (from the worktree root) | Tests |
|---|---|---|
| Full suite (canonical) | `bash test/run_all_tests.sh` | 2071, all green, `KNOWN_RED` empty — at GLP `fa9c45eb`, 2026-09-18 |
| Dart unit tests alone | `cd glp_runtime && dart test` | glp_runtime only |
| Flutter package alone | `cd glp_multiagent && flutter test` | glp_multiagent only |

`run_all_tests.sh` covers the whole Dart tree (Section Q), so it is the canonical gate; the package runs are for a faster loop, not for coverage.  🔴 **Section Q gates on the known-red list, not on all-green**: a test named in `KNOWN_RED` may be red without failing the suite, any other red fails it, and a listed test that starts passing also fails it.  When you fix a listed test, delete its entry in the same commit.  A count is quotable only with the commit it was taken at.

Section SG's warm call failed intermittently from 2026-09-15 to 2026-09-18 on `anchor_friend/4`, whose first clause passed an unbound tail; a rule gave it one re-run, and the rule went with the fix at `11625f89`.

Redirect output to `/Users/udi/Grassroots/tmp/<name>.txt 2>&1` and read the file; never `/tmp/`.  🔴 **Never run `run_all_tests.sh` concurrently with `dart test` or `flutter test`** — Section Q invokes both, and a parallel run contends on the Dart build lock and silently aborts the suite.

### Baseline before commit

Before changing the runtime, types, root `self.glp` or any cross-cutting code: run the full suite on your branch and record the green count and the known-red lines; make the change; re-run.  The commit gate is the green count plus the known red set unchanged — add no new failures.  A red test you did not cause is baselined and reported, added to `KNOWN_RED` with its owner only if it is another project's to fix.  For a change confined to a single play, test or program the baseline may be skipped at your judgment.

### Tests

When you fix a bug, add a test that exercises the fix; when you add a feature, add tests that cover its main cases.  Tests are never removed.  `test/run_all_tests.sh` has sections A (typed runtime tests) onward; Section A uses heredoc-based REPL sessions, with `POSITIVE_FILES` for positive-typecheck-only files and `NEGATIVE_FILES` for negative ones.

## Archiving

🔴 **Live code never points to archived code (Udi, 2026-08-03).**  No file the suite loads, program another program calls, or path a live script or Dart source names may resolve into an archive.  Repointing a live reference at an archived path is the violation, not the remedy.  Before archiving anything, grep for every reference to it and count what breaks.

The repository holds no archive: `programs/archive/`, `programs/old-archive/`, `archive/` and `test/archive/` were deleted on 2026-09-16, and nothing superseded is kept in the tree — git history is where it lives.  The rule above governs any archive a later decision creates.

## Spec-first development

🔴 **No implementation without a spec.**  The spec is the owning paper.  Identify which paper covers the area you are touching and quote the section; if it is clear, implement to match without asking; if it is unclear or absent, STOP, report to the paper's Cowork session through Integration, and wait.  The code is never the source of truth when the paper is unclear.  When quoting a spec, quote exactly; if the spec is silent, say so.

## Bug protocol — no workarounds

When you hit a bug or unexpected behaviour: STOP; check the spec; report in this form, with no intervening prose — **Failing goal** (the goal that fails), **Type and procedure declarations** (the relevant type definitions and the procedure declaration), **Suspected clause(s)** — and wait.  A workaround is special-casing to dodge the bug, restructuring to route around it, commenting out or marking a failing test expected to fail, or adding checks for cases the spec does not address.

## Language design authority

🔴 The GLP language definition — guards, system predicates, body kernels, directives, type-system features, primitive types — is GLP-Spec's and is not revised, extended or added to without Udi's express approval, whichever project proposes it.  See `docs/DISCIPLINE.md` §1.14.

## GLP code in papers

🔴 **Never include in a paper GLP code that has not been typechecked and run.**  Papers carry GLP programs in the body as exposition and have no code appendix; each points to a `programs/` directory that is a superset of what it presents.  All `.glp` and `.vglp` code lives in `programs/`; paper repos hold no copies.  🔴 Never program from ignorance of GLP and its type system: read the manual and cheat sheet, and if they do not answer, STOP and state the gap.

## Code modification protocol

`.glp` files Udi wrote are never modified without discussion and explicit approval; files you created in this session may be modified freely.  Dart files you own you may modify, stating what and why.  Do not combine an agreed change with an undiscussed structural change; never make silent improvements to surrounding code.  Discussion mode and implementation mode are as in root `claude.md`: no code change, test run or git operation until Udi says to act; if he says "stop", halt at once.

## Flutter `glp_multiagent`

🔴 `glp_multiagent/assets/glp/` is generated and gitignored: `bash tool/sync_glp_assets.sh` from `glp_multiagent/` rebuilds it from `programs/`, and it runs before any build.  Never commit the tree and never edit a file in it — edit the source under `programs/` and re-run the script.  The suite runs it for you: Section Q generates the bundle before it runs, because without it `flutter test` dies building the asset bundle, the 61 `glp_multiagent` tests do not run, and before 2026-09-16 the suite still reported green.

When modifying `glp_runtime` code that affects the Flutter app: `cd <worktree>/glp_multiagent && pkill -f "glp_multiagent" 2>/dev/null; flutter clean && flutter pub get && flutter build macos`.  `flutter clean` is required.  App log: `/Users/udi/Grassroots/tmp/glp_multiagent_trace.log`, cleared before each run.  iOS runs on the simulator; a physical phone is not needed (Udi, 2026-09-15).

## Environment

Dart binary `/opt/homebrew/bin/dart`.  Root `self.glp` is `programs/self.glp`, the ancestor scope of every program.  Prefer absolute paths; keep the working directory across commands.

## Reference specifications (on demand)

`docs/glp-bytecode-v216-complete.md` (instruction set), `docs/glp-runtime-spec.txt`, `docs/guards-reference.md`, `docs/body-kernels-reference.md`, `docs/glp-compiler-spec.md`, `docs/glp-arithmetic-spec.md`, `docs/glp-io-spec.md`, `docs/parser-spec.md`, `docs/naming-conventions.md`, `docs/mutual-ref-spec.md`, `docs/glp-predicate-taxonomy.md`, `docs/known-issues.md`, `docs/Mandatory protocol for debugging the GLP implementation with GLP programs.txt`, `/Grassroots/CSSN/docs/cssn-glp-implementation-spec.md`.  A spec that a paper has absorbed becomes a pointer to the paper or is deleted.
