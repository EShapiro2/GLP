# Instructions for Claude Code (GLP)

**Last updated:** 2026-05-18
**Working directory:** `/Users/udi/Grassroots/GLP/`

## Code ownership

Every directory under `programs/` (and the Dart impl: `glp_runtime`, `glp_multiagent`, `test/`) is owned by exactly one paper — its **paper → code authority**.  Confirm you own a directory before editing it; cross-cutting changes (system-mode strip, DCE, wire format) are hub/Overview-coordinated.  Implementation decisions go in the owning paper's arXiv "Implementation Notes" appendix, not a separate spec doc.  **At session start, before any work, read `/Grassroots/docs/glp-paper-code-map.md` in full and state that you have done so** — it is the authoritative map, ownership policy, project roster, and the procedure for requesting changes to code you do not own.

## Mandatory reading at session start

Read these files in order before doing anything else, then state which you have read:

1. `/Users/udi/Grassroots/docs/claude.md` — project-wide instructions (these override everything)
2. This file (`CLAUDE.md`)
3. `docs/DISCIPLINE.md` — development discipline
4. `docs/typed-glp-manual.md` — typed GLP programming guide
5. `docs/glp-cheat-sheet.md` — patterns and idioms; "GLP is NOT Prolog"

Then STOP and wait for Udi's direction.  Do not read any other files until then.

## 🔴 After context compaction

If you see a session summary replacing the original conversation: STOP immediately.  Tell Udi you have emerged from compaction.  Summarise where things stand from the summary and verify against the filesystem (list directories, read files) before claiming anything.  Never assume the summary is complete or that prior agreements still hold.

If asked "did you do X?" after compaction, **verify on disk** before answering — never answer from memory alone.

## Operating GLP

### REPL

The REPL is the only tool: loading a `.glp` file in the REPL runs the full pipeline (SRSW → PE → type-check → compile → execute).  There is no separate compiler, type-checker, or runner.  Old standalone tools have been archived to `glp_runtime/bin/archive/` — do not execute them.

**Always invoke**: `bin/glpc` from `/Users/udi/Grassroots/GLP/glp_runtime/`.  `glpc` runs the AOT-compiled REPL (~0.3 s startup) and rebuilds it automatically whenever any `lib/` or `bin/` Dart source changes, so it is always current.  `self.glp` is read at runtime, so `.glp` edits need no rebuild.  It gives verdicts identical to the underlying `dart run bin/glp_repl.dart`, which it wraps.

🔴 **Do NOT use `dart run bin/glp_repl.dart` for routine checks.**  It pays full JIT startup every invocation (~0.8 s warm, several seconds when the `.dill` is cold) and is the slow path we deliberately retired.  Use `glpc`.  Reach for `dart run bin/glp_repl.dart` only to debug `glpc` itself or a suspected AOT/JIT discrepancy.

**Non-interactive use** (no approval prompt needed):

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/path/to/file.glp\ngoal.\n:quit\n' | bin/glpc
```

Do not use heredoc (`<<<`) — that requires approval per invocation.

**Loading a project**: enter the directory path at the prompt (e.g. `/Users/udi/Grassroots/GLP/programs/cssn/`).  This invokes the project linker, which resolves all `M # goal(...)` cross-module calls at compile time.

**REPL commands**: `:quit`, `:trace`, `:debug`, `:limit N`, `:activate <module>`.

### Test suites

| Suite | Command (run from `/Users/udi/Grassroots/GLP/`) | Tests |
|---|---|---|
| REPL test suite (canonical) | `bash test/run_all_tests.sh` | 489 |
| Dart unit tests | `cd glp_runtime && dart test` | 353 |
| Book examples (compilation only) | `bash test/run_book_tests.sh` | 141 files |

**Redirect output to `/private/tmp/`** rather than asking Udi to paste:

```bash
bash test/run_all_tests.sh > /private/tmp/glp-tests.txt 2>&1
cd glp_runtime && dart test > /private/tmp/glp-dart.txt 2>&1
```

Then `Read` the file.  Do not use `/tmp/` (not in allowed directories).

🔴 **Never run `run_all_tests.sh` and `dart test` concurrently** — `run_all_tests.sh` itself invokes `dart test` (Sections M/O), so a parallel `dart test` contends on the Dart build lock and silently aborts the run mid-suite.  Always run them sequentially.

**Staleness is handled automatically**: `glpc` rebuilds the AOT binary when `lib/`/`bin/` change, and `run_all_tests.sh` rebuilds its `.dill` the same way.  Only if you bypass both and call `dart run bin/glp_repl.dart` directly after editing `lib/` might you need `rm glp_runtime/.dart_tool/repl.dill` — another reason to use `glpc`.

### Baseline-before-commit (mandatory)

Before changing GLP runtime, types, root self.glp, or any cross-cutting code:

1. Run `bash test/run_all_tests.sh > /private/tmp/glp-baseline.txt 2>&1`.  Confirm it ends with `ALL TESTS PASSED!`.
2. Make the change.
3. Re-run the same suite.  If anything new fails, STOP and investigate before committing.
4. Commit and push only after both runs are clean.

For changes confined to a single play/test/program, the baseline can be skipped at your judgment.

### Adding new tests

`test/run_all_tests.sh` has sections A (typed runtime tests) through P+ (module boundary, SecureBonds, etc.).  Section A uses heredoc-based REPL sessions; positive-typecheck-only files go in the `POSITIVE_FILES` array; negative-typecheck files go in `NEGATIVE_FILES`.  When you fix a bug, add a test that exercises the fix.  When you add a feature, add tests that cover its main cases.  Tests are never removed.

## Spec-first development

🔴 **No implementation without a spec.**

1. Identify which spec(s) cover the area you are touching.  Quote the relevant section.
2. If the spec is clear: implement to match.  Do not ask permission to follow the spec.
3. If the spec is unclear or absent: STOP.  Propose a spec amendment.  Wait for agreement.  Then implement.
4. The code is never the source of truth when the spec is unclear — don't look at existing code to find "what works" if the spec doesn't say.

Applies equally to actor scripts and demo plays.  Before writing or modifying any `agent/4` protocol (groups, befriending, introductions), find and read the relevant spec (e.g. `/Users/udi/Grassroots/CSSN/docs/cssn-glp-implementation-spec.md`).

If you find yourself making the code work without spec backing → STOP and report.

When quoting a spec: quote exactly.  Don't paraphrase.  If the spec covers the case, say "the spec says X".  If the spec is silent, say "the spec doesn't address Y".  Never say "the spec is clear" then spend ten minutes explaining it.

## GLP code in papers and exposition

🔴 **NEVER program based on ignorance of GLP and its type system.**  Read the manual and cheat sheet; if they do not answer a question, STOP, state the gap, and wait until it is fixed.

🔴 **NEVER include in a paper GLP code that has not been typechecked and runtime tested.**  Every GLP program, clause, or fragment in any paper must first pass the type checker and run in the REPL.  An example that cannot be typechecked or run is fixed or removed — never shipped.

🔴 **A paper includes GLP programs in its body; it has no worked-examples appendix.**  GLP programs belong in the body as exposition.  A debugged paper carries no appendix of worked examples (per-program type automata, moded clauses, type-assignment tables that duplicate tested code).  The complete tested programs live in `/Grassroots/GLP/programs/<Paper>/`, to which the paper points.  Math appendices (proofs, definitions, constructions) stay.  This applies to all papers.

## Standing principle: do the right thing

See the Standing Principle in `/Grassroots/docs/claude.md`: do the more complete treatment, and never the minimal fix when it is not the correct one.

## Working modes

**Discussion mode (default).**

- No code changes.  No test runs.  No git operations.
- Brief responses.  Stay on topic.
- Wait for an explicit signal — "go ahead", "let's implement", "discussion over" — before moving to implementation.
- If Udi says "stop", halt immediately; no cleanup, no finishing the in-flight action.

**Implementation mode.**

- Only after explicit agreement.
- Make the agreed change, test, report.
- Do not exceed scope.  Do not silently add extras.

## Code modification protocol

- **`.glp` files Udi wrote**: never modify without discussion and explicit approval.  `.glp` files you created in this session may be modified freely.
- **Dart files**: you may modify, but state what you are changing and why.
- **Before any code change, identify the spec.**  See "Spec-first development" above.
- Don't combine an agreed change with an undiscussed structural change.  If you think something else should also change, say so first.
- Never make silent "improvements" to surrounding code.

## Bug protocol — no workarounds

When you hit a bug or unexpected behaviour:

1. STOP.  Do not work around, bypass, or compensate.
2. Check the spec.  Is the behaviour consistent with it?
3. Report in this exact format with no intervening prose:

   **Failing goal:**
   ```
   <the goal that fails>
   ```

   **Type and procedure declarations:**
   ```prolog
   <relevant type definitions>
   <procedure declaration>
   ```

   **Suspected clause(s):**
   ```prolog
   <the clause(s) that should match but don't>
   ```

4. Wait for discussion.  Do not attempt a fix.

What counts as a workaround: special-casing to dodge the bug; restructuring to route around; commenting out a failing test; marking it "expected to fail"; adding null checks for cases the spec doesn't address.

## Language design authority

🔴 The GLP language definition — guards, system predicates, body kernels, directives, type system features, primitive types — cannot be revised, extended, or added to without explicit discussion with Udi and his express approval.  Propose first, wait for approval, then implement.  See `docs/DISCIPLINE.md` §1.14.

## Communication style

Conversation style — terseness, plain English, numbered questions (at most two sentences each), alternatives, the vocabulary bans, never "final" — is in `/Grassroots/docs/writing-style-guide.md`, which governs chat as well as papers.  The working protocol (decisions, finishing a discussion, compaction) is in `/Grassroots/docs/claude.md`.

- **One-liner shell commands** when giving them to Udi.  Never a command that opens an interactive editor or pager (vi, vim, nano, less, `git` without `--no-pager`, `git rebase` without `GIT_EDITOR=true`, `crontab -e`).
- When showing GLP code: always include the type declarations, the procedure declaration, and the full clause, with no intervening text between related code blocks.

## Git and commit discipline

### Standard push workflow

```bash
git add <specific-files>
git commit -m "Single-line message"
git pull --no-rebase --no-edit origin main
git push origin main
```

- **Specific files only.**  Never `git add -A` or `git add .` — multiple sessions may be touching the repo.
- **Single-line commit messages.**  No multi-line — they confuse the shell quoting.
- **Pull with `--no-rebase --no-edit`** before push to integrate Overleaf-side or other-session changes without dropping into an editor.

### Multi-session protocol

Multiple Claude sessions may run on this repo concurrently:

1. Commit only files you worked on this session.  Don't sweep up other sessions' work-in-progress.
2. Never `git reset`, `git revert`, `git restore`, or `git checkout -- <file>` on files you did not modify — those changes may be another session's work.
3. On merge conflicts or unexpected changes from other sessions, STOP and report.

If your session is on a `claude/<session-id>` branch (older convention) rather than `main`, push to your branch and offer Udi the merge command:

```bash
cd /Users/udi/Grassroots/GLP
git checkout main
git pull origin main
git fetch origin claude/<branch-name>
git merge -m "Merge claude/<branch-name> into main" origin/claude/<branch-name>
git push origin main
```

### After completing a task

Always offer to fetch / merge / push.

## Environment

| Path | Value (Mac) |
|---|---|
| GLP root | `/Users/udi/Grassroots/GLP` |
| REPL source | `/Users/udi/Grassroots/GLP/glp_runtime/bin/glp_repl.dart` |
| Root self.glp | `/Users/udi/Grassroots/GLP/programs/self.glp` |
| Programs | `/Users/udi/Grassroots/GLP/programs/` |
| Test scripts | `/Users/udi/Grassroots/GLP/test/` |
| Dart binary | `/opt/homebrew/bin/dart` |

All `.glp` code lives in `/Users/udi/Grassroots/GLP/programs/`.  No `.glp` source files in paper repos (CSSN, GLP-arXiv, GLP-ICLP-2026, etc.) — paper repos may reference paths but must not contain copies.

All papers with example GLP programs must be tested.  Their directory in `programs/` is their paper directory.  E.g. `programs/TGLP/`.

When invoking commands, prefer absolute paths.  Maintain the current working directory across commands; only `cd` when Udi explicitly requests it.

## maGLP work — scope restriction

When working on maGLP (multi-agent GLP) code:

- Modify only files in `glp_runtime/lib/multiagent/` and `glp_runtime/test/multiagent/`.
- Do NOT modify core GLP files (`runner.dart`, `heap_fcp.dart`, `compiler/`, etc.) without explicit discussion and approval.
- If a bug in core GLP blocks maGLP work, STOP and report — do not work around.

## Flutter `glp_multiagent` app

When modifying `glp_runtime` code that affects the Flutter app (`glp_multiagent/`):

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent
pkill -f "glp_multiagent" 2>/dev/null
flutter clean
flutter pub get
flutter build macos
```

`flutter clean` is required — `flutter build macos` alone can use cached deps and miss your `glp_runtime` changes.  App log: `/private/tmp/glp_multiagent_trace.log` (clear before each test run).

## Reference specifications (read as needed, not at session start)

- `docs/glp-bytecode-v216-complete.md` — instruction set
- `docs/glp-runtime-spec.txt` — runtime architecture
- `docs/guards-reference.md` — guards catalog (success/suspend/fail semantics, negation, groundness implications)
- `docs/body-kernels-reference.md` — body kernels
- `docs/glp-compiler-spec.md` — compiler
- `docs/glp-arithmetic-spec.md` — arithmetic
- `docs/glp-io-spec.md` — I/O
- `docs/parser-spec.md` — parser
- `docs/naming-conventions.md` — naming
- `docs/mutual-ref-spec.md` — mutual references
- `docs/glp-predicate-taxonomy.md` — predicate taxonomy
- `docs/known-issues.md` — outstanding known issues
- `docs/Mandatory protocol for debugging the GLP implementation with GLP programs.txt` — debugging protocol
- `docs/grassroots-testing-framework.md` — theatre-style play testing
- `docs/village-market-scenario.md` — village market scenario
- CSSN GLP implementation spec: `/Users/udi/Grassroots/CSSN/docs/cssn-glp-implementation-spec.md`
- FCP reference (in `/tmp/FCP/` if cloned): `https://github.com/EShapiro2/FCP`

## Critical implementation context

- **SRSW** (Single Reader Single Writer): each variable occurs at most once as writer and at most once as reader in a clause.  Relaxations: ground guards make the variable usable multiply; constant types (`Integer`, `Number`, `String`, `Constant`) permit multiple occurrences; guard occurrences of `X?` do not count toward the head+body single-reader limit.
- **SRSW is mandatory**: all GLP code must pass SRSW.  Never invent a `skipSRSW` option.
- **Three-phase execution**: HEAD (tentative unification) → GUARDS (pure tests, three-valued: success / suspend / fail) → BODY (mutations).
- **Anonymous writer `_`**: a writer with no paired reader, exempt from SRSW — for discarded values.  `_?` (anonymous reader) is not permitted.
- **Writer MGU**: only binds writers, never readers; never binds writer to writer.
- **Three-valued unification**: success / suspend (unbound reader encountered) / fail (mismatch).
- **Duplicate `PartialEvaluator` class**: `analyzer.dart` has its own `class PartialEvaluator` separate from `partial_evaluator.dart`.  Changes to PE logic must be applied to **both**.

## #remember directive

When Udi says `#remember <something>`, add that information to this file so it persists across sessions.  Place it under the most appropriate existing section; only create a new section if no existing one fits.
