# Typed GLP Workstream — Handoff Report

**Date**: 2026-03-07
**Purpose**: Context for a new Claude Chat session picking up where the previous one left off.

---

## Spec Documents (read these first)

1. **Master plan**: `/Grassroots/GLP/docs/type system/parameterized-types-plan.md`
   — The full two-stage plan for parameterized types. Stage 1 is done. Stage 2 is in progress.

2. **Current plan**: `/Grassroots/GLP/docs/type system/current_plan.md`
   — Checklist of steps. Steps 1–6 are done. Step 7 (Step 2.4) is CURRENT. Step 2.6 (archive book/) is also done (out of order).

3. **Infrastructure fix spec** (already implemented): `/Grassroots/GLP/docs/type system/fix-template-propagation.md`
   — Describes two bugs that were fixed as a prerequisite for Step 2.4. Committed as `d11111f9`.

4. **Typed GLP manual**: `/Grassroots/GLP/docs/type system/typed-glp-manual.md`
   — Reference for the type system syntax and semantics.

5. **GLP cheat sheet**: `/Grassroots/GLP/docs/glp-cheat-sheet.md`

---

## Current Status

### What is done

| Step | Description | Commit |
|------|-------------|--------|
| 1 | Stage 1: Parser, expansion, tests for parameterized types | multiple commits |
| 1.4 | Add parameterized type defs to `self.glp` alongside monomorphic | included in Stage 1 |
| 2.1 | Convert test files (`programs/tests/typed/`) | `09be0ec0` and earlier |
| 2.2 | Convert typed_book (97 files) | `58f79570` |
| 2.3 | Convert module applications (CSSG, CSSN, simulated UI) | `a1a0567e` |
| Fix | Parameterized proc decl type checking | `910faf19` |
| Fix | Template propagation (Bug 1 + Bug 2 from `fix-template-propagation.md`) | `d11111f9` |
| 2.6 | Archive `book/` to `programs/archive/book/` (done out of order) | `1473fb31` |

All 390 REPL tests pass on the current HEAD (`1473fb31`).

### What is in progress: Step 2.4

**Goal**: Parameterize the prelude's generic procedures in `programs/self.glp` and remove old monomorphic type definitions.

**Changes needed in `programs/self.glp`**:

Procedure declarations to parameterize:
- `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`
- `procedure send(_?, Channel?, Channel).` → `procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).`
- `procedure receive(_, Channel?, Channel).` → `procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).`
- `procedure new_channel(Channel, Channel).` → `procedure new_channel(Channel(X, Y), Channel(Y, X)).`
- `procedure dl_append(DiffList?, DiffList?, DiffList).` → `procedure dl_append(DiffList(X)?, DiffList(X)?, DiffList(X)).`
- `procedure dl_to_list(DiffList?, Stream).` → `procedure dl_to_list(DiffList(X)?, Stream(X)).`

Monomorphic type definitions to remove (lines 10–12, 20 of current `self.glp`):
- `Stream ::= [] ; [_|Stream].` — REMOVE (keep `Stream(X) ::=`)
- `OpenStream ::= [_|Stream].` — REMOVE (keep `OpenStream(X) ::=`)
- `DiffList ::= Stream \ Stream?.` — REMOVE (keep `DiffList(X) ::=`)
- `Channel ::= ch(Stream, Stream?).` — REMOVE (keep `Channel(In, Out) ::=`)

**Known blocker**: `cssg_modules/self.glp` still uses bare `Channel` on lines 83 and 87–88:
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
UserInMsg   ::= msg(Constant, Constant, UserContent)
              ; intro_result(Constant, Channel)
              ; intro_rejected(Constant).
```
These must be converted to `Channel(IntroStream, IntroStream)` (or similar parameterized form) before removing the monomorphic `Channel` from the root `self.glp`.

**Not a blocker**: `cssg_modules/mad_boot.glp` uses `procedure tee(Stream?, Stream, Stream).` with bare `Stream`, but it's in `-mode(system)` which is not type-checked.

### What comes after Step 2.4

| Step | Description |
|------|-------------|
| 2.5 | Remove renamed procedure copies (send_agent, send_user, etc.) — the Section 14 workarounds |
| 2.7 | Adopt tight typing discipline (documentation updates) |
| 2.8 | Final validation |

Step 2.6 (archive book/) is already done.

---

## Key Files

- **Root prelude**: `/Grassroots/GLP/programs/self.glp` — the file being modified in Step 2.4
- **CSSG module types**: `/Grassroots/GLP/programs/cssg_modules/self.glp` — needs bare `Channel` converted
- **Param expansion engine**: `/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/param_expansion.dart`
- **Type AST** (TypeEnvironment class): `/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/type_ast.dart`
- **Type env builder**: `/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/type_environment_builder.dart`
- **Type checker**: `/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/type_checker.dart`
- **Module hierarchy**: `/Grassroots/GLP/glp_runtime/lib/runtime/module_hierarchy.dart`
- **Engine**: `/Grassroots/GLP/glp_runtime/lib/engine/glp_engine.dart`

## Test Commands

- Full REPL test suite (390 tests): `cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh`
- Dart unit tests: `cd /Users/udi/Grassroots/GLP/glp_runtime && dart test`

---

## Important Context for Claude Chat

- Claude Chat should NOT edit files directly. It should provide instructions for Claude Code to execute.
- Claude Chat should NOT edit `bib.bib` in any paper repo.
- The GLP CLAUDE.md (`/Grassroots/GLP/CLAUDE.md`) has mandatory reading and session-start protocols.
- All `.glp` code lives in `/Grassroots/GLP/programs/` — no GLP source in paper repos.
