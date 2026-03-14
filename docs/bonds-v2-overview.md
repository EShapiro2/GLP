# Bonds V2 — Phased Migration Plan

**Date:** 2026-03-13
**Source:** `programs/typed_book/bonds/` (read-only reference)
**Target:** `programs/bonds_v2/` (new directory)
**Reference model:** `programs/cssn_modules_v2/`

## Why Phased

The bonds codebase is ~209KB. Combined with docs and CSSN v2 reference it exceeds Claude Code context limits. Each phase below reads only the files it needs.

## Phase Order

1. `self.glp` — types + shared helpers (extracted from agent.glp + mediator.glp)
2. `agent.glp` — agent/6 + local procedures
3. `mediator.glp` — ui_mediator/5
4. `boot.glp` — network switches + play wiring (extracted from old boot.glp)
5. `actors.glp` — self-contained test plays (extracted from old actors.glp)
6. `play12/` — village actors with escrow fix

Each phase has its own instruction file: `docs/bonds-v2-phase-N.md`

## Common Rules (apply to ALL phases)

### Startup (every phase)
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read the phase-specific instruction file
4. Do NOT read the full manual or cheat sheet — the phase file contains everything you need

### Parametric Types
- Every `XxxStream ::= [] ; [Xxx | XxxStream].` or `XxxList ::= [] ; [Xxx | XxxList].` is ELIMINATED
- Use `Stream(Xxx)` from prelude instead
- Named channel aliases using parametric types are OK for readability:
  ```prolog
  FriendChannel ::= ch(Stream(FriendMsg), Stream(FriendMsg)?).
  ```
- Structural union types (Bond, Lot, TradeResponse, etc.) stay as-is

### Module Structure
- Every file gets `-module(name).` and `-mode(system).`
- Shared types + helpers go in `self.glp` (ancestor scoping)
- `exported procedure` for cross-module entry points
- `imported procedure` for cross-module dependencies
- `M # goal(...)` for cross-module calls in boot.glp
- `merge` must be defined locally in each module that uses it (Stream(X) subtyping limitation — same as CSSN v2)

### Escrow Fix (apply in ALL phases that touch play12)
Charlie deposits `lot(frank, 0, 5)` not `lot(alice, 0, 8)`. See `Grassroots-Bonds/docs/fix-play12-escrow-instructions.md` for full details.

### Testing
After EACH phase, verify the new file compiles:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```
Then load the new file(s) and check for type errors.

### Do NOT
- Modify anything in `programs/typed_book/bonds/`
- Read files not listed in the phase instructions
- Invent new logic — this is a mechanical restructuring
