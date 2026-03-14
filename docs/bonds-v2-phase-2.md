# Bonds V2 Phase 2: Create agent.glp

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/bonds_v2/self.glp` (created in Phase 1 — your types and helpers)
5. Read `programs/cssn_modules_v2/agent.glp` (reference model — first 80 lines for structure)
6. Read `programs/typed_book/bonds/agent.glp` — SKIP the type definitions and shared helper procedures (everything before `bind_response`), as those are now in self.glp. Read from `bind_response` to end of file.

## Task

Create `programs/bonds_v2/agent.glp` containing the agent and its LOCAL procedures.

## Module Declaration

```prolog
-module(agent).
-mode(system).
```

## Exported Procedure

```prolog
exported procedure agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?, Stream(Bond)?, Constant?).
```

## Local Procedures (must stay in agent.glp)

These call `merge` or recursively call `agent`, so they cannot be in self.glp:

1. **merge** — must be local per CSSN v2 convention (Stream(X) subtyping limitation):
```prolog
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).
```

2. **bind_response / handle_response** — call merge and add_output
3. **do_trade / do_trade_result** — call agent recursively
4. **handle_trade_fill** — calls agent recursively
5. **trade_dispatch** — calls agent recursively
6. **redemption_result / redemption_reject** — call agent recursively
7. **do_deposit_escrow / do_deposit_escrow_result** — call agent recursively

All `agent/6` clauses.

## Updating Procedure Declarations

Replace old stream type names with parametric forms in ALL procedure declarations:

| Old | New |
|-----|-----|
| `BondList` | `Stream(Bond)` |
| `LotList` | `Stream(Lot)` |
| `UserInStream` | `Stream(UserInMsg)` |
| `NetInStream` | `Stream(NetInMsg)` |
| `OutputsList` | `Stream(OutputEntry)` |

**Clause bodies do NOT change** — only procedure declaration lines.

## Verification

Load self.glp then agent.glp:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/bonds_v2/self.glp\nload ../programs/bonds_v2/agent.glp\n' | dart run bin/glp_repl.dart
```
Should load without type errors.

## Do NOT
- Duplicate types or helpers already in self.glp
- Read boot.glp, actors.glp, or mediator.glp
- Modify any existing files
