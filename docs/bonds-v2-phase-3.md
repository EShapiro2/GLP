# Bonds V2 Phase 3: Create mediator.glp

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/bonds_v2/self.glp` (created in Phase 1)
5. Read `programs/typed_book/bonds/mediator.glp` (source — SKIP type definitions at the top, read from `send` onward)

## Task

Create `programs/bonds_v2/mediator.glp` containing the UI mediator.

## Module Declaration

```prolog
-module(mediator).
-mode(system).
```

## Exported Procedure

```prolog
exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, Stream(PendingEntry)?, Constant?).
```

## Local Procedures

The mediator uses `send`, `receive`, `new_channel` as local unit-clause helpers for channel operations. These stay local:

```prolog
procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(X, Channel(Stream(X), Y)?, Channel(Stream(X), Y)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).

procedure new_channel(Channel(X, Y), Channel(Y, X)).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

`lookup_pending` is now in self.glp — do NOT duplicate it.

All `ui_mediator` clauses — copy from old mediator.glp, updating type names in declarations only.

## Updating Procedure Declarations

Replace old type names:
| Old | New |
|-----|-----|
| `PendingList` | `Stream(PendingEntry)` |
| `AgentChannel` | `AgentChannel` (already a named alias in self.glp) |
| `UserChannel` | `UserChannel` (already a named alias in self.glp) |

## Verification

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/bonds_v2/self.glp\nload ../programs/bonds_v2/mediator.glp\n' | dart run bin/glp_repl.dart
```

## Do NOT
- Duplicate types or lookup_pending from self.glp
- Read agent.glp, boot.glp, or actors.glp
