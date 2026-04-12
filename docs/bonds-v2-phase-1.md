# Bonds V2 Phase 1: Create self.glp

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`  
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/cssn_modules_v2/self.glp` (reference model)
5. Read the FIRST 400 lines of `programs/typed_book/bonds/agent.glp` (types + helpers)
6. Read `programs/typed_book/bonds/play12/self.glp` (narrative types)

## Task

Create `programs/bonds_v2/self.glp` containing all shared type definitions and shared helper procedures, following `cssn_modules_v2/self.glp` as the model.

## Type Definitions

Copy ALL type definitions from agent.glp, applying these transformations:

### ELIMINATE these standalone stream types (use Stream(X) instead):
- `BondList` → `Stream(Bond)`
- `LotList` → `Stream(Lot)`
- `FriendStream` → `Stream(FriendMsg)`
- `UserInStream` → `Stream(UserInMsg)`
- `NetInStream` → `Stream(NetInMsg)`
- `MediatorToAgentStream` → `Stream(MediatorToAgentMsg)`
- `AgentToUserStream` → `Stream(AgentToUserMsg)`
- `OutputStream` → `Stream(OutputMsg)`
- `OutputsList` → `Stream(OutputEntry)`
- `NarrativeStream` → `Stream(NarrativeItem)`

### KEEP as named parametric aliases (for readability):
```prolog
FriendChannel ::= ch(Stream(FriendMsg), Stream(FriendMsg)?).
AgentChannel  ::= ch(Stream(AgentToUserMsg), Stream(MediatorToAgentMsg)?).
UserChannel   ::= ch(Stream(UserCmd), Stream(UserNotify)?).
ActorChannel  ::= ch(Stream(UserNotify), Stream(UserCmd)?).
```

### KEEP all structural unions as-is:
Bond, Lot, TradeResponse, EscrowCancel, EscrowBenResult, EscrowDepResult, FriendContent, FriendMsg, Response, Decision, NetColdCall, UserContent, PendingValue, AgentContent, UserInMsg, MediatorToAgentMsg, AgentToUserMsg, NetInMsg, OutputKey, OutputContent, OutputMsg, OutputEntry, NarrativeItem.

### IMPORTANT: Inside structural unions, replace old stream type names with Stream(X):
- Where `BondList` appears inside a union variant, use `Stream(Bond)`
- Where `LotList` appears, use `Stream(Lot)`
- Example: `TradeResponse ::= trade_accept(Stream(Bond)) ; trade_decline(Stream(Bond)) ; trade_decline_menu(Stream(Bond), Stream(Bond)).`

### Also add mediator types (from mediator.glp — same unions):
ReqId, UserCmd, UserNotify, PendingEntry. Plus `UserChannel` and `ActorChannel` aliases.

## Helper Procedures

Copy these procedures from agent.glp, updating all procedure declarations to use `Stream(X)` instead of old type names:

1. `lookup_send` / `lookup_send_step`
2. `add_output`
3. `close_outputs`
4. `inject_msg`
5. `create_bonds`
6. `append` (for `Stream(Bond)`)
7. `select_bonds_exact`
8. `select_bonds_by_spec` / `select_by_spec_continue`
9. `bind_trade_accept` / `bind_trade_decline`
10. `inject_trade_result`
11. `classify_trade`
12. `build_menu` / `build_menu_acc` / `menu_update`
13. `escrow` (timer-vs-cancel race)
14. `inject_escrow_ben_result` / `inject_escrow_dep_result`
15. `bind_escrow_cancel`

Also from mediator.glp:
16. `lookup_pending`

Example of updating a procedure declaration:
```prolog
%% Before:
procedure create_bonds(Constant?, Constant?, Constant?, Constant?, BondList).
%% After:
procedure create_bonds(Constant?, Constant?, Constant?, Constant?, Stream(Bond)).
```

```prolog
%% Before:
procedure append(BondList?, BondList?, BondList).
%% After:
procedure append(Stream(Bond)?, Stream(Bond)?, Stream(Bond)).
```

```prolog
%% Before:
procedure lookup_send(OutputKey?, OutputMsg?, OutputsList?, OutputsList).
%% After:
procedure lookup_send(OutputKey?, OutputMsg?, Stream(OutputEntry)?, Stream(OutputEntry)).
```

**Clause bodies do NOT change** — they use variables, not type names. Only the `procedure` declaration line changes.

## Module Declaration

```prolog
-module(bonds).
-mode(system).
```

## Verification

After creating the file, load it in the REPL:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/bonds_v2/self.glp\n' | dart run bin/glp_repl.dart
```
Should load without errors.

## Do NOT
- Read boot.glp, actors.glp, or the full agent.glp
- Add any logic not in the source files
- Create any other files in this phase
