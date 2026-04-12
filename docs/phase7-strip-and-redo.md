# Phase 7: Strip Old Trade Code + Implement New Spec

## Step 1: Strip ALL old Phase 7 trade code

Remove everything trade-related from all four bond files. The old code uses `trade_reject`, `trade_insufficient`, `trade_opened`, `trade_was_rejected`, `trade_insufficient_other`, `trade_failed_mine`, `trade_rejected_by_me`, `trade_rejected_return`, `trade_insufficient_return`. ALL of this must go.

Specifically, remove from **bond_agent.glp**:
- Types: `Lot`, `LotList`, `TradeResponse`
- From `FriendContent`: `trade_propose(...)` variant
- From `UserContent`: `trade(...)`, `accept_trade(...)`, `reject_trade(...)` variants
- From `PendingValue`: `trade_pending(...)` variant
- From `AgentContent`: all `trade_*` variants
- From `UserInMsg`: `trade_complete(...)`, `trade_rejected_return(...)`, `trade_insufficient_return(...)` variants
- From `OutputContent`: all `trade_*` variants
- Procedures: `select_bonds_exact`, `select_bonds_by_spec`, `select_by_spec_continue`, `bind_trade_accept`, `bind_trade_reject`, `bind_trade_insufficient`, `inject_trade_result`, `do_trade`, `do_trade_result`, `handle_trade_accept_result`
- Agent clauses: entire "Phase 7: Trade" section (all trade agent clauses)

Remove from **bond_mediator.glp**: all trade-related types, mediator clauses

Remove from **bond_actors.glp**: all trade-related types, play8/play9 actors

Remove from **bond_boot.glp**: play8/fplay8, play9/fplay9

## Step 2: Verify clean state

Run fplay1–fplay7 and fplay4b. All must pass. This confirms the strip was clean.

## Step 3: Implement new Phase 7 per `docs/phase7-instructions.md`

Read `docs/phase7-instructions.md` and implement everything specified there. Key differences from the old code:

- `TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).` — decline carries bonds back (NOT separate trade_reject + trade_insufficient)
- `inject_trade_result` has NO OurBonds parameter — bonds come through the response
- `bind_trade_decline(TradeResponse, BondList?)` takes bonds to return
- Only 4 notifications: `trade_proposed`, `trade_completed`, `trade_failed`, `trade_returned`
- Responder gets NO notification on success (synchronous) or rejection (own action)
- Responder gets `trade_failed` only when accept fails due to insufficient holdings
- `handle_trade_fill(ok, ...)` has NO lookup_send notification
- `reject_trade` agent clause has NO lookup_send notification
