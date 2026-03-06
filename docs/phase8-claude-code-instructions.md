# Claude Code Instructions: Phase 8 Escrow Implementation

## Mandatory Reading (BEFORE any action)

Read these files in order, acknowledge each:
1. `/Users/udi/Grassroots/GLP/CLAUDE.md`
2. `/Users/udi/Grassroots/GLP/docs/DISCIPLINE.md`
3. `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md`
4. `/Users/udi/Grassroots/GLP/docs/glp-cheat-sheet.md`
5. Then STOP and wait — do not read any other files yet.

When Udi says to proceed, follow these instructions exactly.

---

## Environment Setup

```bash
export PATH="/home/user/dart-sdk/bin:$PATH"
```

Clone repos:
```bash
git clone --depth 1 https://github.com/EShapiro2/FCP.git /tmp/FCP
git clone --depth 1 https://github.com/EShapiro2/Art-of-GLP-2025.git /tmp/Art-of-GLP-2025
```

Set up your working branch:
```bash
cd /Users/udi/Grassroots/GLP && git checkout main && git pull origin main
```

---

## Baseline Test

Run baseline before any changes and record the result:
```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh > /private/tmp/baseline-tests.txt 2>&1
```
Read `/private/tmp/baseline-tests.txt`. Report test count. If anything unexpected fails, STOP and report.

Then commit baseline:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "Checkpoint: before phase8 escrow implementation"
```

---

## Overview of Changes

Four files need editing, in this order:

1. **bond_agent.glp** — fix 2 bugs (SRSW violation + missing `?` in OutputContent)
2. **bond_mediator.glp** — add escrow types and clauses
3. **bond_actors.glp** — add escrow types and play10/play11 actors
4. **bond_boot.glp** — add play10/fplay10 and play11/fplay11

Type-check after each file. Do NOT proceed to the next file if type-check fails.

The REPL load command for type-checking is:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\n:quit' | dart run bin/glp_repl.dart
```
Replace the filename for each file. All four files must be loaded together to run plays:
```bash
echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\nfplay11.\n:quit' | dart run bin/glp_repl.dart
```

---

## Step 1: Fix bond_agent.glp (2 edits)

File: `/Users/udi/Grassroots/GLP/programs/typed_book/bonds/bond_agent.glp`

Read the file before editing. Verify the two targets exist exactly as shown.

### Edit 1A: Fix SRSW violation in beneficiary escrow_offer clause

Find (near bottom, in the `--- Phase 8: Escrow ---` section):
```
%% Beneficiary: incoming escrow_offer
agent(Id, UserIn, [msg(From, Id1, escrow_offer(ReleaseTime, BenResult?))|NetIn],
```

Replace with:
```
%% Beneficiary: incoming escrow_offer
agent(Id, UserIn, [msg(From, Id1, escrow_offer(ReleaseTime, BenResult))|NetIn],
```

That is the single character change: `BenResult?` → `BenResult` (remove the `?`).

**Why**: `BenResult?` in the head would be a reader with no paired writer — SRSW violation. `BenResult` (writer) in head + `BenResult?` (reader) in body = 1W + 1R = correct. The body already reads it with `inject_escrow_ben_result(BenResult?, ...)`.

### Edit 1B: Fix OutputContent — add missing `?`

Find in the `OutputContent` type definition:
```
                ; escrow_offer(Constant, EscrowBenResult)
```

Replace with:
```
                ; escrow_offer(Constant, EscrowBenResult?)
```

**Why**: The escrow_offer message carries a reader reference (the BenResult variable the beneficiary will monitor). The `?` is required in the type to match the actual data mode.

### After Step 1: Type-check

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\n:quit' | dart run bin/glp_repl.dart > /private/tmp/step1-typecheck.txt 2>&1
```

Read `/private/tmp/step1-typecheck.txt`. It must show successful load with no errors. If there are errors, fix them before proceeding.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "phase8: fix SRSW violation in escrow_offer clause + OutputContent type"
```

---

## Step 2: Update bond_mediator.glp

File: `/Users/udi/Grassroots/GLP/programs/typed_book/bonds/bond_mediator.glp`

Read the entire file before making any edits. The edits are grouped by location.

### Edit 2A: Add EscrowCancel type to type definitions

The mediator needs the escrow types. Add after `TradeResponse` and before `Installment`:

Find:
```
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).

Installment ::= installment(Constant, Constant).
```

Replace with:
```
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).

EscrowCancel    ::= cancel.
EscrowBenResult ::= escrow_bonds(BondList) ; escrow_cancelled.
EscrowDepResult ::= escrow_bonds(BondList) ; escrow_expired.

Installment ::= installment(Constant, Constant).
```

### Edit 2B: Add escrow_offer to FriendContent

Find:
```
                ; trade_propose(LotList, BondList, TradeResponse?).
FriendMsg     ::= msg(Constant, Constant, FriendContent).
```

Replace with:
```
                ; trade_propose(LotList, BondList, TradeResponse?)
                ; escrow_offer(Constant, EscrowBenResult?).
FriendMsg     ::= msg(Constant, Constant, FriendContent).
```

### Edit 2C: Add escrow entries to AgentContent

Find the end of AgentContent:
```
               ; trade_completed(Constant)
               ; trade_failed(Constant)
               ; trade_returned(Constant).
```

Replace with:
```
               ; trade_completed(Constant)
               ; trade_failed(Constant)
               ; trade_returned(Constant)
               ; escrow_deposited(Constant, Constant, EscrowCancel?)
               ; escrow_received(Constant, Constant)
               ; escrow_released(Constant)
               ; escrow_cancelled(Constant)
               ; escrow_expired(Constant)
               ; escrow_returned(Constant)
               ; escrow_failed(Constant).
```

### Edit 2D: Add escrow_pending to PendingValue

Find:
```
               ; trade_pending(TradeResponse?, LotList, BondList)
               ; error.
```

Replace with:
```
               ; trade_pending(TradeResponse?, LotList, BondList)
               ; escrow_pending(EscrowCancel?)
               ; error.
```

### Edit 2E: Add deposit_escrow and cancel_escrow to UserContent

Find the end of UserContent:
```
              ; accept_trade(Constant, PendingValue)
              ; reject_trade(Constant, PendingValue).
```

Replace with:
```
              ; accept_trade(Constant, PendingValue)
              ; reject_trade(Constant, PendingValue)
              ; deposit_escrow(Constant, LotList, Constant)
              ; cancel_escrow(Constant, PendingValue).
```

### Edit 2F: Add deposit_escrow and cancel_escrow to UserCmd

Find the end of UserCmd:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId).
```

Replace with:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId)
          ; deposit_escrow(Constant, LotList, Constant)
          ; cancel_escrow(Constant, ReqId).
```

### Edit 2G: Add escrow entries to UserNotify

Find the end of UserNotify:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant).
```

Replace with:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant)
             ; escrow_deposited(Constant, Constant, ReqId)
             ; escrow_received(Constant, Constant)
             ; escrow_released(Constant)
             ; escrow_cancelled(Constant)
             ; escrow_expired(Constant)
             ; escrow_returned(Constant)
             ; escrow_failed(Constant).
```

### Edit 2H: Add escrow mediator clauses

Add these clauses BEFORE the `--- Termination ---` section (before `ui_mediator(_, ch([], []), ch([], []), _, _).`):

```

%% --- Agent-to-user: escrow_deposited (non-ground — store CancelSignal in pending) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_deposited(Target, ReleaseTime, CancelSignal?)),
            AgentCh?, AgentCh1),
    ground(Target?), ground(ReleaseTime?) |
    send_user(escrow_deposited(Target?, ReleaseTime?, req(N?)), UserCh?, UserCh1),
    N1 := N? + 1,
    ui_mediator(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), escrow_pending(CancelSignal)) | Ps?], N1?).

%% --- Agent-to-user: escrow_received (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_received(From, ReleaseTime)),
            AgentCh?, AgentCh1),
    ground(From?), ground(ReleaseTime?) |
    send_user(escrow_received(From?, ReleaseTime?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- Agent-to-user: escrow_released (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_released(From)),
            AgentCh?, AgentCh1),
    ground(From?) |
    send_user(escrow_released(From?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- Agent-to-user: escrow_cancelled (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_cancelled(From)),
            AgentCh?, AgentCh1),
    ground(From?) |
    send_user(escrow_cancelled(From?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- Agent-to-user: escrow_expired (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_expired(Target)),
            AgentCh?, AgentCh1),
    ground(Target?) |
    send_user(escrow_expired(Target?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- Agent-to-user: escrow_returned (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_returned(Target)),
            AgentCh?, AgentCh1),
    ground(Target?) |
    send_user(escrow_returned(Target?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- Agent-to-user: escrow_failed (ground — pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_failed(Target)),
            AgentCh?, AgentCh1),
    ground(Target?) |
    send_user(escrow_failed(Target?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- User-to-agent: deposit_escrow (pass through) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(deposit_escrow(Target, GiveSpec, ReleaseTime), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(GiveSpec?), ground(ReleaseTime?) |
    send_agent(msg('_user', Id?, deposit_escrow(Target?, GiveSpec?, ReleaseTime?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).

%% --- User-to-agent: cancel_escrow (with pending lookup) ---

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(cancel_escrow(Target, ReqId), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(ReqId?) |
    lookup_pending(ReqId?, Pv, Ps?, Ps1),
    send_agent(msg('_user', Id?, cancel_escrow(Target?, Pv?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps1?, N?).
```

### After Step 2: Type-check

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_mediator.glp\n:quit' | dart run bin/glp_repl.dart > /private/tmp/step2-typecheck.txt 2>&1
```

Read `/private/tmp/step2-typecheck.txt`. Must load cleanly. Fix any errors.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "phase8: add escrow types and clauses to mediator"
```

---

## Step 3: Update bond_actors.glp

File: `/Users/udi/Grassroots/GLP/programs/typed_book/bonds/bond_actors.glp`

Read the entire file before making any edits.

### Edit 3A: Add escrow types

The actors file needs the escrow types. Find the type definitions section. After the existing trade-related types, add the escrow types. Find:

```
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).
```

Add after it:
```

EscrowCancel    ::= cancel.
EscrowBenResult ::= escrow_bonds(BondList) ; escrow_cancelled.
EscrowDepResult ::= escrow_bonds(BondList) ; escrow_expired.
```

### Edit 3B: Add escrow entries to UserCmd

Find the end of UserCmd:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId).
```

Replace with:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId)
          ; deposit_escrow(Constant, LotList, Constant)
          ; cancel_escrow(Constant, ReqId).
```

### Edit 3C: Add escrow entries to UserNotify

Find the end of UserNotify:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant).
```

Replace with:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant)
             ; escrow_deposited(Constant, Constant, ReqId)
             ; escrow_received(Constant, Constant)
             ; escrow_released(Constant)
             ; escrow_cancelled(Constant)
             ; escrow_expired(Constant)
             ; escrow_returned(Constant)
             ; escrow_failed(Constant).
```

### Edit 3D: Add play10 actors (time release)

Add at the END of the file:

```

%% =============================================================================
%% PLAY 10 — Escrow: Time Release
%% =============================================================================
%%
%% Alice:
%% 1. connect(bob)
%% 2. Wait connected(bob)
%% 3. credit(bob,5,0)
%% 4. Wait credit_opened(bob,5)
%% 5. deposit_escrow(bob,[lot(bob,0,3)],T)
%% 6. Wait escrow_deposited(bob,T,req(1))
%% 7. Wait escrow_expired(bob)
%% 8. balance
%% 9. Wait balance_report(_)
%% 10. done
%%
%% Bob:
%% 1. Wait befriend(alice,req(1)) → decision(yes,alice,req(1))
%% 2. Wait credit_proposed(alice,5,0,req(2)) → accept_credit(alice,req(2))
%% 3. Wait escrow_received(alice,T)
%% 4. Wait escrow_released(alice)
%% 5. balance
%% 6. Wait balance_report(_)
%% 7. done

%% --- Alice (play10) ---

procedure alice_p10(Constant?, ActorChannel?).
alice_p10(T, ch(In, [connect(bob)|Out?])) :-
    ground(T?) |
    alice_p10_wait_connected(T?, In?, Out).

procedure alice_p10_wait_connected(Constant?, UserNotifyStream?, UserCmdStream).
alice_p10_wait_connected(T, [connected(bob)|In],
                         [credit(bob, 5, 0)|Out?]) :-
    alice_p10_wait_credit(T?, In?, Out).
alice_p10_wait_connected(T, [_|In], Out?) :-
    otherwise | alice_p10_wait_connected(T?, In?, Out).
alice_p10_wait_connected(_, [], []).

procedure alice_p10_wait_credit(Constant?, UserNotifyStream?, UserCmdStream).
alice_p10_wait_credit(T, [credit_opened(bob, 5)|In],
                      [deposit_escrow(bob, [lot(bob, 0, 3)], T?)|Out?]) :-
    alice_p10_wait_deposited(In?, Out).
alice_p10_wait_credit(T, [_|In], Out?) :-
    otherwise | alice_p10_wait_credit(T?, In?, Out).
alice_p10_wait_credit(_, [], []).

procedure alice_p10_wait_deposited(UserNotifyStream?, UserCmdStream).
alice_p10_wait_deposited([escrow_deposited(bob, _, _)|In], Out?) :-
    alice_p10_wait_expired(In?, Out).
alice_p10_wait_deposited([_|In], Out?) :-
    otherwise | alice_p10_wait_deposited(In?, Out).
alice_p10_wait_deposited([], []).

procedure alice_p10_wait_expired(UserNotifyStream?, UserCmdStream).
alice_p10_wait_expired([escrow_expired(bob)|In], [balance|Out?]) :-
    alice_p10_wait_balance(In?, Out).
alice_p10_wait_expired([_|In], Out?) :-
    otherwise | alice_p10_wait_expired(In?, Out).
alice_p10_wait_expired([], []).

procedure alice_p10_wait_balance(UserNotifyStream?, UserCmdStream).
alice_p10_wait_balance([balance_report(_)|In], [done|Out?]) :-
    alice_p10_finish(In?, Out).
alice_p10_wait_balance([_|In], Out?) :-
    otherwise | alice_p10_wait_balance(In?, Out).
alice_p10_wait_balance([], []).

procedure alice_p10_finish(UserNotifyStream?, UserCmdStream).
alice_p10_finish(_, []).

%% --- Bob (play10) ---

procedure bob_p10(ActorChannel?).
bob_p10(ch([befriend(alice, ReqId)|In],
           [decision(yes, alice, ReqId?)|Out?])) :-
    ground(ReqId?) |
    bob_p10_wait_connected(In?, Out).
bob_p10(ch([_|In], Out?)) :-
    otherwise | bob_p10(ch(In?, Out)).

procedure bob_p10_wait_connected(UserNotifyStream?, UserCmdStream).
bob_p10_wait_connected([connected(alice)|In], Out?) :-
    bob_p10_wait_credit_proposed(In?, Out).
bob_p10_wait_connected([_|In], Out?) :-
    otherwise | bob_p10_wait_connected(In?, Out).
bob_p10_wait_connected([], []).

procedure bob_p10_wait_credit_proposed(UserNotifyStream?, UserCmdStream).
bob_p10_wait_credit_proposed([credit_proposed(alice, 5, 0, ReqId)|In],
                             [accept_credit(alice, ReqId?)|Out?]) :-
    ground(ReqId?) |
    bob_p10_wait_credit_opened(In?, Out).
bob_p10_wait_credit_proposed([_|In], Out?) :-
    otherwise | bob_p10_wait_credit_proposed(In?, Out).
bob_p10_wait_credit_proposed([], []).

procedure bob_p10_wait_credit_opened(UserNotifyStream?, UserCmdStream).
bob_p10_wait_credit_opened([credit_opened(alice, 5)|In], Out?) :-
    bob_p10_wait_escrow_received(In?, Out).
bob_p10_wait_credit_opened([_|In], Out?) :-
    otherwise | bob_p10_wait_credit_opened(In?, Out).
bob_p10_wait_credit_opened([], []).

procedure bob_p10_wait_escrow_received(UserNotifyStream?, UserCmdStream).
bob_p10_wait_escrow_received([escrow_received(alice, _)|In], Out?) :-
    bob_p10_wait_escrow_released(In?, Out).
bob_p10_wait_escrow_received([_|In], Out?) :-
    otherwise | bob_p10_wait_escrow_received(In?, Out).
bob_p10_wait_escrow_received([], []).

procedure bob_p10_wait_escrow_released(UserNotifyStream?, UserCmdStream).
bob_p10_wait_escrow_released([escrow_released(alice)|In], [balance|Out?]) :-
    bob_p10_wait_balance(In?, Out).
bob_p10_wait_escrow_released([_|In], Out?) :-
    otherwise | bob_p10_wait_escrow_released(In?, Out).
bob_p10_wait_escrow_released([], []).

procedure bob_p10_wait_balance(UserNotifyStream?, UserCmdStream).
bob_p10_wait_balance([balance_report(_)|_], [done|Out?]) :-
    bob_p10_finish(Out).
bob_p10_wait_balance([_|In], Out?) :-
    otherwise | bob_p10_wait_balance(In?, Out).
bob_p10_wait_balance([], []).

procedure bob_p10_finish(UserCmdStream).
bob_p10_finish([]).

%% =============================================================================
%% PLAY 11 — Escrow: Cancel
%% =============================================================================
%%
%% Alice:
%% 1. connect(bob)
%% 2. Wait connected(bob)
%% 3. credit(bob,5,0)
%% 4. Wait credit_opened(bob,5)
%% 5. deposit_escrow(bob,[lot(bob,0,3)],T)
%% 6. Wait escrow_deposited(bob,T,ReqId)
%% 7. cancel_escrow(bob,ReqId)
%% 8. Wait escrow_returned(bob)
%% 9. balance
%% 10. Wait balance_report(_)
%% 11. done
%%
%% Bob:
%% 1. Wait befriend(alice,req(1)) → decision(yes,alice,req(1))
%% 2. Wait credit_proposed(alice,5,0,req(2)) → accept_credit(alice,req(2))
%% 3. Wait escrow_received(alice,T)
%% 4. Wait escrow_cancelled(alice)
%% 5. balance
%% 6. Wait balance_report(_)
%% 7. done

%% --- Alice (play11) ---

procedure alice_p11(Constant?, ActorChannel?).
alice_p11(T, ch(In, [connect(bob)|Out?])) :-
    ground(T?) |
    alice_p11_wait_connected(T?, In?, Out).

procedure alice_p11_wait_connected(Constant?, UserNotifyStream?, UserCmdStream).
alice_p11_wait_connected(T, [connected(bob)|In],
                         [credit(bob, 5, 0)|Out?]) :-
    alice_p11_wait_credit(T?, In?, Out).
alice_p11_wait_connected(T, [_|In], Out?) :-
    otherwise | alice_p11_wait_connected(T?, In?, Out).
alice_p11_wait_connected(_, [], []).

procedure alice_p11_wait_credit(Constant?, UserNotifyStream?, UserCmdStream).
alice_p11_wait_credit(T, [credit_opened(bob, 5)|In],
                      [deposit_escrow(bob, [lot(bob, 0, 3)], T?)|Out?]) :-
    alice_p11_wait_deposited(In?, Out).
alice_p11_wait_credit(T, [_|In], Out?) :-
    otherwise | alice_p11_wait_credit(T?, In?, Out).
alice_p11_wait_credit(_, [], []).

procedure alice_p11_wait_deposited(UserNotifyStream?, UserCmdStream).
alice_p11_wait_deposited([escrow_deposited(bob, _, ReqId)|In],
                         [cancel_escrow(bob, ReqId?)|Out?]) :-
    ground(ReqId?) |
    alice_p11_wait_returned(In?, Out).
alice_p11_wait_deposited([_|In], Out?) :-
    otherwise | alice_p11_wait_deposited(In?, Out).
alice_p11_wait_deposited([], []).

procedure alice_p11_wait_returned(UserNotifyStream?, UserCmdStream).
alice_p11_wait_returned([escrow_returned(bob)|In], [balance|Out?]) :-
    alice_p11_wait_balance(In?, Out).
alice_p11_wait_returned([_|In], Out?) :-
    otherwise | alice_p11_wait_returned(In?, Out).
alice_p11_wait_returned([], []).

procedure alice_p11_wait_balance(UserNotifyStream?, UserCmdStream).
alice_p11_wait_balance([balance_report(_)|In], [done|Out?]) :-
    alice_p11_finish(In?, Out).
alice_p11_wait_balance([_|In], Out?) :-
    otherwise | alice_p11_wait_balance(In?, Out).
alice_p11_wait_balance([], []).

procedure alice_p11_finish(UserNotifyStream?, UserCmdStream).
alice_p11_finish(_, []).

%% --- Bob (play11) ---

procedure bob_p11(ActorChannel?).
bob_p11(ch([befriend(alice, ReqId)|In],
           [decision(yes, alice, ReqId?)|Out?])) :-
    ground(ReqId?) |
    bob_p11_wait_connected(In?, Out).
bob_p11(ch([_|In], Out?)) :-
    otherwise | bob_p11(ch(In?, Out)).

procedure bob_p11_wait_connected(UserNotifyStream?, UserCmdStream).
bob_p11_wait_connected([connected(alice)|In], Out?) :-
    bob_p11_wait_credit_proposed(In?, Out).
bob_p11_wait_connected([_|In], Out?) :-
    otherwise | bob_p11_wait_connected(In?, Out).
bob_p11_wait_connected([], []).

procedure bob_p11_wait_credit_proposed(UserNotifyStream?, UserCmdStream).
bob_p11_wait_credit_proposed([credit_proposed(alice, 5, 0, ReqId)|In],
                             [accept_credit(alice, ReqId?)|Out?]) :-
    ground(ReqId?) |
    bob_p11_wait_credit_opened(In?, Out).
bob_p11_wait_credit_proposed([_|In], Out?) :-
    otherwise | bob_p11_wait_credit_proposed(In?, Out).
bob_p11_wait_credit_proposed([], []).

procedure bob_p11_wait_credit_opened(UserNotifyStream?, UserCmdStream).
bob_p11_wait_credit_opened([credit_opened(alice, 5)|In], Out?) :-
    bob_p11_wait_escrow_received(In?, Out).
bob_p11_wait_credit_opened([_|In], Out?) :-
    otherwise | bob_p11_wait_credit_opened(In?, Out).
bob_p11_wait_credit_opened([], []).

procedure bob_p11_wait_escrow_received(UserNotifyStream?, UserCmdStream).
bob_p11_wait_escrow_received([escrow_received(alice, _)|In], Out?) :-
    bob_p11_wait_escrow_cancelled(In?, Out).
bob_p11_wait_escrow_received([_|In], Out?) :-
    otherwise | bob_p11_wait_escrow_received(In?, Out).
bob_p11_wait_escrow_received([], []).

procedure bob_p11_wait_escrow_cancelled(UserNotifyStream?, UserCmdStream).
bob_p11_wait_escrow_cancelled([escrow_cancelled(alice)|In], [balance|Out?]) :-
    bob_p11_wait_balance(In?, Out).
bob_p11_wait_escrow_cancelled([_|In], Out?) :-
    otherwise | bob_p11_wait_escrow_cancelled(In?, Out).
bob_p11_wait_escrow_cancelled([], []).

procedure bob_p11_wait_balance(UserNotifyStream?, UserCmdStream).
bob_p11_wait_balance([balance_report(_)|_], [done|Out?]) :-
    bob_p11_finish(Out).
bob_p11_wait_balance([_|In], Out?) :-
    otherwise | bob_p11_wait_balance(In?, Out).
bob_p11_wait_balance([], []).

procedure bob_p11_finish(UserCmdStream).
bob_p11_finish([]).
```

### After Step 3: Type-check actors standalone

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_actors.glp\n:quit' | dart run bin/glp_repl.dart > /private/tmp/step3-typecheck.txt 2>&1
```

Read `/private/tmp/step3-typecheck.txt`. Must load cleanly. Fix any errors.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "phase8: add play10/play11 actors"
```

---

## Step 4: Update bond_boot.glp

File: `/Users/udi/Grassroots/GLP/programs/typed_book/bonds/bond_boot.glp`

Read the entire file before making any edits.

### Edit 4A: Add escrow types

Find the type definitions section. After the existing trade-related types, add the escrow types. Find:

```
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).
```

Add after it:
```

EscrowCancel    ::= cancel.
EscrowBenResult ::= escrow_bonds(BondList) ; escrow_cancelled.
EscrowDepResult ::= escrow_bonds(BondList) ; escrow_expired.
```

### Edit 4B: Add escrow entries to UserCmd

Find the end of UserCmd:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId).
```

Replace with:
```
          ; accept_trade(Constant, ReqId)
          ; reject_trade(Constant, ReqId)
          ; deposit_escrow(Constant, LotList, Constant)
          ; cancel_escrow(Constant, ReqId).
```

### Edit 4C: Add escrow entries to UserNotify

Find the end of UserNotify:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant).
```

Replace with:
```
             ; trade_completed(Constant)
             ; trade_failed(Constant)
             ; trade_returned(Constant)
             ; escrow_deposited(Constant, Constant, ReqId)
             ; escrow_received(Constant, Constant)
             ; escrow_released(Constant)
             ; escrow_cancelled(Constant)
             ; escrow_expired(Constant)
             ; escrow_returned(Constant)
             ; escrow_failed(Constant).
```

### Edit 4D: Add play10/fplay10/play11/fplay11

Add at the END of the file:

```

%% =============================================================================
%% PLAY 10 — Escrow: Time Release
%% =============================================================================
%%
%% T = now + 500 ms. Alice deposits 3 bob-coins for Bob.
%% Timer expires. Bob gets bonds. Alice gets escrow_expired.

play10 :-
    now(Now),
    T := Now? + 500,
    network2(ch(AliceNetOut?, AliceNetIn),
             ch(BobNetOut?, BobNetIn)),

    %% Alice
    alice_p10(T?, ch(AliceActorIn?, AliceActorOut)),
    tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
    agent(alice, AliceAgentIn?, AliceNetIn?,
          [output('_user', AliceAgentToUser), output('_net', AliceNetOut)],
          [], 1),
    ui_mediator(alice, ch(AliceAgentToUser?, AliceAgentIn),
                ch(AliceMedIn?, AliceMedOut), [], 1),
    tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
    sink(AliceDispCmd?), sink(AliceDispNotify?),

    %% Bob
    bob_p10(ch(BobActorIn?, BobActorOut)),
    tee(BobActorOut?, BobMedIn, BobDispCmd),
    agent(bob, BobAgentIn?, BobNetIn?,
          [output('_user', BobAgentToUser), output('_net', BobNetOut)],
          [], 1),
    ui_mediator(bob, ch(BobAgentToUser?, BobAgentIn),
                ch(BobMedIn?, BobMedOut), [], 1),
    tee(BobMedOut?, BobActorIn, BobDispNotify),
    sink(BobDispCmd?), sink(BobDispNotify?).

%% =============================================================================
%% FLUTTER PLAY 10 — Escrow: Time Release (with tagged output for UI)
%% =============================================================================

fplay10 :-
    now(Now),
    T := Now? + 500,
    network2(ch(AliceNetOut?, AliceNetIn),
             ch(BobNetOut?, BobNetIn)),

    %% Alice
    alice_p10(T?, ch(AliceActorIn?, AliceActorOut)),
    tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
    agent(alice, AliceAgentIn?, AliceNetIn?,
          [output('_user', AliceAgentToUser), output('_net', AliceNetOut)],
          [], 1),
    ui_mediator(alice, ch(AliceAgentToUser?, AliceAgentIn),
                ch(AliceMedIn?, AliceMedOut), [], 1),
    tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
    send_to_user_tagged(alice, AliceDispCmd?, AliceDispNotify?),

    %% Bob
    bob_p10(ch(BobActorIn?, BobActorOut)),
    tee(BobActorOut?, BobMedIn, BobDispCmd),
    agent(bob, BobAgentIn?, BobNetIn?,
          [output('_user', BobAgentToUser), output('_net', BobNetOut)],
          [], 1),
    ui_mediator(bob, ch(BobAgentToUser?, BobAgentIn),
                ch(BobMedIn?, BobMedOut), [], 1),
    tee(BobMedOut?, BobActorIn, BobDispNotify),
    send_to_user_tagged(bob, BobDispCmd?, BobDispNotify?).

%% =============================================================================
%% PLAY 11 — Escrow: Cancel
%% =============================================================================
%%
%% T = now + 5000 ms (far future). Alice deposits 3 bob-coins for Bob.
%% Alice cancels immediately. Alice gets bonds back. Bob gets escrow_cancelled.

play11 :-
    now(Now),
    T := Now? + 5000,
    network2(ch(AliceNetOut?, AliceNetIn),
             ch(BobNetOut?, BobNetIn)),

    %% Alice
    alice_p11(T?, ch(AliceActorIn?, AliceActorOut)),
    tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
    agent(alice, AliceAgentIn?, AliceNetIn?,
          [output('_user', AliceAgentToUser), output('_net', AliceNetOut)],
          [], 1),
    ui_mediator(alice, ch(AliceAgentToUser?, AliceAgentIn),
                ch(AliceMedIn?, AliceMedOut), [], 1),
    tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
    sink(AliceDispCmd?), sink(AliceDispNotify?),

    %% Bob
    bob_p11(ch(BobActorIn?, BobActorOut)),
    tee(BobActorOut?, BobMedIn, BobDispCmd),
    agent(bob, BobAgentIn?, BobNetIn?,
          [output('_user', BobAgentToUser), output('_net', BobNetOut)],
          [], 1),
    ui_mediator(bob, ch(BobAgentToUser?, BobAgentIn),
                ch(BobMedIn?, BobMedOut), [], 1),
    tee(BobMedOut?, BobActorIn, BobDispNotify),
    sink(BobDispCmd?), sink(BobDispNotify?).

%% =============================================================================
%% FLUTTER PLAY 11 — Escrow: Cancel (with tagged output for UI)
%% =============================================================================

fplay11 :-
    now(Now),
    T := Now? + 5000,
    network2(ch(AliceNetOut?, AliceNetIn),
             ch(BobNetOut?, BobNetIn)),

    %% Alice
    alice_p11(T?, ch(AliceActorIn?, AliceActorOut)),
    tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
    agent(alice, AliceAgentIn?, AliceNetIn?,
          [output('_user', AliceAgentToUser), output('_net', AliceNetOut)],
          [], 1),
    ui_mediator(alice, ch(AliceAgentToUser?, AliceAgentIn),
                ch(AliceMedIn?, AliceMedOut), [], 1),
    tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
    send_to_user_tagged(alice, AliceDispCmd?, AliceDispNotify?),

    %% Bob
    bob_p11(ch(BobActorIn?, BobActorOut)),
    tee(BobActorOut?, BobMedIn, BobDispCmd),
    agent(bob, BobAgentIn?, BobNetIn?,
          [output('_user', BobAgentToUser), output('_net', BobNetOut)],
          [], 1),
    ui_mediator(bob, ch(BobAgentToUser?, BobAgentIn),
                ch(BobMedIn?, BobMedOut), [], 1),
    tee(BobMedOut?, BobActorIn, BobDispNotify),
    send_to_user_tagged(bob, BobDispCmd?, BobDispNotify?).
```

### After Step 4: Type-check boot standalone

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_boot.glp\n:quit' | dart run bin/glp_repl.dart > /private/tmp/step4-typecheck.txt 2>&1
```

Read `/private/tmp/step4-typecheck.txt`. Must load cleanly. Fix any errors.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "phase8: add play10/play11 boot entries"
```

---

## Step 5: Full Load Test (all four files)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\n:quit' | dart run bin/glp_repl.dart > /private/tmp/step5-fullload.txt 2>&1
```

Read `/private/tmp/step5-fullload.txt`. All four must load without errors.

---

## Step 6: Run fplay11 (cancel — no timer wait, should be fast)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\nfplay11.\n:quit' | dart run bin/glp_repl.dart > /private/tmp/fplay11.txt 2>&1
```

Read `/private/tmp/fplay11.txt`.

**Expected output pattern for fplay11**:
- Alice side: `connect(bob)` → `credit(bob,5,0)` → `deposit_escrow(bob,[lot(bob,0,3)],T)` → `cancel_escrow(bob,req(1))` → `balance` → `done`
- Alice notifications: `connected(bob)` → `credit_opened(bob,5)` → `escrow_deposited(bob,T,req(1))` → `escrow_returned(bob)` → `balance_report([bond(bob,0,...),...])`  (5 bob-coins)
- Bob side: `decision(yes,alice,req(1))` → `accept_credit(alice,req(2))` → `balance` → `done`
- Bob notifications: `befriend(alice,req(1))` → `connected(alice)` → `credit_opened(alice,5)` → `escrow_received(alice,T)` → `escrow_cancelled(alice)` → `balance_report([bond(alice,0,...),...])`  (5 alice-coins, no bob-coins)

The play should complete. If it hangs, report the last output seen.

---

## Step 7: Run fplay10 (time release — waits ~500ms)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\nfplay10.\n:quit' | dart run bin/glp_repl.dart > /private/tmp/fplay10.txt 2>&1
```

Read `/private/tmp/fplay10.txt`.

**Expected output pattern for fplay10**:
- Alice: sees `escrow_expired(bob)`, final balance has 2 bob-coins
- Bob: sees `escrow_released(alice)`, final balance has 5 alice-coins + 3 bob-coins = 8 bonds

**Note**: fplay10 requires the `wait_until` guard to SUSPEND (not fail) until time T. If this runtime fix is not in place, the escrow clause 1 will fail immediately and clause 2 will fire instead (cancel-case fires on empty `cancel` match). Check the output to determine which case fired.

If fplay10 shows the cancel path instead of the time path, report this immediately — it means the `wait_until` runtime fix is not in place. Do NOT attempt to fix the runtime; just report. See `/Users/udi/Grassroots/GLP/docs/wait-until-audit-report.md` for context.

---

## Step 8: Run regression tests

Verify existing plays still pass:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && for P in fplay1 fplay2 fplay3 fplay4 fplay5 fplay6 fplay7 fplay8 fplay9; do echo -e "load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/regression-${P}.txt 2>&1 && echo "${P}: done" || echo "${P}: FAILED"; done
```

Read each `/private/tmp/regression-fplayN.txt` for any that show unexpected output.

Then run the full REPL test suite:
```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh > /private/tmp/final-tests.txt 2>&1
```

Read `/private/tmp/final-tests.txt`. The test count should be >= baseline (same or more).

---

## Step 9: Final commit and merge instructions

```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "phase8: escrow complete — plays 10+11 implemented and tested"
```

Push to your branch and provide Udi with the merge commands in the mandatory format.

---

## Bug Protocol Reminder

If any type error or unexpected behavior occurs:
1. STOP immediately
2. Show the full error output
3. Do NOT attempt to fix without discussion
4. Wait for Udi's direction

The one exception: if it's an obvious consequence of a typo in the instructions (e.g. mismatched arity), you may fix it and report what you changed.
