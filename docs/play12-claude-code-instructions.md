# Claude Code Instructions: Play 12 — Village Market Month

## Mandatory Reading

Same startup sequence as always (CLAUDE.md → DISCIPLINE.md → manual → cheat sheet).
Then read these additional files before starting:

1. `docs/village-market-scenario.md` — the full scenario spec with dialogues
2. `programs/typed_book/bonds/bond_actors.glp` — existing actors (all plays 1–11)
3. `programs/typed_book/bonds/bond_boot.glp` — existing boot (network2, play/fplay wiring)
4. `programs/typed_book/cssn/play_ui_sim_boot.glp` — **network3 analogue** (3-way routing)
5. `glp_runtime/lib/multiagent/repl_play_runner.dart` — has `bondsFiles`
6. `glp_multiagent/lib/main_bonds.dart` — current Flutter app (2 panels)

## Overview

Add one new play (play12 / fplay12) with 6 agents and a `network6` router.
The scenario is in `docs/village-market-scenario.md`. This play demonstrates
the full grassroots bond economy: mint, trade (with reject/counter-propose
negotiations), payment, redemption, escrow, and balance — using 6 agents.

Four deliverables, in order:
1. Add `network6` to `bond_boot.glp`
2. Add play12 actors to `bond_actors.glp`
3. Add play12/fplay12 boot entries to `bond_boot.glp`
4. Update `main_bonds.dart` to 6 panels

Type-check after each file change. Do NOT proceed if type-check fails.

---

## Deliverable 1: network6 in bond_boot.glp

### Analogue: `network3` in `programs/typed_book/cssn/play_ui_sim_boot.glp`

`network3` has 6 channels (3 agents × 2 directions) and routes by pattern-matching
the destination in the message head. Two message types:
- Cold-call (2-arg): `msg(Dest, Content)` — route to Dest
- Friend-to-friend (3-arg): `msg(From, To, Content)` — route to To

`network6` has the same structure but with 6 agents:
`alice, bob, charlie, diana, eve, frank`.

Signature:
```
network6(ch(AliceOut?, AliceIn), ch(BobOut?, BobIn),
         ch(CharlieOut?, CharlieIn), ch(DianaOut?, DianaIn),
         ch(EveOut?, EveIn), ch(FrankOut?, FrankIn))
```

Each agent can send to 5 others. That's 6×5 = 30 cold-call clauses + 30 friend clauses + 1 termination = 61 clauses. Generate them mechanically from the `network3` pattern:

For each sending agent S and destination agent D (S ≠ D):
- Cold-call: match `msg(D, X)` on S's output, deliver to D's input
- Friend: match `msg(S, D, X)` on S's output, deliver to D's input

All other channels pass through unchanged (reader on out, same var on in).
Recursive call at the end of each clause.

Termination: `network6(ch([], []), ch([], []), ch([], []), ch([], []), ch([], []), ch([], []))`.

**Important**: `network6` is untyped (no procedure declaration) — same as `network3`.

Add `network6` at the END of `bond_boot.glp`, before any play12 entries.

### Type-check

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_boot.glp\n:quit' | dart run bin/glp_repl.dart
```

Commit: `git add -A && git commit -m "play12: add network6 (6-way message router)"`

---

## Deliverable 2: Play12 actors in bond_actors.glp

### Analogues

- `alice_p9` / `bob_p9` in `bond_actors.glp` — trade with accept (play 9)
- `alice_p11` / `bob_p11` — escrow with cancel (play 11)
- `alice_p10` / `bob_p10` — escrow with time release (play 10)

### Actor pattern

Every actor is a state machine: `procedure name(args). name(match, output) :- next.`
Each state waits for a notification, emits a command, transitions to next state.
The `[_|In]` + `otherwise` clause skips irrelevant notifications.
The `[]` clause terminates.

### The 6 actors

Read `docs/village-market-scenario.md` for the exact sequence each agent performs.
Here's the compressed command/wait sequence for each:

**Alice** (baker, initiates connections to Bob, Charlie, and Eve):
```
connect(bob) → wait connected(bob)
connect(charlie) → wait connected(charlie)
connect(eve) → wait connected(eve)
mint(15, 0) → wait minted(15, 0)
trade(bob, [lot(alice,0,15)], [lot(bob,0,15)]) → wait trade_completed(bob)
mint(15, 0) → wait minted(15, 0)
trade(charlie, [lot(alice,0,15)], [lot(charlie,0,15)]) → wait trade_completed(charlie)
wait payment_received(bob, 5)                          %% Act 3a: Bob buys bread
wait trade_proposed(eve, ...) → accept_trade(eve, ReqId)  %% Act 4a: Eve trades for alice-coins
wait payment_received(eve, 3)                          %% Act 4b: Eve buys bread
trade(eve, [lot(bob,0,10)], [lot(frank,0,5)]) → wait trade_completed(eve) OR wait trade_returned  %% Act 7: sale of debt (first proposal)
  ... negotiation: reject/re-propose may happen ...
balance → wait balance_report(_) → done
```

**Bob** (farmer, needs planting capital):
```
wait befriend(alice, ReqId) → decision(yes, alice, ReqId)
wait connected(alice)
mint(15, 0) → wait minted                  %% for Alice symmetric trade
wait trade_proposed(alice, ...) → accept_trade(alice, ReqId)  %% symmetric credit
connect(diana) → wait connected(diana)      %% connect to lender
wait trade_proposed(diana, ...) → reject_trade(diana, ReqId)  %% reject Diana's 50% offer
mint(24, 25) → wait minted                  %% mint bonds for counter-proposal
trade(diana, [lot(bob,25,24)], [lot(diana,0,20)]) → wait trade_completed(diana)  %% counter at 20%
pay(alice, 5) → wait paid(alice, 5)          %% Act 3a: bread
balance → wait balance_report(_) → done
```

Note: maturity values 25 and 28 are constants (bond labels), not real timestamps.
Only the escrow timer T is a real timestamp (now + 700ms).

**Charlie** (carpenter, peer exchange with Eve, escrow with Frank):
```
wait befriend(alice, ReqId) → decision(yes, alice, ReqId)
wait connected(alice)
connect(eve) → wait connected(eve)
connect(frank) → wait connected(frank)
mint(15, 0) → wait minted           %% for Alice trade
wait trade_proposed(alice,...) → accept_trade
mint(10, 0) → wait minted           %% for Eve trade
trade(eve, [lot(charlie,0,10)], [lot(eve,0,10)]) → wait trade_completed(eve)
wait payment_received(eve, 6)       %% Act 3c: bookshelf
deposit_escrow(frank, [lot(alice,0,8)], Day15) → wait escrow_deposited(frank,...)
wait escrow_expired(frank)          %% Act 5: dock delivered
balance → wait balance_report(_) → done
```

**Diana** (doctor, lender):
```
wait befriend(bob, ReqId) → decision(yes, bob, ReqId)
wait connected(bob)
connect(frank) → wait connected(frank)
mint(20, 0) → wait minted
trade(bob, [lot(diana,0,20)], [lot(bob,25,30)]) → wait trade_returned(bob)  %% rejected!
wait trade_proposed(bob,...) → accept_trade(bob, ReqId)  %% accept Bob's 24-bond counter
mint(15, 0) → wait minted
trade(frank, [lot(diana,0,15)], [lot(frank,28,18)]) → wait trade_completed(frank)
wait payment_received(frank, 3)     %% Act 3b: medical checkup
wait redeem_received(frank, 5)      %% Act 6: Frank redeems
balance → wait balance_report(_) → done
```

**Eve** (teacher, peers with Alice, Charlie, and Frank):
```
wait befriend(alice, ReqId) → decision(yes, alice, ReqId)
wait connected(alice)
wait befriend(charlie, ReqId) → decision(yes, charlie, ReqId)
wait connected(charlie)
connect(frank) → wait connected(frank)
mint(10, 0) → wait minted
wait trade_proposed(charlie,...) → accept_trade  %% symmetric with Charlie
mint(10, 0) → wait minted
trade(frank, [lot(eve,0,10)], [lot(frank,0,10)]) → wait trade_completed(frank)
pay(charlie, 6) → wait paid(charlie, 6)     %% Act 3c: bookshelf
trade(charlie, [lot(frank,0,5)], [lot(alice,0,5)]) → wait trade_completed(charlie)  %% Act 4a
pay(alice, 3) → wait paid(alice, 3)         %% Act 4b: bread
wait trade_proposed(alice,...) → reject_trade(alice, ReqId)  %% Act 7: too expensive
trade(alice, [lot(frank,0,3)], [lot(bob,0,10)]) → wait trade_returned(alice)  %% counter rejected
wait trade_proposed(alice,...) → accept_trade(alice, ReqId)  %% Act 7: final deal
balance → wait balance_report(_) → done
```

**Frank** (fisherman, needs boat repair capital):
```
wait befriend(charlie, ReqId) → decision(yes, charlie, ReqId)
wait connected(charlie)
wait befriend(diana, ReqId) → decision(yes, diana, ReqId)
wait connected(diana)
wait befriend(eve, ReqId) → decision(yes, eve, ReqId)
wait connected(eve)
mint(18, 28) → wait minted
wait trade_proposed(diana,...) → accept_trade(diana, ReqId)  %% accept 15-for-18 loan
mint(10, 0) → wait minted
wait trade_proposed(eve,...) → accept_trade(eve, ReqId)  %% symmetric with Eve
pay(diana, 3) → wait paid(diana, 3)         %% Act 3b: medical checkup
wait escrow_received(charlie, Day15)        %% Act 5
wait escrow_released(charlie)               %% dock delivered
redeem(diana, 5, 0) → wait redeemed(diana, 5)  %% Act 6
balance → wait balance_report(_) → done
```

### Timing/ordering constraint

All actors run concurrently. The scenario is ordered by data dependencies:
an actor blocks in a `wait` state until the expected notification arrives.
The `[_|In] :- otherwise` clauses skip unexpected notifications.

The escrow timer Day15 should be `now + 700` (700ms = 7 days at 100ms/day).
Pass `T` (the escrow release time) as a parameter to actors that need it,
same as play10/play11 pass `T` to `alice_p10(T, ...)`.

### Naming

Actors: `alice_p12(T, ActorChannel?)`, `bob_p12(ActorChannel?)`, etc.
States: `alice_p12_wait_connected`, `alice_p12_wait_minted`, etc.

### Negotiation pattern (new — no existing analogue)

When Diana proposes a trade to Bob and Bob rejects, Diana sees `trade_returned(bob)`
(her bonds come back). Then Bob sends a counter-proposal, and Diana sees
`trade_proposed(bob, ...)` and accepts. This is just the normal trade flow:
proposer gets `trade_returned` on reject, `trade_completed` on accept.
Responder gets `trade_proposed` and chooses `accept_trade` or `reject_trade`.

The Act 7 negotiation (Alice↔Eve, 2 rejections) is the same pattern repeated:
Alice proposes → Eve rejects → Eve counter-proposes → Alice rejects →
Alice re-proposes → Eve accepts.

### Type-check

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_actors.glp\n:quit' | dart run bin/glp_repl.dart
```

Commit: `git add -A && git commit -m "play12: add 6-agent village market actors"`

---

## Deliverable 3: play12 / fplay12 boot entries in bond_boot.glp

### Analogue: play10/fplay10 in `bond_boot.glp`

Same wiring pattern but with 6 agents instead of 2, and `network6` instead of `network2`.

Each agent block is identical in structure:
```
agent_p12(..., ch(AgentActorIn?, AgentActorOut)),
tee(AgentActorOut?, AgentMedIn, AgentDispCmd),
agent(name, AgentAgentIn?, AgentNetIn?,
      [output('_user', AgentAgentToUser), output('_net', AgentNetOut)],
      [], 1),
ui_mediator(name, ch(AgentAgentToUser?, AgentAgentIn),
            ch(AgentMedIn?, AgentMedOut), [], 1),
tee(AgentMedOut?, AgentActorIn, AgentDispNotify),
sink(AgentDispCmd?), sink(AgentDispNotify?),   %% play12
%% OR: send_to_user_tagged(name, AgentDispCmd?, AgentDispNotify?)  %% fplay12
```

Parameters:
```
play12 :-
    now(Now),
    T := Now? + 700,     %% escrow release time (~7 days)
    network6(ch(AliceNetOut?, AliceNetIn),
             ch(BobNetOut?, BobNetIn),
             ch(CharlieNetOut?, CharlieNetIn),
             ch(DianaNetOut?, DianaNetIn),
             ch(EveNetOut?, EveNetIn),
             ch(FrankNetOut?, FrankNetIn)),
    %% ... 6 agent blocks ...
```

Alice and Charlie need T (for escrow). Others don't.

### Type-check (full load)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\n:quit' | dart run bin/glp_repl.dart
```

### Run test

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e 'load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\nfplay12.\n:quit' | dart run bin/glp_repl.dart
```

Expected: tagged output from all 6 agents showing the full scenario flow.
If it hangs, report last output seen.

### Regression

```bash
for P in fplay1 fplay2 fplay3 fplay4 fplay5 fplay6 fplay7 fplay8 fplay9 fplay10 fplay11; do echo -e "load ../programs/typed_book/bonds/bond_agent.glp\nload ../programs/typed_book/bonds/bond_mediator.glp\nload ../programs/typed_book/bonds/bond_actors.glp\nload ../programs/typed_book/bonds/bond_boot.glp\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/regression-${P}.txt 2>&1 && echo "${P}: done" || echo "${P}: FAILED"; done
```

Commit: `git add -A && git commit -m "play12: add play12/fplay12 boot (6-agent village market)"`

---

## Deliverable 4: Update Flutter app to 6 panels

### File: `glp_multiagent/lib/main_bonds.dart`

### Analogue: `glp_multiagent/lib/main_cssg.dart` — has 4 panels

Changes:
1. Add 4 more agents to `_agentInfos` (currently has Alice, Bob):
   - Charlie (amber), Diana (purple), Eve (pink), Frank (orange) — pick distinct colors
2. Add Play 12 button to control bar (primary, with label "Play 12 (Village)")
3. Keep existing play buttons for 1–11

### Build and test

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_bonds.dart
open build/macos/Build/Products/Release/glp_multiagent.app
```

Click "Play 12 (Village)" — should see 6 panels with tagged output.

Commit: `git add -A && git commit -m "play12: update Flutter app to 6 agent panels"`

---

## Bug Protocol

Same as phase 8: STOP on errors, show full output, do NOT fix without discussion.
Exception: obvious typos from these instructions.
