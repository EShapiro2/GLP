# Village Market Month — Scenario Specification

## Overview

A 30-day scenario demonstrating a grassroots digital economy among 6 villagers.
All financial operations use only **mint**, **trade** (accept/reject), **pay**, **redeem**,
**escrow**, and **balance**. No `credit` or `loan` commands.

Runtime: 1 day = 100ms. Month = 3 seconds. Escrow timer ~500ms.

## Cast

| Agent   | Role       | Situation                                      |
|---------|------------|-------------------------------------------------|
| Alice   | Baker      | Established, steady income, good credit         |
| Bob     | Farmer     | Needs seasonal capital for planting             |
| Charlie | Carpenter  | Established, steady work                        |
| Diana   | Doctor     | High income, liquid, acts as local lender        |
| Eve     | Teacher    | Moderate income, steady                         |
| Frank   | Fisherman  | Needs working capital for boat repair           |

## Trust Graph (8 edges)

```
Alice --- Bob --- Diana --- Frank
  |  \                   / |
Charlie ------ Eve ------+
```

Alice↔Bob (neighbors), Alice↔Charlie (Charlie built Alice's shop),
Alice↔Eve (Eve teaches in Alice's neighborhood), Bob↔Diana (Diana treats Bob's family),
Charlie↔Eve (Eve teaches Charlie's kids), Charlie↔Frank (Charlie hires Frank for dock work),
Diana↔Frank (Frank supplies fish to Diana), Eve↔Frank (cousins). 8 edges total.

---

## Act 1: Connections (Days 1–2)

All 6 pairs connect. Each pair: one sends `connect`, the other receives
`befriend` and responds `decision(yes)`. Order:

1. Alice connects to Bob
2. Alice connects to Charlie
3. Alice connects to Eve
4. Bob connects to Diana
5. Charlie connects to Eve
6. Charlie connects to Frank
7. Diana connects to Frank
8. Eve connects to Frank

---

## Act 2: Credit Formation via Trade (Days 2–4)

### 2a. Alice ↔ Bob — Symmetric exchange (peers)

> **Alice:** "Bob, let's set up a credit line — 15 coins each, so we can trade freely."
> **Bob:** "Sounds good. 15 for 15."

1. Alice: `mint(15, 0)`, Bob: `mint(15, 0)`
2. Alice: `trade(bob, [lot(alice, 0, 15)], [lot(bob, 0, 15)])`
3. Bob: `accept_trade(alice, ReqId)`

Result: Alice holds 15 bob-coins, Bob holds 15 alice-coins.

### 2b. Alice ↔ Charlie — Symmetric exchange (peers)

> **Alice:** "Charlie, same deal — 15 for 15?"
> **Charlie:** "Done."

1. Alice: `mint(15, 0)`, Charlie: `mint(15, 0)`
2. Alice: `trade(charlie, [lot(alice, 0, 15)], [lot(charlie, 0, 15)])`
3. Charlie: `accept_trade(alice, ReqId)`

Result: Alice holds 15 charlie-coins, Charlie holds 15 alice-coins.

### 2c. Bob ↔ Diana — Negotiated loan (Bob needs planting capital)

> **Diana:** "Bob, I can lend you 20 coins for planting season. I'd want 30 bonds
> back on day 25. That's 50% interest."
> **Bob:** "50% is steep, Diana. I'll give you 24 bonds maturing day 25. That's 20%."
> **Diana:** "Fair enough. 20 coins for 24 bonds."

Diana's first proposal gets rejected. Bob counteroffers.

1. Diana: `mint(20, 0)`
2. Bob: `mint(30, Day25)` — mints 30 in case Diana's proposal stands
3. Diana: `trade(bob, [lot(diana, 0, 20)], [lot(bob, Day25, 30)])`
4. Bob: `reject_trade(diana, ReqId)` — too expensive

Bob now counteroffers (he already minted the bonds, just proposes fewer):

5. Bob: `trade(diana, [lot(bob, Day25, 24)], [lot(diana, 0, 20)])`
6. Diana: `accept_trade(bob, ReqId)`

Result: Bob holds 20 diana-coins. Diana holds 24 bob-bonds(Day25).
Bob still has 6 excess bob-bonds(Day25) he minted but didn't trade — they sit in his holdings as unmoved self-bonds.

### 2d. Charlie ↔ Eve — Symmetric exchange (peers)

> **Charlie:** "Eve, 10 for 10?"
> **Eve:** "Works for me."

1. Charlie: `mint(10, 0)`, Eve: `mint(10, 0)`
2. Charlie: `trade(eve, [lot(charlie, 0, 10)], [lot(eve, 0, 10)])`
3. Eve: `accept_trade(charlie, ReqId)`

Result: Charlie holds 10 eve-coins, Eve holds 10 charlie-coins.

### 2e. Diana ↔ Frank — Negotiated loan (Frank needs boat repair money)

> **Diana:** "Frank, I hear you need money for the boat. I'll lend you 15 coins
> for 20 bonds maturing day 28."
> **Frank:** "20 for 15 is harsh. How about 18?"
> **Diana:** "Deal. 15 coins for 18 bonds, day 28."

1. Diana: `mint(15, 0)`
2. Frank: `mint(18, Day28)`
3. Diana: `trade(frank, [lot(diana, 0, 15)], [lot(frank, Day28, 18)])`
4. Frank: `accept_trade(diana, ReqId)`

Result: Frank holds 15 diana-coins. Diana holds 18 frank-bonds(Day28).

### 2f. Eve ↔ Frank — Symmetric exchange (cousins)

> **Eve:** "Frank, we're family. 10 for 10, no interest."
> **Frank:** "Of course. 10 for 10."

1. Eve: `mint(10, 0)`, Frank: `mint(10, 0)`
2. Eve: `trade(frank, [lot(eve, 0, 10)], [lot(frank, 0, 10)])`
3. Frank: `accept_trade(eve, ReqId)`

Result: Eve holds 10 frank-coins, Frank holds 10 eve-coins.

### Holdings after Act 2

| Agent   | Holdings                                          |
|---------|---------------------------------------------------|
| Alice   | 15 bob(0), 15 charlie(0)                          |
| Bob     | 15 alice(0), 20 diana(0)                          |
| Charlie | 15 alice(0), 10 eve(0)                            |
| Diana   | 24 bob(Day25), 18 frank(Day28)                    |
| Eve     | 10 charlie(0), 10 frank(0)                        |
| Frank   | 15 diana(0), 10 eve(0)                            |

Diana is pure investor — holds only future bonds. Everyone else has spendable coins.

---

## Act 3: Commerce (Days 5–6)

### 3a. Bob buys bread from Alice

> **Bob:** "Alice, 5 loaves please."
> **Alice:** "That'll be 5 coins."

Bob: `pay(alice, 5)` — sends 5 alice-coins back to Alice.

### 3b. Frank pays Diana for medical checkup

> **Frank:** "Diana, my shoulder's been hurting. Can you look at it?"
> **Diana:** "Of course. 3 coins."

Frank: `pay(diana, 3)` — sends 3 diana-coins back to Diana.

### 3c. Eve buys a shelf from Charlie

> **Eve:** "Charlie, I need a bookshelf for the classroom."
> **Charlie:** "That'll be 6 coins."

Eve: `pay(charlie, 6)` — sends 6 charlie-coins back to Charlie.

### Holdings after Act 3

| Agent   | Holdings                                          |
|---------|---------------------------------------------------|
| Alice   | 15 bob(0), 15 charlie(0), 5 alice(0)              |
| Bob     | 10 alice(0), 20 diana(0)                          |
| Charlie | 15 alice(0), 10 eve(0), 6 charlie(0)              |
| Diana   | 24 bob(Day25), 18 frank(Day28), 3 diana(0)        |
| Eve     | 4 charlie(0), 10 frank(0)                         |
| Frank   | 12 diana(0), 10 eve(0)                            |

Diana now has 3 spendable diana-coins (earned from Frank's payment).

---

## Act 4: Trade — Portfolio Rebalancing (Day 7)

### 4a. Eve wants alice-coins to buy bread, trades with Charlie

> **Eve:** "Charlie, I want to buy bread from Alice but I don't have alice-coins.
> You have 15. Can we swap? I'll give you 5 frank-coins for 5 alice-coins."
> **Charlie:** "I could use frank-coins — I might need fish. Deal."

1. Eve: `trade(charlie, [lot(frank, 0, 5)], [lot(alice, 0, 5)])`
2. Charlie: `accept_trade(eve, ReqId)`

Result: Eve gains 5 alice-coins, Charlie gains 5 frank-coins.

### 4b. Eve buys bread from Alice

> **Eve:** "Alice, 3 loaves please."
> **Alice:** "3 coins."

Eve: `pay(alice, 3)` — sends 3 alice-coins to Alice.

### Holdings after Act 4

| Agent   | Holdings                                                    |
|---------|-------------------------------------------------------------|
| Alice   | 15 bob(0), 15 charlie(0), 5 alice(0), 3 alice(0)           |
| Bob     | 10 alice(0), 20 diana(0)                                    |
| Charlie | 10 alice(0), 10 eve(0), 6 charlie(0), 5 frank(0)           |
| Diana   | 24 bob(Day25), 18 frank(Day28), 3 diana(0)                  |
| Eve     | 4 charlie(0), 5 frank(0), 2 alice(0)                        |
| Frank   | 12 diana(0), 10 eve(0)                                      |

---

## Act 5: Escrow — Conditional Delivery (Day 8)

Charlie pre-pays Frank for custom fishing dock construction.
If Frank delivers, timer expires and Frank gets the coins. If he doesn't,
Charlie cancels and gets his coins back.

> **Charlie:** "Frank, I want you to build a fishing dock at my workshop.
> I'll put 8 alice-coins in escrow. They release to you on day 15.
> If the dock isn't done by then, I cancel."
> **Frank:** "Fair. I can finish in a week."

1. Charlie: `deposit_escrow(frank, [lot(alice, 0, 8)], Day15)`
2. Charlie receives `escrow_deposited(frank, Day15, ReqId)` with cancel signal
3. Frank receives `escrow_received(charlie, Day15)`

Timer ticks... Frank builds the dock. Charlie doesn't cancel.

Day 15 arrives:
4. Frank receives `escrow_released(charlie)` — gets 8 alice-coins
5. Charlie receives `escrow_expired(frank)` — dock is done, payment released

### Holdings after Act 5

| Agent   | Holdings                                                    |
|---------|-------------------------------------------------------------|
| Charlie | 2 alice(0), 10 eve(0), 6 charlie(0), 5 frank(0)            |
| Frank   | 12 diana(0), 10 eve(0), 8 alice(0)                          |
| (others unchanged)                                                        |

---

## Act 6: Redemption — Risk Management (Day 12)

Frank worries about holding too many diana-coins. Diana's wealthy but
Frank wants to diversify. He redeems some from Diana.

> **Frank:** "Diana, I'd like to redeem 5 of your coins."

Frank: `redeem(diana, 5, 0)` — sends 5 diana-coins to Diana, requesting bonds back.

Diana holds: 24 bob(Day25), 18 frank(Day28), 3 diana(0).
Diana returns 5 bonds from her holdings. The implementation selects bonds with
maturity ≥ 0 from Diana's holdings — she returns what she has (bob-bonds or her
own diana-coins).

Result: Frank reduces diana-exposure, gains whatever Diana returns.

---

## Act 7: Sale of Debt (Day 18)

Alice holds 15 bob-bonds. She's heard Bob might have trouble this season.
She wants to sell some bob-debt to Eve at a discount.

> **Alice:** "Eve, I have 10 bob-bonds. I'll sell them to you for 7 frank-coins.
> That's a 30% discount — I think he's good for it, but I want to reduce exposure."
> **Eve:** "Hmm, 7 is a lot. I'll give you 5 frank-coins for the 10 bob-bonds."
> **Alice:** "Let's split the difference. 6 frank-coins for 10 bob-bonds."
> **Eve:** "Deal."

Negotiation:
1. Alice: `trade(eve, [lot(bob, 0, 10)], [lot(frank, 0, 7)])`
2. Eve: `reject_trade(alice, ReqId)` — too expensive
3. Eve: `trade(alice, [lot(frank, 0, 5)], [lot(bob, 0, 10)])`
4. Alice: `reject_trade(eve, ReqId)` — too cheap
5. Alice: `trade(eve, [lot(bob, 0, 10)], [lot(frank, 0, 6)])`

Wait — does Eve have 6 frank-coins? After Act 4, Eve has 5 frank(0).
Let me adjust: Alice asks for 5 first, Eve counters 3, they agree on 4.

5. Alice: `trade(eve, [lot(bob, 0, 10)], [lot(frank, 0, 5)])`
6. Eve: `reject_trade(alice, ReqId)`
7. Eve: `trade(alice, [lot(frank, 0, 3)], [lot(bob, 0, 10)])`
8. Alice: `reject_trade(eve, ReqId)`
9. Alice: `trade(eve, [lot(bob, 0, 10)], [lot(frank, 0, 4)])`
10. Eve: `accept_trade(alice, ReqId)`

Result: Alice gets 4 frank-coins (sold bob-debt at 60% discount).
Eve gets 10 bob-bonds (bought cheap, bets on Bob being good for it).

---

## Act 8: More Commerce (Day 20)

### 8a. Diana buys fish from Frank

Diana earned 3 diana-coins from Frank's payment (Act 3) plus got bonds
from redemption. Now she wants fresh fish.

> **Diana:** "Frank, I'd like 3 fish for dinner."
> **Frank:** "3 coins please."

But Diana needs frank-coins or eve-coins to pay Frank. She has 3 diana(0)
and some bonds from the redemption in Act 6. If she got bob-bonds back
from the redemption, she can't pay Frank with those.

Alternative: Diana pays Frank with diana-coins. But `pay(frank, 3)` selects
frank-coins from Diana's holdings. Diana may not have frank-coins anymore.

Let me adjust the story. Diana trades with Frank instead: she offers
diana-coins for eve-coins Frank holds, then uses eve-coins to pay.

Actually simpler: Frank accepts diana-coins as payment because he can
redeem them from Diana later. But `pay` requires coins of the target's
issuer. So Diana can't `pay(frank, ...)` with diana-coins.

Better approach: Diana offers to trade diana-coins for fish. In the
implementation, this is a trade where Diana gives diana-coins and gets...
nothing? No, trade requires both sides to exchange bonds.

In practice, Diana would give 3 diana-coins to Frank as payment. Since
Frank can redeem them, they're as good as cash. But the implementation's
`pay` command selects Target-coins. So Diana would need frank-coins.

Let me adjust: Skip this commerce. Or use a trade where Diana gives
3 diana-coins and Frank gives 3 eve-coins (which Diana can use elsewhere).

> **Diana:** "Frank, how about this — I give you 3 diana-coins, you give me
> 3 eve-coins. The diana-coins are worth the same. And I'll take 3 fish."
> **Frank:** "Fine. You're a good customer."

1. Diana: `trade(frank, [lot(diana, 0, 3)], [lot(eve, 0, 3)])`
2. Frank: `accept_trade(diana, ReqId)`

Diana gets 3 eve-coins. Frank gets 3 diana-coins. The fish is the
real-world good that motivates the trade.

---

## Act 9: Final Balances (Day 28)

Everyone checks their balance. Each agent: `balance` then `done`.

---

## Summary of Instruments Demonstrated

| Instrument      | Where                                  |
|-----------------|----------------------------------------|
| Mint            | Acts 2 (all formation)                 |
| Symmetric swap  | Acts 2a, 2b, 2d, 2f                   |
| Asymmetric swap | Acts 2c, 2e (loans with interest)      |
| Negotiation     | Acts 2c, 2e, 7 (reject + counterpropose) |
| Payment         | Acts 3a, 3b, 3c, 4b                   |
| Trade/rebalance | Act 4a                                 |
| Escrow          | Act 5 (time-release)                   |
| Redemption      | Act 6                                  |
| Sale of debt    | Act 7 (discounted trade)               |

## Implementation Notes

- Maturities: Use small values. Day25 = T+2500, Day28 = T+2800 at 100ms/day.
  Or just use abstract values (25, 28) if maturity arithmetic isn't time-triggered.
  Only the escrow timer is real-time.
- Escrow release: now + 700ms (Day 15 minus Day 8 = 7 days = 700ms).
  Short enough for the demo to not stall.
- Bob's rejected trade in Act 2c: Bob minted 30 bob-bonds(Day25) but only
  traded 24. The 6 excess self-bonds remain in his holdings — harmless.
- network6: 61 clauses routing messages among 6 agents.
- Flutter app: 6 panels, one per agent.
