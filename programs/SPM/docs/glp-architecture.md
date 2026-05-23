# GLP Architecture for the SPM Paper

**Status:** draft v2 for review
**Date:** 2026-05-23
**Scope:** GLP implementation of the SPM paper's CVA protocols — clean GSG, secure GSG with Restore and Replace, secure bonds and coins.

This document specifies the GLP architecture by which the paper's CVA-level transactions become GLP processes.  The paper (`/Users/udi/Grassroots/SPM/`) is the semantic spec; this document is the implementation spec.  All paper-level claims, invariants, theorems, and transaction semantics are inherited from the paper unchanged.

References to the paper are by section number, e.g. §5.2 = `sections/cva.tex` Definition 5.2.  Spec resolutions live in `/SPM/docs/spec-issues.md`.

---

## 1. Purpose and scope

The CVA model (§5) is an abstract specification: agents hold inbox and outbox **sets**, react to messages whose preconditions hold by computing a state update and adding to the outbox, and advance a local date.  Inboxes are monotone non-decreasing (set semantics; messages are not consumed); a reactive transaction is rendered inert after firing because its precondition becomes false (idempotence by precondition).  GLP is concurrent committed-choice logic programming: agents are GLP processes that read stream variables, with three-valued guards selecting clauses.

The translation is direct enough that the paper's text can be read off the GLP code, but a fixed convention is needed for: how an agent process is shaped, how the network delivers messages, how the CVA set inbox is realised over FIFO streams, how reactive vs.\ volitional transactions translate to clauses, how dates are advanced, how faults are injected.  This document fixes that convention.

What this document does NOT do:
- Re-derive any property of the protocols.  Properties are in the paper.
- Pin down the platform-specific clauses.  Those follow per-section in the paper (§6, §8, §10) and are translated mechanically against the conventions below.
- Specify clause bodies.  The first code artifact is `self.glp` files containing type definitions and procedure declarations only; clause bodies are written and reviewed after.

---

## 2. CVA → GLP mapping

### 2.1 Per-agent process

Each CVA agent `p` is one GLP process.  The local state of §5.1 — `(known, outbox, inbox, platform, date)` — maps to GLP as follows.

| CVA local state | GLP realisation |
|---|---|
| `known_p ⊆ Π` | a field of platform state (set of agent ids) |
| `o_p` (outbox) | an output stream `NetOut` of `msg(Sender, Recipient, Cargo)` terms |
| `i_p` (inbox, set) | a field `Inbox` of platform state (set of `msg(Sender, Cargo)` terms), grown by absorbing messages from the input stream `NetIn`; never shrunk |
| `a_p` (platform state) | the platform-specific components of `Platform`, defined per protocol |
| `t_p` (date) | an integer `Date` argument, advanced on `tick` events from the test harness |
| volitional state | a field `Volitions` of platform state (set of pending volition terms) |
| identity record `IR_p` | a separate `IRec` argument carrying `ir(KSet, Sigma)`; survives crash |

The standing agent procedure has signature:

```glp
agent(Id, IRec, UserIn, NetIn, NetOut, Date, Platform)
```

with:
- `Id` the agent's constant identifier, ground throughout.
- `IRec` the intrinsic identity record, ground throughout (preserved across crash).
- `UserIn` an input stream of person-originated terms (volitions, queries, ticks, crashes — see §2.5 of this document).
- `NetIn` an input stream of `msg(Sender, Cargo)` terms delivered by the network.
- `NetOut` an output stream of `msg(Sender, Recipient, Cargo)` terms going to the network.
- `Date` the current local date, an integer.
- `Platform` the per-platform state, opaque to the CVA substrate.  Includes `Known`, `Inbox`, `Volitions`, the platform-specific fields, and any per-platform out-of-order retention buffer (see §3.4).

Each clause of `agent/7` is either an input-absorption clause (head matches a `UserIn` or `NetIn` head term and updates Platform) or a transaction-firing clause (no head consumption from any stream; guard checks Inbox/Volitions/Platform and fires the matching CVA transaction).  See §3 for the rule.

### 2.2 Network mediator (paper's Communicate)

CVA Communicate (§5.2) moves a message from one agent's outbox to another's inbox.  In GLP this is a `network/1` process taking a list of per-agent endpoints:

```glp
network(NetEnds)
```

where `NetEnds = [net_end(Id, AgentNetOut, AgentNetIn, DropPolicy) | …]`.  For each agent the mediator reads from its `AgentNetOut` and routes each `msg(Id, Recipient, Cargo)` to the matching `Recipient`'s `AgentNetIn` as `msg(Id, Cargo)`.

The mediator is FIFO per (sender, recipient) pair.  This matches the paper's bonds discipline (Append-and-acknowledge expects in-order block delivery — §10.3) and is sound for GSG (the paper's set semantics is preserved; FIFO only restricts reachability).

Delivery policy is per-endpoint:
- **Reliable** (default for bonds): every message routed.
- **Lossy** (configurable for GSG plays exercising §8.6 message-loss convergence): mediator may drop selected messages per a scripted `DropPolicy`.

Both A and B were closed (per spec-issues.md): set inbox, read-don't-consume.  The mediator does not duplicate or reorder messages; the absorption convention (§3.1 below) realises set semantics on top of FIFO delivery.

### 2.3 Date and Advance-date

CVA Advance-date (§5.2) is unary and always enabled; the paper relies on it firing infinitely often along any correct run to drive periodic re-broadcast (§8.5).

GLP has no real time, so the test play injects `tick` events on each agent's `UserIn` stream.  An `agent/7` clause matches `tick` at the head of `UserIn` and recurses with `Date + 1`.  The play's tick schedule (e.g. one tick per N messages processed, or a `tick_all` action) is the operational substitute for "infinitely often".

This is a deliberate implementation choice: by making date advance external, the test harness controls exactly when periodic behaviour fires, which makes test plays deterministic.  CVA correctness is preserved because the paper's only requirement is "advances eventually" (§5.2), which the play schedule satisfies.

### 2.4 Discover and the known set

CVA Discover (§5.2) is binary, guarded by `{p}`, and only sets `known'_p := known_p ∪ {q}`.  In GLP it is realised as a volitional transaction at `p` whose effect is on the `Known` field of Platform.  The person's volition arrives as `will(discover(q))` on `UserIn`; the volition lives in `Volitions` until the agent fires the discover transaction (which has no further precondition).

### 2.5 Person / UserIn

The person behind agent `p` is modelled as a stream of UserIn events:

| UserIn event | Effect on the agent |
|---|---|
| `will(V)` | add volition `V` to `Volitions` |
| `unwill(V)` | remove volition `V` from `Volitions` |
| `tick` | `Date := Date + 1`; may enable periodic re-broadcast |
| `crash` | trigger fault harness (§4) |
| `query(Q, A)` | read-only inspection: bind `A` to the answer; no protocol effect |

Each is a clause of `agent/7`.  No CVA-level meaning is attached except `will`/`unwill` (paper §2.4 change-volition transactions) and `tick` (Advance-date); `crash` and `query` are implementation-level only.

Volitions are stateful: a `will(V)` whose corresponding transaction cannot fire now (state precondition false) stays in `Volitions` until either the state changes and the transaction fires, or the person sends `unwill(V)`.  This is the faithful translation of paper §2.4: a volition is consumed only when the matching transaction fires, never by silent expiry.

---

## 3. Per-transaction translation rule

### 3.1 The agent step pattern

The agent loops in two interleaved modes.  At any step the agent does one of:

(a) **Absorb** the head of `UserIn` or `NetIn`:
- `UserIn` head `will(V)` / `unwill(V)`: mutate `Volitions`.
- `UserIn` head `tick`: increment `Date`.
- `UserIn` head `crash` / `query`: handle per harness (§4) or per platform (read-only).
- `NetIn` head `msg(S, C)`: add to `Inbox` (or to platform-specific retention buffer; §3.4).

(b) **Fire** a CVA platform transaction whose preconditions hold against the current Platform.

GLP committed choice selects one of (a) or (b) per recursion, with absorption clauses generally taking priority when input is available (an absorption clause's head pattern matches; a firing clause's guard is checked against state).  After every step the agent recurses with the updated state.

The agent terminates only when `UserIn = []` and no firing clause matches (see §3.6).

### 3.2 Reactive transactions (firing clauses)

A CVA reactive transaction has form

> *provided `message(s, p, c) ∈ i_p` and `precondition(a_p)`, do effect on `a_p` and add messages to `o_p`*

It translates to a firing clause of `agent/7`:

```glp
agent(Id, IRec, UserIn, NetIn, NetOut, Date, Platform) :-
    contains_msg(Platform?, msg(S, C), Platform1?),
    precondition(S?, C?, Platform1?) |
    apply_effect(S?, C?, Platform1?, Platform2, NetOut, NetOut1),
    agent(Id?, IRec?, UserIn?, NetIn?, NetOut1, Date?, Platform2?).
```

Conventions:
- `contains_msg/3` is a defined helper that finds a matching message in `Inbox` and returns the (logical) "inbox without that message".  Under the paper's set semantics the message is *retained* in `i_p`, but its precondition is rendered inert after firing, so for implementation purposes we may remove it.  Removal is safe when the precondition is rendered false by the same step's state update (e.g. an epoch advances, a frontier advances, a domain-membership becomes true); this is the case for every reactive transaction in the paper.
- Guard checks the platform-level precondition.  Three-valued: if true, fire; if false, try next clause; if suspended, wait.
- Effect on `a_p` is realised by computing `Platform2` from `Platform1`.
- Adding messages to `o_p` is realised by writing them at the head of `NetOut`.

### 3.3 Volitional transactions (firing clauses)

A CVA volitional transaction has form

> *guarded by `{p}`, for `q ∈ known_p` with `precondition(a_p, q)`, do effect on `a_p` and add messages to `o_p`*

It translates to a firing clause that consults `Volitions`:

```glp
agent(Id, IRec, UserIn, NetIn, NetOut, Date, Platform) :-
    contains_volition(Platform?, offer_friendship(Q), Platform1?),
    is_known(Q?, Platform1?), epoch_even(Q?, Platform1?) |
    apply_offer(Q?, Platform1?, Platform2, NetOut, NetOut1),
    agent(Id?, IRec?, UserIn?, NetIn?, NetOut1, Date?, Platform2?).
```

`contains_volition/3` finds and removes the volition from `Volitions` (per the paper's "discharged upon satisfaction" rule, §2.4).  If the precondition fails the clause doesn't fire and the volition stays in `Volitions`.  If the person sends `unwill(V)` later, the volition is removed without firing.

### 3.4 Out-of-order retention (bonds-specific)

GSG reactive transactions have *monotone* preconditions: once false they stay false (e.g. a `stream_update(f, x)` with `x ≤ epoch_r(q, f)` is permanently moot).  GSG can therefore drop a message whose precondition fails on first encounter and rely on §8.6 message-loss convergence via periodic re-broadcast.

Bonds Append-and-acknowledge has a precondition `n = |L̂_c[s]| + 1` that may *become* true later: if `block(3, τ)` arrives before `block(2, τ)`, the precondition fails now but holds after the predecessor arrives (§10.3 explicitly: "A block with `n > |L̂_c[s]| + 1` is retained in `i_c` until its predecessors arrive").

The implementation honours this by giving the bonds Platform a `Pending` buffer.  The NetIn absorption clause for bonds:

```glp
agent(..., NetIn, ..., Platform) :- NetIn = [msg(S, block(N, Tau)) | NetIn1] |
    add_to_inbox_or_pending(S?, block(N?, Tau?), Platform?, Platform1?),
    agent(..., NetIn1?, ..., Platform1?).
```

decides between `Inbox` and `Pending` per the precondition.  After Append-and-acknowledge fires and advances the frontier, the firing clause's body scans `Pending` for any block now at the frontier and re-inserts it into `Inbox` (or fires it directly).

`Pending` is a bonds-only concession; GSG's Platform has no analogue.

### 3.5 Worked example: Offer friendship (§6.3)

Paper:

> **Offer friendship**.  A unary transaction at `p`, guarded by `{p}`, for `q ∈ known_p` such that `epoch_p(q)` is even:
> - Let `x := epoch_p(q) + 1`.
> - Add `message(p, q, friend_request(x))` to `o_p`.

GLP firing clause (shape; final form once types are fixed in §6):

```glp
agent(Id, IRec, UserIn, NetIn,
      [msg(Id?, Q?, friend_request(X?)) | NetOut?],
      Date, Platform) :-
    contains_volition(Platform?, offer_friendship(Q), Platform1?),
    is_known(Q?, Platform1?),
    epoch(Q?, Platform1?, E?), even(E?), X := E? + 1 |
    agent(Id?, IRec?, UserIn?, NetIn?, NetOut, Date?, Platform1?).
```

Per §6.3 Offer friendship does not modify `FMap` itself; only `Volitions` shrinks via `contains_volition`.

### 3.6 Worked example: Integrate accept (§6.3)

Paper:

> **Integrate accept**.  Provided `message(q, p, accept(x)) ∈ i_p`, `epoch_p(q)` even and `epoch_p(q) < x`:
> - Set `FMap_p[q] := x`.
> - Broadcast `stream_update(q, x)` to `Rec_p` (after update); send snapshot to `q`.

GLP firing clause:

```glp
agent(Id, IRec, UserIn, NetIn, NetOut, Date, Platform) :-
    contains_msg(Platform?, msg(Q, accept(X)), Platform1?),
    epoch(Q?, Platform1?, E?), even(E?), E? < X? |
    set_epoch(Q?, X?, Platform1?, Platform2?),
    broadcast_update(Id?, Q?, X?, Platform2?, NetOut, NetOut1),
    send_snapshot(Id?, Q?, Platform2?, NetOut1, NetOut2),
    agent(Id?, IRec?, UserIn?, NetIn?, NetOut2, Date?, Platform2?).
```

### 3.7 Termination

`agent/7` terminates when no firing clause matches and `UserIn = []`.  In test plays the actors close their UserIn streams; quiescence is detected by the harness (§5.3).

---

## 4. Fault harness

The paper specifies three faults: identity loss (§3, §4.3 Replace), state loss (§3, §4.3 Crash + Recover), and message loss (§8.6 Convergence under Message Loss).

### 4.1 Crash (state loss)

Effect of CVA Crash (§4.3): reset `known`, `outbox`, `inbox`, platform state; identity record `IR_p` survives.  In GLP terms: kill the agent process, restart with fresh streams and empty Platform, identity preserved.

Implementation: the play wraps each agent in a `supervisor(Id, IRec, …)` process that holds the current `(UserIn, NetIn, NetOut)` triple.  On `crash` from the actor, the supervisor:

1. Closes the current `NetIn` and `UserIn` (the running `agent/7` terminates via §3.7).
2. Allocates fresh streams `(UserIn', NetIn', NetOut')`.
3. Re-routes the network mediator's per-agent endpoint to `(NetOut', NetIn')`.
4. Re-spawns `agent(Id, IRec, UserIn', NetIn', NetOut', 0, init_platform)`.

In-transit messages addressed to the crashed agent at the moment of crash are lost (they sat in the old NetIn).  Messages from the crashed agent that already reached the mediator may still be delivered to their recipients — implementing the paper's "outbox is reset" requires the supervisor to also signal the mediator to discard pending outbox messages, which is straightforward.

### 4.2 Identity loss

1. Crash `p` (§4.1) so `p`'s state is empty.
2. Spawn a fresh agent `p'` with its own `IRec_{p'}` and (for bond sovereigns) its inherited `S_{p'} = S_p`, both provided out-of-band by the person.
3. The play emits, from the persons of `p`'s identity custodians, `will(vouch(p, p'))` on their UserIn streams.
4. Replace cascade (§8.7) proceeds via the agents' own clauses.

`p` remains a dead process after the crash — the supervisor does not respawn.  The abstract `F'_p := ∅` is realised by `p`'s process being gone (§4.3 paper, line 85 "abstract retirement, not a write to the old machine").

### 4.3 Message loss

The network mediator implements per-endpoint `DropPolicy`:
- `drop_none` — default.
- `drop_one(CargoPattern)` — drop the next message whose cargo matches; revert to `drop_none`.
- `drop_schedule(List)` — drop messages matching the listed (recipient, cargo) entries in order.

The play installs the policy at boot or via actor commands.

This realises the paper's "in-transit case of a crash" (§8.5) at full granularity, sufficient for exercising §8.6 Convergence under Message Loss.

---

## 5. Test plays

### 5.1 Play structure

A test play is one GLP procedure, e.g. `play1 :- …`.  It composes:

1. **Boot** — for each agent, allocate `(UserIn, NetIn, NetOut)`, spawn `supervisor(Id, IRec, …)` and the initial `agent(Id, IRec, …)` underneath, register the agent's `net_end` with the mediator.
2. **Network** — spawn `network(NetEnds)`.
3. **Actor** — one process per agent that writes the scripted UserIn events.  Closes UserIn at end of script.
4. **Observer** — optional process reading per-agent outputs for the play's expected result (via `tee` on per-agent streams as in Ohad's harness).

The play succeeds when all actor scripts complete and quiescence (§5.3) is detected.

### 5.2 Volition injection

The actor writes UserIn events in scripted order.  Example script for `alice` to befriend `bob`:

```glp
[ will(discover(bob)),
  will(offer_friendship(bob)),
  tick, tick,
  query(epoch(bob), E) ]
```

The volition `offer_friendship(bob)` may arrive before `discover(bob)` has fired; it sits in `Volitions` until the discover transaction adds `bob` to `Known`, at which point the agent's offer-friendship firing clause matches and the volition is consumed (§3.3).

### 5.3 Quiescence detection

CVA quiescence (§6 Definition 6.1): no transaction other than Advance-date is enabled.

At the GLP level this means:
- Every actor's UserIn is closed.
- The network mediator has drained: no message in any agent's NetIn or in transit.
- No agent has a queued volition whose precondition holds.
- No agent has a Pending inbox entry whose precondition holds.

Detection approach (first pass): the observer waits a fixed number of ticks after the last actor event, then inspects state via `query` on each agent.  If state matches the play's expected outcome, the play succeeds.

A more robust detector (per-agent `quiescent(Id)` writer bound when `agent/7` has no enabled firing clause and `UserIn = []`) is a later refinement.

---

## 6. Type structure

The `self.glp` files (the first reviewable code artifact) contain full type definitions and procedure declarations along the structure below.  Concrete syntax follows GLP conventions (parametric types, `?` for reader modes, type unions); the shapes here are pseudo-GLP to fix the architecture.

### 6.1 CVA substrate types (shared across all platforms)

```
%% Messages
Cargo                          %% platform-specific
NetMsg(C)    ::= msg(Constant, Constant, C)
                              %% msg(Sender, Recipient, Cargo) in NetOut
InMsg(C)     ::= msg(Constant, C)
                              %% msg(Sender, Cargo) in NetIn (recipient implicit)

%% UserIn events
UserEvent(V, Q, A) ::= will(V)
                     ; unwill(V)
                     ; tick
                     ; crash
                     ; query(Q, A)

%% Identity record (intrinsic, immutable)
IdentityRecord ::= ir(KSet, Sigma)
KSet           ::= [] ; [Constant | KSet]
Sigma          ::= constant      %% supermajority threshold

%% Network-mediator endpoint
NetEnd(C)    ::= net_end(Constant, Stream(NetMsg(C))?, Stream(InMsg(C)), DropPolicy)
DropPolicy   ::= drop_none ; drop_one(Cargo) ; drop_schedule(Stream(DropEntry))
```

The platform-specific `Cargo`, `Volition`, `Query`, `Answer`, and `Platform` types are defined in per-platform `self.glp` files.

### 6.2 GSG (clean, §6) platform sketch

```
GsgCargo     ::= friend_request(Constant)
               ; accept(Constant)
               ; unfriend(Constant)
               ; stream_update(Constant, Constant)

GsgVolition  ::= discover(Constant)
               ; offer_friendship(Constant)
               ; accept_offer(Constant)        %% volition guard for Accept offer
               ; end_friendship(Constant)

GsgPlatform  ::= gsg(Known, Inbox, Volitions, FMap, FoFMap)
Known        ::= …                       %% set of Constant
Inbox        ::= …                       %% set of InMsg(GsgCargo)
Volitions    ::= …                       %% set of GsgVolition
FMap         ::= …                       %% map Constant → FMapEntry
FMapEntry    ::= fme(Epoch, Data)
FoFMap       ::= …                       %% nested map Constant → Constant → Epoch
Epoch        ::= constant                %% even = inactive, odd = active
```

Map representation (assoc-list vs Ohad's `map_*` kernels) is settled at types-time, not now.

### 6.3 Secure GSG (§8) platform sketch (additive over GSG)

```
SecureGsgCargo ::= friend_request(Constant, IdentityRecord)
                 ; accept(Constant, IdentityRecord)
                 ; unfriend(Constant)
                 ; stream_update(Constant, Constant)
                 ; checkpoint(Constant, IdentityRecord, FriendList)
                 ; vouch(Constant, Constant, FoFEntries)
                 ; new_identity(Constant, Constant, KSet, IdentityRecord)
                 ; rebind(Constant, IdentityRecord, FMapSnap, FoFEntries)

SecureGsgVolition ::= discover(Constant)
                    ; offer_friendship(Constant)
                    ; accept_offer(Constant)
                    ; end_friendship(Constant)
                    ; vouch(Constant, Constant)
                    ; announce_new_identity(Constant)

%% FMap entries: skeleton/stub/full distinguished by tag
FMapEntry    ::= full(Epoch, Data, KSet, Sigma)
               ; skeleton(KSet, Sigma)        %% (epoch 0, ⊥ data, real IR) per §8.5
               ; stub(Epoch)                  %% (epoch e, ⊥ data, ⊥ IR) per §8.7

SecureGsgPlatform ::= secure_gsg(Known, Inbox, Volitions, FMap, FoFMap,
                                 LastBroadcastDate)
```

- `LastBroadcastDate` resolves spec-issue D (one re-broadcast per date; precondition `d_p < t_p`).
- The tagged `FMapEntry` discriminates skeleton (§8.5), stub (§8.7), and full entries cleanly per spec-issue E.
- Issue O (Integrate checkpoint promoting stubs when sender's IR matches) is pending paper Claude; the implementation will follow the resolution.

### 6.4 Secure bonds / coins (§10) platform sketch (additive over Secure GSG)

```
BondCargo    ::= become_custodian                            %% §10 (issue N)
               ; request(BondTx)
               ; block(Constant, BondTx)
               ; ack(Constant)
               ; final(Constant, BondTx)                     %% §10 (issue G)

BondTx       ::= mint(Constant, Constant)
               ; pay(Constant, Constant, BondMset)
               ; redeem(Constant)
               ; swap(Constant, Constant, BondMset, BondMset)

Bond         ::= bond(Constant, Constant, Constant)
BondMset     ::= …                       %% multiset of Bond

%% New volitions (M, N): currency formation and bond transactions
BondVolition ::= form_currency(KSet, Sigma)                  %% §10 Form currency (issue M)
               ; request_pay(Constant, BondMset)
               ; request_mint(Constant, Constant)
               ; request_redeem(Constant)
               ; request_swap(Constant, BondMset, BondMset)

%% Platform: an agent can simultaneously be sovereign, holder, custodian
SecureBondsPlatform ::= sbp(Known, Inbox, Volitions, FMap, FoFMap,
                            LastBroadcastDate,
                            Sovereign,                       %% own currency, if formed
                            Holder,                          %% own bond holdings
                            CustodianOf,                     %% map sovereign → log copy
                            Pending)                         %% out-of-order blocks (§3.4)

Sovereign    ::= not_formed
               ; sovereign(Log, AckFrontier, CustodianSet)
Log          ::= …                       %% list of block(N, BondTx)
AckFrontier  ::= …                       %% map Constant → Constant (ℓ_p)
CustodianSet ::= …                       %% S_p (subset of FMap domain)

Holder       ::= holder(BondMset)        %% B_p

CustodianOf  ::= …                       %% map Sovereign → Log copy (L̂_p[s])

Pending      ::= …                       %% map Sovereign → set of out-of-order blocks
```

Issue M (Form currency) and N (become_custodian) are now on-protocol per the paper's resolution: a unary volitional transaction `form_currency` at the sovereign emits `become_custodian` to each `c ∈ S`; a reactive `become_custodian` at the custodian initialises `L̂_c[s] := ε`.

### 6.5 Procedure declarations

Each `self.glp` file declares its public procedures with modes.  Top-level:

```glp
procedure agent(Constant?, IdentityRecord?,
                Stream(UserEvent(Volition, Query, Answer))?,
                Stream(InMsg(Cargo))?,
                Stream(NetMsg(Cargo)),
                Constant?, Platform?).
procedure supervisor(Constant?, IdentityRecord?, …).
procedure network(Stream(NetEndConfig)?).
procedure actor(Constant?, Stream(UserEvent(Volition, Query, Answer))).
```

The `agent` declaration is parameterised in `Cargo`, `Volition`, `Query`, `Answer`, `Platform`; each platform `self.glp` re-declares it with its concrete types.

Helper procedures (`contains_msg/3`, `contains_volition/3`, `add_to_inbox/3`, `set_epoch/4`, `broadcast_update/6`, etc.) are declared in the `self.glp` of the platform that needs them.

The complete declarations come in the next deliverable.

---

## 7. Directory layout

```
GLP/programs/SPM/
├── docs/
│   └── glp-architecture.md          (this file)
├── cva/
│   ├── self.glp                     CVA substrate types + agent/supervisor/network/actor decls
│   ├── network.glp                  network mediator implementation
│   ├── supervisor.glp               crash-restart wrapper
│   └── actor.glp                    actor script helpers
├── gsg/
│   ├── self.glp                     GSG cargo, volitions, platform; agent decl
│   ├── agent.glp                    agent/7 clauses for §6 transactions
│   ├── platform.glp                 helpers (epoch, FMap, broadcast, snapshot, contains_*)
│   └── plays/
│       ├── play_befriend.glp
│       ├── play_befriend_simultaneous.glp
│       ├── play_unfriend.glp
│       └── play_three_agents.glp
├── secure_gsg/
│   ├── self.glp                     extends gsg/self.glp with IR, checkpoint, replace cargos
│   ├── agent.glp                    additional clauses for §8 transactions
│   ├── platform.glp                 IR storage, checkpoint construction, replace cascade helpers
│   └── plays/
│       ├── play_crash_restore.glp
│       ├── play_replace.glp
│       └── play_message_loss.glp
└── secure_bonds/
    ├── self.glp                     bond cargo, transactions, sovereign/holder/custodian state
    ├── agent.glp                    additional clauses for §10 transactions
    ├── platform.glp                 log, ack frontier, finality helpers, pending buffer
    └── plays/
        ├── play_form_and_mint.glp
        ├── play_pay_supermajority.glp
        ├── play_sovereign_state_loss.glp
        └── play_holder_recovery.glp
```

Each `self.glp` is the *first* file in its directory: types + procedure declarations only, no clause bodies.  Clauses come after the type review.

---

## 8. Build order

1. **`cva/`** — substrate.  Types, network mediator, supervisor (crash), actor.  Test: two agents do `Discover` and exchange one message.
2. **`gsg/`** — clean social graph.  All of §6.  Test plays: pairwise befriend, simultaneous offer, unfriend, three-agent friend-of-friend visibility.
3. **`secure_gsg/`** — security layer.  IR transport on befriend, checkpoint + Restore, Replace cascade.  Test plays: crash + passive restore, identity loss + Replace, single-message loss + convergence.
4. **`secure_bonds/`** — bond layer.  Form currency + Become custodian + supermajority finality.  Mint, Pay, Redeem, Swap.  Sovereign state-loss recovery; holder recovery.  Stop before bond message-loss / custodian recovery per J, K.

Each stage has a `self.glp` review checkpoint before clauses are written.

---

## 9. Open spec issues acknowledged in this document

Per `/SPM/docs/spec-issues.md`:

- **A–N**: all closed in the paper.  This document follows the resolutions throughout.
- **O** (Integrate checkpoint does not promote stubs): raised after the A–N triage; paper Claude considering.  Implementation follows whatever resolution lands; if Integrate checkpoint is extended to promote stubs from the sender's IR (the fix I proposed), one extra clause in `secure_gsg/agent.glp`; if the resolution differs, the fix is still localised.

Working choices the architecture pins down (compatible with the paper but not derivable from it):

- **Volitions are stateful per agent** (paper §2.4 implication, made explicit here in §2.5 and §3.3).
- **Set-inbox realised by remove-on-fire** (sound under paper's idempotence-by-precondition discipline, see §3.2).
- **`Pending` buffer per-platform where preconditions can become true later** (bonds only, see §3.4).
- **Network FIFO per (sender, recipient)** (stricter than paper's set semantics; preserves all properties).

---

## 10. What this document does not commit to yet

- Concrete map implementation (Ohad's `map_*` kernels vs.\ assoc-list).  Decided when types are written.
- Concrete `Query` / `Answer` shape for read-only inspection.  Decided per-platform when first query is needed.
- Concrete quiescence detector (per-agent `quiescent(Id)` writer vs.\ tick-based heuristic).  Tick heuristic first; refine if needed.
- Whether the supervisor is a separate process or absorbed into boot wiring.  Decided when the crash play is written.

These are local decisions that don't affect the substrate or the per-transaction translation rule.
