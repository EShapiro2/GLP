# madGLP Specification

**Version**: 4.0  
**Date**: 2026-01-30  
**Status**: DRAFT  
**Source**: CGLP Paper (`~/Grassroots/CGLP`), Section 7 "Multiagent Deterministic GLP (madGLP)"

---

## 1. Overview

This document specifies **Multiagent Deterministic GLP (madGLP)**, an implementation-ready transition system that implements maGLP using only local variable pairs connected by global links. While maGLP defines shared variable pairs that span agent boundaries, madGLP replaces each such pair with two fully local pairs connected through a global writers table and message passing, with forwarding handled by spawned GLP goals.

### 1.1 Key Design Principles

**Local Pairs with Global Links**: A maGLP shared variable pair `(X, X?)` with writer X at agent p and reader X? at agent q is implemented by two local pairs connected by a global link:

- At agent p: a local pair `(X_p, X_p?)` where both variables remain in p's resolvent
- At agent q: a local pair `(X_q, X_q?)` where both variables remain in q's resolvent  
- A global link connecting X_p to X_q, realized as a `global_send` goal at p and an entry in q's global writers table

**Push-Based Communication**: When X_p is assigned a term T at agent p, a spawned `global_send` goal detects this (when X_p? becomes known) and sends an assignment message to q. Upon receipt, q looks up the target writer in its global writers table, assigns X_q := T_q↓, and removes the entry.

**Uniform Forwarding**: All outgoing communication is handled by `global_send` goals, including forwarding when both ends of a variable pair are exported.

### 1.2 Definitions

- **Local variable pair**: A writer X and its paired reader X? both occurring in the same agent's resolvent
- **Global variable name**: A term `_w(p, i)` or `_r(p, i)` identifying a variable exported by agent p at index i
- **Global link**: The combination of a `global_send` goal at one agent and a global writers table entry at another
- **Global writers table**: A table tracking writers that await incoming assignments from remote agents

---

## 2. Global Variable Names

Global variable names appear only in messages between agents, never in resolvents. They identify the source of a global link and enable message routing.

**Definition [Global Variable Name]**

A global variable name is a term of the form `_w(p, i)` or `_r(p, i)`, where:

- `p ∈ Π` is an agent identifier
- `i ∈ ℕ` is an index allocated by p during globalization
- `_w(p, i)` denotes a writer globalized at p
- `_r(p, i)` denotes a reader globalized at p

---

## 3. Global Writers Table

The global writers table tracks local writers that await incoming assignments from remote agents. Each entry maps a global name to the local writer that will be assigned when a message arrives.

### 3.1 Entry Types

**Definition [Global Writers Table Entry]**

A global writers table entry at agent p is either:

- **(X, q)** for entries created by Globalize: X ∈ 𝒱 is a local writer that will be assigned when a callback message arrives from agent q
- **(X, q, i)** for entries created by Localize: X ∈ 𝒱 is a local writer, q ∈ Π is the remote agent, and i ∈ ℕ is the index in q's global name (needed to match incoming messages)

### 3.2 Table Structure

**Definition [Global Writers Table]**

The global writers table W_p of agent p is an indexed array of entries. For entries created by Globalize at index i, the index i is the index in the global name `_r(p, i)`. For entries created by Localize, the entry stores the remote index explicitly.

**What the Table Stores**: The table contains only writers that await incoming assignments. No entries are created for outgoing links—those are handled by `global_send` goals.

**Index Allocation**: A single counter is used for index allocation at each agent, shared across both Globalize and Localize operations. Indices are never reused.

**Entry Removal**: When an assignment message arrives and the corresponding writer is bound, the entry is removed from the table. This may leave gaps in the array; indices are not reused. Implementations may use a sparse representation (e.g., a map from index to entry) rather than a dense array.

---

## 4. The global_send Predicate

Outgoing communication is handled by spawned goals rather than by table entries. This uniform approach correctly handles all cases, including forwarding when both ends of a variable pair are exported.

**Definition [global_send Predicate]**

The system predicate `global_send/3` is defined as:

```prolog
global_send(T, G, Q) :- known(T) | '_send'(T, G, Q).
```

where:

- T is the reader whose value will be sent when known
- G is the global variable name (`_w(p,i)` or `_r(p,i)`) identifying the link
- Q is the destination agent

The guard `known(T)` succeeds when T is bound to a non-variable term. The builtin `'_send'(T, G, Q)` globalizes T and adds message `(G := T↑, Q)` to the agent's outgoing message set.

**Forwarding via global_send**: When an agent exports both ends of a variable pair (e.g., sending `[X, X?]` to another agent), the Globalize operation spawns a `global_send` goal for the exported writer. If a value arrives on one global link and is assigned to a local writer X, then X? becomes known, triggering any `global_send` goal watching X?. This automatically forwards the value without requiring special forwarding logic in the Receive transaction.

---

## 5. Globalize and Localize Operations

### 5.1 Globalize

**Definition [Globalize]**

Given agent p, remote agent q, and term T, the globalization by p, written T_p↑, may update the global writers table W'_p and spawn goals into p's resolvent as follows. For each variable Y occurring in T:

1. **If Y is a writer**: allocate the next index i, replace Y in T_p↑ with `_w(p, i)`, and spawn goal `global_send(Y?, _w(p,i), q)` into p's resolvent. No entry is created—the `global_send` goal handles outgoing communication.

2. **If Y? is a reader**: allocate the next index i, create entry `(Y, q)` at index i in W'_p, and replace Y? in T_p↑ with `_r(p, i)`. No goal is spawned—p will receive the callback on this link.

### 5.2 Localize

**Definition [Localize]**

Given agent q, remote agent p, and globalized term T_p↑, the localization by q, written T_q↓, may update the global writers table W'_q and spawn goals into q's resolvent as follows. For each global name in T_p↑:

1. **If `_w(p, i)`**: create fresh local pair `(Y_q, Y_q?)`, allocate the next index k in W'_q, add entry `(Y_q, p, i)`, and replace `_w(p, i)` with Y_q? (the reader) in T_q↓. No goal is spawned—q will receive the assignment on this link.

2. **If `_r(p, i)`**: create fresh local pair `(Z_q, Z_q?)`, replace `_r(p, i)` with Z_q (the writer) in T_q↓, and spawn goal `global_send(Z_q?, _r(p,i), p)` into q's resolvent. No entry is created—the `global_send` goal handles outgoing communication.

### 5.3 Globalize-Localize Correspondence

The pairing between Globalize and Localize ensures correct dataflow:

**Writer globalized at p**: Globalize spawns `global_send(Y?, _w(p,i), q)` at p. Localize adds entry `(Y_q, p, i)` at q and puts Y_q? in q's term. When p assigns Y, the spawned goal fires and sends the value to q, where the entry routes it to Y_q, making it available via Y_q?.

**Reader globalized at p**: Globalize adds entry `(Y, q)` at p. Localize puts Z_q in q's term and spawns `global_send(Z_q?, _r(p,i), p)`. When q assigns Z_q, the spawned goal fires and sends the value back to p, where the entry routes it to Y, making it available via Y?.

### 5.4 Exporting Both Ends of a Pair

Consider agent p exporting term `[X, X?]` to agent q. Globalize processes both:

- Writer X: spawns `global_send(X?, _w(p,0), q)`, no entry
- Reader X?: entry `(X, q)` at index 1, no spawn

At q, Localize creates two independent pairs:

- For `_w(p,0)`: pair `(Y_q, Y_q?)`, entry `(Y_q, p, 0)`, term gets Y_q?
- For `_r(p,1)`: pair `(Z_q, Z_q?)`, term gets Z_q, spawns `global_send(Z_q?, _r(p,1), p)`

The term at q is `[Y_q?, Z_q]`. When q assigns Z_q := T:

1. Z_q? becomes known (= T)
2. `global_send(Z_q?, _r(p,1), p)` fires, sends `_r(p,1) := T↑` to p
3. p receives, finds entry `(X, q)` at index 1, assigns X := T↓
4. X? becomes known (= T)
5. `global_send(X?, _w(p,0), q)` fires, sends `_w(p,0) := T↑` to q
6. q receives, finds entry `(Y_q, p, 0)`, assigns Y_q := T↓
7. Y_q? becomes known (= T)

The value flows from Z_q through p's local pair to Y_q?, correctly implementing the semantics where both ends of the exported pair eventually share the same value.

---

## 6. Local State Structure

### 6.1 madGLP Local State

**Definition [madGLP Local State]**

The local state of agent p ∈ Π is a tuple s_p = (R_p, W_p, M_p) where:

**Resolvent R_p = (A_p, S_p, F_p)** is a deterministic resolvent:

- A_p ∈ 𝒜* is a queue of active goals awaiting reduction
- S_p ⊆ 𝒜 × 2^(𝒱?) contains suspended goals, each paired with its blocking readers
- F_p ⊆ 𝒜 contains failed goals

**Global Writers Table W_p** tracks writers awaiting incoming assignments (Section 3).

**Message Set M_p** is a set of pending outgoing messages as pairs `(m, q)` where q is the destination agent and m is an assignment message of the form `_w(a, i) := T` or `_r(a, i) := T`.

---

## 7. Transition System

**Definition [madGLP Transition System]**

The madGLP transition system over agents P ⊂ Π and GLP program M is the multiagent transition system madGLP = (C, c₀, T) where:

- **C** is the set of configurations where each c_p is a madGLP local state
- **c₀** is the initial configuration where for each p ∈ P:
  - A_p = [agent(p, ch(_?, _), ch(_?, _))]
  - S_p = ∅, F_p = ∅, W_p = ∅, M_p = ∅
- **T** consists of the Reduce, Send, Receive, and Network transactions defined below

---

## 8. Transactions

### 8.1 Reduce Transaction (Unary)

**Definition [madGLP Reduce Transaction]**

The unary Reduce transaction for agent p transitions s_p → s'_p where A_p = A · A_r:

**Case Reduce**: The GLP reduction of A with first applicable clause C succeeds with (B, σ̂):

1. Apply σ̂ to bind writers and σ̂? to propagate to readers in the resolvent
2. Let R be goals in S_p suspended on readers that were instantiated; move them to active
3. A'_p := (A_r · B · R)σ̂σ̂?, update S'_p accordingly

**Case Suspend**: No clause succeeds but suspension set W ≠ ∅:

- A'_p := A_r
- S'_p := S_p ∪ {(A, W)}

**Case Fail**: No clause succeeds and W = ∅:

- A'_p := A_r
- F'_p := F_p ∪ {A}

**Note on Outgoing Messages**: The Reduce transaction does not directly generate outgoing messages. Instead, when a writer X is assigned, X? becomes known, which may trigger a `global_send` goal watching X?. That goal's reduction (via the `'_send'` builtin) adds the message to M_p. This uniform approach handles both direct sends and forwarding.

### 8.2 Send Transaction (Unary)

**Definition [madGLP Send Transaction]**

The unary Send transaction for agent p is enabled when `(m, q) ∈ M_p`. It removes `(m, q)` from M_p and places m in the communication channel to q.

**Wire Format**: The transmitted message includes the destination agent q in the header, enabling the communication infrastructure to route the message correctly. The message body contains the assignment (global name and globalized term).

### 8.3 Receive Transaction (Unary)

**Definition [madGLP Receive Transaction]**

The unary Receive transaction processes a message m from the communication channel:

**Case `m = (_w(p, i) := T↑)`**: The message is destined for the agent that localized `_w(p,i)`. Agent q searches its global writers table for an entry `(X_q, p, i)` matching the remote agent p and remote index i. Localize T↑ by q to get T_q↓, assign X_q := T_q↓, apply {X_q? := T_q↓} to goals containing X_q?, reactivate suspended goals, and remove the entry from W'_q.

**Case `m = (_r(p, i) := T↑)`**: The message is destined for agent p who created this global name. Agent p finds entry `(X, q)` at index i in W_p. Localize T↑ by p to get T_p↓, assign X := T_p↓, apply {X? := T_p↓} to goals containing X?, reactivate suspended goals, and remove the entry from W'_p.

**Automatic Forwarding**: The Receive transaction simply assigns the local writer and removes the entry. If the assigned writer's reader (X?) is being watched by a `global_send` goal (because it was also exported), that goal will fire on a subsequent Reduce, automatically forwarding the value.

### 8.4 Network Transaction (Binary)

**Definition [madGLP Network Transaction]**

The binary Network transaction (s_p, s_q) → (s'_p, s'_q) where p ≠ q and `msg(q, T)` appears in p's network output stream:

1. Globalize T by p for q to get T_p↑, updating W'_p and spawning `global_send` goals as specified
2. Advance p's network output stream
3. Localize T_p↑ by q from p to get T_q↓, updating W'_q and spawning `global_send` goals as specified
4. Add T_q↓? to q's network input stream (where T_q↓? replaces each variable Y in T_q↓ with Y?)

---

## 9. Key Properties

### 9.1 Transaction Degrees

The Reduce, Send, and Receive transactions are unary. Only the Network transaction is binary, as it must atomically establish the global link between two agents. This is acceptable because Network transactions occur only during cold-calls, which are rare bootstrap operations.

### 9.2 Correspondence to maGLP

The maGLP binary Communicate transaction, which atomically transfers an assignment from one agent's writer to another agent's reader, is implemented in madGLP by the sequence: Reduce (assigns writer, triggering `global_send`) → Send → Receive (applies assignment). The correctness of this implementation relies on monotonicity.

### 9.3 Global Writers Table Lifecycle

An entry is added to the global writers table when a global link is established with the agent as the receiver: either when globalizing a reader (expecting a callback) or when localizing a writer global name (expecting the assignment). The entry is removed when the assignment arrives and the writer is bound.

### 9.4 Message Routing

Messages use global names to enable routing:

- Message `_w(p, i) := T` is sent from p to whoever localized `_w(p, i)`; that agent searches for an entry with remote agent p and remote index i
- Message `_r(p, i) := T` is sent to p (the original globalizer); p has an entry at index i in its global writers table

---

## 10. Example Scenarios

### 10.1 Direct Communication (Client-Monitor)

Consider the initial goal `client(Xs)@p, monitor(Xs?)@q`, establishing a shared pair with writer Xs at p and reader Xs? at q.

**Stage 0: Network Transaction**

The Network transaction establishes the link for Xs:

- p spawns `global_send(Xs?, _w(p,0), q)`
- q creates entry `(Xs_q, p, 0)`
- q's resolvent contains Xs_q?

**Stage 1: p Assigns Xs**

p assigns Xs := [add|Xs1]:

1. Xs? becomes known (= [add|Xs1])
2. `global_send(Xs?, _w(p,0), q)` fires
3. The term [add|Xs1] is globalized: Xs1 becomes `_w(p,1)`, spawns `global_send(Xs1?, _w(p,1), q)`
4. Message `_w(p,0) := [add|_w(p,1)]` sent to q

**Stage 2: q Receives**

q receives `_w(p,0) := [add|_w(p,1)]`:

1. Find entry `(Xs_q, p, 0)`
2. Localize: create pair `(Xs1_q, Xs1_q?)`, entry `(Xs1_q, p, 1)`
3. Assign Xs_q := [add|Xs1_q?]
4. Remove entry `(Xs_q, p, 0)`

### 10.2 Callback Scenario (value Request)

p assigns Xs1 := [value(V?)|Xs2], exporting reader V?:

1. Globalize V?: entry `(V, q)` at index k in W_p, becomes `_r(p,k)`
2. Message `_w(p,1) := [value(_r(p,k))|_w(p,2)]` sent to q

q receives and localizes:

1. For `_r(p,k)`: create pair `(V_q, V_q?)`, put V_q in term, spawn `global_send(V_q?, _r(p,k), p)`
2. For `_w(p,2)`: create pair `(Xs2_q, Xs2_q?)`, entry `(Xs2_q, p, 2)`, put Xs2_q? in term

When q's monitor assigns V_q := Sum:

1. V_q? becomes known
2. `global_send(V_q?, _r(p,k), p)` fires
3. Message `_r(p,k) := Sum↑` sent to p
4. p receives, finds entry `(V, q)` at index k, assigns V := Sum↓
5. V? becomes known in p's resolvent

### 10.3 Friend-Mediated Introduction

Bob introduces Alice to Charlie by sending writer X to Alice and reader X? to Charlie.

**Bob exports X to Alice**:

Bob sends term X to Alice via Network transaction:

- Globalize X: spawns `global_send(X?, _w(bob,0), alice)`, no entry
- Alice localizes `_w(bob,0)`: creates pair `(X_a, X_a?)`, entry `(X_a, bob, 0)`, receives X_a?

Alice now holds reader X_a? and will receive values when Bob's X is assigned.

**Bob exports X? to Charlie**:

Bob sends term X? to Charlie via Network transaction:

- Globalize X?: entry `(X, charlie)` at index 1 in W_bob, becomes `_r(bob,1)`
- Charlie localizes `_r(bob,1)`: creates pair `(X_c, X_c?)`, spawns `global_send(X_c?, _r(bob,1), bob)`, receives X_c (the writer)

Charlie now holds writer X_c and can send values back to Bob.

**State after introduction**:

- Bob: `global_send(X?, _w(bob,0), alice)` goal, entry `(X, charlie)` at index 1
- Alice: entry `(X_a, bob, 0)`, holds X_a?
- Charlie: `global_send(X_c?, _r(bob,1), bob)` goal, holds X_c

**Charlie sends a message**:

Charlie assigns X_c := T:

1. X_c? becomes known (= T)
2. `global_send(X_c?, _r(bob,1), bob)` fires
3. Message `_r(bob,1) := T↑` sent to Bob
4. Bob receives, finds entry `(X, charlie)` at index 1, assigns X := T↓
5. X? becomes known at Bob (= T)
6. `global_send(X?, _w(bob,0), alice)` fires
7. Message `_w(bob,0) := T↑` sent to Alice
8. Alice receives, finds entry `(X_a, bob, 0)`, assigns X_a := T↓
9. X_a? becomes known at Alice (= T)

The value flows from Charlie through Bob to Alice, with Bob's local pair `(X, X?)` serving as the forwarding point.

---

## 11. Implementation Notes

### 11.1 Global Name Allocation

Each agent maintains a counter for allocating global name indices. The counter is incremented for each variable globalized (either writer or reader).

### 11.2 Entry Lookup

For entries created by Globalize (form `(X, q)`), lookup is direct by index—the entry at index i corresponds to global name `_r(p, i)`.

For entries created by Localize (form `(X, q, i)`), lookup requires searching for a matching `(q, i)` pair. Implementations may maintain a secondary index mapping `(remote_agent, remote_index)` to local entries for efficiency.

### 11.3 Heap Representation

Local variable pairs use standard two-cell allocation:

- Writer cell: WrtTag, content is null (unbound), SuspensionListNode (waiting), or Pointer (bound)
- Reader cell: RoTag, content is Pointer to writer cell

No special representation is needed for "imported" variables—all variables are local pairs. The global writers table provides routing information separately from the heap representation.

### 11.4 Serialization

Terms crossing agent boundaries are serialized with global names substituted for variables. The serialization format must preserve:

- Functor/arity structure
- Global name encoding: type tag + agent identifier + index
- Constants: type tag + value bytes

### 11.5 The '_send' Builtin

The `'_send'(T, G, Q)` builtin:

1. Globalizes term T (which may spawn additional `global_send` goals for nested variables)
2. Adds message `(G := T↑, Q)` to M_p

This builtin is invoked only when the `global_send` goal's guard succeeds, ensuring the reader argument is bound.

---

## 12. Invariants

The following invariants are maintained by madGLP:

**SRSW Property**: Within any agent's resolvent, each variable occurs at most once as a reader and at most once as a writer (inherited from GLP).

**Entry Lifecycle**: Every global writers table entry is created exactly once (by Globalize or Localize) and removed exactly once (by Receive). An entry is never modified between creation and removal.

**Callback Atomicity**: When a `global_send` goal fires, the globalization of its term value (which may spawn additional `global_send` goals for nested variables) and the addition of the message to M_p occur atomically within the same Reduce transaction.

**Index Uniqueness**: Each (agent, index) pair uniquely identifies a global name. Indices are allocated sequentially and never reused, even after entry removal.

**Message Ordering**: Messages between any pair of agents are delivered in FIFO order. This ensures that if agent p sends two messages to agent q, q receives them in the order sent.

---

## 13. Security Extensions

The security extensions from the previous specification (Section 7) remain applicable. Each message in M_p can be cryptographically protected with attestation, signature, and encryption as described there.

---

## 14. References

- **CGLP Paper**: `~/Grassroots/CGLP`, Section 7 "Multiagent Deterministic GLP (madGLP)"
- **Previous Spec**: `archive/irmaGLP-spec-v3.1-2026-01-30.md` (request-based model, now superseded)
- **Related Specs**:
  - `/docs/glp-runtime-spec.txt` - Single-agent runtime
  - `/docs/glp-bytecode-v216-complete.md` - Bytecode instruction set

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 4.0 | 2026-01-30 | Claude | Complete rewrite based on madGLP design from CGLP paper. Replaced request-based model with push-based global_send mechanism. Simplified global writers table. Added Globalize/Localize operations. |
| 4.1 | 2026-01-30 | Claude | Added Section 12 (Invariants). Clarified single-counter index allocation in Section 3.2. Added wire format clarification in Section 8.2. |
