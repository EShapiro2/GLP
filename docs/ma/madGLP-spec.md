# madGLP Specification

**Version**: 5.3
**Date**: 2026-02-10
**Status**: DRAFT  
**Source**: CGLP Paper (`~/Grassroots/CGLP`), Section 7 "Multiagent Deterministic GLP (madGLP)"

---

## 1. Overview

This document specifies **Multiagent Deterministic GLP (madGLP)**, an implementation-ready transition system that implements maGLP using only local variable pairs connected by global links. While maGLP defines shared variable pairs that span agent boundaries, madGLP replaces each such pair with two fully local pairs connected through a global writers table and message passing, with forwarding handled by spawned GLP goals.

### 1.1 Key Design Principles

**Local Pairs with Global Links**: A maGLP shared variable pair `(X, X?)` with writer X at agent p and reader X? at agent q is implemented by two local pairs connected by a global link:

- At agent p: a local pair `(X_p, X_p?)` where both variables remain in p's resolvent
- At agent q: a local pair `(X_q, X_q?)` where both variables remain in q's resolvent  
- A global link connecting X_p to X_q, realized as a `global_send` goal at the writer-owner and an entry in the reader-owner's global writers table

**Push-Based Communication**: When the writer-owner assigns a term T, a spawned `global_send` goal detects this (when the paired reader becomes known) and sends an assignment message to the reader-owner. Upon receipt, the reader-owner looks up the target writer in its global writers table, assigns it T↓, and removes the entry.

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

- **(X, q)** for entries created by Globalize: X ∈ 𝒱 is a local writer that will be assigned when an assignment message arrives from agent q
- **(X, q, i)** for entries created by Localize: X ∈ 𝒱 is a local writer, q ∈ Π is the remote agent, and i ∈ ℕ is the index in q's global name (needed to match incoming messages)

### 3.2 Table Structure

**Definition [Global Writers Table]**

The global writers table W_p of agent p is an indexed array of entries. For entries created by Globalize at index i, the index i is the index in the global name `_w(p, i)`. For entries created by Localize, the entry stores the remote index explicitly.

**What the Table Stores**: The table contains only writers that await incoming assignments. No entries are created for outgoing links—those are handled by `global_send` goals.

**Index Allocation**: A single counter is used for index allocation at each agent, shared across both Globalize and Localize operations. Index 0 is reserved for the network input serializer (see Section 4.1). The counter starts at 1; indices are never reused.

**Entry Removal**: When an assignment message arrives and the corresponding writer is bound, the entry is removed from the table—except for the serializer entry at index 0, which is permanent. This may leave gaps in the array; indices are not reused. Implementations may use a sparse representation (e.g., a map from index to entry) rather than a dense array.

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

**Forwarding via global_send**: When an agent exports both ends of a variable pair (e.g., sending `[X, X?]` to another agent), the Globalize operation creates an entry for the exported writer and spawns a `global_send` goal for the exported reader. If a value arrives on the writer's link and is assigned to the local writer X, then X? becomes known, triggering the `global_send` goal watching X?. This automatically forwards the value to the reader's link without requiring special forwarding logic in the Receive transaction.

### 4.1 Index-0 Serializer for Cold-Calls

Index 0 is reserved at each agent for the network input serializer. This provides a well-known address that any agent can use for cold-calls.

**Definition [Serializer Entry]**

At boot time, each agent p creates a permanent entry at index 0 mapping `_r(p, 0)` to the local writer N_p for p's network input stream. This entry is never removed.

**Cold-Call Mechanism**: To send a cold-call message T to agent q, any agent p uses `global_send(T, _w(q,0), q)`. This sends the assignment `_w(q,0) := [T↑ | _w(q,0)]`, wrapping the content in a list cell and reusing the serializer writer in the tail.

**Serializer Semantics (Index 0)**:
- **Many-to-one**: Multiple agents can send to `_w(q,0)` simultaneously
- **Reusable**: The writer `_w(q,0)` is reused in the message tail `[T | _w(q,0)]`
- **Permanent entry**: The entry at index 0 is never removed
- **List extension**: Each received message extends the network input stream by one element

**Normal Semantics (Index > 0)**:
- **One-to-one**: Single writer, single reader
- **Single use**: Entry removed after assignment
- **Direct assignment**: Message contains T directly, not wrapped in list

**Remark [Serializer as Merge]**: The index-0 serializer implements a many-to-one merge pattern, combining cold-call messages from multiple senders into a single network input stream. The order of messages from different senders is non-deterministic, while messages from the same sender preserve FIFO order.

**Remark [Network Output Processing]**: Each agent spawns a `send_to_net` goal to process its network output stream:

```prolog
send_to_net([msg(Q, T) | In]) :-
    global_send(T?, _w(Q,0), Q?), send_to_net(In?).
send_to_net([]).
```

This reads cold-call messages `msg(Q, T)` from the output stream and uses `global_send` with the well-known serializer address `_w(Q,0)` to send them.

---

## 5. Globalize and Localize Operations

### 5.1 Globalize

**Definition [Globalize]**

Given agent p, remote agent q, and term T, the globalization by p, written T_p↑, may update the global writers table W'_p and spawn goals into p's resolvent as follows. For each variable Y occurring in T:

1. **If Y is a writer**: allocate the next index i, replace Y in T_p↑ with `_w(p, i)`, and create entry `(Y, q)` at index i in W'_p. No goal is spawned—p will receive the assignment on this link (q gets the writer and will send the value back).

2. **If Y? is a reader**: allocate the next index i, replace Y? in T_p↑ with `_r(p, i)`, and spawn goal `global_send(Y?, _r(p,i), q)` into p's resolvent. No entry is created—the `global_send` goal handles outgoing communication (p keeps the writer and will send the value).

### 5.2 Localize

**Definition [Localize]**

Given agent q, remote agent p, and globalized term T_p↑, the localization by q, written T_q↓, may update the global writers table W'_q and spawn goals into q's resolvent as follows. For each global name in T_p↑:

1. **If `_w(p, i)`**: create fresh local pair `(Y_q, Y_q?)`, replace `_w(p, i)` with Y_q (the writer) in T_q↓, and spawn goal `global_send(Y_q?, _w(p,i), p)` into q's resolvent. No entry is created—the `global_send` goal handles outgoing communication (q gets the writer and will send the value to p).

2. **If `_r(p, i)`**: create fresh local pair `(Z_q, Z_q?)`, allocate the next index k in W'_q, add entry `(Z_q, p, i)`, and replace `_r(p, i)` with Z_q? (the reader) in T_q↓. No goal is spawned—q will receive the assignment on this link (p keeps the writer and will send the value).

### 5.3 Globalize-Localize Correspondence

The pairing between Globalize and Localize ensures correct dataflow:

**Writer globalized at p**: Globalize creates entry `(Y, q)` at p. Localize creates fresh pair `(Y_q, Y_q?)` at q, puts Y_q (writer) in q's term, and spawns `global_send(Y_q?, _w(p,i), p)`. When q assigns Y_q, the spawned goal fires and sends the value to p, where the entry routes it to Y, making it available via Y?.

**Reader globalized at p**: Globalize spawns `global_send(Y?, _r(p,i), q)` at p. Localize creates fresh pair `(Z_q, Z_q?)` at q, adds entry `(Z_q, p, i)`, and puts Z_q? (reader) in q's term. When p assigns Y, the spawned goal fires and sends the value to q, where the entry routes it to Z_q, making it available via Z_q?.

### 5.4 Exporting Both Ends of a Pair

Consider agent p exporting term `[X, X?]` to agent q. Globalize processes both (index 0 is reserved for the serializer, so indices start at 1):

- Writer X: entry `(X, q)` at index 1, no spawn
- Reader X?: spawns `global_send(X?, _r(p,2), q)`, no entry

At q, Localize creates two independent pairs:

- For `_w(p,1)`: pair `(Y_q, Y_q?)`, term gets Y_q (writer), spawns `global_send(Y_q?, _w(p,1), p)`
- For `_r(p,2)`: pair `(Z_q, Z_q?)`, entry `(Z_q, p, 2)`, term gets Z_q? (reader)

The term at q is `[Y_q, Z_q?]`. When q assigns Y_q := T:

1. Y_q? becomes known (= T)
2. `global_send(Y_q?, _w(p,1), p)` fires, sends `_w(p,1) := T↑` to p
3. p receives, finds entry `(X, q)` at index 1, assigns X := T↓
4. X? becomes known (= T)
5. `global_send(X?, _r(p,2), q)` fires, sends `_r(p,2) := T↑` to q
6. q receives, finds entry `(Z_q, p, 2)`, assigns Z_q := T↓
7. Z_q? becomes known (= T)

The value flows from Y_q through p's local pair to Z_q?, correctly implementing the semantics where both ends of the exported pair eventually share the same value.

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
  - S_p = ∅, F_p = ∅, M_p = ∅
  - W_p = {(N_p, *) at index 0} where N_p is the local writer for p's network input stream (serializer entry)
- **T** consists of the Reduce, Send, and Receive transactions defined below

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

**Case `m = (_w(p, i) := T↑)` with i > 0**: The message is destined for agent p who created this global name (by globalizing a writer). Agent p finds entry `(X, q)` at index i in W_p. The entry provides the remote agent identity q, which is used when localizing T↑: any variables in T↑ get their global links pointing to q. Localize T↑ by p from q to get T_p↓, assign X := T_p↓, apply {X? := T_p↓} to goals containing X?, reactivate suspended goals, and remove the entry from W'_p.

**Case `m = (_w(q, 0) := [T↑ | _w(q,0)])` (Serializer)**: Cold-call message to agent q's network input. Agent q finds the permanent entry `(N_q, *)` at index 0. Localize T↑ by q to get T_q↓. Assign N_q := [T_q↓ | N'_q] where N'_q is a fresh writer. Update the entry to `(N'_q, *)` at index 0 (extending the stream). Reactivate any goals suspended on N_q?. The entry is NOT removed—it is updated with the fresh writer for the next message.

**Case `m = (_r(p, i) := T↑)`**: The message is destined for the agent that localized `_r(p,i)` (by localizing a reader global name). Agent q searches its global writers table for an entry `(X_q, p, i)` matching the remote agent p and remote index i. The entry provides the remote agent identity p, which is used when localizing T↑: any variables in T↑ get their global links pointing to p. Localize T↑ by q from p to get T_q↓, assign X_q := T_q↓, apply {X_q? := T_q↓} to goals containing X_q?, reactivate suspended goals, and remove the entry from W'_q.

**Remote Agent Identity in Entries**: The entry stores the remote agent identity (q in `(X, q)` or p in `(X_q, p, i)`), which serves two purposes:
1. For `(X_q, p, i)` entries: enables lookup by matching the message's global name `_r(p, i)` to the entry's `(p, i)` pair
2. For both entry types: provides the remote agent identity needed during Localize to properly bake the destination into any nested global links (spawned `global_send` goals or new entries)

**Automatic Forwarding**: The Receive transaction simply assigns the local writer and removes the entry. If the assigned writer's reader (X?) is being watched by a `global_send` goal (because it was also exported), that goal will fire on a subsequent Reduce, automatically forwarding the value.

---

## 9. Key Properties

### 9.1 Transaction Degrees

All madGLP transactions are unary: Reduce, Send, and Receive each affect only one agent's local state. Cold-calls use the same `global_send` mechanism as established links, sending to the well-known serializer address `_w(q,0)`. This uniformity simplifies the implementation and enables fully decentralized operation.

### 9.2 Correspondence to maGLP

The maGLP binary Communicate transaction, which atomically transfers an assignment from one agent's writer to another agent's reader, is implemented in madGLP by the sequence: Reduce (assigns writer, triggering `global_send`) → Send → Receive (applies assignment). The correctness of this implementation relies on monotonicity.

The maGLP binary Cold-call transaction is implemented in madGLP by: `global_send` with index 0 → Send → Receive (serializer case). The sender uses the well-known serializer address `_w(q,0)`, and the receiver extends its network input stream via the permanent index-0 entry.

### 9.3 Global Writers Table Lifecycle

An entry is added to the global writers table when a global link is established with the agent as the receiver: either when globalizing a writer (expecting the remote agent to send the value back) or when localizing a reader global name (expecting the globalizer to send the value). The entry is removed when the assignment arrives and the writer is bound.

### 9.4 Message Routing

Messages use global names to enable routing:

- Message `_w(p, i) := T` is sent to p (the original globalizer); p has an entry at index i in its global writers table
- Message `_r(p, i) := T` is sent from p to whoever localized `_r(p, i)`; that agent searches for an entry with remote agent p and remote index i

---

## 10. Example Scenarios

### 10.1 Direct Communication (Client-Monitor)

Consider the initial goal `client(Xs)@p, monitor(Xs?)@q`, establishing a shared pair with writer Xs at p and reader Xs? at q.

**Stage 0: Boot-Time Setup**

At boot time, each agent has its serializer entry created:
- p: entry `(N_p, *)` at index 0 for network input
- q: entry `(N_q, *)` at index 0 for network input

The initial shared pair `(Xs, Xs?)` is established via cold-call: p sends `global_send(msg(q, Xs?), _w(q,0), q)` through `send_to_net`. The term sent contains reader Xs?, so globalize processes a reader:

- p: globalize Xs? (reader) → spawns `global_send(Xs?, _r(p,1), q)`, global name `_r(p,1)`
- Cold-call message `_w(q,0) := [msg(q, _r(p,1)) | _w(q,0)]` sent to q
- q localizes `_r(p,1)`: creates pair `(Xs_q, Xs_q?)`, entry `(Xs_q, p, 1)`, puts Xs_q? (reader) in term
- q's resolvent contains Xs_q?

**Stage 1: p Assigns Xs**

p assigns Xs := [add|Xs1]:

1. Xs? becomes known (= [add|Xs1])
2. `global_send(Xs?, _r(p,1), q)` fires
3. The term [add|Xs1] is globalized for q: Xs1 is a writer, so entry `(Xs1, q)` at index 2 in W_p, becomes `_w(p,2)`
4. Message `_r(p,1) := [add|_w(p,2)]` sent to q

**Stage 2: q Receives**

q receives `_r(p,1) := [add|_w(p,2)]`:

1. Find entry `(Xs_q, p, 1)` by searching for `(p, 1)`
2. Localize `_w(p,2)`: create pair `(Xs1_q, Xs1_q?)`, spawn `global_send(Xs1_q?, _w(p,2), p)`, put Xs1_q (writer) in term
3. Assign Xs_q := [add|Xs1_q]
4. Remove entry `(Xs_q, p, 1)`

### 10.2 Return Value Scenario

p assigns Xs1 := [value(V?)|Xs2], exporting reader V? and writer Xs2:

1. Globalize V? (reader): spawns `global_send(V?, _r(p,3), q)`, becomes `_r(p,3)`
2. Globalize Xs2 (writer): entry `(Xs2, q)` at index 4 in W_p, becomes `_w(p,4)`
3. Message `_w(p,2) := [value(_r(p,3))|_w(p,4)]` sent to q

q receives and localizes:

1. For `_r(p,3)`: create pair `(V_q, V_q?)`, entry `(V_q, p, 3)`, put V_q? (reader) in term
2. For `_w(p,4)`: create pair `(Xs2_q, Xs2_q?)`, spawn `global_send(Xs2_q?, _w(p,4), p)`, put Xs2_q (writer) in term

When p assigns V := Sum (the client computes the return value):

1. V? becomes known at p (= Sum)
2. `global_send(V?, _r(p,3), q)` fires
3. Message `_r(p,3) := Sum↑` sent to q
4. q receives, finds entry `(V_q, p, 3)`, assigns V_q := Sum↓
5. V_q? becomes known in q's resolvent

### 10.3 Friend-Mediated Introduction

Bob introduces Alice to Charlie by sending reader X? to Alice and writer X to Charlie. Alice will receive values (she gets the reader end); Charlie will send values (he gets the writer end).

**Bob exports X? to Alice**:

Bob sends term containing X? to Alice via cold-call to Alice's serializer:

- Globalize X? (reader): spawns `global_send(X?, _r(bob,1), alice)`, no entry
- Cold-call message sent to Alice's serializer `_w(alice,0)`
- Alice localizes `_r(bob,1)`: creates pair `(X_a, X_a?)`, entry `(X_a, bob, 1)`, puts X_a? (reader) in term

Alice now holds reader X_a? and will receive values when the writer is assigned.

**Bob exports X to Charlie**:

Bob sends term containing X to Charlie via cold-call to Charlie's serializer:

- Globalize X (writer): entry `(X, charlie)` at index 2 in W_bob, becomes `_w(bob,2)`
- Cold-call message sent to Charlie's serializer `_w(charlie,0)`
- Charlie localizes `_w(bob,2)`: creates pair `(X_c, X_c?)`, spawns `global_send(X_c?, _w(bob,2), bob)`, puts X_c (writer) in term

Charlie now holds writer X_c and can assign values.

**State after introduction**:

- Bob: `global_send(X?, _r(bob,1), alice)` goal, entry `(X, charlie)` at index 2
- Alice: entry `(X_a, bob, 1)`, holds X_a?
- Charlie: `global_send(X_c?, _w(bob,2), bob)` goal, holds X_c

**Charlie sends a message**:

Charlie assigns X_c := T:

1. X_c? becomes known (= T)
2. `global_send(X_c?, _w(bob,2), bob)` fires
3. Message `_w(bob,2) := T↑` sent to Bob
4. Bob receives, finds entry `(X, charlie)` at index 2, assigns X := T↓
5. X? becomes known at Bob (= T)
6. `global_send(X?, _r(bob,1), alice)` fires
7. Message `_r(bob,1) := T↑` sent to Alice
8. Alice receives, finds entry `(X_a, bob, 1)`, assigns X_a := T↓
9. X_a? becomes known at Alice (= T)

The value flows from Charlie through Bob to Alice, with Bob's local pair `(X, X?)` serving as the forwarding point.

---

## 11. Implementation Notes

### 11.1 Global Name Allocation

Each agent maintains a counter for allocating global name indices. The counter is incremented for each variable globalized (either writer or reader).

### 11.2 Entry Lookup

For entries created by Globalize (form `(X, q)`), lookup is direct by index—the entry at index i corresponds to global name `_w(p, i)`.

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

The `'_send'(T, G, Q)` builtin behavior depends on whether G is a serializer address (index 0) or a normal global name (index > 0):

**Case G = `_w(q, 0)` (Serializer)**:
1. Globalizes term T for remote agent Q
2. Adds message `(_w(q,0) := [T↑ | _w(q,0)], Q)` to M_p — content wrapped in list cell, writer reused in tail

**Case G = `_w(p, i)` or `_r(p, i)` with i > 0 (Normal)**:
1. Globalizes term T for remote agent Q
2. Adds message `(G := T↑, Q)` to M_p — content sent directly

The destination Q is baked into all nested global links created during globalization:
- For each writer Y in T: creates entry `(Y, Q)` at p — Q identifies who will send the assignment back
- For each reader Y? in T: spawns `global_send(Y?, _r(p,i), Q)` — Q is the destination

This builtin is invoked only when the `global_send` goal's guard succeeds, ensuring the reader argument is bound.

---

## 12. External I/O System Predicates

This section specifies the system predicates for GLP-to-Dart communication. These predicates bridge the GLP world (streams, goals) with the Dart world (isolates, Flutter windows).

### 12.1 Overview

Each agent has two output channels:
- **Network output**: Messages to other agents, routed by Dart's IsolateManager
- **UI output**: Messages to the agent's Flutter window (local to isolate)

These channels are fundamentally different:
- Network output uses madGLP's `'_send'` builtin with globalization
- UI output uses a local `'_send_to_ui'` builtin without globalization (no variables cross the boundary)

### 12.2 Network Output: send_to_net/1

The network output stream carries cold-call messages of the form `msg(Q, T)` where Q is the destination agent and T is the term to send (which may contain variables).

**Definition [send_to_net System Predicate]**

The system predicate `send_to_net/1` reads cold-call messages from the network output stream and sends them via the serializer mechanism:

```prolog
procedure send_to_net(Stream?).
send_to_net([msg(Q, T) | In]) :-
    global_send(T?, _w(Q,0), Q?), send_to_net(In?).
send_to_net([]).
```

where:
- `Stream?` is the network output stream (reader)
- `_w(Q,0)` is the well-known serializer address for agent Q's network input

**Semantics**: For each `msg(Q, T)` in the stream:
1. Wait for the message to be known (via list pattern match)
2. Call `global_send(T?, _w(Q,0), Q?)` which:
   - Waits for T? to be known (the `known(T)` guard in `global_send`)
   - Globalizes T for agent Q, creating entries for nested writers and spawning `global_send` goals for nested readers
   - Adds message `(_w(Q,0) := [T↑ | _w(Q,0)], Q)` to M_p (serializer format)
3. Recurse with the tail

**Unified Mechanism**: Cold-calls use the same 3-argument `global_send(T, G, Q)` as established links (Section 4), with the serializer address `_w(Q,0)` as the global name G. This unifies cold-calls with regular communication—both use `global_send`, differing only in the target address.

### 12.3 Network Input: Receive via Serializer

The receiving side of a cold-call is handled by the Receive transaction's serializer case (Section 8.3). The GLP agent then reads from its network input stream like any other stream. The localized term contains only local variables, with global links established via the spawned `global_send` goals and entries.

**Note on Atomicity**: Cold-calls use unary transactions only: sender's Reduce (fires `global_send`) → Send → receiver's Receive (serializer case). No binary transaction is required. Correctness relies on monotonicity—once global links are established, values flow forward.

---

### 12.4 UI Output: send_to_ui/1

**Definition [send_to_ui System Predicate]**

The system predicate `send_to_ui/1` iterates over a stream and sends each ground element to the local Flutter window:

```prolog
procedure send_to_ui(Stream?).
send_to_ui([X|In]) :- ground(X?) | '_send_to_ui'(X?), send_to_ui(In?).
send_to_ui([]).
```

where:
- `Stream?` is the input stream of terms to send to the UI (reader)

**Semantics**: For each element X in the input stream:
1. Wait for X? to be ground (the `ground/1` guard)
2. Call `'_send_to_ui'(X?)` which delivers X to the Dart/Flutter layer
3. Recurse with the tail

**Guard Requirement**: The `ground(X?)` guard ensures no unbound variables cross the GLP-Dart boundary. UI messages must be fully instantiated.

### 12.5 The '_send_to_ui' Builtin

**Definition ['_send_to_ui' Builtin]**

The `'_send_to_ui'(T)` builtin:

1. Serializes term T (which must be ground)
2. Delivers T to the Dart layer via a Dart callback mechanism
3. The Dart layer forwards T to the Flutter window for display

This builtin does NOT:
- Globalize T (no variables, so nothing to globalize)
- Add to M_p (not routed through madGLP message system)
- Create global links (purely local to isolate)

**Dart Implementation**: The Dart runtime registers a callback that receives serialized terms from `'_send_to_ui'` and forwards them to the Flutter UI layer for rendering.

### 12.6 Comparison: Network vs UI Output

| Aspect | Network (`send_to_net`) | UI (`send_to_ui`) |
|--------|------------------------|-------------------|
| Goal | `global_send(T?, _w(Q,0), Q?)` | `'_send_to_ui'(T)` |
| Globalization | Yes (creates global links) | No |
| Variables allowed | Yes (globalized) | No (must be ground) |
| Guard | `known(T)` (in `global_send`) | `ground(X?)` |
| Routing | Via M_p → IsolateManager → Receive (serializer) | Direct to Dart callback |
| Destination | Remote agent Q's serializer (`_w(Q,0)`) | Local Flutter window |

### 12.7 UI Agent and Writer Binding

The architecture supports interactive user queries through **writer variables**. When the social agent needs user input (e.g., "accept this friend request?"), it sends a term containing an unbound writer to the UI. The user's response binds that writer, which flows back to the social agent.

**Example: Friend Request Protocol**

```prolog
%% Social agent sends befriend request with response writer
agent(Id, [msg(Id1, intro(From, Resp))|In], Fs) :-
    Id? =?= Id1? |
    lookup_send(user, befriend(From?, Resp), Fs?, Fs1),  %% Resp is a WRITER
    agent(Id?, In?, Fs1?).
```

The user sees: `befriend(alice, X35)` where `X35` is a writer.

The user responds by binding the writer:
- `X35 = accept(Ch)` — accept with a fresh channel
- `X35 = no` — reject

**UI Agent Validation with no_readers**

The `ui_agent` mediates between social agent and Dart, using `no_readers/1` guard to ensure output is safe:

```prolog
procedure ui_agent(Channel?, Channel?).

%% From social agent to user: wait until no readers, then forward
ui_agent(AgentCh, DartCh) :-
    receive(Msg, AgentCh?, AgentCh1),
    no_readers(Msg?) |
    send(Msg?, DartCh?, DartCh1),
    ui_agent(AgentCh1?, DartCh1?).

%% From user to social agent: pass through
ui_agent(AgentCh, DartCh) :-
    receive(Msg, DartCh?, DartCh1) |
    send(Msg?, AgentCh?, AgentCh1),
    ui_agent(AgentCh1?, DartCh1?).
```

**no_readers vs ground**

| Guard | Meaning | Use Case |
|-------|---------|----------|
| `ground(X?)` | X has no variables (fully instantiated) | Final values only |
| `no_readers(X?)` | X has no reader variables (writers OK) | Interactive queries |

The `no_readers` guard allows writers in output, enabling the query-response pattern where users bind writers to provide input.

**Future: Widget-Based Responses**

Currently, users type bindings as text (e.g., `X35 = accept(Ch)`). In future versions, the UI will render interactive widgets:

- `befriend(alice, X35)` → Button: [Accept] [Reject]
- Clicking [Accept] binds `X35 = accept(Ch)` with a fresh channel
- Clicking [Reject] binds `X35 = no`

The GLP semantics remain unchanged—only the UI rendering differs.

### 12.8 Usage Example

In an agent's initialization:

```prolog
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    %% Start UI output processor
    send_to_ui(UserOut?),
    %% Start network output processor (Q is established by Network transaction)
    %% ... handle messages from UserIn? and NetIn? ...
```

The `send_to_ui` goal consumes terms written to UserOut and delivers them to the Flutter window. Network output is handled by `global_send` goals spawned during globalization, not by an explicit `send_to_net` call in user code.

---

## 13. Invariants

The following invariants are maintained by madGLP:

**SRSW Property**: Within any agent's resolvent, each variable occurs at most once as a reader and at most once as a writer (inherited from GLP).

**Entry Lifecycle**: Every global writers table entry is created exactly once (by Globalize or Localize) and removed exactly once (by Receive). An entry is never modified between creation and removal.

**Send Atomicity**: When a `global_send` goal fires, the globalization of its term value (which may spawn additional `global_send` goals for nested variables) and the addition of the message to M_p occur atomically within the same Reduce transaction.

**Index Uniqueness**: Each (agent, index) pair uniquely identifies a global name. Indices are allocated sequentially and never reused, even after entry removal.

**Message Ordering**: Messages between any pair of agents are delivered in FIFO order. This ensures that if agent p sends two messages to agent q, q receives them in the order sent.

---

## 14. Security Extensions

The security extensions from the previous specification (Section 7) remain applicable. Each message in M_p can be cryptographically protected with attestation, signature, and encryption as described there.

---

## 15. Reserved Constants

Constants beginning with underscore (`'_...'`) are reserved for system use and MUST NOT be used in user programs.

### 15.1 System-Reserved Constants

| Constant | Purpose |
|----------|---------|
| `'_user'` | Identifies messages from the local user input channel |
| `'_net'` | Identifies the network output channel |
| `'_w(p,i)'` | Global writer name (agent p, index i) |
| `'_r(p,i)'` | Global reader name (agent p, index i) |

### 15.2 Enforcement

The GLP compiler rejects underscore-prefixed constants in user mode (default). System code that needs these constants must use the `-mode(system).` directive at the top of the file.

**Rationale**: Reserved constants prevent naming collisions between user-defined agent identifiers and system channels. Without this restriction, a user could name an agent `user` or `net`, causing ambiguity with system channel identifiers.

---

## 16. References

- **CGLP Paper**: `~/Grassroots/CGLP`, Section 7 "Multiagent Deterministic GLP (madGLP)"
- **Previous Spec**: `archive/irmaGLP-spec-v3.1-2026-01-30.md` (request-based model, now superseded)
- **Related Specs**:
  - `/docs/glp-runtime-spec.txt` - Single-agent runtime
  - `/docs/glp-bytecode-v216-complete.md` - Bytecode instruction set

---

## 17. Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 5.3 | 2026-02-10 | Claude | **Corrected Globalize/Localize direction**: Swapped gs/entry placement in Sections 5.1-5.4, 8.3, 9.3-9.4, 10.1-10.3, 11.2, 11.5, 12.2. Writer → entry at globalizer (receiver gets writer, sends back). Reader → gs at globalizer (globalizer keeps writer, sends to receiver). Updated all examples and remarks to match. Aligns with corrected paper appendix. |
| 5.2 | 2026-02-09 | Claude | Fixed cold-call polarity error in Receive serializer case (Section 8.3): changed `N_q := [T_q↓? \| N'_q]` to `N_q := [T_q↓ \| N'_q]`. Removed duplicate definition in Section 12.3, replaced with reference to 8.3. |
| 5.1 | 2026-02-02 | Claude | Added Section 15: Reserved Constants. Documents `'_user'`, `'_net'`, `'_w(p,i)'`, `'_r(p,i)'` as system-reserved. Describes `-mode(system).` directive for system code. Renumbered References→16, Document History→17. |
| 5.0 | 2026-02-02 | Claude | **Major revision**: Unified cold-calls with established links via index-0 serializer. Removed binary Network Transaction. Added Section 4.1 (Index-0 Serializer). Updated all examples to use indices starting at 1. Updated '_send' builtin with index check. Updated Receive transaction with serializer case. Updated send_to_net to use 3-arg global_send with _w(Q,0). All transactions now unary. |
| 4.6 | 2026-02-01 | Claude | Added Section 12.7: UI Agent and Writer Binding - documents no_readers guard for interactive queries, writer binding protocol, and future widget-based responses. Renumbered 12.7→12.8. |
| 4.5 | 2026-01-31 | Claude | Fixed Section 12: send_to_net uses 2-arg global_send for cold-calls, corrected section numbering (12.5-12.7), updated comparison table. |
| 4.4 | 2026-01-31 | Claude | Added Section 12: External I/O System Predicates (send_to_net/1, send_to_ui/1, '_send_to_ui' builtin). Renumbered subsequent sections. |
| 4.3 | 2026-01-31 | Claude | Clarified how remote agent identity is stored in entries and used during Receive to properly bake Q into nested global links during Localize. Enhanced '_send' builtin documentation. |
| 4.2 | 2026-01-30 | Claude | Verified alignment with revised paper. No "callback" terminology—consistently uses "assignment" throughout. |
| 4.0 | 2026-01-30 | Claude | Complete rewrite based on madGLP design from CGLP paper. Replaced request-based model with push-based global_send mechanism. Simplified global writers table. Added Globalize/Localize operations. |
| 4.1 | 2026-01-30 | Claude | Added Section 12 (Invariants). Clarified single-counter index allocation in Section 3.2. Added wire format clarification in Section 8.2. |
