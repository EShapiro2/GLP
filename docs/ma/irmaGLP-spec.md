# irmaGLP Specification

**Version**: 1.1  
**Date**: 2026-01-17  
**Status**: DRAFT (Incorporates Paper Issue Resolutions)  
**Source**: GLP-2025 Paper, Section "Smartphone Implementation-ready Multiagent Transition System for GLP" (Appendix)  
**Note**: This specification incorporates clarifications from `/docs/ma/irmaGLP-paper-issues-and-resolutions.md`

---

## 1. Overview

### 1.1 Purpose

This document specifies **Implementation-Ready Multiagent GLP (irmaGLP)**, a deterministic transition system for multiagent GLP execution suitable for smartphone and workstation deployment.

irmaGLP provides concrete data structures and message-passing mechanisms for cross-agent communication, replacing maGLP's abstract shared-variable model with explicit variable tables and message queues.

### 1.2 Relationship to Other Specifications

```
GLP Core (nondeterministic transition system)
    ↓
irGLP (deterministic single-agent with Q, S, F)
    ↓
maGLP (abstract multiagent with shared variables)
    ↓
irmaGLP (concrete multiagent with V_p, M_p) ← THIS SPEC
```

### 1.3 Scope

This specification defines:
- Local state structure for each agent (R_p, V_p, M_p)
- Three transaction types: Reduce, Communicate, Network
- Helper routines: abandon, request, export, reactivate
- Variable lifecycle management across agents

This specification does NOT define:
- Single-agent GLP semantics (see irGLP spec)
- Cryptographic mechanisms (deferred to security extensions)
- Network transport layer (implementation-specific)

---

## 2. Definitions and Notation

### 2.1 Basic Notation

- **Π**: Set of all agent identifiers
- **P ⊆ Π**: Set of active agents in computation
- **M**: GLP module (program) executed by all agents
- **V**: Set of writer variables
- **V?**: Set of reader variables (paired with writers)
- **𝒜**: Set of atoms (unit goals)
- **𝒯**: Set of terms
- **⊥**: Bottom (undefined/unbound)

### 2.2 Variable Locality

A variable Y is **fully local** to agent p if (Y, ·, ·) ∉ V_p, meaning both Y and its paired counterpart are in p's resolvent.

A variable Y is **non-local** to agent p if (Y, ·, ·) ∈ V_p, meaning its paired counterpart is in a remote agent's resolvent.

### 2.3 Global Variable Identity

Variables crossing agent boundaries require globally unique identifiers.

**Format**: `creator:localId`

**Components**:
- creator ∈ Π: agent who allocated this variable
- localId: unique integer within creator's heap

**Example**: `alice:1042` identifies variable with local ID 1042 created by agent alice.

When serializing terms for inter-agent transport, local variable IDs are replaced with global IDs to enable routing through variable tables.

### 2.4 Variable Roles

Each variable in V_p has one of three roles:

1. **Writer**: p holds writer X, paired reader X? is remote
2. **Created Reader**: p created reader X? (also created paired writer X), remote agent holds writer X
3. **Imported Reader**: p received reader X? from another agent, remote agent q created both X and X?

---

## 3. Local State Structure

### 3.1 Implementation-Ready Resolvent (R_p)

**Definition**: The resolvent R_p = (A_p, S_p, F_p) partitions goals into three categories:

#### Active Goals (A_p ∈ 𝒜*)
- FIFO queue of goals ready for reduction
- Goals are selected from head of queue (deterministic scheduling)
- Format: A_p = A₁ · A₂ · ... · Aₙ (sequence notation)

#### Suspended Goals (S_p ⊆ 𝒜 × 2^(V?))
- Set of pairs (Goal, BlockingReaders)
- Each (A, W) ∈ S_p represents goal A suspended on reader set W
- W contains all readers from suspension sets across all clause attempts
- When any reader X? ∈ W receives a value or is abandoned, A reactivates

#### Failed Goals (F_p ⊆ 𝒜)
- Set of goals that failed definitively (no clause succeeded or suspended)
- Terminal state for fault analysis
- Goals in F_p never reactivate

**Invariant**: Each goal appears in exactly one of A_p, S_p, or F_p.

### 3.2 Variable Table (V_p)

**Definition**: V_p ⊆ (V ∪ V?) × Π × (𝒯 ∪ Π ∪ {⊥})

The variable table V_p is a set of triples (Y, creator, state) where:

#### Entry Structure
For each (Y, q, s) ∈ V_p:

1. **Writer Entry**: Y ∈ V (writer variable)
   - q ∈ Π: agent who created Y (**INVARIANT: q = p always** - writers are never imported)
   - s ∈ 𝒯 ∪ {⊥}: current value (⊥ if unbound)
   - Paired reader Y? is non-local

2. **Created Reader Entry**: Y ∈ V? (reader variable), q = p
   - q = p: this agent created both Y? and paired writer Y
   - s ∈ Π ∪ {⊥}: requester agent (⊥ if no request yet)
   - Writer Y is non-local

3. **Imported Reader Entry**: Y ∈ V? (reader variable), q ≠ p
   - q ∈ Π: agent who created Y? and paired writer Y
   - s = q if read request sent to creator, s = ⊥ otherwise
   - Both Y? and writer Y are non-local to p

#### Core Invariant

**V_p contains exactly those variables whose paired counterparts are non-local.**

- Variables with both parts local to p do NOT appear in V_p
- Variables with one part in p's resolvent and counterpart remote MUST appear in V_p
- When a variable becomes fully local (both parts in p), remove from V_p
- When a variable is exported (leaves p), add to V_p if created by p

### 3.3 Message Queue (M_p)

**Definition**: M_p is a set of pending messages as pairs (content, destination).

Each message (m, q) ∈ M_p where q ∈ Π is the destination agent.

#### Message Types

1. **Assignment**: (X?:=T, q)
   - Reader X? is local to agent q
   - Term T is the value to assign (may be ⊥ for abandonment)
   - Generated when p binds a writer whose paired reader is in V_p

2. **Read Request**: (request(X?, p), q)
   - Agent p requests value of X? from creator q
   - Generated when p needs value of imported reader X?
   - One request per reader (idempotent)

3. **Abandon Notification**: (abandon(Y), q)
   - Reader Y? became unreachable at p
   - Notifies agent q that paired writer Y can be cleaned up
   - Message contains the writer Y (computed from reader Y?)

#### Queue Properties

- FIFO per destination: messages to same agent delivered in order
- At-most-once delivery: each message delivered at most once
- Eventual delivery: assuming network connectivity

---

## 4. Helper Routines

These routines are called during transactions to maintain variable table and message queue consistency.

### 4.1 abandon(Y?) for agent p

**Purpose**: Notify other agents when reader Y? becomes unreachable.

**Input**: Reader variable Y? that is being abandoned by agent p

**Effect**: Updates V'_p and M'_p

**CRITICAL**: An agent can only abandon a **reader**, which causes its dual writer to be abandoned at the remote agent.

**Algorithm**:
```
Let Y = paired writer of Y? (conceptually Y? without the ? marker)

If (Y, q, s) ∈ V_p where q ≠ p:
  // Imported variable - notify creator
  Remove (Y, q, s) from V'_p
  Add (abandon(Y'), q) to M'_p

Else if (Y, p, s) ∈ V_p where s ≠ ⊥:
  // Created reader with requester - notify requester
  Remove (Y, p, s) from V'_p
  Add (abandon(Y'), s) to M'_p

Else:
  // Local abandonment only
  Remove (Y, ·, ·) from V'_p if present
```

**Notes**:
- Sends paired variable Y' in message (receiver needs to know which part was abandoned)
- Only notifies if there's a remote party that needs to know

### 4.2 request(X?) for agent p

**Purpose**: Send read request for imported reader that hasn't been requested yet.

**Input**: Reader variable X? imported from agent q

**Effect**: Updates V'_p and M'_p

**Precondition**: (X?, q, ⊥) ∈ V_p where q ≠ p

**Algorithm**:
```
If (X?, q, ⊥) ∈ V'_p and q ≠ p:
  // Reader imported but not yet requested
  Update entry to (X?, q, q) in V'_p
  Add (request(X?, p), q) to M'_p
```

**Notes**:
- Idempotent: request sent only once (state changes from ⊥ to q)
- Subsequent calls with same X? have no effect

### 4.3 reactivate(X?) for agent p

**Purpose**: Find and reactivate all goals suspended on reader X?.

**Input**: Reader variable X? that just received a value or was abandoned

**Output**: Set R of goals to reactivate

**Effect**: Updates S'_p

**Algorithm**:
```
Let R = {G : (G, W) ∈ S'_p and X? ∈ W}

For each (G, W) where G ∈ R:
  Remove (G, W) from S'_p

Return R
```

**Notes**:
- Goals in R should be appended to A'_p
- Each goal reactivates at most once per suspension

### 4.4 export(T) for agent p

**Purpose**: Update variable table when term T is sent outside agent p.

**Input**: Term T being exported from agent p

**Output**: Term T' (possibly modified with relay variables)

**Effect**: Updates V'_p and A'_p

**Algorithm**:
```
Set T' := T

For each variable Y occurring in T:
  
  If Y created by p and (Y, p, ·) ∉ V'_p:
    // Local variable being exported for first time
    Add (Y, p, ⊥) to V'_p
  
  Else if Y created by agent q where q ≠ p:
    // Non-local variable
    
    If Y ∈ V or (Y, q, ⊥) ∈ V'_p:
      // Writer or non-requested reader - just remove
      Remove (Y, q, ·) from V'_p
    
    Else if (Y, q, q) ∈ V'_p:
      // Requested reader - needs relay
      Create fresh pair (Z, Z?)
      Replace Y with Z? in T'
      Add goal forward(Y?, Z) to A'_p
      Add (Z?, p, ⊥) to V'_p

Return T'
```

**Notes**:
- Relay mechanism: when re-exporting a requested reader, create forwarding goal
- The forwarding goal ensures values propagate through the relay chain
- Fresh pair (Z, Z?) allows original reader to leave while maintaining request relationship

**Forwarding Goal Semantics**:

The forwarding goal `relay(Y, Z)` is defined as:
```prolog
relay(Y, Z) :- known(Z?) | Y = Z?.
```

This ensures that when relay reader Z? receives a value V, the original writer Y
is bound to V, maintaining the request relationship across export boundaries.

**How it works**:
1. Goal suspends on Z? until Z? is bound
2. When Z? receives value V, the guard `known(Z?)` succeeds
3. Body executes: Y = Z? unifies writer Y with value V
4. The value propagates from relay reader back to original writer

---

## 5. Transactions

### 5.1 Reduce Transaction (Unary)

**Definition**: For agent p with local state (R_p, V_p, M_p) where R_p = (A_p, S_p, F_p) and A_p = A · A_r (A is head goal).

**Transition**: (R_p, V_p, M_p) → (R'_p, V'_p, M'_p)

**Initialize**: (R'_p, V'_p, M'_p) := (R_p, V_p, M_p)

**Three Cases**:

#### Case 1: Reduction Succeeds

If GLP reduction of A with first applicable clause C ∈ M succeeds with (B, σ̂):

1. **Reactivate suspended goals**:
   ```
   For each {X?:=T} ∈ σ̂?:
     Let R_X = reactivate(X?)  // modifies S'_p
   Let R = union of all R_X
   ```

2. **Update active queue**:
   ```
   A'_p := (A_r · B · R)σ̂σ̂?
   ```
   Apply both writer substitution σ̂ and reader substitution σ̂? to:
   - Remaining active goals A_r
   - New body goals B
   - Reactivated goals R

3. **Update variable table for assigned readers**:
   ```
   Let W = {X? : {X?:=T} ∈ σ̂?} be the set of readers assigned by this reduction
   
   For each X? ∈ W where (X?, q, ⊥) ∈ V'_p:
     Update to (X?, q, q) in V'_p
     // Marks that reader was assigned; paired writer also assigned
   ```
   
   **Rationale**: When a reader gets a value, its paired writer is implicitly bound.
   We mark imported readers that received assignments so we can track their state.

4. **Queue messages for remote readers**:
   ```
   For each {X?:=T} ∈ σ̂?:
     If (X?, p, r) ∈ V'_p where r ≠ ⊥:
       // Reader X? was created by p, agent r requested it
       Add (X?:=T, r) to M'_p
   ```

5. **Handle abandoned readers**:
   ```
   For each reader Y? in A:
     If Y? not instantiated by σ̂? and Y? not in B:
       Call abandon(Y?)  // modifies V'_p and M'_p
   ```
   
   **Note**: Only readers can be abandoned. A reader Y? is abandoned when it
   disappears from the computation without being instantiated.

6. **Set result**:
   ```
   R'_p := (A'_p, S'_p, F'_p)
   ```

#### Case 2: Goal Suspends

Else if W = ⋃(C∈M) W_C ≠ ∅ (union of suspension sets across all clause attempts):

1. **Remove from active queue**:
   ```
   A'_p := A_r
   ```

2. **Add to suspended set**:
   ```
   S'_p := S'_p ∪ {(A, W)}
   ```

3. **Send read requests**:
   ```
   For each X? ∈ W:
     Call request(X?)  // modifies V'_p and M'_p
   ```

4. **Set result**:
   ```
   R'_p := (A'_p, S'_p, F'_p)
   ```

#### Case 3: Goal Fails

Otherwise (no clause succeeded or suspended):

1. **Remove from active queue**:
   ```
   A'_p := A_r
   ```

2. **Add to failed set**:
   ```
   F'_p := F'_p ∪ {A}
   ```

3. **Abandon all readers in goal**:
   ```
   For each reader Y? in A:
     Call abandon(Y?)  // modifies V'_p and M'_p
   ```

4. **Set result**:
   ```
   R'_p := (A'_p, S'_p, F'_p)
   ```

### 5.2 Communicate Transaction (Binary)

**Definition**: For agents p ≠ q with message (m, q) ∈ M_p.

**Transition**: (c_p, c_q) → (c'_p, c'_q)

**Initialize**: (c'_p, c'_q) := (c_p, c_q)

**Remove message**: Remove (m, q) from M'_p

**Two Message Types**:

#### Type 1: Assignment Message

If m = (X?:=T) where X? is local to agent q:

1. **Reactivate suspended goals**:
   ```
   Let R = reactivate(X?)  // for agent q, modifies S'_q
   ```

2. **Apply assignment**:
   ```
   If T ≠ ⊥:
     A'_q := (A_q · R){X?:=T}
     Apply {X?:=T} to S'_q and F_q
   Else:
     // Abandonment notification
     A'_q := A_q · R
   ```

3. **Remove from variable table**:
   ```
   Remove (X?, ·, ·) from V'_q
   ```

4. **Import variables from T**:
   ```
   For each variable Y in T where (Y, ·, ·) ∉ V'_q:
     // Y is fully local to q - both parts in q's resolvent
     If Y created by agent r:
       Add (Y, r, ⊥) to V'_q
   ```

#### Type 2: Read Request Message

If m = request(X?, p):

1. **Check for abandonment request**:
   ```
   If p = ⊥:
     Call abandon(X?) for agent q  // modifies V'_q and M'_q
     Return
   ```

2. **Record requester if unbound**:
   ```
   If (X?, q, ⊥) ∈ V'_q:
     Update to (X?, q, p) in V'_q
   ```

3. **Reply if bound**:
   ```
   Else if (X, q, T) ∈ V'_q:
     // Writer X bound to T
     Add (X?:=T, p) to M'_q
   ```

### 5.3 Network Transaction (Binary)

**Definition**: For agents p ≠ q when msg(q, X) appears in p's network output stream.

**Transition**: (c_p, c_q) → (c'_p, c'_q)

**Initialize**: (c'_p, c'_q) := (c_p, c_q)

**Process Cold-Call**:

1. **Export term from p**:
   ```
   X' := export(X)  // for agent p, modifies V'_p and M'_p
   ```

2. **Deliver to q's network input**:
   ```
   Add X' to q's network input stream
   ```

3. **Import variables into q**:
   ```
   For each variable Y in X' where (Y, ·, ·) ∉ V'_q:
     // Y is fully local to q - both parts in q's resolvent
     If Y created by agent r:
       Add (Y, r, ⊥) to V'_q
   ```

**Notes**:
- Network transactions represent "cold-call" befriending
- Agent p sends term via network channel without prior relationship
- Export/import ensures variable table consistency

---

## 6. Variable Lifecycle

### 6.1 Creation

Variables are created during clause reduction:
1. Fresh pair (X, X?) allocated on heap
2. Writer X appears in body, reader X? also appears or is implicit
3. If reduction succeeds, both parts initially local

### 6.2 Export

When term containing variable Y leaves agent p:
1. Call export(Y) before sending
2. If Y created by p: add (Y, p, ⊥) to V_p
3. If Y imported: handle based on request status

### 6.3 Import

When term containing variable Y arrives at agent q:
1. If Y created by agent r ≠ q: add (Y, r, ⊥) to V_q
2. Y becomes local to q but marked as non-local origin

### 6.4 Assignment

When writer X is bound to term T at agent p:
1. If reader X? is local: apply {X?:=T} immediately
2. If (X?, p, r) ∈ V_p where r ≠ ⊥: queue (X?:=T, r) to M_p
3. Call reactivate(X?) to wake suspended goals

### 6.5 Request

When agent p needs value of imported reader X?:
1. Check (X?, q, s) ∈ V_p
2. If s = ⊥: call request(X?) which sends request to q
3. If s = q: request already sent, wait for reply

### 6.6 Abandonment

When variable Y becomes unreachable at agent p:
1. Call abandon(Y)
2. Notify remote agents as needed
3. Remove from V_p
4. Reactivate suspended goals (they will fail)

---

## 7. Correctness Properties

### 7.1 Variable Table Invariants

**Completeness**: Every variable with a non-local counterpart appears in V_p.

**Exclusivity**: No variable appears in V_p if both parts are local to p.

**Consistency**: Creator field matches the agent that allocated the variable.

**State Accuracy**:
- Writer state reflects current binding (T or ⊥)
- Reader state reflects requester (agent or ⊥)

### 7.2 Message Queue Invariants

**FIFO per Destination**: Messages to same agent delivered in order.

**At-Most-Once**: Each message delivered at most once.

**Eventual Delivery**: All queued messages eventually delivered (assuming connectivity).

### 7.3 Suspension Invariants

**Blocking Reader Accuracy**: W in (A, W) ∈ S_p contains exactly those readers preventing A's reduction.

**Single Reactivation**: Each suspended goal reactivates at most once per suspension episode.

**Progress**: If any reader X? ∈ W receives value, goal A reactivates.

### 7.4 Global Properties

**Determinism**: Given same initial configuration and message delivery order, execution is deterministic.

**Fairness**: Active goals eventually reduce (FIFO scheduling).

**Termination**: If all agents quiesce (empty A_p and M_p), computation is complete.

---

## 8. Implementation Notes

### 8.1 Data Structures

**Active Queue**: Implement as FIFO queue (e.g., `Queue<Goal>` in Dart)

**Suspended Set**: Implement as map from readers to goals (e.g., `Map<int, Set<Goal>>`)

**Variable Table**: Implement as map from varId to entry (e.g., `Map<int, VarEntry>`)

**Message Queue**: Implement as queue per destination (e.g., `Map<AgentId, Queue<Message>>`)

### 8.2 Global Variable Identity

Variables crossing agent boundaries need globally unique IDs:

**Format**: `creator:localId`

**Example**: `alice:1042` for variable with local ID 1042 created by alice

**Implementation**: When serializing terms, replace local IDs with global IDs.

### 8.3 Serialization

Terms must be serialized to bytes for inter-agent transport:

1. **Constants**: Type tag + value bytes
2. **Variables**: Type tag + global ID string
3. **Structures**: Type tag + functor + arity + serialized args
4. **Lists**: Convert to structure form, then serialize

**Round-trip requirement**: deserialize(serialize(T)) ≡ T (up to variable renaming)

### 8.4 Scheduler Integration

**Reduce Phase**:
1. While A_p non-empty: execute Reduce transaction
2. Check for reactivations after each reduction
3. Stop when A_p empty (agent quiescent)

**Communicate Phase**:
1. Deliver messages from M_p to destination agents
2. May cause reactivations
3. Stop when all M_p empty (system quiescent)

**Network Phase**:
1. Process network channel outputs
2. Deliver to network channel inputs
3. May initiate new computation

---

## 9. Extensions

### 9.1 Security (Deferred)

Future extensions will add:
- Agent identity via public key cryptography
- Message authentication and encryption
- TEE attestation for code verification
- Byzantine agent detection

See GLP-2025 paper "Extensions for Secure Multiagent GLP" section.

### 9.2 Persistence (Deferred)

Future extensions will add:
- Checkpoint/restore of (R_p, V_p, M_p)
- Durable message queues
- Crash recovery protocols

### 9.3 Optimization (Deferred)

Future optimizations:
- Batch message delivery
- Lazy reactivation
- Variable table compression
- Message coalescing

---

## 10. References

- **GLP-2025 Paper**: "The Art of Grassroots Logic Programming" (2025)
  - Section 3: GLP Core
  - Section 5: Multiagent GLP
  - Appendix: Implementation-Ready Transition Systems

- **Related Specs**:
  - `/docs/glp-runtime-spec.txt` - Single-agent runtime
  - `/docs/glp-bytecode-v216-complete.md` - Bytecode instruction set
  - `/docs/irmaGLP-implementation-plan.md` - Implementation roadmap

---

## Appendix A: Formal Notation Summary

| Symbol | Meaning |
|--------|---------|
| Π | Set of all agent identifiers |
| P ⊆ Π | Set of active agents |
| M | GLP module (program) |
| R_p = (A_p, S_p, F_p) | Resolvent: active, suspended, failed |
| V_p | Variable table |
| M_p | Message queue |
| σ̂ | Writer substitution (tentative during HEAD) |
| σ̂? | Reader substitution (paired with σ̂) |
| W | Suspension set (blocked readers) |
| ⊥ | Bottom (undefined) |

---

## Appendix B: Example Execution Trace

**Scenario**: Agent alice sends message to agent bob

**Initial State**:
```
alice: A_p = [send(bob, hello)]
       V_p = {(bob, alice, ⊥)}  // bob is friend channel writer
       M_p = {}

bob:   A_p = []
       V_p = {}
       M_p = {}
```

**Step 1: alice reduces send(bob, hello)**
```
Clause: send(To, Msg) :- lookup_send(To?, msg(alice, To?, Msg?), Fs?, ...)
Reduction succeeds with σ̂ = {To := bob, Msg := hello, ...}
σ̂? = {To? := bob, Msg? := hello, ...}

After reduction:
alice: A_p = [lookup_send(bob, msg(alice, bob, hello), ...)]
       V_p = {(bob, alice, ⊥)}
       M_p = {}
```

**Step 2: alice reduces lookup_send(...)**
```
Finds bob in friends list, binds output channel
Writes msg(alice, bob, hello) to bob's output stream
This creates assignment for bob's input reader

After reduction:
alice: A_p = []
       V_p = {(bob, alice, msg(alice, bob, hello))}
       M_p = {(bob?:=msg(alice, bob, hello), bob)}
```

**Step 3: Communicate transaction delivers message**
```
Message (bob?:=msg(alice, bob, hello), bob) sent from alice to bob
Bob's reader bob? receives the message

After delivery:
bob:   A_p = [process(msg(alice, bob, hello))]  // reactivated goal
       V_p = {}  // bob? removed after assignment
       M_p = {}
```

**Result**: Message successfully routed from alice to bob via V_p/M_p mechanism.

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-17 | Claude | Initial draft from GLP-2025 paper |
| 1.1 | 2026-01-17 | Claude | Incorporated paper issue resolutions |
