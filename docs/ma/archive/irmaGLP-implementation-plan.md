# irmaGLP Implementation Plan

**Status**: Implementation Ready  
**Date**: January 2026  
**Prerequisite**: Working glp_multiagent demo with Alice↔Bob↔Charlie  

## 1. Current State

We have a working multiagent demo where:
- Each agent runs in a separate Flutter window (Dart isolate)
- Agents communicate via Dart coordinator routing
- Messages contain `msg(From, To, Content)` - Dart routes by `To` field
- Each agent has pre-configured friends list
- GLP enforces "can only send to friends"

**What's missing for true irmaGLP:**
- V_p (Variable Table): No tracking of non-local variables
- M_p (Message Queue): No outbound message buffering
- Shared channels: Friends don't truly share GLP variables
- Friend introduction creates channels in GLP, but Dart can't observe them

## 2. Why irmaGLP is Needed

The friend-mediated introduction protocol requires:

```prolog
%% Bob introduces Alice to Charlie
social_graph(Id, [msg(user, Id?, introduce(P, Q))|In], Fs) :-
    new_channel(ch(PQ, QP?), ch(QP, PQ?)) |
    lookup_send(P?, msg(Id?, P?, intro(Q?, ch(QP?, PQ))), Fs?, Fs1),
    lookup_send(Q?, msg(Id?, Q?, intro(P?, ch(PQ?, QP))), Fs1?, Fs2),
    ...
```

This creates a **shared channel** `ch(PQ, QP)` between Alice and Charlie:
- Alice gets writer `PQ` and reader `QP?`
- Charlie gets writer `QP` and reader `PQ?`
- When Alice writes to `PQ`, Charlie's `PQ?` should receive the value

**Problem:** Alice and Charlie run in separate Dart isolates. They cannot share GLP heap variables directly.

**Solution:** irmaGLP's V_p tracks which variables have remote counterparts, and M_p routes assignments between agents.

## 3. irmaGLP Architecture

### 3.1 Local State per Agent

Each agent p has:
- **R_p**: Resolvent (active goals, suspended goals, failed goals)
- **V_p**: Variable table (tracks non-local variable counterparts)
- **M_p**: Message queue (outbound messages)

### 3.2 Variable Table (V_p)

```dart
class VariableTable {
  final Map<int, VariableEntry> _entries = {};
}

class VariableEntry {
  final int varId;
  final String creator;      // Agent who created this variable
  final VariableRole role;   // writer, createdReader, importedReader
  dynamic state;             // For writers: bound value or null
                             // For readers: requester agent or null
}

enum VariableRole {
  writer,           // We hold writer, reader is remote
  createdReader,    // We created reader, writer is remote
  importedReader,   // We received this reader from another agent
}
```

**Invariant:** V_p contains exactly those variables whose paired counterparts are non-local.

### 3.3 Message Queue (M_p)

```dart
class MessageQueue {
  final Queue<OutboundMessage> _queue = Queue();
}

class OutboundMessage {
  final String destination;
  final MessageType type;
  final List<int> payload;  // Serialized content
}

enum MessageType {
  assignment,    // (X?:=T, destination)
  readRequest,   // (request(X?, requester), destination)
  abandon,       // (abandon(X), destination)
}
```

### 3.4 Global Variable Identity

Variables crossing agent boundaries need globally unique IDs:

```
Format: creator:localId
Example: alice:1042
```

When Alice sends a term containing her local variable 42 to Bob, Bob's GLPSAM records it as an imported variable from Alice.

## 4. Transactions

### 4.1 Reduce Transaction (Local)

When agent p reduces a goal:

1. Standard GLPSAM reduction
2. For each binding `X = T`:
   - If reader X? is local: apply binding locally
   - If reader X? is in V_p (remote): queue `(X?:=T, destination)` to M_p
3. For each abandoned variable Y:
   - Call `abandon(Y)` helper

### 4.2 Communicate Transaction (Cross-Agent)

When message `(m, q)` is delivered from M_p to agent q:

**Assignment `(X?:=T)`:**
1. Reactivate goals suspended on X?
2. Apply binding if T ≠ ⊥
3. Remove X? from V_q
4. For each variable Y in T not local to q: add to V_q

**Read Request `request(X?, p)`:**
1. If p = ⊥: abandon X?
2. Else if X? unbound in V_q: record requester p
3. Else if X? bound to T in V_q: queue `(X?:=T, p)` to M_q

**Abandon `abandon(X)`:**
1. Mark X as abandoned
2. Reactivate goals suspended only on X (they will fail)

### 4.3 Network Transaction (Cold-Call)

When `msg(q, X)` appears in p's network output:

1. `X' := export(X)` for agent p
2. Add X' to q's network input
3. Update V_q for variables in X'

## 5. Helper Routines

### 5.1 Export

When term T leaves agent p:

```dart
Term export(Term t, String agentId, VariableTable vp, MessageQueue mp) {
  // Traverse term, for each variable:
  // - If created by us and not in V_p: add to V_p
  // - If imported reader being re-exported: create relay
  // Return term with global variable IDs
}
```

### 5.2 Reactivate

When variable X? receives value or is abandoned:

```dart
List<Goal> reactivate(int varId, SuspensionSet sp) {
  // Find all goals suspended on this variable
  // Remove from suspension set
  // Return goals to re-enqueue
}
```

### 5.3 Abandon

When variable X becomes unreachable:

```dart
void abandon(int varId, VariableTable vp, MessageQueue mp) {
  final entry = vp.lookup(varId);
  if (entry == null) return;
  
  if (entry.role == VariableRole.importedReader) {
    // Notify creator
    mp.add(OutboundMessage(
      destination: entry.creator,
      type: MessageType.abandon,
      payload: serialize(varId),
    ));
  } else if (entry.role == VariableRole.createdReader && entry.state != null) {
    // Notify requester
    mp.add(OutboundMessage(
      destination: entry.state,  // requester
      type: MessageType.abandon,
      payload: serialize(varId),
    ));
  }
  
  vp.remove(varId);
}
```

## 6. Payload Serialization

### 6.1 Term Serialization

```dart
List<int> serializeTerm(Term term, String agentId) {
  // Recursive serialization:
  // - Constants: type tag + value bytes
  // - Variables: type tag + global ID (creator:localId)
  // - Structs: type tag + functor + arity + args
}

Term deserializeTerm(List<int> bytes, VariableTable vp) {
  // Recursive deserialization:
  // - Variables: lookup/create in local heap, update V_p
}
```

### 6.2 Message Serialization

```dart
List<int> serializeMessage(OutboundMessage msg) {
  // type tag + destination + payload
}

(String destination, MessageType type, List<int> payload) 
    deserializeMessage(List<int> bytes) {
  // Parse message header and payload
}
```

## 7. Integration with Existing Code

### 7.1 Files to Create

```
glp_runtime/lib/multiagent/
├── variable_table.dart      # V_p implementation
├── message_queue.dart       # M_p implementation  
├── payload_serializer.dart  # Term/message serialization
├── irma_context.dart        # AgentContext with V_p, M_p
└── helpers.dart             # export, reactivate, abandon
```

### 7.2 Files to Modify

**glp_runtime/lib/runtime/runtime.dart:**
- Add hooks for binding notifications
- Add hooks for variable abandonment

**glp_runtime/lib/runtime/scheduler.dart:**
- Integrate with V_p/M_p after each reduction
- Process M_p after quiescence

**glp_multiagent/lib/main.dart:**
- Replace SimpleRouter with irmaGLP message routing
- Use payload serialization for MethodChannel
- Remove `To` field routing, use V_p instead

### 7.3 GLP Program Changes

**social_agent.glp** - No changes needed! The GLP programs work the same way. The difference is in how Dart handles variables that cross agent boundaries.

## 8. Implementation Phases

### Phase 1: V_p Implementation

1. Create `variable_table.dart`
2. Unit tests for V_p operations
3. Track variables when terms are exported

### Phase 2: M_p Implementation

1. Create `message_queue.dart`
2. Unit tests for M_p operations
3. Queue messages when binding remote readers

### Phase 3: Serialization

1. Create `payload_serializer.dart`
2. Round-trip tests for all term types
3. Global variable ID encoding/decoding

### Phase 4: Integration

1. Hook V_p/M_p into runtime
2. Update scheduler to flush M_p
3. Update multiagent app to use serialized payloads

### Phase 5: Testing

1. Two-agent variable synchronization
2. Three-agent introduction (Alice↔Bob↔Charlie, Bob introduces)
3. Abandonment propagation

## 9. Test Scenarios

### 9.1 Basic Variable Sharing

```
Alice creates ch(X, Y?)
Alice sends ch(X, Y?) to Bob
Alice writes X = hello
Bob's Y? should receive hello
```

### 9.2 Friend-Mediated Introduction

```
Initial: Alice↔Bob, Bob↔Charlie, Alice and Charlie not friends

1. Bob receives: introduce(alice, charlie)
2. Bob creates: ch(AC, CA?) for Alice, ch(CA, AC?) for Charlie
3. Bob sends to Alice: intro(charlie, ch(CA?, AC))
4. Bob sends to Charlie: intro(alice, ch(AC?, CA))
5. Alice accepts: adds (charlie, AC) to friends
6. Charlie accepts: adds (alice, CA) to friends
7. Alice writes AC = hello
8. Charlie's AC? receives hello
9. Alice and Charlie can now communicate directly
```

### 9.3 Abandonment

```
Alice sends reader X? to Bob
Bob doesn't use X?, it gets garbage collected
Bob's V_p abandon helper notifies Alice
Alice's writer X is marked abandoned
```

## 10. Success Criteria

1. **Variable synchronization works**: Bindings propagate between agents
2. **Introduction protocol completes**: Alice and Charlie become friends via Bob
3. **No Dart-level routing by message content**: Routing based purely on V_p
4. **Abandonment propagates**: Unreachable variables are cleaned up
5. **Serialization is opaque**: Dart only sees bytes, not term structure

## 11. Future Work

After irmaGLP is working:
- Cold-call befriending (via network channel)
- TEE attestation guards
- Encrypted payloads
- Persistent V_p/M_p for crash recovery

---

## Appendix A: Variable Table Invariants

1. **Completeness**: Every variable with a non-local counterpart is in V_p
2. **Exclusivity**: No variable appears in V_p if both parts are local
3. **Consistency**: Creator field matches the agent that allocated the variable
4. **State accuracy**: Writer state reflects current binding; reader state reflects requester

## Appendix B: Message Queue Invariants

1. **FIFO per destination**: Messages to same agent delivered in order
2. **At-most-once**: Each message delivered at most once (Dart handles retries if needed)
3. **Eventual delivery**: All queued messages eventually delivered (assuming connectivity)

## Appendix C: Coordinator Role

The coordinator:
1. Routes serialized payloads between agents (dumb pipe)
2. Spawns/kills agent windows
3. Does NOT interpret message content
4. Does NOT maintain its own V_p for agent variables

The coordinator may have its own GLPSAM for simulation control, but that's separate from agent communication.
