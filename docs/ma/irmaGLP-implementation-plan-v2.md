# irmaGLP Implementation Plan (Revised)

**Version**: 2.2  
**Date**: 2026-01-17  
**Status**: Phase 5 Complete (128 tests passing)  
**Prerequisite**: Working glp_multiagent demo with Alice↔Bob↔Charlie  
**Source Spec**: `/docs/ma/irmaGLP-spec.md`

---

## CHANGES FROM VERSION 1.0

This version incorporates all corrections from the paper issue resolutions:
1. ✅ abandon() takes **reader** Y? as parameter (not variable Y)
2. ✅ Writer entry INVARIANT: creator q = p always
3. ✅ W in Reduce = domain(σ̂?) (readers assigned by reduction)
4. ✅ export_reader clause defined: `known(Z?) | Y = Z?.`
5. ✅ "Fully local" = (Y, ·, ·) ∉ V_p notation
6. ✅ Global variable ID formalized: creator:localId
7. ✅ Only readers can be abandoned

See `/docs/ma/irmaGLP-paper-issues-and-resolutions.md` for details.

---

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

---

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

---

## 3. irmaGLP Architecture

### 3.1 Local State per Agent

Each agent p has:
- **R_p = (A_p, S_p, F_p)**: Resolvent (active goals, suspended goals, failed goals)
- **V_p**: Variable table (tracks non-local variable counterparts)
- **M_p**: Message queue (outbound messages)

### 3.2 Variable Table (V_p)

**Dart Implementation**:

```dart
class VariableTable {
  final Map<int, VariableEntry> _entries = {};
  
  // Core operations
  void add(int varId, VariableEntry entry) { ... }
  void remove(int varId) { ... }
  VariableEntry? lookup(int varId) { ... }
  List<VariableEntry> getByCreator(String creator) { ... }
}

class VariableEntry {
  final int varId;
  final String creator;      // Agent who created this variable
  final VariableRole role;   // writer, createdReader, importedReader
  dynamic state;             // See state semantics below
  
  VariableEntry(this.varId, this.creator, this.role, [this.state]);
}

enum VariableRole {
  writer,           // We hold writer, reader is remote
  createdReader,    // We created reader, writer is remote
  importedReader,   // We received this reader from another agent
}
```

**State Semantics**:
- **Writer entry** (role = writer):
  - state ∈ {Term, null}: bound value or null if unbound
  - **INVARIANT**: creator must equal current agent (writers never imported)
  
- **Created reader entry** (role = createdReader):
  - state ∈ {String, null}: requester agent ID or null if no request yet
  - creator must equal current agent
  
- **Imported reader entry** (role = importedReader):
  - state ∈ {String, null}: creator ID if request sent, null otherwise
  - creator ≠ current agent

**Core Invariant:** V_p contains exactly those variables whose paired counterparts are non-local.

### 3.3 Message Queue (M_p)

**Dart Implementation**:

```dart
class MessageQueue {
  final Map<String, Queue<OutboundMessage>> _queuesByDestination = {};
  
  void add(OutboundMessage msg) { ... }
  OutboundMessage? poll(String destination) { ... }
  bool isEmpty() { ... }
  List<String> destinations() { ... }
}

class OutboundMessage {
  final String destination;
  final MessageType type;
  final List<int> payload;  // Serialized content
  
  OutboundMessage(this.destination, this.type, this.payload);
}

enum MessageType {
  assignment,    // (X?:=T, destination)
  readRequest,   // (request(X?, requester), destination)
  abandon,       // (abandon(Y), destination) where Y is writer
}
```

**Queue Properties**:
- FIFO per destination
- At-most-once delivery
- Eventual delivery (assuming connectivity)

### 3.4 Global Variable Identity

**Format**: `creator:localId`

**Components**:
- creator: agent ID who allocated the variable
- localId: unique integer within creator's heap

**Example**: `alice:1042` = variable with local ID 1042 created by agent alice

**Usage**: When serializing terms for inter-agent transport, local variable IDs are replaced with global IDs to enable V_p routing.

---

## 4. Transactions

### 4.1 Reduce Transaction (Local)

**Specification Reference**: Section 5.1 of irmaGLP-spec.md

When agent p reduces goal A with clause C yielding (B, σ̂):

1. **Reactivate suspended goals**:
   ```dart
   Set<Goal> reactivated = {};
   for (var binding in σ̂_reader.entries) {
     reactivated.addAll(reactivate(binding.key, S_p));
   }
   ```

2. **Update active queue**:
   ```dart
   A_p = applySubstitutions(A_r + B + reactivated, σ̂, σ̂_reader);
   ```

3. **Update V_p for assigned readers** (CORRECTED):
   ```dart
   Set<int> W = σ̂_reader.keys.toSet(); // Readers assigned by reduction
   
   for (var readerId in W) {
     var entry = V_p.lookup(readerId);
     if (entry != null && entry.role == VariableRole.importedReader && entry.state == null) {
       // Mark that reader was assigned; paired writer also assigned
       V_p.update(readerId, state: entry.creator);
     }
   }
   ```

4. **Queue messages for remote readers**:
   ```dart
   for (var binding in σ̂_reader.entries) {
     var readerId = binding.key;
     var value = binding.value;
     var entry = V_p.lookup(readerId);
     
     if (entry != null && entry.role == VariableRole.createdReader && entry.state != null) {
       // Reader was created by p, agent r requested it
       String requester = entry.state;
       M_p.add(OutboundMessage(
         requester,
         MessageType.assignment,
         serializeAssignment(readerId, value),
       ));
     }
   }
   ```

5. **Handle abandoned readers** (CORRECTED):
   ```dart
   for (var readerId in getReadersIn(A)) {
     if (!σ̂_reader.containsKey(readerId) && !appearsIn(readerId, B)) {
       abandon(readerId, V_p, M_p); // Pass READER, not writer
     }
   }
   ```

### 4.2 Communicate Transaction (Cross-Agent)

**Specification Reference**: Section 5.2 of irmaGLP-spec.md

When message (m, q) ∈ M_p is delivered to agent q:

**Assignment Message (X?:=T)**:

```dart
void handleAssignment(int readerId, Term value, AgentContext ctx) {
  // 1. Reactivate
  var reactivated = reactivate(readerId, ctx.S_p);
  
  // 2. Apply assignment
  if (value != null) {
    ctx.A_p.addAll(applySubstitution(reactivated, readerId, value));
    applyToSuspended(ctx.S_p, readerId, value);
    applyToFailed(ctx.F_p, readerId, value);
  } else {
    // Abandonment notification
    ctx.A_p.addAll(reactivated);
  }
  
  // 3. Remove from V_p
  ctx.V_p.remove(readerId);
  
  // 4. Import variables from T
  for (var varId in getVariablesIn(value)) {
    if (ctx.V_p.lookup(varId) == null) { // Fully local check
      String creator = getCreator(varId);
      ctx.V_p.add(varId, VariableEntry(varId, creator, determineRole(varId)));
    }
  }
}
```

**Read Request Message (request(X?, p))**:

```dart
void handleReadRequest(int readerId, String requester, AgentContext ctx) {
  if (requester == null) {
    // Abandonment request
    abandon(readerId, ctx.V_p, ctx.M_p);
    return;
  }
  
  var entry = ctx.V_p.lookup(readerId);
  
  if (entry != null && entry.state == null) {
    // Unbound - record requester
    ctx.V_p.update(readerId, state: requester);
  } else {
    // Already bound - reply immediately
    int writerId = getPairedWriter(readerId);
    var writerEntry = ctx.V_p.lookup(writerId);
    if (writerEntry != null && writerEntry.state != null) {
      ctx.M_p.add(OutboundMessage(
        requester,
        MessageType.assignment,
        serializeAssignment(readerId, writerEntry.state),
      ));
    }
  }
}
```

### 4.3 Network Transaction (Cold-Call)

**Specification Reference**: Section 5.3 of irmaGLP-spec.md

When msg(q, X) appears in p's network output:

```dart
void handleNetworkOutput(String destination, Term term, AgentContext ctx) {
  // 1. Export term
  Term exported = export(term, ctx.agentId, ctx.V_p, ctx.A_p);
  
  // 2. Deliver to destination (via coordinator)
  coordinator.deliver(destination, exported);
  
  // 3. Destination imports variables (happens at receiver)
}

void handleNetworkInput(Term term, AgentContext ctx) {
  // Import variables
  for (var varId in getVariablesIn(term)) {
    if (ctx.V_p.lookup(varId) == null) { // Fully local check
      String creator = getCreator(varId);
      ctx.V_p.add(varId, VariableEntry(varId, creator, determineRole(varId)));
    }
  }
  
  // Add to network input stream for GLPSAM to process
  ctx.chNetIn.add(term);
}
```

---

## 5. Helper Routines

### 5.1 abandon(readerId) - CORRECTED

**Specification Reference**: Section 4.1 of irmaGLP-spec.md

**CRITICAL**: An agent can only abandon a **reader**, which causes its dual writer to be abandoned at the remote agent.

```dart
void abandon(int readerId, VariableTable vp, MessageQueue mp) {
  var entry = vp.lookup(readerId);
  if (entry == null) return;
  
  // Compute paired writer ID
  int writerId = getPairedWriter(readerId);
  
  if (entry.role == VariableRole.importedReader) {
    // Notify creator
    mp.add(OutboundMessage(
      entry.creator,
      MessageType.abandon,
      serializeAbandon(writerId), // Send WRITER in message
    ));
  } else if (entry.role == VariableRole.createdReader && entry.state != null) {
    // Notify requester
    mp.add(OutboundMessage(
      entry.state, // requester
      MessageType.abandon,
      serializeAbandon(writerId), // Send WRITER in message
    ));
  }
  
  vp.remove(readerId);
}
```

### 5.2 request(readerId)

**Specification Reference**: Section 4.2 of irmaGLP-spec.md

```dart
void request(int readerId, VariableTable vp, MessageQueue mp, String agentId) {
  var entry = vp.lookup(readerId);
  if (entry == null) return;
  
  if (entry.role == VariableRole.importedReader && entry.state == null) {
    // Reader imported but not yet requested
    vp.update(readerId, state: entry.creator);
    mp.add(OutboundMessage(
      entry.creator,
      MessageType.readRequest,
      serializeReadRequest(readerId, agentId),
    ));
  }
}
```

### 5.3 export(term, agentId, vp, activeQueue) - CORRECTED

**Specification Reference**: Section 4.4 of irmaGLP-spec.md

```dart
Term export(Term term, String agentId, VariableTable vp, Queue<Goal> activeQueue) {
  Term result = term.clone();
  
  for (var varId in getVariablesIn(term)) {
    String creator = getCreator(varId);
    
    if (creator == agentId && vp.lookup(varId) == null) {
      // Local variable being exported for first time
      vp.add(varId, VariableEntry(varId, agentId, determineRole(varId)));
    } else if (creator != agentId) {
      // Non-local variable
      var entry = vp.lookup(varId);
      
      if (entry == null || entry.state == null) {
        // Writer or non-requested reader - just remove
        vp.remove(varId);
      } else if (entry.role == VariableRole.importedReader && entry.state == entry.creator) {
        // Requested reader - needs relay
        var (relayWriter, relayReader) = heap.allocateFreshPair();
        result = result.replaceVariable(varId, relayReader);
        
        // Add forwarding goal: export_reader(Y?, Z) :- known(Z?) | Y = Z?.
        activeQueue.add(Goal('export_reader', [varId, relayWriter]));
        vp.add(relayReader, VariableEntry(relayReader, agentId, VariableRole.createdReader));
      }
    }
  }
  
  return result;
}
```

**Forwarding Goal Implementation**:

The GLP clause `export_reader(Y?, Z) :- known(Z?) | Y = Z?.` must be added to the runtime or standard library:

```prolog
%% Relay forwarding: when relay reader Z? is bound, unify original writer Y with value
export_reader(Y?, Z) :- known(Z?) | Y = Z?.
```

This suspends on Z? until bound, then unifies the original writer Y with the relay reader's value.

### 5.4 reactivate(readerId, suspendedSet)

**Specification Reference**: Section 4.3 of irmaGLP-spec.md

```dart
Set<Goal> reactivate(int readerId, Map<Goal, Set<int>> suspendedSet) {
  Set<Goal> result = {};
  
  for (var entry in suspendedSet.entries.toList()) {
    Goal goal = entry.key;
    Set<int> blockers = entry.value;
    
    if (blockers.contains(readerId)) {
      result.add(goal);
      suspendedSet.remove(goal);
    }
  }
  
  return result;
}
```

---

## 6. Payload Serialization

### 6.1 Global Variable ID Encoding

**Format**: `creator:localId`

```dart
String encodeGlobalVarId(String creator, int localId) {
  return '$creator:$localId';
}

(String creator, int localId) decodeGlobalVarId(String globalId) {
  var parts = globalId.split(':');
  return (parts[0], int.parse(parts[1]));
}
```

### 6.2 Term Serialization

```dart
List<int> serializeTerm(Term term, String agentId) {
  // Recursive serialization:
  // - Constants: type tag + value bytes
  // - Variables: type tag + global ID (creator:localId)
  // - Structs: type tag + functor + arity + serialized args
  // - Lists: convert to .(H, T) structure form, then serialize
}

Term deserializeTerm(List<int> bytes, VariableTable vp, Heap heap) {
  // Recursive deserialization:
  // - Variables: parse global ID, lookup/create in heap, update V_p if needed
  // - Constants: reconstruct value
  // - Structs: deserialize args recursively
}
```

### 6.3 Message Serialization

```dart
List<int> serializeMessage(OutboundMessage msg) {
  // Message format:
  // [type tag][destination length][destination bytes][payload]
  BytesBuilder builder = BytesBuilder();
  builder.addByte(msg.type.index);
  builder.add(utf8.encode(msg.destination));
  builder.add(msg.payload);
  return builder.toBytes();
}

OutboundMessage deserializeMessage(List<int> bytes) {
  // Parse message header and payload
  int typeIndex = bytes[0];
  // ... extract destination and payload
  return OutboundMessage(destination, MessageType.values[typeIndex], payload);
}
```

---

## 7. Integration with Existing Code

### 7.1 Files to Create

```
glp_runtime/lib/multiagent/
├── variable_table.dart          # V_p implementation
├── message_queue.dart           # M_p implementation  
├── payload_serializer.dart      # Term/message serialization with global IDs
├── irma_context.dart            # AgentContext with V_p, M_p, R_p
├── helpers.dart                 # abandon, request, export, reactivate
└── export_reader.glp            # Forwarding clause: export_reader(Y?, Z) :- known(Z?) | Y = Z?.
```

### 7.2 Files to Modify

**glp_runtime/lib/runtime/runtime.dart:**
- Add hooks for binding notifications
- Add hooks for reader abandonment detection
- Integrate V_p/M_p updates

**glp_runtime/lib/runtime/scheduler.dart:**
- After each reduction: check for V_p/M_p updates
- After quiescence: flush M_p (send all queued messages)
- Process incoming messages from other agents

**glp_multiagent/lib/main.dart:**
- Replace SimpleRouter with irmaGLP message routing (serialized payloads)
- Remove `msg(From, To, Content)` routing by `To` field
- Use V_p-based routing instead
- Add V_p/M_p to each AgentContext

### 7.3 Standard Library Addition

Add to runtime or standard library:

```prolog
%% export_reader.glp
%% Relay forwarding for requested readers being re-exported

export_reader(Y?, Z) :- known(Z?) | Y = Z?.
```

This clause is essential for the relay mechanism in export().

---

## 8. Implementation Phases

### Phase 1: V_p Implementation

**Goal**: Implement VariableTable with corrected semantics

**Tasks**:
1. Create `variable_table.dart` with:
   - VariableEntry class (varId, creator, role, state)
   - VariableTable class (add, remove, lookup, getByCreator)
   - Enforce INVARIANT: creator = p for writer entries
   
2. Unit tests:
   - Add/remove/lookup operations
   - Writer entries always have creator = current agent
   - Reader entries track requesters correctly
   - Invariant: V_p contains exactly non-local variables

**Success Criteria**:
- All unit tests pass
- V_p correctly tracks non-local variables
- Creator field enforced for writers

### Phase 2: M_p Implementation

**Goal**: Implement MessageQueue with FIFO per destination

**Tasks**:
1. Create `message_queue.dart` with:
   - OutboundMessage class (destination, type, payload)
   - MessageQueue class with per-destination queues
   - FIFO ordering per destination
   
2. Unit tests:
   - Add messages for multiple destinations
   - Poll maintains FIFO order per destination
   - At-most-once delivery

**Success Criteria**:
- All unit tests pass
- Messages delivered in order per destination
- No message duplication

### Phase 3: Serialization

**Goal**: Implement payload serialization with global variable IDs

**Tasks**:
1. Create `payload_serializer.dart` with:
   - Global ID encoding: `creator:localId`
   - Term serialization (constants, variables, structs, lists)
   - Message serialization (type, destination, payload)
   
2. Round-trip tests:
   - serializeTerm → deserializeTerm produces equivalent term
   - Global IDs correctly encode creator and localId
   - All term types supported

**Success Criteria**:
- All round-trip tests pass
- Global IDs correctly formatted
- No information loss in serialization

### Phase 4: Helper Routines

**Goal**: Implement abandon, request, export, reactivate with corrections

**Tasks**:
1. Create `helpers.dart` with corrected implementations:
   - `abandon(readerId, ...)` - takes READER, sends WRITER in message
   - `request(readerId, ...)` - idempotent read requests
   - `export(term, ...)` - relay mechanism with export_reader goal
   - `reactivate(readerId, ...)` - find and reactivate suspended goals
   
2. Unit tests for each helper:
   - abandon() only accepts readers
   - request() is idempotent
   - export() creates relay for requested readers
   - reactivate() finds all blocked goals

3. Add export_reader.glp clause to runtime

**Success Criteria**:
- All helper tests pass
- abandon() correctly handles readers
- export() relay mechanism works
- export_reader clause integrated

### Phase 5: Runtime Integration

**Goal**: Hook V_p/M_p into GLPSAM runtime

**Tasks**:
1. Modify `runtime.dart`:
   - Add V_p/M_p to runtime context
   - Hook binding notifications → check V_p, queue to M_p
   - Hook reader abandonment detection
   - Compute W = domain(σ̂?) in Reduce
   
2. Modify `scheduler.dart`:
   - After each reduction: process V_p/M_p updates
   - After quiescence: flush M_p to coordinator
   - Handle incoming messages from coordinator

3. Integration tests:
   - Single agent with V_p/M_p (no cross-agent yet)
   - Verify bindings trigger M_p queuing
   - Verify abandonment detected and queued

**Success Criteria**:
- Runtime correctly updates V_p/M_p
- W correctly computed as domain(σ̂?)
- Messages queued for remote readers

### Phase 6: Multiagent Integration

**Goal**: Connect agents via coordinator with serialized payloads

**Tasks**:
1. Modify `main.dart`:
   - Replace SimpleRouter with SerializedRouter
   - Route based on V_p, not message content
   - Use payload serialization for MethodChannel
   - Add V_p/M_p to each AgentContext
   
2. Update coordinator:
   - Accept serialized payloads (opaque bytes)
   - Route to destination agent by ID
   - No interpretation of payload content

3. Integration tests:
   - Two agents: variable synchronization works
   - Messages routed via V_p
   - Serialization opaque to Dart layer

**Success Criteria**:
- Alice→Bob variable sharing works
- Coordinator only sees bytes
- V_p correctly routes messages

### Phase 7: End-to-End Testing

**Goal**: Verify complete irmaGLP protocol

**Tasks**:
1. Test Scenario 1: Basic variable sharing
   - Alice creates ch(X, Y?)
   - Alice sends ch(X, Y?) to Bob
   - Alice writes X = hello
   - Verify: Bob's Y? receives hello
   
2. Test Scenario 2: Friend-mediated introduction
   - Initial: Alice↔Bob, Bob↔Charlie
   - Bob receives introduce(alice, charlie)
   - Bob creates shared channels for Alice and Charlie
   - Bob sends introduction messages
   - Alice and Charlie accept, add each other to friends
   - Alice writes to shared channel
   - Verify: Charlie receives message
   
3. Test Scenario 3: Abandonment propagation
   - Alice sends reader X? to Bob
   - Bob doesn't use X?, gets garbage collected
   - Verify: Bob's abandon notifies Alice
   - Verify: Alice's writer X marked abandoned

**Success Criteria**:
- All three scenarios pass
- Introduction protocol completes
- Abandonment propagates correctly

---

## 9. Test Specifications

### 9.1 Unit Test: VariableTable

```dart
test('Writer entry must have creator = current agent', () {
  var vp = VariableTable('alice');
  
  // This should succeed
  vp.add(42, VariableEntry(42, 'alice', VariableRole.writer));
  
  // This should throw AssertionError
  expect(
    () => vp.add(43, VariableEntry(43, 'bob', VariableRole.writer)),
    throwsA(isA<AssertionError>()),
  );
});

test('V_p contains exactly non-local variables', () {
  var vp = VariableTable('alice');
  var heap = Heap();
  
  // Create local pair (both parts local)
  var (writer, reader) = heap.allocateFreshPair();
  expect(vp.lookup(writer), isNull);
  expect(vp.lookup(reader), isNull);
  
  // Export writer (reader becomes remote)
  vp.add(writer, VariableEntry(writer, 'alice', VariableRole.writer));
  expect(vp.lookup(writer), isNotNull);
  
  // Import reader back (both parts local again)
  vp.remove(writer);
  expect(vp.lookup(writer), isNull);
});
```

### 9.2 Unit Test: abandon() with Readers

```dart
test('abandon() only accepts readers', () {
  var vp = VariableTable('alice');
  var mp = MessageQueue();
  
  // Set up imported reader
  vp.add(100, VariableEntry(100, 'bob', VariableRole.importedReader));
  
  // Abandon the reader (NOT the writer)
  abandon(100, vp, mp);
  
  // Should queue message with WRITER ID to bob
  var msg = mp.poll('bob');
  expect(msg, isNotNull);
  expect(msg!.type, MessageType.abandon);
  
  var writerId = deserializeAbandonMessage(msg.payload);
  expect(writerId, 99); // Paired writer of reader 100
});
```

### 9.3 Integration Test: Variable Synchronization

```dart
test('Alice→Bob variable synchronization', () async {
  // Setup
  var alice = createAgent('alice');
  var bob = createAgent('bob');
  var coordinator = Coordinator([alice, bob]);
  
  // Alice creates channel and sends to Bob
  alice.execute('create_channel(Ch)');
  alice.execute('send(bob, Ch)');
  
  // Bob receives channel
  await coordinator.deliverPendingMessages();
  var bobCh = bob.getVariable('Ch');
  
  // Alice writes to channel
  alice.execute('write_channel(Ch, hello)');
  
  // Bob should receive value
  await coordinator.deliverPendingMessages();
  var value = bob.readChannel(bobCh);
  
  expect(value, 'hello');
});
```

### 9.4 End-to-End Test: Friend Introduction

```dart
test('Bob introduces Alice to Charlie', () async {
  // Setup: Alice↔Bob, Bob↔Charlie
  var alice = createAgent('alice', friends: ['bob']);
  var bob = createAgent('bob', friends: ['alice', 'charlie']);
  var charlie = createAgent('charlie', friends: ['bob']);
  var coordinator = Coordinator([alice, bob, charlie]);
  
  // User tells Bob to introduce Alice and Charlie
  bob.userInput('introduce(alice, charlie)');
  await bob.runUntilQuiescent();
  
  // Bob creates channels and sends introduction messages
  await coordinator.deliverPendingMessages();
  
  // Alice receives intro(charlie, Ch)
  await alice.runUntilQuiescent();
  expect(alice.friends, contains('charlie'));
  
  // Charlie receives intro(alice, Ch)
  await charlie.runUntilQuiescent();
  expect(charlie.friends, contains('alice'));
  
  // Alice sends message to Charlie via shared channel
  alice.userInput('send(charlie, hello)');
  await alice.runUntilQuiescent();
  await coordinator.deliverPendingMessages();
  await charlie.runUntilQuiescent();
  
  // Charlie should receive hello
  expect(charlie.lastMessage, 'hello');
});
```

---

## 10. Success Criteria

1. ✅ **Variable synchronization works**: Bindings propagate between agents via V_p/M_p
2. ✅ **Introduction protocol completes**: Alice and Charlie become friends via Bob
3. ✅ **No Dart-level content routing**: Routing based purely on V_p, Dart sees only bytes
4. ✅ **Abandonment propagates**: Unreachable readers trigger abandon messages
5. ✅ **Serialization is opaque**: Coordinator cannot interpret message payloads
6. ✅ **Correctness properties hold**: All V_p/M_p invariants maintained

---

## 11. Future Work

After irmaGLP is working:
- Cold-call befriending (via network channel with export())
- TEE attestation guards for secure execution
- Encrypted payloads (opaque to coordinator)
- Persistent V_p/M_p for crash recovery
- Performance optimizations (batch messaging, lazy reactivation)

---

## Appendix A: Variable Table Invariants (Corrected)

1. **Completeness**: Every variable with a non-local counterpart is in V_p
2. **Exclusivity**: No variable appears in V_p if both parts are local (fully local)
3. **Consistency**: Creator field matches the agent that allocated the variable
4. **Writer constraint**: For writer entries, creator = current agent (writers never imported)
5. **State accuracy**: 
   - Writer state reflects current binding (Term or null)
   - Reader state reflects requester (agent ID or null)

---

## Appendix B: Message Queue Invariants

1. **FIFO per destination**: Messages to same agent delivered in order
2. **At-most-once**: Each message delivered at most once (no duplication)
3. **Eventual delivery**: All queued messages eventually delivered (assuming connectivity)
4. **Content opacity**: Dart transport sees only bytes, no interpretation

---

## Appendix C: Coordinator Role (Unchanged)

The coordinator:
1. Routes serialized payloads between agents (dumb pipe)
2. Spawns/kills agent windows via DesktopMultiWindow
3. Does NOT interpret message content (only sees bytes)
4. Does NOT maintain its own V_p for agent variables

The coordinator may have its own GLPSAM for simulation control, but that's separate from agent communication.

---

## Appendix D: Key Corrections Summary

| Item | V1.0 (Incorrect) | V2.0 (Corrected) |
|------|------------------|------------------|
| abandon() parameter | Variable Y | Reader Y? |
| Writer entry creator | Implicit constraint | Explicit INVARIANT: q = p |
| W in Reduce | Unclear | W = domain(σ̂?) |
| export_reader | Not defined | Clause: `known(Z?) \| Y = Z?.` |
| "Not local" check | Ambiguous | `(Y, ·, ·) ∉ V_p` |
| Global ID format | Informal | Formalized: creator:localId |
| Abandonment source | Any variable | Only readers |

---

## Implementation Progress

### Completed Phases

| Phase | Status | Tests | Files Created |
|-------|--------|-------|---------------|
| 1. V_p Implementation | ✅ Complete | 20 | variable_table.dart |
| 2. M_p Implementation | ✅ Complete | 22 | message_queue.dart |
| 3. Serialization | ✅ Complete | 36 | payload_serializer.dart |
| 4. Helper Routines | ✅ Complete | 26 | helpers.dart, relay.glp |
| 5. Runtime Integration | ✅ Complete | 24 | irma_context.dart |
| 6. Multiagent Integration | ⏳ Pending | - | - |
| 7. End-to-End Testing | ⏳ Pending | - | - |

**Total Tests**: 128 passing

### Phase 5 Completed: Runtime Integration

**Integration Approach**: Heap callback (not runner callback)
- IrmaContext registers `onBind` callbacks when variables are added to V_p
- When a variable is bound, the callback automatically queues messages to M_p
- This decouples GLP runtime from network transport (clean separation)
- No modifications to runner.dart required

**Implemented**:
- ✅ `registerWriter(varId)` - adds to V_p and sets up binding callback
- ✅ `registerCreatedReader(varId)` - adds to V_p and sets up binding callback  
- ✅ Binding callbacks queue assignment messages when requester exists
- ✅ `handleAssignment/handleReadRequest/handleAbandon` - incoming message handlers
- ✅ `importTerm/exportTerm` - term import/export with V_p tracking
- ✅ `flushMessages()` - delivers via callback
- ✅ 24 IrmaContext unit tests (including 5 heap callback integration tests)

**Key Design Decision**: Heap callbacks vs runner callbacks
- For smartphone deployment over Internet, heap callbacks are correct
- GLP runtime stays clean - just binds variables
- irmaGLP observes bindings and handles messaging
- Matches spec: "when writer X is bound to T, if requester exists, queue message"

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01 | Claude | Initial implementation plan |
| 2.0 | 2026-01-17 | Claude | Aligned with irmaGLP-spec.md v1.1, all corrections applied |
| 2.1 | 2026-01-17 | Claude | Phase 5 progress: IrmaContext created (19 tests), total 123 tests |
| 2.2 | 2026-01-17 | Claude | Phase 5 complete: Heap callback integration (24 tests), total 128 tests |
