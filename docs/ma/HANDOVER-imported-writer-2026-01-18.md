# Handover: Imported Writer Implementation

**Date:** 2026-01-18  
**Status:** Implementation complete, unit tests passing  
**Next:** Integration testing with multi-agent introduction scenario

## Summary

Implemented the friend-mediated introduction protocol's key missing piece: when an agent binds an **imported writer**, it must notify the **creator** (not attempt to send directly to the unknown requester). The creator then routes the value to the actual requester.

This resolves Issue #1 from `irmaGLP-paper-issues-and-resolutions.md`.

## What Changed

### 1. VariableRole Enum Split (`variable_table.dart`)

```dart
enum VariableRole {
  createdWriter,   // Writer created by this agent (q = p)
  importedWriter,  // Writer received from another agent (q ≠ p)
  createdReader,   // Reader created by this agent
  importedReader,  // Reader received from another agent
}
```

**Why:** The original `writer` role couldn't distinguish between a writer the agent created vs one it received. This distinction is critical for routing.

### 2. New Method: `registerImportedWriter()` (`irma_context.dart`)

```dart
void registerImportedWriter(int varId, String creator) {
  vp.add(varId, VariableEntry(
    varId: varId,
    creator: creator,
    role: VariableRole.importedWriter,
  ));
  
  // Set up heap callback - when bound, notify creator
  runtime.heap.onBind(varId, (boundValue) {
    if (runtime.heap.isFullyBound(varId)) {
      _onWriterBound(varId, boundValue);
    }
  });
}
```

### 3. Enhanced `_onWriterBound()` (`irma_context.dart`)

Now handles two cases:
- **Created writer with requester:** Send assignment directly to requester, store value
- **Imported writer:** Send assignment to **creator** (creator routes to requester), store value

### 4. Three-Case `handleAssignment()` (`irma_context.dart`)

When creator receives assignment notification:
1. **Reader not in V_p:** Apply directly to heap (local use)
2. **Created reader with pending request:** Forward to requester, store value
3. **Created reader, no request yet:** Store value for later (value-before-request)

### 5. Value-First Logic in `handleReadRequest()` (`irma_context.dart`)

When creator receives read request:
- **Created reader with stored value:** Reply immediately
- **Created reader, no value:** Record requester for later
- **Created writer already bound:** Send value immediately
- **Created writer not bound:** Record requester for later

### 6. Writer Detection in `importTerm()` (`irma_context.dart`)

```dart
if (term is VarRef) {
  if (!vp.contains(term.varId)) {
    if (term.isReader) {
      // Import as reader (existing logic)
    } else {
      // Import as WRITER - new!
      registerImportedWriter(term.varId, sender);
    }
  }
}
```

## Protocol Flow: Friend Introduction

```
Alice                    Bob (creator)                Charlie
  |                          |                           |
  |  1. Bob creates channel ch(CA, CA?)                  |
  |     exports CA (writer) to Alice                     |
  |     exports CA? (reader) to Charlie                  |
  |                          |                           |
  |                          |<--- 2. request(CA?) ------|
  |                          |     Bob stores requester  |
  |                          |                           |
  |  3. Alice binds CA=T     |                           |
  |     (imported writer)    |                           |
  |--- assignment(CA,T) ---->|                           |
  |     (notify creator)     |                           |
  |                          |                           |
  |                          |--- 4. assignment(CA?,T) ->|
  |                          |     (forward to Charlie)  |
  |                          |                           |
```

**Key insight:** Alice never knows about Charlie. Bob acts as routing hub.

## Files Modified

| File | Changes |
|------|---------|
| `lib/multiagent/variable_table.dart` | Split `writer` → `createdWriter` + `importedWriter` |
| `lib/multiagent/irma_context.dart` | `registerImportedWriter()`, enhanced `_onWriterBound()`, three-case `handleAssignment()`, value-first `handleReadRequest()`, writer detection in `importTerm()` |
| `lib/multiagent/irma_agent.dart` | Added `registerImportedWriter()` delegation |
| `lib/multiagent/helpers.dart` | Updated `export()` to use `createdWriter` |
| `test/multiagent/variable_table_test.dart` | Added Writer Types + Introduction Scenario groups |
| `test/multiagent/irma_context_test.dart` | Added Introduction Protocol group (4 scenarios) |
| `test/multiagent/irma_agent_test.dart` | Added `registerImportedWriter` test |
| `test/multiagent/helpers_test.dart` | Updated enum references |

## Test Results

```
151 tests passing
2 pre-existing failures (dump_bytecode_test, trace_social_graph_test - unrelated null checks)
```

New test groups:
- `VariableTable - Writer Types`: Verifies enum split
- `VariableTable - Introduction Scenario`: Charlie receives writer+reader from Bob
- `IrmaContext - Introduction Protocol`: 4 scenarios covering the full flow

## Integration Testing Instructions

### Prerequisites

1. Ensure `social_graph.pl` is compiled to bytecode
2. Have three agent instances: alice, bob, charlie

### Test Scenario: Bob Introduces Alice to Charlie

#### Setup

```dart
// Create three agents
final alice = IrmaAgent(agentId: 'alice', runtime: GlpRuntime());
final bob = IrmaAgent(agentId: 'bob', runtime: GlpRuntime());
final charlie = IrmaAgent(agentId: 'charlie', runtime: GlpRuntime());

// Wire up message delivery
alice.context.onMessageReady = (dest, msg) => deliver(dest, msg);
bob.context.onMessageReady = (dest, msg) => deliver(dest, msg);
charlie.context.onMessageReady = (dest, msg) => deliver(dest, msg);

void deliver(String dest, OutboundMessage msg) {
  final target = switch (dest) {
    'alice' => alice,
    'bob' => bob,
    'charlie' => charlie,
    _ => throw 'Unknown agent: $dest',
  };
  target.receiveMessage(msg.serialize(), msg.sender);
}
```

#### Step 1: Bob Creates Channel

```dart
// Bob creates channel with writer CA and reader CA?
final caWriter = bob.runtime.heap.allocateVariable();
final caReader = bob.runtime.heap.allocateVariable();
bob.context.registerWriter(caWriter);
bob.context.registerCreatedReader(caReader);

// Link writer and reader (same variable in Bob's heap)
// In practice this happens via reduce/unification
```

#### Step 2: Bob Exports to Alice and Charlie

```dart
// Export writer to Alice
final channelForAlice = StructTerm('ch', [
  VarRef(caWriter, isReader: false),  // CA - writer
  // ... other channel components
]);
alice.context.importTerm(channelForAlice, 'bob');

// Export reader to Charlie  
final channelForCharlie = StructTerm('ch', [
  VarRef(caReader, isReader: true),  // CA? - reader
  // ... other channel components
]);
charlie.context.importTerm(channelForCharlie, 'bob');
```

#### Step 3: Charlie Requests Value

```dart
// Charlie's agent suspends on CA?, triggering read request
charlie.context.processSuspension({caReader});
charlie.context.flushMessages();

// Verify: Bob should have recorded charlie as requester
expect(bob.context.vp.lookup(caReader)?.state, 'charlie');
```

#### Step 4: Alice Binds Writer

```dart
// Alice binds the imported writer (e.g., via reduce)
alice.runtime.heap.bindVariable(caWriter, ConstTerm('hello_charlie'));
alice.context.flushMessages();

// Verify: Message should go to bob (creator), not charlie
```

#### Step 5: Verify Charlie Receives Value

```dart
// Bob receives assignment, forwards to charlie
// (happens automatically via handleAssignment)

// Check charlie's heap has the value
expect(charlie.runtime.heap.isFullyBound(caReader), isTrue);
expect(charlie.runtime.heap.getValue(caReader), ConstTerm('hello_charlie'));
```

### Edge Cases to Test

1. **Value before request:** Alice binds before Charlie requests
   - Bob stores value
   - When Charlie requests, Bob replies immediately

2. **Multiple requesters:** Not currently supported (last requester wins)
   - Future work: support broadcast to multiple readers

3. **Abandon after value stored:** Charlie abandons after Bob stored value
   - Value should be discarded, entry removed

4. **Creator crashes:** Not currently handled
   - Future work: timeout/retry mechanism

### Debugging Tips

1. **Check V_p state:**
   ```dart
   print(agent.context.vp.toString());
   ```

2. **Check message queue:**
   ```dart
   print('Messages for bob: ${agent.context.mp.countFor("bob")}');
   ```

3. **Enable heap tracing:**
   ```dart
   agent.runtime.heap.onBind(varId, (v) => print('Bound $varId to $v'));
   ```

## Known Limitations

1. **Single requester per reader:** Only one agent can request a created reader. Multiple requesters would need broadcast support.

2. **No timeout:** If creator never receives a value, requester waits forever.

3. **No error propagation:** If Alice's reduce fails, Charlie doesn't know.

4. **Pre-existing test failures:** `dump_bytecode_test` and `trace_social_graph_test` have null check errors unrelated to this work.

## Documentation Updated

- `irmaGLP-spec.md` → v2.1 (V_p definition, Reduce, Communicate transactions)
- `irmaGLP-paper-issues-and-resolutions.md` → Issue #1 marked RESOLVED
- `glp_appendix_smartphone.tex` → Matching paper updates

## Next Steps

1. **Integration test** with actual `social_graph.pl` running introduction scenario
2. **Fix pre-existing test failures** in dump_bytecode and trace tests
3. **Implement broadcast** for multiple readers of same variable
4. **Add timeout/retry** for robustness
