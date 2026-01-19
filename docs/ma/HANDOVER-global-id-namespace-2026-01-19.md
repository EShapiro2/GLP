# Handover Report: Global ID Namespace Fix for irmaGLP Relay Mechanism

**Date:** 2026-01-19  
**Session Focus:** Fixing variable ID namespace issues in friend-mediated introduction protocol  
**Status:** In Progress - Fixes Partially Implemented

## Executive Summary

The three-way Flutter test (Alice↔Bob↔Charlie) revealed that the introduction protocol successfully creates and sends channel variables, but messages sent through introduced channels fail to reach their destinations. The root cause is a **variable ID namespace mismatch**: when Alice sends an assignment to Bob (the variable creator), she sends her local varId instead of Bob's original ID, preventing Bob from looking it up in his V_p.

## Problem Analysis

### Observed Behavior

From the trace output and screenshot:

1. Bob executes `introduce(alice, charlie)` successfully
2. Both Alice and Charlie receive intro offers and register imported variables
3. Alice accepts intro, binds the imported writer, sends assignment to Bob
4. Bob receives the 36-byte assignment but **nothing happens after that**
5. Charlie never receives the message Alice sent

### Root Cause

**Bug 1: Assignment payload missing varId**

In `irma_context.dart`, the `_queueAssignment` method was serializing only the VALUE without the variable ID:

```dart
// BUG - missing varId!
final payload = _serializer.serializeTerm(value, agentId);
```

The serializer has `createAssignmentPayload(varId, value)` but it wasn't being used.

**Bug 2: Wrong variable ID namespace**

Even with varId included, Alice would send "alice:1032" (her local ID) but Bob needs "bob:1117" (his original ID) to look it up in his V_p.

When Bob serializes `ch(X1117?, X1118)` and sends it to Alice, the wire format contains "bob:1117" and "bob:1118". But when Alice deserializes, she allocates fresh local IDs (e.g., 1031, 1032) and **discards the global→local mapping**. Her V_p stores `creator: 'bob'` but not the creator's original localId.

### Spec Reference

Per irmaGLP-spec.md, variable IDs in messages should always be the **creator's global IDs**. The GlobalVarId format is `creator:localId` where localId is the creator's original heap ID.

## Fixes Implemented

### 1. Extended VariableEntry with creatorLocalId

**File:** `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/variable_table.dart`

Added `creatorLocalId` field to store the creator's original variable ID:

```dart
class VariableEntry {
  final int varId;           // Our local heap ID
  final String creator;      // Agent who created this variable
  final int creatorLocalId;  // Creator's original local ID
  final VariableRole role;
  dynamic state;
  
  VariableEntry({
    required this.varId,
    required this.creator,
    required this.role,
    int? creatorLocalId,    // NEW: optional, defaults to varId
    this.state,
  }) : creatorLocalId = creatorLocalId ?? varId;
}
```

### 2. Added mapping-aware deserialization

**File:** `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/payload_serializer.dart`

Added static method that returns both the term AND the global→local mapping:

```dart
static (Term, Map<int, GlobalVarId>) deserializeAgentMessagePayloadWithMapping(
  List<int> payload,
  int Function() allocateFreshVar,
) {
  final globalToLocal = <String, int>{};
  // ... deserialization ...
  
  // Invert to get local -> global mapping
  final localToGlobal = <int, GlobalVarId>{};
  for (final entry in globalToLocal.entries) {
    localToGlobal[entry.value] = GlobalVarId.decode(entry.key);
  }
  
  return (term, localToGlobal);
}
```

### 3. Fixed _queueAssignment to use creator's ID

**File:** `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_context.dart`

Updated to use `createAssignmentPayload` with the creator's local ID:

```dart
void _queueAssignment(int varId, Term value, String destination) {
  final entry = vp.lookup(varId);
  
  // Use creator's local ID for the global variable ID
  final creatorLocalId = entry?.creatorLocalId ?? varId;
  final creator = entry?.creator ?? agentId;
  
  // Create assignment payload with proper global ID
  final globalIdSerializer = PayloadSerializer(creator);
  final payload = globalIdSerializer.createAssignmentPayload(creatorLocalId, value);
  
  mp.add(OutboundMessage(...));
}
```

### 4. Updated registerImportedWriter signature

**File:** `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_context.dart`

Added optional `creatorLocalId` parameter:

```dart
void registerImportedWriter(int varId, String creator, {int? creatorLocalId}) {
  vp.add(varId, VariableEntry(
    varId: varId,
    creator: creator,
    role: VariableRole.importedWriter,
    creatorLocalId: creatorLocalId ?? varId,
  ));
  // ... callback registration ...
}
```

### 5. Updated importTerm to accept and use mapping

**File:** `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_context.dart`

```dart
void importTerm(Term term, String fromAgent, {Map<int, GlobalVarId>? globalIdMapping}) {
  _importTermRecursive(term, fromAgent, globalIdMapping ?? {});
}

void _importTermRecursive(Term term, String fromAgent, Map<int, GlobalVarId> globalIdMapping) {
  if (term is VarRef) {
    if (!vp.contains(term.varId)) {
      final globalId = globalIdMapping[term.varId];
      final creator = globalId?.creator ?? fromAgent;
      final creatorLocalId = globalId?.localId;
      
      if (term.isReader) {
        vp.add(term.varId, VariableEntry(
          varId: term.varId,
          creator: creator,
          role: VariableRole.importedReader,
          creatorLocalId: creatorLocalId,
        ));
      } else {
        registerImportedWriter(term.varId, creator, creatorLocalId: creatorLocalId);
      }
    }
  }
  // ... struct recursion ...
}
```

### 6. Updated Flutter app to use mapping

**File:** `/Users/udi/Grassroots/GLP/glp_multiagent/lib/main.dart`

Changed `_onIrmaMessageReceived` to use the new static method:

```dart
if (msg.type == MessageType.agentMessage) {
  final (term, globalIdMapping) = PayloadSerializer.deserializeAgentMessagePayloadWithMapping(
    msg.payload,
    () {
      final (writerId, _) = _agent!.runtime.heap.allocateFreshPair();
      return writerId;
    },
  );
  
  // Pass mapping to importTerm
  _agent!.context.importTerm(term, from.toLowerCase(), globalIdMapping: globalIdMapping);
  // ...
}
```

## Files Modified

| File | Change |
|------|--------|
| `glp_runtime/lib/multiagent/variable_table.dart` | Added `creatorLocalId` field to VariableEntry |
| `glp_runtime/lib/multiagent/payload_serializer.dart` | Added `deserializeAgentMessagePayloadWithMapping` static method |
| `glp_runtime/lib/multiagent/irma_context.dart` | Fixed `_queueAssignment`, updated `registerImportedWriter`, updated `importTerm` |
| `glp_multiagent/lib/main.dart` | Updated `_onIrmaMessageReceived` to use mapping |

## Remaining Work

### Not Yet Verified

1. **Test the fixes** - Run the Flutter app again with `flutter run -d macos` and repeat the test scenario
2. **Check request() helper** - Verify that read requests also use `creatorLocalId` when sending to the creator
3. **Run unit tests** - Ensure the changes don't break existing multiagent unit tests

### Potential Issues

1. The `helpers.dart` `request()` function may also need updating to use `creatorLocalId`
2. The `abandon()` helper may need similar treatment
3. Need to verify that the relay forwarding callback in `_setupRelayForwarding` properly handles the namespace

## Test Scenario

To verify the fix:

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos
```

1. Click "Alice↔Bob↔Charlie" to spawn the linear topology
2. In Bob's window: `introduce(alice, charlie)`
3. In Alice's window: `accept_intro(charlie)`
4. In Charlie's window: `accept_intro(alice)`
5. In Alice's window: `send(charlie, hello)`

**Expected:** Charlie should see `received(alice, hello)` in output

## Data Flow (After Fix)

```
Alice                          Bob                          Charlie
  |                             |                             |
  |  (Bob creates ch pair)      |                             |
  |  ch(R1117?, W1118)          |                             |
  |<-- intro(charlie,ch(...)) --|                             |
  |     [bob:1117, bob:1118]    |-- intro(alice,ch(...)) ---->|
  |                             |     [bob:1118, bob:1117]    |
  |                             |                             |
  | accept_intro(charlie)       |                             |
  | Alice binds W1032           |       accept_intro(alice)   |
  | (locally mapped from 1118)  |       Charlie binds W1031   |
  |                             |       (locally mapped 1117) |
  |                             |                             |
  | send(charlie, hi)           |                             |
  | binds W1032 to list         |                             |
  | callback fires              |                             |
  | _queueAssignment uses       |                             |
  | creatorLocalId=1118         |                             |
  |-- assign(bob:1118, [...]) ->|                             |
  |                             | lookup 1118 in V_p          |
  |                             | finds createdWriter         |
  |                             | state=charlie (requester)   |
  |                             |-- assign(bob:1118,[...]) -->|
  |                             |                             | Charlie binds
  |                             |                             | reader 1031
```

## Session Transcript

Full conversation available at:
`/mnt/transcripts/2026-01-19-12-57-56-export-reader-relay-implementation.txt`

## Previous Related Sessions

- `HANDOVER-imported-writer-2026-01-18.md` - Imported writer callback mechanism
- `HANDOVER-play_introduction-2026-01-18.md` - Introduction protocol design
- `HANDOVER-2026-01-17-irmaGLP-phase1-4.md` - Initial irmaGLP implementation
