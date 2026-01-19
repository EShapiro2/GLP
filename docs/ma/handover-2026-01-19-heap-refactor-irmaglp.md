# Heap Address Refactor: Implications for irmaGLP

**Date:** 2026-01-19  
**Branch:** `heap-address-refactor` (ready to merge to main)  
**Status:** Refactor complete, 256/259 tests passing

## 1. Summary

The heap address refactor simplifies variable identity by eliminating the separate `varTable` lookup layer. Variables are now identified directly by their writer cell's heap address. This architectural change provides clearer semantics for irmaGLP's distributed variable tracking, particularly for distinguishing imported readers from imported writers during term deserialization.

## 2. What Changed

### Before: Indirect Variable Identity

The previous design maintained a separate namespace for variable IDs:

```dart
// Old design
int nextVarId = 0;
Map<int, (int, int)> varTable = {};  // varId -> (writerAddr, readerAddr)

int allocateVariable() {
  final varId = nextVarId++;
  final wAddr = HP++;
  final rAddr = HP++;
  varTable[varId] = (wAddr, rAddr);
  return varId;
}
```

This created ambiguity when processing VarRef terms because the varId alone didn't convey whether it referred to a writer or reader cell. The `isReader` flag existed but was separate from the addressing scheme.

### After: Direct Address Identity

The new design uses heap addresses directly as variable identifiers:

```dart
// New design
int allocateVariable() {
  final wAddr = HP++;  // Writer at even address
  final rAddr = HP++;  // Reader at wAddr + 1
  return wAddr;        // varId IS the writer address
}
```

Key properties of the new design:

| Property | Value |
|----------|-------|
| varId | Equals writerAddr |
| readerAddr | Always writerAddr + 1 |
| VarRef(id, isReader: false) | References cell at `id` |
| VarRef(id, isReader: true) | References cell at `id + 1` |

## 3. Benefits for irmaGLP

### 3.1 Clearer Import Semantics

When `importTerm` processes a deserialized term, the VarRef's `isReader` flag now has unambiguous meaning:

```dart
void _importTermRecursive(Term term, String fromAgent, ...) {
  if (term is VarRef) {
    if (term.isReader) {
      // Imported READER: we received X? from another agent
      // The paired writer X is at the remote agent
      // We may need to send read requests to get values
      registerImportedReader(term.varId, fromAgent);
    } else {
      // Imported WRITER: we received X from another agent
      // This is the output channel - when we bind it, notify creator
      registerImportedWriter(term.varId, fromAgent);
    }
  }
}
```

### 3.2 Simplified Heap Callbacks

The `onBind` callback registration is now straightforward because varId equals writerAddr:

```dart
void registerImportedWriter(int varId, String creator) {
  // varId IS the writer address - no lookup needed
  runtime.heap.onBind(varId, (Term value) {
    // When this writer is bound, notify the creator
    _onWriterBound(varId, value);
  });
}
```

### 3.3 Address Arithmetic for Channel Handling

When Bob creates a channel `ch(QtoP?, PtoQ)` and sends it to Alice:

| Term | varId | isReader | Heap Address | Role at Alice |
|------|-------|----------|--------------|---------------|
| QtoP? | N | true | N+1 | Imported reader (Alice reads Bob's output) |
| PtoQ | M | false | M | Imported writer (Alice writes to Bob's input) |

Alice can now unambiguously identify that `PtoQ` is an imported writer requiring a heap callback for outbound message routing.

## 4. The Friend-Mediated Introduction Problem

### 4.1 Current State

The introduction protocol partially works:

- Bob can execute `introduce(alice, charlie)` successfully
- Alice and Charlie can accept introductions via `accept_intro`
- Messages do not flow between Alice and Charlie after introduction

### 4.2 Root Cause Analysis

When Bob creates the introduction channel `ch(QtoP?, PtoQ)` and sends it to Alice:

1. Bob serializes the channel term with VarRef structures
2. Alice deserializes, creating fresh local variables via `allocateFreshVar()`
3. `importTerm` should register the writer part as `importedWriter` with a heap callback
4. When Alice's GLP writes to the stream, the callback should fire and route through V_p

The suspected failure point is that the heap callback either isn't being registered or isn't firing when the GLP writes to the imported writer stream.

### 4.3 Why the New Architecture Helps

With the old architecture, there was potential confusion about:

- Whether the callback was registered on the correct address
- Whether varId lookups through varTable were consistent
- Whether the isReader flag was being interpreted correctly during serialization/deserialization

The new architecture eliminates these ambiguities. When Alice receives `PtoQ` (a writer), the varId directly identifies the heap cell where the callback should be registered. There is no intermediate lookup that could introduce inconsistency.

## 5. Proposed Next Steps

### 5.1 Merge the Heap Refactor

The `heap-address-refactor` branch should be merged to main. All tests pass at the same rate as the baseline, confirming no regressions.

### 5.2 Add Diagnostic Logging

Before attempting fixes, add targeted logging to trace the introduction flow:

```dart
// In PayloadSerializer.deserializeAgentMessagePayload
print('[DESERIALIZE] VarRef varId=$localVarId isReader=${isReader}');

// In IrmaContext.importTerm
print('[IMPORT] Registering ${term.isReader ? "reader" : "writer"} varId=${term.varId}');

// In IrmaContext.registerImportedWriter
print('[IMPORT WRITER] varId=$varId, setting up heap callback');

// In heap onBind callback
print('[HEAP CALLBACK] Writer $varId bound to $value');
```

### 5.3 Verify the Callback Chain

Run the three-agent simulation and execute the introduction protocol. The logs should reveal:

1. Whether `importTerm` correctly identifies the channel writer as an imported writer
2. Whether `registerImportedWriter` successfully registers the heap callback
3. Whether the callback fires when the GLP writes `[msg | Tail]` to the stream
4. Whether the V_p lookup finds the correct routing information

### 5.4 Likely Fix Areas

Based on prior analysis, the most probable issues are:

**Stream Extension Pattern**: When GLP writes `[msg | Tail]` to a stream, it binds the current writer to a cons cell containing a fresh tail writer. The callback may fire for the initial binding but not propagate to observe subsequent writes. The fix would ensure the callback forwards to the new tail writer.

**V_p State Management**: The imported writer's V_p entry may not have the correct `state` field set to indicate who should receive notifications. The fix would ensure proper state initialization during `importTerm`.

**Message Routing Through Creator**: When Alice writes to an imported writer, the message should route through Bob (the creator) who then forwards to Charlie. The routing logic in `_onWriterBound` and `handleAssignment` should be verified.

### 5.5 Testing Strategy

Once the fix is identified, add a focused integration test:

```dart
test('introduction protocol: messages flow after accept', () async {
  // Setup three agents
  // Bob introduces Alice to Charlie
  // Alice and Charlie accept
  // Alice sends message to Charlie via introduced channel
  // Verify Charlie receives the message
});
```

## 6. Files Modified in Heap Refactor

| File | Changes |
|------|---------|
| `lib/runtime/heap_fcp.dart` | Removed varTable, varId == writerAddr |
| `lib/runtime/terms.dart` | VarRef stores explicit isReader flag |
| `lib/runtime/commit.dart` | Address arithmetic instead of varTable lookup |
| `lib/runtime/suspend_ops.dart` | Address arithmetic instead of varTable lookup |
| `lib/runtime/module_runtime.dart` | Use allocateVariable() properly |
| `lib/multiagent/irma_context.dart` | Use isWriterBound() instead of varTable |
| `lib/bytecode/runner.dart` | Address arithmetic instead of varTable lookup |

## 7. Summary

The heap address refactor establishes a cleaner foundation for irmaGLP's distributed variable semantics. By eliminating the varTable indirection, variable identity is now unambiguous: varId equals writerAddr, and the isReader flag determines whether we reference the writer cell or its paired reader cell.

The next step is to merge this refactor and add diagnostic logging to trace the exact failure point in the introduction protocol's message flow. The architectural clarity provided by the refactor should make the root cause easier to identify and fix.
