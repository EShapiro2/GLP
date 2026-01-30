# madGLP Implementation Plan

**Date**: 2026-01-30  
**Status**: PLANNING  
**Source**: madGLP-spec.md v4.0

---

## Overview

This document outlines the implementation changes needed to migrate from the request-based irmaGLP model to the push-based madGLP model. The fundamental shift is from pull-based communication (readers request values) to push-based communication (`global_send` goals automatically push values when writers are bound).

---

## Architecture Comparison

| Aspect | Old (irmaGLP) | New (madGLP) |
|--------|---------------|--------------|
| Communication model | Pull: readers request, creators respond | Push: `global_send` goals fire when readers become known |
| Variable table | V_p with 4 entry types | W_p (Global Writers Table) with 2 entry types |
| Table contents | Created/imported × writer/reader | Only writers awaiting incoming assignments |
| Message types | Assignment, ReadRequest, Abandon | Assignment only |
| Outgoing comm | Reduce generates messages directly | `global_send` goals in resolvent |
| Forwarding | Relay variables + forwarding logic | Automatic via `global_send` watching readers |
| Helpers | abandon(), request(), export() | Globalize(), Localize() |

---

## Phase 0: Test Plan (TDD - Tests Before Implementation)

Per DISCIPLINE.md Section 1.1 and 2.4: Tests are derived from specs before implementation. We write tests first, watch them fail (red), then implement to make them pass (green).

### 0.1 Unit Tests for GlobalWritersTable

**File:** `glp_runtime/test/multiagent/global_writers_table_test.dart`

**Tests derived from spec Section 3:**

```dart
group('GlobalWritersTable', () {
  // Entry creation
  test('addGlobalizeEntry allocates sequential indices', () {
    final table = GlobalWritersTable('p');
    final i1 = table.addGlobalizeEntry(100, 'q');
    final i2 = table.addGlobalizeEntry(200, 'r');
    expect(i1, 0);
    expect(i2, 1);
  });

  test('addLocalizeEntry stores remote index', () {
    final table = GlobalWritersTable('q');
    table.addLocalizeEntry(100, 'p', 5);  // Entry for _w(p,5)
    final entry = table.findByRemote('p', 5);
    expect(entry, isNotNull);
    expect(entry!.writerAddr, 100);
  });

  // Lookup
  test('lookupByIndex returns GlobalizeEntry at index', () {
    final table = GlobalWritersTable('p');
    final i = table.addGlobalizeEntry(100, 'q');
    final entry = table.lookupByIndex(i);
    expect(entry, isNotNull);
    expect(entry!.writerAddr, 100);
  });

  test('findByRemote searches LocalizeEntries', () {
    final table = GlobalWritersTable('q');
    table.addLocalizeEntry(100, 'p', 0);
    table.addLocalizeEntry(200, 'p', 1);
    table.addLocalizeEntry(300, 'r', 0);
    
    expect(table.findByRemote('p', 0)?.writerAddr, 100);
    expect(table.findByRemote('p', 1)?.writerAddr, 200);
    expect(table.findByRemote('r', 0)?.writerAddr, 300);
    expect(table.findByRemote('p', 2), isNull);
  });

  // Entry removal
  test('removeGlobalizeEntry leaves gaps', () {
    final table = GlobalWritersTable('p');
    table.addGlobalizeEntry(100, 'q');  // index 0
    table.addGlobalizeEntry(200, 'r');  // index 1
    table.removeGlobalizeEntry(0);
    
    expect(table.lookupByIndex(0), isNull);
    expect(table.lookupByIndex(1), isNotNull);
    
    // Next allocation should be 2, not reuse 0
    final i3 = table.addGlobalizeEntry(300, 's');
    expect(i3, 2);
  });

  test('removeLocalizeEntry by remote agent and index', () {
    final table = GlobalWritersTable('q');
    table.addLocalizeEntry(100, 'p', 5);
    table.removeLocalizeEntry('p', 5);
    expect(table.findByRemote('p', 5), isNull);
  });
});
```

### 0.2 Unit Tests for Globalize Operation

**File:** `glp_runtime/test/multiagent/globalize_test.dart`

**Tests derived from spec Section 5.1:**

```dart
group('Globalize', () {
  test('writer variable: spawns global_send, no entry', () {
    // Given: term with writer Y
    // When: globalize(Y, q)
    // Then: term becomes _w(p,0), spawn global_send(Y?, _w(p,0), q), no table entry
  });

  test('reader variable: creates entry, no spawn', () {
    // Given: term with reader Y?
    // When: globalize(Y?, q)
    // Then: term becomes _r(p,0), entry (Y, q) at index 0, no spawn
  });

  test('mixed term: correct handling of both', () {
    // Given: term [X, Y?]
    // When: globalize([X, Y?], q)
    // Then: term becomes [_w(p,0), _r(p,1)]
    //       spawn global_send(X?, _w(p,0), q)
    //       entry (Y, q) at index 1
  });

  test('nested structure: recursive globalization', () {
    // Given: term foo(bar(X), Y?)
    // When: globalize(foo(bar(X), Y?), q)
    // Then: correctly processes nested variables
  });

  test('index allocation is sequential', () {
    // Given: term [X, Y, Z?]
    // When: globalize
    // Then: indices 0, 1, 2 allocated in order
  });
});
```

### 0.3 Unit Tests for Localize Operation

**File:** `glp_runtime/test/multiagent/localize_test.dart`

**Tests derived from spec Section 5.2:**

```dart
group('Localize', () {
  test('_w(p,i): creates entry with remote index, returns reader', () {
    // Given: globalized term _w(p,5)
    // When: localize at q
    // Then: creates pair (Y_q, Y_q?), entry (Y_q, p, 5), term gets Y_q?
  });

  test('_r(p,i): spawns global_send, returns writer', () {
    // Given: globalized term _r(p,3)
    // When: localize at q
    // Then: creates pair (Z_q, Z_q?), spawn global_send(Z_q?, _r(p,3), p), term gets Z_q
  });

  test('mixed global names: correct handling', () {
    // Given: term [_w(p,0), _r(p,1)]
    // When: localize at q
    // Then: [Y_q?, Z_q], entry for Y_q, spawn for Z_q
  });
});
```

### 0.4 Unit Tests for global_send Goal

**File:** `glp_runtime/test/multiagent/global_send_test.dart`

**Tests derived from spec Section 4:**

```dart
group('GlobalSendGoal', () {
  test('fires when reader becomes known', () {
    // Given: global_send goal watching reader X?
    // When: writer X is bound to value T
    // Then: goal fires with value T
  });

  test('produces correct message', () {
    // Given: goal with global name _w(p,0), destination q
    // When: fires with value T
    // Then: message (_w(p,0) := T↑, q) added to M_p
  });

  test('nested variables spawn additional goals', () {
    // Given: goal fires with value containing variable Z
    // When: value is globalized
    // Then: new global_send goal spawned for Z
  });

  test('goal removed after firing', () {
    // Given: goal registered
    // When: fires
    // Then: goal no longer registered (one-shot)
  });
});
```

### 0.5 Integration Tests for Transactions

**File:** `glp_runtime/test/multiagent/mad_transactions_test.dart`

**Tests derived from spec Sections 8.1-8.4:**

```dart
group('Receive Transaction', () {
  test('_w(p,i) message: finds entry by remote, binds writer', () {
    // Given: LocalizeEntry (X_q, p, 0) exists at q
    // When: receive message _w(p,0) := T
    // Then: X_q bound to localized T, entry removed
  });

  test('_r(p,i) message: finds entry by index, binds writer', () {
    // Given: GlobalizeEntry (X, q) at index 0 exists at p
    // When: receive message _r(p,0) := T
    // Then: X bound to localized T, entry removed
  });

  test('receive triggers reactivation', () {
    // Given: goal suspended on X_q?
    // When: assignment to X_q arrives
    // Then: goal reactivated
  });
});

group('Network Transaction', () {
  test('globalizes at sender, localizes at receiver', () {
    // Given: p sends term X to q
    // When: Network transaction
    // Then: p has global_send goal, q has table entry
  });

  test('receiver gets reader form', () {
    // Given: p sends writer X
    // When: q receives
    // Then: q's resolvent has Y_q? (reader)
  });
});
```

### 0.6 Error Handling Tests

**File:** `glp_runtime/test/multiagent/mad_error_handling_test.dart`

**Tests derived from spec Section 12 (Invariants) - negative cases:**

```dart
group('Error Handling', () {
  test('receive for non-existent GlobalizeEntry throws', () {
    // Given: no entry at index 5
    // When: receive message _r(p,5) := T
    // Then: throws StateError
  });

  test('receive for non-existent LocalizeEntry throws', () {
    // Given: no entry matching (p, 3)
    // When: receive message _w(p,3) := T
    // Then: throws StateError
  });

  test('duplicate LocalizeEntry with same (agent, index) is rejected', () {
    // Given: entry (X, p, 5) already exists
    // When: addLocalizeEntry(Y, p, 5)
    // Then: throws ArgumentError
  });

  test('global_send on already-known reader fires immediately', () {
    // Given: goal to be registered on reader X?
    // And: X already bound (X? already known)
    // When: goal registration attempted
    // Then: goal fires immediately (no error)
  });

  test('removing non-existent entry is safe', () {
    // Given: no entry at index 3
    // When: removeGlobalizeEntry(3)
    // Then: no error (idempotent)
  });
});
```

### 0.7 End-to-End Scenario Tests

**File:** `glp_runtime/test/multiagent/mad_scenarios_test.dart`

**Tests derived from spec Section 10:**

```dart
group('Direct Communication (Section 10.1)', () {
  test('client-monitor: value flows from p to q', () {
    // Setup: p has client(Xs), q has monitor(Xs?)
    // Action: p assigns Xs := [add|Xs1]
    // Verify: q receives the value via global link
  });
});

group('Return Value Scenario (Section 10.2)', () {
  test('value request: value flows back from q to p', () {
    // Setup: p sends [value(V?)|...] to q
    // Action: q assigns V_q := Sum
    // Verify: p receives Sum via global link
  });
});

group('Friend-Mediated Introduction (Section 10.3)', () {
  test('three agents: value flows Charlie → Bob → Alice', () {
    // Setup: Bob sends X to Alice, X? to Charlie
    // Action: Charlie assigns X_c := T
    // Verify: T flows through Bob to Alice
  });
});

group('Both Ends Exported (Section 5.4)', () {
  test('[X, X?] exported: forwarding works correctly', () {
    // Setup: p exports [X, X?] to q
    // Action: q assigns Z_q := T (the writer it received)
    // Verify: T flows back through p and arrives at Y_q?
  });
});
```

### 0.8 Test Execution Strategy

**Phase 0a: Write all test files with failing tests (RED)**

Create the test files above. They will fail because the implementation doesn't exist yet.

**Phase 0b: Stub implementations to make tests compile**

Create minimal class/function stubs so tests compile but fail at runtime.

**Expected baseline before implementation:**
- All new tests: FAIL (expected)
- Existing tests: PASS (must not regress)

---

## Implementation Phases

### Phase 1: Data Structures

**1.1 Replace VariableTable with GlobalWritersTable**

The new table stores only writers awaiting incoming assignments. Two entry types:

```dart
/// Entry created by Globalize (when exporting a reader)
/// Direct index lookup: entry at index i corresponds to _r(p, i)
class GlobalizeEntry {
  final int writerAddr;      // Local writer X
  final String remoteAgent;  // Agent q who will send assignment
}

/// Entry created by Localize (when importing a writer global name)
/// Search-based lookup: must match (remoteAgent, remoteIndex)
class LocalizeEntry {
  final int writerAddr;      // Local writer X_q
  final String remoteAgent;  // Agent p who created the global name
  final int remoteIndex;     // Index i in _w(p, i)
}

class GlobalWritersTable {
  final String agentId;
  int _nextIndex = 0;
  
  // For Globalize entries: direct index lookup
  final Map<int, GlobalizeEntry> _globalizeEntries = {};
  
  // For Localize entries: search by (agent, index)
  final List<LocalizeEntry> _localizeEntries = [];
  
  /// Allocate next index and add Globalize entry
  int addGlobalizeEntry(int writerAddr, String remoteAgent);
  
  /// Add Localize entry (index not controlled by us)
  void addLocalizeEntry(int writerAddr, String remoteAgent, int remoteIndex);
  
  /// Lookup by index (for _r(p, i) messages where we are p)
  GlobalizeEntry? lookupByIndex(int index);
  
  /// Search for entry matching remote (agent, index) (for _w(p, i) messages)
  LocalizeEntry? findByRemote(String agent, int index);
  
  /// Remove entry
  void removeGlobalizeEntry(int index);
  void removeLocalizeEntry(String agent, int index);
}
```

**Files to modify:**
- `variable_table.dart` → rename to `global_writers_table.dart`, complete rewrite

**1.2 Update Message Types**

Remove ReadRequest and Abandon. Keep only Assignment with new format:

```dart
enum MessageType {
  assignment,  // _w(p, i) := T or _r(p, i) := T
}

class AssignmentMessage {
  final GlobalName globalName;  // _w(p,i) or _r(p,i)
  final Uint8List payload;      // Serialized term with global names
}

class GlobalName {
  final bool isWriter;    // true for _w, false for _r
  final String agent;     // p
  final int index;        // i
}
```

**Files to modify:**
- `message_queue.dart` - simplify to single message type

**1.3 Add GlobalName Support**

Terms crossing boundaries need to carry global names instead of agent:localId pairs:

```dart
/// Global variable name: _w(p, i) or _r(p, i)
class GlobalVarName {
  final bool isWriter;  // true = _w, false = _r
  final String agent;
  final int index;
}
```

**Files to modify:**
- `payload_serializer.dart` - serialize/deserialize global names

---

### Phase 2: Core Operations

**2.1 Implement Globalize**

Replace `export()` helper with `globalize()`:

```dart
class GlobalizeResult {
  final Term globalizedTerm;           // T_p↑ with global names substituted
  final List<GlobalSendGoal> spawns;   // global_send goals to add to resolvent
}

GlobalizeResult globalize(
  Term term,
  String remoteAgent,
  GlobalWritersTable table,
  bool Function(int) isReader,
) {
  // For each variable Y in term:
  // - If Y is writer: allocate index i, replace with _w(p,i), 
  //   spawn global_send(Y?, _w(p,i), q). NO entry.
  // - If Y? is reader: allocate index i, create entry (Y, q) at index i,
  //   replace with _r(p,i). NO spawn.
}
```

**2.2 Implement Localize**

Replace import logic with `localize()`:

```dart
class LocalizeResult {
  final Term localizedTerm;            // T_q↓ with fresh local pairs
  final List<GlobalSendGoal> spawns;   // global_send goals to add to resolvent
}

LocalizeResult localize(
  Term globalizedTerm,
  String remoteAgent,
  GlobalWritersTable table,
  List<int> Function() allocatePair,
) {
  // For each global name in term:
  // - If _w(p,i): create pair (Y,Y?), add entry (Y,p,i), replace with Y?. NO spawn.
  // - If _r(p,i): create pair (Z,Z?), replace with Z, spawn global_send(Z?,_r(p,i),p). NO entry.
}
```

**Files to modify:**
- `helpers.dart` - replace export/request/abandon with globalize/localize

---

### Phase 3: The global_send Mechanism

**3.1 Understanding global_send (Symmetric Design)**

The `global_send` goal is spawned whenever an agent needs to send a value outward when a local reader becomes known. This happens symmetrically in two situations:

1. **Globalizing a writer Y**: The agent spawns `global_send(Y?, _w(p,i), q)` to send Y?'s value to q when known
2. **Localizing `_r(p,i)`**: The agent creates fresh pair (Z, Z?) and spawns `global_send(Z?, _r(p,i), p)` to send Z?'s value to p when known

Both are the same operation: "I have a local pair, I need to send the reader's value somewhere when it's known."

**3.2 Implementation Approach**

**Option A: Native Dart goal registry** (simpler, recommended for initial implementation)
- Maintain a registry mapping reader addresses to pending `global_send` goals
- When a writer is bound, check if its paired reader has a registered goal
- If so, fire the goal (globalize value, send message, register any new goals for nested variables)

**Option B: GLP goal in resolvent** (closer to spec, more complex)
- Spawn actual `global_send(T, G, Q)` goals into the resolvent
- Requires bytecode support for `known/1` guard and `'_send'/3` builtin

Recommend Option A initially, with path to Option B later.

```dart
/// A pending global_send goal waiting for a reader to become known
/// 
/// Spawned symmetrically when:
/// - Globalizing a writer Y: watches Y?, sends to destination
/// - Localizing _r(p,i): watches Z?, sends back to p
class GlobalSendGoal {
  final int readerAddr;       // Reader to watch
  final GlobalName globalName; // _w(p,i) or _r(p,i)
  final String destination;   // Agent to send to
  
  GlobalSendGoal({
    required this.readerAddr,
    required this.globalName,
    required this.destination,
  });
}

/// Registry for pending global_send goals
class GlobalSendRegistry {
  final Map<int, GlobalSendGoal> _goals = {};
  
  void register(GlobalSendGoal goal) {
    _goals[goal.readerAddr] = goal;
  }
  
  /// Called when a writer is bound. Returns new goals spawned for nested variables.
  List<GlobalSendGoal> onWriterBound(
    int writerAddr,
    Term value,
    GlobalWritersTable table,
    MessageQueue queue,
    String localAgent,
  ) {
    final goal = _goals.remove(writerAddr); // One-shot: remove after firing
    if (goal == null) return [];
    
    // Globalize the value (may produce new goals for nested variables)
    final result = globalize(
      variables: extractVariables(value),
      localAgent: localAgent,
      remoteAgent: goal.destination,
      table: table,
    );
    
    // Queue the message
    queue.add(AssignmentMessage(
      globalName: goal.globalName,
      payload: serialize(value, result.globalNames),
    ));
    
    // Return new goals for nested variables (caller must register them)
    return result.spawns.map((s) => GlobalSendGoal(
      readerAddr: s.readerAddr,
      globalName: s.globalName,
      destination: s.destAgent,
    )).toList();
  }
}
```

**3.3 Hook into Heap Binding**

When a writer is bound, check if its reader has a pending `global_send` goal:

```dart
// In heap binding logic or IrmaContext
void bindWriter(int writerAddr, Term value) {
  // ... existing binding logic ...
  
  // Check for global_send goal on the paired reader
  final newGoals = _globalSendRegistry.onWriterBound(
    writerAddr, value, _writersTable, _messageQueue, agentId);
  
  // Register any new goals spawned for nested variables
  for (final goal in newGoals) {
    _globalSendRegistry.register(goal);
  }
}
```

**Files to create/modify:**
- New file `global_send.dart` - `GlobalSendGoal` and `GlobalSendRegistry` classes
- `irma_context.dart` - integrate registry, hook into binding

---

### Phase 4: Transaction Updates

**4.1 Update Reduce Transaction**

The Reduce transaction no longer directly generates outgoing messages. Instead:
- Binding a writer triggers `global_send` callbacks (Phase 3)
- Remove direct message generation from reduce

**4.2 Implement Send Transaction**

Simple: dequeue from M_p, put in channel to destination.

**4.3 Implement Receive Transaction**

```dart
void handleReceive(AssignmentMessage msg) {
  final name = msg.globalName;
  
  if (name.isWriter) {
    // _w(p, i) := T - we localized this, search for entry
    final entry = table.findByRemote(name.agent, name.index);
    if (entry == null) throw StateError('No entry for ${name}');
    
    // Localize the term
    final result = localize(msg.term, name.agent, table, allocatePair);
    
    // Bind the writer
    bindWriter(entry.writerAddr, result.localizedTerm);
    
    // Register any spawned goals
    for (final spawn in result.spawns) {
      _globalSendRegistry.register(GlobalSendGoal(
        readerAddr: spawn.readerAddr,
        globalName: spawn.globalName,
        destination: spawn.destAgent,
      ));
    }
    
    // Remove entry
    table.removeLocalizeEntry(name.agent, name.index);
  } else {
    // _r(p, i) := T - we globalized this, direct lookup
    final entry = table.lookupByIndex(name.index);
    if (entry == null) throw StateError('No entry at index ${name.index}');
    
    // Localize the term  
    final result = localize(msg.term, entry.remoteAgent, table, allocatePair);
    
    // Bind the writer
    bindWriter(entry.writerAddr, result.localizedTerm);
    
    // Register any spawned callbacks
    for (final spawn in result.spawns) {
      registerGlobalSendCallback(spawn);
    }
    
    // Remove entry
    table.removeGlobalizeEntry(name.index);
  }
  
  // Reactivate suspended goals
  reactivateGoalsOn(entry.writerAddr);
}
```

**4.4 Update Network Transaction**

```dart
void handleNetwork(String destination, Term term) {
  // Globalize
  final result = globalize(term, destination, table, isReader);
  
  // Register spawned global_send callbacks
  for (final spawn in result.spawns) {
    registerGlobalSendCallback(spawn);
  }
  
  // Send globalized term to destination for localization
  // (This is the atomic part - in practice, send via channel)
  sendToDestination(destination, result.globalizedTerm);
}

void receiveNetwork(String source, Term globalizedTerm) {
  // Localize
  final result = localize(globalizedTerm, source, table, allocatePair);
  
  // Register spawned global_send callbacks
  for (final spawn in result.spawns) {
    registerGlobalSendCallback(spawn);
  }
  
  // Add to network input stream (with readers replacing variables)
  addToInputStream(toReaderForm(result.localizedTerm));
}
```

**Files to modify:**
- `irma_context.dart` - major rewrite of transaction handling

---

### Phase 5: Cleanup

**5.1 Remove Dead Code**

- Remove `request()` helper
- Remove `abandon()` helper  
- Remove `VariableRole` enum
- Remove `ReadRequest` and `Abandon` message types
- Remove requester tracking
- Remove requestSent tracking

**5.2 Update Tests**

All existing multiagent tests will need updates:
- `variable_table_test.dart` → `global_writers_table_test.dart`
- `irma_context_test.dart` - rewrite for new transaction model
- `helpers_test.dart` - rewrite for globalize/localize
- `bidirectional_stream_test.dart` - should work with new model
- `simple_imported_reader_test.dart` - update for push model

---

## Migration Strategy

**Recommended approach: Parallel implementation**

1. Create new files alongside old ones:
   - `global_writers_table.dart` (new) alongside `variable_table.dart` (old)
   - `mad_helpers.dart` (new) alongside `helpers.dart` (old)
   - `mad_context.dart` (new) alongside `irma_context.dart` (old)

2. Write new tests for new implementation

3. Once new implementation passes tests, remove old files

4. Rename if desired (e.g., `mad_context.dart` → `irma_context.dart`)

---

## Risk Assessment

**High risk areas:**

1. **Goal firing timing**: Ensuring `global_send` goals fire at the right time and don't cause infinite loops with nested variables

2. **Globalize recursion**: When globalizing a term that contains variables, those variables spawn goals. When those goals fire, they globalize the value, which may contain more variables. Need to handle this correctly.

3. **Entry lifecycle**: Ensuring entries are removed at the right time and not accessed after removal

**Mitigation:**

- Extensive unit tests for edge cases
- Trace logging during development
- Start with simple scenarios (direct communication) before complex ones (friend-mediated introduction)

---

## Test Plan

**Unit tests:**
1. GlobalWritersTable: add/remove/lookup for both entry types
2. Globalize: writer variables, reader variables, mixed terms, nested structures
3. Localize: _w names, _r names, mixed terms
4. GlobalSendGoal: registration, firing, nested variable handling

**Integration tests:**
1. Direct communication (client-monitor scenario)
2. Callback scenario (value request pattern)
3. Both ends exported (forwarding)
4. Friend-mediated introduction

**Regression tests:**
- Ensure existing single-agent tests still pass

---

## Estimated Effort

| Phase | Estimated Time | Dependencies |
|-------|----------------|--------------|
| Phase 1: Data Structures | 2-3 hours | None |
| Phase 2: Core Operations | 3-4 hours | Phase 1 |
| Phase 3: global_send | 4-5 hours | Phase 2 |
| Phase 4: Transactions | 3-4 hours | Phase 3 |
| Phase 5: Cleanup | 2-3 hours | Phase 4 |
| Testing | 4-5 hours | All phases |

**Total: ~20-24 hours of focused work** (for developer familiar with codebase; double for new developer)

---

## Correctness Requirements

### Goal Atomicity (from spec Section 12)

When a `global_send` goal fires, the following must happen atomically within a single Reduce transaction:

1. The value T (from the now-known reader) is globalized
2. Globalization may spawn additional `global_send` goals for nested variables in T
3. These new goals must be registered before the current goal completes
4. The message `(G := T↑, Q)` is added to M_p

This atomicity ensures that if T contains variable Z, the goal for Z is registered before any subsequent Reduce could observe Z becoming known.

**Implementation note**: The `onWriterBound` method must:
```dart
List<GlobalSendGoal> onWriterBound(int writerAddr, Term value, ...) {
  // 1. Globalize (may produce new spawns)
  final result = globalize(value, destination, table, isReader);
  
  // 2. Add message to outgoing set
  messageQueue.add(AssignmentMessage(globalName, result.globalizedTerm));
  
  // 3. Return new goals (caller registers them immediately)
  return result.spawns.map((s) => GlobalSendGoal(...)).toList();
}
// Caller then registers all returned goals before proceeding
```

---

## Debugging and Tracing Strategy

The goal firing mechanism is the highest-risk area. To debug issues:

### Trace Points

Add trace logging at these points (enabled via a debug flag):

1. **Goal registration**: Log reader address, global name, destination
2. **Writer binding**: Log writer address, value, whether reader has goal
3. **Goal firing**: Log which goal fired, value received, spawned goals count
4. **Message send**: Log global name, destination, serialized term size
5. **Message receive**: Log global name, source, entry found/not found
6. **Entry lifecycle**: Log entry creation (Globalize/Localize) and removal (Receive)

### Debugging Scenarios

When debugging goal firing issues, create minimal test cases:

1. **Single variable**: p sends X to q, p assigns X := 1. Verify message arrives.
2. **Nested variable**: p sends X to q, p assigns X := foo(Y), p assigns Y := 2. Verify both messages arrive in order.
3. **Forwarding**: p sends [X, X?] to q, q assigns Z := 3. Verify value arrives at Y?.

### Invariant Checks

Add runtime assertions (debug mode only) to verify invariants:

```dart
void assertInvariants() {
  // Each index used at most once
  assert(_usedIndices.length == _nextIndex);
  
  // No duplicate (agent, remoteIndex) in LocalizeEntries
  final seen = <(String, int)>{};
  for (final entry in _localizeEntries) {
    assert(!seen.contains((entry.remoteAgent, entry.remoteIndex)));
    seen.add((entry.remoteAgent, entry.remoteIndex));
  }
}
```

---

## Open Questions

1. **Suspension handling**: The spec mentions reactivating suspended goals when a writer is bound. The current implementation stores suspensions on writer cells for local variables and in VariableEntry for imported readers. In madGLP, all variables are local pairs, so suspensions should just use the standard heap mechanism. Need to verify this works correctly.

2. ~~**Index allocation**: Should we use a single counter for both writer and reader globalizations, or separate counters?~~ **RESOLVED**: Spec Section 3.2 now explicitly states a single counter is used.

3. **Garbage collection**: When are `global_send` goals removed if they never fire? The spec doesn't address this. For now, assume they persist until the reader becomes known or the agent terminates.

---

## Next Steps

1. Review this plan
2. Decide on parallel vs. in-place implementation
3. Start with Phase 1 (data structures)
4. Proceed through phases with tests at each stage
