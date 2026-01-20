# Shared Logic Variable Test Design

**Date:** 2026-01-20  
**Status:** DRAFT  
**Purpose:** Minimal test for IRMA shared variable communication between two isolates

---

## 1. Test Scenario

Two isolates (@1 and @2) share a logic variable X. Isolate @1 has the writer X, isolate @2 has the imported reader X?. Both run goals that require the variable to be bound.

**Program:**
```prolog
p(a).
q(a).
```

**Goals:**
- @1 runs `p(X)` - matches `p(a)`, binds writer X = a
- @2 runs `q(X?)` - needs X? to be bound, suspends until assignment arrives

**Expected Flow:**
1. @1 runs `p(X)`, matches `p(a)`, binds X = a
2. Binding triggers heap callback, which queues assignment message to @2
3. Flush messages: @1 sends `(X? := a)` to @2
4. @2 receives assignment, binds imported reader X? = a
5. Suspended goal `q(X?)` is reactivated (from VariableEntry.suspensions)
6. @2 runs `q(a)`, matches `q(a)`, succeeds

---

## 2. Component Architecture

### 2.1 Per-Isolate Components

Each isolate needs:

| Component | Purpose |
|-----------|---------|
| `GlpRuntime` | Heap, goal queue, system predicates |
| `BytecodeRunner` | Executes bytecode instructions |
| `Scheduler` | Manages goal execution, provides `drainWithStatus()` |
| `IrmaContext` | V_p (variable table), M_p (message queue), message routing |

### 2.2 Key Classes

**BytecodeProgram** - Compiled program
- `.ops` - List of bytecode instructions
- `.labels` - Map from label name (e.g., "p/1") to PC

**CallEnv** - Goal arguments
- `.argBySlot` - Map from slot index to Term
- Used to pass arguments when spawning a goal

**GoalRef** - Reference to a goal in the queue
- `.id` - Goal ID (integer)
- `.pc` - Program counter (entry point)

**VariableEntry** - V_p entry for shared variables
- `.varId` - Local heap ID
- `.creator` - Agent who created the variable
- `.creatorLocalId` - Creator's original local ID
- `.role` - createdWriter, importedReader, etc.
- `.state` - Role-dependent state (requester, value, etc.)
- `.suspensions` - List of goals suspended on this variable (for imported readers)

---

## 3. Test Setup Procedure

### 3.1 Compile Program

```dart
final compiler = GlpCompiler();
final program = compiler.compile('p(a). q(a).');
```

### 3.2 Create Isolate @1

```dart
// Runtime and runner
final runtime1 = GlpRuntime();
final runner1 = BytecodeRunner(program);
final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});

// IRMA context
final ctx1 = IrmaContext(agentId: 'isolate1', runtime: runtime1);
```

### 3.3 Create Isolate @2

```dart
// Runtime and runner
final runtime2 = GlpRuntime();
final runner2 = BytecodeRunner(program);
final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});

// IRMA context
final ctx2 = IrmaContext(agentId: 'isolate2', runtime: runtime2);
```

### 3.4 Allocate Shared Variable X

**In @1 (creator):**
```dart
// Allocate writer X (two-cell variable: writer at N, reader at N+1)
final writerVarId = runtime1.heap.allocateVariable();

// Register in V_p as created writer (paired reader is non-local)
ctx1.registerWriter(writerVarId);
// This sets up heap callback: when X is bound, notify requester
```

**In @2 (importer):**
```dart
// Allocate single cell for imported reader (no local writer)
final importedReaderId = runtime2.heap.allocateImportedReader();

// Create VariableEntry for V_p
final readerEntry = VariableEntry(
  varId: importedReaderId,
  isReader: true,
  creator: 'isolate1',
  role: VariableRole.importedReader,
  creatorLocalId: writerVarId,  // Creator's ID for message routing
);

// Add to V_p
ctx2.vp.add(VarKey(importedReaderId, true), readerEntry);

// Attach entry to heap cell (for deref and suspension storage)
runtime2.heap.cells[importedReaderId].content = readerEntry;
```

### 3.5 Set Up Message Routing

```dart
ctx1.onMessageReady = (destination, message) {
  if (destination == 'isolate2' && message.type == MessageType.assignment) {
    // Parse payload and deliver to @2
    final (creatorLocalId, value) = parseAssignment(message.payload);
    ctx2.handleAssignment('isolate1', creatorLocalId, value);
  }
};
```

### 3.6 Spawn Goals

**In @1: p(X)**
```dart
final goalId1 = 1;
final entryPC1 = program.labels['p/1']!;
final env1 = CallEnv(args: {0: VarRef(writerVarId, isReader: false)});

runtime1.setGoalEnv(goalId1, env1);
runtime1.setGoalProgram(goalId1, 'main');
runtime1.gq.enqueue(GoalRef(goalId1, entryPC1));
```

**In @2: q(X?)**
```dart
final goalId2 = 1;
final entryPC2 = program.labels['q/1']!;
final env2 = CallEnv(args: {0: VarRef(importedReaderId, isReader: true)});

runtime2.setGoalEnv(goalId2, env2);
runtime2.setGoalProgram(goalId2, 'main');
runtime2.gq.enqueue(GoalRef(goalId2, entryPC2));
```

---

## 4. Test Execution

### 4.1 Run @1

```dart
final result1 = scheduler1.drainWithStatus();
// Expected: succeeded
// X is now bound to 'a'
// Heap callback fired, assignment message queued in ctx1.mp
```

### 4.2 Flush Messages

```dart
ctx1.flushMessages();
// Calls onMessageReady callback
// ctx2.handleAssignment('isolate1', writerVarId, ConstTerm('a'))
```

### 4.3 Run @2

Before assignment arrives, @2's goal `q(X?)` would suspend because X? is unbound.

After assignment arrives:
- `handleAssignment` stores value in entry.state
- `handleAssignment` activates suspensions from entry.suspensions
- `handleAssignment` binds heap cell

```dart
final result2 = scheduler2.drainWithStatus();
// Expected: succeeded (if goal was reactivated)
// X? is now bound to 'a'
```

---

## 5. Verification Points

| Check | Expected |
|-------|----------|
| @1 drain status | `ExecutionStatus.succeeded` |
| @1 X value | `ConstTerm('a')` |
| @1 message count | 1 assignment to isolate2 |
| @2 drain status | `ExecutionStatus.succeeded` |
| @2 X? value | `ConstTerm('a')` |

---

## 6. Key Implementation Details

### 6.1 Suspension Storage (Per Recent Implementation)

For imported readers, suspensions are stored in `VariableEntry.suspensions`, not in the heap cell content. This is because the heap cell content holds the VariableEntry itself (for V_p routing).

When a goal suspends on an imported reader:
1. `SuspendOps.suspendGoalFCP` checks if cell content is VariableEntry
2. If so, suspension is added to `entry.suspensions`
3. Cell content (VariableEntry) is preserved

When assignment arrives:
1. `handleAssignment` calls `_activateSuspensionsFromEntry(entry)`
2. Walks entry.suspensions, activates armed records
3. Returns GoalRefs which are enqueued in runtime.gq

### 6.2 Message Payload Format

Assignment messages contain:
- Global variable ID: `"creator:localId"` format (e.g., "isolate1:0")
- Serialized term value

The receiver uses `creatorLocalId` to look up the entry in V_p via `findByCreatorLocalId()`.

---

## 7. Potential Issues to Watch

1. **Goal suspension timing**: If @2 runs before assignment arrives, goal must suspend properly and be reactivated when assignment arrives.

2. **Entry attachment**: The VariableEntry must be attached to the heap cell content for `SuspendOps` to find it.

3. **Payload parsing**: The test needs to correctly parse the assignment payload to extract creatorLocalId and value.

4. **Entry removal**: After binding, the entry should be removed from V_p (variable is now local).

---

## 8. Files Involved

| File | Role |
|------|------|
| `lib/compiler/compiler.dart` | GlpCompiler |
| `lib/bytecode/runner.dart` | BytecodeRunner, BytecodeProgram, CallEnv |
| `lib/runtime/runtime.dart` | GlpRuntime |
| `lib/runtime/scheduler.dart` | Scheduler, ExecutionStatus, DrainResult |
| `lib/runtime/machine_state.dart` | GoalRef, GoalQueue |
| `lib/runtime/heap_fcp.dart` | HeapFCP |
| `lib/runtime/terms.dart` | VarRef, ConstTerm, StructTerm |
| `lib/multiagent/irma_context.dart` | IrmaContext |
| `lib/multiagent/variable_table.dart` | VariableTable, VariableEntry, VarKey |
| `lib/multiagent/message_queue.dart` | MessageQueue, OutboundMessage, MessageType |
| `lib/runtime/suspend_ops.dart` | SuspendOps.suspendGoalFCP |
