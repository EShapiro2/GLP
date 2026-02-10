# irmaGLP VarKey Implementation Handover

**Date**: 2026-01-19  
**Author**: Claude (AI Assistant)  
**Version**: 1.0  
**Status**: VarKey complete, friend-mediated introduction partially working

---

## 1. Executive Summary

### What Was Done
Implemented **VarKey composite key architecture** for VariableTable (V_p), enabling the same varId to have separate reader and writer entries. This was required for Bob to properly track variables during friend-mediated introductions where he sends both parts of a channel variable to different agents.

### Current Test Status
- **156 passing** tests (up from 99)
- **2 failing** tests (pre-existing, unrelated to VarKey)
- All new VarKey tests pass (29 tests in `variable_table_test.dart`)

### What Still Needs Work
**The friend-mediated introduction protocol does not fully work end-to-end.** When Bob introduces Alice to Charlie:
- ✅ Bob successfully sends `intro_offer` to both Alice and Charlie
- ✅ Alice and Charlie can accept the introduction (`accept_intro(...)`)
- ✅ They see `accepted_intro(...)` feedback
- ❌ **Messages between Alice and Charlie don't arrive** (e.g., `send(charlie, hi)` from Alice fails to reach Charlie)

---

## 2. VarKey Architecture

### The Problem It Solved

Per the irmaGLP spec, Bob creates a channel `ch(AC?, CA)` for Alice and `ch(CA?, AC)` for Charlie. For varId 1042 (representing AC/AC?):
- Bob sends **AC (writer)** to Charlie
- Bob sends **AC? (reader)** to Alice

Previously, VariableTable used just `varId` as the key, so adding the reader entry would overwrite the writer entry. VarKey solves this:

```dart
/// Composite key for VariableTable entries
/// Combines varId with isReader flag to allow same varId to have
/// separate reader and writer entries.
class VarKey {
  final int varId;
  final bool isReader;
  
  VarKey(this.varId, this.isReader);
  
  @override
  bool operator ==(Object other) =>
      other is VarKey && 
      other.varId == varId && 
      other.isReader == isReader;
  
  @override
  int get hashCode => Object.hash(varId, isReader);
  
  @override
  String toString() => isReader ? '$varId?' : '$varId';
}
```

### Files Modified

| File | Changes |
|------|---------|
| `lib/multiagent/variable_table.dart` | Added `VarKey` class, changed `VariableTable` internal map from `Map<int, VariableEntry>` to `Map<VarKey, VariableEntry>` |
| `lib/multiagent/irma_context.dart` | Updated all `vp.add()`, `vp.lookup()`, `vp.contains()`, `vp.remove()`, `vp.updateState()` calls to use `VarKey` |
| `lib/multiagent/helpers.dart` | Updated all V_p operations to use `VarKey` |
| `test/multiagent/variable_table_test.dart` | Complete rewrite with 29 VarKey tests |
| `test/multiagent/irma_context_test.dart` | Updated for VarKey API |
| `test/multiagent/helpers_test.dart` | Updated for VarKey API |
| `test/multiagent/irma_agent_test.dart` | Updated for VarKey API |

### API Changes

**Before:**
```dart
vp.add(42, entry);
vp.lookup(42);
vp.contains(42);
```

**After:**
```dart
vp.add(VarKey(42, false), entry);  // writer
vp.add(VarKey(42, true), entry);   // reader
vp.lookup(VarKey(42, false));
vp.contains(VarKey(42, true));
```

---

## 3. The Remaining Bug: Dynamic Friend Channel Observation

### Symptom
Alice sends `send(charlie, hi)` → Alice sees `sent(charlie, hi)` but Charlie never receives the message.

### Root Cause Analysis

When Bob introduces Alice to Charlie, he creates channel variables and sends them via `intro_offer` messages. The GLP program does this in `social_graph.glp`:

```prolog
%% User requests introduction: introduce(P, Q)
social_graph(Id, [introduce(P, Q)|In], Fs, Pending) :-
    ground(Id?), ground(P?), ground(Q?) |
    lookup_send(P?, msg(Id?, P?, intro(Q?, ch(QtoP?, PtoQ))), Fs?, Fs1, Status1),
    lookup_send(Q?, msg(Id?, Q?, intro(P?, ch(PtoQ?, QtoP))), Fs1?, Fs2, Status2),
    ...
```

When Alice accepts the introduction:
```prolog
handle_accept_intro(found, Id, Other, ch(FIn, FOut), In, Fs, Pending) :-
    ground(Other?) |
    tag_stream(Other?, FIn?, Tagged),
    merge(In?, Tagged?, In1),
    add_friend(Other?, FOut?, Fs?, Fs1),  %% <-- Adds (charlie, OutStream) to friends
    ...
```

**The Problem:** Pre-configured friends (like Bob) have their output streams observed by `OutputObserver` instances created in `main.dart` during initialization. But dynamically-added friends (Charlie, from Alice's perspective after introduction) are added to the `Fs` list purely at the GLP level - there's no corresponding `OutputObserver` created.

### Why Pre-Configured Friends Work

In `main.dart`, when the agent initializes:

```dart
for (final friend in widget.friends) {
  final friendLower = friend.toLowerCase();
  final channel = createExternalChannel(_agent!.runtime.heap, friendLower);
  friendChannels[friendLower] = channel;
  
  // Output observer for sending to this friend
  friendOutputs[friendLower] = OutputObserver(
    _agent!.runtime.heap,
    friendLower,
    channel.outputVarId,
    (term) {
      _pendingFriendOutputTerms.putIfAbsent(friendLower, () => []).add(term);
    },
    () {
      debugPrint('=== FRIEND $friendLower OUTPUT CLOSED ===');
    },
  );
}
```

This creates heap onBind callbacks that fire when the output stream is written to, triggering network sends.

### Why Dynamic Friends Don't Work

When Alice adds Charlie as a friend after accepting introduction:
1. The GLP program adds `(charlie, FOut?)` to the friends list
2. `FOut` is a fresh variable created by Bob, received via the `intro_offer` message
3. **No `OutputObserver` is created** for this variable
4. When Alice's GLP writes `msg(alice, charlie, hi)` to `FOut`, nothing observes it
5. The message never gets sent over the network

### Architectural Gap

The channel variables from introductions are different from pre-configured friend channels:

| Aspect | Pre-configured Friends | Introduction Friends |
|--------|------------------------|---------------------|
| Who creates channel | Local agent | Bob (introducer) |
| Where variables live | Local heap, known IDs | Received via message, allocated dynamically |
| Output observer | Created at init time | **NONE** |
| V_p tracking | Yes (via `exportTerm`) | Yes (via `importTerm`) |

---

## 4. Proposed Solution Approaches

### Approach A: Dynamic Observer Registration

When `importTerm` receives a writer variable that will be used for output, automatically create an observer:

```dart
void importTerm(Term term, String fromAgent, {Map<int, GlobalVarId>? globalIdMapping}) {
  _importTermRecursive(term, fromAgent, globalIdMapping ?? {});
}

void _importTermRecursive(Term term, String fromAgent, ...) {
  if (term is VarRef && !term.isReader) {
    // This is an imported WRITER - it's an output channel
    // We need to observe when Alice writes to it
    
    // 1. Register in V_p (already done)
    registerImportedWriter(term.varId, creator, creatorLocalId: ...);
    
    // 2. NEW: Also notify the app layer to observe this writer
    onImportedWriterCreated?.call(term.varId, creator);
  }
  ...
}
```

Then in `main.dart`, handle this callback to create a dynamic observer.

**Complexity**: Medium. Requires callback plumbing from `IrmaContext` to Flutter app.

### Approach B: Unified Output Stream via `netOutput`

Instead of per-friend output observers, route ALL friend messages through a single `netOutput` stream. The GLP program would write `msg(From, To, Content)` to netOutput, and the app layer would extract `To` and route accordingly.

This is already partially implemented - the current code routes via `net` channel. But the GLP program writes directly to friend-specific output streams.

**Complexity**: High. Requires GLP program changes.

### Approach C: Message Interception at V_p Level

Since imported writers are tracked in V_p with heap callbacks, intercept the binding there:

When Alice writes to the imported writer (Charlie's input channel):
1. The heap callback `_onWriterBound` fires
2. It detects `importedWriter` role
3. It sends assignment to Bob (the creator)
4. Bob receives assignment, sees it's for a `createdReader` with Charlie as requester
5. Bob forwards to Charlie

**This is how it should work already!** Let me trace why it doesn't...

### The Actual Problem

Wait - I think I see it. When Alice writes `send(charlie, hi)`, the GLP program:
1. Looks up `(charlie, OutStream)` in friends list
2. Writes `msg(alice, charlie, hi)` to `OutStream`

But `OutStream` is just a variable in the GLP resolvent. The issue is:
- This variable **was received from Bob** as part of `ch(FIn?, FOut)`
- The `FOut` (writer) part is what Alice writes to
- It **is** registered in Alice's V_p as `importedWriter` (via `importTerm`)
- When bound, `_onWriterBound` should fire and notify Bob

Let me check if the binding callback is actually registered...

**Hypothesis**: The imported writer's heap callback might not be firing because:
1. The variable isn't being bound through `heap.bindVariable()`
2. OR the callback was never registered because `importTerm` doesn't see this as a writer

Looking at `handle_accept_intro`:
```prolog
handle_accept_intro(found, Id, Other, ch(FIn, FOut), In, Fs, Pending) :-
    ...
    add_friend(Other?, FOut?, Fs?, Fs1),  %% FOut is the WRITER
```

And `importTerm` in Alice's context should register `FOut` as importedWriter when the `intro_offer` message arrives.

**Need to debug**: Add logging to verify:
1. Is `importTerm` called with the channel term?
2. Does it identify `FOut` as a writer (not reader)?
3. Is `registerImportedWriter` called?
4. Does the heap callback fire when Alice writes to `FOut`?

---

## 5. Test Commands

### Run All Multiagent Tests
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/ > /private/tmp/ma-tests.txt 2>&1
```

### Run Specific Test File
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/variable_table_test.dart > /private/tmp/vt-test.txt 2>&1
```

### Run Flutter App
```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos
```

### Interactive Test Sequence
1. Click "Alice↔Bob↔Charlie" to spawn agents
2. In Bob's window: `introduce(alice, charlie)`
3. In Alice's window: `accept_intro(charlie)`
4. In Charlie's window: `accept_intro(alice)`
5. In Alice's window: `send(charlie, hi)`
6. **Expected**: Charlie sees `received(alice, hi)`
7. **Actual**: Charlie sees nothing

---

## 6. Key Files Reference

| File | Purpose |
|------|---------|
| `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/variable_table.dart` | VarKey and VariableTable |
| `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_context.dart` | Agent context with V_p/M_p |
| `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_agent.dart` | High-level agent wrapper |
| `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/helpers.dart` | abandon/request/export helpers |
| `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/payload_serializer.dart` | Message serialization |
| `/Users/udi/Grassroots/GLP/glp_multiagent/lib/main.dart` | Flutter app, agent windows |
| `/Users/udi/Grassroots/GLP/glp_multiagent/lib/irma_router.dart` | Coordinator message routing |
| `/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp` | GLP agent program |
| `/Users/udi/Grassroots/GLP/docs/ma/irmaGLP-spec.md` | Specification v2.1 |

---

## 7. Debugging the Introduction Protocol

### Add Debug Logging

In `irma_context.dart`, the `importTerm` method should log each imported variable:

```dart
void _importTermRecursive(Term term, String fromAgent, Map<int, GlobalVarId> globalIdMapping) {
  if (term is VarRef) {
    final key = VarKey(term.varId, term.isReader);
    if (!vp.contains(key)) {
      print('[DEBUG IRMA $agentId] importTerm: varId=${term.varId}, isReader=${term.isReader}');
      ...
    }
  }
  ...
}
```

### Verify Heap Callback Registration

In `registerImportedWriter`:
```dart
void registerImportedWriter(int varId, String creator, {int? creatorLocalId}) {
  print('[DEBUG IRMA $agentId] registerImportedWriter: varId=$varId, creator=$creator');
  
  // Register heap callback
  runtime.heap.onBind(varId, (Term value) {
    print('[DEBUG IRMA $agentId] IMPORTED WRITER BOUND: varId=$varId, value=$value');
    _onWriterBound(varId, value);
  });
}
```

### Check GLP Variable Flow

The issue might be that the GLP program writes to the stream using list unification (`Out? = [Msg|Out1]`) rather than direct binding. The `OutputObserver` class handles this via the heap's value chain, but imported writers might not have this observer set up.

---

## 8. Next Steps

1. **Debug session**: Add logging to trace the exact flow when Alice sends to Charlie
   - Verify `importTerm` is called when Alice receives `intro_offer`
   - Verify the channel's writer part is registered as `importedWriter`
   - Verify heap callback fires when GLP writes to the stream

2. **Identify the gap**: Determine whether:
   - The variable isn't being observed (callback not registered)
   - The variable is being observed but callback doesn't fire (binding mechanism issue)
   - The callback fires but message routing fails (V_p lookup issue)

3. **Implement fix**: Based on findings, either:
   - Fix callback registration in `importTerm`
   - Add dynamic observer creation for imported output writers
   - Or fix the message routing path through Bob

4. **Update spec**: Document the solution in irmaGLP-spec.md once working

---

## 9. Summary of Changes This Session

| Item | Before | After |
|------|--------|-------|
| VariableTable key | `int varId` | `VarKey(varId, isReader)` |
| Passing tests | 99 | **156** |
| Failing tests | 4 | **2** |
| VarKey tests | 0 | **29** |
| Introduction protocol | Not working | **Partially working** (accepts work, messages don't flow) |

The VarKey foundation is solid. The remaining work is debugging why the imported writer channels from introductions don't fire their heap callbacks when written to by the GLP program.
