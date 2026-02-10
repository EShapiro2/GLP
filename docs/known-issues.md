# GLP Known Issues

## Issue 1: Localize uses writer address where reader address is needed

**Status**: Open
**Discovered**: 2026-02-10
**Affects**: Multi-agent (madGLP) programs where a term with unbound variables is sent between agents

### Summary

The `localize()` function in `mad_helpers.dart` substitutes the writer address into the term where the spec requires the reader address. This causes `ground()` guards on the receiving agent to fail definitively instead of suspending.

### Root Cause

`localize()` takes a `freshAddrAllocator: int Function()` callback that returns only the writer address. The caller discards the reader address:

```dart
freshAddrAllocator: () {
  final (w, _) = runtime.heap.allocateVariable();  // allocates pair (writerN, readerN+1)
  return w;                                          // discards reader address
},
```

Inside `localize()`:

```dart
final writerAddr = freshAddrAllocator();
final readerAddr = writerAddr;  // WRONG: should be the actual reader address
```

When localizing `_w(p, i)` (incoming writer from remote agent), the spec says to replace it with `Y_q?` (the reader). But because `readerAddr == writerAddr`, the code substitutes `VarRef(writerAddr)` — a writer, not a reader.

### Consequence

On the receiving agent, the term contains a VarRef pointing to a writer cell. When `ground()` traverses the term and finds this unbound writer, it takes the "unbound writer → definitive failure" path (correct for single-agent SRSW, wrong here). The goal fails instead of suspending on the reader and waking when the remote assignment arrives.

### Observable Effect

In `three_agent_pipeline_boot.glp`, agent3's `consumer_init` receives a partially-bound list like `[got(1), got(2) | X2]` where X2 is a localized variable. Because X2 is a writer (should be reader), `ground(Ys?)` fails and the goal terminates instead of suspending until the rest of the list arrives. The test passes as a **false positive** because a failed goal reports agent completion (zero remaining goals).

### Fix Status

**Fixed**: Changed `freshAddrAllocator` signature from `int Function()` to `(int, int) Function()`, returning both `(writerAddr, readerAddr)`. Updated `localize()` and all 4 callers in `mad_context.dart`.

Note: this fix alone does NOT resolve the pipeline test failure — the root cause is in globalise/send (see Issue 2 and `docs/bug-send-globalise-localise.md`).

### Broader Concern: N+1 Arithmetic

The heap-pointer architecture spec states that writer and reader cells point to each other via cross-pointers, so address arithmetic (`writerAddr + 1`) should never be needed. However, `pairedReaderAddr()` in `heap_fcp.dart` has a fallback `return writerAddr + 1`. An audit should verify that no code depends on the N/N+1 allocation convention — all navigation between paired cells should use the cross-pointers.

### Files Involved

- `glp_runtime/lib/multiagent/mad_helpers.dart` — `localize()` function (lines 212-255)
- `glp_runtime/lib/multiagent/mad_context.dart` — all `freshAddrAllocator` callbacks
- `glp_runtime/lib/runtime/heap_fcp.dart` — `allocateVariable()`, `pairedReaderAddr()` fallback
- `programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp` — test that exercises the bug

### Test

After fixing all issues, `three_agent_pipeline_boot.glp` should show agent3's `consumer_init` suspending on `ground(Ys?)`, then waking when the full list `[got(1), got(2), got(3)]` arrives, then completing via `wrap` and `consume`.

---

## Issue 2: TermVar.pairedReaderAddr returns wrong address

**Status**: Open
**Discovered**: 2026-02-10
**Affects**: All multi-agent programs that send terms containing writers
**See also**: `docs/bug-send-globalise-localise.md`

### Summary

`TermVar.pairedReaderAddr` (line 98 of `mad_helpers.dart`) returns `addr` (the writer address itself) instead of the actual paired reader address from the heap. This causes `globalize()` to create `GlobalSendSpawn` entries with the wrong `readerAddr`, so the `onBind` callback and `GlobalSendRegistry` goal are registered on the writer address instead of the reader address.

### Code

```dart
// mad_helpers.dart line 98
int get pairedReaderAddr => addr;  // BUG: should look up from heap
```

Used at line 183:
```dart
spawns.add(GlobalSendSpawn(
  readerAddr: v.pairedReaderAddr,  // gets writerAddr instead of readerAddr
  ...
));
```

### Fix

`TermVar` needs access to the heap to look up the paired reader via cross-pointer. Either pass the heap to `globalize()` or resolve the paired address in `_extractTermVarsRecursive` where the heap is available.

---

## Issue 3: No local-write-back mechanism for localized _w variables

**Status**: Open
**Discovered**: 2026-02-10
**Affects**: Multi-agent programs where the receiver writes back on a sent writer (response channel)
**See also**: `docs/bug-send-globalise-localise.md`, `writer_response_boot.glp`

### Summary

When agent q localizes `_w(p, i)`, it creates a fresh pair `(Y_q, Y_q?)` and a `LocalizeEntry(Y_q, p, i)`. The reader `Y_q?` goes into the term. If agent q later binds the writer `Y_q` (e.g., `bind_done(done)` unifies the writer with `done`), there is no mechanism to detect this and send `_w(p, i) := done` back to agent p.

### Root Cause

The `LocalizeEntry` in the `GlobalWritersTable` is designed for the **incoming** direction: when agent p sends `_w(p, i) := T` to agent q, `_handleWriterAssignment` finds the entry and binds `Y_q`. But in the writer_response pattern, agent q is the one that writes, and the value needs to flow **out** to agent p.

No `onBind` callback is registered on the fresh writer created by localize-`_w`. The comment in `mad_context.dart` (line 264-267) says:

```dart
// Note: For _w(p, i) localizations, we do NOT set up callbacks.
// Per spec Section 5.2: "No goal is spawned—q will receive the assignment on this link"
```

This is correct for the case where p sends a value to q. But when q writes on the localized variable and the value needs to go back to p, there must be an `onBind` callback that creates and sends the assignment message.

### Observable Effect

In `writer_response_boot.glp`, agent2 binds its localized writer to `done`, agent2 completes, but agent1 remains suspended forever on `wait_response(Resp?)` because the value `done` never arrives.

### Fix Plan

When localize creates a fresh pair for `_w(p, i)`, register an `onBind` callback on the fresh writer. When fired, it should:
1. Globalize the value (handling any nested variables)
2. Create a message `_w(p, i) := T↑`
3. Queue it for delivery to agent p

This is essentially a `global_send` goal, but triggered from the localize-`_w` path rather than the globalize-writer path.

### Files Involved

- `glp_runtime/lib/multiagent/mad_context.dart` — `_handleSerializerAssignment` (no `onBind` for localize-`_w` fresh pairs)
- `glp_runtime/lib/multiagent/global_writers_table.dart` — `LocalizeEntry` structure
- `programs/typed_book/multiagent_tests/writer_response_boot.glp` — test that exercises the bug
