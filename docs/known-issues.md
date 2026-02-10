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

**Status**: Fixed
**Discovered**: 2026-02-10
**Affects**: All multi-agent programs that send terms containing writers
**See also**: `docs/bug-send-globalise-localise.md`

### Summary

`TermVar.pairedReaderAddr` returned `addr` (the writer address itself) instead of the actual paired reader address from the heap. `TermVar` only stored a single address, with no way to look up the paired address.

### Fix

Redesigned `TermVar` to carry both `writerAddr` and `readerAddr` fields, populated by `_extractTermVarsRecursive()` using the heap's cross-pointer methods (`tryWriterForReader`, `pairedReaderAddr`). All call sites updated.

---

## Issue 3: No local-write-back mechanism for localized _w variables

**Status**: Fixed
**Discovered**: 2026-02-10
**Affects**: Multi-agent programs where the receiver writes back on a sent writer (response channel)
**See also**: `docs/bug-send-globalise-localise.md`, `writer_response_boot.glp`

### Summary

When agent q localizes `_w(p, i)`, it creates a fresh pair `(Y_q, Y_q?)` and a `LocalizeEntry(Y_q, p, i)`. The reader `Y_q?` goes into the term. If agent q later binds the writer `Y_q`, there was no mechanism to send `_w(p, i) := done` back to agent p.

### Fix

Added `_registerWriteBackCallbacks()` and `_sendWriteBack()` methods to `MadContext`. When `localize()` processes `_w(p, i)`, an `onBind` callback is registered on the fresh writer. When fired, it globalizes the value and queues `_w(p, i) := T↑` for delivery to agent p. Registered in all four localize call sites.

---

## Issue 4: Type checker rejects well-typed `=` with reader argument

**Status**: Open
**Discovered**: 2026-02-10
**Affects**: Any typed program using `=` (unification) with a reader variable

### Summary

The type checker rejects the following well-typed clause:

```prolog
procedure bind_later(_).
bind_later(Done?) :- wait(1000) | done(Done).
```

Error: "Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)" for `Done` at the `=` call site (or equivalent body atom).

### Analysis

The prelude declares `=` as:

```prolog
procedure =(_?, _).
X = X?.
```

Position 0 is `_?` (reader), position 1 is `_` (writer). In the clause `bind_later(Done?)`, `Done` is the reader of the writer passed by the caller. Using `Done` as the first argument of `=` (the `_?` position) should be well-typed since `Done` is already a reader. The type checker incorrectly rejects this.

### Workaround

Use `done(Done)` instead of `Done = done` to avoid `=` entirely.

### Files Involved

- `glp_runtime/lib/analysis/type_checker/` — type checker implementation

---

## Issue 5: localize() spawn uses reader address; onBind needs writer address

**Status**: Fixed
**Discovered**: 2026-02-10
**Affects**: Multi-agent programs where a localized `_r(p, i)` should trigger a `global_send` back to agent p

### Summary

In `localize()`, processing `_r(p, i)` creates a `GlobalSendSpawn` with `readerAddr: readerAddr`. But `registerGlobalSendSpawns()` passes `spawn.readerAddr` to `heap.onBind()`, which is indexed by **writer** address. The callback never fires because the reader address is not a valid key for `onBind`.

### Fix

Changed `localize()` to pass `writerAddr` in the spawn's `readerAddr` field. The field name is misleading (it is actually the `onBind` key), but the semantics are now correct.

---

## Issue 6: globalize-reader path has no onBind callback; entry stores reader address

**Status**: Fixed
**Discovered**: 2026-02-10
**Affects**: Multi-agent programs where agent p sends a reader `X?` and later binds the paired writer

### Summary

Two bugs in the globalize-reader path:

1. `globalize()` passed `v.addr` (the reader address) to `addGlobalizeEntry()`, which stores it as `writerAddr`. But `_handleReaderAssignment` later calls `bindVariable(entry.writerAddr, ...)` — passing a reader address to `bindVariable` is incorrect.

2. No `onBind` callback was registered for globalize-reader entries. When agent p globalizes reader `X?` as `_r(p, i)` and later binds the paired writer, nothing detected this and sent `_r(p, i) := T↑` to agent q.

### Fix

1. Changed `globalize()` to pass `v.writerAddr` (the actual writer) to `addGlobalizeEntry()`.

2. Added `onBind` registration in `MadContext.send()` for globalize-reader entries. When the paired writer is bound, `_sendWriteBack()` globalizes the value and queues the assignment message.

### Test

`send_reader_boot.glp` exercises this path: agent1 sends `data(X?)` where `X?` is an unbound reader, then binds the writer to `done` after 1000ms. Agent2 should receive `done` via the globalize-reader → onBind → `_sendWriteBack` path.
