# GLP Known Issues

**Last updated:** 2026-06-21

## Issue 0a: Parser does not support `=..` as a goal in clause bodies

**Status:** Fixed (the parser has supported body `=..` since `e803730f`, 2026-01-17; verified and regression-tested 2026-06-10).

Original (pre-fix) symptom:

```glp
%% Reported as FAILING (no longer):
compose(List, Tuple) :- Tuple? =.. List?.
%% Error: "Expected predicate name or comparison" at =..

%% Always worked (in clause head):
X? =.. [Y|Ys] :- list(Ys?) | list_to_tuple([Y|Ys], X).
```

**Resolution:** `=..` as a body goal is specified by the language docs (`docs/glp-predicate-taxonomy.md` "Usage": `Term =.. [Functor | Args]` in a body; `docs/guards-reference.md`: `X? =.. [F|Args], ...` in a body). The parser's body-goal path (`lib/compiler/parser.dart` `_parseGoalOrGuard`, reader/variable + `TokenType.UNIV` case) handles it for reader-, writer-, and compound-led forms. Verified: `comp(L, T?) :- list(L?) | T =.. L?.` with `comp([foo, a, b], T).` yields `T = foo(a, b)`. Regression test: `programs/tests/typed/univ_body.glp` + `test/run_all_tests.sh` Section A30.

**Aside (separate, out of scope):** the dual operator `..=` is declared in `programs/self.glp` (`procedure ..=(Stream(_), _?).`) but has no clause, so `List ..= Compound?` parses and type-checks yet fails at runtime with "Spawn could not find procedure label: ..=/2". Not part of this issue; recorded as Issue 10.

## Issue 0b: REPL cannot parse compound terms inside lists in goal arguments

**Status:** Fixed (resolved by the GlpEngine refactor `997eed0e`, 2026-02-01; verified and regression-tested 2026-06-10).
**Resolution:** The goal-term builders moved into `glp_runtime/lib/engine/glp_engine.dart` (`_buildListTerm` and `_buildListTermForConj`), where the list-head dispatch now includes a `StructTerm` case (→ `_buildStructTerm` / `_buildStructTermForConj`). The original pointer to `bin/glp_repl.dart` is stale. Verified: `distribute_indexed([send(1,a), send(2,b), send(1,c), send(2,d)], Y, Z).` yields `Y = [a, c]`, `Z = [b, d]`. Regression test: `test/run_all_tests.sh` Section A29.

```glp
%% This FAILS in REPL goal:
distribute_indexed([send(1,a), send(2,b)], Y, Z).
%% Error: Exception: Unsupported list head type: StructTerm

%% This WORKS:
distribute_indexed([], Y, Z).
```

What works: simple lists `[a,b,c]`, nested lists `[[a,b], [1,2]]`, variables `[X?, Y?]`. What fails: structs in lists `[send(1,a), foo(x)]`.

Impact: can't test predicates that take lists of structures from REPL goals.  Workaround: test from within a program clause.

---

## Issue 1: Localize uses writer address where reader address is needed

**Status**: Fixed — functional defect resolved (verified by trace 2026-06-10); see Investigation Result. The N+1 audit it requested found latent reliance on the allocation convention, recorded separately as Issue 9.
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

### Investigation Result (2026-06-10): functional defect resolved

Ran `three_agent_pipeline_boot.glp` via `multiagent_glp_test.dart` with `traceGlp`+`traceMad`. agent3 exhibits exactly the correct behavior the Test section requires — it **suspends** (does not fail) while the list tail is unbound, and **wakes** as each piece arrives:

```
[agent3] consumer_init(agent3, X1?) → suspended
[agent3] consumer_init(agent3, [msg(agent3, data([got(1), got(2) | X2?])) | X3?]) → suspended   % ground(Ys?) suspends
[agent3] consumer_init(agent3, [msg(agent3, data([got(1), got(2) | [got(3) | X5?]])) | X3?]) → suspended
[agent3] consumer_init(agent3, [... data([got(1), got(2) | [got(3) | []]]) ...]) :- wrap(...), consume(...)   % wakes when ground
[agent3] wrap([got(1), got(2), got(3)], done([...])) :- true
[agent3] consume(done([got(1), got(2), got(3)])) :- true
```

The localized tail variables behave as readers (suspend `ground`, wake on binding), not as definitively-failing unbound writers. The defect described in Summary/Consequence is gone, closed by Issue 1's partial fix together with Issues 2, 5, 6. No definitive failure.

### N+1 Audit Result (2026-06-10)

Performed; latent reliance found. Findings and the deferred core fix are recorded as Issue 9.

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

## Issue 3: Spurious write-back mechanism for localized _w variables

**Status**: Removed
**Discovered**: 2026-02-10
**Affects**: N/A (the mechanism was incorrect and has been removed)

### Summary

A write-back mechanism (`_registerWriteBackCallbacks`, `_sendWriteBack`) was added to handle the case where agent q localizes `_w(p, i)`, creates a fresh pair `(Y_q, Y_q?)`, and then binds Y_q locally. The write-back sent `_w(p, i) := T` back to agent p.

### Why It Was Wrong

This mechanism does not exist in GLP. The data flow for `_w(p, i)` is strictly p→q: p assigns the writer, the `global_send` goal at p fires, and the value is delivered to q's entry. There is no reverse flow. If a program needs q→p flow (the receiver writes back), the sender must export the reader, producing `_r(p, i)`, and the `global_send` spawned at q by `localize` handles the outgoing direction.

### Resolution

Removed `_registerWriteBackCallbacks()`, `_sendWriteBack()`, and all call sites from `mad_context.dart`. Test programs that relied on this mechanism need to use the correct polarity (export reader for q→p flow).

---

## Issue 4: Type checker rejects well-typed `=` with reader argument

**Status**: Not a bug — the type checker is correct (verified 2026-06-10). The clause in the report is genuinely ill-moded; see Resolution. (Title is a misnomer: `=` with a *reader* at arg0 type-checks fine; what is rejected is a *writer* at arg0.)
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

The root self.glp declares `=` as:

```prolog
procedure =(_?, _).
X = X?.
```

Position 0 is `_?` (reader), position 1 is `_` (writer). In the clause `bind_later(Done?)`, `Done` is the reader of the writer passed by the caller. Using `Done` as the first argument of `=` (the `_?` position) should be well-typed since `Done` is already a reader. The type checker incorrectly rejects this.

### Workaround

Use `done(Done)` instead of `Done = done` to avoid `=` entirely.

### Files Involved

- `glp_runtime/lib/analysis/type_checker/` — type checker implementation

### Resolution (2026-06-10): the checker is correct; do not change it

The Analysis above is mistaken about the mode of `Done` in the body. Apply the manual's rules (typed-glp-manual §2A; §11 variable-flow table):

- `procedure bind_later(_)` — arg0 has no `?`, so it is **↑ produce** (an output the procedure fills).
- In the head `bind_later(Done?)`, a ↑-produce position takes a **reader** hole — hence `Done?` (reader) in the head is correct (§2A: ↑ → reader).
- By SRSW, the paired occurrence in the body is the **writer** `Done` — this is the §11 case "output constructed by body: head reader `X?`, body writer `X`". So in the body, `Done` is a **writer**, not a reader (the Analysis's claim that it is "the reader of the writer passed by the caller" is wrong — the caller passes a writer because arg0 is an output).
- `=` is declared `=(_?, _)` (clause `X? = X.`): arg0 is **↓ consume** (a reader). Placing the body writer `Done` at `=`'s arg0 puts a writer at a consume position → "writer requires ↑ (produce), got ↓ (consume)". The rejection is correct.

Empirical confirmation (2026-06-10):

- `Done = done` in `bind_later(Done?) :- wait(1000) | Done = done.` → rejected (writer at arg0). Correct.
- `In? = Out` in `copy_val(In, Out?) :- In? = Out.` → type-checks (a **reader** at arg0 is accepted). So the checker does not reject "`=` with a reader argument" — it rejects a writer there.
- The report's own workaround (produce via a helper head: `result(done).` then `... | result(Out)`) type-checks and runs (`bind_helper(Y)` → `Y = done`). This is the GLP-correct idiom: construct outputs in a head, do not bind a writer to a constant via `=` in the body (manual §6, cheat-sheet §1 / §10).

No type-checker change. (Unrelated note: a runtime `=`-aliasing quirk — `copy_val(hello, X)` type-checks but yields `X = <unbound>` — is a separate matter from this type-checking issue and from the `=` clause-ordering note in MEMORY; not addressed here.)

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

## Issue 6: globalize-reader entry stores reader address instead of writer address

**Status**: Fixed (part 1); part 2 removed
**Discovered**: 2026-02-10
**Affects**: Multi-agent programs where agent p globalizes a reader `X?` as `_r(p, i)`

### Summary

`globalize()` passed `v.addr` (the reader address) to `addGlobalizeEntry()`, which stores it as `writerAddr`. But `_handleReaderAssignment` later calls `bindVariable(entry.writerAddr, ...)` — passing a reader address to `bindVariable` is incorrect.

### Fix

Changed `globalize()` to pass `v.writerAddr` (the actual writer) to `addGlobalizeEntry()`.

### Note on onBind

A previous fix also added an onBind callback in `send()` for globalize-reader entries, using `_sendWriteBack`. This was incorrect — for `_r(p, i)`, agent p creates an entry and WAITS. The `global_send` is spawned at q by `localize`, not at p. Agent p does not send anything for `_r` entries. The onBind and write-back have been removed.

---

## Issue 7: Receive drops early messages; hold mechanism required

**Status**: Fixed (2026-06-10, commit ae327fe8). Unit test: `mad_transactions_test.dart` ("early _r assignment is held, then delivered when localize creates the entry"). Integration test: `test/multiagent/reverse_order_delivery_test.dart` (reverse-order cold-call reaches the same outcome).
**Discovered**: 2026-06-10
**Affects**: madGLP over any channel that does not deliver per-pair FIFO (BLE/IP, multi-media routing). Masked in-process: Dart isolate ports happen to deliver in order.

### Summary

madGLP-spec v5.6 (Section 8.3, Early Messages) requires: a message `_r(p, i) := T↑` arriving before its entry `(X_q, p, i)` exists is held and processed when localization creates the entry. The code instead processes every message on arrival: `_handleReaderAssignment` in `mad_context.dart` throws `StateError('No LocalizeEntry...')` when the entry is absent, the isolate loop catches and prints it, and the message is dropped.

### Why it has not bitten

The entry for `_r(p, i)` is created when localizing the earlier p→q message that carried `_r(p, i)`. Dart isolate ports deliver per-pair in order, so in-process the carrier always arrives first. Real transports give no such guarantee, and the GLP-Networking-API paper states Unordered delivery: the layer need not provide ordering.

### Fix

In `mad_context.dart`: when `_handleReaderAssignment` finds no entry, store the assignment in a hold table keyed by `(remoteAgent, remoteIndex)`. When `localize()` creates a LocalizeEntry, check the hold table and deliver any held assignment for that key. Only the `_r` case needs holding: `_w(p, i)` entries exist before the global name leaves the agent, and the serializer entry at index 0 is permanent.

### Test

A harness that delivers two messages of a pair in reverse order (the `_r(p, i) := T` assignment before its carrier) and verifies the run completes with the same outcome.

### Related Check

Verify the plays do not rely on per-sender cold-call order. Established channels are unaffected — each message carries its continuation, so they are dataflow-ordered under any delivery order — but two cold-calls from the same sender may now arrive in either order.

**Inspection result (2026-06-10):** All test-exercised plays were inspected (cssn_modules_v2 3-adult and 6-agent village; bonds_v2 actors p2–p11 and play12; social_graph typed_actors / typed_ui_actors). No play relies on per-sender cold-call arrival order: in each play every sender's cold-calls go to *distinct* recipients (a recipient may receive cold-calls from several *different* senders, which were always unordered). The only repeated `connect(X)` occurrences are comments or alternative committed-choice clauses (one fires). No agent issues two order-dependent cold-calls to the same recipient. Plays pass over the reverse-capable simulation router.

---

## Issue 8: Live app path not on the networking seam

**Status**: Fixed (2026-06-10, commit `8e951bf0`). See Resolution.
**Discovered**: 2026-06-10
**Affects**: The Flutter `glp_multiagent` app and `agent_runtime.dart`

### Summary

The networking seam (seam spec v0.2/v0.3) was implemented for the isolate test
stack: `isolate_manager.dart` routes through `SimulationNetwork`/`SimulationRouter`
behind the `GlpNetwork` interface. Two parts of the live app path were left on
the old transport:

1. **`agent_runtime.dart`** (the Flutter-app runtime) still uses its
   `onMessageReady` → `onSendMadMessage` / `onMadMessageReceived` path, not a
   `GlpNetwork`.
2. **Connectivity callbacks** (`onPeerConnected`, `onPeerDisconnected`,
   `onPeerDiscovered`) are not forwarded cross-isolate to agents in the live
   stack. They fire and are tested at the router level in-process; no GLP play
   currently consumes them.

### Why deferred

Blind migration of an untested live path is the wrong fix. `agent_runtime.dart`
drives the Flutter app, which is not covered by `dart test`; migrating it without
the ability to manually verify the app would risk silent breakage. Connectivity
callbacks have no consumer in any current play.

### Fix

Migrate `agent_runtime.dart` to construct a `GlpNetwork` (a `SimulationNetworkClient`
or the real BLE/IP layer) and route outgoing/incoming traffic through it, and add
router→client connectivity-event forwarding. Verification by Claude Code (manual
app check waived by Udi, 2026-06-10): headless tests driving `AgentRuntime`'s
message path written first and kept green across the migration, plus
`flutter analyze` and a `flutter build` if the SDK is available. Residual
UI-wiring risk accepted. Until merged, the isolate test stack is the seam's
reference path.

### Resolution (2026-06-10, commit `8e951bf0`)

1. **`agent_runtime.dart` migrated to `GlpNetwork`** (mirrors `isolate_manager.dart`).
   It constructs a `SimulationNetworkClient` whose `sendToRouter` forwards to the
   existing `onSendMadMessage(to, payload)` callback; outgoing is
   `ctx.onMessageReady(destId, msg) → network.send(directory.pkOf(destId), msg.payload)`;
   incoming is `onMadMessageReceived(from, payload) → network.onMessageReceived →
   deserialize → handleMadAssignment`. `MessageType` is off the wire (the legacy
   `agentMessage` branch removed). `ctx.network` is set, so the `sign/2` and
   `verify_attestation/4` kernels (§4) work on the app path too. The external
   (to/from + opaque payload) contract is unchanged, so `isolate_protocol.dart`
   needs no change. Routing keys are the directory's; when no directory is
   supplied a deterministic id→key is derived (`_pkFor`), real keys go via the
   keyPair for signing.
2. **Connectivity forwarding**: `AgentRuntime` exposes `onPeerConnected`/
   `onPeerDisconnected`/`onPeerDiscovered` and an `onConnectivityEvent(pk, t,
   event)` entry the coordinator calls to forward router events to the client.

**Verification (Claude Code; manual app check waived):**
- Headless characterization test `test/multiagent/agent_runtime_test.dart`
  ("a cold-calls b; b receives and surfaces the value") written first and kept
  **green across the migration**; plus a connectivity-forwarding test.
- `dart test` (full): `+378 ~5 -0`.
- `flutter analyze` (glp_multiagent): clean except 2 pre-existing issues
  (`mad_router.dart` unnecessary import, `main.dart` unused element).
- `flutter build macos`: success (built `glp_multiagent.app`, 40.2 MB).

**Note on the REPL gate:** at commit time the REPL suite was red (490/511) due
to another session's *uncommitted* `-expose` compiler work (`ast.dart`,
`parser.dart`, `project_linker.dart`; build `8c14e233`) — all 21 failures are
module-system/project-load tests in that domain. Task A's REPL was 511/511
before those changes; this migration touches only `agent_runtime.dart`, not the
REPL path. Push is held until the combined tree is REPL-green.

---

## Issue 9: Latent reliance on the reader = writer+1 allocation convention

**Status**: Fixed (2026-06-17) — `pairedReaderAddr` now uses an allocation-time writer→reader index; no `+1` arithmetic remains. `run_all_tests.sh` 485/485 with the fix; `dart test` +372 with 5 pre-existing failures in the other session's madGLP UI-mediator WIP (unrelated).
**Discovered**: 2026-06-10 (audit requested by Issue 1)
**Affects**: Latent only — no active failure. Any change to allocation (interleaved or relocating allocation, GC compaction) would turn the residual into one.

### Findings (updated 2026-06-17 — earlier list was stale)

The bulk of the reliance is **gone**: all the direct `writerAddr + 1` reader derivations formerly in `lib/bytecode/runner.dart` (the cited lines 2346/2574/2580/2716 no longer exist) have been replaced with `readerForWriter()` ("Per spec v3.2: use readerForWriter() instead of +1 arithmetic"). The original Findings list above those lines was stale.

**One residual reliance remains**: `heap_fcp.dart` `pairedReaderAddr()` — fallback `return writerAddr + 1` (line ~242). It is reached when `readerForWriter()` returns null, i.e. for a **bound** writer: a writer cell has a single content slot, and binding overwrites the pointer-to-reader with the bound value, so the cross-pointer is destroyed and there is no pointer way to recover the reader. Still live — called by `glp_engine.dart` (query-variable binding / answer extraction, ~8 sites) and `mad_context.dart`. Correct today because `allocateVariable()` always allocates `(HP, HP+1)`.

### Why it is still here

Not necessity of the arithmetic — risk/benefit. Removing it requires a core cell-design change (widen the writer cell to keep a reader field that survives binding, or thread the reader address through callers). It is correct under the current contiguous allocator and there is no active bug, so the change has been deferred. The cost of keeping it is that it silently encodes the very assumption `terms.dart` forbids ("MUST NOT assume reader_addr == writer_addr + 1") — a landmine for any future GC compaction / relocating allocator.

### Fail-loud probe result (2026-06-17) — the fallback is LOAD-BEARING

Replaced the `return writerAddr + 1` with a `throw` and ran the full suite. Result: **65 failures** across core programs (merge, reverse, quicksort, fibonacci, inner product) and the CSSN plays — ordinary code routinely asks for the reader of a *bound* writer. (No `StateError` text surfaced: the engine catches the throw and turns it into silent goal failure, which is itself worth noting.) So the fallback is not dead and cannot be deleted; option 1 below ("delete if dead") is ruled out. Reverted to the `+1` fallback; suite green again.

### Fix applied (2026-06-17)

Took the "retain the reader through binding" route, implemented as a side index rather than a wider cell: `HeapFCP._readerForWriterIndex` maps `writerAddr -> readerAddr`, populated in `allocateVariable()`. It survives binding (the cell's pointer does not), so `pairedReaderAddr` returns the recorded reader with no arithmetic. Falls back to the bidirectional pointer for any writer not in the index (still unbound), and throws rather than guessing if neither yields a reader. `run_all_tests.sh` stays 485/485 with zero throws. The index grows with allocation like `cells` (no GC today); a future relocating/compacting GC must update it — noted at the field.

This closes the reader/writer-address-confusion family root for the single-isolate heap: Issue 12 (guard deref), Issue 1 (localize, already fixed), and this all stemmed from deriving reader/writer identity from a bare address; reader recovery is now an explicit recorded link, not arithmetic.

---

## Issue 10: `..=/2` is only half-wired in the toolchain (not just a missing clause)

**Status**: Parked (2026-06-17) — bigger than a missing clause; needs compiler work. Re-scoped after an attempt.
**Discovered**: 2026-06-10 (while testing Issue 0a); re-scoped 2026-06-17
**Affects**: Any program using the decomposition operator `..=`

### Summary

`..=` is declared in `programs/self.glp` (`procedure ..=(Stream(_), _?).`) but is not actually usable: `List ..= Compound?` fails at runtime with "Spawn could not find procedure label: ..=/2". Its dual `=..` works.

### Re-scope (2026-06-17): not a one-line clause

An attempt to add the obvious self.glp clause — `Y ..= X? :- compound(X?) | '_tuple_to_list'(X?, Y).`, the exact mirror of the `=..` decompose clause — did **not** work: the runtime still reported "Spawn could not find procedure label: ..=/2", i.e. the clause does not compile into a callable procedure. Root cause not pinned. Reverted. Along the way, two more gaps surfaced showing `..=` was scaffolded but never finished:

- **Parser**: compound-led `..=` (`foo(a,b) ..= L`) has no `UNIV_DECOMPOSE` branch where compound-led `=..` is handled (`parser.dart` `_parseGoal` ~888, `_parseAtom` ~792). Only variable-led `..=` parses.
- **Printer**: `glp_printer.dart` `_isInfixOperator` lists `=..` but omits `..=`.

So completing `..=` is compiler work: pin why a `..=/2` clause does not register to a spawn label, add compound-led parsing, fix the printer, then add a Section A regression test. First trace how `=..` body goals compile to a resolvable spawn target and mirror it for `..=`.

---

## Issue 11: Fast REPL binary exists but goes stale and isn't rebuilt

**Status**: Fixed (2026-06-17) — option 1 implemented as `bin/glpc`
**Discovered**: 2026-06-17
**Affects**: Developer and agent workflow — iterating on `.glp` files

### Summary

A fast path already exists: commit `015beb13` (Dec 2025) added an AOT-compiled REPL binary `glp_runtime/glp_repl` (`dart compile exe`). Measured startup: **0.01 s** for `./glp_repl` vs **0.77 s** for `dart run bin/glp_repl.dart` with a warm `.dill` (seconds when the `.dill` is cold — which it is after every `lib/` edit, since the protocol requires `rm .dart_tool/repl.dill`).

The fast path effectively regressed, but by **staleness, not code**:
1. The committed `glp_repl` binary is frozen at its last manual build (Jan 28 2026); months of `lib/` changes are not in it. Running it now tests **old code**, so it cannot be used for current checks.
2. There is **no rebuild step** — the binary is committed once and never refreshed, so it silently drifts out of sync. The working instruction therefore falls back to the always-current but slow `dart run bin/glp_repl.dart`.

So the canonical workflow pays full `dart run` startup on every one-file typecheck / one-goal run, and the suite (`run_all_tests.sh`, which also invokes `dart test`) costs minutes.

### Fix (options, pick per cost/benefit)

1. **Rebuild-on-staleness**: a tiny wrapper/script that rebuilds `glp_repl` via `dart compile exe` when any `lib/` or `programs/self.glp` source is newer than the binary, then runs it. Keeps the 0.01 s path always current. Likely stop committing the 7.5 MB binary — build on demand, gitignore it.
2. **Persistent/daemon REPL** kept warm across checks (load root `self.glp` once, accept successive load/goal commands over a socket or stdin loop), so startup is paid once per session.
3. **Minimal non-REPL check CLI** — a thin wrapper over `checkSource`/the engine for the common "does this file typecheck / run" case, built fresh each time.

Whichever is chosen, keep `dart run` REPL as the canonical full pipeline; the fast path is an addition that must (a) stay current with `lib/` and (b) give identical typecheck/runtime verdicts.

### Resolution (2026-06-17)

Implemented option 1 as `glp_runtime/bin/glpc`: runs an AOT-compiled `bin/glp_repl_exe`, rebuilding it via `dart compile exe` whenever any `lib/` or `bin/` Dart source is newer than the binary. `self.glp` is read at runtime, so `.glp` edits need no rebuild. Startup ~0.33 s (incl. the staleness scan) vs ~0.77 s warm `dart run`. The 7.5 MB binary is gitignored and built on demand; the old committed `glp_repl` (stale Jan 28) is superseded. `dart run bin/glp_repl.dart` remains the canonical full pipeline; `glpc` gives identical verdicts. Options 2 (daemon) and 3 (non-REPL check CLI) remain available if more speed is needed, especially to accelerate `run_all_tests.sh` (still on `dart run` per-invocation).

---

## Issue 12: Guard dereference confuses heap address with clause-variable index (fail instead of suspend)

**Status**: Fixed (2026-06-17) — verified by re-baseline 483/483 + regression test A19b
**Discovered**: 2026-06-17 (bounded-buffer, consumer-first goal order)
**Affects**: Every guard evaluated via the generic guard path — `integer`, `number`, `string`, `constant`, `ground`, `known`, `unknown`, `compound`, `list`, `module`, the comparisons (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`, `@<`), and `=?=`. Intermittent: fires only on a heap-address ↔ clause-index numeric collision.

### Summary

`integer(X1?)` on an unbound reader returned **fail** instead of **suspend**, so `consumer` died on its guard unless the slot was already filled. Goal order then decided the outcome: producer-first filled the slots before the guard ran (worked); consumer-first hit the guard on empty slots (failed instead of waiting). Two failure modes: (1) fail-instead-of-suspend (observed), and (2) the more dangerous silent wrong verdict, when the swapped-in variable is bound to a different value.

### Root Cause

`_dereferenceWithTracking` (`lib/bytecode/runner.dart`) began with a shortcut:

```dart
if (t is VarRef && cx.clauseVars.containsKey(t.addr)) {
  final resolved = cx.clauseVars[t.addr]; ...
}
```

`VarRef.addr` is a **heap address**; `clauseVars` is keyed by **clause-variable index**. When a heap address numerically equals a live clause-variable index, the shortcut silently swaps a guard argument for whatever clause var shares that number — e.g. `X1`'s reader for the unbound writer **tail** `Xs`. The guard then tests the tail (an unbound writer ⇒ fail), not the reader (unbound reader ⇒ suspend).

This is a mechanical-migration error: commit `57cf5d96` ("pointer architecture migration") renamed `t.varId` → `t.addr` here. `varId` was the clause index (correct key); `addr` is the heap address (wrong key). `varId` was removed from `VarRef` (`terms.dart` §3.2.1), so a `VarRef` never carries a clause index — the shortcut is both wrong and unnecessary.

### Why undetected

Foundational in location, narrow in trigger. Collisions need a small goal on a fresh heap (low addresses overlapping low clause indices); real programs have grown heap addresses, run guards on already-bound data, and use producer-first ordering — all of which dodge it. The suite shares those habits, so it was 483/483 green. The appendix's consumer-first, freshly-loaded, tiny bounded-buffer goal is almost the only shape that reliably lands on it.

### Fix

Removed the `clauseVars` shortcut from `_dereferenceWithTracking`. Dereference now resolves the actual heap cell, so an unbound reader is tracked and the guard suspends.

### Test

`test/run_all_tests.sh` Section **A19b**: bounded-buffer consumer-first and producer-first both suspend (consumer-first failed before the fix). Re-baseline 483/483 → 485/485 green.

### Family

Same theme as **Issue 1** (localize put a writer address where a reader was needed — fixed), **Issue 9** (reader recovery by `+1` arithmetic — fixed via the allocation-time index), and a sibling of **Issue 13** (madGLP `receive/3 _w`-dereference — open): all are the post-migration confusion between a variable's heap address and its reader/writer role. Distinct site and fix from each — this one is a *wrong* lookup removed; receive/3 is a *missing* `_w` indirection to add; Issue 9 was reader recovery without a stored link.

---

## Issue 13: madGLP `receive/3` drops a nested reader pattern over a `_w` remote writer

**Status (RESOLVED — phantom, 2026-06-22)**: The premise is disproven. `receive/3` does **not** mishandle a `_w` writer: `programs/tests/mad_w_clean.glp` (= `mad_w_probe.glp` with the single change `bob_consumer(ch(S?, _))` → `bob_consumer(ch(S?, closed))`) makes the same cross-isolate `_w` **match through `receive`** (`bob_ch_matched`), two isolates (`glp_multiagent/test/mad_w_clean_test.dart`). The `mad_w_probe`/`probe13_strict` `otherwise` came entirely from the malformed channel `ch(S?, _)` — an anonymous `_` at the Out position (writer with no paired reader): `receive` is a **PE-unfolded defined guard**, and the partial evaluator statically reduces `receive(NestedReader, ch(S?, _), Cont)` to failure → compiled straight to `otherwise`. Compile-time, single-heap, no `_w` (reproduced by `programs/tests/recv2x2/`: `_` Out → otherwise, `closed` Out → matched). The two `isolate_manager_test` timeouts attached to this issue were a **separate** boot-config scope omission (agents crash at init with `UnknownTypeError: Response` because `tests/agent_roundtrip/self.glp` is not supplied) — now retired via skip (same class as the fixed roundtrip harness omission). No runtime/`receive/3`/`_w` defect exists. Follow-ups ticketed as Issues 18–19. Record corrected in `madglp-w-writer-return-bug.md` and `iglp-bug-reports-index.md` §2. The pre-resolution status is preserved below.

**Status (updated 2026-06-21)**: The **user-facing symptom is gone** — the cross-isolate befriend round-trip now **completes** (`glp_multiagent/test/roundtrip_isolate_test.dart`, two isolates, `connected` both sides). That test was only red because of an **unrelated test-harness omission**: it passed `glpSources` but not `glpSourcePaths`, so the loader could not do `self.glp` ancestor-scope discovery and every agent died at init with `UnknownTypeError: Response` (NOT a typechecker bug — the checker resolves `Response` whenever `self.glp` scope is established, as `single_heap_roundtrip_test` and a project-load both show). Supplying `glpSourcePaths: _sourceFiles` greens it and the `_w` return hop completes. **A residual soundness defect remains, now latent:** `receive/3` matching a nested reader against an *unbound* `_w` writer still falls to `otherwise` (`mad_w_probe` → `bob_ch_otherwise`), where the **control** `programs/tests/probe13_strict/` shows a plain unbound writer **commits** the same `receive`-as-guard match. The befriend flow works only because its response writer is *bound* by the time `receive` runs; a flow receiving an unbound `_w` writer would still strand. Original report: `docs/madglp-w-writer-return-bug.md`. Repro of the residual: `programs/tests/mad_w_probe.glp` + `glp_multiagent/test/mad_w_probe_test.dart`; control: `programs/tests/probe13_strict/`.
**Discovered**: 2026-06-17
**Affects**: Soundness (now latent) — `receive/3` mishandles an *unbound* `_w` writer (commits for a plain unbound writer, falls to `otherwise` for `_w`). No live caller currently hits the unbound case, so the befriend round-trip completes.

### Summary

A `_w`-backed reader is admissible in ordinary clause-head unification, survives reader→writer→reader forwarding through a list, and survives escrow across a suspension — but **fails only through the `receive/3` channel kernel**: matching a nested *reader* sub-pattern (`wrapped(Y?)`) against a channel message carrying a `_w` writer does not unify, and the clause falls through to `otherwise`. The probe (`mad_w_probe.glp`) isolates exactly this: three paths against the same `_w` match; the `receive/3` path falls to `otherwise`.

This is the mediator's return hop `receive(msg('_user', Id, decision(Dec, From, response(Resp?))), UserCh?, UserCh1)` — nested reader `response(Resp?)` over a `_w`-backed message — so the agent's `decision` clause never commits and the round-trip strands. Single-heap (local writer) completes; two-isolate (`_w`-backed) strands. Same clauses, only the transport differs.

### Family / relation to the fixed issues

Same root family as Issues 1, 9, 12 (heap-address vs reader/writer-role confusion from the pointer-architecture migration), but a **distinct** site and fix: not the `pairedReaderAddr` arithmetic (Issue 9, fixed) nor the guard-deref lookup (Issue 12, fixed) — it is a *missing* `_w` indirection inside `receive/3`'s unification of a nested reader sub-pattern. The Issue 9 index fix does not address it (the madGLP UI-mediator `dart test` cases for this remained failing with that fix in).

### Fix

Investigate `receive/3`'s dereferencing / cell-walk of a `_w` remote writer when binding sub-patterns; plain list head-matching of the same value succeeds, so the divergence is inside the `receive/3` kernel. The UI `.glp` is correct and must not change for this bug. Multi-isolate gate stays red until fixed.

---

## Issue 14: Type checker does not re-check reader/writer polarity at polymorphic instantiation

**Status**: Fixed (2026-06-17) — isolating repro `programs/tests/min_polarity_bug3.glp` is accepted before the fix and rejected after, with the polarity error in the offending clause; `run_all_tests.sh` 485→486 green; dart test unchanged (the 6 madGLP failures are the pre-existing open Issues 13/15, identical on a clean tree). Full report: `docs/typechecker-polymorphic-polarity-bug.md` (index: `docs/iglp-bug-reports-index.md` #1).
**Discovered**: 2026-06-16
**Affects**: Soundness — accepts programs that deadlock/strand at runtime.

### Summary

When a procedure parameter has a polymorphic element type (`Stream(X)?`, `Channel(X,Y)?`), reader/writer (producer/consumer) polarity obligations inside the body are discharged against the **abstract** type variable and are **not re-discharged** when `X` is instantiated to a concrete type at a call site. A concrete parameter type carries the obligation to the wiring site and the clash is caught; a polymorphic one absorbs it into the variable and it is never enforced.

### Repro (differential)

Identical except the consumer's declared parameter type. `producer` emits `befriend(Constant, Response)` (slot 2 = writer); consumer body matches `befriend(From, Resp?)` (slot 2 = reader); `go` wires them through one stream, so polarities must be dual.

- `programs/tests/min_polarity_bug.glp` — concrete consumer `Stream(ConsMsg)?` → correctly **errors** with `(S, S?) not dual`.
- `programs/tests/min_polarity_bug2.glp` — only change, polymorphic consumer `Stream(X)?` → **passes** (the duality error is silently dropped). The checker reports both errors in bug1, so this is genuine acceptance, not early-exit.

Real-world: `typed_ui_mediator.glp`'s `Channel(X,Y)` hides the agent↔mediator polarity clash → the befriend round-trip strands.

### Fix

When the per-instantiation clause-template check binds a parameter type variable to a concrete type, re-run the clause's **mode/polarity** discharge under that substitution, not only the structural/shape unification.

### Resolution (2026-06-17)

Two coupled gaps, both fixed:

1. **Call-site inference failed for the common reader case** (`well_typed_clause.dart` `_inferConcreteDecl`). It looked up the argument's type under the same-polarity key — a reader argument `S?` keyed `"S?"` — but in a well-formed clause only the *paired writer* `S` is recorded from a prior atom/head (SRSW). So a polymorphic parameter passed a reader (the usual shape: `pconsumer(S?)`, `ui_mediator(_, Ch?, ...)`) never inferred its type parameter, and **no instantiation was collected** — even in project mode. Fix: resolve the element type polarity-agnostically by base name (`callerVarTypes[name] ?? callerVarTypes['name?']`); both halves report the same `DFAState.baseName`. A successful inference now *only records* the instantiation for the re-check below; it deliberately **no longer re-types the call site's own arguments** against the inferred concrete declaration. Doing so would force the caller's own duality check against the inferred element type and wrongly reject correct wiring (e.g. a friend-channel `Stream<FriendMsg>` writer feeding a `Stream<NetInMsg>` consumer in `programs/social/network/boot.glp`). The polarity obligation belongs to the parameterized procedure's body, discharged by Phase 2.

2. **Single-file/REPL mode never ran Phase 2** (`type_checker.dart` `checkModule`). The per-instantiation clause-template check existed only in the project linker (`typeCheckProject`); a file loaded directly in the REPL got each parameterized procedure checked once under the wildcard self-check (`X := _`), where a body sub-pattern against a wildcard element passes vacuously. Fix: when no external collector is supplied, `checkModule` runs a self-contained per-instantiation Phase 2 + zero-instantiation wildcard fallback over the module's own clauses, mirroring `typeCheckProject`.

**Resulting signal.** At each concrete instantiation the parameterized procedure's clauses are re-checked; a body polarity clash surfaces as a *clause* well-typing failure (well-typed-clause condition 1: head not well-typed) in the offending clause — e.g. `bug3`'s `pconsumer` head at `X := ProdMsg`: `Resp?` (reader) where `ProdMsg`'s `befriend` slot 2 is a writer. The report's predicted `(S, S?) not dual` (condition 3, variable-pair duality) **cannot** fire: once inference binds `X := ProdMsg` both `go` endpoints are `Stream<ProdMsg>` and so are dual; the defect lives in `pconsumer`'s body, where the per-instantiation error correctly points (confirmed with Udi, 2026-06-17).

**Repros.** `min_polarity_bug.glp` (concrete consumer) and `min_polarity_bug2.glp` (polymorphic consumer) are the differential pair from the report, but both also carry an unrelated producer-head error (the `_R?` "error A") and so fail type-checking regardless — they document the gap but guard nothing. `min_polarity_bug3.glp` is `bug2` with the producer emitting `[]` (well-typed head), isolating the gap: accepted before the fix, rejected after with only the `pconsumer` polarity error. It is the Section A regression guard (`NEGATIVE_FILES`).

**Update (2026-06-19): the wildcard fallback was removed.** Following the TGLP paper revision (`parameterized-types.tex` §"Programs and Modules"; `modules.tex` §"Self-contained type checking"), the unit of type checking is the linked program, and checking a parameterized procedure with its parameter left free — equivalently, under the wildcard `_` declaration — is **unsound** (a parameter-inspecting clause is accepted vacuously). The zero-instantiation wildcard fallback described in item 2 above was therefore deleted from both `checkModule` and `typeCheckProject`: a parameterized procedure is now checked **only** per concrete instantiation, and one that is never instantiated within a program is not type-checked. The `DeferredParamProc` machinery that fed the fallback was removed. Spec: `type system/typed-program.md` "Programs and Modules". New regression guards (`run_all_tests.sh`): `param_free_not_checked.glp` (Section B/positive — a parameterized proc with a coverage gap, never instantiated, loads clean) paired with `param_instantiated_coverage_gap.glp` (Section C/negative — same proc instantiated, the gap is caught). REPL 492→494; dart unchanged; no negative test relied on the fallback.

**Update (2026-06-19, later): Phase A — modular checking via abstract parameters landed.** Following the TGLP paper's `sec:abstract-parameters` (`lem:parametricity`, `def:parametrically-well-typed`, `def:abstract-type`), a parameterized procedure that does **not** inspect its parameter and does **not** use it as a type-definition top-level alternative is now checked **once** against its **abstract instance** (each type parameter replaced by a distinct zero-alternative abstract type, `$abstract_X`), instead of going unchecked when never instantiated. The check is run by seeding the abstract instance into the per-instantiation closure, so body-induced types are materialized and a type-changing recursive call is caught as a monomorphic-recursion duality clash. The abstract route is a **commitment** (Decision 1): the procedure is certified — and suppressed in the program closure — whether the abstract check passes or fails, so a never-instantiated clean procedure with a coverage gap is now correctly **rejected**. Implementation: `certifyParametricProcedures` + `certifiedKeys` suppression in `type_checker.dart`; routing predicates `procInspectsParameter` / `paramUsedAsTypeAlternative` + `buildAbstractInstance` in `param_expansion.dart`; wired into `checkModule` and `project_linker.dart`. Spec: `type system/typed-program.md` "Modular Checking via Abstract Parameters". **Consequence:** `param_free_not_checked.glp` **flipped** to a negative guard (case ii: clean, coverage gap, never instantiated → rejected); a 4-case routing matrix now guards the feature — `param_abstract_covered.glp` (case i, positive), `param_free_not_checked.glp` (case ii, negative), `param_inspect_uninstantiated.glp` (case iii, positive), `min_polarity_bug3.glp` (case iv, negative) — plus `param_instantiated_coverage_gap.glp` (gap masked by an instantiation, still rejected). `monomorphic_recursion.glp` is now correctly rejected via the materialized `Stream<Box<$abstract_X>>` type. **Surfaced and fixed two ill-typed book programs.** `distribute_indexed.glp` and `merge_tree.glp` were genuinely ill-typed — each routed a wildcard-typed value (`_`) into a parametric `Stream(X)` output, which by `def:well-typed-clause` 3(a) (head-head strict duality, no subtyping relaxation) and `def:abstract-type` (abstract ≠ wildcard) is a duality clash; the abstract route correctly rejected them (paper-confirmed with Udi, 2026-06-19). **Root cause:** the data type pinned the carried value to wildcard `_` (`SendMsg ::= send(Number, _)`; `ListOfLists ::= [] ; [Stream(_) | ListOfLists]`) while the procedure promised a fully parametric `Stream(X)` output — unsound, since the procedure cannot actually produce a stream of an arbitrary caller-chosen type from untyped payloads. **Fix:** parameterize the data type so the carried value type *is* the output element type — `SendMsg(X) ::= send(Number, X)` / `SendStream(X) ::= [] ; [SendMsg(X) | SendStream(X)]` with `distribute_indexed(SendStream(X)?, Stream(X), Stream(X))`; and `ListOfLists(X) ::= [] ; [Stream(X) | ListOfLists(X)]` with `merge_tree(ListOfLists(X)?, Stream(X))` and `merge_layer(ListOfLists(X)?, ListOfLists(X))`. Both now load clean and run (`distribute_indexed` → `Y=[a,c], Z=[b,d]`; `merge_tree` → `Out=[a,x,1,p,b,y,2,q]`). REPL suite **496/496 green**. (Edited only the tested copies under `programs/book/`; the snapshot — since 2026-08-03 `programs/old-archive/book/`, and until then `programs/archive/book/` — keeps the original unparameterized forms, as do the retired `book 2` / `OLD typed book` copies since relocated to `/Grassroots/OLD/GLP/programs/`.  `run_book_tests.sh`, which compiled that corpus, is retired to `test/archive/`: live code never points at archived code, so it could not follow the rename.)

---

## Issue 15: Suspended agent goal not re-awoken when NetIn is bound after suspension via nested merge

**Status (RESOLVED — phantom, 2026-06-22)**: Not a runtime wake-up bug. Instrumented `NoMoreClauses` showed the friend-intro goal suspends on **UserIn**, not NetIn — NetIn is already bound with a committable intro, but the legacy clause's head `intro(Other, Ch?)` reads the channel slot with a **reader**, which cannot match the channel structure delivered there (a head reader matches only a writer). Canonical `social/graph` already has the correct polarity (`intro(Other, Ch)` head-writer captures, body forwards `Ch?`) and uses no `merge`-into-NetIn, so the legacy timing isn't even expressible there — i.e. the bug lived only in the stale `tests/agent_roundtrip` fixtures. It's a **typing/legacy-fixture artifact**. The red gate `probe15_intro_test` (+ `probe15_intro_boot.glp`) was **eliminated**; `clause_select_probe_test` (rotted sibling scaffolding) is retired (skip). No runtime fix; canonical is correct. The pre-resolution status is preserved below.

**Status**: Open — owned by the madGLP/IGLP session. Full report: `docs/agent-netin-wakeup-stall-bug.md` (index: `docs/iglp-bug-reports-index.md` #3).
**Discovered**: 2026-06-17
**Affects**: Liveness — a ready, committable message is never processed; the transaction stalls silently. Single isolate, no MAD/`_w`.

### Summary

An `agent/4` goal suspends with both `UserIn` and `NetIn` unbound. A later friend-channel `merge` binds `NetIn` to a committable `intro(...)` message, but the goal is **not re-scheduled** and the intro clause never fires — it appears to wait only on `UserIn` (the first-tried clauses), not on the `NetIn` argument a later clause matches.

### Ruled out (isolation probes all pass — `programs/tests/clause_select_probe.glp`)

Clause selection, channel-with-unbound-writer head-matching, and `otherwise` catch-all ordering all commit the later clause when the NetIn message is present at first reduction. So the fault is **timing/wake-up**, not matching.

### What to investigate

Whether a goal that suspends because its first-tried clauses block on one unbound argument (`UserIn`) correctly registers a wake-up dependency on the *other* argument (`NetIn`) that a later clause matches — specifically when that argument is bound after suspension by a (possibly nested) `merge`. A standalone minimal repro that suspends *before* delivering NetIn through a nested merge was not yet achieved (the next thing to force).

### Repro

Full: `glp_multiagent/test/scenario_single_isolate_test.dart` (phase 2 — `befriend_intro` never appears), trace `/private/tmp/scen-log.txt`. Distinct from Issue 13 (single-isolate, local channel, non-wakeup — not a `receive/3` unification failure).

---

## Issue 16: CSSN v2 (`programs/social/network`) is type-incorrect — surfaced by correct parametric typing

**Status**: Open — owned by a separate CSSN session. The type-system correctness work that surfaced it has landed; CSSN v2 is intentionally left failing (Udi: "ignore cssn, another conversation will fix it when the type system is correct"). **Section K (CSSN v2) of `run_all_tests.sh` and `cssn_v2_isolate_test.dart` are expected-failing until CSSN is retyped.**
**Discovered**: 2026-06-17

### Summary

The parametric-typing closure work (extends Issue 14: closure of parameterized-procedure instantiations under calls; correct call-site argument typing; the finiteness rule on recursive parameterized types) made the checker actually check call-site duality against concrete instantiations. CSSN v2 then fails type-checking — correctly. It is the only project that regressed; `bonds`, `spm/v2` (CSSG), and `child_safe` all load clean.

### Root cause (a real CSSN defect, previously hidden)

`programs/social/network/self.glp` merges a friend-channel stream into NetIn (`merge(NetIn?, FIn?, NetIn1)` in `smaller_dispatch`/`await_canonical`). That stream is typed `Stream<FriendMsg>`, and `FriendMsg ::= msg(Constant, Constant, FriendContent) ; canonical` carries the `canonical` handshake constructor, which is **not** in `NetInMsg ::= msg(Constant, NetColdCall) ; msg(Constant, Constant, FriendContent)`. So `Stream<FriendMsg>` ⊄ `Stream<NetInMsg>`: `(AliceFromCarol, AliceFromCarol?) not dual … Stream<FriendMsg> is not a subtype of Stream<NetInMsg>`. `canonical` is stripped at runtime (`[canonical|FIn]`) but the type of the post-handshake stream still admits `canonical`.

### Fix direction (CSSN-side)

Make the stream merged into NetIn genuinely canonical-free, e.g. carry the `canonical` handshake on a separate signal/path rather than inside the friend-message stream, OR have `NetInMsg` accept `canonical` and the agent ignore it (watch NetIn coverage). This is CSSN structural design, not a type-checker change. **Do not weaken the type checker to re-admit it.**

### What landed (the correct type system)

- `analysis/type_checker/type_checker.dart` — `checkInstantiationsClosed`: per-instantiation clause checking closed under calls (fixpoint); `checkModule` runs it in single-file/REPL mode; project mode via `typeCheckProject`. **Monomorphic recursion**: a body call to a procedure on the current instantiation cycle (`activeInstantiations`) is checked at the enclosing instantiation, never re-inferred, so a recursive call at a different instantiation is a type error (closes polymorphic recursion). The closure **materializes** types that arise only through it (e.g. `Stream<Box<Msg>>` from a type-changing procedure) and re-runs to a fixpoint; monomorphic recursion keeps that set finite, so it terminates with no size bound.
- `analysis/type_checker/well_typed_clause.dart` — `_inferConcreteDecl` resolves a reader argument via its paired writer (base name); call-site arguments are typed against the inferred concrete declaration; a referenced type that is missing but materializable (known template) is kept (not bailed) so the closure can materialize it.
- `analysis/type_checker/param_expansion.dart` — `_checkNoGrowingTypeRecursion`: the finiteness rule, scoped to the **self-referential occurrence** (a parameter may not be a proper subterm of an argument *of the self-reference*; sibling elements like `StreamBox(X) ::= [Box(X)|StreamBox(X)]` are fine), enforced statically at the parsing/expansion stage. `materializeInstantiations`: expand closure-induced type names into monomorphic definitions.
- Guards (all in `run_all_tests.sh` Section C): `min_polarity_closure.glp` (closure under calls), `growing_type_recursion.glp` (type-def finiteness rule), `monomorphic_recursion.glp` (recursion at a different instantiation rejected).
- Spec `docs/type system/typed-program.md` harmonised with the paper §Parameterised Types: self-referential-occurrence scoping of the finiteness rule, and monomorphic recursion.

---

## Issue 17: `glp_runtime` `dart test` baseline carries 6 reds — 4 stale rot, 2 the live Issue-13 strand

**Update (2026-06-22) — RESOLVED.** All six reds are now retired (skip-with-reason) — every one was legacy `tests/agent_roundtrip` rot, not a runtime defect:
- `isolate_manager_test` ×2 — boot-config scope omission (`UnknownTypeError: Response`), not the "Issue-13 strand"; multi-isolate path covered green by `cssn_v2`/`bonds_v2`.
- `clause_select_probe_test` ×1 — rotted Issue-15 scaffolding (superseded by the now-eliminated probe15).
- `ui_mediator_test` ×3 — inline `_output` fixed (now routes through root `send_to_user`), but the test loads `typed_social_agent`/`typed_ui_mediator` without their `tests/agent_roundtrip/self.glp` scope, so the goal-check hard-fails on `Unresolved type: Response`; canonical mediator covered by live cssn/social-graph.  **Disposition 2026-08-03: the test is REMOVED** (Udi's instruction) — see Issue 20's third instalment.  `typed_ui_mediator.glp` stays and is exposed by `agent_roundtrip/self.glp`.

Baseline now **379 pass / 11 skip / 0 fail**; REPL **501/501**. All retired tests exercise deprecated `tests/agent_roundtrip`, superseded by canonical `social/graph` + `cssn`.

**Status**: Open tracking entry (so the 6-red baseline is not silently normalized). Logged 2026-06-21 during the Issue-13/15 fix task.
**Discovered**: 2026-06-21 (baseline before touching code).

### The split

A clean `main` `cd glp_runtime && dart test` ends **379 pass / 5 skip / 6 fail**. The 6 reds are NOT all Issues 13/15 — they are three distinct things:

| Red | Cause | Disposition |
|---|---|---|
| `isolate_manager_test` ×2 (30 s `TimeoutException`) | befriend round-trip strands on the cross-isolate `_w` writer | **Issue 13** (live) |
| `ui_mediator_test` ×3 | the test's inline GLP calls `'_output'` directly, now rejected by the primitive-layer enforcement (`Constant '_output' names a language primitive …`) introduced by the TGLP **system-mode strip** | **RETIRED 2026-08-03** — fix-or-retire was the disposition and retire is the answer; the test is deleted (Udi's instruction) |
| `clause_select_probe_test` ×1 | `programs/tests/clause_select_probe.glp` now fails type-checking (`Variable mode mismatch: reader requires ↓, got ↑`, line 50); `got_intro` still fires but the goal suspends and the "NetIn clause commits" assertion fails | **stale rot** — this is **Issue 15's own isolation probe**; part of 15's scaffolding has already rotted. Fix-or-retire folded into the Issue-15 work |

### Gate consequence

The Issue-13/15 fix gate cannot mean "all 6 glp_runtime reds green": the `ui_mediator` `_output` reds and the `clause_select_probe` type-error are independent of both bugs and will not flip from the `_w`/wakeup fixes. Issue 13's true green gates are `glp_multiagent/test/roundtrip_isolate_test.dart` + the two `isolate_manager_test` timeouts. Issue 15 has **no** red guard (`scenario_single_isolate_test` passes but asserts only befriend→connected, never the phase-2 intro) — a confirmed-red regression must be built first.

### Also

The REPL suite (`bash test/run_all_tests.sh`) is **496/496 green** on the same tree; the rot is confined to `glp_runtime` Dart tests.

---

## Issue 18: malformed channel `ch(S?, _)` diagnostic — DROPPED (2026-06-22)

Ticketed then **dropped**: a PE diagnostic for an unpaired-Out channel (`ch(S?, _)` silently reducing `receive` to `otherwise`). Not pursued — the only bugs it would have helped *diagnose* (Issues 13 and 15) are resolved phantoms, and distinguishing an unpaired `_` Out from `new_channel`'s legitimate paired-but-unbound Out isn't worth the `lib/compiler` risk for a pure diagnostics gain. (Evidence probe `programs/tests/recv2x2/` retained.)

---

## Issue 19: unhandled `UnknownTypeError` kills an agent isolate silently → 30s hang

**Status**: Resolved (2026-06-22) — both halves landed. Checker side (TGLP, commit `c1d565eb`): `program_dfa.dart` `_resolveTypeExpr` surfaces an unresolved type as a located `TypeError` ("Unresolved type: …") instead of an unhandled `UnknownTypeError`. Isolate side (IGLP): `_agentIsolateEntry` catches init failure and reports `AgentInitFailed` to the manager. A missing-scope load now fails with a clear error, not a 30s hang.
**Layer**: type checker (`program_dfa.dart` `_resolveTypeExpr`) + isolate lifecycle (`isolate_manager.dart` `_agentIsolateEntry`).

When an agent isolate loads a program whose scope is incompletely supplied, `buildProgramDFA` throws an **unhandled** `UnknownTypeError` (e.g. `Response`) that kills the isolate. `IsolateManager` is not notified, so the manager waits on a dead isolate until the test's 30s timeout — a missing-scope condition becomes a silent hang instead of a reported error. The type-checker should surface unresolved types as locatable diagnostics, and/or the isolate entry should catch init failures and report them to the manager. This is what turned the (now-retired) `isolate_manager_test` scope omission into a 30s timeout rather than a clear failure.

---

## Issue 20: a parameter-inspecting procedure that nothing instantiates is checked by nothing

**Status**: Half fixed (2026-08-03).  The silence is fixed — a program load now names every such procedure.  What the measurement then showed is open: 45 procedures across five programs are checked by nothing, and each is a place a type error can sit undetected.
**Layer**: type checker (`lib/analysis/type_checker/type_checker.dart`), surfaced by the program linker (`lib/compiler/program_linker.dart`).

A parameterised procedure that inspects a type parameter has no well-typing of its own and acquires one only per instantiation (`parameterized-types.tex` sec:programs-and-modules).  Loaded standalone it is rejected; inside a program, one that no call instantiates goes **unchecked**, which the paper licenses — "a procedure with no caller in its program goes unchecked" (sec:abstract-parameters).  Until 2026-08-03 the checker said nothing at all about it, so a program containing wholly unchecked clauses printed a clean verdict, indistinguishable from one whose clauses had all been verified.

How it surfaced: not by reading the checker.  `programs/tests/agent_roundtrip/typed_actors.glp` had `bob_actor/1` and `charlie_actor/1` passing a raw `Response` where `UserContent.decision`'s third argument is a `PendingValue` — an untagged value at a tagged-union position.  Every actor is declared `Channel(X, Y)?` and nothing instantiates X and Y, so none of them was ever checked.  It was found because the file entered a program for the first time in months and the play stopped where it should not have (GLP `54dd7020`).  Declared concretely, the checker rejects the same clause at once — `programs/tests/typed/tagged_union_untagged_neg.glp`.

**What landed.**  A `TypeWarning` per such procedure, and one line at the point the program is pronounced well-typed:

```
[TYPE] 5 parameterized procedure(s) unchecked in this program — no instantiation: typed_actors:alice_actor/1, typed_actors:bob_actor/1, typed_actors:charlie_actor/1, typed_social_agent:agent/4, typed_social_agent:inject_msg/5
```

Not an error: an error at load refuses all 54 program directories in `programs/`, because root `programs/self.glp` exposes the four `social/graph/routing` modules into every program and their procedures are parameter-inspecting.  Rejection belongs after the tree is clean, not before.  Regression tests: `programs/tests/param_unchecked/` (the report fires and the program still loads) and `tagged_union_untagged_neg.glp` in `NEGATIVE_FILES`.

**The measurement, 2026-08-03, over every program directory under `programs/`.**  Five carry unchecked procedures; the other 49 carry none.

| Program | Unchecked |
|---|---|
| `tests/agent_roundtrip/play_ui_madglp` | 14 → **0** (see below) |
| `cssn` | 12 |
| `social/graph` | 10 |
| `tests/agent_roundtrip/play_madglp` | 5 → **0** (see below) |
| `currencies` | 4 |

`typed_social_agent:agent/4` is among them, in both agent_roundtrip programs: the agent itself has never been type-checked in either.  The `social/graph/routing` procedures (`send_user/3`, `send_net/3`, `send_friend/4`, `send_child/4`, `send_parent/4`, `add_friend_output/4`, `inject_msg/5`, `already_friend/4`, `smaller_dispatch/7`, `await_friend_channel/6`, `inject_intro_result/3`, `intro_await_peer/3`) account for CSSN's, `social/graph`'s and Currencies' entries; `output.glp`'s own header says its clauses rely on per-instantiation checking to reject an `Ent` lacking the constructor they destructure, and in these programs that checking never happens.

**Open**: give the inspected arguments concrete element types, program by program, so the clauses are checked; then the report can become a rejection.  Ownership follows the code map — the routing modules are SGSG's, the agent_roundtrip fixture IGLP's, `cssn` CSSN's, `currencies` Currencies'.

**First instalment, 2026-08-03: the actor half of `play_madglp`, 5 unchecked down to 2.**  `ActorIn` and `ActorOut` moved from `typed_actors.glp` into the directory's `self.glp` — a caller in another module has to be able to name them — and the three `<who>_actor(Channel(X, Y)?)` declarations and `actor/2`'s became `Channel(ActorIn, ActorOut)?`.  The checker, seeing the actors for the first time, rejected two of them at once: `bob_actor` and `charlie_actor` covered no empty input, though `ActorIn` has a `[]` alternative and every continuation in the file carries an explicit `[], []` clause.  Both got one.  That is the value of the exercise in miniature — the declarations were what the checker lacked, and the first thing it did with them was find a defect.  Runtime behaviour is unchanged, measured on the madGLP play: 40 `connected(`, 18 `received(`, both messages, `introduce` reached, identical to before.

**Second instalment, 2026-08-03: the agent half, `play_madglp` to ZERO unchecked.**  `agent/4` was declared `agent(Constant?, Stream(X)?, Stream(X)?, OutputsList?)` --- two different streams under one element type.  They are two things: the second carries what the actor or mediator writes, the third carries cold calls PLUS every friend's messages, merged in on accept (`merge(NetIn?, FIn?, NetIn1)`).  What forced them together was the outputs list: `OutputEntry ::= output(Constant, OutputStream?)` gave person, network and friend entries one stream type, so `OutputStream` had to be the union of all three message shapes and no consumer could be given the arm it handles.

Canonical `social/graph` had already retired that shape, for the reason its own `self.glp` records --- the union "broke duality" --- and `cssn`, `currencies` and `grassapp` carry the per-kind constructors too.  This fixture was the last holdout.  It now has `user_output(ActorIn?) ; net_output(NetOutStream?) ; friend_output(Constant, FriendStream?)`, one router per kind in place of `lookup_send/4`'s Constant key, and `agent(Constant?, ActorOut?, NetInStream?, OutputsList?)`.

Three defects fell out of it, none visible while the types were parametric.  `bob_actor` and `charlie_actor` covered no empty input.  And all three channel-carrying positions --- `PendingValue.channel`, `AgentContent.befriend_intro`, `FriendContent.intro` --- were `?`-typed as if they forwarded a writer, which a channel does not: canonical carries `?` on the response writer alone and its channel arm is plain.  Fixing that flipped `channel(Ch)` back to `channel(Ch?)` in the actors and in the mediator.

Two consequences beyond the count.  `typed_social_agent.glp` moved from `NEGATIVE_FILES` to `POSITIVE_FILES` --- it type-checks standalone now, which the negative entry's own comment had predicted the sweep would do.  The same comment predicted the sweep would unskip `ui_mediator_test`; it does NOT.  Unskipped and measured: all three fail with the goal producing no output and no load-time diagnostic at all, so the cause is on the co-loaded goal path and not in the fixture's types.  The skip records that now instead of the prediction.

**And the test is gone, 2026-08-03** (Udi's instruction, after the measurement above).  `glp_runtime/test/multiagent/ui_mediator_test.dart` is deleted rather than repaired: it read three files by hardcoded path, concatenated them as strings, regex-stripped `-mode(system).` to make the source loadable, injected its own procedure and ran with `strictTypes = false`.  Each of those is a workaround for something the module system now does properly --- ancestor scope from the directory's `self.glp`, `-expose` for visibility, program directories that load and run --- and the last means it exercised none of the typing the mediator now carries.  `typed_ui_mediator.glp` itself stays and is exposed to every converted boot.  One reference survives, in `test/run_all_tests.sh`'s `NEGATIVE_FILES` comment: that file is held for GLP-Spec's granted commit and is not IGLP's to touch this task.

**Third instalment, same day: the UI half, `play_ui_madglp` to ZERO unchecked.**  `typed_ui_actors.glp` declared every procedure over a bare parameter --- `Channel(X, Y)?` for the four entry actors, `Stream(X)?, Stream(X)` for the nine continuations, one parameter standing for BOTH the notification stream it reads and the command stream it writes.  The ground Dart-facing protocol they speak (`ReqId`, `UserCmd`, `UserNotify`) was defined inside `typed_ui_mediator.glp` and so could not be named from another module; it moved to `self.glp`, and the thirteen declarations became `Channel(Stream(UserNotify), Stream(UserCmd))?` and `Stream(UserNotify)?, Stream(UserCmd)`.

The checker's first look found the same defect it had found in the no-UI pair, in the same two roles: `bob_ui_actor` and `charlie_ui_actor` covered no empty input.  Both got a `ch([], [])` clause.

**Both agent_roundtrip programs are now checked end to end, and the table above reads 0 for each.**  What remains of Issue 20 is the routing procedures, which are SGSG's: 12 in `cssn`, 10 in `social/graph`, 4 in `currencies`, all of them `social/graph/routing`, reached through the root's exposures.

---

## Issue 21: the linked program's type namespace drops the arity, and the goal-check environment is built out of scope order — RESOLVED (2026-08-03)

Reported by SGSG Code as one defect — "a parameterised type defined in a directory's `self.glp` is invisible to that directory's other modules" — blocking the whole SPM implementation, since `programs/spm/{cva,gsg,secure_gsg}` would not load.  Measured before repair, as instructed, and it was three things: one non-defect, one defect that stopped the load, and one behind it that stopped every goal.

**The reported defect does not exist.**  Their seven-line reproduction is now `programs/tests/param_ancestor_scope/` (Section X8) and it loads and runs: a directory `self.glp`'s parameterised type reaches that directory's other modules exactly as a monomorphic one does.  Their measurement ran from a program root OUTSIDE `programs/`, where by the rule Section S10 pins there is no ancestor scope at all — so neither `Para` NOR `Plain` was in scope.  What made it look asymmetric is the type-parameter rule: an unknown capitalised name in a declaration reads as a type parameter, which `Plain` can be and `Para(Constant)` cannot, so `Plain` went quiet and `Para` was reported unresolved.  A wholly invented name behaves like `Plain`, which is how the asymmetry was settled.

**Defect 1, the load.**  `linkedFlatModule` in `program_linker.dart` built the linked program's type definitions with `typeDefs.putIfAbsent(td.name, ...)` — keyed by bare NAME.  The arity is part of a type constructor's identity as `n` is part of `p/n`: `NetMsg` and `NetMsg(C)` are two constructors, and a scope may hold both (the per-module environment does, keeping monomorphic types and parameterised templates in separate maps, which is why the per-module check passed and only the linked check failed).  Keyed by name, whichever arity the directory walk reached second was dropped and every reference to it went unresolved.  `spm/cva/self.glp`'s `NetMsg(C)` displaced the arity-0 `NetMsg` of `programs/system/mad_predicates.glp` — which the root `self.glp` `-expose`s into EVERY program — so `mad_predicates.glp:19`'s `NetStream` lost its element type and all three `spm` directories failed to load.  Fixed by keying on `name/arity`.  Fixture: `programs/tests/type_name_collision/` (Section X9), five lines and no platform code, which reproduces the exact `spm` error when the fix is backed out.

**Defect 2, every goal.**  With the load fixed, `spm/gsg` still refused every goal with `UnknownTypeError: UserEvent`.  `loadProgramDirectory` extended the goal-check environment by merging the program's modules in DISCOVERY order.  Each merge expands that module's parameterised type references against what the environment holds so far, so a module merged before the `self.glp` defining a template it names loses the template and its declaration keeps an unresolved type — and the goal checker then trips over a declaration that has nothing to do with the goal.  `spm/gsg/plays/play_befriend.glp:24` names `UserEvent(V, Q, A)` from `gsg/self.glp` one directory up.  Fixed by merging in scope order (modules.tex §Scope construction): shallower directories first, a directory's `self.glp` before its siblings.  Section X10 pins both halves on the three `spm` directories.

**Still open, and NOT fixed here: the same line conflates two same-name SAME-arity types from different scopes,** first-wins and silently.  `programs/book/streams/objects_monitors/network_switch_3way.glp` defines `NetMsg ::= msg(Constant, Constant)` where `mad_predicates` has `NetMsg ::= msg(Constant, _)`, and the suite loads that file; whichever the walk reaches first is the one the whole linked program gets.  Sibling modules cannot see each other's definitions (§Design, sibling isolation), so these are two types and the linked program must keep them apart — the analogue of step 3's `M:p/n` renaming, applied to types.  §Static Linking's step 3 names procedures only, so implementing it would put the code ahead of the paper: reported to GLP-Spec 2026-08-03 as a specification gap, and awaiting their sentence before any code moves.
