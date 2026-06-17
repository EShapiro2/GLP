# GLP Known Issues

**Last updated:** 2026-06-10

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

## Issue 10: `..=/2` declared but has no clause

**Status**: Open
**Discovered**: 2026-06-10 (while testing Issue 0a)
**Affects**: Any program using the decomposition operator `..=`

### Summary

`..=` is declared in `programs/self.glp` (`procedure ..=(Stream(_), _?).`) and parses and type-checks as a body goal, but has no clause: `List ..= Compound?` fails at runtime with "Spawn could not find procedure label: ..=/2". Its dual `=..` (compose) is declared, implemented, and regression-tested (Issue 0a, Section A30).

### Fix

Implement `..=/2` (decompose a compound into `[Functor | Args]`) per its declaration in `self.glp` — clause or kernel, matching how `=..` is realized — with a Section A regression test.

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

**Status**: Open — owned by the madGLP/IGLP session. Full report: `docs/madglp-w-writer-return-bug.md`; reproducer: `programs/tests/mad_w_probe.glp` + `glp_multiagent/test/mad_w_probe_test.dart`.
**Discovered**: 2026-06-17
**Affects**: Soundness — a writer that crosses isolates (serialized as `_w(p,i)`) cannot be bound after a round-trip, though the identical clauses work in one heap. Concretely: the cold-call return hop never delivers the decision, so befriending strands.

### Summary

A `_w`-backed reader is admissible in ordinary clause-head unification, survives reader→writer→reader forwarding through a list, and survives escrow across a suspension — but **fails only through the `receive/3` channel kernel**: matching a nested *reader* sub-pattern (`wrapped(Y?)`) against a channel message carrying a `_w` writer does not unify, and the clause falls through to `otherwise`. The probe (`mad_w_probe.glp`) isolates exactly this: three paths against the same `_w` match; the `receive/3` path falls to `otherwise`.

This is the mediator's return hop `receive(msg('_user', Id, decision(Dec, From, response(Resp?))), UserCh?, UserCh1)` — nested reader `response(Resp?)` over a `_w`-backed message — so the agent's `decision` clause never commits and the round-trip strands. Single-heap (local writer) completes; two-isolate (`_w`-backed) strands. Same clauses, only the transport differs.

### Family / relation to the fixed issues

Same root family as Issues 1, 9, 12 (heap-address vs reader/writer-role confusion from the pointer-architecture migration), but a **distinct** site and fix: not the `pairedReaderAddr` arithmetic (Issue 9, fixed) nor the guard-deref lookup (Issue 12, fixed) — it is a *missing* `_w` indirection inside `receive/3`'s unification of a nested reader sub-pattern. The Issue 9 index fix does not address it (the madGLP UI-mediator `dart test` cases for this remained failing with that fix in).

### Fix

Investigate `receive/3`'s dereferencing / cell-walk of a `_w` remote writer when binding sub-patterns; plain list head-matching of the same value succeeds, so the divergence is inside the `receive/3` kernel. The UI `.glp` is correct and must not change for this bug. Multi-isolate gate stays red until fixed.
