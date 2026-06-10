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

**Aside (separate, out of scope):** the dual operator `..=` is declared in `programs/self.glp` (`procedure ..=(Stream(_), _?).`) but has no clause, so `List ..= Compound?` parses and type-checks yet fails at runtime with "Spawn could not find procedure label: ..=/2". Not part of this issue; flagged for a future fix.

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

**Status**: Fixed — functional defect resolved (verified by trace 2026-06-10); see Investigation Result. The N+1 audit it names found latent reliance on the allocation convention that remains open as a separate, deferred concern (core-heap change, needs approval) — see N+1 Audit Result.
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

### N+1 Audit Result (2026-06-10): latent reliance found; core fix deferred (needs approval)

The "Broader Concern: N+1 Arithmetic" audit was performed. Code **does** rely on the N/N+1 allocation convention, in violation of the explicit rule in `lib/runtime/terms.dart` ("MUST NOT: Code must not assume reader_addr == writer_addr + 1"):

- `heap_fcp.dart` `pairedReaderAddr()` — fallback `return writerAddr + 1`. Reached when `readerForWriter()` returns null, which it does for a **bound** writer (the bidirectional pointer is consumed on binding). There is no cross-pointer way to recover the reader of a bound writer under the current cell design, so this fallback is structurally necessary, not merely defensive.
- `lib/bytecode/runner.dart` — direct `writerAddr + 1` reader derivation at lines 2346, 2574, 2580, 2716.

These are **currently correct** because `allocateVariable()` always allocates `(HP, HP+1)`, so the convention holds in practice; hence no active failure and the pipeline passes. But they are latent fragility: any change to allocation (e.g. interleaved/relocating allocation, GC compaction) would break them.

Removing the reliance requires a core-heap change — either retaining the reader pointer on bound writers, or threading the reader address through the call sites instead of deriving it. Per `GLP/CLAUDE.md`, modifying core GLP files (`runner.dart`, `heap_fcp.dart`) requires explicit discussion and approval. **Deferred pending that decision** — not fixed unilaterally. Recommend a dedicated task with approval.

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

**Status**: Open — ready; verification by Claude Code (manual app check waived 2026-06-10)
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
