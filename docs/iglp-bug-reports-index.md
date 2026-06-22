# IGLP bug reports — index (from the single-isolate UI / §7.4 work)

Three distinct, independently reproducible bugs were surfaced while implementing
the §7.4 UI mediator on the social-graph protocol. Each has its own report and
reproducer(s). Severity order: all are soundness/liveness, not cosmetic.

| # | Report | Layer | One line |
|---|--------|-------|----------|
| 1 | [typechecker-polymorphic-polarity-bug.md](typechecker-polymorphic-polarity-bug.md) | Type checker (TGLP) | **RESOLVED (2026-06-22).** Reported gap misdiagnosed; two real soundness holes found and fixed in `lib/analysis` (standalone parametric check; cross-module instantiation closure). See §1. |
| 2 | [madglp-w-writer-return-bug.md](madglp-w-writer-return-bug.md) | madGLP runtime (`receive/3`) | **DISPROVEN — phantom (2026-06-22).** The `_w` is innocent: `mad_w_clean` (= `mad_w_probe` with `ch(S?,_)`→`ch(S?,closed)`) matches the same cross-isolate `_w` through `receive`. Real cause of the `otherwise` was the malformed channel `ch(S?,_)` (anon Out, no paired reader) which the PE-unfolded `receive` statically reduces to fail — compile-time, single-heap, no `_w` (see `programs/tests/recv2x2`). See §2. |
| 3 | [agent-netin-wakeup-stall-bug.md](agent-netin-wakeup-stall-bug.md) | GLP runtime (suspend/wake) | A suspended agent goal is not re-awoken when a *different* argument (NetIn) is bound after suspension by a nested `merge`, so a committable clause never runs. |

## 1. Typechecker polymorphic-polarity gap — RESOLVED (TGLP, 2026-06-22)

**Resolution.** The reported gap was misdiagnosed: the two cases compared differ in the slot's `?` — the probe's slot is output-typed, but the real `FriendContent.intro` slot is input-typed (`FriendChannel?`), so the reader head is correct and was correctly accepted. Investigation then found and fixed two real soundness holes in `lib/analysis` (the type-checker, owned by TGLP): (1) a standalone parametric procedure was checked with its type parameter substituted by the wildcard `_`, vacuously accepting any clause (the route the paper names as unsound); now a fresh abstract type is substituted, so a functor or constant at a parameter position is rejected with a locatable error. (2) A parametric exported procedure instantiated only via a cross-module `#` call was never checked at the caller's instantiation; now each `imported` declaration is seeded as a concrete instantiation into the linked-program closure, which checks the callee's clauses at it over an environment carrying every module's types and declarations. Paper sound throughout (well-typing is established on the linked program); two clarifying sentences added to the TGLP paper (§abstract-parameters, §static-linking), no soundness change. Verified REPL 496/496 (incl. CSSN, social-graph, Bonds v2 multi-module projects + a new cross-module regression test); `dart test` shows no new failures. **Spin-offs, non-blocking:** (a) the legacy `typed_social_agent.glp` runtime stall is a program defect — two opposite-polarity definitions of its own `intro` (`OutputContent.intro(…, FriendChannel)` sender vs `FriendContent.intro(…, FriendChannel?)` receiver), each locally well-typed, never compared because the channel crosses the mediator as data; remedy is one slot polarity end-to-end as canonical `social/graph` (a program-fix ticket for the program owner). (b) Fix 2 seeds the `imported` declaration rather than the call site (a sound upper bound — confirm it covers all call sites, or switch the seed to the resolved call sites to match the paper; a TGLP follow-up).

**Original report (misdiagnosed; kept for history):**

- **Claim:** single-project compilation enforces producer/consumer duality, but a
  polymorphic element type (`Stream(X)`, `Channel(X,Y)`) discharges the polarity
  obligation against the abstract `X` and never re-discharges it at the concrete
  instantiation.
- **Repro:** `programs/tests/min_polarity_bug.glp` (concrete — correctly **errors**)
  vs `programs/tests/min_polarity_bug2.glp` (only change: polymorphic consumer —
  **passes**, but the duality error should still fire).
  Run: `echo -e 'load ../programs/tests/min_polarity_bug2.glp\n:quit' | dart run bin/glp_repl.dart`
- **Fix test:** bug2 must report the `(S, S?) not dual` error.

## 2. `receive/3` `_w` writer-return — DISPROVEN (2026-06-22)

**Resolution.** Phantom bug. Controlled refutation: `programs/tests/mad_w_clean.glp` (= `mad_w_probe.glp` with the single change `bob_consumer(ch(S?, _))` → `bob_consumer(ch(S?, closed))`) makes the **same** cross-isolate `_w` writer **match through `receive`** (`bob_ch_matched`), two isolates (`glp_multiagent/test/mad_w_clean_test.dart`). The `otherwise` came entirely from the malformed channel `ch(S?, _)` (anonymous Out, no paired reader): `receive` is a PE-unfolded defined guard, and the partial evaluator statically reduces `receive(NestedReader, ch(S?, _), Cont)` to failure — **compile-time, single-heap, no `_w`** (reproduced by `programs/tests/recv2x2/`: `_` Out → otherwise, `closed` Out → matched). The `isolate_manager_test` timeouts this was tied to were a separate boot-config scope omission (`UnknownTypeError: Response`), now retired. No `receive/3`, runtime, or `_w` defect. Original claim kept below for history.

**Original claim (misdiagnosed; kept for history):**

- **Claim:** a `_w` reader is admissible in ordinary head unification,
  reader→writer→reader forwarding, and escrow-across-suspension — it fails **only**
  through `receive/3` matching a nested reader sub-pattern over a `_w`-bearing
  message. This is the mediator return hop
  `receive(msg(_user, Id, decision(_, _, response(Resp?))), UserCh?, _)`, so the
  cold-call befriend round-trip strands across isolates.
- **Repro:** `programs/tests/mad_w_probe.glp` +
  `glp_multiagent/test/mad_w_probe_test.dart` (the `bob_consumer`/`receive/3` case
  falls to `otherwise`; the list-pattern cases match).
- **Discriminator:** `glp_runtime/test/multiagent/single_heap_roundtrip_test.dart`
  (same `.glp`, single heap → completes) vs
  `glp_multiagent/test/roundtrip_isolate_test.dart` (two isolates → strands).

## 3. Agent NetIn wake-up stall

- **Claim:** when an agent goal suspends because its first-tried clauses block on
  one unbound argument (UserIn), it is not re-awoken when a *later* clause's
  argument (NetIn) is bound after suspension by a nested friend-channel `merge`.
- **Ruled out by isolation probes (all pass):** clause selection, channel-with-
  unbound-writer head-matching, and `otherwise` catch-all ordering. See
  `programs/tests/clause_select_probe.glp` + its test.
- **Repro:** `glp_multiagent/test/scenario_single_isolate_test.dart` (phase 2 —
  `befriend_intro` never appears); trace `/private/tmp/scen-log.txt`.
- **Open:** a standalone minimal repro that suspends the goal *before* delivering
  the NetIn message through a nested merge was not achieved; that timing is the
  next thing to force.

## Status of the UI `.glp` (not a bug — leave as is)

The shared `programs/book/social_graph/self.glp`, the concrete agent-facing
`Channel(Stream(AgentMsg), Stream(UserMsg))` in `ui_mediator`, and the §15B
writer-forwarding polarity in `typed_social_agent.glp` (cold-call clause 214,
intro clause 203) are correct: both files typecheck strict, and the single-heap
round-trip completes. Bugs #2 and #3 block the multi-isolate and intro paths
respectively; the cold-call round-trip works fully in a single isolate.
