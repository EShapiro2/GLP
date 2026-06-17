# IGLP bug reports — index (from the single-isolate UI / §7.4 work)

Three distinct, independently reproducible bugs were surfaced while implementing
the §7.4 UI mediator on the social-graph protocol. Each has its own report and
reproducer(s). Severity order: all are soundness/liveness, not cosmetic.

| # | Report | Layer | One line |
|---|--------|-------|----------|
| 1 | [typechecker-polymorphic-polarity-bug.md](typechecker-polymorphic-polarity-bug.md) | Type checker | Reader/writer polarity is not re-checked when a polymorphic type parameter is instantiated — `Channel(X,Y)` hides a producer/consumer duality violation. |
| 2 | [madglp-w-writer-return-bug.md](madglp-w-writer-return-bug.md) | madGLP runtime (`receive/3`) | A `_w`-backed cross-isolate reader fails to unify against a nested reader sub-pattern in `receive/3`, where ordinary head-matching succeeds. |
| 3 | [agent-netin-wakeup-stall-bug.md](agent-netin-wakeup-stall-bug.md) | GLP runtime (suspend/wake) | A suspended agent goal is not re-awoken when a *different* argument (NetIn) is bound after suspension by a nested `merge`, so a committable clause never runs. |

## 1. Typechecker polymorphic-polarity gap

- **Claim:** single-project compilation enforces producer/consumer duality, but a
  polymorphic element type (`Stream(X)`, `Channel(X,Y)`) discharges the polarity
  obligation against the abstract `X` and never re-discharges it at the concrete
  instantiation.
- **Repro:** `programs/tests/min_polarity_bug.glp` (concrete — correctly **errors**)
  vs `programs/tests/min_polarity_bug2.glp` (only change: polymorphic consumer —
  **passes**, but the duality error should still fire).
  Run: `echo -e 'load ../programs/tests/min_polarity_bug2.glp\n:quit' | dart run bin/glp_repl.dart`
- **Fix test:** bug2 must report the `(S, S?) not dual` error.

## 2. `receive/3` `_w` writer-return

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
