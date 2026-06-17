# Bug: agent goal not re-awoken when NetIn is bound after it suspends (friend-channel intro)

**Date:** 2026-06-17
**Component:** GLP runtime — goal suspension / wake-up (single isolate, no MAD/`_w`)
**Severity:** Liveness — a ready, committable message is never processed; the
transaction stalls silently.

## Symptom

In the single-isolate scenario (`glp_multiagent/test/scenario_single_isolate_test.dart`,
phase 2), charlie befriends bob, then introduces alice and bob. The introduction
message reaches bob's agent over the friend channel, but bob never emits
`befriend_intro` — clause 232 (`Introduction from friend`) never fires, and bob's
agent sits suspended with the message ready at its `NetIn` head.

Trace (`/private/tmp/scen-log.txt`), the same agent goal across two snapshots:

```
agent(bob, X153?, X157?, X158?) → suspended
   % suspended with NetIn (X157?) and Outs (X158?) both unbound

agent(bob, X153?, [msg(charlie, bob, intro(alice, ch(X198?, X199))) | X210?],
                  [output(charlie, X160) | [output(_user, X174) | [output(_net, X6)]]]) → suspended
   % SAME goal, re-rendered: X157? has since been bound by the friend-channel
   % merge to the intro message, and Outs (X158?) bound with an `_user` channel —
   % yet the goal is still suspended and is never re-scheduled.
```

Clause 232 is fully committable against the second form (NetIn head is a ground
3-field `intro`, `Outs` contains `_user`, guard `Id =?= Id1, ground(Other?)`
holds) — but it never runs.

## What is ruled out (isolated probes all PASS)

`programs/tests/clause_select_probe.glp` + `glp_runtime/test/multiagent/clause_select_probe_test.dart`
reproduce each suspected mechanism in isolation; all commit the later clause:

1. **Clause selection** — a committable NetIn clause fires while UserIn is
   unbound and the user clauses' heads suspend. (`got_net` / `got_intro` fires.)
2. **Channel with unbound writer** — a 3-field `intro(Other, ch(In, Out))` whose
   channel carries an unbound write end, delivered via `merge`, commits the
   intro clause. Not a nested-writer matching problem.
3. **`otherwise` catch-all ordering** — a user catch-all with `otherwise` placed
   *before* the net clause (exactly as `agent/4` is ordered) does **not** block
   the committable net clause.

So the stall is not clause selection, not channel/writer head-matching, and not
`otherwise` ordering.

## Diagnosis

The remaining variable is **timing / wake-up**: in the probes the NetIn message
is present (via `merge`) when the goal is first reduced, so it commits
immediately. In the real scenario bob's agent **suspends first** (UserIn and
NetIn both unbound), and the friend-channel merge binds NetIn **afterward**. The
goal is not re-awoken on that binding — it appears to wait only on UserIn (the
first-tried clauses), not on NetIn, so a NetIn binding delivered later through
the friend-channel merge chain never re-schedules it.

The friend-channel path is a multi-level `merge` (the accepted channel's input is
merged into bob's existing NetIn), so the suspension dependency that must fire is
on a variable produced deep in that merge chain.

## What to investigate

Whether a goal that suspends because its *first-tried* clauses block on one
unbound argument (UserIn) correctly registers a wake-up dependency on the *other*
argument (NetIn) that a *later* clause matches — specifically when that other
argument is bound after suspension by a (possibly nested) `merge`. If the
dependency is only registered for the first blocking variable, a later-arriving,
committable message on a different argument is never processed.

## Reproducer

- Full: `glp_multiagent/test/scenario_single_isolate_test.dart` (phase 2 —
  `befriend_intro` never appears), trace `/private/tmp/scen-log.txt`.
- Isolation probes (all pass, narrowing the cause):
  `programs/tests/clause_select_probe.glp` + its test.

A minimal standalone reproducer that suspends the goal *before* delivering the
NetIn message through a nested merge was not yet achieved — the dataflow delivers
the message before suspension in every reduced isolation. That timing is the next
thing to force.

## Distinction from the `_w` / `receive/3` bug

Unrelated to `docs/madglp-w-writer-return-bug.md`: this is single-isolate, the
channel is local (no `_w`), and the failure is a non-wakeup of a suspended goal,
not a `receive/3` unification failure.
