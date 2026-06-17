# madGLP bug: cross-isolate `_w` writer does not survive escrow-and-return

**Date:** 2026-06-17
**Component:** madGLP runtime — cross-isolate writer serialization (`_w` mutual refs)
**Severity:** Soundness — a transmitted writer cannot be bound after a round-trip
across isolates, though the identical clauses work in one heap.

## One-line

A cold-call **response writer** that crosses isolates (serialized as `_w(bob,0)`)
is escrowed by the mediator and returned to the agent, but at the return hop the
`_w`-backed reader **fails head unification** where a local reader succeeds — so
`bind_response` never fires and the friendship never forms.

## Discriminator (the evidence)

The **same** real `agent/4` and `ui_mediator/5` clauses
(`programs/book/social_graph/typed_social_agent.glp`, `typed_ui_mediator.glp`),
with only the transport swapped:

- **Single heap** (`test/multiagent/single_heap_roundtrip_test.dart`, in-heap
  `pipe` transport, response variable is a local writer):
  ```
  bob_got(befriend(alice, req(1)))
  got(bob, connected(alice))      <- bob accepts, binds, connects
  got(alice, connected(bob))      <- writer reaches alice; she connects
  => COMPLETES, connected on both sides.
  ```

- **Two isolates** (`glp_multiagent/test/roundtrip_isolate_test.dart`, MAD
  transport, response variable is `_w`-backed):
  ```
  bob: < befriend(alice, req(1))            <- card appears
  bob: ui_mediator ... [pending(req(1), response(X13?))]
  bob: send(msg(_user, bob, decision(yes, alice, response(X13?))))
  bob: agent(bob, [msg(_user, bob, decision(yes, alice, response(X13?)))|_], ...)
         :- agent(bob, ...)                 <- CATCH-ALL eats the decision
  bob: suspended                            <- no bind_response, no connected
  => STRANDS.
  ```

Same clauses; the only difference is whether the response variable is local or
`_w`-backed. So the clause logic (polarity, escrow) is correct; the cross-isolate
writer-return is the fault.

## Where it fails

Agent decision clause (`typed_social_agent.glp:140`):
```prolog
agent(Id, [msg('_user', Id1, decision(Dec, From, response(Resp?)))|UserIn], NetIn, Outs) :-
    Id? =?= Id1? |
    bind_response(Dec?, From?, Resp, Outs?, Outs1, NetIn?, NetIn1),
    ...
```
Incoming message: `decision(yes, alice, response(X13?))` where `X13` is the
`_w(bob,0)`-backed reader. Head pattern `response(Resp?)` vs `response(X13?)`:
- local `X13` (single heap): unifies, clause commits, `bind_response` writes. ✓
- `_w`-backed `X13` (cross isolate): does **not** match — the catch-all
  (`agent(Id, [_|UserIn], ...)`, `otherwise`) commits instead, dropping the
  message. ✗

## Narrowed: the fault is `receive/3`, not `_w` head-matching in general

`programs/tests/mad_w_probe.glp` + `glp_multiagent/test/mad_w_probe_test.dart`
transmit a bare writer over MAD (so it deserializes to a `_w`-backed variable on
the receiver) and exercise four paths against the **same** `_w`:

| path | result |
|---|---|
| `_w` reader matched directly in a nested clause head `w(X?)` | matches |
| `_w` captured as reader, forwarded as writer through a **list**, re-matched as `wrapped(Y?)` | matches |
| `_w` escrowed in a structure across a **suspension**, re-matched after a `go` on another stream | matches |
| `_w` placed on a channel stream, pulled back with **`receive/3`** into `wrapped(Y?)` | **falls to `otherwise`** |

So a `_w`-backed reader is admissible in ordinary clause-head unification,
survives reader→writer→reader forwarding, and survives escrow across suspension.
It fails **only** through the `receive/3` channel kernel: matching a nested
**reader** sub-pattern (`wrapped(Y?)`) against a channel message that carries a
`_w` writer does not unify — the clause falls through to `otherwise`.

This is exactly the mediator's return hop:
`receive(msg('_user', Id, decision(Dec, From, response(Resp?))), UserCh?, UserCh1)`
— nested reader `response(Resp?)` over a `_w`-backed message — so the decision
is never delivered to the agent and the round-trip strands.

## What to investigate

The `receive/3` kernel's unification of a nested **reader** pattern against a
message containing a `_w` remote writer. Plain list head-matching of the same
value succeeds, so the divergence is inside `receive/3` (likely how it
dereferences / walks `_w` cells when binding sub-patterns), not in general head
unification.

Minimal reproducer: `mad_w_probe.glp` (the `bob_consumer` clause, `receive/3`
case) vs the list-pattern cases in the same file that succeed.

## Status of the UI `.glp`

Correct and **do not change** for this bug. The `.glp` work done alongside this
(shared `self.glp`, concrete agent-facing `Channel(Stream(AgentMsg),
Stream(UserMsg))`, §15B writer-forwarding in the cold-call/intro clauses) makes
both files typecheck strict and makes the single-heap round-trip complete. The
multi-isolate gate cannot go green until this madGLP bug is fixed.
