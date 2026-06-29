# GLP/Dart UI-API — Implementation Brief

Handoff spec for a Claude Code session. Grounded in paper §7 (VEUI/UIVE), in the
`social_graph` mediator, and in the existing `glp_multiagent` app.

## Goal

Build **one generic Dart UI runtime** that turns any grassroots GLP app's mediator
into a working two-surface smartphone UI, plus a small **per-app manifest**. Prove
it on GSG (the social graph) by replacing the simulator panels in `lib/main.dart`,
reusing the existing multi-isolate harness as the test bed. No new Dart per app —
only a new manifest.

## Principle (paper §7.4)

The mediator is the contract. Its GLP types `UserCmd` (person -> mediator) and
`UserNotify` (mediator -> person) declare the whole UI protocol. Everything crossing
the Dart boundary is a **ground term or a request id `req(N)`**. Opaque values a
transaction waits on (Response writers, channels, cancel signals) stay GLP-side,
escrowed in the mediator's pending list, and never reach Dart. Therefore the Dart
runtime depends on no platform; only the manifest does.

## Architecture

Reuse, do not rewrite, the harness in `lib/`:
- isolate spawning (one agent per isolate), `IsolateRouter`, inter-agent routing;
- GLP loading (`typed_social_agent.glp`, `typed_ui_mediator.glp`, `play_ui_boot.glp`);
- the per-agent `commandPort` (SendPort) — the channel that carries ground `UserCmd`;
- agent output (`AgentOutput`) — the channel that carries ground `UserNotify`.

Replace only the per-agent view: today each agent is a raw tagged log + a free-text
command line (`AgentState.outputLog`, `inputController`). Swap that panel for the
two-surface UI (Outbox / Inbox, with Friends as the rendered state) produced by the
generic runtime from the GSG manifest. Keep three agents on screen (Alice, Bob,
Carol) so they drive each other's inboxes — that is the test harness.

## Generic runtime — three renderers

The runtime holds: the transport (send a ground `UserCmd`, receive ground
`UserNotify`), an inbox list, and an activity store (named lists / values / threads).
It renders strictly from the manifest:

1. **Outbox form** — each `UserCmd` with no `ReqId` is a composable action. Fields =
   constructor arguments (type -> input widget). Submit sends the ground term.
   Outcomes are not synchronous; they arrive later as notifies (non-blocking).
2. **Inbox card** — each `UserNotify` carrying a `ReqId` is a card. Its title/body
   come from the ground fields; its buttons are the answering `UserCmd`s, each built
   by slotting the card's `ReqId` (plus any extra ground args the manifest names).
3. **Activity rule** — each all-ground `UserNotify` updates state: append to a list,
   set a value, extend a thread keyed by a field, or remove an entry.

## Manifest schema (general — must also fit bonds and child-safe)

Declarative data, no Dart logic. Sketch:

```
Manifest {
  title: String
  commands: [ CommandDesc ]      // outbox forms (the free UserCmds)
  inbox:    [ InboxDesc ]        // one per ReqId-bearing UserNotify
  activity: [ ActivityDesc ]     // one per all-ground UserNotify
}

CommandDesc  { ctor; args:[ {name, type, label} ]; label }
InboxDesc    { notifyCtor; args:[names]; title; subtitle;
               answers:[ AnswerDesc ] }
AnswerDesc   { label; cmdCtor;                       // answering UserCmd
               fill:[ from card field | reqId | picker(list) | const ] }
ActivityDesc { notifyCtor; args:[names];
               effect: appendTo(list) | removeFrom(list)
                     | setValue(key) | extendThread(keyField) }
```

Future-proofing (do not implement now, but the schema must not preclude):
- **child-safe**: an `AnswerDesc` whose `cmdCtor` takes an extra ground arg
  (`approve_child_intro(OtherChild, MyChild, req)`) via `picker(myChildren)`, and a
  guardian addressee label — i.e. answers are not always bare yes/no.
- **bonds**: data-carrying accept (`accept_trade` filled from card fields),
  held-open cancel (`cancel_escrow` as an inbox answer that stays available),
  value/list activity (`balance_report` -> setValue, `minted` -> appendTo).

## GSG manifest (implement this instance)

From `programs/book/social_graph/typed_ui_mediator.glp` (verified):

`UserCmd`  = `decision(Decision,Constant,ReqId)`, `accept_intro(Constant,ReqId)`,
`reject_intro(Constant)`, `connect(Constant)`, `send(Constant,Constant)`,
`introduce(Constant,Constant)`.

`UserNotify` = `befriend(Constant,ReqId)`, `befriend_intro(Constant,Constant,ReqId)`,
`connected(Constant)`, `rejected`, `rejected(Constant)`, `received(Constant,Constant)`.

Mapping:
- Outbox commands: `connect(target)` (offer friendship); `introduce(p,q)`.
  (`send` is messaging — GrassApp, not GSG; omit from the GSG manifest for now.)
- Inbox cards:
  - `befriend(From, req)` -> "From wants to connect" -> answers
    Accept = `decision(yes, From, req)`, Decline = `decision(no, From, req)`.
  - `befriend_intro(From, Other, req)` -> "From introduces Other" -> answers
    Accept = `accept_intro(Other, req)`, Decline = `reject_intro(Other)`.
    NOTE: `reject_intro` takes **no** `ReqId` — the answer omits the id.
- Activity (Friends list + transient outcomes):
  - `connected(Who)` -> appendTo(friends).
  - `rejected(Who)` / `rejected` -> clear the matching outgoing offer (no friend added).
  - `received(From,Text)` -> messaging; ignore in GSG (GrassApp later).

Result on screen = the GSG figure: a Requests inbox (offers + introductions) and a
Friends list, with a `connect` FAB; introduce/unfriend as per-friend actions.

## Gaps / landmines (handle explicitly)

1. **Stale GLP dir.** `main.dart` `_defaultGlpDir = '../programs/typed_book/social_graph'`
   does not exist. Use `../programs/book/social_graph`. Verify the three `_glpFiles`
   load from there.
2. **No `unfriend` in this mediator.** The GSG figure shows unfriend, but
   `social_graph` mediator has no `unfriend`/`unfriended`. The child-safe mediator
   (`programs/book/cssg/typed_ui_mediator.glp`) already has both — port them
   (`unfriend(Target)` pass-through cmd; `unfriended(Who)` ground notify ->
   removeFrom(friends)). Either add them to `social_graph` for v1, or ship GSG v1
   without unfriend and note it. Decide with Udi.
3. **`reject_intro` asymmetry.** It carries no `ReqId` (unlike `accept_intro`). The
   answer builder must support answers that don't slot the id.
4. **No delivery ticks.** `send` is pass-through with no response in the mediator, so
   no ack stream exists. Ticks (GrassApp) are out of scope until the agent
   acknowledges the send loop.
5. **Transport shape.** Confirm in `isolate_protocol.dart` whether the isolate
   forwards the mediator's `UserNotify` as structured ground terms or as strings via
   `AgentOutput`. The runtime needs parsed terms; if only strings cross today, add a
   structured path or a parser at the boundary (still ground-terms-only in spirit).

## Validation (multi-isolate, no new backend)

Spawn Alice, Bob, Carol. Then:
- Alice `connect(bob)` -> Bob's inbox shows `befriend(alice, req)` -> Bob Accept ->
  `connected` lands on both -> both Friends lists update.
- Alice `introduce(bob, carol)` -> Carol's inbox shows `befriend_intro(alice, bob? )`
  (per agent logic) -> Accept -> connection forms.
- Decline paths: `decision(no,...)` and `reject_intro(...)` leave no friend.

If those three flows render and round-trip through the real mediator with zero
GSG-specific Dart in the runtime (only the manifest), §7.4's claim is demonstrated.

## Scope

This round: GSG only. GrassApp (group constructors, delivery-tick acks) and the
child-safe guardian answers are deliberately deferred; the schema above must leave
room for them.
