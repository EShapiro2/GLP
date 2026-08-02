# Social Graph Typing Status Report

Date: 2026-02-21

## Files

- `typed_social_agent.glp` — Agent, types, helpers (shared by all modes)
- `typed_actors.glp` — Actor scripts (Alice, Bob, Charlie)
- `typed_ui_mediator.glp` — UI mediator (between agent and Dart UI)
- `play_dglp_boot.glp` — Single-isolate boot (network3 + play)
- `play_madglp_boot.glp` — Multi-isolate boot (agent_init + actor dispatch)

## Current State

### Uncommitted changes

1. **`typed_social_agent.glp` line 20**: Changed `PendingValue ::= response(Response?) ; channel(Channel?) ; error.` to `PendingValue ::= response(Response) ; channel(Channel) ; error.` — removed `?` from fields. Needs discussion (see Bug 1).

2. **`typed_actors.glp`**: Added full type definitions (duplicated from `typed_social_agent.glp` since cross-file types are not supported). Changed procedure declarations from `Stream?/Stream` to `ActorIn?/ActorOut`. Actor code itself is unchanged from the archive version (no PendingValue wrappers).

3. **`cssg/typed_social_agent.glp` line 336**: Swapped `Ch?` and `Ch` in the intro clause to fix a mode mismatch caught by the type checker. This fix is correct and tested — all 317 REPL tests pass.

### Test results

- **REPL test suite**: 317/317 pass.
- **Multi-isolate tests** (`isolate_manager_test.dart`): 72 pass, 5 skipped, 0 failures. However, the play tests have no completion assertions — they just wait 5 seconds.
- **Single-isolate play** (`play.`): Suspends. Never ran to completion with the typed agent (see Bug 1).
- **Multi-isolate play**: Same — runs but no evidence of completion.

## Bug 1: PendingValue type mismatch between agent and actors

### The type definitions

```glp
Response ::= accept(Channel) ; no.
PendingValue ::= response(Response?) ; channel(Channel?) ; error.

AgentContent ::= befriend(Constant, Response)
               ; befriend_intro(Constant, Constant, Channel)
               ; ...

UserContent ::= decision(Decision, Constant, PendingValue)
              ; accept_intro(Constant, PendingValue)
              ; ...
```

### The problem

The agent sends `befriend(Constant, Response)` to the user. The user sends back `decision(Decision, Constant, PendingValue)`. But `Response` and `PendingValue` are different types. The actor receives a `Response` and must return a `PendingValue`. The `PendingValue` type wraps a `Response?` (reader) inside `response(...)`. This creates a type mismatch: the actor has a `Response`, the agent expects a `PendingValue` containing a `Response`.

The same applies to `befriend_intro` sending `Channel` vs `accept_intro` expecting `PendingValue` containing a `Channel`.

### The conflicting clauses

Agent sends befriend to user (line 195–198):
```glp
agent(Id, UserIn, [msg(Id1, intro(From, Resp))|NetIn], Outs) :-
    Id? =?= Id1? |
    lookup_send(person, msg(agent, person, befriend(From?, Resp?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?).
```

Agent receives decision from user (line 150–153):
```glp
agent(Id, [msg(person, Id1, decision(Dec, From, response(Resp?)))|UserIn], NetIn, Outs) :-
    Id? =?= Id1? |
    bind_response(Dec?, From?, Resp, Outs?, Outs1, NetIn?, NetIn1),
    agent(Id?, UserIn?, NetIn1?, Outs1?).
```

The agent sends `Resp?` (reader of a `Response`) in `befriend`. Then it expects `response(Resp?)` — a `PendingValue` wrapping a `Response` reader — in `decision`. The actor (or mediator) must wrap the received value inside `response(...)` to produce the `PendingValue`.

### The SRSW violation (in dGLP single-isolate mode)

In dGLP, the agent and actor share the heap. The agent sends `Resp?` (reader) to the actor via `befriend`. The actor receives this reader and must wrap it in `response(...)`. But wrapping a reader and having the agent extract it with another `?` creates two readers for the same variable — violating SRSW. The type checker catches this:

```
Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
```

### Why the mediator works

The mediator (`typed_ui_mediator.glp`) avoids the SRSW violation differently. It receives `Resp?` from the agent (line 60), then stores `response(Resp)` — note **no** `?` — using the clause-local writer (line 66). Later, when the user responds, it retrieves the stored PendingValue and sends `Resp?` to the agent (line 117). The mediator's clause-local variable provides a fresh writer/reader pair at each step.

Key mediator lines:
```glp
%% Store: Resp? reads from agent, response(Resp) stores the clause-local writer
[pending(req(N?), response(Resp)) | Ps?]        %% line 66

%% Retrieve and send: Resp? reads the stored writer
send(msg(person, Id?, decision(Dec?, From?, Resp?)), ...)  %% line 117
```

### What about the PendingValue type itself?

The original `PendingValue` type was `response(Response?) ; channel(Channel?)` with `?` on the fields, stating that the wrapped values are readers. This matched the mediator's storage of `response(Resp)` where `Resp` is bound to a reader from the agent. The mediator stores a reader-bound-writer.

After the uncommitted change, `PendingValue ::= response(Response) ; channel(Channel)` — no `?`. This needs further analysis to determine whether it correctly describes what both the mediator and the agent expect.

### The open question

The PendingValue wrapping was introduced in commit `071813b` to fix a "mediator double-reader bug." The commit message claims "All five execution modes verified working (dGLP, dGLP+mediator, madGLP headless, madGLP+mediator headless, Flutter interactive UI)." However, bisection testing shows the dGLP play (without mediator) has never run to completion in git history. The actors were never updated to match the PendingValue-wrapped agent interface.

The fundamental design question: should PendingValue exist at all, or should the agent use bare `Response` and `Channel` in `UserContent` (as the archive version did)? PendingValue serves the mediator path (where the mediator holds the value in escrow), but it creates an asymmetry: the agent sends `Response` out via `befriend` but expects `PendingValue` back via `decision`. Whether this asymmetry can be made type-safe requires rethinking the types, the agent code, or both.

## Bug 2 (fixed): cssg intro clause mode mismatch

In `cssg/typed_social_agent.glp` line 336, the intro clause had `Ch?` (reader) in the head where a writer was needed, and `Ch` (writer) in the body where a reader was needed. Fixed by swapping.

## Other observations

1. **Cross-file type visibility**: The type checker does not share type definitions across files. Types must be duplicated in each `.glp` file that needs them.

2. **`typed_actors.glp` type checking**: With full type definitions added, the type checker catches the SRSW violation in `accept_intro(Other?, Ch?)` and `decision(yes, From?, Resp?)` — confirming the PendingValue mismatch is a real type error, not just a runtime issue.

3. **`typed_ui_mediator.glp`**: Has its own PendingValue definition (line 45) — `PendingValue ::= response(Response?) ; channel(Channel?) ; error.` — still with `?`. This file was not modified. If PendingValue is changed in the agent, it should be checked here too.
