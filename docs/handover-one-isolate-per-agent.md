# Handover: One Isolate Per Agent (Including Children)

## Problem

The current mad_boot files colocate parent + children in one isolate. This is wrong — every agent should have their own isolate, just as in real life every person has their own phone. Parent-child streams should cross isolate boundaries via madGLP, same as network streams.

## Root Cause

`send_to_net/1` only handles 2-arg `msg(Q, T)` (cold-call format on the `_net` stream). Parent-child communication uses 3-arg `msg(From, To, Content)` on separate output streams like `output(child(carol), ChildOut)`. There's no madGLP predicate to globalize these non-network output streams.

## Fix: Three Parts

### Part 1: Add `send_to_remote/2` to madGLP predicates

**File**: `glp_runtime/lib/engine/glp_engine.dart`

In the `_madPredicatesSource` constant string, add after `send_to_net/1`:

```glp
%% send_to_remote/2 - Globalize any output stream to a specific remote agent
%% Used for parent-child streams that cross isolate boundaries.
procedure send_to_remote(Constant?, Stream(_)?).
send_to_remote(Agent, [Msg | In]) :- ground(Agent?), ground(Msg?) |
    global_send(Msg?, '_w'(Agent?, 0), Agent?), send_to_remote(Agent?, In?).
send_to_remote(_, []).
```

This works exactly like `send_to_net` but takes an explicit destination agent instead of extracting it from the message format.

### Part 2: Rewrite boot files — one isolate per agent

**Directory**: `programs/cssn_modules_v2/mad_boot/`

Rewrite `mad_fplay4.glp` through `mad_fplay13.glp` so every agent (adult and child) gets its own `@agent` directive.

**Adult agent init** (same as before):
```glp
agent_init(alice, 4, _)@alice,
```

The init wires: actor + tee + agent + mediator + tee + output.
For adults with children, the child output stream is globalized:
```glp
agent_init(Id, PlayNum, NetIn) :-
    ground(Id?), ground(PlayNum?) |
    actor_dispatch(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    agent(Id?, AgentIn?, NetIn?,
          [output('_user', AgentToUser), output('_net', NetOut),
           output(child(carol), ChildOut)]),
    send_to_net(NetOut?),
    send_to_remote(carol, ChildOut?),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).
```

**Child agent init** (new pattern):
```glp
child_init(carol, alice, 4, _)@carol,
```

```glp
child_init(Id, Parent, PlayNum, NetIn) :-
    ground(Id?), ground(Parent?), ground(PlayNum?) |
    actor_dispatch(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    child_agent(Id?, Parent?, AgentIn?, NetIn?,
          [output('_user', AgentToUser), output(parent(Parent?), ParentOut)]),
    send_to_remote(Parent?, ParentOut?),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).
```

Note: Children have NO `_net` output (they communicate through their parent), so no `send_to_net` — only `send_to_remote(Parent?, ParentOut?)`.

**Updated isolate counts**:

| Play | Before | After | Agents |
|------|--------|-------|--------|
| fplay1-3 | 3 | 3 | alice, bob, charlie (no change — no children) |
| fplay4-7 | 2 | 4 | alice, bob, carol, dave |
| fplay8 | 2 | 2 | alice, bob (no change — no children) |
| fplay9-10 | 2 | 3 | alice, bob, dave |
| fplay11 | 3 | 6 | alice, bob, charlie, carol, dave, eve |
| fplay12 | 3 | 5 | alice, bob, charlie, dave, eve |
| fplay13 | 3 | 6 | alice, bob, frank, carol, dave, eve |

### Part 3: Update Dart test

**File**: `glp_runtime/test/multiagent/cssn_v2_isolate_test.dart`

The test should already work with the new boot files since IsolateManager handles any number of isolates. Just verify the expected isolate counts match the table above.

## Key Details

- `send_to_remote(Agent, Stream)` sends each ground element to Agent's network input at `_w(Agent, 0)` — the same endpoint that `send_to_net` targets. This means the receiving agent sees parent/child messages on its `NetIn` stream, which is exactly how the agent code already processes them (both cold-call and friend messages arrive on `NetIn`).

- The single-isolate `boot.glp` wires parent-child with `merge(AliceToCarol?, [], CarolFromAlice)`. In multi-isolate, this merge disappears — the messages go through IsolateManager.

- For fplay13 (village), Bob has TWO children (dave and eve). The boot needs:
  ```
  output(child(dave), DaveOut), output(child(eve), EveOut)
  ```
  with `send_to_remote(dave, DaveOut?)` and `send_to_remote(eve, EveOut?)`.

## Order of Operations

1. Add `send_to_remote/2` to `_madPredicatesSource` in `glp_engine.dart`
2. Rewrite mad_fplay4.glp (simplest child case: 4 agents)
3. Run its test to verify
4. Rewrite remaining boot files
5. Run all 13 tests
6. Run all 428 REPL tests to confirm no regression
7. Commit and push

## Reference Files

- `glp_runtime/lib/engine/glp_engine.dart` — `_madPredicatesSource` constant
- `programs/cssn_modules_v2/boot.glp` — single-isolate wiring (source of truth for per-play agent configs)
- `programs/cssn_modules_v2/mad_boot/mad_fplay1.glp` — working adult-only pattern
- `programs/cssn_modules_v2/agent.glp` — agent/4 output list structure
- `programs/cssn_modules_v2/child_agent.glp` — child_agent/5 output list structure
