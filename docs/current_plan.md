# Current Plan: Debug dGLP/madGLP Social Agent Play

Started: 2026-02-02

## Goal

Get the social agent play working in both:
1. **dGLP mode**: Single-process with GLP network switch (network3)
2. **madGLP mode**: Multi-isolate with send_to_net and index-0 serializer

## Current State

### The Problem

The dGLP version (`play_dglp.glp`) with `agent/4` using separate UserIn and NetIn streams **deadlocks**.

The original dGLP version (`play_alice_bob_charlie1.glp`) with `agent/3` using **merged** input stream works.

### Why agent/4 Deadlocks in dGLP

`agent/4` has separate stream arguments:
```prolog
procedure agent(_?, Stream?, Stream?, OutputsList?).
agent(Id, [Msg | UserIn], NetIn, Outputs) :- ...  % waits on UserIn
agent(Id, UserIn, [Msg | NetIn], Outputs) :- ...  % waits on NetIn
```

Each clause waits on only ONE stream. If a message arrives on NetIn but the goal is suspended waiting on UserIn, it never wakes up.

### Why agent/3 Works in dGLP

`agent/3` uses merge to combine streams:
```prolog
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [friend(user, UserOut), friend(net, NetOut)]).

procedure agent(_?, Stream?, FriendsList?).
agent(Id, [Msg | In], Friends) :- ...  % waits on merged stream
```

Messages from either source appear on the merged stream, so the agent wakes up regardless of which stream has a message.

## Key Question

**Can we use the same `agent/4` code for both dGLP and madGLP?**

Options:
1. **Use agent/3 with merge for both** - simpler, but madGLP currently uses agent/4 with separate streams
2. **Use agent/4 for madGLP only** - different agent code for each mode
3. **Add merge to dGLP boot** - wrap agent/4 with merge in dGLP mode

## Files

| File | Status | Notes |
|------|--------|-------|
| `programs/typed_book/social_graph/play_alice_bob_charlie1.glp` | Works | Original dGLP with agent/3 + merge |
| `programs/typed_book/social_graph/play_dglp.glp` | Deadlocks | dGLP with agent/4 (no merge) |
| `programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp` | Untested | madGLP with agent/4 |

## Next Steps

- [ ] 1. Decide: same agent code for both modes, or different?
- [ ] 2. If same: determine how to handle the merge requirement in dGLP
- [ ] 3. Test the chosen approach
- [ ] 4. Verify madGLP version works (requires Phase 3 implementation)

## Context

This is Phase 5 of the "Unify Cold-Calls with Global Links via Index-0 Serializer" plan.
See `/Users/udi/.claude/plans/fizzy-splashing-spark.md` for full context.
