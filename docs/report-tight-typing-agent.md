# Report: Tight Typing of `agent/4` in `typed_social_agent.glp`

**Date: 2026-02-21 (updated)**

## Summary

The `agent/4` procedure has been tightly typed with `UserInStream?` and `NetInStream?` replacing the generic `Stream?` arguments. The type checker now verifies the content of messages flowing through both streams and has caught a real mode bug in the intro-from-friend clause.

## Type Definitions Added

```glp
UserInMsg ::= msg(Constant, Constant, UserContent)
            ; intro_result(Constant, Channel)
            ; intro_rejected(Constant).
UserInStream ::= [] ; [UserInMsg | UserInStream].

NetInMsg ::= msg(Constant, NetColdCall)
           ; msg(Constant, Constant, FriendContent).
NetInStream ::= [] ; [NetInMsg | NetInStream].
```

Key design decisions:
- `UserInMsg` uses `UserContent` directly (which already includes `response(Response)` as an alternative), avoiding the need for a union alias type.
- `NetInMsg` has `msg/2` and `msg/3` alternatives — these are distinguishable because the type automaton uses functor/arity pairs as transition labels.
- `PendingValue` changed from `channel(Channel?)` to `channel(_)` — wildcard avoids mode conflicts from `send` PE expansion inside a consumed wrapper.

## Procedure Declaration Changes

| Procedure | Old | New |
|-----------|-----|-----|
| `agent/4` | `Constant?, Stream?, Stream?, OutputsList?` | `Constant?, UserInStream?, NetInStream?, OutputsList?` |
| `inject_msg/5` | `..., Stream?, Stream` | `..., UserInStream?, UserInStream` |
| `inject_intro_result/3` | `..., Stream?, Stream` | `..., UserInStream?, UserInStream` |
| `bind_response/7` | `..., Stream?, Stream` | `..., NetInStream?, NetInStream` |
| `handle_response/6` | `..., Stream?, Stream` | `..., NetInStream?, NetInStream` |

## New Procedure Added

`merge_net_in/3` — identical clauses to `merge/3`, typed for `NetInStream`:
```glp
procedure merge_net_in(NetInStream?, NetInStream?, NetInStream).
```

Needed because `merge/3` returns `Stream`, but `agent/4` now expects `NetInStream?`. Without subtyping or parametrized types, a typed copy is required. Used in `handle_response` and the `intro_result` agent clause.

## Bug Caught by the Type Checker

**File:** `typed_social_agent.glp`, line 336

**Clause:** Introduction from friend — pass channel to user via mediator

```glp
%% BUGGY (current code):
agent(Id, UserIn, [msg(From, Id1, intro(Other, Ch?))|NetIn], Outs) :-
    Id? =?= Id1?, ground(Other?) |
    lookup_send('_user', msg(agent, '_user', befriend_intro(From?, Other?, Ch)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?).
```

**Type checker error:**
```
Head of agent is not well-typed:
Inconsistent path: Variable mode mismatch: writer requires ↑ (produce), got ↓ (consume)
Path: ([|]/2, 0, input) → (msg/3, 1, input) → (intro/2, 3, input) → (Ch, 2, input)
```

**Explanation:** `FriendContent` defines `intro(Constant, IntroChannel)`. On the consumed NetIn stream, `IntroChannel` at position 2 of `intro` is consumed (mode ↓). The head should have a writer `Ch` to receive the channel value, with reader `Ch?` in the body to pass it to the mediator. The current code has it inverted: `Ch?` (reader) in the head, `Ch` (writer) in the body.

**Fix:**
```glp
agent(Id, UserIn, [msg(From, Id1, intro(Other, Ch))|NetIn], Outs) :-
    Id? =?= Id1?, ground(Other?) |
    lookup_send('_user', msg(agent, '_user', befriend_intro(From?, Other?, Ch?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?).
```

**Correction to earlier report:** The earlier draft (2026-02-21) incorrectly identified the `child_intro` clause as buggy. The child_intro clause (line 340) already has the correct moding: `Ch` (writer) in head, `Ch?` (reader) in body. The intro clause (line 336) is the one with inverted moding.

## Limitations

1. **`PendingValue` channel wildcard**: Using `channel(_)` instead of a specific channel type because the PE expansion of `send` inside a consumed channel wrapper creates mode conflicts that the type checker cannot resolve. Needs parametrized types or session types to type precisely.

2. **`intro_await_peer` session typing**: The intro channel transitions from carrying `IntroContent` (handshake) to carrying friend messages after the ack/nack. This protocol change cannot be expressed without session types. The procedure keeps generic `Channel?`.

3. **No subtyping / parametrized types**: `merge_net_in` is a verbatim copy of `merge` with different types. With subtyping or parametrized types, a single polymorphic merge would suffice.

## Files Modified

- `programs/typed_book/cssg/typed_social_agent.glp` — type definitions, procedure declarations, `merge_net_in` added
