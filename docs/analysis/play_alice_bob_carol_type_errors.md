# Type Error Analysis: play_alice_bob_carol.glp

## Current Type Definitions

```prolog
FriendEntry ::= friend(String?, MsgStream).
FriendsList ::= [] ; [FriendEntry|FriendsList].
```

## Procedure Declarations

```prolog
procedure lookup_send_step(String?, _?, FriendsList?, FriendsList).
procedure add_friend(_?, Stream, FriendsList?, FriendsList).
procedure add_friend_and_notify(_?, Stream, FriendsList?, FriendsList).
```

## Error Summary

### lookup_send_step (first clause, line 72)

```prolog
lookup_send_step(Key, Msg, [friend(K, [Msg?|Out1?])|Rest], [friend(K?, Out1)|Rest?]) :-
    Key? =?= K? | true.
```

Errors:
- `K?` reader in input position — but FriendEntry has `String?` which expects reader in consume context
- `Msg`, `Out1` writers in input stream position — but MsgStream is writer type being consumed
- `K` writer in output's friend name position — but type says `String?` (reader)
- `Out1?` reader in output stream position — but MsgStream is writer type being produced

### add_friend (line 94)

```prolog
add_friend(Name, Out?, Fs, [friend(Name?, Out)|Fs?]).
```

Errors:
- `Name` writer in output friend's name position — type expects `String?` (reader)
- `Out?` reader in output friend's stream position — type expects `MsgStream` (writer)

### add_friend_and_notify (line 116)

```prolog
add_friend_and_notify(From, FOut?, Fs, [friend(From?, FOut)|Fs1?]) :-
    ground(From?) |
    lookup_send(user, msg(agent, user, connected(From?)), Fs?, Fs1).
```

Same pattern as add_friend.

### agent_init (line 203)

```prolog
agent(Id?, In?, [friend(user, UserOut), friend(net, NetOut)]).
```

- `UserOut`, `NetOut` are writers in stream positions where type expects... what exactly?

## Core Problem

The `FriendEntry` type is `friend(String?, MsgStream)`:
- Position 1: `String?` — reader mode
- Position 2: `MsgStream` — writer mode (produces messages)

When `FriendsList?` is consumed (input), we're consuming `FriendEntry` values. But `MsgStream` is a writer type — we cannot consume a writer.

When `FriendsList` is produced (output), we're producing `FriendEntry` values. The type expects `MsgStream` (writer) in position 2.

But the clauses do the opposite:
- Input: put `[Msg?|Out1?]` — readers where writer expected
- Output: put `Out1` — writer... this should match?

The issue is more subtle. The mode of a subterm depends on:
1. The mode declared in the type (String? vs MsgStream)
2. The context (input vs output position in procedure)

## Mode Algebra Question

When `FriendEntry` appears in:
- **Input position** (FriendsList?): We consume the entry. What is the effective mode of the `MsgStream` inside?
- **Output position** (FriendsList): We produce the entry. What is the effective mode of the `MsgStream` inside?

The fundamental tension: We need the stream to be **writable** so we can send messages to friends, but the type system sees `MsgStream` as a thing to be produced/consumed based on context.

## Possible Solutions

### Option A: Use MsgStream? in the type

```prolog
FriendEntry ::= friend(String?, MsgStream?).
```

This makes the stream always a reader. Then use reader/writer pairs in clauses.

### Option B: Revisit the moding pattern

Apply the Head-Body Variable Flow Principle. The stream is mutable state being threaded through — perhaps the type should reflect that streams are always readers being passed around, with writes happening through the reader/writer pair pattern.

## Questions for Discussion

1. What does `MsgStream` vs `MsgStream?` mean semantically in a type definition?
2. When FriendEntry is consumed, what happens to the mode of its MsgStream component?
3. Is the correct pattern to always store `MsgStream?` (readers) and obtain writers through unification?
