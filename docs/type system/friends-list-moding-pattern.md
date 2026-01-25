# Friends List Moding Pattern

**Date**: 2026-01-25  
**Status**: ACTIVE

This document describes the correct moding pattern for aggregate types that contain mutable nested state (like streams), using the FriendsList example.

---

## 1. The Problem

When a procedure consumes an aggregate and produces a new one, nested streams must remain writable for future operations. The naive approach breaks this:

```prolog
%% WRONG: Out becomes a reader after this
lookup_send_step(Key, Msg, [friend(K, Out)|Rest], [friend(K?, Out?)|Rest1?]) :-
```

When `FriendsList?` is consumed, each `FriendEntry` becomes `FriendEntry?`, and the Stream position flips to `Stream?` (reader). Storing `Out?` in the output means the stream is now a reader - unusable for future sends.

---

## 2. The Solution

Use the **reader/writer pair** to transfer the stream:

```prolog
%% CORRECT: Out? reads, Out writes - stream preserved as writer
lookup_send_step(Key, Msg, [friend(K, Out?)|Rest], [friend(K?, Out)|Rest1?]) :-
```

- Input: `Out?` (reader) - consumes the stream from the entry
- Output: `Out` (writer) - produces the stream in the new entry

This satisfies SRSW (one reader, one writer) and preserves writer capability.

---

## 3. Type Definitions

```prolog
FriendEntry ::= friend(String?, Stream).
FriendsList ::= [] ; [FriendEntry|FriendsList].
```

When consumed (`FriendEntry?`):
- Position 1: `String` (writer) - receive the name
- Position 2: `Stream?` (reader) - the stream flips to reader

When produced (`FriendEntry`):
- Position 1: `String?` (reader) - produce the name
- Position 2: `Stream` (writer) - must remain a writer

---

## 4. Complete Pattern

### lookup_send_step

```prolog
procedure lookup_send_step(String?, _?, FriendsList?, FriendsList).

%% Matching clause - send message to this entry
lookup_send_step(Key, Msg, [friend(K, [Msg?|Out1?])|Rest], [friend(K?, Out1)|Rest?]) :-
    Key? =?= K? | true.

%% Pass-through clause - skip this entry, preserve stream
lookup_send_step(Key, Msg, [friend(K, Out?)|Rest], [friend(K?, Out)|Rest1?]) :-
    otherwise |
    lookup_send_step(Key?, Msg?, Rest?, Rest1).

lookup_send_step(_, _, [], []).
```

### collect_friends

```prolog
procedure collect_friends(FriendsList?, Stream, FriendsList).

collect_friends([friend(user, Out?)|Rest], Names?, [friend(user, Out)|Rest1?]) :- 
    collect_friends(Rest?, Names, Rest1).
collect_friends([friend(net, Out?)|Rest], Names?, [friend(net, Out)|Rest1?]) :- 
    collect_friends(Rest?, Names, Rest1).
collect_friends([friend(Name, Out?)|Rest], [Name?|Names?], [friend(Name?, Out)|Rest1?]) :-
    ground(Name?) | collect_friends(Rest?, Names, Rest1).
collect_friends([], [], []).
```

### handle_response (adding new friend)

```prolog
procedure handle_response(Response?, _?, FriendsList?, FriendsList, Stream?, Stream).

handle_response(accept(ch(FIn, FOut?)), From, Fs, [friend(From?, FOut)|Fs?], In, In1?) :-
    ground(From?) |
    tag_stream(From?, FIn?, Tagged),
    merge(In?, Tagged?, In1).
```

Note: `FOut?` in head (reader from consumed Channel position 2) pairs with `FOut` in output (writer stored in new FriendEntry).

### agent_init (initial list construction)

```prolog
procedure agent_init(_?, Channel?, Channel?).

agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [friend(user, UserOut), friend(net, NetOut)]).
```

Note: `UserOut?` and `NetOut?` in head are readers; their paired writers `UserOut` and `NetOut` are stored in the list.

---

## 5. The Principle

This is an instance of the Head-Body Variable Flow Principle from `glp-programming-idioms.md`:

> **Data flowing from body to head (output construction):**
> - Head uses a **reader** (a "hole" to be filled)
> - Body uses the paired **writer** to produce the value

Extended to aggregate types: when consuming an aggregate and producing a new one, nested mutable state (streams) must use the reader/writer pair pattern to preserve writer capability across the transformation.

---

## 6. Key Insight

The type `FriendEntry ::= friend(String?, Stream)` declares that a FriendEntry **stores** a writer. When you consume a FriendsList and produce a new one, you must transfer that writer - not convert it to a reader.

The transfer uses SRSW's reader/writer pairing:
- Read the stream with `Out?` (consuming the old entry)
- Write the same stream with `Out` (producing the new entry)

This is not "passing through unchanged" - it's reading and writing the same logical value through its paired variables.
