# Step 2.4e — Test-Critical Batches

**Prerequisite**: Easy batches A–E done (commits 0cc84a82–f7a16b48). 390/390 tests passing.

**Goal**: Convert all test-exercised files that reference root monomorphic `Stream`/`Channel`/`DiffList`, so they survive the removal of those definitions.

**Rule**: Run `bash test/run_all_tests.sh` after each batch. 390 must pass. Commit after each.

**Note**: `-mode(system)` does NOT exempt from type checking. All files are type-checked.

---

## Batch F: `typed_book/streams/` — test-critical files

### typed_book/streams/producers_consumers/merge_tree.glp

`ListOfLists ::= [] ; [Stream | ListOfLists].` references bare root `Stream`.
This is a list of streams of any element type. Change to `Stream(_)` (meta-interpreter exception — merge_tree operates on arbitrary streams):
```
ListOfLists ::= [] ; [Stream(_) | ListOfLists].
```
`merge` and `merge_tree` already have parameterized proc decls (`Stream(X)`). No other changes needed.

### typed_book/streams/producers_consumers/mwm.glp

`StreamCmd ::= stream(Stream) ; merge(StreamOfStreams).` — bare `Stream`.
MWM operates on arbitrary streams. Same exception:
```
StreamCmd ::= stream(Stream(_)) ; merge(StreamOfStreams).
```
`mwm` already has `Stream(X)` in its proc decl. `mwm_copy` already has `Stream(X)?`. No other changes needed.

### typed_book/streams/producers_consumers/biased_merge.glp

`Msg ::= started ; halted ; _.` — the `_` is the issue.
This is a stream of mixed control messages and data. The `_` covers arbitrary data messages. Under tight typing, we should keep `_` only as a meta-interpreter exception since bmerge operates on arbitrary message streams. Leave `_` with a comment:
```
%% Msg includes any user data — bmerge is generic over message content
Msg ::= started ; halted ; _.
```
`MsgStream` is already precise (uses `Msg`, not `_`). No bare `Stream`/`Channel` refs. **No changes needed** — this file doesn't reference root monomorphic types and will survive removal.

### typed_book/streams/producers_consumers/distribute_binary.glp

`Req ::= req(Stream, _).` — bare `Stream` for address and `_` for content.
The address is a list of 0/1 bits (integers). The content is arbitrary. For tight typing:
```
Req ::= req(Stream(Number), _).
```
`ReqStream` is already precise. The `_` for content stays — distribute_binary is generic over content. Add comment:
```
%% Content (_) is generic — distribute_binary routes without inspecting content
Req ::= req(Stream(Number), _).
```

### typed_book/streams/producers_consumers/observer.glp

`NonEmptyList ::= [_ | Stream].` — bare root `Stream` and `_`.
Observer operates on arbitrary non-empty lists. Use `OpenStream(_)` from root:
```
procedure head_tail(OpenStream(_)?, _, Stream(_)).
```
Remove local `NonEmptyList` definition. The `_` stays (observer is generic).

### typed_book/streams/producers_consumers/cooperative_producers.glp

`Control ::= control(Stream, Control).` — bare root `Stream`.
The stream in Control carries arbitrary produced elements. This is generic:
```
Control ::= control(Stream(_), Control).
```
Proc decls already use `Stream(X)` — no other changes needed.

### typed_book/streams/buffered_communication/bounded_buffer.glp

`Buffer ::= Stream \ Stream?.` — bare root `Stream`.
Buffer is a difference list of arbitrary elements. Use `DiffList(_)`:
Actually, `Buffer` is used by bounded_buffer-specific send/receive, not the prelude `dl_append`. Keep `Buffer` as a named type but fix the bare `Stream`:
```
Buffer ::= Stream(_) \ Stream(_)?.
```
Proc decls already use `Stream(X)` — verify no other bare refs.

### typed_book/streams/objects_monitors/many_counters.glp

`CounterEntry ::= (_, Stream).` — bare `Stream` and `_` for key.
The key is a counter name (a constant). The stream carries CounterMsg:
```
CounterEntry ::= (Constant, Stream(CounterMsg)).
```
`CounterList` stays same (uses `CounterEntry`).
`Command ::= create(_) ; (_, CounterMsg).` — same issue with `_` for name:
```
Command ::= create(Constant) ; (Constant, CounterMsg).
```
`CommandStream` stays same.

Fix proc decl `procedure send(CounterList?, _?, CounterMsg?, CounterList).`:
```
procedure send(CounterList?, Constant?, CounterMsg?, CounterList).
```
And `procedure counter(Stream(X)?, Number?)` — already parameterized. But should be precise:
```
procedure counter(Stream(CounterMsg)?, Number?).
```

### typed_book/streams/objects_monitors/counter.glp

`CounterStream ::= [] ; [CounterMsg | CounterStream].` — local precise type. No bare root refs.
Change to use `Stream(CounterMsg)`:
```
procedure counter(Stream(CounterMsg)?, Number?).
```
Remove local `CounterStream` definition.

Run tests. Commit: `fix(types): convert typed_book/streams to parameterized types`

---

## Batch G: `typed_book/social_networks/broadcast.glp`

```
FollowerEntry ::= (_, Stream).
```
Follower key is a name (Constant), stream carries broadcast posts (any content):
```
FollowerEntry ::= (Constant, Stream(_)).
```
The `_` stays — broadcast is generic over message content.

Proc decl already uses `_?` for the post — that's correct (generic broadcast).

Run tests. Commit: `fix(types): convert typed_book/social_networks/broadcast.glp`

---

## Batch H: `typed_book/social_graph/typed_social_agent.glp`

This is a big file. Bare refs:
- `Response ::= accept(Channel) ; no.` 
- `PendingValue ::= response(Response) ; channel(Channel) ; error.`
- `AgentContent ::= ... ; befriend_intro(Constant, Constant, Channel) ; ...`
- `FriendContent ::= ... ; intro(Constant, Channel).`
- `OutputEntry ::= output(String, Stream?).`

This file has no intro handshake protocol (it's simpler than CSSG — accept_intro directly opens the channel without ack/nack). So the `Channel` in `Response` is a friend channel, and the `Channel` in `PendingValue` is also a friend channel.

Add type definitions (after existing content types, before OutputEntry):
```
%% Friend messages and channels
FriendMsg     ::= msg(Constant, Constant, FriendContent).
FriendStream  ::= [] ; [FriendMsg | FriendStream].
FriendChannel ::= ch(FriendStream, FriendStream?).

%% Output types
OutputMsg     ::= msg(Constant, Constant, AgentContent)
                ; msg(Constant, NetColdCall).
OutputStream  ::= [] ; [OutputMsg | OutputStream].
```

Then change:
- `Response ::= accept(Channel) ; no.` → `Response ::= accept(FriendChannel) ; no.`
- `PendingValue ::= response(Response) ; channel(Channel) ; error.` → `PendingValue ::= response(Response) ; channel(FriendChannel) ; error.`
- `AgentContent`: `befriend_intro(Constant, Constant, Channel)` → `befriend_intro(Constant, Constant, FriendChannel)`
- `FriendContent`: `intro(Constant, Channel).` → `intro(Constant, FriendChannel).`
- `OutputEntry ::= output(String, Stream?).` → `OutputEntry ::= output(String, OutputStream?).`

Also fix `lookup_send` and `add_output` proc decls to use precise types:
- `procedure lookup_send(String?, _?, ...)` → `procedure lookup_send(String?, OutputMsg?, ...)`  
- `procedure add_output(String?, Stream(X), ...)` → `procedure add_output(String?, OutputStream, ...)`

Run tests. Commit: `fix(types): convert typed_book/social_graph/typed_social_agent.glp`

---

## Batch I: `social_graph_simulated_ui_modules/` — actors.glp, mediator.glp, boot.glp

SG-SIM self.glp already has UserCmd, UserNotify, OutputMsg, OutputStream etc.
It's MISSING: UserCmdStream, UserNotifyStream, UserChannel, ActorChannel.

### Step 1: Add stream/channel types to self.glp

After the `UserNotify` definition and before `PendingEntry`, add:
```
UserCmdStream    ::= [] ; [UserCmd | UserCmdStream].
UserNotifyStream ::= [] ; [UserNotify | UserNotifyStream].
UserChannel      ::= ch(UserCmdStream, UserNotifyStream?).

%% Actor's view of the user channel (reversed)
ActorChannel ::= ch(UserNotifyStream, UserCmdStream?).
```

### Step 2: actors.glp

All exported procedures: `Channel?` → `ActorChannel?`
All internal procedures: `Stream?` / `Stream` → `UserNotifyStream?` / `UserCmdStream`

For example:
- `exported procedure alice1(Channel?).` → `exported procedure alice1(ActorChannel?).`
- `procedure alice1_wait_connected(Stream?, Stream).` → `procedure alice1_wait_connected(UserNotifyStream?, UserCmdStream).`

Apply this systematically to all 9 exported procedures and all ~27 internal procedures.

### Step 3: mediator.glp

`send_agent` and `send_user` need precise channel types. But the mediator operates on agent↔mediator channel and user↔mediator channel.

The SG-SIM self.glp doesn't have AgentChannel. For the mediator:
- Arg 2 (agent channel) carries a mix of messages. Without AgentToUserStream etc., use `Channel(_, _)` — OR add the precise stream types.

Since these modules are `-mode(system)` but still type-checked, and we want tight typing, add the agent stream/channel types to self.glp:
```
%% Agent ↔ mediator precise streams (for SG-SIM mediator)
AgentToUserMsg        ::= msg(Constant, Constant, AgentContent).
AgentToUserStream     ::= [] ; [AgentToUserMsg | AgentToUserStream].
MediatorToAgentMsg    ::= msg(Constant, Constant, UserContent).
MediatorToAgentStream ::= [] ; [MediatorToAgentMsg | MediatorToAgentStream].
AgentChannel          ::= ch(AgentToUserStream, MediatorToAgentStream?).
```

Then in mediator.glp:
- `procedure send_agent(_?, Channel?, Channel).` → `procedure send_agent(MediatorToAgentMsg?, AgentChannel?, AgentChannel).`
- `procedure send_user(_?, Channel?, Channel).` → `procedure send_user(UserNotify?, UserChannel?, UserChannel).`
- `exported procedure ui_mediator(Constant?, Channel?, Channel?, PendingList?, Constant?).` → `exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).`

### Step 4: boot.glp

Update imported proc decls to match the new exports:
- `imported procedure agent#agent(Constant?, Stream?, Stream?, OutputsList?).` — agent's export uses `Stream(X)?` — check what the actual export says. Looking at agent.glp: `exported procedure agent(Constant?, Stream?, Stream?, OutputsList?).` — this also needs conversion. But wait, agent's streams carry UserInMsg and NetInMsg...

Actually, looking at the SG-SIM agent.glp more carefully, it uses bare `Stream` in several proc decls that need conversion too. The agent.glp was partially converted in 2.4c (self.glp and intro_await_peer), but agent.glp's own proc decls like `inject_msg`, `inject_intro_result`, `bind_response`, `handle_response`, `agent` still have bare `Stream`.

This is a cascade: to fix boot.glp imported decls, we need to fix agent.glp exports, which means fixing agent.glp proc decls.

For SG-SIM agent.glp, the input streams are heterogeneous (user messages + intro results). Without named types, we'd need to add UserInMsg, UserInStream, NetInMsg, NetInStream to self.glp.

**Decision**: Add the full set of SG-SIM stream types matching CSSG:
```
%% Agent input types
UserInMsg    ::= msg(Constant, Constant, UserContent)
               ; intro_result(Constant, IntroChannel)
               ; intro_rejected(Constant).
UserInStream ::= [] ; [UserInMsg | UserInStream].

NetInMsg     ::= msg(Constant, NetColdCall)
               ; msg(Constant, Constant, FriendContent).
NetInStream  ::= [] ; [NetInMsg | NetInStream].
```

Then update agent.glp proc decls:
- `exported procedure agent(Constant?, Stream?, Stream?, OutputsList?).` → `exported procedure agent(Constant?, UserInStream?, NetInStream?, OutputsList?).`
- `procedure inject_msg(Response?, Constant?, Constant?, Stream?, Stream).` → `procedure inject_msg(Response?, Constant?, Constant?, UserInStream?, UserInStream).`
- `procedure inject_intro_result(IntroResult?, Stream?, Stream).` → `procedure inject_intro_result(IntroResult?, UserInStream?, UserInStream).`
- `procedure bind_response(Decision?, Constant?, Response, OutputsList?, OutputsList, Stream?, Stream).` → `procedure bind_response(Decision?, Constant?, Response, OutputsList?, OutputsList, NetInStream?, NetInStream).`
- `procedure handle_response(Response?, Constant?, OutputsList?, OutputsList, Stream?, Stream).` → `procedure handle_response(Response?, Constant?, OutputsList?, OutputsList, NetInStream?, NetInStream).`

Then boot.glp:
- `imported procedure agent#agent(Constant?, Stream?, Stream?, OutputsList?).` → `imported procedure agent#agent(Constant?, UserInStream?, NetInStream?, OutputsList?).`
- `imported procedure mediator#ui_mediator(Constant?, Channel?, Channel?, PendingList?, Constant?).` → `imported procedure mediator#ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).`
- All `imported procedure actors#*N(Channel?).` → `actors#*N(ActorChannel?).`

Run tests. Commit: `fix(types): complete SG-SIM module conversion to parameterized types`

---

## Batch J: `tests/typed/cssg_precise/typed_social_agent.glp`

Deferred from easy Batch D. Needs same treatment as cssg_modules:
- `IntroResult ::= intro_result(Constant, Channel)` → `IntroChannel`
- `UserInMsg ::= ... ; intro_result(Constant, Channel)` → `IntroChannel`
- `procedure intro_await_peer(Constant?, Channel?, IntroResult).` → `IntroChannel?`
- `OutputEntry ::= output(OutputKey, Stream?).` → `output(OutputKey, OutputStream?)`
  - Need to add `OutputStream` type or use existing output types
- `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`
- `procedure add_output(OutputKey?, Stream, ...)` → precise output stream type
- `PendingValue ::= ... ; channel(_)` → `channel(IntroChannel)`

This file is large but follows CSSG module patterns. Check if `OutputMsg`/`OutputStream` types need to be added.

Run tests. Commit: `fix(types): convert tests/typed/cssg_precise/typed_social_agent.glp`

---

## After all test-critical batches

Run full test suite. Then proceed to remove monomorphic types from root `self.glp` (the final 2.4e step), followed by discipline-only file conversions.
