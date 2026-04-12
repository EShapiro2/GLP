# Step 2.4e: Remove Monomorphic Types — Batch Conversion Plan

**Date**: 2026-03-08
**Context**: Steps 2.4a–d are done (390/390 tests pass). Before removing monomorphic `Stream`, `Channel`, `DiffList` from root `self.glp`, every file that defines local imprecise copies or references the root monomorphic types must be converted to use parameterized types.

**Principle**: Every local `Stream ::= [] ; [_|Stream].` or `Channel ::= ch(Stream, Stream?).` should become `Stream(X)` or `Channel(In, Out)` from the root. Local types like `MsgStream ::= [] ; [Msg|MsgStream].` should become `Stream(Msg)`. This is the core of the tight typing discipline.

---

## Batch order

Execute one batch at a time. Run `bash test/run_all_tests.sh` after each batch. All 390 tests must pass. Commit after each batch.

---

## Batch 1: `programs/social_graph_simulated_ui_modules/` (incomplete from 2.4c)

Files: `ui/actors.glp`, `ui/mediator.glp`, `boot.glp`

**actors.glp**: All bare `Stream` in proc decls are the mediator's notify/cmd streams. All bare `Channel` in exported procs are the actor channel. These use the types already defined in self.glp:
- `procedure aliceN_wait_*(Stream?, Stream).` → use `UserNotifyStream?` / `UserCmdStream` (from self.glp) — BUT these types don't exist in SG-SIM self.glp. Unlike CSSG, SG-SIM doesn't have UserNotifyStream/UserCmdStream. The actors read/write UserCmd and UserNotify on their channels, but SG-SIM's self.glp doesn't define named streams for these. So the actors' internal procedures that process streams of notifications/commands need types.

**Decision needed**: Add `UserCmdStream`, `UserNotifyStream`, `UserChannel`, `ActorChannel` to SG-SIM self.glp (matching CSSG), OR use the generic channel types. Since we want tight typing, add the named types.

**mediator.glp**: `send_agent(_?, Channel?, Channel).` and `send_user(_?, Channel?, Channel).` need precise channel types. `ui_mediator(Constant?, Channel?, Channel?, PendingList?, Constant?)` needs precise types for the agent and user channels. Since SG-SIM currently has no `AgentChannel` equivalent (all messages go through generic channels), we need to decide whether to add full CSSG-style precise types or use `Channel(X, Y)` instantiations.

**boot.glp**: imported proc decls use bare `Channel?` and `Stream?` — must match whatever exports are declared in agent.glp, mediator.glp, actors.glp.

This batch requires adding type definitions to SG-SIM self.glp first. It's the most complex batch.

---

## Batch 2: `programs/typed_book/meta/` (3 files)

Files: `plain/plain_meta.glp`, `debugging/runtime_control_meta.glp`, `enhanced/snapshot_meta_cp.glp`

All have `procedure merge(Stream?, Stream?, Stream).` — change to parameterized `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`

`snapshot_meta_cp.glp` also has `Chain ::= chain(Stream?, Stream).` — needs domain-specific stream type or parameterized reference.

---

## Batch 3: `programs/typed_book/streams/` (5 files)

- `buffered_communication/bounded_buffer.glp`: `Buffer ::= Stream \ Stream?.` → use `DiffList(X)` or define domain-specific type
- `producers_consumers/observer.glp`: `NonEmptyList ::= [_ | Stream].` → `OpenStream(X)` from root
- `producers_consumers/distribute_indexed.glp`: `SendStream` — local, but check for `_`
- `producers_consumers/distribute_binary.glp`: `Req ::= req(Stream, _).` — needs precise stream type
- `producers_consumers/biased_merge.glp`: local `MsgStream` — already precise? Check for `_`
- `producers_consumers/merge_tree.glp`: `ListOfLists ::= [] ; [Stream | ListOfLists].` → needs `Stream(X)` instantiation
- `producers_consumers/mwm.glp`: `StreamCmd ::= stream(Stream) ; merge(StreamOfStreams).` — uses bare `Stream`
- `producers_consumers/cooperative_producers.glp`: `Control ::= control(Stream, Control).` — needs precise stream type
- `objects_monitors/many_counters.glp`: `CounterEntry ::= (_, Stream).` — needs precise stream type
- `objects_monitors/counter.glp`: local `CounterStream` — check for `_`

---

## Batch 4: `programs/typed_book/social_graph/` (~15 files)

The biggest cluster. Most files have:
- `Response ::= accept(Channel) ; no.` → `accept(FriendChannel)` or `accept(Channel(FriendStream, FriendStream))`
- `FriendEntry ::= (_, Stream).` → precise types
- `AgentEntry ::= (_, Channel?).` → precise types
- Local `merge` declarations with bare `Stream`

These files are the typed versions of the social graph examples from the book. Many define their own protocol types. Each file needs individual analysis.

---

## Batch 5: `programs/typed_book/social_graph_simulated_ui/` (2 files)

- `typed_social_agent.glp`: `Response`, `PendingValue`, `IntroResult`, `OutputEntry` — all bare `Channel`/`Stream`
- `typed_ui_mediator.glp`: `Response`, `PendingValue` — bare `Channel`

These are the monolithic (non-modular) versions. Same changes as the module versions but in a single file.

---

## Batch 6: `programs/typed_book/cssg/` and `typed_book/cssn/`

- `cssg/typed_social_agent.glp`: bare `Channel` in `IntroResult`
- `cssg/backup/typed_social_agent.glp`: bare `Channel` in `Response`, `IntroResult`
- `cssn/typed_social_agent.glp`: bare `Channel` in `IntroResult`, `GroupJoinResult`

Same conversions as the module versions.

---

## Batch 7: `programs/typed_book/constitutional_consensus/` (5 files)

All have `Block ::= block(Number, _, Stream).` — the third field is a stream of transactions. Need to determine the element type and use `Stream(X)`.

Also `State ::= state(Blocklace, Mode, Stream, Stream, _).` in 2 files.

---

## Batch 8: `programs/typed_book/cryptocurrencies/` (4 files)

All have `BlockStream ::= [] ; [_ | BlockStream].` — this is `Stream(_)` which we're eliminating. Need to determine what goes in blocks and use the precise element type.

---

## Batch 9: `programs/typed_book/bonds/` and `programs/typed_book/social_networks/`

- bonds: `NarrativeStream` in boot.glp, play12 files
- social_networks: `Feed ::= feed(Stream).`, `FollowerEntry ::= (_, Stream).`, `Block ::= block(Msg, Stream).`

---

## Batch 10: `programs/typed_book/recursive/` (2 files)

- `list_processing/nth.glp`: `NonEmptyList ::= [_ | Stream].` → `OpenStream(X)`
- `list_processing/member.glp`: same

---

## Batch 11: `programs/multiagent/` (~10 files)

These define local `Channel ::= ch(MsgStream?, MsgStream).` and `MsgStream ::= [] ; [_ | MsgStream].` Should use parameterized root types.

---

## Batch 12: `programs/paper/` (5 files)

The paper examples. `merge.glp`, `channel.glp`, `dl_append.glp`, `monitor.glp`, `coop_stream.glp`. These should be canonical examples of parameterized types.

---

## Batch 13: `programs/tests/` (4 files)

- `type_channel_test.glp`: local `Stream`, `Channel`, `Response`
- `typed/cssg_precise/typed_social_agent.glp`: bare `Stream`, `Channel`
- `test_agent_init.glp`: bare `Channel`

---

## Batch 14: Stragglers

- `cssg_modules/mad_boot.glp`: `procedure tee(Stream?, Stream, Stream).` → parameterized
- `typed_book/test_friend.glp`: `FriendEntry ::= friend(String, Stream?).`
- Any remaining files caught by final grep

---

## Final step

After all batches: remove monomorphic `Stream`, `OpenStream`, `Channel`, `DiffList` from `programs/self.glp`. Run tests. Commit.

Update `current_plan.md`.
