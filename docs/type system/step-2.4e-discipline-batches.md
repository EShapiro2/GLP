# Step 2.4e Discipline-Only: Convert Remaining Files

**Date**: 2026-03-11
**Context**: 390/390 REPL tests pass. Monomorphic types removed from self.glp. These files are NOT exercised by the REPL test suite but should be converted for discipline.

**Rule**: Run `bash test/run_all_tests.sh` after each batch. 390 must pass. Commit after each batch.

**Principles**:
- Remove local imprecise copies like `MsgStream ::= [] ; [_ | MsgStream].` — use `Stream(Msg)` from root
- Remove local `Channel ::= ch(MsgStream?, MsgStream).` — use `Channel(MsgStream, MsgStream)` from root or define precise named channel types
- Replace bare `Stream` with precise instantiation or `Stream(X)` for generic procedures
- Replace bare `Channel` with precise channel types (FriendChannel, IntroChannel, etc.)
- Replace `_` in type definitions with concrete element types. Read the code to determine what elements the list/stream actually carries
- **Meta-interpreters are the exception** — `DumpList`, `ShotList`, `FailList`, `ResolvList` carry arbitrary goals/terms and may keep `_` with a comment
- **Never use `Channel(_, _)` or `Stream(_)` except in meta-interpreters**
- `-mode(system)` does NOT exempt from type checking

---

## Batch order (easiest first)

### Batch 1: `typed_book/social_graph/` (~15 files)

The biggest cluster. Most files share the same patterns:
- `Response ::= accept(Channel) ; no.` — needs FriendChannel
- `FriendEntry ::= (_, Stream).` or `(_, Stream?).` — needs `(Constant, Stream(FriendMsg))` or similar
- `AgentEntry ::= (_, Channel?).` — needs `(Constant, Channel(...)?)` with precise types
- `LookupResult ::= found(Stream) ; not_found.` — needs precise stream type
- `Control ::= control(Stream, Control).` — needs precise stream type
- `OutputEntry ::= output(String, Stream?).` / `output(OutputKey, Stream?).` — needs OutputStream

For each file: read the code to determine what messages flow through the streams/channels, then use the precise type. Many of these files define their own protocol types — use those.

Files (alphabetical):
- `agent.glp` — `FriendEntry ::= (Constant?, Stream).`
- `network.glp`, `network2.glp`, `network3.glp`, `network4.glp` — `AgentEntry ::= (_, Channel?).`
- `play_alice_bob_full.glp` — `Response`, `FriendEntry`
- `play_alice_bob_typed.glp` — `Response`, `FriendEntry`, `AgentEntry`
- `play_alice_bob_carol.glp` — `Response`
- `play_cold_call.glp` — `Response`, `FriendEntry`
- `play_typed_cold_call.glp` — `Response`, `FriendEntry`, `AgentEntry`
- `play_typed_routed.glp` — `Response`, `FriendEntry`
- `play_typed_simple.glp` — `Response`, `FriendEntry`
- `response_handling.glp`, `response_handling_unfolded.glp` — `Response`
- `social_graph_protocol.glp` — `Response`, `FriendEntry`
- `social_graph_protocol_v2.glp` — `Resp`, `FriendEntry`, `LookupResult`
- `stream_security.glp` — `Control ::= control(Stream, Control).`
- `typed_actors.glp` — `Response`, `PendingValue`
- `typed_ui_mediator.glp` — `Response`, `PendingValue`
- `ui_agent.glp` — `Response`, `FriendEntry`

Also local imprecise lists in play files:
- `play_4agents.glp`, `play_4agent.glp`, `play_alice_bob.glp`, `play_introduction.glp` — `FriendList ::= [] ; [_ | FriendList].`
- `play_4agent.glp` — `MsgList ::= [] ; [_ | MsgList].`
- `response_handling.glp`, `response_handling_unfolded.glp` — `MsgList ::= [] ; [_ | MsgList].`
- `streams.glp` — `TaggedList ::= [] ; [_ | TaggedList].`

### Batch 2: `typed_book/social_graph_simulated_ui/` (2 files)

- `typed_social_agent.glp` — `Response`, `PendingValue`, `IntroResult`, `OutputEntry` — all bare Channel/Stream
- `typed_ui_mediator.glp` — `Response`, `PendingValue` — bare Channel

Same treatment as the module versions. Add IntroChannel, FriendChannel, OutputStream types.

### Batch 3: `typed_book/cssg/` and `typed_book/cssn/` (3 files)

- `cssg/typed_social_agent.glp` — `IntroResult` with bare Channel
- `cssg/backup/typed_social_agent.glp` — `Response`, `IntroResult`, `OutputEntry` with bare Channel/Stream
- `cssn/typed_social_agent.glp` — `IntroResult`, `GroupJoinResult` with bare Channel

Same conversions as the module versions (IntroChannel, FriendChannel, GroupSetupCreatorChannel).

### Batch 4: `typed_book/constitutional_consensus/` (5 files)

All have `Block ::= block(Number, _, Stream).` and local imprecise lists.

Read the code to determine what the `_` and `Stream` carry. The `_` is likely a hash/content field, the `Stream` is a stream of transactions. Define concrete types. Also `State ::= state(Blocklace, Mode, Stream, Stream, _).`

Files: `consensus.glp`, `play_high_throughput.glp`, `play_low_throughput.glp`, `play_agents.glp`, `test_blocklace.glp`

Also: `ParticipantList`, `ResultList` with `_` — determine element types.

### Batch 5: `typed_book/cryptocurrencies/` (4 files)

All have `BlockStream ::= [] ; [_ | BlockStream].` and `BalanceList`, `RequestList` with `_`.

Read the code — blocks likely contain transaction records. Define `Block` type and use `Stream(Block)`.

Files: `gc.glp`, `play_payment.glp`, `play_mutual_credit.glp`, `play_redemption.glp`

### Batch 6: `typed_book/social_networks/` (7 files)

Local imprecise lists: `FollowerList`, `CmdList`, `MsgList`, `FriendList`, `UserList`, `PayloadList` — all `[] ; [_ | ...]`.

Also `Feed ::= feed(Stream).` and `Block ::= block(Msg, Stream).`

Files: `broadcast.glp` (already done), `direct_messaging.glp`, `feed.glp`, `feed_server.glp`, `follower_mgmt.glp`, `group_formation.glp`, `group_messaging.glp`, `interlaced_streams.glp`, `play_feed.glp`, `play_group_interlaced.glp`

### Batch 7: `multiagent/` (~8 files)

Local `MsgStream ::= [] ; [_ | MsgStream].` and `Channel ::= ch(MsgStream?, MsgStream).`

Files: `play_alice_bob.glp`, `play_cold_call_test.glp`, `play_friend_introduction.glp`, `play_introduction.glp`, `play_introduction_social_agent.glp`, `social_agent.glp`, `social_agent_v2.glp`, `ui_mediator.glp`, `old/social_agent_typed.glp`

### Batch 8: `typed_book/meta/` and `typed_book/recursive/` and `typed_book/streams/`

Meta-interpreters (exception — keep `_` with comment): `failsafe_meta.glp`, `control_meta.glp`, `snapshot_meta.glp`

Recursive: `flatten.glp`, `flatten_original.glp` — `NestedList ::= [] ; [_ | NestedList].` — genuinely heterogeneous, may need `_`

`translate.glp` — `WordList` — determine element type

`streams/producers_consumers/channels.glp` — `List ::= [] ; [_ | List].`

`cryptocurrencies/test_repayments.glp` — `PrefList`

---

## Approach for Claude Code

For each batch:
1. Read every file in the batch
2. Determine what each bare `Stream`, `Channel`, and `_` should become by reading the code
3. Apply the conversions
4. Run tests
5. Commit with message describing the batch
