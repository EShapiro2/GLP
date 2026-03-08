# Step 2.4 Instructions: Parameterize self.glp and Remove Monomorphic Types

**Date**: 2026-03-08 (revised after failed attempt with FriendChannel)
**Context**: This is Step 7 in `current_plan.md`, corresponding to Step 2.4 in `parameterized-types-plan.md`.

**Goal**: Parameterize the prelude's generic procedure declarations in `programs/self.glp`, convert all downstream files that reference bare `Channel` or `Stream` to use precise types, and remove the old monomorphic type definitions.

**Prerequisite reading**: `docs/type system/typed-glp-handoff-2026-03-08.md`, `docs/type system/parameterized-types-plan.md`, `docs/type system/current_plan.md`.

---

## Key design decision: IntroResult carries IntroChannel

The channel inside `intro_result(Constant, ...)` is constructed by `intro_await_peer` from an IntroChannel's streams. The streams have type IntroStream (the tail after stripping the ack is still IntroStream by the recursive type definition). So the result channel is `ch(IntroStream, IntroStream?)` = `IntroChannel`.

The agent later treats this channel as a friend channel (merging FIn into NetIn, etc.), but that's a protocol transition at runtime. Since agent modules are `-mode(system)`, the clause-level type mismatch isn't checked. The type definitions need to be consistent with the *producer* (`intro_await_peer`), not the *consumer* (`agent`).

Also: `intro_await_peer`'s procedure declaration must change from bare `Channel?` to `IntroChannel?`.

---

## Sub-step order

Execute these in order. Run `bash test/run_all_tests.sh` after each sub-step. All 390 tests must pass after each one.

---

## 2.4.0 — Baseline

Run `bash test/run_all_tests.sh`. Confirm 390 pass. Commit baseline if there are uncommitted changes.

---

## 2.4a — Fix `programs/cssg_modules/self.glp` and `programs/cssg_modules/agent.glp`

**In `cssg_modules/self.glp`** — two bare `Channel` references → `IntroChannel` (already defined in this file):

**Edit 1** — `IntroResult` type definition:
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
```
→
```
IntroResult ::= intro_result(Constant, IntroChannel) ; intro_rejected(Constant).
```

**Edit 2** — `UserInMsg`, the `intro_result` alternative:
```
               ; intro_result(Constant, Channel)
```
→
```
               ; intro_result(Constant, IntroChannel)
```

**In `cssg_modules/agent.glp`** — update `intro_await_peer` proc declaration:
```
procedure intro_await_peer(Constant?, Channel?, IntroResult).
```
→
```
procedure intro_await_peer(Constant?, IntroChannel?, IntroResult).
```

Run tests. Commit: `fix(types): replace bare Channel with IntroChannel in cssg_modules`

---

## 2.4b — Fix `programs/cssn_modules/self.glp` and agent.glp

**In `cssn_modules/self.glp`** — replace bare `Channel` with `IntroChannel`, `FriendChannel`, or `GroupChannel` as appropriate:

**Edit 1** — `FriendContent`: two `Channel` → `GroupChannel`:
```
                ; group_invite(GroupId, Channel)
                ; group_invite_child(GroupId, Constant, Channel)
```
→
```
                ; group_invite(GroupId, GroupChannel)
                ; group_invite_child(GroupId, Constant, GroupChannel)
```

**Edit 2** — `AgentContent`: two `Channel` → `GroupChannel`:
```
               ; group_invite(GroupId, Channel)
               ; group_invite_child(GroupId, Constant, Channel)
```
→
```
               ; group_invite(GroupId, GroupChannel)
               ; group_invite_child(GroupId, Constant, GroupChannel)
```

**Edit 3** — `PendingValue`: `group_channel(Channel)` → `group_channel(GroupChannel)`:
```
               ; group_channel(Channel) ; error.
```
→
```
               ; group_channel(GroupChannel) ; error.
```

**Edit 4** — `IntroResult`: `Channel` → `IntroChannel`:
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
```
→
```
IntroResult ::= intro_result(Constant, IntroChannel) ; intro_rejected(Constant).
```

**Edit 5** — `UserInMsg`: two `Channel` references:
```
            ; intro_result(Constant, Channel)
```
→
```
            ; intro_result(Constant, IntroChannel)
```
and:
```
            ; group_join_ack(GroupId, Constant, Channel)
```
→
```
            ; group_join_ack(GroupId, Constant, GroupChannel)
```

**Edit 6** — `OutputContent`: two `Channel` → `GroupChannel`:
```
                ; group_invite(GroupId, Channel)
                ; group_invite_child(GroupId, Constant, Channel)
```
→
```
                ; group_invite(GroupId, GroupChannel)
                ; group_invite_child(GroupId, Constant, GroupChannel)
```

**In `cssn_modules/agent.glp`** — if it has an `intro_await_peer` declaration with bare `Channel?`, update it to `IntroChannel?`. (Check the file — it may differ from CSSG.)

Run tests. Commit: `fix(types): replace bare Channel with precise types in cssn_modules`

---

## 2.4c — Fix `programs/social_graph_simulated_ui_modules/self.glp` and agent.glp

This module lacks named stream/channel subtypes. Add them, then replace bare references.

**Add new type definitions** in `self.glp` after the existing `IntroContent` definition and before `IntroResult`:

```
%% Intro channel (handshake protocol)
IntroStream  ::= [] ; [IntroContent | IntroStream].
IntroChannel ::= ch(IntroStream, IntroStream?).

%% Friend messages and channels
FriendMsg     ::= msg(Constant, Constant, FriendContent).
FriendStream  ::= [] ; [FriendMsg | FriendStream].
FriendChannel ::= ch(FriendStream, FriendStream?).

%% Output types
OutputContent ::= befriend(Constant, Response?)
                ; befriend_intro(Constant, Constant, IntroChannel)
                ; connected(Constant)
                ; rejected
                ; rejected(Constant)
                ; received(Constant, Constant)
                ; response(Response)
                ; text(Constant)
                ; intro(Constant, IntroChannel).
OutputMsg     ::= msg(Constant, Constant, OutputContent)
                ; msg(Constant, NetColdCall).
OutputStream  ::= [] ; [OutputMsg | OutputStream].
```

**Then replace bare type references in `self.glp`:**

1. `Response ::= accept(Channel) ; no.` → `Response ::= accept(FriendChannel) ; no.`

2. `PendingValue ::= response(Response?) ; channel(Channel?) ; error.` → `PendingValue ::= response(Response?) ; channel(IntroChannel) ; error.`

3. `AgentContent`: `befriend_intro(Constant, Constant, Channel)` → `befriend_intro(Constant, Constant, IntroChannel)`

4. `FriendContent`: `intro(Constant, Channel).` → `intro(Constant, IntroChannel).`

5. `IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).` → `IntroResult ::= intro_result(Constant, IntroChannel) ; intro_rejected(Constant).`

6. `OutputEntry ::= output(String, Stream?).` → `OutputEntry ::= output(String, OutputStream?).`

**In `social_graph_simulated_ui_modules/agent.glp`** — update `intro_await_peer` proc declaration:
```
procedure intro_await_peer(Constant?, Channel?, IntroResult).
```
→
```
procedure intro_await_peer(Constant?, IntroChannel?, IntroResult).
```

Run tests. Commit: `fix(types): add precise types to social_graph_simulated_ui_modules`

---

## 2.4d — Parameterize root `programs/self.glp` procedure declarations

Change these procedure declarations:

1. `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`

2. `procedure send(_?, Channel?, Channel).` → `procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).`

3. `procedure receive(_, Channel?, Channel).` → `procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).`

4. `procedure new_channel(Channel, Channel).` → `procedure new_channel(Channel(X, Y), Channel(Y, X)).`

5. `procedure dl_append(DiffList?, DiffList?, DiffList).` → `procedure dl_append(DiffList(X)?, DiffList(X)?, DiffList(X)).`

6. `procedure dl_to_list(DiffList?, Stream).` → `procedure dl_to_list(DiffList(X)?, Stream(X)).`

**Do NOT change:**
- Builtin declarations that genuinely accept any term: `ground(_?)`, `=(_?, _)`, `=?=(_?, _?)`, etc.
- Arithmetic: `:=` and arithmetic guards
- MWM procedures (untyped section)
- System predicates (`_output`, `_send`, etc.)

Run tests. Commit: `feat(types): parameterize prelude procedure declarations in self.glp`

---

## 2.4e — Remove monomorphic type definitions from root `programs/self.glp`

Remove these lines (the monomorphic definitions, keeping the parameterized versions):

```
Stream ::= [] ; [_|Stream].
OpenStream ::= [_|Stream].
DiffList ::= Stream \ Stream?.
```

and:

```
Channel ::= ch(Stream, Stream?).
```

Also remove the comments labelling them as "monomorphic — kept for backward compatibility".

The parameterized definitions remain:
```
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
DiffList(X) ::= Stream(X) \ Stream(X)?.
Channel(In, Out) ::= ch(In, Out?).
```

Run tests. Commit: `feat(types): remove monomorphic Stream/Channel/DiffList from self.glp`

---

## After all sub-steps

Run full test suite one final time. All 390 tests must pass.

Update `docs/type system/current_plan.md`: mark Step 7 as done, advance to Step 8 (Step 2.5).

Offer to push.

---

## Troubleshooting

If tests fail after 2.4e, the most likely cause is a `.glp` file somewhere that still references bare `Channel`, `Stream`, `OpenStream`, or `DiffList` without type parameters. Search for these in `programs/` (excluding `archive/`) and convert them.

Files in `-mode(system)` still have their type definitions parsed as part of ancestor scope construction, so even system-mode files must not reference removed types. Procedure declarations in system-mode files may also be validated against the type environment.
