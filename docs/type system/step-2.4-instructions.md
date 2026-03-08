# Step 2.4 Instructions: Parameterize self.glp and Remove Monomorphic Types

**Date**: 2026-03-08
**Context**: This is Step 7 in `current_plan.md`, corresponding to Step 2.4 in `parameterized-types-plan.md`.

**Goal**: Parameterize the prelude's generic procedure declarations in `programs/self.glp`, convert all downstream files that reference bare `Channel` or `Stream` to use precise types, and remove the old monomorphic type definitions.

**Prerequisite reading**: `docs/type system/typed-glp-handoff-2026-03-08.md`, `docs/type system/parameterized-types-plan.md`, `docs/type system/current_plan.md`.

---

## Sub-step order

Execute these in order. Run `bash test/run_all_tests.sh` after each sub-step. All 390 tests must pass after each one.

---

## 2.4.0 — Baseline

Run `bash test/run_all_tests.sh`. Confirm 390 pass. Commit baseline if there are uncommitted changes.

---

## 2.4a — Fix `programs/cssg_modules/self.glp`

Two bare `Channel` references need to become `FriendChannel` (already defined in this file). The channel in `intro_result` has completed the intro handshake and is used by the agent as a friend channel going forward.

**Edit 1** — In `IntroResult` type definition, change:
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
```
to:
```
IntroResult ::= intro_result(Constant, FriendChannel) ; intro_rejected(Constant).
```

**Edit 2** — In `UserInMsg` type definition, change the `intro_result` alternative from:
```
               ; intro_result(Constant, Channel)
```
to:
```
               ; intro_result(Constant, FriendChannel)
```

Run tests. Commit: `fix(types): replace bare Channel with FriendChannel in cssg_modules/self.glp`

---

## 2.4b — Fix `programs/cssn_modules/self.glp`

Multiple bare `Channel` references. Each one is either an intro channel or a group channel. Named types `IntroChannel` and `GroupChannel` are already defined in this file.

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

**Edit 4** — `IntroResult`: `Channel` → `FriendChannel` (same reasoning as CSSG — post-handshake, used as friend channel):
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
```
→
```
IntroResult ::= intro_result(Constant, FriendChannel) ; intro_rejected(Constant).
```

**Edit 5** — `UserInMsg`: two `Channel` references:
```
            ; intro_result(Constant, Channel)
```
→
```
            ; intro_result(Constant, FriendChannel)
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

Run tests. Commit: `fix(types): replace bare Channel with FriendChannel/GroupChannel in cssn_modules/self.glp`

---

## 2.4c — Fix `programs/social_graph_simulated_ui_modules/self.glp`

This module lacks named stream/channel subtypes. Add them following the CSSG convention, then replace bare `Channel` and `Stream` references.

**Add new type definitions** after the existing `IntroContent` definition and before `IntroResult`:

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

**Then replace bare type references:**

1. `Response ::= accept(Channel) ; no.` → `Response ::= accept(FriendChannel) ; no.`

2. `PendingValue ::= response(Response?) ; channel(Channel?) ; error.` → `PendingValue ::= response(Response?) ; channel(IntroChannel) ; error.`
   (Note: drop the `?` on IntroChannel to match CSSG convention — the pending value stores the channel itself, not a reader of it.)

3. `AgentContent`: `befriend_intro(Constant, Constant, Channel)` → `befriend_intro(Constant, Constant, IntroChannel)`

4. `NetColdCall ::= intro(Constant, Response).` — This already references `Response`, not bare `Channel`. No change needed.

5. `FriendContent`: `intro(Constant, Channel).` → `intro(Constant, IntroChannel).`

6. `IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).` → `IntroResult ::= intro_result(Constant, FriendChannel) ; intro_rejected(Constant).`
   (Note: After intro handshake completes, the result is a friend channel — matching how agent.glp uses it for friend communication.)

7. `OutputEntry ::= output(String, Stream?).` → `OutputEntry ::= output(String, OutputStream?).`

Run tests. Commit: `fix(types): add precise channel/stream types to social_graph_simulated_ui_modules/self.glp`

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

Remove these four lines (the monomorphic definitions, keeping the parameterized versions):

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

Files in `-mode(system)` still have their type definitions parsed as part of ancestor scope construction, so even system-mode files must not reference removed types.
