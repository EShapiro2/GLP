# Step 2.4b — Revised: Fix `programs/cssn_modules/self.glp` and `agent.glp`

**Replaces the original 2.4b instructions in step-2.4-instructions.md**

## Background

The group invite protocol uses an asymmetric setup channel:
- Invitee→Creator stream: carries `IntroContent` (ack/nack) — same as intro handshake
- Creator→Invitee stream: carries `GroupSetupResponse` (group_channel(GroupChannel))

This is an asymmetric `Channel(In, Out)` with different types in each direction.

## New type definitions to add in `cssn_modules/self.glp`

Add these after the existing `GroupChannel` definition:

```
%% Group invite setup protocol (asymmetric channel)
%% Creator→Invitee stream carries the group channel after handshake
GroupSetupResponse       ::= group_channel(GroupChannel).
GroupSetupResponseStream ::= [] ; [GroupSetupResponse | GroupSetupResponseStream].

%% The two endpoints of the group invite setup channel
%% Invitee reads responses, writes ack/nack (IntroStream)
GroupSetupInviteeChannel ::= ch(GroupSetupResponseStream, IntroStream?).
%% Creator reads ack/nack (IntroStream), writes responses
GroupSetupCreatorChannel ::= ch(IntroStream, GroupSetupResponseStream?).
```

## Type definition changes in `cssn_modules/self.glp`

**Edit 1** — `FriendContent`: two `Channel` → `GroupSetupInviteeChannel`:
```
                ; group_invite(GroupId, Channel)
                ; group_invite_child(GroupId, Constant, Channel)
```
→
```
                ; group_invite(GroupId, GroupSetupInviteeChannel)
                ; group_invite_child(GroupId, Constant, GroupSetupInviteeChannel)
```

**Edit 2** — `AgentContent`: two `Channel` → `GroupSetupInviteeChannel`:
```
               ; group_invite(GroupId, Channel)
               ; group_invite_child(GroupId, Constant, Channel)
```
→
```
               ; group_invite(GroupId, GroupSetupInviteeChannel)
               ; group_invite_child(GroupId, Constant, GroupSetupInviteeChannel)
```

**Edit 3** — `PendingValue`: `group_channel(Channel)` → `group_channel(GroupSetupInviteeChannel)`:
```
               ; group_channel(Channel) ; error.
```
→
```
               ; group_channel(GroupSetupInviteeChannel) ; error.
```

**Edit 4** — `IntroResult`: `Channel` → `IntroChannel`:
```
IntroResult ::= intro_result(Constant, Channel) ; intro_rejected(Constant).
```
→
```
IntroResult ::= intro_result(Constant, IntroChannel) ; intro_rejected(Constant).
```

**Edit 5** — `GroupJoinResult`: `Channel` → `GroupSetupCreatorChannel`:
```
GroupJoinResult ::= group_join_ack(GroupId, Constant, Channel)
                  ; group_join_nack(GroupId, Constant).
```
→
```
GroupJoinResult ::= group_join_ack(GroupId, Constant, GroupSetupCreatorChannel)
                  ; group_join_nack(GroupId, Constant).
```

**Edit 6** — `UserInMsg`: three `Channel` references:
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
            ; group_join_ack(GroupId, Constant, GroupSetupCreatorChannel)
```

**Edit 7** — `OutputContent`: two `Channel` → `GroupSetupInviteeChannel`:
```
                ; group_invite(GroupId, Channel)
                ; group_invite_child(GroupId, Constant, Channel)
```
→
```
                ; group_invite(GroupId, GroupSetupInviteeChannel)
                ; group_invite_child(GroupId, Constant, GroupSetupInviteeChannel)
```

## Procedure declaration changes in `cssn_modules/agent.glp`

**Edit 8** — `intro_await_peer`:
```
procedure intro_await_peer(Constant?, Channel?, IntroResult).
```
→
```
procedure intro_await_peer(Constant?, IntroChannel?, IntroResult).
```

**Edit 9** — `group_await_join`:
```
procedure group_await_join(GroupId?, Constant?, Channel?, GroupJoinResult).
```
→
```
procedure group_await_join(GroupId?, Constant?, GroupSetupCreatorChannel?, GroupJoinResult).
```

**Edit 10** — `await_group_channel`: the third arg `Stream?` carries GroupSetupResponseStream (what the invitee reads from the setup channel):
```
procedure await_group_channel(Constant?, GroupId?, Stream?, UserInStream?, NetInStream?, OutputsList?).
```
→
```
procedure await_group_channel(Constant?, GroupId?, GroupSetupResponseStream?, UserInStream?, NetInStream?, OutputsList?).
```

## Verification

Run `bash test/run_all_tests.sh`. All 390 tests must pass.

Commit: `fix(types): replace bare Channel with precise types in cssn_modules`
