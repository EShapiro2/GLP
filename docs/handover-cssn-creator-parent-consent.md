# Handover: Fix child-child group-join to require creator's parent consent

## Scope

Single bug fix in `cssn_modules_v2/child_agent.glp`. No changes to types, prelude, or adult agent.

## The bug

Spec (CSSN paper Definition 4.2, item 2, "Join group (child, child creator)"): participants `{r, s, p, q}`, guards `{r, s, p, q}`, where `p` is creator `r`'s parent and `q` is joiner `s`'s parent. All four must be willing.

Current impl (`cssn_modules_v2/child_agent.glp`, clause "Child invites friend to group"):

```glp
child_agent(Id, ParentId, [msg('_user', Id1, invite_group(GroupId, Invitee))|UserIn], NetIn, Outs) :-
    Id? =?= Id1?, ground(GroupId?), ground(Invitee?),
    new_channel(InviteeCh, CreatorCh) |
    lookup_send(friend(Invitee?),
        msg(Id?, Invitee?, group_invite(GroupId?, InviteeCh?)), Outs?, Outs1),
    group_await_join(GroupId?, Invitee?, CreatorCh?, Result),
    inject_group_result(Result?, UserIn?, UserIn1),
    child_agent(Id?, ParentId?, UserIn1?, NetIn?, Outs1?).
```

The creator child sends the invite to the joiner directly without consulting its own parent. The joiner's path does consult the joiner's parent (via `group_consent_req` in the `~(From? =?= ParentId?)` clause). So only 3 of the 4 guards participate; the creator's parent is never involved. This is a gap against the spec.

## The fix

Route the creator child's invitation through its parent first (consent request), just like the joiner does on receipt. The creator child initiates `group_consent_req` to its parent, waits for a `yes` decision, and then proceeds with the existing send-invite / `group_await_join` flow.

### Step 1: Replace the "Child invites friend to group" clause

In `cssn_modules_v2/child_agent.glp`, replace the clause shown above with:

```glp
%% Child invites friend to group — first request parent consent, then proceed
child_agent(Id, ParentId, [msg('_user', Id1, invite_group(GroupId, Invitee))|UserIn], NetIn, Outs) :-
    Id? =?= Id1?, ground(GroupId?), ground(Invitee?),
    new_channel(InviteeCh, CreatorCh) |
    lookup_send(parent(ParentId?),
        msg(Id?, ParentId?, group_consent_req(GroupId?, Consent)), Outs?, Outs1),
    child_invite_consent_wait(Consent?, Id?, ParentId?, GroupId?, Invitee?,
        InviteeCh, CreatorCh?, UserIn?, NetIn?, Outs1?).
```

Note: `InviteeCh` is passed as a writer (not `?`) because the wait procedure will forward it on to the joiner when consent is granted. `CreatorCh?` is the reader side that feeds `group_await_join`.

### Step 2: Add `child_invite_consent_wait` procedure

Add as a local procedure in the same module (follow the existing style of `child_consent_wait`):

```glp
%% =============================================================================
%% CHILD_INVITE_CONSENT_WAIT/10 — blocking parent consent for child-initiated invites
%% =============================================================================

procedure child_invite_consent_wait(Decision?, Constant?, Constant?, GroupId?, Constant?,
    Channel(Stream(GroupSetupResponse), Stream(IntroContent)),
    Channel(Stream(IntroContent), Stream(GroupSetupResponse))?,
    Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?).

%% Parent approved — send invite to joiner friend, then await join
child_invite_consent_wait(yes, Id, ParentId, GroupId, Invitee,
        InviteeCh, CreatorCh, UserIn, NetIn, Outs) :-
    ground(Id?), ground(Invitee?) |
    lookup_send(friend(Invitee?),
        msg(Id?, Invitee?, group_invite(GroupId?, InviteeCh?)), Outs?, Outs1),
    group_await_join(GroupId?, Invitee?, CreatorCh?, Result),
    inject_group_result(Result?, UserIn?, UserIn1),
    child_agent(Id?, ParentId?, UserIn1?, NetIn?, Outs1?).

%% Parent rejected — nack the invite channel, carry on
child_invite_consent_wait(no, Id, ParentId, _, _,
        ch(_, [nack|[]]), _, UserIn, NetIn, Outs) :-
    child_agent(Id?, ParentId?, UserIn?, NetIn?, Outs?).
```

Placement: anywhere after the existing `child_consent_wait` procedure; the two are siblings.

### Step 3: No changes needed on the parent side

The adult agent's existing consent-handling clauses already handle a `group_consent_req` from any child:

- `agent(Id, UserIn, [msg(Child, Id1, group_consent_req(GroupId, Consent?))|NetIn], Outs)` — forwards to UI as `child_group_consent(Child, GroupId, Consent)`.
- `agent(Id, [msg('_user', Id1, approve_child_group(_, _, consent(yes)))|UserIn], NetIn, Outs)` — binds Consent := yes.
- Same for reject → binds Consent := no.

These clauses don't distinguish between a creator-parent consent and a joiner-parent consent; both flows use the same mechanism.

### Step 4: Village scenario impact

Currently, in the child-managed group act of the village scenario, Carol (child) invites Dave and Eve (children). With the fix, Carol's agent will first send `group_consent_req` to Alice (Carol's parent) for each invitation.

Alice's actor script (`village/alice.glp`) must be updated to answer these consent requests with `approve_child_group(Carol, group_id(Carol, kids_chat), consent(yes))` — she is already approving joiner-side consent for Carol's friends, so this is adding the symmetric approval on the creator side.

Check whether the village script currently handles an approval for Carol as the creator's child. If not, add it:

- For each `invite_group(kids_chat, Dave)` and `invite_group(kids_chat, Eve)` Carol issues, Alice receives a `child_group_consent(Carol, kids_chat, ReqId)` notification and must respond with `approve_child_group(Carol, kids_chat, ReqId)`.

Bob's actor script also needs to respond for Dave/Eve as joiner-parents (existing behaviour, unchanged).

### Step 5: Test

Run the village scenario via the REPL (`/Users/udi/Grassroots/GLP/CLAUDE.md` documents the REPL). Expected output changes for the child-managed group act: Carol's narrative should show a consent request to Alice before each invite is sent, and the group creation proceeds only after Alice approves.

All other acts (adult friendships, child introductions via parents, adult-managed group, leave, unfriend) should be unchanged.

## Out of scope

- Adult user-initiated unfriend: noted as a known limitation in the CSSN paper §7, not fixed in this task.
- Any paper changes: the CSSN paper's §5 already describes the fix target; it does not need updating as a result of this task.

## Acceptance

The village scenario compiles and runs; Alice's narrative includes approving Carol's two invite-consents; Carol's narrative shows the consent request/grant before sending each invite; Dave and Eve still join the group as before.
