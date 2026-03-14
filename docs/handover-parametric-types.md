# Handover to Claude Code: Parametric Types Refactoring for CSSN v2

**Date:** 2026-03-12
**From:** Claude Chat
**Status:** Ready for implementation

## Context

The CSSN v2 modules (`programs/cssn_modules_v2/`) use manually-defined concrete stream and channel types instead of the parametric types available from the prelude. This violates the Tight Typing Discipline (§18 of the typed-glp-manual). The code should be refactored to use `Stream(X)` and `Channel(In, Out)` from the prelude throughout.

## Mandatory Reading

1. `/Users/udi/Grassroots/GLP/CLAUDE.md`
2. `/Users/udi/Grassroots/GLP/docs/DISCIPLINE.md`
3. `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md` — especially:
   - §17 (Parameterized Types) — how `Stream(X)`, `Channel(In, Out)` work
   - §18 (Tight Typing Discipline) — the project discipline requiring parametric types
   - §19 (Modules) — module structure, exports/imports, self.glp scope chain
4. `/Users/udi/Grassroots/GLP/docs/glp-cheat-sheet.md` — §12 (Modules)
5. `/Users/udi/Grassroots/GLP/programs/self.glp` — the prelude defining `Stream(X)`, `Channel(In, Out)`, `DiffList(X)`, and generic procedures `merge`, `send`, `receive`, `new_channel`

## The Problem

The file `cssn_modules_v2/self.glp` manually defines concrete stream and channel types that are redundant instantiations of the prelude's parametric types:

| Current (concrete, redundant) | Should be (parametric) |
|------|------|
| `FriendStream ::= [] ; [FriendMsg \| FriendStream].` | Remove; use `Stream(FriendMsg)` |
| `FriendChannel ::= ch(FriendStream, FriendStream?).` | Remove; use `Channel(Stream(FriendMsg), Stream(FriendMsg))` |
| `IntroStream ::= [] ; [IntroContent \| IntroStream].` | Remove; use `Stream(IntroContent)` |
| `IntroChannel ::= ch(IntroStream, IntroStream?).` | Remove; use `Channel(Stream(IntroContent), Stream(IntroContent))` |
| `GroupStream ::= [] ; [GroupMsg \| GroupStream].` | Remove; use `Stream(GroupMsg)` |
| `GroupChannel ::= ch(GroupStream, GroupStream?).` | Remove; use `Channel(Stream(GroupMsg), Stream(GroupMsg))` |
| `AgentToUserStream ::= [] ; [AgentToUserMsg \| AgentToUserStream].` | Remove; use `Stream(AgentToUserMsg)` |
| `MediatorToAgentStream ::= [] ; [MediatorToAgentMsg \| MediatorToAgentStream].` | Remove; use `Stream(MediatorToAgentMsg)` |
| `UserCmdStream ::= [] ; [UserCmd \| UserCmdStream].` | Remove; use `Stream(UserCmd)` |
| `UserNotifyStream ::= [] ; [UserNotify \| UserNotifyStream].` | Remove; use `Stream(UserNotify)` |
| `NarrativeStream ::= [] ; [NarrativeItem \| NarrativeStream].` | Remove; use `Stream(NarrativeItem)` |
| `GroupSetupResponseStream ::= [] ; [GroupSetupResponse \| GroupSetupResponseStream].` | Remove; use `Stream(GroupSetupResponse)` |
| `OutputStream ::= [] ; [OutputMsg \| OutputStream].` | Remove; use `Stream(OutputMsg)` |

Similarly for composite channel types:

| Current | Should be |
|------|------|
| `AgentChannel ::= ch(AgentToUserStream, MediatorToAgentStream?).` | Remove; use `Channel(Stream(AgentToUserMsg), Stream(MediatorToAgentMsg))` |
| `UserChannel ::= ch(UserCmdStream, UserNotifyStream?).` | Remove; use `Channel(Stream(UserCmd), Stream(UserNotify))` |
| `ActorChannel ::= ch(UserNotifyStream, UserCmdStream?).` | Remove; use `Channel(Stream(UserNotify), Stream(UserCmd))` |
| `GroupSetupInviteeChannel ::= ch(GroupSetupResponseStream, IntroStream?).` | Remove; use `Channel(Stream(GroupSetupResponse), Stream(IntroContent))` |
| `GroupSetupCreatorChannel ::= ch(IntroStream, GroupSetupResponseStream?).` | Remove; use `Channel(Stream(IntroContent), Stream(GroupSetupResponse))` |

The element types (`FriendMsg`, `IntroContent`, `GroupMsg`, `UserCmd`, `UserNotify`, `AgentContent`, `OutputMsg`, `NarrativeItem`, etc.) are NOT redundant — they define the message structure. Keep them.

## What To Do

### Step 1: Baseline

Run all tests. All 424 must pass. Commit.

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

### Step 2: Refactor self.glp

In `programs/cssn_modules_v2/self.glp`:

1. **Remove** all concrete stream type definitions listed above (the `XyzStream ::= [] ; [Xyz | XyzStream]` definitions).
2. **Remove** all concrete channel type definitions listed above (the `XyzChannel ::= ch(XyzStream, XyzStream?)` definitions).
3. **Keep** all element/message types (`FriendMsg`, `FriendContent`, `IntroContent`, `GroupMsg`, `GroupContent`, `UserContent`, `AgentContent`, `UserCmd`, `UserNotify`, `OutputMsg`, `OutputContent`, `NarrativeItem`, `GroupSetupResponse`, `NetColdCall`, `NetInMsg`, `UserInMsg`, etc.).
4. **Keep** all non-stream/channel types (`Response`, `Decision`, `Handshake`, `GroupId`, `PendingValue`, `PendingEntry`, `ReqId`, `IntroResult`, `GroupJoinResult`, `OutputKey`, `OutputEntry`, `OutputsList`).
5. **Update** `OutputEntry` — currently `OutputEntry ::= output(OutputKey, OutputStream?) ; group_entry(OutputKey, GroupStream?).` Change to `OutputEntry ::= output(OutputKey, Stream(OutputMsg)?) ; group_entry(OutputKey, Stream(GroupMsg)?).`
6. **Update** `OutputsList` — currently `OutputsList ::= [] ; [OutputEntry|OutputsList].` This is `Stream(OutputEntry)`. Change to use the parametric form or keep as-is if there is a type checker limitation (discuss with Udi).
7. **Update** `PendingList` — same consideration as OutputsList.

### Step 3: Update procedure declarations everywhere

Every procedure declaration that references a removed type name needs updating. Find all occurrences with grep.

For example, in `self.glp` helper procedures:
- `procedure lookup_send(OutputKey?, OutputMsg?, OutputsList?, OutputsList).` — OutputsList may need to stay as a named type if `Stream(OutputEntry)` causes issues. If OutputsList stays, that's fine — it's not a redundant stream-of-X definition, it's a named type alias.
- `procedure inject_msg(Response?, Constant?, Constant?, UserInStream?, UserInStream).` → `procedure inject_msg(Response?, Constant?, Constant?, Stream(UserInMsg)?, Stream(UserInMsg)).`

In `agent.glp`:
- `exported procedure agent(Constant?, UserInStream?, NetInStream?, OutputsList?).` → `exported procedure agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, OutputsList?).`

In `child_agent.glp`:
- `exported procedure child_agent(Constant?, Constant?, UserInStream?, NetInStream?, OutputsList?).` → `exported procedure child_agent(Constant?, Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, OutputsList?).`

In `boot.glp` — imported procedure declarations must match the exports:
- `imported procedure agent#agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, OutputsList?).`
- `imported procedure child_agent#child_agent(Constant?, Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, OutputsList?).`
- `imported procedure mediator#ui_mediator(Constant?, ...)` — update channel types similarly.
- All actor imports — update `ActorChannel?` to the parametric form.

In `ui/mediator.glp`:
- `exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).` → update channel types.

In `ui/actors.glp`:
- All actor procedure declarations — update `ActorChannel?`.

In `village/*.glp`:
- All actor procedure declarations — update `ActorChannel?` and `NarrativeStream`.

### Step 4: Resolve the merge duplication

Currently both `agent.glp` and `child_agent.glp` have local copies of `merge`, `setup_group_hub_side`, `setup_group_creator`, and `await_group_channel` because "the type checker cannot verify that concrete stream types are subtypes of the polymorphic Stream(X)."

After removing the concrete stream types and using `Stream(X)` everywhere, this limitation may no longer apply. Try moving `merge` and the merge-dependent procedures to `self.glp` (where the other shared helpers already live) and removing the local copies. If the type checker accepts this, it eliminates ~50 lines of duplicated code per module.

If the type checker still rejects it, keep the local copies but update their declarations to use parametric types.

### Step 5: Test after each file

After each file is modified, load the project in the REPL to verify it compiles:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '/Users/udi/Grassroots/GLP/programs/cssn_modules_v2\n:quit' | dart run bin/glp_repl.dart
```

After all files are updated, run the full test suite:

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

All 424 tests must pass. Then test fplay13 specifically:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '/Users/udi/Grassroots/GLP/programs/cssn_modules_v2\n:limit 5000000\nfplay13.\n:quit' | dart run bin/glp_repl.dart
```

Commit after all tests pass.

## Approach

Work incrementally. The safest order is:

1. Start with self.glp — remove the concrete stream/channel types, update any procedure declarations in that file that reference them.
2. Load the project — the type checker will report errors wherever the removed type names are still used in other files.
3. Fix each file based on the errors: agent.glp, child_agent.glp, boot.glp, mediator.glp, actors.glp, village/*.glp.
4. After everything compiles, try moving merge to self.glp (Step 4).
5. Run full tests.

## Key Rules

- All `.glp` code lives in `/Users/udi/Grassroots/GLP/programs/` — never in paper repos.
- Discuss changes with Udi before modifying `.glp` files.
- Test before and after every change.
- Commit after each successful phase.
- The refactoring must not change runtime behaviour — only type annotations and declarations change.
