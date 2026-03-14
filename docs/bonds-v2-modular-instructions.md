# Bonds V2: Modular + Parametric Types

**Date:** 2026-03-12
**Workstream:** Grassroots Bonds — modular rewrite with parametric types
**Source:** `programs/typed_book/bonds/` (read-only reference)
**Target:** `programs/bonds_v2/` (new directory, sibling to `cssn_modules_v2`)

## Mandatory Startup

1. Read `/Users/udi/Grassroots/claude.md`
2. Read `/Users/udi/Grassroots/GLP/CLAUDE.md`
3. Read `/Users/udi/Grassroots/GLP/docs/DISCIPLINE.md`
4. Read `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md` — especially Sections 17 (Parameterized Types), 18 (Tight Typing Discipline), 19 (Modules)
5. Read `/Users/udi/Grassroots/GLP/docs/glp-cheat-sheet.md` — especially Section 12 (Modules)
6. Read this file completely.
7. Read the CSSN v2 code as the reference model: `programs/cssn_modules_v2/self.glp`, `programs/cssn_modules_v2/agent.glp`, `programs/cssn_modules_v2/boot.glp`
8. Read the current bonds code: `programs/typed_book/bonds/agent.glp`, `programs/typed_book/bonds/mediator.glp`, `programs/typed_book/bonds/actors.glp`, `programs/typed_book/bonds/boot.glp`, `programs/typed_book/bonds/play12/self.glp` (if it exists), and the play12 actor files.
9. STOP and wait for user direction.

## Overview

This is a mechanical restructuring of the existing bonds code into:
1. **Parametric types** — all stream and channel types use `Stream(X)` and `Channel(In, Out)` from the prelude. No standalone stream type definitions.
2. **Module structure** — following CSSN v2 conventions exactly.

**No logic changes.** The agent, mediator, actors, and boot logic are identical to the current code. Only types, module declarations, and cross-module call syntax change.

## Design Principles

### Parametric Types

**Rule:** Every type of the form `XxxList ::= [] ; [Xxx | XxxList].` or `XxxStream ::= [] ; [Xxx | XxxStream].` is ELIMINATED. Use `Stream(Xxx)` from the prelude instead.

**Rule:** Every channel type of the form `XxxChannel ::= ch(XxxStream, YyyStream?).` is ELIMINATED. Use `Channel(Stream(Xxx), Stream(Yyy))` from the prelude instead.

**Named parametric aliases for readability:** Where channel types would be excessively verbose in procedure declarations, define named aliases that use parametric types internally. For example:

```prolog
%% Named aliases using parametric types — for readability only
FriendChannel ::= ch(Stream(FriendMsg), Stream(FriendMsg)?).
AgentChannel  ::= ch(Stream(AgentToUserMsg), Stream(MediatorToAgentMsg)?).
ActorChannel  ::= ch(Stream(UserNotify), Stream(UserCmd)?).
UserChannel   ::= ch(Stream(UserCmd), Stream(UserNotify)?).
```

These are acceptable because they are defined in terms of parametric prelude types, not standalone stream definitions. The key rule is: **no standalone stream type definitions** — only the prelude's `Stream(X)` and `Channel(In, Out)`.

**Structural union types stay as-is:** `Bond`, `Lot`, `TradeResponse`, `EscrowBenResult`, `EscrowDepResult`, `EscrowCancel`, `FriendContent`, `FriendMsg`, `Response`, `Decision`, `UserContent`, `AgentContent`, `UserInMsg`, `NetInMsg`, `UserCmd`, `UserNotify`, `OutputKey`, `OutputEntry`, `OutputMsg`, `PendingValue`, `PendingEntry`, `ReqId`, `NarrativeItem`, etc. — all remain. These are tagged unions, not streams.

**Procedure declarations** use `Stream(X)` directly:

```prolog
%% Before:
procedure agent(Constant?, UserInStream?, NetInStream?, OutputsList?, BondList?, Constant?).

%% After:
procedure agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?, Stream(Bond)?, Constant?).
```

```prolog
%% Before:
procedure append(BondList?, BondList?, BondList).

%% After:
procedure append(Stream(Bond)?, Stream(Bond)?, Stream(Bond)).
```

```prolog
%% Before:
procedure create_bonds(Constant?, Constant?, Constant?, Constant?, BondList).

%% After:
procedure create_bonds(Constant?, Constant?, Constant?, Constant?, Stream(Bond)).
```

And so on for every procedure declaration.

### Module Structure

**Target directory: `programs/bonds_v2/`**

```
bonds_v2/
  self.glp              — shared types + shared helper procedures
  agent.glp             — -module(agent), exports agent/6
  mediator.glp          — -module(mediator), exports ui_mediator/5
  actors.glp            — -module(actors), exports fplay1–fplay6, fplay8–fplay11
  boot.glp              — -module(boot), imported procedures, network switches, play wiring
  play12/
    self.glp            — NarrativeItem type + narrative stream alias (if needed)
    alice.glp           — -module(alice), exports alice_p12/2
    bob.glp             — -module(bob), exports bob_p12/2
    charlie.glp         — -module(charlie), exports charlie_p12/2
    diana.glp           — -module(diana), exports diana_p12/2
    eve.glp             — -module(eve), exports eve_p12/2
    frank.glp           — -module(frank), exports frank_p12/2
```

### self.glp — What Goes Here

All type definitions (both structural unions and named parametric aliases) defined ONCE, visible to all modules via ancestor scoping.

Shared helper procedures that do NOT depend on `merge`:
- `lookup_send` / `lookup_send_step`
- `add_output`
- `close_outputs`
- `inject_msg`
- `new_friend_channel`
- `create_bonds`
- `append` (for `Stream(Bond)`)
- `bind_trade_accept` / `bind_trade_decline`
- `inject_trade_result`
- `inject_escrow_ben_result` / `inject_escrow_dep_result`
- `bind_escrow_cancel`
- `select_bonds_exact` / `select_bonds_by_spec` / `select_by_spec_continue`
- `build_menu` / `build_menu_acc` / `menu_update`
- `classify_trade`
- `escrow` (the timer-vs-cancel race)

Mediator helpers:
- `lookup_pending`

### agent.glp — What Goes Here

```prolog
-module(agent).
-mode(system).
```

**Exports:**
```prolog
exported procedure agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?, Stream(Bond)?, Constant?).
```

**Local procedures (depend on merge or call agent recursively):**
- `merge` — must be local per CSSN v2 convention (Stream(X) subtyping limitation)
- `bind_response` / `handle_response` — call merge
- `do_trade` / `do_trade_result` — call agent
- `handle_trade_fill` — calls agent
- `trade_dispatch` — calls agent
- `redemption_result` / `redemption_reject` — call agent
- `do_deposit_escrow` / `do_deposit_escrow_result` — call agent

All agent clauses remain here.

### mediator.glp — What Goes Here

```prolog
-module(mediator).
-mode(system).
```

**Exports:**
```prolog
exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, Stream(PendingEntry)?, Constant?).
```

**Local:**
- The mediator redefines `send`, `receive`, `new_channel` as unit clauses locally (same as current code). These are the prelude's definitions but the mediator needs them locally because it uses the channel-consuming form.

All mediator clauses remain here.

### actors.glp — What Goes Here

```prolog
-module(actors).
-mode(system).
```

**Exports:** All actor entry points: `fplay1` through `fplay6`, `fplay8` through `fplay11`, and `fplay4b`.

**Note:** These are the self-contained single-agent or two-agent test plays. They do NOT use `M # goal(...)` — they wire agent/mediator/network directly within each play. This is the same structure as CSSN v2's `ui/actors.glp`.

### boot.glp — What Goes Here

```prolog
-module(boot).
-mode(system).
```

**Imported procedures:**
```prolog
imported procedure agent#agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?, Stream(Bond)?, Constant?).
imported procedure mediator#ui_mediator(Constant?, AgentChannel?, UserChannel?, Stream(PendingEntry)?, Constant?).
imported procedure actors#fplay1(...).
%% ... etc for all exported actors
imported procedure alice#alice_p12(Constant?, ActorChannel?, Stream(NarrativeItem)).
%% ... etc for all play12 village actors
```

**Local:**
- `merge` (needed for network wiring)
- `tee`, `sink`
- `send_to_user_tagged`
- `send_to_user_narrate`
- Network switches (network2, network3, network6)

**Plays:**
- `fplay12` — the village market play, using `M # goal(...)` for cross-module calls

### play12/ actors — What Goes Here

Each actor file (alice.glp, bob.glp, etc.) declares its module and exports its entry point:

```prolog
-module(alice).
-mode(system).

exported procedure alice_p12(Constant?, ActorChannel?, Stream(NarrativeItem)).
```

The play12/self.glp defines `NarrativeItem` and any types specific to the village scenario that are not in the parent self.glp.

**IMPORTANT:** Apply the escrow fix from `Grassroots-Bonds/docs/fix-play12-escrow-instructions.md` to the v2 charlie.glp and frank.glp. Do NOT copy the old alice-coins escrow — use `lot(frank, 0, 5)` and the updated narrative strings as specified in that file.

## Type Mapping Reference

This is the complete mapping from old standalone types to new parametric types. Use this as a checklist.

### Stream types to ELIMINATE (replace with `Stream(X)` in all procedure declarations):

| Old Type | New Usage |
|----------|-----------|
| `BondList` | `Stream(Bond)` |
| `LotList` | `Stream(Lot)` |
| `FriendStream` | `Stream(FriendMsg)` |
| `UserInStream` | `Stream(UserInMsg)` |
| `NetInStream` | `Stream(NetInMsg)` |
| `MediatorToAgentStream` | `Stream(MediatorToAgentMsg)` |
| `AgentToUserStream` | `Stream(AgentToUserMsg)` |
| `OutputStream` | `Stream(OutputMsg)` |
| `OutputsList` | `Stream(OutputEntry)` |
| `UserCmdStream` | `Stream(UserCmd)` |
| `UserNotifyStream` | `Stream(UserNotify)` |
| `PendingList` | `Stream(PendingEntry)` |
| `NarrativeStream` | `Stream(NarrativeItem)` |

### Channel types — named parametric aliases (KEEP as named types in self.glp, but defined using parametric prelude types):

| Named Alias | Definition |
|-------------|------------|
| `FriendChannel` | `ch(Stream(FriendMsg), Stream(FriendMsg)?)` |
| `AgentChannel` | `ch(Stream(AgentToUserMsg), Stream(MediatorToAgentMsg)?)` |
| `UserChannel` | `ch(Stream(UserCmd), Stream(UserNotify)?)` |
| `ActorChannel` | `ch(Stream(UserNotify), Stream(UserCmd)?)` |

### Structural union types to KEEP as-is:

`Bond`, `Lot`, `TradeResponse`, `EscrowCancel`, `EscrowBenResult`, `EscrowDepResult`, `FriendContent`, `FriendMsg`, `Response`, `Decision`, `NetColdCall`, `NetInMsg`, `UserInMsg`, `UserContent`, `AgentContent`, `AgentToUserMsg`, `MediatorToAgentMsg`, `OutputKey`, `OutputContent`, `OutputMsg`, `OutputEntry`, `PendingValue`, `UserCmd`, `UserNotify`, `PendingEntry`, `ReqId`, `NarrativeItem`.

## Transformation Process

For each source file:

1. **Read the original file** from `programs/typed_book/bonds/`.
2. **Remove all standalone stream type definitions** (every `XxxStream ::= [] ; [Xxx | XxxStream].` and `XxxList ::= [] ; [Xxx | XxxList].`).
3. **Replace standalone channel types** with named parametric aliases using prelude types.
4. **Update all procedure declarations** to use `Stream(X)` instead of the old named stream types.
5. **Update clause heads** — clause heads do NOT use type names, so they should not need changes. The data structures (`[X|Xs]`, `ch(In, Out?)`, `[bond(I,M,S)|Rest]`, etc.) remain the same.
6. **Add module infrastructure** (`-module(...)`, `exported procedure`, `imported procedure`).
7. **Move shared types and helpers to self.glp.**
8. **Update cross-module calls in boot.glp** to use `M # goal(...)`.

## Testing

After creating all files:

1. Load the project via the REPL to verify compilation:
   ```
   GLP> programs/bonds_v2/
   ```
   This should load all modules via the self.glp scope chain and static linker.

2. If project loading doesn't work (the bonds boot.glp has plays that need individual loading), load files individually:
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_runtime
   BONDS=/Users/udi/Grassroots/GLP/programs/bonds_v2
   printf 'load $BONDS/agent.glp\nload $BONDS/mediator.glp\nload $BONDS/actors.glp\nload $BONDS/boot.glp\n:limit 1000000\nfplay1.\n' | dart run bin/glp_repl.dart
   ```

3. Test each play (fplay1 through fplay6, fplay8 through fplay11, fplay4b):
   Expected: `→ succeeds` or `→ suspended` (suspended is normal for escrow timer plays: fplay3, fplay4, fplay4b).

4. Test play 12 (village market):
   ```bash
   printf 'load $BONDS/agent.glp\nload $BONDS/mediator.glp\nload $BONDS/actors.glp\nload $BONDS/play12/alice.glp\nload $BONDS/play12/bob.glp\nload $BONDS/play12/charlie.glp\nload $BONDS/play12/diana.glp\nload $BONDS/play12/eve.glp\nload $BONDS/play12/frank.glp\nload $BONDS/boot.glp\n:limit 5000000\nfplay12.\n' | dart run bin/glp_repl.dart
   ```

5. Run the full GLP test suite to verify no regressions:
   ```bash
   cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
   ```

6. **Do NOT modify the original code** in `programs/typed_book/bonds/`. The v2 is a separate copy.

## After Testing

Commit and push. Provide merge instructions to the user.
