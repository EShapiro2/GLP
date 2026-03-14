# Bonds V2 Phase 4: Create boot.glp

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/bonds_v2/self.glp` (Phase 1 output)
5. Read `programs/cssn_modules_v2/boot.glp` (reference model — first 100 lines for module structure, then skim one fplay for the `M # goal(...)` call pattern)
6. Read `programs/typed_book/bonds/boot.glp` (source)

## Task

Create `programs/bonds_v2/boot.glp` — the play orchestrator with module infrastructure.

## What Changes

### 1. Add module declaration
```prolog
-module(boot).
-mode(system).
```

### 2. Remove all duplicate type definitions
The current boot.glp has NarrativeItem/NarrativeStream types. These are now in self.glp via ancestor scoping. Delete them.

### 3. Add imported procedure declarations
```prolog
%% From agent.glp
imported procedure agent#agent(Constant?, Stream(UserInMsg)?, Stream(NetInMsg)?, Stream(OutputEntry)?, Stream(Bond)?, Constant?).

%% From mediator.glp
imported procedure mediator#ui_mediator(Constant?, AgentChannel?, UserChannel?, Stream(PendingEntry)?, Constant?).

%% From actors.glp — one per exported play actor
imported procedure actors#alice_p1(ActorChannel?).
imported procedure actors#bob_p2(ActorChannel?).
%% ... etc for every actor procedure used in boot plays

%% From play12/*.glp — village actors
imported procedure alice#alice_p12(Constant?, ActorChannel?, Stream(NarrativeItem)).
imported procedure bob#bob_p12(ActorChannel?, Stream(NarrativeItem)).
imported procedure charlie#charlie_p12(Constant?, ActorChannel?, Stream(NarrativeItem)).
imported procedure diana#diana_p12(ActorChannel?, Stream(NarrativeItem)).
imported procedure eve#eve_p12(ActorChannel?, Stream(NarrativeItem)).
imported procedure frank#frank_p12(ActorChannel?, Stream(NarrativeItem)).
```

NOTE: Check the actual signatures of each actor in actors.glp and play12/*.glp. The above are approximate — match exactly.

### 4. Keep merge, tee, sink, send_to_user_tagged, send_to_user_narrate LOCAL
These stay in boot.glp (merge needs to be local per CSSN v2 convention).

### 5. Keep ALL network switches unchanged
network2, network6 — these are pure stream routing, no module changes needed.

### 6. Change direct calls to M # goal(...) in play bodies

In every fplayN body, change:
```prolog
%% Before:
agent(alice, AliceAgentIn?, AliceNetIn?, [...]),
ui_mediator(alice, ch(...), ch(...), [], 1),
alice_p1(ch(AliceActorIn?, AliceActorOut)),

%% After:
agent # agent(alice, AliceAgentIn?, AliceNetIn?, [...]),
mediator # ui_mediator(alice, ch(...), ch(...), [], 1),
actors # alice_p1(ch(AliceActorIn?, AliceActorOut)),
```

For play12 village actors:
```prolog
%% Before:
alice_p12(alice, ch(...), AliceNarr),
%% After:
alice # alice_p12(alice, ch(...), AliceNarr),
```

### 7. Replace NarrativeStream with Stream(NarrativeItem) in procedure declarations

## What Does NOT Change
- The structure of each play body (wiring logic)
- Network switch clauses
- tee/sink/merge/send_to_user_tagged/send_to_user_narrate implementations

## Verification
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/bonds_v2/self.glp\nload ../programs/bonds_v2/agent.glp\nload ../programs/bonds_v2/mediator.glp\nload ../programs/bonds_v2/boot.glp\n' | dart run bin/glp_repl.dart
```

## Do NOT
- Read actors.glp or play12 files (you only need their signatures for imported procedure declarations)
- Change any play wiring logic
