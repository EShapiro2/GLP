# Bonds V2 Phase 5: Create actors.glp

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/bonds_v2/self.glp` (Phase 1 output — for type names)
5. Read `programs/typed_book/bonds/actors.glp` (source)

## Task

Create `programs/bonds_v2/actors.glp` — the scripted test actors.

## What Changes

### 1. Add module declaration
```prolog
-module(actors).
-mode(system).
```

### 2. Remove ALL duplicate type definitions at the top
The current actors.glp redeclares Bond, BondList, ReqId, Lot, LotList, UserCmd, Decision, UserNotify, UserCmdStream, UserNotifyStream, ActorChannel. ALL of these are now in self.glp. Delete them entirely.

### 3. Add `exported procedure` for every actor entry point

Every actor procedure that boot.glp calls must be exported. Find all procedure declarations and add `exported`:
```prolog
exported procedure alice_p1(ActorChannel?).
exported procedure alice_p2(ActorChannel?).
exported procedure bob_p2(ActorChannel?).
%% ... etc for every actor
```

### 4. Update procedure declarations
Replace any remaining old type names with parametric:
- `UserCmdStream` → `Stream(UserCmd)` 
- `UserNotifyStream` → `Stream(UserNotify)`
- `BondList` → `Stream(Bond)` (if used in declarations)
- `LotList` → `Stream(Lot)` (if used in declarations)

Most actor procedures just use `ActorChannel?` which is already a named alias in self.glp.

## What Does NOT Change
- Actor clause bodies — the logic of each actor is identical
- The actor wiring (reading notifications, writing commands)

## Verification
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
printf 'load ../programs/bonds_v2/self.glp\nload ../programs/bonds_v2/actors.glp\n' | dart run bin/glp_repl.dart
```

## Do NOT
- Read boot.glp, agent.glp, or mediator.glp
- Change any actor logic
