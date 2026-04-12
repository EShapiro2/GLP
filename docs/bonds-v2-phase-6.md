# Bonds V2 Phase 6: Create play12/ Village Actors

## Startup
1. Read `CLAUDE.md`
2. Read `docs/DISCIPLINE.md`
3. Read `docs/bonds-v2-overview.md` (common rules)
4. Read `programs/bonds_v2/self.glp` (Phase 1 output)
5. Read `programs/typed_book/bonds/play12/self.glp` (source)
6. Read ALL play12 actor files: alice.glp, bob.glp, charlie.glp, diana.glp, eve.glp, frank.glp from `programs/typed_book/bonds/play12/`
7. Read `Grassroots-Bonds/docs/fix-play12-escrow-instructions.md` (escrow fix)

## Task

Create `programs/bonds_v2/play12/` directory with self.glp + 6 actor modules.

## play12/self.glp

If the parent self.glp doesn't define NarrativeItem, define it here:
```prolog
-module(play12).
-mode(system).

NarrativeItem ::= friend(Constant) ; say(Constant) ; act(Constant) ; event(Constant).
```

Check parent self.glp first — if NarrativeItem is already there, this file may not be needed (or can be empty except for the module declaration).

## Each Actor Module

Each file gets:

### Module declaration + export
```prolog
-module(alice).  %% or bob, charlie, etc.
-mode(system).

exported procedure alice_p12(Constant?, ActorChannel?, Stream(NarrativeItem)).
```

### Remove duplicate type definitions
The old play12 self.glp defines types locally. In v2, types come from ancestor self.glp files. Remove any duplicate type declarations from the actor files.

### Update procedure declarations
Replace old type names:
- `NarrativeStream` → `Stream(NarrativeItem)`
- `UserCmdStream` → `Stream(UserCmd)`
- `UserNotifyStream` → `Stream(UserNotify)`

### Actor clause bodies — NO CHANGES (except escrow fix below)

## ESCROW FIX — Apply to charlie.glp and frank.glp

### charlie.glp (4 changes):
1. `deposit_escrow(frank, [lot(alice, 0, 8)], T?)` → `deposit_escrow(frank, [lot(frank, 0, 5)], T?)`
2. `say('Frank, 8 alice-coins in escrow for the dock — release day 15')` → `say('Frank, 5 frank-coins in escrow for the dock — release day 15')`
3. `act('Escrow deposited: 8 alice-coins for Frank, release day 15')` → `act('Escrow deposited: 5 frank-coins for Frank, release day 15')`
4. `act('Balance: 2 alice-coins, 10 eve-coins, 6 charlie-coins, 5 frank-coins')` → `act('Balance: 10 alice-coins, 10 eve-coins, 6 charlie-coins')`

### frank.glp (3 changes):
1. `event('Escrow from Charlie: 8 alice-coins, release day 15')` → `event('Escrow from Charlie: 5 frank-coins, release day 15')`
2. `event('Escrow released — received 8 alice-coins for dock')` → `event('Escrow released — received 5 frank-coins for dock')`
3. `act('Balance: 8 alice-coins, 7 diana-coins, 10 eve-coins')` → `act('Balance: 7 diana-coins, 10 eve-coins')`

## Verification
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
BONDS=../programs/bonds_v2
printf "load $BONDS/self.glp\nload $BONDS/agent.glp\nload $BONDS/mediator.glp\nload $BONDS/actors.glp\nload $BONDS/play12/alice.glp\nload $BONDS/play12/bob.glp\nload $BONDS/play12/charlie.glp\nload $BONDS/play12/diana.glp\nload $BONDS/play12/eve.glp\nload $BONDS/play12/frank.glp\nload $BONDS/boot.glp\n:limit 5000000\nfplay12.\n" | dart run bin/glp_repl.dart
```

## Do NOT
- Modify the old play12 files in typed_book/bonds/play12/
- Change any actor logic (except the escrow fix)
