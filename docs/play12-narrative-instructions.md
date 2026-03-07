# Claude Code Instructions: Play 12 Narrative Trace

## Mandatory Reading

Read these before starting:
1. CLAUDE.md, DISCIPLINE.md (especially §1.12 rule 5), manual, cheat sheet
2. `programs/typed_book/bonds/play12/` — all 6 actor files
3. `programs/typed_book/bonds/boot.glp` — fplay12 wiring
4. `docs/village-market-scenario.md` — scenario with dialogues
5. `glp_runtime/lib/multiagent/repl_play_runner.dart` — tagged output parsing
6. `glp_multiagent/lib/main_bonds.dart` — Flutter panel rendering

## Overview

Add a narrative output stream to each play12 actor. The actor emits
descriptive terms alongside its state transitions. The Dart layer
renders them verbatim — ALL application-specific text lives in GLP.

**DISCIPLINE §1.12 rule 5: NEVER put application-specific code in Dart.**

## Part 1: GLP Changes

### 1a. Add NarrativeItem type to `play12/self.glp`

```glp
NarrativeItem   ::= friend(Constant)
                  ; say(Constant)
                  ; act(Constant)
                  ; event(Constant).
NarrativeStream ::= [] ; [NarrativeItem | NarrativeStream].
```

Four kinds:
- `friend(bob)` → "Now friends with Bob" (connection established)
- `say('...')` → quoted dialogue (what this agent says to another)
- `act('...')` → transaction description (what happened, with ✓)
- `event('...')` → something received/observed (payment received, escrow released)

### 1b. Add `send_to_user_narrate` to `boot.glp`

Next to `send_to_user_tagged`. Consumes a narrative stream and emits
tagged output via `_output/1`:

```glp
send_to_user_narrate(Id, [friend(X)|Ns]) :-
    ground(Id?), ground(X?) |
    '_output'(tagged(Id?, friend(X?))),
    send_to_user_narrate(Id?, Ns?).
send_to_user_narrate(Id, [say(X)|Ns]) :-
    ground(Id?), ground(X?) |
    '_output'(tagged(Id?, say(X?))),
    send_to_user_narrate(Id?, Ns?).
send_to_user_narrate(Id, [act(X)|Ns]) :-
    ground(Id?), ground(X?) |
    '_output'(tagged(Id?, act(X?))),
    send_to_user_narrate(Id?, Ns?).
send_to_user_narrate(Id, [event(X)|Ns]) :-
    ground(Id?), ground(X?) |
    '_output'(tagged(Id?, event(X?))),
    send_to_user_narrate(Id?, Ns?).
send_to_user_narrate(_, []).
```

This is untyped (like `send_to_user_tagged`) since `_output/1` is a kernel predicate.

### 1c. Change actor signatures — add NarrativeStream parameter

Current:
```glp
exported procedure alice_p12(Constant?, ActorChannel?).
```

New:
```glp
exported procedure alice_p12(Constant?, ActorChannel?, NarrativeStream).
```

The narrative stream is a **writer** (no `?`) — the actor constructs it.
Same for all 6 actors. Charlie's already has `Constant?` for T.

All internal wait-state procedures also get the extra parameter:
```glp
procedure alice_p12_wait_connected_bob(UserNotifyStream?, UserCmdStream, NarrativeStream).
```

### 1d. Add narrative emissions to each actor

Each state transition emits a narrative term on the stream. The pattern:

**Before** (current):
```glp
alice_p12(_, ch(In, [connect(bob)|Out?])) :-
    alice_p12_wait_connected_bob(In?, Out).
```

**After**:
```glp
alice_p12(_, ch(In, [connect(bob)|Out?]), Narr?) :-
    alice_p12_wait_connected_bob(In?, Out, Narr).
```

Note: the entry clause doesn't emit a narrative yet — it just passes the stream.
The `wait_connected` clause emits when the connection is confirmed:

**Before**:
```glp
alice_p12_wait_connected_bob([connected(bob)|In],
                             [connect(charlie)|Out?]) :-
    alice_p12_wait_connected_charlie(In?, Out).
```

**After**:
```glp
alice_p12_wait_connected_bob([connected(bob)|In],
                             [connect(charlie)|Out?],
                             [friend(bob)|Narr?]) :-
    alice_p12_wait_connected_charlie(In?, Out, Narr).
```

The skip clause passes through:
```glp
alice_p12_wait_connected_bob([_|In], Out?, Narr?) :-
    otherwise | alice_p12_wait_connected_bob(In?, Out, Narr).
alice_p12_wait_connected_bob([], [], []).
```

### 1e. Full narrative text for each actor

Below is EXACTLY what each actor emits. Follow `docs/village-market-scenario.md`
for the dialogue text. All text is a GLP constant (atom or quoted string).

**Alice** (Baker):
```
friend(bob)
friend(charlie)
friend(eve)
say('Bob, lets trade — 15 coins each')
act('Credit line with Bob: 15 alice-coins for 15 bob-coins')
say('Charlie, same deal — 15 for 15?')
act('Credit line with Charlie: 15 alice-coins for 15 charlie-coins')
event('Bob bought 5 loaves — paid 5 alice-coins')
event('Eve bought 3 loaves — paid 3 alice-coins')
say('Eve, Ill sell you 10 bob-bonds for 4 frank-coins')
act('Sold debt: 10 bob-bonds to Eve for 4 frank-coins')
act('Balance: ...')    %% use actual balance_report content if feasible, else summary
```

**Bob** (Farmer):
```
friend(alice)
say('Sure Alice, 15 for 15')
act('Credit line with Alice: 15 bob-coins for 15 alice-coins')
friend(diana)
say('Diana, 24 bonds maturing day 25 for 20 coins — 20 percent interest')
act('Loan from Diana: 20 diana-coins for 24 bob-bonds(25)')
say('Alice, 5 loaves please')
act('Paid Alice 5 for bread')
act('Balance: ...')
```

**Charlie** (Carpenter):
```
friend(alice)
say('Alice, happy to trade 15 for 15')
act('Credit line with Alice: 15 charlie-coins for 15 alice-coins')
friend(eve)
friend(frank)
say('Eve, 10 for 10?')
act('Credit line with Eve: 10 charlie-coins for 10 eve-coins')
event('Eve bought a bookshelf — paid 6 charlie-coins')
say('Frank, 8 alice-coins in escrow for the dock — release day 15')
act('Escrow deposited: 8 alice-coins for Frank, release day 15')
event('Escrow released — Frank got paid for dock')
act('Balance: ...')
```

**Diana** (Doctor):
```
friend(bob)
say('Bob, deal — 20 diana-coins for 24 bob-bonds maturing day 25')
act('Loan to Bob: 20 diana-coins for 24 bob-bonds(25)')
friend(frank)
say('Frank, 15 coins for 18 bonds maturing day 28')
act('Loan to Frank: 15 diana-coins for 18 frank-bonds(28)')
event('Frank paid 3 for medical checkup')
event('Frank redeemed 5 diana-coins')
act('Balance: ...')
```

**Eve** (Teacher):
```
friend(alice)
friend(charlie)
say('Charlie, 10 for 10 — works for me')
act('Credit line with Charlie: 10 eve-coins for 10 charlie-coins')
friend(frank)
say('Frank, were family — 10 for 10')
act('Credit line with Frank: 10 eve-coins for 10 frank-coins')
say('Charlie, 6 coins for the bookshelf')
act('Paid Charlie 6 for bookshelf')
say('Charlie, can I swap 5 frank-coins for 5 alice-coins?')
act('Swap with Charlie: 5 frank-coins for 5 alice-coins')
say('Alice, 3 loaves please')
act('Paid Alice 3 for bread')
say('Alice, deal — 4 frank-coins for 10 bob-bonds')
act('Bought debt: 10 bob-bonds from Alice for 4 frank-coins')
act('Balance: ...')
```

**Frank** (Fisherman):
```
friend(charlie)
friend(diana)
say('Diana, deal — 18 bonds maturing day 28 for 15 coins')
act('Loan from Diana: 15 diana-coins for 18 frank-bonds(28)')
friend(eve)
say('Eve, of course — 10 for 10')
act('Credit line with Eve: 10 frank-coins for 10 eve-coins')
say('Diana, checkup please — 3 coins')
act('Paid Diana 3 for checkup')
event('Escrow from Charlie: 8 alice-coins, release day 15')
event('Escrow released — received 8 alice-coins for dock')
say('Diana, Id like to redeem 5 of your coins')
act('Redeemed 5 diana-coins from Diana')
act('Balance: ...')
```

### 1f. Narrative emissions for `balance`

For the balance line, the actor can either:
- Emit a static `act('Final balance')` and let the existing `balance_report`
  show in the code trace, or
- Emit `act('Balance: N bob-coins, M charlie-coins, ...')` with hardcoded
  expected values from the scenario spec.

Use the hardcoded summary approach — the actor knows what it expects.

### 1g. Wire narrative in fplay12 boot

Current fplay12 (per agent block):
```glp
play12#alice # alice_p12(T?, ch(AliceActorIn?, AliceActorOut)),
tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
agent # agent(alice, ...),
mediator # ui_mediator(alice, ...),
tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
send_to_user_tagged(alice, AliceDispCmd?, AliceDispNotify?),
```

New (add narrative stream + consumer):
```glp
play12#alice # alice_p12(T?, ch(AliceActorIn?, AliceActorOut), AliceNarr),
send_to_user_narrate(alice, AliceNarr?),
tee(AliceActorOut?, AliceMedIn, AliceDispCmd),
agent # agent(alice, ...),
mediator # ui_mediator(alice, ...),
tee(AliceMedOut?, AliceActorIn, AliceDispNotify),
send_to_user_tagged(alice, AliceDispCmd?, AliceDispNotify?),
```

Same for all 6 agents. Add `AliceNarr`, `BobNarr`, `CharlieNarr`, `DianaNarr`, `EveNarr`, `FrankNarr`.

Also update imported declarations in boot.glp:
```glp
imported procedure play12#alice#alice_p12(Constant?, ActorChannel?, NarrativeStream).
%% etc for all 6
```

### 1h. Type-check and test

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo -e '../programs/typed_book/bonds\nfplay12.\n:quit' | dart run bin/glp_repl.dart
```

Expected: tagged output includes `friend(...)`, `say(...)`, `act(...)`, `event(...)` lines
interleaved with the existing `cmd(...)` and `notify(...)` lines.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "play12: add narrative trace to all 6 actors"
```

---

## Part 2: Dart Changes (systemic only — NO application knowledge)

### 2a. Extend `PlayOutput` in `repl_play_runner.dart`

Current regex:
```dart
static final _taggedRegex = RegExp(r'^tagged\((\w+), (cmd|notify)\((.+)\)\)$');
```

New regex — add the 4 narrative kinds:
```dart
static final _taggedRegex = RegExp(r'^tagged\((\w+), (cmd|notify|friend|say|act|event)\((.+)\)\)$');
```

No other changes to `PlayOutput` — the `kind` field already takes any string.

### 2b. Update `main_bonds.dart` rendering

Current rendering logic uses `line.startsWith('>')` and `line.startsWith('<')`.

Change to handle 6 kinds. The display line format:

| kind | prefix | display |
|------|--------|---------|
| `cmd` | `> ` | `> connect(bob)` |
| `notify` | `< ` | `< connected(bob)` |
| `friend` | (none) | `Now friends with Bob` — capitalize content |
| `say` | (none) | `"I'll sell you 10 bob-bonds..."` — wrap in quotes |
| `act` | (none) | `  Credit line with Bob: ...` — indent 2 spaces |
| `event` | (none) | `Bob bought 5 loaves...` — plain |

**The Dart code does NOT know what these texts mean.** It just:
- Wraps `say` content in quotes
- Prepends "Now friends with " to `friend` content (capitalizing first letter)
- Indents `act` content with 2 spaces
- Leaves `event` content as-is

These are **formatting rules based on the tag type**, not application knowledge.
The tag types (friend/say/act/event) are part of the GLP→Dart protocol.

Style per kind:

| kind | color | weight | italic |
|------|-------|--------|--------|
| `cmd` | blue 800 | normal | no |
| `notify` | green 800 | bold | no |
| `friend` | grey 600 | normal | no |
| `say` | indigo 700 | normal | yes |
| `act` | grey 800 | normal | no |
| `event` | teal 700 | normal | no |

### 2c. Add view mode toggle

Add a toggle in the control bar: **Code** / **Story**.

- **Code mode**: show only `cmd` and `notify` lines (current behavior)
- **Story mode**: show only `friend`, `say`, `act`, `event` lines
- Default: **Story** for play 12, **Code** for plays 1–11

The toggle is a `bool _storyMode` in the state. The `onOutput` callback
adds ALL lines to the log. The `ListView.builder` filters by mode.

### 2d. Build and test

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos --release --target=lib/main_bonds.dart
open build/macos/Build/Products/Release/glp_multiagent.app
```

Click Play 12 — should show narrative panels in Story mode.
Toggle to Code — should show raw cmd/notify trace.

Commit:
```bash
cd /Users/udi/Grassroots/GLP && git add -A && git commit -m "play12: narrative rendering in Flutter (systemic, no app-specific code)"
```

---

## Regression

All plays 1–11 must still work (actors unchanged, boot wiring unchanged).

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
for P in fplay1 fplay2 fplay3 fplay4 fplay5 fplay6 fplay8 fplay9 fplay10 fplay11; do
  echo -e "../programs/typed_book/bonds\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/narr-reg-${P}.txt 2>&1
  echo "${P}: done"
done
```

## Bug Protocol

Same as always. STOP on errors, show full output, wait for direction.
