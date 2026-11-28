# GFF's app

The app of GFF, Grassroots Feeds and Followers, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from the act schemas of the contract Feeds.  It is the host of `../gsg` on Feeds's platform: the same entry and network (`miniapp.glp`, the same file), a scripted person with GFF's scripts, and GFF's play.

## The contract

Feeds, `programs/gff/Feeds.txt` and `Feeds.glp`, adopted from the Calculus (GFF until 2026-11-16), is the contract the app is compiled from.  It extends and restricts Labelled Overlay: Post (Feeds:1), Deliver from a Bob who knows Alice (Feeds:2), and Labeled point transitive only if Carol knows Alice (Feeds:3, restricting Labelled Overlay:2); every other clause of Labelled Overlay, Overlay, Friendship and Knowledge is kept.  Its schemas are establish, acquaint, introduce, befriend and unfriend; point_friend, point_transitive and unpoint; labeled_point_friend, labeled_point_transitive, labeled_point_transitive_friend, labeled_unpoint and labeled_unpoint_self; post; and deliver, which is unguarded: the sender's machine takes it.  The header of each `.txt` file says what the contract is written from, and its Interface pragmas give the words the app shows.

## The cascade

1. The act schemas, `programs/gff/Feeds.glp`, printed from the Logic pragmas of `Feeds.txt` by `calculus/tools/adopt.py`; the checker's verdict `syntactically_grassroots` (`cascade/Feeds/verdict.txt`).
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from Feeds.glp and the Interface pragmas of Feeds.txt and of the contracts it is written from.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.  It is the file `cascade/Feeds/platform.vglp`.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/3`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of Feeds, as printed, with the words of its pragmas: the tabs Contacts, Friends, Pointing, Posts and Feeds, and the buttons Establish, Acquaint, Introduce, Befriend, Unfriend, Point, Unpoint, Labeled point, Labeled unpoint and Post |
| `miniapp.glp` | `miniapp/3`: the agent, its dispatcher, and the agent's network over the conversations, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/5` and `cued_person/6`: the scripted person, by role, `alice`, `bob`, `carol` or `dave`, with the keys the person knows, the second with a tail of steps the play gives |
| `plays.glp` | `play_feeds/4`, the mini-app with no super-app, and the watchers that give a person the rest of their script once another person's screen shows what the play waits for |
| `self.glp` | the platform's types written again, the person's steps, and the exports |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gff\n:limit 300000000\nplay_feeds(A, B, C, D).\n:quit\n' | bin/glpc
```

`play_feeds` is GFF's play: four executions, alice, bob, carol and dave, over four conversations, alice's and bob's, bob's and carol's, carol's and dave's, and bob's and dave's, the play handing each its ends as the social graph does.  An item is an utterance alice signs at its posting; at a person who holds it the state has `item(X, alice)`, X the item and alice the author of the feed, and at a person who received it `item_from(X, alice, Q)` besides, Q the person it came from; at the person who delivered it to R, `delivered(X, alice, R)`.

1. Each establishes their identity (Knowledge:1); alice and bob, bob and carol, and carol and dave meet, the first by the second's key, the second answering yes to its hello, and the first then offers the second friendship, who answers yes (Knowledge:2, Friendship:1).  Each two friends have a pointer to each other labeled by the other (Labelled Overlay's definitions): at bob, `pointer(alice, alice)`.
2. Alice posts two items (Feeds:1).  Bob has an alice-pointer to her, so both reach him from her (Feeds:2, deliver).
3. Carol follows alice's feed through bob, her friend, who has an alice-pointer: her one step, Labeled point, with the label alice, adds an alice-pointer from carol to bob, `pointer(bob, alice)` (Labelled Overlay:1, labeled_point_friend).  Both items reach her from bob.
4. Once both have reached carol, dave follows alice's feed through carol likewise, and both items reach him from carol.
5. Once both have reached dave, carol introduces dave to bob (Knowledge:3); once bob knows dave, carol introduces bob to dave; once dave knows bob, dave adds an alice-pointer to bob, bob having an alice-pointer to alice and knowing dave (Feeds:3, labeled_point_transitive).
6. Once dave has it, alice posts a third item, which reaches bob, carol and dave, each once.
7. Once the third item has reached dave, carol removes her alice-pointer to bob, Labeled unpoint (Labelled Overlay:3, labeled_unpoint), naming bob and the label alice.
8. Once carol's screen shows it gone, alice posts a fourth item, which reaches bob, and from bob dave, and not carol.

The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with each answer the person gives (`gave(C, A)`).

A "once" between two persons is the play's: a watcher on one person's screen gives the other person the rest of their script.  A person whose script has reached its tail takes no step until the play gives it.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsg/self.glp` does.
