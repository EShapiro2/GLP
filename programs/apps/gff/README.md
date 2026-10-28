# GFF's app

The app of GFF, Grassroots Feeds and Followers, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GFF's act schemas.  It is the host of `../gsg` and `../gsnp` on GFF's platform: the same entry and network (`miniapp.glp`, the same file), a scripted person with GFF's scripts, and GFF's play.

## The contract

GFF is GFF's contract (Coordination Appendix C; Udi, 2026-10-27); on the branch GFF-chooser its clauses and pragmas are `programs/gff/GFF.txt` and its speech-act schemas `programs/gff/GFF.glp` (7fb6cd1c).  GFF extends GSG, and through it GKK.  Its clauses are Feed (GFF:1, post), Following (GFF:2: friends_follow and friend_following_ended, a friend's following; follow_through, a person's Follow of a non-friend through a friend who follows them; path_broken and friendship_ended), Unfollow (GFF:3, unfollow), Delivery (GFF:4, deliver_own and deliver_on, an item held as its identity x with path(x, y), y the signatures over x along the path it came by, and from(x, c), the friend it came from) and Updating (GFF:5, update and update_ceased, Carol's role marked every_so_often).

## The cascade

1. The speech-act schemas, `programs/gff/GFF.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas, compiled into vGLP (vGLP, Section 5.4): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GFF.glp and GFF.txt.  GFF.txt has an Algorithms section, so the platform calls the app's chooser, `chooser.glp` (`programs/vglp/cva/compiler/CHOOSER.md`).  `bash programs/apps/compile.sh --check` re-prints it and compares.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`.
5. The record: `cascade/GFF/`, by `bash cascade.sh gff --record`, holds `chooser.glp` beside the platform, and its run links it.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GFF, as printed from GFF.glp and GFF.txt, with the words of its pragmas: the tabs Contacts, Friends, Posts and Feeds, and the buttons Post, Follow and Unfollow |
| `chooser.glp` | `choose/4`, the module chooser, GFF's chooser, written from the Updating line of GFF.txt's Algorithms section ("The chooser") |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/4` and `cued_person/5`: the scripted person, by role, `alice` to `eve` for `play_feeds` and `choice_alice` to `choice_eve` for `play_choice` |
| `plays.glp` | `play_feeds/5` and `play_choice/6`, the mini-app with no super-app, and the watchers that give a person the rest of their script once another person's screen shows what the play waits for |
| `self.glp` | the platform's types written again, the person's steps, and the exports |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gff\n:limit 30000000\nplay_feeds(A, B, C, D, E).\n:quit\n' | bin/glpc
printf '../programs/apps/gff\n:limit 30000000\nplay_choice(A, B, C, D, F, E).\n:quit\n' | bin/glpc
```

`play_feeds`, five executions over four conversations, alice's with bob, bob's with carol, carol's with dave and dave's with eve:

1. Each two friends meet and befriend, and follow each other (friends_follow); alice posts two items.
2. Once both items have reached bob, he introduces alice to carol.  Once bob's update that he follows alice has reached her, carol follows alice through bob by her Follow; once both items have reached her, along alice and bob, she introduces alice to dave, who follows through carol, and dave introduces her to eve, who follows through dave.
3. Once eve follows alice, alice posts a third item, which reaches eve along alice, bob, carol and dave.
4. Once eve holds it, carol ceases following alice.  Dave's following through carol ends (path_broken), and eve's through dave; carol's and eve's updates that they no longer follow alice reach dave, whose screen then shows no friend of his following alice.

`play_choice`, six executions over six conversations: alice befriends bob and carol and posts one item; carol befriends dave, dave frank, and eve bob and frank; dave follows alice through carol and frank through dave.  Once frank holds the item, bob introduces alice to eve; told by bob's and frank's updates that each follows alice, eve follows her through bob and through frank, holds the item by both paths, alice and bob, and alice, carol, dave and frank, and ceases her following through frank.

Each run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are held exactly by the suite's Section APPS.

## The chooser

On the branch GFF-chooser the platform's agent puts the Proposes of the unguarded parts it opens to one procedure, the chooser, the longest waited first, with its machine state and the every_so_often parts it has committed (`programs/vglp/cva/compiler/CHOOSER.md`).  GFF's chooser, `chooser.glp`, written from the Updating line, takes `update(Carol, Alice, b)` where the newer of `update` and `update_ceased` of Carol to Alice about b among the parts committed is not `update`, and `update_ceased(Carol, Alice, b)` where it is; it holds them otherwise, and takes every other part as the default does.  So Carol updates a friend once after each change of whether Carol follows b, and a new friend once upon befriending.  play_feeds took 19 updates and 5 updates ceased, and play_choice 30 updates and none ceased, counted on the persons' screens.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`, as `../gsg/self.glp` does.  `Drawn` is written as the compilation forms it, `menu_w(_) ; approval_w(_) ; ...`: written with `Menu` and `Approval`, the person's heads do not typecheck, the question the dispatcher draws carrying the grant's input in place of the request's reader.
