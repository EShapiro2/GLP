# GFF's app

The app of GFF, Grassroots Feeds and Followers, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GFF's act schemas.  It is the host of `../gsg` and `../gsnp` on GFF's platform: the same entry and network (`miniapp.glp`, the same file), a scripted person with GFF's scripts, and GFF's play.

## The contract

GFF is GFF's contract (Coordination Appendix C; Udi, 2026-10-27); its clauses and its pragmas are `programs/gff/GFF.txt`, as GFF's paper has them, and its act schemas `programs/gff/GFF.glp`.  GFF extends GSG, and through it GKK, whose clauses GKK:1 to GKK:3, GSG:1 and GSG:2 it cites (`../gsg/README.md`).  Its own clauses are Feed (GFF:1), Following (GFF:2), Follower discovery (GFF:3) and Delivery (GFF:4); its schemas are post; follow, friends_follow and through_friend, and unfollow; ask and its three answers, answer_follows, answer_follows_through and answer_does_not_follow; and deliver_own and deliver_on.  Following a friend, following through a friend, the asking, the answers and the deliveries are unguarded: the machines take them.

## The cascade

1. The act schemas, `programs/gff/GFF.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GFF.glp and GFF.txt.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GFF, as printed from GFF.glp and GFF.txt, with the words of its pragmas: the tabs Contacts, Friends, Posts and Feeds, and the button Follow |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/4` and `cued_person/5`: the scripted person, by role, `alice`, `bob`, `carol`, `dave` or `eve` for `play_feeds` and `choice_alice` to `choice_eve` for `play_choice`, the second with a tail of steps the play gives |
| `chooser.glp` | `choose/3`, the module chooser, GFF's chooser, written from the Algorithms section of GFF.txt ("The chooser") |
| `plays.glp` | `play_feeds/5` and `play_choice/6`, the mini-app with no super-app, and the watchers that give a person the rest of their script once another person's screen shows what the play waits for |
| `self.glp` | the platform's types written again, the person's steps, and the exports |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gff\n:limit 30000000\nplay_feeds(A, B, C, D, E).\n:quit\n' | bin/glpc
```

`play_feeds` is GFF's play (Code_inbox.md, GFF #1 Cowork, 2026-10-27 16:34 UTC, with its revision of 17:21 UTC adding the introductions): five executions, alice, bob, carol, dave and eve, over four conversations, one between each two friends, the play handing each its ends as the social graph does.

1. Each establishes their identity (GKK:1); in alice and bob, bob and carol, carol and dave, and dave and eve, the first meets the second by their key, the second answering yes to its hello, and then offers the second friendship, who answers yes (GKK:2, GSG:1).
2. Alice posts two items (GFF:1).  Each is an utterance she signs at its posting.
3. Friends follow each other: once two are friends, each one's machine follows the other (GFF:2, friends_follow).  So bob follows alice, and alice's machine delivers both items to him as she signed them (GFF:4, deliver_own).
4. Once he follows alice and both items have reached him, bob introduces alice to carol (GKK:3), whom carol must know to decide to follow her (GFF:2).
5. Carol decides to follow alice (GFF:2, follow), her one step of following.  Her machine asks dave and bob (ask); dave answers that he does not follow alice (answer_does_not_follow), and bob with a path, an item of alice's feed he holds (answer_follows).  Once both have answered, her machine follows her through bob, the one friend with a path (through_friend; the chooser, below).  Bob's machine delivers the two items to her, signed by bob (deliver_on).  Once both have reached her, carol introduces alice to dave.
6. Dave likewise, through carol, and eve through dave.
7. Once eve's screen shows that she follows alice, alice posts a third item, which reaches eve signed by alice, bob, carol and dave, once each: `item(speech(speech(speech(speech(utterance('Third item'), alice), bob), carol), dave), alice)`.
8. Once eve's screen shows the third item, carol ends her following of alice (GFF:2, unfollow), which deletes her decision.
9. Once carol's screen shows that she no longer follows alice, alice posts a fourth item, which reaches bob and not carol, dave or eve.

The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS.

A "once" between two persons is the play's: a watcher on one person's screen gives the other person the rest of their script.  A person whose script has reached its tail takes no step, and reads and reports nothing, until the play gives it.

Each person introduces the next only once both items have reached them.  Had bob proposed the introduction while alice's deliveries to him were open, his attempt would have yielded and been proposed again, and carol would have been asked a second time; her script answers once.

## Following: the person's step and the machine's

Friends follow each other by an act each machine takes, `friends_follow`.  A person follows anyone else by one step, Follow, `follow(Me, B)`.  The asking and the following through a friend are unguarded acts, and the platform's agent takes them itself: after each trigger it takes its own transactions, one at a time, until none holds (vGLP, Section 5.4, "Triggers").  No code of this directory proposes them; which of them the agent takes, and when, is the chooser's (below).

## The chooser

On the branch GFF-chooser, an experiment Udi approved on 2026-10-27 (reported by GFF #1 Cowork), the platform's agent puts the Proposes of the unguarded parts it opens to one procedure, the chooser, the longest waited first, with its machine state, and takes the part the chooser returns, or none yet (`programs/vglp/cva/compiler/CHOOSER.md`).  GFF.txt has an Algorithms section, so its platform calls the app's chooser, `chooser.glp`, written from it: "when Alice decides to follow Bob, Alice's machine asks every friend whether they follow Bob (GFF:3) and follows Bob through the friend with the shortest path to Bob (GFF:2)".  The chooser holds back `through_friend(Alice, Carol, Bob)` until every friend of Alice has answered about Bob, and then allows it only with a friend whose answer is a path of the fewest signatures; every other part it allows as the default does, the first.  An answer `path_through(Bob, Carol, D)`, that Carol follows Bob through their friend D, names no path whose length Alice's state holds; the chooser follows through such a friend only where no friend answered with a path.

`play_choice` shows the choice: six executions, alice, bob, carol, dave, frank and eve, over six conversations, alice's with bob and with carol, carol's with dave, dave's with frank, and eve's with bob and with frank.

1. Each establishes their identity; alice meets and befriends bob and carol, carol dave, dave frank, and eve bob and frank (GKK:2, GSG:1), and friends follow each other (GFF:2, friends_follow).
2. Alice posts one item (GFF:1), which reaches bob and carol signed by her (GFF:4, deliver_own).
3. Carol introduces alice to dave, who decides to follow her; his machine asks carol and frank, frank answers that he does not follow her and carol with a path, and his machine follows her through carol.  Dave introduces alice to frank, who follows her through dave likewise, after dave's answer with a path and eve's that she does not follow her.  The item reaches frank signed by alice, carol and dave.
4. Once frank's screen shows the item, bob introduces alice to eve, who decides to follow her.
5. Eve's machine asks frank and bob.  Frank answers first, with a path of three signatures, alice's, carol's and dave's, four hops from eve; then bob, with a path of one, alice's, two hops from eve.  Her machine then follows alice through bob, and the item reaches her signed by alice and bob.

```bash
printf '../programs/apps/gff\n:limit 30000000\nplay_choice(A, B, C, D, F, E).\n:quit\n' | bin/glpc
```

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`, as `../gsg/self.glp` does.  `Drawn` is written as the compilation forms it, `menu_w(_) ; approval_w(_) ; ...`: written with `Menu` and `Approval`, the person's heads do not typecheck, the question the dispatcher draws carrying the grant's input in place of the request's reader.
