# GFF's app

The app of GFF, Grassroots Feeds and Followers, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GFF's act schemas.  It is the host of `../gsg` and `../gsnp` on GFF's platform: the same entry and network (`miniapp.glp`, the same file), a scripted person with GFF's scripts, and GFF's play.

## The contract

GFF is GFF's contract (Coordination Appendix C; Udi, 2026-10-27); its clauses and its pragmas are `programs/gff/GFF.txt`, as GFF's paper has them, and its act schemas `programs/gff/GFF.glp`.  GFF extends GSG, and through it GKK, whose clauses GKK:1 to GKK:3, GSG:1 and GSG:2 it cites (`../gsg/README.md`).  Its own clauses are Feed (GFF:1), Following (GFF:2), Follower discovery (GFF:3) and Delivery (GFF:4); its schemas are post; follow_author and follow_through, and unfollow; ask and its three answers, answer_follows, answer_follows_through and answer_does_not_follow; and deliver_own and deliver_on.  The answers and the deliveries are unguarded: the machines take them.

## The cascade

1. The act schemas, `programs/gff/GFF.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation") with the contract's pragmas: `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GFF.glp and GFF.txt.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GFF, as printed, with the words of its pragmas: the tabs Contacts, Friends and Feeds, the last listing `follows` |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/4` and `cued_person/5`: the scripted person, by role, `alice`, `bob`, `carol`, `dave` or `eve`, the second with a tail of steps the play gives |
| `plays.glp` | `play_feeds/5`, the mini-app with no super-app, and the watchers that give a person the rest of their script once another person's screen shows what the play waits for |
| `self.glp` | the platform's types written again, the person's steps, and the exports |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gff\n:limit 30000000\nplay_feeds(A, B, C, D, E).\n:quit\n' | bin/glpc
```

`play_feeds` is GFF's play (Code_inbox.md, GFF #1 Cowork, 2026-10-27 16:34 UTC, with its revision of 17:21 UTC adding the introductions): five executions, alice, bob, carol, dave and eve, over four conversations, one between each two friends, the play handing each its ends as the social graph does.

1. Each establishes their identity (GKK:1); in alice and bob, bob and carol, carol and dave, and dave and eve, the first meets the second by their key, the second answering yes to its hello, and then offers the second friendship, who answers yes (GKK:2, GSG:1).
2. Alice posts two items (GFF:1).  Each is an utterance she signs at its posting.
3. Bob follows alice (GFF:2, follow_author).  Alice's machine delivers both items to him (GFF:4, deliver_own), as she signed them at their posting.
4. Once both items have reached him, bob introduces alice to carol (GKK:3), whom carol must know to ask about her (GFF:3).
5. Carol asks bob whether he follows alice (GFF:3, ask); bob's machine answers with a path, an item of alice's feed he holds (answer_follows); carol follows alice through bob (GFF:2, follow_through); bob's machine delivers the two items to her, signed by bob (deliver_on); carol introduces alice to dave.
6. Dave likewise, through carol, and eve through dave.
7. Once eve's screen shows that she follows alice, alice posts a third item, which reaches eve signed by alice at its posting and by bob, carol and dave on its way: `item(speech(speech(speech(speech(utterance('Third item'), alice), bob), carol), dave), alice)`.
8. Once eve's screen shows the third item, carol ends her following of alice (GFF:2, unfollow).
9. Once carol's screen shows that she no longer follows alice, alice posts a fourth item, which reaches bob and not carol, dave or eve.

The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS.

A "once" between two persons is the play's: a watcher on one person's screen gives the other person the rest of their script.  A person whose script has reached its tail takes no step, and reads and reports nothing, until the play gives it.

Bob introduces alice to carol only once both items have reached him.  Had he proposed the introduction while alice's deliveries to him were open, his attempt would have yielded and been proposed again, and carol would have been asked a second time; her script answers once.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`, as `../gsg/self.glp` does.  `Drawn` is written as the compilation forms it, `menu_w(_) ; approval_w(_) ; ...`: written with `Menu` and `Approval`, the person's heads do not typecheck, the question the dispatcher draws carrying the grant's input in place of the request's reader.
