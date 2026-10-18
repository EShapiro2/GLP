# GSN's app

The app of GSN, the grassroots social network, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSN's act schemas.  It is GSG's app, `../gsg`, on GSN's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GSN's scripts.

## The contract

GSN is GSC's contract (Coordination Appendix C); its clauses are `programs/gsc/GSN.md`, as GAI's paper has them, and its act schemas `programs/gsc/GSN.glp`.  GSN extends GSG, whose clauses GSG:1 to GSG:3 it cites (`../gsg/README.md`):

1. *Message.*  An agent may send a message to a friend, with content they created or received from another agent.
2. *Group.*  An agent may create a group, and is then its leader and first member.
3. *Joining.*  The leader of a group may add a friend to it, by an act of both.
4. *Leaving.*  A member of a group may leave it by their own act; when the leader leaves, the group ends.
5. *Group message.*  A member of a group may send a message to the group, with content they created or received from another agent, and the leader delivers it to every member.

Ten schemas: GSG's `befriend` and `unfriend`, and `message`, `create`, `join`, `leave`, `close`, `post_own`, `post` and `deliver` (`GSN.md`, "The schemas, and what they realise").  A group is named by its leader and a name, and content and a group's name are speech acts, `speech(Utterance, Peer)`, an utterance and the person who signs it.

## The cascade

1. The act schemas, `programs/gsc/GSN.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas, `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GSN.glp; `bash programs/apps/compile.sh --check` re-prints it and compares.
3. GLP: the canonical compilation compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GSN, as printed |
| `miniapp.glp` | `miniapp/4`, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice` or `bob` |
| `plays.glp` | `play_network/2`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `../gsn_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsn\n:limit 2000000\nplay_network(A, B).\n:quit\n' | bin/glpc
```

`play_network` runs two executions over one conversation, the play handing each its end of the root channel as the social graph does: Alice and Bob ask to know each other and become friends; Alice sends Bob a message; she creates the group `friends` and adds Bob to it, and he answers yes; Bob sends the group a message, and Alice delivers it to him; Bob leaves the group.  The persons' screens, held exactly by the suite's Section APPS, are the two threads, with Bob's answers:

```
A = [discover(bob), propose(attempt(alice, 1, 1, befriend(alice, bob))), tally(...), commit(...),
     propose(attempt(alice, 2, 2, message(alice, bob, speech(utterance(hello), alice)))), tally(...), commit(...),
     propose(attempt(alice, 3, 3, create(alice, speech(utterance(friends), alice)))),
     propose(attempt(alice, 4, 4, join(alice, bob, speech(utterance(friends), alice)))), tally(...), commit(...),
     start(attempt(bob, 5, 5, post(bob, alice, speech(utterance(hi_all), bob), speech(utterance(friends), alice)))), outcome(...),
     propose(attempt(alice, 6, 6, deliver(alice, bob, speech(utterance(hi_all), bob), speech(utterance(friends), alice)))), tally(...), commit(...),
     start(attempt(bob, 7, 7, leave(alice, bob, speech(utterance(friends), alice)))), outcome(...) | _]
B = [discover(alice), start(attempt(alice, 1, 1, befriend(alice, bob))), gave(befriend(alice, bob), yes), answer(...), vote(...), outcome(...),
     start(attempt(alice, 2, 2, message(...))), outcome(...),
     start(attempt(alice, 4, 4, join(...))), gave(join(alice, bob, speech(utterance(friends), alice)), yes), answer(...), vote(...), outcome(...),
     propose(attempt(bob, 5, 5, post(...))), tally(...), commit(...),
     start(attempt(alice, 6, 6, deliver(...))), outcome(...),
     propose(attempt(bob, 7, 7, leave(...))), tally(...), commit(...) | _]
```

A `(...)` abbreviates an attempt or a class named in full before it; the suite holds the lines whole.

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsn %s/programs/social/graph/core\n:boot %s/programs/apps/gsn_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSG's app's (`../gsg/README.md`, "On two smartphones"): the two super-apps befriend, Alice invites Bob to `gsn`, the conversation opens, and the run reaches Bob's vote on the friendship and does not settle, the platform's agent never quiescent, its clock goal writing ticks without end (vGLP, Section 5.4).  The Flutter super-app is not yet within reach for the reasons `../gsg/README.md` names, with `../programs/apps/gsn` for the asset step's list and Bob's acts for `gsn` in `superapp_ui/3`.

## The screens

What each person sees, step by step, as the bridge draws the constructs of the platform's two questions with the default widgets (vGLP, Section 6.2, Definition "Default Widget"), and the thread.  The super-app first, as for GSG's app: Alice and Bob install `gsn.glpw`, become friends in the super-app's social graph, and Alice invites Bob to `gsn`.

The request question is a form headed `menu`: its field `List(Key)` shows the persons the agent knows, and its field `Request` is a menu of `propose` and `known`.  Under `propose`, the field `Class` is a menu of ten forms, one per schema: `befriend`, `unfriend`, `message`, `create`, `join`, `leave`, `close`, `post_own`, `post` and `deliver`, each with a peer field per person it names and a `Speech` field per speech act.  A `Speech` field is a form headed `speech`, with a field `Utterance`, a menu of `utterance`, a text field, and `speech`, drawn closed until the person opens it, and a peer field, `Peer`, the person who signs it.

1. **Knowing each other.**  Alice chooses `known` and types `bob`; Bob chooses `known` and types `alice`.  Once their agents have exchanged hellos, Alice's thread shows `discover(bob)` and Bob's `discover(alice)`.
2. **Friendship** (GSG:2).  Alice proposes `befriend`, `alice`, `bob`.  Bob's approval question shows the attempt `attempt(alice, 1, 1, befriend(alice, bob))`, with the buttons `yes` and `no`; he taps `yes`.  Alice's thread shows `tally` and `commit`, Bob's `answer`, `vote` and `outcome`.
3. **A message** (GSN:1).  Alice proposes `message`, `alice`, `bob`, and the speech act `utterance` `hello`, signed `alice`.  Bob is not asked: his machine checks that Alice is his friend and votes.  Alice's thread shows `propose(attempt(alice, 2, 2, message(alice, bob, speech(utterance(hello), alice))))`, `tally` and `commit`; Bob's shows the same attempt in `start` and `outcome`, and so shows him the message.
4. **A group** (GSN:2).  Alice proposes `create`, `alice`, and the name, the speech act `utterance` `friends`, signed `alice`.  It has one participant, so it is taken at once: her thread shows `propose(attempt(alice, 3, 3, create(alice, speech(utterance(friends), alice))))`.
5. **Joining** (GSN:3).  Alice proposes `join`, `alice`, `bob`, and the group's name.  Bob's approval question shows `attempt(alice, 4, 4, join(alice, bob, speech(utterance(friends), alice)))`; he taps `yes`.  Alice's thread shows `tally` and `commit`, Bob's `answer`, `vote` and `outcome`.
6. **A group message** (GSN:5).  Bob proposes `post`, `bob`, `alice`, the content, the speech act `utterance` `hi_all`, signed `bob`, and the group's name.  Alice is not asked.  Bob's thread shows `propose(attempt(bob, 5, 5, post(...)))`, `tally` and `commit`; Alice's `start` and `outcome`.  Then Alice's machine, with nobody asked, delivers it to Bob: her thread shows `propose(attempt(alice, 6, 6, deliver(alice, bob, speech(utterance(hi_all), bob), speech(utterance(friends), alice))))`, `tally` and `commit`, and Bob's the same attempt in `start` and `outcome`.  Her thread shows no delivery to herself.
7. **Leaving** (GSN:4).  Bob proposes `leave`, `alice`, `bob`, and the group's name.  Alice is not asked.  Bob's thread shows `propose(attempt(bob, 7, 7, leave(alice, bob, speech(utterance(friends), alice))))`, `tally` and `commit`; Alice's `start` and `outcome`.

The menu of step 1 offers `deliver`, a class nobody guards, beside the nine the persons guard.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry: `Peer`, `Speech`, `Utterance`, `Class`, `Request`, `YesNo`, `Decision`, `Attempt`, `Cargo`, `Content`, `Net`, `Txn` and `Output`, `Approval` and `Menu`; and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
