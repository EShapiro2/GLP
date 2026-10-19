# GSNP's app

The app of GSNP, the grassroots social network with provenance, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSNP's act schemas.  It is GSN's app, `../gsn`, on GSNP's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GSNP's scripts, for three persons.

## The contract

GSNP is GSC's contract (Coordination Appendix C); its clauses are `programs/gsc/GSNP.md`, as GAI's paper has them, and its act schemas `programs/gsc/GSNP.glp`.  GSNP extends GSN, whose clauses GSN:1 to GSN:5 it cites (`../gsn/README.md`):

1. *Authorship.*  An agent signs the content they create.
2. *Forwarding.*  An agent forwards content they received by sending it with the signatures it has and adding their own, and warrants that they know personally the agent they received it from.
3. *Confidence.*  An agent shall disclose content they received only by forwarding it.

Twelve schemas: GSG's `establish`, `befriend` and `unfriend`; `sign` and `forward`; and GSN's `create`, `join`, `leave`, `close`, `post_own`, `post` and `deliver`, a group message carried as an item (`GSNP.md`, "The schemas, and what they realise").  `sign(p? ; x)` adds `item(x)` at `p`; `forward(p?, q ; x)` requires `item(x)` at `p` and adds `item(sgn(x, p))` at `q`, the item signed again by `p`, and is GSN:1's message.  A speech act is `speech(Utterance, Peer)`, an utterance or a speech act and the person who signs it, so `sgn(x, p)` is `speech(x, p)`.  The warranty of GSNP:2 and GSNP:3 describe no act: the platform asks nobody on a forward.

## The cascade

1. The act schemas, `programs/gsc/GSNP.glp`; the checker's verdict `syntactically_grassroots` (`cascade/GSNP/verdict.txt`).
2. The canonical CVA platform of the schemas, `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GSNP.glp, the platform of the record `cascade/GSNP/2026-10-19`; `bash programs/apps/compile.sh --check` re-prints it and compares.
3. GLP: the canonical compilation compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GSNP, as printed |
| `miniapp.glp` | `miniapp/4`, the same file as `../gsg/miniapp.glp` and `../gsn/miniapp.glp` |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice`, `bob` or `carol` |
| `plays.glp` | `play_provenance/3`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `self.vglp` | the app's widget declaration, `Approval =::= approval_sentence.` |
| `screens/` | the persons' screens, as the bridge draws them, and their README |
| `../gsnp_boot.glp` | the super-app hosting the app on three smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsnp\n:limit 4000000\nplay_provenance(A, B, C).\n:quit\n' | bin/glpc
```

`play_provenance` runs three executions over two conversations, Alice's with Bob and Bob's with Carol, the play handing each its ends of the root channels as the social graph does.  Each establishes their identity (GSG:1); Alice and Bob ask to know each other, Alice offers Bob friendship, and he is asked and answers yes (GSG:2); Alice signs the item `'Flood on Main Street'` (GSNP:1) and forwards it to Bob, and nobody is asked (GSN:1 and GSNP:2); Bob then asks to know Carol, who asked to know him, and offers her friendship, and she is asked and answers yes; and Bob forwards to Carol the item he holds, signed by Alice, its author, and again by Alice, who forwarded it to him, and nobody is asked.  Carol then holds `speech(speech(speech(utterance('Flood on Main Street'), alice), alice), bob)`: the item signed by Alice and then by Bob.  The persons' screens, held exactly by the suite's Section APPS:

```
A = [propose(attempt(alice, 1, 1, establish(alice))), discover(bob), propose(attempt(alice, 2, 2, befriend(alice, bob))), tally(attempt(alice, 2, 2, befriend(alice, bob))), commit(attempt(alice, 2, 2, befriend(alice, bob))), propose(attempt(alice, 3, 3, sign(alice, speech(utterance('Flood on Main Street'), alice)))), propose(attempt(alice, 4, 4, forward(alice, bob, speech(utterance('Flood on Main Street'), alice)))), tally(attempt(alice, 4, 4, forward(alice, bob, speech(utterance('Flood on Main Street'), alice)))), commit(attempt(alice, 4, 4, forward(alice, bob, speech(utterance('Flood on Main Street'), alice)))) | _]
B = [propose(attempt(bob, 1, 1, establish(bob))), discover(alice), start(attempt(alice, 2, 2, befriend(alice, bob))), gave(befriend(alice, bob), yes), answer(attempt(alice, 2, 2, befriend(alice, bob))), vote(attempt(alice, 2, 2, befriend(alice, bob))), outcome(attempt(alice, 2, 2, befriend(alice, bob))), start(attempt(alice, 4, 4, forward(alice, bob, speech(utterance('Flood on Main Street'), alice)))), outcome(attempt(alice, 4, 4, forward(alice, bob, speech(utterance('Flood on Main Street'), alice)))), discover(carol), propose(attempt(bob, 5, 5, befriend(bob, carol))), tally(attempt(bob, 5, 5, befriend(bob, carol))), commit(attempt(bob, 5, 5, befriend(bob, carol))), propose(attempt(bob, 6, 6, forward(bob, carol, speech(speech(utterance('Flood on Main Street'), alice), alice)))), tally(attempt(bob, 6, 6, forward(bob, carol, speech(speech(utterance('Flood on Main Street'), alice), alice)))), commit(attempt(bob, 6, 6, forward(bob, carol, speech(speech(utterance('Flood on Main Street'), alice), alice)))) | _]
C = [propose(attempt(carol, 1, 1, establish(carol))), discover(bob), start(attempt(bob, 5, 5, befriend(bob, carol))), gave(befriend(bob, carol), yes), answer(attempt(bob, 5, 5, befriend(bob, carol))), vote(attempt(bob, 5, 5, befriend(bob, carol))), outcome(attempt(bob, 5, 5, befriend(bob, carol))), start(attempt(bob, 6, 6, forward(bob, carol, speech(speech(utterance('Flood on Main Street'), alice), alice)))), outcome(attempt(bob, 6, 6, forward(bob, carol, speech(speech(utterance('Flood on Main Street'), alice), alice)))) | _]
```

### On three smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsnp %s/programs/social/graph/core\n:boot %s/programs/apps/gsnp_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSN's app's on two (`../gsn/README.md`, "On two smartphones"), on three isolates: Alice connects to Bob, and once they are connected Bob connects to Carol; each pair becomes friends in the super-app's social graph; Alice invites Bob to `gsnp` and Bob invites Carol; the two conversations open, and the steps of `play_provenance` run over them; each event reduces to quiescence and the boot settles.  Each person's lines are what the super-app told them, `sg(connected(bob))` for Alice, `sg(befriend(alice))`, `sg(connected(alice))` and `sg(connected(carol))` for Bob, and `sg(befriend(bob))` and `sg(connected(bob))` for Carol, and the screens of `play_provenance` above.  Section APPS holds each person's super-app lines and their screen each exactly, in order: across three isolates the two are not ordered against each other, Bob's connection to Carol running beside Alice's invitation.

## The screens

What each person sees, as the bridge draws it today: the approval question by `approval_sentence`, the widget the app's declaration in `self.vglp` names (vGLP, Section 6.2, Definition "Widget Declaration"), one sentence for the attempt's class and two buttons labelled for it (`glp_multiagent/lib/ui_runtime/named_widgets.dart`), and the request question and the thread by the default widgets (Definition "Default Widget"), both of which vGLP is changing.  `screens/README.md` names each file.

The request question is a form headed `menu`, as on GSN's app.  Under `propose`, the field `Proposal` is a menu of eleven forms, one per schema that marks a role, `sign` and `forward` among them; `deliver` is not among them, nobody guarding it.  A `Speech` field is a form headed `speech`, drawn closed until the person opens it, with a field `Utterance`, a menu of `utterance`, a text field, and `speech`, a speech act signed again, and a peer field, `Peer`, the person who signs it.  To forward an item they hold, a person writes it whole into the form `forward`: the text, its author's signature, and the signature of each agent that forwarded it to them.

1. **Signing an item** (GSNP:1).  Alice proposes `sign`, `alice`, and the speech act `utterance` `Flood on Main Street`, signed `alice`, a class of one participant, taken at once.
2. **Forwarding it** (GSN:1 and GSNP:2).  Alice proposes `forward`, `alice`, `bob`, and the item.  Bob is not asked: his machine checks that Alice is his friend and votes.  His thread shows the attempt in `start` and `outcome`, and so shows him the item.
3. **A friendship** (GSG:2).  Bob proposes `befriend`, `bob`, `carol`.  Carol's approval question shows "Bob asks to be your friend.", with the buttons Accept and Decline; she taps Accept.
4. **Forwarding it again.**  Bob proposes `forward`, `bob`, `carol`, and the item he holds: the speech act `speech`, whose utterance is the speech act `utterance` `Flood on Main Street` signed `alice`, signed `alice`.  Carol is not asked.  Her thread shows the attempt in `start` and `outcome`, the item opened down to its text: signed by Alice, again by Alice, and forwarded by Bob.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsn/self.glp` does, with GSNP's classes `sign` and `forward` in place of GSN's `message`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
