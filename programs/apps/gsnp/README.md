# GSNP's app

The app of GSNP, the grassroots social network with provenance, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSNP's act schemas.  It is GSN's app, `../gsn`, on GSNP's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GSNP's scripts, for three persons.

## The contract

GSNP is GSC's contract (Coordination Appendix C); its clauses are `programs/gsc/GSNP.md`, as GAI's paper has them, and its act schemas `programs/gsc/GSNP.glp`.  GSNP extends GSN, whose clauses GSN:1 to GSN:5 it cites (`../gsn/README.md`):

1. *Authorship.*  An agent signs the content they create.
2. *Forwarding.*  An agent forwards content they received by sending it with the signatures it has and adding their own, and warrants that they know personally the agent they received it from.
3. *Confidence.*  An agent shall disclose content they received only by forwarding it.

Sixteen schemas: GSG's `establish`, `befriend` and `unfriend`; `sign`, `message` and `forward`; and GSN's `create`, `join`, `leave`, `close`, `drop`, `post_own`, `post`, `post_forward`, `deliver` and `deliver_own`, a group message carried as an item (`GSNP.md`, "The schemas, and what they realise"; GLP 120bf8bd).  `sign(p? ; x)` adds `item(x)` and `authored(x)` at `p`; `message(p?, q ; x)` requires `authored(x)` at `p` and adds `item(x)` at `q`, the item as its author signed it; `forward(p?, q ; x)` requires `item(x)` and forbids `authored(x)` at `p`, and adds `item(sgn(x, p))` at `q`, the item signed again by `p`.  A speech act is `speech(Utterance, Peer)`, an utterance or a speech act and the person who signs it, so `sgn(x, p)` is `speech(x, p)`.  The warranty of GSNP:2 and GSNP:3 describe no act: the platform asks nobody on a forward.

## The cascade

1. The act schemas, `programs/gsc/GSNP.glp`; the checker's verdict `syntactically_grassroots` (`cascade/GSNP/verdict.txt`).
2. The canonical CVA platform of the schemas, `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GSNP.glp; `bash programs/apps/compile.sh --check` re-prints it and compares.
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
| `self.vglp` | the app's widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu.` |
| `screens/` | the persons' screens, as the construct family draws them, and their README |
| `../gsnp_boot.glp` | the super-app hosting the app on three smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsnp\n:limit 4000000\nplay_provenance(A, B, C).\n:quit\n' | bin/glpc
```

`play_provenance` runs three executions over two conversations, Alice's with Bob and Bob's with Carol, the play handing each its ends of the root channels as the social graph does.  Each establishes their identity (GSG:1); Alice and Bob ask to know each other, Alice offers Bob friendship, and he is asked and answers yes (GSG:2); Alice signs the item `'Flood on Main Street'` (GSNP:1), which records her its author, and sends it to Bob in a message, which carries it as she signed it, and nobody is asked (GSN:1); Bob then asks to know Carol, who asked to know him, and offers her friendship, and she is asked and answers yes; and Bob forwards to Carol the item he holds, and nobody is asked (GSNP:2).  Carol then holds `speech(speech(utterance('Flood on Main Street'), alice), bob)`: the item signed by Alice and then by Bob.  The persons' screens, the views their agents write with the answers they give, held exactly by the suite's Section APPS:

```
A = [view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([item(speech(utterance('Flood on Main Street'), alice)), authored(speech(utterance('Flood on Main Street'), alice)), friend(bob), identity], [bob]) | _]
B = [view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), view([item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice]), view([item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice, carol]), view([friend(carol), item(speech(utterance('Flood on Main Street'), alice)), friend(alice), identity], [alice, carol]) | _]
C = [view([], []), view([identity], []), view([identity], [bob]), gave(befriend(bob, carol), yes), view([friend(bob), identity], [bob]), view([item(speech(speech(utterance('Flood on Main Street'), alice), bob)), friend(bob), identity], [bob]) | _]
```

### On three smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsnp %s/programs/social/graph/core\n:boot %s/programs/apps/gsnp_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSN's app's on two (`../gsn/README.md`, "On two smartphones"), on three isolates: Alice connects to Bob, and once they are connected Bob connects to Carol; each pair becomes friends in the super-app's social graph; Alice invites Bob to `gsnp` and Bob invites Carol; the two conversations open, and the steps of `play_provenance` run over them; each event reduces to quiescence and the boot settles.  Each person's lines are what the super-app told them, `sg(connected(bob))` for Alice, `sg(befriend(alice))`, `sg(connected(alice))` and `sg(connected(carol))` for Bob, and `sg(befriend(bob))` and `sg(connected(bob))` for Carol, and the views of `play_provenance` above.  Section APPS holds each person's super-app lines and their screen each exactly, in order: across three isolates the two are not ordered against each other, Bob's connection to Carol running beside Alice's invitation.

## The screens

What each person sees, as the construct family draws the app: the tabs Friends and Groups of GSN's app (`../gsn/README.md`, "The screens"), drawn from the last view the agent wrote, the approval questions at the top of the tab they concern.  The tabs carry no control that signs an item, sends an item in a message or forwards one: the chat a friend opens has a field with Send, proposing a message of the text typed, which the person has not signed, so its condition, `authored(x)` at the sender, does not hold and the agent drops it; a group has no field and no Send, a post carrying an item its poster holds; and the items the person holds are drawn under Messages at the foot of Friends.  The app's play signs, sends and forwards its item by the scripted persons (`person.glp`), and the screens show the friendships and a group.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice, Bob and Carol over two conversations and takes each person's acts on the tabs' controls: Alice and Bob, then Bob and Carol, tap Join, add each other by key and become friends, Bob and then Carol asked and tapping Accept; Alice creates the group friends and adds Bob, who taps Join.  `screens/README.md` names each file.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsn/self.glp` does, with GSNP's classes `sign`, `forward`, `post_forward` and `deliver_own` and its atoms `item` and `authored` in place of GSN's `received` and `received_in`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
