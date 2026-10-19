# GSNP's app

The app of GSNP, the grassroots social network with provenance, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSNP's act schemas.  It is GSN's app, `../gsn`, on GSNP's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GSNP's scripts, for the five persons of the play of GAI's GSNP figure.

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
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `dan`, `eve`, `carol`, `bob` or `fay` |
| `plays.glp` | `play_provenance/5`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `self.vglp` | the app's widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu.` |
| `screens/` | the screens of GAI's figure of the app, and their README |
| `../gsnp_boot.glp` | the super-app hosting the app on five smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsnp\n:limit 6000000\nplay_provenance(D, E, C, B, F).\n:quit\n' | bin/glpc
```

`play_provenance` runs the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4): five executions, Dan's, Eve's, Carol's, Bob's and Fay's, over four conversations, Dan's with Eve, Eve's with Carol, Carol's with Bob and Bob's with Fay, the play handing each its ends of the root channels as the social graph does.  Each establishes their identity (GSG:1); Dan and Eve, Eve and Carol, Carol and Bob, and Bob and Fay each ask to know each other, the first of each two offers the second friendship, and the second is asked and answers yes (GSG:2); Dan signs the item `'Flood on Main Street'` (GSNP:1), which records him its author, and sends it to Eve in a message, which carries it as he signed it, and nobody is asked (GSN:1); Eve forwards it to Carol, and Carol to Bob, and nobody is asked (GSNP:2).  Bob then holds `speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)`: the item signed by Dan, then by Eve, then by Carol.  Each person who passes the item on offers their next friendship once the item has reached them, so that they propose nothing while an attempt that adds the item to their state is open: the agent drops a request whose condition does not hold when it reads it (vGLP, Section 5.4, "The compilation").  The persons' screens, the views their agents write with the answers they give, held exactly by the suite's Section APPS:

```
D = [view([], []), view([identity], []), view([identity], [eve]), view([friend(eve), identity], [eve]), view([item(speech(utterance('Flood on Main Street'), dan)), authored(speech(utterance('Flood on Main Street'), dan)), friend(eve), identity], [eve]) | _]
E = [view([], []), view([identity], []), view([identity], [dan]), view([identity], [carol, dan]), gave(befriend(dan, eve), yes), view([friend(dan), identity], [carol, dan]), view([item(speech(utterance('Flood on Main Street'), dan)), friend(dan), identity], [carol, dan]), view([friend(carol), item(speech(utterance('Flood on Main Street'), dan)), friend(dan), identity], [carol, dan]) | _]
C = [view([], []), view([identity], []), view([identity], [bob]), view([identity], [bob, eve]), gave(befriend(eve, carol), yes), view([friend(eve), identity], [bob, eve]), view([item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), identity], [bob, eve]), view([friend(bob), item(speech(speech(utterance('Flood on Main Street'), dan), eve)), friend(eve), identity], [bob, eve]) | _]
B = [view([], []), view([identity], []), view([identity], [fay]), view([identity], [carol, fay]), gave(befriend(carol, bob), yes), view([friend(carol), identity], [carol, fay]), view([item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), identity], [carol, fay]), view([friend(fay), item(speech(speech(speech(utterance('Flood on Main Street'), dan), eve), carol)), friend(carol), identity], [carol, fay]) | _]
F = [view([], []), view([identity], []), view([identity], [bob]), gave(befriend(bob, fay), yes), view([friend(bob), identity], [bob]) | _]
```

### On five smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsnp %s/programs/social/graph/core\n:boot %s/programs/apps/gsnp_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSN's app's on two (`../gsn/README.md`, "On two smartphones"), on five isolates: Dan connects to Eve, and once connected Eve connects to Carol, Carol to Bob and Bob to Fay; each pair becomes friends in the super-app's social graph; once connected to the next, each invites them to `gsnp`; the four conversations open, and the steps of `play_provenance` run over them; each event reduces to quiescence and the boot settles.  Each person's lines are what the super-app told them, `sg(befriend(...))` and `sg(connected(...))`, and the views of `play_provenance` above.  Section APPS holds each person's super-app lines and their screen each exactly, in order: across the isolates the two are not ordered against each other.

## The screens

What each person sees, as the construct family draws the app: the tabs Friends and Groups of GSN's app (`../gsn/README.md`, "The screens"), drawn from the last view the agent wrote, the approval questions at the top of the tab they concern, with GSNP's items (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 1 to 3).  An item the person holds is drawn in the chat with the friend it came from, its last signer: its text, then "Author: Dan", its first signer, then, where it was forwarded, "via Eve, via Carol", the others who signed it, in order; under an item the person did not author, Forward, and under one they authored, nothing.  Forward opens a picker of the person's friends; picking Fay shows "Forward to Fay?  You warrant that you know Carol personally.", Carol being the friend the item came from, with Yes, which proposes `forward(Me, fay, x)`, and No, which closes it.  The chat's text field and Send sign the text and send it: `sign(Me, x)`, and, once the view holds `authored(x)`, `message(Me, Q, x)`.  A group has no text field and no Send, a post carrying an item its poster holds.

`glp_multiagent/test/apps_screens_test.dart` runs the play on five phones and takes each person's acts on the tabs' controls: each two of Dan and Eve, Eve and Carol, Carol and Bob, and Bob and Fay tap Join, add each other by key and become friends, the second asked and tapping Accept; Dan opens his chat with Eve, types Flood on Main Street and taps Send; Eve opens her chat with Dan, taps Forward under the item, picks Carol and taps Yes; Carol does the same in her chat with Eve, picking Bob; and Bob opens his chat with Carol and taps Forward and picks Fay.  `screens/README.md` names the screens it renders.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsn/self.glp` does, with GSNP's classes `sign`, `forward`, `post_forward` and `deliver_own` and its atoms `item` and `authored` in place of GSN's `received` and `received_in`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
