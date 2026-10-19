# GAI's app

The app of GAI, accountable AI agents, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GAI's act schemas.  It is GSG's app, `../gsg`, on GAI's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GAI's scripts, for the three agents of the example of GAI's paper (Section 3.2, and Table "The components that change in the example"): Alice and Bob, persons, and Alf, Alice's AI agent.

## The contract

GAI is GAI's contract (Coordination Appendix C); its clauses are `programs/gai/GAI.md`, as GAI's paper has them, and its act schemas `programs/gai/GAI.glp`, at GLP 5744daec, recorded as `cascade/GAI/2026-10-19-5`.  GAI extends Wards (`programs/gsc/Wards.md`), whose clauses Wards:1 to Wards:4 it cites, with GSG and GSNP's signing, messaging and forwarding beneath it:

1. *Developer.*  The *developer* of an AI agent is the person who signed its code.  A person who creates an AI agent attests that it runs code signed by its developer.
2. *Principal.*  The guardian of an AI agent is its *principal*.  Every act of the AI agent names its principal and carries its principal's attestation.
3. *Accountability.*  The developer of an AI agent is accountable for its defects, and its principal for operating it recklessly.
4. *Retiring.*  A principal retires their AI agent by ending their friendship with it.

Twelve schemas over eleven predicates (`GAI.md`, "The schemas"): `establish`, `befriend`, `unfriend_p` and `unfriend_q`; `enter(p?, r? ; x)`, a person creating an AI agent and becoming its principal and its friend (Wards:1, GAI:2), which requires `item(x)` at the person, `x` the code signed by its developer, and adds `agent`, `principal(p)` and the attestation `attestation(sgn(x, p))` at the AI agent and `principal_of(r)` at the person (GAI:1); `agent_befriend(r?, q?, p? ; y)` and `agents_befriend(r?, s?, p?, q? ; y, z)`, an AI agent entering a friendship by an act of both and of the principal of each AI agent among them (Wards:2), each AI agent presenting its attestation, and the other party recording `agent_of(r, p, y)`, the AI agent, its principal and its attestation (GAI:2); `agent_unfriend(r, q, p?)`, the principal ending it (Wards:3); `retire(p?, r)` (GAI:4), which ends the friendship of the AI agent and its principal and adds `retired` at the AI agent; and `sign(p? ; x)`, `message(p?, q ; x)` and `forward(p?, q ; x)`, as `GSNP.glp` has them, with `retired` forbidden at the acting role.  GAI:3 describes no act.  The AI agent's guarded roles are taken by its program: "The principal's will is given once, when they create the AI agent; after that the AI agent acts on its own" (GAI's paper, Section 3.1).

## The cascade

1. The act schemas, `programs/gai/GAI.glp`; the checker's verdict `grassroots_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [retired])` (`cascade/GAI/verdict.txt`).
2. The canonical CVA platform of the schemas, `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GAI.glp, the same file as the recorded run's, `cascade/GAI/platform.vglp`; `bash programs/apps/compile.sh --check` re-prints it and compares.
3. GLP: the canonical compilation compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GAI, as printed |
| `miniapp.glp` | `miniapp/4`, the same file as `../gsg/miniapp.glp` |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice`, `alf` or `bob`, `alf`'s script the AI agent's program |
| `plays.glp` | `play_agents/3`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `self.vglp` | the app's widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu.` |
| `screens/` | the screen of GAI's GSNP figure that comes from this app, and its README |
| `../gai_boot.glp` | the super-app hosting the app on three smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gai\n:limit 6000000\nplay_agents(A, R, B).\n:quit\n' | bin/glpc
```

`play_agents` runs the example of GAI's paper: three executions, Alice's, Alf's and Bob's, over three conversations, Alice's with Alf, Alf's with Bob and Bob's with Alice, the play handing each its ends of the root channels as the social graph does.  Alice and Bob establish their identities (GSG:1); Alice signs the code of Alf, `speech(utterance('The code of Alf'), acme)`, signed by acme, its developer (Acme Ltd of the paper's Figure "The GAI app"), and holds it as her item k (GSNP:1); she enters her AI agent Alf with it, which answers yes, and she is its principal and its friend, and Alf holds its attestation `sgn(k, alice)` (Wards:1, GAI:1, GAI:2); once Alf holds its attestation and knows Bob, it proposes to befriend him with it, and Bob and Alice, its principal, are asked and answer yes (Wards:2), Bob recording `agent_of(alf, alice, y)`, `y` the attestation (GAI:2); once they are friends Alf signs the item `'Today\'s summary of the news'` (GSNP:1) and sends it to Bob by message (GSN:1), and nobody is asked; once the item has reached him Bob offers Alice friendship and she answers yes (GSG:2); once they are friends Alice retires Alf (GAI:4), which ends her friendship with it, and Alf is asked nothing; and Bob ends his friendship with Alf, `unfriend_p(bob, alf)` (GSG:3), which requires at Alf only `identity`.  Bob holds `speech(utterance('Today\'s summary of the news'), alf)`, the item signed once, by Alf.  Bob offers Alice friendship once the item has reached him, and Alice retires Alf once they are friends, so that Alf signs and sends the item before it is retired: sign and message forbid `retired` at the acting role.  Bob ends his friendship with Alf once he and Alice are friends; his view does not show that Alf is retired, so his `unfriend_p` and Alice's `retire`, Alf a participant of both, are concurrent, and the platform takes them in turn (vGLP, Section 3.2, "The protocol"), Bob's first in this run.  The final states are those of the paper's table after Unfriend.  The agents' screens, the views their agents write with the answers they give, held exactly by the suite's Section APPS:

```
A = [view([], []), view([identity], []), view([identity], [alf]), view([identity], [alf, bob]), view([item(speech(speech(utterance('The code of Alf'), acme), alice)), authored(speech(speech(utterance('The code of Alf'), acme), alice)), identity], [alf, bob]), view([principal_of(alf), friend(alf), item(speech(speech(utterance('The code of Alf'), acme), alice)), authored(speech(speech(utterance('The code of Alf'), acme), alice)), identity], [alf, bob]), gave(agent_befriend(alf, bob, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), yes), gave(befriend(bob, alice), yes), view([friend(bob), principal_of(alf), friend(alf), item(speech(speech(utterance('The code of Alf'), acme), alice)), authored(speech(speech(utterance('The code of Alf'), acme), alice)), identity], [alf, bob]), view([retired_of(alf), friend(bob), principal_of(alf), item(speech(speech(utterance('The code of Alf'), acme), alice)), authored(speech(speech(utterance('The code of Alf'), acme), alice)), identity], [alf, bob]) | _]
R = [view([], []), view([], [alice]), gave(enter(alice, alf, speech(speech(utterance('The code of Alf'), acme), alice)), yes), view([identity, agent, principal(alice), friend(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice]), view([identity, agent, principal(alice), friend(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice, bob]), view([friend(bob), identity, agent, principal(alice), friend(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice, bob]), view([item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), identity, agent, principal(alice), friend(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice, bob]), view([item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), identity, agent, principal(alice), friend(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice, bob]), view([retired, item(speech(utterance('Today\'s summary of the news'), alf)), authored(speech(utterance('Today\'s summary of the news'), alf)), identity, agent, principal(alice), attestation(speech(speech(speech(utterance('The code of Alf'), acme), alice), alice))], [alice, bob]) | _]
B = [view([], []), view([identity], []), view([identity], [alice]), view([identity], [alf, alice]), gave(agent_befriend(alf, bob, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), yes), view([friend(alf), agent_of(alf, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), identity], [alf, alice]), view([item(speech(utterance('Today\'s summary of the news'), alf)), friend(alf), agent_of(alf, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), identity], [alf, alice]), view([friend(alice), item(speech(utterance('Today\'s summary of the news'), alf)), friend(alf), agent_of(alf, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), identity], [alf, alice]), view([friend(alice), item(speech(utterance('Today\'s summary of the news'), alf)), agent_of(alf, alice, speech(speech(speech(utterance('The code of Alf'), acme), alice), alice)), identity], [alf, alice]) | _]
```

### On three smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gai %s/programs/social/graph/core\n:boot %s/programs/apps/gai_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSNP's app's on five (`../gsnp/README.md`, "On five smartphones"), on three isolates, Alf, an agent "with its machine" (GAI's paper, Section 3.2), on a smartphone of its own: Alice connects to Alf, and once connected Alf connects to Bob and Bob to Alice; each pair becomes friends in the super-app's social graph; once connected to the next, each invites them to `gai`; the three conversations open, and the steps of `play_agents` run over them; each event reduces to quiescence and the boot settles, `Boot settled: 3 agents (alice, alf, bob)`.  Each agent's lines are what the super-app told it, `sg(befriend(...))` and `sg(connected(...))`, and the views of `play_agents` above.  Section APPS holds each agent's super-app lines and their screen each exactly, in order.

## The screens

What Bob sees, as the construct family draws the app: the tab Friends of GSG's app (`../gsg/README.md`, "The screens"), drawn from the last view the agent wrote, the approval questions at its top, with the items and the chats of GSNP's app (`../gsnp/README.md`, "The screens").  An item whose author Bob holds recorded as an AI agent, `agent_of(alf, alice, y)`, names the author with its principal, "Author: Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph).  End friendship on a friend's row proposes `unfriend_p(Me, Q)`, GAI's `unfriend` for the person in its first role (Code_inbox.md, GAI #4 Cowork, 2026-10-19 16:57 UTC, item 4).

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice, Alf and Bob over three conversations, Alice and Alf scripted by `person.glp`, and takes Bob's acts on the tab's controls: Bob taps Join and adds Alf and Alice by key; Alf proposes to befriend him and Bob taps Yes; once the item has reached him, Bob opens his chat with Alf; then he taps Befriend on Alice's row, Alice answers yes and retires Alf; and then he taps End friendship on Alf's row.  `screens/README.md` names the screen it renders.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsnp/self.glp` does, with GAI's classes and its atoms `retired`, `agent`, `principal`, `principal_of`, `item`, `attestation`, `agent_of`, `retired_of` and `authored`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
