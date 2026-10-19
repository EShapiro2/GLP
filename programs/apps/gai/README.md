# GAI's app

The app of GAI, accountable AI agents, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GAI's act schemas.  It is GSG's app, `../gsg`, on GAI's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GAI's scripts, for the three agents of the example of GAI's paper (Section 3.2, and Table "The components that change in the example"): Alice and Bob, persons, and Alf, Alice's AI agent.

## The contract

GAI is GAI's contract (Coordination Appendix C); its clauses are `programs/gai/GAI.md`, as GAI's paper has them, and its act schemas `programs/gai/GAI.glp`.  GAI extends Wards (`programs/gsc/Wards.md`), whose clauses Wards:1 to Wards:4 it cites, with GSG and GSNP's signing and forwarding beneath it:

1. *Developer.*  Every AI agent is attested by its *developer*, a person, and shall provide the attestation on request.
2. *Principal.*  The guardian of an AI agent is its *principal*.  Every act of the AI agent names its principal, and the principal knows the developer of the AI agent.
3. *Accountability.*  The developer of an AI agent is accountable for its defects, and its principal for operating it recklessly.
4. *Retiring.*  A principal retires their AI agent by ending their friendship with it.

Eleven schemas over nine predicates (`GAI.md`, "The schemas"): `establish`, `befriend`, `unfriend_p` and `unfriend_q`; `enter(p?, r?)`, a person creating an AI agent and becoming its principal and its friend (Wards:1, GAI:2), which adds `agent` and `principal(p)` at the AI agent and `principal_of(r)` at the person; `agent_befriend(r?, q?, p?)` and `agents_befriend(r?, s?, p?, q?)`, an AI agent entering a friendship by an act of both and of the principal of each AI agent among them (Wards:2), which adds at the counterparty `agent_of(r, p)`, its record that `r` is an AI agent of `p` (GAI:2); `agent_unfriend(r, q, p?)`, the principal ending it (Wards:3); `retire(p?, r)` (GAI:4), which ends the friendship of the AI agent and its principal and adds `retired` at the AI agent; and `sign(p? ; x)` and `forward(p?, q ; x)`.  GAI:1, GAI:3 and the principal knowing the developer (GAI:2) describe no act.  The AI agent's guarded roles are taken by its program: "The principal's will is given once, when they create the AI agent; after that the AI agent acts on its own" (GAI's paper, Section 3.1).

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

`play_agents` runs the example of GAI's paper: three executions, Alice's, Alf's and Bob's, over three conversations, Alice's with Alf, Alf's with Bob and Bob's with Alice, the play handing each its ends of the root channels as the social graph does.  Alice and Bob establish their identities (GSG:1); Alice enters her AI agent Alf, which answers yes, and she is its principal and its friend (Wards:1, GAI:2); once Alf is an AI agent and knows Bob, it proposes to befriend him, and Bob and Alice, its principal, are asked and answer yes (Wards:2), Bob recording `agent_of(alf, alice)`; once they are friends Alf signs the item `'Today\'s summary of the news'` (GSNP:1) and forwards it to Bob (GSNP:2), and nobody is asked; once the item has reached him Bob offers Alice friendship and she answers yes (GSG:2); and once they are friends Alice retires Alf (GAI:4), which ends her friendship with it, and Alf is asked nothing.  Bob then holds `speech(speech(utterance('Today\'s summary of the news'), alf), alf)`, the item signed by Alf and signed again by Alf, its forwarder.  Bob offers Alice friendship once the item has reached him, and Alice retires Alf once they are friends, so that Alf signs and forwards the item before it is retired: sign and forward forbid `retired` at the acting role.  The final states are those of the paper's table after Retire.  The agents' screens, the views their agents write with the answers they give, held exactly by the suite's Section APPS:

```
A = [view([], []), view([identity], []), view([identity], [alf]), view([identity], [alf, bob]), view([principal_of(alf), friend(alf), identity], [alf, bob]), gave(agent_befriend(alf, bob, alice), yes), gave(befriend(bob, alice), yes), view([friend(bob), principal_of(alf), friend(alf), identity], [alf, bob]), view([retired_of(alf), friend(bob), principal_of(alf), identity], [alf, bob]) | _]
R = [view([], []), view([], [alice]), gave(enter(alice, alf), yes), view([identity, agent, principal(alice), friend(alice)], [alice]), view([identity, agent, principal(alice), friend(alice)], [alice, bob]), view([friend(bob), identity, agent, principal(alice), friend(alice)], [alice, bob]), view([item(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), identity, agent, principal(alice), friend(alice)], [alice, bob]), view([retired, item(speech(utterance('Today\'s summary of the news'), alf)), friend(bob), identity, agent, principal(alice)], [alice, bob]) | _]
B = [view([], []), view([identity], []), view([identity], [alice]), view([identity], [alf, alice]), gave(agent_befriend(alf, bob, alice), yes), view([friend(alf), agent_of(alf, alice), identity], [alf, alice]), view([item(speech(speech(utterance('Today\'s summary of the news'), alf), alf)), friend(alf), agent_of(alf, alice), identity], [alf, alice]), view([friend(alice), item(speech(speech(utterance('Today\'s summary of the news'), alf), alf)), friend(alf), agent_of(alf, alice), identity], [alf, alice]) | _]
```

### On three smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gai %s/programs/social/graph/core\n:boot %s/programs/apps/gai_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSNP's app's on five (`../gsnp/README.md`, "On five smartphones"), on three isolates, Alf, an agent "with its machine" (GAI's paper, Section 3.2), on a smartphone of its own: Alice connects to Alf, and once connected Alf connects to Bob and Bob to Alice; each pair becomes friends in the super-app's social graph; once connected to the next, each invites them to `gai`; the three conversations open, and the steps of `play_agents` run over them; each event reduces to quiescence and the boot settles, `Boot settled: 3 agents (alice, alf, bob)`.  Each agent's lines are what the super-app told it, `sg(befriend(...))` and `sg(connected(...))`, and the views of `play_agents` above.  Section APPS holds each agent's super-app lines and their screen each exactly, in order.

## The screens

What Bob sees, as the construct family draws the app: the tab Friends of GSG's app (`../gsg/README.md`, "The screens"), drawn from the last view the agent wrote, the approval questions at its top, with the items of GSNP's app (`../gsnp/README.md`, "The screens").  An item whose author Bob holds recorded as an AI agent, `agent_of(alf, alice)`, names the author with its principal, "Author: Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph).

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice, Alf and Bob over three conversations, Alice and Alf scripted by `person.glp`, and takes Bob's acts on the tab's controls: Bob taps Join and adds Alf and Alice by key; Alf proposes to befriend him and Bob taps Yes; once the item has reached him, Bob opens his chat with Alf; then he taps Befriend on Alice's row, Alice answers yes and retires Alf.  `screens/README.md` names the screen it renders.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as `../gsnp/self.glp` does, with GAI's classes and its atoms `retired`, `agent`, `principal`, `principal_of`, `agent_of`, `retired_of` and `item`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
