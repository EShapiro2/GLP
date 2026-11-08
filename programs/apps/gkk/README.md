# GKK's app

The app of GKK, Grassroots Keys and Knowledge, the base mini-app of the grassroots app, which every other mini-app extends (GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC), as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GKK's act schemas, as `../gsg` is from GSG's.

## The contract

GKK is GSC's contract, the base contract every contract extends (Coordination Appendix C); its clauses are `programs/gsc/GKK.txt`, as GSC's paper, Section 2.1, has them, and its act schemas `programs/gsc/GKK.glp`.

GKK:1 is the schema `establish(p?)`, which adds `identity` at the person, and every role of the other two schemas requires `identity`.  In the app the key pair is the one the super-app holds, which the super-app hands the mini-app as the `Key` of its entry `miniapp/4`, and which the platform's agent takes as its `Peer`.  The person establishes their identity by their first request, `propose(establish(P))`, `P` their key, which every view shows, its first argument, and the request form reads off the last view drawn (vGLP, Section 5.4, "The compilation": the view carries "its own key k"; Section 6.2: "A declared widget is drawn also with the last value drawn on construct 0"), a class of one participant, taken at once.  GKK:2 is `acquaint(p?, q?)`, guarded by both, adding `knows` of the other at each; GKK:3 is `introduce(p?, q? ; c)`, guarded by both, `c` the person introduced, who takes no part: `p` requires `knows(q)` and `knows(c)`, `q` requires `knows(p)` and gains `knows(c)`.  The checker's verdict is `syntactically_grassroots`, with acquaint its introductory act (`programs/gsc`, `intro_acts/2`).

## The cascade

1. The act schemas, `programs/gsc/GKK.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GKK.glp.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

`cascade.sh gkk` runs the cascade on GKK, its run with scripted persons this directory's play.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GKK, as printed, with the words of its pragmas and its two widget declarations, `Approval =::= approval_sentence.` and `Request =::= social_menu(...)`, the request labels its pragmas give, its request question bare, of the request type `Request?` (vGLP, Section 5.4, "The compilation") |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations; GSG's app's file, the same in each app (`../gsg/README.md`) |
| `person.glp` | `miniapp_person/5`: the scripted person, by role, with the keys the person knows, by which its script names its peers, `alice`, `bob` or `carol` |
| `plays.glp` | `play_keys/3`, the mini-app with no super-app |
| `maglp.glp` | `gkk_agent/4`, the mini-app as maGLP: the agent's key by `self_key/1`, a cold call with a conversation to each agent the person names, and the mini-app over the conversations; `programs/tests/multiagent/gkk_maglp_boot.glp` boots it |
| `self.glp` | the platform's types written again, the exports, and `agent/2`, which answers an agent's initial goal, the person's first act, `gkk_agent`, naming the entry |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the figure of the app in GSC's paper, Section 2.1, and their README |

## How the mini-app runs

As GSG's app runs (`../gsg/README.md`, "How the mini-app runs"): the agent runs with the person's key as its `Peer`, its dispatcher draws the agent's questions on the person channel and its output as construct 0, and its network is the conversations the social graph hands the mini-app, one per friend in the super-app.  Acquaintance and introducing are acts among those the agent can reach: the participants of acquaint and introduce are its two roles, so `p` and `q` each run the mini-app over a conversation between them; Carol, the person introduced, takes no part in the introduction, and Bob gains `knows(carol)` by it.  At the commit Alice's agent introduces Bob's agent and Carol's to each other, `intro(carol)` to Bob's and `intro(bob)` to Carol's: Alice's role requires `knows(c)`, `c` an agent variable bound to Carol, and Bob's role gains `knows(c)`, so the introduction rule gives Alice the pair {Bob, Carol} (vGLP, Section 3.2, Definition "Introduction Rule, Introduce Peers"; Section 5.4, "Act schemas as local specifications").  The two agents then exchange ihellos, over a conversation between them where one is open, and know each other, so that Bob's agent can reach Carol's.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gkk\n:limit 20000000\nplay_keys(A, B, C).\n:quit\n' | bin/glpc
```

`play_keys` runs three executions, over a conversation between Alice and Bob, one between Alice and Carol, and one between Bob and Carol.  Each person establishes their identity; Alice and Bob ask to know each other, and so do Alice and Carol; Alice asks to come to know Bob, Bob is asked and answers yes, and each then knows the other; Alice and Carol come to know each other likewise; and Alice, who knows Bob and Carol, introduces Carol to Bob, Bob is asked and answers yes, and he then knows Carol; Carol is not asked.  At the commit Alice's agent introduces Bob's and Carol's to each other; they exchange ihellos and know each other, and Bob asks to come to know Carol, Carol is asked and answers yes, and each then knows the other, Bob holding `knows(carol)` twice, the introduction's and the acquaintance's (GKK:2; GKK has no befriend, which is GSG's).  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with the agent's own key and the persons it knows, as the last view shows them, where the script looks (`menu(K, Ks)`) and each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS:

```
A = [view(alice, [], [], [], [tab('Contacts', knows)]), menu(alice, []), view(alice, [], [], [establish(alice)], [tab('Contacts', knows)]), view(alice, [identity], [], [], [tab('Contacts', knows)]), view(alice, [identity], [], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [identity], [bob], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(bob), identity], [bob, carol], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)]), menu(alice, [bob, carol]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(alice, [knows(carol), knows(bob), identity], [bob, carol], [], [tab('Contacts', knows)]) | _]
B = [view(bob, [], [], [], [tab('Contacts', knows)]), view(bob, [], [], [establish(bob)], [tab('Contacts', knows)]), view(bob, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, bob), yes), view(bob, [identity], [alice], [], [tab('Contacts', knows)]), view(bob, [identity], [alice], [acquaint(alice, bob)], [tab('Contacts', knows)]), view(bob, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), gave(introduce(alice, bob, carol), yes), view(bob, [knows(alice), identity], [alice], [introduce(alice, bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(alice), identity], [alice, carol], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(bob, [knows(carol), knows(carol), knows(alice), identity], [alice, carol], [], [tab('Contacts', knows)]) | _]
C = [view(carol, [], [], [], [tab('Contacts', knows)]), view(carol, [], [], [establish(carol)], [tab('Contacts', knows)]), view(carol, [identity], [], [], [tab('Contacts', knows)]), gave(acquaint(alice, carol), yes), view(carol, [identity], [alice], [], [tab('Contacts', knows)]), view(carol, [identity], [alice], [acquaint(alice, carol)], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice], [], [tab('Contacts', knows)]), view(carol, [knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)]), gave(acquaint(bob, carol), yes), view(carol, [knows(alice), identity], [alice, bob], [acquaint(bob, carol)], [tab('Contacts', knows)]), view(carol, [knows(bob), knows(alice), identity], [alice, bob], [], [tab('Contacts', knows)]) | _]
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): Every word on its screens is the contract's, from its pragmas and those of the contracts it extends (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"); the fields of Acquaint and Introduce are named "person", GKK's "<person>", and the two answers of every question, which no pragma names, as the constants yes and no.  One tab, Contacts, the list of GKK's pragma "Contacts: the persons Alice knows.", which the view names, `tab('Contacts', knows)`.  The request question is drawn by `social_menu`, which the platform declares, with the last value drawn on construct 0: the person's own name; before their identity is established, Establish, GKK's "[Establish]", proposing `establish(Me)`; after it, the persons their agent knows and their contacts, one row each, a person not yet known with Acquaint, proposing `acquaint(Me, Q)`; beside each contact, Introduce, with a field named "person" for the key of the person `C` introduced, proposing `introduce(Me, Q, C)`; and Acquaint with a field named "person" for the key of a peer not listed.  The approval question is drawn by `approval_sentence` at the top of the tab, the sentence the platform writes from GKK's pragmas: "Alice wishes that you know each other." for `acquaint(alice, Me)`, and "Alice asks to introduce Carol to you." for `introduce(alice, Me, carol)`, each with yes and no.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice, Bob and Carol and takes each person's acts by tapping and typing on the tab's controls; `screens/README.md` names the three screens it renders.
