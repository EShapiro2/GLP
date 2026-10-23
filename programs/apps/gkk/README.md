# GKK's app

The app of GKK, Grassroots Keys and Knowledge, the base mini-app of the grassroots app, which every other mini-app extends (GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC), as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GKK's act schemas, as `../gsg` is from GSG's.

## The contract

GKK is GSC's contract, the base contract every contract extends (Coordination Appendix C); its clauses are `programs/gsc/GKK.txt`, as GSC's paper, Section 2.1, has them, and its act schemas `programs/gsc/GKK.glp`:

1. *Identity.*  Alice may act to establish their identity by choosing their own key pair.  They make themselves known by the public key, sign their acts with the private key, and are accountable for every act signed with it.
2. *Meeting.*  Alice and Bob may come to know each other by an act of both.
3. *Introduction.*  If Alice and Bob know each other and Alice knows Carol, then an act of Alice and Bob can introduce Carol to Bob, following which Bob knows Carol.

GKK:1 is the schema `establish(p?)`, which adds `identity` at the person, and every role of the other two schemas requires `identity`.  In the app the key pair is the one the super-app holds, which the super-app hands the mini-app as the `Key` of its entry `miniapp/4`, and which the platform's agent takes as its `Peer`.  The person establishes their identity by their first request, `propose(establish(P))`, `P` their key, which every request question shows (vGLP, Section 5.4: the request question "shows the agent's own key"), a class of one participant, taken at once.  GKK:2 is `meet(p?, q?)`, guarded by both, adding `knows` of the other at each; GKK:3 is `introduce(p?, q? ; c)`, guarded by both, `c` the person introduced, who takes no part: `p` requires `knows(q)` and `knows(c)`, `q` requires `knows(p)` and gains `knows(c)`.  The checker's verdict is `syntactically_grassroots`, with meet its introductory act (`programs/gsc`, `intro_acts/2`).

## The cascade

1. The act schemas, `programs/gsc/GKK.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GKK.glp.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

`cascade.sh gkk` runs the cascade on GKK, its run with scripted persons this directory's play.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GKK, as printed, with the words of its pragmas and its two widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)`, the request labels its pragmas give |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations; GSG's app's file, the same in each app (`../gsg/README.md`) |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice`, `bob` or `carol` |
| `plays.glp` | `play_keys/3`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, and the exports |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the figure of the app in GSC's paper, Section 2.1, and their README |

## How the mini-app runs

As GSG's app runs (`../gsg/README.md`, "How the mini-app runs"): the agent runs with the person's key as its `Peer`, its dispatcher draws the agent's questions on the person channel and its output as construct 0, and its network is the conversations the social graph hands the mini-app, one per friend in the super-app.  Meeting and introducing are acts among those the agent can reach: the participants of meet and introduce are its two roles, so `p` and `q` each run the mini-app over a conversation between them; Carol, the person introduced, takes no part in the introduction, and Bob gains `knows(carol)` by it.  At the commit Alice's agent introduces Bob's agent and Carol's to each other, `intro(carol)` to Bob's and `intro(bob)` to Carol's: Alice's role requires `knows(c)`, `c` an agent variable bound to Carol, and Bob's role gains `knows(c)`, so the introduction rule gives Alice the pair {Bob, Carol} (vGLP, Section 3.2, Definition "Introduction Rule, Introduce Peers"; Section 5.4, "Act schemas as local specifications").  The two agents then exchange ihellos, over a conversation between them where one is open, and know each other, so that Bob's agent can reach Carol's.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gkk\n:limit 20000000\nplay_keys(A, B, C).\n:quit\n' | bin/glpc
```

`play_keys` runs three executions, over a conversation between Alice and Bob, one between Alice and Carol, and one between Bob and Carol.  Each person establishes their identity; Alice and Bob ask to know each other, and so do Alice and Carol; Alice asks to meet Bob in person, Bob is asked and answers yes, and each then knows the other; Alice and Carol meet likewise; and Alice, who knows Bob and Carol, introduces Carol to Bob, Bob is asked and answers yes, and he then knows Carol; Carol is not asked.  At the commit Alice's agent introduces Bob's and Carol's to each other; they exchange ihellos and know each other, and Bob asks to meet Carol in person, Carol is asked and answers yes, and each then knows the other, Bob holding `knows(carol)` twice, the introduction's and the meeting's (GKK:2; GKK has no befriend, which is GSG's).  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with the persons a request question shows where the script looks (`menu(K, Ks)`) and each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS:

```
A = [view([], [], [tab('Contacts', knows)]), menu(alice, []), view([identity], [], [tab('Contacts', knows)]), view([identity], [bob], [tab('Contacts', knows)]), view([identity], [bob, carol], [tab('Contacts', knows)]), view([knows(bob), identity], [bob, carol], [tab('Contacts', knows)]), view([knows(carol), knows(bob), identity], [bob, carol], [tab('Contacts', knows)]), menu(alice, [bob, carol]) | _]
B = [view([], [], [tab('Contacts', knows)]), view([identity], [], [tab('Contacts', knows)]), view([identity], [alice], [tab('Contacts', knows)]), gave(meet(alice, bob), yes), view([knows(alice), identity], [alice], [tab('Contacts', knows)]), gave(introduce(alice, bob, carol), yes), view([knows(carol), knows(alice), identity], [alice], [tab('Contacts', knows)]), view([knows(carol), knows(alice), identity], [alice, carol], [tab('Contacts', knows)]), view([knows(carol), knows(carol), knows(alice), identity], [alice, carol], [tab('Contacts', knows)]) | _]
C = [view([], [], [tab('Contacts', knows)]), view([identity], [], [tab('Contacts', knows)]), view([identity], [alice], [tab('Contacts', knows)]), gave(meet(alice, carol), yes), view([knows(alice), identity], [alice], [tab('Contacts', knows)]), view([knows(alice), identity], [alice, bob], [tab('Contacts', knows)]), gave(meet(bob, carol), yes), view([knows(bob), knows(alice), identity], [alice, bob], [tab('Contacts', knows)]) | _]
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): Every word on its screens is the contract's, from its pragmas and those of the contracts it extends (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"); the request `known(Q)` is in no contract, so its button and its field are drawn by their type's names, known and Peer, and the two answers of every question, which no pragma names, as the constants yes and no.  One tab, Contacts, the list of GKK's pragma "Contacts: the persons Alice knows.", which the view names, `tab('Contacts', knows)`.  The request question is drawn by `social_menu`, which the platform declares, with the last value drawn on construct 0: the person's own name; before their identity is established, Join, proposing `establish(Me)`; after it, the persons their agent knows and their contacts, one row each, a person not yet met with Meet, proposing `meet(Me, Q)`; where the person has two contacts or more, Introduce, with a picker at each of its places `Q` and `C`, each named Peer, proposing `introduce(Me, Q, C)`; and the field Peer with known, answering `known(Q)`.  The approval question is drawn by `approval_sentence` at the top of the tab, the sentence the platform writes from GKK's pragmas: "Alice wishes that you know each other." for `meet(alice, Me)`, and "Alice asks to introduce Carol to you." for `introduce(alice, Me, carol)`, each with yes and no.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice, Bob and Carol and takes each person's acts by tapping and typing on the tab's controls; `screens/README.md` names the three screens it renders.
