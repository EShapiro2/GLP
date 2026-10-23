# GSG's app

The app of GSG, the grassroots social graph, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSG's act schemas.

## The contract

GSG is GSC's contract (Coordination Appendix C); its clauses are `programs/gsc/GSG.txt`, as GSC's paper, Section 2.2, has them, and its act schemas `programs/gsc/GSG.glp`.  GSG extends GKK, the base contract (`programs/gsc/GKK.txt`; the app `../gkk`), whose clauses it cites: Identity, GKK:1; Meeting, GKK:2; and Introduction, GKK:3.

1. *Friendship.*  Alice and Bob, who know each other personally and are not friends, may jointly decide to become friends by an act of both.
2. *Ending a friendship.*  Alice may end their friendship with Bob by their own act.

GKK:1 is the schema `establish`, guarded by the person, which adds `identity` at them, and every role of the other schemas requires `identity`.  In the app the key pair is the one the super-app holds, which the super-app hands the mini-app as the `Key` of its entry `miniapp/4`, and which the platform's agent takes as its `Peer`: every act the agent takes names its person by that key.  The person establishes their identity by their first request, `propose(establish(P))`, `P` their key, which every request question shows (vGLP, Section 5.4: the request question "shows the agent's own key"), a class of one participant, taken at once.  GKK:2 is the schema `meet`, guarded by both, adding `knows` of the other at each, and GKK:3 `introduce`; GSG:1 is the schema `befriend`, guarded by both, requiring `knows` of the other at each, so the two meet before they befriend, and GSG:2 the schema `unfriend`, guarded by either friend; that the two know each other personally describes no act.

## The cascade

1. The act schemas, `programs/gsc/GSG.glp`; the checker's verdict `syntactically_grassroots` (`cascade/GSG/verdict.txt`).
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GSG.glp.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GSG, as printed, with the words of its pragmas and its two widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)`, the request labels its pragmas give |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice` or `bob` |
| `plays.glp` | `play_friends/2` and `play_held/3`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, and the exports |
| `../gsg_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of GAI's figure of the app, and their README |

## How the mini-app runs

The super-app grants the mini-app the person's key, the person channel, the channel to the social graph and a calendar (GSG paper, Section 2, "What the super-app grants a mini-app").

- The agent runs with the person's key as its `Peer`.
- Its dispatcher draws the agent's questions on the person channel: the request questions and the approval questions.  The program's output to the person is drawn as construct `0`: the agent writes there its view, `view(S, K)`, its machine state and the persons it knows, when it starts and whenever either changes, and a notice, `refused(P, Q, D)`, when an approval the person made, by a request or by an answer yes, ends by a refusal, `P` the part of the refused attempt, `Q` its refuser and `D` the reason, `declined` or `unable(Lacks, Has)`; the screen shows the last view and the notices written after it (vGLP, Section 5.4, "The compilation").
- Its network is the conversations the social graph hands the mini-app at each invitation, one per friend.  Each message `msg(Q, C)` the agent sends goes on the conversation with `Q`; each message arriving on a conversation joins the agent's input.  A message to a participant with whom no conversation is open yet is held, and goes on the conversation, in order, once the social graph opens it.
- The calendar is not read: the platform has no clock.  The agent takes its own transactions when it starts and after each trigger, one at a time until none holds (vGLP, Section 5.4, "Triggers").

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsg\nplay_held(In, ToBob, ToCarol).\n:limit 600000\nplay_friends(A, B).\n:quit\n' | bin/glpc
```

`play_friends` runs two executions over one conversation, the play handing each its end of the root channel as the social graph does.  Each person establishes their identity; Alice and Bob ask to know each other; Alice asks to meet Bob in person, and Bob is asked and answers yes; Alice offers him friendship, and Bob is asked and answers yes; then Bob ends the friendship, and nobody is asked.  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with the persons a request question shows where the script looks (`menu(K, Ks)`) and each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS:

```
A = [view([], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, []), view([identity], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, [bob]), view([friend(bob), knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]) | _]
B = [view([], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), gave(meet(alice, bob), yes), view([knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), gave(befriend(alice, bob), yes), view([friend(alice), knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]) | _]
```

`play_held` runs the router on a hello sent to Bob and one to Carol before any conversation is open, then Bob's conversation, a start to Bob, and Carol's conversation: each hello goes on its recipient's conversation once it opens.

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsg %s/programs/social/graph/core\n:boot %s/programs/apps/gsg_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

Two isolates, each the super-app of `programs/social/graph/core` with its own key pair.  Each person trusts the compiler's key and installs `gsg.glpw`; Alice connects to Bob and they become friends in the super-app's social graph; Alice invites Bob to the mini-app and Bob accepts; the handshake opens their conversation; then the steps of `play_friends` run over it.  The boot harness reduces each event until the program is quiescent, as the device runtime does (IGLP, Implementation Notes: "On each event the agent binds what the event delivers, reduces until quiescent ... there is no clock and no polling"), and after each event the platform's agent takes its own transactions until none holds and then waits for the next (vGLP, Section 5.4, "Triggers"), so each event reduces to quiescence and the boot settles.  The two persons' lines interleave as the isolates run; each person's, in order, held exactly by the suite's Section APPS:

```
[alice] sg(connected(bob)), view([], [], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, []), view([identity], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), menu(alice, [bob]), view([friend(bob), knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(bob), identity], [bob], [tab('Contacts', knows), tab('Friends', friend)])
[bob]   sg(befriend(alice)), sg(connected(alice)), view([], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [], [tab('Contacts', knows), tab('Friends', friend)]), view([identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), gave(meet(alice, bob), yes), view([knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), gave(befriend(alice, bob), yes), view([friend(alice), knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)]), view([knows(alice), identity], [alice], [tab('Contacts', knows), tab('Friends', friend)])
```

### In the Flutter super-app

The app's agent runtime (`glp_multiagent/lib/ui_runtime`) reduces each event until quiescent with a cap, as the boot harness does, and the platform's agent reaches quiescence after each event, as the boot shows.  The asset step, `glp_multiagent/tool/sync_glp_assets.sh`, builds `gsg.glpw` and `gsn.glpw` into the bundle's `social/graph/core` with the other mini-apps' artefacts.  What is not there: the app's play, `superapp_ui/3` in `programs/social/graph/core/home_ui.glp`, scripts Bob to accept an invitation to `ping`, and Bob's acts for GSG's app, installing `gsg.glpw` and accepting the invitation to it, are not in it.

Once Bob's acts are there, Alice installs the app as she installs any: on the Apps panel she trusts the compiler's key, installs the mini-app by its name, `gsg`, and its program, `gsg.glpw`, connects to Bob, and, once he is her friend, invites him to `gsg`.

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): Every word on its screens is the contract's, from its pragmas and those of the contracts it extends (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"); the request `known(Q)` is in no contract, so its button and its field are drawn by their type's names, known and Peer, and the two answers of every question, which no pragma names, as the constants yes and no.  Two tabs, the lists of GKK's and GSG's pragmas, which the view names: Contacts, "the persons Alice knows", and Friends, "the friends of Alice".  The request question is drawn by `social_menu`, which the platform declares (vGLP, Section 6.2, Definition "Widget Declaration"), with the last value drawn on construct 0: in Contacts the person's own name; before their identity is established, Establish, proposing `establish(Me)`; after it, the persons known and their contacts, a contact who is not a friend with Befriend, proposing `befriend(Me, Q)`, and any other with Meet, proposing `meet(Me, Q)`; where the person has two contacts or more, Introduce, proposing `introduce(Me, Q, C)`; and the field Peer with known, answering `known(Q)`; in Friends each friend with Unfriend, proposing `unfriend(Me, Q)`.  The approval question is drawn by `approval_sentence` at the top of the tab it concerns, the sentence the platform writes from the contract's pragmas, with yes and no: "Alice wishes that you know each other." and "Alice asks to introduce Carol to you." in Contacts, and "Alice asks to be your friend." in Friends.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice and Bob over one conversation and takes each person's acts by tapping and typing on the tabs' controls: each taps Establish and adds the other by key; Alice taps Meet on Bob's row, and Bob is asked and taps yes; Alice taps Befriend on Bob's row; Bob is asked in Friends and taps yes; Bob taps Unfriend on Alice's row in Friends, and Alice is not asked.  `screens/README.md` names the three screens it renders, those of GAI's figure.

Had Bob tapped no, the else-branch of his answer is taken: his vote is no, declined, the attempt aborts at both, neither view changes, and Alice's request, her approval, ends by Bob's refusal, so her screen shows the notice `refused(befriend(alice, bob), bob, declined)` (vGLP's `play_refused`, `programs/vglp/cva/social_graph`, shows the same on the platform of GSC's GSG), drawn as the program writes it, the request by its label: "Befriend Alice, Bob — Bob: declined".

## GSN's app

GSN's app, `../gsn`, is this host on GSN's platform: its `miniapp.glp` is this one, the same file, and the suite's Section APPS holds the two equal.

The host carries a message only to a participant with whom the social graph has opened a conversation, at an invitation between friends (GSG paper, Section 2, "Activation and joining").  The platform's initiator knows each participant of its attempt, by a hello on their conversation, so a platform's transactions run only among persons who are friends in the super-app; a participant beyond them needs the warm call of the GSG paper, Section 4.4, "Contact between two participants", which this host does not make.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry: `Peer`, `Class`, `Proposal`, `Request`, `YesNo`, `Reason`, `Vote`, `Decision`, `Rho`, `Names`, `Attempt`, `Cargo`, `Content`, `Net`, `Atom` and `Output`, `Approval` and `Menu`; and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`.  vGLP's Section 6.3 has the compilation write these into each module that imports a compiled procedure; once the compiler does, these copies go.
