# GSG's app

The app of GSG, the grassroots social graph, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSG's act schemas.

## The contract

GSG is GSC's contract (Coordination Appendix C; GSC owns every contract that other contracts extend); its clauses are `programs/gsc/GSG.md`, as GAI's paper has them, and its act schemas `programs/gsc/GSG.glp`:

1. *Identity.*  A person may act to establish their identity by choosing their own key pair.  They identify themselves with the public key, sign their acts with the private key, and are accountable for every act signed with it.
2. *Friendship.*  Two persons who are not friends may jointly decide to become friends by an act of both, after which they are friends.
3. *Ending a friendship.*  A person may end their friendship with another person by their own act, and then the friendship ends.

GSG:1 is the schema `establish`, guarded by the person, which adds `identity` at them, and every role of the other two schemas requires `identity`.  In the app the key pair is the one the super-app holds, which the super-app hands the mini-app as the `Key` of its entry `miniapp/4`, and which the platform's agent takes as its `Peer`: every act the agent takes names its person by that key.  The person establishes their identity by their first request, `propose(establish(P))`, `P` their key, which every request question shows (vGLP, Section 5.4: the request question "shows the agent's own key"), a class of one participant, taken at once.  GSG:2 is the schema `befriend`, guarded by both, and GSG:3 the schema `unfriend`, guarded by either friend.

## The cascade

1. The act schemas, `programs/gsc/GSG.glp`; the checker's verdict `syntactically_grassroots` (`cascade/GSG/verdict.txt`).
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GSG.glp.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GSG, as printed |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice` or `bob` |
| `plays.glp` | `play_friends/2` and `play_held/3`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, and the exports |
| `self.vglp` | the app's widget declaration, `Approval =::= approval_sentence.` |
| `../gsg_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |

## How the mini-app runs

The super-app grants the mini-app the person's key, the person channel, the channel to the social graph and a calendar (GSG paper, Section 2, "What the super-app grants a mini-app").

- The agent runs with the person's key as its `Peer`.
- Its dispatcher draws the agent's questions on the person channel: the request questions and the approval questions.  The agent's thread, the program's output to the person, is drawn as construct `0`.
- Its network is the conversations the social graph hands the mini-app at each invitation, one per friend.  Each message `msg(Q, C)` the agent sends goes on the conversation with `Q`; each message arriving on a conversation joins the agent's input.  A message to a participant with whom no conversation is open yet is held, and goes on the conversation, in order, once the social graph opens it.
- The calendar is not read: the platform has no clock.  The agent takes its own transactions when it starts and after each trigger, one at a time until none holds (vGLP, Section 5.4, "Triggers").

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsg\nplay_held(In, ToBob, ToCarol).\n:limit 600000\nplay_friends(A, B).\n:quit\n' | bin/glpc
```

`play_friends` runs two executions over one conversation, the play handing each its end of the root channel as the social graph does.  Each person establishes their identity; Alice asks to know Bob and offers him friendship; Bob is asked and answers yes; then Bob ends the friendship, and nobody is asked.  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens, held exactly by the suite's Section APPS:

```
A = [menu(alice, []), propose(attempt(alice, 1, 1, establish(alice))), discover(bob), menu(alice, [bob]), propose(attempt(alice, 2, 2, befriend(alice, bob))), tally(attempt(alice, 2, 2, befriend(alice, bob))), commit(attempt(alice, 2, 2, befriend(alice, bob))), start(attempt(bob, 3, 3, unfriend(bob, alice))), outcome(attempt(bob, 3, 3, unfriend(bob, alice))) | _]
B = [propose(attempt(bob, 1, 1, establish(bob))), discover(alice), start(attempt(alice, 2, 2, befriend(alice, bob))), gave(befriend(alice, bob), yes), answer(attempt(alice, 2, 2, befriend(alice, bob))), vote(attempt(alice, 2, 2, befriend(alice, bob))), outcome(attempt(alice, 2, 2, befriend(alice, bob))), propose(attempt(bob, 3, 3, unfriend(bob, alice))), tally(attempt(bob, 3, 3, unfriend(bob, alice))), commit(attempt(bob, 3, 3, unfriend(bob, alice))) | _]
```

`play_held` runs the router on a hello sent to Bob and one to Carol before any conversation is open, then Bob's conversation, a start to Bob, and Carol's conversation: each hello goes on its recipient's conversation once it opens.

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsg %s/programs/social/graph/core\n:boot %s/programs/apps/gsg_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

Two isolates, each the super-app of `programs/social/graph/core` with its own key pair.  Each person trusts the compiler's key and installs `gsg.glpw`; Alice connects to Bob and they become friends in the super-app's social graph; Alice invites Bob to the mini-app and Bob accepts; the handshake opens their conversation; then the steps of `play_friends` run over it.  The boot harness reduces each event until the program is quiescent, as the device runtime does (IGLP, Implementation Notes: "On each event the agent binds what the event delivers, reduces until quiescent ... there is no clock and no polling"), and after each event the platform's agent takes its own transactions until none holds and then waits for the next (vGLP, Section 5.4, "Triggers"), so each event reduces to quiescence and the boot settles.  The two persons' lines interleave as the isolates run; each person's, in order, held exactly by the suite's Section APPS:

```
[alice] sg(connected(bob)), menu(alice, []), propose(attempt(alice, 1, 1, establish(alice))), discover(bob), menu(alice, [bob]),
        propose(attempt(alice, 2, 2, befriend(alice, bob))), tally(...), commit(...), start(attempt(bob, 3, 3, unfriend(bob, alice))), outcome(...)
[bob]   sg(befriend(alice)), sg(connected(alice)), propose(attempt(bob, 1, 1, establish(bob))), discover(alice),
        start(attempt(alice, 2, 2, befriend(alice, bob))), gave(befriend(alice, bob), yes), answer(...), vote(...), outcome(...),
        propose(attempt(bob, 3, 3, unfriend(bob, alice))), tally(...), commit(...)
```

A `(...)` names the attempt named in full before it; the suite holds the lines whole.

### In the Flutter super-app

The app's agent runtime (`glp_multiagent/lib/ui_runtime`) reduces each event until quiescent with a cap, as the boot harness does, and the platform's agent reaches quiescence after each event, as the boot shows.  The asset step, `glp_multiagent/tool/sync_glp_assets.sh`, builds `gsg.glpw` and `gsn.glpw` into the bundle's `social/graph/core` with the other mini-apps' artefacts.  What is not there: the app's play, `superapp_ui/3` in `programs/social/graph/core/home_ui.glp`, scripts Bob to accept an invitation to `ping`, and Bob's acts for GSG's app, installing `gsg.glpw` and accepting the invitation to it, are not in it.

Once Bob's acts are there, Alice installs the app as she installs any: on the Apps panel she trusts the compiler's key, installs the mini-app by its name, `gsg`, and its program, `gsg.glpw`, connects to Bob, and, once he is her friend, invites him to `gsg`.

## The screens

What each person sees, step by step, as the bridge draws the constructs of the platform's two questions, and the thread: the approval question by `approval_sentence`, the widget the app's declaration in `self.vglp` names (vGLP, Section 6.2, Definition "Widget Declaration"), one sentence for the attempt's class and two buttons labelled for it (`glp_multiagent/lib/ui_runtime/named_widgets.dart`), and the request question and the thread by the default widgets (Definition "Default Widget").

The super-app, first: Alice and Bob each install the app; Alice connects to Bob, Bob is asked on his super-app's card and consents, and they are friends in the super-app's social graph; Alice invites Bob to `gsg`, and the conversation between their two executions opens.

Then the app:

1. **Alice's request question.**  A form headed `menu`, with three fields.  The first, `Key`, shows her agent's own key, `alice`, and is not edited.  The second, `List(Key)`, shows the persons her agent knows: none.  The third, `Request`, is a menu of two forms: `propose`, whose one field `Proposal`, the classes a person proposes, is a menu of `establish(Peer)`, with one peer field, and `befriend(Peer, Peer)` and `unfriend(Peer, Peer)`, each with two peer fields; and `known`, with one peer field, `Peer`.  She chooses `propose`, then `establish`, types her key, `alice`, as the question shows it, and submits; her thread shows `propose(attempt(alice, 1, 1, establish(alice)))`, the attempt's initiator, its number, its rank and its class, taken at once, Alice its one participant.  On her next request question, still showing none, she chooses `known`, types `bob` and submits.  Her next request question is drawn at once, showing none: it is asked when the agent reads her request, before Bob is known.
2. **Bob's request question**, the same form.  He establishes his identity likewise, his thread showing `propose(attempt(bob, 1, 1, establish(bob)))`; then he chooses `known`, types `alice` and submits.
3. **The threads.**  Each agent sends the other a hello on their conversation; once each has sent and received one, Alice's thread shows `discover(bob)` and Bob's `discover(alice)`.
4. **Alice offers friendship.**  On her open request question she chooses `propose`, then `befriend`, and types her key, `alice`, and `bob`.  Her next request question shows `bob`.  Her thread shows `propose(attempt(alice, 2, 2, befriend(alice, bob)))`, her second attempt.
5. **Bob is asked.**  His thread shows `start(attempt(alice, 2, 2, befriend(alice, bob)))`, and his approval question is drawn: "Alice asks to be your friend.", and two buttons, Accept and Decline, granting `yes` and `no`.  He taps Accept; the question is withdrawn, and his thread shows `answer(...)` and then `vote(...)`.
6. **They are friends.**  Alice's thread shows `tally(...)` and `commit(...)`; Bob's shows `outcome(...)`.
7. **Bob ends the friendship.**  On his request question he chooses `propose`, then `unfriend`, and types his key, `bob`, and `alice`.  His thread shows `propose(attempt(bob, 3, 3, unfriend(bob, alice)))`, `tally(...)` and `commit(...)`.  Alice is not asked: her thread shows `start(attempt(bob, 3, 3, unfriend(bob, alice)))` and `outcome(...)`.

Had Bob tapped Decline in step 5, the else-branch of his answer is taken: his thread shows `answer(...)`, `vote(...)` and `outcome(...)`, his vote no, and Alice's shows `tally(...)` and no commit, the attempt aborted at both (vGLP's `play_refused`, `programs/vglp/cva/social_graph`, shows the same on GSN's platform).

## GSN's app

GSN's app, `../gsn`, is this host on GSN's platform: its `miniapp.glp` is this one, the same file, and the suite's Section APPS holds the two equal.

The host carries a message only to a participant with whom the social graph has opened a conversation, at an invitation between friends (GSG paper, Section 2, "Activation and joining").  The platform's initiator knows each participant of its attempt, by a hello on their conversation, so a platform's transactions run only among persons who are friends in the super-app; a participant beyond them needs the warm call of the GSG paper, Section 4.4, "Contact between two participants", which this host does not make.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry: `Peer`, `Class`, `Proposal`, `Request`, `YesNo`, `Decision`, `Attempt`, `Cargo`, `Content`, `Net`, `Txn` and `Output`, `Approval` and `Menu`; and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`.  vGLP's Section 6.3 has the compilation write these into each module that imports a compiled procedure; once the compiler does, these copies go.
