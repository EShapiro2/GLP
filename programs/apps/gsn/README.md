# GSN's app

The app of GSN, the grassroots social network, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GSN's act schemas.  It is GSG's app, `../gsg`, on GSN's platform: the same entry and network (`miniapp.glp`, the same file), and the same scripted person with GSN's scripts.

## The contract

GSN is GSC's contract (Coordination Appendix C); its clauses are `programs/gsc/GSN.md`, and its act schemas `programs/gsc/GSN.glp`.  GSN extends GSG, which extends GKK, and cites their clauses, GKK:1 to GKK:3 and GSG:1 and GSG:2 (`../gkk/README.md`, `../gsg/README.md`):

1. *Message.*  Alice may send a message to their friend Bob, with content they created or received from another agent.
2. *Group.*  Alice may create a group, and is then its leader and first member.
3. *Joining.*  Alice, the leader of a group, may add their friend Bob to it, by an act of both.
4. *Leaving.*  Bob, a member of a group, may leave it by their own act; when the leader leaves, the group ends.  A member of a group who is not a friend of its leader is removed from it.
5. *Group message.*  Bob, a member of a group, may send a message to the group, with content they created or received from another agent, and the leader delivers it to every member.

Fourteen schemas: GKK's `establish`, `meet` and `introduce`, GSG's `befriend` and `unfriend`, and `message`, `create`, `join`, `leave`, `close`, `drop`, `post_own`, `post` and `deliver` (`GSN.md`, "The schemas, and what they realise"; GLP 120bf8bd).  `drop`, which no role guards, ends the membership of a member who is not the leader's friend; `post` and `deliver` require the member's friendship at the leader.  A group is named by its leader and a name, and content and a group's name are speech acts, `speech(Utterance, Peer)`, an utterance and the person who signs it.

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
| `self.vglp` | the app's widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu.` |
| `screens/` | the screens of GAI's figure of the app, and their README |
| `../gsn_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gsn\n:limit 2000000\nplay_network(A, B).\n:quit\n' | bin/glpc
```

`play_network` runs two executions over one conversation, the play handing each its end of the root channel as the social graph does: Alice and Bob each establish their identity (GKK:1), ask to know each other, meet in person (GKK:2) and become friends (GSG:1); Alice creates the group `friends` and adds Bob to it, and he answers yes; she sends Bob the message 'Hi Bob'; Bob sends the group the message 'Hi all', and Alice delivers it to him; Bob leaves the group.  Alice sends her message last: nothing on her screen shows a message committed, her state being unchanged by it, and her agent drops a request made while the message is open.  The persons' screens, the views their agents write with Bob's answers, held exactly by the suite's Section APPS:

```
A = [view([], []), view([identity], []), view([identity], [bob]), view([friend(bob), identity], [bob]), view([member(dot(community(alice), speech(utterance(friends), alice))), listed(alice, speech(utterance(friends), alice)), friend(bob), identity], [bob]), view([listed(bob, ...), member(dot(community(alice), ...)), listed(alice, ...), friend(bob), identity], [bob]), view([posted(speech(utterance('Hi all'), bob), speech(utterance(friends), alice)), ...], [bob]), view([delivered(speech(utterance('Hi all'), bob), bob, speech(utterance(friends), alice)), posted(...), ...], [bob]), view([delivered(...), posted(...), member(dot(community(alice), ...)), listed(alice, ...), friend(bob), identity], [bob]) | _]
B = [view([], []), view([identity], []), view([identity], [alice]), gave(befriend(alice, bob), yes), view([friend(alice), identity], [alice]), gave(join(alice, bob, speech(utterance(friends), alice)), yes), view([member(dot(community(alice), speech(utterance(friends), alice))), friend(alice), identity], [alice]), view([received(speech(utterance('Hi Bob'), alice), alice), member(dot(community(alice), ...)), friend(alice), identity], [alice]), view([received_in(speech(utterance('Hi all'), bob), alice, speech(utterance(friends), alice)), received(...), member(dot(community(alice), ...)), friend(alice), identity], [alice]), view([received_in(...), received(...), friend(alice), identity], [alice]) | _]
```

A `...` abbreviates a speech act or an atom named in full before it; the suite holds the lines whole.  A member atom holds the group's community name, `<alice>.n` written `dot(community(alice), n)`, one argument, as GSN.glp writes `member(<a>.n)` (vGLP, Section 5.4, "Act schemas as local specifications").

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gsn %s/programs/social/graph/core\n:boot %s/programs/apps/gsn_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

As GSG's app's (`../gsg/README.md`, "On two smartphones"): the two super-apps befriend, Alice invites Bob to `gsn`, the conversation opens, and the steps of `play_network` run over it; each event reduces to quiescence and the boot settles.  Each person's lines, in order, held exactly by the suite's Section APPS, are what the super-app told them, `sg(connected(bob))` for Alice and `sg(befriend(alice))`, `sg(connected(alice))` for Bob, followed by the views of `play_network` above.  The Flutter super-app is not yet within reach for the reason `../gsg/README.md` names, Bob's acts for `gsn` not being in `superapp_ui/3`.

## The screens

What each person sees, as the construct family draws the app: GSG's app's tab Friends (`../gsg/README.md`, "The screens") and a second tab, Groups, the request type offering `create` (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`).  In Friends, tapping a friend opens the chat with them: the messages received from them, each its text with its author's name under it, and a field with Send, proposing `message(Me, Q, x)`, `x` the text typed, signed by the person.  In Groups: the field New group with Create, proposing `create(Me, n)`; each group the person is in, with its name, its leader and its messages, and, in a group the person leads, its members, whom the leader's state lists (`listed(q, n)`, which `join` adds at the leader); in a group the person leads, Add member, a picker of their friends not in it, proposing `join(Me, Q, n)`, and Close group; in any group, a field with Send; and in a group they do not lead, Leave group.  The approval question of a join is drawn at the top of Groups, "Alice asks you to join the group friends.", with Join and Decline.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice and Bob over one conversation and takes each person's acts on the tabs' controls: each taps Join and adds the other by key; Alice taps Befriend and Bob Accept; Alice opens her chat with Bob, types Hi Bob and taps Send, and Bob is not asked; Bob opens his chat with Alice; Alice creates the group friends on Groups and taps Bob under Add member; Bob is asked on Groups and taps Join; and Bob types Hi all in the group and taps Send, proposing `post(bob, alice, x, n)` in the order of post's head, and Alice's agent, which opens the delivery, delivers it to him.  `screens/README.md` names the screens it renders.

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry: `Peer`, `Speech`, `Utterance`, `Class`, `Proposal`, `Request`, `YesNo`, `Decision`, `Attempt`, `Cargo`, `Content`, `Net`, `Atom` and `Output`, `Approval` and `Menu`; and the types the canonical compilation forms for them, `Question`, `Drawn` and `Ask(Q)`.  Once vGLP's compiler writes these into each module that imports a compiled procedure (vGLP, Section 6.3), these copies go.
