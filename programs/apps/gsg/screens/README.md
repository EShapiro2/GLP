# The screens of GSG's app

Alice's and Bob's phones running GSG's app, as the bridge draws it (`../README.md`, "The screens"): the bridge draws the approval question by `approval_sentence`, the widget the app's declaration `Approval =::= approval_sentence.` names (`../self.vglp`; vGLP, Section 6.2, Definition "Widget Declaration"), a sentence and the buttons Yes and No (`glp_multiagent/lib/ui_runtime/named_widgets.dart`), every other question by the default widget of its type (Definition "Default Widget"), and the agent's thread, construct 0, under the questions.  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts on these widgets, the acts of the plays' scripted persons (`../person.glp`).  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

A request question's field `Key` shows the agent's own key, not edited, and its field `List(Key)` the persons the agent knew when it asked the question (vGLP, Section 5.4: the request question "shows the agent's own key and the persons it knows").  Where a person proposes a class that names them, they type their key as the question shows it.

| File | What it shows |
|---|---|
| `gsg-01-alice-establish.png` | Alice's first request question, which shows her key, `alice`, and nobody known, on which she has chosen `propose` and `establish` and typed her key as it shows it, to establish her identity (GSG:1). |
| `gsg-02-alice-menu.png` | Her next request question, which shows her key and still nobody known, on which she has chosen `known` and typed `bob`; her thread below shows her identity established. |
| `gsg-03-alice-befriend.png` | Her open request question, which shows her key, on which she has chosen `propose` and `befriend` and typed her key, `alice`, and `bob` (GSG:2); her thread shows `discover` of Bob after their hellos. |
| `gsg-04-alice-menu-bob.png` | Her next request question, which shows her key and Bob known, and her thread, which shows her proposal, attempt 2 of the class `befriend(alice, bob)`. |
| `gsg-05-bob-approval.png` | Bob's approval question, "alice asks to befriend you.", with the attempt's number and rank and the buttons Yes and No, below his open request question, which shows his key, `bob`. |
| `gsg-06-bob-yes.png` | Bob's thread after he taps Yes, which shows his `answer`, his `vote` and the `outcome` of the attempt; the question is withdrawn. |
| `gsg-07-alice-friends.png` | Alice's thread, which shows the `tally` and the `commit` of the attempt, after which Alice and Bob are friends. |
| `gsg-08-bob-unfriend.png` | Bob's request question, which shows his key, `bob`, on which he has chosen `propose` and `unfriend` and typed his key and `alice` to end the friendship (GSG:3). |
| `gsg-09-bob-unfriended.png` | Bob's thread, which shows his proposal, attempt 3 of the class `unfriend(bob, alice)`, its `tally` and its `commit`. |
| `gsg-10-alice-unfriended.png` | Alice's thread, which shows the `start` and the `outcome` of Bob's attempt; the friendship has ended, and nobody asked her. |
