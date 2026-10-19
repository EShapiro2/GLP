# The screens of GSN's app

Alice's and Bob's phones running GSN's app, as the bridge draws it (`../README.md`, "The screens"): the bridge draws the approval question by `approval_sentence`, the widget the app's declaration `Approval =::= approval_sentence.` names (`../self.vglp`; vGLP, Section 6.2, Definition "Widget Declaration"), one sentence for the attempt's class and two buttons labelled for it (`glp_multiagent/lib/ui_runtime/named_widgets.dart`), every other question by the default widget of its type (Definition "Default Widget"), and the agent's thread, construct 0, under the questions.  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts on these widgets, the acts of the plays' scripted persons (`../person.glp`).  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

Before the first screen Alice and Bob establish their identities, ask to know each other and become friends, as on GSG's app (`../../gsg/screens/`).  A request question shows the agent's own key, as on GSG's app, and where a person proposes a class that names them, or signs a speech act of theirs, they type their key as the question shows it.  A speech act is a field of a recursive type, drawn closed with the button `Open` until the person opens it, in a request question and in the thread alike; where a screen shows one opened, the person opened it.  The approval question of a join names the group by its name in its sentence.  A screen of a long form is scrolled to the form's heading.

| File | What it shows |
|---|---|
| `gsn-01-alice-message.png` | Alice's request question, on which she has chosen `propose` and `message`, typed her key, `alice`, and `bob`, and written the speech act `hello`, signed with her key (GSN:1). |
| `gsn-02-bob-message.png` | Bob's thread, which shows the `start` and the `outcome` of Alice's attempt; Bob has opened its message, `hello`, signed `alice`, and nobody asked him. |
| `gsn-03-alice-create.png` | Alice's request question, on which she has chosen `propose` and `create` and typed her key, `alice`, and the group's name, the speech act `friends`, signed with her key (GSN:2). |
| `gsn-04-alice-group-created.png` | Alice's thread, which shows her proposal of `create`, taken at once: the group `friends` exists, and Alice is its leader and first member. |
| `gsn-05-alice-join.png` | Alice's request question, on which she has chosen `propose` and `join` and typed her key, `alice`, `bob` and the group's name, to add Bob to the group (GSN:3). |
| `gsn-06-bob-join.png` | Bob's approval question, "Alice asks you to join the group friends.", and the buttons Join and Decline. |
| `gsn-07-bob-joined.png` | Bob's thread after he taps Join, which shows his `vote` and the `outcome` of the attempt; Bob is now a member of the group. |
| `gsn-08-alice-group.png` | Alice's thread, which shows the `tally` and the `commit` of Bob's joining the group `friends`, whose leader is Alice and whose members are Alice and Bob. |
| `gsn-09-bob-post.png` | Bob's request question, scrolled to its form `post`, on which he has typed his key, `bob`, `alice`, the group's name, and the content `hi_all`, signed with his key, in the order of the class `post` as vGLP's compiler prints it from `GSN.glp` (GSN:5). |
| `gsn-10-alice-group-message.png` | Alice's thread, which shows the `outcome` of Bob's message to the group, opened, the group's name before the content, and below it her machine's proposal to deliver it to Bob; nobody was asked. |
| `gsn-11-bob-group-message.png` | Bob's thread, which shows the `start` and the `outcome` of Alice's delivery of the message `hi_all` to him as a member of the group `friends`. |
