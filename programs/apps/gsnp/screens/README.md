# The screens of GSNP's app

Alice's, Bob's and Carol's phones running GSNP's app, as the bridge draws it today (`../README.md`, "The screens"): the approval question by `approval_sentence`, the widget the app's declaration `Approval =::= approval_sentence.` names (`../self.vglp`; vGLP, Section 6.2, Definition "Widget Declaration"), one sentence for the attempt's class and two buttons labelled for it (`glp_multiagent/lib/ui_runtime/named_widgets.dart`); the request question and the person's thread, construct 0, by the default widget of each type (Definition "Default Widget").  vGLP is changing both, so these screens show the forms and the thread as the default widgets draw them now.  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice, Bob and Carol over two conversations, Alice's with Bob and Bob's with Carol, and takes each person's acts on these widgets, the acts of the play's scripted persons (`../person.glp`).  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

Before the first screen Alice and Bob establish their identities, ask to know each other and become friends, as on GSG's app (`../../gsg/screens/`), and Carol establishes hers and asks to know Bob.  A speech act is a field of a recursive type, drawn closed with the button `Open` until the person opens it, in a request question and in the thread alike; where a screen shows one opened, the person opened it.  An item signed again is a speech act whose utterance is a speech act: the menu `Utterance` with `speech` chosen, the signature inside it first.  A screen of a long form is scrolled to the form's heading.

| File | What it shows |
|---|---|
| `gsnp-01-alice-sign.png` | Alice's request question, on which she has chosen `propose` and `sign`, typed her key, `alice`, and written the item, the speech act `Flood on Main Street`, signed with her key (GSNP:1, Authorship). |
| `gsnp-02-alice-forward.png` | Alice's request question, on which she has chosen `propose` and `forward` and typed her key, `alice`, `bob`, and the item she holds, `Flood on Main Street`, signed `alice` (GSN:1, Message, and GSNP:2, Forwarding). |
| `gsnp-03-bob-item.png` | Bob's thread, which shows the `start` and the `outcome` of Alice's forward, nobody having been asked, the platform asking nobody on a forward; Bob has opened its item, `Flood on Main Street`, signed `alice`, which he holds signed again by alice, who forwarded it. |
| `gsnp-04-carol-approval.png` | Carol's approval question, "Bob asks to be your friend.", and the buttons Accept and Decline, after Bob has asked to know her and proposed `befriend`. |
| `gsnp-05-bob-forward.png` | Bob's request question, scrolled to its form `forward`, on which he has typed his key, `bob`, `carol`, and the item he holds: `Flood on Main Street`, signed `alice`, its author, and signed again `alice`, who forwarded it to him. |
| `gsnp-06-carol-item.png` | Carol's thread, which shows the `start` and the `outcome` of Bob's forward, nobody having been asked, the item opened down to its text: `Flood on Main Street`, signed `alice`, signed again `alice`, and forwarded by `bob`, so that Carol holds it signed by Alice and then by Bob. |
