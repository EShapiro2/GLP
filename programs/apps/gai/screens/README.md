# The screens of GAI's app

The screens of GAI's figure of its app (GAI's paper, Figure "The GAI app"; Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, and its answers at 21:09 UTC), and the screen of GAI's GSNP figure that comes from this app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph): Alice's and Bob's phones as the construct family draws the app, in the tabs Friends and AI agents, drawn from the last view each agent wrote (`../README.md`, "The screens").  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice, Alf and Bob over three conversations, Alice's with Alf, Alf's with Bob and Bob's with Alice, Alf scripted by `../person.glp`, its script the AI agent's program, and plays the paper's Table 2 exactly, taking Alice's and Bob's acts by tapping and typing on their tabs' controls.  The developer's key is `acme_ltd`, drawn Acme Ltd, a key's underscores drawn as spaces and each word capitalised.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gai-b-alice-agents.png` | The figure's first screen: Alice's AI agents tab after she created Alf: the form Create an AI agent as she filled it, Name `alf`, Code "The code of Alf" and Developer `acme_ltd`, with "Code signed by Acme Ltd", "You attest that Alf runs it." and Create, disabled, Alf being her AI agent; and Alf listed with Retire.  Her Friends tab is badged with the question of the next screen. |
| `gai-c-alice-asked.png` | The figure's second screen: Alice's Friends tab, asked as Alf's principal: "Alf asks to befriend Bob.", "Approve?", Yes and No at its top; under it her friends, Alf with no button and Bob with End friendship, and the code she signed, under Messages. |
| `gai-d-bob-asked.png` | The figure's third screen: Bob's Friends tab: "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme Ltd", "Befriend Alf?", Yes and No at its top; under it Alf, with Befriend, and Alice, his friend, with End friendship. |
| `gai-a-bob-chat-alf.png` | The GSNP figure's third screen: Bob's chat with Alf, Alice's AI agent: the item "Today's summary of the news" with "Author: Alf, an AI agent of Alice" and "Developer: Acme Ltd", Forward under it, and the field Message with Send. |

Alf sends the item to Bob by message (`message(p?, q ; x)` adds `item(x)` at `q`, `programs/gai/GAI.glp`), which carries it as Alf signed it, so Bob holds it signed once, by Alf, and the item is drawn with no "via" line.  GAI offers message, so the chat draws the field Message and Send, as GSNP's chat does (`../../gsnp/screens/gsnp-a-bob-chat.png`).
