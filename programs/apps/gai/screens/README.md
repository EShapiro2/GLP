# The screens of GAI's app

The screen of GAI's GSNP figure that comes from GAI's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph): Bob's phone as the construct family draws the app, one screen with the tab Friends, drawn from the last view the agent wrote (`../README.md`, "The screens").  The file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

It is written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice, Alf and Bob over three conversations, Alice's with Alf, Alf's with Bob and Bob's with Alice, Alice and Alf scripted by `../person.glp`, Alf's script the AI agent's program, and takes Bob's acts by tapping and typing on the tab's controls.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gai-a-bob-chat-alf.png` | Bob's chat with Alf, Alice's AI agent: the item "Today's summary of the news" with "Author: Alf, an AI agent of Alice", Forward under it, and the field Message with Send. |

Alf sends the item to Bob by message (`message(p?, q ; x)` adds `item(x)` at `q`, `programs/gai/GAI.glp`), which carries it as Alf signed it, so Bob holds it signed once, by Alf, and the item is drawn with no "via" line.  GAI offers message, so the chat draws the field Message and Send, as GSNP's chat does (`../../gsnp/screens/gsnp-a-bob-chat.png`).
