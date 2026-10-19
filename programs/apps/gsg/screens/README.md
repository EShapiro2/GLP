# The screens of GSG's app

The screens of GAI's figure of GSG's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 5), in the figure's order: Alice's and Bob's phones as the construct family draws the app, one screen with the tab Friends, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tab's controls.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gsg-a-bob-asked.png` | Bob's Friends tab with the question "Alice asks to be your friend." and the buttons Accept and Decline at its top. |
| `gsg-b-alice-friends.png` | Alice's Friends tab after the befriend commits, Bob listed with End friendship. |
| `gsg-c-alice-ended.png` | Alice's Friends tab after Bob ends the friendship, Bob listed with Befriend. |
