# The screens of GSNP's app

The screens of GAI's figure of GSNP's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 4 and 5), in the figure's order: Bob's phone as the construct family draws the app, one screen with the tabs Friends and Groups, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Dan, Eve, Carol, Bob and Fay over four conversations, Dan's with Eve, Eve's with Carol, Carol's with Bob and Bob's with Fay, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gsnp-a-bob-chat.png` | Bob's chat with Carol, the item "Flood on Main Street" with "Author: Dan" and "via Eve, via Carol", and Forward under it. |
| `gsnp-b-bob-forward.png` | The same screen after Bob taps Forward and picks Fay, with the question "Forward to Fay?  You warrant that you know Carol personally." and the buttons Yes and No. |
