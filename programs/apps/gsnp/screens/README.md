# The screens of GSNP's app

Alice's, Bob's and Carol's phones as the construct family draws GSNP's app, one screen with the tabs Friends and Groups, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice, Bob and Carol over two conversations, Alice's with Bob and Bob's with Carol, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gsnp-1-bob-asked.png` | Bob's Friends tab with the question "Alice asks to be your friend." and the buttons Accept and Decline at its top. |
| `gsnp-2-carol-asked.png` | Carol's Friends tab with the question "Bob asks to be your friend." and the buttons Accept and Decline at its top. |
| `gsnp-3-bob-friends.png` | Bob's Friends tab with Alice and Carol his friends, each with End friendship. |
| `gsnp-4-alice-group.png` | Alice's Groups tab with the group friends she leads, its members Bob and Alice, and Close group, and no field or Send in it. |
