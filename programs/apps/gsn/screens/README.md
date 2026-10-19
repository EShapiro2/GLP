# The screens of GSN's app

The screens of GAI's figure of GSN's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 5, its sentence for a join at 11:35 UTC, and its screen (c) at 13:31 UTC), in the figure's order: Bob's phone as the construct family draws the app, one screen with the tabs Friends and Groups, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen, 390 by 844 logical pixels at a device pixel ratio of 3, in the light theme, set in Roboto.

They are written by `glp_multiagent/test/apps_screens_test.dart`, which loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`, after `bash tool/sync_glp_assets.sh`:

```bash
flutter test --update-goldens test/apps_screens_test.dart
```

| File | What it shows |
|---|---|
| `gsn-a-bob-chat.png` | Bob's chat with Alice in his Friends tab, her message "Hi Bob" with her name under it. |
| `gsn-b-bob-asked-to-join.png` | Bob's Groups tab with the question "Alice asks you to join the group friends." and the buttons Join and Decline at its top. |
| `gsn-c-bob-group.png` | Bob's Groups tab with the group friends, led by Alice, the message "Hi all" with Bob's name under it as Alice delivered it to him, and no member list. |
