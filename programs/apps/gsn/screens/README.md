# The screens of GSN's app

The screens of GAI's figure of GSN's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 5, its sentence for a join at 11:35 UTC, and its screen (c) at 13:31 UTC), in the figure's order: Bob's phone as the construct family draws the app, one screen with the tabs Friends and Groups, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as vGLP's figure of the app is (`vGLP/figures/gsg-app-friends.png`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gsn-a-bob-chat.png` | Bob's chat with Alice in his Friends tab, her message "Hi Bob" with her name under it. |
| `gsn-b-bob-asked-to-join.png` | Bob's Groups tab with the question "Alice asks you to join the group friends." and the buttons Join and Decline at its top. |
| `gsn-c-bob-group.png` | Bob's Groups tab with the group friends, led by Alice, the message "Hi all" with Bob's name under it as Alice delivered it to him, and no member list. |
