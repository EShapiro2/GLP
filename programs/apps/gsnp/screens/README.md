# The screens of GSNP's app

The screens of GAI's figure of GSNP's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 4 and 5), in the figure's order: Bob's phone as the construct family draws the app, one screen with the tabs Friends and Groups, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as vGLP's figure of the app is (`vGLP/figures/gsg-app-friends.png`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Dan, Eve, Carol, Bob and Fay over four conversations, Dan's with Eve, Eve's with Carol, Carol's with Bob and Bob's with Fay, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gsnp-a-bob-chat.png` | Bob's chat with Carol, the item "Flood on Main Street" with "Author: Dan" and "via Eve, via Carol", and Forward under it. |
| `gsnp-b-bob-forward.png` | The same screen after Bob taps Forward and picks Fay, with the question "Forward to Fay?  You warrant that you know Carol personally." and the buttons Yes and No. |
