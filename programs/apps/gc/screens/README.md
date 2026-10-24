# The screens of GC's app

The screens of the figure of GC's app in GSC's paper (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC), in the figure's order: Alice's and Bob's phones as the construct family draws the app, the tab Coins, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GSG's app are (`../../gsg/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls: each establishes their identity, and Alice meets Bob by his key, the cold call he answers yes (GKK:1, GKK:2); Alice mints two Alice-coins and Bob three Bob-coins (GC:1); Alice asks Bob to swap her two Alice-coins for his three Bob-coins, and he answers yes (GC:2); and Alice pays two Bob-coins to Bob (GC:3).  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gc-a-alice-mint.png` | Alice mints (GC:1): her Coins tab, the tab of GC, once she has typed 2 into Mint's field, GC's "<amount>", and tapped Mint, GC's "[Mint]": a row for her coins, Alice, and their count, 2. |
| `gc-b-bob-asked.png` | Alice asks Bob to swap (GC:2): Bob's Coins tab with GC's question "Alice asks to swap coins with you." and the answers yes and no at its top, and his row, Bob, 3. |
| `gc-c-bob-holdings.png` | Bob's Coins tab once he has answered yes and the swap has committed: a row for the Alice-coins he holds, Alice, 2, with the buttons of the acts that name a person beside the proposer, Meet, Introduce, Swap, Pay and Redeem. |
| `gc-d-alice-pay.png` | Alice pays two Bob-coins to Bob (GC:3): her Coins tab once she has tapped Pay on Bob's row and typed 2 into its field: one Bob-coin left, the row Bob. |
