# The screens of GSN's app

The screens of GAI's figure of GSN's app (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 5, its sentence for a join at 11:35 UTC, and its screen (c) at 13:31 UTC), in the figure's order: Bob's phone, and Alice's for the leader's acts, as the construct family draws the app, one screen with the tabs Contacts, Friends, Groups, Messages and Posts, the lists of GKK's, GSG's and GSN's pragmas, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as vGLP's figure of the app is (`vGLP/figures/gsg-app-friends.png`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/3` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gsn-a-bob-chat.png` | Bob's Friends tab once Alice has sent him Hi Bob (GSN:1): a row for Alice, the atom friend(alice), with Unfriend, GSG's "[Unfriend]", and Message, GSN's "[Message]"; the message itself is listed in his Messages tab. |
| `gsn-b-bob-asked-to-join.png` | Bob's first tab, Contacts, badged, with GSN's question "Alice asks you to join her group.", the atoms Alice's role requires, "Groups: friends, Alice" and "Friends: Bob", the group's name, friends, signed by Alice, and the answers yes and no at its top (GSN:3); under it his name, Acquaint with its field person, Create with its field name, and Alice with Introduce. |
| `gsn-c-bob-group.png` | Bob's Groups tab once he has joined and posted Hi all (GSN:3, GSN:4): the group friends, its leader Alice, with Leave, GSN's "[Leave]", and Post, GSN's "[Post]"; the post delivered to him is listed in his Posts tab. |
| `gsn-d-alice-group.png` | Alice's Groups tab with the group friends she leads (GSN:2, GSN:3, GSN:4): the group's name and hers, with Close, GSN's "[Close]", Join, GSN's "[Join]", and Post own, GSN's "[Post own]", each drawn in the group's row, an atom the act requires at the proposer's role naming the group (vGLP 6e9f71d, Section 6.2). |
