# The screens of GKK's app

The screens of the figure of GKK's app in GSC's paper, Section 2.1 (Code_inbox.md, GSC #5 Cowork, 2026-10-20 17:53 UTC), in the figure's order: Alice's and Bob's phones as the construct family draws the app, one screen with the tab Contacts, the list of GKK's pragmas, drawn from the last view the agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GSG's app are (`../../gsg/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice, Bob and Carol, over a conversation between Alice and Bob and one between Alice and Carol, and takes each person's acts by tapping and typing on the tab's controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gkk-a-alice-identity.png` | Alice's Contacts tab, the tab of GKK, before she establishes her identity (GKK:1): her name; Establish, GKK's "[Establish]", an act that names no one beside its proposer, on the tab of its contract; and Acquaint, GKK's "[Acquaint]", with the field for the key of a peer not listed, named "person", GKK's "<person>" (vGLP e286276, Section 5.4, "The contract's words"). |
| `gkk-b-bob-asked-to-meet.png` | Alice types Bob's key into Acquaint's field and taps Acquaint (GKK:2): her request is the cold call, the hello carrying it, which asks Bob GKK's question "Alice wishes that you know each other.", with yes and no, at the top of his Contacts tab; his yes is his request of the same acquaintance (vGLP, Section 5.4, "The compilation"). |
| `gkk-c-bob-contacts.png` | Bob's Contacts tab once he has answered yes to "Alice asks to introduce Carol to you." (GKK:3): a row for each atom of knows in his state, Carol and then Alice, each with the buttons of the acts that name a person beside the proposer, Acquaint and Introduce. |
