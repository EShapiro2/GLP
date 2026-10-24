# The screens of GSNP's app

The screens of GAI's two figures of the app, "The GSNP app" and "The GAI app", taken from GSNP's app, the full platform, which extends GAI (Code_inbox.md, GAI #8 Cowork, 2026-10-24 10:47 UTC: "The four screens of the GAI figure are taken from the GSNP app, the full platform").  The app is compiled from GSNP's contract and pragmas and those of the contracts it extends, and drawn by the construct family (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as vGLP's figure of the app is (`vGLP/figures/gsg-app-friends.png`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app and runs its entry `miniapp/4`, once for Dan, Eve, Carol, Bob and Fay over four conversations, and once for Acme Ltd, Alice, Alf and Bob over four, Alf scripted by `../person.glp`, its script the AI agent's program; it takes each person's acts by tapping and typing on the tabs' controls.  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

GAI's Figure "The GSNP app":

| File | What it shows |
|---|---|
| `gsnp-a-bob-chat.png` | Bob's Items tab: the item "Flood on Main Street", under it "Dan, Eve, Carol", its author and the agents that forwarded it, in order, and beside it Enter, Forward, Post own and Post forward. |
| `gsnp-b-bob-forward.png` | The same tab after Bob taps Forward and picks Fay under Peer: the warranty "You warrant that you know personally the agent you received this from." and the button Forward. |
| `gsnp-c-fay-chat.png` | Fay's Items tab after the forward: the item with "Dan, Eve, Carol, Bob" under it. |
| `gai-a-bob-chat-alf.png` | Bob's Items tab: the item "Today's summary of the news", under it "Alf", its author, and beside it Enter, Forward, Post own and Post forward. |

GAI's Figure "The GAI app", in the order of its play: Alice enters Alf; she proposes Alf's meeting Bob, Alf and Bob saying yes, since a machine takes a proposal only where it knows every agent it sends to (vGLP, Section 4) and Alf comes to know Bob by that meeting; Alf proposes befriending Bob, and Alice and Bob each answer on their phones (Code_inbox.md, GAI #8 Cowork, 2026-10-24 14:39 UTC):

| File | What it shows |
|---|---|
| `gai-b-alice-agents.png` | Alice's AI agents tab after she entered Alf: Sign with its field text; Alf, under it "Alice, The code of Alf", her record of Alf, its principal and its attestation; and beside Alf the buttons of the acts whose variable its row fills. |
| `gai-c-alice-asked.png` | The same tab as Alf, Alice's AI agent, proposes befriending Bob, its program pressing Agent befriend, the button drawn at its guarding role: at the top Alice, its principal, asked GAI's question to the principal, "Alf asks to befriend Bob.", with yes and no. |
| `gai-d-bob-asked.png` | Bob's AI agents tab, asked the same proposal of Alf's by GAI's question to the other agent, "Alf, an AI agent of Alice, asks to befriend you.", with yes and no. |
| `gai-e-bob-friends.png` | Bob's Friends tab after his yes: Alf, his friend, under it "Alice, The code of Alf", his record of Alf, and beside it the buttons of the acts whose variable its row fills. |

The words on these screens are the contracts' pragmas, the platform's yes and no, and Peer, the name of the type of a place no pragma names a field for (vGLP, Section 6.2, "The words a platform shows").  The developer, Acme Ltd, is not drawn: a row shows the atoms naming its item, and Bob's record of Alf, `agent_of(alf, alice, y)`, is drawn by Alice and the text of the attestation `y`, not by its signers; and GAI's contract carries one question for each guarding role of its line, in the order of the act's roles, the AI agent, the other agent, the principal and the other agent's guardian (`programs/gai/GAI.txt`, "Knowing and relations"), so Alice, the principal, is asked her own question, naming Alf and Bob, and Bob his, naming Alf and Alice, its principal, and not its developer.
