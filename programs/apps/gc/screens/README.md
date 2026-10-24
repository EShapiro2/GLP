# The screens of GC's app

The screens of the figure of GC's app in GC's paper and GSC's (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 3), in the figure's order: Alice's and Bob's phones as the construct family draws the app, the tab Coins, drawn from the last view the agent wrote (`../README.md`, "The screens"); and the six phones at the end of the village market of GC's paper, each villager's Coins tab, for its Figure fig:village-screens (Currencies #11 Cowork, 2026-10-21 07:25 UTC, item 6).  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GSG's app are (`../../gsg/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls: each establishes their identity, and Alice meets Bob by his key, the cold call he answers yes (GKK:1, GKK:2); Alice mints two Alice-coins and Bob three Bob-coins (GC:1); Alice asks Bob to swap her two Alice-coins for his three Bob-coins, and he answers yes (GC:2); and Alice pays two Bob-coins to Bob (GC:3).  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gc-a-alice-mint.png` | Alice mints (GC:1): her Coins tab, the tab of GC, once she has typed 2 into Mint's field, GC's "<amount>", and tapped Mint, GC's "[Mint]": a row for her coins, Alice, and their count, 2. |
| `gc-b-bob-asked.png` | Alice asks Bob to swap (GC:2): Bob's Coins tab with GC's question "Alice asks to swap coins with you.", under it the fields of the swap, each named by GC's words, issuer given: Alice, amount given: 2, issuer wanted: Bob, amount wanted: 3, and the answers yes and no, at its top; below, Mint and his row, Bob, 3. |
| `gc-c-bob-holdings.png` | Bob's Coins tab once he has answered yes and the swap has committed: a row for the Alice-coins he holds, Alice, 2, with the buttons of the acts that name a person beside the proposer, Meet, Introduce, Swap, Pay and Redeem. |
| `gc-d-alice-pay.png` | Alice pays two Bob-coins to Bob (GC:3): her Coins tab once she has tapped Pay on Bob's row, typed 2 into its field and sent it: one Bob-coin left, the row Bob, 1, with the buttons of the acts that name a person. |

## The village market

The same test then plays the village market of GC's paper, Appendix "Village Market Demonstration", on the app, the six villagers at their phones and a conversation between each two who transact, seven in all, each act a tap: each establishes their identity and meets those they transact with (GKK:1, GKK:2); each mints the coins of their issue the operations need, 30, 39, 25, 35, 20 and 28 (GC:1); then the six operations, symmetric mutual credit, Alice with Bob and with Charlie 15 coins each, Charlie with Eve 10, Eve with Frank 10, asymmetric mutual credit, Diana's 20 diana-coins for 24 bob-coins and her 15 for 18 frank-coins, Eve's portfolio swap, 5 frank-coins to Charlie for 5 alice-coins, and Alice's sale of 10 bob-coins to Eve for 4 frank-coins, each a swap the other party is asked about and answers yes (GC:2); the payments, Bob to Alice 5, Eve to Charlie 6, Frank to Diana 3, Eve to Alice 3, in coins of the payee (GC:3); and Frank's redemption of 5 diana-coins, one at a time, each against a frank-coin Diana holds (GC:4).  At the end each villager's Coins tab lists the holdings of the paper's Table 1, a row for each issuer with its count, their own coins included, and is shot.

| File | What it shows |
|---|---|
| `gc-village-alice.png` | Alice's Coins tab: 5 bob-coins, 8 alice-coins, 15 charlie-coins, 4 frank-coins. |
| `gc-village-bob.png` | Bob's: 10 alice-coins, 20 diana-coins. |
| `gc-village-charlie.png` | Charlie's: 10 alice-coins, 10 eve-coins, 6 charlie-coins, 5 frank-coins. |
| `gc-village-diana.png` | Diana's: 24 bob-coins, 13 frank-coins, 8 diana-coins. |
| `gc-village-eve.png` | Eve's: 4 charlie-coins, 1 frank-coin, 2 alice-coins, 10 bob-coins. |
| `gc-village-frank.png` | Frank's: 7 diana-coins, 10 eve-coins, 5 frank-coins. |
