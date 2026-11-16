# The screens of GC's app

The screens of the figure of GC's app in GC's paper and GSC's (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 3), in the figure's order: Alice's and Bob's phones as the construct family draws the app, the tab Coins, drawn from the last view the agent wrote (`../README.md`, "The screens"); and the six phones at the end of the village market of GC's paper, each villager's Coins tab, for its Figure fig:village-screens (Currencies #11 Cowork, 2026-10-21 07:25 UTC, item 6).  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; taken on 2026-11-16 at GLP main 8f1f1f57, GC as adopted from the Calculus's publication of that day, each scenario in a simulator run of its own; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GSG's app are (`../../gsg/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/3` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls: each establishes their identity, and Alice meets Bob by his key, the cold call he answers yes (GKK:1, GKK:2); Alice issues two Alice-coins and Bob three Bob-coins (Notes:1, Issue); Alice taps Swap, the button "[Swap]" of Coins' Interface pragmas, in Bob's row of her Contacts, fills the four fields beside it, issuer given alice, amount given 2, issuer wanted bob and amount wanted 3, and taps Swap, asking Bob to swap her two Alice-coins for his three Bob-coins, and he answers yes (Notes:2, Swap); and Alice pays two Bob-coins to Bob, their issuer (Coins:1, Pay issuer).  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gc-a-alice-mint.png` | Alice issues coins (Notes:1): her Coins tab, the tab of Coins, once she has typed 2 into Issue's field amount, on her Contacts tab, and tapped Issue: a row for her coins, Alice, and their count, 2, with Swap, Pay and Deposit. |
| `gc-b-bob-asked.png` | Alice asks Bob to swap (Notes:2): Bob's first tab, Contacts, badged, with the question of Coins' Interface pragmas, "Alice asks to swap coins with you.", under it amount given: 2, issuer wanted: Bob, amount wanted: 3, Contacts: Bob, Coins: Alice 2, and the answers yes and no, at its top; below, his name, Acquaint with its field person and Issue with its field amount. |
| `gc-c-bob-holdings.png` | Bob's Coins tab once he has answered yes and the swap has committed: a row for the Alice-coins he holds, Alice, 2, with Swap, Pay and Deposit, and Pay issuer and Redeem, the two buttons of a coin whose issuer is another person. |
| `gc-d-alice-pay.png` | Alice pays two Bob-coins to Bob by Pay issuer (Coins:1): her Coins tab after the payment: one Bob-coin left, the row Bob, 1, with Swap, Pay, Deposit, Pay issuer and Redeem. |

## The village market

The same test then plays the village market of GC's paper, Appendix "Village Market Demonstration", on the app, the six villagers at their phones and a conversation between each two who transact, seven in all, each act a tap: each establishes their identity and meets those they transact with (GKK:1, GKK:2); each issues the coins the operations need, 30, 39, 25, 35, 20 and 28 (Notes:1); then the six operations, symmetric mutual credit, Alice with Bob and with Charlie 15 coins each, Charlie with Eve 10, Eve with Frank 10, asymmetric mutual credit, Diana's 20 diana-coins for 24 bob-coins and her 15 for 18 frank-coins, Eve's portfolio swap, 5 frank-coins to Charlie for 5 alice-coins, and Alice's sale of 10 bob-coins to Eve for 4 frank-coins, each a swap proposed by Swap in the other party's row of the proposer's Contacts with its four fields, which the other party is asked about and answers yes (Notes:2); the payments, Bob to Alice 5, Eve to Charlie 6, Frank to Diana 3, Eve to Alice 3, in coins of the payee, by Pay issuer (Coins:1); and Frank's redemption of 5 diana-coins, one at a time, each against a frank-coin Diana holds (Coins:2).  At the end each villager's Coins tab lists the holdings of the paper's Table 1, a row for each issuer with its count, their own coins included, in the order of the villager's state, and is shot.

Each row of a Coins tab carries Swap, Pay and Deposit, and Pay issuer and Redeem where the coin's issuer is another person.

| File | What it shows |
|---|---|
| `gc-village-alice.png` | Alice's Coins tab: Frank 4, Alice 8, Charlie 15, Bob 5. |
| `gc-village-bob.png` | Bob's: Diana 20, Alice 10. |
| `gc-village-charlie.png` | Charlie's: Charlie 6, Frank 5, Eve 10, Alice 10. |
| `gc-village-diana.png` | Diana's: Diana 8, Frank 13, Bob 24. |
| `gc-village-eve.png` | Eve's: Bob 10, Alice 2, Frank 1, Charlie 4. |
| `gc-village-frank.png` | Frank's: Frank 5, Diana 7, Eve 10. |
