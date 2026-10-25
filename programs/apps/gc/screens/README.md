# The screens of GC's app

The screens of the figure of GC's app in GC's paper and GSC's (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 3), in the figure's order: Alice's and Bob's phones as the construct family draws the app, the tab Coins, drawn from the last view the agent wrote (`../README.md`, "The screens"); and the six phones at the end of the village market of GC's paper, each villager's Coins tab, for its Figure fig:village-screens (Currencies #11 Cowork, 2026-10-21 07:25 UTC, item 6).  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto; taken on 2026-10-25 on branch GC-shots2 at GLP 19e57f2f, from GC.txt and GC.glp as they stand at GLP main f7eb3824, swap and redeem adding knows of the issuer at the receiving role; the simulator's screenshot is the display alone, so it is framed as the phone, in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GSG's app are (`../../gsg/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice and for Bob over one conversation, and takes each person's acts by tapping and typing on the tabs' controls: each establishes their identity, and Alice meets Bob by his key, the cold call he answers yes (GKK:1, GKK:2); Alice mints two Alice-coins and Bob three Bob-coins (GC:1); Alice taps Swap, GC's "[Swap]", in Bob's row of her Contacts, fills the four fields beside it, issuer given alice, amount given 2, issuer wanted bob and amount wanted 3, and taps Swap, asking Bob to swap her two Alice-coins for his three Bob-coins, and he answers yes (GC:2); and Alice pays two Bob-coins to Bob (GC:3).  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The script builds the bundled assets (`tool/sync_glp_assets.sh`), boots the simulator, sets its status bar, and runs the scenario there (`integration_test/apps_screens_test.dart`), one person's phone on the screen at a time; at each screen the host takes the simulator's screenshot, `xcrun simctl io <device> screenshot --mask=alpha`, and `tool/sim_shot.py` draws the bezel around it and writes it here, under the screen's name.  The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gc-a-alice-mint.png` | Alice mints (GC:1): her Coins tab, the tab of GC, once she has typed 2 into Mint's field, GC's "<amount>", and tapped Mint, GC's "[Mint]": the field amount, Mint, and a row for her coins, Alice, and their count, 2, with no button beside it, Pay and Redeem taking a person other than the viewer (vGLP, Section 6.2). |
| `gc-b-bob-asked.png` | Alice asks Bob to swap (GC:2): Bob's Coins tab with GC's question "Alice asks to swap coins with you.", under it the fields of the swap, each named by GC's words, issuer given: Alice, amount given: 2, issuer wanted: Bob, amount wanted: 3, and the answers yes and no, at its top; below, Mint and his row, Bob, 3; and beside the tab's name Coins the number of approval questions open on the tab, 1. |
| `gc-c-bob-holdings.png` | Bob's Coins tab once he has answered yes and the swap has committed: a row for the Alice-coins he holds, Alice, 2, with Pay and Redeem beside it, the acts that delete an Alice-coin at the proposer's role; Swap, which requires that the proposer knows the other person, is drawn in the rows of his Contacts (vGLP, Section 6.2). |
| `gc-d-alice-pay.png` | Alice pays two Bob-coins to Bob (GC:3): her Coins tab once she has tapped Pay on Bob's row, typed 2 into its field and sent it: one Bob-coin left, the row Bob, 1, with Pay and Redeem. |

## The village market

The same test then plays the village market of GC's paper, Appendix "Village Market Demonstration", on the app, the six villagers at their phones and a conversation between each two who transact, seven in all, each act a tap: each establishes their identity and meets those they transact with (GKK:1, GKK:2); each mints the coins of their issue the operations need, 30, 39, 25, 35, 20 and 28 (GC:1); then the six operations, symmetric mutual credit, Alice with Bob and with Charlie 15 coins each, Charlie with Eve 10, Eve with Frank 10, asymmetric mutual credit, Diana's 20 diana-coins for 24 bob-coins and her 15 for 18 frank-coins, Eve's portfolio swap, 5 frank-coins to Charlie for 5 alice-coins, and Alice's sale of 10 bob-coins to Eve for 4 frank-coins, each a swap proposed by Swap in the other party's row of the proposer's Contacts with its four fields, which the other party is asked about and answers yes (GC:2); the payments, Bob to Alice 5, Eve to Charlie 6, Frank to Diana 3, Eve to Alice 3, in coins of the payee (GC:3); and Frank's redemption of 5 diana-coins, one at a time, each against a frank-coin Diana holds (GC:4).  At the end each villager's Coins tab lists the holdings of the paper's Table 1, a row for each issuer with its count, their own coins included, in the order of the villager's state, and is shot: beside the row of each issuer but the villager, Pay and Redeem, the villager knowing each such issuer by meeting them or by holding their coins, and beside their own row no button.

| File | What it shows |
|---|---|
| `gc-village-alice.png` | Alice's Coins tab: Frank 4, Alice 8, Charlie 15, Bob 5. |
| `gc-village-bob.png` | Bob's: Diana 20, Alice 10. |
| `gc-village-charlie.png` | Charlie's: Charlie 6, Frank 5, Eve 10, Alice 10. |
| `gc-village-diana.png` | Diana's: Diana 8, Frank 13, Bob 24. |
| `gc-village-eve.png` | Eve's: Bob 10, Alice 2, Frank 1, Charlie 4. |
| `gc-village-frank.png` | Frank's: Frank 5, Diana 7, Eve 10. |
