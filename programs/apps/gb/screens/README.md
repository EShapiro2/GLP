# The screens of GB's app

The screens of GB's app for GB's paper (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 19:57 and 20:27 UTC, task 2; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 4: "a mint of a bond, a loan asked of the borrower, the redemption of a matured bond, an escrow deposit"), in that order: Alice's, Bob's and Carol's phones as the construct family draws the app, from the last view each agent wrote (`../README.md`, "The screens").  Each file is one phone's screen taken on the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point, its status bar at 9:41 with full signal, Wi-Fi and battery, in the light theme, set in Roboto, framed as the phone in a black bezel with rounded corners, 1080 by 2220 pixels on white, as the screens of GC's app are (`../../gc/screens/`).

They are taken by `glp_multiagent/tool/sim_screens.sh`, which plays on the simulator the scenario of `glp_multiagent/test/apps_screens_test.dart`: it loads this app, runs its entry `miniapp/4` for Alice, Bob and Carol, a conversation between Alice and each of the others, each with a calendar the test writes, day 1 first, and takes each person's acts by tapping and typing on the tabs' controls: each establishes their identity and gets day 1 as their date (GKK:1, GB:2); Alice meets Bob and Carol by their keys (GKK:2); Alice mints five Alice-coins and three Alice-bonds maturing at day 10, and Bob four Bob-bonds maturing at day 5 (GB:1); Alice asks Bob for a loan, two Alice-coins for three Bob-bonds, and he answers yes (GB:3); the test writes day 5 on Bob's calendar, and his date advances to it (GB:2); Alice redeems a matured Bob-bond against an Alice-coin Bob holds (GB:5); and Alice deposits two Alice-coins with Carol, the escrow agent, who answers yes (GB:7).  From `glp_multiagent/`:

```bash
bash tool/sim_screens.sh
```

The same scenario runs headless in the suite, `flutter test test/apps_screens_test.dart`, the phones side by side in the test's view: it takes every act and checks every screen, and writes nothing.

| File | What it shows |
|---|---|
| `gb-a-alice-mint-bond.png` | Alice mints bonds (GB:1): her Coins tab, the tab of GB, with Mint bond's fields, GB's "<maturity> <amount>", filled with 10 and 3, before she taps Mint bond, GB's "[Mint bond]"; above it Mint coin and her row of five Alice-coins. |
| `gb-b-bob-asked-loan.png` | Alice asks Bob for a loan (GB:3): Bob's Coins tab with GB's question "Alice asks to swap coins or bonds with you." and the answers yes and no at its top. |
| `gb-c-alice-redeem.png` | Alice redeems a matured Bob-bond (GB:5), Bob's date at day 5: her Bonds tab, a row for her Alice-bonds, 3, maturing at day 10, and one for her Bob-bonds, 3, maturing at day 5, with Redeem's fields, GB's "<maturity given> <issuer wanted> <maturity wanted>", filled with 5 and alice, the maturity wanted left empty for a coin, before she taps Redeem, GB's "[Redeem]". |
| `gb-d-carol-asked-deposit.png` | Alice deposits two Alice-coins with Carol (GB:7): Carol's Coins tab with GB's question "Alice asks to deposit coins or bonds with you." and the answers yes and no at its top. |
