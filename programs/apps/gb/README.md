# GB's app

The app of GB, grassroots bonds, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GB's act schemas and pragmas, as `../gc` is from GC's (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 19:57 and 20:27 UTC, task 2; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 4, and 2026-10-24 07:24 UTC).

## The contract

GB is Currencies' contract (Coordination Appendix C); its clauses and pragmas are `programs/currencies/bonds/GB.txt`, as GB's paper, Section 3.1, has them in its box, and its act schemas `programs/currencies/bonds/GB.glp`, as its Appendix A has them.  GB extends GC, Currencies' coins contract (`programs/currencies/coins/GC.txt`; the app `../gc`), and through it GKK, the base contract (`programs/gsc/GKK.txt`; the app `../gkk`), whose clauses it cites, and not GSG: no act of it requires a friend.

GKK:1 is `establish`, which adds `identity`, `knows` being reflexive, GKK:2 `acquaint`, GKK:3 `introduce`.  GC:1 is `issue(p? ; k)`, which adds k copies of `coin(p)` at `p`, and GB:1 `issue_bond(p? ; d, k)`, which adds k copies of `bond(p, d)` at `p` where `d` is later than the current date, `today(t)`, which the agent reads from the calendar; GC:2 is `swap`, coins for coins, and GB:2 `swap_coin_for_bond` and `swap_bonds`, each guarded by both, who know each other; GC:3 is `pay`; GC:4 is `redeem`, a coin against a coin, and GB:3 `redeem_coin_against_bond`, `redeem_bond` and `redeem_bond_against_bond`, each guarded by the bearer, a bond presented only where its maturity date is at most the current date at its issuer; GB:4 is the escrow, `deposit` and `deposit_bond`, guarded by the depositor and the escrow agent, for a beneficiary and on a condition, `release` and `release_bond`, guarded by the escrow agent and the beneficiary, and `return` and `return_bond`, guarded by the escrow agent and the depositor.  GC:5 describes no act.  The checker's verdict is `syntactically_grassroots`.

## The cascade

1. The act schemas, `programs/currencies/bonds/GB.glp`, and the pragmas, `GB.txt`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP with the words of the pragmas (Section 5.4, "The compilation" and "The contract's words"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GB, as printed, with the words of its pragmas and its two widget declarations, the request labels and placements its pragmas give |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations; the file of the other apps, but that it hands the agent the calendar, GB requiring the day |
| `person.glp` | `miniapp_person/5`: the scripted person, by role, with the keys the person knows, by which its script names its peers, `alice`, `bob` or `carol`, and `market_alice` to `market_escrow`, the parties of the village market |
| `plays.glp` | `play_bonds/3`, the mini-app with no super-app, the play writing the calendars, and `play_village/7`, the village market of GB's paper on it |
| `self.glp` | the platform's types written again, and the exports |
| `../gb_boot.glp` | the super-app hosting the app on three smartphones, the persons of `play_bonds`, under `:boot`, as far as the first day: the days after it, which the play writes on a person's calendar off another person's screen, have no source on a phone under the boot harness, which waits on no clock |
| `../gb_village_boot.glp` | the super-app hosting the app on seven smartphones, the parties of `play_village`, under `:boot`, as far as the first day likewise |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the app for GB's paper, and their README |

## The day

GB requires the day: `issue_bond`, `redeem_bond` and `redeem_bond_against_bond` require `today(t)`, which the compiled agent holds beside its app state, `t` the last date it read from its calendar (vGLP, Section 5.4, "Act schemas as local specifications").  The calendar is the fourth grant of `miniapp/4`, a stream of dates the device writes from its clock, one a day (GSG, miniapp/4); here the play writes it, and in the screen test the test does (`glp_multiagent/test/apps_screens_test.dart`, `_Run.day`).  `today` is no atom of the view, so no tab shows it and no scripted person waits on it.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gb\n:limit 60000000\nplay_bonds(A, B, C).\n:quit\n' | bin/glpc
```

`play_bonds` runs three executions, alice's with a conversation with each of bob and carol.  Each establishes their identity, day 1 the first date of each calendar; alice comes to know bob and then carol; alice issues five Alice-coins and three Alice-bonds due day 10, and bob four Bob-bonds due day 5; alice asks bob for a loan, two Alice-coins for three Bob-bonds, `swap_coin_for_bond(alice, bob, alice, 2, bob, 5, 3)`, and he answers yes; the play then writes day 5 on bob's calendar, the issuer's; alice redeems a mature Bob-bond against an Alice-coin bob holds, `redeem_bond(alice, bob, 5, alice)`, and nobody is asked; and alice deposits two Alice-coins with carol for bob on the condition day 10, `deposit(alice, carol, alice, 2, bob, speech(utterance('day 10'), alice))`, and carol answers yes.  The run then suspends.  The suite's Section APPS holds each person's screen.

`play_village` runs the village market of GB's paper, Section 7.1, on the app: the six villagers and the escrow agent, under the roles `market_alice` to `market_escrow` (`person.glp`), ten conversations, one between each two who transact and alice's with frank, whom she comes to know (GKK:2) before she asks eve for frank-coins; the seven operations at the paper's amounts, charlie's deposit with the escrow agent for frank on the condition day 30, released to frank by the escrow agent and frank; and, once frank's calendar gives day 28, diana's redemption of a mature frank-bond against a frank-coin frank holds, her presentation of it before that day, after frank's fifth redemption, refused by frank's machine, the bond not mature by the current date at its issuer.  The play writes the calendars: day 1 on each; day 25 on bob's once he holds the diana-coins of his loan; day 28 on frank's once diana's screen shows her presentation refused; and day 30 on the escrow agent's once it holds the deposit.  Each party's last view holds the holdings of the paper's Table 1, held by Section APPS.

```bash
printf '../programs/apps/gb\n:limit 400000000\nplay_village(A, B, C, D, E, F, G).\n:quit\n' | bin/glpc
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): three tabs, the lists of the pragmas, Contacts (GKK's), Coins and Bonds (GB's); on Coins a row for each issuer with the count, on Bonds a row for the bonds of each issuer and maturity.  The buttons are those of the pragmas, one for each act in brackets: Establish, Acquaint and Introduce (GKK's); Issue, Swap, Pay and Redeem (GC's); Issue bond with its fields maturity and amount; Swap coin for bond and Swap bonds; Redeem coin against bond, Redeem bond and Redeem bond against bond; Deposit and Deposit bond, with the fields amount and condition; Release and Release bond; Return and Return bond.  An approval question is drawn at the top of the tab of its contract, with the sentence the pragmas give the role asked.  `screens/README.md` names the four screens.
