# GB's app

The app of GB, grassroots bonds, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GB's act schemas and pragmas, as `../gc` is from GC's (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 19:57 and 20:27 UTC, task 2; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 4, and 2026-10-24 07:24 UTC).

## The contract

GB is Currencies' contract (Coordination Appendix C); its clauses and pragmas are `programs/currencies/bonds/GB.txt`, as GB's paper, Section 3.1, has them in its box, and its act schemas `programs/currencies/bonds/GB.glp`, as its Appendix A has them.  GB extends GKK, the base contract (`programs/gsc/GKK.txt`; the app `../gkk`), whose clauses it cites, and not GSG: no act of it requires a friend.

GKK:1 is `establish`, GKK:2 `acquaint`, GKK:3 `introduce`.  GB:1 is `mint_coin(p? ; k)`, which adds k copies of `coin(p)` at `p`, and `mint_bond(p? ; d, k)`, which adds k copies of `bond(p, d)` at `p` where `d` is later than `p`'s date; GB:2 is `first_date` and `advance`, unguarded, which give `p` the day as their date once and then take the date to the next day while the day is not past, the day `today(d)` read from the calendar; GB:3 is the swap, `swap_cc` to `swap_bb`, a coin or a bond given and a coin or a bond taken, guarded by both, who know each other; GB:4 is `pay`; GB:5 is the redemption, `redeem_cc` to `redeem_bb`, guarded by the bearer, a bond presented only where it is mature by its issuer's date; GB:7 is the escrow, `deposit_c`, `deposit_b`, `release_c`, `release_b`, `return_c` and `return_b`, each guarded by both parties.  GB:6 describes no act.  The checker's verdict is `syntactically_grassroots`.

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
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice`, `bob` or `carol`, and `market_alice` to `market_escrow`, the parties of the village market |
| `plays.glp` | `play_bonds/3`, the mini-app with no super-app, the play writing the calendars, and `play_village/7`, the village market of GB's paper on it |
| `self.glp` | the platform's types written again, and the exports |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the app for GB's paper, and their README |

## The day

GB requires the day: `first_date` and `advance` require `today(d)`, which the compiled agent holds beside its app state, `d` the last date it read from its calendar (vGLP, Section 5.4, "Act schemas as local specifications").  The calendar is the fourth grant of `miniapp/4`, a stream of dates the device writes from its clock, one a day (GSG, miniapp/4); here the play writes it, and in the screen test the test does (`glp_multiagent/test/apps_screens_test.dart`, `_Run.day`).  Neither `today` nor the person's date is on a tab: no pragma lists `date`.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gb\n:limit 60000000\nplay_bonds(A, B, C).\n:quit\n' | bin/glpc
```

`play_bonds` runs three executions, alice's with a conversation with each of bob and carol.  Each establishes their identity and gets day 1, the first date of each calendar, as their date; alice comes to know bob and then carol; alice mints five Alice-coins and three Alice-bonds maturing at day 10, and bob four Bob-bonds maturing at day 5; alice asks bob for a loan, two Alice-coins for three Bob-bonds, `swap_cb(alice, bob, alice, 2, bob, 5, 3)`, and he answers yes; the play then writes day 5 on bob's calendar, and his date advances a day at a time to day 5, and, once his screen shows it, on alice's; alice redeems a matured Bob-bond against an Alice-coin bob holds, `redeem_bc(alice, bob, 5, alice)`, and nobody is asked; and alice deposits two Alice-coins with carol, `deposit_c(alice, carol, alice, 2)`, and carol answers yes.  The run then suspends.  The suite's Section APPS holds each person's screen.

`play_village` runs the village market of GB's paper, Section 7.1, on the app: the six villagers and the escrow agent, under the roles `market_alice` to `market_escrow` (`person.glp`), nine conversations, one between each two who transact; the seven operations at the paper's amounts, and, once frank's date is day 28, diana's redemption of a matured frank-bond against a frank-coin he holds, her presentation of it before that day refused by his machine, the bond not mature by his date.  The play writes the calendars: day 1 on each; day 25 on bob's once he holds the diana-coins of his loan; day 28 on frank's once diana's screen shows her presentation refused; and day 30 on the escrow agent's once it holds the deposit, on which it releases the deposit to frank (GB:7).  Each party's last view holds the holdings of the paper's Table 1, held by Section APPS.

```bash
printf '../programs/apps/gb\n:limit 400000000\nplay_village(A, B, C, D, E, F, G).\n:quit\n' | bin/glpc
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): three tabs, the lists of the pragmas, Contacts (GKK's), Coins and Bonds (GB's).  On Coins, the tab of GB, Mint coin with its field amount and Mint bond with its fields maturity and amount; the coins the person holds, a row for each issuer with the count.  On Bonds, a row for the bonds of each issuer and maturity, the count beside the issuer and the maturity under it.  Beside each person a list shows, the buttons of the acts that name a person: Swap, Pay, Redeem, Deposit, Release and Return.  The schemas of one act share its button (GSC's ruling, Code_inbox.md, 2026-10-24 08:06 UTC): Swap is one button for `swap_cc` to `swap_bb`, its fields the pragma's six, and the person leaves a maturity empty for a coin, which chooses the schema; Redeem and Deposit likewise.  An approval question is drawn at the top of the tab of its contract.  `screens/README.md` names the four screens.
