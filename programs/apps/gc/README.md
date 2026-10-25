# GC's app

The app of GC, grassroots currencies, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GC's act schemas, as `../gsg` is from GSG's (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC).

## The contract

GC is Currencies' contract (Coordination Appendix C); its clauses are `programs/currencies/coins/GC.txt`, as GC's paper, Section 2.1, and GSC's paper have them, and its act schemas `programs/currencies/coins/GC.glp`.  GC extends GKK, the base contract (`programs/gsc/GKK.txt`; the app `../gkk`), whose clauses it cites, and not GSG: no act of it requires a friend.

1. *Minting.*  Alice may decide to issue Alice-coins.
2. *Swap.*  Alice and Bob, who know each other, may jointly decide to swap coins they hold.
3. *Payment.*  Alice may decide to pay Bob-coins they hold to Bob, whom they know.
4. *Redemption.*  Alice may decide to redeem a Bob-coin they hold against any coin held by Bob, whom they know.
5. *Pricing.*  Alice prices their offerings in Alice-coins and accepts them in payment at those prices.

GKK:1 is the schema `establish`, guarded by the person, which adds `identity` at them, and every role of the other schemas requires `identity`; GKK:2 is `meet`, guarded by both, adding `knows` of the other at each; GKK:3 is `introduce`.  GC:1 is `mint(p?)`, which adds `coin(p)` at `p`, `coin(u)` in the state of `p` a coin issued by `u` and held by `p`; GC:2 is `swap(p?, q? ; u, v)`, guarded by both, requiring `knows` of the other at each, `p` giving a `u`-coin and taking a `v`-coin and `q` the converse; GC:3 is `pay(p?, q)`, guarded by `p`, requiring `knows(q)` and `coin(q)` at `p`, a `q`-coin paid to `q`; GC:4 is `redeem(p?, q ; r)`, guarded by `p`, `p` giving `q` a `q`-coin and taking an `r`-coin `q` holds.  GC:5 describes no act.  Mint, swap and pay carry amounts, the numbers act schemas allow: `mint(p? ; k)` mints k coins, `swap(p?, q? ; u, k, v, k1)` exchanges k coins of u for k1 coins of v, and `pay(p?, q ; k)` pays k; redeem presents one coin.  The checker's verdict is `syntactically_grassroots`.

## The cascade

1. The act schemas, `programs/currencies/coins/GC.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GC.glp, the platform of the recorded run of the cascade on GC, `cascade/GC/platform.vglp`.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GC, as printed, with the words of its pragmas and its two widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)`, the request labels and placements its pragmas give |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations; GSG's app's file, the same in each app (`../gsg/README.md`) |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice` or `bob` |
| `plays.glp` | `play_coins/2`, the mini-app with no super-app, and `play_village/6`, the village market of GC's paper on it |
| `self.glp` | the platform's types written again, and the exports |
| `../gc_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the app for GC's paper and GSC's, the four of the figure of the app and the six phones at the end of the village market, and their README |

## How the mini-app runs

As GSG's app runs (`../gsg/README.md`, "How the mini-app runs"): the agent runs with the person's key as its `Peer`, its dispatcher draws the agent's questions on the person channel and its output as construct 0, and its network is the conversations the social graph hands the mini-app, one per friend in the super-app.  The participants of a swap and of a payment are its two roles, so the payer and the payee each run the mini-app over a conversation between them.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gc\n:limit 60000000\nplay_coins(A, B).\n:quit\n' | bin/glpc
```

`play_coins` runs two executions over one conversation.  Each person establishes their identity (GKK:1), and Bob mints three Bob-coins (GC:1); Alice asks to meet Bob in person, her request's hello asking him, he answers yes, and each then knows the other (GKK:2); Alice mints two Alice-coins and asks Bob to swap them for his three Bob-coins, `swap(alice, bob, alice, 2, bob, 3)`, and Bob is asked and answers yes (GC:2); and Alice pays two Bob-coins to Bob, `pay(alice, bob, 2)`, and nobody is asked (GC:3).  Bob mints before he answers the meeting, so that he holds the Bob-coins when Alice's swap reaches him: Alice's script cannot see his holdings (`person.glp`).  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, `view(S, K, A, Ts)`, the machine state, the persons known, the approvals pending and the tabs, with the persons a request question shows where the script looks (`menu(K, Ks)`) and each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS:

```
A = [view([], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, []), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [bob], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(bob), identity], [bob], [mint(alice, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(bob), identity], [bob], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, [bob]), view([coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [pay(alice, bob, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]) | _]
B = [view([], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [mint(bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(meet(alice, bob), yes), view([coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), identity], [alice], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(swap(alice, bob, alice, 2, bob, 3), yes), view([knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]) | _]
```

`play_village` runs the village market of GC's paper, Appendix "Village Market Demonstration", on the app: the six villagers under the roles `market_alice` to `market_frank` (`person.glp`), seven conversations, each villager minting the coins of its issue the operations need, 30, 39, 25, 35, 20 and 28, and the six operations at the paper's amounts; each villager's last view holds the holdings of the paper's Table 1, held by Section APPS.

```bash
printf '../programs/apps/gc\n:limit 400000000\nplay_village(A, B, C, D, E, F).\n:quit\n' | bin/glpc
```

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gc %s/programs/social/graph/core\n:boot %s/programs/apps/gc_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

Two isolates, each the super-app of `programs/social/graph/core` with its own key pair, as for GSG's app (`../gsg/README.md`, "On two smartphones"): each person trusts the compiler's key and installs `gc.glpw`; Alice connects to Bob and they become friends in the super-app's social graph, over which alone the super-app carries a mini-app's messages; Alice invites Bob to the mini-app and Bob accepts; the handshake opens their conversation; then the steps of `play_coins` run over it.  Each person's lines, in order, held exactly by the suite's Section APPS:

```
[alice] sg(connected(bob)), view([], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, []), view([], [], [establish(alice)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [bob], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(bob), identity], [bob], [mint(alice, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(bob), identity], [bob], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), menu(alice, [bob]), view([coin(bob), coin(bob), coin(bob), knows(bob), knows(bob), identity], [bob], [pay(alice, bob, 2)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), knows(bob), knows(bob), identity], [bob], [], [tab('Contacts', knows), tab('Coins', coin, counted)])
[bob]   sg(befriend(alice)), sg(connected(alice)), view([], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([], [], [establish(bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([identity], [], [mint(bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), identity], [], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(meet(alice, bob), yes), view([coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(bob), identity], [alice], [meet(alice, bob)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), gave(swap(alice, bob, alice, 2, bob, 3), yes), view([knows(alice), coin(bob), coin(bob), coin(bob), identity], [alice], [swap(alice, bob, alice, 2, bob, 3)], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)]), view([coin(bob), coin(bob), coin(alice), coin(alice), knows(alice), knows(alice), identity], [alice], [], [tab('Contacts', knows), tab('Coins', coin, counted)])
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): two tabs, the lists of the pragmas, Contacts (GKK's, "Contacts: the persons Alice knows.") and Coins (GC's, "Coins: the coins Alice holds."), every word on them the contract's (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows").  On Contacts, as GKK's app draws it (`../gkk/README.md`, "The screens"): Establish, Meet with a field for the key of a person not listed, and the persons known, each with the buttons of the acts that name a person beside the proposer, Meet, Introduce, Swap, Pay and Redeem.  On Coins, the tab of GC: Mint with its field amount, GC's "[Mint] <amount>"; and the coins the person holds, a row for each issuer, named, with its count, the contract adding and deleting `coin` with a multiplicity, and beside the row of an issuer the person knows the buttons of the acts that name a person, Swap, GC's "[Swap] <issuer given> <amount given> <issuer wanted> <amount wanted>", Pay, "[Pay] <amount>", and Redeem, "[Redeem] <issuer wanted>", each opening its fields and a button of its name.  An approval question is drawn at the top of the tab of its contract: GC's "Alice asks to swap coins with you.", then each value of the binding the question does not name by the name of its field, "issuer given: Alice", "amount given: 2", "issuer wanted: Bob", "amount wanted: 3", and the answers yes and no, the platform's own words.  A notice is drawn as its refuser, its reason, declined or unable, and the act's button.  No term, functor, type name, attempt number or Submit button is shown.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice and Bob over one conversation and takes each person's acts by tapping and typing on the tabs' controls, in the order of Currencies' task: each taps Establish; Alice types Bob's key into Meet's field and taps Meet, and Bob is asked and taps yes; Alice types 2 into Mint's field and taps Mint, and Bob 3; Alice taps Swap beside Bob, fills its four fields and taps Swap, and Bob is asked and taps yes; and Alice taps Pay beside her Bob-coins, types 2 and taps Pay, and Bob is not asked.  It then runs the village market of GC's paper on the app for the six villagers at their phones, each act a tap, and checks each phone's Coins tab against the paper's Table 1 at the end.  `screens/README.md` names the four screens of the figure of the app and the six phones of the village market.

Had Bob tapped no, the else-branch of his answer is taken: his vote is no, declined, the attempt aborts at both, neither view changes, and Alice's request ends by Bob's refusal, so her screen shows the notice `refused(swap(alice, bob, alice, 2, bob, 3), bob, declined)`, in the Coins tab (vGLP's `play_busy`, `programs/vglp/cva/currency`, shows the same on the platform of GC).

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as GSG's app's does (`../gsg/README.md`, "The types written again").
