# GC's app

The app of GC, grassroots currencies, as a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"), compiled down the abstraction cascade from GC's act schemas, as `../gsg` is from GSG's (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2, on GSC #5 Cowork's request of 13:29 UTC).

## The contract

GC is Currencies' contract (Coordination Appendix C); its clauses are `programs/currencies/coins/GC.txt`, as GC's paper, Section 2.1, and GSC's paper have them, and its act schemas `programs/currencies/coins/GC.glp`.  GC extends GKK, the base contract (`programs/gsc/GKK.txt`; the app `../gkk`), whose clauses it cites, and not GSG: no act of it requires a friend.

1. *Minting.*  Alice may decide to issue Alice-coins.
2. *Swap.*  Alice and Bob, who know each other, may jointly decide to swap coins they hold.
3. *Payment.*  Alice may decide to pay Bob-coins they hold to Bob, whom they know.
4. *Redemption.*  Alice may decide to redeem a Bob-coin they hold against any coin held by Bob, whom they know.
5. *Pricing.*  Alice prices their offerings in Alice-coins and accepts them in payment at those prices.

GKK:1 is the schema `establish`, guarded by the person, which adds `identity` at them, and every role of the other schemas requires `identity`; GKK:2 is `meet`, guarded by both, adding `knows` of the other at each; GKK:3 is `introduce`.  GC:1 is `mint(p?)`, which adds `coin(p)` at `p`, `coin(u)` in the state of `p` a coin issued by `u` and held by `p`; GC:2 is `swap(p?, q? ; u, v)`, guarded by both, requiring `knows` of the other at each, `p` giving a `u`-coin and taking a `v`-coin and `q` the converse; GC:3 is `pay(p?, q)`, guarded by `p`, requiring `knows(q)` and `coin(q)` at `p`, a `q`-coin paid to `q`; GC:4 is `redeem(p?, q ; r)`, guarded by `p`, `p` giving `q` a `q`-coin and taking an `r`-coin `q` holds.  GC:5 describes no act.  The schemas are at unit amounts: mint mints one coin, and a swap exchanges one coin for one.  The checker's verdict is `syntactically_grassroots` (`cascade/GC/verdict.txt`).

## The cascade

1. The act schemas, `programs/currencies/coins/GC.glp`; the checker's verdict `syntactically_grassroots`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP (Section 5.4, "The compilation"): `platform.vglp`, as `programs/vglp/cva/compiler` prints it from GC.glp, the platform of the recorded run of the cascade on GC, `cascade/GC/platform.vglp`.  `bash programs/apps/compile.sh --check` re-prints it and compares; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GC, as printed |
| `miniapp.glp` | `miniapp/4`: the agent, its dispatcher, and the agent's network over the conversations; GSG's app's file, the same in each app (`../gsg/README.md`) |
| `person.glp` | `miniapp_person/4`: the scripted person, by role, `alice` or `bob` |
| `plays.glp` | `play_coins/2`, the mini-app with no super-app |
| `self.glp` | the platform's types written again, and the exports |
| `self.vglp` | the app's widget declarations, `Approval =::= approval_sentence.` and `Menu =::= social_menu.` |
| `../gc_boot.glp` | the super-app hosting the app on two smartphones, under `:boot` |
| `../compile.sh` | the check that each app's `platform.vglp` is what the compiler prints |
| `screens/` | the screens of the figure of the app in GSC's paper, and their README |

## How the mini-app runs

As GSG's app runs (`../gsg/README.md`, "How the mini-app runs"): the agent runs with the person's key as its `Peer`, its dispatcher draws the agent's questions on the person channel and its output as construct 0, and its network is the conversations the social graph hands the mini-app, one per friend in the super-app.  The participants of a swap and of a payment are its two roles, so the payer and the payee each run the mini-app over a conversation between them.

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/gc\n:limit 600000\nplay_coins(A, B).\n:quit\n' | bin/glpc
```

`play_coins` runs two executions over one conversation.  Each person establishes their identity, and Bob mints a Bob-coin; Alice and Bob ask to know each other; Alice asks to meet Bob in person, Bob is asked and answers yes, and each then knows the other; Alice mints an Alice-coin and asks Bob to swap it for his Bob-coin, `swap(alice, bob, alice, bob)`, and Bob is asked and answers yes; and Alice pays the Bob-coin to Bob, `pay(alice, bob)`, and nobody is asked.  Bob mints before he answers the meeting, so that he holds the Bob-coin when Alice's swap reaches him: Alice's script cannot see his holdings (`person.glp`).  The run then suspends: each agent has taken its own transactions until none holds, and no trigger is left.  The persons' screens are the views their agents write, with the persons a request question shows where the script looks (`menu(K, Ks)`) and each answer the person gives (`gave(C, A)`), held exactly by the suite's Section APPS:

```
A = [view([], []), menu(alice, []), view([identity], []), view([identity], [bob]), view([knows(bob), identity], [bob]), view([coin(alice), knows(bob), identity], [bob]), view([coin(bob), knows(bob), identity], [bob]), view([knows(bob), identity], [bob]), menu(alice, [bob]) | _]
B = [view([], []), view([identity], []), view([coin(bob), identity], []), view([coin(bob), identity], [alice]), gave(meet(alice, bob), yes), view([knows(alice), coin(bob), identity], [alice]), gave(swap(alice, bob, alice, bob), yes), view([coin(alice), knows(alice), identity], [alice]), view([coin(bob), coin(alice), knows(alice), identity], [alice]) | _]
```

### On two smartphones

```bash
G=$(cd .. && pwd); printf ':artefact %s/programs/apps/gc %s/programs/social/graph/core\n:boot %s/programs/apps/gc_boot.glp %s/programs/social/graph/core\n:quit\n' $G $G $G $G | bin/glpc
```

Two isolates, each the super-app of `programs/social/graph/core` with its own key pair, as for GSG's app (`../gsg/README.md`, "On two smartphones"): each person trusts the compiler's key and installs `gc.glpw`; Alice connects to Bob and they become friends in the super-app's social graph, over which alone the super-app carries a mini-app's messages; Alice invites Bob to the mini-app and Bob accepts; the handshake opens their conversation; then the steps of `play_coins` run over it.  Each person's lines, in order, held exactly by the suite's Section APPS:

```
[alice] sg(connected(bob)), view([], []), menu(alice, []), view([identity], []), view([identity], [bob]), view([knows(bob), identity], [bob]), view([coin(alice), knows(bob), identity], [bob]), view([coin(bob), knows(bob), identity], [bob]), view([knows(bob), identity], [bob]), menu(alice, [bob])
[bob]   sg(befriend(alice)), sg(connected(alice)), view([], []), view([identity], []), view([coin(bob), identity], []), view([coin(bob), identity], [alice]), gave(meet(alice, bob), yes), view([knows(alice), coin(bob), identity], [alice]), gave(swap(alice, bob, alice, bob), yes), view([coin(alice), knows(alice), identity], [alice]), view([coin(bob), coin(alice), knows(alice), identity], [alice])
```

## The screens

What each person sees, as the construct family draws the app (`glp_multiagent/lib/ui_runtime/named_widgets.dart` and `tab_screen.dart`): one screen with two tabs, under the header with the app's name and the person's initial.  Contacts, as GKK's app draws it (`../gkk/README.md`, "The screens"), the request type offering meeting and no friendship: the person's own name, Join, the persons known with Meet, the introduction, and the field Add a person by key.  Coins, the request type offering `mint(Me)`: the person's own name; Mint, proposing `mint(Me)`; the person's holdings, the coins their state holds, `coin(U)`, each drawn by its issuer's name as GC writes it, "Alice-coin", its hyphen the non-breaking hyphen so that no line breaks inside it, one row for each issuer, "2 Alice-coins" where they hold two, and "No coins" where they hold none; on the row of a coin of a person they know, Pay, proposing `pay(Me, U)`, and, once tapped, "You asked to pay a Bob-coin to Bob."; and the swap, composed as the sentence "Swap an Alice-coin for a Bob-coin with Bob.", each place tapped and chosen from a picker, and the button Swap, proposing `swap(Me, Q, U, V)`, and, once sent, "You asked to swap an Alice-coin for a Bob-coin with Bob.".  The approval question of a swap is drawn by `approval_sentence` at the top of Coins: "Alice asks to swap an Alice-coin for a Bob-coin.", with Yes and No, granting `yes` and `no`.  The words are the compiler's, the names of GC's schemas and of the platform's answers, `YesNo ::= yes ; no`, with a coin drawn as Alice-coin and Bob-coin; the sentences once sent are Code's, made by analogy with GKK's "You asked to introduce Carol to Bob.".  Redemption, `redeem(Me, Q, R)`, has no control on the screen.  No term, functor, type name, attempt number or Submit button is shown.

`glp_multiagent/test/apps_screens_test.dart` runs the app for Alice and Bob over one conversation and takes each person's acts by tapping and typing on the tab's controls, in the order of Currencies' task: each taps Join and adds the other by key; Alice taps Meet on Bob's row, and Bob is asked and taps Yes; Alice taps Mint, and so does Bob; Alice composes the swap of her Alice-coin for a Bob-coin with Bob and taps Swap; Bob is asked and taps Yes; and Alice taps Pay on her Bob-coin's row, and Bob is not asked.  `screens/README.md` names the four screens it renders.

Had Bob tapped No, the else-branch of his answer is taken: his vote is no, declined, the attempt aborts at both, neither view changes, and Alice's request ends by Bob's refusal, so her screen shows the notice `refused(swap(alice, bob, alice, bob), bob, declined)`, in the Coins tab (vGLP's `play_busy`, `programs/vglp/cva/currency`, shows the same on the platform of GC).

## The types written again

`self.glp` writes again the types of `platform.vglp` that the imported declarations of `platform#agent` and `platform#dispatch` carry, as GSG's app's does (`../gsg/README.md`, "The types written again").
