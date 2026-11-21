# DGC's app

The app of DGC, Denominated Grassroots Currencies, is a mini-app of the Grassroots Super-App (GSG paper, Section 2, "The Grassroots Super-App"). It is compiled down the abstraction cascade from DGC's act schemas and pragmas, as `../gb` is compiled from GB's.

## The contract

DGC is Currencies' contract (Coordination Appendix C), the contract of SGC's paper. Its clauses and pragmas are in `programs/currencies/sovereign/Denominated-Currencies.txt`, and its act schemas in `programs/currencies/sovereign/Denominated-Currencies.glp`, which `calculus/tools/adopt.py` prints. DGC extends Bonds and Denominated Coins. Through them it extends GKK, the base contract (`programs/gsc/Knowledge.txt`; the app `../gkk`), and not GSG: no act of it requires a friend.

Each coin and bond carries its denomination, a constant such as `usd`:

- **Coins.** `coin(p, f)` is a coin of issuer `p` in denomination `f`. `first_issue(p? ; f, k)` is `p`'s first issue, in `f`, and it adds `issued_in(f)`. `issue(p? ; f, k)` issues again in `f`.
- **Bonds.** `bond(p, d, f)` is a bond of issuer `p`, due `d`, in denomination `f`. DGC:1, Issue denominated bond, is `issue_denominated_bond(p? ; d, f, k)`. It adds `k` copies of `bond(p, d, f)` and `issued_bond_in(f, d)`, where `d` is later than the current date. The current date is `today(t)`, which the agent reads from the machine's clock by `now/1`.
- **Swap, redeem and escrow.** DGC:2 restricts these, from Notes, Coins and Bonds, to one denomination: `swap`, `swap_coin_for_bond`, `swap_bonds`, the four redemptions, the deposits with `escrow_denomination`, release and return.
- **Accept.** DGC:3 is `accept(p? ; q, f, n)` and `accept_by_bond`. Each adds `n` copies of `accepts(q, f)` at `p`.
- **Pay accepted coins.** DGC:4 is `pay_accepted_coins`, `pay_own_accepted_coins` and `pay_accepted_coins_to_the_issuer`. Each deletes one acceptance at the payee.

## The cascade

1. The act schemas, `Denominated-Currencies.glp`, and the pragmas, `Denominated-Currencies.txt`.
2. The canonical CVA platform of the schemas (vGLP, Section 3.2, Definition "Canonical CVA Platform"), compiled into vGLP with the words of the pragmas (Section 5.4, "The compilation" and "The contract's words"). This is `platform.vglp`, as `programs/vglp/cva/compiler` prints it. `bash programs/apps/compile.sh --check` re-prints it and compares it; `bash programs/apps/compile.sh` writes it.
3. GLP: the canonical compilation (vGLP, Section 6.3) compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/3`, which the super-app activates by `run/3`.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of DGC, as printed, with the words of its pragmas |
| `miniapp.glp` | `miniapp/3`: the agent, its dispatcher, and the agent's network over the conversations; the file of the other apps |
| `person.glp` | `miniapp_person/5`: the scripted person, by role, `alice` or `bob`, with the keys the person knows; GB's app's person with DGC's scripts and classes |
| `plays.glp` | `play_denominated/2`: the mini-app with no super-app, the persons setting the clock |
| `self.glp` | the platform's types written again, and the exports |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/apps/dgc\n:limit 60000000\nplay_denominated(A, B).\n:quit\n' | bin/glpc
```

`play_denominated` runs two executions, alice's and bob's, over one conversation, in the denomination `usd`:

1. Each sets the clock to day 1 and establishes their identity.
2. They come to know each other.
3. Each issues two usd-denominated coins of their own, and alice issues three usd-denominated Alice-bonds due day 10, `issue_denominated_bond(alice, 10, usd, 3)`.
4. Alice swaps her two Alice-coins for bob's two Bob-coins, `swap(alice, bob, alice, bob, usd, 2, 2)`, and he answers yes.
5. Bob accepts one more usd-denominated Alice-coin in payment, `accept(bob, alice, usd, 1)`, and pays alice one of her own Alice-coins, `pay_issuer(bob, alice, usd, 1)`.
6. Alice pays that coin to bob, `pay_own_accepted_coins(alice, bob, usd)`, and his acceptance is used up.

The run then suspends.

## The screens

What each person sees, and every word of it, comes from the pragmas in `platform.vglp`.
