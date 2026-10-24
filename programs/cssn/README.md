# CSSN's app

The app of CSSN, the child-safe social network (CSSN, Section 4), as a mini-app of the Grassroots Super-App (GSG paper, Section 2), compiled down the abstraction cascade from its contract: its act schemas, `CSSN.glp`, and its pragmas, the lines after `Pragmas` in `CSSN.txt` and in the `NAME.txt` of each contract it extends, GSN, GSG and GKK (`programs/gsc`) and GnW (`programs/gai`).  Every word the app shows is the contract's (claude.md, "Sessions": "THE CONTRACT IS THE SOURCE OF ALL TRUTH RELATED TO THE APP"; vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows").

## The contract

`CSSN.txt` holds the clauses, CSSN:1 to CSSN:6, each opening with its modality, and the pragmas: the tab `Children: the children Alice is parenting.` and a line for each clause with acts, its buttons, the roles' names after a button where the clause's names do not occur in the order of the roles, and one question per guarding role of the widest schema the button covers, in role order (vGLP, Section 5.4).  `CSSN.glp` holds the twenty-nine schemas over sixteen predicates: GSN's fourteen, with child forbidden where CSSN says, and the fifteen of the children, which realise GnW.

## The cascade

1. The act schemas, `CSSN.glp`; GSC's checker's verdict `syntactically_grassroots_with_wards` (`programs/gsc`).
2. The canonical CVA platform, `platform.vglp`, as vGLP's compiler prints it from the schemas, the verdict, the introduction predicate `knows` and the pragmas: `bash programs/cssn/compile.sh` prints it, `--check` re-prints it and compares.
3. GLP: the canonical compilation compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/4`.

The platform carries the two widget declarations the compiler prints, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)` with the labels of the pragmas, so the app writes no `self.vglp`.

## The files

| File | What it holds |
|---|---|
| `CSSN.txt`, `CSSN.glp` | the contract, CSSN's Cowork's text |
| `platform.vglp` | the canonical CVA platform of CSSN, as printed |
| `compile.sh` | prints `platform.vglp` from the contract, or checks it |
| `miniapp.glp` | `miniapp/4`, the same file as `../apps/gsg/miniapp.glp` |
| `person.glp` | the scripted person, by role: the village's seven, and `fig_alice` to `fig_dave` of the app's scenario; `village_person/5`, a parent looking also at their children's phones |
| `plays.glp` | `play_village/7`, the village of Section 4.6.3, with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `CSSN_sglp.glp` | the profiles of CSSN's simulation, sGLP's term form |
| `figures/` | the screens of Figure 3, the band of Figure 2, `village.py`, and their README |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/cssn\n:limit 100000000\nplay_village(A, B, G, F, C, D, E).\n:quit\n' | bin/glpc
```

## The screens

`glp_multiagent/test/apps_screens_test.dart` plays the scenario of Figure 3 on the app: one person at their phone, the other three scripted (`person.glp`, `fig_alice` to `fig_dave`), each act taken on the controls the construct family draws from the platform's widget declarations; `tool/sim_screens.sh` plays it on the iOS Simulator and writes `figures/cssn-app-1.png` to `cssn-app-6.png` (`figures/README.md`).
