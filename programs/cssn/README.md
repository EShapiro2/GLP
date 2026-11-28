# CSSN's app

The app of CSSN, the child-safe social network (CSSN, Section 4), as a mini-app of the Grassroots Super-App (GSA's paper), compiled down the abstraction cascade from its contract: its schemas, `Children.glp`, and its Interface pragmas, the lines after `Interface pragmas` in `Children.txt` and in the `NAME.txt` of each contract it is written from, Warded Groups (`Warded-Groups.txt`, beside it) and Groups, Guardians, Friendship and Knowledge (`programs/gsc`).  The contracts' names are `calculus/NAMES.txt`'s.  Every word the app shows is the contract's (`organisation/claude.md`, "Sessions": "THE CONTRACT IS THE SOURCE OF ALL TRUTH RELATED TO THE APP"; vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows").

## The contract

`Children.txt` and `Warded-Groups.txt` are adopted from the Calculus (`calculus/README.md`, "Adopting a contract"): each holds the contract, its Logic pragmas, both the Calculus's, and CSSN's Interface pragmas, the tab `Children: those Alice is a parent of.` and, for each clause whose acts the contract names, a line with a button for each act, its fields, and a question for each other person asked (vGLP, Section 5.4); `Children.txt` ends with its Implementation pragma, `Introduction: knows.`  `Children.glp` and `Warded-Groups.glp` are the full schemas as `calculus/tools/adopt.py` prints them from the Logic pragmas, never written by hand; `adopt.py --check` holds them, and `cascade/Children/` and `cascade/Warded-Groups/`, to what the tools print.

## The cascade

1. The schemas, `Children.glp`; GSC's checker's verdict `syntactically_grassroots_with_wards` (`programs/gsc`).
2. The canonical CVA platform, `platform.vglp`, as vGLP's compiler prints it from the schemas, the verdict, the introduction predicate `knows` and the pragmas: `bash programs/cssn/compile.sh` prints it, `--check` re-prints it and compares.
3. GLP: the canonical compilation compiles `platform.vglp` when the directory loads.
4. The mini-app: `miniapp.glp` wraps the compiled agent and its dispatcher in the root's entry template `miniapp/3`.

The platform carries the two widget declarations the compiler prints, `Approval =::= approval_sentence.` and `Menu =::= social_menu(...)` with the labels of the pragmas, so the app writes no `self.vglp`.

## The files

| File | What it holds |
|---|---|
| `Children.txt`, `Children.glp`, `Warded-Groups.txt`, `Warded-Groups.glp` | the two contracts, adopted from the Calculus, and their schemas as printed |
| `platform.vglp` | the canonical CVA platform of Children, as printed |
| `compile.sh` | prints `platform.vglp` from the contract, or checks it |
| `miniapp.glp` | `miniapp/3`, the same file as `../apps/gsg/miniapp.glp` |
| `person.glp` | the scripted person, by role, with the keys the person knows, by which its script names its peers: the village's seven, and `fig_alice` to `fig_dave` of the app's scenario; `village_person/6`, a parent looking also at their children's phones |
| `plays.glp` | `play_village/7`, the village of Section 4.6.3, and `play_figure/4`, the app's scenario with all four persons scripted, each with no super-app |
| `self.glp` | the platform's types written again, the router's types, and the exports |
| `../sglp/cssn/` | CSSN's simulation under sGLP, CSSN's files under `programs/sglp`, where a simulation of another project's platform lives (sGLP #11, Code_inbox 2026-11-07 21:21 UTC, Udi approving): `CSSN_sglp.glp`, the profiles in sGLP's term form, and `sim/`, the measures of a run's log and the figure of its edges |
| `figures/` | the screens of Figure 3, the band of Figure 2, `village.py`, and their README |

## Running it

From `glp_runtime/`:

```bash
printf '../programs/cssn\n:limit 100000000\nplay_village(A, B, G, F, C, D, E).\n:quit\n' | bin/glpc
```

## The screens

`glp_multiagent/test/apps_screens_test.dart` plays the scenario of Figure 3 on the app: one person at their phone, the other three scripted (`person.glp`, `fig_alice` to `fig_dave`), each act taken on the controls the construct family draws from the platform's widget declarations; `tool/sim_screens.sh` plays it on the iOS Simulator and writes `figures/cssn-app-1.png` to `cssn-app-6.png` (`figures/README.md`).
