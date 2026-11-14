# Simulations of GSG under sGLP, 2026-11-14

GSG, the Grassroots Social Graph, run under sGLP on the canonical platform of its cascade record `cascade/GSG/2026-11-13`, by the harness of every canonical CVA platform (`programs/sglp/cva/`), from the sources in `programs/sglp/gsg/`. Each record was made by `programs/sglp/gsg/record.sh`, which runs `programs/sglp/cva/record.sh` over that tag, and `record.sh --check` passes on all four.

`gsg-simulation.html` replays the runs: the graph at each step, the event log, and what each person holds.

| Record | Persons | Horizon | Seeds | What happens |
|---|---|---|---|---|
| `scripted6/` | 6, scripted | 3 days | 1, 7 | 872 and 860 proposals, 487 and 479 refused by the platform; six meetings and six introductions; 17 and 21 friendships made, 9 and 12 ended; 8 and 9 at the end |
| `scripted12/` | 12, scripted, 4 choosy | 4 days | 1 | 2,298 proposals, 1,372 refused by the platform, 112 declined; 52 friendships made, 24 ended; 28 at the end, 5 across the two circles |
| `village1/` | 16 adults, one village | 1 day | 1 | all 120 pairs meet; 353 offers of friendship, 92 refused (already friends), 77 declined; 134 made, 15 ended; by hour 16 every pair but one is friends |
| `village3/` | 47 adults, three villages | 1 day | 1 | 1,569 offers, 352 refused because the other did not yet know the offerer (an introduction is one-way), 276 because already friends, 299 declined; 477 made, 41 ended; friendships across villages 18 at hour 6, 50 at hour 12, 92 of 436 at the end |

The scripted runs draw each proposal from a list. In the village runs, CSSN's simulation of the social graph (`programs/sglp/cssn/CSSN_sglp.glp`) without the children, a person reads its own view and acts on it, and has two dimensions, as a parent has in CSSN: the approach (local, offering in its own village 95 times in 100, or open, 60) and the policy (keen, accepting an offer 90 times in 100, or choosy, 50). CSSN's rates are a year's; these are a day's. Where an adult has no one to offer friendship to, CSSN proposes meeting the first person known again, which on GSG adds a copy of `knows` each time; here it proposes `establish`, which the platform refuses and which changes nothing.

Each record holds what `cva/record.sh` writes: the source as run (`GSG_sglp.glp`), the printed `profiles.glp` and `population.glp`, each seed's log and draw, and `manifest.md`.
