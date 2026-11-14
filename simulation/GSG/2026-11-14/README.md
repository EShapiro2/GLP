# Simulations of GSG under sGLP, 2026-11-14

GSG, the Grassroots Social Graph, run under sGLP on its canonical platform, by the harness of every canonical CVA platform (`programs/sglp/cva/run.sh`), from the sources in `programs/sglp/gsg/`. The platform is the one `cascade.sh gsg` recorded in `cascade/GSG` at 47907f4 (2026-11-09); no tag `cascade/GSG/<date>` holds that platform, so the records were written by hand in `record.sh`'s form rather than by `record.sh`. Re-recording them with `record.sh GSG ... --cascade cascade/GSG/<date>` against a tagged platform would make them checkable by `record.sh --check`.

`gsg-simulation.html` replays the five runs: the graph at each step, the event log, and what each person holds.

| Record | Persons | Horizon | Seed | What happens |
|---|---|---|---|---|
| `scripted6/` | 6, scripted | 3 days | 1, 7 | six meetings and six introductions; 17 and 21 friendships made, 9 and 12 ended; 8 and 9 at the end |
| `scripted12/` | 12, scripted, 4 choosy | 4 days | 1 | 2,298 proposals, 1,474 refused by the platform, 112 declined; 52 friendships made, 24 ended; 28 at the end, 5 across the two circles |
| `village1/` | 16 adults, one village | 1 day | 1 | all 120 pairs meet; 341 offers of friendship, 92 refused (already friends), 78 answered no; 134 made, 15 ended; by hour 17 every pair but one is friends |
| `village3/` | 47 adults, three villages | 1 day | 1 | 1,555 offers, 357 refused because the other did not yet know the offerer (an introduction is one-way), 259 because already friends, 326 answered no; 471 made, 41 ended; friendships across villages 18 at hour 6, 53 at hour 12, 89 of 430 at the end |

In the village runs a person has two dimensions, as a parent has in CSSN: the approach (local, offering in its own village 95 times in 100, or open, 60) and the policy (keen, accepting an offer 90 times in 100, or choosy, 50). CSSN's rates are a year's; these are a day's. Where an adult has no one to offer friendship to, CSSN proposes meeting the first person known again, which on GSG adds a copy of `knows` each time; here it proposes `establish`, which the platform refuses and which changes nothing.

Each record holds the source as it stood, the printed `profiles.glp` and `population.glp`, each seed's log and draw, and a `manifest.md`; the scripted records also hold each log's timeline (`timeline.awk`).
