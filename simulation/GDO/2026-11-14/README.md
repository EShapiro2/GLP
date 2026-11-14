# GDO simulated under sGLP, 2026-11-14

The Location-Based, Bounded Out-Degree Instance, the world standing still: forty persons for thirty days, seed 1, d = 3, e = 500 m, nobody travelling and no friendship changing (the configuration's last four fields 0), as the stochastic persons' policy (`programs/sglp/lbm/GDO_sglp.glp`) on GDO's canonical platform.  Three records, each made by `programs/sglp/lbm/sim/record.sh` — `programs/sglp/cva/record.sh` over the cascade record `cascade/GDO/2026-11-13`, on the merge of main `98ca7d4ca` — re-recorded from those of 2026-11-10 over the fresh cascade record (pull request 60), with the measures they had then.  The three configurations, LBM's of 2026-11-03: `nearest4_l1` — persons uniform on 10 km, Manhattan distance, each knowing its 4 nearest and 2 drawn uniformly; `villages4_l2` — four villages of 2 km, Euclidean distance; `random6_l1` — the 6 known at the start all drawn uniformly, so the friendships are not local.  Each record holds the source as run, the printed modules, `1.log`, `1.draw`, the manifest, `1.census`, `measures.txt` and `summary.md`.

| Record | Geometry, metric | d, e | Step, travellers | Meet, Part | Days | Edges | Mean edge | Optimum | Stretch | Optimal edges | Ideal of friends | Transitive | Pointings made / ended after day 1 | Friendships made / ended after day 1 | Across components |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| `nearest4_l1` | uniform, l1 | 3, 500 m | 0 m, 0 | 0, 0 | 30 | 120 | 1482 m | 1482 m | 1.00 | 100% | 1482 m | 0 | 13 / 0 | 0 / 0 | 0 |
| `villages4_l2` | villages, l2 | 3, 500 m | 0 m, 0 | 0, 0 | 30 | 120 | 521 m | 521 m | 1.00 | 100% | 521 m | 0 | 9 / 0 | 0 / 0 | 0 |
| `random6_l1` | uniform, l1 | 3, 500 m | 0 m, 0 | 0, 0 | 30 | 120 | 2490 m | 1482 m | 1.68 | 49% | 3213 m | 41 | 59 / 43 | 0 / 0 | 0 |

## Against the records of 2026-11-03

`nearest4_l1` and `villages4_l2` end where they ended then: 120 edges, out-degree 3 at every person, every pointing to one of the 3 nearest of all (stretch 1.00), no transitive edge needed.  The way there is shorter (13 and 9 pointings made after the first day against 24 and 21): the source has changed since — the setting-up at an hour's rate, the act decided when the answer is due — so the logs are not line for line those of 2026-11-03.

`random6_l1` ends close to where it ended: mean edge 2490 m against 2404, stretch 1.68 against 1.62, 41 transitive edges against 46, the overlay well below the best the friends alone allow (3213 m) and above the optimum (1482 m).  Transitive pointing is still trial and refusal — 1553 of 1598 requests refused by the peer, the contract recording at Alice nothing of whom Bob points to — and the run took 103 minutes for it, against ten for each of the others.

In no run does a pointing cross a component of the friendship graph.
