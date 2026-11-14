# GDO's simulation under sGLP

The grassroots directed overlay simulated on the pattern of CSSN's Section 5 (`programs/sglp/cssn/CSSN_sglp.glp`; `simulation/CSSN/`): stochastic persons in place of people on the canonical CVA platform the cascade compiles from GDO's schemas, `cascade/GDO/platform.glp`, by the harness of every canonical platform, `programs/sglp/cva/`.  What the clauses permit (GDO:1 to GDO:3) the platform enforces; what the paper's Location-Based, Bounded Out-Degree Instance says a person's machine takes is the stochastic person's menu, `programs/sglp/lbm/GDO_sglp.glp`.  No Python: the source is sGLP in the term form, the run is the REPL's, and the measures are an awk script over the log.

## The files

| File | What it holds |
|---|---|
| `../GDO_sglp.glp` | the owner's sGLP source: the configuration line, the census (`gd_town40`, `gd_town100`), the one dimension of profiles (`tempo`: `eager`, `lazy`), the instance (`gd_instance`) and the run declaration |
| `record.sh` | a record with a configuration of its own, as CSSN's mixes are: the source with the configuration and the run line in place, recorded whole by `programs/sglp/cva/record.sh` over a cascade record of GDO (its newest tag), and beside it the map and the measures; `cva/record.sh --check` passes on the record |
| `census.sh` | prints the map of a configuration, one line per person, `<a> <key> <x> <y> <known at the start> <walks>`, after a first line with the configuration and the motion rule, without a run |
| `overlay.awk` | the measures from a run's log and its map: requests and refusals by class, the churn (pointings and friendships made and ended, by day where the persons travel), the overlay at the end at the positions at the end (out-degrees, edge lengths, friend and transitive pointings, Proposition 2's check) against the optimum — the graph in which every person points to its d nearest persons of all: the stretch (mean edge over the optimum's), the gap, and the optimal edges, the pointings that are to one of the person's d nearest of all — and against the ideal of friends (the d nearest friends); with `days=k` (`months=k`) a row per day (month) of how the overlay tracks the optimum; the last line a tab-separated summary; it reads the metric, d, the map's seed and the step off the map's first line |
| `table.sh` | the Markdown table of one or more records against the optimum, from their maps, manifests and measures |
| `replay.sh` | builds a self-contained page replaying records for the eye, with the chosen record's summary (its `summary.md`: what we see, against the optimum, to improve) and the records' measures against the optimum in tables under the map (at the end, and the chosen record by day): the walkers gliding between their daily places (along the streets, from place to place) with the trail of their last days, the others still, the friendships and the pointings from the views, a pointing just made flashing green and one just ended fading dashed |

## The configuration

One line of the source, `gd_config(cfg(Geometry, Metric, Nearest, Random, D, E, MapSeed, Step, Movers, Meet, Part))`:

- `Geometry`: `uniform`, forty (or a hundred) persons uniform on a square of 10 km; `villages`, four clusters of 2 km round the quarters' centres, the persons dealt to them in turn; `manhattan`, two hundred drivers (`gd_town200`) at real pickup points of 2015-01-15 (LBM-code's inputs, the frame rotated onto the street grid, `manhattan/README.md` beside this file), on 21 km.
- `Metric`: `l1`, Manhattan, or `l2`, Euclidean; metres.
- `Nearest`, `Random`: how many nearest persons, and how many drawn uniformly, each knows at the start.
- `D`, `E`: the instance's d and e.
- `MapSeed`: the seed of the map.  The census is a function of this line alone, so the run's seed varies the persons' behaviour only, and the map is printed without the run by `census.sh`.
- `Step`, `Movers`: the first Movers persons of the census travel — each draws a destination by a generator of the person's own seeded from its start and the map's seed (the minimal standard, 48271 modulo 2³¹−1): on a square a point drawn uniformly, in Manhattan one of the two hundred drivers' places (the k-th person's start, k the draw modulo 200 plus 1) — and goes Step metres a day towards it along the grid, the x axis first and the rest of the day's step along y, drawing the next destination on arriving; the rest stay put (0 and 0 for a town that stands still).  The trips are a function of the configuration alone, so `overlay.awk` and the replay page move the persons the same way; the map marks the travellers, and its first line ends in `trips`, the motion rule.  (A random walk, a step drawn in −Step..Step on each axis a day, preceded the trips and was dropped with its records; `60586d42` in the history has both.)
- `Meet`, `Part`: the friendship graph changes — at each tick, with Meet chances in 1000 the person meets a stranger (acquaint, GKK:2, then befriends them), with Part chances in 1000 ends a friendship drawn uniformly (unfriend, GSG:2; a friendship ended is not made again by either side, each holding the friend in a view seen); 0 and 0 and the graph stands after the setup.

No clock is given to a profile, so a person reckons the day from its own ticks: its proposals in the approved lists of the views it has seen and the refusals of its proposals, over the tempo's ticks a day (`gd_day`).  Persons of one tempo agree within a tick or so; it is by that day that each computes where everyone is.

The number of persons is the census the run declaration names, `gd_town40` or `gd_town100`; the horizon, the seed and the mix of tempos are the run declaration's.  To change a parameter, edit the line and run again: `record.sh` keeps the source as it stood with each record, so two configurations are two records.

## The profiles

One dimension, `tempo`, of the one role `agent`: `eager` answers the instance 24 times a day on average, `lazy` 4 times.  Both approve every approval at once (an acquaintance's hello, an offer of friendship, an introduction); GDO's own acts ask no approval, Bob being a reading role.  A person first establishes their identity, then comes to know, one per menu, each person they know from outside the app, then offers friendship to each person their agent knows, each after an hour on average; then the instance, at the tempo's rate.  The act is decided when the answer is due, from the screen as it then stands (`gd_tick`), not when the menu is drawn, so that an act committed meanwhile is not proposed again (CSSN's parents propose `establish` twice for that reason).

## The instance

At a tick the person reads the atoms of the last view --- `knows`, `friend`, `points_to` --- and its attributes --- its location, every person's location, d, e, the metric --- and answers with the one act the instance takes, in this order: ceasing, `unpoint(Far)`, where it points to more than d, Far the farthest; pointing to friend, `point_friend(NearF)`, where it points to fewer than d, or to d and the nearest unpointed friend is closer than Far by more than e; transitive pointing, `point_transitive(B, NearK)`, where the nearest unpointed person known is closer than Far by more than e, B drawn uniformly from the persons pointed to.  The contract records at Alice nothing of whom Bob points to, so B's machine refuses the act where B does not point to NearK, as a parent's request is refused in CSSN where the children do not know each other; the refusals are counted.  Where the instance takes nothing the person introduces a person they point to to a friend (GKK:3), so that transitive pointing has persons to reach; and where that too is impossible, answers with an act its own machine refuses.

## Running it

From the repository root, with the REPL built (`glp_runtime/bin/glpc`):

```bash
# the printed modules, from the source over the platform's interface
bash programs/sglp/transform.sh --interface cascade/GDO/interface.glp --source programs/sglp/lbm/GDO_sglp.glp --out <dir>
# one run: 40 persons, 30 days, seed 1 (the run declaration's), its log and draw
bash programs/sglp/cva/run.sh cascade/GDO/platform.glp <dir> census "30 days" 1 <log>
# the map, the measures, and a replay page of one or more records (directories holding 1.log and 1.census)
bash programs/sglp/lbm/sim/census.sh cascade/GDO/platform.glp <dir> gd_town40 <map>
awk -v census=<map> -v days=6 -f programs/sglp/lbm/sim/overlay.awk <log>
bash programs/sglp/lbm/sim/table.sh <record> ...
bash programs/sglp/lbm/sim/replay.sh <out.html> <record> ...
# or a record whole, as CSSN's mixes: the configuration and the run line in place, recorded by cva/record.sh over
# GDO's newest cascade record, with its map and measures; cva/record.sh --check passes on it
bash programs/sglp/lbm/sim/record.sh simulation/GDO-LB/<date>/<name> 'cfg(uniform, l1, 4, 2, 3, 200, 7, 2000, 8, 100, 50)' gd_town40 '6 days'
bash programs/sglp/cva/record.sh --check simulation/GDO-LB/<date>/<name>
```

A day of forty persons takes about two minutes at d = 3 and four at d = 8; a day of a hundred persons six to eleven minutes (main `98ca7d4ca`).  The records: `simulation/GDO/2026-11-14` (the world standing still) and `simulation/GDO-LB/2026-11-14` (GDO-LB, the persons travelling and the friendships changing), each record of the form `cva/record.sh` writes, `record.sh --check` passing on it.

The source is the realisation of GDO-LB, Location-Based GDO (`programs/lbm/GDO-LB.txt`, the restriction of GDO by its three conditions, GRH's): until GSC's term form of forall and exists and vGLP's environment predicates reach the cascade and GDO-LB compiles to a platform of its own, the conditions are the stochastic person's policy over GDO's platform, `gd_choose`, and the person's location, d and e are its attributes.
