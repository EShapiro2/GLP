# programs/sglp/gsg: the sGLP sources of GSG's simulation

Sources in the term form `programs/sglp/transform.sh --interface` reads, for the canonical platform of GSG (`cascade/GSG/platform.glp`, `interface.glp`). The runs made from them are recorded in `simulation/GSG/2026-11-14/`, each by `record.sh` beside them.

| File | What it is |
|---|---|
| `GSG_sglp.glp`, `gen.sh` | six scripted persons; each draws one proposal from its list twice an hour; everyone approves. `gen.sh` prints the source. |
| `GSG12_sglp.glp`, `gen12.sh` | twelve scripted persons in two circles of six joined by one acquaintance; one third of them choosy, declining one request in three. |
| `GSGV1_sglp.glp`, `GSGV3_sglp.glp`, `genV.sh` | the village runs: CSSN's simulation of the social graph (`programs/sglp/cssn/CSSN_sglp.glp`) without the children. A census of villages of ten households of one or two adults; each adult reads its own view and offers friendship to those it knows (local or open), introduces friends and ends friendships; each answers offers by its policy (keen or choosy). CSSN's helpers are copied as they stand, renamed `cs_` to `gs_`. `genV.sh <villages> <days> <seed> [local share] [keen share]` prints the source. |
| `timeline.awk` | prints the events of a run's log: identities, knows and friends. |
| `record.sh` | records one of the sources over a cascade record of GSG, by `programs/sglp/cva/record.sh`, as GDO's are recorded: `bash programs/sglp/gsg/record.sh simulation/GSG/<date>/<name> <source> [cascade/GSG/<date>] [-- --seeds 1,7]`. |

A run, from the repository root:

    bash programs/sglp/transform.sh --interface cascade/GSG/interface.glp \
         --source programs/sglp/gsg/GSGV3_sglp.glp --out /tmp/gsgv3
    bash programs/sglp/cva/run.sh cascade/GSG/platform.glp /tmp/gsgv3 census '1 day' 1 village3.log

The scripted sources take a number of agents (`6` or `12`) in place of `census`. Runs go one at a time.
