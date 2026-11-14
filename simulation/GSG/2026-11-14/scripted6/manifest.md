# A simulation of GSG under sGLP

The owner's sGLP source run on the canonical platform its cascade record compiled, by the harness of every canonical CVA platform (programs/sglp/cva/), as programs/sglp/cva/record.sh recorded it on 2026-11-14 11:57 UTC.  The suite's Section SGLP re-runs the transformation and each run's first simulated day and holds them to this record (record.sh --check).

- name: GSG
- cascade: cascade/GSG/2026-11-13
- cascade commit: 21701666
- source: GSG_sglp.glp
- source path: /tmp/tmp.8T7pUpeNwO/GSG_sglp.glp
- source commit: outside the repository
- programs/sglp commit: 11dc4dee
- repository commit: 11dc4dee
- agents: 6
- until: 3 day
- seeds: 1,7

| File | What it holds |
|---|---|
| `GSG_sglp.glp` | the owner's sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over the platform's interface |
| `1.log`, `1.draw` | seed 1: 3255 answers, the clock 258763.22390378098; the population's draw, draw scripted 6 agreeable 6 |
| `7.log`, `7.draw` | seed 7: 3249 answers, the clock 259129.90407620318; the population's draw, draw scripted 6 agreeable 6 |
