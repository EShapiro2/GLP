# A simulation of GDO under sGLP

The owner's sGLP source run on the canonical platform its cascade record compiled, by the harness of every canonical CVA platform (programs/sglp/cva/), run.sh, on 2026-11-10 08:48 UTC, wall-clock 2681 s, in the steps programs/sglp/lbm/sim/record.sh takes; the lines below are those programs/sglp/cva/record.sh writes, and record.sh --check passes on the record (the transformation as printed, the first simulated day as recorded).

- name: GDO
- cascade: cascade/GDO/2026-11-09-3
- cascade commit: 60223f7a
- source: GDO_sglp.glp
- source path: programs/lbm/GDO_sglp.glp (since moved to programs/sglp/lbm/) with the configuration cfg(uniform, l1, 8, 2, 8, 2000, 7, 2000, 20, 100, 50) and the census gd_town100 in place
- source commit: outside the repository
- programs/sglp commit: 3f22e3f4
- repository commit: f6b9edad
- agents: census
- until: 4 days
- seeds: 1

| File | What it holds |
|---|---|
| `GDO_sglp.glp` | the owner's sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over the platform's interface |
| `1.log`, `1.draw` | seed 1: 25606 answers, the clock 345501.8559871317; the population's draw, draw eager 52 lazy 48 |
| `1.census`, `measures.txt`, `summary.md` | the map (programs/sglp/lbm/sim/census.sh), the measures (overlay.awk) and what we see in the run |
