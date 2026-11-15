# A simulation of GDO under sGLP

The owner's sGLP source run on the canonical platform its cascade record compiled, by the harness of every canonical CVA platform (programs/sglp/cva/), as programs/sglp/cva/record.sh recorded it on 2026-11-14 09:37 UTC.  The suite's Section SGLP re-runs the transformation and each run's first simulated day and holds them to this record (record.sh --check).

- name: GDO
- cascade: cascade/GDO/2026-11-13
- cascade commit: 5cf2952c
- source: GDO_sglp.glp
- source path: /tmp/tmp.sir39VlRfo/GDO_sglp.glp
- source commit: outside the repository
- programs/sglp commit: 1c861c63
- repository commit: 1c861c63
- agents: census
- until: 4 day
- seeds: 1

| File | What it holds |
|---|---|
| `GDO_sglp.glp` | the owner's sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over the platform's interface |
| `1.log`, `1.draw` | seed 1: 19797 answers, the clock 345581.9042228305; the population's draw, draw eager 52 lazy 48 |
