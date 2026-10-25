# A simulation of CSSN under sGLP

The owner's sGLP source run on the canonical platform its cascade record compiled, by the harness of every canonical CVA platform (programs/sglp/cva/), as programs/sglp/cva/record.sh recorded it on 2026-10-25 08:04 UTC.  The suite's Section SGLP re-runs the transformation and each run's first simulated day and holds them to this record (record.sh --check).

- name: CSSN
- cascade: cascade/CSSN/2026-10-24-2
- cascade commit: c08f3ede
- source: CSSN_sglp.glp
- source path: /Users/udi/Grassroots/tmp/cssn-sim2/src/strict_choosy/CSSN_sglp.glp
- source commit: outside the repository
- programs/sglp commit: 38f7b4d0
- repository commit: 7f3f05db
- agents: census
- until: 1 year
- seeds: 1

| File | What it holds |
|---|---|
| `CSSN_sglp.glp` | the owner's sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over the platform's interface |
| `1.log`, `1.draw` | seed 1: 8113 answers, the clock 31549286.765516706; the population's draw, draw local 9 strict 17 choosy 20 |
