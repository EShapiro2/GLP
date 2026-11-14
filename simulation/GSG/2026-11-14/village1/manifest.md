# A simulation of GSG under sGLP

The sGLP source beside this file run on the canonical platform of GSG by the harness of every canonical CVA platform (programs/sglp/cva/run.sh), on 2026-11-12 to 2026-11-14.  Written by hand in record.sh's form; record.sh was not used, the platform being the one recorded in cascade/GSG at the commit below, which no tag cascade/GSG/<date> holds.

- name: GSG
- cascade: cascade/GSG at 47907f4 (cascade.sh gsg, 2026-11-09 23:00 UTC; platform.glp e162a476, interface.glp f260acd8)
- source: GSGV1_sglp.glp (programs/sglp/gsg/GSGV1_sglp.glp)
- programs/sglp commit: 47907f4
- repository commit: 47907f4
- agents: census
- until: 1 day
- seeds: 1

| File | What it holds |
|---|---|
| `GSGV1_sglp.glp` | the sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over cascade/GSG/interface.glp |
| `1.log`, `1.draw` | seed 1: 1614 answers, the clock 86228.34592884172; the population's draw, draw local 10 open 6 keen 6 choosy 10 |
