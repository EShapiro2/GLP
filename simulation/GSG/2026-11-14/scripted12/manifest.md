# A simulation of GSG under sGLP

The sGLP source beside this file run on the canonical platform of GSG by the harness of every canonical CVA platform (programs/sglp/cva/run.sh), on 2026-11-12 to 2026-11-14.  Written by hand in record.sh's form; record.sh was not used, the platform being the one recorded in cascade/GSG at the commit below, which no tag cascade/GSG/<date> holds.

- name: GSG
- cascade: cascade/GSG at 47907f4 (cascade.sh gsg, 2026-11-09 23:00 UTC; platform.glp e162a476, interface.glp f260acd8)
- source: GSG12_sglp.glp (programs/sglp/gsg/GSG12_sglp.glp)
- programs/sglp commit: 47907f4
- repository commit: 47907f4
- agents: 12
- until: 4 days
- seeds: 1

| File | What it holds |
|---|---|
| `GSG12_sglp.glp` | the sGLP source, as it stood |
| `profiles.glp`, `population.glp` | printed by programs/sglp/transform.sh --interface from it over cascade/GSG/interface.glp |
| `1.log`, `1.draw` | seed 1: 3284 answers, the clock 345421.7875943305; the population's draw, draw scripted 12 agreeable 8 choosy 4 |
