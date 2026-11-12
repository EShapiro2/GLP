# A run of the cascade on GDO-LB

- Contract: `GDO-LB` (`gdo_lb` to the checker), its owner's two files `programs/lbm/GDO-LB.txt` and `programs/lbm/GDO-LB.glp`, copied here as they stood at `a4c6da18`.
- The paper: `git@github.com:EShapiro2/GRH-Grassroots-Ride-Hailing.git` at `a16c0aa7`, for the paper's own record.
- The tools: this repository at `a4c6da18` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-12 12:49 UTC by `cascade.sh gdo_lb`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GDO-LB.txt` | the contract |
| `GDO-LB.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 8 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gdo_lb)
