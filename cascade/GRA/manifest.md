# A run of the cascade on GRA

- Contract: `GRA` (`gra` to the checker), its owner's two files `programs/lbm/GRA.txt` and `programs/lbm/GRA.glp`, copied here as they stood at `4e59348a`.
- The paper: `https://github.com/EShapiro2/grh-grassroots-ride-hailing` at `a16c0aa7`, for the paper's own record.
- The tools: this repository at `4e59348a` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-10 14:25 UTC by `cascade.sh gra`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GRA.txt` | the contract |
| `GRA.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 19 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gra)
