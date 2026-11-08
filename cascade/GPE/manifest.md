# A run of the cascade on GPE

- Contract: `GPE` (`gpe` to the checker), its owner's two files `programs/social/graph/GPE.txt` and `programs/social/graph/GPE.glp`, copied here as they stood at `42f3644d`.
- The paper: `git@github.com:EShapiro2/GSG-Grassroots-Social-Graph.git` at `771b6a08`, for the paper's own record.
- The tools: this repository at `42f3644d` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-07 23:54 UTC by `cascade.sh gpe`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GPE.txt` | the contract |
| `GPE.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 6 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gpe)
