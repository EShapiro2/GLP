# A run of the cascade on GNG

- Contract: `GNG` (`gng` to the checker), its owner's two files `programs/go/GNG.txt` and `programs/go/GNG.glp`, copied here as they stood at `02f6cfd1`.
- The paper: `https://github.com/EShapiro2/GO-Grassroots-Organisations.git` at `9fe16288`, for the paper's own record.
- The tools: this repository at `02f6cfd1` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-13 00:37 UTC by `cascade.sh gng`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GNG.txt` | the contract |
| `GNG.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [org], [guardianship(org, created, creator)], [], [identity, person])` |
| `vmat.tex` | the VMAT form, 89 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gng)
