# A run of the cascade on GD-S

- Contract: `GD-S` (`gd_s` to the checker), its owner's two files `programs/ggga/GD-S.txt` and `programs/ggga/GD-S.glp`, copied here as they stood at `6cf1355a`.
- The paper: `git@github.com:EShapiro2/SRC---Sybil-Resilient-Communities.git` at `bb178919`, for the paper's own record.
- The tools: this repository at `6cf1355a` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-13 00:31 UTC by `cascade.sh gd_s`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GD-S.txt` | the contract |
| `GD-S.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 9 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gd_s)
