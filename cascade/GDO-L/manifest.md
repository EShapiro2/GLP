# A run of the cascade on GDO-L

- Contract: `GDO-L` (`gdo_l` to the checker), its owner's two files `programs/gff/GDO-L.txt` and `programs/gff/GDO-L.glp`, copied here as they stood at `b2d5b610`.
- The paper: `https://github.com/EShapiro2/GFF-Grassroots-Feeds-and-Followers.git` at `3604c9b8`, for the paper's own record.
- The tools: this repository at `b2d5b610` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-06 16:05 UTC by `cascade.sh gdo_l`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GDO-L.txt` | the contract |
| `GDO-L.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 12 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gdo_l)
