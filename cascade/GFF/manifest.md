# A run of the cascade on GFF

- Contract: `GFF` (`gff` to the checker), its owner's two files `programs/gff/GFF.txt` and `programs/gff/GFF.glp`, copied here as they stood at `7ce76a00`.
- The paper: `https://github.com/EShapiro2/GFF-Grassroots-Feeds-and-Followers.git` at `791e2f24`, for the paper's own record.
- The tools: this repository at `7ce76a00` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/apps/gff (the plays with scripted persons).
- Run on 2026-11-13 00:33 UTC by `cascade.sh gff`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GFF.txt` | the contract |
| `GFF.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 14 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gff)
