# A run of the cascade on GFF

- Contract: `GFF` (`gff` to the checker), its owner's two files `programs/gff/GFF.txt` and `programs/gff/GFF.glp`, copied here as they stood at `ca9abe75`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- The tools: this repository at `ca9abe75` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/apps/gff (the plays with scripted persons).
- Run on 2026-11-10 20:57 UTC by `cascade.sh gff`; the suite's Section CAS re-runs it and holds these files to what the tools print.

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
