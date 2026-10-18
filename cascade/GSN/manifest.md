# A run of the cascade on GSN

- Contract: `GSN` (`gsn` to the checker), its owner's two files `programs/gsc/GSN.md` and `programs/gsc/GSN.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `d745b320`, for the paper's own record.
- The tools: this repository at `cdbbdd57` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-18 22:09 UTC by `cascade.sh gsn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSN.md` | the clauses |
| `GSN.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 11 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| (no run) | vGLP keeps no plays for gsn |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsn
