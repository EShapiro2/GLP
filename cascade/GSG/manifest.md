# A run of the cascade on GSG

- Contract: `GSG` (`gsg` to the checker), its owner's two files `programs/gsc/GSG.md` and `programs/gsc/GSG.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `321487a2`, for the paper's own record.
- The tools: this repository at `8c3fb2e5` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-18 12:19 UTC by `cascade.sh gsg`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSG.md` | the clauses |
| `GSG.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 2 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| (no run) | vGLP keeps no plays for gsg |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsg
