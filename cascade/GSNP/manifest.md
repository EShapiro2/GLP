# A run of the cascade on GSNP

- Contract: `GSNP` (`gsnp` to the checker), its owner's two files `programs/gsc/GSNP.md` and `programs/gsc/GSNP.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `fba42c2b`, for the paper's own record.
- The tools: this repository at `cd37314b` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-19 15:55 UTC by `cascade.sh gsnp`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSNP.md` | the clauses |
| `GSNP.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_seated_and_listed_roles` |
| `vmat.tex` | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| (no run) | vGLP keeps no plays for gsnp |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsnp
