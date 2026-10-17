# A run of the cascade on cssn

- Contract: `cssn`, its owner's two files `programs/cssn/contract.md` and `programs/cssn/contract.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/SGLP.git` at `f3c4e88c`, for the paper's own record.
- The tools: this repository at `99d4e115` --- programs/jurix (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/cssn (the plays with scripted persons).
- Run on 2026-10-17 20:39 UTC by `cascade.sh cssn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `contract.md` | the clauses |
| `contract.glp` | the act schemas, each in Jurix's notation above its term |
| `verdict.txt` | the checker's verdict: `not_grassroots([obstructed(enter, 1, atom(identity, []), unobtainable), obstructed(enter, 1, atom(child, []), blocked_by(enter, 2, atom(child, []))), obstructed(enter, 2, atom(identity, []), blocked_by(establish, 1, atom(identity, [])))])` |
| `vmat.tex` | the compiler to VMAT's refusal |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- VMAT: % not compiled:
- platform: %% not compiled(cssn)
