# A run of the cascade on GAI

- Contract: `GAI` (`gai` to the checker), its owner's two files `programs/gai/GAI.md` and `programs/gai/GAI.glp`, copied here as they stood.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `3d81f152`, for the paper's own record.
- The tools: this repository at `9fa21950` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-18 12:58 UTC by `cascade.sh gai`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GAI.md` | the clauses |
| `GAI.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `not_grassroots([obstructed(befriend, 1, atom(agent, []), blocked_by(enter, 2, atom(agent, []))), obstructed(befriend, 2, atom(agent, []), blocked_by(enter, 2, atom(agent, [])))])` |
| `vmat.tex` | the compiler to VMAT's refusal |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- VMAT: % not compiled:
- platform: %% not compiled(gai)
