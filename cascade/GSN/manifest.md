# A run of the cascade on GSN

- Contract: `GSN` (`gsn` to the checker), its owner's two files `programs/gsc/GSN.md` and `programs/gsc/GSN.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `f3c4f43f`, for the paper's own record.
- The tools: this repository at `bf6e2016` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/social_network (the plays with scripted persons).
- Run on 2026-10-19 18:00 UTC by `cascade.sh gsn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSN.md` | the clauses |
| `GSN.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_seated_and_listed_roles` |
| `vmat.tex` | the VMAT form, 12 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_network |
