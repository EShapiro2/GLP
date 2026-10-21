# A run of the cascade on GSG

- Contract: `GSG` (`gsg` to the checker), its owner's two files `programs/gsc/GSG.txt` and `programs/gsc/GSG.glp`, copied here as they stood at `3b3db4f2`.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `72e734d6`, for the paper's own record.
- The tools: this repository at `3b3db4f2` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/social_graph (the plays with scripted persons).
- Run on 2026-10-21 03:14 UTC by `cascade.sh gsg`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSG.txt` | the contract |
| `GSG.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 5 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_befriend play_unfriend play_refused play_menu |
