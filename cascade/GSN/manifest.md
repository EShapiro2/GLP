# A run of the cascade on GSN

- Contract: `GSN` (`gsn` to the checker), its owner's two files `programs/gsc/GSN.txt` and `programs/gsc/GSN.glp`, copied here as they stood at `7d83539b`.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `e0ef9594`, for the paper's own record.
- The tools: this repository at `7d83539b` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/social_network (the plays with scripted persons).
- Run on 2026-10-25 18:28 UTC by `cascade.sh gsn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSN.txt` | the contract |
| `GSN.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 14 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_network |
