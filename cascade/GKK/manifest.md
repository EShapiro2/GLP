# A run of the cascade on GKK

- Contract: `GKK` (`gkk` to the checker), its owner's two files `programs/gsc/GKK.txt` and `programs/gsc/GKK.glp`, copied here as they stood at `adbc199d`.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `e0ef9594`, for the paper's own record.
- The tools: this repository at `adbc199d` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/apps/gkk (the plays with scripted persons).
- Run on 2026-10-27 17:30 UTC by `cascade.sh gkk`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GKK.txt` | the contract |
| `GKK.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 3 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_keys |
