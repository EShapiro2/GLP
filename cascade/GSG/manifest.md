# A run of the cascade on GSG

- Contract: `GSG` (`gsg` to the checker), its owner's two files `programs/gsc/GSG.txt` and `programs/gsc/GSG.glp`, copied here as they stood at `eec325c7`.
- The paper: `https://github.com/EShapiro2/jurix-formal-grassroots-social-contracts` at `7a27266b`, for the paper's own record.
- The tools: this repository at `eec325c7` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/social_graph (the plays with scripted persons).
- Run on 2026-11-10 11:57 UTC by `cascade.sh gsg`; the suite's Section CAS re-runs it and holds these files to what the tools print.

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
| `run.log` | the plays' logs: play_refresh play_befriend play_unfriend play_refused play_menu |
