# A run of the cascade on GC

- Contract: `GC` (`gc` to the checker), its owner's two files `programs/currencies/coins/GC.txt` and `programs/currencies/coins/GC.glp`, copied here as they stood at `20a62430`.
- The paper: `git@github.com:EShapiro2/GC-Grassroots-Currencies.git` at `35795d6e`, for the paper's own record.
- The tools: this repository at `20a62430` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/currency (the plays with scripted persons).
- Run on 2026-10-27 17:30 UTC by `cascade.sh gc`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GC.txt` | the contract |
| `GC.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 7 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_pay_swap play_redeem play_busy play_declined |
