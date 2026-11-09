# A run of the cascade on DGC

- Contract: `DGC` (`dgc` to the checker), its owner's two files `programs/currencies/sovereign/DGC.txt` and `programs/currencies/sovereign/DGC.glp`, copied here as they stood at `5c482b98`.
- The paper: `git@github.com:EShapiro2/SGC-Sovereign-Grassroots-Currencies.git` at `dd2d1dc5`, for the paper's own record.
- The tools: this repository at `5c482b98` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-09 09:08 UTC by `cascade.sh dgc`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `DGC.txt` | the contract |
| `DGC.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 26 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for dgc |

What the tools refused or lacked:
- run: vGLP keeps no plays for dgc
