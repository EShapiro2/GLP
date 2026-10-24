# A run of the cascade on GB

- Contract: `GB` (`gb` to the checker), its owner's two files `programs/currencies/bonds/GB.txt` and `programs/currencies/bonds/GB.glp`, copied here as they stood at `901f409a`.
- The paper: `git@github.com:EShapiro2/Grassroots-Bonds.git` at `6627434c`, for the paper's own record.
- The tools: this repository at `901f409a` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-24 16:42 UTC by `cascade.sh gb`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GB.txt` | the contract |
| `GB.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 22 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gb |

What the tools refused or lacked:
- run: vGLP keeps no plays for gb
