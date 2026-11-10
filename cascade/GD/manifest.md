# A run of the cascade on GD

- Contract: `GD` (`gd` to the checker), its owner's two files `programs/ggga/GD.txt` and `programs/ggga/GD.glp`, copied here as they stood at `ffa59869`.
- The paper: `https://github.com/EShapiro2/src---sybil-resilient-communities` at `bb178919`, for the paper's own record.
- The tools: this repository at `ffa59869` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-10 12:02 UTC by `cascade.sh gd`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GD.txt` | the contract |
| `GD.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 9 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gd |

What the tools refused or lacked:
- run: vGLP keeps no plays for gd
