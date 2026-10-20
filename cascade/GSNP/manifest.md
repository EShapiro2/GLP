# A run of the cascade on GSNP

- Contract: `GSNP` (`gsnp` to the checker), its owner's two files `programs/gai/GSNP.md` and `programs/gai/GSNP.glp`, copied here as they stood at `e4788aeb`.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `a4c056dd`, for the paper's own record.
- The tools: this repository at `e4788aeb` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-20 18:26 UTC by `cascade.sh gsnp`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSNP.md` | the contract |
| `GSNP.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_listed_roles` |
| `vmat.tex` | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gsnp |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsnp
