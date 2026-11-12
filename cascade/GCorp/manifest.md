# A run of the cascade on GCorp

- Contract: `GCorp` (`gcorp` to the checker), its owner's two files `programs/go/GCorp.txt` and `programs/go/GCorp.glp`, copied here as they stood at `dac0a720`.
- The paper: `https://github.com/EShapiro2/GO-Grassroots-Organisations.git` at `9fe16288`, for the paper's own record.
- Parametric: checked as written, with no realisation, so no platform is compiled (GSC, Definition Parametric contract).
- The tools: this repository at `dac0a720` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-12 12:34 UTC by `cascade.sh gcorp`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GCorp.txt` | the contract |
| `GCorp.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [org], [guardianship(org, created, creator)], [], [identity, person])` |
| `vmat.tex` | the VMAT form, 47 displays, one per schema |
| (no platform) | a parametric contract, with no realisation |

What the tools refused or lacked:
- platform: a parametric contract has no realisation (GSC, Definition Parametric contract); its instances are compiled
