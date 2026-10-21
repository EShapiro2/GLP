# A run of the cascade on GAI

- Contract: `GAI` (`gai` to the checker), its owner's two files `programs/gai/GAI.txt` and `programs/gai/GAI.glp`, copied here as they stood at `9a498a60`.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `6d09cadd`, for the paper's own record.
- The tools: this repository at `9a498a60` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-21 03:15 UTC by `cascade.sh gai`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GAI.txt` | the contract |
| `GAI.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `grassroots_with_wards(meet, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_meet, ward(1, agent, 3), free(2)), relay(agents_meet, ward(1, agent, 3), ward(2, agent, 4))], [retired])` |
| `vmat.tex` | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gai |

What the tools refused or lacked:
- run: vGLP keeps no plays for gai
