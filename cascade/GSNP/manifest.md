# A run of the cascade on GSNP

- Contract: `GSNP` (`gsnp` to the checker), its owner's two files `programs/gai/GSNP.txt` and `programs/gai/GSNP.glp`, copied here as they stood at `6f5ac772`.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `2e3e5cc8`, for the paper's own record.
- The tools: this repository at `6f5ac772` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-05 07:53 UTC by `cascade.sh gsnp`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSNP.txt` | the contract |
| `GSNP.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [agent], [guardianship(agent, identity, principal, principal_of, guardian_of)], [relay(agent_acquaint, ward(1, agent, 3), free(2)), relay(agents_acquaint, ward(1, agent, 3), ward(2, agent, 4))], [retired])` |
| `vmat.tex` | the VMAT form, 29 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gsnp)
