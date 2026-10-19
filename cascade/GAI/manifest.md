# A run of the cascade on GAI

- Contract: `GAI` (`gai` to the checker), its owner's two files `programs/gai/GAI.md` and `programs/gai/GAI.glp`, copied here as they stood.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `434806bc`, for the paper's own record.
- The tools: this repository at `ffd0387e` --- programs/gsc (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-10-19 18:01 UTC by `cascade.sh gai`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GAI.md` | the clauses |
| `GAI.glp` | the act schemas, each in GSC's notation above its term |
| `verdict.txt` | the checker's verdict: `grassroots_with_wards(befriend, [agent], [guardianship(agent, identity, principal, principal_of)], [relay(agent_befriend, ward(1, agent, 3), free(2)), relay(agents_befriend, ward(1, agent, 3), ward(2, agent, 4))], [retired])` |
| `vmat.tex` | the VMAT form, 12 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gai |

What the tools refused or lacked:
- run: vGLP keeps no plays for gai
