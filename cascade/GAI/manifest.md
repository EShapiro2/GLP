# A run of the cascade on GAI

- Contract: `GAI` (`gai` to the checker), its owner's two files `programs/gai/GAI.txt` and `programs/gai/GAI.glp`, copied here as they stood at `48286901`.
- The paper: `https://github.com/EShapiro2/GAI-Grassroots-Building-AI-Resilience-with-AI.git` at `229da098`, for the paper's own record.
- An instance of gnw, programs/gsc/GnW.glp, under [=(ward, agent), =(creator, principal), =(created, principal_of), =(guardian_of, operator_of), =(guarded_by, operated_by), =(retired, retired_agent), =(ward_acquaint, agent_acquaint), =(ward_befriend, agent_befriend), =(ward_unfriend, agent_unfriend), =(ward_end, agent_end), =(wards_acquaint, agents_acquaint), =(wards_befriend, agents_befriend)]: its term formed by programs/gsc's instantiate/4 from the base's, its file holding only what it adds or replaces (GSC, Definition Parametric contract).
- The tools: this repository at `48286901` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-11 23:42 UTC by `cascade.sh gai`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GAI.txt` | the contract |
| `GAI.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [agent], [guardianship(agent, operator_of, operated_by)], [relay(agent_acquaint, ward(1, agent, no_guardian), free(2)), relay(agents_acquaint, ward(1, agent, no_guardian), ward(2, agent, no_guardian))], [identity, person])` |
| `vmat.tex` | the VMAT form, 19 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gai)
