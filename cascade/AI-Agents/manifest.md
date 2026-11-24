# A run of the cascade on AI-Agents

- Contract: `AI-Agents` (`ai_agents` to the checker), its owner's two files `programs/gai/AI-Agents.txt` and `programs/gai/AI-Agents.glp`, copied here as they stood at `8475b1c6`.
- The paper: `/Users/udi/Grassroots/GAI (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gai/AI-Agents.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gai/AI-Agents.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 21:57 UTC by `cascade.sh ai_agents`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8475b1c6`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `AI-Agents.txt` | `programs/gai/AI-Agents.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `AI-Agents.glp` | `programs/gai/AI-Agents.txt` | calculus/tools/adopt.py, copied from `programs/gai/AI-Agents.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `AI-Agents.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `AI-Agents.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `AI-Agents.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ai_agent], [guardianship(ai_agent, guardian, has_guardian)], [relay(ai_agent_acquaint, ward(1, ai_agent, no_guardian), free(2)), relay(ai_agents_acquaint, ward(1, ai_agent, no_guardian), ward(2, ai_agent, no_guardian))], [identity, person])` |
| `vmat.tex` | `AI-Agents.glp` | programs/gsc, compile_contract/2 | the VMAT form, 23 displays, one per schema |
| `platform.vglp` | `AI-Agents.glp` read by programs/gsc's as_read/2, and `AI-Agents.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for ai_agents |

What the tools refused or lacked:
- run: vGLP keeps no plays for ai_agents
