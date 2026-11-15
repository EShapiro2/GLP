# A run of the cascade on GAI

- Contract: `GAI` (`gai` to the checker), its owner's two files `programs/gai/GAI.txt` and `programs/gai/GAI.glp`, copied here as they stood at `e77043c7`.
- The paper: `/Users/udi/Grassroots/GAI (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gai/GAI.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gai/GAI.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-15 16:49 UTC by `cascade.sh gai`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `e77043c7`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GAI.txt` | `programs/gai/GAI.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GAI.glp` | `programs/gai/GAI.txt` | calculus/tools/adopt.py, copied from `programs/gai/GAI.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GAI.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GAI.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GAI.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ai_agent], [guardianship(ai_agent, guardian, has_guardian)], [relay(ai_agent_acquaint, ward(1, ai_agent, no_guardian), free(2)), relay(ai_agents_acquaint, ward(1, ai_agent, no_guardian), ward(2, ai_agent, no_guardian))], [identity, person])` |
| `vmat.tex` | `GAI.glp` | programs/gsc, compile_contract/2 | the VMAT form, 23 displays, one per schema |
| `platform.vglp` | `GAI.glp` read by programs/gsc's as_read/2, and `GAI.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for gai |

What the tools refused or lacked:
- run: vGLP keeps no plays for gai
